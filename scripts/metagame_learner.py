#!/usr/bin/env python3
"""Goal-aware, theory-independent move scoring for the equational metagame.

The learner deliberately treats operation names as data: feature positions are
fixed, while the operation names only enter the local structural tests used to
make those positions.  This makes a consistently renamed theory a useful
invariance test rather than a special case in the model.
"""

from __future__ import annotations

import argparse
import gzip
import hashlib
import json
import math
import random
import statistics
import time
from pathlib import Path

import torch

import metagame_eq as eq
import metagame_setter as setter
import metagame_solver as solver

SEED = 20261112
TEMPERATURE = 0.75
FEATURE_NAMES = (
    "completing_count", "completes_merge", "merge_goal_left",
    "merge_goal_right", "arg_has_left_class", "arg_has_right_class",
    "arg_has_goal_subterm_class", "built_matches_rule_side",
    "built_rule_side_count", "class_count", "candidate_count",
    "moves_played", "budget_left",
)


def _class(p: eq.Position, t: eq.Term):
    return p.find(p.ids[t]) if t in p.ids else None


def _goal_subterms(goal: eq.Goal):
    return tuple(eq.subterms(goal.left)) + tuple(eq.subterms(goal.right))


def _pattern_matches(p, pattern, term, env=None):
    env = {} if env is None else env
    if pattern.op == "var":
        name = str(pattern.args[0])
        old = env.get(name)
        actual = _class(p, term)
        if actual is None:
            return None
        if old is not None and p.find(old) != actual:
            return None
        return {**env, name: actual}
    if pattern.op != term.op or len(pattern.args) != len(term.args):
        return None
    for left, right in zip(pattern.args, term.args):
        env = _pattern_matches(p, left, right, env)
        if env is None:
            return None
    return env


def feature_vector(position: eq.Position, goal: eq.Goal, move: eq.Move,
                   counters: dict[str, int] | None = None) -> list[float]:
    """Return the fixed numeric feature vector for one move.

    The signature intentionally contains only position, goal, move and live
    counters.  In particular it accepts no recorded proof/needed-set input.
    """
    counters = counters or {}
    left, right = _class(position, goal.left), _class(position, goal.right)
    stamp = (len(position.terms),
             len({position.find(i) for i in range(len(position.terms))}))
    cached = getattr(position, "_learner_completion_cache", None)
    if cached is None or cached[0] != stamp:
        # A position only gains terms and merges classes, so this pair
        # changes whenever the position does; a stale map is never reused.
        cached = (stamp, eq.completing_moves(position))
        setattr(position, "_learner_completion_cache", cached)
    info_map = cached[1]
    info = info_map.get(move, eq.MoveCompletion(0, False))
    args = tuple(_class(position, a) for a in move.args)
    arg_classes = set(x for x in args if x is not None)
    sub_classes = set(_class(position, t) for t in _goal_subterms(goal))
    child = eq.Term(move.op, tuple(position.representative_term(a)
                                   for a in move.args))

    matches = 0
    for rule in position.theory.rules:
        for side in (rule.left, rule.right):
            if _pattern_matches(position, side, child) is not None:
                matches += 1

    merged_left = merged_right = 0
    if info.merges:
        # Only a move that completes a merging rule instance can change any
        # class: its new node is absent modulo current classes, so the first
        # merge after it must come from a rule instance containing that node.
        # Other moves leave the goal's classes alone and need no trial.
        trial = position.copy()
        old_left = _class(trial, goal.left)
        old_right = _class(trial, goal.right)
        try:
            eq.apply(trial, move)
            after_left, after_right = _class(trial, goal.left), _class(trial, goal.right)
            merged_left = int(old_left is not None and after_left != old_left)
            merged_right = int(old_right is not None and after_right != old_right)
        except ValueError:
            pass
    return [
        float(info.count), float(info.merges), float(merged_left),
        float(merged_right), float(left is not None and left in arg_classes),
        float(right is not None and right in arg_classes),
        float(bool(arg_classes & sub_classes)), float(matches > 0), float(matches),
        float(counters.get("class_count", stamp[1])),
        float(counters["candidate_count"] if "candidate_count" in counters
              else len(info_map)),
        float(counters.get("moves_played", 0)), float(counters.get("budget_left", 0)),
    ]


def _generators(goal):
    names = set()
    def visit(t):
        if t.op == "gen":
            names.add(str(t.args[0]))
        else:
            for a in t.args:
                if isinstance(a, eq.Term):
                    visit(a)
    visit(goal.left); visit(goal.right)
    return tuple(sorted(names))


def _parse_goal(row):
    return eq.Goal(solver.parse_term(row["goal"][0]),
                   solver.parse_term(row["goal"][1]))


def load_records(root: Path):
    """Deduplicate EQ-4 records by theory and canonical goal."""
    out = {}
    for path in sorted(root.rglob("*.jsonl.gz")):
        with gzip.open(path, "rt") as stream:
            for line in stream:
                row = json.loads(line)
                goal = _parse_goal(row)
                key = (row["theory"], setter.canonical_goal(goal))
                if key not in out or row["needed_size"] < out[key][0]["needed_size"]:
                    out[key] = (row, goal)
    return list(out.values())


def load_test_keys(path: Path):
    keys = set()
    with gzip.open(path, "rt") as stream:
        for line in stream:
            row = json.loads(line)
            keys.add((row["theory"], setter.canonical_goal(_parse_goal(row))))
    return keys


def is_test_goal(theory: str, goal: eq.Goal, excluded: set[tuple[str, tuple[str, str]]]):
    """Canonical, orientation-insensitive test-set membership check."""
    return (theory, setter.canonical_goal(goal)) in excluded


def _target_move(p, term, mapping):
    args = []
    for old in term.args:
        current = mapping.get(old)
        if current is None or current not in p.ids:
            return None
        args.append(p.representative_term(current))
    wanted = eq.Move(term.op, tuple(args))
    return wanted if wanted in eq.candidates(p) else None


def examples_for_record(row, goal, negative_limit=8, seed=SEED):
    """Replay one needed set and return (X, y), or None if it cannot replay."""
    p = eq.start(row["theory"], goal, _generators(goal))
    mapping = setter._initial_mapping(p)
    rng = random.Random(seed)
    xs, ys = [], []
    for step, term in enumerate([solver.parse_term(x) for x in row["needed_terms"]]):
        move = _target_move(p, term, mapping)
        if move is None:
            return None
        choices = eq.candidates(p)
        counters = {"class_count": len({p.find(i) for i in range(len(p.terms))}),
                    "candidate_count": len(choices), "moves_played": step,
                    "budget_left": len(row["needed_terms"]) - step}
        for candidate in choices:
            if candidate == move:
                xs.append(feature_vector(p, goal, candidate, counters)); ys.append(1.0)
        negatives = [c for c in choices if c != move]
        rng.shuffle(negatives)
        for candidate in negatives[:negative_limit]:
            xs.append(feature_vector(p, goal, candidate, counters)); ys.append(0.0)
        result = setter.add_term_by_classes(p, term, mapping)
        if not result:
            return None
    if not eq.entails(p, goal):
        return None
    return xs, ys


def build_training(records, excluded, theories):
    xs, ys, drops, used = [], [], 0, 0
    for row, goal in records:
        key = (row["theory"], setter.canonical_goal(goal))
        if row["theory"] not in theories or is_test_goal(row["theory"], goal, excluded):
            continue
        example = examples_for_record(row, goal, seed=SEED + used)
        if example is None:
            drops += 1
            continue
        x, y = example
        xs.extend(x); ys.extend(y); used += 1
    if not xs:
        raise ValueError("no replayable training examples")
    return torch.tensor(xs, dtype=torch.float32), torch.tensor(ys, dtype=torch.float32), used, drops


class _Scaled:
    """Inputs are standardised with the training mean and deviation."""
    def scale(self, x):
        return (x - self.mean) / self.std
    def set_scale(self, x):
        self.mean = x.mean(0); self.std = x.std(0).clamp_min(1e-6)
        return self.scale(x)
    def score(self, x):
        with torch.no_grad():
            row = self.scale(torch.tensor([x], dtype=torch.float32))
            return float(self.model(row).item())
    def score_many(self, xs):
        with torch.no_grad():
            rows = self.scale(torch.tensor(xs, dtype=torch.float32))
            return self.model(rows).view(-1).tolist()


class Logistic(_Scaled):
    def __init__(self, n):
        self.model = torch.nn.Linear(n, 1)
    def fit(self, x, y):
        x = self.set_scale(x)
        torch.manual_seed(SEED); self.model.reset_parameters()
        opt = torch.optim.LBFGS(self.model.parameters(), max_iter=80, line_search_fn="strong_wolfe")
        def closure():
            opt.zero_grad(); loss = torch.nn.functional.binary_cross_entropy_with_logits(self.model(x).squeeze(1), y); loss.backward(); return loss
        opt.step(closure)


class SmallMLP(_Scaled):
    def __init__(self, n):
        self.model = torch.nn.Sequential(torch.nn.Linear(n, 12), torch.nn.Tanh(), torch.nn.Linear(12, 1))
    def fit(self, x, y):
        x = self.set_scale(x)
        torch.manual_seed(SEED + 1); self.model.apply(lambda m: m.reset_parameters() if hasattr(m, "reset_parameters") else None)
        opt = torch.optim.Adam(self.model.parameters(), lr=.02)
        for _ in range(100):
            opt.zero_grad(); loss = torch.nn.functional.binary_cross_entropy_with_logits(self.model(x).squeeze(1), y); loss.backward(); opt.step()


def learned_attempt(theory, goal, generators, model, budget, seed, greedy=False):
    p = eq.start(theory, goal, generators)
    moves = []; rng = random.Random(seed)
    for step in range(budget):
        if eq.entails(p, goal): return True, moves
        choices = eq.candidates(p)
        if not choices: break
        counters = {"class_count": len({p.find(i) for i in range(len(p.terms))}), "candidate_count": len(choices), "moves_played": step, "budget_left": budget-step}
        vectors = [feature_vector(p, goal, c, counters) for c in choices]
        scores = (model.score_many(vectors) if hasattr(model, "score_many")
                  else [model.score(v) for v in vectors])
        if greedy:
            index = max(range(len(choices)), key=lambda i: (scores[i], str(choices[i])))
        else:
            peak = max(scores); weights = [math.exp((s - peak) / TEMPERATURE) for s in scores]
            index = rng.choices(range(len(choices)), weights=weights, k=1)[0]
        chosen = choices[index]
        eq.apply(p, chosen)
        moves.append(chosen)
    return eq.entails(p, goal), moves


# --- evaluation -----------------------------------------------------------
# Revised by claude-6 in review: the first version scored every goal in one
# process, re-ran the EQ-5 baselines, and did not finish.  This version runs a
# fixed subsample of the EQ-5 test set in worker processes at budget 2n and
# reads the baseline figures for the same goals from the EQ-5 results file.

_WORKER = {}


def _init_worker(models):
    torch.set_num_threads(1)
    _WORKER["models"] = models


def _eval_goal(job):
    eq5_index, row, model_name, multipliers, attempts = job
    goal = _parse_goal(row); gens = _generators(goal)
    model = _WORKER["models"][model_name]
    out = []
    for mult in multipliers:
        budget = mult * row["needed_size"]
        solved = []
        for attempt in range(attempts):
            seed = solver._stable_seed(SEED, eq5_index, model_name, mult, attempt)
            ok, moves = learned_attempt(row["theory"], goal, gens, model, budget, seed)
            assert len(moves) <= budget
            solved.append(bool(ok))
        ok, moves = learned_attempt(row["theory"], goal, gens, model, budget, 0,
                                    greedy=True)
        assert len(moves) <= budget
        out.append({"index": eq5_index, "model": model_name, "mult": mult,
                    "solved": solved, "greedy": bool(ok)})
    return out


def sample_test_rows(rows, theory, per_bin):
    """Fixed subsample: the first per_bin EQ-5 rows of each needed-size bin."""
    bins = {}
    for index, row in enumerate(rows):
        if row["theory"] == theory and not row.get("named"):
            bins.setdefault(min(row["needed_size"], 7), []).append((index, row))
    named = [(i, r) for i, r in enumerate(rows)
             if r["theory"] == theory and r.get("named")]
    return [x for size in sorted(bins) for x in bins[size][:per_bin]], named


def baseline_rates(eq5_results, indices, mult):
    """Mean EQ-5 solve rate per policy over exactly these goals."""
    out = {}
    for policy in ("uniform", "completing", "merging"):
        flags = [a["solved"] for i in indices for a in eq5_results[i]["attempts"]
                 if a["policy"] == policy and a["multiplier"] == mult]
        out[policy] = sum(flags) / len(flags) if flags else None
    return out


def main():
    import concurrent.futures
    parser = argparse.ArgumentParser()
    parser.add_argument("--eq4", type=Path, default=Path("/home/joe/code/storage/metagame/eq4"))
    parser.add_argument("--eq5", type=Path, default=Path("/home/joe/code/storage/metagame/eq5"))
    parser.add_argument("--out", type=Path, default=Path("/home/joe/code/storage/metagame/learn0"))
    parser.add_argument("--note", type=Path, default=Path(__file__).parents[1] / "holes/excursions/E-metagame/LEARN-0.md")
    parser.add_argument("--per-bin", type=int, default=20)
    parser.add_argument("--attempts", type=int, default=5)
    parser.add_argument("--workers", type=int, default=4)
    parser.add_argument("--only", default=None, help="one held-out theory")
    args = parser.parse_args(); started = time.monotonic(); torch.set_num_threads(4)
    args.out.mkdir(parents=True, exist_ok=True)
    records = load_records(args.eq4)
    test_keys = load_test_keys(args.eq5 / "testset.jsonl.gz")
    with gzip.open(args.eq5 / "testset.jsonl.gz", "rt") as stream:
        rows = [json.loads(line) for line in stream]
    with gzip.open(args.eq5 / "results.jsonl.gz", "rt") as stream:
        eq5_results = {r["index"]: r for r in map(json.loads, stream)}
    everything = {"group", "left_group", "exponent_two", "monoid"}
    report = {"features": FEATURE_NAMES, "temperature": TEMPERATURE,
              "per_bin": args.per_bin, "attempts": args.attempts, "splits": []}
    for held in ("group", "left_group", "exponent_two"):
        if args.only and held != args.only:
            continue
        models, training = {}, {}
        for label, theories in (("held_out", everything - {held}),
                                ("in_theory", everything)):
            x, y, used, drops = build_training(records, test_keys, theories)
            training[label] = {"examples": len(y), "positives": int(y.sum()),
                               "records": used, "drops": drops}
            names = ("logistic", "mlp") if label == "held_out" else ("logistic",)
            for name in names:
                model = (Logistic if name == "logistic" else SmallMLP)(len(FEATURE_NAMES))
                model.fit(x, y)
                models[f"{label}/{name}"] = model
        ordinary, named = sample_test_rows(rows, held, args.per_bin)
        jobs = [(i, row, name, (2,), args.attempts)
                for name in models for i, row in ordinary]
        jobs += [(i, row, name, (2, 4), args.attempts)
                 for name in models for i, row in named]
        jobs.sort(key=lambda j: -j[1]["needed_size"])
        with concurrent.futures.ProcessPoolExecutor(
                max_workers=args.workers, initializer=_init_worker,
                initargs=(models,)) as pool:
            results = [r for rs in pool.map(_eval_goal, jobs) for r in rs]
        weights = {name: [float(v) for v in m.model.weight.detach().view(-1)]
                   + [float(m.model.bias.detach().item())]
                   for name, m in models.items() if name.endswith("logistic")}
        split = {"held_out": held, "training": training, "weights": weights,
                 "results": results,
                 "goals": {i: {"needed_size": r["needed_size"],
                               "named": r.get("named")}
                           for i, r in ordinary + named}}
        report["splits"].append(split)
        (args.out / f"results-{held}.json").write_text(json.dumps(split, sort_keys=True))
    report["seconds"] = time.monotonic() - started
    write_receipt(args.note, report, eq5_results)


def write_receipt(path, report, eq5_results):
    lines = ["# LEARN-0 — goal-aware move scorer, leave one theory out", "",
             "## Features", ""]
    lines += [f"{n}. `{name}`" for n, name in enumerate(FEATURE_NAMES, 1)]
    lines += ["", f"Softmax temperature `{TEMPERATURE}`; inputs standardised with the training mean and deviation. "
              f"Test goals: the first {report['per_bin']} EQ-5 rows of each needed-size bin of the held-out theory; "
              f"{report['attempts']} sampled attempts and one greedy attempt per goal, budget 2n. "
              "Baseline columns are the EQ-5 figures (20 attempts) for the same goals.", ""]
    for split in report["splits"]:
        held = split["held_out"]
        lines += [f"## Held out: {held}", ""]
        for label, t in split["training"].items():
            lines.append(f"- training `{label}`: {t['records']} records, {t['examples']} examples ({t['positives']} positive), {t['drops']} dropped as not replayable.")
        goals = {int(k): v for k, v in split["goals"].items()}
        model_names = sorted({r["model"] for r in split["results"]})
        lines += ["", "| needed | goals | uniform | completing | merging | "
                  + " | ".join(f"{m} sampled | {m} greedy" for m in model_names) + " |",
                  "|---:|---:|---:|---:|---:|" + "---:|---:|" * len(model_names)]
        sizes = sorted({min(g["needed_size"], 7) for g in goals.values() if not g["named"]})
        def cell(v):
            return "—" if v is None else f"{v:.1%}"
        for size in sizes:
            idx = [i for i, g in goals.items() if not g["named"] and min(g["needed_size"], 7) == size]
            base = baseline_rates(eq5_results, idx, 2)
            cells = []
            for m in model_names:
                rs = [r for r in split["results"] if r["model"] == m and r["mult"] == 2 and r["index"] in idx]
                flags = [f for r in rs for f in r["solved"]]
                cells += [cell(sum(flags) / len(flags) if flags else None),
                          cell(sum(r["greedy"] for r in rs) / len(rs) if rs else None)]
            lines.append(f"| {size if size <= 6 else '>6'} | {len(idx)} | {cell(base['uniform'])} | {cell(base['completing'])} | {cell(base['merging'])} | " + " | ".join(cells) + " |")
        lines += ["", "Named goals:", ""]
        for i, g in goals.items():
            if not g["named"]:
                continue
            for mult in (2, 4):
                base = baseline_rates(eq5_results, [i], mult)
                parts = []
                for m in model_names:
                    rs = [r for r in split["results"] if r["model"] == m and r["mult"] == mult and r["index"] == i]
                    if rs:
                        parts.append(f"{m} {sum(rs[0]['solved'])}/{len(rs[0]['solved'])} sampled, greedy {'solved' if rs[0]['greedy'] else 'failed'}")
                lines.append(f"- `{g['named']}` at {mult}n: merging {cell(base['merging'])}, completing {cell(base['completing'])}; " + "; ".join(parts) + ".")
        lines += ["", "Logistic weights (feature order as above, then bias):", ""]
        for name, w in split["weights"].items():
            lines.append(f"- `{name}`: " + ", ".join(f"{x:.3f}" for x in w))
        lines.append("")
    lines += [f"Wall time: {report['seconds']:.0f}s.", ""]
    path.write_text("\n".join(lines))


if __name__ == "__main__":
    main()
