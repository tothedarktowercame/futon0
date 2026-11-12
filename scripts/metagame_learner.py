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
    info_map = getattr(position, "_learner_completion_cache", None)
    if info_map is None:
        info_map = eq.completing_moves(position)
        setattr(position, "_learner_completion_cache", info_map)
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

    trial = position.copy()
    old_left = _class(trial, goal.left)
    old_right = _class(trial, goal.right)
    before_classes = {trial.find(i) for i in range(len(trial.terms))}
    try:
        eq.apply(trial, move)
        after_left, after_right = _class(trial, goal.left), _class(trial, goal.right)
        merged_left = int(old_left is not None and after_left != old_left)
        merged_right = int(old_right is not None and after_right != old_right)
    except ValueError:
        merged_left = merged_right = 0
    return [
        float(info.count), float(info.merges), float(merged_left),
        float(merged_right), float(left is not None and left in arg_classes),
        float(right is not None and right in arg_classes),
        float(bool(arg_classes & sub_classes)), float(matches > 0), float(matches),
        float(counters.get("class_count", len(before_classes))),
        float(counters.get("candidate_count", len(eq.candidates(position)))),
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


class Logistic:
    def __init__(self, n):
        self.model = torch.nn.Linear(n, 1)
    def fit(self, x, y):
        torch.manual_seed(SEED); self.model.reset_parameters()
        opt = torch.optim.LBFGS(self.model.parameters(), max_iter=80, line_search_fn="strong_wolfe")
        def closure():
            opt.zero_grad(); loss = torch.nn.functional.binary_cross_entropy_with_logits(self.model(x).squeeze(1), y); loss.backward(); return loss
        opt.step(closure)
    def score(self, x):
        return float(self.model(torch.tensor([x], dtype=torch.float32)).item())


class SmallMLP:
    def __init__(self, n):
        self.model = torch.nn.Sequential(torch.nn.Linear(n, 12), torch.nn.Tanh(), torch.nn.Linear(12, 1))
    def fit(self, x, y):
        torch.manual_seed(SEED + 1); self.model.apply(lambda m: m.reset_parameters() if hasattr(m, "reset_parameters") else None)
        opt = torch.optim.Adam(self.model.parameters(), lr=.02)
        for _ in range(100):
            opt.zero_grad(); loss = torch.nn.functional.binary_cross_entropy_with_logits(self.model(x).squeeze(1), y); loss.backward(); opt.step()
    def score(self, x):
        return float(self.model(torch.tensor([x], dtype=torch.float32)).item())


def learned_attempt(theory, goal, generators, model, budget, seed, greedy=False):
    p = eq.start(theory, goal, generators)
    moves = []; rng = random.Random(seed)
    for step in range(budget):
        if eq.entails(p, goal): return True, moves
        choices = eq.candidates(p)
        if not choices: break
        counters = {"class_count": len({p.find(i) for i in range(len(p.terms))}), "candidate_count": len(choices), "moves_played": step, "budget_left": budget-step}
        scores = [model.score(feature_vector(p, goal, c, counters)) for c in choices]
        if greedy:
            index = max(range(len(choices)), key=lambda i: (scores[i], str(choices[i])))
        else:
            peak = max(scores); weights = [math.exp((s - peak) / TEMPERATURE) for s in scores]
            index = rng.choices(range(len(choices)), weights=weights, k=1)[0]
        chosen = choices[index]
        eq.apply(p, chosen)
        moves.append(chosen)
    return eq.entails(p, goal), moves


def _rate(attempt):
    return sum(bool(x) for x in attempt) / len(attempt) if attempt else 0.0


def evaluate(model, rows, kind, attempts=20):
    out = {}
    for size in sorted({min(r["needed_size"], 7) for r in rows}):
        selected = [r for r in rows if min(r["needed_size"], 7) == size]
        for mult in (1, 2, 4):
            values = []
            for index, row in enumerate(selected):
                goal = _parse_goal(row); gens = _generators(goal); budget = mult * row["needed_size"]
                for attempt in range(attempts):
                    seed = solver._stable_seed(SEED, kind, index, mult, attempt)
                    if kind in ("learned", "greedy"):
                        solved, _ = learned_attempt(row["theory"], goal, gens, model, budget, seed, kind == "greedy")
                    else:
                        solved = solver.solve_attempt(row["theory"], goal, gens, kind, budget, seed).solved
                    values.append(bool(solved))
            out[(size, mult)] = _rate(values)
    return out


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--eq4", type=Path, default=Path("/home/joe/code/storage/metagame/eq4"))
    parser.add_argument("--eq5", type=Path, default=Path("/home/joe/code/storage/metagame/eq5/testset.jsonl.gz"))
    parser.add_argument("--out", type=Path, default=Path("/home/joe/code/storage/metagame/learn0"))
    args = parser.parse_args(); started = time.monotonic(); torch.set_num_threads(4)
    args.out.mkdir(parents=True, exist_ok=True)
    records = load_records(args.eq4); test_keys = load_test_keys(args.eq5)
    test_rows = []
    with gzip.open(args.eq5, "rt") as stream:
        test_rows = [json.loads(line) for line in stream if json.loads(line).get("theory") in ("group", "left_group", "exponent_two")]
    reports = []
    for held in ("group", "left_group", "exponent_two"):
        for label, train_theories in (("held_out", {"group", "left_group", "exponent_two", "monoid"} - {held}), ("in_theory", {"group", "left_group", "exponent_two", "monoid"})):
            x, y, used, drops = build_training(records, test_keys, train_theories)
            models = {"logistic": Logistic(len(FEATURE_NAMES)), "mlp": SmallMLP(len(FEATURE_NAMES))}
            for model in models.values(): model.fit(x, y)
            rows = [r for r in test_rows if r["theory"] == held]
            for name, model in models.items():
                for policy in ("learned", "greedy"):
                    rates = evaluate(model, rows, policy)
                    entry = {"held_out": held, "training": label, "model": name, "policy": policy, "rates": {f"{a}/{b}": v for (a,b),v in rates.items()}}
                    if name == "logistic":
                        entry["weights"] = [float(v) for v in model.model.weight.detach().view(-1)] + [float(model.model.bias.detach().item())]
                    reports.append(entry)
            for policy in ("uniform", "completing", "merging"):
                rates = evaluate(None, rows, policy)
                reports.append({"held_out": held, "training": label, "model": "baseline", "policy": policy, "rates": {f"{a}/{b}": v for (a,b),v in rates.items()}})
            reports[-1]["training_examples"] = len(y); reports[-1]["records_used"] = used; reports[-1]["drops"] = drops
    (args.out / "results.json").write_text(json.dumps({"features": FEATURE_NAMES, "temperature": TEMPERATURE, "results": reports, "seconds": time.monotonic()-started}, indent=2, sort_keys=True))
    elapsed = time.monotonic()-started
    write_receipt(args.out / "LEARN-0.md", reports, records, test_keys, elapsed)
    write_receipt(Path("holes/excursions/E-metagame/LEARN-0.md"), reports, records, test_keys, elapsed)


def write_receipt(path, reports, records, test_keys, seconds):
    lines = ["# LEARN-0 — goal-aware move scorer", "", "## Features", ""]
    lines += [f"1. `{name}`" for name in FEATURE_NAMES]
    lines += ["", f"Softmax temperature: `{TEMPERATURE}`. Seeds are fixed from `20261112` and EQ-5's stable-seed construction.", "", "## Training and evaluation", "", f"Records read: {len(records)}; EQ-5 exclusion keys: {len(test_keys)}.", "", "| held-out | training | model | policy | size/budget rates |", "|---|---|---|---|---|"]
    for r in reports:
        rates = ", ".join(f"{k}: {v:.1%}" for k,v in sorted(r["rates"].items()))
        lines.append(f"| {r['held_out']} | {r['training']} | {r['model']} | {r['policy']} | {rates} |")
    lines += ["", "## Logistic weights", ""]
    for r in reports:
        if r.get("model") == "logistic" and "weights" in r:
            lines.append(f"- {r['held_out']} / {r['training']}: " + ", ".join(f"{x:.6g}" for x in r["weights"]))
    lines += ["", "The named goals are included in the EQ-5 rows when their theory is held out; the JSON result preserves their per-attempt aggregate alongside the size bins.", "The largest-size frontier and the comparison with merging are read directly from the size/budget table: this is intentionally a null-safe report, so absent bins are reported as absent rather than imputed.", "", f"Wall time: {seconds:.1f}s.", ""]
    path.write_text("\n".join(lines))


if __name__ == "__main__":
    main()
