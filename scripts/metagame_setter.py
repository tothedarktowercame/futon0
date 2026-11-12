#!/usr/bin/env python3
"""Generate hindsight equational goals from unbiased random construction.

The setter samples uniformly from :func:`metagame_eq.candidates`.  Its
``needed_terms`` result is inclusion-minimal under one-at-a-time removal; it
is not promised to have minimum cardinality.
"""

from __future__ import annotations

import argparse
import concurrent.futures
import gzip
import json
import random
import signal
import time
from dataclasses import dataclass
from pathlib import Path

import metagame_eq as eq


@dataclass
class Run:
    theory: str
    generator_count: int
    k: int
    seed: int
    index: int
    terms: list[eq.Term]
    position: eq.Position
    policy: str = "uniform"


@dataclass
class GoalRecord:
    theory: str
    generator_count: int
    k: int
    goal: eq.Goal
    needed: list[eq.Term]
    seed: int
    run_index: int
    policy: str = "uniform"

    def json(self):
        return {
            "theory": self.theory,
            "generators": self.generator_count,
            "K": self.k,
            "goal": [str(self.goal.left), str(self.goal.right)],
            "needed_terms": [str(t) for t in self.needed],
            "needed_size": len(self.needed),
            "seed": self.seed,
            "run_index": self.run_index,
            "policy": self.policy,
        }


def generators(n: int) -> tuple[eq.Term, ...]:
    return tuple(eq.gen(chr(ord("a") + i)) for i in range(n))


def empty_position(theory: str, n: int) -> eq.Position:
    p = eq.Position(eq.THEORIES[theory])
    p.add_term(eq.E)
    for g in generators(n):
        p.add_term(g)
    eq.closure(p)
    return p


def _policy_probability(policy: str):
    if policy == "uniform":
        return "uniform", 0.0
    name, open_paren, value = policy.partition("(")
    if name not in ("completing", "merging") or open_paren != "(" or not value.endswith(")"):
        raise ValueError(f"unknown policy: {policy}")
    probability = float(value[:-1])
    if not 0 <= probability <= 1:
        raise ValueError(f"policy probability outside [0,1]: {policy}")
    return name, probability


def choose_move(p: eq.Position, rng: random.Random, policy="uniform"):
    choices = eq.candidates(p)
    if not choices:
        return None
    name, probability = _policy_probability(policy)
    if name != "uniform" and rng.random() < probability:
        information = eq.completing_moves(p)
        preferred = [move for move in choices
                     if (information[move].count > 0 if name == "completing"
                         else information[move].merges)]
        if preferred:
            return rng.choice(preferred)
    return rng.choice(choices)


def random_run(theory: str, n: int, k: int, seed: int, index: int,
               policy="uniform") -> Run:
    rng = random.Random(seed)
    p = empty_position(theory, n)
    made = []
    for _ in range(k):
        move = choose_move(p, rng, policy)
        if move is None:
            break
        made.append(eq.apply(p, move))
    return Run(theory, n, k, seed, index, made, p, policy)


def _initial_mapping(p: eq.Position) -> dict[eq.Term, eq.Term]:
    return {t: p.representative_term(t) for t in p.terms}


def add_term_by_classes(p: eq.Position, term: eq.Term,
                        mapping: dict[eq.Term, eq.Term]) -> bool:
    """Replay one recorded term using its current argument classes.

    The recorded argument may have ceased to be a representative after the
    original move.  ``mapping`` preserves its identity across that change.
    Returns true exactly when a new operation node is added.
    """
    if term in mapping:
        mapping[term] = p.representative_term(mapping[term])
        return False
    args = []
    for old in term.args:
        current = mapping.get(old)
        if current is None and old in p.ids:
            current = p.representative_term(old)
        if current is None:
            return False
        current = p.representative_term(current)
        mapping[old] = current
        args.append(current)
    actual_args = tuple(args)
    existing = eq._lookup(p, term.op, actual_args)
    if existing is not None:
        mapping[term] = p.terms[p.find(existing)]
        return False
    actual = eq.Term(term.op, actual_args)
    p.add_term(actual)
    eq.closure(p)
    mapping[term] = p.representative_term(actual)
    return True


def replay_terms(theory: str | eq.Theory, goal: eq.Goal,
                 gens: tuple[eq.Term, ...], terms: list[eq.Term]):
    p = eq.start(theory, goal, gens)
    mapping = _initial_mapping(p)
    added = []
    for term in terms:
        if add_term_by_classes(p, term, mapping):
            added.append(term)
    return p, added, mapping


def needed_terms(theory: str | eq.Theory, goal: eq.Goal,
                 gens: tuple[eq.Term, ...], terms: list[eq.Term]):
    p, keep, _ = replay_terms(theory, goal, gens, terms)
    if not eq.entails(p, goal):
        raise AssertionError(f"full monotone replay failed to prove {goal}")
    i = 0
    while i < len(keep):
        trial = keep[:i] + keep[i + 1:]
        q, actually_added, _ = replay_terms(theory, goal, gens, trial)
        if eq.entails(q, goal):
            keep = actually_added
        else:
            i += 1
    return keep


def _rename_serial(term: eq.Term, names: dict[str, str]) -> str:
    if term.op == "e":
        return "e"
    if term.op == "gen":
        old = str(term.args[0])
        if old not in names:
            names[old] = f"g{len(names)}"
        return names[old]
    return f"{term.op}(" + ",".join(_rename_serial(a, names)
                                     for a in term.args) + ")"


def canonical_goal(goal: eq.Goal) -> tuple[str, str]:
    def orientation(left, right):
        names: dict[str, str] = {}
        return _rename_serial(left, names), _rename_serial(right, names)
    return min(orientation(goal.left, goal.right),
               orientation(goal.right, goal.left))


def classify_run(run: Run):
    gens = generators(run.generator_count)
    trivial = 0
    records = []
    for goal in eq.facts(run.position):
        initial = eq.start(run.theory, goal, gens)
        if eq.entails(initial, goal):
            trivial += 1
            continue
        proof = needed_terms(run.theory, goal, gens, run.terms)
        records.append(GoalRecord(run.theory, run.generator_count, run.k,
                                  goal, proof, run.seed, run.index, run.policy))
    return trivial, records


def _cell(args):
    theory, n, k, seconds, base_seed, policy = args
    deadline = time.monotonic() + seconds
    index = goals_seen = trivial = 0
    records = {}
    started = time.monotonic()
    while time.monotonic() < deadline:
        seed = base_seed + index
        run = random_run(theory, n, k, seed, index, policy)
        fs = eq.facts(run.position)
        goals_seen += len(fs)
        ntrivial, found = classify_run(run)
        trivial += ntrivial
        for record in found:
            records.setdefault(canonical_goal(record.goal), record)
        index += 1
    elapsed = time.monotonic() - started
    return {"theory": theory, "generators": n, "K": k, "policy": policy,
            "runs": index,
            "elapsed": elapsed, "goals_seen": goals_seen,
            "trivial": trivial, "records": list(records.values())}


class _ProofTimeout(Exception):
    pass


def _alarm(_signum, _frame):
    raise _ProofTimeout


def optimum_check(records: list[GoalRecord], seed=20261112,
                  sample_size=200, timeout=0.5):
    eligible = [r for r in records if 1 <= len(r.needed) <= 3]
    random.Random(seed).shuffle(eligible)
    chosen = eligible[:sample_size]
    old = signal.signal(signal.SIGALRM, _alarm)
    agreement = timeouts = 0
    differences = []
    try:
        for record in chosen:
            signal.setitimer(signal.ITIMER_REAL, timeout)
            try:
                answer = eq.shortest_proof(record.theory, record.goal,
                                           max_moves=len(record.needed))
            except _ProofTimeout:
                timeouts += 1
                continue
            finally:
                signal.setitimer(signal.ITIMER_REAL, 0)
            optimum = answer[0] if answer else None
            if optimum == len(record.needed):
                agreement += 1
            else:
                differences.append((len(record.needed), optimum))
    finally:
        signal.signal(signal.SIGALRM, old)
    return {"sampled": len(chosen), "agree": agreement,
            "timeouts": timeouts, "differences": differences}


def _demo_keys():
    a, b = eq.gen("a"), eq.gen("b")
    return {
        "exponent_two_commutes": ("exponent_two", canonical_goal(
            eq.Goal(eq.mul(a, b), eq.mul(b, a)))),
        "inverse_product": ("group", canonical_goal(eq.Goal(
            eq.inv(eq.mul(a, b)), eq.mul(eq.inv(b), eq.inv(a))))),
        "left_group_right_inverse": ("left_group", canonical_goal(
            eq.Goal(eq.mul(a, eq.inv(a)), eq.E))),
    }


def _hist(records):
    out = {"1": 0, "2": 0, "3": 0, "4": 0, "5": 0, "more": 0}
    for r in records:
        key = str(len(r.needed)) if len(r.needed) <= 5 else "more"
        out[key] += 1
    return out


def stratified_sample(records: list[GoalRecord], count: int, seed: int):
    """Sample reproducibly across every represented needed-set size."""
    bins: dict[int, list[GoalRecord]] = {}
    for record in records:
        bins.setdefault(len(record.needed), []).append(record)
    rng = random.Random(seed)
    for values in bins.values():
        rng.shuffle(values)
    out = []
    while len(out) < count and any(bins.values()):
        for size in sorted(bins):
            if bins[size] and len(out) < count:
                out.append(bins[size].pop())
    return out


def write_results(results, out_dir: Path, note: Path, seconds: float):
    out_dir.mkdir(parents=True, exist_ok=True)
    by_theory: dict[str, dict[tuple[str, str], GoalRecord]] = {}
    for cell in results:
        table = by_theory.setdefault(cell["theory"], {})
        for record in cell["records"]:
            table.setdefault(canonical_goal(record.goal), record)
    all_records = []
    for theory, table in by_theory.items():
        selected = list(table.values())[:50000]
        all_records.extend(selected)
        with gzip.open(out_dir / f"{theory}.jsonl.gz", "wt") as stream:
            for record in selected:
                stream.write(json.dumps(record.json(), sort_keys=True) + "\n")

    optimum = optimum_check(all_records)
    samples = stratified_sample(all_records, 20, 20261112)
    verified = 0
    for record in samples:
        p, _, _ = replay_terms(record.theory, record.goal,
                               generators(record.generator_count), record.needed)
        verified += int(eq.entails(p, record.goal))

    demos = _demo_keys()
    found = {(r.theory, canonical_goal(r.goal)) for r in all_records}
    lines = ["# EQ-2: hindsight goals from random construction", "",
             (f"Each of 18 cells ran for {seconds:.0f} wall seconds, with at "
              "most six worker processes. Seeds are `202611120000 + cell*10^7 "
              "+ run_index`; the gzip records retain each exact seed."), "",
             "## Cells", "",
             "| theory | generators | K | runs | runs/s | goals | trivial | distinct non-trivial | needed 1/2/3/4/5/>5 |",
             "|---|---:|---:|---:|---:|---:|---:|---:|---|" ]
    for c in sorted(results, key=lambda x: (x["theory"], x["generators"], x["K"])):
        h = _hist(c["records"])
        lines.append(f"| {c['theory']} | {c['generators']} | {c['K']} | {c['runs']} | {c['runs']/c['elapsed']:.2f} | {c['goals_seen']} | {c['trivial']} | {len(c['records'])} | {h['1']}/{h['2']}/{h['3']}/{h['4']}/{h['5']}/{h['more']} |")
    lines += ["", "## Yield", "",
              "Distinct goals are deduplicated per theory up to generator renaming and side swapping.", "",
              "| theory | CPU hours | needed >=2 / CPU-hour | needed >=3 / CPU-hour |",
              "|---|---:|---:|---:|"]
    for theory in sorted(by_theory):
        cpu = sum(c["elapsed"] for c in results if c["theory"] == theory) / 3600
        rs = list(by_theory[theory].values())
        ge2 = sum(len(r.needed) >= 2 for r in rs) / cpu if cpu else 0
        ge3 = sum(len(r.needed) >= 3 for r in rs) / cpu if cpu else 0
        lines.append(f"| {theory} | {cpu:.3f} | {ge2:.1f} | {ge3:.1f} |")
    lines += ["", "The Euclid redesign marks were 500 distinct goals/hour needing at least two constructions and 50/hour needing at least three; the table reports the untuned comparison.", "",
              "## Optimum check", "",
              f"Seeded sample: {optimum['sampled']}; greedy needed size agreed with exhaustive shortest proof in {optimum['agree']}; timeouts: {optimum['timeouts']}; non-agreements (needed, optimum): `{optimum['differences']}`. Each call had a 0.5 s wall limit.", "",
              "## Demo goals and fresh replay", ""]
    for name, key in demos.items():
        lines.append(f"- `{name}`: {'appeared' if key in found else 'did not appear'}.")
    lines += [f"- Fresh-start replay proved {verified}/{len(samples)} goals sampled with seed 20261112 from their recorded needed terms only.", "",
              "## Sampled goals", ""]
    for r in samples:
        lines.append(f"- `{r.goal.left} = {r.goal.right}` ({r.theory}, needed {len(r.needed)}): " + ", ".join(f"`{t}`" for t in r.needed))
    lines += ["", "## Findings", "",
              "No monotonic-replay assertion failed. The stored proof is removal-minimal in its recorded construction order, not guaranteed minimum; the optimum sample measures that distinction.", ""]
    note.parent.mkdir(parents=True, exist_ok=True)
    note.write_text("\n".join(lines))
    (out_dir / "summary.json").write_text(json.dumps({
        "cells": [{k: v for k, v in c.items() if k != "records"} for c in results],
        "optimum": optimum, "fresh_replay": [verified, len(samples)]}, indent=2))


def _has_identity_argument(term: eq.Term):
    if term.op in ("gen", "e"):
        return False
    return any(arg == eq.E or _has_identity_argument(arg) for arg in term.args)


def write_eq4_results(results, out_dir: Path, note: Path, seconds: float):
    tables = {}
    for cell in results:
        key = (cell["theory"], cell["policy"])
        table = tables.setdefault(key, {})
        for record in cell["records"]:
            table.setdefault(canonical_goal(record.goal), record)
    for (theory, policy), table in tables.items():
        target = out_dir / policy
        target.mkdir(parents=True, exist_ok=True)
        with gzip.open(target / f"{theory}.jsonl.gz", "wt") as stream:
            for record in list(table.values())[:50000]:
                stream.write(json.dumps(record.json(), sort_keys=True) + "\n")

    lines = ["# EQ-4 — rule-completing setter policies", "",
             (f"Each of 32 cells ran for {seconds:.0f} wall seconds with at most six "
              "workers. Every non-uniform choice uses only the current rule statements, "
              "their present instances, and the supplied probability."), "",
             "## Cells", "",
             "| theory | K | policy | runs | runs/s | trivial share | distinct non-trivial | needed 1/2/3/4/5/>5 |",
             "|---|---:|---|---:|---:|---:|---:|---|"]
    for cell in sorted(results, key=lambda c: (c["theory"], c["K"], c["policy"])):
        hist = _hist(cell["records"])
        share = cell["trivial"] / cell["goals_seen"] if cell["goals_seen"] else 0
        lines.append(f"| {cell['theory']} | {cell['K']} | {cell['policy']} | {cell['runs']} | {cell['runs']/cell['elapsed']:.2f} | {share:.1%} | {len(cell['records'])} | {hist['1']}/{hist['2']}/{hist['3']}/{hist['4']}/{hist['5']}/{hist['more']} |")
    lines += ["", "## Yield by theory and policy", "",
              "Rates deduplicate goals across K. The ‘clean’ columns drop a goal if `e` occurs as an argument of any operation on either side.", "",
              "| theory | policy | >=2/h | >=3/h | >=4/h | clean >=2/h | clean >=3/h | clean >=4/h |",
              "|---|---|---:|---:|---:|---:|---:|---:|"]
    for key in sorted(tables):
        theory, policy = key
        cpu_hours = sum(c["elapsed"] for c in results
                        if (c["theory"], c["policy"]) == key) / 3600
        records = list(tables[key].values())
        clean = [r for r in records if not (_has_identity_argument(r.goal.left) or
                                             _has_identity_argument(r.goal.right))]
        rates = [sum(len(r.needed) >= n for r in group) / cpu_hours
                 for group in (records, clean) for n in (2, 3, 4)]
        lines.append(f"| {theory} | {policy} | " + " | ".join(f"{x:.1f}" for x in rates) + " |")

    demos = _demo_keys()
    appearances = []
    for name, (theory, goal_key) in demos.items():
        for (candidate_theory, policy), table in tables.items():
            if candidate_theory == theory and goal_key in table:
                record = table[goal_key]
                appearances.append((name, policy, len(record.needed)))
    lines += ["", "## Held-out DEMO goals", ""]
    if appearances:
        for name, policy, size in appearances:
            lines.append(f"- `{name}` appeared under `{policy}` with needed size {size}; it is no longer held out.")
    else:
        lines.append("None of the three DEMO goals appeared under any policy.")

    lines += ["", "## Fresh replay samples", ""]
    for policy in sorted({c["policy"] for c in results}):
        records = [r for (th, pol), table in tables.items() if pol == policy
                   for r in table.values()]
        sample = stratified_sample(records, 20, 20261112)
        proved = 0
        for record in sample:
            p, _, _ = replay_terms(record.theory, record.goal,
                                   generators(record.generator_count), record.needed)
            proved += int(eq.entails(p, record.goal))
        lines.append(f"- `{policy}`: {proved}/{len(sample)} sampled goals re-proved from their needed terms on a fresh start.")
    lines += ["", "## Comparison", "",
              "Packet 2’s uniform group reference was 183 distinct goals/CPU-hour needing at least two constructions and 23 needing at least three. The table above is the untuned comparison.", ""]
    note.parent.mkdir(parents=True, exist_ok=True)
    note.write_text("\n".join(lines))
    (out_dir / "summary.json").write_text(json.dumps({
        "cells": [{k: v for k, v in c.items() if k != "records"}
                  for c in results]}, indent=2))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--seconds", type=float, default=180)
    parser.add_argument("--workers", type=int, default=6)
    parser.add_argument("--out", type=Path,
                        default=Path("/home/joe/code/storage/metagame/eq2"))
    parser.add_argument("--note", type=Path,
                        default=Path(__file__).parents[1] / "holes/excursions/E-metagame/EQ-2.md")
    parser.add_argument("--eq4", action="store_true")
    args = parser.parse_args()
    cells = []
    i = 0
    if args.eq4:
        for theory in ("group", "left_group", "exponent_two", "monoid"):
            for k in (10, 14):
                for policy in ("uniform", "completing(0.5)",
                               "completing(0.8)", "merging(0.8)"):
                    cells.append((theory, 2, k, args.seconds,
                                  202611120000 + i * 10_000_000, policy))
                    i += 1
    else:
        for theory in ("group", "left_group", "exponent_two"):
            for n in (2, 3):
                for k in (6, 10, 14):
                    cells.append((theory, n, k, args.seconds,
                                  202611120000 + i * 10_000_000, "uniform"))
                    i += 1
    with concurrent.futures.ProcessPoolExecutor(max_workers=args.workers) as pool:
        results = list(pool.map(_cell, cells))
    if args.eq4:
        write_eq4_results(results, args.out, args.note, args.seconds)
    else:
        write_results(results, args.out, args.note, args.seconds)


if __name__ == "__main__":
    main()
