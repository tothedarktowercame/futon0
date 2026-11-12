#!/usr/bin/env python3
"""Measure solver baselines after adding already-proved equations as rules."""

from __future__ import annotations

import concurrent.futures
import gzip
import json
import statistics
import time
from collections import Counter
from dataclasses import dataclass
from pathlib import Path

import metagame_eq as eq
import metagame_setter as setter
import metagame_solver as solver


@dataclass(frozen=True)
class Derived:
    name: str
    base: str
    theory: eq.Theory


def _extend(base: str, name: str, *rules: eq.Rule) -> Derived:
    source = eq.THEORIES[base]
    return Derived(name, base, eq.Theory(source.rules + rules,
                                         source.hypotheses,
                                         source.signature))


def derived_theories() -> tuple[Derived, ...]:
    inverse_product = eq.Rule(
        "inverse_product", eq.inv(eq.mul(eq.X, eq.Y)),
        eq.mul(eq.inv(eq.Y), eq.inv(eq.X)))
    return (
        _extend("left_group", "left_group+right_inverse",
                eq.RULE_LIBRARY["right_inverse"]),
        _extend("left_group", "left_group+right_inverse+right_identity",
                eq.RULE_LIBRARY["right_inverse"],
                eq.RULE_LIBRARY["right_identity"]),
        _extend("group", "group+inverse_product", inverse_product),
        _extend("exponent_two", "exponent_two+commutes",
                eq.RULE_LIBRARY["commutative"]),
    )


def _spec(name: str) -> Derived:
    return next(item for item in derived_theories() if item.name == name)


def load_jsonl(path: Path):
    with gzip.open(path, "rt") as stream:
        return [json.loads(line) for line in stream]


def run_row(spec: Derived, index: int, row: dict, attempts: int = 20):
    """Run exactly EQ-5's seeded stochastic grid under ``spec``."""
    goal = solver._row_goal(row)
    generators = sorted(solver._generator_names(goal.left) |
                        solver._generator_names(goal.right))
    size = row["needed_size"]
    initial = eq.start(spec.theory, goal, generators)
    zero = eq.entails(initial, goal)
    results = []
    first_solution = None
    for policy in ("uniform", "completing", "merging"):
        for multiplier in (1, 2, 4):
            budget = multiplier * size
            for attempt_index in range(attempts):
                seed = solver._stable_seed(20261112, index, policy,
                                           multiplier, attempt_index)
                attempt = solver.solve_attempt(spec.theory, goal, generators,
                                               policy, budget, seed)
                if attempt.solved and first_solution is None:
                    first_solution = [move.result for move in attempt.moves]
                results.append({"policy": policy, "multiplier": multiplier,
                                "budget": budget, "attempt": attempt_index,
                                "solved": attempt.solved,
                                "moves": len(attempt.moves)})

    proof = ([solver.parse_term(text) for text in row["needed_terms"]]
             if row.get("needed_terms") else first_solution)
    new_needed = 0 if zero else None
    if not zero and proof is not None:
        try:
            new_needed = len(setter.needed_terms(spec.theory, goal,
                                                  tuple(generators), proof))
        except AssertionError:
            # A recorded proof should always replay; a sampled solution can be
            # absent only when no attempt solved the named fixture.
            new_needed = None
    return {"derived": spec.name, "base": spec.base, "index": index,
            "row": row, "zero": zero, "new_needed": new_needed,
            "attempts": results}


def _worker(args):
    name, index, row, attempts = args
    return run_row(_spec(name), index, row, attempts)


def _rate(attempts):
    return sum(item["solved"] for item in attempts) / len(attempts)


def write_report(results, baseline, elapsed: float, note: Path):
    baseline = {item["index"]: item for item in baseline}
    lines = ["# EQ-6 — proved lemmas as closure rules", "",
             "The frozen EQ-5 rows, row indices, attempt seeds, and budgets are unchanged. Each new equation is an ordinary closure rule: it can merge present terms but cannot construct a term.", "",
             "## Solver rates", "",
             "Each cell is `derived (EQ-5 base)`.", "",
             "| derived theory | old needed | policy | budget | goals | mean solve rate |",
             "|---|---:|---|---:|---:|---:|"]
    keys = sorted({(r["derived"], min(r["row"]["needed_size"], 7),
                    a["policy"], a["multiplier"])
                   for r in results for a in r["attempts"]})
    for name, size, policy, multiplier in keys:
        selected = [r for r in results if r["derived"] == name and
                    min(r["row"]["needed_size"], 7) == size]
        derived_attempts = [a for r in selected for a in r["attempts"]
                            if a["policy"] == policy and
                            a["multiplier"] == multiplier]
        base_attempts = [a for r in selected
                         for a in baseline[r["index"]]["attempts"]
                         if a["policy"] == policy and
                         a["multiplier"] == multiplier]
        label = str(size) if size <= 6 else ">6"
        lines.append(f"| {name} | {label} | {policy} | {multiplier}n | {len(selected)} | {_rate(derived_attempts):.1%} ({_rate(base_attempts):.1%}) |")

    lines += ["", "## Needed-set shifts", "",
              "| derived theory | old → new | goals |",
              "|---|---:|---:|"]
    for name in sorted({r["derived"] for r in results}):
        counts = Counter((r["row"]["needed_size"], r["new_needed"])
                         for r in results if r["derived"] == name)
        for (old, new), count in sorted(counts.items(),
                                        key=lambda x: (x[0][0], 999 if x[0][1] is None else x[0][1])):
            lines.append(f"| {name} | {old} → {new if new is not None else 'unavailable'} | {count} |")

    lines += ["", "## Zero-move and named goals", "",
              "| derived theory | goals true at start | named goal | named result |",
              "|---|---:|---|---|"]
    for name in sorted({r["derived"] for r in results}):
        selected = [r for r in results if r["derived"] == name]
        named = [r for r in selected if r["row"].get("named")]
        named_text = "; ".join(
            f"{r['row']['named']}: {'0 moves' if r['zero'] else 'not initially true'}"
            for r in named) or "—"
        lines.append(f"| {name} | {sum(r['zero'] for r in selected)} | "
                     f"{', '.join(r['row']['named'] for r in named) or '—'} | {named_text} |")

    missing = sum(r["new_needed"] is None for r in results)
    lines += ["", "## Run receipt", "",
              f"- Wall time: {elapsed:.1f} seconds, using four worker processes.",
              f"- Rows evaluated: {len(results)}; needed sets unavailable: {missing}.",
              "- Nothing was cut and the run was not relaunched."]
    note.parent.mkdir(parents=True, exist_ok=True)
    note.write_text("\n".join(lines) + "\n")


def main():
    root = Path("/home/joe/code/storage/metagame/eq5")
    out = Path("/home/joe/code/storage/metagame/eq6")
    note = Path(__file__).parents[1] / "holes/excursions/E-metagame/EQ-6.md"
    rows = load_jsonl(root / "testset.jsonl.gz")
    baseline = load_jsonl(root / "results.jsonl.gz")
    jobs = [(spec.name, index, row, 20)
            for spec in derived_theories()
            for index, row in enumerate(rows) if row["theory"] == spec.base]
    started = time.monotonic()
    with concurrent.futures.ProcessPoolExecutor(max_workers=4) as pool:
        results = list(pool.map(_worker, jobs))
    elapsed = time.monotonic() - started
    out.mkdir(parents=True, exist_ok=True)
    with gzip.open(out / "results.jsonl.gz", "wt") as stream:
        for result in results:
            stream.write(json.dumps(result, sort_keys=True) + "\n")
    write_report(results, baseline, elapsed, note)


if __name__ == "__main__":
    main()
