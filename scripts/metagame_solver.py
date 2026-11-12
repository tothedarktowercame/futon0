#!/usr/bin/env python3
"""Non-learning solver baselines for equational metagame goals."""

from __future__ import annotations

import argparse
import concurrent.futures
import gzip
import hashlib
import json
import random
import signal
import statistics
from dataclasses import dataclass
from pathlib import Path

import metagame_eq as eq
import metagame_setter as setter


@dataclass
class Attempt:
    solved: bool
    moves: list[eq.Move]


def parse_term(text: str) -> eq.Term:
    index = 0

    def read():
        nonlocal index
        if text.startswith("e", index) and (index + 1 == len(text) or
                                             text[index + 1] in ",)"):
            index += 1
            return eq.E
        start = index
        while index < len(text) and (text[index].isalnum() or text[index] == "_"):
            index += 1
        name = text[start:index]
        if index == len(text) or text[index] in ",)":
            return eq.gen(name)
        if text[index] != "(":
            raise ValueError(text)
        index += 1
        args = []
        if text[index] != ")":
            while True:
                args.append(read())
                if text[index] == ")":
                    break
                if text[index] != ",":
                    raise ValueError(text)
                index += 1
        index += 1
        return eq.Term(name, tuple(args))

    result = read()
    if index != len(text):
        raise ValueError(text)
    return result


def _generator_names(term: eq.Term):
    if term.op == "gen":
        return {str(term.args[0])}
    return set().union(*(_generator_names(arg) for arg in term.args), set())


def _stable_seed(*parts):
    digest = hashlib.sha256("|".join(map(str, parts)).encode()).digest()
    return int.from_bytes(digest[:8], "big")


def _demo_rows():
    a, b = eq.gen("a"), eq.gen("b")
    return [
        {"theory": "exponent_two", "goal": [str(eq.mul(a, b)), str(eq.mul(b, a))],
         "needed_size": 8, "seed": 0, "named": "exponent_two_commutes"},
        {"theory": "group",
         "goal": [str(eq.inv(eq.mul(a, b))), str(eq.mul(eq.inv(b), eq.inv(a)))],
         "needed_size": 8, "seed": 0, "named": "inverse_product"},
        {"theory": "left_group", "goal": [str(eq.mul(a, eq.inv(a))), "e"],
         "needed_size": 6, "seed": 0, "named": "left_group_right_inverse"},
    ]


def build_testset(root: Path, seed=20261112, per_bin=60):
    dedup = {}
    for path in sorted(root.glob("*/*.jsonl.gz")):
        with gzip.open(path, "rt") as stream:
            for line in stream:
                row = json.loads(line)
                goal = eq.Goal(*(parse_term(x) for x in row["goal"]))
                key = (row["theory"], setter.canonical_goal(goal))
                old = dedup.get(key)
                if old is None or row["needed_size"] < old["needed_size"]:
                    dedup[key] = row
    bins = {}
    for row in dedup.values():
        size = row["needed_size"] if row["needed_size"] <= 6 else 7
        bins.setdefault((row["theory"], size), []).append(row)
    chosen = []
    for key in sorted(bins):
        values = sorted(bins[key], key=lambda row: (row["seed"], row["goal"]))
        random.Random(_stable_seed(seed, *key)).shuffle(values)
        chosen.extend(values[:per_bin])
    chosen.extend(_demo_rows())
    return chosen


def write_testset(rows, path: Path):
    path.parent.mkdir(parents=True, exist_ok=True)
    with gzip.open(path, "wt") as stream:
        for row in rows:
            stream.write(json.dumps(row, sort_keys=True) + "\n")


def _row_goal(row):
    return eq.Goal(*(parse_term(x) for x in row["goal"]))


def solve_attempt(theory: str | eq.Theory, goal: eq.Goal, generators,
                  policy: str, budget: int, seed: int):
    p = eq.start(theory, goal, generators)
    if eq.entails(p, goal):
        return Attempt(True, [])
    rng = random.Random(seed)
    moves = []
    policy_name = {"completing": "completing(1)",
                   "merging": "merging(1)"}.get(policy, policy)
    for _ in range(budget):
        move = setter.choose_move(p, rng, policy_name)
        if move is None:
            break
        eq.apply(p, move)
        moves.append(move)
        if eq.entails(p, goal):
            return Attempt(True, moves)
    return Attempt(False, moves)


class _TimedOut(Exception):
    pass


def _alarm(_signum, _frame):
    raise _TimedOut


def exhaustive(theory, goal, size, timeout=2.0):
    old = signal.signal(signal.SIGALRM, _alarm)
    signal.setitimer(signal.ITIMER_REAL, timeout)
    try:
        answer = eq.shortest_proof(theory, goal, max_moves=size)
        return {"solved": answer is not None,
                "moves": answer[0] if answer else None, "timeout": False}
    except _TimedOut:
        return {"solved": False, "moves": None, "timeout": True}
    finally:
        signal.setitimer(signal.ITIMER_REAL, 0)
        signal.signal(signal.SIGALRM, old)


def _solve_row(args):
    index, row, attempts = args
    goal = _row_goal(row)
    generators = sorted(_generator_names(goal.left) | _generator_names(goal.right))
    size = row["needed_size"]
    results = []
    for policy in ("uniform", "completing", "merging"):
        for multiplier in (1, 2, 4):
            budget = multiplier * size
            for attempt_index in range(attempts):
                seed = _stable_seed(20261112, index, policy, multiplier,
                                    attempt_index)
                result = solve_attempt(row["theory"], goal, generators, policy,
                                       budget, seed)
                results.append({"policy": policy, "multiplier": multiplier,
                                "budget": budget, "attempt": attempt_index,
                                "solved": result.solved,
                                "moves": len(result.moves)})
    exact = exhaustive(row["theory"], goal, size)
    return {"index": index, "row": row, "attempts": results,
            "exhaustive": exact}


def _size_label(size):
    return str(size) if size <= 6 else ">6"


def write_report(results, note: Path, attempts: int):
    stochastic = []
    for result in results:
        row = result["row"]
        if row.get("named"):
            continue
        for attempt in result["attempts"]:
            stochastic.append({**attempt, "theory": row["theory"],
                               "size": min(row["needed_size"], 7),
                               "goal_index": result["index"]})
    lines = ["# EQ-5 — non-learning solver baselines", "",
             f"The frozen test set contains {len(results)} goals. Stochastic policies received {attempts} seeded attempts per goal and budget; exhaustive search had a two-second wall limit per goal.", "",
             "## Stochastic policies", "",
             "| theory | needed | policy | budget | goals | mean solve rate | median solved moves |",
             "|---|---:|---|---:|---:|---:|---:|"]
    group_keys = sorted({(x["theory"], x["size"], x["policy"], x["multiplier"])
                         for x in stochastic})
    for theory, size, policy, multiplier in group_keys:
        rows = [x for x in stochastic if (x["theory"], x["size"], x["policy"],
                                           x["multiplier"]) ==
                (theory, size, policy, multiplier)]
        goals = len({x["goal_index"] for x in rows})
        solved_moves = [x["moves"] for x in rows if x["solved"]]
        median = statistics.median(solved_moves) if solved_moves else "—"
        lines.append(f"| {theory} | {_size_label(size)} | {policy} | {multiplier}n | {goals} | {sum(x['solved'] for x in rows)/len(rows):.1%} | {median} |")

    lines += ["", "## Goal variability at budget 2n", "",
              "| theory | policy | every attempt | no attempt | some attempts |",
              "|---|---|---:|---:|---:|"]
    for theory, policy in sorted({(x["theory"], x["policy"]) for x in stochastic}):
        rows = [x for x in stochastic if x["theory"] == theory and
                x["policy"] == policy and x["multiplier"] == 2]
        rates = {}
        for row in rows:
            rates.setdefault(row["goal_index"], []).append(row["solved"])
        total = len(rates)
        every = sum(all(xs) for xs in rates.values())
        none = sum(not any(xs) for xs in rates.values())
        some = total - every - none
        lines.append(f"| {theory} | {policy} | {every/total:.1%} | {none/total:.1%} | {some/total:.1%} |")

    lines += ["", "## Exhaustive search", "",
              "| needed | goals | solved | timed out | shorter than recorded |",
              "|---:|---:|---:|---:|---:|"]
    ordinary = [r for r in results if not r["row"].get("named")]
    shorter = []
    for size in sorted({min(r["row"]["needed_size"], 7) for r in ordinary}):
        rows = [r for r in ordinary if min(r["row"]["needed_size"], 7) == size]
        short = [r for r in rows if r["exhaustive"]["solved"] and
                 r["exhaustive"]["moves"] < size]
        shorter.extend(short)
        lines.append(f"| {_size_label(size)} | {len(rows)} | {sum(r['exhaustive']['solved'] for r in rows)/len(rows):.1%} | {sum(r['exhaustive']['timeout'] for r in rows)} | {len(short)} |")
    lines += ["", "Shorter cases (up to 20):"]
    for result in shorter[:20]:
        lines.append(f"- `{result['row']['goal'][0]} = {result['row']['goal'][1]}`: recorded {result['row']['needed_size']}, exhaustive {result['exhaustive']['moves']}.")

    lines += ["", "## Named goals", "",
              "| goal | policy | n | 2n | 4n |", "|---|---|---:|---:|---:|"]
    for result in [r for r in results if r["row"].get("named")]:
        for policy in ("uniform", "completing", "merging"):
            rates = []
            for multiplier in (1, 2, 4):
                rows = [x for x in result["attempts"] if x["policy"] == policy and
                        x["multiplier"] == multiplier]
                rates.append(sum(x["solved"] for x in rows) / len(rows))
            lines.append(f"| {result['row']['named']} | {policy} | " +
                         " | ".join(f"{rate:.1%}" for rate in rates) + " |")
        exact = result["exhaustive"]
        exact_text = ("timeout" if exact["timeout"] else
                      ("solved" if exact["solved"] else "not found"))
        lines.append(f"| {result['row']['named']} | exhaustive | {exact_text} | — | — |")

    lines += ["", "## Fifty-percent frontier at budget 2n", "",
              "| theory | policy | largest needed size with mean solve rate >= 50% |",
              "|---|---|---:|"]
    for theory, policy in sorted({(x["theory"], x["policy"]) for x in stochastic}):
        passing = []
        for size in sorted({x["size"] for x in stochastic if x["theory"] == theory}):
            rows = [x for x in stochastic if x["theory"] == theory and
                    x["policy"] == policy and x["size"] == size and
                    x["multiplier"] == 2]
            if rows and sum(x["solved"] for x in rows) / len(rows) >= .5:
                passing.append(size)
        lines.append(f"| {theory} | {policy} | {_size_label(max(passing)) if passing else 'none'} |")
    note.parent.mkdir(parents=True, exist_ok=True)
    note.write_text("\n".join(lines) + "\n")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", type=Path,
                        default=Path("/home/joe/code/storage/metagame/eq4"))
    parser.add_argument("--out", type=Path,
                        default=Path("/home/joe/code/storage/metagame/eq5"))
    parser.add_argument("--note", type=Path,
                        default=Path(__file__).parents[1] / "holes/excursions/E-metagame/EQ-5.md")
    parser.add_argument("--attempts", type=int, default=20)
    parser.add_argument("--workers", type=int, default=6)
    parser.add_argument("--per-bin", type=int, default=60)
    args = parser.parse_args()
    rows = build_testset(args.root, per_bin=args.per_bin)
    write_testset(rows, args.out / "testset.jsonl.gz")
    jobs = [(i, row, args.attempts) for i, row in enumerate(rows)]
    with concurrent.futures.ProcessPoolExecutor(max_workers=args.workers) as pool:
        results = list(pool.map(_solve_row, jobs))
    args.out.mkdir(parents=True, exist_ok=True)
    with gzip.open(args.out / "results.jsonl.gz", "wt") as stream:
        for result in results:
            stream.write(json.dumps(result, sort_keys=True) + "\n")
    write_report(results, args.note, args.attempts)


if __name__ == "__main__":
    main()
