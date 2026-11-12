#!/usr/bin/env python3
"""ZERO hindsight-theorem generator for Euclid I.1--I.3 givens."""
from __future__ import annotations

import argparse
import itertools
import json
import multiprocessing as mp
import os
from pathlib import Path
import random
import time

import euclid_engine as eng
import euclid_play as play
import euclid_spiel as spiel


def initial_state(proposition):
    state = play._start_state(proposition)
    kinds = {o["name"]: o["kind"]
             for o in play.tr.objects_from_hyps(proposition)}
    return state, kinds


def notation(action):
    rule, args, bindings = action
    made = " ".join(f"{base}={name}" for base, name in bindings)
    body = f"{rule}({','.join(args)})"
    return body + (f" [{made}]" if made else "")


def choose_action(legal, rng, sampling="rule-uniform"):
    if sampling == "move-uniform":
        return legal[rng.randrange(len(legal))]
    if sampling != "rule-uniform":
        raise ValueError(f"unknown sampling distribution: {sampling}")
    by_rule = {}
    for action in legal:
        by_rule.setdefault(action[0], []).append(action)
    rules = sorted(by_rule)
    rule = rules[rng.randrange(len(rules))]
    groundings = by_rule[rule]
    return groundings[rng.randrange(len(groundings))]


def play_random(proposition, seed, plies=6, deadline=None,
                sampling="rule-uniform"):
    rng = random.Random(seed)
    state, kinds = initial_state(proposition)
    start_facts = set(state.facts)
    start_declared = set(state.declared)
    moves, snapshots = [], []
    closure_seconds = 0.0
    for ply in range(plies):
        if deadline is not None and time.monotonic() >= deadline:
            break
        t0 = time.perf_counter()
        legal = spiel.enumerate_actions(proposition, state, kinds, ply,
                                        validate=True)
        closure_seconds += time.perf_counter() - t0
        if not legal:
            break
        action = choose_action(legal, rng, sampling)
        before = set(state.facts)
        successor, refusal = spiel.admit_candidate(state, ply, action)
        if refusal is not None:  # guarded enumeration promises this cannot occur
            raise AssertionError(f"guarded candidate refused: {action}: {refusal}")
        state = successor
        moves.append(action)
        snapshots.append(set(state.facts) - before)
    made = set(state.declared) - start_declared
    return state, kinds, start_facts, made, moves, snapshots, closure_seconds


def _mentions(fact, names):
    return any(isinstance(x, str) and x in names for x in fact[1:])


def metric_goals(state, made):
    """Bound: comparisons among live length and angle atoms, no invented atoms.

    At most 3*C(L,2)+3*C(A,2), where L/A are distinct atoms occurring in
    the exact arithmetic rows.  Trivial comparisons of an atom with itself
    are deliberately absent.
    """
    # LinearArithmetic stores normalized rows as ((atom, coefficient), ...).
    atoms = {atom for coeffs, _const, _op in state.la.rows
             for atom, _coefficient in coeffs}
    lengths = sorted((a for a in atoms if a and a[0] == "len"), key=repr)
    angles = sorted((a for a in atoms if a and a[0] == "ang"), key=repr)
    out = []
    for one, two in itertools.combinations(lengths, 2):
        if not (_mentions(one, made) or _mentions(two, made)):
            continue
        args = one[1:] + two[1:]
        for kind in ("_eq_len", "_lt_len", "_gt_len"):
            fact = (kind,) + args
            if eng._fact_holds(state, fact):
                out.append(fact)
    for one, two in itertools.combinations(angles, 2):
        if not (_mentions(one, made) or _mentions(two, made)):
            continue
        args = one[1:] + two[1:]
        candidates = (("_eq_ang",) + args,
                      ("_gt_ang",) + args,
                      ("_gt_ang",) + two[1:] + one[1:])
        for fact in candidates:
            if eng._fact_holds(state, fact):
                out.append(fact)
    return out, {"length_atoms": len(lengths), "angle_atoms": len(angles),
                 "candidate_bound": 3 * (len(lengths) * (len(lengths)-1) // 2
                                           + len(angles) * (len(angles)-1) // 2)}


def replay(proposition, moves, keep=None):
    state, _kinds = initial_state(proposition)
    chosen = range(len(moves)) if keep is None else sorted(keep)
    for index in chosen:
        rule, args, bindings = moves[index]
        try:
            eng.apply_step(state, index + 1, rule, args, dict(bindings))
        except (eng.MissingPremise, TypeError, ValueError):
            return None
    return state


def goal_holds(state, goal):
    return state is not None and eng._fact_holds(state, goal)


def minimise_needed(proposition, moves, goal, initial=None):
    keep = set(range(len(moves))) if initial is None else set(initial)
    changed = True
    while changed:
        changed = False
        for index in sorted(keep):
            trial = keep - {index}
            if goal_holds(replay(proposition, moves, trial), goal):
                keep = trial
                changed = True
    return tuple(sorted(keep))


def provenance_needed(proposition, moves, snapshots, final_state, goal):
    """Trace State.facts premises to introducing moves and object creators."""
    introduced = {}
    for index, facts in enumerate(snapshots):
        for fact in facts:
            introduced.setdefault(fact, index)
    creators = {}
    for index, (_rule, _args, bindings) in enumerate(moves):
        for _base, name in bindings:
            creators[name] = index
    needed, todo, seen = set(), [goal], set()
    while todo:
        fact = todo.pop()
        if fact in seen:
            continue
        seen.add(fact)
        index = introduced.get(fact)
        if index is not None:
            needed.add(index)
        for value in fact[1:]:
            if isinstance(value, str) and value in creators:
                needed.add(creators[value])
        provenance = final_state.facts.get(fact)
        if provenance:
            todo.extend(provenance[1])
    # A construction may use an earlier made object without that dependency
    # occurring in the final fact's recorded premise chain.
    changed = True
    while changed:
        changed = False
        for index in tuple(needed):
            for value in moves[index][1]:
                creator = creators.get(value)
                if creator is not None and creator not in needed:
                    needed.add(creator)
                    changed = True
    if not goal_holds(replay(proposition, moves, needed), goal):
        # Provenance is authoritative for facts, but forward closure can omit
        # object dependencies.  Complete it by exact deletion replay.
        needed = set(minimise_needed(proposition, moves, goal))
    return minimise_needed(proposition, moves, goal, needed)


def _rename(value, names):
    if isinstance(value, str):
        return names.get(value, value)
    if isinstance(value, (tuple, list)):
        return [_rename(v, names) for v in value]
    return value


def canonical_key(goal, moves, needed):
    names = {}
    for index in needed:
        _rule, _args, bindings = moves[index]
        for _base, name in bindings:
            names.setdefault(name, f"made{len(names)}")
    payload = {"goal": _rename(goal, names),
               "moves": [[moves[i][0], _rename(moves[i][1], names),
                           _rename(moves[i][2], names)] for i in needed]}
    return json.dumps(payload, sort_keys=True, separators=(",", ":"))


def records_for_run(proposition, seed, plies=6, deadline=None,
                    sampling="rule-uniform"):
    t0 = time.perf_counter()
    state, kinds, start_facts, made, moves, snapshots, closure = play_random(
        proposition, seed, plies, deadline, sampling)
    if len(moves) != plies:
        return [], {"runs": 0, "records": 0,
                    "closure_seconds": closure, "needed_seconds": 0.0,
                    "wall_seconds": time.perf_counter() - t0}
    goals = [f for f in state.facts
             if f not in start_facts and _mentions(f, made)]
    metrics, bound = metric_goals(state, made)
    goals.extend(metrics)
    records = []
    needed_seconds = 0.0
    for goal in goals:
        if deadline is not None and time.monotonic() >= deadline:
            break
        metric = goal[0].startswith("_")
        n0 = time.perf_counter()
        if metric:
            needed = minimise_needed(proposition, moves, goal)
            method = "deletion-replay"
        else:
            needed = provenance_needed(proposition, moves, snapshots, state, goal)
            method = "fact-provenance"
        needed_seconds += time.perf_counter() - n0
        if len(needed) < 2:
            continue
        records.append({
            "proposition": proposition,
            "givens": sorted(start_facts, key=repr),
            "run_seed": seed,
            "sampling": sampling,
            "moves": [{"notation": notation(a), "tuple": [a[0], list(a[1]), dict(a[2])]}
                      for a in moves],
            "goal": list(goal), "metric": metric,
            "needed_move_indices": list(needed), "method": method,
            "needed_size": len(needed),
            "canonical_key": canonical_key(goal, moves, needed),
            "metric_bound": bound,
        })
    return records, {"runs": 1, "records": len(records),
                     "closure_seconds": closure,
                     "needed_seconds": needed_seconds,
                     "wall_seconds": time.perf_counter() - t0}


def verify_record(record):
    moves = [(m["tuple"][0], tuple(m["tuple"][1]),
              tuple(sorted(m["tuple"][2].items()))) for m in record["moves"]]
    state = replay(record["proposition"], moves, record["needed_move_indices"])
    return goal_holds(state, tuple(record["goal"]))


def _worker(worker, out_dir, duration, seed, sampling):
    path = Path(out_dir) / f"worker-{worker:02d}.jsonl"
    stats_path = Path(out_dir) / f"worker-{worker:02d}-stats.json"
    deadline = time.monotonic() + duration
    stats = {"runs": 0, "records": 0, "closure_seconds": 0.0,
             "needed_seconds": 0.0, "wall_seconds": 0.0}
    with path.open("a") as stream:
        run = 0
        while time.monotonic() < deadline:
            proposition = 1 + ((worker + run) % 3)
            records, one = records_for_run(
                proposition, seed + worker * 10_000_000 + run,
                deadline=deadline, sampling=sampling)
            for record in records:
                stream.write(json.dumps(record, separators=(",", ":")) + "\n")
            stream.flush()
            for key in stats:
                stats[key] += one[key]
            run += 1
    stats_path.write_text(json.dumps(stats, indent=2) + "\n")


def run_parallel(out_dir, duration, workers, seed, sampling="rule-uniform"):
    out = Path(out_dir)
    out.mkdir(parents=True, exist_ok=True)
    ctx = mp.get_context("spawn")
    processes = [ctx.Process(target=_worker,
                             args=(i, out, duration, seed, sampling))
                 for i in range(workers)]
    for process in processes:
        process.start()
    for process in processes:
        process.join()
    if any(process.exitcode != 0 for process in processes):
        raise SystemExit("one or more hindsight workers failed")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--out", required=True)
    parser.add_argument("--duration", type=int, default=1800)
    parser.add_argument("--workers", type=int, default=12)
    parser.add_argument("--seed", type=int, default=2026111201)
    parser.add_argument("--sampling", choices=("move-uniform", "rule-uniform"),
                        default="rule-uniform")
    args = parser.parse_args()
    run_parallel(args.out, args.duration, args.workers, args.seed, args.sampling)


if __name__ == "__main__":
    main()
