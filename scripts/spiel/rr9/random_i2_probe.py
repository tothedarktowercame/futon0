#!/usr/bin/env python3
"""Uniform-legal-action random-play measurement for compact Euclid I.2."""

from __future__ import annotations

import argparse
import json
import math
import multiprocessing as mp
import random
import statistics
import sys
import time
from datetime import datetime, timezone
from collections import Counter
from pathlib import Path

SCRIPTS = Path(__file__).resolve().parents[2]
if str(SCRIPTS) not in sys.path:
    sys.path.insert(0, str(SCRIPTS))

import euclid_engine as eng  # noqa: E402
import euclid_play as play  # noqa: E402
import euclid_spiel  # noqa: E402,F401
import pyspiel  # noqa: E402

GAME = "euclid_book1(proposition=2,move_cap=10,dense_actions=true)"
DEFAULT_SEED = 9_202_611_110


def _grounded_json(move):
    rule, args, bindings = move
    return {"rule": rule, "args": list(args), "bindings": dict(bindings)}


def _grounded_tuple(move):
    return move["rule"], tuple(move["args"]), tuple(sorted(move.get("bindings", {}).items()))


def _student_moves(grounded):
    return [{"rule": move["rule"], "args": list(move["args"]),
             "bindings": dict(move.get("bindings", {}))} for move in grounded]


def replay_validate(grounded):
    """Replay through both independent public paths used by the game and page."""
    game = pyspiel.load_game(GAME)
    state = game.new_initial_state()
    engine_error = None
    for index, encoded in enumerate(grounded):
        wanted = _grounded_tuple(encoded)
        legal = state.legal_actions()
        matches = [action for action in legal if state._action_by_id[action] == wanted]
        if len(matches) != 1:
            engine_error = f"ply {index + 1}: grounded move has {len(matches)} legal matches"
            break
        state.apply_action(matches[0])
    engine_ok = engine_error is None and state.goal_reached() and state.returns() == [1.0]

    student = play.play(2, _student_moves(grounded))
    student_entries_ok = (len(student.get("moves", ())) == len(grounded)
                          and all(entry.get("ok") for entry in student.get("moves", ())))
    student_ok = bool(student.get("goal_reached") and not student.get("contradictory")
                      and student_entries_ok)
    return {
        "engine_ok": engine_ok,
        "engine_error": engine_error,
        "student_ok": student_ok,
        "student_goal_reached": bool(student.get("goal_reached")),
        "student_contradictory": bool(student.get("contradictory")),
        "student_entries_ok": student_entries_ok,
    }


def ground_specs(specs):
    """Resolve `(rule, args)` witnesses through the legal-action interface."""
    state = pyspiel.load_game(GAME).new_initial_state()
    out = []
    for index, (rule, args) in enumerate(specs):
        legal = state.legal_actions()
        matches = [action for action in legal
                   if state._action_by_id[action][0] == rule
                   and state._action_by_id[action][1] == tuple(args)]
        if len(matches) != 1:
            raise ValueError(f"ply {index + 1}: {(rule, args)!r} has {len(matches)} matches")
        grounded = state._action_by_id[matches[0]]
        out.append(_grounded_json(grounded))
        state.apply_action(matches[0])
    return out


def _canonical_sequence(grounded):
    """Canonicalize generated object names by first introduction."""
    given = {obj["name"] for obj in play.tr.objects_from_hyps(2)}
    names = {}
    next_name = 1
    result = []
    for move in grounded:
        for value in move.get("bindings", {}).values():
            if value not in given and value not in names:
                names[value] = f"v{next_name}"
                next_name += 1
        args = [names.get(arg, arg) for arg in move["args"]]
        bindings = [(base, names.get(value, value))
                    for base, value in sorted(move.get("bindings", {}).items())]
        result.append((move["rule"], tuple(args), tuple(bindings)))
    return tuple(result)


def _one_game(game, game_index, seed):
    rng = random.Random(seed)
    state = game.new_initial_state()
    branching = []
    action_strings = []
    grounded = []
    while not state.is_terminal():
        legal = state.legal_actions()
        branching.append(len(legal))
        if not legal:
            break
        action = rng.choice(legal)
        action_strings.append(state.action_to_string(0, action))
        grounded.append(_grounded_json(state._action_by_id[action]))
        state.apply_action(action)
    won = bool(state.goal_reached())
    record = {"game": game_index, "seed": seed, "length": len(grounded),
              "won": won, "branching": branching}
    if won:
        record["moves"] = action_strings
        record["grounded_moves"] = grounded
        record["validation"] = replay_validate(grounded)
    return record


def _worker(args):
    worker, indices, base_seed, out_dir = args
    game = pyspiel.load_game(GAME)
    path = Path(out_dir) / f"part-{worker:02d}.jsonl"
    with path.open("w", encoding="utf-8") as handle:
        for index in indices:
            record = _one_game(game, index, base_seed + index)
            handle.write(json.dumps(record, sort_keys=True, separators=(",", ":")) + "\n")
    return str(path)


def wilson(successes, trials, z=1.959963984540054):
    if trials == 0:
        return 0.0, 0.0
    p = successes / trials
    denominator = 1 + z * z / trials
    centre = (p + z * z / (2 * trials)) / denominator
    half = z * math.sqrt(p * (1 - p) / trials + z * z / (4 * trials * trials)) / denominator
    return centre - half, centre + half


def summarize(records, base_seed, workers):
    valid_wins = []
    invalid_wins = []
    by_ply = [[] for _ in range(10)]
    for record in records:
        for ply, count in enumerate(record["branching"]):
            by_ply[ply].append(count)
        if record["won"]:
            validation = record.get("validation", {})
            if validation.get("engine_ok") and validation.get("student_ok"):
                valid_wins.append(record)
            else:
                invalid_wins.append(record)
    low, high = wilson(len(valid_wins), len(records))
    canonical = {}
    for record in valid_wins:
        key = _canonical_sequence(record["grounded_moves"])
        entry = canonical.setdefault(repr(key), {"count": 0, "example_seed": record["seed"],
                                                  "moves": record["moves"]})
        entry["count"] += 1
    branch_table = []
    for ply, values in enumerate(by_ply, 1):
        if values:
            branch_table.append({"ply": ply, "samples": len(values),
                                 "mean": statistics.fmean(values),
                                 "median": statistics.median(values), "max": max(values)})
    return {
        "game": GAME, "games": len(records), "workers": workers,
        "base_seed": base_seed, "seed_rule": "seed = base_seed + game_index",
        "raw_wins": sum(record["won"] for record in records),
        "validated_wins": len(valid_wins), "invalid_wins": invalid_wins,
        "rate": len(valid_wins) / len(records) if records else 0.0,
        "wilson_95": [low, high],
        "wins_by_length": dict(sorted(Counter(r["length"] for r in valid_wins).items())),
        "distinct_winning_sequences": list(canonical.values()),
        "branching_by_ply": branch_table,
    }


def run(games, workers, base_seed, out_dir):
    started = datetime.now(timezone.utc).isoformat()
    start_clock = time.monotonic()
    out_dir.mkdir(parents=True, exist_ok=True)
    jobs = [(worker, list(range(worker, games, workers)), base_seed, str(out_dir))
            for worker in range(workers)]
    context = mp.get_context("spawn")
    pool = context.Pool(workers)
    try:
        parts = pool.map(_worker, jobs)
        pool.close()
        pool.join()
    except BaseException:
        pool.terminate()  # only this driver's own recorded worker pool
        pool.join()
        raise
    records = []
    for part in parts:
        with Path(part).open(encoding="utf-8") as handle:
            records.extend(json.loads(line) for line in handle if line.strip())
        Path(part).unlink()
    records.sort(key=lambda record: record["game"])
    raw = out_dir / "games.jsonl"
    with raw.open("w", encoding="utf-8") as handle:
        for record in records:
            handle.write(json.dumps(record, sort_keys=True, separators=(",", ":")) + "\n")
    summary = summarize(records, base_seed, workers)
    summary["started_utc"] = started
    summary["elapsed_seconds"] = time.monotonic() - start_clock
    summary["games_per_second"] = (games / summary["elapsed_seconds"]
                                   if summary["elapsed_seconds"] else 0.0)
    (out_dir / "summary.json").write_text(json.dumps(summary, indent=2, sort_keys=True) + "\n",
                                           encoding="utf-8")
    return summary


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--games", type=int, default=100_000)
    parser.add_argument("--workers", type=int, default=12)
    parser.add_argument("--seed", type=int, default=DEFAULT_SEED)
    parser.add_argument("--out", type=Path, required=True)
    args = parser.parse_args()
    print(json.dumps(run(args.games, args.workers, args.seed, args.out), indent=2, sort_keys=True))


if __name__ == "__main__":
    main()
