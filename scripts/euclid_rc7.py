#!/usr/bin/env python3
"""Probe-7 random-rate and reverse-curriculum utilities."""

from __future__ import annotations

import argparse
import json
import math
import multiprocessing as mp
from pathlib import Path
import random
import time

import pyspiel

import euclid_spiel  # registers the game


def _random_worker(args):
    worker, seed, deadline, quota = args
    rng = random.Random(seed + worker)
    game = pyspiel.load_game(
        "euclid_book1(proposition=2,move_cap=10,dense_actions=true)")
    games = 0
    wins = []
    while games < quota and time.monotonic() < deadline:
        state = game.new_initial_state()
        while not state.is_terminal():
            state.apply_action_tuple(state.sample_action_tuple(rng))
        games += 1
        if state.returns()[0] > 0:
            wins.append({"worker": worker, "game": games,
                         "moves": state.history_moves})
    return games, wins


def random_rate(out, workers=6, games=100_000, minutes=30, seed=711):
    started = time.monotonic()
    deadline = started + minutes * 60
    quota = math.ceil(games / workers)
    with mp.get_context("spawn").Pool(workers) as pool:
        results = pool.map(_random_worker,
                           [(i, seed, deadline, quota) for i in range(workers)])
    total = sum(result[0] for result in results)
    wins = [win for result in results for win in result[1]]
    wins = wins[:games]
    total = min(total, games)
    out.mkdir(parents=True, exist_ok=True)
    with (out / "random-wins.jsonl").open("w") as stream:
        for win in wins:
            stream.write(json.dumps(win) + "\n")
    minimized = []
    for win in wins:
        moves = minimize(win["moves"])
        if not _reaches_goal(moves):
            raise RuntimeError("minimized winning trace failed fresh replay")
        minimized.append({**win, "original_length": len(win["moves"]),
                          "minimized_length": len(moves), "moves": moves})
    with (out / "minimized-wins.jsonl").open("w") as stream:
        for win in minimized:
            stream.write(json.dumps(win) + "\n")
    summary = {"games": total, "wins": len(wins),
               "elapsed_seconds": time.monotonic() - started,
               "workers": workers, "seed": seed}
    (out / "random-summary.json").write_text(json.dumps(summary, indent=2) + "\n")
    print(json.dumps(summary))


def _reaches_goal(moves):
    game = pyspiel.load_game(
        f"euclid_book1(proposition=2,move_cap={max(10, len(moves) + 1)},"
        "dense_actions=true)")
    state = game.new_initial_state()
    try:
        for rule, args, bindings in moves:
            state.apply_action_tuple(
                (rule, tuple(args), tuple((k, v) for k, v in bindings)))
    except Exception:
        return False
    return state.goal_reached()


def minimize(moves):
    """Deletion-minimise a winning trace by exact replay."""
    moves = list(moves)
    changed = True
    while changed:
        changed = False
        for index in range(len(moves)):
            candidate = moves[:index] + moves[index + 1:]
            if _reaches_goal(candidate):
                moves = candidate
                changed = True
                break
    return moves


def minimize_log(out):
    wins_file = out / "random-wins.jsonl"
    wins = [json.loads(line) for line in wins_file.read_text().splitlines() if line]
    minimized = []
    for win in wins:
        moves = minimize(win["moves"])
        if not _reaches_goal(moves):
            raise RuntimeError("minimized winning trace failed fresh replay")
        minimized.append({**win, "original_length": len(win["moves"]),
                          "minimized_length": len(moves), "moves": moves})
    with (out / "minimized-wins.jsonl").open("w") as stream:
        for win in minimized:
            stream.write(json.dumps(win) + "\n")
    print(json.dumps({"wins": len(wins),
                      "minimized_lengths": [w["minimized_length"]
                                            for w in minimized]}))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--out", type=Path, required=True)
    parser.add_argument("--workers", type=int, default=6)
    parser.add_argument("--games", type=int, default=100_000)
    parser.add_argument("--minutes", type=int, default=30)
    parser.add_argument("--seed", type=int, default=711)
    parser.add_argument("--minimize-only", action="store_true")
    args = parser.parse_args()
    if args.minimize_only:
        minimize_log(args.out)
    else:
        random_rate(args.out, args.workers, args.games, args.minutes, args.seed)


if __name__ == "__main__":
    main()
