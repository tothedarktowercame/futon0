#!/usr/bin/env python3
"""Reverse start-state curriculum for Probe 7's I.2 demonstration arm."""

from __future__ import annotations

import argparse
import json
from pathlib import Path
import signal
import subprocess
import sys
import time

import pyspiel

import euclid_spiel  # registers the game


WITNESS = [
    ("line_from_points", ("a", "b")),
    ("proposition_1", ("a", "b", "L_1")),
    ("line_from_points", ("d_2", "a")),
    ("line_from_points", ("d_2", "b")),
    ("circle_from_points", ("b", "c")),
    ("intersection_circle_line_extending_points", ("C_5", "L_4", "b", "d_2")),
    ("circle_from_points", ("d_2", "g_6")),
    ("intersection_circle_line_extending_points", ("C_7", "L_3", "a", "d_2")),
]


def grounded_witness():
    state = pyspiel.load_game(
        "euclid_book1(proposition=2,move_cap=10,dense_actions=true)"
    ).new_initial_state()
    moves = []
    for rule, args in WITNESS:
        state.legal_actions()
        matches = [move for move in state._action_by_id.values()
                   if move[0] == rule and move[1] == args]
        if len(matches) != 1:
            raise RuntimeError((rule, args, matches))
        moves.append(matches[0])
        state.apply_action_tuple(matches[0])
    if state.returns()[0] != 1:
        raise RuntimeError("demonstration does not reach I.2")
    return moves


def orderly_run(command, timeout, **kwargs):
    """Run one packaged-AZ phase, giving its SIGTERM handler time to join workers."""
    process = subprocess.Popen(command, **kwargs)
    try:
        stdout, stderr = process.communicate(timeout=timeout)
    except subprocess.TimeoutExpired:
        process.send_signal(signal.SIGTERM)
        try:
            stdout, stderr = process.communicate(timeout=60)
        except subprocess.TimeoutExpired:
            process.kill()
            stdout, stderr = process.communicate()
        raise TimeoutError(f"phase exceeded {timeout} seconds")
    if process.returncode:
        raise subprocess.CalledProcessError(process.returncode, command,
                                            output=stdout, stderr=stderr)
    return subprocess.CompletedProcess(command, process.returncode, stdout, stderr)


def load_solution(path):
    if path is None:
        return grounded_witness()
    records = [json.loads(line) for line in path.read_text().splitlines() if line]
    if not records:
        raise RuntimeError(f"no solutions in {path}")
    record = min(records, key=lambda candidate: len(candidate["moves"]))
    moves = [(rule, tuple(move_args), tuple(tuple(binding) for binding in bindings))
             for rule, move_args, bindings in record["moves"]]
    state = pyspiel.load_game(
        "euclid_book1(proposition=2,move_cap=10,dense_actions=true)"
    ).new_initial_state()
    for move in moves:
        state.apply_action_tuple(move)
    if not state.goal_reached():
        raise RuntimeError(f"solution from {path} does not reach I.2")
    return moves


def run(args):
    args.out.mkdir(parents=True, exist_ok=True)
    moves = load_solution(args.solution_file)
    (args.out / "demonstration.json").write_text(json.dumps(moves, indent=2) + "\n")
    records = []
    previous = None
    runner = Path(__file__).with_name("euclid_open_spiel_alpha_zero.py")
    python = sys.executable
    for k in range(1, len(moves) + 1):
        stage = args.out / f"stage-{k}"
        stage.mkdir(exist_ok=True)
        start = stage / "start.json"
        start.write_text(json.dumps(moves[:-k], indent=2) + "\n")
        train = [python, str(runner), "--proposition", "2", "--out", str(stage),
                 "--steps", str(args.steps), "--actors", str(args.actors),
                 "--simulations", str(args.simulations), "--move-cap", str(k + 2),
                 "--start-file", str(start)]
        if previous is not None:
            train += ["--resume-from", str(previous),
                      "--resume-step", str(args.steps)]
        began = time.monotonic()
        orderly_run(train, args.stage_timeout * 60)
        train_seconds = time.monotonic() - began
        evaluate = train[:]
        evaluate[train.index("--steps") + 1] = str(args.steps)
        evaluate += ["--eval-step", str(args.steps), "--eval-games", "10",
                     "--eval-game-seconds", str(args.eval_game_seconds)]
        result = orderly_run(evaluate, args.stage_timeout * 60,
                             stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                             text=True)
        report = json.loads(result.stdout.strip().splitlines()[-1])
        record = {"k": k, "prefix_length": len(moves) - k,
                  "train_seconds": train_seconds, **report}
        records.append(record)
        (args.out / "stages.json").write_text(json.dumps(records, indent=2) + "\n")
        print(json.dumps(record), flush=True)
        if report["wins"] < args.threshold:
            break
        previous = stage


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--out", type=Path, required=True)
    parser.add_argument("--actors", type=int, default=1)
    parser.add_argument("--simulations", type=int, default=16)
    parser.add_argument("--steps", type=int, default=5)
    parser.add_argument("--threshold", type=int, default=8)
    parser.add_argument("--stage-timeout", type=int, default=8)
    parser.add_argument("--eval-game-seconds", type=int, default=20)
    parser.add_argument("--solution-file", type=Path)
    run(parser.parse_args())


if __name__ == "__main__":
    main()
