#!/usr/bin/env python3
"""Funded DEMO backward curriculum for Probe 10."""

from __future__ import annotations

import json
import os
from pathlib import Path
import re
import subprocess
import time

import pyspiel

import euclid_spiel  # noqa: F401


ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "holes/excursions/E-euclid-tutor-data/spiel/rc10"
PYTHON = Path("/home/joe/.venvs/openspiel/bin/python")
CPU_PREFIX = ["taskset", "-c", "20-31"]
RUNNER = ROOT / "scripts/euclid_open_spiel_alpha_zero.py"
SOURCE_DEMO = (ROOT / "holes/excursions/E-euclid-tutor-data/spiel/rc7/"
               "demo2/demonstration.json")
HASH_SEED = "0"


def run_command(command, log_path, timeout, env):
    started = time.monotonic()
    with log_path.open("w") as log:
        process = subprocess.Popen(command, stdout=log, stderr=subprocess.STDOUT,
                                   env=env)
        try:
            returncode = process.wait(timeout=timeout)
        except subprocess.TimeoutExpired:
            process.terminate()
            try:
                returncode = process.wait(timeout=30)
            except subprocess.TimeoutExpired:
                process.kill()
                returncode = process.wait()
    return returncode, time.monotonic() - started


def latest_checkpoint(stage_dir):
    choices = []
    for path in stage_dir.glob("checkpoint-*"):
        try:
            choices.append((int(path.name.split("-")[-1]), path))
        except ValueError:
            pass
    if not choices:
        raise RuntimeError(f"no checkpoint in {stage_dir}")
    return max(choices)[0]


def evaluation(stage_dir, checkpoint, prefix, move_cap, perturb, seed, label,
               env, timeout):
    record = stage_dir / f"eval-{label}-wins.jsonl"
    command = CPU_PREFIX + [str(PYTHON), str(RUNNER), "--proposition", "2",
               "--out", str(stage_dir), "--simulations", "32",
               "--eval-step", str(checkpoint), "--eval-games", "10",
               "--eval-game-seconds", "30", "--start-file", str(prefix),
               "--move-cap", str(move_cap), "--start-perturb", str(perturb),
               "--start-seed", str(seed), "--eval-record", str(record)]
    code, elapsed = run_command(command, stage_dir / f"eval-{label}.log",
                                timeout, env)
    if code:
        raise RuntimeError(f"{label} evaluation exited {code}")
    lines = (stage_dir / f"eval-{label}.log").read_text().splitlines()
    result = json.loads(next(line for line in reversed(lines)
                             if line.startswith("{")))
    result["seconds"] = elapsed
    return result


def legal_counts(prefix, move_cap, perturb, seed):
    game = pyspiel.load_game(
        f"euclid_book1(proposition=2,move_cap={move_cap},dense_actions=true,"
        f"start_file={prefix},start_perturb={perturb},start_seed={seed})"
    )
    return [len(game.new_initial_state().legal_actions()) for _ in range(10)]


def completed_actor_games(stage_dir):
    return sum(len(re.findall(r"^\[.*\] Game \d+:", path.read_text(), re.M))
               for path in stage_dir.glob("log-actor-*.txt"))


def main():
    OUT.mkdir(parents=True, exist_ok=True)
    demonstration = json.loads(SOURCE_DEMO.read_text())
    (OUT / "demonstration.json").write_text(json.dumps(demonstration, indent=2) + "\n")
    env = dict(os.environ, PYTHONHASHSEED=HASH_SEED, OMP_NUM_THREADS="1",
               OPENBLAS_NUM_THREADS="1", MKL_NUM_THREADS="1",
               XLA_FLAGS="--xla_cpu_multi_thread_eigen=false")
    deadline = time.monotonic() + 15 * 60
    results = []
    resume_dir = None
    resume_step = None
    last_stage = None
    for k in range(1, 9):
        if time.monotonic() >= deadline:
            break
        stage_dir = OUT / f"stage-{k}"
        stage_dir.mkdir(exist_ok=True)
        prefix = stage_dir / "prefix.json"
        prefix.write_text(json.dumps(demonstration[:8-k], indent=2) + "\n")
        train_env = env
        command = CPU_PREFIX + [str(PYTHON), str(RUNNER), "--proposition", "2",
                   "--out", str(stage_dir), "--steps", "100",
                   "--actors", "12", "--simulations", "32",
                   "--start-file", str(prefix), "--move-cap", str(k + 2),
                   "--start-perturb", "2", "--start-perturb-rate", "0.5",
                   "--start-seed", str(2026111100 + k)]
        if resume_dir is not None:
            command += ["--resume-from", str(resume_dir),
                        "--resume-step", str(resume_step)]
        cap = min(240, max(1, int(deadline - time.monotonic())))
        code, train_seconds = run_command(command, stage_dir / "train.log",
                                          cap, train_env)
        checkpoint = latest_checkpoint(stage_dir)
        games = completed_actor_games(stage_dir)
        clean_counts = legal_counts(prefix, k + 2, 0, 2026111100 + k)
        perturbed_counts = legal_counts(prefix, k + 2, 2, 2026111200 + k)
        clean = evaluation(stage_dir, checkpoint, prefix, k + 2, 0,
                           2026111100 + k, "clean", env,
                           max(1, min(360, int(deadline - time.monotonic()))))
        perturbed = evaluation(stage_dir, checkpoint, prefix, k + 2, 2,
                               2026111200 + k, "perturbed", env,
                               max(1, min(360, int(deadline - time.monotonic()))))
        advanced = clean["wins"] >= 8 and perturbed["wins"] >= 6
        result = {"stage": k, "moves_remaining": k, "games": games,
                  "train_seconds": train_seconds, "train_returncode": code,
                  "checkpoint": checkpoint, "clean": clean,
                  "perturbed": perturbed, "advanced": advanced,
                  "clean_legal_counts": clean_counts,
                  "perturbed_legal_counts": perturbed_counts}
        results.append(result)
        (OUT / "stages.json").write_text(json.dumps(results, indent=2) + "\n")
        last_stage = (stage_dir, checkpoint)
        if not advanced:
            break
        resume_dir, resume_step = stage_dir, checkpoint

    if last_stage is not None and time.monotonic() < deadline:
        stage_dir, checkpoint = last_stage
        empty = OUT / "initial-prefix.json"
        empty.write_text("[]\n")
        final = evaluation(stage_dir, checkpoint, empty, 10, 0,
                           2026111300, "from-start", env,
                           max(1, min(360, int(deadline - time.monotonic()))))
    else:
        final = None
    summary = {"arm": "DEMO", "python_hash_seed": HASH_SEED,
               "actors": 12, "simulations": 32, "stages": results,
               "from_start": final}
    (OUT / "summary.json").write_text(json.dumps(summary, indent=2) + "\n")
    print(json.dumps(summary))


if __name__ == "__main__":
    main()
