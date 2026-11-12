#!/usr/bin/env python3
"""Reproducible E4b temperature selection and equal-budget prover matrix."""
from __future__ import annotations

import argparse
import glob
import gzip
import json
import multiprocessing as mp
import os
import pickle
from pathlib import Path
import random
import resource

import numpy as np

import euclid_candidate_model as models
import euclid_hindsight_prove as prover
import euclid_hindsight_train as train


MODEL = None
WORKER_CPUS = None


def _pin_for_jax():
    """Create each JAX CPU client while its worker sees one striped CPU."""
    global WORKER_CPUS
    WORKER_CPUS = tuple(sorted(os.sched_getaffinity(0)))
    identity = mp.current_process()._identity
    ordinal = identity[0] - 1 if identity else 0
    os.sched_setaffinity(0, {WORKER_CPUS[ordinal % len(WORKER_CPUS)]})


def _widen_after_jax():
    os.sched_setaffinity(0, set(WORKER_CPUS))


def _identity(proposition, seed, kind, goal):
    return (int(proposition), int(seed), kind,
            json.dumps(goal, sort_keys=True, separators=(",", ":")))


def select_records(dataset, single_file, conjunctive_glob, output,
                   seed=2026111206, count=40):
    with gzip.open(dataset, "rb") as stream:
        payload = pickle.load(stream)
    held = {
        _identity(row["proposition"], row["seed"], row["kind"], row["goal"])
        for row in payload["examples"] if row["split"] == "heldout"
    }
    found = {}
    inputs = [(single_file, "single")]
    inputs += [(path, "conjunctive")
               for path in sorted(glob.glob(conjunctive_glob))]
    for path, expected in inputs:
        with open(path) as stream:
            for line in stream:
                record = json.loads(line)
                kind = "conjunctive" if record.get("conjunctive") else "single"
                if kind != expected or record.get("needed_size", 0) < 3:
                    continue
                key = _identity(record["proposition"], record["run_seed"],
                                kind, record["goal"])
                if key in held:
                    found.setdefault(key, record)
    rng = random.Random(seed)
    by_kind = {kind: [row for key, row in found.items() if key[2] == kind]
               for kind in ("single", "conjunctive")}
    each = count // 2
    if any(len(rows) < each for rows in by_kind.values()):
        raise ValueError({kind: len(rows) for kind, rows in by_kind.items()})
    selected = []
    for kind in ("single", "conjunctive"):
        rows = sorted(by_kind[kind], key=lambda row: (
            row["proposition"], row["run_seed"], row["canonical_key"]))
        selected.extend(rng.sample(rows, each))
    selected.sort(key=lambda row: ("conjunctive" if row.get("conjunctive")
                                   else "single", row["proposition"],
                                   row["run_seed"], row["canonical_key"]))
    receipt = [{"id": index, "kind": "conjunctive" if row.get("conjunctive")
                else "single", "proposition": row["proposition"],
                "run_seed": row["run_seed"], "goal": row["goal"],
                "needed_size": row["needed_size"],
                "canonical_key": row["canonical_key"]}
               for index, row in enumerate(selected)]
    Path(output).parent.mkdir(parents=True, exist_ok=True)
    Path(output).write_text(json.dumps(receipt, indent=2) + "\n")
    return receipt


def _init_model(snapshot):
    global MODEL
    _pin_for_jax()
    MODEL = models.CandidateModel(seed=2026111202)
    models.load_plain_arrays(MODEL, snapshot)
    _widen_after_jax()


def _init_worker():
    _pin_for_jax()


def _task_count_worker(_task):
    # Smoke constructs the same model/client as a search worker, then widens.
    model = models.CandidateModel(seed=2026111202)
    del model
    _widen_after_jax()
    return {"pid": os.getpid(),
            "tasks": len(list(Path(f"/proc/{os.getpid()}/task").iterdir())),
            "affinity": sorted(os.sched_getaffinity(0))}


def smoke(workers, output):
    context = mp.get_context("spawn")
    with context.Pool(workers, initializer=_init_worker) as pool:
        rows = pool.map(_task_count_worker, range(workers * 4), chunksize=1)
    unique = {row["pid"]: row for row in rows}
    Path(output).write_text(json.dumps({"workers_requested": workers,
        "workers_seen": len(unique), "processes": list(unique.values())},
        indent=2) + "\n")


def _temperature_worker(task):
    row, temperature = task
    start = train._state(row["proposition"])
    result = prover.prove(start, model=MODEL, max_calls=400,
                          wall_seconds=2700, temperature=temperature,
                          goal=row["goal"])
    result.update({"record_id": row["id"], "kind": row["kind"],
                   "proposition": row["proposition"],
                   "run_seed": row["run_seed"], "goal": row["goal"],
                   "needed_size": row["needed_size"],
                   "temperature": temperature,
                   "replay_confirmed": prover.replay(
                       start, result, goal=row["goal"])
                       if result["solved"] else None})
    return result


def run_temperatures(selection, snapshot, output, workers=12):
    rows = json.loads(Path(selection).read_text())
    tasks = [(row, temperature) for temperature in (1.0, 2.0, 4.0)
             for row in rows]
    destination = Path(output)
    destination.parent.mkdir(parents=True, exist_ok=True)
    context = mp.get_context("spawn")
    with context.Pool(workers, initializer=_init_model,
                      initargs=(snapshot,)) as pool:
        for result in pool.imap_unordered(_temperature_worker, tasks):
            with destination.open("a") as stream:
                stream.write(json.dumps(result, sort_keys=True) + "\n")


def _logsumexp(values):
    values = np.asarray(values, dtype=np.float64)
    maximum = float(values.max())
    return maximum + float(np.log(np.exp(values - maximum).sum()))


def calibrate(dataset, snapshot, output, legacy=False):
    with gzip.open(dataset, "rb") as stream:
        payload = pickle.load(stream)
    held = [row for row in payload["examples"] if row["split"] == "heldout"]
    _pin_for_jax()
    model = models.CandidateModel(seed=2026111202)
    if legacy:
        train.load_legacy(model, snapshot)
    else:
        models.load_plain_arrays(model, snapshot)
    _widen_after_jax()
    temperatures = (0.5, 1.0, 1.5, 2.0, 3.0, 4.0)
    sums = {temperature: 0.0 for temperature in temperatures}
    for row in held:
        batch = models.batch_features([row["features"]])
        logits, _value = model(batch)
        values = np.asarray(logits[0, :len(row["target"])], dtype=np.float64)
        positive = np.flatnonzero(row["target"])
        for temperature in temperatures:
            scaled = values / temperature
            sums[temperature] += (_logsumexp(scaled) -
                                  _logsumexp(scaled[positive]))
    result = {"positions": len(held), "legacy": legacy,
              "temperatures": [{"temperature": temperature,
                  "mean_nll": sums[temperature] / len(held)}
                 for temperature in temperatures]}
    result["selected"] = min(result["temperatures"],
                             key=lambda row: row["mean_nll"])["temperature"]
    Path(output).write_text(json.dumps(result, indent=2) + "\n")
    print(json.dumps(result, sort_keys=True))


def _final_worker(task):
    problem, arm, snapshot, seed, temperature = task
    start = prover.problem_start(problem)
    if snapshot is not None:
        model = models.CandidateModel(seed=2026111202)
        models.load_plain_arrays(model, snapshot)
        _widen_after_jax()
        result = prover.prove(start, model=model, max_calls=400,
                              wall_seconds=2700, temperature=temperature)
    elif arm == "generation0":
        model = models.CandidateModel(seed=2026111202)
        _widen_after_jax()
        result = prover.prove(start, model=model, max_calls=400,
                              wall_seconds=2700, temperature=temperature)
    else:
        _widen_after_jax()
        result = prover.prove(start, random_seed=seed, max_calls=400,
                              wall_seconds=2700, temperature=temperature)
    result.update({"problem": problem, "arm": arm, "seed": seed,
                   "temperature": temperature,
                   "worker_pid": os.getpid(),
                   "peak_rss_kib": resource.getrusage(
                       resource.RUSAGE_SELF).ru_maxrss,
                   "replay_confirmed": prover.replay(start, result)
                   if result["solved"] else None})
    return result


def run_final(mixed, e2b, mixed_temperature, e2b_temperature, output,
              workers=4):
    destination = Path(output)
    destination.parent.mkdir(parents=True, exist_ok=True)
    context = mp.get_context("spawn")
    tasks = []
    for problem in ("i1", "i3", "i2-stage2", "i2"):
        tasks += [(problem, "mixed", mixed, None, mixed_temperature),
                  (problem, "e2b", e2b, None, e2b_temperature),
                  (problem, "generation0", None, None, 1.0)]
        tasks += [(problem, f"random-{seed}", None, seed, 1.0)
                  for seed in (2026111203, 2026111204, 2026111205)]
    with context.Pool(workers, initializer=_init_worker,
                      maxtasksperchild=1) as pool:
        for result in pool.imap_unordered(_final_worker, tasks):
            with destination.open("a") as stream:
                stream.write(json.dumps(result, sort_keys=True) + "\n")


def main():
    parser = argparse.ArgumentParser()
    sub = parser.add_subparsers(dest="command", required=True)
    select = sub.add_parser("select")
    select.add_argument("--dataset", required=True)
    select.add_argument("--single-file", required=True)
    select.add_argument("--conjunctive-glob", required=True)
    select.add_argument("--output", required=True)
    select.add_argument("--seed", type=int, default=2026111206)
    temperatures = sub.add_parser("temperatures")
    temperatures.add_argument("--selection", required=True)
    temperatures.add_argument("--snapshot", required=True)
    temperatures.add_argument("--output", required=True)
    temperatures.add_argument("--workers", type=int, default=12)
    final = sub.add_parser("final")
    final.add_argument("--mixed", required=True)
    final.add_argument("--e2b", required=True)
    final.add_argument("--mixed-temperature", type=float, required=True)
    final.add_argument("--e2b-temperature", type=float, required=True)
    final.add_argument("--workers", type=int, default=4)
    final.add_argument("--output", required=True)
    calibration = sub.add_parser("calibrate")
    calibration.add_argument("--dataset", required=True)
    calibration.add_argument("--snapshot", required=True)
    calibration.add_argument("--output", required=True)
    calibration.add_argument("--legacy", action="store_true")
    smoke_parser = sub.add_parser("smoke")
    smoke_parser.add_argument("--workers", type=int, required=True)
    smoke_parser.add_argument("--output", required=True)
    args = parser.parse_args()
    if args.command == "select":
        select_records(args.dataset, args.single_file, args.conjunctive_glob,
                       args.output, seed=args.seed)
    elif args.command == "temperatures":
        run_temperatures(args.selection, args.snapshot, args.output,
                         workers=args.workers)
    elif args.command == "final":
        run_final(args.mixed, args.e2b, args.mixed_temperature,
                  args.e2b_temperature, args.output, workers=args.workers)
    elif args.command == "calibrate":
        calibrate(args.dataset, args.snapshot, args.output,
                  legacy=args.legacy)
    else:
        smoke(args.workers, args.output)


if __name__ == "__main__":
    main()
