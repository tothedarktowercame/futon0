#!/usr/bin/env python3
"""Acceptance measurements for DESIGN-2's grounded-candidate learner."""

from __future__ import annotations

import argparse
from dataclasses import replace
import json
from pathlib import Path
import time

from flax import nnx
import numpy as np

import euclid_candidate_alpha_zero as loop
import euclid_candidate_features as features
import euclid_candidate_model as candidate_model
import euclid_candidate_puct as puct
import euclid_spiel


def _model(snapshot, seed):
    model = candidate_model.CandidateModel(seed=seed)
    candidate_model.load_plain_arrays(model, snapshot)
    return model


def _raw_prior_receipt(model, state):
    candidates = puct.premise_certified_candidates(state)
    arrays = features.extract(state, candidates).arrays
    batch = candidate_model.batch_features([arrays])
    logits, _value = nnx.jit(lambda network, inputs: network(inputs))(
        model, batch)
    logits = np.asarray(logits[0, :len(candidates)])
    order = np.argsort(-logits, kind="stable")
    ranks = np.empty(len(order), dtype=np.int64)
    ranks[order] = np.arange(1, len(order) + 1)
    winning = []
    for index, candidate in enumerate(candidates):
        successor, refusal = euclid_spiel.admit_candidate(
            state.engine_state, len(state.history_moves), candidate)
        if refusal is None:
            child = puct.CandidatePUCT._child_state(
                state, candidate, successor)
            if child.goal_reached():
                winning.append((int(ranks[index]), candidate))
    winning.sort(key=lambda item: item[0])
    return {
        "best_winning_rank": winning[0][0] if winning else None,
        "winning_candidates": len(winning),
        "total_candidates": len(candidates),
        "best_winning_candidate": winning[0][1] if winning else None,
        "initial_moves": len(state.initial_moves),
    }


def held_out(config_path, untrained, trained, seeds):
    config = loop.Config.load(config_path)
    models = {
        "generation_0": _model(untrained, config.seed),
        "trained": _model(trained, config.seed),
    }
    receipts = []
    for seed in seeds:
        seeded = replace(config, start_seed=seed, start_perturb_rate=1.0)
        state = loop._game(seeded).new_initial_state()
        row = {"seed": seed}
        for label, model in models.items():
            row[label] = _raw_prior_receipt(model, state)
        receipts.append(row)
    return receipts


def _cost_mode(model, config, root_scan, repetitions):
    game = loop._game(config)
    bot = puct.CandidatePUCT(
        model, simulations=32, root_win_in_one=root_scan,
        temperature=0.0, seed=config.seed)
    # Compile before the measured repetitions.
    bot.search(game.new_initial_state())
    rows = []
    for _index in range(repetitions):
        state = game.new_initial_state()
        started = time.perf_counter()
        result = bot.search(state)
        elapsed = time.perf_counter() - started
        rows.append({"wall_seconds": elapsed, **result.stats})
    phases = (
        "candidate_generation_seconds", "features_seconds", "model_seconds",
        "admission_seconds", "root_scan_seconds", "other_seconds")
    return {
        "mean_wall_seconds": float(np.mean(
            [row["wall_seconds"] for row in rows])),
        "max_wall_seconds": float(np.max(
            [row["wall_seconds"] for row in rows])),
        "mean_breakdown_seconds": {
            phase.removesuffix("_seconds"): float(np.mean(
                [row[phase] for row in rows])) for phase in phases
        },
        "moves": rows,
    }


def costs(snapshot, stage2_prefix, seed, repetitions):
    config = loop.Config(
        proposition=2, move_cap=4, start_file=str(stage2_prefix),
        simulations=32, root_win_in_one=False, seed=seed,
        thread_cpus="19-31")
    model = _model(snapshot, seed)
    return {
        "root_scan_off": _cost_mode(model, config, False, repetitions),
        "root_scan_on": _cost_mode(model, config, True, repetitions),
    }


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--config", type=Path, required=True)
    parser.add_argument("--untrained", type=Path, required=True)
    parser.add_argument("--trained", type=Path, required=True)
    parser.add_argument("--stage2-prefix", type=Path, required=True)
    parser.add_argument("--seeds", type=int, nargs="+", required=True)
    parser.add_argument("--repetitions", type=int, default=10)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    result = {
        "held_out": held_out(
            args.config, args.untrained, args.trained, args.seeds),
        "stage2_cost": costs(
            args.trained, args.stage2_prefix,
            loop.Config.load(args.config).seed, args.repetitions),
    }
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(result, indent=2) + "\n")
    print(json.dumps(result, sort_keys=True))


if __name__ == "__main__":
    main()
