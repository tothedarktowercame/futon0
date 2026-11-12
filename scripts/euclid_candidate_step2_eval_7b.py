#!/usr/bin/env python3
"""Corrected held-out and root-coverage receipts for DESIGN-2 packet 7b."""

from __future__ import annotations

import argparse
from dataclasses import replace
import glob
import hashlib
import json
from pathlib import Path

from flax import nnx
import numpy as np

import euclid_candidate_alpha_zero as loop
import euclid_candidate_features as features
import euclid_candidate_model as candidate_model
import euclid_candidate_puct as puct
import euclid_candidate_replay as replay
import euclid_spiel


def _key_digest(state):
    encoded = repr(state.logical_state_key()).encode()
    return hashlib.sha256(encoded).hexdigest()


def _feature_digest(arrays):
    digest = hashlib.sha256()
    for name in sorted(arrays):
        if name.startswith("candidate_") or name.startswith("rule_"):
            continue
        array = np.ascontiguousarray(arrays[name])
        digest.update(name.encode() + b"\0")
        digest.update(array.dtype.str.encode() + b"\0")
        digest.update(repr(array.shape).encode() + b"\0")
        digest.update(array.tobytes())
    return digest.hexdigest()


def _training_keys(config, run_dir):
    summary = json.loads((Path(run_dir) / "summary.json").read_text())
    keys = set()
    for actor, receipt in summary["actors"].items():
        game = loop._game(config, seed_offset=int(actor) * 1_000_000)
        # Include the possible in-flight start at shutdown. This is a strict
        # superset of starts that could have produced a receipt.
        for _ in range(receipt["games"] + 1):
            keys.add(game.new_initial_state().logical_state_key())
    return keys


def _receipt_feature_digests(run_dir):
    result = set()
    pattern = str(Path(run_dir) / "receipts/actor-*.jsonl")
    for path in sorted(glob.glob(pattern)):
        for record in replay.read_receipts(path):
            result.add(_feature_digest(record.features))
    return result


def _model(snapshot, seed):
    model = candidate_model.CandidateModel(seed=seed)
    candidate_model.load_plain_arrays(model, snapshot)
    return model


def _logits(model, arrays):
    batch = candidate_model.batch_features([arrays])
    logits, _value = nnx.jit(lambda network, inputs: network(inputs))(
        model, batch)
    return np.asarray(logits[0, :int(arrays["candidate_mask"].sum())])


def _winning_indices(state, candidates):
    result = []
    for index, candidate in enumerate(candidates):
        successor, refusal = euclid_spiel.admit_candidate(
            state.engine_state, len(state.history_moves), candidate)
        if refusal is None:
            child = puct.CandidatePUCT._child_state(state, candidate, successor)
            if child.goal_reached():
                result.append(index)
    return result


def _ranks(logits, candidates, winning):
    order = np.argsort(-logits, kind="stable")
    overall = np.empty(len(order), dtype=np.int64)
    overall[order] = np.arange(1, len(order) + 1)
    winner = min(winning, key=lambda index: int(overall[index]))
    winning_rule = candidates[winner][0]

    rule_scores = {}
    rule_order = []
    for index, candidate in enumerate(candidates):
        rule = candidate[0]
        if rule not in rule_scores:
            rule_scores[rule] = float(logits[index])
            rule_order.append(rule)
        else:
            rule_scores[rule] = max(rule_scores[rule], float(logits[index]))
    ranked_rules = sorted(
        rule_order, key=lambda rule: -rule_scores[rule])
    rule_rank = ranked_rules.index(winning_rule) + 1

    within = [index for index, candidate in enumerate(candidates)
              if candidate[0] == winning_rule]
    within.sort(key=lambda index: -float(logits[index]))
    return {
        "overall_rank": int(overall[winner]),
        "rule_rank": rule_rank,
        "within_rule_rank": within.index(winner) + 1,
        "winning_rule": winning_rule,
        "winning_candidate": candidates[winner],
    }


def corrected_held_out(config_path, run_dir, untrained, trained, seed_start,
                       count):
    config = loop.Config.load(config_path)
    training_keys = _training_keys(config, run_dir)
    receipt_digests = _receipt_feature_digests(run_dir)
    models = {
        "generation_0": _model(untrained, config.seed),
        "trained": _model(trained, config.seed),
    }
    positions = []
    seen = set()
    seed = seed_start
    while len(positions) < count:
        seeded = replace(config, start_seed=seed, start_perturb_rate=1.0)
        state = loop._game(seeded).new_initial_state()
        key = state.logical_state_key()
        if (len(state.initial_moves) > len(loop._game(config).start_moves)
                and key not in training_keys and key not in seen):
            candidates = puct.premise_certified_candidates(state)
            arrays = features.extract(state, candidates).arrays
            feature_digest = _feature_digest(arrays)
            if feature_digest not in receipt_digests:
                winning = _winning_indices(state, candidates)
                if winning:
                    row = {
                        "seed": seed,
                        "logical_state_key_sha256": _key_digest(state),
                        "feature_sha256": feature_digest,
                        "perturb_moves": len(state.initial_moves)
                                         - len(loop._game(config).start_moves),
                        "total_candidates": len(candidates),
                        "winning_candidates": len(winning),
                    }
                    ablated = {name: np.asarray(value).copy()
                               for name, value in arrays.items()}
                    ablated["object_creation_order"].fill(0)
                    for label, model in models.items():
                        row[label] = {
                            "normal": _ranks(
                                _logits(model, arrays), candidates, winning),
                            "creation_order_zero": _ranks(
                                _logits(model, ablated), candidates, winning),
                        }
                    positions.append(row)
                    seen.add(key)
        seed += 1
    return positions


def root_coverage(snapshot, stage2_prefix, seed, moves=10):
    config = loop.Config(
        proposition=2, move_cap=4, start_file=str(stage2_prefix),
        simulations=32, root_win_in_one=False, seed=seed,
        thread_cpus="19-31")
    model = _model(snapshot, seed)
    bot = puct.CandidatePUCT(
        model, simulations=32, root_win_in_one=False,
        temperature=0.0, seed=seed)
    game = loop._game(config)
    bot.search(game.new_initial_state())  # JIT warm-up, not recorded.
    result = []
    for _ in range(moves):
        searched = bot.search(game.new_initial_state())
        result.append({
            "total_candidates": len(searched.candidate_keys),
            "visited_children": sum(
                visit > 0 for visit in searched.visit_counts),
            "visit_counts_nonzero": [
                visit for visit in searched.visit_counts if visit > 0],
        })
    return result


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--config", type=Path, required=True)
    parser.add_argument("--run-dir", type=Path, required=True)
    parser.add_argument("--untrained", type=Path, required=True)
    parser.add_argument("--trained", type=Path, required=True)
    parser.add_argument("--stage2-prefix", type=Path, required=True)
    parser.add_argument("--seed-start", type=int, default=2026111401)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    config = loop.Config.load(args.config)
    result = {
        "held_out": corrected_held_out(
            args.config, args.run_dir, args.untrained, args.trained,
            args.seed_start, 10),
        "root_coverage": root_coverage(
            args.trained, args.stage2_prefix, config.seed, 10),
    }
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(result, indent=2) + "\n")
    print(json.dumps(result, sort_keys=True))


if __name__ == "__main__":
    main()
