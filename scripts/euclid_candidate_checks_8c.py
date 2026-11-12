#!/usr/bin/env python3
"""Exhaustive two-ply receipt for DESIGN-2 packet 8c."""

from __future__ import annotations

import argparse
import json
import multiprocessing as mp
from pathlib import Path
import time

import numpy as np
import pyspiel

import euclid_spiel


def candidates(state):
    codec = state._euclid_game.action_codec
    rules = set(codec.rules)
    by_kind = {kind: set(names[:euclid_spiel.DENSE_KIND_CAPS[kind]])
               for kind, names in codec._by_kind(state).items()}
    free = {rule: set(codec.layouts[rule][1]) for rule in rules}
    return euclid_spiel.enumerate_actions(
        state._proposition, state.engine_state, state.initial_kinds,
        len(state.history_moves), state._parent_legal_keys, rules, by_kind,
        free, state._canonical_names(), validate=False)


def game(prefix):
    return pyspiel.load_game(
        "euclid_book1(proposition=2,move_cap=4,dense_actions=true,"
        f"alpha_zero_facade=true,start_file={prefix})")


def child(parent, move, engine_state):
    result = parent.clone()
    result.engine_state = engine_state.copy()
    result.history_moves.append(move)
    result._actions = result._action_by_id = None
    result._parent_legal_keys = frozenset()
    return result


def scan_chunk(payload):
    prefix, indexed = payload
    root = game(prefix).new_initial_state()
    rows = []
    for index, move in indexed:
        successor, refusal = euclid_spiel.admit_candidate(
            root.engine_state, len(root.history_moves), move)
        if refusal is not None:
            rows.append({"index": index, "move": move,
                         "first_move_refusal": str(refusal)})
            continue
        position = child(root, move, successor)
        winning_replies = []
        for reply in candidates(position):
            reply_state, refusal = euclid_spiel.admit_candidate(
                position.engine_state, len(position.history_moves), reply)
            if refusal is None and child(position, reply, reply_state).goal_reached():
                winning_replies.append(reply)
                break
        if winning_replies:
            rows.append({"index": index, "move": move,
                         "winning_replies": winning_replies})
    return rows


def prior_ranks(state, moves, snapshot, seed):
    from flax import nnx
    import euclid_candidate_features as features
    import euclid_candidate_model as model_module

    model = model_module.CandidateModel(seed=seed)
    model_module.load_plain_arrays(model, snapshot)
    arrays = features.extract(state, moves).arrays
    logits, _ = nnx.jit(lambda network, inputs: network(inputs))(
        model, model_module.batch_features([arrays]))
    values = np.asarray(logits[0, :len(moves)])
    order = np.argsort(-values, kind="stable")
    ranks = np.empty(len(order), dtype=np.int64)
    ranks[order] = np.arange(1, len(order) + 1)
    return ranks.tolist()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--prefix", type=Path, required=True)
    parser.add_argument("--packet7", type=Path, required=True)
    parser.add_argument("--packet8b", type=Path, required=True)
    parser.add_argument("--workers", type=int, default=12)
    parser.add_argument("--seed", type=int, default=2026111208)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    root = game(args.prefix).new_initial_state()
    moves = candidates(root)
    chunks = [[] for _ in range(args.workers)]
    for index, move in enumerate(moves):
        chunks[index % args.workers].append((index, move))
    started = time.perf_counter()
    with mp.get_context("fork").Pool(args.workers) as pool:
        nested = pool.map(scan_chunk, [(str(args.prefix), part)
                                       for part in chunks])
    elapsed = time.perf_counter() - started
    all_rows = sorted((row for group in nested for row in group),
                  key=lambda row: row["index"])
    refused = [row for row in all_rows if "first_move_refusal" in row]
    rows = [row for row in all_rows if "winning_replies" in row]
    rank7 = prior_ranks(root, moves, args.packet7, args.seed)
    rank8 = prior_ranks(root, moves, args.packet8b, args.seed)
    for row in rows:
        row["packet7_rank"] = rank7[row["index"]]
        row["packet8b_rank"] = rank8[row["index"]]
    result = {"premise_certified_candidates": len(moves),
              "admission_refusals": len(refused),
              "admitted_first_moves": len(moves) - len(refused),
              "elapsed_seconds": elapsed,
              "moves_leaving_win_in_one": len(rows), "rows": rows,
              "refused": refused}
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(result, indent=2) + "\n")
    print(json.dumps(result, sort_keys=True))


if __name__ == "__main__":
    main()
