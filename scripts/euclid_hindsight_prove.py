#!/usr/bin/env python3
"""Budgeted ZERO best-first proof search over candidate moves."""
from __future__ import annotations

import argparse
import heapq
import json
import math
import multiprocessing as mp
from pathlib import Path
import random
import time

import numpy as np

import euclid_candidate_features as features
import euclid_candidate_model as models
import euclid_hindsight as hindsight
import euclid_hindsight_train as train
import euclid_spiel


PAGE_SIZE = 10


def _softmax_costs(scores):
    values = np.asarray(scores, dtype=np.float64)
    shifted = values - values.max()
    log_z = math.log(float(np.exp(shifted).sum()))
    return -(shifted - log_z)


def _child(parent, action):
    engine_state, refusal = euclid_spiel.admit_candidate(
        parent.engine_state, len(parent.history_moves), action)
    if engine_state is None:
        return None, refusal
    child = parent.clone()
    canonical = parent._canonical_action(action)
    child.engine_state = engine_state
    child.history_moves.append(canonical)
    child._actions = None
    child._action_by_id = None
    child._parent_legal_keys = frozenset()
    return child, None


def _model_scores(model, state, legal):
    arrays = features.extract(state, legal, goal=None).arrays
    batch = models.batch_features([arrays])
    logits, _ = model(batch)
    return np.asarray(logits[0, :len(legal)], dtype=np.float64)


class Node:
    __slots__ = ("state", "path", "path_cost", "ordered", "costs")

    def __init__(self, state, path=(), path_cost=0.0):
        self.state = state
        self.path = tuple(path)
        self.path_cost = float(path_cost)
        self.ordered = None
        self.costs = None


def prove(start, *, model=None, random_seed=None, max_calls=400,
          wall_seconds=600, page_size=PAGE_SIZE):
    """Return a receipt for one best-first search arm."""
    if (model is None) == (random_seed is None):
        raise ValueError("choose exactly one of model or random_seed")
    rng = random.Random(random_seed)
    begin = time.monotonic()
    serial = 0
    root = Node(start.clone())
    frontier = [(0.0, serial, root, 0)]
    seen = {train.state_key(root.state)}
    calls = expanded = admitted = refused = 0
    result = None

    while frontier and time.monotonic() - begin < wall_seconds:
        priority, _tie, node, page = heapq.heappop(frontier)
        if node.ordered is None:
            if calls >= max_calls:
                continue
            legal = train.candidates(node.state)
            calls += 1
            expanded += 1
            if not legal:
                continue
            scores = ([rng.random() for _ in legal]
                      if model is None else _model_scores(model, node.state, legal))
            costs = _softmax_costs(scores)
            order = np.argsort(-np.asarray(scores), kind="stable")
            node.ordered = [legal[int(i)] for i in order]
            node.costs = [float(costs[int(i)]) for i in order]
        lo, hi = page * page_size, min((page + 1) * page_size, len(node.ordered))
        if lo >= hi:
            continue
        if hi < len(node.ordered):
            serial += 1
            next_priority = node.path_cost + node.costs[hi]
            heapq.heappush(frontier, (next_priority, serial, node, page + 1))
        for index in range(lo, hi):
            action = node.ordered[index]
            child, refusal = _child(node.state, action)
            if child is None:
                refused += 1
                continue
            admitted += 1
            path = node.path + (action,)
            if child.goal_reached():
                result = path
                frontier.clear()
                break
            if len(child.history_moves) >= child._move_cap:
                continue
            key = train.state_key(child)
            if key in seen:
                continue
            seen.add(key)
            serial += 1
            cost = node.path_cost + node.costs[index]
            heapq.heappush(frontier, (cost, serial, Node(child, path, cost), 0))

    elapsed = time.monotonic() - begin
    return {
        "solved": result is not None,
        "network_calls": calls,
        "nodes_expanded": expanded,
        "states_seen": len(seen),
        "admitted": admitted,
        "refused": refused,
        "wall_seconds": elapsed,
        "proof_length": len(result) if result is not None else None,
        "proof": [hindsight.notation(action) for action in (result or ())],
        "proof_tuples": [[a[0], list(a[1]), dict(a[2])] for a in (result or ())],
    }


def replay(start, receipt):
    state = start.clone()
    for raw in receipt["proof_tuples"]:
        action = raw[0], tuple(raw[1]), tuple(sorted(raw[2].items()))
        state.apply_action_tuple(action)
    return state.goal_reached()


def problem_start(name):
    if name in ("i1", "i2", "i3"):
        return train._state(int(name[1]))
    if name == "i2-stage2":
        import euclid_candidate_checks_8c as checks
        prefix = (Path(__file__).resolve().parents[1] /
                  "holes/excursions/E-euclid-tutor-data/spiel/rc13/stage-2/prefix.json")
        return checks.game(prefix).new_initial_state()
    raise ValueError(name)


def run_arm(args):
    problem, arm, snapshot, seed, max_calls, wall_seconds = args
    start = problem_start(problem)
    if arm == "trained":
        model = models.CandidateModel(seed=2026111202)
        models.load_plain_arrays(model, snapshot)
        receipt = prove(start, model=model, max_calls=max_calls,
                        wall_seconds=wall_seconds)
    elif arm == "generation0":
        model = models.CandidateModel(seed=2026111202)
        receipt = prove(start, model=model, max_calls=max_calls,
                        wall_seconds=wall_seconds)
    else:
        receipt = prove(start, random_seed=seed, max_calls=max_calls,
                        wall_seconds=wall_seconds)
    receipt.update({"problem": problem, "arm": arm, "seed": seed,
                    "replay_confirmed": replay(start, receipt) if receipt["solved"] else None})
    return receipt


def run_suite(snapshot, output, max_calls=400, wall_seconds=600):
    problems = ("i1", "i3", "i2-stage2", "i2")
    rows = []
    context = mp.get_context("spawn")
    for problem in problems:
        arms = [(problem, "trained", snapshot, None, max_calls, wall_seconds),
                (problem, "generation0", snapshot, None, max_calls, wall_seconds)]
        arms += [(problem, f"random-{seed}", snapshot, seed, max_calls, wall_seconds)
                 for seed in (2026111203, 2026111204, 2026111205)]
        with context.Pool(len(arms)) as pool:
            for row in pool.imap_unordered(run_arm, arms):
                rows.append(row)
                Path(output).parent.mkdir(parents=True, exist_ok=True)
                with Path(output).open("a") as stream:
                    stream.write(json.dumps(row, sort_keys=True) + "\n")
    return rows


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--snapshot", required=True)
    parser.add_argument("--output", required=True)
    parser.add_argument("--max-calls", type=int, default=400)
    parser.add_argument("--wall-seconds", type=int, default=600)
    args = parser.parse_args()
    run_suite(args.snapshot, args.output, args.max_calls, args.wall_seconds)


if __name__ == "__main__":
    main()
