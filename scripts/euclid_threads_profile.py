#!/usr/bin/env python3
"""Profile one clean rc13 stage-2 root move by mutually exclusive phases."""

from __future__ import annotations

import json
import os
import argparse
from pathlib import Path
import time

import numpy as np
import pyspiel
from open_spiel.python.algorithms import mcts
from open_spiel.python.algorithms.alpha_zero import evaluator as evaluator_lib
from open_spiel.python.algorithms.alpha_zero import model_nnx

import euclid_spiel  # noqa: F401
import euclid_widening
from euclid_open_spiel_alpha_zero import WinInOneEvaluator, load_actor_weights


ROOT = Path(__file__).resolve().parents[1]
RC13 = ROOT / "holes/excursions/E-euclid-tutor-data/spiel/rc13"


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--search", choices=("packaged", "widening"),
                        default="packaged")
    args = parser.parse_args()
    # Create the fixed-size JAX pool under one-CPU affinity, as actors do.
    pool = set(range(20, 32))
    os.sched_setaffinity(0, {20})
    prefix = RC13 / "stage-2/prefix.json"
    game = pyspiel.load_game(
        f"euclid_book1(proposition=2,move_cap=4,dense_actions=true,"
        f"alpha_zero_facade=true,start_file={prefix},start_perturb=0)"
    )
    model = model_nnx.Model.build_model(
        "mlp", game.observation_tensor_shape(), game.num_distinct_actions(),
        32, 2, 1e-4, 1e-3, str(RC13 / "stage-2"),
        decouple_weight_decay=False)
    load_actor_weights(model, RC13 / "stage-2/checkpoint-1")
    os.sched_setaffinity(0, pool)

    evaluator = evaluator_lib.AlphaZeroEvaluator(game, model)
    scanner = WinInOneEvaluator(evaluator)
    state = game.new_initial_state()

    started = time.perf_counter()
    wins = scanner._winning_actions(state)
    scan_seconds = time.perf_counter() - started
    if wins:
        raise AssertionError("clean stage-2 position unexpectedly has mate in one")

    if args.search == "widening":
        evaluator = euclid_widening.WideningEvaluator(
            evaluator, game.num_distinct_actions())
        bot = euclid_widening.ProgressiveWideningBot(
            game, 1.41, 32, evaluator,
            random_state=np.random.RandomState(13), dirichlet_noise=None)
    else:
        bot = mcts.MCTSBot(
            game, 1.41, 32, evaluator, solve=False,
            random_state=np.random.RandomState(13), dirichlet_noise=None,
            child_selection_fn=mcts.SearchNode.puct_value,
            dont_return_chance_node=True)
    timings = {"legal": 0.0, "inference": 0.0, "clone": 0.0,
               "apply": 0.0, "legal_in_apply": 0.0}
    counts = {"legal": 0, "inference": 0, "clone": 0, "apply": 0}
    application_depth = 0

    original_legal = euclid_spiel.EuclidState._legal_actions
    original_clone = euclid_spiel.EuclidState.clone
    original_apply = euclid_spiel.EuclidState._apply_action
    inference_owner = (evaluator.wrapped if args.search == "widening"
                       else evaluator)
    original_inference = inference_owner._inference

    def timed_legal(self, player):
        fresh = self._actions is None
        then = time.perf_counter()
        result = original_legal(self, player)
        if fresh:
            elapsed = time.perf_counter() - then
            timings["legal"] += elapsed
            if application_depth:
                timings["legal_in_apply"] += elapsed
            counts["legal"] += 1
        return result

    def timed_clone(self):
        then = time.perf_counter()
        result = original_clone(self)
        timings["clone"] += time.perf_counter() - then
        counts["clone"] += 1
        return result

    def timed_apply(self, action):
        nonlocal application_depth
        then = time.perf_counter()
        application_depth += 1
        try:
            return original_apply(self, action)
        finally:
            application_depth -= 1
            timings["apply"] += time.perf_counter() - then
            counts["apply"] += 1

    def timed_inference(state):
        then = time.perf_counter()
        result = original_inference(state)
        timings["inference"] += time.perf_counter() - then
        counts["inference"] += 1
        return result

    euclid_spiel.EuclidState._legal_actions = timed_legal
    euclid_spiel.EuclidState.clone = timed_clone
    euclid_spiel.EuclidState._apply_action = timed_apply
    inference_owner._inference = timed_inference
    started = time.perf_counter()
    root = bot.mcts_search(state)
    search_seconds = time.perf_counter() - started
    # OpenSpiel calls back into legal-action generation before its Python
    # apply callback returns.  Assign that nested interval to enumeration,
    # rather than counting it in both phases.
    state_seconds = (timings["clone"] + timings["apply"]
                     - timings["legal_in_apply"])
    other_seconds = max(0.0, search_seconds - timings["legal"]
                        - timings["inference"] - state_seconds)
    result = {
        "search": args.search,
        "total_seconds": scan_seconds + search_seconds,
        "root_scan_seconds": scan_seconds,
        "root_legal_actions": len(state.legal_actions()),
        "tree_legal_enumeration": {
            "calls": counts["legal"],
            "seconds": timings["legal"],
            "mean_seconds": (timings["legal"] / counts["legal"]
                             if counts["legal"] else 0.0),
        },
        "network_inference": {
            "calls": counts["inference"],
            "seconds": timings["inference"],
        },
        "state_clone_application": {
            "clone_calls": counts["clone"],
            "apply_calls": counts["apply"],
            "seconds": state_seconds,
        },
        "other_seconds": other_seconds,
        "root_children": len(root.children),
    }
    if args.search == "widening":
        result["widening"] = bot.last_stats
    print(json.dumps(result, indent=2))


if __name__ == "__main__":
    main()
