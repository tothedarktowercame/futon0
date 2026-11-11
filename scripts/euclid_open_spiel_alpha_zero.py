#!/usr/bin/env python3
"""Run OpenSpiel's packaged AlphaZero on the rules-only Euclid game."""

from __future__ import annotations

import argparse
import json
from pathlib import Path
import sys

from open_spiel.python.algorithms import mcts
from open_spiel.python.algorithms.alpha_zero import alpha_zero
from open_spiel.python.algorithms.alpha_zero import evaluator as evaluator_lib
from open_spiel.python.algorithms.alpha_zero import model_nnx
import pyspiel

sys.path.insert(0, str(Path(__file__).resolve().parent))
import euclid_spiel  # noqa: E402,F401  (registers the Python game)

DENSE_I1_ACTIONS = euclid_spiel.DenseActionCodec(1).size


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--proposition", type=int, required=True)
    parser.add_argument("--out", type=Path, required=True)
    parser.add_argument("--steps", type=int, default=20)
    parser.add_argument("--actors", type=int, default=4)
    parser.add_argument("--simulations", type=int, default=32)
    parser.add_argument("--eval-step", type=int)
    parser.add_argument("--eval-games", type=int, default=20)
    parser.add_argument("--warm-from", type=Path)
    parser.add_argument("--warm-step", type=int, default=30)
    args = parser.parse_args()
    game = (f"euclid_book1(proposition={args.proposition},move_cap=6,"
            "dense_actions=true,alpha_zero_facade=true)")
    config = alpha_zero.Config(
        game=game,
        path=str(args.out.resolve()),
        learning_rate=1e-3,
        weight_decay=1e-4,
        decouple_weight_decay=False,
        train_batch_size=64,
        replay_buffer_size=256,
        replay_buffer_reuse=4,
        max_steps=args.steps,
        checkpoint_freq=1,
        actors=args.actors,
        evaluators=0,
        uct_c=1.41,
        max_simulations=args.simulations,
        policy_alpha=1.0,
        policy_epsilon=0.25,
        temperature=1.0,
        temperature_drop=4,
        evaluation_window=50,
        eval_levels=2,
        nn_model="mlp",
        nn_width=32,
        nn_depth=2,
        observation_shape=None,
        output_size=None,
        quiet=True,
        verbose=False,
        nn_api_version="nnx",
    )
    if args.eval_step is not None:
        loaded_game = pyspiel.load_game(game)
        model = model_nnx.Model.build_model(
            model_type=config.nn_model,
            input_shape=loaded_game.observation_tensor_shape(),
            output_size=loaded_game.num_distinct_actions(),
            nn_width=config.nn_width,
            nn_depth=config.nn_depth,
            weight_decay=config.weight_decay,
            learning_rate=config.learning_rate,
            path=config.path,
            decouple_weight_decay=config.decouple_weight_decay,
        )
        model.load_checkpoint(args.eval_step)
        evaluator = evaluator_lib.AlphaZeroEvaluator(loaded_game, model)
        wins = 0
        for _ in range(args.eval_games):
            state = loaded_game.new_initial_state()
            bot = mcts.MCTSBot(
                loaded_game, config.uct_c, config.max_simulations, evaluator,
                solve=False, dirichlet_noise=None,
                child_selection_fn=mcts.SearchNode.puct_value,
                dont_return_chance_node=True,
            )
            while not state.is_terminal():
                state.apply_action(bot.step(state))
            wins += int(state.returns()[0] > 0)
        print(json.dumps({"step": args.eval_step, "games": args.eval_games, "wins": wins}))
        return
    if args.warm_from is not None:
        # OpenSpiel has no resume/transfer flag. Keep its training loop intact,
        # but initialise each packaged model with every shape-compatible leaf
        # from I.1. The differently sized I.2 policy head remains fresh.
        import jax

        original_init = alpha_zero._init_model_from_config

        def warm_init(target_config):
            target = original_init(target_config)
            source_config = target_config.replace(
                game="euclid_book1(proposition=1,move_cap=6,dense_actions=true,alpha_zero_facade=true)",
                output_size=DENSE_I1_ACTIONS,
                path=str(args.warm_from.resolve()),
            )
            source = original_init(source_config)
            source.load_checkpoint(args.warm_step)
            target._state = target._state.replace(params=jax.tree.map(
                lambda t, s: s if t.shape == s.shape else t,
                target._state.params, source._state.params,
            ))
            return target

        alpha_zero._init_model_from_config = warm_init
    alpha_zero.alpha_zero(config)


if __name__ == "__main__":
    main()
