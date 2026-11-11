#!/usr/bin/env python3
"""Run OpenSpiel's packaged AlphaZero on the rules-only Euclid game."""

from __future__ import annotations

import argparse
import json
from pathlib import Path
import signal
import sys

from open_spiel.python.algorithms import mcts
from open_spiel.python.algorithms.alpha_zero import alpha_zero
from open_spiel.python.algorithms.alpha_zero import evaluator as evaluator_lib
from open_spiel.python.algorithms.alpha_zero import model_nnx
import pyspiel

sys.path.insert(0, str(Path(__file__).resolve().parent))
import euclid_spiel  # noqa: E402,F401  (registers the Python game)

DENSE_I1_ACTIONS = euclid_spiel.DenseActionCodec(1).size


def _orderly_shutdown(_signum, _frame):
    """Let OpenSpiel's training loop close and join every worker process."""
    raise KeyboardInterrupt


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
    parser.add_argument("--resume-from", type=Path)
    parser.add_argument("--resume-step", type=int)
    parser.add_argument("--start-file", type=Path)
    parser.add_argument("--start-perturb", type=int, default=0)
    parser.add_argument("--start-perturb-rate", type=float, default=1.0)
    parser.add_argument("--start-seed", type=int, default=0)
    parser.add_argument("--move-cap", type=int, default=6)
    parser.add_argument("--eval-game-seconds", type=int, default=60)
    parser.add_argument("--eval-record", type=Path)
    args = parser.parse_args()
    start_param = (f",start_file={args.start_file.resolve()}"
                   if args.start_file is not None else "")
    start_param += (f",start_perturb={args.start_perturb},"
                    f"start_perturb_rate={args.start_perturb_rate},"
                    f"start_seed={args.start_seed}")
    game = (f"euclid_book1(proposition={args.proposition},move_cap={args.move_cap},"
            f"dense_actions=true,alpha_zero_facade=true{start_param})")
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
        wins = timeouts = 0
        win_records = []

        def evaluation_timeout(_signum, _frame):
            raise TimeoutError("greedy evaluation game exceeded its time limit")

        previous_handler = signal.signal(signal.SIGALRM, evaluation_timeout)
        for _ in range(args.eval_games):
            signal.alarm(args.eval_game_seconds)
            try:
                state = loaded_game.new_initial_state()
                bot = mcts.MCTSBot(
                    loaded_game, config.uct_c, config.max_simulations, evaluator,
                    solve=False, dirichlet_noise=None,
                    child_selection_fn=mcts.SearchNode.puct_value,
                    dont_return_chance_node=True,
                )
                while not state.is_terminal():
                    state.apply_action(bot.step(state))
                if state.returns()[0] > 0:
                    all_moves = state.initial_moves + state.history_moves
                    fresh = euclid_spiel.play._start_state(args.proposition)
                    for ply, (rule, move_args, bindings) in enumerate(all_moves, 1):
                        euclid_spiel.eng.apply_step(
                            fresh, ply, rule, move_args, dict(bindings))
                    fresh_state = euclid_spiel.EuclidState(loaded_game, fresh)
                    checked = fresh_state.goal_reached()
                    objects = fresh_state._object_list()
                    play_checked = euclid_spiel.play._goal_reached(
                        args.proposition, fresh, objects)
                    if not (checked and play_checked):
                        raise AssertionError("evaluation win failed fresh referee replay")
                    wins += 1
                    win_records.append({
                        "initial_moves": all_moves[:len(state.initial_moves)],
                        "moves": all_moves[len(state.initial_moves):],
                        "spiel_goal_reached": checked,
                        "play_goal_reached": play_checked,
                    })
            except TimeoutError:
                timeouts += 1
            finally:
                signal.alarm(0)
        signal.signal(signal.SIGALRM, previous_handler)
        result = {"step": args.eval_step, "games": args.eval_games,
                  "wins": wins, "timeouts": timeouts,
                  "verified_wins": len(win_records)}
        if args.eval_record is not None:
            args.eval_record.parent.mkdir(parents=True, exist_ok=True)
            with args.eval_record.open("w") as stream:
                for record in win_records:
                    stream.write(json.dumps(record) + "\n")
        print(json.dumps(result))
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
    if args.resume_from is not None:
        if args.resume_step is None:
            parser.error("--resume-from requires --resume-step")
        original_init = alpha_zero._init_model_from_config

        def resume_init(target_config):
            target = original_init(target_config)
            source = original_init(target_config.replace(
                path=str(args.resume_from.resolve())))
            source.load_checkpoint(args.resume_step)
            target._state = source._state
            return target

        alpha_zero._init_model_from_config = resume_init
    # bg.py stops a run with SIGTERM.  Turn that into the interruption the
    # packaged loop already handles: it broadcasts an exit message and joins
    # every actor in its finally block.  Without this bridge, SIGTERM kills the
    # learner immediately and leaves its multiprocessing children orphaned.
    previous_term = signal.signal(signal.SIGTERM, _orderly_shutdown)
    try:
        alpha_zero.alpha_zero(config)
    finally:
        signal.signal(signal.SIGTERM, previous_term)


if __name__ == "__main__":
    main()
