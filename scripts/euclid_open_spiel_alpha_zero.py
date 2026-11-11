#!/usr/bin/env python3
"""Run OpenSpiel's packaged AlphaZero on the rules-only Euclid game."""

from __future__ import annotations

import argparse
from collections import OrderedDict
import json
import os
from pathlib import Path
import signal
import sys
import types

from open_spiel.python.algorithms import mcts
from open_spiel.python.algorithms.alpha_zero import alpha_zero
from open_spiel.python.algorithms.alpha_zero import evaluator as evaluator_lib
from open_spiel.python.algorithms.alpha_zero import model_nnx
import numpy as np
import pyspiel

sys.path.insert(0, str(Path(__file__).resolve().parent))
import euclid_spiel  # noqa: E402,F401  (registers the Python game)

DENSE_I1_ACTIONS = euclid_spiel.DenseActionCodec(1).size


def actor_weights_path(checkpoint_path):
    """The plain-array inference snapshot paired with an Orbax checkpoint."""
    return Path(f"{checkpoint_path}.actor-weights")


def save_actor_weights(model, checkpoint_path):
    """Write model parameters once for reliable concurrent actor loading."""
    import jax

    destination = actor_weights_path(checkpoint_path)
    destination.mkdir(parents=True, exist_ok=True)
    leaves, _ = jax.tree_util.tree_flatten(model._state.params)
    manifest = []
    for index, leaf in enumerate(leaves):
        array = np.asarray(leaf)
        filename = f"{index:04d}.npy"
        np.save(destination / filename, array, allow_pickle=False)
        manifest.append({"file": filename, "shape": list(array.shape),
                         "dtype": str(array.dtype)})
    (destination / "manifest.json").write_text(
        json.dumps(manifest, indent=2) + "\n")
    return destination


def load_actor_weights(model, checkpoint_path):
    """Install a learner-exported inference snapshot without reading Orbax."""
    import jax

    source = actor_weights_path(checkpoint_path)
    manifest = json.loads((source / "manifest.json").read_text())
    target_leaves, treedef = jax.tree_util.tree_flatten(model._state.params)
    if len(manifest) != len(target_leaves):
        raise ValueError("actor parameter leaf count does not match model")
    loaded = []
    for expected, entry in zip(target_leaves, manifest):
        array = np.load(source / entry["file"], mmap_mode="r",
                        allow_pickle=False)
        if array.shape != expected.shape or array.dtype != expected.dtype:
            raise ValueError("actor parameter shape or dtype does not match model")
        loaded.append(jax.device_put(np.asarray(array)))
    params = jax.tree_util.tree_unflatten(treedef, loaded)
    model._state = model._state.replace(params=params)


def actor_update_checkpoint(logger, queue, model, az_evaluator):
    """Consume learner snapshots without concurrent Orbax/TensorStore reads."""
    path = None
    while True:
        try:
            path = queue.get_nowait()
        except alpha_zero.spawn.Empty:
            break
    if path:
        logger.print("Inference cache:", az_evaluator.cache_info())
        logger.print("Loading actor weights", path)
        checkpoint = Path(model._path) / f"checkpoint-{path}"
        load_actor_weights(model, checkpoint)
        az_evaluator.clear_cache()
    elif path is not None:
        return False
    return True


class WinInOneEvaluator(mcts.Evaluator):
    """Give terminal winning children all prior mass before neural inference."""

    def __init__(self, wrapped, cache_size=4096):
        self.wrapped = wrapped
        self.cache_size = cache_size
        self.cache = OrderedDict()

    def _winning_actions(self, state):
        key = state.logical_state_key()
        if key in self.cache:
            self.cache.move_to_end(key)
            return self.cache[key]
        actions = tuple(state.legal_actions())
        wins = []
        for action in actions:
            child = state.clone()
            try:
                child.apply_certified_action(action)
            except (euclid_spiel.eng.MissingPremise, TypeError, ValueError):
                # The neutral enumerator checks stated premises and arithmetic
                # entailments; the transactional consistency guard can still
                # refuse a grounded candidate.  Such a candidate is not a win.
                continue
            if child.goal_reached():
                wins.append(action)
        result = tuple(wins)
        self.cache[key] = result
        self.cache.move_to_end(key)
        if len(self.cache) > self.cache_size:
            self.cache.popitem(last=False)
        return result

    def evaluate(self, state):
        if self._winning_actions(state):
            return np.array([1.0, -1.0])
        return self.wrapped.evaluate(state)

    def prior(self, state):
        wins = self._winning_actions(state)
        if wins:
            probability = 1.0 / len(wins)
            return [(action, probability) for action in wins]
        return self.wrapped.prior(state)

    def cache_info(self):
        return self.wrapped.cache_info()

    def clear_cache(self):
        self.cache.clear()
        return self.wrapped.clear_cache()


def maybe_win_in_one(evaluator, enabled=True):
    return WinInOneEvaluator(evaluator) if enabled else evaluator


_PACKAGED_ACTOR = alpha_zero.actor


def plain_weight_actor(**kwargs):
    """Spawn target that consumes learner-exported parameter arrays."""
    original_update = alpha_zero.update_checkpoint
    alpha_zero.update_checkpoint = actor_update_checkpoint
    try:
        return _PACKAGED_ACTOR(**kwargs)
    finally:
        alpha_zero.update_checkpoint = original_update


def win_in_one_actor(**kwargs):
    """Spawn target that installs the wrapper inside the actor process."""
    original_evaluator = evaluator_lib.AlphaZeroEvaluator

    def wrapped_evaluator(game, model):
        return WinInOneEvaluator(original_evaluator(game, model))

    evaluator_lib.AlphaZeroEvaluator = wrapped_evaluator
    try:
        return _PACKAGED_ACTOR(**kwargs)
    finally:
        evaluator_lib.AlphaZeroEvaluator = original_evaluator


def win_in_one_plain_weight_actor(**kwargs):
    """Spawn target combining mate detection and plain weight transport."""
    original_evaluator = evaluator_lib.AlphaZeroEvaluator
    original_update = alpha_zero.update_checkpoint

    def wrapped_evaluator(game, model):
        return WinInOneEvaluator(original_evaluator(game, model))

    evaluator_lib.AlphaZeroEvaluator = wrapped_evaluator
    alpha_zero.update_checkpoint = actor_update_checkpoint
    try:
        return _PACKAGED_ACTOR(**kwargs)
    finally:
        alpha_zero.update_checkpoint = original_update
        evaluator_lib.AlphaZeroEvaluator = original_evaluator


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
    parser.add_argument("--win-in-one", action=argparse.BooleanOptionalAction,
                        default=True)
    args = parser.parse_args()
    if args.win_in_one and args.resume_from is not None:
        alpha_zero.actor = win_in_one_plain_weight_actor
    elif args.win_in_one:
        alpha_zero.actor = win_in_one_actor
    elif args.resume_from is not None:
        alpha_zero.actor = plain_weight_actor
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
        evaluator = maybe_win_in_one(
            evaluator_lib.AlphaZeroEvaluator(loaded_game, model), args.win_in_one)
        wins = timeouts = search_failures = 0
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
            except ValueError as exc:
                if str(exc) != "max() iterable argument is empty":
                    raise
                # A timed-out/degenerate MCTS can leave a root with no child.
                # It is an evaluation non-win, not a reason to lose the run's
                # remaining receipts.
                timeouts += 1
                search_failures += 1
            finally:
                signal.alarm(0)
        signal.signal(signal.SIGALRM, previous_handler)
        result = {"step": args.eval_step, "games": args.eval_games,
                  "wins": wins, "timeouts": timeouts,
                  "search_failures": search_failures,
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
        learner_pid = os.getpid()

        def resume_init(target_config):
            target = original_init(target_config)
            # alpha_zero starts actors before it initializes the learner.  Only
            # the learner process restores the carried Orbax state; actors
            # begin with an ordinary model and receive exported parameters.
            if os.getpid() != learner_pid:
                return target
            source = original_init(target_config.replace(
                path=str(args.resume_from.resolve())))
            source.load_checkpoint(args.resume_step)
            target._state = source._state
            packaged_save = target.save_checkpoint

            def save_with_actor_weights(_model, step):
                result = packaged_save(step)
                checkpoint = Path(_model._path) / f"checkpoint-{step}"
                save_actor_weights(_model, checkpoint)
                return result

            target.save_checkpoint = types.MethodType(
                save_with_actor_weights, target)
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
