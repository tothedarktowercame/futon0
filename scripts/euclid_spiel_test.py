from collections import deque
import json
import random

import pyspiel

import euclid_spiel  # registers euclid_book1
import euclid_play
from euclid_open_spiel_alpha_zero import WinInOneEvaluator, maybe_win_in_one
from open_spiel.python.algorithms import mcts
import numpy as np


def _game(cap=6):
    return pyspiel.load_game(f"euclid_book1(proposition=1,move_cap={cap})")


def test_game_loads_and_every_initial_action_applies():
    state = _game().new_initial_state()
    actions = state.legal_actions()
    assert actions
    for action in actions:
        child = state.clone()
        child.apply_action(action)


def test_neutral_breadth_first_search_reaches_i1_goal():
    initial = _game().new_initial_state()
    queue = deque([initial])
    seen = set()
    while queue and len(seen) < 1000:
        state = queue.popleft()
        key = str(state)
        if key in seen:
            continue
        seen.add(key)
        state.legal_actions()
        # Preserve the wrapper's neutral rule/argument order; OpenSpiel itself
        # requires the stable numeric ids to be returned sorted by integer.
        for grounded in state._actions:
            action = euclid_spiel.stable_action_id(grounded)
            child = state.clone()
            child.apply_action(action)
            if child.returns() == [1.0]:
                return
            queue.append(child)
    raise AssertionError(f"I.1 goal not reached in {len(seen)} neutral BFS states")


def test_open_spiel_random_sim_consistency():
    pyspiel.random_sim_test(_game(cap=4), num_sims=2, serialize=False, verbose=False)


def test_incremental_actions_equal_reference_on_random_deep_states():
    for proposition in (1, 2, 3):
        game = pyspiel.load_game(f"euclid_book1(proposition={proposition},move_cap=6)")
        state = game.new_initial_state()
        rng = random.Random(700 + proposition)
        for _ply in range(3):
            sampled = state.sample_action_tuple(rng)
            state.legal_actions()
            state.apply_action(euclid_spiel.stable_action_id(sampled))
            expected = euclid_spiel.enumerate_actions_reference(
                proposition, state.engine_state, state.initial_kinds, len(state.history_moves)
            )
            state.legal_actions()
            assert state._actions == expected


def test_neutral_sampler_only_returns_legal_actions():
    state = _game().new_initial_state()
    legal = euclid_spiel.enumerate_actions_reference(
        1, state.engine_state, state.initial_kinds, 0
    )
    rng = random.Random(20261111)
    for _ in range(20):
        action = state.sample_action_tuple(rng)
        assert action in legal
        child = state.clone()
        child.apply_action_tuple(action)


def test_compact_neutral_sampler_only_returns_encodable_actions():
    game = pyspiel.load_game(
        "euclid_book1(proposition=2,move_cap=10,dense_actions=true)"
    )
    state = game.new_initial_state()
    rng = random.Random(711)
    for _ in range(20):
        action = state.sample_action_tuple(rng)
        assert game.action_codec.encode(state, action) is not None
        state.apply_action_tuple(action)
        if state.is_terminal():
            break


def test_grounded_action_ids_are_stable_between_states():
    parent = _game().new_initial_state()
    parent.legal_actions()
    child = parent.clone()
    child.apply_action(parent.legal_actions()[0])
    child.legal_actions()

    parent_by_key = {(rule, args): euclid_spiel.stable_action_id(action)
                     for action in parent._actions for rule, args, _bindings in [action]}
    child_by_key = {(rule, args): euclid_spiel.stable_action_id(action)
                    for action in child._actions for rule, args, _bindings in [action]}
    common = parent_by_key.keys() & child_by_key.keys()
    assert common
    for key in common:
        assert parent_by_key[key] == child_by_key[key]


def test_observation_tensor_has_fixed_declared_shape_at_several_depths():
    state = _game().new_initial_state()
    rng = random.Random(9182)
    for _ in range(4):
        observation = state.observation_tensor()
        assert observation.shape == (euclid_spiel.OBSERVATION_SIZE,)
        assert observation.dtype.name == "float32"
        state.apply_action_tuple(state.sample_action_tuple(rng))


def test_dense_action_ids_round_trip_and_fit_policy_head():
    for proposition in (1, 2):
        game = pyspiel.load_game(
            f"euclid_book1(proposition={proposition},move_cap=6,dense_actions=true)"
        )
        state = game.new_initial_state()
        rng = random.Random(3000 + proposition)
        for _ply in range(3):
            legal = state.legal_actions()
            assert legal
            assert max(legal) < game.num_distinct_actions()
            reference = euclid_spiel.enumerate_actions_reference(
                proposition, state.engine_state, state.initial_kinds,
                len(state.history_moves),
            )
            expected = [action for action in reference
                        if game.action_codec.encode(state, action) is not None]
            assert state._actions == expected
            for action_id in legal:
                grounded = state._action_by_id[action_id]
                assert game.action_codec.decode(state, action_id) == grounded
                assert game.action_codec.encode(state, grounded) == action_id
            state.apply_action(rng.choice(legal))


def _proposition_4_dense_state():
    game = pyspiel.load_game(
        "euclid_book1(proposition=5,move_cap=4,dense_actions=true)"
    )
    state = game.new_initial_state()
    # I.4's hypotheses provide two complete triangles and the SAS equalities.
    state.engine_state = euclid_play._start_state(4)
    return game, state


def test_dense_proposition_4_is_named_by_its_six_points():
    game, state = _proposition_4_dense_state()
    codec = game.action_codec
    variables, free, determined = codec.layouts["proposition_4"]
    assert free == ("a", "b", "c", "d", "e", "f")
    assert set(determined) == {"AB", "BC", "AC", "DE", "EF", "DF"}

    full = ("a", "b", "c", "d", "e", "f",
            "AB", "BC", "AC", "DE", "EF", "DF")
    action = ("proposition_4", full, ())
    action_id = codec.encode(state, action)
    assert action_id is not None
    assert codec.decode(state, action_id) == action
    state.legal_actions()
    assert state._action_by_id[action_id] == action
    assert state.action_to_string(0, action_id) == "proposition_4(a,b,c,d,e,f)"


def test_dense_proposition_4_is_illegal_when_a_side_is_missing():
    game, state = _proposition_4_dense_state()
    full = ("a", "b", "c", "d", "e", "f",
            "AB", "BC", "AC", "DE", "EF", "DF")
    action_id = game.action_codec.encode(state, ("proposition_4", full, ()))
    assert action_id is not None

    state.engine_state.facts = {
        fact: why for fact, why in state.engine_state.facts.items()
        if "DF" not in fact
    }
    state._actions = state._action_by_id = None
    assert game.action_codec.decode(state, action_id) is None
    assert action_id not in state.legal_actions()


def test_dense_i4_codec_contains_superposition_with_six_free_points():
    codec = euclid_spiel.DenseActionCodec(4)
    assert "superposition" in codec.rules
    assert codec.layouts["superposition"][1] == ("a", "b", "c", "d", "g", "h")


def test_alpha_zero_facade_has_nonacting_second_player():
    game = pyspiel.load_game(
        "euclid_book1(proposition=1,move_cap=2,dense_actions=true,alpha_zero_facade=true)"
    )
    state = game.new_initial_state()
    assert game.num_players() == 2
    assert state.current_player() == 0
    state.apply_action(state.legal_actions()[0])
    assert state.current_player() in (0, pyspiel.PlayerId.TERMINAL)
    assert len(state.returns()) == 2
    assert sum(state.returns()) == 0


def test_compact_i2_is_reachable_with_ten_move_horizon():
    game = pyspiel.load_game(
        "euclid_book1(proposition=2,move_cap=10,dense_actions=true)"
    )
    state = game.new_initial_state()
    witness = [
        ("line_from_points", ("a", "b")),
        ("proposition_1", ("a", "b", "L_1")),
        ("line_from_points", ("d_2", "a")),
        ("line_from_points", ("d_2", "b")),
        ("circle_from_points", ("b", "c")),
        ("intersection_circle_line_extending_points", ("C_5", "L_4", "b", "d_2")),
        ("circle_from_points", ("d_2", "g_6")),
        ("intersection_circle_line_extending_points", ("C_7", "L_3", "a", "d_2")),
    ]
    for rule, args in witness:
        state.legal_actions()
        matches = [action_id for action_id, grounded in state._action_by_id.items()
                   if grounded[0] == rule and grounded[1] == args]
        assert len(matches) == 1, (rule, args)
        state.apply_action(matches[0])
    assert state.returns() == [1.0]


def test_i2_start_state_prefix_leaves_a_reachable_remaining_solution(tmp_path):
    """Every reverse-curriculum start replays its prefix exactly."""
    witness = [
        ("line_from_points", ("a", "b")),
        ("proposition_1", ("a", "b", "L_1")),
        ("line_from_points", ("d_2", "a")),
        ("line_from_points", ("d_2", "b")),
        ("circle_from_points", ("b", "c")),
        ("intersection_circle_line_extending_points", ("C_5", "L_4", "b", "d_2")),
        ("circle_from_points", ("d_2", "g_6")),
        ("intersection_circle_line_extending_points", ("C_7", "L_3", "a", "d_2")),
    ]
    source = pyspiel.load_game(
        "euclid_book1(proposition=2,move_cap=10,dense_actions=true)"
    ).new_initial_state()
    grounded = []
    for rule, args in witness:
        source.legal_actions()
        matches = [move for move in source._action_by_id.values()
                   if move[0] == rule and move[1] == args]
        assert len(matches) == 1
        move = matches[0]
        grounded.append(move)
        source.apply_action_tuple(move)

    for prefix_length in range(len(grounded)):
        start_file = tmp_path / f"start-{prefix_length}.json"
        start_file.write_text(json.dumps(grounded[:prefix_length]))
        remaining = grounded[prefix_length:]
        game = pyspiel.load_game(
            f"euclid_book1(proposition=2,move_cap={len(remaining) + 2},"
            f"dense_actions=true,start_file={start_file})"
        )
        state = game.new_initial_state()
        assert state.history_moves == []
        for move in remaining:
            state.apply_action_tuple(move)
        assert state.returns() == [1.0]

    prefix = grounded[:4]
    start_file = tmp_path / "perturb-prefix.json"
    start_file.write_text(json.dumps(prefix))
    clean_game = pyspiel.load_game(
        f"euclid_book1(proposition=2,move_cap=6,dense_actions=true,"
        f"start_file={start_file},start_perturb=0,start_seed=100)"
    )
    clean = clean_game.new_initial_state()
    assert clean.initial_moves == prefix
    assert clean.history_moves == []

    perturbed_game = pyspiel.load_game(
        f"euclid_book1(proposition=2,move_cap=6,dense_actions=true,"
        f"start_file={start_file},start_perturb=2,start_seed=100)"
    )
    for _ in range(10):
        perturbed = perturbed_game.new_initial_state()
        extras = perturbed.initial_moves[len(prefix):]
        assert perturbed.initial_moves[:len(prefix)] == prefix
        assert 0 <= len(extras) <= 2
        assert perturbed.history_moves == []
        assert perturbed._move_cap == 6 + len(extras)
        replay = clean_game.new_initial_state()
        for move in extras:
            replay.legal_actions()
            assert move in replay._actions
            replay.apply_action_tuple(move)
        assert replay.engine_state.facts == perturbed.engine_state.facts
        assert replay.engine_state.la.rows == perturbed.engine_state.la.rows


class _FlatEvaluator(mcts.Evaluator):
    def evaluate(self, state):
        return np.array([0.25, -0.25])

    def prior(self, state):
        actions = state.legal_actions()
        return [(action, 1.0 / len(actions)) for action in actions]

    def cache_info(self):
        return None

    def clear_cache(self):
        return None


def test_win_in_one_forces_stage_one_win_and_is_neutral_at_i2_start(tmp_path):
    witness = [
        ("line_from_points", ("a", "b")),
        ("proposition_1", ("a", "b", "L_1")),
        ("line_from_points", ("d_2", "a")),
        ("line_from_points", ("d_2", "b")),
        ("circle_from_points", ("b", "c")),
        ("intersection_circle_line_extending_points", ("C_5", "L_4", "b", "d_2")),
        ("circle_from_points", ("d_2", "g_6")),
    ]
    source = pyspiel.load_game(
        "euclid_book1(proposition=2,move_cap=10,dense_actions=true)"
    ).new_initial_state()
    prefix = []
    for rule, args in witness:
        source.legal_actions()
        move = next(move for move in source._action_by_id.values()
                    if move[0] == rule and move[1] == args)
        prefix.append(move)
        source.apply_action_tuple(move)
    start_file = tmp_path / "stage-one.json"
    start_file.write_text(json.dumps(prefix))
    game = pyspiel.load_game(
        f"euclid_book1(proposition=2,move_cap=3,dense_actions=true,"
        f"alpha_zero_facade=true,start_file={start_file})"
    )
    base = _FlatEvaluator()
    assert maybe_win_in_one(base, False) is base
    evaluator = WinInOneEvaluator(base)
    for _ in range(10):
        state = game.new_initial_state()
        bot = mcts.MCTSBot(
            game, 1.41, 2, evaluator, solve=False, dirichlet_noise=None,
            child_selection_fn=mcts.SearchNode.puct_value,
            dont_return_chance_node=True,
        )
        state.apply_action(bot.step(state))
        assert state.goal_reached()

    initial = pyspiel.load_game(
        "euclid_book1(proposition=2,move_cap=10,dense_actions=true,"
        "alpha_zero_facade=true)"
    ).new_initial_state()
    wrapped = _FlatEvaluator()
    checked = WinInOneEvaluator(wrapped)
    assert np.array_equal(checked.evaluate(initial), wrapped.evaluate(initial))
    assert checked.prior(initial) == wrapped.prior(initial)
