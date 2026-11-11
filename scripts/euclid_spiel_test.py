from collections import deque
import random

import pyspiel

import euclid_spiel  # registers euclid_book1


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
            for action_id in legal:
                grounded = state._action_by_id[action_id]
                assert game.action_codec.decode(state, action_id) == grounded
                assert game.action_codec.encode(state, grounded) == action_id
            state.apply_action(rng.choice(legal))


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
