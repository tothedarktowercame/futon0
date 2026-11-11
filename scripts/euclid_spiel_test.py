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
        for action in state.legal_actions():
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
            state.apply_action(state._actions.index(sampled))
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
