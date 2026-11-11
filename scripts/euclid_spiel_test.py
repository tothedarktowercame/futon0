from collections import deque

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
