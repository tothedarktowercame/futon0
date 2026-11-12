import euclid_hindsight as h
import euclid_engine as eng
import random
import time

import pytest


def i1_moves():
    return [
        ("circle_from_points", ("a", "b"), (("C", "C_1"),)),
        ("circle_from_points", ("b", "a"), (("C", "C_2"),)),
        ("intersection_circles", ("C_1", "C_2"), (("c", "c_3"),)),
    ]


def test_metric_needed_set_and_irrelevant_move():
    moves = i1_moves()
    state = h.replay(1, moves)
    assert state is not None
    goal = ("_eq_len", "c_3", "a", "a", "b")
    assert eng._fact_holds(state, goal)
    assert h.minimise_needed(1, moves, goal) == (0, 1, 2)
    goals, bound = h.metric_goals(state, {"C_1", "C_2", "c_3"})
    assert ("_eq_len", "a", "b", "a", "c_3") in goals
    assert bound["length_atoms"] == 3

    irrelevant = ("line_from_points", ("a", "b"), (("L", "L_4"),))
    moves.insert(2, irrelevant)
    assert h.minimise_needed(1, moves, goal) == (0, 1, 3)


def test_canonical_key_ignores_made_names():
    one = i1_moves()
    rename = {"C_1": "Cx1", "C_2": "Cx2", "c_3": "cx3"}
    two = [(r, tuple(rename.get(x, x) for x in a),
            tuple((b, rename[n]) for b, n in bindings))
           for r, a, bindings in one]
    goal1 = ("_eq_len", "c_3", "a", "a", "b")
    goal2 = ("_eq_len", "cx3", "a", "a", "b")
    assert h.canonical_key(goal1, one, (0, 1, 2)) == h.canonical_key(
        goal2, two, (0, 1, 2))


def test_generated_records_replay():
    records, _stats = h.records_for_run(1, 17, plies=3)
    assert all(h.verify_record(record) for record in records)


def test_rule_uniform_then_grounding_uniform():
    actions = [("many", (str(i),), ()) for i in range(20)] + [("one", ("x",), ())]
    rng = random.Random(7)
    picked = [h.choose_action(actions, rng, "rule-uniform")[0] for _ in range(1000)]
    assert 450 < picked.count("one") < 550
    assert h.choose_action(actions, random.Random(1), "move-uniform") in actions


def test_two_fact_i1_goal_needs_all_three_moves_and_replays():
    moves = i1_moves()
    goals = (("_eq_len", "c_3", "a", "a", "b"),
             ("_eq_len", "c_3", "b", "a", "b"))
    needed = h.minimise_needed_many(1, moves, goals)
    assert needed == (0, 1, 2)
    assert h.auxiliary_moves(moves, needed, goals) == [0, 1]
    assert h.i1_shape(moves, needed)
    assert h.goals_hold(h.replay(1, moves, needed), goals)


def test_conjunctive_canonical_key_is_order_independent():
    moves = i1_moves()
    one = ("_eq_len", "c_3", "a", "a", "b")
    two = ("_eq_len", "c_3", "b", "a", "b")
    assert h.canonical_key_many((one, two), moves, (0, 1, 2)) == \
        h.canonical_key_many((two, one), moves, (0, 1, 2))


def test_per_record_timeout_interrupts_a_slow_replay():
    with pytest.raises(h.RecordTimeout):
        h._timed_needed(0.001, time.sleep, 0.02)
