import euclid_hindsight as h
import euclid_engine as eng


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
