import euclid_hindsight_filter as f


def rec(moves, goal, needed, proposition=1):
    return {"proposition": proposition, "run_seed": 1,
            "moves": [{"tuple": [r, list(a), dict(b)]} for r, a, b in moves],
            "goal": list(goal), "needed_move_indices": list(needed)}


def i1_moves():
    return [
        ("circle_from_points", ("a", "b"), (("C", "C_1"),)),
        ("circle_from_points", ("b", "a"), (("C", "C_2"),)),
        ("intersection_circles", ("C_1", "C_2"), (("c", "c_3"),)),
    ]


def test_degenerate_collinear_angle():
    moves = [
        ("extend_point_longer", ("AB", "a", "b", "b", "a"), (("x", "x_1"),)),
        ("extend_point_longer", ("AB", "b", "a", "x_1", "b"), (("x", "x_2"),)),
    ]
    row = rec(moves, ("_eq_ang", "a", "b", "x_1", "a", "b", "x_2"), (0, 1))
    assert "degenerate" in f.classify(row)


def test_direct_output_and_naming_marker():
    moves = [
        ("extend_point_longer", ("AB", "a", "b", "b", "a"), (("x", "x_1"),)),
        ("extend_point_longer", ("AB", "b", "a", "x_1", "b"), (("x", "x_2"),)),
    ]
    assert "direct" in f.classify(rec(moves, ("on_line", "x_2", "AB"), (0, 1)))
    assert "naming" in f.classify(rec(moves, ("rayang", "b", "a", "x_2", "x_1"), (0, 1)))
    assert f.zero_length(("_lt_len", "a", "a", "a", "x_2"))


def test_i1_equal_sides_is_not_trivial():
    row = rec(i1_moves(), ("_eq_len", "a", "c_3", "b", "c_3"), (0, 1, 2))
    assert f.classify(row) == []
