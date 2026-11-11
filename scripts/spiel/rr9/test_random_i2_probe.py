from random_i2_probe import ground_specs, replay_validate


WITNESS = [
    ("line_from_points", ("a", "b")),
    ("proposition_1", ("a", "b", "L_1")),
    ("line_from_points", ("d_2", "a")),
    ("line_from_points", ("d_2", "b")),
    ("circle_from_points", ("b", "c")),
    ("intersection_circle_line_extending_points", ("C_5", "L_4", "b", "d_2")),
    ("circle_from_points", ("d_2", "g_6")),
    ("intersection_circle_line_extending_points", ("C_7", "L_3", "a", "d_2")),
]


def test_known_i2_witness_validates_and_truncation_does_not():
    grounded = ground_specs(WITNESS)
    valid = replay_validate(grounded)
    assert valid["engine_ok"] is True
    assert valid["student_ok"] is True

    truncated = replay_validate(grounded[:-1])
    assert truncated["engine_ok"] is False
    assert truncated["student_ok"] is False
