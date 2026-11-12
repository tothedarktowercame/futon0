import euclid_hindsight_train as t


def test_split_is_by_run_and_deterministic():
    assert t.split_for(2, 17) == t.split_for(2, 17)
    assert {t.split_for(2, seed) for seed in range(100)} == {"train", "heldout"}


def test_action_signature_ignores_fresh_result_name():
    left=("circle_from_points",("a","b"),(("C","C_1"),))
    right=("circle_from_points",("a","b"),(("C","C_9"),))
    assert t.action_signature(left)==t.action_signature(right)
