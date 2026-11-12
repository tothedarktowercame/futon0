import euclid_hindsight_train as t


def test_split_is_by_run_and_deterministic():
    assert t.split_for(2, 17) == t.split_for(2, 17)
    assert {t.split_for(2, seed) for seed in range(100)} == {"train", "heldout"}


def test_action_signature_ignores_fresh_result_name():
    left=("circle_from_points",("a","b"),(("C","C_1"),))
    right=("circle_from_points",("a","b"),(("C","C_9"),))
    assert t.action_signature(left)==t.action_signature(right)


def test_candidates_have_only_declared_feature_objects():
    state = t._state(2)
    objects = set(state._object_order())
    assert all(set(action[1]) <= objects for action in t.candidates(state))


def test_dataset_split_has_no_shared_logical_states():
    # Exercise the post-split invariant without rebuilding the full E1c set.
    examples = [
        {"split": "train", "state_key": "shared"},
        {"split": "heldout", "state_key": "shared"},
        {"split": "heldout", "state_key": "held-only"},
    ]
    assert t.make_state_disjoint(examples) == (1, 1)
    assert ({x["state_key"] for x in examples if x["split"] == "train"} &
            {x["state_key"] for x in examples if x["split"] == "heldout"}) == set()
