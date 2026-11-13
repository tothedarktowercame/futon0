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


def test_book_goal_match_accepts_atom_order_and_renamed_witness():
    record = {
        "proposition": 1,
        "goal": [["_eq_len", "made_point", "b", "a", "b"],
                 ["_eq_len", "made_point", "a", "a", "b"]],
    }
    assert t.book_goal_match(record)[0] == 1
    record["goal"][0][1] = "another_point"
    assert t.book_goal_match(record) is None


def test_book_goal_match_catches_i1_goal_from_other_givens():
    """Review (claude-9): E4a found I.1's goal by random play from I.2's
    givens; the first matcher compared only with the record's own goal."""
    from_i2 = {"proposition": 2,
               "goal": [["_eq_len", "a", "b", "b", "c_4"],
                        ["_eq_len", "a", "c_4", "b", "c_4"]]}
    assert t.book_goal_match(from_i2)[0] == 1
    from_i3 = {"proposition": 3,
               "goal": [["_eq_len", "c1", "c0", "d_9", "c1"],
                        ["_eq_len", "d_9", "c0", "c0", "c1"]]}
    assert t.book_goal_match(from_i3)[0] == 1
    # Same shape but the second atom is about a different made point.
    near_miss = {"proposition": 2,
                 "goal": [["_eq_len", "a", "b", "b", "c_4"],
                          ["_eq_len", "a", "d_5", "b", "d_5"]]}
    assert t.book_goal_match(near_miss) is None
    # A witness must be a made object, not a given point.
    given_only = {"proposition": 2,
                  "goal": [["_eq_len", "a", "b", "b", "c"],
                           ["_eq_len", "a", "c", "b", "c"]]}
    assert t.book_goal_match(given_only) is None


def test_book_goal_match_covers_later_book_goals_and_rejects_near_miss():
    matching = {"proposition": 4,
                "goal": [["_eq_len", "b", "c", "e", "f"],
                         ["_eq_ang", "a", "b", "c", "d", "e", "f"],
                         ["_eq_ang", "a", "c", "b", "d", "f", "e"]]}
    assert t.book_goal_match(matching)[0] == 4
    matching["goal"][2][-1] = "a"
    assert t.book_goal_match(matching) is None


def test_book_goal_match_rejects_structured_arithmetic_arguments():
    record = {"proposition": 4,
              "goal": ["sublen", ["len", "a", "b"], "a", "c"]}
    assert t.book_goal_match(record) is None


def test_book_goal_match_reads_angles_and_inequalities_in_any_order():
    """Review (claude-9): I.4 stated with reversed angles, and I.19 stated as
    a less-than, must still be recognised as Book goals."""
    reversed_i4 = {"proposition": 4,
                   "goal": [["_eq_len", "c", "b", "f", "e"],
                            ["_eq_ang", "f", "e", "d", "c", "b", "a"],
                            ["_eq_ang", "b", "c", "a", "e", "f", "d"]]}
    assert t.book_goal_match(reversed_i4)[0] == 4
    i19_as_lt = {"proposition": 19, "goal": ["_lt_len", "b", "a", "c", "a"]}
    assert t.book_goal_match(i19_as_lt)[0] == 19
