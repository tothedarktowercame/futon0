import hashlib
from fractions import Fraction
import json
import os
from pathlib import Path
import subprocess
import sys

import numpy as np
import pyspiel

import euclid_candidate_features as features
import euclid_spiel


ROOT = Path(__file__).resolve().parents[1]
PREFIX = ROOT / "holes/excursions/E-euclid-tutor-data/spiel/rc13/stage-2/prefix.json"


def _candidates(state):
    codec = state._euclid_game.action_codec
    rules = set(codec.rules)
    by_kind = {
        kind: set(names[:euclid_spiel.DENSE_KIND_CAPS[kind]])
        for kind, names in codec._by_kind(state).items()
    }
    free = {rule: set(codec.layouts[rule][1]) for rule in rules}
    return euclid_spiel.enumerate_actions(
        state._proposition, state.engine_state, state.initial_kinds,
        len(state.history_moves), state._parent_legal_keys,
        rules, by_kind, free, state._canonical_names(), validate=False)


def _made_i1(line_name="L_1", circle_name="C_2"):
    state = pyspiel.load_game(
        "euclid_book1(proposition=1,move_cap=6,dense_actions=true)"
    ).new_initial_state()
    state.apply_action_tuple((
        "line_from_points", ("a", "b"), (("L", line_name),)))
    state.apply_action_tuple((
        "circle_from_points", ("a", "b"), (("C", circle_name),)))
    return state


def _prefix_digest():
    state = pyspiel.load_game(
        f"euclid_book1(proposition=2,move_cap=4,dense_actions=true,"
        f"alpha_zero_facade=true,start_file={PREFIX})"
    ).new_initial_state()
    encoded = features.extract(state, _candidates(state))
    digest = hashlib.sha256()
    for name, array in sorted(encoded.arrays.items()):
        digest.update(name.encode())
        digest.update(str(array.shape).encode())
        digest.update(array.tobytes())
    return digest.hexdigest()


def test_non_given_rename_preserves_all_arrays():
    left = _made_i1("left_line", "left_circle")
    right = _made_i1("right_line", "right_circle")
    left_arrays = features.extract(left, _candidates(left)).arrays
    right_arrays = features.extract(right, _candidates(right)).arrays
    assert left_arrays.keys() == right_arrays.keys()
    for name in left_arrays:
        assert np.array_equal(left_arrays[name], right_arrays[name]), name


def test_alias_is_absent_and_canonical_record_is_unchanged():
    plain = _made_i1()
    aliased = plain.clone()
    euclid_spiel.eng.apply_step(
        aliased.engine_state, 3, "name_point", ("a",), {"x": "alias_a"})
    assert "alias_a" not in aliased._object_order()
    plain_arrays = features.extract(plain, _candidates(plain)).arrays
    alias_arrays = features.extract(aliased, _candidates(aliased)).arrays
    for name in plain_arrays:
        assert np.array_equal(plain_arrays[name], alias_arrays[name]), name


def test_i1_two_move_fixture_every_array():
    state = _made_i1()
    # Both entries are taken, in order, from enumerate_actions(validate=False).
    candidates = _candidates(state)[:2]
    assert candidates == [
        ("angle_self_zero", ("a", "b"), ()),
        ("angle_self_zero", ("b", "a"), ()),
    ]
    actual = {name: value.tolist()
              for name, value in features.extract(state, candidates).arrays.items()}
    expected = {
        "schema_version": [1],
        "vocabulary_version": list(bytes.fromhex(
            features.VOCABULARY.vocabulary_version)),
        "object_mask": [True, True, True, True, True],
        "object_kind": [1, 1, 2, 2, 3],
        "object_given": [True, True, True, False, False],
        "object_creation_order": [0, 1, 2, 3, 4],
        "object_creator_rule": [1, 1, 1, 29, 10],
        "object_result_role": [2, 2, 2, 180, 98],
        "object_parent": [[0, 0], [0, 0], [0, 0], [0, 1], [0, 1]],
        "object_parent_mask": [[False, False], [False, False], [False, False],
                               [True, True], [True, True]],
        "object_goal_predicate": [
            [4, 4, 4, 0, 4, 0, 4, 0], [4, 0, 0, 4, 4, 4, 0, 4],
            [0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0]],
        "object_goal_role": [
            [17, 18, 19, 0, 17, 0, 19, 0], [17, 0, 0, 20, 17, 18, 0, 20],
            [0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0]],
        "object_goal_mask": [
            [True, True, True, False, True, False, True, False],
            [True, False, False, True, True, True, False, True],
            [False, False, False, False, False, False, False, False],
            [False, False, False, False, False, False, False, False],
            [False, False, False, False, False, False, False, False]],
        "fact_mask": [True] * 20,
        "fact_predicate": [17, 18, 18, 18, 18, 1, 26, 28, 28, 30,
                           30, 34, 34, 36, 37, 37, 37, 37, 41, 41],
        "fact_object": [
            [0, 4, 0], [0, 1, 2], [0, 1, 3], [1, 0, 2], [1, 0, 3],
            [0, 1, 0], [0, 4, 0], [2, 4, 0], [3, 4, 0], [0, 1, 0],
            [1, 0, 0], [0, 4, 0], [1, 4, 0], [1, 4, 0], [0, 2, 0],
            [0, 3, 0], [1, 2, 0], [1, 3, 0], [0, 1, 0], [1, 0, 0]],
        "fact_object_mask": [
            [True, True, False], [True, True, True], [True, True, True],
            [True, True, True], [True, True, True], [True, True, False],
            [True, True, False], [True, True, False], [True, True, False],
            [True, True, False], [True, True, False], [True, True, False],
            [True, True, False], [True, True, False], [True, True, False],
            [True, True, False], [True, True, False], [True, True, False],
            [True, True, False], [True, True, False]],
        "fact_role": [[3, 4, 0], [3, 4, 5], [3, 4, 5], [3, 4, 5],
                      [3, 4, 5]] + [[3, 4, 0]] * 15,
        "fact_constant_type": [[0, 0, 0]] * 20,
        "fact_constant_bucket": [[0, 0, 0]] * 20,
        "candidate_mask": [True, True],
        "candidate_rule": [4, 4],
        "candidate_object": [[0, 1], [1, 0]],
        "candidate_argument_role": [[78, 77], [78, 77]],
        "candidate_argument_kind": [[1, 1], [1, 1]],
        "candidate_argument_mask": [[True, True], [True, True]],
        "candidate_premise_predicate": [[30], [30]],
        "candidate_premise_mask": [[True], [True]],
        "candidate_conclusion_predicate": [[15], [15]],
        "candidate_conclusion_mask": [[True], [True]],
    }
    assert actual == expected


def test_vocabulary_metadata_round_trip(tmp_path):
    path = tmp_path / "vocabulary.json"
    features.VOCABULARY.save(path)
    saved = json.loads(path.read_text())
    assert saved == features.VOCABULARY.to_dict()
    assert saved["schema_version"] == features.SCHEMA_VERSION
    assert len(saved["vocabulary_version"]) == 64


def test_fact_numeric_constants_have_typed_stable_buckets():
    state = _made_i1()
    state.engine_state.facts[("probe_numeric", "a", Fraction(3, 2), 7)] = (
        "test", [])
    arrays = features.extract(state, []).arrays
    row = next(i for i, types in enumerate(arrays["fact_constant_type"])
               if np.any(types))
    assert arrays["fact_predicate"][row] == features.VOCABULARY.predicates["<unk>"]
    assert arrays["fact_object_mask"][row].tolist() == [True, False, False]
    assert arrays["fact_constant_type"][row].tolist() == [0, 2, 1]
    assert all(bucket > 0 for bucket in
               arrays["fact_constant_bucket"][row, 1:])


def test_real_fact_change_changes_features_and_repeat_is_identical():
    state = _made_i1()
    candidates = _candidates(state)[:8]
    original = features.extract(state, candidates).arrays
    repeated = features.extract(state, candidates).arrays
    assert all(np.array_equal(original[name], repeated[name])
               for name in original)

    changed = state.clone()
    removed = next(iter(changed.engine_state.facts))
    del changed.engine_state.facts[removed]
    modified = features.extract(changed, candidates).arrays
    assert any(not np.array_equal(original[name], modified[name])
               for name in original)


def test_explicit_hindsight_goal_changes_only_goal_conditioning():
    state = _made_i1()
    candidates = _candidates(state)[:8]
    default = features.extract(state, candidates).arrays
    explicit_default = features.extract(
        state, candidates, goal=features.play.GOALS[1][0]).arrays
    # I.1 has two conjuncts, so a single explicit conjunct is intentionally
    # different; repeating the default remains byte-identical.
    repeated = features.extract(state, candidates).arrays
    assert all(np.array_equal(default[name], repeated[name]) for name in default)
    other = features.extract(
        state, candidates, goal=("_lt_len", "a", "b", "a", "a")).arrays
    assert not np.array_equal(explicit_default["object_goal_predicate"],
                              other["object_goal_predicate"])
    assert "_lt_len" in features.VOCABULARY.predicates


def test_explicit_two_atom_goal_equals_i1_book_goal():
    state = _made_i1()
    candidates = _candidates(state)[:8]
    default = features.extract(state, candidates).arrays
    explicit = features.extract(
        state, candidates, goal=list(features.play.GOALS[1])).arrays
    assert all(np.array_equal(default[name], explicit[name]) for name in default)


def test_prefix_features_are_deterministic_between_processes():
    command = [sys.executable, "-c",
               "import euclid_candidate_features_test as t; "
               "print(t._prefix_digest())"]
    environment = dict(os.environ, PYTHONHASHSEED="0")
    first = subprocess.check_output(
        command, cwd=Path(__file__).parent, env=environment,
        text=True, timeout=300).strip()
    second = subprocess.check_output(
        command, cwd=Path(__file__).parent, env=environment,
        text=True, timeout=300).strip()
    assert first == second
