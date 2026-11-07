#!/usr/bin/env python3
"""Tests for euclid_review.py (E-euclid-tutor engine review).

Real engine runs, no mocks: every fixture replays through the live
euclid_engine/euclid_search modules loaded by euclid_review.
Run from scripts/: python3 -m pytest euclid_review_test.py -q
"""

import importlib.util
from pathlib import Path

spec = importlib.util.spec_from_file_location(
    "euclid_review", Path(__file__).resolve().parent / "euclid_review.py")
rv = importlib.util.module_from_spec(spec)
spec.loader.exec_module(rv)


def _row(rep, i):
    return rep["steps"][i - 1]


def test_fixture_a_book_i1_all_valid_all_progress():
    rep = rv.review("I.1", rv.fixture_book_i1())
    assert rep["invalid_steps"] == 0
    for i in (1, 2, 3):
        r = _row(rep, i)
        assert r["valid"], (i, r)
        assert r["progress"] == "progress", (i, r)
    # d strictly decreases 3 -> 2 -> 1 -> 0 and the goal holds
    assert [_row(rep, i)["d_after"] for i in (1, 2, 3)] == [2, 1, 0]
    assert rep["goal_held"] is True
    assert rep["remaining_distance"] == 0


def test_fixture_b_useless_step_valid_no_progress():
    rep = rv.review("I.1", rv.fixture_i1_useless_step())
    assert rep["invalid_steps"] == 0
    useless = _row(rep, 2)
    assert useless["rule"].startswith("line_from_points")
    assert useless["valid"] is True
    # the label the code could get wrong: d unchanged
    assert useless["progress"] == "no-progress"
    assert useless["d_before"] == useless["d_after"] == 2
    # the other steps are unchanged relative to fixture (a)
    assert _row(rep, 1)["progress"] == "progress"
    assert _row(rep, 3)["progress"] == "progress"
    assert _row(rep, 4)["progress"] == "progress"
    assert [_row(rep, i)["d_after"] for i in (1, 3, 4)] == [2, 1, 0]
    assert rep["goal_held"] is True
    assert rep["remaining_distance"] == 0


def test_fixture_c_diagram_trusting_step_invalid():
    rep = rv.review("I.1", rv.fixture_i1_diagram_trusting())
    assert rep["invalid_steps"] == 1
    bad = _row(rep, 2)
    assert bad["rule"].startswith("intersection_circles")
    assert bad["valid"] is False
    # review fix (claude-9): ACE does not exist yet, and the reason says so
    assert bad["why"] == "uses ACE before it exists"
    assert bad["progress"] == "invalid"
    assert "without it" in bad["state_note"]
    # later steps are still reviewed against the state WITHOUT the bad step
    for i in (3, 4):
        r = _row(rep, i)
        assert r["valid"] is True, (i, r)
        assert r["progress"] == "progress", (i, r)
    assert rep["goal_held"] is True
    assert rep["remaining_distance"] == 0


def test_fixture_d_i3_prop2_omitted_first_invalid_identified():
    rep = rv.review("I.3", rv.fixture_i3_prop2_omitted())
    assert rep["invalid_steps"] >= 1
    first = _row(rep, 1)
    assert first["rule"].startswith("circle_from_points")
    assert first["valid"] is False
    # the point d was never constructed
    assert first["why"] == "uses d before it exists"
    assert rep["goal_held"] is False
    assert rep["remaining_distance"] == 3  # untouched root state


def test_object_used_before_it_exists():
    """Review fix (claude-9): a step naming a circle nobody constructed is
    reported as using it before it exists, not as a missing premise."""
    rep = rv.review("I.1", [
        ("circle_from_points", ("a", "b"), {"C": "BCD"}),
        ("intersection_circles", ("BCD", "ZZZ"), {"c": "c"}),
    ])
    bad = _row(rep, 2)
    assert bad["valid"] is False
    assert bad["why"] == "uses ZZZ before it exists"
    assert _row(rep, 1)["valid"] is True


def test_diagram_trusting_intersection_of_existing_circles():
    """Review addition (claude-9): both circles exist and meet in a typical
    picture of I.2 (centre b through c; centre a through b), but whether they
    meet depends on the lengths, so the engine must refuse the intersection
    and name intersects_cc."""
    rep = rv.review("I.2", [
        ("circle_from_points", ("b", "c"), {"C": "CGH"}),
        ("circle_from_points", ("a", "b"), {"C": "ABX"}),
        ("intersection_circles", ("CGH", "ABX"), {"c": "p"}),
    ], max_depth=1, node_budget=200)
    assert _row(rep, 1)["valid"] and _row(rep, 2)["valid"]
    bad = _row(rep, 3)
    assert bad["valid"] is False
    assert "intersects_cc" in bad["why"], bad["why"]
