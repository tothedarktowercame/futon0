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
    # the missing premise is named: intersects_cc BCD ACE (no second circle)
    assert "('intersects_cc', 'BCD', 'ACE')" in bad["why"]
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
    # the point d was never constructed: neq a d is not derivable
    assert "('neq', 'a', 'd')" in first["why"]
    assert rep["goal_held"] is False
    assert rep["remaining_distance"] == 3  # untouched root state
