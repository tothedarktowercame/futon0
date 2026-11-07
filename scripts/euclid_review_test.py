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


# -- an engine failure is not a verdict (review fix, claude-9) -----------------

def _patched_engine(monkeypatch, exc, when):
    """Wrap rv.eng.apply_step so that `when(state, i, rule, args)` triggers
    `exc`; the review's Distance BFS calls apply_step too, so gate narrowly."""
    orig = rv.eng.apply_step

    def fake(state, i, rule, args, bindings):
        if when(rule, args):
            raise exc
        return orig(state, i, rule, args, bindings)

    monkeypatch.setattr(rv.eng, "apply_step", fake)


def test_engine_runtime_error_is_not_invalid(monkeypatch):
    """(a) apply_step raising RuntimeError on the reviewed step: the row is
    valid=None / engine-error (never 'invalid'), n_invalid unchanged, the
    error text is visible, and the report is marked incomplete. Injected
    on the useless line step so the other steps stay reviewable."""
    _patched_engine(
        monkeypatch, RuntimeError("engine bug!"),
        when=lambda rule, args: rule == "line_from_points"
        and args == ("a", "b"))
    steps = rv.fixture_i1_useless_step()
    rep = rv.review("I.1", steps)
    row = _row(rep, 2)
    assert row["valid"] is None
    assert row["progress"] == "engine-error"
    assert "engine bug!" in row["why"]
    assert rep["invalid_steps"] == 0
    assert rep["engine_errors"] == 1
    assert rep["incomplete"] is True
    # the OTHER steps are still reviewed (that is why we emit a row)
    assert _row(rep, 1)["valid"] is True
    assert _row(rep, 3)["valid"] is True


def test_undecided_fm_too_large_is_not_invalid(monkeypatch):
    """(b) eng.FMTooLarge (undecided arithmetic): valid=None /
    progress='undecided', counted in undecided_steps, not invalid_steps."""
    _patched_engine(
        monkeypatch, rv.eng.FMTooLarge(999),
        when=lambda rule, args: rule == "line_from_points"
        and args == ("a", "b"))
    rep = rv.review("I.1", rv.fixture_i1_useless_step())
    row = _row(rep, 2)
    assert row["valid"] is None
    assert row["progress"] == "undecided"
    assert "could not decide" in row["why"]
    assert rep["undecided_steps"] == 1
    assert rep["invalid_steps"] == 0
    assert rep["engine_errors"] == 0


def test_formatter_never_shows_none_as_invalid(monkeypatch):
    """The formatted report says UNDECIDED / ENGINE ERROR and declares the
    review incomplete; it can never print INVALID for valid=None."""
    _patched_engine(
        monkeypatch, RuntimeError("boom"),
        when=lambda rule, args: rule == "line_from_points"
        and args == ("a", "b"))
    text = rv.format_report(rv.review("I.1", rv.fixture_i1_useless_step()))
    assert "ENGINE ERROR" in text
    assert "REVIEW INCOMPLETE" in text
    row2 = [l for l in text.splitlines() if l.strip().startswith("2.")][0]
    assert "INVALID" not in row2


def test_distance_exception_is_not_silent_no_progress(monkeypatch):
    """A crash inside the Distance BFS is recorded in dist.errors and
    skipped; it never converts a distance into a fabricated value, and
    None still reads as 'unknown', not 'no-progress'."""
    _patched_engine(
        monkeypatch, KeyError("bogus"),
        when=lambda rule, args: rule == "line_from_points"
        and args == ("a", "b"))
    rep = rv.review("I.1", rv.fixture_i1_useless_step())
    assert rep["invalid_steps"] == 0
    # review (claude-9): this was under `if rep["distance_move_errors"]:`,
    # which passes vacuously if the crash never fires
    assert rep["distance_move_errors"] > 0
    assert rep["incomplete"] is True


def test_tainted_distance_never_labels_no_progress(monkeypatch):
    """Review (claude-9): a crashed move skipped inside the BFS can only
    make a distance too LARGE, so a finite but inflated d must not produce
    "no-progress" (or "progress"). Fake a search that records a crash and
    returns the same finite d everywhere: untainted, every Book I.1 step
    would read no-progress (3 -> 3)."""
    def fake_search(self, state):
        self.errors.append(("circle_from_points", ("b", "a"), "KeyError"))
        return 3

    monkeypatch.setattr(rv.Distance, "_search", fake_search)
    rep = rv.review("I.1", rv.fixture_book_i1())
    labels = [r["progress"] for r in rep["steps"]]
    assert labels == ["unknown"] * 3, labels
    assert rep["incomplete"] is True
