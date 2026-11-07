#!/usr/bin/env python3
"""Tests for euclid_review_export.py (E-euclid-tutor viewer review mode).

The exported badge for every row must equal what review() actually said
(valid / progress), and the drawn set must be exactly the valid=True rows.
Real engine runs, no mocks. Run from scripts/:
python3 -m pytest euclid_review_export_test.py -q
"""

import importlib.util
import os
from pathlib import Path

_spec = importlib.util.spec_from_file_location(
    "euclid_review_export", Path(__file__).resolve().parent
    / "euclid_review_export.py")
ex = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(ex)


def _export(name):
    return ex.export_fixture(name), ex.rv.review(
        ex.rv.FIXTURE_PROP[name], ex.rv.FIXTURES[name]())


def test_badges_equal_review_rows():
    for name in ex.rv.FIXTURES:
        trace, rep = _export(name)
        assert len(trace["steps"]) == len(rep["steps"])
        for row, orig in zip(trace["steps"], rep["steps"]):
            assert row["valid"] == orig["valid"], (name, row, orig)
            assert row["progress"] == orig["progress"], (name, row, orig)
            assert row["why"] == orig["why"]
            # the badge is a pure function of the row's (valid, progress)
            assert row["badge"] == ex.badge_of(orig)


def test_drawn_set_is_exactly_valid_steps():
    for name in ex.rv.FIXTURES:
        trace, rep = _export(name)
        drawn = [r["n"] for r in trace["steps"] if r["drawn"]]
        valid = [r["step"] for r in rep["steps"] if r["valid"] is True]
        assert drawn == valid, (name, drawn, valid)


def test_objects_only_defined_at_drawn_steps():
    """No object may cite a step that was not applied (it would be drawn
    from a state the engine never reached — the review's whole point)."""
    for name in ex.rv.FIXTURES:
        trace, rep = _export(name)
        drawn = {r["n"] for r in trace["steps"] if r["drawn"]}
        for o in trace["objects"]:
            if o["def_step"] == 0:
                continue  # hypothesis object
            assert o["def_step"] in drawn, (name, o)


def test_bad_case_invalid_fixture_marks_invalid_and_draws_nothing():
    """The exact case the mode exists for: i1-diagram-trusting has an
    invalid step (uses ACE before it exists); that step draws nothing and
    is badged invalid, while the Book steps around it still draw."""
    trace, rep = _export("i1-diagram-trusting")
    bad = trace["steps"][1]
    assert bad["badge"] == "invalid"
    assert bad["drawn"] is False
    assert rep["invalid_steps"] == 1
    assert trace["summary"]["steps_drawn"] == 3


def test_badge_classes():
    assert ex.badge_of({"valid": True, "progress": "progress"}) == \
        "valid-progress"
    assert ex.badge_of({"valid": True, "progress": "no-progress"}) == \
        "valid-no-progress"
    assert ex.badge_of({"valid": True, "progress": "unknown"}) == "unknown"
    assert ex.badge_of({"valid": False, "progress": "invalid"}) == "invalid"
    for p in ("undecided", "engine-error", "blocked"):
        assert ex.badge_of({"valid": None, "progress": p}) == "unknown"


def test_written_file_round_trips(tmp_path):
    """The JSON on disk carries the same rows as export_fixture."""
    for name in ex.rv.FIXTURES:
        trace, rep = _export(name)
        path = tmp_path / ("review-%s.json" % name)
        import json
        path.write_text(json.dumps(trace))
        back = json.loads(path.read_text())
        for row, orig in zip(back["steps"], rep["steps"]):
            assert row["valid"] == orig["valid"]
            assert row["progress"] == orig["progress"]
