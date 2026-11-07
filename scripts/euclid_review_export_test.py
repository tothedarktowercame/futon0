#!/usr/bin/env python3
"""Tests for euclid_review_export.py (E-euclid-tutor viewer review mode).

The exported badge for every row must equal what review() actually said
(valid / progress), and the drawn set must be exactly the valid=True rows.
Real engine runs, no mocks. Run from scripts/:
python3 -m pytest euclid_review_export_test.py -q
"""

import functools
import importlib.util
import os
from pathlib import Path

_spec = importlib.util.spec_from_file_location(
    "euclid_review_export", Path(__file__).resolve().parent
    / "euclid_review_export.py")
ex = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(ex)


@functools.lru_cache(maxsize=None)
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


# -- envelope posterior (viewer review mode, packet P-euclid-viewer-envelope) -

def test_envelope_posteriors_sum_to_one():
    """(a) per step the posteriors sum to 1 (1e-9), unless every clone's
    likelihood was zero — in which case they are all zero (the row's
    posterior says so, and all_below_threshold records the event)."""
    for name in ex.rv.FIXTURES:
        trace, _ = _export(name)
        for row in trace["steps"]:
            e = row["envelope"]
            assert set(e["posterior"]) == set(e["probabilities"]), row
            total = sum(e["posterior"].values())
            all_zero = all(p == 0.0 for p in e["probabilities"].values())
            if all_zero:
                assert all(v == 0.0 for v in e["posterior"].values())
            else:
                assert abs(total - 1.0) < 1e-9, (name, row["n"], total)


def test_invalid_step_raises_trusting_lowers_full_engine():
    """(b) in i1-diagram-trusting the INVALID step 2 (uses ACE before it
    exists) raises the trusting clone's posterior and lowers the full
    engine's. Measured numbers, not adjusted: 0.2099 -> 0.475 (trusting),
    0.3149 -> 0.2375 (full-engine)."""
    trace, _ = _export("i1-diagram-trusting")
    before, after = trace["steps"][0]["envelope"], \
        trace["steps"][1]["envelope"]
    assert after["posterior"]["trusting"] > before["posterior"]["trusting"]
    assert after["posterior"]["full-engine"] < \
        before["posterior"]["full-engine"]


def test_book_i1_full_engine_ends_highest():
    """(c) in book-i1 the full engine ends with the highest posterior."""
    trace, _ = _export("book-i1")
    last = trace["steps"][-1]["envelope"]["posterior"]
    assert last["full-engine"] == max(last.values()), last


def test_exported_envelope_equals_fresh_replay():
    """(d) no copy drift: replaying a fresh envelope over the review's own
    state sequence (observe before any advance; note_step only for the
    steps review applied) reproduces the stored dicts exactly."""
    name = "i1-diagram-trusting"  # has both an applied and an invalid step
    trace, rep = _export(name)
    prop = ex.rv.FIXTURE_PROP[name]
    steps = ex.rv.FIXTURES[name]()
    env = ex.make_envelope(prop)
    tc = env.clones[-1]
    state = ex.rv.initial_state(prop)
    for step, row, out in zip(steps, rep["steps"], trace["steps"]):
        ev = env.observe(prop, state, step)
        assert out["envelope"]["posterior"] == ev["posterior"], row["step"]
        assert out["envelope"]["probabilities"] == ev["probabilities"]
        assert out["envelope"]["below_threshold"] == ev["below_threshold"]
        assert out["envelope"]["all_below_threshold"] == \
            ev["all_below_threshold"]
        if row["valid"] is True:
            tc.note_step(prop, state, step)
            trial = ex.rv.srch.clone_state(state)
            ex.rv.eng.apply_step(trial, row["step"], step[0], step[1],
                                 step[2])
            state = trial


def test_badge_classes():
    assert ex.badge_of({"valid": True, "progress": "progress"}) == \
        "valid-progress"
    assert ex.badge_of({"valid": True, "progress": "no-progress"}) == \
        "valid-no-progress"
    assert ex.badge_of({"valid": True, "progress": "unknown"}) == "unknown"
    assert ex.badge_of({"valid": False, "progress": "invalid"}) == "invalid"
    for p in ("undecided", "engine-error", "blocked"):
        assert ex.badge_of({"valid": None, "progress": p}) == "unknown"


def test_committed_traces_match_a_fresh_export():
    """Review (claude-9): the viewer serves the committed traces/*.json, and
    the old round-trip test only read back a file it had just written, so a
    hand-edited or stale committed file (e.g. an INVALID row flipped to
    VALID) passed every test. Compare each committed file with a fresh
    export; regenerate with `python3 euclid_review_export.py`."""
    import json
    for name in ex.rv.FIXTURES:
        trace, _ = _export(name)
        path = Path(ex.OUT_DIR) / ("review-%s.json" % name)
        assert path.exists(), path
        on_disk = json.loads(path.read_text())
        fresh = json.loads(json.dumps(trace))
        assert on_disk == fresh, (
            "%s is stale or edited; rerun euclid_review_export.py" % path)
