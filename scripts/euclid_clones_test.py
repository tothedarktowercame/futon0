#!/usr/bin/env python3
"""Tests for euclid_clones.py (E-euclid-tutor clone envelope v0).

Real engine, no mocks (the single sanctioned exception is test (d), which
monkeypatches the Book step data to prove predict() never reads it).
Run from scripts/: python3 -m pytest euclid_clones_test.py -q
"""

import importlib.util
from pathlib import Path

spec = importlib.util.spec_from_file_location(
    "euclid_clones", Path(__file__).resolve().parent / "euclid_clones.py")
ec = importlib.util.module_from_spec(spec)
spec.loader.exec_module(ec)

rv, srch, eng = ec.rv, ec.srch, ec.eng


def _advance(state, step, i=1):
    trial = srch.clone_state(state)
    eng.apply_step(trial, i, step[0], step[1], step[2])
    return trial


def test_a_book_i1_full_engine_hits_all_three():
    env = ec.standard_envelope()
    full = env.clones[0]
    assert full.name == "full-engine"
    state = rv.initial_state("I.1")
    for i, step in enumerate(rv.fixture_book_i1(), start=1):
        pred = full.predict("I.1", state)
        assert ec.canon_step(step[0], step[1]) in pred, (i, pred)
        ev = env.observe("I.1", state, step)
        assert full.name in ev["predicted_by"], (i, ev)
        state = _advance(state, step, i)
    assert (full.hits, full.observations) == (3, 3)
    # the no-circle clone predicts nothing at the root and is ruled out at
    # step 1 (its recorded predictions at the root are empty)
    noc = env.clones[1]
    assert noc.predict("I.1", rv.initial_state("I.1")) == set()
    assert noc.name in env.events[0]["ruled_out"]  # ruled out at step 1


def test_b_useless_first_step_no_clone_predicted_all_kept():
    env = ec.standard_envelope()
    state = rv.initial_state("I.1")
    useless = ("line_from_points", ("a", "b"), {"L": "AB"})
    ev = env.observe("I.1", state, useless)
    assert ev["no_clone_predicted_this_step"] is True
    assert ev["predicted_by"] == []
    assert set(ev["ruled_out"]) == {c.name for c in env.clones}
    # silent emptying is not allowed: all clones are still in the envelope
    assert {c.name for c in env.active} == {c.name for c in env.clones}


def test_c_depth1_clone_predicts_nothing_at_root_intersection_at_d1():
    env = ec.standard_envelope()
    d1 = env.clones[3]
    assert d1.name == "depth-1" and d1.max_depth == 1
    state = rv.initial_state("I.1")
    assert d1.predict("I.1", state) == set()  # goal is 3 moves away
    state = _advance(state, ("circle_from_points", ("a", "b"), {"C": "BCD"}))
    state = _advance(state, ("circle_from_points", ("b", "a"), {"C": "ACE"}),
                     i=2)
    pred = d1.predict("I.1", state)  # one move remains
    assert ec.canon_step("intersection_circles", ("BCD", "ACE")) in pred
    assert len(pred) >= 1


def test_d_predict_does_not_read_the_book_steps(monkeypatch):
    env = ec.standard_envelope()
    state = rv.initial_state("I.1")
    before = {c.name: c.predict("I.1", state) for c in env.clones}
    # garbage the Book step data: predictions must be unchanged
    monkeypatch.setattr(eng, "PROP1_STEPS_DATA", [("not_a_rule", (1, 2), {})],
                        raising=True)
    monkeypatch.setattr(eng, "PROP2_STEPS_DATA", None, raising=True)
    monkeypatch.setattr(eng, "PROP3_STEPS_DATA", [("x", ("y",), {})],
                        raising=True)
    after = {c.name: c.predict("I.1", state) for c in env.clones}
    assert before == after
