#!/usr/bin/env python3
"""Tests for euclid_clones.py (E-euclid-tutor clone envelope).

Slice 3 (likelihood scoring): probabilities per observed step, per-clone
log-likelihood, normalised posterior, ruled-out = below threshold.
Real engine, no mocks (the single sanctioned exception is the leakage
guard, which monkeypatches the Book step data).
Run from scripts/: python3 -m pytest euclid_clones_test.py -q
"""

import importlib.util
from pathlib import Path

spec = importlib.util.spec_from_file_location(
    "euclid_clones", Path(__file__).resolve().parent / "euclid_clones.py")
ec = importlib.util.module_from_spec(spec)
spec.loader.exec_module(ec)

rv, srch, eng = ec.rv, ec.srch, ec.eng

I2_STEPS = [("circle_from_points", ("b", "c"), {"C": "CGH"}),
            ("circle_from_points", ("a", "b"), {"C": "ABX"}),
            ("intersection_circles", ("CGH", "ABX"), {"c": "p"})]


def _advance(state, step, i=1):
    trial = srch.clone_state(state)
    eng.apply_step(trial, i, step[0], step[1], step[2])
    return trial


def test_a_book_i1_full_engine_highest_posterior():
    spec2 = importlib.util.spec_from_file_location(
        "cdg_a", Path(__file__).resolve().parent / "euclid_clone_diagram.py")
    cdg = importlib.util.module_from_spec(spec2)
    spec2.loader.exec_module(cdg)
    env = ec.standard_envelope()
    tc = cdg.TrustingClone("trusting", "I.1")
    env.clones.append(tc)
    env.active.append(tc)
    full = env.clones[0]
    assert full.name == "full-engine"
    state = rv.initial_state("I.1")
    for i, step in enumerate(rv.fixture_book_i1(), start=1):
        ev = env.observe("I.1", state, step)
        assert full.name in ev["predicted_by"], (i, ev)
        tc.note_step("I.1", state, step)  # advance the trusting picture
        state = _advance(state, step, i)
    assert (full.hits, full.observations) == (3, 3)
    post = env.events[-1]["posterior"]
    # the full engine ends with the highest posterior, and the trusting
    # clone — which ties it on hits — must NOT tie it on posterior
    assert post["full-engine"] == max(post.values()), post
    assert post["full-engine"] > post["trusting"], post


def test_b_useless_first_step_no_clone_predicted_all_kept():
    env = ec.standard_envelope()
    state = rv.initial_state("I.1")
    useless = ("line_from_points", ("a", "b"), {"L": "AB"})
    ev = env.observe("I.1", state, useless)
    assert ev["no_clone_predicted_this_step"] is True
    assert ev["predicted_by"] == []
    # silent emptying is not allowed: all clones are still in the envelope
    # (nobody fell below threshold after one step either)
    assert {c.name for c in env.active} == {c.name for c in env.clones}
    assert ev["below_threshold"] == []


def test_c_depth1_guesing_not_ruled_out_by_step_one():
    env = ec.standard_envelope()
    d1 = env.clones[3]
    assert d1.name == "depth-1" and d1.max_depth == 1
    root = rv.initial_state("I.1")
    assert d1.predict("I.1", root) == set()  # goal is 3 moves away
    # a guessing clone spreads mass uniformly over its legal moves
    step1 = ("circle_from_points", ("a", "b"), {"C": "BCD"})
    canonical = ec.canon_step(step1[0], step1[1])
    legal = len(srch.legal_moves(root, d1.rules("I.1")))
    assert d1.probability("I.1", root, canonical) == 1.0 / legal
    # ... so it is NOT ruled out by a step it could have guessed
    ev = env.observe("I.1", root, step1)
    assert d1.name not in ev["below_threshold"], ev
    assert d1 in env.active
    assert env.events[-1]["probabilities"][d1.name] == 1.0 / legal


def test_d_eps_field_zero_posterior_never_silent_empty():
    # eps is a constructor FIELD: with eps = 0 a miss has probability 0,
    # the posterior shows 0.0, and the envelope never silently empties
    full = ec.Clone("full-engine", eps=0.0)
    env = ec.Envelope([full])
    root = rv.initial_state("I.1")
    useless = ("line_from_points", ("a", "b"), {"L": "AB"})
    assert env.clones[0].eps == 0.0
    ev = env.observe("I.1", root, useless)
    assert ev["probabilities"]["full-engine"] == 0.0
    assert ev["posterior"]["full-engine"] == 0.0
    assert ev["ruled_out"] == ["full-engine"]
    assert ev["all_below_threshold"] is True
    # not silently emptied
    assert [c.name for c in env.active] == ["full-engine"]


def test_e_predict_does_not_read_the_book_steps(monkeypatch):
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
