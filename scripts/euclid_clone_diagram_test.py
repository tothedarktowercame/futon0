#!/usr/bin/env python3
"""Tests for euclid_clone_diagram.py (E-euclid-tutor slice 2).

Real engine, no mocks. Run from scripts/:
python3 -m pytest euclid_clone_diagram_test.py -q
"""

import importlib.util
import pytest
from pathlib import Path

spec = importlib.util.spec_from_file_location(
    "euclid_clone_diagram",
    Path(__file__).resolve().parent / "euclid_clone_diagram.py")
cd = importlib.util.module_from_spec(spec)
spec.loader.exec_module(cd)

ec, rv, srch, eng, tr = cd.ec, cd.rv, cd.srch, cd.eng, cd.tr

I2_STEPS = [("circle_from_points", ("b", "c"), {"C": "CGH"}),
            ("circle_from_points", ("a", "b"), {"C": "ABX"}),
            ("intersection_circles", ("CGH", "ABX"), {"c": "p"})]


def _after(prop, steps):
    state = rv.initial_state(prop)
    for i, s in enumerate(steps, start=1):
        eng.apply_step(state, i, s[0], s[1], s[2])
    return state


def test_a_trusting_clone_accepts_picture_true_intersection():
    tc = cd.TrustingClone("trusting", "I.2")
    # the fixture is not vacuous: check the picture first — in the I.2
    # placement (FREE: a=(0.3,0.9), b=(0,0), c=(1,0)) the circles centred
    # b through c (r=1) and a through b (r~0.95) DO meet (centres ~0.95
    # apart, |r1-r2| ~ 0.05 < 0.95 < r1+r2)
    state2 = _after("I.2", I2_STEPS[:2])
    # circles exist in the clone's picture only after placement; place them
    tc.note_step("I.2", rv.initial_state("I.2"), I2_STEPS[0])
    tc.note_step("I.2", _after("I.2", I2_STEPS[:1]), I2_STEPS[1])
    assert tc.holds_in_picture(("intersects_cc", "CGH", "ABX")) is True
    # the full engine refuses the intersection and names intersects_cc
    try:
        eng.apply_step(srch.clone_state(state2), 3, *I2_STEPS[2][:2],
                       I2_STEPS[2][2])
        raise AssertionError("engine accepted the unprovable intersection")
    except eng.MissingPremise as e:
        assert e.premise == ("intersects_cc", "CGH", "ABX"), e
    # the trusting clone accepts it and records the trusted fact
    rep = tc.apply_step(srch.clone_state(state2), 3, *I2_STEPS[2][:2],
                        I2_STEPS[2][2])
    assert ("on_circle", "p", "CGH") in rep["added"], rep["added"]
    assert ("on_circle", "p", "ABX") in rep["added"], rep["added"]
    assert tc.trust_log == [
        "trusted from the picture: ('intersects_cc', 'CGH', 'ABX')"], \
        tc.trust_log


def test_b_picture_false_premise_still_refused():
    # a different picture: c far away (r_bc = 5) so the circle centred b
    # through c swallows the circle centred a through b — they do NOT meet
    xy = {"a": (0.3, 0.9), "b": (0.0, 0.0), "c": (5.0, 0.0)}
    tc = cd.TrustingClone("trusting", "I.2", xy=xy)
    state2 = _after("I.2", I2_STEPS[:2])
    tc.note_step("I.2", rv.initial_state("I.2"), I2_STEPS[0])
    tc.note_step("I.2", _after("I.2", I2_STEPS[:1]), I2_STEPS[1])
    assert tc.holds_in_picture(("intersects_cc", "CGH", "ABX")) is False
    try:
        tc.apply_step(srch.clone_state(state2), 3, *I2_STEPS[2][:2],
                      I2_STEPS[2][2])
        raise AssertionError("trusting clone accepted a picture-false "
                             "premise")
    except eng.MissingPremise as e:
        assert e.premise == ("intersects_cc", "CGH", "ABX")
    assert tc.trust_log == []


def test_c_envelope_posterior_favours_the_trusting_clone():
    env = ec.standard_envelope()
    tc = cd.TrustingClone("trusting", "I.2")
    env.clones.append(tc)
    env.active.append(tc)
    state = rv.initial_state("I.2")
    for i, step in enumerate(I2_STEPS, start=1):
        ev = env.observe("I.2", state, step)
        tc.note_step("I.2", state, step)  # advance the clone's picture
        if i < 3:
            eng.apply_step(state, i, step[0], step[1], step[2])
    post = ev["posterior"]
    # only the trusting clone predicted the diagram-trusting step ...
    assert ev["predicted_by"] == ["trusting"], ev
    # ... and it ends with the highest posterior
    assert post["trusting"] == max(post.values()), post
    assert post["trusting"] > post["full-engine"], post


def test_d_trusting_predicts_at_least_the_full_engine_on_book_i1():
    full = ec.Clone("full-engine")
    tc = cd.TrustingClone("trusting", "I.1")
    root = rv.initial_state("I.1")
    assert full.predict("I.1", root) <= tc.predict("I.1", root)
    # and concretely: both Book circles are among the trusting predictions
    pred = tc.predict("I.1", root)
    assert ("circle_from_points", ("a", "b")) in pred
    assert ("circle_from_points", ("b", "a")) in pred


def test_refused_step_leaves_no_trust_behind():
    """Review fix (claude-9): I.2, line AB and circle CGH (centre b through
    c), then intersection_circle_line_between_points(CGH, AB, a, b). The
    picture has a inside CGH (trusted) but b is the centre, so the outside
    premise fails and the step is refused. Before the fix the trusted
    inside fact stayed in the state and in trust_log."""
    # explicit picture (a inside CGH): the default I.2 placement moved in
    # 94fe35f, and this test is about the refusal, not the default picture
    tc = cd.TrustingClone("trusting", "I.2",
                          xy={"a": (0.3, 0.9), "b": (0.0, 0.0), "c": (1.0, 0.0)})
    s = cd.rv.initial_state("I.2")
    tc.apply_step(s, 1, "line_from_points", ("a", "b"), {"L": "AB"})
    tc.apply_step(s, 2, "circle_from_points", ("b", "c"), {"C": "CGH"})
    assert tc.holds_in_picture(("inside", "a", "CGH")) is True
    before = dict(s.facts)
    with pytest.raises(cd.eng.MissingPremise):
        tc.apply_step(s, 3, "intersection_circle_line_between_points",
                      ("CGH", "AB", "a", "b"), {"e": "e"})
    assert s.facts == before
    assert tc.trust_log == []


# -- picture failures must surface (review, claude-9) -------------------------

def test_place_step_failure_is_recorded_not_hidden(monkeypatch):
    """(a) tr.place_step raising once: the engine result is still returned,
    picture_errors has the entry, the picture is stale, and a later
    trusted-kind premise is NOT trusted (refused)."""
    tc = cd.TrustingClone("trusting", "I.2")

    def boom(*a, **k):
        raise RuntimeError("placement exploded")

    monkeypatch.setattr(cd.tr, "place_step", boom)
    rep = tc.apply_step(cd.rv.initial_state("I.2"), 1, I2_STEPS[0][0],
                        I2_STEPS[0][1], I2_STEPS[0][2])
    assert ("on_circle", "c", "CGH") in rep["added"], rep["added"]
    assert len(tc.picture_errors) == 1, tc.picture_errors
    assert tc.picture_errors[0][0] == "circle_from_points"
    assert "placement exploded" in tc.picture_errors[0][2]
    assert tc.picture_stale is True
    # stale picture -> no trust: the picture-true intersection is refused
    assert tc.holds_in_picture(("intersects_cc", "CGH", "ABX")) is None
    s2 = _after("I.2", I2_STEPS[:2])
    with pytest.raises(cd.eng.MissingPremise):
        tc.apply_step(s2, 3, I2_STEPS[2][0], I2_STEPS[2][1], I2_STEPS[2][2])
    assert tc.trust_log == []


def test_missing_point_is_none_not_false():
    """(3) 'not in the picture' is None, not False: a fact about a point
    the picture has not placed answers None."""
    tc = cd.TrustingClone("trusting", "I.2")
    assert tc.holds_in_picture(("intersects_cc", "CGH", "ABX")) is None
    assert tc.holds_in_picture(("same_side", "z", "q", "BC")) is None
    assert tc.holds_in_picture(("inside", "z", "CGH")) is None


def test_i4_clone_advances_picture_beyond_the_old_dict():
    """(2) the old {"I.1":1,...}[self.prop] raised KeyError (hidden by the
    bare except) so the I.4 picture never advanced. euclid_trace now has
    I.4/I.5 placements; the clone builds and the superposition step's
    primed points land in the picture."""
    tc = cd.TrustingClone("trusting", "I.4")
    assert tc.prop_n == 4
    s = cd.eng._metric_state(cd.eng.PROP4_HYPS_DATA)
    rule, args, bindings = cd.eng.PROP4_STEPS_DATA[0]
    rep = tc.apply_step(s, 1, rule, args, bindings)
    assert any(f[0] == "eq_ang" for f in rep["added"])
    assert tc.picture_stale is False, tc.picture_errors
    assert tc.picture_errors == []
    assert "b'" in tc.xy and "c'" in tc.xy


def test_unsupported_proposition_fails_loudly():
    """No euclid_trace placement for I.9: ValueError, not a silent
    fallback to some other proposition's picture."""
    with pytest.raises(ValueError):
        cd.TrustingClone("trusting", "I.9")
    with pytest.raises(ValueError):
        cd.TrustingClone("trusting", "I.x")
