"""Tests for euclid_engine.py — replays of LeanEuclid Book I.1-I.3.
Run from ~/code/futon0: python3 -m pytest scripts/euclid_engine_test.py -q
"""

import importlib.util
from pathlib import Path
import pytest

spec = importlib.util.spec_from_file_location(
    "euclid_engine", Path(__file__).resolve().parents[1] / "scripts/euclid_engine.py")
eng = importlib.util.module_from_spec(spec)
spec.loader.exec_module(eng)


# --- I.1 (Book/Prop01.lean:5) ----------------------------------------------
# theorem proposition_1 : distinctPointsOnLine a b AB ->
#   exists c, |c-a| = |a-b| and |c-b| = |a-b|
def prop1_state():
    s = eng.State()
    s.assume(("on_line", "a", "AB"), "hypothesis")
    s.assume(("on_line", "b", "AB"), "hypothesis")
    s.assume(("neq", "a", "b"), "hypothesis")
    return s


PROP1_STEPS = [
    ("circle_from_points", ("a", "b"), {"C": "BCD"}),
    ("circle_from_points", ("b", "a"), {"C": "ACE"}),
    ("intersection_circles", ("BCD", "ACE"), {"c": "c"}),
    ("point_on_circle_onlyif", ("a", "b", "c", "BCD"), {}),
    ("point_on_circle_onlyif", ("b", "a", "c", "ACE"), {}),
]


def test_prop1_replay():
    s = eng.State()
    s.assume(("on_line", "a", "AB"), "hypothesis")
    s.assume(("on_line", "b", "AB"), "hypothesis")
    s.assume(("neq", "a", "b"), "hypothesis")
    eng.close(s)
    report = []
    for i, (rule, args, b) in enumerate(PROP1_STEPS, start=1):
        # point_on_circle_onlyif is a forward rule, applied as a named step:
        # run closure and assert the rule's conclusion appeared.
        if rule == "point_on_circle_onlyif":
            a0, b0, c0, alpha = args
            before = len(s.facts)
            eng.close(s)
            # premises: centre a alpha, on_circle b alpha, on_circle c alpha
            for pf in (("centre", a0, alpha), ("on_circle", b0, alpha),
                       ("on_circle", c0, alpha)):
                assert s.has(pf), f"step {i}: premise {pf} not derivable"
            assert eng.la_entails_eq(s.la, a0, c0, a0, b0), \
                f"step {i}: radii equality not derived"
            report.append({"step": i, "rule": rule, "premises": [
                ("centre", a0, alpha), ("on_circle", b0, alpha),
                ("on_circle", c0, alpha)]})
        else:
            report.append(eng.apply_step(s, i, rule, args, b))
    # conclusion: exists c, |c-a| = |a-b| and |c-b| = |a-b|
    assert s.has(("on_circle", "c", "BCD")) and s.has(("on_circle", "c", "ACE"))
    assert eng.la_entails_eq(s.la, "c", "a", "a", "b")
    assert eng.la_entails_eq(s.la, "c", "b", "a", "b")
    assert len(report) == 5


# --- I.2 (Book/Prop02.lean:5) ----------------------------------------------
PROP2_HYPS = [
    (("on_line", "b", "BC"), "hypothesis"),
    (("on_line", "c", "BC"), "hypothesis"),
    (("neq", "b", "c"), "hypothesis"),
    (("neq", "a", "b"), "hypothesis"),
]
PROP2_STEPS = [
    ("line_from_points", ("a", "b"), {"L": "AB"}),
    ("proposition_1", ("a", "b", "AB"), {"d": "d"}),
    ("line_from_points", ("d", "a"), {"L": "AE"}),
    ("line_from_points", ("d", "b"), {"L": "BF"}),
    ("circle_from_points", ("b", "c"), {"C": "CGH"}),
    ("intersection_circle_line_extending_points", ("CGH", "BF", "b", "d"),
     {"g": "g"}),
    ("circle_from_points", ("d", "g"), {"C": "GKL"}),
    ("intersection_circle_line_extending_points", ("GKL", "AE", "a", "d"),
     {"g": "l"}),
    ("point_on_circle_onlyif", ("b", "c", "g", "CGH"), {}),
    ("point_on_circle_onlyif", ("d", "l", "g", "GKL"), {}),
    ("between_if", ("l", "a", "d"), {}),
    ("between_if", ("g", "b", "d"), {}),
]


def _run_prop2(steps=PROP2_STEPS, hyps=PROP2_HYPS):
    """Shared driver: forward-rule steps are checked like the engine replays
    them — closure first, then premise check on the closed state."""
    s = eng.State()
    for f, w in hyps:
        s.assume(f, w)
    eng.close(s)
    for i, (rule, args, b) in enumerate(steps, start=1):
        if rule == "point_on_circle_onlyif":
            o, w, p, alpha = args
            for pf in (("centre", o, alpha), ("on_circle", w, alpha),
                       ("on_circle", p, alpha)):
                assert s.has(pf), f"step {i}: premise {pf} not derivable"
            assert eng.la_entails_eq(s.la, o, p, o, w)
        elif rule == "between_if":
            a, bb, c = args
            assert s.has(("between", a, bb, c)), \
                f"step {i}: premise between {args} not derivable"
        else:
            eng.apply_step(s, i, rule, args, b)
    return s


def test_prop2_replay():
    import time
    t0 = time.time()
    s = _run_prop2()
    dt = time.time() - t0
    assert eng.la_entails_eq(s.la, "a", "l", "b", "c"), \
        "|a-l| = |b-c| not entailed"
    assert dt < 10


# --- I.3 (Book/Prop03.lean:5) ----------------------------------------------
PROP3_HYPS = [
    (("on_line", "a", "AB"), "hypothesis"),
    (("on_line", "b", "AB"), "hypothesis"),
    (("neq", "a", "b"), "hypothesis"),
    (("on_line", "c0", "C"), "hypothesis"),
    (("on_line", "c1", "C"), "hypothesis"),
    (("neq", "c0", "c1"), "hypothesis"),
]
PROP3_STEPS = [
    ("proposition_2'", ("a", "c0", "c1", "C"), {"d": "d"}),
    ("circle_from_points", ("a", "d"), {"C": "DEF"}),
    ("intersection_circle_line_between_points", ("DEF", "AB", "a", "b"),
     {"e": "e"}),
    ("point_on_circle_onlyif", ("a", "d", "e", "DEF"), {}),
]


def _run_prop3(steps=PROP3_STEPS, hyps=PROP3_HYPS, with_gt=True):
    s = eng.State()
    for f, w in hyps:
        s.assume(f, w)
    if with_gt:  # hypothesis |a-b| > |c0-c1|
        s.la.add({eng.len_atom("a", "b"): 1,
                  eng.len_atom("c0", "c1"): -1}, 0, ">")
    eng.close(s)
    for i, (rule, args, b) in enumerate(steps, start=1):
        if rule == "point_on_circle_onlyif":
            o, w, p, alpha = args
            for pf in (("centre", o, alpha), ("on_circle", w, alpha),
                       ("on_circle", p, alpha)):
                assert s.has(pf), f"step {i}: premise {pf} not derivable"
            assert eng.la_entails_eq(s.la, o, p, o, w)
        else:
            eng.apply_step(s, i, rule, args, b)
    return s


def test_prop3_replay():
    s = _run_prop3()
    assert s.has(("between", "a", "e", "b")), "between a e b missing"
    assert eng.la_entails_eq(s.la, "a", "e", "c0", "c1"), \
        "|a-e| = |c0-c1| not entailed"


# --- Negative controls ------------------------------------------------------

def test_control_a_missing_circle_stops_replay():
    """I.1 without the circle_from_points for ACE: intersection_circles must
    stop and name the missing premise (intersects_cc BCD ACE)."""
    s = eng.State()
    s.assume(("on_line", "a", "AB"), "hypothesis")
    s.assume(("on_line", "b", "AB"), "hypothesis")
    s.assume(("neq", "a", "b"), "hypothesis")
    eng.close(s)
    eng.apply_step(s, 1, "circle_from_points", ("a", "b"), {"C": "BCD"})
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(s, 2, "intersection_circles", ("BCD", "ACE"), {"c": "c"})
    assert ei.value.premise == ("intersects_cc", "BCD", "ACE")
    assert ei.value.step == 2


def test_control_b_swapped_arguments_stop_replay():
    """I.2 with intersection_circle_line_extending_points (CGH BF d b) — the
    swapped 'inside' slot: d is not inside CGH (b is), so the replay must stop
    at that step and name ('inside', 'd', 'CGH')."""
    steps = list(PROP2_STEPS)
    idx = next(i for i, st in enumerate(steps)
               if st[0] == "intersection_circle_line_extending_points"
               and st[1][0] == "CGH")
    b, d = steps[idx][1][2], steps[idx][1][3]
    steps[idx] = (steps[idx][0], (steps[idx][1][0], steps[idx][1][1], d, b),
                  steps[idx][2])
    s = eng.State()
    for f, w in PROP2_HYPS:
        s.assume(f, w)
    eng.close(s)
    with pytest.raises(eng.MissingPremise) as ei:
        for i, (rule, args, bind) in enumerate(steps, start=1):
            if rule in ("point_on_circle_onlyif", "between_if"):
                # forward steps after the stuck point are never reached
                continue
            eng.apply_step(s, i, rule, args, bind)
    assert ei.value.premise == ("inside", "d", "CGH")


def test_control_c_diagram_trust_stops_replay():
    """proposition_1' (Book/Prop01.lean:24) style: the side-selecting
    intersection_opposite_side needs ('not_on_line', x, AB). If the hypothesis
    is omitted — it is exactly what the diagram would show — the replay must
    stop naming it; no rule invents a side fact."""
    s = eng.State()
    s.assume(("on_line", "a", "AB"), "hypothesis")
    s.assume(("on_line", "b", "AB"), "hypothesis")
    s.assume(("neq", "a", "b"), "hypothesis")
    # NOTE: hypothesis ¬(x.onLine AB) deliberately omitted
    eng.close(s)
    eng.apply_step(s, 1, "circle_from_points", ("a", "b"), {"C": "BCD"})
    eng.apply_step(s, 2, "circle_from_points", ("b", "a"), {"C": "ACE"})
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(s, 3, "intersection_opposite_side",
                       ("BCD", "ACE", "x", "a", "b", "AB"), {"a": "c"})
    assert ei.value.premise == ("not_on_line", "x", "AB")


def test_metric_arithmetic_bounds():
    """The arithmetic module: decides linear equalities/inequalities exactly;
    refuses nothing linear. |a-b| = 2, |b-c| = 3, between a b c gives |a-c|=5
    and |a-c| > |a-b|."""
    la = eng.LinearArithmetic()
    la.add({eng.len_atom("a", "b"): 1}, 2, "=")
    la.add({eng.len_atom("b", "c"): 1}, 3, "=")
    la.add({eng.len_atom("a", "b"): 1, eng.len_atom("b", "c"): 1,
            eng.len_atom("a", "c"): -1}, 0, "=")
    assert la.entails({eng.len_atom("a", "c"): 1}, 5, "=")
    assert la.entails({eng.len_atom("a", "c"): 1,
                       eng.len_atom("a", "b"): -1}, 0, ">")
    assert not la.entails({eng.len_atom("a", "c"): 1}, 6, "=")


def test_prop3_needs_strict_length_hypothesis():
    """Review (claude-9): I.3 needs |ab| > |c0c1| so that b lies OUTSIDE the
    circle of radius |c0c1| about a. With only >=, b may lie on the circle and
    intersection_circle_line_between_points must not fire."""
    for rel in (None, ">="):
        s = eng.State()
        for f, w in eng.PROP3_HYPS_DATA:
            s.assume(f, w)
        if rel:
            s.la.add({eng.len_atom("a", "b"): 1,
                      eng.len_atom("c0", "c1"): -1}, 0, rel)
        eng.close(s)
        with pytest.raises(eng.MissingPremise) as ei:
            for i, (r, a, b) in enumerate(eng.PROP3_STEPS_DATA, 1):
                eng.apply_step(s, i, r, a, b)
        assert "('outside', 'b', 'DEF')" in str(ei.value)


# --- I.4 (Book/Prop04.lean:5, SAS via superposition) ------------------------

PROP4_HYPS_DATA = [
    # formTriangle a b c AB BC AC and d e f DE EF DF, destructed
    (("dpl", "a", "b", "AB"), "h"),
    (("on_line", "b", "BC"), "h"), (("on_line", "c", "BC"), "h"),
    (("on_line", "c", "AC"), "h"), (("on_line", "a", "AC"), "h"),
    (("dpl", "d", "e", "DE"), "h"),
    (("on_line", "e", "EF"), "h"), (("on_line", "f", "EF"), "h"),
    (("on_line", "f", "DF"), "h"), (("on_line", "d", "DF"), "h"),
    # SMT-sourced destructed hypothesis (Lean proves f not on DE in SMT)
    (("not_on_line", "f", "DE"), "h"),
    (("eq_len", "a", "b", "d", "e"), "h"),
    (("eq_len", "a", "c", "d", "f"), "h"),
    (("eq_ang", "b", "a", "c", "e", "d", "f"), "h"),
]

PROP4_STEPS_DATA = [
    ("superposition", ("a", "b", "c", "d", "e", "f", "AB", "BC", "AC", "DE"),
     {"b'": "b'", "c'": "c'", "BC'": "BC'", "AC'": "AC'"}),
    ("assert_point_eq_on_ray", ("b'", "e", "d", "DE"), {}),
    ("assert_point_eq_angle", ("c'", "f", "e", "d", "DE"), {}),
]


def _prop4_state(hyps=PROP4_HYPS_DATA):
    s = eng.State()
    for f, w in hyps:
        if f[0] == "eq_len":
            eng.eq_len(s.la, *f[1:])
        elif f[0] == "eq_ang":
            eng.eq_ang(s.la, *f[1:])
        s.assume(f, w)
    eng.close(s)
    return s


def test_prop4_replay():
    import time
    t0 = time.time()
    s = _prop4_state()
    for i, (r, a, b) in enumerate(PROP4_STEPS_DATA, 1):
        eng.apply_step(s, i, r, a, b)
    dt = time.time() - t0
    assert eng.la_entails_eq(s.la, "b", "c", "e", "f"), "I.4 |b-c| = |e-f|"
    assert eng.la_entails_eq_ang(s.la, "a", "b", "c", "d", "e", "f"), \
        "I.4 angle at b"
    assert eng.la_entails_eq_ang(s.la, "a", "c", "b", "d", "f", "e"), \
        "I.4 angle at c"
    assert dt < 10


def test_control_prop4_ssa_stops():
    """Negative control (a): drop the included angle (SSA, not SAS). The
    c' = f assert must stop: its equal-angle entailment premise fails."""
    hyps = [h for h in PROP4_HYPS_DATA if h[0][0] != "eq_ang"]
    s = _prop4_state(hyps)
    with pytest.raises(eng.MissingPremise) as ei:
        for i, (r, a, b) in enumerate(PROP4_STEPS_DATA, 1):
            eng.apply_step(s, i, r, a, b)
    assert "eq_ang" in str(ei.value)


# --- I.5 (Book/Prop05.lean:5, isosceles base angles) -------------------------

PROP5_HYPS_DATA = [
    # formTriangle a b c AB BC AC, destructed
    (("dpl", "a", "b", "AB"), "h"),
    (("on_line", "b", "BC"), "h"), (("on_line", "c", "BC"), "h"),
    (("on_line", "c", "AC"), "h"), (("on_line", "a", "AC"), "h"),
    # SMT-sourced destructed hypotheses: off-line facts, line distinctness
    # and the same-side facts Lean's SMT derives from formTriangle and the
    # extensions (the engine takes them as stated inputs)
    (("not_on_line", "c", "AB"), "h"),
    (("not_on_line", "b", "AC"), "h"),
    (("not_on_line", "c", "GB"), "h"),
    (("not_on_line", "b", "FC"), "h"),
    (("neq_line", "AB", "AC"), "h"), (("neq_line", "AB", "GB"), "h"),
    (("neq_line", "AC", "FC"), "h"),
    # the Book hypotheses
    (("between", "a", "b", "d"), "h"),
    (("between", "a", "c", "e"), "h"),
    (("neq", "b", "d"), "h"), (("neq", "c", "e"), "h"),
    (("eq_len", "a", "b", "a", "c"), "h"),
]

PROP5_STEPS_DATA = [
    ("point_between_points_shorter_than", ("AB", "b", "d", "c", "e"),
     {"a": "f"}),
    ("proposition_3", ("a", "e", "f", "a", "AC", "AB"), {"q": "g"}),
    ("line_from_points", ("c", "f"), {"L": "FC"}),
    ("line_from_points", ("b", "g"), {"L": "GB"}),
    ("proposition_4", ("a", "f", "c", "a", "g", "b", "AB", "FC", "AC",
                       "AC", "GB", "AB"), {}),
    ("proposition_4", ("f", "b", "c", "g", "c", "b", "AB", "BC", "FC",
                       "AC", "BC", "GB"), {}),
    ("sum_angles_onlyif", ("b", "a", "g", "c", "AB", "GB"), {}),
    ("sum_angles_onlyif", ("c", "a", "f", "b", "AC", "FC"), {}),
]


def _prop5_state(hyps=PROP5_HYPS_DATA):
    s = eng.State()
    for f, w in hyps:
        if f[0] == "eq_len":
            eng.eq_len(s.la, *f[1:])
        s.assume(f, w)
    eng.close(s)
    return s


def test_prop5_replay():
    import time
    t0 = time.time()
    s = _prop5_state()
    for i, (r, a, b) in enumerate(PROP5_STEPS_DATA, 1):
        eng.apply_step(s, i, r, a, b)
    dt = time.time() - t0
    assert eng.la_entails_eq_ang(s.la, "a", "b", "c", "a", "c", "b"), \
        "I.5 base angles equal"
    assert eng.la_entails_eq_ang(s.la, "c", "b", "d", "b", "c", "e"), \
        "I.5 exterior angles equal"
    assert dt < 10


def test_control_prop5_not_isosceles_stops():
    """Negative control (b): drop |a-b| = |a-c|. The first proposition_4
    application must stop: its |a-c| = |a-b| entailment premise fails."""
    hyps = [h for h in PROP5_HYPS_DATA
            if h[0] != ("eq_len", "a", "b", "a", "c")]
    s = _prop5_state(hyps)
    with pytest.raises(eng.MissingPremise) as ei:
        for i, (r, a, b) in enumerate(PROP5_STEPS_DATA, 1):
            eng.apply_step(s, i, r, a, b)
    assert "eq_len" in str(ei.value) or "gt_len" in str(ei.value)


def test_control_prop5_angle_sum_needs_side_fact():
    """Negative control (c), targeting the angle arithmetic: the sum-angles
    premise c not on GB feeds pasch_2, which derives g~c on AB, without
    which sum_angles_onlyif (b, a, g, c, AB, GB) cannot fire. Drop it and
    the angle sum ang:a:b:g = ang:a:b:c + ang:c:b:g must be unreachable —
    the step stops at that exact premise."""
    hyps = [h for h in PROP5_HYPS_DATA
            if h[0] != ("not_on_line", "c", "GB")]
    s = _prop5_state(hyps)
    with pytest.raises(eng.MissingPremise) as ei:
        for i, (r, a, b) in enumerate(PROP5_STEPS_DATA, 1):
            eng.apply_step(s, i, r, a, b)
    assert ei.value.premise == ("not_on_line", "c", "GB")
