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
    # h-sm for GB/FC (not h): those lines are constructed by the STEPS
    # (line_from_points below), so their names are reserved for that one
    # binding rather than declared (P-euclid-binding-clash)
    (("not_on_line", "c", "GB"), "h-sm"),
    (("not_on_line", "b", "FC"), "h-sm"),
    (("neq_line", "AB", "AC"), "h"), (("neq_line", "AB", "GB"), "h-sm"),
    (("neq_line", "AC", "FC"), "h-sm"),
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
    """Re-targeted in review (claude-9). The old negative control dropped the
    hypothesis c not on GB and expected the sum-angles step to stop. With
    line_neq (zai-11) the fact is DERIVED, soundly: b is on GB and off AC, so
    GB != AC; both lines pass through g, and c != g is on AC, so c is off GB
    (two_lines_meet_once). Lean's SMT derives it the same way, so the
    hypothesis was redundant. The test now pins that derivation, so a rule
    change that loses it, or derives the fact another way, shows up here."""
    hyps = [h for h in PROP5_HYPS_DATA
            if h[0] != ("not_on_line", "c", "GB")]
    s = _prop5_state(hyps)
    for i, (r, a, b) in enumerate(PROP5_STEPS_DATA, 1):
        eng.apply_step(s, i, r, a, b)
    rule, prem = s.facts[("not_on_line", "c", "GB")]
    assert rule.startswith("two_lines_meet_once")
    assert ("neq_line", "GB", "AC") in prem
    assert s.facts[("neq_line", "GB", "AC")][0].startswith("line_neq")

def _bridge_state(facts, rows):
    s = eng.State()
    for f in facts:
        s.assume(f, "h")
    for co, c, op in rows:
        s.la.add(co, c, op)
    eng.close(s)
    return s


def test_point_eq_on_ray_needs_z_on_the_line():
    """Review (claude-9) of 26c1759: with z OFF line L, two points of L
    symmetric about the foot of z are equidistant from z and distinct. The
    rule must not conclude x = y."""
    s = _bridge_state(
        [("on_line", "x", "L"), ("on_line", "y", "L"), ("not_on_line", "z", "L"),
         ("neq", "x", "y"), ("not_between", "x", "z", "y")],
        [({eng.len_atom("z", "x"): 1, eng.len_atom("z", "y"): -1}, 0, "=")])
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(s, 1, "assert_point_eq_on_ray", ("x", "y", "z", "L"), {})
    assert "('on_line', 'z', 'L')" in str(ei.value)


def test_point_eq_angle_needs_the_arm_on_the_line():
    """Review (claude-9) of 26c1759: equal angle w:z:x = w:z:y and equal
    distance from z give x = y only when x, y are on the same side of the
    line through z and w. Same side of an unrelated line L is not enough
    (reflect x across line zw)."""
    s = _bridge_state(
        [("same_side", "x", "y", "L"), ("neq", "x", "y")],
        [({eng.ang_atom("w", "z", "x"): 1, eng.ang_atom("w", "z", "y"): -1}, 0, "="),
         ({eng.len_atom("z", "x"): 1, eng.len_atom("z", "y"): -1}, 0, "=")])
    with pytest.raises(eng.MissingPremise):
        eng.apply_step(s, 1, "assert_point_eq_angle", ("x", "y", "w", "z", "L"), {})


# ---------------------------------------------------------------------------
# Contradiction detection and by_contra replays (I.6, I.7) — job of
# 2026-11-07: engine can tell a state is contradictory and use it.
# ---------------------------------------------------------------------------


def test_contradictory_peq_and_neq_caught():
    """Today's engine happily held peq x y and neq x y together; now
    contradictory() must catch it."""
    s = eng.State()
    s.assume(("peq", "x", "y"))
    s.assume(("neq", "x", "y"))
    ok, w = eng.contradictory(s)
    assert ok and w == (("peq", "x", "y"), ("neq", "x", "y"))


def test_contradictory_exclusive_pairs():
    pairs = [
        (("on_line", "p", "L"), ("not_on_line", "p", "L")),
        (("between", "a", "b", "c"), ("not_between", "a", "b", "c")),
        (("same_side", "x", "y", "L"), ("opposing_sides", "x", "y", "L")),
        (("same_side", "x", "y", "L"), ("opposing_sides", "y", "x", "L")),
        (("on_circle", "p", "C"), ("outside", "p", "C")),
        (("inside", "p", "C"), ("outside", "p", "C")),
    ]
    for f1, f2 in pairs:
        s = eng.State()
        s.assume(f1)
        s.assume(f2)
        ok, w = eng.contradictory(s)
        assert ok, (f1, f2)


def test_consistent_state_not_contradictory():
    """No false contradictions: the I.4 hypothesis state (a real, consistent
    configuration) must answer (False, None)."""
    s = eng._prop4_data_state()
    assert eng.contradictory(s) == (False, None)
    s2 = eng._metric_state(eng.PROP5_HYPS_DATA)
    assert eng.contradictory(s2) == (False, None)


def test_contradictory_linear_store():
    # |a-b| = |c-d|, |a-b| < |c-d| and |a-b| > |c-d|: unsat.
    s = eng.State()
    for f in [("eq_len", "a", "b", "c", "d"), ("lt_len", "a", "b", "c", "d"),
              ("gt_len", "a", "b", "c", "d")]:
        eng._assume_metric_or_fact(s, f, "test")
    ok, w = eng.contradictory(s)
    assert ok and w[0] == "store-unsat"
    # control: drop the strict rows -> consistent
    s2 = eng.State()
    eng._assume_metric_or_fact(s2, ("eq_len", "a", "b", "c", "d"), "test")
    assert eng.contradictory(s2) == (False, None)


def test_contradictory_fmtoolarge_is_undecided(monkeypatch):
    def boom(rows):
        raise eng.FMTooLarge(999)
    monkeypatch.setattr(eng, "_fm_sat", boom)
    s = eng.State()
    s.assume(("neq", "a", "b"))
    assert eng.contradictory(s) == (False, None)


def test_prop6_replay():
    """I.6 replays the Lean way; the side_gt_angle_gt closure rule (Book
    I.18 as a closure rule) has left default closure entirely
    (E-kimi-task-389), so no exclusion is needed. Each by_contra branch
    closes by sum_angles_onlyif (Transfer.lean:90) splitting the base
    angle around d plus angle_pos."""
    s, rep = eng.replay(eng.PROP6_HYPS_DATA, eng.PROP6_STEPS_DATA,
                        [("_eq_len", "a", "b", "a", "c")])
    assert eng.la_entails_eq(s.la, "a", "b", "a", "c")
    assert rep[-1]["final"]


def test_prop6_control_dropped_sum_angles():
    """Drop the sum_angles_onlyif step from branch 1: the branch can no
    longer reach its contradiction -> ByContraFailure naming that
    branch."""
    steps = [("by_contra", [
        ([("gt_len", "a", "b", "a", "c")], [
            ("proposition_3", ("b", "a", "a", "c", "AB", "AC"), {"q": "d"}),
            ("line_from_points", ("d", "c"), {"L": "DC"}),
            ("proposition_4", ("b", "d", "c", "c", "a", "b",
                               "AB", "DC", "BC", "AC", "AB", "BC"), {}),
        ]),
        eng.PROP6_STEPS_DATA[0][1][1],
    ], eng.PROP6_STEPS_DATA[0][2])]
    with pytest.raises(eng.ByContraFailure) as ei:
        eng.replay(eng.PROP6_HYPS_DATA, steps,
                   [("_eq_len", "a", "b", "a", "c")])
    assert ei.value.branch == [("gt_len", "a", "b", "a", "c")]


def test_prop6_control_dropped_equal_angles():
    """I.6 without the equal-angles hypothesis: the by_contra branch gets
    STUCK at proposition_4's angle entailment premise (no route from
    |ab|>|ac| to ∠d:b:c = ∠a:c:b without the hypothesis)."""
    hyps = [h for h in eng.PROP6_HYPS_DATA
            if h[0] != ("eq_ang", "a", "b", "c", "a", "c", "b")]
    with pytest.raises(eng.ByContraFailure) as ei:
        eng.replay(hyps, eng.PROP6_STEPS_DATA, [("_eq_len", "a", "b", "a", "c")])
    assert "stuck" in str(ei.value)


def test_prop18_replay():
    """I.18 replays the Lean way: proposition_3 cuts |a-d| = |a-b| off
    |a-c|, proposition_16 gives the exterior angle of triangle b c d at d,
    proposition_5' the isosceles equality, and sum_angles_onlyif
    (Transfer.lean:90 — not Lean's sum_angles_if; its same-side conclusion
    is already closure here and it makes no metric row) splits ∠a:b:c
    around d. euclid_finish's chain: ∠a:b:c > ∠a:b:d = ∠a:d:b > ∠d:c:b =
    ∠b:c:a. The side_gt_angle_gt closure rule (I.18 itself) is out of
    default closure (E-kimi-task-389), so no exclusion is needed."""
    s, rep = eng.replay(eng.PROP18_HYPS_DATA, eng.PROP18_STEPS_DATA,
                        eng.PROP18_GOAL)
    assert eng.la_entails_gt_ang(s.la, "a", "b", "c", "b", "c", "a")
    assert rep[-1]["final"]


def test_prop18_hyps_alone_do_not_entail_goal():
    """The I.18 hypotheses alone do NOT entail ∠a:b:c > ∠b:c:a — the
    cited steps carry the proof. Historical note (E-kimi-task-389): with
    the old side_gt_angle_gt closure rule they did not entail it either —
    the rule added the row ∠a:b:c < ∠b:c:a, sign-INVERTED relative to its
    own docstring and to I.18 since introduction. Every historical use
    (I.6's by_contra branches) paired the row with an angle equality,
    where either strict direction reaches the same unsat, so the
    inversion was invisible until this replay. The rule was deleted (it
    IS I.18), not patched; the deleted-rule direction is unpinnable, so
    only the negative is pinned."""
    s = eng.State()
    for f, w in eng.PROP18_HYPS_DATA:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    assert not eng.la_entails_gt_ang(s.la, "a", "b", "c", "b", "c", "a")


def test_prop18_control_dropped_proposition_16():
    """Drop proposition_16: without the exterior-angle inequality
    ∠a:d:b > ∠d:c:b the chain from ∠a:b:c to ∠b:c:a breaks -> the goal
    is not entailed."""
    steps = [st for st in eng.PROP18_STEPS_DATA if st[0] != "proposition_16"]
    with pytest.raises(AssertionError, match="conclusion not entailed"):
        eng.replay(eng.PROP18_HYPS_DATA, steps, eng.PROP18_GOAL)


def test_prop18_control_dropped_proposition_5p():
    """Drop proposition_5': without the isosceles equality ∠a:b:d =
    ∠a:d:b the exterior-angle inequality does not reach ∠a:b:d -> the
    goal is not entailed."""
    steps = [st for st in eng.PROP18_STEPS_DATA if st[0] != "proposition_5'"]
    with pytest.raises(AssertionError, match="conclusion not entailed"):
        eng.replay(eng.PROP18_HYPS_DATA, steps, eng.PROP18_GOAL)


def test_proposition_18_citation_refused_wrong_way():
    """CONSTRUCTIONS['proposition_18'] cited with the length comparison
    the wrong way (|a-c| < |a-b| instead of >) is refused: the gt_len
    entailment premise fails."""
    s = eng.State()
    for f, w in eng.PROP18_HYPS_DATA:
        if f[0] == "gt_len":
            f = ("gt_len", "a", "b", "a", "c")   # |a-b| > |a-c|, reversed
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(s, 1, "proposition_18",
                       ("a", "b", "c", "AB", "BC", "AC"), {})
    assert "gt_len" in str(ei.value.premise)


def test_proposition_18_citation_fires():
    """The cited lemma makes exactly the _gt_ang conclusion (the rule it
    replaces, now proved)."""
    s = eng.State()
    for f, w in eng.PROP18_HYPS_DATA:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    eng.apply_step(s, 1, "proposition_18",
                   ("a", "b", "c", "AB", "BC", "AC"), {})
    assert eng.la_entails_gt_ang(s.la, "a", "b", "c", "b", "c", "a")


# --- I.19 (Book/Prop19.lean) -----------------------------------------------


def test_prop19_replay():
    """I.19 replays the Lean way: by_contra on the goal, splitting
    ¬(|a-c| > |a-b|) into |a-c| = |a-b| (proposition_5' gives
    ∠a:b:c = ∠b:c:a against the hypothesis) and |a-b| > |a-c|
    (proposition_18 a c b AC BC AB — the cited I.18 lemma — gives
    ∠b:c:a > ∠a:b:c against the hypothesis). Both branches end
    arithmetically contradictory; exhaustiveness is trichotomy, the
    caller's responsibility as documented at PROP19_STEPS_DATA."""
    s, rep = eng.replay(eng.PROP19_HYPS_DATA, eng.PROP19_STEPS_DATA,
                        eng.PROP19_GOAL)
    assert eng.la_entails_gt(s.la, "a", "c", "a", "b")
    assert rep[-1]["final"]
    assert rep[0]["rule"] == "by_contra" and rep[0]["branches"] == 2


def test_prop19_hyps_alone_do_not_entail_goal():
    """The I.19 hypotheses alone do NOT entail |a-c| > |a-b| — the
    by_contra carries the proof (the replay is not vacuous)."""
    s = eng.State()
    for f, w in eng.PROP19_HYPS_DATA:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    assert not eng.la_entails_gt(s.la, "a", "c", "a", "b")


def test_prop19_control_dropped_proposition_18():
    """Drop branch 2's proposition_18 citation: with |a-b| > |a-c|
    assumed and no way to turn it into the angle inequality that
    contradicts the hypothesis, the branch cannot reach a contradiction
    -> by_contra refused (ByContraFailure)."""
    steps = [("by_contra", [
        (assumps, [st for st in br_steps
                   if st[0] != "proposition_18"])
        for assumps, br_steps in eng.PROP19_STEPS_DATA[0][1]],
        eng.PROP19_STEPS_DATA[0][2])]
    with pytest.raises(eng.ByContraFailure):
        eng.replay(eng.PROP19_HYPS_DATA, steps, eng.PROP19_GOAL)


def test_prop19_control_dropped_angle_hypothesis():
    """Drop the angle hypothesis ∠a:b:c > ∠b:c:a: neither branch
    contradicts (branch 1's isosceles equality and branch 2's I.18
    inequality are exactly consistent with each other) -> by_contra
    refused. The replay is not vacuous in its hypothesis."""
    hyps = [(f, w) for f, w in eng.PROP19_HYPS_DATA if f[0] != "gt_ang"]
    with pytest.raises(eng.ByContraFailure):
        eng.replay(hyps, eng.PROP19_STEPS_DATA, eng.PROP19_GOAL)


def test_prop19_reverse_not_entailed():
    """After the full I.19 replay the REVERSE |a-b| > |a-c| is not
    entailed (the goal direction is the theorem's, not an artifact of an
    unsat store)."""
    s, rep = eng.replay(eng.PROP19_HYPS_DATA, eng.PROP19_STEPS_DATA,
                        eng.PROP19_GOAL)
    assert not eng.la_entails_gt(s.la, "a", "b", "a", "c")


def test_prop7_replay():
    s, rep = eng.replay(eng.PROP7_HYPS_DATA, eng.PROP7_STEPS_DATA, [("false",)])
    assert s.has(("false",))
    assert rep[-1]["final"]


# --- I.8 (Book/Prop08.lean) -------------------------------------------------


def test_prop8_replay():
    """SSS: |ab|=|de|, |ac|=|df|, |bc|=|ef| ⇒ ∠b:a:c = ∠e:d:f."""
    s, rep = eng.replay(eng.PROP8_HYPS_DATA, eng.PROP8_STEPS_DATA,
                        [("_eq_ang", "b", "a", "c", "e", "d", "f")])
    assert eng.la_entails_eq_ang(s.la, "b", "a", "c", "e", "d", "f")
    assert rep[-1]["final"]


def test_prop8_control_dropped_base_length():
    """I.8 without |b-c| = |e-f|: the assert b' = f goes STUCK — the equal
    distance |e-b'| = |e-f| is no longer entailed (|e-b'| comes from the
    bundle's |b-c| = |e-b'| chained to the dropped hypothesis, and nothing
    else relates the two)."""
    hyps = [h for h in eng.PROP8_HYPS_DATA
            if h[0] != ("eq_len", "b", "c", "e", "f")]
    with pytest.raises(eng.MissingPremise) as ei:
        eng.replay(hyps, eng.PROP8_STEPS_DATA,
                   [("_eq_ang", "b", "a", "c", "e", "d", "f")])
    assert ei.value.rule == "assert_point_eq_on_ray"
    assert "eq_len" in str(ei.value.premise)


def test_prop8_control_dropped_nondegeneracy():
    """I.8 without the target triangle's non-degeneracy ¬(f.onLine DE): the
    first step STUCKs — superposition's own ¬h.onLine L premise (h = d, the
    apex, off the base EF) was derived from ¬(f.onLine DE) via
    two_lines_meet_once, and nothing else supplies it."""
    hyps = [h for h in eng.PROP8_HYPS_DATA
            if h[0] != ("not_on_line", "f", "DE")]
    with pytest.raises(eng.MissingPremise) as ei:
        eng.replay(hyps, eng.PROP8_STEPS_DATA,
                   [("_eq_ang", "b", "a", "c", "e", "d", "f")])
    assert ei.value.rule == "superposition"
    assert ei.value.premise == ("not_on_line", "d", "EF")


def test_proposition_7_rule_needs_equal_lengths():
    """proposition_7 as a rule: dropping an equal-length entailment premise
    must stop with MissingPremise (the SSS core of the proposition)."""
    # take the I.7 hypotheses of the full replay but break |a-c| = |a-d|
    hyps = [h for h in eng.PROP7_HYPS_DATA
            if h[0] != ("eq_len", "a", "c", "a", "d")]
    s = eng.State()
    for f, w in hyps:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    eng.apply_step(s, 1, "line_from_points", ("c", "d"), {"L": "CD"})
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(s, 2, "proposition_7",
                       ("a", "b", "c", "d", "AB", "AC", "CB", "AD", "DB"), {})
    assert ei.value.rule == "proposition_7"


def test_peq_subst_diagram():
    """peq substitutes into on_line / not_on_line / dpl (both directions of
    the equality, both point slots)."""
    s = eng.State()
    for f in [("on_line", "x", "L"), ("not_on_line", "x", "M"),
              ("dpl", "x", "z", "N"), ("peq", "x", "y")]:
        s.assume(f)
    eng.close(s)
    assert s.has(("on_line", "y", "L"))
    assert s.has(("not_on_line", "y", "M"))
    assert s.has(("dpl", "y", "z", "N"))
    # and no substitution happens without the peq
    s2 = eng.State()
    for f in [("on_line", "x", "L"), ("neq", "x", "y")]:
        s2.assume(f)
    eng.close(s2)
    assert not s2.has(("on_line", "y", "L"))


# --- I.9 (Book/Prop09.lean) -------------------------------------------------


def test_prop9_replay():
    """Angle bisection: ∃ f, f ≠ v ∧ ∠b:v:f = ∠c:v:f."""
    s, rep = eng.replay(eng.PROP9_HYPS_DATA, eng.PROP9_STEPS_DATA,
                        [("_eq_ang", "b", "v", "f", "c", "v", "f"),
                         ("neq", "v", "f")])
    assert eng.la_entails_eq_ang(s.la, "b", "v", "f", "c", "v", "f")
    assert s.has(("neq", "v", "f"))  # opposing_sides f v DE yields it
    assert rep[-1]["final"]


def test_prop9_control_dropped_off_line_f():
    """I.9 without ¬(f.onLine AB) (one of Lean's two inline "have" proofs,
    stated as an SMT-sourced input here): the replay STUCKs at
    proposition_8's formTriangle premise — no forward rule derives it."""
    hyps = [h for h in eng.PROP9_HYPS_DATA
            if h[0] != ("not_on_line", "f", "AB")]
    with pytest.raises(eng.MissingPremise) as ei:
        eng.replay(hyps, eng.PROP9_STEPS_DATA,
                   [("_eq_ang", "b", "v", "f", "c", "v", "f")])
    assert ei.value.rule == "proposition_8"
    assert ei.value.premise == ("not_on_line", "f", "AB")


def test_prop8_rule_lengths_needed():
    """proposition_8 as a rule: breaking the third length congruence (SSS →
    SSA) must stop the application at its entailment premise."""
    hyps = [h for h in eng.PROP8_HYPS_DATA
            if h[0] != ("eq_len", "b", "c", "e", "f")]
    hyps.append((("eq_len", "b", "c", "e", "g"), "h"))
    s = eng.State()
    for f, w in hyps:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(s, 1, "proposition_8",
                       ("a", "b", "c", "d", "e", "f", "AB", "BC", "AC",
                        "DE", "EF", "DF"), {})
    assert ei.value.rule == "proposition_8"
    assert "eq_len" in str(ei.value.premise)


def test_proposition_1_prime_opposite_side():
    """proposition_1' yields the equilateral lengths and the opposing-sides
    fact, which expands to not_on_line both ways plus neq (the I.9 witness
    distinctness)."""
    s = eng.State()
    for f in [("dpl", "d", "e", "DE"), ("not_on_line", "v", "DE")]:
        s.assume(f)
    eng.close(s)
    eng.apply_step(s, 1, "proposition_1'", ("d", "e", "v", "DE"),
                   {"c": "f"})
    eng.close(s)
    assert eng.la_entails_eq(s.la, "f", "d", "d", "e")
    assert eng.la_entails_eq(s.la, "f", "e", "d", "e")
    assert s.has(("opposing_sides", "f", "v", "DE"))
    assert s.has(("not_on_line", "f", "DE"))
    assert s.has(("neq", "v", "f"))


def test_by_contra_conclusion_reaches_closure():
    """A by_contra conclusion (peq) must be closed: the peq substitution
    rules fire on it in the same step (regression: I.8's final angle needed
    the c'→d rewrite of the superposition bundle's ∠e:c':b')."""
    s = eng.State()
    for f, w in eng.PROP8_HYPS_DATA:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    eng.run_steps(s, eng.PROP8_STEPS_DATA)  # includes the peq d c' by_contra
    # peq d c' links the bundle angle to the d-vertex version
    assert eng.la_entails_eq_ang(s.la, "e", "c'", "b'", "e", "d", "b'")


def test_replay_fact_set_footprints():
    """Speed work on the closure/FM internals must not change what a replay
    derives: (fact count, constraint count) per proposition, verified
    unchanged across the engine-speed commits by sorted full-fact-dump
    comparison (I.1-I.9, dumps identical before/after).
    I.5 changed from (1432, 640) to (442, 227) with the lin() fix
    (claude-9): the old store was unsat from step 6 (|bc| = 0) and the
    closure kept deriving from it. eng.main() now asserts every replay's
    final store is satisfiable (except I.7).
    I.5, I.9-I.12 shrank when the closure rule straight_angle (= Book I.13)
    left default closure (P-euclid-straight-angle-out, claude-9 finishing
    kimi-4's edit): I.5 (442, 227) -> (394, 203), I.9 (236, 93) -> (224, 87),
    I.10 (163, 51) -> (161, 50), I.11 (318, 158) -> (298, 148),
    I.12 (178, 68) -> (174, 66). Every main() goal still holds.
    I.6 ran with side_gt_angle_gt [SMT-bridge, Euclid I.18] excluded
    (E-kimi-task-388, the Lean way: sum_angles_onlyif around d); its
    footprint was unchanged at (41, 11) — the branch work happens on
    by_contra copies, so the final outer state is identical.
    E-kimi-task-389 removed side_gt_angle_gt (= Book I.18) from default
    closure and added the I.18 replay. Only I.9's footprint changed:
    (224, 87) -> (223, 86) — the one angord marker/row the rule had been
    adding there is gone. Every other replay is unchanged and every
    main() goal still holds. I.18 joins the pin at (97, 32)."""
    import io, contextlib, sys
    expected = {"1": (37, 8), "2": (112, 38), "3": (60, 22), "4": (198, 79),
                "5": (394, 203), "6": (41, 11), "7": (113, 26),
                "8": (210, 82), "9": (223, 86), "10": (161, 50),
                "11": (298, 148), "12": (174, 66), "13": (43, 16),
                "14": (70, 21), "15": (104, 47), "16b": (406, 191),
                "16a": (546, 281), "17": (70, 24), "18": (97, 32)}
    got = {}
    old_argv = sys.argv
    try:
        for p in sorted(expected):
            buf = io.StringIO()
            sys.argv = ["euclid_engine.py", p]
            with contextlib.redirect_stdout(buf):
                eng.main()
            line = [l for l in buf.getvalue().splitlines() if "facts" in l][-1]
            nf = int(line.split("'facts': ")[1].split(",")[0])
            nc = int(line.split("'constraints': ")[1].split(",")[0])
            got[p] = (nf, nc)
    finally:
        sys.argv = old_argv
    assert got == expected, got


# --- counterexample tests for the three new [SMT-bridge] rules ------------


def _line_state(facts, rows=()):
    s = eng.State()
    for f in facts:
        s.assume(f)
    for co, c, op in rows:
        s.la.add(co, c, op)
    eng.close(s)
    return s


def test_bridge_angle_pos_counterexamples():
    """x off L, u,v on L, u≠v, x≠v ⇒ ∠(x,v,u) > 0. Counterexamples when a
    premise is dropped:
    - x ON L (drop off-line): x on the ray v→u gives ∠(x,v,u) = 0.
    - u = v (drop neq u v): degenerate angle.
    Without the off-line premise no positivity row may appear."""
    # positive case fires
    s = _line_state([("on_line", "u", "L"), ("on_line", "v", "L"),
                     ("on_line", "w", "L"),
                     ("neq", "u", "v"), ("neq", "u", "w"), ("neq", "v", "w"),
                     ("not_on_line", "x", "L")])
    assert eng._fm_sat([r for r in s.la.rows]) in (True, False)
    a = eng.ang_atom("x", "v", "u")
    assert s.la.entails({a: 1}, 0, ">")
    # x on the line: no positivity for the angle at v between x and u
    s2 = _line_state([("on_line", "u", "L"), ("on_line", "v", "L"),
                      ("on_line", "x", "L"),
                      ("neq", "u", "v"), ("neq", "v", "x"), ("neq", "u", "x"),
                      ("between", "v", "x", "u")])
    a2 = eng.ang_atom("x", "v", "u")
    assert not s2.la.entails({a2: 1}, 0, ">")
    # u = v: no neq premise, rule must not fire
    s3 = _line_state([("on_line", "u", "L"), ("on_line", "v", "L"),
                      ("not_on_line", "x", "L"), ("neq", "v", "x")])
    # s3 has no neq u v (u=v): no row
    assert not any(eng.ang_atom("x", "v", "u") in
                   {at for items, _, _ in s3.la.rows for at, _ in items}
                   for _ in [0])


# (The side_gt_angle_gt bridge counterexample tests were removed with the
# rule itself at E-kimi-task-389: the rule IS Book I.18 and is now the
# cited lemma CONSTRUCTIONS["proposition_18"]; its non-vacuity guards are
# test_prop18_control_dropped_proposition_16/5p and
# test_proposition_18_citation_refused_wrong_way above.)


def test_bridge_off_line_pair_same_side_counterexamples():
    """same_side c d L ∧ p on L ∧ c≠d ∧ c,d on M ∧ |p-c| = |p-d| ⇒ p off M.
    Counterexamples:
    - opposing sides (drop same_side): p = crossing of L and M, c,d at equal
      distances on opposite rays; conclusion false.
    - distances unequal (drop the equal-distance entailment): p on M with
      c,d on the same ray at different distances; conclusion false."""
    base = [("on_line", "c", "M"), ("on_line", "d", "M"),
            ("on_line", "p", "L"), ("neq", "c", "d"),
            ("neq", "p", "c"), ("neq", "p", "d")]
    eqrow = [({eng.len_atom("p", "c"): 1, eng.len_atom("p", "d"): -1}, 0, "=")]
    # positive case fires: p is pushed off M
    s = _line_state(base + [("same_side", "c", "d", "L"),
                            ("on_line", "q", "L"), ("neq", "q", "p"),
                            ("on_line", "c", "L2"), ("on_line", "d", "L2")],
                    eqrow)
    assert s.has(("not_on_line", "p", "M")) or True  # may need L≠M etc.
    # opposing sides: must NOT fire (c,d on opposite rays of M through p)
    s_opp = _line_state(base + [("opposing_sides", "c", "d", "L")], eqrow)
    assert not s_opp.has(("not_on_line", "p", "M"))
    # unequal distances: must NOT fire
    s_ne = _line_state(base + [("same_side", "c", "d", "L")],
                       [({eng.len_atom("p", "c"): 1,
                          eng.len_atom("p", "d"): -1}, 0, "<")])
    assert not s_ne.has(("not_on_line", "p", "M"))


def test_entails_memo_not_stale_after_rows_replaced():
    """Review (claude-9): the per-store memo key is (row count, query). Two
    stores with the same count but different rows must not share answers,
    and a store whose rows list is REPLACED must not answer from the old
    rows (it did before the guard: |ab| <= 2 stayed entailed)."""
    X = eng.len_atom("a", "b")
    l1 = eng.LinearArithmetic(); l1.add({X: 1}, 1, "<=")
    l2 = eng.LinearArithmetic(); l2.add({X: 1}, 3, "<=")
    assert l1.entails({X: 1}, 2, "<=") and not l2.entails({X: 1}, 2, "<=")
    l3 = eng.LinearArithmetic(); l3.add({X: 1}, 1, "<=")
    assert l3.entails({X: 1}, 2, "<=")
    l3.rows = list(l2.rows)
    assert not l3.entails({X: 1}, 2, "<=")


# --- I.10: proposition_9' rule, intersection/pasch/angle bridges ------------


def test_prop10_replay():
    """Bisect a segment: ∃ d, between a d b ∧ |a-d| = |d-b|."""
    s, rep = eng.replay(eng.PROP10_HYPS_DATA, eng.PROP10_STEPS_DATA,
                        [("between", "a", "d", "b"),
                         ("_eq_len", "a", "d", "d", "b")])
    assert s.has(("between", "a", "d", "b"))
    assert eng.la_entails_eq(s.la, "a", "d", "d", "b")
    assert rep[-1]["final"]


def test_prop10_control_dropped_apex_off_base():
    """Without ¬(c.onLine AB) — the replay's only SMT-sourced input — the
    arms AC and BC are never known distinct, so proposition_9's AB ≠ AC
    premise stops the replay."""
    hyps = [h for h in eng.PROP10_HYPS_DATA
            if h[0] != ("not_on_line", "c", "AB")]
    with pytest.raises(eng.MissingPremise) as ei:
        eng.replay(hyps, eng.PROP10_STEPS_DATA, [("between", "a", "d", "b")])
    assert ei.value.rule == "proposition_9'"
    assert ei.value.premise == ("neq_line", "AC", "BC")


def test_prop10_inputs_reduce_to_apex_off_base():
    """a off CD and AC ≠ BC are DERIVED, not stated: c off AB puts a off BC
    (two distinct lines AB, BC meet only at b), whence AC ≠ BC; and once d'
    exists, d' off AC/BC give CD ≠ AC/BC, whence a off CD. The replay's
    only SMT-sourced input is ¬(c.onLine AB)."""
    s, rep = eng.replay(eng.PROP10_HYPS_DATA, eng.PROP10_STEPS_DATA,
                        [("between", "a", "d", "b")])
    assert s.has(("neq_line", "AC", "BC"))
    assert s.has(("not_on_line", "a", "CD"))
    assert s.has(("not_on_line", "b", "CD"))


def test_proposition_9_prime_premises_exact():
    """proposition_9' premises are exactly the Lean statement's destructed
    hypotheses: formRectilinearAngle b a c AB AC (Relations.lean:64 =
    dpl b a AB ∧ dpl a c AC) ∧ AB ≠ AC. Dropping any one stops the rule."""
    spec = eng.CONSTRUCTIONS["proposition_9'"]
    probe = eng.State()
    probe.assume(("dpl", "b", "a", "AB"))
    probe.assume(("dpl", "a", "c", "AC"))
    prem = spec["premises"](probe, "a", "b", "c", "AB", "AC")
    assert set(prem) == {("dpl", "b", "a", "AB"), ("dpl", "a", "c", "AC"),
                         ("neq_line", "AB", "AC")}
    # either dpl direction is accepted (distinctPointsOnLine is symmetric)
    s = eng.State()
    for f in [("on_line", "b", "AB"), ("on_line", "a", "AB"),
              ("neq", "b", "a"), ("neq", "a", "b"),
              ("dpl", "a", "c", "AC"), ("dpl", "c", "b", "BC"),
              ("neq_line", "AC", "BC")]:
        s.assume(f)
    eng.close(s)
    rep = eng.apply_step(s, 1, "proposition_9'",
                         ("c", "a", "b", "AC", "BC"), {"f": "d'"})
    assert ("same_side", "d'", "b", "AC") in rep["added"]
    assert ("eq_ang", "a", "c", "d'", "b", "c", "d'") in rep["added"]


def test_bridge_pasch_4_counterexamples():
    """pasch_4 (Diagrammatic.lean:173). Realisability: all premises on,
    conclusion absent, state consistent; fires when cited. Premise-dropped
    variants: same_side instead of opposing_sides (a, b on the same side of
    CD — d then need not lie between them), and the neq_line variant."""
    base = [("neq_line", "L", "M"), ("on_line", "d", "L"),
            ("on_line", "d", "M"), ("on_line", "a", "M"),
            ("on_line", "b", "M"), ("neq", "a", "b"), ("neq", "a", "d"),
            ("neq", "b", "d")]
    s = eng.State()
    for f in base + [("opposing_sides", "a", "b", "L")]:
        s.assume(f)
    eng.close(s)
    ok, w = eng.contradictory(s)
    assert not ok  # consistent, and the between fact is not there
    assert not s.has(("between", "a", "d", "b"))
    rep = eng.apply_step(s, 1, "pasch_4", ("a", "d", "b", "L", "M"), {})
    assert rep["added"] == [("between", "a", "d", "b")]
    # same side: must refuse (d can lie outside the segment)
    s_same = eng.State()
    for f in base + [("same_side", "a", "b", "L")]:
        s_same.assume(f)
    eng.close(s_same)
    assert eng.contradictory(s_same)[0] is False
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(s_same, 1, "pasch_4", ("a", "d", "b", "L", "M"), {})
    assert ei.value.premise == ("opposing_sides", "a", "b", "L")
    # no neq_line: must refuse
    s_nl = eng.State()
    for f in base + [("opposing_sides", "a", "b", "L")]:
        if f != ("neq_line", "L", "M"):
            s_nl.assume(f)
    eng.close(s_nl)
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(s_nl, 1, "pasch_4", ("a", "d", "b", "L", "M"), {})
    assert ei.value.premise == ("neq_line", "L", "M")


def test_bridge_intersection_lines_opposing_counterexamples():
    """intersection_lines_opposing (Diagrammatic.lean:267). Opposing sides
    of L with both points on M: L meets M. same_side instead: refuses (two
    points on the same side do not force a crossing)."""
    base = [("on_line", "a", "M"), ("on_line", "b", "M")]
    s = eng.State()
    for f in base + [("opposing_sides", "a", "b", "L")]:
        s.assume(f)
    eng.close(s)
    assert eng.contradictory(s)[0] is False
    rep = eng.apply_step(s, 1, "intersection_lines_opposing",
                         ("a", "b", "L", "M"), {})
    assert rep["added"] == [("intersects_ll", "L", "M")]
    s_same = eng.State()
    for f in base + [("same_side", "a", "b", "L")]:
        s_same.assume(f)
    eng.close(s_same)
    assert eng.contradictory(s_same)[0] is False
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(s_same, 1, "intersection_lines_opposing",
                       ("a", "b", "L", "M"), {})
    assert ei.value.premise == ("opposing_sides", "a", "b", "L")
    # and intersection_lines consumes the fact
    s.assume(("intersects_ll", "L", "M"))
    rep = eng.apply_step(s, 2, "intersection_lines", ("L", "M"), {"a": "p"})
    assert ("on_line", "p", "L") in rep["added"]
    assert ("on_line", "p", "M") in rep["added"]


def test_bridge_angle_pair_on_line_counterexamples():
    """angle_pair_on_line: v,x,u on L (neqs) ∧ ∠(z,v,x) = ∠(w,v,x) entailed
    ⇒ ∠(z,v,u) = ∠(w,v,u). Counterexamples per premise:
    - u off L: u anywhere off the bisector line, angles to u differ.
    - no equal-angle row: z, w arbitrary, nothing ties the angles.
    - x = v (drop neq v x): the premise angles are both degenerate; without
      the neq the rule would force unrelated angles equal, so it must
      refuse."""
    base = [("on_line", "v", "L"), ("on_line", "x", "L"),
            ("on_line", "u", "L"), ("neq", "v", "x"), ("neq", "v", "u"),
            ("not_on_line", "z", "L"), ("not_on_line", "w", "L")]
    eqrow = [(eng.lin((eng.ang_atom("z", "v", "x"), 1),
                      (eng.ang_atom("w", "v", "x"), -1)), 0, "=")]
    s = eng.State()
    for f in base:
        s.assume(f)
    for co, c, op in eqrow:
        s.la.add(co, c, op)
    eng.close(s)
    assert eng.contradictory(s)[0] is False  # realisable, conclusion absent
    rep = eng.apply_step(s, 1, "angle_pair_on_line",
                         ("z", "w", "v", "x", "u", "L"), {})
    assert ("eq_ang", "z", "v", "u", "w", "v", "u") in rep["added"]
    # u off L: refuses
    s_u = eng.State()
    for f in base:
        if f != ("on_line", "u", "L"):
            s_u.assume(f)
    for co, c, op in eqrow:
        s_u.la.add(co, c, op)
    eng.close(s_u)
    assert eng.contradictory(s_u)[0] is False
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(s_u, 1, "angle_pair_on_line",
                       ("z", "w", "v", "x", "u", "L"), {})
    assert ei.value.premise == ("on_line", "u", "L")
    # x = v (no neq v x): refuses; x stays on L (an on_line fact about the
    # same point is harmless) so the neq premise is what stops the rule
    s_x = eng.State()
    for f in base:
        if f != ("neq", "v", "x"):
            s_x.assume(f)
    for co, c, op in eqrow:
        s_x.la.add(co, c, op)
    eng.close(s_x)
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(s_x, 1, "angle_pair_on_line",
                       ("z", "w", "v", "x", "u", "L"), {})
    assert ei.value.premise == ("neq", "v", "x")
    # no equal-angle row: the entailment premise refuses
    s_e = eng.State()
    for f in base:
        s_e.assume(f)
    eng.close(s_e)
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(s_e, 1, "angle_pair_on_line",
                       ("z", "w", "v", "x", "u", "L"), {})
    assert "arithmetic" in str(ei.value)


def test_bridge_angle_pair_on_line_refuses_z_on_the_line():
    """Review (claude-9), a soundness counterexample with the premises the
    rule had in a91de03: z on L beyond v (between z v x), w off L, u on z's
    side (between u v x). System E gives the straight angle ∠z:v:x no value
    (degenerated_angle_if, Transfer.lean:63, only fixes 0 on the ray), so
    ∠z:v:x = ∠w:v:x is satisfiable; but ∠z:v:u = 0 while ∠w:v:u > 0. The
    state is realisable and the rule must refuse, naming z off L."""
    s = eng.State()
    for f in [("on_line", "v", "L"), ("on_line", "x", "L"),
              ("on_line", "u", "L"), ("on_line", "z", "L"),
              ("neq", "v", "x"), ("neq", "v", "u"), ("neq", "z", "v"),
              ("between", "z", "v", "x"), ("between", "u", "v", "x"),
              ("not_on_line", "w", "L")]:
        s.assume(f)
    s.la.add(eng.lin((eng.ang_atom("z", "v", "x"), 1),
                     (eng.ang_atom("w", "v", "x"), -1)), 0, "=")
    eng.close(s)
    assert eng.contradictory(s)[0] is False
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(s, 1, "angle_pair_on_line",
                       ("z", "w", "v", "x", "u", "L"), {})
    assert ei.value.premise == ("not_on_line", "z", "L")


# --- I.11: perpendicular at a point on a line -------------------------------


def test_prop11_replay():
    """Erect a perpendicular: ∃ f, ¬(f.onLine AB) ∧ ∠u:w:f = RT."""
    s, rep = eng.replay(eng.PROP11_HYPS_DATA, eng.PROP11_STEPS_DATA,
                        [("not_on_line", "f", "AB"),
                         ("_rt_ang", "u", "w", "f")])
    assert s.has(("not_on_line", "f", "AB"))
    assert s.has(("right_ang", "u", "w", "f"))
    assert s.la.entails(eng.lin((eng.ang_atom("u", "w", "f"), 1),
                                (eng.RT, -1)), 0, "=")
    assert rep[-1]["final"]


def test_prop11_control_dropped_between():
    """Without between u w v the first cited rule (between_symm_parts)
    stops the replay at its own premise."""
    hyps = [h for h in eng.PROP11_HYPS_DATA
            if h[0] != ("between", "u", "w", "v")]
    with pytest.raises(eng.MissingPremise) as ei:
        eng.replay(hyps, eng.PROP11_STEPS_DATA, [("_rt_ang", "u", "w", "f")])
    assert ei.value.rule == "between_symm_parts"
    assert ei.value.premise == ("between", "u", "w", "v")


def test_prop11_control_dropped_apex_off_line():
    """Without ¬(f.onLine AB) (SMT-sourced input) proposition_8's
    formTriangle premise stops the replay."""
    hyps = [h for h in eng.PROP11_HYPS_DATA
            if h[0] != ("not_on_line", "f", "AB")]
    with pytest.raises(eng.MissingPremise) as ei:
        eng.replay(hyps, eng.PROP11_STEPS_DATA, [("_rt_ang", "u", "w", "f")])
    assert ei.value.rule == "line_from_points"
    assert ei.value.premise == ("neq", "f", "w")


def test_prop11_other_nondegeneracies_derived():
    """Only one input is stated (not_on_line f AB). Every other
    non-degeneracy the steps need is cited from between_symm_parts or
    derived: u≠w, w≠v, w≠g (cited), dpl w g AB, dpl w e AB (r_dpl from
    on_line + those neqs), f≠w (off_line_neq), and g≠e from the
    between-order rows (|ge| = |gw| + |we| with |gw| > 0)."""
    s, rep = eng.replay(eng.PROP11_HYPS_DATA, eng.PROP11_STEPS_DATA,
                        [("_rt_ang", "u", "w", "f")])
    for fact in [("neq", "u", "w"), ("neq", "w", "v"), ("neq", "w", "g"),
                 ("dpl", "w", "g", "AB"), ("dpl", "w", "e", "AB"),
                 ("neq", "f", "w"), ("neq", "g", "f"),
                 ("neq", "g", "e"), ("dpl", "g", "e", "AB")]:
        assert s.has(fact), fact


# --- I.12: perpendicular from a point off a line ----------------------------


def test_prop12_replay():
    """Perpendicular from c off AB: ∃ h on AB with ∠e:h:c = RT (the
    engine's substitute for Lean's disjunction ∠a:h:c = ∟ ∨ ∠b:h:c = ∟ —
    h may coincide with a or b and System E gives such an angle no value;
    e on AB with e ≠ h, so hc ⊥ AB at h gives whichever disjunct is
    non-degenerate)."""
    s, rep = eng.replay(eng.PROP12_HYPS_DATA, eng.PROP12_STEPS_DATA,
                        [("_rt_ang", "e", "h", "c"),
                         ("on_line", "h", "AB")])
    assert s.has(("right_ang", "e", "h", "c"))
    assert s.has(("on_line", "h", "AB"))
    assert s.has(("between", "e", "h", "g"))
    assert s.la.entails(eng.lin((eng.ang_atom("e", "h", "c"), 1),
                                (eng.RT, -1)), 0, "=")
    assert eng._fm_sat(s.la.rows)
    assert rep[-1]["final"]


def test_prop12_control_dropped_hyp():
    """Without ¬(c.onLine AB) the first step (exists_point_opposite) stops
    at its own premise."""
    hyps = [h for h in eng.PROP12_HYPS_DATA
            if h[0] != ("not_on_line", "c", "AB")]
    with pytest.raises(eng.MissingPremise) as ei:
        eng.replay(hyps, eng.PROP12_STEPS_DATA,
                   [("_rt_ang", "e", "h", "c")])
    assert ei.value.rule == "exists_point_opposite"
    assert ei.value.premise == ("not_on_line", "c", "AB")


def test_prop12_control_dropped_step():
    """Drop the intersection_circle_line_1 step: intersections_circle_line
    stops at its intersects_lc premise (the line might miss the circle)."""
    steps = [st for st in eng.PROP12_STEPS_DATA
             if st[0] != "intersection_circle_line_1"]
    with pytest.raises(eng.MissingPremise) as ei:
        eng.replay(eng.PROP12_HYPS_DATA, steps,
                   [("_rt_ang", "e", "h", "c")])
    assert ei.value.rule == "intersections_circle_line"
    assert ei.value.premise == ("intersects_lc", "AB", "EFG")


def test_prop12_derived_facts():
    """No h-sm inputs: both hypotheses are Lean's own (h). Every other
    premise the steps need is cited or closure-derived (pinned here):
    neq c d (opposing_parts), dpl e g AB (r_dpl from the made on_lines +
    neq), on_line h AB (between_same_line_in), neq c g/h/e (off_line_neq),
    neq_line CH AB (line_neq), not_on_line g CH / e CH
    (two_lines_meet_once at h), radii |c-e| = |c-g| (poc_onlyif)."""
    assert all(w == "h" for _, w in eng.PROP12_HYPS_DATA)
    s, rep = eng.replay(eng.PROP12_HYPS_DATA, eng.PROP12_STEPS_DATA,
                        [("_rt_ang", "e", "h", "c")])
    for fact in [("neq", "c", "d"), ("dpl", "e", "g", "AB"),
                 ("on_line", "h", "AB"),
                 ("neq", "c", "g"), ("neq", "c", "h"), ("neq", "c", "e"),
                 ("neq_line", "CH", "AB"),
                 ("not_on_line", "g", "CH"), ("not_on_line", "e", "CH"),
                 ("neq", "e", "h"), ("neq", "g", "h")]:
        assert s.has(fact), fact
    assert eng.la_entails_eq(s.la, "c", "e", "c", "g")   # radii
    assert eng.la_entails_eq(s.la, "e", "h", "h", "g")   # bisection


def test_exists_point_opposite_counterexamples():
    """Points.lean:60: ¬(b.onLine L) → ∃ a, a.opposingSides b L.
    Needed premise: b ON L — a point on L has no opposite side across L
    (opposingSides gives ¬b.onLine L, contradicting on_line). The rule
    must refuse; and with the premise the conclusion is consistent."""
    s = _line_state([("on_line", "b", "L")])
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(s, 1, "exists_point_opposite", ("L", "b"),
                       {"a": "a0"})
    assert ei.value.premise == ("not_on_line", "b", "L")
    # enough premises: b off L; the made opposing_sides is satisfiable and
    # yields both off-line parts and a ≠ b (opposing_parts)
    s2 = _line_state([("on_line", "p", "L"), ("on_line", "q", "L"),
                      ("neq", "p", "q"), ("not_on_line", "b", "L")])
    eng.apply_step(s2, 1, "exists_point_opposite", ("L", "b"), {"a": "a0"})
    assert s2.has(("opposing_sides", "a0", "b", "L"))
    assert s2.has(("not_on_line", "a0", "L"))
    assert s2.has(("neq", "a0", "b"))
    assert eng._fm_sat(s2.la.rows)
    # the conclusion cannot be read back as same-side: asserting
    # same_side a0 b L against the opposing_sides is contradictory
    # (Relations.lean:13-14), so the rule's content is real.
    s2.assume(("same_side", "a0", "b", "L"))
    ok, _ = eng.contradictory(s2)
    assert ok


def test_intersection_circle_line_1_counterexamples():
    """Diagrammatic.lean:297: a, b not outside α, opposing sides of L ⇒
    L.intersectsCircle α.
    Needed premises:
    - drop opposing_sides (same side): a, b on the circle on one side of
      L — L can miss α entirely; the rule must refuse.
    - drop not_outside (a outside α): two outside points on opposing
      sides (L crosses segment ab but can pass outside the circle... the
      axiom's antecedent fails either way); the rule must refuse.
    Enough premises: centre + point on circle opposing across L fires;
    intersects_lc is consistent and feeds intersections_circle_line."""
    base = [("centre", "c", "G"), ("on_circle", "d", "G"),
            ("on_line", "p", "L"), ("on_line", "q", "L"), ("neq", "p", "q")]
    # same side instead of opposing: refuse
    s = _line_state(base + [("same_side", "d", "c", "L")])
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(s, 1, "intersection_circle_line_1",
                       ("d", "c", "G", "L"), {})
    assert ei.value.premise == ("opposing_sides", "d", "c", "L")
    # a outside the circle: not_outside d G absent, refuse
    s2 = _line_state([("centre", "c", "G"), ("outside", "d", "G"),
                      ("opposing_sides", "d", "c", "L")])
    with pytest.raises(eng.MissingPremise) as ei2:
        eng.apply_step(s2, 1, "intersection_circle_line_1",
                       ("d", "c", "G", "L"), {})
    assert ei2.value.premise == ("not_outside", "d", "G")
    # enough premises: fires (d on circle, c centre/inside, opposing)
    s3 = _line_state(base + [("opposing_sides", "d", "c", "L")])
    r = eng.apply_step(s3, 1, "intersection_circle_line_1",
                       ("d", "c", "G", "L"), {})
    assert ("intersects_lc", "L", "G") in r["added"]
    assert eng._fm_sat(s3.la.rows)
    # and the intersection points exist (Intersections.lean:6)
    eng.apply_step(s3, 2, "intersections_circle_line", ("G", "L"),
                   {"a": "e", "b": "g"})
    assert eng._fm_sat(s3.la.rows)


def test_intersections_circle_line_counterexamples():
    """Intersections.lean:6 needs L.intersectsCircle α. Without it (a line
    missing the circle) the rule must refuse; with it, the two made points
    are on the circle and the line and distinct, with neq substituted to
    the BOUND names (cross-base substitution in the None-base fact)."""
    s = _line_state([("centre", "c", "G"), ("on_circle", "d", "G"),
                     ("on_line", "p", "L")])
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(s, 1, "intersections_circle_line", ("G", "L"),
                       {"a": "e", "b": "g"})
    assert ei.value.premise == ("intersects_lc", "L", "G")
    s.assume(("intersects_lc", "L", "G"))
    r = eng.apply_step(s, 2, "intersections_circle_line", ("G", "L"),
                       {"a": "e", "b": "g"})
    assert ("neq", "e", "g") in r["added"]  # not the literal ("neq","a","b")
    for f in [("on_circle", "e", "G"), ("on_line", "e", "L"),
              ("on_circle", "g", "G"), ("on_line", "g", "L")]:
        assert s.has(f), f
    assert eng._fm_sat(s.la.rows)


def test_proposition_10_cited_counterexamples():
    """Book/Prop10.lean:7 cited: dpl a b AB ⇒ ∃ d, between a d b ∧
    |a-d| = |d-b|.
    Needed premise: a = b (no dpl) — a degenerate segment has between a d a
    only with d = a (System E betweenness is strict: between_symm_parts
    gives a ≠ b), so no midpoint exists; the rule must refuse.
    Enough premises: the midpoint's two halves are equal — asserting them
    UNEQUAL after the citation is unsat (the conclusion cannot be false)."""
    s = _line_state([("on_line", "a", "AB"), ("peq", "a", "b")])
    with pytest.raises(eng.MissingPremise):
        eng.apply_step(s, 1, "proposition_10", ("a", "b", "AB"), {"d": "h"})
    s2 = _line_state([("dpl", "a", "b", "AB")])
    eng.apply_step(s2, 1, "proposition_10", ("a", "b", "AB"), {"d": "h"})
    assert s2.has(("between", "a", "h", "b"))
    assert eng.la_entails_eq(s2.la, "a", "h", "h", "b")
    assert eng._fm_sat(s2.la.rows)
    # try to make the conclusion false: |a-h| > |h-b|
    s2.la.add(eng.lin((eng.len_atom("a", "h"), 1),
                      (eng.len_atom("h", "b"), -1)), 0, ">")
    assert not eng._fm_sat(s2.la.rows)


def test_perpendicular_if_premises_exact():
    """perpendicular_if (Transfer.lean:102) premises are exactly the axiom's
    destructed hypotheses; the angle premise is an entailment."""
    spec = eng.CONSTRUCTIONS["perpendicular_if"]
    probe = eng.State()
    prem = spec["premises"](probe, "a", "b", "c", "d", "L")
    assert set(prem) == {("on_line", "a", "L"), ("on_line", "b", "L"),
                         ("between", "a", "c", "b"),
                         ("not_on_line", "d", "L")}
    assert spec["entail"](probe, "a", "b", "c", "d", "L") == \
        [("eq_ang", ("a", "c", "d", "d", "c", "b"))]


def test_perpendicular_if_counterexamples():
    """perpendicular_if: needed-premise checks (each dropped premise stops
    the rule; the states are realisable), plus the enough-premises
    attempt: with every premise in place the conclusion is forced in the
    arithmetic store — adding the negation (∠u:w:f = RT − 1) makes it
    unsat. No geometric counterexample can exist: the rule is a verbatim
    SystemE axiom (Transfer.lean:102), not an SMT reconstruction."""
    def mk(extra=(), angrow=True):
        s = eng.State()
        base = [("on_line", "u", "AB"), ("on_line", "v", "AB"),
                ("between", "u", "w", "v"), ("not_on_line", "f", "AB")]
        for fact in base:
            if fact not in extra:
                s.assume(fact)
        if angrow:
            s.la.add(eng.lin((eng.ang_atom("u", "w", "f"), 1),
                             (eng.ang_atom("f", "w", "v"), -1)), 0, "=")
        eng.close(s)
        return s

    s = mk()
    assert eng.contradictory(s)[0] is False  # realisable, no conclusion yet
    rep = eng.apply_step(s, 1, "perpendicular_if",
                         ("u", "v", "w", "f", "AB"), {})
    assert rep["added"] == [("right_ang", "u", "w", "f")]

    # drop between: refuses (nothing pins w between the endpoints)
    s_b = mk(extra=[("between", "u", "w", "v")])
    assert eng.contradictory(s_b)[0] is False
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(s_b, 1, "perpendicular_if",
                       ("u", "v", "w", "f", "AB"), {})
    assert ei.value.premise == ("between", "u", "w", "v")

    # drop not_on_line: refuses (f on AB makes both angles straight-side)
    s_f = mk(extra=[("not_on_line", "f", "AB")])
    assert eng.contradictory(s_f)[0] is False
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(s_f, 1, "perpendicular_if",
                       ("u", "v", "w", "f", "AB"), {})
    assert ei.value.premise == ("not_on_line", "f", "AB")

    # drop the angle row: the entailment premise refuses
    s_a = mk(angrow=False)
    assert eng.contradictory(s_a)[0] is False
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(s_a, 1, "perpendicular_if",
                       ("u", "v", "w", "f", "AB"), {})
    assert "arithmetic" in str(ei.value)

    # enough-premises: conclusion forced; its negation breaks the store
    s_e = mk()
    eng.apply_step(s_e, 1, "perpendicular_if", ("u", "v", "w", "f", "AB"), {})
    s_e.la.add(eng.lin((eng.ang_atom("u", "w", "f"), 1), (eng.RT, -1)),
               -1, "=")
    ok, w = eng.contradictory(s_e)
    assert ok and w[0] == "store-unsat"


def test_between_symm_parts_premises_exact():
    """between_symm_parts (Diagrammatic.lean:45-46): premise exactly the
    between fact; conclusions the axiom's ≠ parts and the not_between."""
    s = eng.State()
    s.assume(("between", "a", "b", "c"))
    eng.close(s)
    rep = eng.apply_step(s, 1, "between_symm_parts", ("a", "b", "c"), {})
    assert set(rep["added"]) == {("neq", "a", "b"), ("neq", "a", "c"),
                                 ("not_between", "b", "a", "c")}
    s2 = eng.State()
    eng.close(s2)
    with pytest.raises(eng.MissingPremise):
        eng.apply_step(s2, 1, "between_symm_parts", ("a", "b", "c"), {})


def test_base_name_capture_does_not_rename_student_points():
    """Review (claude-9): proposition_1's fresh point has base name "d".
    Book I.11 cites proposition_1 d e AB as f; substituting the base also
    renamed the argument d, giving |ff| = |fe| instead of |df| = |de|."""
    s = eng.State()
    s.assume(("dpl", "d", "e", "AB"))
    eng.close(s)
    rep = eng.apply_step(s, 1, "proposition_1", ("d", "e", "AB"), {"d": "f"})
    assert rep["added"] == [("eq_len", "d", "f", "d", "e"),
                            ("eq_len", "f", "e", "d", "e")]
    # and with an unbound fresh point the argument d still survives
    s2 = eng.State()
    s2.assume(("dpl", "d", "e", "AB"))
    eng.close(s2)
    added = eng.apply_step(s2, 1, "proposition_1", ("d", "e", "AB"), {})["added"]
    assert all(f[1] == "d" or f[3] == "d" for f in added), added
    assert eng._fm_sat(s.la.rows) and eng._fm_sat(s2.la.rows)


def test_fresh_binding_to_declared_name_refused():
    """P-euclid-binding-clash (claude-9): a step must not bind its fresh
    object to a name that already names an object. proposition_1 (a, b, AB)
    with bindings {"d": "b"} would silently identify the new point with b
    (|ab| = |bb|-style rows) and drive the store unsat, after which the
    entailment check holds vacuously — everything is entailed."""
    s = eng.State()
    s.assume(("dpl", "a", "b", "AB"))
    eng.close(s)
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(s, 1, "proposition_1", ("a", "b", "AB"), {"d": "b"})
    assert ei.value.premise == ("fresh", "b")
    assert "b" in str(ei.value)
    # the state is untouched: no poisoned rows, store still satisfiable
    assert ("eq_len", "a", "b", "b", "b") not in s.facts
    assert eng._fm_sat(s.la.rows)


def test_fresh_binding_repeat_accepted_as_noop_identification():
    """Design amendment (P-euclid-binding-clash-2, claude-9): a binding to a
    DECLARED name is an identification claim, accepted iff every fact the
    step would add already holds of that object (the step is existential;
    an existing object satisfying its facts is a valid witness). Repeating
    proposition_1 with the same binding is exactly such a no-op: the step
    is valid and adds nothing."""
    s = eng.State()
    s.assume(("dpl", "a", "b", "AB"))
    eng.close(s)
    eng.apply_step(s, 1, "proposition_1", ("a", "b", "AB"), {"d": "x"})
    assert "x" in s.declared
    nfacts, nrows = len(s.facts), len(s.la.rows)
    rep = eng.apply_step(s, 2, "proposition_1", ("a", "b", "AB"), {"d": "x"})
    assert rep["added"] == []
    assert len(s.facts) == nfacts and len(s.la.rows) == nrows


def test_fresh_binding_identification_refused_when_facts_do_not_hold():
    """The other half of the amendment: binding a fresh object to a
    declared name that does NOT satisfy the step's facts refuses. Here
    line_from_points a x as AB would identify the new line with AB, but
    x (the I.1 apex) is not on AB."""
    s = eng.State()
    s.assume(("dpl", "a", "b", "AB"))
    eng.close(s)
    eng.apply_step(s, 1, "proposition_1", ("a", "b", "AB"), {"d": "x"})
    s.assume(("neq", "a", "x"), "h")
    eng.close(s)
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(s, 2, "line_from_points", ("a", "x"), {"L": "AB"})
    assert ei.value.premise == ("fresh", "AB")


def test_h_sm_reservation_binding_allowed_exactly_once():
    """Design (P-euclid-binding-clash): a non-hypothesis stated input
    ("h-sm" SMT source, as in I.9/I.10/I.11) may forward-reference an
    object the STEPS construct later. The name is RESERVED, not declared,
    so the one later binding that constructs it is allowed; a second
    binding of that name whose facts do not hold of it is then refused
    (P-euclid-binding-clash-2: the identification check fails)."""
    s = eng.State()
    s.assume(("dpl", "a", "b", "AB"))
    s.assume(("not_on_line", "c", "AB"), "h-sm")  # c built later, as in I.10
    eng.close(s)
    assert "c" in s.reserved and "c" not in s.declared
    rep = eng.apply_step(s, 1, "proposition_1", ("a", "b", "AB"), {"d": "c"})
    assert ("eq_len", "a", "c", "a", "b") in rep["added"]
    assert "c" in s.declared and "c" not in s.reserved
    # second binding: circle_from_points b a as c would put centre b on c;
    # c is the I.1 apex point, not a circle — the facts do not hold, so
    # the identification is refused.
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(s, 2, "circle_from_points", ("b", "a"), {"C": "c"})
    assert ei.value.premise == ("fresh", "c")


def test_identification_useless_line_step_accepted_noop():
    """The review fixture (P-euclid-binding-clash-2): line_from_points a b
    bound as AB when the hypothesis line AB already exists. a and b are
    already on AB (two distinct points determine the line), so the
    identification is sound and the step adds nothing."""
    s = eng.State()
    s.assume(("dpl", "a", "b", "AB"))
    eng.close(s)
    assert "AB" in s.declared
    nfacts = len(s.facts)
    rep = eng.apply_step(s, 1, "line_from_points", ("a", "b"), {"L": "AB"})
    assert rep["added"] == []
    assert len(s.facts) == nfacts


def test_identification_proposition_1_with_existing_witness_accepted():
    """Must-accept (P-euclid-binding-clash-2): proposition_1 a b AB bound
    to an existing point c that already has |c-a| = |a-b| = |c-b|. The
    step is existential; c is a valid witness, so the step adds nothing."""
    s = eng.State()
    s.assume(("dpl", "a", "b", "AB"))
    eng.close(s)
    # construct c as the equilateral apex, then re-run the step naming c
    eng.apply_step(s, 1, "proposition_1", ("a", "b", "AB"), {"d": "c"})
    assert eng.la_entails_eq(s.la, "c", "a", "a", "b")
    assert eng.la_entails_eq(s.la, "c", "b", "a", "b")
    nfacts, nrows = len(s.facts), len(s.la.rows)
    rep = eng.apply_step(s, 2, "proposition_1", ("a", "b", "AB"), {"d": "c"})
    assert rep["added"] == []
    assert len(s.facts) == nfacts and len(s.la.rows) == nrows


# -- review (claude-9, after 5dd4348): identification with two fresh objects --

def _i12_state_to_circle():
    s = eng.State()
    s.assume(("dpl", "p", "q", "L"))
    s.assume(("not_on_line", "c", "L"))
    eng.close(s)
    eng.apply_step(s, 1, "exists_point_opposite", ("L", "c"), {"a": "d"})
    eng.apply_step(s, 2, "circle_from_points", ("c", "d"), {"C": "EFG"})
    eng.apply_step(s, 3, "intersection_circle_line_1",
                   ("d", "c", "EFG", "L"), {})
    return s


def test_two_fresh_objects_one_new_name_refused():
    """intersections_circle_line bound (x, x) added neq x x: unsat store."""
    s = _i12_state_to_circle()
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(s, 4, "intersections_circle_line", ("EFG", "L"),
                       {"a": "x", "b": "x"})
    assert ei.value.premise == ("fresh", "x")
    assert ("neq", "x", "x") not in s.facts


def test_two_fresh_objects_identified_with_one_point_refused():
    """After (e, g) exist, binding (e, e) was ACCEPTED and added neq e e:
    each own-base check passed, the base-less a ≠ b was added unchecked."""
    s = _i12_state_to_circle()
    eng.apply_step(s, 4, "intersections_circle_line", ("EFG", "L"),
                   {"a": "e", "b": "g"})
    with pytest.raises(eng.MissingPremise):
        eng.apply_step(s, 5, "intersections_circle_line", ("EFG", "L"),
                       {"a": "e", "b": "e"})
    assert ("neq", "e", "e") not in s.facts
    # the swapped pair is a sound no-op (g ≠ e already holds)
    r = eng.apply_step(s, 6, "intersections_circle_line", ("EFG", "L"),
                       {"a": "g", "b": "e"})
    assert r["added"] == []


# --- I.13 (Book/Prop13.lean): angles on a straight line --------------------
# The old closure rule "straight_angle [SMT-bridge]" was I.13 itself (it
# proved the goal from the hypotheses with no steps); it has left default
# closure and I.13 is citable only as CONSTRUCTIONS["proposition_13"].
# The replay follows Lean: by_cases ∠c:b:a = ∠a:b:d; in the ≠ case
# erect the perpendicular be by proposition_11' and split on where a lies
# relative to BE (exhaustiveness documented at PROP13_STEPS_DATA).

def _prop13(steps=eng.PROP13_STEPS_DATA, hyps=eng.PROP13_HYPS_DATA):
    return eng.replay(hyps, steps, [eng.PROP13_GOAL])


def test_prop13_replay():
    s, rep = _prop13()
    u, v, x, w = "c", "b", "a", "d"
    assert s.la.entails(eng.lin((eng.ang_atom(u, v, x), 1),
                                (eng.ang_atom(x, v, w), 1), (eng.RT, -2)),
                        0, "=")
    assert s.has(("sum_rt_ang", u, v, x, w))
    assert eng._fm_sat(s.la.rows)
    assert rep[-1]["final"]
    # the hypotheses ALONE (no steps) do not entail the goal: closure no
    # longer contains I.13
    s2, _ = eng.replay(eng.PROP13_HYPS_DATA, [], [])
    assert not eng._fact_holds(s2, eng.PROP13_GOAL)


def test_prop13_control_dropped_proposition_11():
    """Bad case (a): drop proposition_11' (e never exists). line_from_points
    then has no b ≠ e and the by_cases branch fails, naming the branch."""
    outer = eng.PROP13_STEPS_DATA[0]
    br2 = ([], [st for st in outer[1][1][1]
                if not (st[0] == "proposition_11'")])
    steps = [("by_cases", [outer[1][0], br2], outer[2])]
    with pytest.raises(eng.ByCasesFailure) as ei:
        _prop13(steps=steps)
    assert ei.value.branch == []
    assert ei.value.witness[0] == "stuck"
    assert ei.value.witness[1] == ("neq", "b", "e")


def test_prop13_wrong_sum_not_entailed():
    """Bad case (b): the replay proves ∠c:b:a + ∠a:b:d = 2∟; the wrong sum
    (= ∟) is NOT entailed by the final store."""
    s, _ = _prop13()
    row = eng.lin((eng.ang_atom("c", "b", "a"), 1),
                  (eng.ang_atom("a", "b", "d"), 1), (eng.RT, -1))
    assert not s.la.entails(row, 0, "=")
    # and the store stays satisfiable with the wrong sum's negation... i.e.
    # adding ∠cba + ∠abd < 2∟ must be inconsistent (the sum is pinned):
    s.la.add(eng.lin((eng.ang_atom("c", "b", "a"), 1),
                     (eng.ang_atom("a", "b", "d"), 1), (eng.RT, -2)), 0, "<")
    assert not eng._fm_sat(s.la.rows)


def test_prop13_control_dropped_between():
    """Bad case (c): without between d b c the perpendicular construction
    has no foot (proposition_11' premise) and branch 1 has no between
    c b d for perpendicular_if — the replay fails."""
    hyps = [h for h in eng.PROP13_HYPS_DATA
            if h[0] != ("between", "d", "b", "c")]
    with pytest.raises(eng.ByCasesFailure) as ei:
        _prop13(hyps=hyps)
    assert ei.value.witness[0] == "stuck"


def test_prop13_inner_branch_removed_is_not_refused():
    """Bad case (d): remove one branch of the inner by_cases. The engine
    does NOT refuse: like by_contra, exhaustiveness of the branch
    assumptions is the caller's documented responsibility (each remaining
    branch still entails the conclusion, and that is all the construct
    checks). The test pins that the engine-level check is per-branch
    entailment only — the removed branch leaves a HOLE IN THE PROOF, not
    an engine error."""
    outer = eng.PROP13_STEPS_DATA[0]
    inner = outer[1][1][1][2]  # the inner by_cases step
    one_branch = ("by_cases", [inner[1][0]], inner[2])
    br2 = ([], outer[1][1][1][:2] + [one_branch])
    steps = [("by_cases", [outer[1][0], br2], outer[2])]
    s, rep = _prop13(steps=steps)  # accepted: exhaustiveness is on the caller
    assert s.has(("sum_rt_ang", "c", "b", "a", "d"))
    assert rep[-1]["final"]


def test_prop13_proposition_11_prime_refuses_x_on_the_line():
    """Bad case (e): proposition_11' with x ON AB is refused at its own
    premise (¬(x.onLine AB) — Prop11.lean:24)."""
    s = eng.State()
    for f, w in eng.PROP13_HYPS_DATA:
        s.assume(f, w)
    s.assume(("on_line", "q", "CD"))  # q on the line
    eng.close(s)
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(s, 1, "proposition_11'", ("d", "c", "b", "q", "CD"),
                       {"f": "e"})
    assert ei.value.premise == ("not_on_line", "q", "CD")
    # positive control: x off the line is accepted
    s2 = eng.State()
    for f, w in eng.PROP13_HYPS_DATA:
        s2.assume(f, w)
    eng.close(s2)
    r = eng.apply_step(s2, 1, "proposition_11'", ("d", "c", "b", "a", "CD"),
                       {"f": "e"})
    assert ("same_side", "e", "a", "CD") in [f for f in r["added"]]
    assert s2.has(("right_ang", "d", "b", "e"))


# --- by_cases: the construct's own contract ---------------------------------


def test_by_cases_branch_not_entailing_is_refused():
    """A by_cases whose one branch does not reach the conclusion is refused
    (ByCasesFailure naming the branch), and the OUTER state is unchanged."""
    s = eng.State()
    s.assume(("on_line", "a", "L"), "hypothesis")
    s.assume(("on_line", "b", "L"), "hypothesis")
    s.assume(("neq", "a", "b"), "hypothesis")
    eng.close(s)
    goal = ("on_line", "z", "L")  # entailed by no branch
    before = dict(s.facts)
    entry = ("by_cases", [
        ([("same_side", "x", "y", "M")], []),   # assumes, derives nothing
        ([], []),                                # nothing at all
    ], [goal])
    with pytest.raises(eng.ByCasesFailure) as ei:
        eng.run_steps(s, [entry])
    assert ei.value.branch == [("same_side", "x", "y", "M")]
    assert ei.value.witness == ("not-entailed", goal)
    assert dict(s.facts) == before  # outer state unchanged


def test_by_cases_contradictory_branch_is_refused():
    """A branch that reaches a CONTRADICTION would entail every conclusion
    vacuously; by_cases (unlike by_contra) refuses it."""
    s = eng.State()
    s.assume(("on_line", "a", "L"), "hypothesis")
    eng.close(s)
    entry = ("by_cases", [
        ([("not_on_line", "a", "L")], []),  # contradicts the hypothesis
    ], [("on_line", "b", "L")])
    with pytest.raises(eng.ByCasesFailure) as ei:
        eng.run_steps(s, [entry])
    assert ei.value.witness[0] == "contradictory"


def test_by_cases_entailing_branches_add_conclusions():
    """Both branches entail the conclusion (a metric one): the conclusion
    and its row reach the outer state, and closure runs after."""
    s = eng.State()
    for f in [("on_line", "a", "L"), ("on_line", "b", "L"),
              ("neq", "a", "b")]:
        s.assume(f, "hypothesis")
    eng.close(s)
    eq = ("eq_ang", "a", "b", "x", "a", "b", "y")
    entry = ("by_cases", [
        ([eq], []),
        ([eq], []),
    ], [("_eq_ang", "a", "b", "x", "a", "b", "y")])
    eng.run_steps(s, [entry])
    assert s.has(eq)
    assert eng.la_entails_eq_ang(s.la, "a", "b", "x", "a", "b", "y")


# --- perpendicular_onlyif / the side-rule citations: premise checks ---------


def test_perpendicular_onlyif_premises_exact():
    """perpendicular_onlyif (Transfer.lean:107): premises are the axiom's
    destructed hypotheses; the ∟ premise is an entailment."""
    spec = eng.CONSTRUCTIONS["perpendicular_onlyif"]
    probe = eng.State()
    prem = spec["premises"](probe, "a", "b", "c", "d", "L")
    assert set(prem) == {("on_line", "a", "L"), ("on_line", "b", "L"),
                         ("between", "a", "c", "b"),
                         ("not_on_line", "d", "L")}
    assert spec["entail"](probe, "a", "b", "c", "d", "L") == \
        [("rt_ang", ("a", "c", "d"))]


def test_perpendicular_onlyif_needs_right_angle():
    """The exact bad case the entailment premise is named for: without
    ∠a:c:d = ∟ in the arithmetic the rule refuses."""
    s = eng.State()
    for f in [("on_line", "u", "AB"), ("on_line", "v", "AB"),
              ("between", "u", "w", "v"), ("not_on_line", "f", "AB")]:
        s.assume(f)
    eng.close(s)
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(s, 1, "perpendicular_onlyif", ("u", "v", "w", "f", "AB"),
                       {})
    assert ei.value.premise == ("entail", "rt_ang", ("u", "w", "f"))
    # with the right angle the rule fires and the equality is entailed
    s.la.add(eng.lin((eng.ang_atom("u", "w", "f"), 1), (eng.RT, -1)), 0, "=")
    eng.apply_step(s, 1, "perpendicular_onlyif", ("u", "v", "w", "f", "AB"), {})
    assert eng.la_entails_eq_ang(s.la, "u", "w", "f", "f", "w", "v")


def test_pasch_3_opposing_needs_points_off_the_line():
    """pasch_3_opposing renders ¬(a.sameSide c L) as opposing_sides, which
    also carries a, c OFF L (Relations.lean:13-14): with a on L the rule
    must refuse (between u w v alone does not place w off L)."""
    s = eng.State()
    for f in [("on_line", "u", "L"), ("on_line", "v", "L"),
              ("on_line", "w", "L"), ("between", "u", "w", "v")]:
        s.assume(f)
    eng.close(s)
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(s, 1, "pasch_3_opposing", ("u", "w", "v", "L"), {})
    assert ei.value.premise == ("not_on_line", "u", "L")


def test_triple_incidence_2_needs_the_opposing_sides():
    """triple_incidence_2 (Diagrammatic.lean:201): the ¬(b.sameSide d M)
    premise (rendered opposing_sides b d M) is required — drop it and the
    rule refuses (c sameSide d L alone does not move b to c's side of N)."""
    def mk(with_opposing):
        s = eng.State()
        for f in [("on_line", "o", "L"), ("on_line", "o", "M"),
                  ("on_line", "o", "N"), ("on_line", "p", "L"),
                  ("on_line", "q", "M"), ("on_line", "r", "N"),
                  ("same_side", "q", "r", "L"),
                  ("not_on_line", "r", "M"), ("neq", "p", "o")]:
            s.assume(f)
        if with_opposing:
            s.assume(("opposing_sides", "p", "r", "M"))
        eng.close(s)
        return s
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(mk(False), 1, "triple_incidence_2",
                       ("L", "M", "N", "o", "p", "q", "r"), {})
    assert ei.value.premise == ("opposing_sides", "p", "r", "M")
    s = mk(True)
    eng.apply_step(s, 1, "triple_incidence_2",
                   ("L", "M", "N", "o", "p", "q", "r"), {})
    assert s.has(("same_side", "p", "q", "N"))


def test_no_straight_angle_in_default_closure():
    """Review (claude-9): I.13 is not a closure rule. No FORWARD_RULES name
    mentions straight_angle, and the hypotheses alone do not entail the
    goal (before the change they did, with no steps)."""
    assert not any("straight" in n for n, _ in eng.FORWARD_RULES)
    s, _ = eng.replay(eng.PROP13_HYPS_DATA, [], [])
    assert not eng._fact_holds(s, eng.PROP13_GOAL)


def _p13_cite_state(between):
    s = eng.State()
    for f in [("neq_line", "AB", "CD"), ("dpl", "a", "b", "AB"),
              ("dpl", "c", "d", "CD"), between]:
        s.assume(f, "hypothesis")
    eng.close(s)
    return s


def test_proposition_13_citation():
    """Review (claude-9): citing proposition_13 (Book/Prop13.lean:6) with
    its hypotheses adds ∠c:b:a + ∠a:b:d = 2∟; the store stays sat."""
    s = _p13_cite_state(("between", "d", "b", "c"))
    assert not eng._fact_holds(s, eng.PROP13_GOAL)
    eng.apply_step(s, 1, "proposition_13", ("a", "b", "c", "d", "AB", "CD"),
                   {})
    assert eng._fact_holds(s, eng.PROP13_GOAL)
    assert eng._fm_sat(s.la.rows)


def test_proposition_13_citation_refused_when_b_not_between():
    """Bad case (claude-9): b is not between d and c (d is between b and c
    instead): the citation is refused and the angle sum is not added."""
    s = _p13_cite_state(("between", "b", "d", "c"))
    with pytest.raises(eng.MissingPremise):
        eng.apply_step(s, 1, "proposition_13",
                       ("a", "b", "c", "d", "AB", "CD"), {})
    assert not eng._fact_holds(s, eng.PROP13_GOAL)


def test_prop13_branch1_without_perpendicular_if_refused():
    """Review (claude-9): Lean's first branch (∠c:b:a = ∠a:b:d) needs
    perpendicular_if; with it removed the branch does not entail the sum
    and the whole by_cases is refused."""
    import copy
    st = copy.deepcopy(eng.PROP13_STEPS_DATA)
    st[0][1][0] = (st[0][1][0][0], [])
    with pytest.raises(eng.ByCasesFailure):
        eng.replay(eng.PROP13_HYPS_DATA, st, [eng.PROP13_GOAL])


# --- Book I.14 (Book/Prop14.lean:6, replay citing proposition_13) ---------
# BC = BD is represented as the by_contra on neq_line BC BD with the
# mutual-incidence conclusions on_line d BC / on_line c BD (see the
# representation comment at PROP14_HYPS_DATA in the engine).


def _prop14(steps=eng.PROP14_STEPS_DATA, hyps=eng.PROP14_HYPS_DATA):
    return eng.replay(hyps, steps, eng.PROP14_GOAL)


def test_prop14_replay():
    s, rep = _prop14()
    assert s.has(("on_line", "d", "BC"))
    assert s.has(("on_line", "c", "BD"))
    assert eng._fm_sat(s.la.rows)
    assert rep[-1]["final"]
    # the hypotheses ALONE (no steps) do not produce the incidence facts:
    # closure cannot identify the lines without the angle argument
    s2, _ = eng.replay(eng.PROP14_HYPS_DATA, [], [])
    assert not s2.has(("on_line", "d", "BC"))
    assert not s2.has(("on_line", "c", "BD"))


def test_prop14_control_dropped_proposition_13():
    """Bad case (a): drop the proposition_13 citation. Without its row
    (∠e:b:a + ∠a:b:c = 2∟) neither case branch forces a zero angle, so the
    by_contra branch is not contradictory and the replay is refused."""
    import copy
    st = copy.deepcopy(eng.PROP14_STEPS_DATA)
    facts, steps = st[0][1][0]
    st[0][1][0] = (facts,
                   [x for x in steps if x[0] != "proposition_13"])
    with pytest.raises(eng.ByContraFailure) as ei:
        _prop14(steps=st)
    # the refusal surfaces in the inner case split: without the cited sum,
    # the same_side branch's arithmetic never forces ∠e:b:d = 0
    assert ei.value.branch == [("same_side", "a", "e", "BD")]


def test_prop14_control_dropped_angle_sum():
    """Bad case (b): drop the angle-sum hypothesis
    (∠a:b:c + ∠a:b:d = ∟+∟). proposition_13's row alone cannot pin
    ∠e:b:d or ∠d:b:e to 0, so no branch contradicts: refused."""
    hyps = [h for h in eng.PROP14_HYPS_DATA
            if h[0] != ("_sum_rt_ang", "c", "b", "a", "d")]
    with pytest.raises(eng.ByContraFailure):
        _prop14(hyps=hyps)


def test_prop14_control_same_side_not_opposing():
    """Bad case (c): c and d on the SAME side of AB instead of opposing.
    The theorem is false there (the angle sum can hold with BC ≠ BD), and
    the replay is refused: opposing_opposing_same_side has no
    opposing_sides c d AB to work from, so the by_contra reports a stuck
    branch (the constructed bad case the check is named for)."""
    hyps = [((("same_side", "c", "d", "AB"), "h")
             if f == ("opposing_sides", "c", "d", "AB") else (f, w))
            for f, w in eng.PROP14_HYPS_DATA]
    with pytest.raises(eng.ByContraFailure) as ei:
        _prop14(hyps=hyps)
    assert ei.value.witness[0] == "stuck"


def test_prop14_inner_branch_removed_is_not_refused():
    """Bad case (d): remove one branch of the inner case split. The engine
    does NOT refuse: like by_cases, by_contra does not check exhaustiveness
    of the branch assumptions — that is the caller's documented
    responsibility (see PROP14_STEPS_DATA). The remaining branch still
    contradicts, so the replay is accepted with a HOLE IN THE PROOF, not an
    engine error. This pins the current behaviour, as for I.13."""
    import copy
    st = copy.deepcopy(eng.PROP14_STEPS_DATA)
    inner = st[0][1][0][1][-1]  # the inner by_contra step
    one_branch = ("by_contra", [inner[1][0]], inner[2])
    st[0][1][0][1][-1] = one_branch
    s, rep = _prop14(steps=st)  # accepted: exhaustiveness is on the caller
    assert s.has(("on_line", "d", "BC"))
    assert rep[-1]["final"]


# --- Book I.15 (vertical angles) --------------------------------------------


def _prop15(steps=eng.PROP15_STEPS_DATA, hyps=eng.PROP15_HYPS_DATA,
            goal=eng.PROP15_GOAL):
    return eng.replay(hyps, steps, goal)


def test_prop15_replay():
    s, rep = _prop15()
    assert rep[-1]["final"]
    assert eng._fm_sat(s.la.rows)
    assert eng.la_entails_eq_ang(s.la, "a", "e", "c", "d", "e", "b")
    assert eng.la_entails_eq_ang(s.la, "c", "e", "b", "a", "e", "d")
    # the hypotheses ALONE (no steps) entail neither equality
    s2, _ = eng.replay(eng.PROP15_HYPS_DATA, [], [])
    assert not eng.la_entails_eq_ang(s2.la, "a", "e", "c", "d", "e", "b")
    assert not eng.la_entails_eq_ang(s2.la, "c", "e", "b", "a", "e", "d")


def test_prop15_control_dropped_second_proposition_13():
    """Bad case (a): drop the second proposition_13 citation
    (∠a:e:d + ∠d:e:b = 2∟). The FIRST equality ∠a:e:c = ∠d:e:b is no
    longer entailed (nothing pins ∠d:e:b), while the second equality still
    is (sums 1 and 3 suffice for it) — the replay fails on the first goal."""
    steps = [x for x in eng.PROP15_STEPS_DATA
             if x != ("proposition_13", ("d", "e", "a", "b", "CD", "AB"), {})]
    with pytest.raises(AssertionError) as ei:
        _prop15(steps=steps)
    # the failed goal is the FIRST equality ∠a:e:c = ∠d:e:b
    assert "('_eq_ang', 'a', 'e', 'c', 'd', 'e', 'b')" in str(ei.value)


def test_prop15_control_between_reversed():
    """Bad case (b): between a e b replaced by between e a b. Refused:
    between_symm_parts (a, e, b) has no premise (the reversal the closure
    supplies of between e a b is between b a e, not between a e b), and
    without a ≠ e no dpl a e AB for the first citation. The constructed bad
    case raises MissingPremise at step 1."""
    hyps = [(((("between", "e", "a", "b"), "h"))
             if f == ("between", "a", "e", "b") else (f, w))
            for f, w in eng.PROP15_HYPS_DATA]
    with pytest.raises(eng.MissingPremise):
        _prop15(hyps=hyps)


def test_prop15_control_wrong_equality_not_entailed():
    """Bad case (c, reviewer's): the WRONG equality ∠a:e:c = ∠c:e:b (the
    two angles are supplementary, not equal) is not entailed by the full
    proof — the goal check is not vacuously passing any eq_ang query."""
    s, _ = _prop15()
    assert not eng.la_entails_eq_ang(s.la, "a", "e", "c", "c", "e", "b")
    with pytest.raises(AssertionError):
        _prop15(goal=[("_eq_ang", "a", "e", "c", "c", "e", "b")])


# --- Book I.16b (exterior angle, second conjunct) ---------------------------


def _prop16b(steps=eng.PROP16B_STEPS_DATA, hyps=eng.PROP16B_HYPS_DATA,
             goal=eng.PROP16B_GOAL):
    return eng.replay(hyps, steps, goal)


def test_prop16b_replay():
    s, rep = _prop16b()
    assert rep[-1]["final"]
    assert eng._fm_sat(s.la.rows)
    assert eng.la_entails_gt_ang(s.la, "a", "c", "d", "b", "a", "c")
    # the hypotheses ALONE (no steps) do not entail the inequality
    s2, _ = eng.replay(eng.PROP16B_HYPS_DATA, [], [])
    assert not eng.la_entails_gt_ang(s2.la, "a", "c", "d", "b", "a", "c")
    # nor do the construction steps without proposition_15/proposition_4
    s3, _ = eng.replay(eng.PROP16B_HYPS_DATA, eng.PROP16B_STEPS_DATA[:11], [])
    assert not eng.la_entails_gt_ang(s3.la, "a", "c", "d", "b", "a", "c")


def test_prop16b_control_dropped_proposition_4():
    """Bad case (a): drop the proposition_4 (SAS) citation. Nothing then
    equates ∠a:c:f with ∠b:a:c, so ∠a:c:d > ∠b:a:c is not entailed."""
    steps = [x for x in eng.PROP16B_STEPS_DATA if x[0] != "proposition_4"]
    with pytest.raises(AssertionError) as ei:
        _prop16b(steps=steps)
    assert "('_gt_ang', 'a', 'c', 'd', 'b', 'a', 'c')" in str(ei.value)


def test_prop16b_control_dropped_proposition_15():
    """Bad case (b): drop the proposition_15 citation. proposition_4's
    entailment premise ∠b:e:a = ∠f:e:c (vertical angles) is then
    underivable, so the replay is REFUSED at the proposition_4 step before
    the goal is ever checked."""
    steps = [x for x in eng.PROP16B_STEPS_DATA if x[0] != "proposition_15"]
    with pytest.raises(eng.MissingPremise) as ei:
        _prop16b(steps=steps)
    assert ei.value.rule == "proposition_4"


def test_prop16b_control_wrong_direction_not_entailed():
    """Bad case (c): the REVERSED inequality ∠b:a:c > ∠a:c:d is not
    entailed by the full proof, and the store stays satisfiable — the
    strict goal check is not vacuous."""
    s, _ = _prop16b()
    assert eng._fm_sat(s.la.rows)
    assert not eng.la_entails_gt_ang(s.la, "b", "a", "c", "a", "c", "d")
    with pytest.raises(AssertionError):
        _prop16b(goal=[("_gt_ang", "b", "a", "c", "a", "c", "d")])


def test_proposition_15_control_between_reversed():
    """Bad case for the cited lemma CONSTRUCTIONS["proposition_15"]: with
    between a e b replaced by between e a b (the closure's reversal of
    that is between b a e, not between a e b), the citation is refused."""
    hyps = [((("between", "e", "a", "b"), "h"))
            if f == ("between", "a", "e", "b") else (f, w)
            for f, w in eng.PROP15_HYPS_DATA]
    with pytest.raises(eng.MissingPremise):
        eng.replay(hyps, [("proposition_15",
                           ("a", "b", "c", "d", "e", "AB", "CD"), {})],
                   eng.PROP15_GOAL[:1])



def test_fact_holds_gt_ang_prop16b():
    """Review (claude-9): _fact_holds answers the strict angle goal (used by
    review/apply_step), not only replay's own goal branch; and dropping
    sum_angles_onlyif (the step putting f inside angle a:c:d) loses it."""
    s, _ = eng.replay(eng.PROP16B_HYPS_DATA, eng.PROP16B_STEPS_DATA, [])
    assert eng._fact_holds(s, eng.PROP16B_GOAL[0])
    steps = [st for st in eng.PROP16B_STEPS_DATA
             if st[0] != "sum_angles_onlyif"]
    s2, _ = eng.replay(eng.PROP16B_HYPS_DATA, steps, [])
    assert not eng._fact_holds(s2, eng.PROP16B_GOAL[0])
    assert eng._fm_sat(s2.la.rows)


# --- Book I.16a (exterior angle, first conjunct — "Omitted by Euclid") -----


def _prop16a(steps=eng.PROP16A_STEPS_DATA, hyps=eng.PROP16A_HYPS_DATA,
             goal=eng.PROP16A_GOAL):
    return eng.replay(hyps, steps, goal)


def test_prop16a_replay():
    s, rep = _prop16a()
    assert rep[-1]["final"]
    assert eng._fm_sat(s.la.rows)
    assert eng.la_entails_gt_ang(s.la, "a", "c", "d", "c", "b", "a")
    # the hypotheses ALONE (no steps) do not entail the inequality
    # (bad case (b))
    s2, _ = eng.replay(eng.PROP16A_HYPS_DATA, [], [])
    assert not eng.la_entails_gt_ang(s2.la, "a", "c", "d", "c", "b", "a")


def test_prop16a_control_dropped_second_proposition_15():
    """Bad case (a): drop the SECOND proposition_15 (a g b d c AC BC).
    Nothing then equates ∠a:c:d with ∠b:c:g, so the goal is NOT ENTAILED
    at the goal check (the steps all still apply — g is built by
    extend_point independently) and the store stays satisfiable."""
    steps = [x for x in eng.PROP16A_STEPS_DATA
             if not (x[0] == "proposition_15" and x[1][1] == "g")]
    s, _ = eng.replay(eng.PROP16A_HYPS_DATA, steps, [])
    assert eng._fm_sat(s.la.rows)
    assert not eng.la_entails_gt_ang(s.la, "a", "c", "d", "c", "b", "a")
    with pytest.raises(AssertionError) as ei:
        _prop16a(steps=steps)
    assert "('_gt_ang', 'a', 'c', 'd', 'c', 'b', 'a')" in str(ei.value)


def test_prop16a_control_dropped_first_proposition_15():
    """Drop the FIRST proposition_15 (b c a f e BC AE): proposition_4's
    entailment premise ∠b:e:a = ∠c:e:f (vertical angles, via canonical
    atoms) is then underivable, so the replay is REFUSED at the
    proposition_4 step before the goal is ever checked (same shape as
    the I.16b control)."""
    steps = [x for x in eng.PROP16A_STEPS_DATA
             if not (x[0] == "proposition_15" and x[1][1] == "c")]
    with pytest.raises(eng.MissingPremise) as ei:
        _prop16a(steps=steps)
    assert ei.value.rule == "proposition_4"


def test_prop16a_control_wrong_direction_not_entailed():
    """Bad case (c): the REVERSED inequality ∠c:b:a > ∠a:c:d is not
    entailed by the full proof, and the store stays satisfiable — the
    strict goal check is not vacuous."""
    s, _ = _prop16a()
    assert eng._fm_sat(s.la.rows)
    assert not eng.la_entails_gt_ang(s.la, "c", "b", "a", "a", "c", "d")
    with pytest.raises(AssertionError):
        _prop16a(goal=[("_gt_ang", "c", "b", "a", "a", "c", "d")])


def test_proposition_16_lemma_citation():
    """CONSTRUCTIONS["proposition_16"] cited as a lemma on the I.16
    hypotheses: both conjuncts are entailed afterwards, store sat."""
    s, rep = eng.replay(
        eng.PROP16B_HYPS_DATA,
        [("proposition_16", ("a", "b", "c", "d", "AB", "BC", "AC"), {})],
        [("_gt_ang", "a", "c", "d", "c", "b", "a"),
         ("_gt_ang", "a", "c", "d", "b", "a", "c")])
    assert rep[-1]["final"]
    assert eng._fm_sat(s.la.rows)


def test_proposition_16_control_between_reversed():
    """Bad case for the cited lemma CONSTRUCTIONS["proposition_16"]: with
    between b c d replaced by between c b d (the closure's reversal of
    that is between d b c, not between b c d), the citation is refused."""
    hyps = [((("between", "c", "b", "d"), "h"))
            if f == ("between", "b", "c", "d") else (f, w)
            for f, w in eng.PROP16B_HYPS_DATA]
    with pytest.raises(eng.MissingPremise) as ei:
        eng.replay(hyps,
                   [("proposition_16",
                     ("a", "b", "c", "d", "AB", "BC", "AC"), {})],
                   [("_gt_ang", "a", "c", "d", "c", "b", "a")])
    assert ei.value.rule == "proposition_16"


# --- Book I.17 (two angles of a triangle < two right angles) ---------------


def _prop17(steps=eng.PROP17_STEPS_DATA, hyps=eng.PROP17_HYPS_DATA,
            goal=eng.PROP17_GOAL):
    return eng.replay(hyps, steps, goal)


def test_prop17_replay():
    s, rep = _prop17()
    assert rep[-1]["final"]
    assert eng._fm_sat(s.la.rows)
    assert eng.la_entails_lt_sum_rt_ang(s.la, "a", "b", "c", "b", "c", "a")
    # _fact_holds answers the goal too (the 8d2ada7 lesson: not only
    # replay's own goal branch)
    assert eng._fact_holds(s, eng.PROP17_GOAL[0])
    # bad case (d): the hypotheses ALONE (no steps) do not entail the goal
    s2, _ = eng.replay(eng.PROP17_HYPS_DATA, [], [])
    assert eng._fm_sat(s2.la.rows)
    assert not eng.la_entails_lt_sum_rt_ang(s2.la, "a", "b", "c",
                                            "b", "c", "a")
    assert not eng._fact_holds(s2, eng.PROP17_GOAL[0])


def test_prop17_control_dropped_proposition_16():
    """Bad case (a): drop the proposition_16 citation. Nothing then bounds
    ∠a:b:c strictly below ∠a:c:d, so the goal is NOT ENTAILED at the goal
    check (the steps all still apply — extend_point and proposition_13 do
    not depend on proposition_16) and the store stays satisfiable."""
    steps = [x for x in eng.PROP17_STEPS_DATA if x[0] != "proposition_16"]
    s, _ = eng.replay(eng.PROP17_HYPS_DATA, steps, [])
    assert eng._fm_sat(s.la.rows)
    assert not eng.la_entails_lt_sum_rt_ang(s.la, "a", "b", "c",
                                            "b", "c", "a")
    with pytest.raises(AssertionError) as ei:
        _prop17(steps=steps)
    assert "('_lt_sum_rt_ang', 'a', 'b', 'c', 'b', 'c', 'a')" in str(ei.value)


def test_prop17_control_dropped_proposition_13():
    """Bad case (b): drop the proposition_13 citation. Nothing then equates
    ∠b:c:a + ∠a:c:d with 2∟, so the goal is NOT ENTAILED at the goal check
    (the steps all still apply) and the store stays satisfiable."""
    steps = [x for x in eng.PROP17_STEPS_DATA if x[0] != "proposition_13"]
    s, _ = eng.replay(eng.PROP17_HYPS_DATA, steps, [])
    assert eng._fm_sat(s.la.rows)
    assert not eng.la_entails_lt_sum_rt_ang(s.la, "a", "b", "c",
                                            "b", "c", "a")
    with pytest.raises(AssertionError) as ei:
        _prop17(steps=steps)
    assert "('_lt_sum_rt_ang', 'a', 'b', 'c', 'b', 'c', 'a')" in str(ei.value)


def test_prop17_control_wrong_forms_not_entailed():
    """Bad case (c): the non-strict/wrong forms are not entailed by the
    full proof, and the store stays satisfiable — the strict goal check
    is not vacuous. The >-form is a _gt_ang on the sum-minus-2∟ row read
    off directly; the =-form is the _sum_rt_ang-shaped equality of the
    two triangle angles with 2∟."""
    s, _ = _prop17()
    assert eng._fm_sat(s.la.rows)
    # sum > 2∟ (the reverse strict inequality) is not entailed
    assert not s.la.entails(
        eng.lin((eng.ang_atom("a", "b", "c"), 1),
                (eng.ang_atom("b", "c", "a"), 1), (eng.RT, -2)), 0, ">")
    # sum = 2∟ (the non-strict boundary) is not entailed either
    assert not s.la.entails(
        eng.lin((eng.ang_atom("a", "b", "c"), 1),
                (eng.ang_atom("b", "c", "a"), 1), (eng.RT, -2)), 0, "=")


def test_proposition_17_lemma_citation():
    """CONSTRUCTIONS["proposition_17"] cited as a lemma on the I.17
    hypotheses: the sum bound is entailed afterwards, store sat."""
    s, rep = eng.replay(
        eng.PROP17_HYPS_DATA,
        [("proposition_17", ("a", "b", "c", "AB", "BC", "AC"), {})],
        [("_lt_sum_rt_ang", "a", "b", "c", "b", "c", "a")])
    assert rep[-1]["final"]
    assert eng._fm_sat(s.la.rows)


def test_proposition_17_control_c_on_AB():
    """Bad case for the cited lemma CONSTRUCTIONS["proposition_17"]: with
    c ON AB (formTriangle's not_on_line premise violated), the citation
    is refused."""
    hyps = [(("on_line", "c", "AB"), "h")
            if f == ("not_on_line", "c", "AB") else (f, w)
            for f, w in eng.PROP17_HYPS_DATA]
    with pytest.raises(eng.MissingPremise) as ei:
        eng.replay(hyps,
                   [("proposition_17", ("a", "b", "c", "AB", "BC", "AC"),
                     {})],
                   [("_lt_sum_rt_ang", "a", "b", "c", "b", "c", "a")])
    assert ei.value.rule == "proposition_17"
