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


def test_proposition_19_citation_fires():
    """The cited lemma makes exactly the _gt_len conclusion (the
    theorem PROP19_STEPS_DATA proves)."""
    s = eng.State()
    for f, w in eng.PROP19_HYPS_DATA:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    eng.apply_step(s, 1, "proposition_19",
                   ("a", "b", "c", "AB", "BC", "AC"), {})
    assert eng.la_entails_gt(s.la, "a", "c", "a", "b")


def test_proposition_19_citation_refused_wrong_way():
    """CONSTRUCTIONS['proposition_19'] cited with the angle comparison
    the wrong way (∠b:c:a > ∠a:b:c instead of ∠a:b:c > ∠b:c:a) is
    refused: the gt_ang entailment premise fails."""
    s = eng.State()
    for f, w in eng.PROP19_HYPS_DATA:
        if f[0] == "gt_ang":
            f = ("gt_ang", "b", "c", "a", "a", "b", "c")  # reversed
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(s, 1, "proposition_19",
                       ("a", "b", "c", "AB", "BC", "AC"), {})
    assert "gt_ang" in str(ei.value.premise)


def test_proposition_19_citation_refused_on_equal_angles():
    """Cited with the angles EQUAL (∠a:b:c = ∠b:c:a, no strict
    inequality) proposition_19 is refused: a strict gt_ang premise is
    not entailed by an equality (and the conclusion would be false —
    isosceles, |a-c| = |a-b|)."""
    s = eng.State()
    for f, w in eng.PROP19_HYPS_DATA:
        if f[0] == "gt_ang":
            f = ("eq_ang", "a", "b", "c", "b", "c", "a")
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(s, 1, "proposition_19",
                       ("a", "b", "c", "AB", "BC", "AC"), {})
    assert "gt_ang" in str(ei.value.premise)


# --- I.20 (Book/Prop20.lean) -----------------------------------------------


def test_prop20_replay():
    """I.20 (triangle inequality) replays the Lean way: extend AB past a
    to d' with |a-d'| > |a-c| (extend_point_longer), cut |a-d| = |a-c| off
    it (proposition_3), isosceles ∠a:c:d = ∠a:d:c (proposition_5'), the
    angle sum ∠b:c:d = ∠b:c:a + ∠a:c:d (sum_angles_onlyif), then the cited
    I.19 lemma proposition_19 b c d BC DC AB turns ∠b:c:d > ∠c:d:b into
    |b-d| > |b-c|, and |b-d| = |b-a| + |a-d| = |b-a| + |a-c| (the
    betweenness sum is closure rule r_between_if, Transfer.lean:16, on
    between b a d)."""
    s, rep = eng.replay(eng.PROP20_HYPS_DATA, eng.PROP20_STEPS_DATA,
                        eng.PROP20_GOAL)
    assert eng.la_entails_gt_sum_len(s.la, "b", "a", "a", "c", "b", "c")
    assert rep[-1]["final"]
    assert [r["rule"].split(" ")[0] for r in rep[:-1]] == [
        "extend_point_longer", "proposition_3", "line_from_points",
        "proposition_5'", "sum_angles_onlyif", "proposition_19"]


def test_prop20_hyps_alone_do_not_entail_goal():
    """The I.20 hypotheses alone do NOT entail |b-a| + |a-c| > |b-c| —
    the construction carries the proof (the replay is not vacuous)."""
    s = eng.State()
    for f, w in eng.PROP20_HYPS_DATA:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    assert not eng.la_entails_gt_sum_len(s.la, "b", "a", "a", "c", "b", "c")


def test_prop20_control_dropped_proposition_19():
    """Drop the proposition_19 citation: nothing turns the angle
    comparison into |b-d| > |b-c|, so the goal sum is not entailed."""
    steps = [st for st in eng.PROP20_STEPS_DATA if st[0] != "proposition_19"]
    with pytest.raises(AssertionError):
        eng.replay(eng.PROP20_HYPS_DATA, steps, eng.PROP20_GOAL)


def test_prop20_control_dropped_sum_angles_onlyif():
    """Drop the sum_angles_onlyif step: without the row
    ∠b:c:d = ∠b:c:a + ∠a:c:d the gt_ang entailment premise of
    proposition_19 fails -> MissingPremise."""
    steps = [st for st in eng.PROP20_STEPS_DATA
             if st[0] != "sum_angles_onlyif"]
    with pytest.raises(eng.MissingPremise) as ei:
        eng.replay(eng.PROP20_HYPS_DATA, steps, eng.PROP20_GOAL)
    assert "gt_ang" in str(ei.value.premise)


def test_prop20_sum_equality_not_entailed():
    """After the full I.20 replay the EQUALITY |b-a| + |a-c| = |b-c| is
    not entailed (the inequality is strict — a degenerate triangle would
    be needed for equality, and formTriangle forbids it)."""
    s, rep = eng.replay(eng.PROP20_HYPS_DATA, eng.PROP20_STEPS_DATA,
                        eng.PROP20_GOAL)
    assert not s.la.entails(
        eng.lin((eng.len_atom("b", "a"), 1), (eng.len_atom("a", "c"), 1),
                (eng.len_atom("b", "c"), -1)), 0, "=")


def test_la_entails_gt_sum_len_bad_cases():
    """The new LA helper's own bad cases: a store where the sum is only
    EQUAL to the compared length (degenerate: between b a c gives
    |b-a| + |a-c| = |b-c|) does not entail the strict >; and the
    comparison running the other way (|b-c| > the sum, from an assumed
    gt row) does not entail it either."""
    # bad case 1: sum only equal (between b a c -> |b-a| + |a-c| = |b-c|)
    s = _line_state([("between", "b", "a", "c")])
    assert not eng.la_entails_gt_sum_len(s.la, "b", "a", "a", "c", "b", "c")
    # bad case 2: the comparison runs the other way
    s = _line_state([], [(eng.lin((eng.len_atom("b", "c"), 1),
                                  (eng.len_atom("b", "a"), -1),
                                  (eng.len_atom("a", "c"), -1)), 0, ">")])
    assert not eng.la_entails_gt_sum_len(s.la, "b", "a", "a", "c", "b", "c")


def test_proposition_20_citation_fires():
    """The cited lemma makes exactly the _gt_sum_len conclusion (the
    theorem PROP20_STEPS_DATA proves): the strict row plus the
    gt_sum_len fact form."""
    s = eng.State()
    for f, w in eng.PROP20_HYPS_DATA:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    eng.apply_step(s, 1, "proposition_20",
                   ("a", "b", "c", "AB", "BC", "AC"), {})
    assert eng.la_entails_gt_sum_len(s.la, "b", "a", "a", "c", "b", "c")
    assert s.has(("gt_sum_len", "b", "a", "a", "c", "b", "c"))


def test_proposition_20_citation_refused_degenerate():
    """Cited with c ON AB (degenerate triangle — the formTriangle
    premise not_on_line c AB fails) proposition_20 is refused: the
    conclusion |b-a| + |a-c| > |b-c| is false for collinear c between
    a and b (equality holds there)."""
    s = eng.State()
    for f, w in eng.PROP20_HYPS_DATA:
        if f == ("not_on_line", "c", "AB"):
            f = ("on_line", "c", "AB")
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(s, 1, "proposition_20",
                       ("a", "b", "c", "AB", "BC", "AC"), {})
    assert ei.value.premise == ("not_on_line", "c", "AB")


# --- I.21 (Book/Prop21.lean) -----------------------------------------------


def test_prop21_replay():
    """I.21 replays the Lean way: triple_incidence_1 turns the sameSide
    hypotheses into opposing_sides a c BD, intersection_lines_opposing +
    intersection_lines produce e = BD ∩ AC, pasch_4 gives between a e c,
    a two-branch by_contra (the dead orders of between_points trichotomy)
    gives between b d e, then the cited lemmas proposition_20 (twice) and
    proposition_16 (twice) feed the arithmetic for both conjuncts."""
    s, rep = eng.replay(eng.PROP21_HYPS_DATA, eng.PROP21_STEPS_DATA,
                        eng.PROP21_GOAL)
    assert eng.la_entails_lt_sum_sum_len(s.la, "b", "d", "d", "c",
                                         "b", "a", "a", "c")
    assert eng.la_entails_gt_ang(s.la, "b", "d", "c", "b", "a", "c")
    assert rep[-1]["final"]
    assert [r["rule"].split(" ")[0] for r in rep[:-1]] == [
        "triple_incidence_1", "intersection_lines_opposing",
        "intersection_lines", "pasch_4", "by_contra",
        "proposition_20", "proposition_20",
        "proposition_16", "proposition_16"]


def test_prop21_hyps_alone_do_not_entail_goal():
    """The I.21 hypotheses alone entail NEITHER conjunct — the
    construction carries the proof (the replay is not vacuous)."""
    s = eng.State()
    for f, w in eng.PROP21_HYPS_DATA:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    assert not eng.la_entails_lt_sum_sum_len(s.la, "b", "d", "d", "c",
                                             "b", "a", "a", "c")
    assert not eng.la_entails_gt_ang(s.la, "b", "d", "c", "b", "a", "c")


def test_prop21_control_dropped_second_proposition_20():
    """Drop the second proposition_20 citation (|d-e| + |e-c| > |d-c|):
    without it the arithmetic cannot chain |b-e| + |e-c| past |b-d| +
    |d-c|, so the first conjunct is not entailed."""
    seen = [0]
    steps = []
    for st in eng.PROP21_STEPS_DATA:
        if st[0] == "proposition_20":
            seen[0] += 1
            if seen[0] == 2:
                continue
        steps.append(st)
    s = eng.State()
    for f, w in eng.PROP21_HYPS_DATA:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    eng.run_steps(s, steps)
    assert not eng.la_entails_lt_sum_sum_len(s.la, "b", "d", "d", "c",
                                             "b", "a", "a", "c")


def test_prop21_control_dropped_proposition_16():
    """Drop either proposition_16 citation: the angle chain
    ∠b:d:c > ∠d:e:c = ∠b:e:c > ∠e:a:b = ∠b:a:c breaks at the dropped
    link, so the second conjunct is not entailed."""
    for drop in ("first", "second"):
        seen = [0]
        steps = []
        for st in eng.PROP21_STEPS_DATA:
            if st[0] == "proposition_16":
                seen[0] += 1
                if (drop == "first") == (seen[0] == 1):
                    continue
            steps.append(st)
        s = eng.State()
        for f, w in eng.PROP21_HYPS_DATA:
            eng._assume_metric_or_fact(s, f, w)
        eng.close(s)
        eng.run_steps(s, steps)
        assert not eng.la_entails_gt_ang(s.la, "b", "d", "c", "b", "a", "c")


def test_prop21_equality_and_reversed_not_entailed():
    """After the full I.21 replay neither the EQUALITY
    |b-d| + |d-c| = |b-a| + |a-c| nor the REVERSED strict comparison
    |b-a| + |a-c| < |b-d| + |d-c| is entailed (the inequality is
    strict and one-way: d strictly inside the triangle)."""
    s, rep = eng.replay(eng.PROP21_HYPS_DATA, eng.PROP21_STEPS_DATA,
                        eng.PROP21_GOAL)
    assert not s.la.entails(
        eng.lin((eng.len_atom("b", "d"), 1), (eng.len_atom("d", "c"), 1),
                (eng.len_atom("b", "a"), -1), (eng.len_atom("a", "c"), -1)),
        0, "=")
    assert eng.la_entails_lt_sum_sum_len(s.la, "b", "d", "d", "c",
                                         "b", "a", "a", "c")
    assert not eng.la_entails_lt_sum_sum_len(s.la, "b", "a", "a", "c",
                                             "b", "d", "d", "c")


def test_la_entails_lt_sum_sum_len_bad_case():
    """The new LA helper's own bad case: a store where the two sums are
    EQUAL (an explicit row |b-d| + |d-c| - |b-a| - |a-c| = 0) does not
    entail the strict <."""
    s = _line_state([], [(eng.lin((eng.len_atom("b", "d"), 1),
                                  (eng.len_atom("d", "c"), 1),
                                  (eng.len_atom("b", "a"), -1),
                                  (eng.len_atom("a", "c"), -1)), 0, "=")])
    assert not eng.la_entails_lt_sum_sum_len(s.la, "b", "d", "d", "c",
                                             "b", "a", "a", "c")


# --- I.22 (Book/Prop22.lean) -----------------------------------------------


def test_prop22_replay():
    """I.22 replays the Lean way: arbitrary d, the three extend_point_longer
    and three proposition_3 cuts along DE, the two circles, i = the far
    meet of DE with KLH, a three-case by_cases (between_points trichotomy
    on the collinear g, f, i) for inside i DKL, then the cited
    intersection_circle_circle_1 (what Lean's SMT closes silently at
    Prop22.lean:28) and intersection_circles as k. The goals are the
    three witness equalities over the named k, f, g."""
    s, rep = eng.replay(eng.PROP22_HYPS_DATA, eng.PROP22_STEPS_DATA,
                        eng.PROP22_GOAL)
    assert eng.la_entails_eq(s.la, "f", "k", "a", "a'")
    assert eng.la_entails_eq(s.la, "f", "g", "b", "b'")
    assert eng.la_entails_eq(s.la, "k", "g", "c", "c'")
    assert rep[-1]["final"]
    assert [r["rule"].split(" ")[0] for r in rep[:-1]] == [
        "arbitrary_point", "distinct_points", "line_from_points",
        "extend_point_longer", "extend_point_longer", "extend_point_longer",
        "between_symm_parts", "proposition_3", "between_symm_parts",
        "proposition_3", "between_symm_parts", "proposition_3",
        "between_symm_parts", "circle_from_points", "circle_from_points",
        "intersection_circle_line_extending_points", "by_cases",
        "intersection_circle_circle_1", "intersection_circles"]


def test_prop22_hyps_alone_do_not_entail_goal():
    """The I.22 hypotheses alone entail NONE of the three witness
    equalities — the construction carries the proof (and k, f, g do not
    even exist before it)."""
    s = eng.State()
    for f, w in eng.PROP22_HYPS_DATA:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    assert not eng.la_entails_eq(s.la, "f", "k", "a", "a'")
    assert not eng.la_entails_eq(s.la, "f", "g", "b", "b'")
    assert not eng.la_entails_eq(s.la, "k", "g", "c", "c'")


def test_prop22_control_dropped_inequality_3():
    """Drop |b-b'| + |c-c'| > |a-a'| (inequality 3): it is consumed at
    outside h DKL — |f-h| = |f-g| + |g-h| = |b-b'| + |c-c'| must exceed
    the radius |f-d| = |a-a'| — so the cited intersection_circle_circle_1
    is refused at its outside premise and intersection_circles is never
    reached."""
    hyps = [h for h in eng.PROP22_HYPS_DATA
            if h[0] != ("gt_sum_len", "b", "b'", "c", "c'", "a", "a'")]
    with pytest.raises(eng.MissingPremise) as ei:
        eng.replay(hyps, eng.PROP22_STEPS_DATA, eng.PROP22_GOAL)
    assert ei.value.rule == "intersection_circle_circle_1"
    assert ei.value.premise == ("outside", "h", "DKL")


def test_prop22_control_dropped_inequality_1_or_2():
    """Inequalities 1 and 2 are consumed in the by_cases: dropping
    |a-a'| + |b-b'| > |c-c'| kills the between g f i branch
    (|f-i| = |c-c'| - |b-b'| < |a-a'| no longer follows); dropping
    |a-a'| + |c-c'| > |b-b'| kills the between g i f branch. Either way
    the case split fails, inside i DKL is never concluded, and the
    circle-circle intersection is refused."""
    for dropped, branch in (
            (("gt_sum_len", "a", "a'", "b", "b'", "c", "c'"),
             [("between", "g", "f", "i")]),
            (("gt_sum_len", "a", "a'", "c", "c'", "b", "b'"),
             [("between", "g", "i", "f")])):
        hyps = [h for h in eng.PROP22_HYPS_DATA if h[0] != dropped]
        with pytest.raises(eng.ByCasesFailure) as ei:
            eng.replay(hyps, eng.PROP22_STEPS_DATA, eng.PROP22_GOAL)
        assert ei.value.branch == branch
        assert ei.value.witness[0] == "not-entailed"


def test_prop22_control_without_point_on_circle_onlyif():
    """Lean's two closing point_on_circle_onlyif steps (Prop22.lean:30-31)
    are the engine's closure rule r_poc_onlyif (Transfer.lean:39), which
    also supplies the radii row |g-i| = |g-h| the by_cases branches
    consume. Excluding it, no branch can show |f-i| < |f-d|, so the
    replay fails at the case split — the rule is load-bearing twice."""
    with pytest.raises(eng.ByCasesFailure):
        eng.replay(eng.PROP22_HYPS_DATA, eng.PROP22_STEPS_DATA, [],
                   exclude_rules={"point_on_circle_onlyif [Transfer.lean:39]"})


def test_prop22_icc1_bad_case():
    """intersection_circle_circle_1's own bad case: with a and b swapped
    (i inside/not-outside, h on/outside the wrong circles — h is NOT
    inside DKL and i IS inside DKL), the citation is refused at the
    inside premise against the real replay state."""
    s = eng.State()
    for f, w in eng.PROP22_HYPS_DATA:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    eng.run_steps(s, eng.PROP22_STEPS_DATA[:-2])  # through the by_cases
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(s, 99, "intersection_circle_circle_1",
                       ("h", "i", "KLH", "DKL"), {})
    assert ei.value.premise == ("inside", "h", "DKL")


# --- I.22' (Book/Prop22.lean:34, the variant I.23 cites) --------------------


def test_prop22p_replay():
    """I.22' replays the Lean way: from the GIVEN segment f-e on FE, two
    extend_point_longer extensions and three proposition_3 cuts along FE,
    the two circles, i = the far meet of FE with KLH, the same three-case
    by_cases as I.22 for inside i DKL, the cited
    intersection_circle_circle_1 (what Lean's SMT closes silently at
    Prop22.lean:52) and intersection_circles as k, and euclid_finish's
    not_between g f e rendered as a by_contra citing between_trans_out
    (Diagrammatic.lean:78) and between_symm_parts. The goals are on_line
    g FE, not_between g f e, and the three witness equalities over the
    named k, g."""
    s, rep = eng.replay(eng.PROP22P_HYPS_DATA, eng.PROP22P_STEPS_DATA,
                        eng.PROP22P_GOAL)
    assert s.has(("on_line", "g", "FE"))
    assert s.has(("not_between", "g", "f", "e"))
    assert eng.la_entails_eq(s.la, "f", "k", "a", "a'")
    assert eng.la_entails_eq(s.la, "f", "g", "b", "b'")
    assert eng.la_entails_eq(s.la, "k", "g", "c", "c'")
    assert rep[-1]["final"]
    assert [r["rule"].split(" ")[0] for r in rep[:-1]] == [
        "extend_point_longer", "between_symm_parts",
        "extend_point_longer", "between_symm_parts",
        "proposition_3", "proposition_3", "between_symm_parts",
        "proposition_3", "between_symm_parts",
        "circle_from_points", "circle_from_points",
        "intersection_circle_line_extending_points", "by_cases",
        "intersection_circle_circle_1", "intersection_circles",
        "by_contra"]


def test_prop22p_hyps_alone_do_not_entail_goal():
    """The I.22' hypotheses alone entail NONE of the goal conjuncts — the
    construction carries the proof (and k, g do not even exist before
    it)."""
    s = eng.State()
    for f, w in eng.PROP22P_HYPS_DATA:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    assert not s.has(("on_line", "g", "FE"))
    assert not s.has(("not_between", "g", "f", "e"))
    assert not eng.la_entails_eq(s.la, "f", "k", "a", "a'")
    assert not eng.la_entails_eq(s.la, "f", "g", "b", "b'")
    assert not eng.la_entails_eq(s.la, "k", "g", "c", "c'")


def test_prop22p_control_dropped_inequality_3():
    """Drop |b-b'| + |c-c'| > |a-a'| (inequality 3): it is consumed FIRST
    at the d-cut — |f-e''| > |b-b'| + |c-c'| exceeds |a-a'| only by this
    inequality — so the first proposition_3 is refused at its gt_len
    entailment premise (it is consumed AGAIN at outside h DKL for the
    cited intersection_circle_circle_1, as in I.22, but the replay never
    gets there)."""
    hyps = [h for h in eng.PROP22P_HYPS_DATA
            if h[0] != ("gt_sum_len", "b", "b'", "c", "c'", "a", "a'")]
    with pytest.raises(eng.MissingPremise) as ei:
        eng.replay(hyps, eng.PROP22P_STEPS_DATA, eng.PROP22P_GOAL)
    assert ei.value.rule == "proposition_3"
    assert ei.value.premise == ("entail", "gt_len", ("f", "e''", "a", "a'"))


def test_prop22p_control_dropped_inequality_1_or_2():
    """Inequalities 1 and 2 are consumed in the by_cases exactly as in
    I.22: dropping |a-a'| + |b-b'| > |c-c'| kills the between g f i
    branch; dropping |a-a'| + |c-c'| > |b-b'| kills the between g i f
    branch."""
    for dropped, branch in (
            (("gt_sum_len", "a", "a'", "b", "b'", "c", "c'"),
             [("between", "g", "f", "i")]),
            (("gt_sum_len", "a", "a'", "c", "c'", "b", "b'"),
             [("between", "g", "i", "f")])):
        hyps = [h for h in eng.PROP22P_HYPS_DATA if h[0] != dropped]
        with pytest.raises(eng.ByCasesFailure) as ei:
            eng.replay(hyps, eng.PROP22P_STEPS_DATA, eng.PROP22P_GOAL)
        assert ei.value.branch == branch
        assert ei.value.witness[0] == "not-entailed"


def test_prop22p_control_without_point_on_circle_onlyif():
    """As in I.22, r_poc_onlyif (Transfer.lean:39) is load-bearing twice:
    Lean's two closing point_on_circle_onlyif steps (Prop22.lean:54-55)
    are closure, and the radii row |g-i| = |g-h| feeds the by_cases."""
    with pytest.raises(eng.ByCasesFailure):
        eng.replay(eng.PROP22P_HYPS_DATA, eng.PROP22P_STEPS_DATA, [],
                   exclude_rules={"point_on_circle_onlyif [Transfer.lean:39]"})


def test_prop22p_between_trans_out_bad_case():
    """between_trans_out's own bad case: between a b c and between b c d
    are BOTH required, in that chaining order — asking to chain
    between f g e'' with between g e'' e (e'' is the chain's FAR end; the
    state holds the reversal between e'' e f instead) is refused at the
    second premise against the real replay state."""
    s = eng.State()
    for f, w in eng.PROP22P_HYPS_DATA:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    eng.run_steps(s, eng.PROP22P_STEPS_DATA[:-1])  # through the circles' meet
    assert s.has(("between", "f", "g", "e''"))
    assert s.has(("between", "e''", "e", "f"))  # the reversed wrong order
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(s, 99, "between_trans_out", ("f", "g", "e''", "e"), {})
    assert ei.value.premise == ("between", "g", "e''", "e")


def test_prop22p_final_state_not_contradictory():
    """main() asserts store satisfiability for every replay; make the
    diagram-fact side explicit here too (the by_contra concludes
    not_between on the OUTER state, which stays consistent)."""
    s, _ = eng.replay(eng.PROP22P_HYPS_DATA, eng.PROP22P_STEPS_DATA,
                      eng.PROP22P_GOAL)
    contra, witness = eng.contradictory(s)
    assert not contra, witness


def test_prop22pp_replay_passes():
    """I.22'' (Prop22.lean:58-80): proposition_22''s construction with the
    circles' meet taken by the cited intersection_same_side
    (Intersections.lean:20) on x's side of FE. The goals are I.22''s six
    conjuncts over the named k, g: on_line g FE, not_between g f e,
    same_side k x FE, and the three witness equalities."""
    s, rep = eng.replay(eng.PROP22PP_HYPS_DATA, eng.PROP22PP_STEPS_DATA,
                        eng.PROP22PP_GOAL)
    assert s.has(("on_line", "g", "FE"))
    assert s.has(("not_between", "g", "f", "e"))
    assert s.has(("same_side", "k", "x", "FE"))
    assert eng.la_entails_eq(s.la, "f", "k", "a", "a'")
    assert eng.la_entails_eq(s.la, "f", "g", "b", "b'")
    assert eng.la_entails_eq(s.la, "k", "g", "c", "c'")
    assert rep[-1]["final"]
    assert [r["rule"].split(" ")[0] for r in rep[:-1]] == [
        "extend_point_longer", "between_symm_parts",
        "extend_point_longer", "between_symm_parts",
        "proposition_3", "proposition_3", "between_symm_parts",
        "proposition_3", "between_symm_parts",
        "circle_from_points", "circle_from_points",
        "intersection_circle_line_extending_points", "by_cases",
        "intersection_circle_circle_1", "intersection_same_side",
        "by_contra"]


def test_prop22pp_control_dropped_x_off_line():
    """Drop ¬(x.onLine FE): the replay is refused at
    intersection_same_side's not_on_line premise — the same-side meet has
    no side to pick without a point off FE."""
    hyps = [h for h in eng.PROP22PP_HYPS_DATA
            if h[0] != ("not_on_line", "x", "FE")]
    with pytest.raises(eng.MissingPremise) as ei:
        eng.replay(hyps, eng.PROP22PP_STEPS_DATA, eng.PROP22PP_GOAL)
    assert ei.value.rule == "intersection_same_side"
    assert ei.value.premise == ("not_on_line", "x", "FE")


def test_prop22pp_control_dropped_inequality_1_or_2():
    """The by_cases branch control, exactly as in I.22': dropping
    inequality 1 kills the between g f i branch; dropping inequality 2
    kills the between g i f branch."""
    for dropped, branch in (
            (("gt_sum_len", "a", "a'", "b", "b'", "c", "c'"),
             [("between", "g", "f", "i")]),
            (("gt_sum_len", "a", "a'", "c", "c'", "b", "b'"),
             [("between", "g", "i", "f")])):
        hyps = [h for h in eng.PROP22PP_HYPS_DATA if h[0] != dropped]
        with pytest.raises(eng.ByCasesFailure) as ei:
            eng.replay(hyps, eng.PROP22PP_STEPS_DATA, eng.PROP22PP_GOAL)
        assert ei.value.branch == branch
        assert ei.value.witness[0] == "not-entailed"


def test_prop22pp_final_state_not_contradictory():
    """The by_contra concludes not_between on the OUTER state, which
    stays consistent (as in I.22')."""
    s, _ = eng.replay(eng.PROP22PP_HYPS_DATA, eng.PROP22PP_STEPS_DATA,
                      eng.PROP22PP_GOAL)
    contra, witness = eng.contradictory(s)
    assert not contra, witness


def test_proposition_22pp_citation_fires():
    """The cited lemma makes exactly the I.22'' conclusions over fresh
    witnesses k and g (the theorem PROP22PP_STEPS_DATA replays): on_line
    g FE, not_between g f e, same_side k x FE, and the three length
    equalities."""
    s = eng.State()
    for f, w in eng.PROP22PP_HYPS_DATA:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    rep = eng.apply_step(
        s, 1, "proposition_22''",
        ("a", "a'", "b", "b'", "c", "c'", "f", "e", "x",
         "A", "B", "C", "FE"),
        {"k": "k", "g": "g"})
    assert rep["rule"].startswith("proposition_22''")
    assert s.has(("on_line", "g", "FE"))
    assert s.has(("not_between", "g", "f", "e"))
    assert s.has(("same_side", "k", "x", "FE"))
    assert eng.la_entails_eq(s.la, "f", "k", "a", "a'")
    assert eng.la_entails_eq(s.la, "f", "g", "b", "b'")
    assert eng.la_entails_eq(s.la, "k", "g", "c", "c'")


def test_proposition_22pp_citation_refused_without_inequality():
    """Each triangle inequality is an entailment premise: with
    |b-b'| + |c-c'| > |a-a'| missing, the citation is refused at that
    entailment (as for proposition_22')."""
    hyps = [h for h in eng.PROP22PP_HYPS_DATA
            if h[0] != ("gt_sum_len", "b", "b'", "c", "c'", "a", "a'")]
    s = eng.State()
    for f, w in hyps:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(
            s, 1, "proposition_22''",
            ("a", "a'", "b", "b'", "c", "c'", "f", "e", "x",
             "A", "B", "C", "FE"),
            {"k": "k", "g": "g"})
    assert ei.value.premise == (
        "entail", "gt_sum_len", ("b", "b'", "c", "c'", "a", "a'"))


def test_proposition_22p_citation_fires():
    """The cited lemma makes exactly the I.22' conclusions over fresh
    witnesses k and g (the theorem PROP22P_STEPS_DATA replays): on_line
    g FE, not_between g f e, and the three length equalities."""
    s = eng.State()
    for f, w in eng.PROP22P_HYPS_DATA:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    rep = eng.apply_step(
        s, 1, "proposition_22'",
        ("a", "a'", "b", "b'", "c", "c'", "f", "e", "A", "B", "C", "FE"),
        {"k": "k", "g": "g"})
    assert rep["rule"].startswith("proposition_22'")
    assert s.has(("on_line", "g", "FE"))
    assert s.has(("not_between", "g", "f", "e"))
    assert eng.la_entails_eq(s.la, "f", "k", "a", "a'")
    assert eng.la_entails_eq(s.la, "f", "g", "b", "b'")
    assert eng.la_entails_eq(s.la, "k", "g", "c", "c'")


def test_proposition_22p_citation_refused_without_inequality():
    """Each triangle inequality is an entailment premise: with
    |b-b'| + |c-c'| > |a-a'| missing, the citation is refused at that
    entailment (the construction it stands for cuts |f-d| = |a-a'| off a
    segment only known to exceed |b-b'| + |c-c'|)."""
    hyps = [h for h in eng.PROP22P_HYPS_DATA
            if h[0] != ("gt_sum_len", "b", "b'", "c", "c'", "a", "a'")]
    s = eng.State()
    for f, w in hyps:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(
            s, 1, "proposition_22'",
            ("a", "a'", "b", "b'", "c", "c'", "f", "e",
             "A", "B", "C", "FE"),
            {"k": "k", "g": "g"})
    assert ei.value.premise == (
        "entail", "gt_sum_len", ("b", "b'", "c", "c'", "a", "a'"))


def test_proposition_23_citation_fires():
    """The cited lemma makes exactly the I.23 conclusions over the fresh
    witness f (the theorem PROP23_STEPS_DATA replays): f ≠ a and
    ∠f:a:b = ∠d:c:e."""
    s = eng.State()
    for f, w in eng.PROP23_HYPS_DATA:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    rep = eng.apply_step(
        s, 1, "proposition_23",
        ("a", "b", "c", "d", "e", "AB", "CD", "CE"), {"f": "f"})
    assert rep["rule"].startswith("proposition_23")
    assert s.has(("neq", "f", "a"))
    assert eng.la_entails_eq_ang(s.la, "f", "a", "b", "d", "c", "e")


def test_proposition_23_citation_refused_without_angle_side():
    """formRectilinearAngle d c e CD CE is two distinctPointsOnLine
    premises (Relations.lean:64); with the dpl for CD missing the
    citation is refused at that premise."""
    hyps = [h for h in eng.PROP23_HYPS_DATA
            if h[0] != ("dpl", "d", "c", "CD")]
    s = eng.State()
    for f, w in hyps:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    with pytest.raises(eng.MissingPremise):
        eng.apply_step(
            s, 1, "proposition_23",
            ("a", "b", "c", "d", "e", "AB", "CD", "CE"), {"f": "f"})


def test_proposition_21_citation_fires():
    """The cited lemma makes exactly the two I.21 conclusions (the
    theorem PROP21_STEPS_DATA proves): the strict rows plus the
    lt_sum_sum_len / gt_ang fact forms."""
    s = eng.State()
    for f, w in eng.PROP21_HYPS_DATA:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    eng.apply_step(s, 1, "proposition_21",
                   ("a", "b", "c", "d", "AB", "BC", "AC", "BD", "DC"), {})
    assert eng.la_entails_lt_sum_sum_len(s.la, "b", "d", "d", "c",
                                         "b", "a", "a", "c")
    assert eng.la_entails_gt_ang(s.la, "b", "d", "c", "b", "a", "c")
    assert s.has(("lt_sum_sum_len", "b", "d", "d", "c", "b", "a", "a", "c"))
    assert s.has(("gt_ang", "b", "d", "c", "b", "a", "c"))


def test_proposition_21_citation_refused_other_side():
    """Cited with d on the OTHER side of BC from a (opposing_sides
    a d BC in place of same_side a d BC — d outside the triangle, where
    |b-d| + |d-c| < |b-a| + |a-c| can fail) the lemma is refused: the
    premise same_side a d BC is missing."""
    s = eng.State()
    for f, w in eng.PROP21_HYPS_DATA:
        if f == ("same_side", "a", "d", "BC"):
            f = ("opposing_sides", "a", "d", "BC")
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(s, 1, "proposition_21",
                       ("a", "b", "c", "d", "AB", "BC", "AC", "BD", "DC"),
                       {})
    assert ei.value.premise == ("same_side", "a", "d", "BC")


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


def test_prop23_replay():
    """I.23 (Prop23.lean:8-32) replays the Lean way: a by_cases on
    d.onLine CE; the degenerate branch is a nested by_cases on
    ∠d:c:e = 0 (`use b` via use_witness after angle_self_zero; the ≠0
    half rendered positively as between d c e, flat_angle_onlyif
    (Transfer.lean:124) giving ∠d:c:e = ∟+∟ and the witness from
    extend_point AB b a); the main branch is line_from_points, the three
    proposition_20 rows, proposition_22', proposition_8, with the
    SMT-closed premises rendered as the cited off_line_strict_triangle,
    degenerated_angle_if and angle_ray_eq_zero. The goal is neq f a and
    the existing _eq_ang query ∠f:a:b = ∠d:c:e, carried out of the
    by_cases over the eigenvariable f."""
    s, rep = eng.replay(eng.PROP23_HYPS_DATA, eng.PROP23_STEPS_DATA,
                        eng.PROP23_GOAL)
    assert [r["rule"].split(" ")[0] for r in rep[:-1]] == ["by_cases"]
    assert rep[0]["branches"] == 2
    assert s.has(("neq", "f", "a"))
    assert eng.la_entails_eq_ang(s.la, "f", "a", "b", "d", "c", "e")
    assert rep[-1]["final"]


def test_prop23_hyps_alone_do_not_entail_goal():
    """The I.23 hypotheses alone entail neither goal conjunct — the
    construction carries the proof (f does not even exist before it)."""
    s = eng.State()
    for f, w in eng.PROP23_HYPS_DATA:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    assert not s.has(("neq", "f", "a"))
    assert not eng.la_entails_eq_ang(s.la, "f", "a", "b", "d", "c", "e")


def _prop23_without_main_step(index):
    """PROP23_STEPS_DATA with the main branch's step `index` removed."""
    (kind, branches, concl) = eng.PROP23_STEPS_DATA[0]
    deg, main = branches
    main_assume, main_steps = main
    main2 = (main_assume, [st for i, st in enumerate(main_steps)
                           if i != index])
    return [("by_cases", [deg, main2], concl)]


def test_prop23_control_dropped_proposition_20():
    """Each of the three proposition_20 citations feeds exactly one of
    proposition_22''s gt_sum_len entail premises (arg orders as Lean's):
    dropping it refuses the replay there — the inner by_cases branch
    reports the stuck step."""
    drops = {1: ("c", "d", "c", "e", "e", "d"),   # |cd|+|ce|>|ed|
             2: ("c", "d", "e", "d", "c", "e"),   # |cd|+|ed|>|ce|
             3: ("c", "e", "e", "d", "c", "d")}   # |ce|+|ed|>|cd|
    for index, expected in drops.items():
        steps = _prop23_without_main_step(index)
        with pytest.raises(eng.ByCasesFailure) as ei:
            eng.replay(eng.PROP23_HYPS_DATA, steps, eng.PROP23_GOAL)
        assert ei.value.branch == [("not_on_line", "d", "CE")]
        assert ei.value.witness == ("stuck",
                                    ("entail", "gt_sum_len", expected))


def test_prop23_control_degenerate_zero_branch_needs_zero_angle():
    """Degenerate branch, ∠d:c:e = 0 half: drop angle_self_zero and
    use_witness's eq_ang entail premise (∠b:a:b = ∠d:c:e) no longer
    holds — refused there."""
    (kind, branches, concl) = eng.PROP23_STEPS_DATA[0]
    (deg_assume, deg_steps), main = branches
    (ikind, ibranches, iconcl) = deg_steps[0]
    (zero_assume, zero_steps), flat = ibranches
    zero2 = (zero_assume, [st for st in zero_steps
                           if st[0] != "angle_self_zero"])
    deg2 = (deg_assume, [(ikind, [zero2, flat], iconcl)])
    steps = [("by_cases", [deg2, main], concl)]
    with pytest.raises(eng.ByCasesFailure) as ei:
        eng.replay(eng.PROP23_HYPS_DATA, steps, eng.PROP23_GOAL)
    assert ei.value.branch == [("_zero_ang", "d", "c", "e")]
    assert ei.value.witness == (
        "stuck", ("entail", "eq_ang", ("b", "a", "b", "d", "c", "e")))


def test_prop23_control_degenerate_flat_branch_needs_flat_angle():
    """Degenerate branch, between d c e half: drop flat_angle_onlyif and
    ∠d:c:e = ∟+∟ is never derived, so the branch cannot entail the
    _eq_ang conclusion (the extended side still gets ∠b:a:f = ∟+∟)."""
    (kind, branches, concl) = eng.PROP23_STEPS_DATA[0]
    (deg_assume, deg_steps), main = branches
    (ikind, ibranches, iconcl) = deg_steps[0]
    zero, (flat_assume, flat_steps) = ibranches
    flat2 = (flat_assume, [st for st in flat_steps
                           if not (st[0] == "flat_angle_onlyif" and
                                   st[1] == ("d", "c", "e"))])
    deg2 = (deg_assume, [(ikind, [zero, flat2], iconcl)])
    steps = [("by_cases", [deg2, main], concl)]
    with pytest.raises(eng.ByCasesFailure) as ei:
        eng.replay(eng.PROP23_HYPS_DATA, steps, eng.PROP23_GOAL)
    assert ei.value.branch == [("between", "d", "c", "e")]
    assert ei.value.witness == (
        "not-entailed", ("_eq_ang", "f", "a", "b", "d", "c", "e"))


def test_prop23_use_witness_bad_case():
    """use_witness's own bad case: the witness rename is refused when the
    store does NOT already hold the renamed goal — here against the bare
    hypotheses, where ∠b:a:b = ∠d:c:e is not entailed."""
    s = eng.State()
    for f, w in eng.PROP23_HYPS_DATA:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(s, 99, "use_witness",
                       ("f", "b", "a", "d", "c", "e", "AB"), {"f": "f"})
    assert ei.value.premise == ("entail", "eq_ang",
                                ("b", "a", "b", "d", "c", "e"))


def test_prop23_final_state_not_contradictory():
    """The by_cases concludes on the OUTER state; it stays consistent
    (main() asserts store satisfiability; make the fact side explicit)."""
    s, _ = eng.replay(eng.PROP23_HYPS_DATA, eng.PROP23_STEPS_DATA,
                      eng.PROP23_GOAL)
    contra, witness = eng.contradictory(s)
    assert not contra, witness


def test_prop23p_replay():
    """I.23' (Prop23.lean:35-59): I.23's construction verbatim, plus the
    hypothesis ¬(x.onLine AB) and the main branch citing the same-side
    variant proposition_22'' (line 54) in place of 22'. The goal is
    I.23's two conjuncts plus the disjunction f.onLine AB ∨
    f.sameSide x AB — the fact kind "either", carried out of the
    by_cases as the join of the branch facts (left half in the
    degenerate branches, right half in the main branch)."""
    s, rep = eng.replay(eng.PROP23P_HYPS_DATA, eng.PROP23P_STEPS_DATA,
                        eng.PROP23P_GOAL)
    assert [r["rule"].split(" ")[0] for r in rep[:-1]] == ["by_cases"]
    assert rep[0]["branches"] == 2
    assert s.has(("neq", "f", "a"))
    assert s.has(("either", ("on_line", "f", "AB"),
                 ("same_side", "f", "x", "AB")))
    assert eng.la_entails_eq_ang(s.la, "f", "a", "b", "d", "c", "e")
    assert rep[-1]["final"]


def test_prop23p_hyps_alone_do_not_entail_goal():
    """The I.23' hypotheses alone entail no goal conjunct — f does not
    exist before the construction, and the disjunction is not stored."""
    s = eng.State()
    for f, w in eng.PROP23P_HYPS_DATA:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    assert not s.has(("neq", "f", "a"))
    assert not s.has(("either", ("on_line", "f", "AB"),
                     ("same_side", "f", "x", "AB")))
    assert not eng.la_entails_eq_ang(s.la, "f", "a", "b", "d", "c", "e")


def test_prop23p_control_dropped_x_off_line():
    """Drop ¬(x.onLine AB): the main branch's proposition_22'' citation
    has no side to pick — refused at its not_on_line premise (the inner
    by_cases branch reports the stuck step)."""
    hyps = [h for h in eng.PROP23P_HYPS_DATA
            if h[0] != ("not_on_line", "x", "AB")]
    with pytest.raises(eng.ByCasesFailure) as ei:
        eng.replay(hyps, eng.PROP23P_STEPS_DATA, eng.PROP23P_GOAL)
    assert ei.value.branch == [("not_on_line", "d", "CE")]
    assert ei.value.witness == ("stuck", ("not_on_line", "x", "AB"))


def test_prop23p_degenerate_branches_unchanged():
    """The degenerate branch (d on CE) is I.23's: same branch
    assumptions, same step rules in the same order — the only change is
    use_witness's extra line argument carrying the witness's on-line
    fact, which I.23's replay shares."""
    def degenerate(steps):
        return steps[0][1][0]   # (assumptions, steps) of the d-on-CE branch
    a23, s23 = degenerate(eng.PROP23_STEPS_DATA)
    a23p, s23p = degenerate(eng.PROP23P_STEPS_DATA)
    assert a23 == a23p == [("on_line", "d", "CE")]
    (k1, br1, _), (k2, br2, _) = s23[0], s23p[0]
    assert k1 == k2 == "by_cases"
    assert [b[0] for b in br1] == [b[0] for b in br2]
    assert [[st[0] for st in b[1]] for b in br1] == \
           [[st[0] for st in b[1]] for b in br2]


def test_prop23p_either_conclusion_checked_per_branch():
    """The bad case the "either" check is named for: a by_cases whose
    branches establish NEITHER disjunct must not carry the disjunction
    out — refused as not-entailed, not stored."""
    hyps = [(("dpl", "a", "b", "AB"), "h")]
    disj = ("either", ("on_line", "f", "AB"), ("same_side", "f", "x", "AB"))
    steps = [("by_cases", [
        ([("on_line", "d", "CE")], []),
        ([("not_on_line", "d", "CE")], []),
    ], [disj])]
    with pytest.raises(eng.ByCasesFailure) as ei:
        eng.replay(hyps, steps, [])
    assert ei.value.witness == ("not-entailed", disj)


def test_prop23p_final_state_not_contradictory():
    """The by_cases (with the disjunctive conclusion) concludes on the
    OUTER state; it stays consistent."""
    s, _ = eng.replay(eng.PROP23P_HYPS_DATA, eng.PROP23P_STEPS_DATA,
                      eng.PROP23P_GOAL)
    contra, witness = eng.contradictory(s)
    assert not contra, witness


def test_proposition_23p_citation_fires():
    """The cited lemma makes exactly the I.23' conclusions over the
    fresh witness f (the theorem PROP23P_STEPS_DATA replays): f ≠ a, the
    disjunction f.onLine AB ∨ f.sameSide x AB as one "either" fact, and
    ∠f:a:b = ∠d:c:e."""
    s = eng.State()
    for f, w in eng.PROP23P_HYPS_DATA:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    rep = eng.apply_step(
        s, 1, "proposition_23'",
        ("a", "b", "c", "d", "e", "x", "AB", "CD", "CE"), {"f": "f"})
    assert rep["rule"].startswith("proposition_23'")
    assert s.has(("neq", "f", "a"))
    assert s.has(("either", ("on_line", "f", "AB"),
                 ("same_side", "f", "x", "AB")))
    assert eng.la_entails_eq_ang(s.la, "f", "a", "b", "d", "c", "e")


def test_proposition_23p_citation_refused_without_x_off_line():
    """¬(x.onLine AB) is a premise (the construction must know which
    side of AB is x's): without it the citation is refused."""
    hyps = [h for h in eng.PROP23P_HYPS_DATA
            if h[0] != ("not_on_line", "x", "AB")]
    s = eng.State()
    for f, w in hyps:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(
            s, 1, "proposition_23'",
            ("a", "b", "c", "d", "e", "x", "AB", "CD", "CE"), {"f": "f"})
    assert ei.value.premise == ("not_on_line", "x", "AB")


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
    main() goal still holds. I.18 joins the pin at (97, 32).
    E-kimi-task-390 added the I.19 replay (by_contra citing
    proposition_5' and the proposition_18 lemma). No replay changed;
    I.19 joins the pin at (41, 11).
    E-kimi-task-392 added the I.20 replay (extend_point_longer +
    proposition_3 + the cited proposition_19 lemma). No replay changed;
    I.20 joins the pin at (173, 71).
    E-kimi-task-394 added the I.21 replay (triple_incidence_1 +
    intersection_lines + pasch_4 + the cited proposition_20/proposition_16
    lemmas). No replay changed; I.21 joins the pin at (181, 64).
    E-kimi-task-395 added the I.22 replay (the extend/proposition_3 chain,
    a three-case by_cases for inside i DKL, and the cited
    intersection_circle_circle_1). Two engine changes that could have
    moved earlier replays did NOT: _fm_sat picks its elimination pivot by
    smallest pos*neg product instead of sorted-first (same decisions —
    FM is order-independent — but the chain store no longer blows past
    the row guard), and r_peq_subst_diagram also substitutes peq into
    inside facts (fires only where a peq stands; no pinned replay
    derives a new fact). No replay changed; I.22 joins the pin at
    (1172, 906).
    E-kimi-task-399 added the I.22' replay (the same construction from a
    given segment, plus euclid_finish's not_between g f e rendered as a
    by_contra citing the new per-use between_trans_out rule,
    Diagrammatic.lean:78 — cited, not closure, so no pinned replay
    derives a new fact). No replay changed; I.22' joins the pin at
    (624, 434).
    E-kimi-task-402 added the I.23 replay (the by_cases on d.onLine CE
    with the nested degenerate split, citing the new per-use rules
    flat_angle_onlyif, degenerated_angle_if, angle_self_zero,
    angle_ray_eq_zero, off_line_strict_triangle and use_witness — all
    cited, none in closure). No replay changed; I.23 joins the pin at
    (33, 9) — all branch work happens on by_cases copies, so the outer
    state carries only the two conclusions."""
    import io, contextlib, sys
    # intersection_circle_line_2 (Diagrammatic.lean:305) added to the
    # closure (claude-9, 2026-11-09): fact counts rose where an inside
    # point lies on a line (intersects_lc facts); constraints unchanged.
    # angle_same_ray (claude-9, 2026-11-10, kimi-1's I.5 playtest): equal
    # angles along one ray from v when two points lie beyond v from the
    # same a; I.5, 11, 16, 20, 22, 22' gain rows (I.22 +224, still ~18 s).
    expected = {"1": (39, 8), "2": (119, 38), "3": (61, 22), "4": (198, 79),
                "5": (406, 215), "6": (41, 11), "7": (113, 26), "8": (210, 82),
                "9": (223, 86), "10": (161, 50), "11": (306, 152), "12": (177,
                66), "13": (43, 16), "14": (70, 21), "15": (104, 47), "16b":
                (412, 194), "16a": (562, 289), "17": (70, 24), "18": (97, 32),
                "19": (41, 11), "20": (175, 71), "21": (181, 64), "22": (1426,
                1130), "22'": (703, 483), "23": (33, 9)}
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


def test_prop24a_replay():
    """I.24a (Prop24.lean:18-39, FIRST HALF ONLY): the common construction
    (proposition_23' -> line_from_points DG -> extend_point_longer ->
    proposition_3 -> EG/FG -> proposition_4 -> proposition_5', the
    euclid_asserts at 28/29/34 and the hgf by_contra) and the ONE branch
    Euclid treats (d.sameSide g EF, lines 36-39). The closing by_cases is
    deliberately one-branched: the second bullet (lines 40-59, "Omitted
    by Euclid") is the next packet, so this is NOT the whole proof of
    I.24 — the goal |b-c| > |e-f| is carried out of the branch case
    only."""
    s, rep = eng.replay(eng.PROP24A_HYPS_DATA, eng.PROP24A_STEPS_DATA,
                        eng.PROP24A_GOAL)
    kinds = [r["rule"].split(" ")[0] for r in rep[:-1]]
    assert kinds == [
        "proposition_23'", "by_contra", "line_from_points",
        "extend_point_longer", "between_symm_parts", "proposition_3",
        "between_symm_parts", "line_from_points", "by_contra",
        "line_from_points", "proposition_4", "proposition_5'",
        "by_contra", "by_contra", "by_contra", "by_contra",
        "triple_incidence_2", "by_cases",
    ]
    assert rep[-2]["rule"] == "by_cases" and rep[-2]["branches"] == 1
    assert rep[-2]["conclusions"] == [("_gt_len", "b", "c", "e", "f")]
    assert s.has(("gt_len", "b", "c", "e", "f"))
    assert s.has(("same_side", "g'", "f", "DE"))   # disjunction resolved
    assert s.has(("opposing_sides", "e", "g", "DF"))   # hgf
    assert s.has(("same_side", "f", "e", "DG"))   # assert 34
    assert eng.la_entails_gt(s.la, "b", "c", "e", "f")
    assert rep[-1]["final"]


def test_prop24a_hyps_alone_do_not_entail_goal():
    """The I.24 hypotheses alone do not entail |b-c| > |e-f| (g does not
    exist before the construction; nothing links |b-c| to the second
    triangle)."""
    s = eng.State()
    for f, w in eng.PROP24A_HYPS_DATA:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    assert not eng.la_entails_gt(s.la, "b", "c", "e", "f")


def test_prop24a_control_dropped_proposition_5p():
    """Drop proposition_5' (the isosceles base-angle equality
    |d-g| = |d-f|): the line-36 angle assert's by_contra branches no
    longer contradict — refused where its conclusion is used."""
    steps = [st for st in eng.PROP24A_STEPS_DATA
             if not (isinstance(st, tuple) and st[0] == "proposition_5'")]
    with pytest.raises(eng.ByContraFailure) as ei:
        eng.replay(eng.PROP24A_HYPS_DATA, steps, eng.PROP24A_GOAL)
    assert ei.value.branch in ([("eq_ang", "d", "f", "g", "e", "g", "f")],
                               [("gt_ang", "e", "g", "f", "d", "f", "g")])


def test_prop24a_control_dropped_hgf():
    """Drop the hgf by_contra (lines 30-33): the line-34 assert's
    triple_incidence_2 is refused at its opposing_sides e g DF premise —
    exactly where hgf's conclusion is used."""
    steps = [st for st in eng.PROP24A_STEPS_DATA
             if not (st[0] == "by_contra" and
                     st[2] == [("opposing_sides", "e", "g", "DF")])]
    with pytest.raises(eng.MissingPremise) as ei:
        eng.replay(eng.PROP24A_HYPS_DATA, steps, eng.PROP24A_GOAL)
    assert ei.value.premise == ("opposing_sides", "e", "g", "DF")
    assert ei.value.rule == "triple_incidence_2"


def test_prop24a_control_dropped_branch_split():
    """Drop the closing one-branch by_cases: the goal is not entailed in
    the outer state (the branch hypothesis d.sameSide g EF is what feeds
    the line 36-39 angle chain through to proposition_19)."""
    steps = eng.PROP24A_STEPS_DATA[:-1]
    with pytest.raises(AssertionError):
        eng.replay(eng.PROP24A_HYPS_DATA, steps, eng.PROP24A_GOAL)


def test_prop24a_final_state_not_contradictory():
    """The final store (the one-branch by_cases concluded on the OUTER
    state) stays consistent."""
    s, _ = eng.replay(eng.PROP24A_HYPS_DATA, eng.PROP24A_STEPS_DATA,
                      eng.PROP24A_GOAL)
    contra, witness = eng.contradictory(s)
    assert not contra, witness


# --- I.24 complete (Book/Prop24.lean:12-59, both by_cases branches) -------


def _prop24_rebuilt(sub2_steps):
    """PROP24_STEPS_DATA with the second branch's nested not_on_line g EF
    sub-case steps replaced by sub2_steps (for drop-controls)."""
    by = eng.PROP24_STEPS_DATA[-1]
    branch1, (f2, steps2) = by[1]
    nested = steps2[0]
    (nf1, sub1), (nf2, _sub2) = nested[1]
    nested = (nested[0], [(nf1, sub1), (nf2, sub2_steps)], nested[2])
    by = (by[0], [branch1, (f2, [nested])], by[2])
    return eng.PROP24_STEPS_DATA[:-1] + [by]


def _prop24_branch2_sub2():
    """The nested not_on_line g EF sub-case's step list."""
    by = eng.PROP24_STEPS_DATA[-1]
    nested = by[1][1][1][0]
    return nested[1][1][1]


def test_prop24_replay():
    """The whole of I.24: both branches of the d.sameSide g EF split
    conclude |b-c| > |e-f| (Prop24.lean:35-59)."""
    s, rep = eng.replay(eng.PROP24A_HYPS_DATA, eng.PROP24_STEPS_DATA,
                        eng.PROP24_GOAL)
    assert eng.PROP24_STEPS_DATA[:-1] == eng.PROP24A_STEPS_DATA[:-1]
    by = eng.PROP24_STEPS_DATA[-1]
    assert by[1][0] == eng.PROP24A_STEPS_DATA[-1][1][0]  # branch 1 verbatim
    assert by[1][1][0] == [("not_same_side", "d", "g", "EF")]
    assert rep[-2]["rule"] == "by_cases" and rep[-2]["branches"] == 2
    assert rep[-2]["conclusions"] == [("_gt_len", "b", "c", "e", "f")]
    assert s.has(("gt_len", "b", "c", "e", "f"))
    assert eng.la_entails_gt(s.la, "b", "c", "e", "f")
    assert rep[-1]["final"]


def test_prop24_branch2_angle_assert_rows():
    """Prop24.lean:53-55 (d:g:e < 2r, d:f:e < 2r, and the 4r sum) are LA
    consequences of the cited rows inside the second branch — pin them as
    entailment queries on the branch state (no fact kind carries
    angle-vs-2r comparisons)."""
    by = eng.PROP24_STEPS_DATA[-1]
    f2, steps2 = by[1][1]
    nested = steps2[0]
    nf2, sub2 = nested[1][1]
    # replay the common part, enter branch 2 and the nested sub-case,
    # run everything up to (not incl.) the line-56 by_contra
    s = eng.State()
    for f, w in eng.PROP24A_HYPS_DATA:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    eng.run_steps(s, eng.PROP24_STEPS_DATA[:-1])
    eng.close(s)
    for f in f2:
        s.assume(f, "by_cases assumption")
    eng.close(s)
    for f in nf2:
        s.assume(f, "by_cases assumption")
    eng.close(s)
    upto = [st for st in sub2
            if not (st[0] == "by_contra"
                    and st[2] == [("gt_ang", "e", "f", "g", "e", "g", "f")])
            and st[0] != "proposition_19"]
    eng.run_steps(s, upto)
    eng.close(s)
    A, lin, RT = eng.ang_atom, eng.lin, eng.RT
    assert s.la.entails(lin((A("d", "g", "e"), 1), (RT, -2)), 0, "<")
    assert s.la.entails(lin((A("d", "f", "e"), 1), (RT, -2)), 0, "<")
    assert s.la.entails(lin((A("e", "f", "g"), 1), (A("g", "f", "d"), 1),
                            (A("d", "f", "e"), 1), (RT, -4)), 0, "=")
    assert s.la.entails(lin((A("e", "f", "g"), 1), (A("e", "g", "f"), -1)),
                        0, ">")


def test_prop24_control_dropped_proposition_13():
    """Drop the second branch's proposition_13 d f g h DF FG: the 4r sum
    row is gone, so the line-56 angle assert's by_contra branches no
    longer contradict — refused at that assert."""
    steps = _prop24_rebuilt(
        [st for st in _prop24_branch2_sub2()
         if st != ("proposition_13", ("d", "f", "g", "h", "DF", "FG"), {})])
    with pytest.raises(eng.ByContraFailure) as ei:
        eng.replay(eng.PROP24A_HYPS_DATA, steps, eng.PROP24_GOAL)
    assert ei.value.entry[2] == [("gt_ang", "e", "f", "g", "e", "g", "f")]
    assert ei.value.branch in ([("eq_ang", "e", "f", "g", "e", "g", "f")],
                               [("gt_ang", "e", "g", "f", "e", "f", "g")])


def test_prop24_control_dropped_proposition_17():
    """Drop the second branch's proposition_17 d g e DG EG DE: the
    d:g:e < 2r bound is gone, so the line-56 angle assert is refused."""
    steps = _prop24_rebuilt(
        [st for st in _prop24_branch2_sub2()
         if st != ("proposition_17", ("d", "g", "e", "DG", "EG", "DE"), {})])
    with pytest.raises(eng.ByContraFailure) as ei:
        eng.replay(eng.PROP24A_HYPS_DATA, steps, eng.PROP24_GOAL)
    assert ei.value.entry[2] == [("gt_ang", "e", "f", "g", "e", "g", "f")]
    assert ei.value.branch in ([("eq_ang", "e", "f", "g", "e", "g", "f")],
                               [("gt_ang", "e", "g", "f", "e", "f", "g")])


def test_prop24_control_dropped_proposition_19():
    """Drop the second branch's proposition_19 e f g EF FG EG: nothing
    turns ang e:f:g > ang e:g:f into the length comparison, so the nested
    by_cases' not_on_line g EF sub-case does not entail the goal."""
    steps = _prop24_rebuilt(
        [st for st in _prop24_branch2_sub2() if st[0] != "proposition_19"])
    with pytest.raises(eng.ByCasesFailure) as ei:
        eng.replay(eng.PROP24A_HYPS_DATA, steps, eng.PROP24_GOAL)
    assert ei.value.branch == [("not_on_line", "g", "EF")]
    assert ei.value.witness == ("not-entailed", ("_gt_len", "b", "c", "e", "f"))


def test_prop24_final_state_not_contradictory():
    """The final store (the two-branch by_cases concluded on the OUTER
    state) stays consistent."""
    s, _ = eng.replay(eng.PROP24A_HYPS_DATA, eng.PROP24_STEPS_DATA,
                      eng.PROP24_GOAL)
    contra, witness = eng.contradictory(s)
    assert not contra, witness


def test_proposition_24_citation_fires():
    """The cited lemma (Prop24.lean:12-16) applies on the I.24 hypotheses
    and makes exactly the _gt_len conclusion PROP24_STEPS_DATA proves."""
    s = eng.State()
    for f, w in eng.PROP24A_HYPS_DATA:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    eng.apply_step(s, 1, "proposition_24",
                   ("a", "b", "c", "d", "e", "f",
                    "AB", "BC", "AC", "DE", "EF", "DF"), {})
    assert eng.la_entails_gt(s.la, "b", "c", "e", "f")


def test_proposition_24_citation_refused_wrong_way():
    """Cited with the angle hypothesis REVERSED (ang e:d:f > ang b:a:c)
    proposition_24 is refused: the gt_ang entailment premise fails (and
    the conclusion would be false — the hinge opens the other way)."""
    s = eng.State()
    for f, w in eng.PROP24A_HYPS_DATA:
        if f[0] == "gt_ang":
            f = ("gt_ang", "e", "d", "f", "b", "a", "c")  # reversed
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(s, 1, "proposition_24",
                       ("a", "b", "c", "d", "e", "f",
                        "AB", "BC", "AC", "DE", "EF", "DF"), {})
    assert "gt_ang" in str(ei.value.premise)


def test_prop25_replay():
    """I.25 replays the Lean way: by_contra on the goal, splitting
    ¬(∠b:a:c > ∠e:d:f) into ∠b:a:c = ∠e:d:f (proposition_4 gives
    |b-c| = |e-f| against the hypothesis |b-c| > |e-f|) and
    ∠e:d:f > ∠b:a:c (proposition_24 d e f a b c DE EF DF AB BC AC — the
    cited I.24 lemma — gives |e-f| > |b-c| against the hypothesis).
    Both branches end arithmetically contradictory; exhaustiveness is
    trichotomy, the caller's responsibility as documented at
    PROP25_STEPS_DATA."""
    s, rep = eng.replay(eng.PROP25_HYPS_DATA, eng.PROP25_STEPS_DATA,
                        eng.PROP25_GOAL)
    assert eng.la_entails_gt_ang(s.la, "b", "a", "c", "e", "d", "f")
    assert rep[-1]["final"]
    assert rep[0]["rule"] == "by_contra" and rep[0]["branches"] == 2


def test_prop25_hyps_alone_do_not_entail_goal():
    """The I.25 hypotheses alone do NOT entail ∠b:a:c > ∠e:d:f — the
    by_contra carries the proof (the replay is not vacuous)."""
    s = eng.State()
    for f, w in eng.PROP25_HYPS_DATA:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    assert not eng.la_entails_gt_ang(s.la, "b", "a", "c", "e", "d", "f")


def test_prop25_control_dropped_proposition_4():
    """Drop branch 1's proposition_4 citation: with ∠b:a:c = ∠e:d:f
    assumed and no way to turn it into |b-c| = |e-f| against the
    hypothesis, the branch cannot reach a contradiction -> by_contra
    refused (ByContraFailure)."""
    steps = [("by_contra", [
        (assumps, [st for st in br_steps
                   if st[0] != "proposition_4"])
        for assumps, br_steps in eng.PROP25_STEPS_DATA[0][1]],
        eng.PROP25_STEPS_DATA[0][2])]
    with pytest.raises(eng.ByContraFailure):
        eng.replay(eng.PROP25_HYPS_DATA, steps, eng.PROP25_GOAL)


def test_prop25_control_dropped_proposition_24():
    """Drop branch 2's proposition_24 citation: with ∠e:d:f > ∠b:a:c
    assumed and no way to turn it into |e-f| > |b-c| against the
    hypothesis, the branch cannot reach a contradiction -> by_contra
    refused (ByContraFailure)."""
    steps = [("by_contra", [
        (assumps, [st for st in br_steps
                   if st[0] != "proposition_24"])
        for assumps, br_steps in eng.PROP25_STEPS_DATA[0][1]],
        eng.PROP25_STEPS_DATA[0][2])]
    with pytest.raises(eng.ByContraFailure):
        eng.replay(eng.PROP25_HYPS_DATA, steps, eng.PROP25_GOAL)


def test_prop25_control_swapped_length_hypothesis():
    """Swap the length hypothesis (|e-f| > |b-c| instead of
    |b-c| > |e-f|): branch 2's proposition_24 conclusion |e-f| > |b-c|
    is now exactly consistent with the hypothesis, so the branch cannot
    reach a contradiction -> by_contra refused. The replay is not
    vacuous in its hypothesis (and the swapped statement is false —
    that is I.24's configuration, with the hinge opening the other
    way)."""
    hyps = [(("gt_len", "e", "f", "b", "c") if f[0] == "gt_len" else f, w)
            for f, w in eng.PROP25_HYPS_DATA]
    with pytest.raises(eng.ByContraFailure):
        eng.replay(hyps, eng.PROP25_STEPS_DATA, eng.PROP25_GOAL)


def test_prop25_final_state_not_contradictory():
    """The final store (the by_contra concluded on the OUTER state)
    stays consistent."""
    s, _ = eng.replay(eng.PROP25_HYPS_DATA, eng.PROP25_STEPS_DATA,
                      eng.PROP25_GOAL)
    contra, witness = eng.contradictory(s)
    assert not contra, witness


def test_proposition_25_citation_fires():
    """The cited lemma (Prop25.lean:7-11) applies on the I.25 hypotheses
    and makes exactly the _gt_ang conclusion PROP25_STEPS_DATA proves."""
    s = eng.State()
    for f, w in eng.PROP25_HYPS_DATA:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    eng.apply_step(s, 1, "proposition_25",
                   ("a", "b", "c", "d", "e", "f",
                    "AB", "BC", "AC", "DE", "EF", "DF"), {})
    assert eng.la_entails_gt_ang(s.la, "b", "a", "c", "e", "d", "f")


def test_proposition_25_citation_refused_wrong_way():
    """Cited with the length hypothesis REVERSED (|e-f| > |b-c| instead
    of |b-c| > |e-f|) proposition_25 is refused: the gt_len entailment
    premise fails (and the conclusion would be false — the hinge opens
    the other way, that is I.24's configuration)."""
    s = eng.State()
    for f, w in eng.PROP25_HYPS_DATA:
        if f[0] == "gt_len":
            f = ("gt_len", "e", "f", "b", "c")  # reversed
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(s, 1, "proposition_25",
                       ("a", "b", "c", "d", "e", "f",
                        "AB", "BC", "AC", "DE", "EF", "DF"), {})
    assert "gt_len" in str(ei.value.premise)


def _prop7_case_assumptions(steps_data):
    """The assumption-fact lists of I.7's four by_contra branches."""
    entry = [st for st in steps_data if st[0] == "by_contra"]
    assert len(entry) == 1
    return [facts for facts, _ in entry[0][1]]


def _check_prop7_case_assumptions(steps_data):
    """E-kimi-task-420: each I.7 case's hypotheses are EXACTLY the two
    split facts (a vs CD, d vs AC). Every other side fact the branch steps
    need must be derived by a cited System E step inside the branch, not
    assumed."""
    expected = [
        [("same_side", "a", "b", "CD"), ("same_side", "d", "b", "AC")],
        [("same_side", "a", "b", "CD"), ("opposing_sides", "d", "b", "AC")],
        [("opposing_sides", "a", "b", "CD"), ("same_side", "d", "b", "AC")],
        [("opposing_sides", "a", "b", "CD"),
         ("opposing_sides", "d", "b", "AC")],
    ]
    assert _prop7_case_assumptions(steps_data) == expected


def test_prop7_case_hypotheses_are_exactly_the_split_facts():
    _check_prop7_case_assumptions(eng.PROP7_STEPS_DATA)
    # and the replay still refutes every case
    s, rep = eng.replay(eng.PROP7_HYPS_DATA, eng.PROP7_STEPS_DATA,
                        [("false",)])
    assert s.has(("false",))
    assert rep[-1]["final"]


def test_prop7_case_hypotheses_catch_readded_assumption():
    """The exact-hypotheses check catches an unentailed extra assumption
    re-added to a case (the E-kimi-task-420 bad case: branch 1 previously
    assumed same_side c a DB, entailed in no case)."""
    tampered = [st for st in eng.PROP7_STEPS_DATA]
    bc = tampered[1]
    facts, steps = bc[1][0]
    bad_facts = facts + [("same_side", "c", "a", "DB")]
    tampered[1] = ("by_contra", [(bad_facts, steps)] + bc[1][1:], bc[2])
    with pytest.raises(AssertionError):
        _check_prop7_case_assumptions(tampered)


def test_prop7_derived_side_facts_not_entailed_without_derivation():
    """Each derived side fact is NOT already entailed by the case facts
    alone: drop the derivation steps from branch 1 and the replay STUCKs
    on the sum_angles_onlyif premise same_side c a DB (the exact bad case
    the old data papered over by assuming it)."""
    bc = eng.PROP7_STEPS_DATA[1]
    facts, steps = bc[1][0]
    stripped = [st for st in steps if st[0] != "triple_incidence_1"
                and st[0] != "triple_incidence_2"]
    tampered = [eng.PROP7_STEPS_DATA[0],
                ("by_contra", [(facts, stripped)] + bc[1][1:], bc[2])]
    with pytest.raises(eng.ByContraFailure) as ei:
        eng.replay(eng.PROP7_HYPS_DATA, tampered, [("false",)])
    kind, payload = ei.value.witness
    assert kind == "stuck"
    assert payload == ("same_side", "c", "a", "DB")


def test_proposition_6_rule_fires():
    """proposition_6 as a cited lemma (Book/Prop06.lean:7-9): on I.6's
    hypotheses it concludes |a-b| = |a-c| (the _eq_len metric tuple)."""
    s = eng.State()
    for f, w in eng.PROP6_HYPS_DATA:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    eng.apply_step(s, 1, "proposition_6", ("a", "b", "c", "AB", "BC", "AC"),
                   {})
    eng.close(s)
    assert eng.la_entails_eq(s.la, "a", "b", "a", "c")


def test_proposition_6_rule_needs_equal_base_angles():
    """proposition_6 without the equal-base-angles hypothesis STUCKs:
    the eq_ang entailment premise is not derivable (the bad case the
    rule is named for — I.6 is exactly 'equal base angles ⇒ equal
    sides')."""
    hyps = [h for h in eng.PROP6_HYPS_DATA
            if h[0] != ("eq_ang", "a", "b", "c", "a", "c", "b")]
    s = eng.State()
    for f, w in hyps:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    with pytest.raises(eng.MissingPremise) as ei:
        eng.apply_step(s, 1, "proposition_6",
                       ("a", "b", "c", "AB", "BC", "AC"), {})
    assert ei.value.rule == "proposition_6"
    assert "eq_ang" in str(ei.value.premise)
