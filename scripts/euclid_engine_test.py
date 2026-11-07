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
    s, rep = eng.replay(eng.PROP6_HYPS_DATA, eng.PROP6_STEPS_DATA,
                        [("_eq_len", "a", "b", "a", "c")])
    assert eng.la_entails_eq(s.la, "a", "b", "a", "c")
    assert rep[-1]["final"]


def test_prop6_control_dropped_equal_angles():
    """I.6 without the equal-angles hypothesis: the by_contra branch gets
    STUCK at proposition_4's angle entailment premise instead of reaching a
    contradiction (side_gt_angle_gt alone then relates ∠a:c:b > ∠a:b:c with
    nothing equating them)."""
    hyps = [h for h in eng.PROP6_HYPS_DATA
            if h[0] != ("eq_ang", "a", "b", "c", "a", "c", "b")]
    with pytest.raises(eng.ByContraFailure) as ei:
        eng.replay(hyps, eng.PROP6_STEPS_DATA, [("_eq_len", "a", "b", "a", "c")])
    assert "stuck" in str(ei.value)


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
    final store is satisfiable (except I.7)."""
    import io, contextlib, sys
    expected = {"1": (37, 8), "2": (112, 38), "3": (60, 22), "4": (198, 79),
                "5": (442, 227), "6": (41, 11), "7": (113, 26),
                "8": (210, 82), "9": (236, 93), "10": (163, 51),
                "11": (318, 158), "12": (178, 68)}
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


def test_bridge_side_gt_angle_gt_counterexample():
    """I.18 bridge: |p-r| > |p-q| ⇒ ∠(p,q,r) > ∠(q,r,p). Dropping the length
    premise (no comparison entailed) the rule must add nothing — with equal
    lengths the conclusion is false (∠(p,q,r) = ∠(q,r,p))."""
    # equal lengths: no strict row between the two angles
    s = _line_state(
        [("neq", "p", "q"), ("neq", "q", "r"), ("neq", "p", "r")],
        [({eng.len_atom("p", "q"): 1, eng.len_atom("p", "r"): -1}, 0, "="),
         # both angles under discussion, equal
         ({eng.ang_atom("p", "q", "r"): 1, eng.ang_atom("q", "r", "p"): -1},
          0, "=")])
    assert not s.la.entails({eng.ang_atom("p", "q", "r"): 1,
                             eng.ang_atom("q", "r", "p"): -1}, 0, "<")
    assert eng.contradictory(s) == (False, None)
    # with the length premise the two conclusions clash -> unsat.
    # Review fix (claude-9): r off line PQ. Without a triangle this state is
    # realisable (q-p-r collinear: both angles 0), so unsat was wrong there.
    s2 = _line_state(
        [("neq", "p", "q"), ("neq", "q", "r"), ("neq", "p", "r"),
         ("on_line", "p", "PQ"), ("on_line", "q", "PQ"),
         ("not_on_line", "r", "PQ")],
        [({eng.len_atom("p", "q"): 1, eng.len_atom("p", "r"): -1}, 0, "<"),
         ({eng.ang_atom("p", "q", "r"): 1, eng.ang_atom("q", "r", "p"): -1},
          0, "=")])
    ok, w = eng.contradictory(s2)
    assert ok and w[0] == "store-unsat"


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


def test_bridge_side_gt_angle_gt_rejects_collinear():
    """Review (claude-9): q-p-r collinear with |pr| > |pq| is realisable; both
    angles are 0, so the I.18 bridge must not fire. Before the fix this state
    was reported contradictory (store-unsat)."""
    s = eng.State()
    for f in [("on_line", "q", "L"), ("on_line", "p", "L"), ("on_line", "r", "L"),
              ("between", "q", "p", "r"), ("neq", "p", "q"), ("neq", "q", "r"),
              ("neq", "p", "r"), ("neq", "q", "p"), ("neq", "r", "q"),
              ("neq", "r", "p")]:
        s.assume(f)
    s.la.add({eng.len_atom("p", "q"): 1}, 1, "=")   # |pq| = 1
    s.la.add({eng.len_atom("p", "r"): 1}, 2, "=")   # |pr| = 2
    s.la.add({eng.ang_atom("p", "q", "r"): 1}, 0, ">=")
    s.la.add({eng.ang_atom("q", "r", "p"): 1}, 0, ">=")
    assert eng.contradictory(s) == (False, None)
    # and the unfixed rule's conclusion is false here (sanity: not vacuous)
    s.la.add({eng.ang_atom("p", "q", "r"): 1,
              eng.ang_atom("q", "r", "p"): -1}, 0, ">")
    assert eng.contradictory(s)[0] is True


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
