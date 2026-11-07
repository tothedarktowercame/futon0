"""Tests for scripts/euclid_trace.py (E-euclid-tutor requisition).

(i)   every euclid_apply step in the I.1-I.3 traces has a Lean file:line,
      and that line in Book/PropNN.lean really contains the rule name;
(ii)  the numeric placement satisfies the trace's metric facts to 1e-9;
(iii) bad case: a placement where the WRONG intersection point is chosen
      must fail the metric/between check — constructed and watched to fail.
"""

import json
import math
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import euclid_trace as tr

TRACES = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
                      "holes", "excursions", "E-euclid-tutor-data",
                      "viewer", "traces")
LEAN_DIR = tr.LEAN_DIR


def load(prop):
    with open(os.path.join(TRACES, "prop%02d.json" % prop)) as fh:
        return json.load(fh)


TR1, TR2, TR3 = load(1), load(2), load(3)
TR4, TR5 = load(4), load(5)
ALL = (TR1, TR2, TR3, TR4, TR5)


# -- (i) Lean alignment -----------------------------------------------------

def test_every_step_has_lean_line():
    for t in ALL:
        for s in t["steps"]:
            assert s["lean"]["file"].startswith("Book/Prop")
            assert isinstance(s["lean"]["line"], int)
            assert 1 <= s["lean"]["line"] <= len(t["lean_text"])


def test_lean_line_really_contains_the_rule():
    """euclid_apply lines must name the rule; assert_point_eq_* steps match
    Prop04.lean's euclid_assert lines instead (point equality / the apex
    angle), which is checked by their asserted content."""
    for t in ALL:
        path = os.path.join(LEAN_DIR, t["lean_file"])
        with open(path) as fh:
            lines = fh.read().splitlines()
        for s in t["steps"]:
            line = lines[s["lean"]["line"] - 1]
            if s["rule"] == "assert_point_eq_on_ray":
                assert "euclid_assert" in line and "=" in line, (s, line)
                assert "∠" not in line, (s, line)
            elif s["rule"] == "assert_point_eq_angle":
                assert "euclid_assert" in line and "∠" in line, (s, line)
            else:
                assert "euclid_apply" in line, (s, line)
                assert s["rule"] in line, (s, line)


def test_lean_lines_are_distinct_and_in_order():
    for t in ALL:
        lines = [s["lean"]["line"] for s in t["steps"]]
        assert lines == sorted(lines)
        assert len(set(lines)) == len(lines)


def test_traces_on_disk_match_rebuild():
    for prop, t in ((1, TR1), (2, TR2), (3, TR3), (4, TR4), (5, TR5)):
        rebuilt = tr.build_trace(prop)
        assert rebuilt["placement"] == t["placement"]
        assert [s["rule"] for s in rebuilt["steps"]] == \
            [s["rule"] for s in t["steps"]]


def test_prop2_a_is_clear_of_the_bc_circle():
    # review, claude-9: keep a away from the circle of radius |bc| around
    # itself so the diagram does not look degenerate
    assert abs(dist(TR2, "b", "a") - dist(TR2, "b", "c")) > 0.2
    assert dist(TR2, "b", "c") < 1.5 * dist(TR2, "b", "a")


# -- (ii) metric facts of the placement --------------------------------------

def dist(t, p, q):
    a, b = t["placement"][p], t["placement"][q]
    return math.hypot(a[0] - b[0], a[1] - b[1])


def test_prop1_equilateral_to_1e_9():
    ab, ca, cb = (dist(TR1, *pq) for pq in (("a", "b"), ("c", "a"),
                                            ("c", "b")))
    assert abs(ca - ab) < 1e-9
    assert abs(cb - ab) < 1e-9


def test_prop2_transferred_length_to_1e_9():
    assert abs(dist(TR2, "a", "l") - dist(TR2, "b", "c")) < 1e-9


def test_prop3_cut_off_to_1e_9():
    assert abs(dist(TR3, "a", "e") - dist(TR3, "c0", "c1")) < 1e-9
    # between a e b, collinear to 1e-9
    a, e, b = (TR3["placement"][k] for k in ("a", "e", "b"))
    ae = math.hypot(e[0] - a[0], e[1] - a[1])
    eb = math.hypot(b[0] - e[0], b[1] - e[1])
    ab = math.hypot(b[0] - a[0], b[1] - a[1])
    assert abs(ae + eb - ab) < 1e-9


def test_recorded_verification_all_ok():
    for t in ALL:
        assert all(c["ok"] for c in t["verification"]), t["verification"]


# -- (ii) metric facts of I.4 / I.5 ------------------------------------------

def ang(t, p, q, r):
    a, b, c = (t["placement"][k] for k in (p, q, r))
    u = (a[0] - b[0], a[1] - b[1])
    v = (c[0] - b[0], c[1] - b[1])
    nu, nv = math.hypot(*u), math.hypot(*v)
    cos = max(-1.0, min(1.0, (u[0] * v[0] + u[1] * v[1]) / (nu * nv)))
    return math.acos(cos)


def test_prop4_sas_facts_to_1e_9():
    assert abs(dist(TR4, "b", "c") - dist(TR4, "e", "f")) < 1e-9
    assert abs(ang(TR4, "a", "b", "c") - ang(TR4, "d", "e", "f")) < 1e-9
    assert abs(ang(TR4, "a", "c", "b") - ang(TR4, "d", "f", "e")) < 1e-9
    # superposition coincidences
    assert dist(TR4, "b'", "e") < 1e-9
    assert dist(TR4, "c'", "f") < 1e-9
    sup = next(s for s in TR4["steps"] if s["rule"] == "superposition")
    assert sup["superposition"]["correspondence"] == \
        [["a", "d"], ["b", "e"], ["c", "f"]]


def test_prop5_isosceles_facts_to_1e_9():
    assert abs(dist(TR5, "a", "b") - dist(TR5, "a", "c")) < 1e-9
    assert abs(ang(TR5, "a", "b", "c") - ang(TR5, "a", "c", "b")) < 1e-9
    assert abs(ang(TR5, "c", "b", "d") - ang(TR5, "b", "c", "e")) < 1e-9
    # constructed points sit where the steps put them
    assert abs(dist(TR5, "a", "g") - dist(TR5, "a", "f")) < 1e-9
    f_step = next(s for s in TR5["steps"]
                  if s["rule"] == "point_between_points_shorter_than")
    assert any(x[0] == "between" and x[1:4] == ["b", "f", "d"]
               for x in f_step["facts_added"])
    # f really is between b and d
    b, f, d = (TR5["placement"][k] for k in ("b", "f", "d"))
    bf = math.hypot(f[0] - b[0], f[1] - b[1])
    fd = math.hypot(d[0] - f[0], d[1] - f[1])
    bd = math.hypot(d[0] - b[0], d[1] - b[1])
    assert abs(bf + fd - bd) < 1e-9


# -- (iii) bad cases for I.4 / I.5 --------------------------------------------

def test_bad_case_prop4_moved_f_breaks_sas():
    bad = json.loads(json.dumps(TR4["placement"]))
    bad["f"] = [bad["f"][0] + 0.5, bad["f"][1] - 0.3]
    b, c, e, f = (bad[k] for k in ("b", "c", "e", "f"))
    bc = math.hypot(c[0] - b[0], c[1] - b[1])
    ef = math.hypot(f[0] - e[0], f[1] - e[1])
    assert abs(bc - ef) >= 1e-9, "moving f must break |b-c| = |e-f|"
    t2 = dict(TR4)
    t2 = json.loads(json.dumps(TR4))
    t2["placement"] = bad
    assert abs(ang(t2, "a", "c", "b") - ang(t2, "d", "f", "e")) >= 1e-9


def test_bad_case_prop5_moved_f_breaks_isosceles():
    bad = json.loads(json.dumps(TR5["placement"]))
    bad["f"] = [bad["f"][0] - 0.4, bad["f"][1] + 0.1]
    # |a-g| = |a-f| is the proposition_3 fact; moving f breaks it
    assert abs(dist(TR5, "a", "g") - math.hypot(
        bad["a"][0] - bad["f"][0], bad["a"][1] - bad["f"][1])) >= 1e-9
    # and the eq_len fact the step recorded names exactly those points
    p3 = next(s for s in TR5["steps"] if s["rule"] == "proposition_3")
    assert any(x[0] == "eq_len" and x[1:5] == ["a", "g", "f", "a"]
               for x in p3["facts_added"])


# -- (iii) bad case: the wrong intersection point must fail ------------------

def flip_intersection(prop, obj):
    """Rebuild the trace placement but pick the OTHER candidate for obj's
    introducing step, keeping everything else the same."""
    t = load(prop)
    step = next(s for s in t["steps"]
                if obj in [o["name"] for o in t["objects"]
                           if o["def_step"] == s["n"]])
    # both candidates for a circle intersection are symmetric about the
    # line joining the two centres (cc) or about the centre's foot (cl);
    # reflect the chosen point through the other candidate's mirror line.
    objs = {o["name"]: o for o in t["objects"]}
    j = objs[obj]["jsx"]
    assert j["type"] == "intersection"
    p = t["placement"][obj]
    if len(j["parents"]) == 2 and objs[j["parents"][0]]["kind"] == "circle" \
            and objs[j["parents"][1]]["kind"] == "circle":
        c1, c2 = (objs[n]["jsx"] for n in j["parents"])
        A, B = t["placement"][c1["center"]], t["placement"][c2["center"]]
        # mirror p through line AB
        ax, ay, bx, by = A + B
        dx, dy = bx - ax, by - ay
        u = ((p[0] - ax) * dx + (p[1] - ay) * dy) / (dx * dx + dy * dy)
        f = (ax + u * dx, ay + u * dy)
        other = (2 * f[0] - p[0], 2 * f[1] - p[1])
    else:
        C, L = j["parents"]
        cj = objs[C]["jsx"]
        cpos = t["placement"][cj["center"]]
        l1 = t["placement"][objs[L]["jsx"]["p"][0]]
        l2 = t["placement"][objs[L]["jsx"]["p"][1]]
        # mirror p through the line through cpos perpendicular to L
        # (equivalently: the two circle-line candidates are symmetric
        # about the foot of the centre on L)
        dx, dy = l2[0] - l1[0], l2[1] - l1[1]
        n = math.hypot(dx, dy)
        dx, dy = dx / n, dy / n
        # foot of the circle's CENTRE on the line = chord midpoint
        vx, vy = cpos[0] - l1[0], cpos[1] - l1[1]
        tt = vx * dx + vy * dy
        foot = (l1[0] + tt * dx, l1[1] + tt * dy)
        other = (2 * foot[0] - p[0], 2 * foot[1] - p[1])
    bad = json.loads(json.dumps(t["placement"]))
    bad[obj] = list(other)
    return t, bad, step


def test_bad_case_prop3_wrong_e_fails():
    t, bad, step = flip_intersection(3, "e")
    # the between a e b fact of the introducing step fails
    a, e, b = (bad[k] for k in ("a", "e", "b"))
    ae = math.hypot(e[0] - a[0], e[1] - a[1])
    eb = math.hypot(b[0] - e[0], b[1] - e[1])
    ab = math.hypot(b[0] - a[0], b[1] - a[1])
    assert abs(ae + eb - ab) >= 1e-9, "wrong candidate must break between"
    assert any(f[0] == "between" for f in
               [tuple(f) for f in step["facts_added"]])
    # and the recorded decision named that very fact
    assert "between" in step["placement_decision"]


def test_bad_case_prop2_wrong_l_fails():
    t, bad, step = flip_intersection(2, "l")
    d = lambda p, q: math.hypot(bad[p][0] - bad[q][0],
                                bad[p][1] - bad[q][1])
    assert abs(d("a", "l") - d("b", "c")) >= 1e-9, \
        "wrong candidate must break the metric fact |a-l| = |b-c|"


def test_bad_case_prop1_flip_keeps_metric_but_is_flagged():
    # I.1 is the honest boundary: BOTH intersection candidates are
    # equilateral, so flipping c keeps the metric facts and the trace
    # records that no fact decided the pick.
    t, bad, step = flip_intersection(1, "c")
    d = lambda p, q: math.hypot(bad[p][0] - bad[q][0],
                                bad[p][1] - bad[q][1])
    assert abs(d("c", "a") - d("a", "b")) < 1e-9  # still equilateral
    assert "no distinguishing fact" in step["placement_decision"]


def test_step_facts_hold_in_every_trace_and_swaps_are_caught():
    """Review (claude-9): verify5 checks only I.5's conclusion, which holds
    for any symmetric placement; swapping f and g, d and f, or b and c in
    prop05.json passed it. metric_facts_hold checks every between / eq_len /
    eq_ang / lt_len fact the steps add."""
    for prop in (2, 3, 4, 5):
        tr_ = load(prop)
        xy = {k: tuple(v) for k, v in tr_["placement"].items() if v}
        res = tr.metric_facts_hold(tr_["steps"], xy)
        assert res, prop
        assert all(ok for _, ok in res), (prop, [f for f, ok in res if not ok])
    t5 = load(5)
    xy = {k: tuple(v) for k, v in t5["placement"].items() if v}
    for a, b in (("f", "g"), ("d", "f"), ("b", "c")):
        x = dict(xy)
        x[a], x[b] = x[b], x[a]
        assert not all(ok for _, ok in tr.metric_facts_hold(t5["steps"], x)), (a, b)


# -- I.10 / I.11 (zai-9 traces; review additions, claude-9) -------------------

TR10, TR11 = load(10), load(11)
INV11 = {v: k for k, v in TR11["name_map"].items()}


def _xy(t, inv=None):
    inv = inv or {}
    return {inv.get(k, k): tuple(v) for k, v in t["placement"].items() if v}


def test_prop10_11_rebuild_matches_disk():
    for prop, t in ((10, TR10), (11, TR11)):
        rebuilt = tr.build_trace(prop)
        assert rebuilt["placement"] == t["placement"]
        assert json.loads(json.dumps(rebuilt["steps"])) == t["steps"]


def test_prop10_11_lean_lines_name_the_rule_and_cover_the_proof():
    """Every linked line names the step's rule, links are distinct and in
    order, and every euclid_apply line of the Lean proof is linked by some
    step (Lean's proof is a subsequence of the engine's)."""
    for t in (TR10, TR11):
        path = os.path.join(LEAN_DIR, t["lean_file"])
        lines = open(path).read().splitlines()
        linked = [s["lean"]["line"] for s in t["steps"] if s["lean"]["line"]]
        assert linked == sorted(linked) and len(set(linked)) == len(linked)
        for s in t["steps"]:
            if s["lean"]["line"]:
                assert s["rule"] in lines[s["lean"]["line"] - 1], s
            else:
                assert "engine-only" in s["lean_note"], s
        applies = [i + 1 for i, l in enumerate(lines)
                   if "euclid_apply" in l and i + 1 < len(lines)
                   and i + 1 <= next(j for j, x in enumerate(lines)
                                     if "use " in x) + 1]
        assert set(applies) == set(linked), (t["prop"], applies, linked)


def test_prop11_shows_lean_names_only():
    names = {o["name"] for o in TR11["objects"]}
    assert not names & {"u", "v", "w", "g"}, names
    assert {"a", "b", "c", "d", "e", "f"} <= names
    for s in TR11["steps"]:
        assert not set(map(str, s["args"])) & {"u", "v", "w", "g"}, s
    p = next(s for s in TR11["steps"] if s["rule"] == "proposition_1")
    assert p["args"][:2] == ["d", "e"] and p["bindings"] == {"d": "f"}


def test_by_contra_steps_draw_nothing():
    n = [s["n"] for s in TR10["steps"] if s["rule"] == "by_contra"]
    assert n, "I.10 has case-split steps"
    assert not [o for o in TR10["objects"] if o["def_step"] in n]
    for s in TR10["steps"]:
        if s["rule"] == "by_contra":
            assert s["facts_added"] == [] and s["lean"]["line"] is None


def test_prop10_11_step_facts_hold():
    for t, inv in ((TR10, None), (TR11, INV11)):
        res = tr.metric_facts_hold(tr._remap_names(t["steps"], inv or {}),
                                   _xy(t, inv))
        assert res and all(ok for _, ok in res), \
            (t["prop"], [f for f, ok in res if not ok])


def test_bad_case_prop10_d_off_the_midpoint_fails():
    xy = _xy(TR10)
    assert all(ok for _, ok in tr.verify10(xy))
    for moved in ((0.1, 0.0), (0.0, 0.1)):  # along AB; off AB
        x = dict(xy)
        x["d"] = (xy["d"][0] + moved[0], xy["d"][1] + moved[1])
        assert not all(ok for _, ok in tr.verify10(x)), moved


def test_bad_case_prop11_f_tilted_or_on_ab_fails():
    xy = _xy(TR11, INV11)
    assert all(ok for _, ok in tr.verify11(xy))
    x = dict(xy)
    x["f"] = (xy["f"][0] + 0.2, xy["f"][1])
    assert not all(ok for _, ok in tr.verify11(x))
    # f on AB but away from c: the old check (|f-c| > 0) passed this
    x = dict(xy)
    x["f"] = (0.7, 0.0)
    off = [ok for d, ok in tr.verify11(x) if "off AB" in d]
    assert off == [False]


# -- I.12 (kimi-1, packet P-euclid-viewer-I12) --------------------------------

TR12 = load(12)


def test_prop12_rebuild_matches_disk():
    rebuilt = tr.build_trace(12)
    assert rebuilt["placement"] == TR12["placement"]
    assert json.loads(json.dumps(rebuilt["steps"])) == TR12["steps"]


def test_prop12_lean_lines_name_the_rule_and_cover_the_proof():
    """Every linked line names the step's rule; links are distinct and in
    order; every euclid_apply line of Prop12.lean is linked. The `use h`
    line (19) sits BEFORE proposition_8's euclid_apply (line 20): the
    order test still holds because `use h` is not an euclid_apply line and
    the engine's step order matches the euclid_apply order around it
    (line_from_points c e at 18, proposition_8 at 20).
    intersection_circle_line_1 (Lean closes the premise by SMT) and
    perpendicular_if (Lean's euclid_finish) have no Lean line and say so.
    """
    path = os.path.join(LEAN_DIR, TR12["lean_file"])
    lines = open(path).read().splitlines()
    linked = [s["lean"]["line"] for s in TR12["steps"] if s["lean"]["line"]]
    assert linked == sorted(linked) and len(set(linked)) == len(linked)
    for s in TR12["steps"]:
        if s["lean"]["line"]:
            assert s["rule"] in lines[s["lean"]["line"] - 1], s
        else:
            assert "engine-only" in s["lean_note"], s
    applies = [i + 1 for i, l in enumerate(lines) if "euclid_apply" in l]
    assert set(applies) == set(linked), (applies, linked)
    engine_only = {s["rule"] for s in TR12["steps"] if not s["lean"]["line"]}
    assert engine_only == {"intersection_circle_line_1", "perpendicular_if"}
    # the `use h` line really does sit between steps 8 and 9's Lean lines
    use_line = next(i + 1 for i, l in enumerate(lines)
                    if l.strip().startswith("use "))
    assert TR12["steps"][7]["lean"]["line"] < use_line < \
        TR12["steps"][8]["lean"]["line"]


def test_prop12_step_facts_hold():
    xy = {k: tuple(v) for k, v in TR12["placement"].items() if v}
    res = tr.metric_facts_hold(TR12["steps"], xy)
    assert res and all(ok for _, ok in res), \
        [f for f, ok in res if not ok]
    assert all(ok for _, ok in tr.verify12(xy))


def test_bad_case_prop12_h_moved_along_ab_fails():
    xy = {k: tuple(v) for k, v in TR12["placement"].items() if v}
    x = dict(xy)
    x["h"] = (xy["h"][0] + 0.15, xy["h"][1])  # along AB, off the midpoint
    fails = [d for d, ok in tr.verify12(x) if not ok]
    assert any("|h-e| = |h-g|" in d for d in fails), fails
    assert any("right" in d for d in fails), fails


def test_bad_case_prop12_c_moved_angle_not_right_fails():
    xy = {k: tuple(v) for k, v in TR12["placement"].items() if v}
    x = dict(xy)
    x["c"] = (xy["c"][0] + 0.3, xy["c"][1])  # c no longer above h
    fails = [d for d, ok in tr.verify12(x) if not ok]
    assert any("right" in d for d in fails), fails


def test_bad_case_prop12_d_on_cs_side_fails():
    xy = {k: tuple(v) for k, v in TR12["placement"].items() if v}
    x = dict(xy)
    x["d"] = (xy["d"][0], -xy["d"][1])  # reflected onto c's side of AB
    fails = [d for d, ok in tr.verify12(x) if not ok]
    assert any("opposite sides" in d for d in fails), fails


def test_bad_case_prop12_e_g_off_the_circle_fails():
    """Review fix (claude-9): e and g pushed outward along AB, symmetric
    about h, keep h the midpoint and the angle right; they must still fail
    because they are no longer on circle EFG."""
    bad = json.loads(json.dumps(TR12["placement"]))
    h = bad["h"]
    for k in ("e", "g"):
        bad[k] = [h[0] + 1.5 * (bad[k][0] - h[0]), h[1] + 1.5 * (bad[k][1] - h[1])]
    fails = [d for d, ok in tr.verify12(bad) if not ok]
    assert fails == ["I.12: e on circle EFG (|c-e| = |c-d|)"], fails


# -- I.13 (kimi-2, packet P-euclid-viewer-I13) --------------------------------
# I.13's trace is a FLATTENED nested by_cases: the outer split (Lean's
# by_cases line), branch 1 (not drawn), branch 2's two construction steps
# (Lean's proposition_11' / line_from_points lines), the inner split (no
# Lean line), then both inner branches (drawn case: a on c's side of BE).

TR13 = load(13)


def test_prop13_rebuild_matches_disk():
    rebuilt = tr.build_trace(13)
    assert rebuilt["placement"] == TR13["placement"]
    assert json.loads(json.dumps(rebuilt["steps"])) == TR13["steps"]


def test_prop13_lean_lines_cover_the_apply_lines_and_the_by_cases():
    path = os.path.join(LEAN_DIR, TR13["lean_file"])
    lines = open(path).read().splitlines()
    linked = {s["lean"]["line"] for s in TR13["steps"]
              if s["lean"]["line"]}
    # every linked line names the step's rule (or is the by_cases line)
    for s in TR13["steps"]:
        ln = s["lean"]["line"]
        if ln is None:
            assert "engine-only" in s["lean_note"], s
        elif s["rule"] == "by_cases":
            assert lines[ln - 1].strip().startswith("by_cases"), s
        else:
            assert s["rule"] in lines[ln - 1], s
    # every euclid_apply line of Prop13.lean is linked, and the by_cases
    # line is linked by the outer split
    applies = {i + 1 for i, l in enumerate(lines) if "euclid_apply" in l}
    assert applies <= linked, (applies, linked)
    assert len(linked) == len(applies) + 1
    # the drawn construction steps are exactly proposition_11' and
    # line_from_points; the inner by_cases has no Lean line
    drawn = {s["rule"] for s in TR13["steps"]
             if s["lean"]["line"] in applies}
    assert drawn == {"proposition_11'", "line_from_points"}
    inner = [s for s in TR13["steps"]
             if s["rule"] == "by_cases" and s["lean"]["line"] is None]
    assert len(inner) == 1


def test_prop13_cases_recorded():
    """Every step says which case it belongs to; the drawn case is branch
    2 with a on c's side of BE; the others say they are not drawn."""
    for s in TR13["steps"]:
        assert s.get("case"), s
        assert s["case"] in s["placement_decision"]
    assert any(s["case"].endswith("not drawn") for s in TR13["steps"])
    drawn = [s for s in TR13["steps"]
             if not s["case"].endswith("not drawn")]
    assert {s["rule"] for s in drawn} >= {
        "by_cases", "proposition_11'", "line_from_points"}
    # the conclusions of both by_cases levels are the I.13 goal, shown as
    # the facts the split adds
    for s in TR13["steps"]:
        if s["rule"] == "by_cases":
            assert s["facts_added"] == [["sum_rt_ang", "c", "b", "a", "d"]]


def test_prop13_step_facts_hold():
    xy = {k: tuple(v) for k, v in TR13["placement"].items() if v}
    res = tr.metric_facts_hold(TR13["steps"], xy)
    assert res and all(ok for _, ok in res), \
        [f for f, ok in res if not ok]
    assert all(ok for _, ok in tr.verify13(xy))


def _xy13():
    return {k: tuple(v) for k, v in TR13["placement"].items() if v}


def test_bad_case_prop13_a_on_cd_fails():
    xy = _xy13()
    assert all(ok for _, ok in tr.verify13(xy))
    x = dict(xy)
    x["a"] = (1.2, 0.0)  # a on CD
    fails = [d for d, ok in tr.verify13(x) if not ok]
    assert any("a off CD" in d for d in fails), fails


def test_bad_case_prop13_e_not_perpendicular_fails():
    x = _xy13()
    x["e"] = (0.3, 1.0)  # tilted off the perpendicular
    fails = [d for d, ok in tr.verify13(x) if not ok]
    assert any("right" in d for d in fails), fails


def test_bad_case_prop13_e_opposite_side_from_a_fails():
    x = _xy13()
    x["e"] = (0.0, -1.0)  # still perpendicular, but across CD from a
    fails = [d for d, ok in tr.verify13(x) if not ok]
    assert fails == ["I.13: e and a on the same side of CD"], fails


def test_bad_case_prop13_a_on_ds_side_of_be_fails():
    x = _xy13()
    x["a"] = (-0.8, 0.9)  # the mirror case: a on d's side of BE
    fails = [d for d, ok in tr.verify13(x) if not ok]
    assert fails == ["I.13: drawn case: a on c's side of BE"], fails


# -- I.14 (kimi-2, packet P-euclid-viewer-I14) --------------------------------
# I.14's trace is a FLATTENED nested by_contra: the outer by_contra (Lean's
# by_contra line), its branch's six steps (extend_point / proposition_13
# are Lean's lines; between_symm_parts x2, pasch_3_opposing and
# opposing_opposing_same_side are engine-only), the inner split (Lean's
# by_cases line; the engine renders it as a by_contra concluding False),
# then both inner branches' steps. Drawn in the TRUE configuration (d on
# the extension of c-b beyond b): a "false" BD off BC would violate the
# angle-sum fact the placement verifies, so the refuted assumption is
# carried by the case notes instead of the picture.

TR14 = load(14)


def test_prop14_rebuild_matches_disk():
    rebuilt = tr.build_trace(14)
    assert rebuilt["placement"] == TR14["placement"]
    assert json.loads(json.dumps(rebuilt["steps"])) == TR14["steps"]


def test_prop14_lean_lines_cover_the_apply_lines_and_the_splits():
    path = os.path.join(LEAN_DIR, TR14["lean_file"])
    lines = open(path).read().splitlines()
    linked = {s["lean"]["line"] for s in TR14["steps"]
              if s["lean"]["line"]}
    # every linked line names the step's rule (or is a split line)
    for s in TR14["steps"]:
        ln = s["lean"]["line"]
        if ln is None:
            assert "engine-only" in s["lean_note"], s
        elif s["rule"] == "by_contra":
            assert (lines[ln - 1].strip().startswith("by_contra")
                    or lines[ln - 1].strip().startswith("by_cases")), s
        else:
            assert s["rule"] in lines[ln - 1], s
    # every euclid_apply line of Prop14.lean is linked, and the by_contra
    # and by_cases lines are linked by the two splits
    applies = {i + 1 for i, l in enumerate(lines) if "euclid_apply" in l}
    assert applies <= linked, (applies, linked)
    assert len(linked) == len(applies) + 2
    # the drawn construction step is extend_point; proposition_13 is a
    # cited lemma with a Lean line but no drawing
    drawn = {s["rule"] for s in TR14["steps"]
             if s["lean"]["line"] in applies}
    assert drawn == {"extend_point", "proposition_13", "sum_angles_onlyif"}


def test_prop14_cases_recorded():
    """Every step carries the drawing decision: the steps run under the
    refuted assumption BC != BD and the diagram draws the true
    configuration instead."""
    for s in TR14["steps"]:
        assert s.get("case"), s
        assert s["case"] in s["placement_decision"]
        assert "BC != BD" in s["case"], s
    assert any("Euclid's omitted case" in s["case"] for s in TR14["steps"])
    # the outer by_contra's conclusions are the goal as mutual incidence
    outer = TR14["steps"][0]
    assert outer["rule"] == "by_contra"
    assert outer["facts_added"] == [["on_line", "d", "BC"],
                                    ["on_line", "c", "BD"]]
    # the inner split concludes False (both branches refuted)
    inner = [s for s in TR14["steps"]
             if s["rule"] == "by_contra"][1]
    assert inner["facts_added"] == [["false"]]


def test_prop14_step_facts_hold():
    xy = {k: tuple(v) for k, v in TR14["placement"].items() if v}
    res = tr.metric_facts_hold(TR14["steps"], xy)
    assert res and all(ok for _, ok in res), \
        [f for f, ok in res if not ok]
    assert all(ok for _, ok in tr.verify14(xy))


def _xy14():
    return {k: tuple(v) for k, v in TR14["placement"].items() if v}


def test_bad_case_prop14_c_d_same_side_of_ab_fails():
    x = _xy14()
    assert all(ok for _, ok in tr.verify14(x))
    x["d"] = (1.8, 0.0)  # d on c's side of AB (and off the extension)
    fails = [d for d, ok in tr.verify14(x) if not ok]
    assert any("opposing sides of AB" in d for d in fails), fails


def test_bad_case_prop14_angle_sum_not_180_fails():
    x = _xy14()
    x["d"] = (-1.8, 0.4)  # d off the line c-b: the sum breaks
    fails = [d for d, ok in tr.verify14(x) if not ok]
    assert any("180 degrees" in d for d in fails), fails


def test_bad_case_prop14_d_not_past_b_fails():
    x = _xy14()
    x["d"] = (0.7, 0.0)  # d between b and c: b no longer between c and d
    fails = [d for d, ok in tr.verify14(x) if not ok]
    assert any("between c b d" in d for d in fails), fails


def test_bad_case_prop14_a_on_the_line_fails():
    x = _xy14()
    x["a"] = (0.6, 0.0)  # a on the line c-d: no triangle, no angles
    fails = [d for d, ok in tr.verify14(x) if not ok]
    assert any("a off the line c-d" in d for d in fails), fails


# -- I.15 (kimi-2, packet P-euclid-viewer-I15) --------------------------------
# I.15 (vertical angles) is FLAT: six steps, no case splits. Steps 1-3
# (between_symm_parts) are engine-only (they rebuild the dpl premises that
# proposition_13 matches on; Lean's SMT produces the neqs silently); steps
# 4-6 are Lean's three euclid_apply proposition_13 lines, in order. The
# crossing is drawn at a non-right angle so the two vertical pairs
# visibly differ.

TR15 = load(15)


def test_prop15_rebuild_matches_disk():
    rebuilt = tr.build_trace(15)
    assert rebuilt["placement"] == TR15["placement"]
    assert json.loads(json.dumps(rebuilt["steps"])) == TR15["steps"]


def test_prop15_lean_lines_cover_the_apply_lines():
    path = os.path.join(LEAN_DIR, TR15["lean_file"])
    lines = open(path).read().splitlines()
    linked = {s["lean"]["line"] for s in TR15["steps"]
              if s["lean"]["line"]}
    for s in TR15["steps"]:
        ln = s["lean"]["line"]
        if ln is None:
            assert "engine-only" in s["lean_note"], s
        else:
            assert s["rule"] in lines[ln - 1], s
    # every euclid_apply line of Prop15.lean is linked, and nothing else
    applies = {i + 1 for i, l in enumerate(lines) if "euclid_apply" in l}
    assert applies == linked, (applies, linked)
    # the engine-only steps are exactly the three between_symm_parts
    engine_only = [s["rule"] for s in TR15["steps"]
                   if s["lean"]["line"] is None]
    assert engine_only == ["between_symm_parts"] * 3


def test_prop15_step_facts_hold():
    xy = {k: tuple(v) for k, v in TR15["placement"].items() if v}
    # the steps add neq / not_between / sum_rt_ang facts (none of the
    # kinds metric_facts_hold covers); check the sum_rt_ang facts
    # numerically: sum_rt_ang p e q r is angle p:e:q + angle q:e:r = pi
    sums = [f for s in TR15["steps"] for f in s["facts_added"]
            if f[0] == "sum_rt_ang"]
    assert len(sums) == 3  # the three proposition_13 citations
    for f in sums:
        _, p, e, q, r = f
        total = tr._ang(xy, p, e, q) + tr._ang(xy, q, e, r)
        assert abs(total - math.pi) < 1e-9, (f, total)
    # and the neq / not_between facts hold of the placement
    for s in TR15["steps"]:
        for f in s["facts_added"]:
            if f[0] == "neq":
                assert tr._norm(tr._sub(xy[f[1]], xy[f[2]])) > 1e-9, f
    assert all(ok for _, ok in tr.verify15(xy))


def _xy15():
    return {k: tuple(v) for k, v in TR15["placement"].items() if v}


def test_bad_case_prop15_e_not_between_a_b_fails():
    x = _xy15()
    assert all(ok for _, ok in tr.verify15(x))
    x["e"] = (2.0, 0.0)  # e on AB but past b: no longer between a and b
    fails = [d for d, ok in tr.verify15(x) if not ok]
    assert any("between a e b" in d for d in fails), fails


def test_bad_case_prop15_e_not_between_d_c_fails():
    x = _xy15()
    x["d"] = (0.375, 0.5)  # d on c's side of e: e not between d and c
    fails = [d for d, ok in tr.verify15(x) if not ok]
    assert any("between d e c" in d for d in fails), fails


def test_bad_case_prop15_lines_parallel_fails():
    x = _xy15()
    # CD through e and "parallel" to AB means coincident with AB
    x["c"], x["d"] = (0.75, 0.0), (-0.375, 0.0)
    fails = [d for d, ok in tr.verify15(x) if not ok]
    assert any("CD != AB" in d for d in fails), fails


def test_bad_case_prop15_perpendicular_crossing_rejected():
    # perpendicular crossing is geometrically FINE for the theorem (both
    # vertical pairs are pairs of right angles, hence equal); verify15
    # rejects it anyway, deliberately: the check pins the drawn general
    # case, where the two pairs visibly differ
    x = _xy15()
    x["c"], x["d"] = (0.0, 1.0), (0.0, -0.5)
    fails = [d for d, ok in tr.verify15(x) if not ok]
    assert [d for d, ok in tr.verify15(x) if ok], fails
    assert len(fails) == 1 and "not right" in fails[0], fails


# -- I.16 (kimi-2, packet P-euclid-viewer-I16) -------------------------------
# I.16 (exterior angle) is FLAT: 14 steps, no case splits, and only the
# SECOND conjunct (Euclid's own proof — angle a:c:d > angle b:a:c, Lean's
# second bullet, Book/Prop16.lean lines 26-32). Seven steps are engine-only
# (three between_symm_parts, two pasch_3_opposing,
# opposing_opposing_same_side, sum_angles_onlyif: the diagram facts Lean's
# SMT produces silently); the seven construction/citation steps link to
# lines 26-32. line_from_points f c is textually identical in both
# bullets, so matching is restricted to lines past the first bullet's
# euclid_finish.

TR16 = load(16)


def test_prop16_rebuild_matches_disk():
    rebuilt = tr.build_trace(16)
    assert rebuilt["placement"] == TR16["placement"]
    assert json.loads(json.dumps(rebuilt["steps"])) == TR16["steps"]


def test_prop16_lean_lines_cover_the_second_bullet():
    path = os.path.join(LEAN_DIR, TR16["lean_file"])
    lines = open(path).read().splitlines()
    for s in TR16["steps"]:
        ln = s["lean"]["line"]
        if ln is None:
            assert "engine-only" in s["lean_note"], s
        else:
            assert s["rule"] in lines[ln - 1], s
    # exactly the second bullet's seven euclid_apply lines are linked
    applies = {i + 1 for i, l in enumerate(lines) if "euclid_apply" in l}
    linked = {s["lean"]["line"] for s in TR16["steps"]
              if s["lean"]["line"]}
    assert linked == {26, 27, 28, 29, 30, 31, 32}, linked
    assert linked <= applies
    # the engine-only steps are exactly the seven silent diagram facts
    engine_only = [s["rule"] for s in TR16["steps"]
                   if s["lean"]["line"] is None]
    assert engine_only == ["between_symm_parts"] * 3 + [
        "pasch_3_opposing"] * 2 + ["opposing_opposing_same_side",
                                   "sum_angles_onlyif"]


def test_prop16_step_facts_hold():
    xy = {k: tuple(v) for k, v in TR16["placement"].items() if v}
    bad = [(f, ok) for f, ok in tr.metric_facts_hold(TR16["steps"], xy)
           if not ok]
    assert not bad, bad
    # the eq_len facts the construction rests on are present and exact
    eqs = [f for s in TR16["steps"] for f in s["facts_added"]
           if f[0] == "eq_len"]
    assert any(f[1:5] == ["a", "e", "e", "c"] for f in eqs), eqs
    assert any(f[1:5] == ["e", "f", "b", "e"] for f in eqs), eqs
    assert all(ok for _, ok in tr.verify16(xy))
    # the last step's info bar states the whole-exceeds-part conclusion
    last = TR16["steps"][-1]
    assert last["rule"] == "sum_angles_onlyif"
    assert "exceeds the PART" in last["placement_decision"]


def _xy16():
    return {k: tuple(v) for k, v in TR16["placement"].items() if v}


def test_bad_case_prop16_d_between_b_and_c_fails():
    x = _xy16()
    assert all(ok for _, ok in tr.verify16(x))
    x["d"] = (0.8, 0.0)  # d between b and c: no exterior angle at c
    fails = [d for d, ok in tr.verify16(x) if not ok]
    assert any("between b c d" in d for d in fails), fails


def test_bad_case_prop16_f_not_on_ray_b_to_e_fails():
    x = _xy16()
    # f reflected across e: still |b-e| = |e-f| but not on the ray b->e
    # (between b e f fails)
    x["f"] = (2 * x["e"][0] - x["f"][0], 2 * x["e"][1] - x["f"][1])
    fails = [d for d, ok in tr.verify16(x) if not ok]
    assert any("midpoint of b-f" in d for d in fails), fails


def test_bad_case_prop16_e_not_midpoint_fails():
    x = _xy16()
    x["e"] = (x["e"][0] + 0.15, x["e"][1])  # off the midpoint of a-c
    fails = [d for d, ok in tr.verify16(x) if not ok]
    assert any("midpoint of a-c" in d for d in fails), fails


def test_bad_case_prop16_degenerate_triangle_fails():
    x = _xy16()
    x["a"] = (0.5, 0.0)  # a on BC: no triangle, no angles
    fails = [d for d, ok in tr.verify16(x) if not ok]
    assert any("a off BC" in d for d in fails), fails


# -- I.17 (kimi-2, packet P-euclid-viewer-I17) -------------------------------
# I.17 (any two angles of a triangle < two right) is FLAT: 4 steps, no
# case splits. d is CONSTRUCTED by step 1 (extend_point BC b c), not a
# hypothesis. between_symm_parts b c d is engine-only (it rebuilds the
# dpl b d BC premise proposition_13 needs; Lean's SMT produces the neq
# silently); the three remaining steps are Lean's three euclid_apply
# lines (Prop17.lean:12-14), in order.

TR17 = load(17)


def test_prop17_rebuild_matches_disk():
    rebuilt = tr.build_trace(17)
    assert rebuilt["placement"] == TR17["placement"]
    assert json.loads(json.dumps(rebuilt["steps"])) == TR17["steps"]


def test_prop17_lean_lines_cover_the_apply_lines():
    path = os.path.join(LEAN_DIR, TR17["lean_file"])
    lines = open(path).read().splitlines()
    for s in TR17["steps"]:
        ln = s["lean"]["line"]
        if ln is None:
            assert "engine-only" in s["lean_note"], s
        else:
            assert s["rule"] in lines[ln - 1], s
    # exactly the three euclid_apply lines are linked, in order
    applies = {i + 1 for i, l in enumerate(lines) if "euclid_apply" in l}
    linked = [s["lean"]["line"] for s in TR17["steps"]
              if s["lean"]["line"]]
    assert linked == sorted(linked) and set(linked) == applies, linked
    # the only engine-only step is between_symm_parts
    engine_only = [s["rule"] for s in TR17["steps"]
                   if s["lean"]["line"] is None]
    assert engine_only == ["between_symm_parts"], engine_only


def test_prop17_step_facts_hold():
    xy = {k: tuple(v) for k, v in TR17["placement"].items() if v}
    bad = [(f, ok) for f, ok in tr.metric_facts_hold(TR17["steps"], xy)
           if not ok]
    assert not bad, bad
    assert all(ok for _, ok in tr.verify17(xy))
    # the last step's info bar states the conclusion's why
    last = TR17["steps"][-1]
    assert last["rule"] == "proposition_13"
    assert "two right angles" in last["placement_decision"]
    assert "a:c:d > angle a:b:c" in last["placement_decision"]


def _xy17():
    return {k: tuple(v) for k, v in TR17["placement"].items() if v}


def test_bad_case_prop17_d_between_b_and_c_fails():
    x = _xy17()
    assert all(ok for _, ok in tr.verify17(x))
    x["d"] = (0.8, 0.0)  # d between b and c: no exterior angle at c
    fails = [d for d, ok in tr.verify17(x) if not ok]
    assert any("between b c d" in d for d in fails), fails


def test_bad_case_prop17_degenerate_triangle_fails():
    x = _xy17()
    x["a"] = (0.5, 0.0)  # a on BC: no triangle, no angles
    fails = [d for d, ok in tr.verify17(x) if not ok]
    assert any("a off BC" in d for d in fails), fails


def test_bad_case_prop17_goal_margin_fails():
    x = _xy17()
    # a far away: the angle at a approaches 0, so the two angles at b
    # and c sum to nearly 180 and the goal margin (0.5 rad short) fails
    x["a"] = (0.8, 10.0)
    fails = [d for d, ok in tr.verify17(x) if not ok]
    assert any("GOAL" in d for d in fails), fails


# -- I.18 (kimi-2, packet P-euclid-viewer-I18) -------------------------------
# I.18 (the greater side subtends the greater angle) is FLAT: 5 steps, no
# case splits. d is CONSTRUCTED by step 1 (proposition_3 a c a b AC AB:
# on segment a-c with |a-d| = |a-b|), not a hypothesis. Steps 1-4 are
# Prop18.lean's euclid_apply lines 13-16; the engine's last step is
# sum_angles_onlyif where Lean cites sum_angles_if (line 17) — the other
# direction (its converse axiom, Transfer.lean:83/:90) — linked to line 17 with an
# "other-direction" lean_note so the reader is not misled.

TR18 = load(18)


def test_prop18_rebuild_matches_disk():
    rebuilt = tr.build_trace(18)
    assert rebuilt["placement"] == TR18["placement"]
    assert json.loads(json.dumps(rebuilt["steps"])) == TR18["steps"]


def test_prop18_lean_lines_cover_the_apply_lines():
    path = os.path.join(LEAN_DIR, TR18["lean_file"])
    lines = open(path).read().splitlines()
    for s in TR18["steps"]:
        ln = s["lean"]["line"]
        assert ln is not None, s  # all five steps linked
        if s["rule"] == "sum_angles_onlyif":
            # the other direction of the axiom Lean cites here
            assert "sum_angles_if" in lines[ln - 1], s
            assert "OTHER direction" in s["lean_note"], s
        else:
            assert s["rule"] in lines[ln - 1], s
    # exactly the five euclid_apply lines are linked, in order
    applies = {i + 1 for i, l in enumerate(lines) if "euclid_apply" in l}
    linked = [s["lean"]["line"] for s in TR18["steps"]]
    assert linked == sorted(linked) and set(linked) == applies, linked


def test_prop18_step_facts_hold():
    xy = {k: tuple(v) for k, v in TR18["placement"].items() if v}
    bad = [(f, ok) for f, ok in tr.metric_facts_hold(TR18["steps"], xy)
           if not ok]
    assert not bad, bad
    assert all(ok for _, ok in tr.verify18(xy))
    # the last step's info bar states the conclusion's why
    last = TR18["steps"][-1]
    assert last["rule"] == "sum_angles_onlyif"
    assert "angle a:b:c > angle a:b:d" in last["placement_decision"]
    assert "= angle b:c:a" in last["placement_decision"]


def _xy18():
    return {k: tuple(v) for k, v in TR18["placement"].items() if v}


def test_bad_case_prop18_d_beyond_c_fails():
    x = _xy18()
    assert all(ok for _, ok in tr.verify18(x))
    # d beyond c on the ray a->c with |a-d| = |a-b|... but |a-b| < |a-c|
    # here, so place d past c instead: not between a and c
    ax, ay = x["a"]
    cx, cy = x["c"]
    ux, uy = (cx - ax), (cy - ay)
    n = math.hypot(ux, uy)
    x["d"] = (ax + 1.3 * ux, ay + 1.3 * uy)  # d beyond c
    fails = [d for d, ok in tr.verify18(x) if not ok]
    assert any("between a d c" in d for d in fails), fails


def test_bad_case_prop18_shorter_side_fails():
    x = _xy18()
    # |a-c| < |a-b|: the hypothesis is violated and the goal flips
    x["a"] = (1.1, 0.5)  # |a-b| = ~1.21 > |a-c| = ~0.71
    fails = [d for d, ok in tr.verify18(x) if not ok]
    assert any("hypothesis |a-c| > |a-b|" in d for d in fails), fails


# -- I.19 (kimi-2, packet P-euclid-viewer-I19) -------------------------------
# I.19 (the greater angle is subtended by the greater side) is a by_contra
# with two cited-lemma branches (Lean's by_cases |(a-c)| = |(a-b)| split):
# branch 1 proposition_5' (equal sides give equal angles, against the
# hypothesis), branch 2 proposition_18 (I.18 in triangle a-c-b). Like
# I.14 the trace draws the TRUE configuration (angle a:b:c > angle b:c:a,
# hence |a-c| > |a-b|); both branch assumptions are refuted, so the
# branch facts must NOT hold in the placement. Steps link to
# Prop19.lean:12 (by_contra), :13 (by_cases), :14 (proposition_5'),
# :16 (proposition_18).

TR19 = load(19)


def test_prop19_rebuild_matches_disk():
    rebuilt = tr.build_trace(19)
    assert rebuilt["placement"] == TR19["placement"]
    assert json.loads(json.dumps(rebuilt["steps"])) == TR19["steps"]


def test_prop19_lean_lines_cover_the_split_and_apply_lines():
    path = os.path.join(LEAN_DIR, TR19["lean_file"])
    lines = open(path).read().splitlines()
    for s in TR19["steps"]:
        ln = s["lean"]["line"]
        assert ln is not None, s  # all four steps linked
        if s["rule"] == "by_contra":
            assert lines[ln - 1].strip().startswith("by_contra"), s
        elif s["rule"] == "by_cases":
            assert lines[ln - 1].strip().startswith("by_cases"), s
        else:
            assert s["rule"] in lines[ln - 1], s
    linked = [s["lean"]["line"] for s in TR19["steps"]]
    assert linked == sorted(linked), linked
    # both euclid_apply lines plus the by_contra and by_cases lines
    applies = {i + 1 for i, l in enumerate(lines) if "euclid_apply" in l}
    assert applies <= set(linked) and len(linked) == len(applies) + 2


def test_prop19_cases_recorded():
    """Every step says which refuted assumption it runs under and that
    the diagram draws the true configuration."""
    for s in TR19["steps"]:
        assert s.get("case"), s
        assert s["case"] in s["placement_decision"], s
        assert "TRUE configuration" in s["case"], s
    cases = [s["case"] for s in TR19["steps"]]
    assert any("|a-c| = |a-b|" in c and "proposition_5'" in c
               and "contradicting the hypothesis" in c for c in cases)
    assert any("|a-b| > |a-c|" in c and "proposition_18" in c
               and "contradicting the hypothesis" in c for c in cases)
    # the outer by_contra's conclusion is the goal
    outer = TR19["steps"][0]
    assert outer["rule"] == "by_contra"
    assert outer["facts_added"] == [["gt_len", "a", "c", "a", "b"]]
    # the split shows exactly the two refuted assumptions
    split = TR19["steps"][1]
    assert split["rule"] == "by_cases"
    assert split["facts_added"] == [["eq_len", "a", "c", "a", "b"],
                                    ["gt_len", "a", "b", "a", "c"]]


def test_prop19_step_facts_hold():
    xy = {k: tuple(v) for k, v in TR19["placement"].items() if v}
    assert all(ok for _, ok in tr.verify19(xy))
    # the by_contra's conclusion (the goal) holds in the true placement
    assert tr.metric_facts_hold([TR19["steps"][0]], xy) == []  # gt_len
    # unchecked kind; checked directly:
    assert dist(TR19, "a", "c") > dist(TR19, "a", "b") + 0.2
    # the REFUTED branch facts (the split's assumptions and branch 1's
    # equal-angles conclusion) must NOT hold in the true placement
    refuted = tr.metric_facts_hold(TR19["steps"][1:], xy)
    assert refuted and not any(ok for _, ok in refuted), refuted


def _xy19():
    return {k: tuple(v) for k, v in TR19["placement"].items() if v}


def test_bad_case_prop19_angle_order_flipped_fails():
    x = _xy19()
    assert all(ok for _, ok in tr.verify19(x))
    x["a"] = (1.1, 0.5)  # nearer c: angle a:b:c < angle b:c:a
    fails = [d for d, ok in tr.verify19(x) if not ok]
    assert any("hypothesis angle a:b:c > angle b:c:a" in d
               for d in fails), fails


def test_bad_case_prop19_isosceles_fails():
    x = _xy19()
    # |a-c| = |a-b| (branch 1's refuted assumption): a directly above
    # the midpoint of b-c at the height that keeps |a-b| unchanged;
    # both the hypothesis margin and the goal fail
    h = math.sqrt(dist(TR19, "a", "b") ** 2 - 0.8 ** 2)
    x["a"] = (0.8, h)
    fails = [d for d, ok in tr.verify19(x) if not ok]
    assert any("GOAL |a-c| > |a-b|" in d for d in fails), fails


# -- I.20 (kimi-2, packet P-euclid-viewer-I20) -------------------------------
# I.20 (two sides of a triangle together exceed the third,
# |b-a| + |a-c| > |b-c|) is FLAT: 6 steps, no case splits. d' and d are
# CONSTRUCTED (step 1 extend_point_longer AB b a (c-a): d' on the ray
# b->a beyond a; step 2 proposition_3 a d' a c AB AC: d on segment a-d'
# with |a-d| = |a-c|), not hypotheses. Steps 1-6 are Prop20.lean's
# euclid_apply lines 12-17, in order. The last step's bar carries the
# conclusion's why: |b-a| + |a-c| = |b-d| > |b-c|.

TR20 = load(20)


def test_prop20_rebuild_matches_disk():
    rebuilt = tr.build_trace(20)
    assert rebuilt["placement"] == TR20["placement"]
    assert json.loads(json.dumps(rebuilt["steps"])) == TR20["steps"]


def test_prop20_lean_lines_cover_the_apply_lines():
    path = os.path.join(LEAN_DIR, TR20["lean_file"])
    lines = open(path).read().splitlines()
    for s in TR20["steps"]:
        ln = s["lean"]["line"]
        assert ln is not None, s  # all six steps linked
        assert s["rule"] in lines[ln - 1], s
    # exactly the six euclid_apply lines are linked, in order
    applies = {i + 1 for i, l in enumerate(lines) if "euclid_apply" in l}
    linked = [s["lean"]["line"] for s in TR20["steps"]]
    assert linked == sorted(linked) and set(linked) == applies, linked


def test_prop20_step_facts_hold():
    xy = {k: tuple(v) for k, v in TR20["placement"].items() if v}
    bad = [(f, ok) for f, ok in tr.metric_facts_hold(TR20["steps"], xy)
           if not ok]
    assert not bad, bad
    assert all(ok for _, ok in tr.verify20(xy))
    # the last step's info bar states the conclusion's why
    last = TR20["steps"][-1]
    assert last["rule"] == "proposition_19"
    assert "|b-a| + |a-c| = |b-a| + |a-d|" in last["placement_decision"]
    assert "> |b-c|" in last["placement_decision"]
    assert "angle at c exceeds" in last["placement_decision"]


def _xy20():
    return {k: tuple(v) for k, v in TR20["placement"].items() if v}


def test_bad_case_prop20_d_between_b_and_a_fails():
    x = _xy20()
    assert all(ok for _, ok in tr.verify20(x))
    # d between b and a instead of beyond a: the betweenness and the
    # length-sum checks fail
    bx, by = x["b"]
    ax, ay = x["a"]
    x["d"] = (bx + 0.5 * (ax - bx), by + 0.5 * (ay - by))
    fails = [d for d, ok in tr.verify20(x) if not ok]
    assert any("between b a d" in d for d in fails), fails


def test_bad_case_prop20_ad_neq_ac_fails():
    x = _xy20()
    # d still on the ray b->a beyond a but |a-d| = 0.7|a-c|: the
    # proposition_3 cut is violated and the I.5' equal-angles step fails
    ax, ay = x["a"]
    dx, dy = x["d"]
    x["d"] = (ax + 0.7 * (dx - ax), ay + 0.7 * (dy - ay))
    fails = [d for d, ok in tr.verify20(x) if not ok]
    assert any("|a-d| = |a-c|" in d for d in fails), fails


# -- I.21 (kimi-2, packet P-euclid-viewer-I21) -------------------------------
# I.21 (lines from inside a triangle: for d inside triangle a-b-c,
# |b-d| + |d-c| < |b-a| + |a-c| and angle b:d:c > angle b:a:c). d is a
# HYPOTHESIS point (strictly inside); e is CONSTRUCTED by step 3
# (intersection_lines BD AC). Steps 1-2, 4-5 are engine-only
# (triple_incidence_1, intersection_lines_opposing, pasch_4, and a
# by_contra fixing the order b d e — premises Lean's SMT closes
# silently); steps 3, 6-9 are Prop21.lean's euclid_apply lines
# 13-16, 18-19. The last step's bar carries the conclusion's why for
# both conjuncts.

TR21 = load(21)


def test_prop21_rebuild_matches_disk():
    rebuilt = tr.build_trace(21)
    assert rebuilt["placement"] == TR21["placement"]
    assert json.loads(json.dumps(rebuilt["steps"])) == TR21["steps"]


def test_prop21_lean_lines_cover_the_apply_lines():
    path = os.path.join(LEAN_DIR, TR21["lean_file"])
    lines = open(path).read().splitlines()
    linked = []
    for s in TR21["steps"]:
        ln = s["lean"]["line"]
        if ln is None:
            # engine-only: the four SMT-silent steps and the by_contra
            assert s["rule"] in ("triple_incidence_1",
                                 "intersection_lines_opposing",
                                 "pasch_4", "by_contra"), s
            assert "engine-only" in s["lean_note"], s
            continue
        assert s["rule"] in lines[ln - 1], s
        linked.append(ln)
    # exactly the five euclid_apply lines are linked, in order
    applies = {i + 1 for i, l in enumerate(lines) if "euclid_apply" in l}
    assert linked == sorted(linked) and set(linked) == applies, linked


def test_prop21_step_facts_hold():
    xy = {k: tuple(v) for k, v in TR21["placement"].items() if v}
    bad = [(f, ok) for f, ok in tr.metric_facts_hold(TR21["steps"], xy)
           if not ok]
    assert not bad, bad
    assert all(ok for _, ok in tr.verify21(xy))
    # the last step's info bar states the conclusion's why (both
    # conjuncts: the length chain and the exterior-angle chain)
    last = TR21["steps"][-1]
    assert last["rule"] == "proposition_16"
    assert "|b-d| + |d-c| < |b-d| + |d-e| + |e-c|" \
        in last["placement_decision"]
    assert "= |b-a| + |a-c|" in last["placement_decision"]
    assert "angle b:d:c > angle d:e:c" in last["placement_decision"]


def _xy21():
    return {k: tuple(v) for k, v in TR21["placement"].items() if v}


def test_bad_case_prop21_d_outside_beyond_bc_fails():
    x = _xy21()
    assert all(ok for _, ok in tr.verify21(x))
    # d reflected across BC (outside the triangle): the same-side
    # hypothesis check fails
    dx, dy = x["d"]
    x["d"] = (dx, -dy)
    fails = [d for d, ok in tr.verify21(x) if not ok]
    assert any("side of BC" in d for d in fails), fails


def test_bad_case_prop21_d_on_ac_fails():
    x = _xy21()
    # d on side AC (the midpoint): not strictly inside — the
    # b.sameSide d AC check fails (cross product zero)
    ax, ay = x["a"]
    cx, cy = x["c"]
    x["d"] = ((ax + cx) / 2, (ay + cy) / 2)
    fails = [d for d, ok in tr.verify21(x) if not ok]
    assert any("side of AC" in d for d in fails), fails
