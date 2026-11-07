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


# -- (i) Lean alignment -----------------------------------------------------

def test_every_step_has_lean_line():
    for t in (TR1, TR2, TR3):
        for s in t["steps"]:
            assert s["lean"]["file"].startswith("Book/Prop")
            assert isinstance(s["lean"]["line"], int)
            assert 1 <= s["lean"]["line"] <= len(t["lean_text"])


def test_lean_line_really_contains_the_rule():
    for t in (TR1, TR2, TR3):
        path = os.path.join(LEAN_DIR, t["lean_file"])
        with open(path) as fh:
            lines = fh.read().splitlines()
        for s in t["steps"]:
            line = lines[s["lean"]["line"] - 1]
            assert "euclid_apply" in line, (s, line)
            assert s["rule"] in line, (s, line)


def test_lean_lines_are_distinct_and_in_order():
    for t in (TR1, TR2, TR3):
        lines = [s["lean"]["line"] for s in t["steps"]]
        assert lines == sorted(lines)
        assert len(set(lines)) == len(lines)


def test_traces_on_disk_match_rebuild():
    for prop, t in ((1, TR1), (2, TR2), (3, TR3)):
        rebuilt = tr.build_trace(prop)
        assert rebuilt["placement"] == t["placement"]
        assert [s["rule"] for s in rebuilt["steps"]] == \
            [s["rule"] for s in t["steps"]]


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
    for t in (TR1, TR2, TR3):
        assert all(c["ok"] for c in t["verification"]), t["verification"]


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
