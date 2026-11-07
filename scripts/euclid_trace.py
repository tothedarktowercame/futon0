#!/usr/bin/env python3
"""Trace generator for the E-euclid-tutor viewer.

Runs the existing euclid_engine replays of Book I.1-I.5 (imported
read-only) and writes one JSON trace per proposition under
holes/excursions/E-euclid-tutor-data/viewer/traces/prop0N.json:

  objects : drawn objects (kind point/line/circle, name, defining step)
  steps   : rule, args, bindings, facts added, Lean file:line of the
            matching euclid_apply line in Book/PropNN.lean
  placement : one numeric placement. Free points at fixed sample
            coordinates, each construction computed numerically.
            Where a construction has two candidate points, the
            candidate is chosen by evaluating the engine's own
            between/opposing_sides facts for that step; the deciding
            fact (or its absence) is recorded. The engine never reads
            coordinates.

Usage: python3 scripts/euclid_trace.py [--out DIR] [1,2,3]
"""

import json
import math
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import euclid_engine as eng  # read-only import; never edited here

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
LEAN_DIR = "/home/joe/code/LeanEuclid"
TRACES_DIR = os.path.join(ROOT, "holes", "excursions", "E-euclid-tutor-data",
                          "viewer", "traces")

EPS = 1e-9


# ---------------------------------------------------------------------------
# engine step -> euclid_apply line in Book/PropNN.lean
# ---------------------------------------------------------------------------

def lean_apply_lines(filename):
    """[(line_no, text)] of every euclid_apply line, in order."""
    out = []
    with open(filename) as fh:
        for i, line in enumerate(fh, start=1):
            if "euclid_apply" in line:
                out.append((i, line))
    return out


def lean_assert_lines(filename):
    """[(line_no, text)] of every euclid_assert line, in order. The engine's
    assert_point_eq_* steps correspond to these Lean lines, not to
    euclid_apply lines (Prop04.lean asserts b' = e and the apex angle)."""
    out = []
    with open(filename) as fh:
        for i, line in enumerate(fh, start=1):
            if "euclid_assert" in line:
                out.append((i, line))
    return out


def lean_lines_for(prop, rule_names):
    """Sequentially match each engine rule to the first unconsumed
    euclid_apply line that names it. assert_point_eq_* rules match the
    euclid_assert lines instead (point equality: the line without an
    angle sign; angle equality: the line with one). Returns [(file, line)].
    """
    fname = os.path.join(LEAN_DIR, "Book", "Prop%02d.lean" % prop)
    lines = lean_apply_lines(fname)
    asserts = lean_assert_lines(fname)
    rel = os.path.join("Book", "Prop%02d.lean" % prop)
    res, k, ka = [], 0, 0
    for rule in rule_names:
        if rule == "assert_point_eq_on_ray":
            while ka < len(asserts) and "∠" in asserts[ka][1]:
                ka += 1
            if ka >= len(asserts):
                raise AssertionError(
                    "no euclid_assert point line found in %s" % fname)
            res.append((rel, asserts[ka][0]))
            ka += 1
            continue
        if rule == "assert_point_eq_angle":
            while ka < len(asserts) and "∠" not in asserts[ka][1]:
                ka += 1
            if ka >= len(asserts):
                raise AssertionError(
                    "no euclid_assert angle line found in %s" % fname)
            res.append((rel, asserts[ka][0]))
            ka += 1
            continue
        while k < len(lines) and rule not in lines[k][1]:
            k += 1
        if k >= len(lines):
            raise AssertionError(
                "no euclid_apply line found for %r in %s" % (rule, fname))
        res.append((rel, lines[k][0]))
        k += 1
    return res


# ---------------------------------------------------------------------------
# numeric geometry (plain floats; the engine never sees these numbers)
# ---------------------------------------------------------------------------

def _sub(p, q):
    return (p[0] - q[0], p[1] - q[1])


def _add(p, q):
    return (p[0] + q[0], p[1] + q[1])


def _mul(p, t):
    return (p[0] * t, p[1] * t)


def _norm(p):
    return math.hypot(p[0], p[1])


def _unit(p):
    n = _norm(p)
    return (p[0] / n, p[1] / n)


def circle_circle(c1, r1, c2, r2):
    """Both intersections of two circles (degenerate -> error)."""
    d = _sub(c2, c1)
    dd = _norm(d)
    if dd < EPS:
        raise AssertionError("concentric circles")
    a = (r1 * r1 - r2 * r2 + dd * dd) / (2 * dd)
    h2 = r1 * r1 - a * a
    if h2 < -EPS:
        raise AssertionError("circles do not meet")
    h = math.sqrt(max(h2, 0.0))
    u = _unit(d)
    perp = (-u[1], u[0])
    base = _add(c1, _mul(u, a))
    return [_add(base, _mul(perp, h)), _add(base, _mul(perp, -h))]


def circle_line(c, r, p1, p2):
    """Both intersections of a circle with the line through p1,p2."""
    d = _sub(p2, p1)
    dd = _norm(d)
    if dd < EPS:
        raise AssertionError("degenerate line")
    u = _unit(d)
    f = _sub(p1, c)
    a = u[0] * u[0] + u[1] * u[1]
    b = 2 * (f[0] * u[0] + f[1] * u[1])
    cc = f[0] * f[0] + f[1] * f[1] - r * r
    disc = b * b - 4 * a * cc
    if disc < -EPS:
        raise AssertionError("line misses circle")
    sq = math.sqrt(max(disc, 0.0))
    return [_add(p1, _mul(u, (-b + sq) / 2)), _add(p1, _mul(u, (-b - sq) / 2))]


def fact_holds(fact, xy):
    """Evaluate a diagram fact numerically for the candidate's placement.
    between a b c == b is between a and c (collinear tolerance EPS)."""
    kind = fact[0]
    if kind == "between":
        a, b, c = (xy[n] for n in fact[1:4])
        ab, bc = _norm(_sub(b, a)), _norm(_sub(c, b))
        ac = _norm(_sub(c, a))
        return abs(ab + bc - ac) < EPS
    if kind == "opposing_sides":
        p, q, on1, on2 = (xy[n] for n in fact[1:5])
        s = lambda x: ((x[0] - on1[0]) * (on2[1] - on1[1])
                       - (x[1] - on1[1]) * (on2[0] - on1[0]))
        return s(p) * s(q) < 0
    return None  # not a geometric discriminator (on_circle/on_line hold
                 # by construction; metric facts are checked in verify())


def choose(cands, facts, obj, name):
    """Pick the candidate satisfying this step's between/opposing_sides
    facts about the new point; record which fact decided it."""
    disc = [f for f in facts
            if f[0] in ("between", "opposing_sides") and obj in f[1:4]]
    for cand in cands:
        for f in disc:
            trial = dict(name)
            trial[obj] = cand
            if fact_holds(f, trial):
                return cand, "fact %r decided" % (f,)
    return cands[0], ("no distinguishing fact recorded (arbitrary pick; "
                      "both candidates satisfy every fact of the step)")


# ---------------------------------------------------------------------------
# placements per proposition
# ---------------------------------------------------------------------------

# free sample points (fixed; nothing reads the engine's coordinate-free
# state, and the engine never reads these). I.2: a is kept well away from
# the |bc|-circle around it (|ba| ≈ 1.3 vs |bc| = 1) so the diagram does
# not look degenerate (review, claude-9).
FREE = {
    1: {"a": (0.0, 0.0), "b": (1.0, 0.0)},
    2: {"a": (-0.5, 1.2), "b": (0.0, 0.0), "c": (1.0, 0.0)},
    3: {"a": (0.0, 0.0), "b": (5.0, 0.0), "c0": (0.0, 1.0), "c1": (2.0, 1.0)},
    # I.4: the second triangle is a rigid copy (rotation 25 degrees) of the
    # first, so the hypothesis metric facts hold to float precision.
    4: {"a": (0.0, 0.0), "b": (2.0, 0.0), "c": (0.6, 1.2),
        "d": (3.4, 0.2)},
    # I.5: isosceles (|ab| = |ac|), d and e produced past b and c by the
    # same factor 1.4 so |bd| = |ce|.
    5: {"a": (0.0, 1.2), "b": (-0.7, 0.0), "c": (0.7, 0.0)},
}

PROP4_ROT = math.radians(25.0)
PROP5_EXTEND = 1.4


def hyps_and_steps(prop):
    if prop == 1:
        return eng._prop1()[0], list(eng.PROP1_STEPS_DATA)
    if prop == 2:
        return list(eng.PROP2_HYPS_DATA), list(eng.PROP2_STEPS_DATA)
    if prop == 4:
        return list(eng.PROP4_HYPS_DATA), list(eng.PROP4_STEPS_DATA)
    if prop == 5:
        return list(eng.PROP5_HYPS_DATA), list(eng.PROP5_STEPS_DATA)
    return list(eng.PROP3_HYPS_DATA), list(eng.PROP3_STEPS_DATA)


def derived_free_points(prop, xy):
    """Sample coordinates for hypothesis points that are constructed in the
    Book proof's setup but free in the engine's hypothesis data."""
    if prop == 4:
        # e, f: rigid copy of b, c under the rotation taking (a->d)
        cs, sn = math.cos(PROP4_ROT), math.sin(PROP4_ROT)
        rot = lambda p: (cs * p[0] - sn * p[1], sn * p[0] + cs * p[1])
        for src, name in (("b", "e"), ("c", "f")):
            v = _sub(xy[src], xy["a"])
            w = rot(v)
            xy[name] = _add(xy["d"], w)
    if prop == 5:
        # d, e: produced past b and c on AB, AC by the same factor
        for src, name in (("b", "d"), ("c", "e")):
            v = _sub(xy[src], xy["a"])
            xy[name] = _add(xy["a"], _mul(v, PROP5_EXTEND))



def objects_from_hyps(prop):
    """Hypothesis objects (def_step 0): the free points and the named
    hypothesis lines, as drawn in the traditional diagrams."""
    if prop == 1:
        return [{"name": "a", "kind": "point", "jsx": {"type": "point"},
                 "def_step": 0},
                {"name": "b", "kind": "point", "jsx": {"type": "point"},
                 "def_step": 0},
                {"name": "AB", "kind": "line", "jsx": {"type": "line",
                  "p": ["a", "b"]}, "def_step": 0, "hyp": True}]
    if prop == 2:
        return [{"name": n, "kind": "point", "jsx": {"type": "point"},
                 "def_step": 0} for n in ("a", "b", "c")] + [
                {"name": "BC", "kind": "line", "jsx": {"type": "line",
                  "p": ["b", "c"]}, "def_step": 0, "hyp": True}]
    if prop == 4:
        return [{"name": n, "kind": "point", "jsx": {"type": "point"},
                 "def_step": 0} for n in ("a", "b", "c", "d", "e", "f")] + [
                {"name": L, "kind": "line", "jsx": {"type": "line",
                  "p": list(pq)}, "def_step": 0, "hyp": True}
                for L, pq in (("AB", ("a", "b")), ("BC", ("b", "c")),
                              ("AC", ("a", "c")), ("DE", ("d", "e")),
                              ("EF", ("e", "f")), ("DF", ("d", "f")))]
    if prop == 5:
        return [{"name": n, "kind": "point", "jsx": {"type": "point"},
                 "def_step": 0} for n in ("a", "b", "c", "d", "e")] + [
                {"name": L, "kind": "line", "jsx": {"type": "line",
                  "p": list(pq)}, "def_step": 0, "hyp": True}
                for L, pq in (("AB", ("a", "b")), ("BC", ("b", "c")),
                              ("AC", ("a", "c")))]
    return [{"name": n, "kind": "point", "jsx": {"type": "point"},
             "def_step": 0} for n in ("a", "b", "c0", "c1")] + [
            {"name": "AB", "kind": "line", "jsx": {"type": "line",
              "p": ["a", "b"]}, "def_step": 0, "hyp": True},
            {"name": "C", "kind": "line", "jsx": {"type": "line",
              "p": ["c0", "c1"]}, "def_step": 0, "hyp": True}]


def place_step(prop, rule, args, bindings, added, xy, objects):
    """Numeric placement for one step: extends xy (name -> (x,y)) and
    appends introduced objects. Returns the decision note."""
    if rule == "line_from_points":
        (p, q), L = args, bindings["L"]
        xy[L] = None  # lines have no coordinates; drawn from points
        objects.append({"name": L, "kind": "line",
                        "jsx": {"type": "line", "p": [p, q]},
                        "def_step": None})
        return "line drawn through existing points"
    if rule == "circle_from_points":
        (a, b), C = args, bindings["C"]
        xy[C] = None
        objects.append({"name": C, "kind": "circle",
                        "jsx": {"type": "circle", "center": a, "through": b},
                        "def_step": None})
        return "circle with centre %s through %s (JSXGraph keeps it under \
dragging)" % (a, b)
    if rule == "intersection_circles":
        (A, B), c = args, bindings["c"]
        ca = next(o["jsx"]["center"] for o in objects if o["name"] == A)
        ra = _norm(_sub(xy[next(o["jsx"]["through"]
                                 for o in objects if o["name"] == A)],
                        xy[ca]))
        cb = next(o["jsx"]["center"] for o in objects if o["name"] == B)
        rb = _norm(_sub(xy[next(o["jsx"]["through"]
                                 for o in objects if o["name"] == B)],
                        xy[cb]))
        cands = circle_circle(xy[ca], ra, xy[cb], rb)
        pick, why = choose(cands, added, c, dict(xy))
        xy[c] = pick
        idx = 0 if abs(_sub(pick, cands[0])[0]) + abs(_sub(pick, cands[0])[1]) \
            < EPS else 1
        objects.append({"name": c, "kind": "point",
                        "jsx": {"type": "intersection", "parents": [A, B],
                                "idx": idx},
                        "def_step": None})
        return why
    if rule in ("intersection_circle_line_extending_points",
                "intersection_circle_line_between_points"):
        (C, L, p, q) = args
        name = bindings[next(k for k in ("g", "e") if k in bindings)]
        ca = next(o["jsx"]["center"] for o in objects if o["name"] == C)
        ra = _norm(_sub(xy[next(o["jsx"]["through"]
                                 for o in objects if o["name"] == C)],
                        xy[ca]))
        l1, l2 = next(o["jsx"]["p"] for o in objects
                      if o["name"] == L and o.get("jsx", {}).get("p"))
        cands = circle_line(xy[ca], ra, xy[l1], xy[l2])
        pick, why = choose(cands, added, name, dict(xy))
        xy[name] = pick
        idx = 0 if _norm(_sub(pick, cands[0])) < EPS else 1
        objects.append({"name": name, "kind": "point",
                        "jsx": {"type": "intersection", "parents": [C, L],
                                "idx": idx},
                        "def_step": None})
        return why
    if rule == "proposition_1":
        # equilateral: d is the intersection of circle(a,b) and circle(b,a);
        # both candidates satisfy the step's metric facts, so the pick is
        # arbitrary and recorded as such. Drawn with JSXGraph intersections
        # of two auxiliary circles, so dragging a/b keeps |ad|=|ab|=|bd|.
        (a, b, AB), d = args, bindings["d"]
        aux1, aux2 = CIRC_AUX[d]
        cands = circle_circle(xy[a], _norm(_sub(xy[b], xy[a])),
                              xy[b], _norm(_sub(xy[a], xy[b])))
        pick, why = choose(cands, added, d, dict(xy))
        xy[d] = pick
        idx = 0 if _norm(_sub(pick, cands[0])) < EPS else 1
        for (cname, ctr, thr) in ((aux1, a, b), (aux2, b, a)):
            objects.append({"name": cname, "kind": "circle",
                            "jsx": {"type": "circle", "center": ctr,
                                    "through": thr},
                            "def_step": None, "aux": True})
        objects.append({"name": d, "kind": "point",
                        "jsx": {"type": "intersection",
                                "parents": [aux1, aux2], "idx": idx},
                        "def_step": None})
        return why
    if rule == "proposition_2'":
        # |a-d| = |b-c|: a free rotation. Drawn as a glider on a circle
        # with centre a and DYNAMIC radius |b-c| (a radius function, so
        # dragging b/c keeps the compass semantics).
        (a, b, c, BC), d = args, bindings["d"]
        comp = "compass_" + d
        objects.append({"name": comp, "kind": "circle",
                        "jsx": {"type": "circle", "center": a,
                                "radius_from": [b, c]},
                        "def_step": None, "aux": True})
        ang = math.pi / 2  # sample direction; a free parameter of the
        # existential step, not decided by any fact
        r = _norm(_sub(xy[c], xy[b]))
        xy[d] = _add(xy[a], (r * math.cos(ang), r * math.sin(ang)))
        objects.append({"name": d, "kind": "point",
                        "jsx": {"type": "glider", "on": comp},
                        "def_step": None})
        return ("free parameter of the existential step: sample direction "
                "chosen (no fact decides it); glider on a circle of dynamic "
                "radius |b-c| preserves |a-d| = |b-c| under dragging")
    if rule == "superposition":
        # rigid motion taking a->d and b->e (|ab| = |de| by hypothesis):
        # b' = T(b) = e exactly, c' = T(c). Drawn at the sample placement;
        # the coincidences b' = e and c' = f are what steps 2-3 assert.
        (a, b, c, d, e, f, AB, BC, AC, DE), B = args, bindings
        bp, cp = B["b'"], B["c'"]
        ab = _sub(xy[b], xy[a])
        de = _sub(xy[e], xy[d])
        ang = math.atan2(de[1], de[0]) - math.atan2(ab[1], ab[0])
        cs, sn = math.cos(ang), math.sin(ang)
        rot = lambda p: (cs * p[0] - sn * p[1], sn * p[0] + cs * p[1])
        xy[bp] = _add(xy[d], rot(ab))
        xy[cp] = _add(xy[d], rot(_sub(xy[c], xy[a])))
        assert _norm(_sub(xy[bp], xy[e])) < EPS, "superposition: b' must be e"
        objects.append({"name": bp, "kind": "point",
                        "jsx": {"type": "point"}, "def_step": None})
        objects.append({"name": cp, "kind": "point",
                        "jsx": {"type": "point"}, "def_step": None})
        objects.append({"name": bindings["BC'"], "kind": "line",
                        "jsx": {"type": "line", "p": [bp, cp]},
                        "def_step": None})
        objects.append({"name": bindings["AC'"], "kind": "line",
                        "jsx": {"type": "line", "p": [d, cp]},
                        "def_step": None})
        return ("rigid motion mapping a->d, b->e: b' lands exactly on e "
                "(asserted in step 2), c' is the image of c; the primed "
                "triangle is triangle abc carried onto d e f")
    if rule == "point_between_points_shorter_than":
        # f on line L with between b f d and |bf| < |ce| (rule args
        # (L, b, c, u, v) with u,v naming the |u-v| bound); any shorter
        # sample works — half the bound is chosen.
        (L, b, c, u, v), name = args, bindings["a"]
        bound = _norm(_sub(xy[v], xy[u]))
        seg = _sub(xy[c], xy[b])
        assert bound < 2 * _norm(seg), "sample needs |uv|/2 < |bc|"
        xy[name] = _add(xy[b], _mul(_unit(seg), bound / 2))
        objects.append({"name": name, "kind": "point",
                        "jsx": {"type": "point"}, "def_step": None})
        return ("sample: |b-%s| = |%s-%s|/2 (any length shorter than the "
                "bound satisfies the step; the engine places no coordinate)"
                % (name, u, v))
    if rule == "proposition_3":
        # q on segment a-b with |a-q| = |c0-c1| (rule args
        # (a, b, c0, c1, AB, C)); |a-b| > |c0-c1| is an entailment premise.
        (a, b, c0, c1, AB, C), name = args, bindings["q"]
        r = _norm(_sub(xy[c1], xy[c0]))
        ab = _norm(_sub(xy[b], xy[a]))
        assert r < ab, "proposition_3 premise |a-b| > |c0-c1| must hold"
        xy[name] = _add(xy[a], _mul(_unit(_sub(xy[b], xy[a])), r))
        objects.append({"name": name, "kind": "point",
                        "jsx": {"type": "point"}, "def_step": None})
        return ("cut |%s-%s| off segment %s-%s from %s; sample direction "
                "along the segment" % (c0, c1, a, b, a))
    if rule in ("assert_point_eq_on_ray", "assert_point_eq_angle",
                "proposition_4", "sum_angles_onlyif"):
        # forward-closure / cited steps: no new objects, no coordinates
        return ("forward-closure step (no new objects): %s only adds facts "
                "to the engine state" % rule)
    raise AssertionError("no numeric placement for rule %r" % rule)


# auxiliary circle names for the proposition_1 sub-construction inside I.2
CIRC_AUX = {"d": ("aux_da", "aux_db")}


def _ang(xy, p, q, r):
    """angle p-q-r at vertex q, in radians."""
    u = _unit(_sub(xy[p], xy[q]))
    v = _unit(_sub(xy[r], xy[q]))
    cos = max(-1.0, min(1.0, u[0] * v[0] + u[1] * v[1]))
    return math.acos(cos)


def verify4(xy):
    ok = abs(_norm(_sub(xy["b"], xy["c"]))
             - _norm(_sub(xy["e"], xy["f"]))) < EPS
    checks = [("I.4: |b-c| = |e-f|", ok)]
    # superposition coincidences: b' = e exactly, c' = f after the asserts
    checks.append(("I.4: b' coincides with e",
                   _norm(_sub(xy["b'"], xy["e"])) < EPS))
    checks.append(("I.4: c' coincides with f (after the two asserts)",
                   _norm(_sub(xy["c'"], xy["f"])) < EPS))
    for (p, q, r, s, t, u_, desc) in (
            ("a", "b", "c", "d", "e", "f", "I.4: ∠abc = ∠def"),
            ("a", "c", "b", "d", "f", "e", "I.4: ∠acb = ∠dfe")):
        checks.append((desc, abs(_ang(xy, p, q, r) - _ang(xy, s, t, u_))
                       < EPS))
    return checks


def verify5(xy):
    checks = []
    for (p, q, r, s, t, u_, desc) in (
            ("a", "b", "c", "a", "c", "b", "I.5: base angles ∠abc = ∠acb"),
            ("c", "b", "d", "b", "c", "e",
             "I.5: exterior ∠cbd = ∠bce")):
        checks.append((desc, abs(_ang(xy, p, q, r) - _ang(xy, s, t, u_))
                       < EPS))
    return checks


def metric_facts_hold(trace_steps, xy):
    """Review addition (claude-9): check every between / eq_len / eq_ang /
    lt_len fact the steps add against the placement. verify5 checks only
    the conclusion, which holds for any symmetric placement, so swapping
    f and g (or d and f, or b and c) in prop05.json went unnoticed.
    Returns a list of (fact, ok); facts naming unplaced objects are skipped."""
    out = []
    dist = lambda a, b: _norm(_sub(xy[a], xy[b]))
    for s in trace_steps:
        for f in s.get("facts_added", []):
            names = [x for x in f[1:] if isinstance(x, str)]
            kind = f[0]
            if kind not in ("between", "eq_len", "eq_ang", "lt_len"):
                continue
            if kind == "between":
                if not all(n in xy and xy[n] for n in f[1:4]):
                    continue
                ok = fact_holds(f, xy)
            else:
                if not all(n in xy and xy[n] for n in names):
                    continue
                if kind == "eq_len":
                    ok = abs(dist(f[1], f[2]) - dist(f[3], f[4])) < EPS
                elif kind == "lt_len":
                    ok = dist(f[1], f[2]) < dist(f[3], f[4]) - EPS
                else:
                    ok = abs(_ang(xy, *f[1:4]) - _ang(xy, *f[4:7])) < EPS
            out.append((tuple(f), bool(ok)))
    return out


def verify(prop, xy):
    """Check the placement against the proposition's metric facts to
    EPS. Returns a list of (description, ok)."""
    d = lambda p, q: _norm(_sub(xy[p], xy[q]))
    if prop == 1:
        return [("I.1: |c-a| = |a-b|", abs(d("c", "a") - d("a", "b")) < EPS),
                ("I.1: |c-b| = |a-b|", abs(d("c", "b") - d("a", "b")) < EPS)]
    if prop == 2:
        return [("I.2: |a-l| = |b-c|", abs(d("a", "l") - d("b", "c")) < EPS)]
    if prop == 4:
        return verify4(xy)
    if prop == 5:
        return verify5(xy)
    a, e, b = xy["a"], xy["e"], xy["b"]
    checks = [("I.3: |a-e| = |c0-c1|",
               abs(d("a", "e") - d("c0", "c1")) < EPS)]
    ab = _norm(_sub(b, a))
    ae = _norm(_sub(e, a))
    eb = _norm(_sub(b, e))
    checks.append(("I.3: between a e b", abs(ae + eb - ab) < EPS))
    return checks


def build_trace(prop):
    hyps, steps = hyps_and_steps(prop)
    # engine replay (premises checked, facts collected); props 4-5 have
    # metric hypotheses (eq_len / eq_ang) that must enter the linear store
    if prop in (4, 5):
        s = eng._metric_state(hyps)
    else:
        s = eng.State()
        for f, w in hyps:
            s.assume(f, w)
    if prop == 3:
        s.la.add({eng.len_atom("a", "b"): 1,
                  eng.len_atom("c0", "c1"): -1}, 0, ">")
    eng.close(s)
    reports = [eng.apply_step(s, i, r, a, b)
               for i, (r, a, b) in enumerate(steps, 1)]

    lean = lean_lines_for(prop, [r for r, _, _ in steps])

    objects = objects_from_hyps(prop)
    xy = {k: v for k, v in FREE[prop].items()}
    derived_free_points(prop, xy)
    trace_steps = []
    for i, ((rule, args, bindings), rep, (lf, ll)) in enumerate(
            zip(steps, reports, lean), 1):
        why = place_step(prop, rule, args, bindings, rep["added"], xy,
                         objects)
        for o in objects:
            if o["def_step"] is None:
                o["def_step"] = i
        entry = {
            "n": i, "rule": rule, "args": list(args),
            "bindings": bindings,
            "facts_added": [list(map(str, f)) for f in rep["added"]],
            "premises": [list(map(str, f)) for f in rep["premises"]],
            "lean": {"file": lf, "line": ll},
            "placement_decision": why,
        }
        if rule == "superposition":
            # viewer hint: both triangles highlighted as a colour pair and
            # the correspondence shown in the fact panel
            entry["superposition"] = {
                "triangles": [["a", "b", "c"], ["d", "e", "f"]],
                "correspondence": [["a", "d"], ["b", "e"], ["c", "f"]],
            }
        trace_steps.append(entry)

    placement = {k: (list(v) if v is not None else None)
                 for k, v in xy.items()}
    checks = verify(prop, xy)
    assert all(ok for _, ok in checks), checks

    lean_file = os.path.join(LEAN_DIR, "Book", "Prop%02d.lean" % prop)
    with open(lean_file) as fh:
        lean_text = fh.read().splitlines()

    return {
        "prop": prop,
        "title": "Elements I.%d (LeanEuclid Book/Prop%02d.lean)" % (prop,
                                                                    prop),
        "objects": objects,
        "steps": trace_steps,
        "placement": placement,
        "verification": [{"check": c, "ok": ok} for c, ok in checks],
        "lean_file": "Book/Prop%02d.lean" % prop,
        "lean_text": lean_text,
    }


def main():
    out = TRACES_DIR
    args = [a for a in sys.argv[1:] if not a.startswith("--")]
    if "--out" in sys.argv:
        out = sys.argv[sys.argv.index("--out") + 1]
    props = [int(c) for c in (args[0] if args else "1,2,3").split(",")]
    os.makedirs(out, exist_ok=True)
    for p in props:
        t = build_trace(p)
        path = os.path.join(out, "prop%02d.json" % p)
        with open(path, "w") as fh:
            json.dump(t, fh, indent=1)
        print("wrote %s (%d steps, %d objects)" %
              (path, len(t["steps"]), len(t["objects"])))


if __name__ == "__main__":
    main()
