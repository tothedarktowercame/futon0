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


def _norm_lean(text):
    """Inside of an euclid_apply parenthetical, whitespace-collapsed and
    with the (c─b) length decoration reduced to its point names, so
    'point_between_points_shorter_than AB c a (c─b)' compares equal to
    the engine args (AB, c, a, c, b)."""
    inner = text.split("euclid_apply", 1)[1]
    inner = inner.split(")", 1)[0].lstrip("(")
    for ch in ("(", ")", "─"):
        inner = inner.replace(ch, " ")
    return " ".join(inner.split())


# Book I.11: the engine renames Lean's points (a b c d e f -> u v w g e f)
# to dodge base-name captures in apply_step (see PROP11_STEPS_DATA). The
# viewer shows Lean's names, so the trace is built in engine names and
# remapped at the end; the map is stated in the JSON.
PROP11_NAME_MAP = {"u": "a", "v": "b", "w": "c", "g": "d"}


def lean_lines_for_named(prop, steps, name_map):
    """Signature matching for props whose Lean proof does not name the
    engine's steps one-to-one (I.10's engine proof has bridge and
    by_contra steps Lean does not cite; I.11 renames points). Each step is
    matched to the first unconsumed euclid_apply line whose normalized
    argument list equals the step's (mapped) rule + args. Steps with no
    Lean line get None — the trace says so instead of inventing a line.
    euclid_assert lines are not used here (I.10/I.11 assert nothing)."""
    fname = os.path.join(LEAN_DIR, "Book", "Prop%02d.lean" % prop)
    lines = lean_apply_lines(fname)
    texts = [_norm_lean(t) for _, t in lines]
    used = [False] * len(lines)
    rel = os.path.join("Book", "Prop%02d.lean" % prop)
    res = []
    for st in steps:
        if st[0] == "by_contra":
            res.append(None)
            continue
        args = [name_map.get(str(x), str(x)) for x in st[1]]
        sig = " ".join([st[0]] + args)
        hit = next((k for k, t in enumerate(texts)
                    if not used[k] and (t == sig or t.startswith(sig + " "))),
                   None)
        if hit is None:
            res.append(None)
        else:
            used[hit] = True
            res.append((rel, lines[hit][0]))
    # second pass (review, claude-9): a step whose rule Lean cites with
    # different arguments (I.11's perpendicular_if: engine a b c f, Lean
    # d e c f) takes the unused Lean line of the same rule, if it lies
    # between its neighbours' lines; marked as a variant citation
    for i, st in enumerate(steps):
        if res[i] is not None or st[0] == "by_contra":
            continue
        lo = max([r[1] for r in res[:i] if r and r[1]] or [0])
        hi = min([r[1] for r in res[i + 1:] if r and r[1]] or [10 ** 9])
        hit = next((k for k, t in enumerate(texts)
                    if not used[k] and t.split(" ", 1)[0] == st[0]
                    and lo < lines[k][0] < hi), None)
        if hit is not None:
            used[hit] = True
            res[i] = (rel, lines[hit][0], "variant")
    return res


def _remap_names(obj, name_map):
    """Deep-rename exact tokens under a name map (dicts' keys and values,
    lists' items). Rule names and jsx recipes never collide with the
    single-letter point names being mapped."""
    if isinstance(obj, dict):
        return {name_map.get(k, k): _remap_names(v, name_map)
                for k, v in obj.items()}
    if isinstance(obj, list):
        return [_remap_names(x, name_map) for x in obj]
    if isinstance(obj, str):
        return name_map.get(obj, obj)
    return obj


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
    # same factor 2.0 so |bd| = |ce| — long enough that f (half of |ce|
    # past b) separates from both b and d on the fitted board (review,
    # claude-9: f sat almost on b and d was at the board edge).
    5: {"a": (0.0, 1.2), "b": (-0.7, 0.0), "c": (0.7, 0.0)},
    # I.10: symmetric base so the bisector/CD is the y-axis and d lands
    # exactly on the midpoint of a-b (verify10 checks |a-d| = |d-b| and
    # between a d b).
    10: {"a": (-1.0, 0.0), "b": (1.0, 0.0)},
    # I.11: u-v with w strictly between; g/e are placed by the rules at
    # |w-g| = |w-e| = |w-v|/2, f is the equilateral apex on g-e (verify11
    # checks angle u-w-f is a right angle).
    11: {"u": (-1.0, 0.0), "v": (1.0, 0.0), "w": (0.0, 0.0)},
    # I.12: a-b the line (long, so the circle's intersections e, g sit
    # strictly inside it), c off it. d is c reflected across AB (see
    # place_step), so the circle centre c through d meets AB at e, g with
    # midpoint h the foot of c on AB: angle e-h-c is exactly right.
    12: {"a": (-2.2, 0.0), "b": (2.2, 0.0), "c": (0.4, 0.9)},
    # I.13: b at the origin, CD horizontal with between d b c, a off CD in
    # the GENERAL position (angle c:b:a not right, so branch 2 of Lean's
    # by_cases is the drawn case) and on c's side of BE (the inner branch
    # that is drawn); e is placed straight up over b by proposition_11'.
    13: {"a": (1.2, 0.9), "b": (0.0, 0.0), "c": (2.0, 0.0),
         "d": (-1.5, 0.0)},
}

PROP4_ROT = math.radians(25.0)
PROP5_EXTEND = 2.0


def hyps_and_steps(prop):
    if prop == 1:
        return eng._prop1()[0], list(eng.PROP1_STEPS_DATA)
    if prop == 2:
        return list(eng.PROP2_HYPS_DATA), list(eng.PROP2_STEPS_DATA)
    if prop == 4:
        return list(eng.PROP4_HYPS_DATA), list(eng.PROP4_STEPS_DATA)
    if prop == 5:
        return list(eng.PROP5_HYPS_DATA), list(eng.PROP5_STEPS_DATA)
    if prop == 10:
        return list(eng.PROP10_HYPS_DATA), list(eng.PROP10_STEPS_DATA)
    if prop == 11:
        return list(eng.PROP11_HYPS_DATA), list(eng.PROP11_STEPS_DATA)
    if prop == 12:
        return list(eng.PROP12_HYPS_DATA), list(eng.PROP12_STEPS_DATA)
    if prop == 13:
        return list(eng.PROP13_HYPS_DATA), list(eng.PROP13_STEPS_DATA)
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
    if prop == 10:
        # c (the equilateral apex) is CONSTRUCTED by step 1, not a
        # hypothesis; the SMT-sourced non-collinearity is an assumption
        # about that later point, so the initial picture is just a-b-AB
        return [{"name": n, "kind": "point", "jsx": {"type": "point"},
                 "def_step": 0} for n in ("a", "b")] + [
                {"name": "AB", "kind": "line", "jsx": {"type": "line",
                  "p": ["a", "b"]}, "def_step": 0, "hyp": True}]
    if prop == 11:
        return [{"name": n, "kind": "point", "jsx": {"type": "point"},
                 "def_step": 0} for n in ("u", "v", "w")] + [
                {"name": "AB", "kind": "line", "jsx": {"type": "line",
                  "p": ["u", "v"]}, "def_step": 0, "hyp": True}]
    if prop == 12:
        # c is a hypothesis here (off AB by hypothesis); d is constructed
        # by step 1 (exists_point_opposite)
        return [{"name": n, "kind": "point", "jsx": {"type": "point"},
                 "def_step": 0} for n in ("a", "b", "c")] + [
                {"name": "AB", "kind": "line", "jsx": {"type": "line",
                  "p": ["a", "b"]}, "def_step": 0, "hyp": True}]
    if prop == 13:
        # both lines are hypotheses (dpl facts); between d b c puts b on
        # CD, and a is on AB and off CD
        return [{"name": n, "kind": "point", "jsx": {"type": "point"},
                 "def_step": 0} for n in ("a", "b", "c", "d")] + [
                {"name": L, "kind": "line", "jsx": {"type": "line",
                  "p": list(pq)}, "def_step": 0, "hyp": True}
                for L, pq in (("AB", ("a", "b")), ("CD", ("c", "d")))]
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
        # equilateral: the new point is the intersection of circle(a,b) and
        # circle(b,a); both candidates satisfy the step's metric facts, so
        # the pick is arbitrary and recorded as such. Drawn with JSXGraph
        # intersections of two auxiliary circles, so dragging the base
        # points keeps the triangle equilateral.
        (a, b, AB), d = args, bindings["d"]
        aux1, aux2 = CIRC_AUX.get(d) or ("aux_%s_a" % d, "aux_%s_b" % d)
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
        assert bound / 2 < _norm(seg), "sample needs |uv|/2 < |bc|"
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
    if rule == "proposition_9'":
        # angle bisector at c of the triangle a-b: the new point sits on
        # the bisector ray. Between the vertex and the far side at 0.6 of
        # the distance to the base midpoint (any interior point of the ray
        # satisfies the step's same_side / eq_ang facts; no fact fixes the
        # distance — it is a free parameter of the existential step).
        (c, a, b, AC, BC), f = args, bindings["f"]
        mid = _mul(_add(xy[a], xy[b]), 0.5)
        vec = _sub(mid, xy[c])
        pick = _add(xy[c], _mul(_unit(vec), 0.6 * _norm(vec)))
        xy[f] = pick
        objects.append({"name": f, "kind": "point",
                        "jsx": {"type": "point"}, "def_step": None})
        return ("free parameter of the existential step: sample distance " 
                "0.6 |c-mid(ab)| along the bisector ray (no fact decides "
                "the distance)")
    if rule == "intersection_lines":
        # (L1, L2) -> point on both: numeric line-line intersection of the
        # two drawn lines (the engine asserts they meet; step 13 of I.10
        # derives intersects_ll first)
        (L1, L2), name = args, bindings["a"]
        p1, p2 = _line_objects(objects, L1)
        q1, q2 = _line_objects(objects, L2)
        xy[name] = _line_line(xy[p1], xy[p2], xy[q1], xy[q2])
        objects.append({"name": name, "kind": "point",
                        "jsx": {"type": "point"}, "def_step": None})
        return "intersection of the drawn lines %s and %s" % (L1, L2)
    if rule == "exists_point_opposite":
        # (L, c) -> point on the other side of L from c. The rule fixes
        # only the side; the distance is a free parameter of the
        # existential step. Reflected across L at c's own distance — the
        # canonical symmetric choice, so I.12's circle (centre c through
        # d) is bisected by L exactly at the foot of c.
        (L, c), name = args, bindings["a"]
        p1, p2 = _line_objects(objects, L)
        xy[name] = _reflect(xy[c], xy[p1], xy[p2])
        objects.append({"name": name, "kind": "point",
                        "jsx": {"type": "point"}, "def_step": None})
        return ("free parameter of the existential step: %s is c "
                "reflected across %s (the rule fixes only the side, not "
                "the distance; the mirror point is the canonical choice)"
                % (name, L))
    if rule == "intersections_circle_line":
        # (C, L) -> the two points where circle C meets line L. The step's
        # facts are symmetric in the two names (e/g: both on the circle
        # and the line, e != g), so which candidate gets which name is a
        # free choice, recorded as I.1 records its pick. Drawn with
        # JSXGraph circle-line intersections, so dragging keeps them on
        # both.
        (C, L), (e, g) = args, (bindings["a"], bindings["b"])
        ca = next(o["jsx"]["center"] for o in objects if o["name"] == C)
        ra = _norm(_sub(xy[next(o["jsx"]["through"]
                                 for o in objects if o["name"] == C)],
                        xy[ca]))
        l1, l2 = _line_objects(objects, L)
        cands = circle_line(xy[ca], ra, xy[l1], xy[l2])
        xy[e], xy[g] = cands[0], cands[1]
        for name, idx in ((e, 0), (g, 1)):
            objects.append({"name": name, "kind": "point",
                            "jsx": {"type": "intersection",
                                    "parents": [C, L], "idx": idx},
                            "def_step": None})
        return ("no distinguishing fact recorded (arbitrary pick; the "
                "step's facts are symmetric in %s and %s — both lie on "
                "the circle and the line): %s = candidate 0, %s = "
                "candidate 1" % (e, g, e, g))
    if rule == "proposition_10":
        # bisect segment args[0]-args[1]: the new point is the midpoint.
        (p, q, _AB), name = args, bindings["d"]
        xy[name] = _mul(_add(xy[p], xy[q]), 0.5)
        objects.append({"name": name, "kind": "point",
                        "jsx": {"type": "point"}, "def_step": None})
        return ("midpoint of %s-%s (the proposition_10 construction "
                "fixed exactly)" % (p, q))
    if rule == "by_contra":
        # no drawing: refuted branches are not drawn, only noted
        branches, _concl = args, bindings
        parts = []
        for facts, bsteps in branches:
            rules = [s[0] for s in bsteps]
            parts.append("case split: assume %s — refuted by %s"
                         % (list(facts), rules or
                            ["contradiction in the store"]))
        return "; ".join(parts) + " (refuted branches are not drawn)"
    if rule == "by_cases":
        # no drawing for the split itself: the DRAWN case is branch 2 in
        # general position (see the entry's case note); the other branches
        # are checked by the engine on state copies and only noted
        return ("case split — the drawn diagram shows the general case; "
                "the other cases are checked by the engine (see the case "
                "note on each step), not drawn")
    if rule == "proposition_11'":
        # (a, b, c, x, AB) -> f same side of AB as x with angle a:c:f
        # right. The rule fixes the side and the right angle; the distance
        # |c-f| is a free parameter of the existential step (unit length
        # chosen, as the engine places no coordinate).
        (a, b, c, x, AB), name = args, bindings["f"]
        l1, l2 = _line_objects(objects, AB)
        dvec = _unit(_sub(xy[a], xy[c]))
        perp = (-dvec[1], dvec[0])
        side = lambda p: (_sub(p, xy[c])[0] * (xy[l2][1] - xy[l1][1])
                          - _sub(p, xy[c])[1] * (xy[l2][0] - xy[l1][0]))
        cand = _add(xy[c], perp)
        if side(cand) * side(xy[x]) < 0:
            cand = _sub(xy[c], perp)
        xy[name] = cand
        objects.append({"name": name, "kind": "point",
                        "jsx": {"type": "point"}, "def_step": None})
        return ("free parameter of the existential step: |%s-%s| = 1 on "
                "the perpendicular to %s at %s, on %s's side of %s (the "
                "rule fixes the side and the right angle, not the "
                "distance)" % (c, name, AB, c, x, AB))
    if rule in ("assert_point_eq_on_ray", "assert_point_eq_angle",
                "proposition_4", "sum_angles_onlyif",
                "between_symm_parts", "pasch_3_opposing", "pasch_4",
                "opposing_same_side_trans", "triple_incidence_2",
                "angle_pair_on_line",
                "proposition_8", "proposition_7", "perpendicular_if",
                "perpendicular_onlyif",
                "intersection_lines_opposing",
                "intersection_circle_line_1"):
        # forward-closure / cited steps: no new objects, no coordinates
        return ("forward-closure step (no new objects): %s only adds facts "
                "to the engine state" % rule)
    raise AssertionError("no numeric placement for rule %r" % rule)


# auxiliary circle names for the proposition_1 sub-construction inside I.2
CIRC_AUX = {"d": ("aux_da", "aux_db")}


def _reflect(p, p1, p2):
    """Mirror point p across the line through p1, p2 (numeric)."""
    d = _sub(p2, p1)
    dd = d[0] * d[0] + d[1] * d[1]
    t = ((p[0] - p1[0]) * d[0] + (p[1] - p1[1]) * d[1]) / dd
    foot = _add(p1, _mul(d, t))
    return (2 * foot[0] - p[0], 2 * foot[1] - p[1])


def _line_line(p1, p2, q1, q2):
    """Intersection of line p1-p2 with line q1-q2 (numeric)."""
    d1, d2 = _sub(p2, p1), _sub(q2, q1)
    det = d1[0] * d2[1] - d1[1] * d2[0]
    if abs(det) < EPS:
        raise AssertionError("parallel lines")
    t = ((q1[0] - p1[0]) * d2[1] - (q1[1] - p1[1]) * d2[0]) / det
    return _add(p1, _mul(d1, t))


def _line_objects(objects, L):
    o = next(o for o in objects
             if o["name"] == L and o["kind"] == "line"
             and o.get("jsx", {}).get("p"))
    return o["jsx"]["p"]


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


def verify10(xy):
    """I.10: d is the midpoint of a-b — between a d b and |a-d| = |d-b|."""
    return [
        ("I.10: between a d b",
         abs(_norm(_sub(xy["d"], xy["a"])) + _norm(_sub(xy["b"], xy["d"]))
             - _norm(_sub(xy["b"], xy["a"]))) < EPS),
        ("I.10: |a-d| = |d-b|",
         abs(_norm(_sub(xy["d"], xy["a"]))
             - _norm(_sub(xy["b"], xy["d"]))) < EPS),
        ("I.10: equilateral apex |a-c| = |a-b|",
         abs(_norm(_sub(xy["c"], xy["a"]))
             - _norm(_sub(xy["b"], xy["a"]))) < EPS),
    ]


def verify11(xy):
    """I.11 (labels in Lean's names; engine u v w g = Lean a b c d): the
    angle a-c-f is right, |c-d| = |c-e|, and f is off AB (its distance to
    the line a-b, not merely to c: review, claude-9)."""
    u, v, f = xy["u"], xy["v"], xy["f"]
    dvec = _sub(v, u)
    off = abs(dvec[0] * (f[1] - u[1]) - dvec[1] * (f[0] - u[0])) / _norm(dvec)
    return [
        ("I.11: |c-d| = |c-e|",
         abs(_norm(_sub(xy["g"], xy["w"]))
             - _norm(_sub(xy["e"], xy["w"]))) < EPS),
        ("I.11: angle a-c-f is right",
         abs(_ang(xy, "u", "w", "f") - math.pi / 2) < EPS),
        ("I.11: f off AB (distance to line a-b > 0)", off > EPS),
    ]


def verify12(xy):
    """I.12: h on AB with |h-e| = |h-g|, |c-e| = |c-g| (radii), the angle
    e-h-c right, and c, d on opposite sides of AB — all to EPS."""
    def off_line(p):
        dvec = _sub(xy["b"], xy["a"])
        return abs(dvec[0] * (p[1] - xy["a"][1])
                   - dvec[1] * (p[0] - xy["a"][0])) / _norm(dvec)
    dist = lambda p, q: _norm(_sub(xy[p], xy[q]))
    return [
        ("I.12: h on AB (distance to line a-b = 0)",
         off_line(xy["h"]) < EPS),
        ("I.12: |h-e| = |h-g|", abs(dist("h", "e") - dist("h", "g")) < EPS),
        ("I.12: |c-e| = |c-g|", abs(dist("c", "e") - dist("c", "g")) < EPS),
        # review fix (claude-9): e, g on circle EFG (centre c through d) and
        # on AB; without these, e and g pushed out symmetrically along AB
        # passed every check.
        ("I.12: e on circle EFG (|c-e| = |c-d|)",
         abs(dist("c", "e") - dist("c", "d")) < EPS),
        ("I.12: e, g on AB", off_line(xy["e"]) < EPS and off_line(xy["g"]) < EPS),
        ("I.12: e != g", dist("e", "g") > EPS),
        ("I.12: angle e-h-c is right",
         abs(_ang(xy, "e", "h", "c") - math.pi / 2) < EPS),
        ("I.12: c and d on opposite sides of AB",
         fact_holds(("opposing_sides", "c", "d", "a", "b"), xy)),
    ]


def verify13(xy):
    """I.13 (drawn case: branch 2, a on c's side of BE): between d b c, a
    off CD, e perpendicular to CD at b, e and a on the same side of CD
    (proposition_11' produces exactly this), the angle sum
    angle c:b:a + angle a:b:d = 180 degrees numerically, the drawn case
    really is general (the two angles unequal), and a is on c's side of
    BE (the inner branch that is drawn)."""
    dist = lambda p, q: _norm(_sub(xy[p], xy[q]))

    def side(p, l1, l2):
        dvec = _sub(xy[l2], xy[l1])
        return (dvec[0] * (xy[p][1] - xy[l1][1])
                - dvec[1] * (xy[p][0] - xy[l1][0]))

    ang_sum = _ang(xy, "c", "b", "a") + _ang(xy, "a", "b", "d")
    return [
        ("I.13: between d b c",
         abs(dist("d", "b") + dist("b", "c") - dist("d", "c")) < EPS),
        ("I.13: a off CD (distance to line c-d > 0)",
         abs(side("a", "c", "d")) / dist("c", "d") > EPS),
        ("I.13: angle d-b-e is right (e perpendicular to CD at b)",
         abs(_ang(xy, "d", "b", "e") - math.pi / 2) < EPS),
        ("I.13: e and a on the same side of CD",
         side("e", "c", "d") * side("a", "c", "d") > 0),
        ("I.13: angle c-b-a + angle a-b-d = 180 degrees",
         abs(ang_sum - math.pi) < EPS),
        ("I.13: general case drawn (angle c-b-a != angle a-b-d)",
         abs(_ang(xy, "c", "b", "a") - _ang(xy, "a", "b", "d")) > EPS),
        ("I.13: drawn case: a on c's side of BE",
         side("a", "b", "e") * side("c", "b", "e") > 0),
    ]


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
    if prop == 10:
        return verify10(xy)
    if prop == 11:
        return verify11(xy)
    if prop == 12:
        return verify12(xy)
    if prop == 13:
        return verify13(xy)
    a, e, b = xy["a"], xy["e"], xy["b"]
    checks = [("I.3: |a-e| = |c0-c1|",
               abs(d("a", "e") - d("c0", "c1")) < EPS)]
    ab = _norm(_sub(b, a))
    ae = _norm(_sub(e, a))
    eb = _norm(_sub(b, e))
    checks.append(("I.3: between a e b", abs(ae + eb - ab) < EPS))
    return checks


def _run_branch13(s, facts, steps):
    """Re-run one by_cases branch on a copy of the pre-split state to get
    per-step reports for the trace (the real by_cases runs branches on
    private copies and reports only witnesses)."""
    sub = s.copy()
    for f in facts:
        eng._assume_metric_or_fact(sub, f, "by_cases assumption")
    eng.close(sub)
    return eng.run_steps(sub, steps)


def build_trace13_rows(hyps, steps):
    """I.13 is a nested by_cases (see PROP13_STEPS_DATA). The trace flattens
    it: the outer split (Lean's by_cases line), branch 1's step (not drawn),
    branch 2's two construction steps (drawn — they ARE Lean's), the inner
    split (no Lean line: euclid_finish does it by SMT), then both inner
    branches' steps (drawn case: a on c's side of BE; the mirror case is
    checked by the engine, not drawn). Returns rows of
    (rule, args, bindings, report, loc, case_note)."""
    lean_file = os.path.join(LEAN_DIR, "Book", "Prop13.lean")
    with open(lean_file) as fh:
        lean_lines = fh.read().splitlines()
    rel = "Book/Prop13.lean"
    ln_by_cases = next(i + 1 for i, l in enumerate(lean_lines)
                       if l.strip().startswith("by_cases"))
    ln_p11 = next(i + 1 for i, l in enumerate(lean_lines)
                  if "proposition_11'" in l)
    ln_lfp = next(i + 1 for i, l in enumerate(lean_lines)
                  if "line_from_points" in l)

    s = eng.State()
    # the engine's own I.13 replay excludes straight_angle (that closure
    # rule IS I.13); the trace replays under the same exclusion
    s.exclude_rules = frozenset({"straight_angle [SMT-bridge]"})
    for f, w in hyps:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    s0 = s.copy()
    top = eng.run_steps(s, steps)[0]  # the single outer by_cases report

    _tag, branches, conclusions = steps[0]
    b1_assume, b1_steps = branches[0]
    b2_assume, b2_steps = branches[1]
    inner = b2_steps[2]  # ("by_cases", inner_branches, [PROP13_GOAL])

    def disp(cs):
        """Conclusions for display: the engine's _sum_rt_ang row is added
        to the state as the fact sum_rt_ang."""
        return [["sum_rt_ang" if c[0] == "_sum_rt_ang" else c[0],
                 *c[1:]] for c in cs]

    DRAWN2 = ("drawn case (branch 2: angle c:b:a != angle a:b:d) — the "
              "general case; the other cases are checked by the engine "
              "and only noted")
    NOT1 = ("branch 1 (angle c:b:a = angle a:b:d): checked by the engine, "
            "not drawn — Lean closes it with euclid_finish alone")
    DRAWNC = ("inner case drawn: a on c's side of BE")
    NOTD = ("inner branch (a on d's side of BE): the mirror image, checked "
            "by the engine, not drawn")

    rows = [("by_cases", [b[0] for b in branches],
             disp(conclusions), top, (rel, ln_by_cases),
             DRAWN2)]
    for (r, a, b), rep in zip(b1_steps, _run_branch13(s0, b1_assume,
                                                      b1_steps)):
        rows.append((r, a, b, rep, None, NOT1))
    sub2 = s0.copy()
    eng.close(sub2)
    reps2 = eng.run_steps(sub2, b2_steps[:2])
    rows.append(("proposition_11'", b2_steps[0][1], b2_steps[0][2],
                 reps2[0], (rel, ln_p11), DRAWN2))
    rows.append(("line_from_points", b2_steps[1][1], b2_steps[1][2],
                 reps2[1], (rel, ln_lfp), DRAWN2))
    rep_inner = eng._by_cases(sub2.copy(), 4, inner)
    rows.append(("by_cases", [b[0] for b in inner[1]],
                 disp(inner[2]), rep_inner, None, DRAWNC))
    for (assume, bsteps), note in zip(inner[1], (DRAWNC, NOTD)):
        for (r, a, b), rep in zip(bsteps, _run_branch13(sub2, assume,
                                                        bsteps)):
            rows.append((r, a, b, rep, None, note))
    return rows, s


def build_trace(prop):
    hyps, steps = hyps_and_steps(prop)
    if prop == 13:
        # nested by_cases: rows built by replaying each branch on a state
        # copy (see build_trace13_rows); the replay itself is the check
        rows, _s = build_trace13_rows(hyps, steps)
        name_map = {}
    else:
        # engine replay (premises checked, facts collected); props 4-5
        # have metric hypotheses (eq_len / eq_ang) that must enter the
        # linear store
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
        if prop in (10, 11, 12):
            # replay() also drives the by_contra steps and asserts the Book
            # conclusion (same queries the engine's own main() uses). I.12's
            # goal substitute is stated in PROP12_STEPS_DATA's comment: the
            # replay proves rt e h c with h on AB, which implies Lean's
            # disjunction (h and e both on AB, e != h, a != b).
            queries = {10: [("between", "a", "d", "b"),
                            ("_eq_len", "a", "d", "d", "b")],
                       11: [("not_on_line", "f", "AB"),
                            ("_rt_ang", "u", "w", "f")],
                       12: [("_rt_ang", "e", "h", "c"),
                            ("on_line", "h", "AB")]}[prop]
            _s, reports_all = eng.replay(hyps, steps, queries)
            reports = reports_all[:-1]  # drop the {"final": ...} entry
        else:
            reports = [eng.apply_step(s, i, r, a, b)
                       for i, (r, a, b) in enumerate(steps, 1)]

        name_map = PROP11_NAME_MAP if prop == 11 else {}
        lean = (lean_lines_for_named(prop, steps, name_map)
                if prop in (10, 11, 12)
                else lean_lines_for(prop, [r for r, _, _ in steps]))
        rows = [(r, a, b, rep, loc, None)
                for (r, a, b), rep, loc in zip(steps, reports, lean)]

    objects = objects_from_hyps(prop)
    xy = {k: v for k, v in FREE[prop].items()}
    derived_free_points(prop, xy)
    trace_steps = []
    for i, (rule, args, bindings, rep, loc, case_note) in enumerate(
            rows, 1):
        # by_contra/by_cases reports have no added/premises keys (branches
        # ran on scratch states); a by_cases step's contribution to the
        # outer state is its conclusions, shown as the facts it adds
        added = rep.get("added", [])
        if rule == "by_cases":
            added = [tuple(c) for c in bindings]
        premises = rep.get("premises", [])
        why = place_step(prop, rule, args, bindings, added, xy, objects)
        if case_note:
            why = case_note + "; " + why
        for o in objects:
            if o["def_step"] is None:
                o["def_step"] = i
        if loc is None:
            lean_entry = {"file": "Book/Prop%02d.lean" % prop, "line": None}
            lean_note = ("engine-only step (%s): no euclid_apply line in "
                         "the Lean proof cites it; Lean leaves this to "
                         "euclid_finish / SMT"
                         % ("case split" if rule in ("by_contra", "by_cases")
                            else rule))
        else:
            lean_entry = {"file": loc[0], "line": loc[1]}
            lean_note = (("Lean cites %s with different arguments; the "
                          "engine's citation is an equivalent variant" % rule)
                         if len(loc) > 2 else None)
        entry = {
            "n": i, "rule": rule, "args": list(args),
            "bindings": bindings,
            "facts_added": [list(map(str, f)) for f in added],
            "premises": [list(map(str, f)) for f in premises],
            "lean": lean_entry,
            "placement_decision": why,
        }
        if case_note:
            entry["case"] = case_note
        if lean_note:
            entry["lean_note"] = lean_note
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
    if prop == 11:
        # the viewer shows Lean's names; the whole trace is remapped and
        # the engine->Lean map is stated in the JSON
        objects = _remap_names(objects, PROP11_NAME_MAP)
        trace_steps = _remap_names(trace_steps, PROP11_NAME_MAP)
        placement = _remap_names(placement, PROP11_NAME_MAP)

    lean_file = os.path.join(LEAN_DIR, "Book", "Prop%02d.lean" % prop)
    with open(lean_file) as fh:
        lean_text = fh.read().splitlines()

    out = {
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
    if prop == 11:
        out["name_map"] = dict(PROP11_NAME_MAP)
        out["name_map_note"] = ("engine points renamed for Lean display: "
                                "engine u v w g = Lean a b c d")
    return out


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
