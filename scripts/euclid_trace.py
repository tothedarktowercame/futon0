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

# Book I.9: the engine renames Lean's vertex a to v (Lean's
# point_between_points_shorter_than introduces a base name "a" that
# would collide; see PROP9_HYPS_DATA's comment). The viewer shows
# Lean's names, so the trace is built in engine names and remapped at
# the end, as for I.11; the map is stated in the JSON.
PROP9_NAME_MAP = {"v": "a"}


def lean_lines_for_named(prop, steps, name_map, after=None):
    """Signature matching for props whose Lean proof does not name the
    engine's steps one-to-one (I.10's engine proof has bridge and
    by_contra steps Lean does not cite; I.11 renames points). Each step is
    matched to the first unconsumed euclid_apply line whose normalized
    argument list equals the step's (mapped) rule + args. Steps with no
    Lean line get None — the trace says so instead of inventing a line.
    euclid_assert lines are not used here (I.10/I.11 assert nothing).
    `after`: only match lines past this line number (I.16 replays the
    SECOND bullet only; line_from_points f c is textually identical in
    both bullets, so the first bullet's lines must be excluded)."""
    fname = os.path.join(LEAN_DIR, "Book", "Prop%02d.lean" % prop)
    lines = [ln for ln in lean_apply_lines(fname)
             if after is None or ln[0] > after]
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
    # I.6 (equal base angles make equal sides — the converse of I.5, a
    # by_contra drawn in the TRUE configuration): the TRUE configuration
    # is forced to be isosceles (|a-b| = |a-c| is the goal), so a sits
    # exactly on the perpendicular bisector of b-c — a.x = 0.75 = 1.5/2
    # makes both the hypothesis (angle a:b:c = angle a:c:b) and the goal
    # exact in float. Base angles ~= 53.1 degrees (the 3-4-5 angle, over
    # 0.1 rad from each of 45/60/90), and |b-c| = 1.5 differs visibly
    # from |a-b| = |a-c| = 1.25, so the triangle does not look
    # equilateral.
    6: {"b": (0.0, 0.0), "c": (1.5, 0.0), "a": (0.75, 1.0)},
    # I.7 (no two distinct points on the same side of AB at the same
    # distances from a and b — conclusion False): the hypotheses are
    # JOINTLY IMPOSSIBLE — that is the theorem (|a-c| = |a-d| and
    # |c-b| = |d-b| together force d to be c's mirror across AB, the
    # OPPOSITE side). So no placement can satisfy them all. The drawing
    # keeps every drawable hypothesis exact — c and d on the same side
    # of AB, c != d, and |a-c| = |a-d| (d on the circle centre a through
    # c, |a-c| = 1.3 exactly, at a visibly different angle: 10 vs ~67.4
    # degrees — on that side of c the arc runs INSIDE triangle a-c-b, as
    # in Euclid's own figure) — and the un-drawable hypothesis visibly
    # fails: |c-b| ~= 1.628 vs |d-b| ~= 0.391, the margin the proof
    # refutes.
    7: {"a": (0.0, 0.0), "b": (1.6, 0.0), "c": (0.5, 1.2),
        "d": (1.3 * math.cos(math.radians(10.0)),
              1.3 * math.sin(math.radians(10.0)))},
    # I.8 (SSS congruence): two scalene triangles a-b-c and d-e-f with
    # the three hypothesis side equalities. e, f are a rigid copy of
    # b, c under the rotation+translation taking a to d
    # (derived_free_points, PROP8_ROT), so the equalities hold to float
    # precision and the proof's superposition lands EXACTLY: b' = f
    # (step 2's assert) and c' = d (the by_contra's conclusion, exact
    # in the drawn TRUE configuration — verify8 checks both
    # coincidences).
    8: {"a": (0.0, 0.0), "b": (2.0, 0.0), "c": (0.6, 1.2),
        "d": (3.4, 0.2)},
    # I.9 (bisect an angle; the engine renames Lean's vertex a to v —
    # see PROP9_HYPS_DATA): |v-c| = 1.3 < 2|v-b| = 3.2, so the step-1
    # sample (d at |v-c|/2 along v->b) satisfies the
    # point_between_points_shorter_than placement assert. Angle
    # b:v:c ~= 67.4 degrees (the 3-4-5 angle), visibly off the special
    # angles. d, e, f are CONSTRUCTED by steps 1, 2, 4; verify9 checks
    # |v-e| = |v-d|, the equilateral apex, and the bisection to 1e-9.
    9: {"v": (0.0, 0.0), "b": (1.6, 0.0), "c": (0.5, 1.2)},
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
    # I.14: the TRUE configuration is drawn (the proof is a by_contra, so
    # no diagram can show the refuted assumption BC != BD — see the case
    # notes). b at the origin; c and d on the same line through b with b
    # between (the theorem's content: BC = BD); a off that line in GENERAL
    # position (angle a:b:c not right); c, d on opposing sides of AB by
    # construction. e (extend_point BC c b) is placed by the rule, between
    # b and d and distinct from both.
    14: {"a": (0.6, 0.8), "b": (0.0, 0.0), "c": (1.5, 0.0),
         "d": (-1.8, 0.0)},
    # I.15 (vertical angles): e is a HYPOTHESIS point here (the crossing
    # of the two lines), placed at the origin. AB horizontal with between
    # a e b; CD on the exact 3-4-5 direction (53.13 degrees, a non-right
    # crossing; the coordinates are exact binary fractions) with between
    # d e c — so the two vertical pairs (a:e:c = d:e:b, obtuse, and
    # c:e:b = a:e:d, acute) visibly differ.
    15: {"e": (0.0, 0.0), "a": (-1.4, 0.0), "b": (1.1, 0.0),
         "c": (0.75, 1.0), "d": (-0.375, -0.5)},
    # I.16 (exterior angle, SECOND conjunct — Euclid's own proof): a
    # scalene triangle a-b-c with d on the extension of b-c beyond c.
    # e (midpoint of a-c) and f (e the midpoint of b-f) are placed by
    # the rules (proposition_10 / proposition_3), f' past f by
    # extend_point_longer. The placement puts f strictly INSIDE the
    # angle a:c:d with a wide margin (angle a:c:d ~= 135 degrees,
    # angle a:c:f = angle b:a:c ~= 69.5 degrees), so Euclid's point is
    # visible: the SAS copies angle b:a:c to angle e:c:f, which is a
    # proper part of the exterior angle.
    16: {"b": (0.0, 0.0), "c": (1.6, 0.0), "d": (2.6, 0.0),
         "a": (0.5, 1.1)},
    # I.17 (any two angles of a triangle < two right): the same scalene
    # triangle as I.16; d is CONSTRUCTED by step 1 (extend_point BC b c),
    # which places it at c + 0.6*(c-b) = (2.56, 0) — on the extension of
    # b-c beyond c. Interior angles ~65.6 deg at b and ~45.0 deg at c
    # sum to ~110.6 deg, under 180 by a wide visible margin.
    17: {"b": (0.0, 0.0), "c": (1.6, 0.0), "a": (0.5, 1.1)},
    # I.18 (the greater side subtends the greater angle): the same
    # scalene triangle as I.16/I.17; here |a-c| = ~1.556 > |a-b| =
    # ~1.208 is the hypothesis. d is CONSTRUCTED by step 1
    # (proposition_3 a c a b AC AB): on segment a-c with |a-d| = |a-b|.
    # angle a:b:c ~= 65.6 deg vs angle b:c:a = 45.0 deg — a ~0.36 rad
    # visible margin.
    18: {"b": (0.0, 0.0), "c": (1.6, 0.0), "a": (0.5, 1.1)},
    # I.19 (the greater angle is subtended by the greater side): the same
    # scalene triangle as I.16-I.18; here the hypothesis is the ANGLE
    # comparison angle a:b:c ~= 65.6 deg > angle b:c:a = 45.0 deg and the
    # goal is the length comparison |a-c| = ~1.556 > |a-b| = ~1.208.
    # A by_contra (like I.14): the TRUE configuration is drawn; both
    # branches' assumptions (|a-c| = |a-b|, |a-b| > |a-c|) are refuted
    # and not drawn as such.
    19: {"b": (0.0, 0.0), "c": (1.6, 0.0), "a": (0.5, 1.1)},
    # I.20 (two sides of a triangle together exceed the third,
    # |b-a| + |a-c| > |b-c|): the same scalene triangle as I.16-I.19.
    # d' and d are CONSTRUCTED (steps 1-2): d' on the ray b->a beyond a
    # with |a-d'| = 1.5|c-a| (the free parameter of extend_point_longer,
    # strictly greater than the bound); d on segment a-d' with
    # |a-d| = |a-c| (the proposition_3 cut), so between b a d and
    # |b-d| = |b-a| + |a-c| = ~2.763 > |b-c| = 1.6 — a ~1.16 visible
    # margin on the goal.
    20: {"b": (0.0, 0.0), "c": (1.6, 0.0), "a": (0.5, 1.1)},
    # I.21 (lines from inside a triangle: for d inside triangle a-b-c,
    # |b-d| + |d-c| < |b-a| + |a-c| and angle b:d:c > angle b:a:c): the
    # same scalene triangle as I.16-I.20, with d STRICTLY INSIDE (well
    # clear of all three sides). e is CONSTRUCTED by step 3
    # (intersection_lines BD AC): numerically the meeting of line b-d
    # with side a-c, landing between a and c with d between b and e —
    # the true configuration. Goal margins are wide: (|b-a|+|a-c|) -
    # (|b-d|+|d-c|) ~= 1.0 and angle b:d:c ~= 132 deg vs angle b:a:c
    # ~= 69 deg.
    21: {"b": (0.0, 0.0), "c": (1.6, 0.0), "a": (0.5, 1.1),
         "d": (0.7, 0.35)},
    # I.22 (triangle from three lengths): the three given segments a-a',
    # b-b', c-c' stand apart (up top, non-parallel) from the construction
    # line DE (drawn along y = -1.2 by steps 1-3 — d and e' are
    # CONSTRUCTED by arbitrary_point / distinct_points, placed by
    # place_step). Triangle inequalities with visible margins >= 0.15:
    # |aa'| = 1.0, |bb'| = sqrt(1.25) ~= 1.118, |cc'| = sqrt(0.9) ~=
    # 0.949: 1.0 + 1.118 - 0.949 ~= 1.17; 1.0 + 0.949 - 1.118 ~= 0.83;
    # 1.118 + 0.949 - 1.0 ~= 1.07. The drawn case of the by_cases is
    # branch 2 (i between g and f), since |cc'| < |bb'| (margin ~0.17).
    22: {"a": (0.0, 2.2), "a'": (1.0, 2.2),
         "b": (2.4, 2.0), "b'": (2.9, 3.0),
         "c": (0.2, 3.2), "c'": (1.1, 3.5)},
    # I.23 (copy an angle): the target line AB along y = 0 and the given
    # angle d:c:e up top, apart from it. The angle at c is exactly 60
    # degrees (the ray c->d at 150 degrees, the ray c->e at 90 degrees;
    # |c-d| = 1.0, |c-e| = 1.3, |e-d| = sqrt(1.39) ~= 1.179): visibly
    # non-degenerate (neither 0 nor straight). The drawn case is the main
    # branch (d off CE); the degenerate branch (d on CE) is handled by
    # the engine, not drawn. f and g are CONSTRUCTED by proposition_22'
    # (place_step): g on the ray a->b with |a-g| = |c-e| = 1.3, f the
    # circles' meet with |a-f| = |c-d| = 1.0, |f-g| = |e-d|.
    23: {"a": (0.0, 0.0), "b": (1.6, 0.0),
         "c": (0.3, 1.8), "d": (0.3 - math.cos(math.radians(30.0)), 2.3),
         "e": (0.3, 3.1)},
    # I.24 (the hinge theorem): two triangles a-b-c and d-e-f standing
    # apart (abc left, def right), with |a-b| = |d-e| = 1.6 and
    # |a-c| = |d-f| = 1.2 exactly, and the given angle at a = 72 degrees
    # visibly greater than the angle at d = 48 degrees (margin 24
    # degrees >= 5). The construction copies the angle at a onto DE at
    # d (proposition_23' places g' on f's side of DE — the drawn ray
    # d->g' makes angle g':d:e = 72 degrees), extends to g'' and cuts
    # |d-g| = |a-c| (proposition_3), all placed by place_step. The drawn
    # case of the by_cases (Prop24.lean:35) is branch 1: d and g on the
    # same side of EF (g lands at distance ~0.16 from EF on d's side).
    # The goal holds visibly: |b-c| ~= 1.677 > |e-f| ~= 1.196.
    24: {"a": (0.0, 0.0), "b": (1.6, 0.0),
         "c": (1.2 * math.cos(math.radians(72.0)),
               1.2 * math.sin(math.radians(72.0))),
         "d": (3.0, 0.0), "e": (4.6, 0.0),
         "f": (3.0 + 1.2 * math.cos(math.radians(48.0)),
               1.2 * math.sin(math.radians(48.0)))},
    # I.25 (the converse, by contradiction from I.4 and I.24): the same
    # two-triangles shape, drawn in the TRUE configuration — here the
    # hypothesis is the LENGTH comparison |b-c| > |e-f| (|a-b| = |d-e|,
    # |a-c| = |d-f| as at I.24) and the goal is the angle comparison.
    # Angle at a = 75 degrees, at d = 55 degrees (margin 20 degrees);
    # |b-c| ~= 1.734 > |e-f| ~= 1.341 (margin ~0.39). Both refuted
    # branches (angle equality; the reversed comparison) fail these
    # checks, as they must.
    25: {"a": (0.0, 0.0), "b": (1.6, 0.0),
         "c": (1.2 * math.cos(math.radians(75.0)),
               1.2 * math.sin(math.radians(75.0))),
         "d": (3.0, 0.0), "e": (4.6, 0.0),
         "f": (3.0 + 1.2 * math.cos(math.radians(55.0)),
               1.2 * math.sin(math.radians(55.0)))},
}

PROP4_ROT = math.radians(25.0)
PROP8_ROT = math.radians(-15.0)
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
    if prop == 6:
        return list(eng.PROP6_HYPS_DATA), list(eng.PROP6_STEPS_DATA)
    if prop == 7:
        return list(eng.PROP7_HYPS_DATA), list(eng.PROP7_STEPS_DATA)
    if prop == 8:
        return list(eng.PROP8_HYPS_DATA), list(eng.PROP8_STEPS_DATA)
    if prop == 9:
        return list(eng.PROP9_HYPS_DATA), list(eng.PROP9_STEPS_DATA)
    if prop == 10:
        return list(eng.PROP10_HYPS_DATA), list(eng.PROP10_STEPS_DATA)
    if prop == 11:
        return list(eng.PROP11_HYPS_DATA), list(eng.PROP11_STEPS_DATA)
    if prop == 12:
        return list(eng.PROP12_HYPS_DATA), list(eng.PROP12_STEPS_DATA)
    if prop == 13:
        return list(eng.PROP13_HYPS_DATA), list(eng.PROP13_STEPS_DATA)
    if prop == 14:
        return list(eng.PROP14_HYPS_DATA), list(eng.PROP14_STEPS_DATA)
    if prop == 15:
        return list(eng.PROP15_HYPS_DATA), list(eng.PROP15_STEPS_DATA)
    if prop == 16:
        # the SECOND conjunct (Euclid's own proof, Lean's second bullet)
        return list(eng.PROP16B_HYPS_DATA), list(eng.PROP16B_STEPS_DATA)
    if prop == 17:
        return list(eng.PROP17_HYPS_DATA), list(eng.PROP17_STEPS_DATA)
    if prop == 18:
        return list(eng.PROP18_HYPS_DATA), list(eng.PROP18_STEPS_DATA)
    if prop == 19:
        return list(eng.PROP19_HYPS_DATA), list(eng.PROP19_STEPS_DATA)
    if prop == 20:
        return list(eng.PROP20_HYPS_DATA), list(eng.PROP20_STEPS_DATA)
    if prop == 21:
        return list(eng.PROP21_HYPS_DATA), list(eng.PROP21_STEPS_DATA)
    if prop == 22:
        return list(eng.PROP22_HYPS_DATA), list(eng.PROP22_STEPS_DATA)
    if prop == 23:
        return list(eng.PROP23_HYPS_DATA), list(eng.PROP23_STEPS_DATA)
    if prop == 24:
        # the COMPLETE I.24 replay (both branches of the closing
        # by_cases) on PROP24A's hypotheses
        return list(eng.PROP24A_HYPS_DATA), list(eng.PROP24_STEPS_DATA)
    if prop == 25:
        return list(eng.PROP25_HYPS_DATA), list(eng.PROP25_STEPS_DATA)
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
    if prop == 8:
        # e, f: rigid copy of b, c under the rotation+translation taking
        # a to d (as I.4), so the three hypothesis side equalities hold
        # exactly and the superposition lands b' on f and c' on d
        cs, sn = math.cos(PROP8_ROT), math.sin(PROP8_ROT)
        rot = lambda p: (cs * p[0] - sn * p[1], sn * p[0] + cs * p[1])
        for src, name in (("b", "e"), ("c", "f")):
            xy[name] = _add(xy["d"], rot(_sub(xy[src], xy["a"])))



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
    if prop == 6:
        # the triangle a-b-c and the three hypothesis lines (dpl a b AB;
        # b, c on BC; c, a on AC); I.6 constructs only inside the
        # by_contra's refuted branches, which are not drawn
        return [{"name": n, "kind": "point", "jsx": {"type": "point"},
                 "def_step": 0} for n in ("a", "b", "c")] + [
                {"name": L, "kind": "line", "jsx": {"type": "line",
                  "p": list(pq)}, "def_step": 0, "hyp": True}
                for L, pq in (("AB", ("a", "b")), ("BC", ("b", "c")),
                              ("AC", ("a", "c")))]
    if prop == 7:
        # the four hypothesis points and the FIVE hypothesis lines
        # (dpl a b AB, dpl a c AC, dpl c b CB, dpl a d AD, dpl d b DB);
        # CD is drawn by step 1 (line_from_points), and the branches'
        # e, f live only under refuted case assumptions (not drawn)
        return [{"name": n, "kind": "point", "jsx": {"type": "point"},
                 "def_step": 0} for n in ("a", "b", "c", "d")] + [
                {"name": L, "kind": "line", "jsx": {"type": "line",
                  "p": list(pq)}, "def_step": 0, "hyp": True}
                for L, pq in (("AB", ("a", "b")), ("AC", ("a", "c")),
                              ("CB", ("c", "b")), ("AD", ("a", "d")),
                              ("DB", ("d", "b")))]
    if prop == 8:
        # the two hypothesis triangles and their six hypothesis lines
        # (two formTriangles destructed); the superposition's primed
        # points b', c' and lines CG, EG are drawn by step 1
        return [{"name": n, "kind": "point", "jsx": {"type": "point"},
                 "def_step": 0} for n in ("a", "b", "c", "d", "e", "f")] + [
                {"name": L, "kind": "line", "jsx": {"type": "line",
                  "p": list(pq)}, "def_step": 0, "hyp": True}
                for L, pq in (("AB", ("a", "b")), ("BC", ("b", "c")),
                              ("AC", ("a", "c")), ("DE", ("d", "e")),
                              ("EF", ("e", "f")), ("DF", ("d", "f")))]
    if prop == 9:
        # the angle's vertex v (Lean's a) with b, c and the two
        # hypothesis lines (formRectilinearAngle destructured: dpl v b
        # AB, dpl v c AC); d, e, f are CONSTRUCTED (steps 1, 2, 4); the
        # h-sm facts about f stand behind the SSS step
        return [{"name": n, "kind": "point", "jsx": {"type": "point"},
                 "def_step": 0} for n in ("v", "b", "c")] + [
                {"name": L, "kind": "line", "jsx": {"type": "line",
                  "p": list(pq)}, "def_step": 0, "hyp": True}
                for L, pq in (("AB", ("v", "b")), ("AC", ("v", "c")))]
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
    if prop == 14:
        # three hypothesis lines (dpl a b AB, dpl b c BC, dpl b d BD); in
        # the drawn TRUE configuration BC and BD are the same straight
        # line through b — drawn as the two named lines, overlapping
        return [{"name": n, "kind": "point", "jsx": {"type": "point"},
                 "def_step": 0} for n in ("a", "b", "c", "d")] + [
                {"name": L, "kind": "line", "jsx": {"type": "line",
                  "p": list(pq)}, "def_step": 0, "hyp": True}
                for L, pq in (("AB", ("a", "b")), ("BC", ("b", "c")),
                              ("BD", ("b", "d")))]
    if prop == 15:
        # five hypothesis points (e included: it is the given crossing of
        # the two lines, between a and b and between d and c) and the two
        # hypothesis lines they determine
        return [{"name": n, "kind": "point", "jsx": {"type": "point"},
                 "def_step": 0} for n in ("a", "b", "c", "d", "e")] + [
                {"name": L, "kind": "line", "jsx": {"type": "line",
                  "p": list(pq)}, "def_step": 0, "hyp": True}
                for L, pq in (("AB", ("a", "b")), ("CD", ("c", "d")))]
    if prop == 16:
        # the triangle a-b-c and d (between b c d) are hypotheses; the
        # three named lines are the hypothesis lines AB, BC, AC
        return [{"name": n, "kind": "point", "jsx": {"type": "point"},
                 "def_step": 0} for n in ("a", "b", "c", "d")] + [
                {"name": L, "kind": "line", "jsx": {"type": "line",
                  "p": list(pq)}, "def_step": 0, "hyp": True}
                for L, pq in (("AB", ("a", "b")), ("BC", ("b", "c")),
                              ("AC", ("a", "c")))]
    if prop == 17:
        # the triangle a-b-c and the three hypothesis lines; d is
        # CONSTRUCTED by step 1 (extend_point BC b c), not a hypothesis
        return [{"name": n, "kind": "point", "jsx": {"type": "point"},
                 "def_step": 0} for n in ("a", "b", "c")] + [
                {"name": L, "kind": "line", "jsx": {"type": "line",
                  "p": list(pq)}, "def_step": 0, "hyp": True}
                for L, pq in (("AB", ("a", "b")), ("BC", ("b", "c")),
                              ("AC", ("a", "c")))]
    if prop == 18:
        # the triangle a-b-c and the three hypothesis lines; d is
        # CONSTRUCTED by step 1 (proposition_3 a c a b AC AB), not a
        # hypothesis
        return [{"name": n, "kind": "point", "jsx": {"type": "point"},
                 "def_step": 0} for n in ("a", "b", "c")] + [
                {"name": L, "kind": "line", "jsx": {"type": "line",
                  "p": list(pq)}, "def_step": 0, "hyp": True}
                for L, pq in (("AB", ("a", "b")), ("BC", ("b", "c")),
                              ("AC", ("a", "c")))]
    if prop == 19:
        # the triangle a-b-c and the three hypothesis lines; I.19
        # constructs nothing (a by_contra with two cited-lemma branches)
        return [{"name": n, "kind": "point", "jsx": {"type": "point"},
                 "def_step": 0} for n in ("a", "b", "c")] + [
                {"name": L, "kind": "line", "jsx": {"type": "line",
                  "p": list(pq)}, "def_step": 0, "hyp": True}
                for L, pq in (("AB", ("a", "b")), ("BC", ("b", "c")),
                              ("AC", ("a", "c")))]
    if prop == 20:
        # the triangle a-b-c and the three hypothesis lines; d' and d
        # are CONSTRUCTED by steps 1-2 (extend_point_longer AB b a (c-a);
        # proposition_3 a d' a c AB AC), not hypotheses
        return [{"name": n, "kind": "point", "jsx": {"type": "point"},
                 "def_step": 0} for n in ("a", "b", "c")] + [
                {"name": L, "kind": "line", "jsx": {"type": "line",
                  "p": list(pq)}, "def_step": 0, "hyp": True}
                for L, pq in (("AB", ("a", "b")), ("BC", ("b", "c")),
                              ("AC", ("a", "c")))]
    if prop == 21:
        # the triangle a-b-c, the interior point d (a hypothesis here),
        # and the five hypothesis lines (AB, BC, AC and the dpl lines BD,
        # DC); e is CONSTRUCTED by step 3 (intersection_lines BD AC)
        return [{"name": n, "kind": "point", "jsx": {"type": "point"},
                 "def_step": 0} for n in ("a", "b", "c", "d")] + [
                {"name": L, "kind": "line", "jsx": {"type": "line",
                  "p": list(pq)}, "def_step": 0, "hyp": True}
                for L, pq in (("AB", ("a", "b")), ("BC", ("b", "c")),
                              ("AC", ("a", "c")), ("BD", ("b", "d")),
                              ("DC", ("d", "c")))]
    if prop == 22:
        # the six hypothesis points (the three given segments) and the
        # three hypothesis lines A, B, C they lie on; d, e', DE and the
        # whole construction are CONSTRUCTED by steps 1-3 and on
        return [{"name": n, "kind": "point", "jsx": {"type": "point"},
                 "def_step": 0}
                for n in ("a", "a'", "b", "b'", "c", "c'")] + [
                {"name": L, "kind": "line", "jsx": {"type": "line",
                  "p": list(pq)}, "def_step": 0, "hyp": True}
                for L, pq in (("A", ("a", "a'")), ("B", ("b", "b'")),
                              ("C", ("c", "c'")))]
    if prop == 23:
        # the target line AB (dpl a b AB) and the given angle's vertex c
        # with d, e and the two hypothesis lines CD, CE
        # (formRectilinearAngle destructured: dpl d c CD, dpl c e CE);
        # DE, f, g, FA, FG are CONSTRUCTED by the main branch's steps
        return [{"name": n, "kind": "point", "jsx": {"type": "point"},
                 "def_step": 0} for n in ("a", "b", "c", "d", "e")] + [
                {"name": L, "kind": "line", "jsx": {"type": "line",
                  "p": list(pq)}, "def_step": 0, "hyp": True}
                for L, pq in (("AB", ("a", "b")), ("CD", ("d", "c")),
                              ("CE", ("c", "e")))]
    if prop in (24, 25):
        # the two hypothesis triangles and their six hypothesis lines
        # (two formTriangles destructured, as at I.8); at I.24 the
        # copied-angle points g', g'', g and lines DG, EG, FG are
        # CONSTRUCTED by steps 1-10, and h lives only in the
        # engine-handled second case; at I.25 nothing is constructed
        # (a by_contra with two cited-lemma branches, as at I.19)
        return [{"name": n, "kind": "point", "jsx": {"type": "point"},
                 "def_step": 0} for n in ("a", "b", "c", "d", "e", "f")] + [
                {"name": L, "kind": "line", "jsx": {"type": "line",
                  "p": list(pq)}, "def_step": 0, "hyp": True}
                for L, pq in (("AB", ("a", "b")), ("BC", ("b", "c")),
                              ("AC", ("a", "c")), ("DE", ("d", "e")),
                              ("EF", ("e", "f")), ("DF", ("d", "f")))]
    return [{"name": n, "kind": "point", "jsx": {"type": "point"},
             "def_step": 0} for n in ("a", "b", "c0", "c1")] + [
            {"name": "AB", "kind": "line", "jsx": {"type": "line",
              "p": ["a", "b"]}, "def_step": 0, "hyp": True},
            {"name": "C", "kind": "line", "jsx": {"type": "line",
              "p": ["c0", "c1"]}, "def_step": 0, "hyp": True}]


def place_step(prop, rule, args, bindings, added, xy, objects):
    """Numeric placement for one step: extends xy (name -> (x,y)) and
    appends introduced objects. Returns the decision note."""
    if prop == 6 and rule in ("proposition_3", "line_from_points"):
        # I.6's only constructions live inside the by_contra's refuted
        # branches (a cut point d and the line through it): under the
        # branch's refuted length assumption d would sit strictly inside
        # the segment; in the TRUE configuration (|a-b| = |a-c|) d would
        # coincide with an endpoint. Nothing is placed; the note says so.
        return ("refuted branch construction — not drawn: under this "
                "branch's refuted assumption the new point would be a "
                "strict interior cut, which the TRUE configuration "
                "(angle a:b:c = angle a:c:b, hence |a-b| = |a-c|) "
                "collapses onto an endpoint")
    if prop == 7 and rule == "extend_point_not_on_line":
        # I.7's e/f live only inside the refuted same/opposing-side
        # branches (past CD on the extensions of AC/AD). The branches
        # are refuted, so the points are not placed; the note says so.
        return ("refuted branch construction — not drawn: under this "
                "branch's refuted case assumptions the extension point "
                "would lie past CD; the diagram shows the hypothesis "
                "configuration (c, d on the same side of AB, "
                "|a-c| = |a-d|)")
    if prop == 23 and rule in ("angle_self_zero", "use_witness",
                               "flat_angle_onlyif", "extend_point",
                               "between_symm_parts"):
        # I.23's degenerate branch (d on CE — "Omitted by Euclid",
        # Prop23.lean:14-21): the branch is HANDLED (each sub-case
        # supplies its own witness: b itself when angle d:c:e = 0, the
        # extension of AB past a when angle d:c:e is straight), not
        # refuted — but it is not the drawn case. Its steps run on a
        # state copy; nothing is placed. In particular the branch's
        # witness name f (the extension point) is NOT the drawn f (the
        # main branch's proposition_22' meet), so placing it would
        # clobber the drawn construction.
        return ("degenerate branch (d on CE, 'Omitted by Euclid'): "
                "handled by the engine, not drawn — the diagram shows "
                "the general case (d off CE), in which the copied angle "
                "at a is constructed by proposition_22'")
    if prop == 24 and rule == "extend_point":
        # I.24's second case ("Omitted by Euclid", Prop24.lean:40-59)
        # constructs h (extend_point FG g f). That case is HANDLED by
        # the engine on a state copy, not drawn — the drawn case is
        # branch 1 (d sameSide g EF). Nothing is placed; placing h would
        # add a point the drawn configuration has no use for.
        return ("second case (not same_side d g EF — 'Omitted by "
                "Euclid'): handled by the engine, not drawn — the "
                "diagram shows Euclid's case (d and g on the same side "
                "of EF)")
    if rule == "proposition_23'":
        # (d, e, a, b, c, f, DE, AB, AC) -> g' with g' != d,
        # angle g':d:e = angle b:a:c, and g' on DE or on f's side of DE
        # (Prop23.lean:35, cited at Prop24.lean:18). The engine's next
        # step eliminates the on-line disjunct, so the sample sits on
        # f's side. The rule fixes the angle and the side; the distance
        # |d-g'| is a free parameter of the existential step (unit
        # length chosen, as the engine places no coordinate).
        (d, e, a, b, c, f, DE, _AB, _AC) = args
        name = bindings["f"]
        ang = _ang(xy, b, a, c)
        base = math.atan2(xy[e][1] - xy[d][1], xy[e][0] - xy[d][0])
        side = lambda p: ((p[0] - xy[d][0]) * (xy[e][1] - xy[d][1])
                          - (p[1] - xy[d][1]) * (xy[e][0] - xy[d][0]))
        cand = None
        for sgn in (1.0, -1.0):
            t = base + sgn * ang
            p = _add(xy[d], (math.cos(t), math.sin(t)))
            if side(p) * side(xy[f]) > 0:
                cand = p
                break
        assert cand is not None
        xy[name] = cand
        objects.append({"name": name, "kind": "point",
                        "jsx": {"type": "point"}, "def_step": None})
        return ("free parameter of the existential step: |%s-%s| = 1 on "
                "the ray from %s making angle %s:%s:%s = angle %s:%s:%s "
                "with %s-%s, on %s's side of %s (the rule fixes the "
                "angle and the side — the on-line disjunct is eliminated "
                "by the next step — not the distance)"
                % (d, name, d, name, d, e, b, a, c, d, e, f, DE))
    if rule == "arbitrary_point":
        # () -> a free point p. No fact constrains it at all; the sample
        # is chosen so the construction that follows stands apart from
        # the hypothesis figure (I.22: below the three given segments).
        name = bindings["p"]
        assert prop == 22, "arbitrary_point placement only sampled for I.22"
        xy[name] = (0.0, -1.2)
        objects.append({"name": name, "kind": "point",
                        "jsx": {"type": "point"}, "def_step": None})
        return ("free parameter of the existential step: an arbitrary "
                "point; the sample sits below the three given segments "
                "so the construction line DE stands apart from them")
    if rule == "distinct_points":
        # (p,) -> a point q != p. The rule fixes only the inequality;
        # the sample sits at unit distance along the x-axis (I.22: so
        # the construction line DE through d and e' is horizontal, apart
        # from the given segments).
        (p,), name = args, bindings["q"]
        assert prop == 22, "distinct_points placement only sampled for I.22"
        xy[name] = _add(xy[p], (1.0, 0.0))
        objects.append({"name": name, "kind": "point",
                        "jsx": {"type": "point"}, "def_step": None})
        return ("free parameter of the existential step: any point "
                "distinct from %s; the sample sits at unit distance "
                "along the x-axis, so DE is horizontal" % p)
    if rule == "proposition_22'":
        # I.23's construction step: (c d c e e d a b CD CE DE AB) ->
        # k (= f, the circles' meet) and g with g on AB,
        # not between g a b, |a-f| = |c-d|, |a-g| = |c-e|,
        # |f-g| = |e-d| (Prop22.lean:34, the variant I.23 cites). g is
        # fixed exactly on the ray a->b (not between g a b forces g onto
        # b's side of a); f is one of two circle meets mirrored across
        # AB — the rule does NOT fix the side (that is I.23''s
        # refinement, proposition_22'', used by I.23'): the sample takes
        # the given angle's side of AB, so the copy and the original
        # open the same way.
        (c, d, _c2, e, _e2, _d2, a, b, _CD, _CE, _DE, AB) = args
        f, g = bindings["k"], bindings["g"]
        xy[g] = _add(xy[a], _mul(_unit(_sub(xy[b], xy[a])),
                                 _norm(_sub(xy[e], xy[c]))))
        cands = circle_circle(xy[a], _norm(_sub(xy[d], xy[c])),
                              xy[g], _norm(_sub(xy[d], xy[e])))
        abvec = _sub(xy[b], xy[a])
        side = lambda p: (abvec[0] * (p[1] - xy[a][1])
                          - abvec[1] * (p[0] - xy[a][0]))
        pick = cands[0] if side(cands[0]) * side(xy[c]) > 0 else cands[1]
        xy[f] = pick
        objects.append({"name": f, "kind": "point",
                        "jsx": {"type": "point"}, "def_step": None})
        objects.append({"name": g, "kind": "point",
                        "jsx": {"type": "point"}, "def_step": None})
        return ("I.22' construction: g on the ray a->b with |a-g| = "
                "|c-e| (fixed exactly by not between g a b); f the "
                "circles' meet with |a-f| = |c-d|, |f-g| = |e-d| — fixed "
                "up to reflection across AB; the side is a free choice "
                "(the rule does not decide it — I.23' adds same_side), "
                "sampled on the given angle's side of AB")
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
    if rule in ("intersection_same_side", "intersection_opposite_side"):
        # the meeting point of two circles on the same / the other side of
        # line L (through the centres c, d) from point b. The engine's
        # reference traces never use these, so there was no placement and
        # a student's point stayed undrawn (codex-1, live loop slice 9,
        # I.9: the apex f of an equilateral on be).
        (A, B, b, c, d, L) = args
        name = bindings["a"]
        def circ(C):
            o = next(o for o in objects if o["name"] == C)
            return xy[o["jsx"]["center"]], _norm(_sub(
                xy[o["jsx"]["through"]], xy[o["jsx"]["center"]]))
        (ca, ra), (cb, rb) = circ(A), circ(B)
        cands = circle_circle(ca, ra, cb, rb)
        p1, p2 = xy[c], xy[d]
        side = lambda x: ((x[0] - p1[0]) * (p2[1] - p1[1])
                          - (x[1] - p1[1]) * (p2[0] - p1[0]))
        want = (lambda v: v > 0) if rule == "intersection_same_side" \
            else (lambda v: v < 0)
        idx = next((i for i, q in enumerate(cands)
                    if want(side(q) * side(xy[b]))), 0)
        xy[name] = cands[idx]
        objects.append({"name": name, "kind": "point",
                        "jsx": {"type": "intersection", "parents": [A, B],
                                "idx": idx},
                        "def_step": None})
        return "%s side of %s from %s" % (
            "same" if rule == "intersection_same_side" else "other", L, b)
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
    if prop == 8 and rule == "superposition":
        # I.8's argument order is (b c a e f d): the rigid motion maps
        # b->e, so b' is the image of C (|e-b'| = |b-c| = |e-f| then
        # forces b' = f — step 2's assert) and c' is the image of A
        # (the by_contra proves c' = d; in the drawn TRUE configuration
        # the coincidence is exact — verify8 checks it). Lean names the
        # images (c', g) and the carried lines (CG', EG). Placement
        # math as the generic superposition handler below.
        (a, b, c, d, e, f, AB, BC, AC, DE), B = args, bindings
        bp, cp = B["b'"], B["c'"]
        ab = _sub(xy[b], xy[a])
        de = _sub(xy[e], xy[d])
        ang = math.atan2(de[1], de[0]) - math.atan2(ab[1], ab[0])
        cs, sn = math.cos(ang), math.sin(ang)
        rot = lambda p: (cs * p[0] - sn * p[1], sn * p[0] + cs * p[1])
        xy[bp] = _add(xy[d], rot(ab))
        xy[cp] = _add(xy[d], rot(_sub(xy[c], xy[a])))
        assert _norm(_sub(xy[bp], xy[e])) < EPS, "superposition: b' must be f"
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
        return ("rigid motion mapping b->e, c->f, a->d: b' (the image "
                "of c) lands exactly on f (asserted in step 2) and c' "
                "(the image of a) lands exactly on d (the by_contra's "
                "conclusion, exact in this TRUE configuration); the "
                "primed triangle is triangle b-c-a carried onto e-f-d. "
                "Lean names the images (c', g) and the carried lines "
                "(CG', EG)")
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
    if rule == "proposition_1'":
        # (a, b, x, AB) -> the equilateral apex on segment a-b, on the
        # side of AB opposite x (I.9's f on DE opposite v). The rule
        # fixes the point exactly (both lengths equal |a-b|); no free
        # parameter.
        (a, b, x, AB), name = args, bindings["c"]
        p1, p2 = xy[a], xy[b]
        seg = _sub(p2, p1)
        mid = _mul(_add(p1, p2), 0.5)
        h = math.sqrt(3.0) / 2.0 * _norm(seg)
        perp = _unit((-seg[1], seg[0]))
        side = lambda p: ((p[0] - p1[0]) * seg[1]
                          - (p[1] - p1[1]) * seg[0])
        cand = _add(mid, _mul(perp, h))
        if side(cand) * side(xy[x]) > 0:
            cand = _sub(mid, _mul(perp, h))
        xy[name] = cand
        objects.append({"name": name, "kind": "point",
                        "jsx": {"type": "point"}, "def_step": None})
        return ("equilateral apex on %s-%s, on the side of %s opposite "
                "%s (proposition_1' fixes the point exactly: both "
                "lengths equal |%s-%s|; no free parameter)"
                % (a, b, AB, x, a, b))
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
        # no drawing for the split itself; I.14's branch steps ARE drawn
        # (in the true configuration — see the case notes), earlier
        # props' refuted branches are only noted
        branches, _concl = args, bindings
        parts = []
        for facts, bsteps in branches:
            rules = [s[0] for s in bsteps]
            parts.append("case split: assume %s — refuted by %s"
                         % (list(facts), rules or
                            ["contradiction in the store"]))
        if prop == 8:
            return ("; ".join(parts) + " (the refuted assumption "
                    "d != c' is not drawable; the diagram shows the "
                    "TRUE configuration — the superposition lands c' "
                    "exactly on d, hence angle b:a:c = angle e:d:f)")
        if prop == 14:
            return ("; ".join(parts) + " (the assumption is not drawable; "
                    "the branch's construction steps are drawn in the "
                    "true configuration)")
        if prop == 19:
            return ("; ".join(parts) + " (the refuted assumptions are "
                    "not drawable; the diagram shows the TRUE "
                    "configuration — angle a:b:c > angle b:c:a, hence "
                    "|a-c| > |a-b|)")
        if prop == 25:
            return ("; ".join(parts) + " (the refuted assumptions are "
                    "not drawable; the diagram shows the TRUE "
                    "configuration — |b-c| > |e-f|, hence "
                    "angle b:a:c > angle e:d:f)")
        if prop == 6:
            return ("; ".join(parts) + " (the refuted assumptions are "
                    "not drawable; the diagram shows the TRUE "
                    "configuration — angle a:b:c = angle a:c:b, hence "
                    "|a-b| = |a-c|)")
        if prop == 7:
            return ("; ".join(parts) + " (every branch is refuted — "
                    "I.7's conclusion is False; the diagram shows the "
                    "hypothesis configuration as stated: c, d on the "
                    "same side of AB with |a-c| = |a-d|. The second "
                    "equality |c-b| = |d-b| cannot also be drawn — that "
                    "impossibility is the theorem)")
        if prop == 21:
            return ("engine-only step fixing the order of b, d, e on BD "
                    "(trichotomy, Diagrammatic.lean:87): the two other "
                    "orders are refuted — between b e d would put b "
                    "opposing d across AC (pasch_3, e on AC) against the "
                    "hypothesis b.sameSide d AC; between e b d would put "
                    "e opposing d across AB (pasch_3, b on AB) against "
                    "same_side e d AB — so between b d e. Lean's SMT "
                    "closes this silently; the diagram draws the TRUE "
                    "configuration (d strictly inside the triangle, e on "
                    "a-c between a and c, d between b and e)")
        return "; ".join(parts) + " (refuted branches are not drawn)"
    if rule == "by_cases":
        # no drawing for the split itself: the DRAWN case is branch 2 in
        # general position (see the entry's case note); the other branches
        # are checked by the engine on state copies and only noted
        if prop == 22:
            return ("engine-only case split (no euclid_apply line — it "
                    "renders the intersects_cc premise Lean's SMT closes "
                    "inside euclid_apply (intersection_circles KLH DKL), "
                    "Prop22.lean:27): where does DE's second meet i with "
                    "KLH fall relative to f? f and i both lie on DE on "
                    "the side of g opposite h, so trichotomy leaves "
                    "three cases: f between g and i (needs |c-c'| > "
                    "|b-b'|), i between g and f (needs |c-c'| < |b-b'| — "
                    "the DRAWN case), and f = i (needs |c-c'| = |b-b'|). "
                    "Each case puts i inside DKL; the split changes no "
                    "drawing")
        if prop == 23:
            if args and args[0] and args[0][0][0] == "_zero_ang":
                return ("the nested split of the degenerate branch "
                        "(Prop23.lean:15, by_cases on angle d:c:e = 0): "
                        "both sub-cases are handled by the engine — "
                        "witness b when the angle is 0, the extension "
                        "of AB past a when it is straight — and not "
                        "drawn; the diagram shows the general case "
                        "(d off CE)")
            return ("the degenerate branch (d on CE, 'Omitted by "
                    "Euclid') is handled by the engine, not drawn; the "
                    "diagram shows the general case (d off CE), in "
                    "which proposition_22' constructs the copied angle")
        if prop == 24:
            return ("case split (Prop24.lean:35, by_cases on "
                    "d.sameSide g EF): branch 1 — d and g on the same "
                    "side of EF — is EUCLID'S case and is drawn; "
                    "branch 2 ('Omitted by Euclid', Prop24.lean:40-59: "
                    "g on EF, or g off EF on the side opposite d) is "
                    "handled by the engine on state copies, not drawn. "
                    "Both branches conclude |b-c| > |e-f|")
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
    if rule == "extend_point_longer":
        # (L, p, q, u, v) -> x on L with between p q x and |q-x| > |u-v|
        # (Points.lean:43; the length bound is passed as its two
        # endpoints u, v — (b-e) at I.16, (c-a) at I.20). The rule fixes
        # the line, the side and the bound; the distance |q-x| is a free
        # parameter of the existential step (1.5|u-v| chosen, strictly
        # greater than the bound, as the engine places no coordinate).
        (L, p, q, u, v), name = args, bindings["x"]
        bound = _norm(_sub(xy[v], xy[u]))
        seg = _sub(xy[q], xy[p])
        xy[name] = _add(xy[q], _mul(_unit(seg), 1.5 * bound))
        objects.append({"name": name, "kind": "point",
                        "jsx": {"type": "point"}, "def_step": None})
        return ("free parameter of the existential step: |%s-%s| = "
                "1.5|%s-%s| > |%s-%s| on the ray from %s through %s "
                "(the rule fixes between %s %s %s and the lower bound, "
                "not the distance)"
                % (q, name, u, v, u, v, p, q, p, q, name))
    if rule == "extend_point":
        # (L, p, q) -> x on L with between p q x: the rule fixes the line
        # and the side; the distance |q-x| is a free parameter of the
        # existential step (0.6 |p-q| chosen, as the engine places no
        # coordinate). For I.14 this lands e on the drawn line BC between
        # b and d — the true configuration, where e's ray IS d's ray.
        (L, p, q), name = args, bindings["x"]
        seg = _sub(xy[q], xy[p])
        xy[name] = _add(xy[q], _mul(_unit(seg), 0.6 * _norm(seg)))
        objects.append({"name": name, "kind": "point",
                        "jsx": {"type": "point"}, "def_step": None})
        return ("free parameter of the existential step: |%s-%s| = "
                "0.6|%s-%s| on the ray from %s through %s (the rule fixes "
                "between %s %s %s, not the distance)"
                % (q, name, p, q, p, q, p, q, name))
    if rule in ("assert_point_eq_on_ray", "assert_point_eq_angle",
                "proposition_4", "proposition_5", "proposition_5'",
                "proposition_13", "proposition_15",
                "proposition_16", "proposition_17", "proposition_18",
                "proposition_19", "proposition_24",
                "sum_angles_onlyif",
                "between_symm_parts", "pasch_3_opposing", "pasch_4",
                "opposing_same_side_trans", "opposing_opposing_same_side",
                "opposing_sides_if",
                "triple_incidence_1", "triple_incidence_2",
                "angle_pair_on_line",
                "proposition_8", "proposition_7", "perpendicular_if",
                "perpendicular_onlyif", "proposition_20",
                "intersection_lines_opposing",
                "intersection_circle_circle_1",
                "off_line_strict_triangle", "degenerated_angle_if",
                "angle_ray_eq_zero",
                "intersection_circle_line_1"):
        # forward-closure / cited steps: no new objects, no coordinates
        if rule == "intersection_circle_circle_1" and prop == 22:
            return ("engine-only step (intersection_circle_circle_1 i h "
                    "KLH DKL, Diagrammatic.lean:312): i is inside DKL "
                    "(the by_cases above) and not outside KLH (it is ON "
                    "KLH), h is on KLH and outside DKL (|f-h| = "
                    "|b-b'| + |c-c'| > |a-a'| by inequality 3) — so the "
                    "circles meet. This is the intersects_cc premise "
                    "Lean's SMT closes silently at Prop22.lean:27")
        if rule == "off_line_strict_triangle" and prop == 23:
            return ("engine-only step (off_line_strict_triangle f a g "
                    "AB): the three strict triangle inequalities among "
                    "|a-f|, |a-g|, |f-g| (the proposition_20 rows "
                    "through the I.22' equalities) put f off AB — the "
                    "premise Lean's SMT closes inside euclid_apply "
                    "(proposition_8 ...) at Prop23.lean:30")
        if rule == "degenerated_angle_if" and prop == 23:
            return ("engine-only step (degenerated_angle_if a g b AB, "
                    "Transfer.lean:63): g on AB with not between g a b "
                    "(I.22''s conclusion) makes angle g:a:b = 0 — the "
                    "rays a->g and a->b coincide. Lean's euclid_finish "
                    "closes this silently")
        if rule == "angle_ray_eq_zero" and prop == 23:
            return ("engine-only step (angle_ray_eq_zero g a b f): "
                    "since angle g:a:b = 0, angle f:a:g = angle f:a:b — "
                    "the copied angle is measured against the target "
                    "ray. Lean's euclid_finish closes this silently")
        if rule == "proposition_20" and prop == 23:
            return ("cited step (proposition_20 %s %s %s %s %s %s — "
                    "Euclid leaves the triangle inequalities implicit; "
                    "Lean's comment flags them): one of the three "
                    "strict inequalities among |c-d|, |c-e|, |e-d| that "
                    "proposition_22''s premises ask for"
                    % tuple(list(args)[:6]))
        if rule == "proposition_4" and prop == 24:
            return ("cited step (proposition_4 a b c d e g AB BC AC DE "
                    "EG DG — SAS): |a-b| = |d-e| (hypothesis), |a-c| = "
                    "|d-g| (the proposition_3 cut), and angle b:a:c = "
                    "angle e:d:g (the copied angle of step 1, by the "
                    "ray-equality chain), so triangle a-b-c congrues "
                    "with d-e-g: |b-c| = |e-g| — the goal's hinge — "
                    "and angle d:g:e = angle a:c:b")
        if rule == "proposition_5'" and prop == 24:
            return ("cited step (proposition_5' d g f DG FG DF — the "
                    "isosceles theorem): |d-g| = |a-c| (the cut) = "
                    "|d-f| (hypothesis), so the base angles agree: "
                    "angle d:g:f = angle d:f:g")
        if rule == "proposition_19" and prop == 24:
            return ("cited step (proposition_19 e f g EF FG EG — the "
                    "greater angle faces the greater side, in triangle "
                    "e-f-g): angle e:f:g > angle e:g:f (the assert "
                    "above), so |e-g| > |e-f|. The conclusion's why: "
                    "|b-c| = |e-g| (the proposition_4 step) > |e-f| — "
                    "the goal")
        if rule == "triple_incidence_2" and prop == 24:
            return ("engine-only step (triple_incidence_2, "
                    "Diagrammatic.lean:201): three lines through one "
                    "point transfer the known side facts into the "
                    "same_side premise the next angle split needs. "
                    "Lean's euclid_assert / SMT closes this silently")
        if rule == "sum_angles_onlyif" and prop == 24:
            return ("engine-only step (sum_angles_onlyif, "
                    "Transfer.lean:90): a point inside an angle splits "
                    "it into a sum — the angle arithmetic behind the "
                    "asserts. Lean's SMT discharges these rows silently")
        if rule == "proposition_4" and prop == 25:
            return ("cited step (proposition_4 a b c d e f AB BC AC DE "
                    "EF DF — SAS): under the branch assumption "
                    "angle b:a:c = angle e:d:f, with |a-b| = |d-e| and "
                    "|a-c| = |d-f| (hypotheses), the triangles congrue "
                    "— so |b-c| = |e-f|, contradicting the hypothesis "
                    "|b-c| > |e-f|")
        if rule == "proposition_24" and prop == 25:
            return ("cited step (proposition_24 d e f a b c DE EF DF "
                    "AB BC AC — the hinge theorem, I.24, with the two "
                    "triangles swapped): under the branch assumption "
                    "angle e:d:f > angle b:a:c, with |d-e| = |a-b| and "
                    "|d-f| = |a-c|, I.24 gives |e-f| > |b-c|, "
                    "contradicting the hypothesis |b-c| > |e-f|")
        if rule == "proposition_8" and prop == 23:
            return ("cited step (proposition_8 c d e a f g CD DE CE FA "
                    "FG AB — SSS): |c-d| = |a-f|, |c-e| = |a-g|, "
                    "|e-d| = |f-g| (the I.22' construction), so angle "
                    "d:c:e = angle f:a:g. The conclusion's why: angle "
                    "f:a:b = angle f:a:g (the zero-angle rewrite above) "
                    "= angle d:c:e — the goal")
        if rule == "assert_point_eq_on_ray" and prop == 8:
            return ("cited assertion (Lean's euclid_assert c' = f): b' "
                    "and f both lie on EF on the same ray from e with "
                    "|e-b'| = |b-c| (the superposition bundle) = |e-f| "
                    "(hypothesis), so b' = f — exact in the drawing")
        if rule == "proposition_7" and prop == 8:
            return ("cited step (proposition_7 e f d c' EF DE DF EG CG "
                    "— I.7, two distances fix the point): d and c' lie "
                    "on the same side of EF (the superposition bundle) "
                    "with |e-d| = |a-b| = |e-c'| and |f-d| = |a-c| = "
                    "|b'-c'| = |f-c'|, so d = c' — contradicting the "
                    "branch assumption d != c'")
        if rule == "triple_incidence_1" and prop == 21:
            return ("engine-only step (triple_incidence_1 AB BD BC b a d "
                    "c, Diagrammatic.lean:187): AB, BD, BC all meet at b; "
                    "with d.sameSide c AB (the hypothesis c.sameSide d AB, "
                    "symmetrized) and a.sameSide d BC, this gives "
                    "opposing_sides a c BD — a and c lie on opposite "
                    "sides of BD. Lean's SMT closes this silently")
        if rule == "intersection_lines_opposing" and prop == 21:
            return ("engine-only step (intersection_lines_opposing a c "
                    "BD AC, Diagrammatic.lean:267): a and c oppose across "
                    "BD, so BD intersects AC — the premise Lean's SMT "
                    "closes inside euclid_apply (intersection_lines BD "
                    "AC) on the next line")
        if rule == "pasch_4" and prop == 21:
            return ("engine-only step (pasch_4 a e c BD AC, "
                    "Diagrammatic.lean:173): a and c oppose across BD and "
                    "e lies on both lines, so between a e c — e is "
                    "between a and c on side AC. Lean's SMT closes this "
                    "silently")
        if rule == "proposition_20" and prop == 21:
            if args[1] == "b":
                return ("cited step (proposition_20 a b e AB BD AC — "
                        "I.20, two sides exceed the third, in triangle "
                        "a-b-e): |b-a| + |a-e| > |b-e|")
            return ("cited step (proposition_20 e d c BD DC AC — I.20 "
                    "in triangle e-d-c): |d-e| + |e-c| > |d-c|")
        if rule == "proposition_16" and prop == 21:
            if args[0] == "c":
                return ("cited step (proposition_16 c e d b AC BD DC — "
                        "the exterior-angle theorem in triangle c-d-e, "
                        "with b on the extension of e-d past d): the "
                        "exterior angle c:d:b (= b:d:c) exceeds the "
                        "interior angle d:e:c")
            return ("cited step (proposition_16 b a e c AB AC BD — the "
                    "exterior-angle theorem in triangle a-b-e, with c on "
                    "the extension of a-e past e): the exterior angle "
                    "b:e:c exceeds the interior angle e:a:b (= b:a:c, e "
                    "on a-c). The conclusion's why: for the lengths, "
                    "|b-d| + |d-c| < |b-d| + |d-e| + |e-c| (the second "
                    "I.20 step) = |b-e| + |e-c| (between b d e) < "
                    "|b-a| + |a-e| + |e-c| (the first I.20 step) = "
                    "|b-a| + |a-c| (between a e c); for the angles, "
                    "angle b:d:c > angle d:e:c (the first I.16 step) = "
                    "angle b:e:c (d on the ray e->b, between b d e) > "
                    "angle e:a:b = angle b:a:c "
                    "(e between a and c)")
        if rule == "proposition_16" and prop == 18:
            return ("cited step (proposition_16 b c d a BC AC BD, the "
                    "exterior-angle theorem in triangle b-c-d): the "
                    "exterior angle a:d:b (= b:d:a as canonical atoms, a "
                    "on the extension of c-d past d) exceeds the interior "
                    "angle d:c:b — angle a:d:b > angle d:c:b")
        if rule == "proposition_5'" and prop == 18:
            return ("cited step (proposition_5' a b d AB BD AC, the "
                    "isosceles theorem): |a-b| = |a-d| (step 1 cut "
                    "|a-d| = |a-b| off a-c), so angle a:b:d = "
                    "angle a:d:b")
        if rule == "sum_angles_onlyif" and prop == 18:
            return ("engine-only step in this direction "
                    "(sum_angles_onlyif b a c d AB BC, Transfer.lean:90): "
                    "the OTHER direction: the converse of the axiom Lean "
                    "cites at Prop18.lean:17 (sum_angles_if, Transfer.lean:83) — the engine DERIVES "
                    "the angle-sum row angle a:b:c = angle a:b:d + "
                    "angle d:b:c that Lean's SMT discharges silently; "
                    "two converse axioms. The conclusion's "
                    "why: angle a:b:c > angle a:b:d (the part, angle "
                    "d:b:c > 0) = angle a:d:b (I.5') > angle d:c:b "
                    "(I.16) = angle b:c:a (d between a and c) — the goal")
        if rule == "proposition_16" and prop == 17:
            return ("cited step (proposition_16 a b c d AB BC AC, the "
                    "exterior-angle theorem): the exterior angle a:c:d "
                    "exceeds the interior angle a:b:c (= c:b:a as "
                    "canonical atoms) — angle a:c:d > angle a:b:c")
        if rule == "proposition_5'" and prop == 19:
            return ("cited step (proposition_5' a b c AB BC AC, the "
                    "isosceles theorem): under the branch assumption "
                    "|a-c| = |a-b| the base angles are equal — angle "
                    "a:b:c = angle a:c:b = angle b:c:a — contradicting "
                    "the hypothesis angle a:b:c > angle b:c:a")
        if rule == "proposition_18" and prop == 19:
            return ("cited step (proposition_18 a c b AC BC AB — I.18, "
                    "the greater side subtends the greater angle, in "
                    "triangle a-c-b): under the branch assumption "
                    "|a-b| > |a-c| the angle opposite a-b exceeds the "
                    "angle opposite a-c — angle b:c:a > angle a:b:c — "
                    "contradicting the hypothesis angle a:b:c > "
                    "angle b:c:a")
        if rule == "proposition_5'" and prop == 20:
            return ("cited step (proposition_5' a c d AC DC AB, the "
                    "isosceles theorem): |a-c| = |a-d| (the step-2 cut), "
                    "so the base angles of triangle a-c-d are equal — "
                    "angle a:c:d = angle a:d:c")
        if rule == "sum_angles_onlyif" and prop == 20:
            return ("cited step (sum_angles_onlyif c b d a BC DC, "
                    "Transfer.lean:90): a lies inside angle b:c:d "
                    "(between b a d), so angle b:c:d = angle b:c:a + "
                    "angle a:c:d")
        if rule == "proposition_19" and prop == 20:
            return ("cited step (proposition_19 b c d BC DC AB — I.19, "
                    "the greater angle is subtended by the greater "
                    "side, in triangle b-c-d): angle b:c:d = "
                    "angle b:c:a + angle a:c:d (step 5) > angle a:c:d "
                    "(angle b:c:a > 0) = angle a:d:c (step 4, I.5') = "
                    "angle b:d:c (a on the ray d->b), so |b-d| > |b-c|. "
                    "The conclusion's why: |b-a| + |a-c| = |b-a| + "
                    "|a-d| (step 2) = |b-d| (between b a d) > |b-c| — "
                    "because in triangle b-c-d the angle at c exceeds "
                    "the angle at d")
        if rule == "proposition_13" and prop == 17:
            return ("cited step (proposition_13 a c b d AC BC, the "
                    "linear pair): angle b:c:a + angle a:c:d = two right "
                    "angles. With angle a:c:d > angle a:b:c (the I.16 "
                    "step), angle a:b:c + angle b:c:a < angle a:c:d + "
                    "angle b:c:a = two right angles — the goal")
        if rule == "sum_angles_onlyif" and prop == 16:
            return ("engine-only step (sum_angles_onlyif c a d f AC BC, "
                    "Transfer.lean:90): angle a:c:d = angle a:c:f + "
                    "angle f:c:d, with angle f:c:d > 0 — so the WHOLE "
                    "(the exterior angle a:c:d) exceeds the PART "
                    "(angle a:c:f = angle e:c:f = angle b:a:c by the "
                    "SAS, proposition_4), hence angle a:c:d > "
                    "angle b:a:c")
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


def verify14(xy):
    """I.14, drawn in the TRUE configuration (the by_contra's assumption
    BC != BD is refuted, hence not drawable — see the case notes): b lies
    between c and d on one straight line (the theorem's content, BC = BD),
    c and d on opposing sides of AB, a off the line c-d, the angle sum
    angle a:b:c + angle a:b:d = 180 degrees numerically, general position
    (angle a:b:c not right), and e (extend_point BC c b) between b and d,
    distinct from d."""
    dist = lambda p, q: _norm(_sub(xy[p], xy[q]))

    def side(p, l1, l2):
        dvec = _sub(xy[l2], xy[l1])
        return (dvec[0] * (xy[p][1] - xy[l1][1])
                - dvec[1] * (xy[p][0] - xy[l1][0]))

    ang_sum = _ang(xy, "a", "b", "c") + _ang(xy, "a", "b", "d")
    return [
        ("I.14: between c b d (d on the extension of c-b beyond b — the "
         "theorem's content, drawn)",
         abs(dist("c", "b") + dist("b", "d") - dist("c", "d")) < EPS),
        ("I.14: c and d on opposing sides of AB",
         side("c", "a", "b") * side("d", "a", "b") < 0),
        ("I.14: a off the line c-d (distance > 0)",
         abs(side("a", "c", "d")) / dist("c", "d") > EPS),
        ("I.14: angle a-b-c + angle a-b-d = 180 degrees",
         abs(ang_sum - math.pi) < EPS),
        ("I.14: general position (angle a-b-c not right)",
         abs(_ang(xy, "a", "b", "c") - math.pi / 2) > EPS),
        ("I.14: between c b e (extend_point BC c b)",
         abs(dist("c", "b") + dist("b", "e") - dist("c", "e")) < EPS),
        ("I.14: e between b and d, distinct from both",
         abs(dist("b", "e") + dist("e", "d") - dist("b", "d")) < EPS),
    ]


def verify15(xy):
    """I.15 (vertical angles): the drawn facts are the hypotheses — both
    betweens (a e b on AB, d e c on CD), e on both lines, the lines
    distinct (CD != AB, i.e. not parallel) — plus the two conclusion
    equalities numerically (angle a:e:c = angle d:e:b and angle c:e:b =
    angle a:e:d), and general position: the crossing is NOT right, so the
    two vertical pairs visibly differ. A perpendicular crossing satisfies
    the theorem geometrically but this check rejects it (both pairs would
    be pairs of right angles and the conclusion would be invisible in the
    diagram)."""
    dist = lambda p, q: _norm(_sub(xy[p], xy[q]))

    def cross(p, q, r):
        """Cross product of (q-p) x (r-p): 0 iff p, q, r collinear."""
        return ((xy[q][0] - xy[p][0]) * (xy[r][1] - xy[p][1])
                - (xy[q][1] - xy[p][1]) * (xy[r][0] - xy[p][0]))

    return [
        ("I.15: between a e b",
         abs(dist("a", "e") + dist("e", "b") - dist("a", "b")) < EPS),
        ("I.15: between d e c",
         abs(dist("d", "e") + dist("e", "c") - dist("d", "c")) < EPS),
        ("I.15: e on line AB",
         abs(cross("a", "b", "e")) < EPS * dist("a", "b")),
        ("I.15: e on line CD",
         abs(cross("c", "d", "e")) < EPS * dist("c", "d")),
        ("I.15: CD != AB (the lines cross, not parallel)",
         abs(cross("a", "b", "c")) > EPS * dist("a", "b")
         * dist("b", "c")),
        ("I.15: angle a:e:c = angle d:e:b (the conclusion, numerically)",
         abs(_ang(xy, "a", "e", "c") - _ang(xy, "d", "e", "b")) < EPS),
        ("I.15: angle c:e:b = angle a:e:d (the conclusion, numerically)",
         abs(_ang(xy, "c", "e", "b") - _ang(xy, "a", "e", "d")) < EPS),
        ("I.15: general position (crossing not right, so the two "
         "vertical pairs visibly differ)",
         abs(_ang(xy, "a", "e", "c") - math.pi / 2) > EPS),
    ]


def verify16(xy):
    """I.16 (exterior angle, second conjunct — Euclid's own proof):
    the drawn facts are the hypotheses (between b c d, the scalene
    triangle) plus the construction (e the midpoint of a-c; f on the
    ray b->e with e the midpoint of b-f, i.e. between b e f and
    |b-e| = |e-f|; f' past f with |e-f'| > |b-e|). The conclusions
    numerically, with a visible margin: angle b:a:c = angle e:c:f
    (the SAS, proposition_4), angle e:c:f is a proper PART of angle
    a:c:d (f strictly inside the exterior angle, and the part-whole
    split angle a:c:d = angle a:c:f + angle f:c:d), and the goal
    angle a:c:d > angle b:a:c by a visible margin (the excess
    angle f:c:d is at least 0.5 rad, not a sliver)."""
    dist = lambda p, q: _norm(_sub(xy[p], xy[q]))

    def cross(p, q, r):
        """Cross product of (q-p) x (r-p): 0 iff p, q, r collinear."""
        return ((xy[q][0] - xy[p][0]) * (xy[r][1] - xy[p][1])
                - (xy[q][1] - xy[p][1]) * (xy[r][0] - xy[p][0]))

    def side(p, l1, l2):
        return cross(l1, l2, p)

    whole = _ang(xy, "a", "c", "d")
    part = _ang(xy, "b", "a", "c")
    copied = _ang(xy, "e", "c", "f")
    rest = _ang(xy, "f", "c", "d")
    return [
        ("I.16: between b c d (d on the extension of b-c beyond c)",
         abs(dist("b", "c") + dist("c", "d") - dist("b", "d")) < EPS),
        ("I.16: scalene triangle (all three side lengths distinct)",
         len({round(dist(p, q), 9)
              for p, q in (("a", "b"), ("b", "c"), ("a", "c"))}) == 3),
        ("I.16: a off BC (a genuine triangle)",
         abs(cross("b", "c", "a")) > EPS * dist("b", "c")
         * dist("a", "c")),
        ("I.16: e the midpoint of a-c (between a e c, |a-e| = |e-c|)",
         abs(dist("a", "e") + dist("e", "c") - dist("a", "c")) < EPS
         and abs(dist("a", "e") - dist("e", "c")) < EPS),
        ("I.16: e the midpoint of b-f (between b e f, |b-e| = |e-f|)",
         abs(dist("b", "e") + dist("e", "f") - dist("b", "f")) < EPS
         and abs(dist("b", "e") - dist("e", "f")) < EPS),
        ("I.16: f' past f on the ray (between e f f', |e-f'| > |b-e|)",
         abs(dist("e", "f") + dist("f", "f'") - dist("e", "f'")) < EPS
         and dist("e", "f'") > dist("b", "e") + EPS),
        ("I.16: angle b:a:c = angle e:c:f (the SAS, numerically)",
         abs(part - copied) < EPS),
        ("I.16: f strictly inside angle a:c:d (e:c:f a proper part — "
         "f on d's side of AC and on a's side of BC)",
         side("f", "c", "a") * side("d", "c", "a") > 0
         and side("f", "c", "d") * side("a", "c", "d") > 0),
        ("I.16: part-whole: angle a:c:d = angle a:c:f + angle f:c:d",
         abs(whole - (_ang(xy, "a", "c", "f") + rest)) < EPS),
        ("I.16: GOAL angle a:c:d > angle b:a:c, with a visible margin "
         "(the excess angle f:c:d at least 0.5 rad)",
         whole > part + 0.5),
    ]


def verify17(xy):
    """I.17 (any two angles of a triangle sum to less than two right):
    the drawn facts are the hypotheses (the scalene triangle a-b-c) plus
    the construction (d on the extension of b-c beyond c, between b c d).
    The conclusions numerically: the linear pair angle a:c:b + angle
    a:c:d = 180 deg exactly (proposition_13); the I.16 step angle a:c:d
    > angle a:b:c; and the goal angle a:b:c + angle b:c:a < 180 deg with
    a visible margin (at least 0.5 rad short, not a sliver)."""
    dist = lambda p, q: _norm(_sub(xy[p], xy[q]))

    def cross(p, q, r):
        """Cross product of (q-p) x (r-p): 0 iff p, q, r collinear."""
        return ((xy[q][0] - xy[p][0]) * (xy[r][1] - xy[p][1])
                - (xy[q][1] - xy[p][1]) * (xy[r][0] - xy[p][0]))

    at_b = _ang(xy, "a", "b", "c")
    at_c = _ang(xy, "b", "c", "a")
    ext = _ang(xy, "a", "c", "d")
    return [
        ("I.17: between b c d (d on the extension of b-c beyond c)",
         abs(dist("b", "c") + dist("c", "d") - dist("b", "d")) < EPS),
        ("I.17: scalene triangle (all three side lengths distinct)",
         len({round(dist(p, q), 9)
              for p, q in (("a", "b"), ("b", "c"), ("a", "c"))}) == 3),
        ("I.17: a off BC (a genuine triangle)",
         abs(cross("b", "c", "a")) > EPS * dist("b", "c")
         * dist("a", "c")),
        ("I.17: linear pair angle a:c:b + angle a:c:d = 180 deg "
         "(proposition_13, numerically)",
         abs(at_c + ext - math.pi) < EPS),
        ("I.17: I.16 step angle a:c:d > angle a:b:c",
         ext > at_b + EPS),
        ("I.17: GOAL angle a:b:c + angle b:c:a < 180 deg, with a visible "
         "margin (at least 0.5 rad short)",
         at_b + at_c < math.pi - 0.5),
    ]


def verify18(xy):
    """I.18 (in a triangle, the greater side subtends the greater angle):
    the drawn facts are the hypotheses (the scalene triangle a-b-c with
    |a-c| > |a-b|) plus the construction (d on segment a-c with
    |a-d| = |a-b|, between a d c). The conclusions numerically: the I.5'
    step angle a:b:d = angle a:d:b; the I.16 step angle a:d:b >
    angle d:c:b (exterior angle of triangle b-c-d); the angle-split
    angle a:b:c = angle a:b:d + angle d:b:c (d inside the angle at b);
    and the goal angle a:b:c > angle b:c:a with a visible margin."""
    dist = lambda p, q: _norm(_sub(xy[p], xy[q]))
    abc = _ang(xy, "a", "b", "c")
    abd = _ang(xy, "a", "b", "d")
    dbc = _ang(xy, "d", "b", "c")
    adb = _ang(xy, "a", "d", "b")
    dcb = _ang(xy, "d", "c", "b")
    bca = _ang(xy, "b", "c", "a")
    return [
        ("I.18: between a d c (d on segment a-c)",
         abs(dist("a", "d") + dist("d", "c") - dist("a", "c")) < EPS),
        ("I.18: |a-d| = |a-b| (proposition_3 cut)",
         abs(dist("a", "d") - dist("a", "b")) < EPS),
        ("I.18: hypothesis |a-c| > |a-b| with a visible margin",
         dist("a", "c") > dist("a", "b") + 0.2),
        ("I.18: I.5' step angle a:b:d = angle a:d:b",
         abs(abd - adb) < EPS),
        ("I.18: I.16 step angle a:d:b > angle d:c:b (exterior angle of "
         "triangle b-c-d)", adb > dcb + EPS),
        ("I.18: angle a:b:c = angle a:b:d + angle d:b:c (d inside the "
         "angle at b)", abs(abc - (abd + dbc)) < EPS),
        ("I.18: GOAL angle a:b:c > angle b:c:a, with a visible margin "
         "(at least 0.15 rad)", abc > bca + 0.15),
    ]


def verify19(xy):
    """I.19 (the greater angle is subtended by the greater side — the
    converse of I.18, a by_contra drawn in the TRUE configuration):
    the drawn facts are the hypotheses — the scalene triangle a-b-c
    with a off BC and angle a:b:c > angle b:c:a by a visible margin —
    plus the conclusion numerically: |a-c| > |a-b| by a visible
    margin. Both refuted branch assumptions (|a-c| = |a-b| and
    |a-b| > |a-c|) fail these checks, as they must."""
    dist = lambda p, q: _norm(_sub(xy[p], xy[q]))

    def cross(p, q, r):
        return ((xy[q][0] - xy[p][0]) * (xy[r][1] - xy[p][1])
                - (xy[q][1] - xy[p][1]) * (xy[r][0] - xy[p][0]))

    abc = _ang(xy, "a", "b", "c")
    bca = _ang(xy, "b", "c", "a")
    return [
        ("I.19: scalene triangle (all three side lengths distinct)",
         len({round(dist(p, q), 9)
              for p, q in (("a", "b"), ("b", "c"), ("a", "c"))}) == 3),
        ("I.19: a off BC (a genuine triangle)",
         abs(cross("b", "c", "a")) > EPS * dist("b", "c")
         * dist("a", "c")),
        ("I.19: hypothesis angle a:b:c > angle b:c:a, with a visible "
         "margin (at least 0.15 rad)", abc > bca + 0.15),
        ("I.19: GOAL |a-c| > |a-b|, with a visible margin (at least "
         "0.2)", dist("a", "c") > dist("a", "b") + 0.2),
    ]


def verify20(xy):
    """I.20 (two sides of a triangle together exceed the third,
    |b-a| + |a-c| > |b-c|): the drawn facts are the hypothesis triangle
    a-b-c plus the construction — d' on the ray b->a beyond a
    (extend_point_longer) and d on segment a-d' with |a-d| = |a-c|
    (proposition_3), so between b a d. The conclusions numerically: the
    I.5' step angle a:c:d = angle a:d:c; the angle-split
    angle b:c:d = angle b:c:a + angle a:c:d (a inside the angle at c);
    the I.19 step angle b:c:d > angle b:d:c; the length sum
    |b-d| = |b-a| + |a-c|; and the goal with a visible margin."""
    dist = lambda p, q: _norm(_sub(xy[p], xy[q]))
    acd = _ang(xy, "a", "c", "d")
    adc = _ang(xy, "a", "d", "c")
    bcd = _ang(xy, "b", "c", "d")
    bca = _ang(xy, "b", "c", "a")
    bdc = _ang(xy, "b", "d", "c")
    return [
        ("I.20: between b a d (d on the ray b->a beyond a)",
         abs(dist("b", "a") + dist("a", "d") - dist("b", "d")) < EPS),
        ("I.20: |a-d| = |a-c| (proposition_3 cut)",
         abs(dist("a", "d") - dist("a", "c")) < EPS),
        ("I.20: I.5' step angle a:c:d = angle a:d:c",
         abs(acd - adc) < EPS),
        ("I.20: angle b:c:d = angle b:c:a + angle a:c:d (a inside the "
         "angle at c)", abs(bcd - (bca + acd)) < EPS),
        ("I.20: I.19 step angle b:c:d > angle b:d:c",
         bcd > bdc + EPS),
        ("I.20: |b-d| = |b-a| + |a-c| (the two sides laid end to end)",
         abs(dist("b", "d") - (dist("b", "a") + dist("a", "c"))) < EPS),
        ("I.20: GOAL |b-a| + |a-c| > |b-c|, with a visible margin (at "
         "least 0.2)",
         dist("b", "a") + dist("a", "c") > dist("b", "c") + 0.2),
    ]


def verify21(xy):
    """I.21 (lines from inside a triangle: for d strictly inside
    triangle a-b-c, |b-d| + |d-c| < |b-a| + |a-c| and
    angle b:d:c > angle b:a:c): the drawn facts are the hypotheses — the
    scalene triangle a-b-c with d strictly inside (on a's side of BC,
    c's side of AB, b's side of AC) — plus the construction (e where BD
    meets AC, with between a e c and between b d e). The conclusions
    numerically: the two I.20 steps |b-a| + |a-e| > |b-e| and
    |d-e| + |e-c| > |d-c|; the two I.16 exterior-angle steps
    angle b:d:c > angle d:e:c and angle b:e:c > angle b:a:c; and both
    goals with visible margins."""
    dist = lambda p, q: _norm(_sub(xy[p], xy[q]))

    def cross(l1, l2, p):
        return ((xy[l2][0] - xy[l1][0]) * (xy[p][1] - xy[l1][1])
                - (xy[l2][1] - xy[l1][1]) * (xy[p][0] - xy[l1][0]))

    bdc = _ang(xy, "b", "d", "c")
    dec = _ang(xy, "d", "e", "c")
    bec = _ang(xy, "b", "e", "c")
    bac = _ang(xy, "b", "a", "c")
    return [
        ("I.21: d on a's side of BC (inside, hyp a.sameSide d BC)",
         cross("b", "c", "d") * cross("b", "c", "a") > 0),
        ("I.21: d on c's side of AB (inside, hyp c.sameSide d AB)",
         cross("a", "b", "d") * cross("a", "b", "c") > 0),
        ("I.21: d on b's side of AC (inside, hyp b.sameSide d AC)",
         cross("a", "c", "d") * cross("a", "c", "b") > 0),
        ("I.21: e on AC between a and c",
         abs(dist("a", "e") + dist("e", "c") - dist("a", "c")) < EPS),
        ("I.21: between b d e (the by_contra's conclusion)",
         abs(dist("b", "d") + dist("d", "e") - dist("b", "e")) < EPS),
        ("I.21: first I.20 step |b-a| + |a-e| > |b-e|",
         dist("b", "a") + dist("a", "e") > dist("b", "e") + EPS),
        ("I.21: second I.20 step |d-e| + |e-c| > |d-c|",
         dist("d", "e") + dist("e", "c") > dist("d", "c") + EPS),
        ("I.21: first I.16 step angle b:d:c > angle d:e:c (exterior "
         "angle of triangle c-d-e)", bdc > dec + EPS),
        ("I.21: second I.16 step angle b:e:c > angle b:a:c (exterior "
         "angle of triangle a-b-e)", bec > bac + EPS),
        ("I.21: GOAL |b-d| + |d-c| < |b-a| + |a-c|, with a visible "
         "margin (at least 0.2)",
         dist("b", "d") + dist("d", "c")
         < dist("b", "a") + dist("a", "c") - 0.2),
        ("I.21: GOAL angle b:d:c > angle b:a:c, with a visible margin "
         "(at least 0.15 rad)", bdc > bac + 0.15),
    ]


def verify22(xy):
    """I.22 (a triangle from three lengths): the drawn facts are the
    hypotheses — three segments a-a', b-b', c-c' apart from the
    construction line DE, with each triangle inequality holding by a
    visible margin (>= 0.15) — plus the construction: the chain
    d, f, g, h on DE with the three proposition_3 cuts, the two circles
    (DKL centre f radius |a-a'|, KLH centre g radius |c-c'|), i the
    second meet of DE with KLH (the drawn by_cases branch 2: i between
    g and f, since |c-c'| < |b-b'|), and k the circles' meet. The goal
    numerically: |f-k| = |a-a'|, |f-g| = |b-b'|, |k-g| = |c-c'|."""
    dist = lambda p, q: _norm(_sub(xy[p], xy[q]))
    aa, bb, cc = dist("a", "a'"), dist("b", "b'"), dist("c", "c'")

    def off_de(p):
        # distance of p from the construction line DE (through d, e')
        d_, e_ = xy["d"], xy["e'"]
        v = _sub(e_, d_)
        return abs(v[0] * (xy[p][1] - d_[1]) - v[1] * (xy[p][0] - d_[0]))
    return [
        ("I.22: |a-a'| + |b-b'| > |c-c'| with margin >= 0.15",
         aa + bb > cc + 0.15),
        ("I.22: |a-a'| + |c-c'| > |b-b'| with margin >= 0.15",
         aa + cc > bb + 0.15),
        ("I.22: |b-b'| + |c-c'| > |a-a'| with margin >= 0.15",
         bb + cc > aa + 0.15),
        ("I.22: the three given segments stand apart from DE (every "
         "given point at least 0.5 off the line)",
         all(off_de(p) > 0.5
             for p in ("a", "a'", "b", "b'", "c", "c'"))),
        ("I.22: the chain order d, f, g, h, e on DE",
         abs(dist("d", "f") + dist("f", "g") + dist("g", "h")
             + dist("h", "e") - dist("d", "e")) < EPS),
        ("I.22: |d-f| = |a-a'| (the first proposition_3 cut)",
         abs(dist("d", "f") - aa) < EPS),
        ("I.22: |f-g| = |b-b'| (the second proposition_3 cut)",
         abs(dist("f", "g") - bb) < EPS),
        ("I.22: |g-h| = |c-c'| (the third proposition_3 cut)",
         abs(dist("g", "h") - cc) < EPS),
        ("I.22: |d-e| exceeds each given length (the extend_point_longer "
         "bounds)", min(dist("d", "e") - aa, dist("d", "e") - bb,
                        dist("d", "e") - cc) > 0.1),
        ("I.22: i between g and h on DE with |g-i| = |g-h| (the second "
         "meet)",
         abs(dist("i", "g") + dist("g", "h") - dist("i", "h")) < EPS
         and abs(dist("g", "i") - dist("g", "h")) < EPS),
        ("I.22: the drawn by_cases branch is branch 2 — i between g "
         "and f, since |c-c'| < |b-b'| with a visible margin",
         cc < bb - 0.15
         and abs(dist("g", "i") + dist("i", "f") - dist("g", "f")) < EPS),
        ("I.22: GOAL |f-k| = |a-a'| (k on circle DKL)",
         abs(dist("f", "k") - aa) < EPS),
        ("I.22: GOAL |f-g| = |b-b'| (the witness g)",
         abs(dist("f", "g") - bb) < EPS),
        ("I.22: GOAL |k-g| = |c-c'| (k on circle KLH)",
         abs(dist("k", "g") - cc) < EPS),
    ]


def verify23(xy):
    """I.23 (copy an angle onto a line at a point): the drawn facts are
    the hypotheses — the target line AB and the given angle d:c:e apart
    from it, visibly non-degenerate (60 degrees, off both 0 and
    straight; d off CE, the drawn main branch) — plus the main branch's
    construction: g on AB on the ray a->b with |a-g| = |c-e|, f with
    |a-f| = |c-d| and |f-g| = |e-d| on the given angle's side of AB.
    The goal numerically: angle f:a:b = angle d:c:e, f != a. The
    degenerate branch (d on CE) is handled by the engine, not drawn."""
    dist = lambda p, q: _norm(_sub(xy[p], xy[q]))

    def cross(l1, l2, p):
        return ((xy[l2][0] - xy[l1][0]) * (xy[p][1] - xy[l1][1])
                - (xy[l2][1] - xy[l1][1]) * (xy[p][0] - xy[l1][0]))

    def off_line(p, l1, l2):
        v = _sub(xy[l2], xy[l1])
        return abs(v[0] * (xy[p][1] - xy[l1][1])
                   - v[1] * (xy[p][0] - xy[l1][0])) / _norm(v)
    dce = _ang(xy, "d", "c", "e")
    fab = _ang(xy, "f", "a", "b")
    return [
        ("I.23: the given angle is non-degenerate: 0.2 rad < angle "
         "d:c:e < pi - 0.2 (neither 0 nor straight)",
         0.2 < dce < math.pi - 0.2),
        ("I.23: d off CE (the drawn main branch, not the degenerate "
         "case)", abs(cross("c", "e", "d")) > EPS),
        ("I.23: the given angle stands apart from AB (c, d, e at least "
         "0.5 off the line)",
         all(off_line(p, "a", "b") > 0.5 for p in ("c", "d", "e"))),
        ("I.23: g on AB (the I.22' conclusion g.onLine AB)",
         abs(cross("a", "b", "g")) < EPS * dist("a", "b")),
        ("I.23: |a-g| = |c-e| and g on the ray a->b",
         abs(dist("a", "g") - dist("c", "e")) < EPS
         and (xy["g"][0] - xy["a"][0]) * (xy["b"][0] - xy["a"][0])
         + (xy["g"][1] - xy["a"][1]) * (xy["b"][1] - xy["a"][1]) > 0),
        ("I.23: |a-f| = |c-d| and |f-g| = |e-d| (the I.22' "
         "construction)",
         abs(dist("a", "f") - dist("c", "d")) < EPS
         and abs(dist("f", "g") - dist("e", "d")) < EPS),
        ("I.23: f off AB, on the given angle's side of it",
         abs(cross("a", "b", "f")) > EPS
         and cross("a", "b", "f") * cross("a", "b", "c") > 0),
        ("I.23: f != a", dist("f", "a") > 0.1),
        ("I.23: GOAL angle f:a:b = angle d:c:e",
         abs(fab - dce) < EPS),
    ]


def verify24(xy):
    """I.24 (the hinge theorem): the drawn facts are the hypotheses —
    two triangles a-b-c and d-e-f apart, with |a-b| = |d-e| and
    |a-c| = |d-f| exactly and angle b:a:c > angle e:d:f by a visible
    margin (>= 5 degrees) — plus the drawn construction: g' on f's
    side of DE with angle g':d:e = angle b:a:c (the proposition_23'
    copy), g'' past g' on DG, and g the proposition_3 cut with
    |d-g| = |a-c|. The drawn case of the by_cases is branch 1: d and
    g on the same side of EF. The goal numerically: |b-c| > |e-f|,
    with |b-c| = |e-g| (the SAS copy). The second case ('Omitted by
    Euclid') is handled by the engine, not drawn."""
    dist = lambda p, q: _norm(_sub(xy[p], xy[q]))

    def cross(l1, l2, p):
        return ((xy[l2][0] - xy[l1][0]) * (xy[p][1] - xy[l1][1])
                - (xy[l2][1] - xy[l1][1]) * (xy[p][0] - xy[l1][0]))

    def mindist(P, Q):
        return min(dist(p, q) for p in P for q in Q)
    bac = _ang(xy, "b", "a", "c")
    edf = _ang(xy, "e", "d", "f")
    gde = _ang(xy, "g'", "d", "e")
    return [
        ("I.24: |a-b| = |d-e| (hypothesis, exact)",
         abs(dist("a", "b") - dist("d", "e")) < EPS),
        ("I.24: |a-c| = |d-f| (hypothesis, exact)",
         abs(dist("a", "c") - dist("d", "f")) < EPS),
        ("I.24: angle b:a:c > angle e:d:f with a visible margin "
         "(>= 5 degrees)", bac > edf + math.radians(5.0)),
        ("I.24: genuine triangles (c off AB, f off DE)",
         abs(cross("a", "b", "c")) > EPS
         and abs(cross("d", "e", "f")) > EPS),
        ("I.24: the two triangles stand apart (every cross-pair of "
         "vertices at least 0.5 apart)",
         mindist(("a", "b", "c"), ("d", "e", "f")) > 0.5),
        ("I.24: angle g':d:e = angle b:a:c (the proposition_23' copy)",
         abs(gde - bac) < EPS),
        ("I.24: g' on f's side of DE (the resolved disjunct)",
         cross("d", "e", "g'") * cross("d", "e", "f") > 0),
        ("I.24: the chain order d, g', g'' on DG with |g'-g''| > |a-c| "
         "(the extend_point_longer bound)",
         abs(dist("d", "g'") + dist("g'", "g''") - dist("d", "g''")) < EPS
         and dist("g'", "g''") > dist("a", "c") + 0.1),
        ("I.24: |d-g| = |a-c| with g between d and g'' (the "
         "proposition_3 cut)",
         abs(dist("d", "g") - dist("a", "c")) < EPS
         and abs(dist("d", "g") + dist("g", "g''")
                 - dist("d", "g''")) < EPS),
        ("I.24: the drawn case is branch 1 — d and g on the same side "
         "of EF, with a visible margin",
         cross("e", "f", "d") * cross("e", "f", "g") > 0
         and abs(cross("e", "f", "g")) / dist("e", "f") > 0.05),
        ("I.24: |b-c| = |e-g| (the proposition_4 copy)",
         abs(dist("b", "c") - dist("e", "g")) < EPS),
        ("I.24: GOAL |b-c| > |e-f| with a visible margin (>= 0.15)",
         dist("b", "c") > dist("e", "f") + 0.15),
    ]


def verify25(xy):
    """I.25 (the converse of the hinge theorem, by contradiction): the
    drawn facts are the hypotheses — two triangles a-b-c and d-e-f
    apart, with |a-b| = |d-e| and |a-c| = |d-f| exactly and |b-c| >
    |e-f| by a visible margin — plus the conclusion numerically:
    angle b:a:c > angle e:d:f (the TRUE configuration; both refuted
    branch assumptions — the angles equal, or the reversed comparison
    — fail these checks, as they must)."""
    dist = lambda p, q: _norm(_sub(xy[p], xy[q]))

    def cross(l1, l2, p):
        return ((xy[l2][0] - xy[l1][0]) * (xy[p][1] - xy[l1][1])
                - (xy[l2][1] - xy[l1][1]) * (xy[p][0] - xy[l1][0]))
    bac = _ang(xy, "b", "a", "c")
    edf = _ang(xy, "e", "d", "f")
    return [
        ("I.25: |a-b| = |d-e| (hypothesis, exact)",
         abs(dist("a", "b") - dist("d", "e")) < EPS),
        ("I.25: |a-c| = |d-f| (hypothesis, exact)",
         abs(dist("a", "c") - dist("d", "f")) < EPS),
        ("I.25: |b-c| > |e-f| (hypothesis) with a visible margin "
         "(>= 0.15)", dist("b", "c") > dist("e", "f") + 0.15),
        ("I.25: genuine triangles (c off AB, f off DE)",
         abs(cross("a", "b", "c")) > EPS
         and abs(cross("d", "e", "f")) > EPS),
        ("I.25: the two triangles stand apart (every cross-pair of "
         "vertices at least 0.5 apart)",
         min(dist(p, q) for p in ("a", "b", "c")
           for q in ("d", "e", "f")) > 0.5),
        ("I.25: GOAL angle b:a:c > angle e:d:f with a visible margin "
         "(>= 5 degrees)", bac > edf + math.radians(5.0)),
    ]


def verify6(xy):
    """I.6 (equal base angles make equal sides — the converse of I.5, a
    by_contra drawn in the TRUE configuration): the drawn facts are the
    hypothesis — a genuine triangle a-b-c with angle a:b:c = angle a:c:b
    — plus the conclusion numerically: |a-b| = |a-c|. The TRUE
    configuration is forced to be isosceles, so the placement puts a
    exactly on the perpendicular bisector of b-c. Both refuted branch
    assumptions (|a-b| > |a-c| and |a-c| > |a-b|) fail these checks, as
    they must."""
    dist = lambda p, q: _norm(_sub(xy[p], xy[q]))

    def cross(p, q, r):
        return ((xy[q][0] - xy[p][0]) * (xy[r][1] - xy[p][1])
                - (xy[q][1] - xy[p][1]) * (xy[r][0] - xy[p][0]))

    abc = _ang(xy, "a", "b", "c")
    acb = _ang(xy, "a", "c", "b")
    return [
        ("I.6: a off BC (a genuine triangle)",
         abs(cross("b", "c", "a")) > EPS * dist("b", "c")
         * dist("a", "c")),
        ("I.6: base angles visibly off the special angles (0.1 rad "
         "from each of 45/60/90 degrees)",
         all(abs(abc - math.radians(x)) > 0.1 for x in (45, 60, 90))),
        ("I.6: hypothesis angle a:b:c = angle a:c:b (to 1e-9)",
         abs(abc - acb) < EPS),
        ("I.6: GOAL |a-b| = |a-c| (to 1e-9)",
         abs(dist("a", "b") - dist("a", "c")) < EPS),
        ("I.6: not equilateral (|b-c| visibly different from |a-b|, at "
         "least 0.1)", abs(dist("b", "c") - dist("a", "b")) > 0.1),
    ]


def verify7(xy):
    """I.7 (no two distinct points on the same side of AB at the same
    distances from a and b — conclusion False): the hypotheses are
    jointly impossible (that IS the theorem), so no placement satisfies
    them all. The drawn placement keeps every drawable hypothesis exact
    — c and d on the same side of AB, c != d, and |a-c| = |a-d| — and
    the un-drawable one visibly fails: |c-b| > |d-b| (had both
    equalities held, d would be c's mirror across AB, on the OPPOSITE
    side)."""
    dist = lambda p, q: _norm(_sub(xy[p], xy[q]))

    def side(p, l1, l2):
        return ((xy[p][0] - xy[l1][0]) * (xy[l2][1] - xy[l1][1])
                - (xy[p][1] - xy[l1][1]) * (xy[l2][0] - xy[l1][0]))

    return [
        ("I.7: c and d on the SAME side of AB (hypothesis same_side "
         "c d AB)", side("c", "a", "b") * side("d", "a", "b") > 0),
        ("I.7: c != d, visibly (at least 0.2 apart)",
         dist("c", "d") > 0.2),
        ("I.7: hypothesis |a-c| = |a-d| (to 1e-9 — d on the circle "
         "centre a through c)",
         abs(dist("a", "c") - dist("a", "d")) < EPS),
        ("I.7: the un-drawable hypothesis |c-b| = |d-b| visibly fails "
         "(|c-b| > |d-b| by at least 0.2) — the two equalities plus "
         "same-side are the contradiction the proof derives",
         dist("c", "b") > dist("d", "b") + 0.2),
        ("I.7: d strictly inside triangle a-c-b (Euclid's figure)",
         side("d", "a", "c") * side("b", "a", "c") > 0
         and side("d", "c", "b") * side("a", "c", "b") > 0),
    ]


def verify8(xy):
    """I.8 (SSS congruence): two genuine scalene triangles with the
    three hypothesis side equalities to 1e-9; the superposition of
    step 1 lands EXACTLY — b' = f (step 2's assert) and c' = d (the
    by_contra's conclusion) — which is the drawn truth; the goal angle
    equality then holds to 1e-9."""
    dist = lambda p, q: _norm(_sub(xy[p], xy[q]))

    def cross(p, q, r):
        return ((xy[q][0] - xy[p][0]) * (xy[r][1] - xy[p][1])
                - (xy[q][1] - xy[p][1]) * (xy[r][0] - xy[p][0]))

    sides1 = [dist("a", "b"), dist("b", "c"), dist("a", "c")]
    return [
        ("I.8: c off AB (a genuine triangle)",
         abs(cross("a", "b", "c")) > EPS * dist("a", "b")
         * dist("b", "c")),
        ("I.8: f off DE (a genuine triangle)",
         abs(cross("d", "e", "f")) > EPS * dist("d", "e")
         * dist("e", "f")),
        ("I.8: hypothesis |a-b| = |d-e| (to 1e-9)",
         abs(dist("a", "b") - dist("d", "e")) < EPS),
        ("I.8: hypothesis |a-c| = |d-f| (to 1e-9)",
         abs(dist("a", "c") - dist("d", "f")) < EPS),
        ("I.8: hypothesis |b-c| = |e-f| (to 1e-9)",
         abs(dist("b", "c") - dist("e", "f")) < EPS),
        ("I.8: the superposition lands b' on f (to 1e-9) — step 2's "
         "assert", dist("b'", "f") < EPS),
        ("I.8: the superposition lands c' on d (to 1e-9) — the "
         "by_contra's conclusion, exact in the TRUE configuration",
         dist("c'", "d") < EPS),
        ("I.8: GOAL angle b:a:c = angle e:d:f (to 1e-9)",
         abs(_ang(xy, "b", "a", "c") - _ang(xy, "e", "d", "f")) < EPS),
        ("I.8: triangles scalene (side lengths pairwise at least 0.1 "
         "apart)", all(abs(x - y) > 0.1 for i, x in enumerate(sides1)
                       for y in sides1[i + 1:])),
    ]


def verify9(xy):
    """I.9 (bisect an angle): a genuine angle b:v:c; the constructions
    land where the rules put them (d on segment v-b at the step-1
    sample distance |v-c|/2, |v-e| = |v-d| by the proposition_3 cut,
    f the equilateral apex on DE opposite v); and the GOAL holds to
    1e-9: angle b:v:f = angle c:v:f with v != f."""
    dist = lambda p, q: _norm(_sub(xy[p], xy[q]))

    def side(p, l1, l2):
        return ((xy[p][0] - xy[l1][0]) * (xy[l2][1] - xy[l1][1])
                - (xy[p][1] - xy[l1][1]) * (xy[l2][0] - xy[l1][0]))

    bvc = _ang(xy, "b", "v", "c")
    return [
        ("I.9: a genuine angle b:v:c (AB != AC)",
         abs(side("c", "v", "b")) > EPS * dist("v", "b")
         * dist("v", "c")),
        ("I.9: angle b:v:c visibly off the special angles (0.1 rad "
         "from each of 45/60/90 degrees)",
         all(abs(bvc - math.radians(x)) > 0.1 for x in (45, 60, 90))),
        ("I.9: d on segment v-b (the step-1 sample)",
         abs(dist("v", "d") + dist("d", "b") - dist("v", "b")) < EPS),
        ("I.9: |v-d| = |v-c|/2 (the step-1 sample distance)",
         abs(dist("v", "d") - dist("v", "c") / 2.0) < EPS),
        ("I.9: |v-e| = |v-d| (the proposition_3 cut, to 1e-9)",
         abs(dist("v", "e") - dist("v", "d")) < EPS),
        ("I.9: e on segment v-c",
         abs(dist("v", "e") + dist("e", "c") - dist("v", "c")) < EPS),
        ("I.9: f the equilateral apex on DE (|f-d| = |f-e| = |d-e|, "
         "to 1e-9)",
         abs(dist("f", "d") - dist("d", "e")) < EPS
         and abs(dist("f", "e") - dist("d", "e")) < EPS),
        ("I.9: f on the side of DE opposite v",
         side("f", "d", "e") * side("v", "d", "e") < 0),
        ("I.9: f off AB and f off AC (the h-sm hypotheses behind the "
         "SSS step)",
         abs(side("f", "v", "b")) > EPS * dist("v", "b")
         * dist("v", "f")
         and abs(side("f", "v", "c")) > EPS * dist("v", "c")
         * dist("v", "f")),
        ("I.9: GOAL angle b:v:f = angle c:v:f (to 1e-9)",
         abs(_ang(xy, "b", "v", "f") - _ang(xy, "c", "v", "f")) < EPS),
        ("I.9: GOAL v != f, visibly (at least 0.1 apart)",
         dist("v", "f") > 0.1),
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
    if prop == 6:
        return verify6(xy)
    if prop == 7:
        return verify7(xy)
    if prop == 8:
        return verify8(xy)
    if prop == 9:
        return verify9(xy)
    if prop == 10:
        return verify10(xy)
    if prop == 11:
        return verify11(xy)
    if prop == 12:
        return verify12(xy)
    if prop == 13:
        return verify13(xy)
    if prop == 14:
        return verify14(xy)
    if prop == 15:
        return verify15(xy)
    if prop == 16:
        return verify16(xy)
    if prop == 17:
        return verify17(xy)
    if prop == 18:
        return verify18(xy)
    if prop == 19:
        return verify19(xy)
    if prop == 20:
        return verify20(xy)
    if prop == 21:
        return verify21(xy)
    if prop == 22:
        return verify22(xy)
    if prop == 23:
        return verify23(xy)
    if prop == 24:
        return verify24(xy)
    if prop == 25:
        return verify25(xy)
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
    return eng.run_steps(sub, steps, allow_contradiction=True)


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
    # straight_angle (a closure rule that WAS I.13) has left default
    # closure, so no exclusion is needed here
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


def _run_branch14(s, facts, steps):
    """Re-run one by_contra branch on a copy of the state to get per-step
    reports (the real by_contra runs branches on private copies and
    reports only witnesses). Returns (reports, branch_state): the branch
    state is closed after the steps so the caller can exhibit the
    contradiction that refutes the branch."""
    sub = s.copy()
    for f in facts:
        eng._assume_metric_or_fact(sub, f, "by_contra assumption")
    eng.close(sub)
    reps = eng.run_steps(sub, steps, allow_contradiction=True)
    eng.close(sub)
    return reps, sub


# I.14 display decision (packet P-euclid-viewer-I14): the true diagram has
# d on line BC — that IS the theorem — but every proof step runs under the
# assumption BC != BD. Drawing a "false" BD slightly off BC would put a
# non-fact in the diagram (the picture would violate the very fact the
# metric check verifies: the angle sum = 180 degrees forces d onto BC).
# So the trace draws the TRUE configuration and every step inside the
# by_contra says in the info bar that it runs under the refuted
# assumption, which is not drawn as such.
DRAWN14 = ("under the assumption BC != BD, which the proof refutes; not "
           "drawn as such — the diagram shows the TRUE configuration "
           "(d on the extension of c-b beyond b, i.e. BC = BD)")
INNER14_1 = ("inner branch 1 (same_side a e BD): refuted by the "
             "arithmetic (angle e:b:d forced to 0); " + DRAWN14)
INNER14_2 = ("inner branch 2 (opposing_sides a e BD — Euclid's omitted "
             "case): refuted by the arithmetic (angle d:b:e forced to "
             "0); " + DRAWN14)


def build_trace14_rows(hyps, steps):
    """I.14 is a by_contra containing a nested by_contra (the engine's
    rendering of Lean's by_cases a.sameSide e BD — see PROP14_STEPS_DATA).
    The trace flattens it: the outer by_contra (Lean's by_contra line),
    the branch's six steps (extend_point and proposition_13 are Lean's;
    the two between_symm_parts, pasch_3_opposing and
    opposing_opposing_same_side are engine-only — Lean's SMT produces
    those silently), the inner split (Lean's by_cases line), then both
    inner branches' steps. Returns rows of
    (rule, args, bindings, report, loc, case_note)."""
    lean_file = os.path.join(LEAN_DIR, "Book", "Prop14.lean")
    with open(lean_file) as fh:
        lean_lines = fh.read().splitlines()
    rel = "Book/Prop14.lean"
    ln_by_contra = next(i + 1 for i, l in enumerate(lean_lines)
                        if l.strip().startswith("by_contra"))
    ln_ext = next(i + 1 for i, l in enumerate(lean_lines)
                  if "extend_point" in l)
    ln_p13 = next(i + 1 for i, l in enumerate(lean_lines)
                  if "proposition_13" in l)
    ln_bycases = next(i + 1 for i, l in enumerate(lean_lines)
                      if l.strip().startswith("by_cases"))
    sao = [i + 1 for i, l in enumerate(lean_lines)
           if "sum_angles_onlyif" in l]
    assert len(sao) == 2, sao

    s = eng.State()
    for f, w in hyps:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    s0 = s.copy()
    top = eng.run_steps(s, steps)[0]  # the single outer by_contra report

    _tag, branches, conclusions = steps[0]
    outer_assume, outer_steps = branches[0]
    inner = outer_steps[6]  # ("by_contra", inner_branches, [("false",)])

    rows = [("by_contra", branches, conclusions,
             dict(top, added=[tuple(c) for c in conclusions]),
             (rel, ln_by_contra),
             DRAWN14 + "; the conclusion (BC = BD, as mutual incidence "
             "on_line d BC / on_line c BD) is what the diagram draws")]
    reps, sub = _run_branch14(s0, outer_assume, outer_steps[:6])
    locs = [(rel, ln_ext), None, None, None, None, (rel, ln_p13)]
    for (r, a, b), rep, loc in zip(outer_steps[:6], reps, locs):
        rows.append((r, a, b, rep, loc, DRAWN14))
    rep_inner = eng._by_contra(sub.copy(), 7, inner)
    rows.append(("by_contra", inner[1], inner[2],
                 dict(rep_inner, added=[("false",)]),
                 (rel, ln_bycases),
                 "Lean's `by_cases a.sameSide e BD` line; the engine "
                 "renders the split as a nested by_contra concluding "
                 "False, because both branches are refuted (a by_cases "
                 "branch must ENTAIL, not contradict). Exhaustiveness: "
                 "a and e are both off BD, so System E's trichotomy "
                 "leaves exactly the two branches; " + DRAWN14))
    for (assume, bsteps), note in zip(inner[1], (INNER14_1, INNER14_2)):
        breps, bstate = _run_branch14(sub, assume, bsteps)
        ok, _w = eng.contradictory(bstate)
        assert ok, "I.14 inner branch must end contradictory"
        for (r, a, b), rep in zip(bsteps, breps):
            loc = ((rel, sao[0]) if r == "sum_angles_onlyif"
                   and note is INNER14_1 else
                   (rel, sao[1]) if r == "sum_angles_onlyif" else None)
            rows.append((r, a, b, rep, loc, note))
    return rows, s


# I.19 display decision (packet P-euclid-viewer-I19): like I.14, the
# proof is a by_contra, so the trace draws the TRUE configuration (the
# scalene triangle with angle a:b:c > angle b:c:a, hence |a-c| > |a-b|)
# and every branch step says in the info bar which refuted assumption it
# runs under (branch 1: |a-c| = |a-b|; branch 2: |a-b| > |a-c|) and which
# hypothesis it contradicts. I.19 constructs nothing: each branch is a
# single cited lemma (proposition_5' / proposition_18) and the
# contradictions are arithmetic (Lean's euclid_finish / SMT).
DRAWN19 = ("under an assumption the proof refutes; not drawn as such — "
           "the diagram shows the TRUE configuration (angle a:b:c > "
           "angle b:c:a, hence |a-c| > |a-b|)")
BR19_1 = ("branch 1 (|a-c| = |a-b|): refuted — proposition_5' gives "
          "angle a:b:c = angle b:c:a, contradicting the hypothesis "
          "angle a:b:c > angle b:c:a; " + DRAWN19)
BR19_2 = ("branch 2 (|a-b| > |a-c|): refuted — proposition_18 (I.18 in "
          "triangle a-c-b) gives angle b:c:a > angle a:b:c, "
          "contradicting the hypothesis; " + DRAWN19)


def build_trace19_rows(hyps, steps):
    """I.19 is a by_contra whose two branches (Lean's by_cases
    |(a─c)| = |(a─b)| split) each hold a single cited lemma (see
    PROP19_STEPS_DATA). The trace flattens it: the by_contra (Lean's
    by_contra line), the split (Lean's by_cases line), then both
    branches' steps, each replayed on a state copy and exhibited
    contradictory. Returns rows of
    (rule, args, bindings, report, loc, case_note)."""
    lean_file = os.path.join(LEAN_DIR, "Book", "Prop19.lean")
    with open(lean_file) as fh:
        lean_lines = fh.read().splitlines()
    rel = "Book/Prop19.lean"
    ln_by_contra = next(i + 1 for i, l in enumerate(lean_lines)
                        if l.strip().startswith("by_contra"))
    ln_by_cases = next(i + 1 for i, l in enumerate(lean_lines)
                       if l.strip().startswith("by_cases"))
    ln_p5 = next(i + 1 for i, l in enumerate(lean_lines)
                 if "proposition_5'" in l)
    ln_p18 = next(i + 1 for i, l in enumerate(lean_lines)
                  if "proposition_18" in l)

    s = eng.State()
    for f, w in hyps:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    s0 = s.copy()
    top = eng.run_steps(s, steps)[0]  # the single outer by_contra report

    _tag, branches, conclusions = steps[0]
    rows = [("by_contra", branches, conclusions,
             dict(top, added=[tuple(c) for c in conclusions]),
             (rel, ln_by_contra),
             DRAWN19 + "; the conclusion (|a-c| > |a-b|) is what the "
             "diagram draws")]
    rows.append(("by_cases", [list(f) for a, _ in branches for f in a],
                 [list(f) for a, _ in branches for f in a], {},
                 (rel, ln_by_cases),
                 "Lean's `by_cases |(a-c)| = |(a-b)|` line; under the "
                 "by_contra's assumption not |a-c| > |a-b|, trichotomy "
                 "on the two lengths leaves exactly these two branches; "
                 + DRAWN19))
    for (assume, bsteps), note, ln in zip(branches, (BR19_1, BR19_2),
                                          (ln_p5, ln_p18)):
        reps, bstate = _run_branch14(s0, assume, bsteps)
        ok, _w = eng.contradictory(bstate)
        assert ok, "I.19 branch must end contradictory"
        for (r, a, b), rep in zip(bsteps, reps):
            rows.append((r, a, b, rep, (rel, ln), note))
    return rows, s


# I.6 display decision (packet P-euclid-play-config-I6-I7): like I.19, the
# proof is a by_contra whose two branches (Lean's by_cases |(a─b)| >
# |(a─c)| split) run under refuted assumptions, so the trace draws the
# TRUE configuration — the isosceles triangle with angle a:b:c =
# angle a:c:b, hence |a-b| = |a-c| — and every step says in the info bar
# which refuted assumption it runs under. Unlike I.19 the branches
# CONSTRUCT (a cut point d and a line through it): those constructions
# are not drawn — in the true configuration the cut would coincide with
# an endpoint (|a-b| = |a-c|), so there is nothing honest to draw.
DRAWN6 = ("under an assumption the proof refutes; not drawn as such — "
          "the diagram shows the TRUE configuration (angle a:b:c = "
          "angle a:c:b, hence |a-b| = |a-c|)")
BR6_1 = ("branch 1 (|a-b| > |a-c|): refuted — proposition_3 cuts d off "
         "segment b-a with |b-d| = |a-c|, then proposition_4 (SAS on "
         "triangles d-b-c, a-c-b, using the hypothesis angle d:b:c = "
         "angle a:c:b) gives angle d:c:b = angle a:b:c = angle a:c:b; "
         "but d is strictly inside segment b-a, so angle d:c:b is a "
         "proper PART of angle a:c:b (sum_angles_onlyif) — the part "
         "equals the whole, impossible; " + DRAWN6)
BR6_2 = ("branch 2 (|a-c| > |a-b|): refuted — the mirror argument on "
         "the other side: proposition_3 cuts d off segment c-a with "
         "|c-d| = |a-b|, proposition_4 gives angle d:b:c = angle a:c:b "
         "= angle a:b:c, and d strictly inside segment c-a makes "
         "angle d:b:c a proper PART of angle a:b:c (sum_angles_onlyif) "
         "— the part equals the whole, impossible; " + DRAWN6)


def build_trace6_rows(hyps, steps):
    """I.6 is a single by_contra whose two branches (Lean's by_cases
    |(a-b)| > |(a-c)| split) each cut a point and cite proposition_4
    (see PROP6_STEPS_DATA). The trace flattens it: the by_contra
    (Lean's by_contra line), the split (Lean's by_cases line), then
    both branches' steps, each replayed on a state copy and exhibited
    contradictory. Returns rows of
    (rule, args, bindings, report, loc, case_note)."""
    lean_file = os.path.join(LEAN_DIR, "Book", "Prop06.lean")
    with open(lean_file) as fh:
        lean_lines = fh.read().splitlines()
    rel = "Book/Prop06.lean"
    ln_by_contra = next(i + 1 for i, l in enumerate(lean_lines)
                        if l.strip().startswith("by_contra"))
    ln_by_cases = next(i + 1 for i, l in enumerate(lean_lines)
                       if l.strip().startswith("by_cases"))
    p3 = [i + 1 for i, l in enumerate(lean_lines) if "proposition_3" in l]
    lfp = [i + 1 for i, l in enumerate(lean_lines)
           if "line_from_points" in l]
    p4 = [i + 1 for i, l in enumerate(lean_lines) if "proposition_4" in l]
    assert len(p3) == len(lfp) == len(p4) == 2, (p3, lfp, p4)

    s = eng.State()
    for f, w in hyps:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    s0 = s.copy()
    top = eng.run_steps(s, steps)[0]  # the single outer by_contra report

    _tag, branches, conclusions = steps[0]
    rows = [("by_contra", branches, conclusions,
             dict(top, added=[tuple(c) for c in conclusions]),
             (rel, ln_by_contra),
             DRAWN6 + "; the conclusion (|a-b| = |a-c|) is what the "
             "diagram draws")]
    rows.append(("by_cases", [list(f) for a, _ in branches for f in a],
                 [list(f) for a, _ in branches for f in a], {},
                 (rel, ln_by_cases),
                 "Lean's `by_cases |(a-b)| > |(a-c)|` line; under the "
                 "by_contra's assumption not |a-b| = |a-c|, trichotomy "
                 "on the two lengths leaves exactly these two branches; "
                 + DRAWN6))
    for (assume, bsteps), note, locs in zip(
            branches, (BR6_1, BR6_2),
            [[(rel, p3[0]), (rel, lfp[0]), (rel, p4[0]), None],
             [(rel, p3[1]), (rel, lfp[1]), (rel, p4[1]), None]]):
        reps, bstate = _run_branch14(s0, assume, bsteps)
        ok, _w = eng.contradictory(bstate)
        assert ok, "I.6 branch must end contradictory"
        for (r, a, b), rep, loc in zip(bsteps, reps, locs):
            rows.append((r, a, b, rep, loc, note))
    return rows, s


# I.7 display decision (packet P-euclid-play-config-I6-I7): I.7 concludes
# False — the hypotheses are jointly impossible, so no placement can
# satisfy them all (the two length equalities force d to be c's mirror
# across AB, the OPPOSITE side). The trace draws the hypotheses as Lean
# states them as far as a diagram can: c, d on the same side of AB with
# |a-c| = |a-d| exact, the second equality visibly failing. Every
# branch of the 2x2 side split (Lean's `by_cases a.sameSide b CD <;>
# by_cases d.sameSide b AC`, rendered by the engine as a by_contra
# concluding False) says in the info bar that its case assumption is
# refuted.
DRAWN7 = ("this case assumption is refuted; the diagram shows the "
          "hypothesis configuration as far as one can be drawn — c, d "
          "on the same side of AB with |a-c| = |a-d|; the second "
          "equality |c-b| = |d-b| cannot hold too (that impossibility "
          "is the theorem), so the drawn d visibly fails it")
BR7_1 = ("branch 1 (a sameSide b across CD, d sameSide b across AC): "
         "refuted — proposition_5' on |a-c| = |a-d| gives angle a:c:d = "
         "angle a:d:c and on |c-b| = |d-b| gives angle b:c:d = "
         "angle b:d:c, and the angle sums (sum_angles_onlyif) force a "
         "part to exceed the whole; " + DRAWN7)
BR7_2 = ("branch 2 (a sameSide b across CD, d opposingSides b across "
         "AC — Euclid omits it): refuted — the mirror of branch 1 with "
         "proposition_5' on the crossed configuration; the angle sums "
         "again force a part past the whole; " + DRAWN7)
BR7_3 = ("branch 3 (a opposingSides b across CD, d sameSide b across "
         "AC — Euclid omits it): refuted — AC and AD are extended past "
         "c, d to e, f beyond CD and proposition_5 (the full isosceles "
         "theorem, under-the-base angles) gives angle e:c:d = "
         "angle f:d:c while proposition_5' gives angle b:c:d = "
         "angle b:d:c; the angle sums force a part past the whole; "
         + DRAWN7)
BR7_4 = ("branch 4 (a opposingSides b across CD, d opposingSides b "
         "across AC — Euclid omits it): refuted — the extension "
         "argument of branch 3 on the crossed configuration; "
         + DRAWN7)


def build_trace7_rows(hyps, steps):
    """I.7's engine proof is line_from_points (CD) then a by_contra
    whose four branches are Lean's 2x2 side split (see PROP7_STEPS_DATA;
    Lean's goal IS False, so the by_contra is the engine's rendering of
    the two nested by_cases). Each branch's hypotheses are exactly the
    two case facts; the further side facts the angle steps need are
    derived in-branch by cited diagrammatic axioms (E-kimi-task-420) —
    those steps have no euclid_apply line (Lean's euclid_finish does
    them silently) and get the generic engine-only Lean link. The trace
    flattens it: the CD step (Lean's line_from_points line), the split
    (Lean's by_cases line), then all four branches' steps, each replayed
    on a state copy and exhibited contradictory. Returns rows of
    (rule, args, bindings, report, loc, case_note)."""
    lean_file = os.path.join(LEAN_DIR, "Book", "Prop07.lean")
    with open(lean_file) as fh:
        lean_lines = fh.read().splitlines()
    rel = "Book/Prop07.lean"
    ln_lfp = next(i + 1 for i, l in enumerate(lean_lines)
                  if "line_from_points" in l)
    ln_by_cases = next(i + 1 for i, l in enumerate(lean_lines)
                       if l.strip().startswith("by_cases"))
    applies = [i + 1 for i, l in enumerate(lean_lines)
               if "euclid_apply" in l and i + 1 > ln_by_cases]

    s = eng.State()
    for f, w in hyps:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    rep0 = eng.run_steps(s, steps[:1])[0]  # line_from_points c d -> CD
    s0 = s.copy()
    top = eng.run_steps(s, steps[1:])[0]  # the by_contra report

    _tag, branches, conclusions = steps[1]
    rows = [("line_from_points", steps[0][1], steps[0][2], rep0,
             (rel, ln_lfp),
             "the line through c and d — drawn in the hypothesis "
             "configuration; the split below is about which sides of "
             "CD and AC the points lie on"),
            ("by_contra", branches, conclusions,
             dict(top, added=[tuple(c) for c in conclusions]),
             (rel, ln_by_cases),
             "Lean's `by_cases a.sameSide b CD <;> by_cases "
             "d.sameSide b AC` line; the engine renders the 2x2 split "
             "as a by_contra concluding False, because every branch is "
             "refuted (a by_cases branch must ENTAIL, not contradict). "
             "Exhaustiveness: a, b are off CD and d, b off AC (derived "
             "in-state), so System E's trichotomy (Relations.lean:13-14) "
             "leaves exactly the four branches. Each branch's hypotheses "
             "are exactly the two case facts; the side facts the angle "
             "steps need are derived in-branch by cited diagrammatic "
             "axioms (E-kimi-task-420), shown as engine-only steps; "
             + DRAWN7)]
    flat = [(r, a, b) for _assume, bsteps in branches for r, a, b in
            bsteps]
    # Only the Lean-facing steps (extend/proposition_5/sum_angles) have
    # euclid_apply lines; the side-fact derivations are engine-only.
    lean_facing = {"extend_point_not_on_line", "proposition_5",
                   "proposition_5'", "sum_angles_onlyif"}
    assert len(applies) == sum(1 for r, _a, _b in flat if r in
                               lean_facing), (len(applies), len(flat))
    EXT_NOTE = ("note: Lean's line %d cites extend_point (extend the "
                "line past the endpoint); the engine's "
                "extend_point_not_on_line additionally lands the point "
                "past CD, so the I.5 under-the-base step can fire — an "
                "equivalent strengthening")
    for (assume, bsteps), note in zip(branches,
                                      (BR7_1, BR7_2, BR7_3, BR7_4)):
        reps, bstate = _run_branch14(s0, assume, bsteps)
        ok, _w = eng.contradictory(bstate)
        assert ok, "I.7 branch must end contradictory"
        for (r, a, b), rep in zip(bsteps, reps):
            if r in lean_facing:
                ln = applies.pop(0)
                loc = ((rel, ln, EXT_NOTE % ln)
                       if r == "extend_point_not_on_line" else (rel, ln))
            else:
                loc = None  # euclid_finish does this silently in Lean
            rows.append((r, a, b, rep, loc, note))
    return rows, s


# I.8 display decision (packet P-euclid-play-config-I8-I9): the TRUE
# configuration is drawn — two triangles with the three side equalities,
# under which the superposition lands EXACTLY (b' = f, c' = d). The
# by_contra refutes d != c' (Lean's by_cases d = g, where the d = g
# branch is euclid_finish); the refuted branch is not drawn as such.
DRAWN8 = ("the diagram shows the TRUE configuration — the "
          "superposition lands b' exactly on f and c' exactly on d, "
          "hence angle b:a:c = angle e:d:f")
BR8_1 = ("branch (d != c'): refuted — proposition_7 (I.7, two "
         "distances fix the point) on the SSS pair (e, f | d, c') with "
         "c'.sameSide d EF from the superposition bundle forces d = "
         "c', contradicting the assumption; the case assumption is "
         "refuted — " + DRAWN8)


def build_trace8_rows(hyps, steps):
    """I.8's engine proof is superposition, assert_point_eq_on_ray
    (Lean's euclid_assert c' = f), then a one-branch by_contra
    rendering Lean's `by_cases d = g` (the d = g branch is
    euclid_finish; the other cites proposition_7 — see
    PROP8_STEPS_DATA). The trace flattens it: the two top-level steps,
    the by_contra row (Lean's by_cases line), then the branch's
    proposition_7 step replayed on a state copy and exhibited
    contradictory. Returns rows of
    (rule, args, bindings, report, loc, case_note)."""
    lean_file = os.path.join(LEAN_DIR, "Book", "Prop08.lean")
    with open(lean_file) as fh:
        lean_lines = fh.read().splitlines()
    rel = "Book/Prop08.lean"
    ln_super = next(i + 1 for i, l in enumerate(lean_lines)
                    if "superposition" in l)
    ln_assert = next(i + 1 for i, l in enumerate(lean_lines)
                     if "euclid_assert" in l)
    ln_by_cases = next(i + 1 for i, l in enumerate(lean_lines)
                       if l.strip().startswith("by_cases"))
    ln_prop7 = next(i + 1 for i, l in enumerate(lean_lines)
                    if "euclid_apply" in l and "proposition_7" in l)

    s = eng.State()
    for f, w in hyps:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    rep0 = eng.run_steps(s, steps[:1])[0]   # superposition
    rep1 = eng.run_steps(s, steps[1:2])[0]  # assert_point_eq_on_ray
    s0 = s.copy()
    top = eng.run_steps(s, steps[2:])[0]    # the by_contra report
    # the goal angle equality is then entailed (engine main()'s p=="8"
    # query: the superposition bundle's angle at c' with c' = d)
    assert eng.la_entails_eq_ang(s.la, "b", "a", "c", "e", "d", "f")

    _tag, branches, conclusions = steps[2]
    rows = [("superposition", steps[0][1], steps[0][2], rep0,
             (rel, ln_super), None),
            ("assert_point_eq_on_ray", steps[1][1], steps[1][2], rep1,
             (rel, ln_assert), None),
            ("by_contra", branches, conclusions,
             dict(top, added=[tuple(c) for c in conclusions]),
             (rel, ln_by_cases),
             "Lean's `by_cases d = g` line; the engine renders it as a "
             "by_contra concluding peq d c' (engine c' = Lean's g), "
             "because the d != c' branch is refuted (a by_cases branch "
             "must ENTAIL, not contradict); the d = c' branch is "
             "Lean's euclid_finish. Exhaustiveness is trichotomy on "
             "point equality; " + DRAWN8)]
    for assume, bsteps in branches:
        reps, bstate = _run_branch14(s0, assume, bsteps)
        ok, _w = eng.contradictory(bstate)
        assert ok, "I.8 branch must end contradictory"
        for (r, a, b), rep in zip(bsteps, reps):
            rows.append((r, a, b, rep, (rel, ln_prop7), BR8_1))
    return rows, s


# I.23 display decision (packet P-euclid-play-config-I22-I23): like
# I.14/I.19, the true (non-degenerate) configuration is drawn — d off CE,
# the given angle visibly neither 0 nor straight — and every
# degenerate-branch step says in the info bar that its branch (d on CE,
# "Omitted by Euclid") is HANDLED by the engine on a state copy, not
# drawn. The branch is not refuted (it is a genuinely possible case,
# closed in Lean with its own witnesses), so the notes say "handled",
# not "refuted".
DRAWN23 = ("drawn case (branch 2: d off CE) — the general case; the "
           "degenerate branch is handled by the engine and only noted")
DEGEN23 = ("degenerate branch (d on CE — 'Omitted by Euclid', "
           "Prop23.lean:14-21): handled by the engine, not drawn — the "
           "diagram shows the general case (d off CE)")
DEGEN23_ZERO = ("degenerate sub-case angle d:c:e = 0: the witness is b "
                "itself (angle b:a:b = 0, the SMT-bridge "
                "angle_self_zero); " + DEGEN23)
DEGEN23_FLAT = ("degenerate sub-case angle d:c:e straight (between d c "
                "e): the witness is the extension of AB past a; "
                + DEGEN23)


def build_trace23_rows(hyps, steps):
    """I.23 is an outer by_cases (Prop23.lean:13, on d.onLine CE) whose
    degenerate branch holds a nested by_cases (Prop23.lean:15, on
    angle d:c:e = 0) — see PROP23_STEPS_DATA. The trace flattens it, as
    I.13: the outer split (Lean's by_cases line), the inner split
    (Lean's nested by_cases line), the two degenerate sub-branches'
    steps (handled by the engine, not drawn), then the main branch's
    eleven steps (drawn — they ARE Lean's construction). Returns rows of
    (rule, args, bindings, report, loc, case_note)."""
    lean_file = os.path.join(LEAN_DIR, "Book", "Prop23.lean")
    with open(lean_file) as fh:
        lean_lines = fh.read().splitlines()
    rel = "Book/Prop23.lean"
    # only the first theorem (proposition_23, lines 8-32): Prop23.lean
    # continues with proposition_23', whose by_cases/apply lines repeat
    # the same shapes
    end = next(i + 1 for i, l in enumerate(lean_lines)
               if l.startswith("theorem proposition_23'"))
    lean_lines = lean_lines[:end]
    by_cases = [i + 1 for i, l in enumerate(lean_lines)
                if l.strip().startswith("by_cases")]
    assert len(by_cases) == 2, by_cases
    ln_outer, ln_inner = by_cases
    ln_use_b = next(i + 1 for i, l in enumerate(lean_lines)
                    if l.strip() in ("· use b", "use b"))
    ln_assert = next(i + 1 for i, l in enumerate(lean_lines)
                     if "euclid_assert" in l)
    ln_ext = next(i + 1 for i, l in enumerate(lean_lines)
                  if "extend_point" in l)
    ln_de = next(i + 1 for i, l in enumerate(lean_lines)
                 if "line_from_points d e" in l)
    p20 = [i + 1 for i, l in enumerate(lean_lines)
           if "euclid_apply" in l and "proposition_20" in l]
    assert len(p20) == 3, p20
    ln_p22 = next(i + 1 for i, l in enumerate(lean_lines)
                  if "proposition_22'" in l)
    ln_fa = next(i + 1 for i, l in enumerate(lean_lines)
                 if "line_from_points a f" in l)
    ln_fg = next(i + 1 for i, l in enumerate(lean_lines)
                 if "line_from_points f g" in l)
    ln_p8 = next(i + 1 for i, l in enumerate(lean_lines)
                 if "proposition_8" in l)

    s = eng.State()
    for f, w in hyps:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    s0 = s.copy()
    top = eng.run_steps(s, steps)[0]  # the single outer by_cases report

    _tag, branches, conclusions = steps[0]
    degen_assume, degen_steps = branches[0]
    main_assume, main_steps = branches[1]
    inner = degen_steps[0]  # ("by_cases", inner_branches, PROP23_GOAL)
    zero_assume, zero_steps = inner[1][0]
    flat_assume, flat_steps = inner[1][1]

    rows = [("by_cases", [b[0] for b in branches],
             [list(c) for c in conclusions], top, (rel, ln_outer),
             DRAWN23)]
    rep_inner = _run_branch13(s0, degen_assume, [inner])[0]
    rows.append(("by_cases", [b[0] for b in inner[1]],
                 [list(c) for c in inner[2]], rep_inner, (rel, ln_inner),
                 DEGEN23))
    # degenerate sub-branch 1 (angle d:c:e = 0): angle_self_zero is the
    # SMT inside Lean's euclid_finish (line 17); use_witness is Lean's
    # `use b` (line 16)
    sub_deg = s0.copy()
    for f in degen_assume:
        eng._assume_metric_or_fact(sub_deg, f, "by_cases assumption")
    eng.close(sub_deg)
    reps_zero = _run_branch13(sub_deg, zero_assume, zero_steps)
    for (r, a, b), rep in zip(zero_steps, reps_zero):
        loc = ((rel, ln_use_b,
                "note: Lean's `use b` (Prop23.lean:16) supplies this "
                "sub-case's witness: the goal's conjuncts restated "
                "about b under the witness name f; the angle equality "
                "is angle b:a:b = 0 = angle d:c:e, closed by Lean's "
                "euclid_finish (line 17) — the SMT angle_self_zero "
                "step above")
               if r == "use_witness" else None)
        rows.append((r, a, b, rep, loc, DEGEN23_ZERO))
    # degenerate sub-branch 2 (between d c e): the first
    # flat_angle_onlyif is Lean's euclid_assert (line 18); extend_point
    # is Lean's line 19; the rest is euclid_finish (line 21, SMT)
    reps_flat = _run_branch13(sub_deg, flat_assume, flat_steps)
    for (r, a, b), rep in zip(flat_steps, reps_flat):
        if r == "flat_angle_onlyif" and a == ("d", "c", "e"):
            loc = (rel, ln_assert,
                   "note: Lean's euclid_assert ∠ d:c:e = ∟ + ∟ "
                   "(Prop23.lean:18): the System E citation is "
                   "flat_angle_onlyif (Transfer.lean:124) — between "
                   "d c e gives the straight angle directly; "
                   "proposition_13 is not needed")
        elif r == "extend_point":
            loc = (rel, ln_ext)
        else:
            loc = None
        rows.append((r, a, b, rep, loc, DEGEN23_FLAT))
    # main branch (d off CE): Lean's construction, drawn
    main_locs = {"line_from_points d e": (rel, ln_de),
                 "line_from_points a f": (rel, ln_fa),
                 "line_from_points f g": (rel, ln_fg)}
    p20i = iter(p20)
    reps_main = _run_branch13(s0, main_assume, main_steps)
    for (r, a, b), rep in zip(main_steps, reps_main):
        key = r + " " + " ".join(str(x) for x in a[:2])
        if r == "line_from_points":
            loc = main_locs[key]
        elif r == "proposition_20":
            loc = (rel, next(p20i))
        elif r == "proposition_22'":
            loc = (rel, ln_p22)
        elif r == "proposition_8":
            loc = (rel, ln_p8)
        else:
            loc = None
        rows.append((r, a, b, rep, loc, DRAWN23))
    return rows, s


# I.24 display decision (packet P-euclid-play-config-I24-I25): the two
# triangles stand apart with the given angle at a visibly greater than
# at d, and Euclid's own case is drawn — d and g on the same side of EF.
# Lean's second bullet ("Omitted by Euclid", Prop24.lean:40-59) is
# HANDLED by the engine on state copies, not drawn, and every
# second-case step's bar says so (the I.23 degenerate-branch pattern).
DRAWN24 = ("drawn case (branch 1: d sameSide g EF) — Euclid's own case; "
           "the second case is handled by the engine and only noted")
BR24_2 = ("second case (not same_side d g EF — 'Omitted by Euclid', "
          "Prop24.lean:40-59): handled by the engine, not drawn — the "
          "diagram shows Euclid's case (d and g on the same side of EF)")
BR24_2_ON = ("second case, sub-case g on EF (Prop24.lean:41-42): the "
             "goal itself closes it — g on EF forces between e f g, so "
             "|e-g| = |e-f| + |f-g| > |e-f| and |b-c| = |e-g|; "
             + BR24_2)
BR24_2_OFF = ("second case, sub-case g off EF (Prop24.lean:43-59): the "
              "angle arithmetic of the two linear pairs at f and the "
              "two triangle angle sums forces angle e:f:g > angle "
              "e:g:f, so proposition_19 gives |e-g| > |e-f|; "
              + BR24_2)


def build_trace24_rows(hyps, steps):
    """I.24's replay is the common construction (17 steps) followed by a
    two-branched by_cases on d.sameSide g EF whose second branch holds a
    nested by_cases on g.onLine EF (see PROP24_STEPS_DATA). The trace
    flattens it, as I.23: the common steps (drawn — they ARE Lean's
    construction), the outer split (Lean's by_cases line), the drawn
    branch 1's steps, then the second case's nested split and both
    sub-branches' steps (handled by the engine, not drawn). Returns rows
    of (rule, args, bindings, report, loc, case_note)."""
    lean_file = os.path.join(LEAN_DIR, "Book", "Prop24.lean")
    with open(lean_file) as fh:
        lean_lines = fh.read().splitlines()
    rel = "Book/Prop24.lean"

    def first(sub, start=0):
        return next(i + 1 for i, l in enumerate(lean_lines)
                    if i + 1 > start and sub in l)

    ln_p23 = first("proposition_23'")
    ln_dg = first("line_from_points d g'")
    ln_ext = first("extend_point_longer")
    ln_p3 = first("proposition_3")
    ln_eg = first("line_from_points e g")
    ln_fg = first("line_from_points f g")
    ln_p4 = first("proposition_4")
    ln_p5 = first("proposition_5'")
    ln_a28 = first("∠ e:d:g > ∠ e:d:f")
    ln_a29 = first("g.sameSide f DE")
    ln_hgf = first("have hgf")
    ln_a34 = first("f.sameSide e DG")
    ln_split = first("by_cases (d.sameSide g EF)")
    ln_a36 = first("∠ d:f:g > ∠ e:g:f")
    efg = [i + 1 for i, l in enumerate(lean_lines)
           if "∠ e:f:g > ∠ e:g:f" in l]
    assert len(efg) == 2, efg
    ln_a37, ln_a56 = efg
    p19 = [i + 1 for i, l in enumerate(lean_lines) if "proposition_19" in l]
    assert len(p19) == 2, p19
    ln_p19a, ln_p19b = p19
    ln_inner = first("by_cases g.onLine EF")
    ln_fin42 = next(i + 1 for i, l in enumerate(lean_lines)
                    if i + 1 > ln_inner and "euclid_finish" in l)
    ln_exth = first("extend_point FG g f")
    ln_a45 = first("¬(g.onLine DF)")
    ln_a46 = first("¬(e.onLine DF)")
    ln_a47 = first("g.opposingSides e DF")
    ln_a48 = first("h.sameSide e DF")
    p13 = [i + 1 for i, l in enumerate(lean_lines) if "proposition_13" in l]
    assert len(p13) == 2, p13
    p17 = [i + 1 for i, l in enumerate(lean_lines) if "proposition_17" in l]
    assert len(p17) == 2, p17

    s = eng.State()
    for f, w in hyps:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    s0 = s.copy()

    common, final = steps[:-1], steps[-1]
    assert final[0] == "by_cases"
    reps_common = eng.run_steps(s, common)
    s1 = s.copy()  # the pre-split state: hypotheses + the common
    # construction; both cases replay on copies of this (the by_cases
    # itself runs them on private copies)
    top = eng.run_steps(s, [final])[0]

    # the common construction (drawn): Lean links by content; the
    # engine-only steps (the disjunction resolution, the neq supports,
    # the closure-derived asserts) get None and a note
    common_locs = {
        "proposition_23'": (rel, ln_p23),
        "extend_point_longer": (rel, ln_ext),
        "proposition_3": (rel, ln_p3),
        "proposition_4": (rel, ln_p4),
        "proposition_5'": (rel, ln_p5),
        "triple_incidence_2": (rel, ln_a34,
                               "note: Lean's euclid_assert f.sameSide e "
                               "DG (Prop24.lean:34): the System E "
                               "citation is triple_incidence_2 "
                               "(Diagrammatic.lean:201) over (DE, DF, "
                               "DG) at d"),
    }
    lfp = iter([(rel, ln_dg), (rel, ln_eg), (rel, ln_fg)])
    rows = []
    for (r, a, b), rep in zip(common, reps_common):
        if r == "line_from_points":
            loc = next(lfp)
        elif r == "by_contra":
            # which assert does this by_contra render? keyed by its
            # conclusion
            concl = b and [list(c) for c in b] or []
            c0 = concl[0][0] if concl else None
            if concl == [["same_side", "g'", "f", "DE"]]:
                loc = None  # the disjunction resolution: no Lean line
            elif concl == [["neq", "f", "g"]]:
                loc = None  # the line_from_points f g premise: SMT
            elif concl == [["gt_ang", "e", "d", "g", "e", "d", "f"]]:
                loc = (rel, ln_a28,
                       "note: Lean's euclid_assert ∠ e:d:g > ∠ e:d:f "
                       "(Prop24.lean:28), rendered as a by_contra on "
                       "the angle trichotomy")
            elif concl == [["same_side", "g", "f", "DE"]]:
                loc = (rel, ln_a29,
                       "note: Lean's euclid_assert g.sameSide f DE "
                       "(Prop24.lean:29), closure-derived and exposed "
                       "by contradicting its opposing_sides rendering")
            elif concl == [["not_on_line", "g", "DF"]]:
                loc = None  # g off DF: closure-only, no Lean line
            elif concl == [["opposing_sides", "e", "g", "DF"]]:
                loc = (rel, ln_hgf,
                       "note: Lean's `have hgf : ¬ g.sameSide e DF` "
                       "(Prop24.lean:30-33); the engine renders the "
                       "conclusion as the positive opposing_sides e g "
                       "DF, and the hgf proof's sum_angles_onlyif step "
                       "stays inside this by_contra")
            else:
                loc = None
        else:
            loc = common_locs.get(r)
        rows.append((r, a, b, rep, loc, None))

    # the outer split (Lean's by_cases line)
    _tag, branches, conclusions = final
    rows.append(("by_cases", [list(f) for f, _s in branches],
                 [list(c) for c in conclusions], top, (rel, ln_split),
                 None))

    (drawn_assume, drawn_steps), (br2_assume, br2_steps) = branches

    # branch 1 (d sameSide g EF): Euclid's case, drawn
    drawn_locs = iter([(rel, ln_a36,
                        "note: Lean's euclid_assert ∠ d:f:g > ∠ e:g:f "
                        "(Prop24.lean:36), rendered as a by_contra on "
                        "the angle trichotomy"),
                       (rel, ln_a37,
                        "note: Lean's euclid_assert ∠ e:f:g > ∠ e:g:f "
                        "(Prop24.lean:37), rendered as a by_contra on "
                        "the angle trichotomy"),
                       (rel, ln_p19a)])
    reps1 = _run_branch13(s1, drawn_assume, drawn_steps)
    for (r, a, b), rep in zip(drawn_steps, reps1):
        loc = next(drawn_locs) if r in ("by_contra", "proposition_19") \
            else None
        rows.append((r, a, b, rep, loc, DRAWN24))

    # branch 2 (not sameSide d g EF): handled by the engine, not drawn
    inner = br2_steps[0]
    assert inner[0] == "by_cases"
    rep_inner = _run_branch13(s1, br2_assume, [inner])[0]
    rows.append(("by_cases", [list(f) for f, _s in inner[1]],
                 [list(c) for c in inner[2]], rep_inner,
                 (rel, ln_inner), BR24_2))
    sub2 = s1.copy()
    for f in br2_assume:
        eng._assume_metric_or_fact(sub2, f, "by_cases assumption")
    eng.close(sub2)
    (on_assume, on_steps), (off_assume, off_steps) = inner[1]
    # sub-case g on EF: the nested by_contra derives between e f g by
    # line trichotomy; Lean's euclid_finish (Prop24.lean:42) closes it
    reps_on = _run_branch13(sub2, on_assume, on_steps)
    for (r, a, b), rep in zip(on_steps, reps_on):
        loc = ((rel, ln_fin42,
                "note: Lean's euclid_finish (Prop24.lean:42) closes "
                "this sub-case outright; the engine renders the "
                "between-trichotomy on EF as a by_contra deriving "
                "between e f g")
               if r == "by_contra" else None)
        rows.append((r, a, b, rep, loc, BR24_2_ON))
    # sub-case g off EF: Lean's lines 43-59
    off_locs = {
        ("extend_point",): (rel, ln_exth),
    }
    assert_locs = iter([(rel, ln_a45,
                         "note: Lean's euclid_assert ¬(g.onLine DF) "
                         "(Prop24.lean:45), exposed by contradicting "
                         "on_line g DF"),
                        (rel, ln_a46,
                         "note: Lean's euclid_assert ¬(e.onLine DF) "
                         "(Prop24.lean:46), closure-derived"),
                        (rel, ln_a47,
                         "note: Lean's euclid_assert g.opposingSides e "
                         "DF (Prop24.lean:47), hgf symmetrized")])
    p13i, p17i = iter(p13), iter(p17)
    reps_off = _run_branch13(sub2, off_assume, off_steps)
    for (r, a, b), rep in zip(off_steps, reps_off):
        if r == "extend_point":
            loc = (rel, ln_exth)
        elif r == "by_contra":
            concl = [list(c) for c in b]
            if concl == [["not_on_line", "g", "DF"]] or \
                    concl == [["not_on_line", "e", "DF"]] or \
                    concl == [["opposing_sides", "g", "e", "DF"]]:
                loc = next(assert_locs)
            elif concl == [["gt_ang", "e", "f", "g", "e", "g", "f"]]:
                loc = (rel, ln_a56,
                       "note: Lean's euclid_assert ∠ e:f:g > ∠ e:g:f "
                       "(Prop24.lean:56), rendered as a by_contra on "
                       "the angle trichotomy; the lines 53-55 asserts "
                       "are LA intermediates the constraint store "
                       "derives inside this check")
            else:
                loc = None  # h off DF / off EF: closure-only
        elif r == "opposing_opposing_same_side" and a[1] == "g" \
                and a[2] == "h" and a[3] == "DF":
            loc = (rel, ln_a48,
                   "note: Lean's euclid_assert h.sameSide e DF "
                   "(Prop24.lean:48): h and e both oppose g across DF")
        elif r == "proposition_13":
            loc = (rel, next(p13i))
        elif r == "proposition_17":
            loc = (rel, next(p17i))
        elif r == "proposition_19":
            loc = (rel, ln_p19b)
        else:
            loc = None
        rows.append((r, a, b, rep, loc, BR24_2_OFF))
    return rows, s


# I.25 display decision (packet P-euclid-play-config-I24-I25): like
# I.19, the proof is a by_contra whose two branches (Lean's by_cases
# ∠b:a:c = ∠e:d:f split) each hold a single cited lemma, so the trace
# draws the TRUE configuration — the two triangles apart with
# |b-c| > |e-f|, hence angle b:a:c > angle e:d:f — and each branch
# step's bar says which refuted assumption it runs under and which
# hypothesis it contradicts.
DRAWN25 = ("under an assumption the proof refutes; not drawn as such — "
           "the diagram shows the TRUE configuration (|b-c| > |e-f|, "
           "hence angle b:a:c > angle e:d:f)")
BR25_1 = ("branch 1 (angle b:a:c = angle e:d:f): refuted — "
          "proposition_4 (SAS) gives |b-c| = |e-f|, contradicting the "
          "hypothesis |b-c| > |e-f|; " + DRAWN25)
BR25_2 = ("branch 2 (angle e:d:f > angle b:a:c): refuted — "
          "proposition_24 (the hinge theorem, I.24, with the triangles "
          "swapped) gives |e-f| > |b-c|, contradicting the hypothesis "
          "|b-c| > |e-f|; " + DRAWN25)


def build_trace25_rows(hyps, steps):
    """I.25 is a by_contra whose two branches (Lean's by_cases
    ∠b:a:c = ∠e:d:f split) each hold a single cited lemma (see
    PROP25_STEPS_DATA), exactly I.19's shape. The trace flattens it:
    the by_contra (Lean's by_contra line), the split (Lean's by_cases
    line), then both branches' steps, each replayed on a state copy and
    exhibited contradictory. Returns rows of
    (rule, args, bindings, report, loc, case_note)."""
    lean_file = os.path.join(LEAN_DIR, "Book", "Prop25.lean")
    with open(lean_file) as fh:
        lean_lines = fh.read().splitlines()
    rel = "Book/Prop25.lean"
    ln_by_contra = next(i + 1 for i, l in enumerate(lean_lines)
                        if l.strip().startswith("by_contra"))
    ln_by_cases = next(i + 1 for i, l in enumerate(lean_lines)
                       if l.strip().startswith("by_cases"))
    ln_p4 = next(i + 1 for i, l in enumerate(lean_lines)
                 if "proposition_4" in l)
    ln_p24 = next(i + 1 for i, l in enumerate(lean_lines)
                  if "proposition_24" in l)

    s = eng.State()
    for f, w in hyps:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    s0 = s.copy()
    top = eng.run_steps(s, steps)[0]  # the single outer by_contra report

    _tag, branches, conclusions = steps[0]
    rows = [("by_contra", branches, conclusions,
             dict(top, added=[tuple(c) for c in conclusions]),
             (rel, ln_by_contra),
             DRAWN25 + "; the conclusion (angle b:a:c > angle e:d:f) "
             "is what the diagram draws")]
    rows.append(("by_cases", [list(f) for a, _ in branches for f in a],
                 [list(f) for a, _ in branches for f in a], {},
                 (rel, ln_by_cases),
                 "Lean's `by_cases ∠ b:a:c = ∠ e:d:f` line; under the "
                 "by_contra's assumption not angle b:a:c > angle "
                 "e:d:f, trichotomy on the two angles leaves exactly "
                 "these two branches; " + DRAWN25))
    for (assume, bsteps), note, ln in zip(branches, (BR25_1, BR25_2),
                                          (ln_p4, ln_p24)):
        reps = _run_branch13(s0, assume, bsteps)
        sub = s0.copy()
        for f in assume:
            eng._assume_metric_or_fact(sub, f, "by_contra assumption")
        eng.close(sub)
        eng.run_steps(sub, bsteps, allow_contradiction=True)
        ok, _w = eng.contradictory(sub)
        assert ok, "I.25 branch must end contradictory"
        for (r, a, b), rep in zip(bsteps, reps):
            rows.append((r, a, b, rep, (rel, ln), note))
    return rows, s


def build_trace(prop):
    hyps, steps = hyps_and_steps(prop)
    if prop == 6:
        # by_contra with two constructing branches (see
        # build_trace6_rows); each branch is replayed on a state copy
        # and exhibited contradictory
        rows, _s = build_trace6_rows(hyps, steps)
        name_map = {}
    elif prop == 7:
        # by_contra rendering Lean's 2x2 side split (see
        # build_trace7_rows); each branch is replayed on a state copy
        # and exhibited contradictory
        rows, _s = build_trace7_rows(hyps, steps)
        name_map = {}
    elif prop == 8:
        # superposition + assert + one-branch by_contra rendering
        # Lean's by_cases d = g (see build_trace8_rows); the branch is
        # replayed on a state copy and exhibited contradictory
        rows, _s = build_trace8_rows(hyps, steps)
        name_map = {}
    elif prop == 13:
        # nested by_cases: rows built by replaying each branch on a state
        # copy (see build_trace13_rows); the replay itself is the check
        rows, _s = build_trace13_rows(hyps, steps)
        name_map = {}
    elif prop == 14:
        # nested by_contra: rows built by replaying each branch on a
        # state copy (see build_trace14_rows); the replay itself is the
        # check (each inner branch is exhibited contradictory)
        rows, _s = build_trace14_rows(hyps, steps)
        name_map = {}
    elif prop == 19:
        # by_contra with two cited-lemma branches (see
        # build_trace19_rows); each branch is replayed on a state copy
        # and exhibited contradictory
        rows, _s = build_trace19_rows(hyps, steps)
        name_map = {}
    elif prop == 23:
        # nested by_cases: the degenerate branch (d on CE, "Omitted by
        # Euclid") is handled by the engine on state copies, not drawn;
        # the main branch (d off CE) is drawn (see build_trace23_rows)
        rows, _s = build_trace23_rows(hyps, steps)
        name_map = {}
    elif prop == 24:
        # the common construction is drawn, then the closing by_cases:
        # branch 1 (d sameSide g EF) is Euclid's case, drawn; branch 2
        # ("Omitted by Euclid") is handled by the engine on state
        # copies, not drawn (see build_trace24_rows)
        rows, _s = build_trace24_rows(hyps, steps)
        name_map = {}
    elif prop == 25:
        # by_contra with two cited-lemma branches (see
        # build_trace25_rows); each branch is replayed on a state copy
        # and exhibited contradictory, as at I.19
        rows, _s = build_trace25_rows(hyps, steps)
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
        if prop in (9, 10, 11, 12, 15, 16, 17, 18, 20, 21, 22):
            # replay() also drives the by_contra steps and asserts the Book
            # conclusion (same queries the engine's own main() uses). I.12's
            # goal substitute is stated in PROP12_STEPS_DATA's comment: the
            # replay proves rt e h c with h on AB, which implies Lean's
            # disjunction (h and e both on AB, e != h, a != b). I.15's are
            # PROP15_GOAL verbatim: both halves of the vertical-angles
            # conjunction as LA entailment queries. I.16's is PROP16B_GOAL
            # verbatim: the second conjunct angle a:c:d > angle b:a:c.
            # I.17's is PROP17_GOAL verbatim: angle a:b:c + angle b:c:a
            # < two right angles. I.18's is PROP18_GOAL verbatim:
            # angle a:b:c > angle b:c:a. I.21's is PROP21_GOAL verbatim:
            # |b-d| + |d-c| < |b-a| + |a-c| and angle b:d:c > angle b:a:c.
            # I.9's are eng.main()'s p=="9" queries verbatim: the
            # bisection angle b:v:f = angle c:v:f and v != f (engine
            # v = Lean's a).
            queries = {9: [("_eq_ang", "b", "v", "f", "c", "v", "f"),
                           ("neq", "v", "f")],
                       10: [("between", "a", "d", "b"),
                            ("_eq_len", "a", "d", "d", "b")],
                       11: [("not_on_line", "f", "AB"),
                            ("_rt_ang", "u", "w", "f")],
                       12: [("_rt_ang", "e", "h", "c"),
                            ("on_line", "h", "AB")],
                       15: list(eng.PROP15_GOAL),
                       16: list(eng.PROP16B_GOAL),
                       17: list(eng.PROP17_GOAL),
                       18: list(eng.PROP18_GOAL),
                       20: list(eng.PROP20_GOAL),
                       21: list(eng.PROP21_GOAL),
                       22: list(eng.PROP22_GOAL)}[prop]
            _s, reports_all = eng.replay(hyps, steps, queries)
            reports = reports_all[:-1]  # drop the {"final": ...} entry
        else:
            reports = [eng.apply_step(s, i, r, a, b)
                       for i, (r, a, b) in enumerate(steps, 1)]

        name_map = (PROP11_NAME_MAP if prop == 11
                    else PROP9_NAME_MAP if prop == 9 else {})
        if prop == 16:
            # second bullet only: match euclid_apply lines past the first
            # bullet's euclid_finish (line 25)
            fname = os.path.join(LEAN_DIR, "Book", "Prop16.lean")
            with open(fname) as fh:
                after16 = next(i for i, l in enumerate(fh, 1)
                               if l.strip().startswith("euclid_finish"))
            lean = lean_lines_for_named(prop, steps, name_map,
                                        after=after16)
        else:
            lean = (lean_lines_for_named(prop, steps, name_map)
                    if prop in (9, 10, 11, 12, 15, 17, 18, 21, 22)
                    else lean_lines_for(prop, [r for r, _, _ in steps]))
        if prop == 18:
            # the engine's last step is sum_angles_onlyif where Lean
            # cites sum_angles_if (Prop18.lean:17) — the OTHER direction
            # of the same Transfer.lean axiom. Link the step to that
            # line, marked so the note says why and the reader is not
            # misled.
            fname = os.path.join(LEAN_DIR, "Book", "Prop18.lean")
            with open(fname) as fh:
                ln18 = next(i for i, l in enumerate(fh, 1)
                            if "sum_angles_if" in l)
            lean = [((os.path.join("Book", "Prop18.lean"), ln18,
                      "other-direction")
                     if st[0] == "sum_angles_onlyif" and loc is None
                     else loc)
                    for st, loc in zip(steps, lean)]
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
            if len(loc) > 2 and loc[2] == "other-direction":
                lean_note = ("Lean's line %d cites sum_angles_if — the "
                             "OTHER direction: sum_angles_if (Transfer.lean:83) "
                             "and sum_angles_onlyif (:90) are converse "
                             "axioms; the engine uses "
                             "sum_angles_onlyif on the same arguments to "
                             "derive the angle-sum row Lean's SMT "
                             "discharges silently" % loc[1])
            elif len(loc) > 2 and loc[2].startswith("note: "):
                # a row builder's own Lean-link note (I.7's
                # extend_point_not_on_line vs Lean's extend_point)
                lean_note = loc[2][len("note: "):]
            else:
                lean_note = (("Lean cites %s with different arguments; the "
                              "engine's citation is an equivalent variant"
                              % rule)
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
    if prop in (9, 11):
        # the viewer shows Lean's names; the whole trace is remapped and
        # the engine->Lean map is stated in the JSON
        nm = PROP11_NAME_MAP if prop == 11 else PROP9_NAME_MAP
        objects = _remap_names(objects, nm)
        trace_steps = _remap_names(trace_steps, nm)
        placement = _remap_names(placement, nm)

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
    if prop == 9:
        out["name_map"] = dict(PROP9_NAME_MAP)
        out["name_map_note"] = ("engine points renamed for Lean display: "
                                "engine v = Lean a")
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
