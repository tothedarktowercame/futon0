#!/usr/bin/env python3
"""euclid_play.py — Witness-style play backend for Book I (E-kimi-task-413,
packet P-euclid-play-backend from claude-9).

A student plays the propositions IN ORDER: proposition N's palette is the
constructions plus proposition_K (and primed variants) for K < N only;
citing proposition_N or later is refused ("I.N is not proved yet").

  catalogue()      ordered entries for every N in euclid_trace.FREE
  palette(N)       rule names citable at N
  play(N, moves)   STATELESS replay of a student's moves from the
                   proposition's hypothesis state

HTTP: python3 euclid_play.py --port P  (binds 127.0.0.1) serves the viewer
directory statically plus GET /api/catalogue and POST /api/play.

Stdlib only; imports euclid_engine as eng and euclid_trace as tr.
Neither is modified.

GOALS: the same per-prop goal queries euclid_trace.build_trace / the
engine's own main() already use. Where the engine names them
(PROP13_GOAL, PROP14_GOAL, PROP15_GOAL, PROP16B_GOAL, PROP17_GOAL,
PROP18_GOAL, PROP19_GOAL, PROP20_GOAL, PROP21_GOAL, and _prop1()'s verify
list) the constant is REFERENCED, not copied. The 1-5 and 10-12 queries
exist only INLINE in eng.main()/tr.build_trace (no module-level name), so
they are restated here verbatim — flagged to claude-9 in the reply bell.
"""

import argparse
import functools
import json
import os
import re
import time
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer

import euclid_engine as eng
import euclid_trace as tr

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
VIEWER_DIR = os.path.join(REPO_ROOT, "holes", "excursions",
                          "E-euclid-tutor-data", "viewer")
INDEX_HTML = os.path.join(VIEWER_DIR, "index.html")

# The goal queries, per prop. Referenced where the engine names them;
# restated verbatim (with provenance) where it does not.
GOALS = {
    # Lean's I.1: ∃ c, |c─a| = |a─b| ∧ |c─b| = |a─b| (c is a witness;
    # the engine's verify list named its circle BCD, so a student's own
    # circle names never reached "Solved")
    1: [("_eq_len", "c", "a", "a", "b"), ("_eq_len", "c", "b", "a", "b")],
    2: [("_eq_len", "a", "l", "b", "c")],   # eng.main() p=="2" assert
    3: [("between", "a", "e", "b"),         # eng.main() p=="3" asserts
        ("_eq_len", "a", "e", "c0", "c1")],
    4: [("_eq_len", "b", "c", "e", "f"),    # eng.main() p=="4" asserts
        ("_eq_ang", "a", "b", "c", "d", "e", "f"),
        ("_eq_ang", "a", "c", "b", "d", "f", "e")],
    5: [("_eq_ang", "a", "b", "c", "a", "c", "b"),  # eng.main() p=="5"
        ("_eq_ang", "c", "b", "d", "b", "c", "e")],
    6: [("_eq_len", "a", "b", "a", "c")],   # eng.main() p=="6" query
    7: [("false",)],                        # eng.main() p=="7" query
    8: [("_eq_ang", "b", "a", "c", "e", "d", "f")],  # eng.main() p=="8"
    9: [("_eq_ang", "b", "v", "f", "c", "v", "f"),  # eng.main() p=="9"
        ("neq", "v", "f")],
    10: [("between", "a", "d", "b"),        # tr.build_trace queries dict
         ("_eq_len", "a", "d", "d", "b")],
    11: [("not_on_line", "f", "AB"),        # tr.build_trace queries dict
         ("_rt_ang", "u", "w", "f")],
    12: [("_rt_ang", "e", "h", "c"),        # tr.build_trace queries dict
         ("on_line", "h", "AB")],           # (I.12's goal substitute)
    13: [eng.PROP13_GOAL],
    14: eng.PROP14_GOAL,
    15: eng.PROP15_GOAL,
    16: eng.PROP16B_GOAL,
    17: eng.PROP17_GOAL,
    18: eng.PROP18_GOAL,
    19: eng.PROP19_GOAL,
    20: eng.PROP20_GOAL,
    21: eng.PROP21_GOAL,
    22: eng.PROP22_GOAL,
    23: eng.PROP23_GOAL,
    24: eng.PROP24_GOAL,
    25: eng.PROP25_GOAL,
}

_PROP_RE = re.compile(r"^proposition_(\d+)'*$")


def palette(n):
    """Rule names a student may use at proposition N: every
    eng.CONSTRUCTIONS entry that is not a proposition, plus
    proposition_K and its primed variants for K < N only."""
    constructions, propositions = [], []
    for name in eng.CONSTRUCTIONS:
        m = _PROP_RE.match(name)
        if m:
            if int(m.group(1)) < n:
                propositions.append(name)
        else:
            constructions.append(name)
    return constructions + propositions


def _titles():
    """The viewer's option text per numeric proposition (index.html)."""
    titles = {}
    with open(INDEX_HTML) as fh:
        for m in re.finditer(r'<option value="(\d+)">([^<]+)</option>',
                             fh.read()):
            titles[int(m.group(1))] = m.group(2).strip()
    return titles


def _lean_statement(n):
    """Book/PropNN.lean from 'theorem' to ':='."""
    path = os.path.join(tr.LEAN_DIR, "Book", "Prop%02d.lean" % n)
    lines, collecting = [], False
    with open(path) as fh:
        for line in fh:
            if not collecting and line.lstrip().startswith("theorem"):
                collecting = True
            if collecting:
                stop = line.rstrip().endswith(":=")
                lines.append(line.rstrip() if not stop
                             else line.rstrip()[:-2].rstrip())
                if stop:
                    break
    return "\n".join(lines)


# ------------------------------------------------------- student wording

# What each rule DOES, in one sentence a student understands, with a
# group for the dropdown. Every CONSTRUCTIONS name must appear here (a
# test refuses an unlabelled rule). Groups: constructions (make new
# objects), diagrammatic (facts about points, lines and circles),
# metric (facts about lengths and angles), propositions (earlier
# propositions).
RULE_LABELS = {
    # constructions
    "arbitrary_point": ("take any point", "constructions"),
    "distinct_points": ("take two different points", "constructions"),
    "line_from_points": ("draw the line through two points",
                         "constructions"),
    "circle_from_points": ("draw the circle with centre a through b",
                           "constructions"),
    "intersection_circles": ("where two circles meet", "constructions"),
    "intersection_same_side": ("where two circles meet, on the same side "
                               "of a line as a given point",
                               "constructions"),
    "intersection_opposite_side": ("where two circles meet, on the "
                                   "opposite side of a line from a given "
                                   "point", "constructions"),
    "intersections_circle_line": ("where a line meets a circle",
                                  "constructions"),
    "intersection_circle_line_extending_points": (
        "where a line meets a circle, outside a given segment",
        "constructions"),
    "intersection_circle_line_between_points": (
        "where a line meets a circle, between two given points",
        "constructions"),
    "intersection_circle_line_1": (
        "where a line meets a circle (a point on each)", "constructions"),
    "intersection_circle_circle_1": (
        "where two circles meet (a point on each)", "constructions"),
    "intersection_lines": ("where two lines meet", "constructions"),
    "intersection_lines_opposing": (
        "where two lines meet, with two points on opposite sides",
        "constructions"),
    "exists_point_opposite": ("take a point on the other side of a line",
                              "constructions"),
    "extend_point": ("lengthen a line past a point", "constructions"),
    "extend_point_not_on_line": (
        "lengthen a line past a point, staying off another line",
        "constructions"),
    "extend_point_longer": ("lengthen a line beyond a given length",
                            "constructions"),
    "point_between_points_shorter_than": (
        "take a point on a line, nearer than a given length",
        "constructions"),
    "use_witness": ("give a name to the point a fact provides",
                    "constructions"),
    "name_point": ("call an existing point by a new name (e.g. the point "
                   "the theorem asks for)", "constructions"),
    # facts about points, lines and circles (the diagrammatic axioms)
    "between_symm_parts": ("betweenness: swap the ends, or take a part",
                           "diagrammatic"),
    "between_trans_out": ("betweenness chains along a line",
                          "diagrammatic"),
    "pasch_3_opposing": ("a line through one side of a triangle meets "
                         "another side (Pasch)", "diagrammatic"),
    "pasch_4": ("a line crossing two sides of a triangle (Pasch)",
                "diagrammatic"),
    "opposing_sides_if": ("two points are on opposite sides of a line "
                          "when the segment between them crosses it",
                          "diagrammatic"),
    "opposing_same_side_trans": ("across a line then back again lands "
                                 "on the opposite side", "diagrammatic"),
    "opposing_opposing_same_side": ("twice across a line lands on the "
                                    "same side", "diagrammatic"),
    "triple_incidence_1": ("three lines through two shared points meet "
                           "in one point", "diagrammatic"),
    "triple_incidence_2": ("three lines through two shared points: "
                           "another common meeting", "diagrammatic"),
    # facts about lengths and angles
    "superposition": ("slide one triangle onto an equal one "
                      "(superposition)", "metric"),
    "assert_point_eq_on_ray": ("two points at the same distance along a "
                               "ray are the same point", "metric"),
    "assert_point_eq_angle": ("two points making the same angle along a "
                              "ray are the same point", "metric"),
    "angle_pair_on_line": ("the two angles one line makes with another",
                           "metric"),
    "perpendicular_if": ("when the two angles at a crossing are equal, "
                         "they are right", "metric"),
    "perpendicular_onlyif": ("right angles at a crossing are equal",
                             "metric"),
    "sum_angles_onlyif": ("angles adding to two right angles", "metric"),
    "flat_angle_onlyif": ("a flat angle equals two right angles",
                          "metric"),
    "degenerated_angle_if": ("a zero angle means the two rays coincide",
                             "metric"),
    "angle_self_zero": ("the angle of a ray with itself is zero",
                        "metric"),
    "angle_ray_eq_zero": ("coincident rays make a zero angle", "metric"),
    "off_line_strict_triangle": ("off a line, the strict triangle "
                                 "inequality holds", "metric"),
    # earlier propositions
    "proposition_1": ("I.1: build an equilateral triangle on a segment",
                      "propositions"),
    "proposition_1'": ("I.1 (second form): an equilateral triangle from "
                       "three points", "propositions"),
    "proposition_2'": ("I.2: copy a length to start at a given point",
                       "propositions"),
    "proposition_3": ("I.3: cut off from the longer line a piece equal "
                      "to the shorter", "propositions"),
    "proposition_4": ("I.4: two triangles with two sides and the angle "
                      "between them equal", "propositions"),
    "proposition_5": ("I.5: the base angles of an isosceles triangle "
                      "are equal", "propositions"),
    "proposition_5'": ("I.5 (second form): isosceles base angles",
                       "propositions"),
    "proposition_6": ("I.6: a triangle with two equal angles has two "
                      "equal sides", "propositions"),
    "proposition_7": ("I.7: two different triangles cannot share both "
                      "sides on one segment", "propositions"),
    "proposition_8": ("I.8: two triangles with three equal sides",
                      "propositions"),
    "proposition_9'": ("I.9: bisect an angle", "propositions"),
    "proposition_10": ("I.10: bisect a segment", "propositions"),
    "proposition_11'": ("I.11: draw a line at right angles from a point "
                        "on a line", "propositions"),
    "proposition_13": ("I.13: angles on one side of a line make two "
                       "right angles", "propositions"),
    "proposition_15": ("I.15: opposite angles at a crossing are equal",
                       "propositions"),
    "proposition_16": ("I.16: an outside angle of a triangle beats "
                       "either far inside angle", "propositions"),
    "proposition_17": ("I.17: two angles of a triangle make less than "
                       "two right angles", "propositions"),
    "proposition_18": ("I.18: the greater side faces the greater angle",
                       "propositions"),
    "proposition_19": ("I.19: the greater angle faces the greater side",
                       "propositions"),
    "proposition_20": ("I.20: two sides of a triangle beat the third",
                       "propositions"),
    "proposition_21": ("I.21: a triangle inside a triangle has shorter "
                       "sides but a wider angle", "propositions"),
    "proposition_22'": ("I.22: build a triangle from three given "
                        "lengths", "propositions"),
    "proposition_22''": ("I.22 (second form): build a triangle from "
                         "three lengths, with a witness", "propositions"),
    "proposition_23": ("I.23: copy an angle onto a line", "propositions"),
    "proposition_23'": ("I.23 (second form): copy an angle onto a line, "
                        "with a witness", "propositions"),
    "proposition_24": ("I.24: two sides equal, the wider angle has the "
                       "longer base", "propositions"),
    "proposition_25": ("I.25: two sides equal, the longer base has the "
                       "wider angle", "propositions"),
}

# Cheap per-argument hints for the form labels, taken from the rule's
# premises; args not listed keep their engine name.
ARG_HINTS = {
    "line_from_points": {"a": "first point", "b": "second point"},
    "name_point": {"p": "the existing point"},
    "circle_from_points": {"a": "the centre", "b": "a point on the circle"},
    "intersection_circles": {"alpha": "first circle",
                             "beta": "second circle"},
    "intersections_circle_line": {"alpha": "the circle", "L": "the line"},
    "intersection_lines": {"L": "first line", "M": "second line"},
    "extend_point": {"L": "the line", "b": "extend past", "c": "from"},
    # the moves with many inputs (Joe, 2026-11-09: "it asks me for all
    # of this"); the page fills a circle's centre itself when it can
    "intersection_same_side": {
        "alpha": "first circle", "beta": "second circle",
        "b": "a point on the side you want", "c": "centre of the first circle",
        "d": "centre of the second circle",
        "L": "the line through both centres"},
    "intersection_opposite_side": {
        "alpha": "first circle", "beta": "second circle",
        "b": "a point on the side you do NOT want",
        "c": "centre of the first circle",
        "d": "centre of the second circle",
        "L": "the line through both centres"},
    "intersection_circle_line_extending_points": {
        "alpha": "the circle", "L": "the line",
        "b": "a point inside the circle, on the line",
        "c": "another point on the line (the new point is beyond the "
             "inside one, away from this one)"},
    "intersection_circle_line_between_points": {
        "alpha": "the circle", "L": "the line",
        "b": "a point inside the circle, on the line",
        "c": "a point outside the circle, on the line"},
    "intersection_circle_circle_1": {
        "a": "a point on the first circle, inside the second",
        "b": "a point on or inside the first circle, outside the second",
        "alpha": "first circle", "beta": "second circle"},
    "intersection_circle_line_1": {
        "a": "a point on or inside the circle",
        "b": "a point on or inside the circle, across the line",
        "alpha": "the circle", "L": "the line"},
    "intersection_lines_opposing": {
        "a": "a point on one side of the first line",
        "b": "a point on the other side", "L": "the first line",
        "M": "the line through those two points"},
    "extend_point_not_on_line": {
        "L": "the line", "M": "a different line to avoid",
        "b": "extend past", "c": "from"},
    "extend_point_longer": {
        "L": "the line", "b": "extend past", "c": "from",
        "s0": "start of a length to exceed", "s1": "end of that length"},
    "point_between_points_shorter_than": {
        "L": "the line", "b": "from", "c": "towards",
        "u": "start of a length", "v": "end of that length"},
}

_FACT_WORDS = {
    "neq": "{1} ≠ {2}",
    "on_line": "{1} is on line {2}",
    "not_on_line": "{1} is not on line {2}",
    "on_circle": "{1} is on circle {2}",
    "centre": "{1} is the centre of circle {2}",
    "between": "{2} is between {1} and {3}",
    "same_side": "{1} and {2} are on the same side of {3}",
    "not_same_side": "{1} and {2} are not on the same side of {3}",
    "opposing_sides": "{1} and {2} are on opposite sides of {3}",
    "intersects_cc": "the circles {1} and {2} intersect",
    "intersects_lc": "line {1} meets circle {2}",
    "inside": "{1} is inside circle {2}",
    "outside": "{1} is outside circle {2}",
    "not_outside": "{1} is on or inside circle {2}",
    "dpl": "{1} and {2} are distinct points on line {3}",
    "peq": "{1} and {2} are the same point",
    "neq_line": "lines {1} and {2} are different",
    "peq_line": "lines {1} and {2} are the same line",
    "eq_len": "|{1}{2}| = |{3}{4}|",
    "_eq_len": "|{1}{2}| = |{3}{4}|",
    "gt_len": "|{1}{2}| > |{3}{4}|",
    "lt_len": "|{1}{2}| < |{3}{4}|",
    "eq_ang": "∠{1}{2}{3} = ∠{4}{5}{6}",
    "_eq_ang": "∠{1}{2}{3} = ∠{4}{5}{6}",
    "gt_ang": "∠{1}{2}{3} > ∠{4}{5}{6}",
    "_rt_ang": "∠{1}{2}{3} is right",
    "rt_ang": "∠{1}{2}{3} is right",
    "right_ang": "∠{1}{2}{3} is right",
    "flat_ang": "∠{1}{2}{3} is a straight angle",
    "zero_ang": "∠{1}{2}{3} is zero",
    "pos_len": "|{1}{2}| > 0",
    "_lt_len": "|{1}{2}| < |{3}{4}|",
    "_gt_len": "|{1}{2}| > |{3}{4}|",
    "_gt_ang": "∠{1}{2}{3} > ∠{4}{5}{6}",
    "gt_sum_len": "|{1}{2}| + |{3}{4}| > |{5}{6}|",
    "lt_sum_sum_len": "|{1}{2}| + |{3}{4}| < |{5}{6}| + |{7}{8}|",
    "sum_rt_ang": "∠{1}{2}{3} and ∠{4}{5}{6} together make two right angles",
    "lt_sum_rt_ang": "∠{1}{2}{3} + ∠{4}{5}{6} is less than two right angles",
}


def _flat_args(xs):
    """Fact arguments with nested tuples/lists spread out (the engine's
    entail premises carry their arguments as one tuple)."""
    out = []
    for x in xs:
        if isinstance(x, (list, tuple)):
            out.extend(_flat_args(x))
        else:
            out.append(x)
    return out


def fact_words(f):
    """A fact tuple as a student-readable clause (mirrors the page's
    factEnglish; used so refusal text never shows a Python repr)."""
    f = list(f)
    kind = f[0] if f else ""
    if kind == "fresh":
        return ("%s already exists; choose a new name, or show that the "
                "new point is %s" % (f[1], f[1]))
    if kind == "entail" and len(f) >= 2:
        # MissingPremise for a metric fact: ("entail", kind, (args...)).
        return fact_words([f[1]] + _flat_args(f[2:]))
    if kind == "either" and len(f) == 3:
        def side(d):
            if d and isinstance(d[0], (list, tuple)):
                return " and ".join(fact_words(x) for x in d)
            return fact_words(d)
        return "either %s, or %s" % (side(f[1]), side(f[2]))
    tmpl = _FACT_WORDS.get(kind)
    args = [str(x) for x in _flat_args(f[1:])]
    if tmpl is not None:
        try:
            return tmpl.format(*([""] + args))
        except IndexError:
            pass
    # Unknown kind or shape: plain words, never a Python repr.
    return " ".join([str(kind).strip("_").replace("_", " ")] + args)


def student_error(err, rule=None):
    """The engine's refusal err (a fact list, or its raw string) as a
    sentence for a student. The raw text stays available on the page
    under a details toggle; this is what is shown instead. Never
    returns a Python repr."""
    if isinstance(err, (list, tuple)):
        f = list(err)
        if f and f[0] == "fresh":
            return fact_words(f)
        if f and f[0] == "neq" and len(f) == 3:
            # the move needs two different points, and nothing shown
            # rules out their being the same (I.2: a may coincide with c)
            return ("Not yet shown: %s and %s are different points. "
                    "Nothing given or shown so far rules out %s = %s."
                    % (f[1], f[2], f[1], f[2]))
        return "Not yet shown: " + fact_words(f)
    s = str(err)
    m = re.match(r"^unknown rule '(.+)' \(not in I\.(\d+)'s palette\)$", s)
    if m:
        return ("There is no move called '%s' in I.%s — choose one from "
                "the list." % (m.group(1), m.group(2)))
    m = re.match(r"^I\.(\d+) is not proved yet$", s)
    if m:
        return ("I.%s has not been proved yet — finish the earlier "
                "propositions first." % m.group(1))
    if s == "blank input":
        return ("Fill in every box first: click the objects on the "
                "drawing, or type their names.")
    m = re.match(r"^no object called (.+)$", s)
    if m:
        return ("There is nothing called %s in the drawing yet — click "
                "an object to use its name." % m.group(1))
    if s.startswith("bad arity"):
        spec = eng.CONSTRUCTIONS.get(rule)
        if spec is not None:
            return ("That is not what this move needs: %s takes %d "
                    "name(s) (%s)." % (rule, len(spec["args"]),
                                       ", ".join(spec["args"])))
        return "That is not the right number of names for this move."
    if re.match(r"^a (by_contra|by_cases) needs at least one case$", s):
        return ("This block needs at least one case — add a case before "
                "closing it.")
    if s == "missing case":
        return "These cases leave a possibility out."
    if s.startswith("an earlier case must close first"):
        return ("An earlier case does not work yet — finish it before "
                "this one.")
    if s.startswith("bad case hypothesis"):
        return ("This case's assumption cannot be made — check the "
                "names in it.")
    if s.startswith("bad case move"):
        return "A move inside this case does not fit — check its names."
    if "did not reach a contradiction" in s or \
            "does not reach a contradiction" in s:
        return ("This case does not lead to a contradiction yet: keep "
                "going inside it, or check its assumptions.")
    if "does not reach the conclusion" in s or \
            s.startswith("by_cases branch"):
        return ("This case does not reach what you are proving yet: "
                "keep going inside it, or check its assumptions.")
    if s.startswith("the case") and "is stuck" in s:
        return "This case is stuck: a fact it needs is not shown yet."
    if s.startswith("the case") and "is contradictory" in s:
        return ("This case leads to a contradiction, so it would prove "
                "anything — check its assumptions.")
    return "The move was refused — open details to see the exact reason."


def rule_info(name):
    """{args, cite, binds, label, group, arg_hints} for a rule. binds are
    the base names the make introduces, read off the make lambda's
    returned pairs on a throwaway state (euclid_engine.py is NOT
    modified). If the make lambda cannot be run that way, binds is [] —
    the page then shows one free "name" field instead of per-bind
    inputs. label/group are the student wording from RULE_LABELS;
    arg_hints the cheap per-arg hints from ARG_HINTS."""
    spec = eng.CONSTRUCTIONS[name]
    binds = []
    try:
        made = spec["make"](eng.State(),
                            *["#arg%d" % k for k in range(len(spec["args"]))])
        binds = [b for b, _facts in made if b is not None]
    except Exception:
        binds = []
    label, group = RULE_LABELS.get(name, (name.replace("_", " "),
                                          "constructions"))
    return {"args": list(spec["args"]), "cite": spec["cite"],
            "binds": binds, "label": label, "group": group,
            "arg_hints": dict(ARG_HINTS.get(name, {}))}


def _goal_reached(n, s, objects, xy=None):
    """GOALS[n] with its existential witnesses free. A goal name that is
    not one of the proposition's given objects is a witness of Lean's
    ∃ (I.1's c, I.2's l, ...): any point in the drawing may play it, so
    a student who names the point (or the circles on the way) differently
    still solves the proposition. Backtracking search, binding each
    witness at the first goal that mentions it. (Joe, 2026-11-09: I.1
    built with circles C and D never showed "Solved", because the goal
    named the engine's circle BCD.)"""
    goals = [tuple(g) for g in GOALS[n]]
    given = {o["name"] for o in tr.objects_from_hyps(n)}
    wit = []
    for g in goals:
        for x in g[1:]:
            if isinstance(x, str) and x not in given and x not in wit:
                wit.append(x)
    if all(eng._fact_holds(s, g) for g in goals):
        return True   # the engine's own names (fast path)
    if not wit:
        return False
    points = {o["name"] for o in objects if o.get("kind") == "point"}
    for f in s.facts:
        if f[0] in ("on_line", "on_circle", "not_on_line", "centre"):
            points.add(f[1])
        elif f[0] in ("neq", "peq"):
            points.update(f[1:3])
        elif f[0] == "between":
            points.update(f[1:4])
    points = sorted(p for p in points if isinstance(p, str))

    # a metric goal over a length or angle no row mentions cannot be
    # entailed: skip it before asking the (slow) linear-arithmetic check
    atoms = {a for row in s.la.rows for a, _c in row[0]}

    def _atoms_present(g):
        k = g[0]
        if k == "_eq_len" and \
                eng.len_atom(g[1], g[2]) == eng.len_atom(g[3], g[4]):
            return True   # |xy| = |xy| holds with no row
        if k in ("_eq_len", "_gt_len", "_lt_len"):
            return (eng.len_atom(g[1], g[2]) in atoms
                    and eng.len_atom(g[3], g[4]) in atoms)
        if k in ("_eq_ang", "_gt_ang"):
            return (eng.ang_atom(*g[1:4]) in atoms
                    and eng.ang_atom(*g[4:7]) in atoms)
        return True

    import math

    def _pt(x):
        v = (xy or {}).get(x)
        return v if v is not None and len(v) == 2 else None

    def _d(p, q):
        return math.hypot(p[0] - q[0], p[1] - q[1])

    def _a(p, v, q):
        a1 = math.atan2(p[1] - v[1], p[0] - v[0])
        a2 = math.atan2(q[1] - v[1], q[0] - v[0])
        t = abs(a1 - a2) % (2 * math.pi)
        return min(t, 2 * math.pi - t)

    def _drawn_ok(g):
        # a numeric prefilter only: a witness must satisfy a metric goal
        # in the drawing (tolerance 1e-6). When a point has no drawn
        # position (made inside a case), the exact check decides.
        k = g[0]
        if k in ("_eq_len", "_gt_len"):
            ps = [_pt(x) for x in g[1:5]]
            if None in ps:
                return True
            l1, l2 = _d(ps[0], ps[1]), _d(ps[2], ps[3])
            return abs(l1 - l2) < 1e-6 if k == "_eq_len" else l1 > l2 - 1e-6
        if k in ("_eq_ang", "_gt_ang"):
            ps = [_pt(x) for x in g[1:7]]
            if None in ps:
                return True
            a1, a2 = _a(*ps[0:3]), _a(*ps[3:6])
            return abs(a1 - a2) < 1e-6 if k == "_eq_ang" else a1 > a2 - 1e-6
        return True

    def ground(g, env):
        return tuple(env.get(x, x) if isinstance(x, str) else x for x in g)

    def search(i, env):
        if i == len(goals):
            return True
        g = goals[i]
        free = [x for x in g[1:] if x in wit and x not in env]
        free = list(dict.fromkeys(free))
        if not free:
            return eng._fact_holds(s, ground(g, env)) and search(i + 1, env)
        x = free[0]
        for p in points:
            env2 = dict(env)
            env2[x] = p
            # bind the remaining free names of this goal before testing
            if search_goal(g, env2, i):
                return True
        return False

    def search_goal(g, env, i):
        free = [x for x in g[1:] if x in wit and x not in env]
        if free:
            for p in points:
                env2 = dict(env)
                env2[free[0]] = p
                if search_goal(g, env2, i):
                    return True
            return False
        gg = ground(g, env)
        if not _atoms_present(gg) or not _drawn_ok(gg):
            return False
        try:
            ok = eng._fact_holds(s, gg)
        except Exception:
            ok = False
        return ok and search(i + 1, env)

    return search(0, {})


def catalogue():
    """Ordered list, one entry per proposition N in tr.FREE."""
    titles = _titles()
    out = []
    for n in sorted(tr.FREE):
        hyps, _steps = tr.hyps_and_steps(n)
        out.append({
            "n": n,
            "title": titles.get(n, "I.%d" % n),
            "statement": _lean_statement(n),
            "hyps": [list(f) for f, _w in hyps],
            "goals": [list(g) for g in GOALS[n]],
            "palette": [dict({"name": nm}, **rule_info(nm))
                        for nm in palette(n)],
        })
    return out


def _start_state(n):
    """The start state exactly as euclid_trace.build_trace builds it:
    metric hyps for 4/5, the prop-3 row, then close."""
    hyps, _steps = tr.hyps_and_steps(n)
    if n in (4, 5):
        s = eng._metric_state(hyps)
    else:
        s = eng.State()
        for f, w in hyps:
            # as eng.replay() does (tr.build_trace's path for 10-21):
            # metric hyps (e.g. I.18's gt_len) also add their LA row
            eng._assume_metric_or_fact(s, f, w)
    if n == 3:
        s.la.add({eng.len_atom("a", "b"): 1,
                  eng.len_atom("c0", "c1"): -1}, 0, ">")
    eng.close(s)
    return s


CASE_RULES = ("by_contra", "by_cases")


def _move_to_step(m):
    """A move dict as an engine step tuple. Case moves become the
    ("by_contra"/"by_cases", branches, conclusions) triples the engine's
    own STEPS_DATA uses (nested case moves recurse); anything else is
    (rule, args, bindings)."""
    if m["rule"] in CASE_RULES:
        return _block_to_engine(m)
    return (m["rule"], tuple(m.get("args") or ()),
            dict(m.get("bindings") or {}))


def _place_name_point(args, added, xy, objects):
    """name_point draws nothing new: the new name sits where its point
    is (an object with the same position, so the goal and the board can
    use it)."""
    p = args[0]
    for f in added:
        if f[0] == "peq" and f[2] == p:
            x = f[1]
            xy[x] = xy.get(p)
            objects.append({"name": x, "kind": "point",
                            "jsx": {"type": "point"}, "def_step": None,
                            "xy": list(xy[p]) if xy.get(p) else None})


def _block_to_engine(mv):
    """{rule, branches: [{hyps, moves}], conclusion} as the engine's
    (rule, [(assumption-facts, steps), ...], conclusions) tuple."""
    branches = []
    for b in mv.get("branches") or []:
        hyps = [tuple(f) for f in b.get("hyps") or []]
        steps = [_move_to_step(m) for m in b.get("moves") or []]
        branches.append((hyps, steps))
    concl = [tuple(c) for c in mv.get("conclusion") or []]
    if mv["rule"] == "by_cases":
        # the engine checks a by_cases conclusion with _fact_holds, which
        # answers metric facts only in their query form (_eq_len ...);
        # the page writes the plain form (Joe at I.2, 2026-11-09: the
        # a = c case's |al| = |bc| was "not entailed")
        concl = [_metric_query(c) for c in concl]
    return (mv["rule"], branches, concl)


def _metric_query(c):
    k = c[0]
    if k == "lt_len":
        return ("_gt_len", c[3], c[4], c[1], c[2])
    if k in ("eq_len", "gt_len", "eq_ang", "gt_ang"):
        return ("_" + k,) + tuple(c[1:])
    return c


def _empty_block(mv):
    """The first case move (at any depth, closed or open) with no
    branches, else None. The engine leaves exhaustiveness of a case
    split to the caller; with zero branches nothing is checked at all
    and the block would assert its conclusion outright (review,
    claude-9: an empty by_contra 'proved' I.21's between b d e)."""
    if mv.get("rule") not in CASE_RULES:
        return None
    if not mv.get("branches"):
        return mv
    for b in mv["branches"]:
        for m in b.get("moves") or []:
            hit = _empty_block(m)
            if hit is not None:
                return hit
    return None


# ---------------------------------------------------------------------------
# Case-split coverage referee (E-kimi-task-419, packet
# P-euclid-play-case-coverage from claude-9). The engine leaves
# exhaustiveness of a case split to the caller (documented at
# eng._by_contra / eng._by_cases); here the caller checks it against the
# System E trichotomy families (between_points, Diagrammatic.lean:87;
# same/opposing/on a line; length/angle comparison; a fact and its
# negation). A family member counts as covered when a case's hypotheses
# entail it on a closed copy of the state, when assuming it is already
# contradictory, or (by_contra) when the conclusion entails it.
# ---------------------------------------------------------------------------


def _seg(x, y):
    return tuple(sorted((x, y)))


def _ang(x, v, y):
    return (v,) + tuple(sorted((x, y)))


def _norm(f):
    """Canonical form for comparing case/member facts: between is
    symmetric in the endpoints, same_side/opposing_sides/ne? in the two
    points, eq_len/eq_ang in the two sides, lt is gt swapped. Angles keep
    the vertex in front: (v, side1, side2) with side1 < side2."""
    f = tuple(f)
    k = f[0]
    if k in ("between", "not_between") and f[3] < f[1]:
        return (k, f[3], f[2], f[1])
    if k in ("same_side", "opposing_sides", "not_same_side") \
            and f[2] < f[1]:
        return (k, f[2], f[1], f[3])
    if k in ("peq", "neq") and f[2] < f[1]:
        return (k, f[2], f[1])
    if k in ("eq_len", "_eq_len", "gt_len", "_gt_len", "lt_len"):
        s1, s2 = _seg(f[1], f[2]), _seg(f[3], f[4])
        if k in ("eq_len", "_eq_len") and s2 < s1:
            s1, s2 = s2, s1
        if k == "lt_len":
            s1, s2 = s2, s1
            k = "gt_len"
        return (k,) + s1 + s2
    if k in ("eq_ang", "_eq_ang", "gt_ang", "_gt_ang"):
        a1, a2 = _ang(f[1], f[2], f[3]), _ang(f[4], f[5], f[6])
        if k in ("eq_ang", "_eq_ang") and a2 < a1:
            a1, a2 = a2, a1
        return (k,) + a1 + a2
    if k in ("neq_line", "peq_line") and f[2] < f[1]:
        return (k, f[2], f[1])
    return f


def _peq_line_holds(s, L, M):
    """Lines as sets of points: L = M when two distinct points lie on
    both. (The engine has no line-equality fact; I.14's conclusion
    "BC = BD" is exactly this, as mutual incidence.)"""
    if L == M:
        return True
    on = {}
    for f in s.facts:
        if f[0] == "on_line":
            on.setdefault(f[2], set()).add(f[1])
        elif f[0] == "dpl":
            on.setdefault(f[3], set()).update((f[1], f[2]))
    common = on.get(L, set()) & on.get(M, set())
    for p in common:
        for q in common:
            if p < q and (s.has(("neq", p, q)) or s.has(("neq", q, p))):
                return True
    return False


_METRIC_HOLDS = {"eq_len": "_eq_len", "gt_len": "_gt_len",
                 "lt_len": "_gt_len", "eq_ang": "_eq_ang",
                 "gt_ang": "_gt_ang"}


def _holds(s, f):
    """_fact_holds with the plain metric names mapped to the LA-query
    forms and the virtual peq_line fact."""
    f = tuple(f)
    if f[0] == "peq_line":
        return _peq_line_holds(s, f[1], f[2])
    if f[0] in ("between", "not_between"):
        return eng._fact_holds(s, f) or \
            eng._fact_holds(s, (f[0], f[3], f[2], f[1]))
    if f[0] in ("peq", "neq", "neq_line"):
        # symmetric; the engine's raw lookup is order-sensitive (kimi-4,
        # E-kimi-task-421: I.8's conclusion peq d c' vs the member peq c' d)
        return eng._fact_holds(s, f) or \
            eng._fact_holds(s, (f[0], f[2], f[1]))
    if f[0] in ("opposing_sides", "same_side", "not_same_side"):
        # symmetric in the two points (Relations.lean:13-14 for
        # opposing); the engine's closure symmetrizes same_side only
        return eng._fact_holds(s, f) or \
            eng._fact_holds(s, (f[0], f[2], f[1], f[3]))
    if f[0] in ("_zero_ang", "zero_ang") and len(f) >= 4:
        # ∠x:v:y = 0: the stored fact (either form) or the LA row; the
        # closure's r_degenerated_angle stores ("zero_ang", v, x, y, L)
        # with the row (kimi-4, E-kimi-task-422: I.23's inner split)
        x, v, y = f[1:4]
        if s.has(("zero_ang", x, v, y)) or s.has(("zero_ang", y, v, x)):
            return True
        # x = y: ∠x:v:x = 0 (degenerated_angle_if with c := b, the
        # engine's angle_self_zero), given v ≠ x (I.23: d = e is a
        # possibility formRectilinearAngle leaves open)
        if (_holds(s, ("peq", x, y)) or x == y) and \
                _holds(s, ("neq", x, v)):
            return True
        try:
            return s.la.entails({eng.ang_atom(x, v, y): 1}, 0, "=")
        except eng.FMTooLarge:
            return False
    if f[0] == "lt_len":  # |x-y| < |u-v| as _gt_len u v x y
        return eng._fact_holds(s, ("_gt_len", f[3], f[4], f[1], f[2]))
    return eng._fact_holds(s, (_METRIC_HOLDS.get(f[0], f[0]),) + f[1:])


def _enrich(s, _depth=0):
    """Referee-side derivations, all System E theorems the engine's
    closure only reaches through cited steps (it cannot use them by
    default: they are construction rules). Used ONLY on probe copies for
    coverage questions, never on the played state:
      between parts (Diagrammatic.lean:45-46, neqs from a between),
      pasch_3 (between p q r, q on L, p off L => opposing_sides p r L),
      opposing_same_side_trans,
      right-angle transport along a line (right_ang x v y, z on the line
      v-y, z != v => right_ang x v z) and across a between (right_ang
      x v y, between x v r => right_ang r v y) — the two halves of
      perpendicular_onlyif/perpendicular_if used in I.13's nested split.
    Metric rows are added directly (ang_atom = RT) since
    _assume_metric_or_fact has no _rt_ang case."""
    if _depth > 8:
        return
    facts = list(s.facts)
    add = []
    for f in facts:
        if f[0] == "between":
            p, q, r = f[1:4]
            for a, b in ((p, q), (q, r), (p, r)):
                if not s.has(("neq", a, b)):
                    add.append(("neq", a, b))
    for f in facts:
        if f[0] != "between":
            continue
        p, q, r = f[1:4]
        for g in facts:
            if g[0] == "on_line" and g[1] == q \
                    and s.has(("not_on_line", p, g[2])):
                cand = ("opposing_sides", p, r, g[2])
                if not s.has(cand):
                    add.append(cand)
    for f in facts:
        if f[0] != "opposing_sides":
            continue
        p, q, L = f[1:4]
        for g in facts:
            if g[0] == "same_side" and g[3] == L:
                if g[2] == q and not s.has(("opposing_sides", g[1], p, L)):
                    add.append(("opposing_sides", g[1], p, L))
                if g[1] == q and not s.has(("opposing_sides", g[2], p, L)):
                    add.append(("opposing_sides", g[2], p, L))
    # opposing_opposing_same_side (the engine's cited rule of that name):
    # p opposite q and q opposite r across L => p, r on the same side
    opp = [f for f in facts if f[0] == "opposing_sides"]
    for f in opp:
        for g in opp:
            if f[3] != g[3]:
                continue
            ends = None
            if f[2] == g[1]:
                ends = (f[1], g[2])
            elif f[2] == g[2]:
                ends = (f[1], g[1])
            elif f[1] == g[1]:
                ends = (f[2], g[2])
            elif f[1] == g[2]:
                ends = (f[2], g[1])
            if ends and ends[0] != ends[1]:
                cand = ("same_side", ends[0], ends[1], f[3])
                if not s.has(cand) and cand not in add:
                    add.append(cand)
    for f in add:
        s.add(f, "coverage referee", [])
    rts = [f for f in facts if f[0] == "right_ang"]
    metric = []
    for f in rts:
        x, v, y = f[1:4]
        for g in facts:
            if g[0] == "on_line" and g[1] == v \
                    and s.has(("on_line", y, g[2])):
                for h in facts:
                    if h[0] == "on_line" and h[2] == g[2] \
                            and h[1] not in (v, y) \
                            and s.has(("neq", h[1], v)):
                        cand = ("right_ang", x, v, h[1])
                        if cand not in s.facts and cand not in metric:
                            metric.append(cand)
        for g in facts:
            if g[0] == "between" and g[1] == x and g[2] == v:
                cand = ("right_ang", g[3], v, y)
                if cand not in s.facts and cand not in metric:
                    metric.append(cand)
    for c in metric:
        x, v, y = c[1:4]
        s.la.add(eng.lin((eng.ang_atom(x, v, y), 1), (eng.RT, -1)), 0, "=")
        s.add(c, "coverage referee", [])
    if add or metric:
        eng.close(s)
        _enrich(s, _depth + 1)


def _order_contradiction(t):
    """System E between_not_trans (Diagrammatic.lean:96): between a b c
    and between a b d give not between c b d. The engine has no negated
    between fact, so its contradictory() cannot see this clash; Lean's SMT
    does (it is why Prop22.lean's trichotomy on g, f, i needs only three
    cases: the fourth order, between f g i, meets between h g f and
    between h g i). True when the probe holds all three."""
    bt = {f[1:4] for f in t.facts if f[0] == "between"}
    by_mid = {}
    for a, b, c in bt:
        by_mid.setdefault((a, b), set()).add(c)
    for (a, b), ends in by_mid.items():
        for c in ends:
            for d in ends:
                if c != d and ((c, b, d) in bt or (d, b, c) in bt):
                    return True
    return False


def _plain_contradictory(t):
    return eng.contradictory(t)[0] or _order_contradiction(t)


def _probe(s, facts, enrich=True):
    """A copy of s with `facts` assumed and closed; with enrich, also
    referee-enriched (_enrich). The contradiction probe uses the plain
    engine close ONLY (packet: "eng.contradictory after close") — the
    enrichment would already refute e.g. I.21's dropped between case via
    pasch_3, hiding it from "missing"."""
    t = s.copy()
    for f in facts:
        eng._assume_metric_or_fact(t, tuple(f), "coverage probe")
    eng.close(t)
    if enrich:
        _enrich(t)
    return t


def _families(rule, cases, conclusion):
    """Candidate System E trichotomy families touching the case (and, for
    by_contra, conclusion) facts, in packet order: between, two points
    and a line, length/angle comparison, a fact and its negation."""
    pool = [tuple(f) for c in cases for f in c]
    if rule == "by_contra":
        pool += list(conclusion)
    fams, seen = [], set()

    def add(name, members, key):
        if key not in seen:
            seen.add(key)
            fams.append((name, members))

    for f in pool:
        k = f[0]
        if k == "between":
            p, q, r = sorted(f[1:4])
            # between_points (Diagrammatic.lean:87) needs the three
            # points DISTINCT (and on one line, checked in coverage):
            # a coincidence is a possibility too, covered only when the
            # state shows the two points differ (the peq probe is then
            # contradictory) or a case covers it. Review (claude-9,
            # E-kimi-task-422): without these, I.22's split with its
            # "i = f" case dropped was accepted.
            add("where %s, %s and %s lie on their line" % (p, q, r),
                [("between", p, q, r), ("between", q, p, r),
                 ("between", p, r, q), ("peq", p, q), ("peq", q, r),
                 ("peq", p, r)], ("between", p, q, r))
        if k in ("between", "not_between"):
            # a fact and its negation (I.24's inner by_contra: between
            # g' d e or not); between is symmetric in its endpoints
            x, m, y = f[1:4]
            x, y = min(x, y), max(x, y)
            add("whether %s is between %s and %s" % (m, x, y),
                [("between", x, m, y), ("not_between", x, m, y)],
                ("bt", x, m, y))
        elif k in ("same_side", "opposing_sides", "not_same_side"):
            p, q, L = f[1], f[2], f[3]
            a, b = min(p, q), max(p, q)
            add("which side of %s %s and %s are on" % (L, a, b),
                [("same_side", a, b, L), ("opposing_sides", a, b, L),
                 ("on_line", a, L), ("on_line", b, L)],
                ("sides", a, b, L))
            if k in ("same_side", "not_same_side"):
                # a fact and its negation (not_same_side: kimi-4's fact
                # kind for Lean's literal negation, I.24)
                add("whether %s and %s are on the same side of %s"
                    % (a, b, L),
                    [("same_side", a, b, L), ("not_same_side", a, b, L)],
                    ("ss", a, b, L))
        elif k in ("eq_len", "_eq_len", "gt_len", "_gt_len", "lt_len"):
            s1, s2 = _seg(f[1], f[2]), _seg(f[3], f[4])
            add("|%s%s| against |%s%s|" % (s1 + s2),
                [("eq_len",) + s1 + s2, ("gt_len",) + s1 + s2,
                 ("gt_len",) + s2 + s1], ("len", s1, s2))
        elif k in ("eq_ang", "_eq_ang", "gt_ang", "_gt_ang"):
            (v1, x1, y1), (v2, x2, y2) = _ang(*f[1:4]), _ang(*f[4:7])
            a1 = (x1, v1, y1)
            a2 = (x2, v2, y2)
            add("∠%s%s%s against ∠%s%s%s" % (a1 + a2),
                [("eq_ang",) + a1 + a2, ("gt_ang",) + a1 + a2,
                 ("gt_ang",) + a2 + a1], ("ang", a1, a2))
        elif k in ("peq", "neq"):
            a, b = min(f[1:3]), max(f[1:3])
            add("whether %s and %s are the same point" % (a, b),
                [("peq", a, b), ("neq", a, b)], ("pt", a, b))
        elif k in ("on_line", "not_on_line"):
            add("whether %s is on %s" % (f[1], f[2]),
                [("on_line", f[1], f[2]), ("not_on_line", f[1], f[2])],
                ("ol", f[1], f[2]))
        elif k == "neq_line":
            a, b = min(f[1:3]), max(f[1:3])
            add("whether %s and %s are the same line" % (a, b),
                [("neq_line", a, b), ("peq_line", a, b)], ("ln", a, b))
    return fams


def coverage(s, rule, cases, conclusion, refuted=()):
    """Is the case split exhaustive? cases: list of hypothesis-fact
    lists; conclusion: the by_contra conclusions (the not-any-case side).
    refuted: conjunctions known NOT to hold in this context (a fact a
    member would contradict, e.g. the sibling cases of an enclosing
    by_cases catch-all branch). Returns {"covered": bool, "missing":
    [uncovered member facts], "family": name or None}."""
    cases = [[tuple(f) for f in c] for c in cases]
    concl = [tuple(f) for f in (conclusion or ()) if f[0] != "false"]
    norm_pool = {_norm(f) for c in cases for f in c}
    if rule == "by_contra":
        norm_pool |= {_norm(f) for f in concl}
    catch_all = rule == "by_cases" and any(not c for c in cases) \
        and any(c for c in cases)

    def member_covered(m, extra=()):
        # extra: the other coordinates of a product member (a 2x2 split
        # such as I.7's, a vs CD crossed with d vs AC)
        if extra:
            ms = [m] + list(extra)
            tp = _probe(s, ms, enrich=False)
            if _plain_contradictory(tp):
                return True
            tm = _probe(s, ms)
            probes = [tp] + ([] if eng.contradictory(tm)[0] else [tm])
            for t in probes:
                for hyps in cases:
                    if hyps and all(_holds(t, h) for h in hyps):
                        return True
                if rule == "by_contra" and concl and \
                        all(_holds(t, c) for c in concl):
                    return True
            if refuted:
                for r in refuted:
                    if r and all(_holds(tm, f) for f in r):
                        return True
            return False
        # A case covers member m when m (with the state) entails EVERY
        # hypothesis of the case, i.e. the case is no stronger than m.
        # Review (claude-9): the converse test (case entails m) let a case
        # carry extra, unentailed assumptions: {between b e d, X} counted
        # as covering between b e d while refuting only the part where X
        # holds -- the same diagram trust found in the engine's I.7.
        # First on a plain-closed probe; then on the referee-enriched one,
        # unless that is already contradictory (it would entail every
        # hypothesis vacuously; impossibility is left to the plain-close
        # probe below, so a refutable-but-unrefuted case still shows as
        # missing).
        tp = _probe(s, [m], enrich=False)
        tm = _probe(s, [m])
        probes = [tp] + ([] if eng.contradictory(tm)[0] else [tm])
        for t in probes:
            for hyps in cases:
                if hyps and all(_holds(t, h) for h in hyps):
                    return True
        if _plain_contradictory(tp):
            return True
        if refuted:
            t = _probe(s, [m])
            for r in refuted:
                if r and all(_holds(t, f) for f in r):
                    return True
        # by_contra: the conclusion side covers m when m entails the
        # conclusion (same direction as the cases above; review,
        # claude-9: "conclusion entails m" let a family the state alone
        # decides -- a on AB -- count as covered)
        if rule == "by_contra" and concl:
            for t in probes:
                if all(_holds(t, c) for c in concl):
                    return True
            # the virtual peq_line L M: the engine cannot assume line
            # equality, so substitute: each conclusion fact on_line x L
            # (or x M) must follow from x lying on the other line
            if m[0] == "peq_line":
                L, M = m[1], m[2]

                def by_subst(c):
                    if c[0] != "on_line":
                        return False
                    other = {L: M, M: L}.get(c[2])
                    return other is not None and \
                        s.has(("on_line", c[1], other))
                if all(by_subst(c) for c in concl):
                    return True
        return False

    fams = _families(rule, cases, concl)
    # a disjunction already in the state is itself an exhaustive family
    # (e.g. proposition_23''s "f on AB, or on x's side", used by I.24)
    for f in s.facts:
        if f[0] == "either":
            fams.append(("the known disjunction: %s or %s"
                         % (" ".join(f[1]), " ".join(f[2])),
                         [tuple(f[1]), tuple(f[2])]))
    def lines_of(x):
        return {f[2] for f in s.facts if f[0] == "on_line" and f[1] == x}

    def collinear_fit(name, members):
        # A between family (between_points, Diagrammatic.lean:87) is
        # exhaustive only for points on one line. If the state puts all
        # three on a line, use it as is. If two are on a line L and the
        # third x is not known to be, the family becomes conditional:
        # x off L, or x on L together with each order/coincidence (I.24:
        # g off DF, or on DF in one of four positions). Otherwise drop it.
        # Review (claude-9, E-kimi-task-422).
        bts = [m for m in members if m[0] == "between"]
        if not bts or any(isinstance(m, list) for m in members):
            return (name, members)
        pts = list(bts[0][1:4])
        common = set.intersection(*(lines_of(x) for x in pts))
        if common:
            return (name, members)
        for x in pts:
            others = [y for y in pts if y != x]
            Ls = sorted(lines_of(others[0]) & lines_of(others[1]))
            if Ls:
                L = Ls[0]
                return ("whether %s is on %s, and if so where %s"
                        % (x, L, name.replace("where ", "", 1)),
                        [("not_on_line", x, L)]
                        + [[m, ("on_line", x, L)] for m in members])
        return None

    matching = []
    for name, members in fams:
        if not any(_norm(m) in norm_pool for m in members
                   if not isinstance(m, list)):
            continue
        fit = collinear_fit(name, members)
        if fit is not None:
            matching.append(fit)
    if not matching:
        return {"covered": False, "missing": [], "family": None}
    best = None
    for name, members in matching:
        missing = [([list(f) for f in m] if isinstance(m, list)
                    else list(m)) for m in members
                   if not (member_covered(m[0], m[1:])
                           if isinstance(m, list) else member_covered(m))]
        if missing and catch_all:
            missing = []  # an "otherwise" case takes everything left
        if not missing:
            return {"covered": True, "missing": [], "family": name}
        if best is None or len(missing) < len(best[1]):
            best = (name, missing)
    # product splits: every case a conjunction of one member from each of
    # two families (I.7: a vs CD crossed with d vs AC)
    # A missing product member is reported as a LIST of facts (the
    # conjunction); the page phrases it with "and" and opens it as one
    # case with both hypotheses.
    if not catch_all and cases and all(len(c) == 2 for c in cases):
        best_prod = None
        for i1, (n1, mem1) in enumerate(matching):
            for n2, mem2 in matching[i1 + 1:]:
                missing = [[list(m1), list(m2)]
                           for m1 in mem1 for m2 in mem2
                           if not member_covered(m1, (m2,))]
                name = "%s, crossed with %s" % (n1, n2)
                if not missing:
                    return {"covered": True, "missing": [], "family": name}
                if best_prod is None or len(missing) < len(best_prod[1]):
                    best_prod = (name, missing)
        if best_prod is not None:
            return {"covered": False, "missing": best_prod[1],
                    "family": best_prod[0]}
    return {"covered": False, "missing": best[1], "family": best[0]}


def _coverage_check_moves(n, s, moves, refuted, i):
    """Replay moves on (a copy of) s, checking every closed case block's
    coverage at the state where it stands; nested blocks recurse. The
    engine's own refusal paths stay authoritative: a move that fails to
    apply stops the walk (run_steps will word the error)."""
    for mv in moves:
        if mv.get("rule") in CASE_RULES:
            err = _coverage_check(n, s, mv, refuted, i)
            if err:
                return err
            # approximate the block's effect for later siblings: assume
            # the conclusions, as the engine does on success
            for c in mv.get("conclusion") or []:
                if tuple(c)[0] != "false":
                    try:
                        eng._assume_metric_or_fact(s, tuple(c),
                                                   "coverage walk")
                    except Exception:
                        pass
            eng.close(s)
        else:
            try:
                eng.apply_step(s, i, mv["rule"], tuple(mv.get("args") or ()),
                               dict(mv.get("bindings") or {}))
                eng.close(s)
            except Exception:
                return None
    return None


# Coverage verdicts by (state, block): the page replays every move on
# each request, so without this a student past I.22's split waited ~15 s
# per move while the same split was re-refereed. The key is the full
# state (facts and arithmetic rows), so a hit is the same question.
_COVERAGE_CACHE = {}


def _coverage_check(n, s, mv, refuted, i):
    """Check one closed case block at state s. Returns None, or a
    structured {"rule", "family", "missing"} for a split that does not
    cover every possibility."""
    rule = mv["rule"]
    branches = mv.get("branches") or []
    concl = [tuple(c) for c in mv.get("conclusion") or []]
    cases = [[tuple(f) for f in b.get("hyps") or []] for b in branches]
    key = (n, frozenset(s.facts), tuple(s.la.rows), rule,
           repr(cases), repr(concl), repr(refuted))
    cov = _COVERAGE_CACHE.get(key)
    if cov is None:
        cov = coverage(s, rule, cases, concl, refuted)
        if len(_COVERAGE_CACHE) > 2000:
            _COVERAGE_CACHE.clear()
        _COVERAGE_CACHE[key] = cov
    if not cov["covered"]:
        return {"rule": rule, "family": cov["family"],
                "missing": cov["missing"]}
    for hyps, b in zip(cases, branches):
        sub = s.copy()
        try:
            for f in hyps:
                eng._assume_metric_or_fact(sub, f, rule + " assumption")
            eng.close(sub)
        except Exception:
            continue  # the engine's own path will report this
        sub_refuted = refuted
        if rule == "by_cases" and not hyps:
            # a catch-all branch: every sibling case is refuted in it
            sub_refuted = tuple(refuted) + tuple(h for h in cases if h)
        err = _coverage_check_moves(n, sub, b.get("moves") or [],
                                    sub_refuted, i + 1)
        if err:
            return err
    return None


def _close_branch(n, s, rule, hyps, steps, concl, i):
    """Check one CLOSED branch on a copy of s, as eng._by_contra /
    eng._by_cases do: assume the hypotheses, run the steps, then require
    a contradiction (by_contra) or every conclusion with no contradiction
    (by_cases). Returns None on success, an error string otherwise."""
    sub = s.copy()
    try:
        for f in hyps:
            eng._assume_metric_or_fact(sub, f, rule + " assumption")
        eng.close(sub)
        eng.run_steps(sub, steps, i + 1)
    except eng.MissingPremise as e:
        return ("the case %s is stuck: %s"
                % ([list(f) for f in hyps], e))
    except (eng.ByContraFailure, eng.ByCasesFailure) as e:
        return "in the case %s: %s" % ([list(f) for f in hyps], e)
    contra, _w = eng.contradictory(sub)
    if rule == "by_contra":
        if not contra:
            return ("the case %s does not reach a contradiction"
                    % [list(f) for f in hyps])
    else:
        if contra:
            return ("the case %s is contradictory — it would entail "
                    "anything" % [list(f) for f in hyps])
        for c in concl:
            if not eng._fact_holds(sub, c):
                return ("the case %s does not reach the conclusion %s"
                        % ([list(f) for f in hyps], list(c)))
    return None


def _open_block(n, s, mv, objects, xy, pal, i, refuted=()):
    """Report on an OPEN case move without touching the outer state.
    Every branch but the last must already close (checked on copies);
    for the last branch the hypotheses are assumed on a copy and its
    moves applied, and the branch is reported (contradiction? conclusion
    reached? its objects). Returns (ok, error, branch_reports, view,
    missing_case) where view is the (objects, xy) of the deepest open
    branch to show, or None; missing_case is the structured coverage
    refusal when a nested closed block's split leaves a possibility
    out."""
    rule = mv["rule"]
    branches = mv.get("branches") or []
    concl = [tuple(c) for c in mv.get("conclusion") or []]
    breps = []
    if not branches:
        return (False, "an open case move needs at least one case",
                breps, None, None)
    for b in branches[:-1]:
        hyps = [tuple(f) for f in b.get("hyps") or []]
        # coverage of closed blocks nested inside this closed case
        cov_s = s.copy()
        for f in hyps:
            eng._assume_metric_or_fact(cov_s, f, rule + " assumption")
        eng.close(cov_s)
        cov_ref = tuple(refuted)
        if rule == "by_cases" and not hyps:
            cov_ref += tuple([tuple(f) for f in bb.get("hyps") or []]
                             for bb in branches if bb.get("hyps"))
        cerr = _coverage_check_moves(n, cov_s, b.get("moves") or [],
                                     cov_ref, i + 1)
        if cerr is not None:
            return False, "missing case", breps, None, cerr
        steps = [_move_to_step(m) for m in b.get("moves") or []]
        err = _close_branch(n, s, rule, hyps, steps, concl, i)
        if err:
            return False, "an earlier case must close first: " + err, \
                breps, None, None
        breps.append({"hyps": [list(f) for f in hyps], "closed": True})
    last = branches[-1]
    hyps = [tuple(f) for f in last.get("hyps") or []]
    sub = s.copy()
    bobjects = [dict(o) for o in objects]
    bxy = dict(xy)
    try:
        for f in hyps:
            eng._assume_metric_or_fact(sub, f, rule + " assumption")
        eng.close(sub)
    except Exception as e:
        return False, ("bad case hypothesis %s: %s"
                       % ([list(f) for f in hyps], e)), breps, None, None
    last_refuted = tuple(refuted)
    if rule == "by_cases" and not hyps:
        # the catch-all case: its siblings are refuted inside it
        last_refuted += tuple([tuple(f) for f in b.get("hyps") or []]
                              for b in branches[:-1] if b.get("hyps"))
    bentries, deeper = _apply_moves(n, sub, last.get("moves") or [],
                                    bobjects, bxy, pal, i + 1,
                                    last_refuted)
    contra, _w = eng.contradictory(sub)
    goal = bool(concl) and all(eng._fact_holds(sub, c) for c in concl)
    breps.append({"hyps": [list(f) for f in hyps], "closed": False,
                  "moves": bentries,
                  "branch_contradictory": contra,
                  "branch_goal_reached": goal})
    miss = None
    for e in bentries:
        if e.get("missing_case"):
            miss = e["missing_case"]
    return True, None, breps, deeper or (bobjects, bxy), miss


def _apply_moves(n, s, moves, objects, xy, pal, i0=1, refuted=()):
    """Apply moves to s (in place), placing new objects, stopping at the
    first failure. Returns (entries, view); view is the (objects, xy) of
    the deepest open case branch when the last move is an open block,
    else None. Entries mirror the submitted move list (case moves carry
    per-branch reports)."""
    entries = []
    view = None
    for i, mv in enumerate(moves, i0):
        rule = mv["rule"]
        entry = {"n": i, "rule": rule, "ok": False, "added": [],
                 "error": None}
        m = _PROP_RE.match(rule)
        if rule in CASE_RULES:
            empty = _empty_block(mv)
            if empty is not None and not (empty is mv and mv.get("open")):
                entry["error"] = ("a %s needs at least one case"
                                  % empty["rule"])
            elif mv.get("open"):
                ok, err, breps, view, miss = _open_block(
                    n, s, mv, objects, xy, pal, i, refuted)
                entry.update(ok=ok, error=err, open=True, branches=breps)
                if miss:
                    entry["missing_case"] = miss
            else:
                cerr = _coverage_check(n, s, mv, refuted, i)
                if cerr is not None:
                    entry["error"] = "missing case"
                    entry["missing_case"] = cerr
                    entries.append(entry)
                    break
                try:
                    rep = eng.run_steps(s, [_block_to_engine(mv)])[0]
                    entry["ok"] = True
                    entry["added"] = [list(c)
                                      for c in rep.get("conclusions", [])]
                    entry["conclusions"] = entry["added"]
                except eng.MissingPremise as e:
                    entry["error"] = (list(e.premise)
                                      if isinstance(e.premise, tuple)
                                      else str(e.premise))
                except (eng.ByContraFailure, eng.ByCasesFailure) as e:
                    entry["error"] = str(e)
                except TypeError as e:
                    entry["error"] = ("bad case move for %s: %s" % (rule, e))
        elif m and rule not in pal:
            entry["error"] = "I.%d is not proved yet" % int(m.group(1))
        elif rule not in pal:
            entry["error"] = ("unknown rule %r (not in I.%d's palette)"
                              % (rule, n))
        elif any(not str(a).strip() for a in (mv.get("args") or ())):
            entry["error"] = "blank input"
        elif [a for a in (mv.get("args") or ())
              if a not in {o["name"] for o in objects}]:
            entry["error"] = "no object called %s" % ", ".join(
                a for a in mv["args"]
                if a not in {o["name"] for o in objects})
        else:
            args = tuple(mv.get("args") or ())
            bindings = dict(mv.get("bindings") or {})
            try:
                rep = eng.apply_step(s, i, rule, args, bindings)
                eng.close(s)
                added = rep["added"]
                if rule == "name_point":
                    _place_name_point(args, added, xy, objects)
                else:
                    tr.place_step(n, rule, args, bindings, added, xy,
                                  objects)
                for o in objects:
                    if o["def_step"] is None:
                        o["def_step"] = i
                entry["ok"] = True
                entry["added"] = [list(f) for f in added]
            except eng.MissingPremise as e:
                entry["error"] = (list(e.premise)
                                  if isinstance(e.premise, tuple)
                                  else str(e.premise))
            except TypeError as e:
                entry["error"] = ("bad arity for %s: %s" % (rule, e))
        entries.append(entry)
        if not entry["ok"]:
            break
    for e in entries:
        if e.get("error") is not None:
            e["error_plain"] = student_error(e["error"], e["rule"])
    return entries, view


def play(n, moves):
    """Stateless: rebuild N's start state, apply each move
    {rule, args, bindings} with eng.apply_step + close, stop at the first
    failure. Case moves are {rule: "by_contra"|"by_cases", branches:
    [{hyps, moves}], conclusion, open}; closed blocks go through
    eng.run_steps (the same tuples the engine uses, refusals as the
    engine words them), open blocks are reported on without touching the
    outer state (see _open_block)."""
    pal = set(palette(n))
    s = _start_state(n)
    objects = tr.objects_from_hyps(n)
    xy = {k: v for k, v in tr.FREE[n].items()}
    tr.derived_free_points(n, xy)
    out_moves, view = _apply_moves(n, s, moves, objects, xy, pal)
    goal_reached = _goal_reached(n, s, objects, xy)
    contradict = eng.contradictory(s)[0]
    if view is not None:
        objects, xy = view
    out_objects = []
    for o in objects:
        d = dict(o)
        if o["kind"] == "point" and o["name"] in xy:
            d["xy"] = list(xy[o["name"]]) if xy[o["name"]] else None
        out_objects.append(d)
    return {"n": n, "moves": out_moves, "goal_reached": goal_reached,
            "objects": out_objects, "contradictory": contradict}


# Session log (--log DIR): one JSON line per /api/play request, so the
# student's attempts (refused moves included) can be read back when
# designing the tutor's interventions. The page replays the whole move
# list on every request, so each line is the attempt so far.
_LOG_PATH = None


def _log_play(data, res):
    if _LOG_PATH is None:
        return
    last = (res.get("moves") or [{}])[-1]
    rec = {"t": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
           "n": data.get("n"), "moves": data.get("moves") or [],
           "ok": last.get("ok"), "error": last.get("error"),
           "error_plain": last.get("error_plain"),
           "missing_case": last.get("missing_case"),
           "goal_reached": res.get("goal_reached")}
    try:
        with open(_LOG_PATH, "a") as fh:
            fh.write(json.dumps(rec) + "\n")
    except OSError:
        pass


class _Handler(SimpleHTTPRequestHandler):
    def __init__(self, *a, **kw):
        super().__init__(*a, directory=VIEWER_DIR, **kw)

    def end_headers(self):
        # the page changes while a student is playing: never serve a
        # cached copy (Joe, 2026-11-09)
        self.send_header("Cache-Control", "no-cache")
        super().end_headers()

    def _send_json(self, obj, code=200):
        body = json.dumps(obj).encode()
        self.send_response(code)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def do_GET(self):
        if self.path == "/api/catalogue":
            self._send_json(catalogue())
        else:
            super().do_GET()

    def do_POST(self):
        if self.path == "/api/play":
            try:
                length = int(self.headers.get("Content-Length", 0))
                data = json.loads(self.rfile.read(length) or b"{}")
                res = play(int(data["n"]), data.get("moves") or [])
                _log_play(data, res)
                self._send_json(res)
            except (KeyError, ValueError, TypeError) as e:
                self._send_json({"error": str(e)}, code=400)
        else:
            self.send_error(404)

    def log_message(self, *a):
        pass


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=8123)
    ap.add_argument("--log", default=None,
                    help="directory for the session log (JSON lines)")
    args = ap.parse_args()
    global _LOG_PATH
    if args.log:
        os.makedirs(args.log, exist_ok=True)
        _LOG_PATH = os.path.join(
            args.log, time.strftime("play-%Y%m%dT%H%M%SZ.jsonl",
                                    time.gmtime()))
    srv = ThreadingHTTPServer(("127.0.0.1", args.port), _Handler)
    print("serving %s on http://127.0.0.1:%d" % (VIEWER_DIR, args.port))
    srv.serve_forever()


if __name__ == "__main__":
    main()
