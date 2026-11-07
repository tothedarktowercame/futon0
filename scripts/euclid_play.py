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
    1: eng._prop1()[2],                     # engine's own verify list
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


def rule_info(name):
    """{args, cite, binds} for a rule. binds are the base names the rule's
    make introduces, read off the make lambda's returned pairs on a
    throwaway state (euclid_engine.py is NOT modified). If the make
    lambda cannot be run that way, binds is [] — the page then shows one
    free "name" field instead of per-bind inputs."""
    spec = eng.CONSTRUCTIONS[name]
    binds = []
    try:
        made = spec["make"](eng.State(),
                            *["#arg%d" % k for k in range(len(spec["args"]))])
        binds = [b for b, _facts in made if b is not None]
    except Exception:
        binds = []
    return {"args": list(spec["args"]), "cite": spec["cite"],
            "binds": binds}


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


def _block_to_engine(mv):
    """{rule, branches: [{hyps, moves}], conclusion} as the engine's
    (rule, [(assumption-facts, steps), ...], conclusions) tuple."""
    branches = []
    for b in mv.get("branches") or []:
        hyps = [tuple(f) for f in b.get("hyps") or []]
        steps = [_move_to_step(m) for m in b.get("moves") or []]
        branches.append((hyps, steps))
    concl = [tuple(c) for c in mv.get("conclusion") or []]
    return (mv["rule"], branches, concl)


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
    if k == "between" and f[3] < f[1]:
        return ("between", f[3], f[2], f[1])
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
    if f[0] in ("opposing_sides", "same_side", "not_same_side"):
        # symmetric in the two points (Relations.lean:13-14 for
        # opposing); the engine's closure symmetrizes same_side only
        return eng._fact_holds(s, f) or \
            eng._fact_holds(s, (f[0], f[2], f[1], f[3]))
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
            add("where %s, %s and %s lie on their line" % (p, q, r),
                [("between", p, q, r), ("between", q, p, r),
                 ("between", p, r, q)], ("between", p, q, r))
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
            if eng.contradictory(tp)[0]:
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
        if eng.contradictory(_probe(s, [m], enrich=False))[0]:
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
    matching = [(name, members) for name, members in fams
                if any(_norm(m) in norm_pool for m in members)]
    if not matching:
        return {"covered": False, "missing": [], "family": None}
    best = None
    for name, members in matching:
        missing = [list(m) for m in members if not member_covered(m)]
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


def _coverage_check(n, s, mv, refuted, i):
    """Check one closed case block at state s. Returns None, or a
    structured {"rule", "family", "missing"} for a split that does not
    cover every possibility."""
    rule = mv["rule"]
    branches = mv.get("branches") or []
    concl = [tuple(c) for c in mv.get("conclusion") or []]
    cases = [[tuple(f) for f in b.get("hyps") or []] for b in branches]
    cov = coverage(s, rule, cases, concl, refuted)
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
        else:
            args = tuple(mv.get("args") or ())
            bindings = dict(mv.get("bindings") or {})
            try:
                rep = eng.apply_step(s, i, rule, args, bindings)
                eng.close(s)
                added = rep["added"]
                tr.place_step(n, rule, args, bindings, added, xy, objects)
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
    goal_reached = all(eng._fact_holds(s, g) for g in GOALS[n])
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


class _Handler(SimpleHTTPRequestHandler):
    def __init__(self, *a, **kw):
        super().__init__(*a, directory=VIEWER_DIR, **kw)

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
                self._send_json(play(int(data["n"]),
                                     data.get("moves") or []))
            except (KeyError, ValueError, TypeError) as e:
                self._send_json({"error": str(e)}, code=400)
        else:
            self.send_error(404)

    def log_message(self, *a):
        pass


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=8123)
    args = ap.parse_args()
    srv = ThreadingHTTPServer(("127.0.0.1", args.port), _Handler)
    print("serving %s on http://127.0.0.1:%d" % (VIEWER_DIR, args.port))
    srv.serve_forever()


if __name__ == "__main__":
    main()
