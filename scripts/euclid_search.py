#!/usr/bin/env python3
"""Uninformed search over the System E engine (BFS with a depth/node budget).

Given a proposition's hypotheses and a goal test, find a step sequence by
breadth-first search over legal moves — construction rules and citable
propositions — with premises checked by the engine. Forward rules stay inside
the engine's closure (never search moves). No prior, no Book proof given.

Dedup key (documented, weaker than full graph canonical form): fresh objects
(names containing '$', i.e. introduced by constructions after the hypotheses)
are renamed #0,#1,... in order of first appearance in the SORTED fact list;
the key is the renamed fact set plus the renamed LinearArithmetic rows. This
is not invariant under all isomorphisms (first-appearance order follows the
sorted-by-original-names enumeration), so some isomorphic states may survive
unmerged — it never merges non-isomorphic states differing in a fact (tested).

Entailment cache: LinearArithmetic.entails is a pure function of the row
snapshot + query, and rows are append-only, so results are memoized on
(tuple(self.rows), normalized query). Cache hits across states that share a
constraint-store prefix are the main saving (closure re-derives entailment
queries repeatedly, e.g. classify_point_vs_circle, positive_length_neq).
"""

from fractions import Fraction
import importlib.util
import itertools
import time
from pathlib import Path

spec = importlib.util.spec_from_file_location(
    "euclid_engine", Path(__file__).resolve().parent / "euclid_engine.py")
eng = importlib.util.module_from_spec(spec)
spec.loader.exec_module(eng)

# --- entailment instrumentation + cache ------------------------------------

_entails_time = 0.0
_entails_calls = 0
_entails_hits = 0
_entails_cache = {}
_orig_entails = eng.LinearArithmetic.entails


def _patched_entails(self, coeffs, const, op):
    global _entails_time, _entails_calls, _entails_hits
    _entails_calls += 1
    key = (tuple(self.rows), tuple(sorted(coeffs.items())),
           Fraction(const), op)
    if key in _entails_cache:
        _entails_hits += 1
        return _entails_cache[key]
    t0 = time.perf_counter()
    r = _orig_entails(self, coeffs, const, op)
    _entails_time += time.perf_counter() - t0
    _entails_cache[key] = r
    return r


eng.LinearArithmetic.entails = _patched_entails


def reset_instruments():
    global _entails_time, _entails_calls, _entails_hits, _entails_cache
    _entails_time, _entails_calls, _entails_hits, _entails_cache = 0.0, 0, 0, {}
    _entails_cache = {}


# --- state cloning and dedup key --------------------------------------------

def clone_state(s):
    s2 = eng.State()
    s2.facts = dict(s.facts)          # provenance values are never mutated
    s2.la.rows = list(s.la.rows)      # rows are immutable tuples
    s2.fresh = s.fresh
    # object-name discipline (P-euclid-binding-clash): a clone that drops
    # declared/reserved silently loses the fresh-binding clash check
    s2.declared = set(s.declared)
    s2.reserved = set(s.reserved)
    return s2


def _is_fresh(x):
    return isinstance(x, str) and "$" in x


def state_key(s):
    mapping = {}

    def rn(x):
        if _is_fresh(x):
            if x not in mapping:
                mapping[x] = "#%d" % len(mapping)
            return mapping[x]
        return x

    facts = tuple(sorted(tuple(rn(y) for y in f) for f in s.facts))
    rows = []
    for items, c, op in s.la.rows:
        items2 = tuple(sorted((tuple(rn(a) for a in at), co)
                              for at, co in items))
        rows.append((items2, c, op))
    return (facts, tuple(sorted(rows)))


# --- objects and candidate moves --------------------------------------------

def object_kinds(s):
    pts, lns, crs = set(), set(), set()
    for f in s.facts:
        k = f[0]
        if k in ("on_line", "not_on_line"):
            pts.add(f[1]); lns.add(f[2])
        elif k in ("on_circle", "inside", "outside", "not_outside", "centre"):
            pts.add(f[1]); crs.add(f[2])
        elif k == "between":
            pts.update(f[1:4])
        elif k == "intersects_cc":
            crs.update(f[1:3])
        elif k == "neq":
            pts.update(f[1:3])
        elif k in ("dpl", "opposing_sides"):
            pts.update(f[1:3]); lns.add(f[3])
        elif k in ("pos_len", "len_sum", "eq_len"):
            pts.update(f[1:])
        elif k == "radii":
            crs.add(f[1]); pts.update(f[2:4])
    return pts, lns, crs


def _c_line(s):
    pts, _, _ = object_kinds(s)
    # (a,b) and (b,a) build the same line (same facts up to the fresh name),
    # so unordered pairs suffice.
    for a, b in itertools.combinations(sorted(pts), 2):
        yield (a, b)


def _c_circle(s):
    pts, _, _ = object_kinds(s)
    for a in sorted(pts):
        for b in sorted(pts):
            if a != b:
                yield (a, b)


def _c_icc(s):
    _, _, crs = object_kinds(s)
    for a, b in itertools.combinations(sorted(crs), 2):
        yield (a, b)


def _c_ext(s):
    for f in s.facts:
        if f[0] == "dpl":
            b, c, L = f[1], f[2], f[3]
            for g in s.facts:
                if g[0] == "inside" and g[1] == b:
                    yield (g[2], L, b, c)


def _c_btw(s):
    _, lns, _ = object_kinds(s)
    ins = [(f[1], f[2]) for f in s.facts if f[0] == "inside"]
    outs = {(f[1], f[2]) for f in s.facts if f[0] == "outside"}
    for b, alpha in ins:
        for c, alpha2 in outs:
            if alpha2 != alpha:
                continue
            for L in sorted(lns):
                if s.has(("on_line", b, L)) and s.has(("on_line", c, L)):
                    yield (alpha, L, b, c)


def _c_opp(s):
    centres = [(f[1], f[2]) for f in s.facts if f[0] == "centre"]
    for f in s.facts:
        if f[0] != "not_on_line":
            continue
        b, L = f[1], f[2]
        onL = [(c, al) for c, al in centres if s.has(("on_line", c, L))]
        for (c, al), (d, be) in itertools.combinations(sorted(onL), 2):
            if s.has(("intersects_cc", al, be)) or \
                    s.has(("intersects_cc", be, al)):
                yield (al, be, b, c, d, L)


def _c_prop1(s):
    # premise (dpl a b AB); (a,b) and (b,a) give the same witness facts
    # (both dpl directions are derived), so take one order per pair.
    for f in s.facts:
        if f[0] == "dpl" and f[1] < f[2]:
            yield (f[1], f[2], f[3])  # (a, b, AB)


def _c_prop2(s):
    pts, _, _ = object_kinds(s)
    for f in s.facts:
        if f[0] != "dpl":
            continue
        b, c, BC = f[1], f[2], f[3]
        for a in sorted(pts):
            yield (a, b, c, BC)


def _dpls(s):
    return [(f[1], f[2], f[3]) for f in s.facts if f[0] == "dpl"]


def _triangles(s):
    """formTriangle-shaped (a, b, c, AB, BC, AC) present in the state:
    dpl a b AB, b and c on BC, a and c on AC."""
    for a, b, AB in _dpls(s):
        for BC in sorted({f[2] for f in s.facts
                          if f[0] == "on_line" and f[1] == b}):
            for c in sorted({f[1] for f in s.facts
                             if f[0] == "on_line" and f[2] == BC
                             and f[1] not in (a, b)}):
                for AC in sorted({f[2] for f in s.facts
                                  if f[0] == "on_line" and f[1] == a}):
                    if s.has(("on_line", c, AC)):
                        yield (a, b, c, AB, BC, AC)


def _c_prop3(s):
    # (a, b, c0, c1, AB, C): cut |c0-c1| (shorter) off segment a-b
    for a, b, AB in _dpls(s):
        for c0, c1, C in _dpls(s):
            if (a, b) != (c0, c1):
                yield (a, b, c0, c1, AB, C)


def _c_extend_pt(s):
    # extend the segment b-c on its own line: (L, b, c)
    for b, c, L in _dpls(s):
        yield (L, b, c)


def _c_pbps(s):
    # point_between_points_shorter_than: (L, b, c, u, v) — a point between
    # b and c at distance < |u-v| from b (entailment premise)
    pts, _, _ = object_kinds(s)
    for b, c, L in _dpls(s):
        for u in sorted(pts):
            for v in sorted(pts):
                if u != v:
                    yield (L, b, c, u, v)


def _c_prop4(s):
    # SAS citation between two in-state triangles (12 args); entailment
    # premises (the equal sides/angle) are checked by apply_step
    for (a, b, c, AB, BC, AC), (d, e, f, DE, EF, DF) in \
            itertools.permutations(sorted(set(_triangles(s))), 2):
        yield (a, b, c, d, e, f, AB, BC, AC, DE, EF, DF)


def _c_super(s):
    # superposition: triangle (a,b,c) onto ray d-g of line L, off-line h
    for a, b, c, AB, BC, AC in _triangles(s):
        for d, g, L in _dpls(s):
            for nl in s.facts:
                if nl[0] == "not_on_line" and nl[2] == L:
                    yield (a, b, c, d, g, nl[1], AB, BC, AC, L)


def _c_assert_ray(s):
    # assert_point_eq_on_ray: x, y, z all on L, x != y not between z
    for L in sorted({f[2] for f in s.facts if f[0] == "on_line"}):
        onL = sorted({f[1] for f in s.facts
                      if f[0] == "on_line" and f[2] == L})
        for x, y, z in itertools.permutations(onL, 3):
            yield (x, y, z, L)


def _c_assert_ang(s):
    # assert_point_eq_angle: x, y same side of L; arm w-z on L, z != w,x,y
    for f in s.facts:
        if f[0] != "same_side":
            continue
        x, y, L = f[1], f[2], f[3]
        onL = sorted({g[1] for g in s.facts
                      if g[0] == "on_line" and g[2] == L})
        for z, w in itertools.permutations(onL, 2):
            yield (x, y, w, z, L)


def _c_sum_ang(s):
    # sum_angles_onlyif: (a, b, c, d, L, M)
    for a, L in sorted({(f[1], f[2]) for f in s.facts
                        if f[0] == "on_line"}):
        for M in sorted({f[2] for f in s.facts
                         if f[0] == "on_line" and f[1] == a}):
            if L == M:
                continue
            for b in sorted({f[1] for f in s.facts
                             if f[0] == "on_line" and f[2] == L
                             and f[1] != a}):
                for c in sorted({f[1] for f in s.facts
                                 if f[0] == "on_line" and f[2] == M
                                 and f[1] != a}):
                    for d in sorted({f[1] for f in s.facts
                                     if f[0] == "not_on_line" and f[1] != a
                                     and f[2] in (L, M)}):
                        yield (a, b, c, d, L, M)


CANDIDATES = {
    "line_from_points": _c_line,
    "circle_from_points": _c_circle,
    "intersection_circles": _c_icc,
    "intersection_circle_line_extending_points": _c_ext,
    "intersection_circle_line_between_points": _c_btw,
    "intersection_opposite_side": _c_opp,
    "proposition_1": _c_prop1,
    "proposition_2'": _c_prop2,
    "proposition_3": _c_prop3,
    "extend_point": _c_extend_pt,
    "point_between_points_shorter_than": _c_pbps,
    "proposition_4": _c_prop4,
    "superposition": _c_super,
    "assert_point_eq_on_ray": _c_assert_ray,
    "assert_point_eq_angle": _c_assert_ang,
    "sum_angles_onlyif": _c_sum_ang,
}

AXIOM_MOVES = ["line_from_points", "circle_from_points",
               "intersection_circles",
               "intersection_circle_line_extending_points",
               "intersection_circle_line_between_points",
               "intersection_opposite_side"]


def legal_moves(s, rules):
    """Moves = (rule, args) with args from state objects; legal iff every
    premise fact of the rule spec is present in the closed state."""
    moves = []
    for rname in rules:
        spec = eng.CONSTRUCTIONS[rname]
        for args in CANDIDATES[rname](s):
            if all(s.has(pf) for pf in spec["premises"](s, *args)):
                moves.append((rname, args))
    return moves


# --- goal tests ---------------------------------------------------------------

def goal_prop1(s, a="a", b="b"):
    """I.1: exists p on two circles with centres a and b respectively, and
    |p-a| = |a-b| and |p-b| = |a-b| entailed."""
    centres = {f[2]: f[1] for f in s.facts if f[0] == "centre"}
    _, _, crs = object_kinds(s)
    for alpha in crs:
        if centres.get(alpha) != a:
            continue
        for beta in crs:
            if centres.get(beta) != b or beta == alpha:
                continue
            for f in s.facts:
                if f[0] == "on_circle" and f[2] == alpha and \
                        s.has(("on_circle", f[1], beta)):
                    p = f[1]
                    if eng.la_entails_eq(s.la, p, a, a, b) and \
                            eng.la_entails_eq(s.la, p, b, a, b):
                        return True
    return False


def goal_prop2(s, a="a", b="b", c="c"):
    """I.2: exists p with |a-p| = |b-c| entailed (p fresh, not b or c)."""
    pts, _, _ = object_kinds(s)
    for p in pts:
        if p in (b, c):
            continue
        if eng.la_entails_eq(s.la, a, p, b, c):
            return True
    return False


def goal_prop3(s, a="a", b="b", c0="c0", c1="c1"):
    """I.3: exists p with between a p b and |a-p| = |c0-c1| entailed."""
    for f in s.facts:
        if f[0] == "between" and f[1] == a and f[3] == b:
            if eng.la_entails_eq(s.la, a, f[2], c0, c1):
                return True
    return False


def goal_prop4(s, a="a", b="b", c="c", d="d", e="e", f="f"):
    """I.4 (Book/Prop04.lean): SAS — |b-c| = |e-f| entailed, plus the two
    base angles equal: ang a:b:c = ang d:e:f and ang a:c:b = ang d:f:e."""
    return (eng.la_entails_eq(s.la, b, c, e, f)
            and eng.la_entails_eq_ang(s.la, a, b, c, d, e, f)
            and eng.la_entails_eq_ang(s.la, a, c, b, d, f, e))


def goal_prop5(s, a="a", b="b", c="c", d="d", e="e"):
    """I.5 (Book/Prop05.lean): isosceles base angles equal and the
    exterior-angle equality: ang a:b:c = ang a:c:b, ang c:b:d = ang b:c:e."""
    return (eng.la_entails_eq_ang(s.la, a, b, c, a, c, b)
            and eng.la_entails_eq_ang(s.la, c, b, d, b, c, e))


# --- the search ----------------------------------------------------------------

def search(hyps, rules, goal, max_depth, node_budget,
           extra_la=None, time_budget=None):
    """BFS over move sequences. Returns (found, path, stats)."""
    reset_instruments()
    t0 = time.perf_counter()
    root = eng.State()
    for f, w in hyps:
        eng._assume_metric_or_fact(root, f, w)  # metric hyps add their row
    for coeffs, const, op in (extra_la or []):
        root.la.add(coeffs, const, op)
    eng.close(root)
    if goal(root):
        return True, [], _stats(0, [], 0.0)
    seen = {state_key(root)}
    queue = [(root, [])]
    nodes = 0
    branch = []
    while queue:
        if nodes >= node_budget:
            break
        if time_budget and time.perf_counter() - t0 > time_budget:
            break
        s, path = queue.pop(0)
        nodes += 1
        if len(path) >= max_depth:
            continue
        moves = legal_moves(s, rules)
        branch.append(len(moves))
        for rname, args in moves:
            s2 = clone_state(s)
            prior = {x for f in s2.facts for x in f[1:] if _is_fresh(x)}
            try:
                eng.apply_step(s2, len(path) + 1, rname, args, {})
            except eng.MissingPremise:
                continue  # premises must have been withdrawn: impossible here
            new = {x for f in s2.facts for x in f[1:] if _is_fresh(x)} - prior
            k = state_key(s2)
            if k in seen:
                continue
            seen.add(k)
            step = (rname, args, _bindings(rname, new))
            queue.append((s2, path + [step]))
            if goal(s2) and store_consistent(s2.la):
                return True, path + [step], \
                    _stats(nodes, branch + [len(moves)],
                           time.perf_counter() - t0)
    return False, None, _stats(nodes, branch, time.perf_counter() - t0)


def _bindings(rname, new_names):
    spec = eng.CONSTRUCTIONS[rname]
    bases = [base for base, _ in spec["make"](
        eng.State(), *([None] * len(spec["args"])))]
    return {b: n for b, n in zip(bases, sorted(new_names))}


def _stats(nodes, branch, wall):
    global _entails_time, _entails_calls, _entails_hits
    return {
        "nodes": nodes,
        "branch_mean": (sum(branch) / len(branch)) if branch else 0.0,
        "branch_max": max(branch) if branch else 0,
        "wall_s": wall,
        "entails_s": _entails_time,
        "entails_calls": _entails_calls,
        "entails_cache_hits": _entails_hits,
    }


def store_consistent(la):
    """True iff the constraint store is satisfiable (direct FM check on the
    rows; an undecided store (FMTooLarge) counts as NOT consistent, so a
    goal is never certified on a store the arithmetic could not check). An unsat store entails
    everything, so goal tests must refuse to certify such states (the engine
    itself does not check this; the search layer does)."""
    try:
        return eng._fm_sat(list(la.rows))
    except eng.FMTooLarge:
        return False  # undecided: refuse to certify (review, claude-9)


def verify_path(hyps, path, goal, extra_la=None):
    """Replay the found steps through the engine step path and check the
    goal on the final state. (eng.replay cannot take metric hypotheses, so
    this mirrors it: assume, close, apply_step per step, assert.)"""
    s = eng.State()
    for f, w in hyps:
        eng._assume_metric_or_fact(s, f, w)  # metric hyps add their row
    for coeffs, const, op in (extra_la or []):
        s.la.add(coeffs, const, op)
    eng.close(s)
    for i, (rule, args, bindings) in enumerate(path, start=1):
        eng.apply_step(s, i, rule, args, bindings)
    assert store_consistent(s.la), "replayed state has an unsatisfiable store"
    assert goal(s), "replayed path does not satisfy the goal"
    return s, len(path)


# --- propositions as data -------------------------------------------------------

def prop_searches():
    gt = ">" 
    prop1_hyps = [(("on_line", "a", "AB"), "h"),
                  (("on_line", "b", "AB"), "h"),
                  (("neq", "a", "b"), "h")]
    prop2_hyps = [(("on_line", "b", "BC"), "h"),
                  (("on_line", "c", "BC"), "h"),
                  (("neq", "b", "c"), "h"),
                  (("neq", "a", "b"), "h")]
    prop3_hyps = prop1_hyps + [
        (("on_line", "c0", "C"), "h"),
        (("on_line", "c1", "C"), "h"),
        (("neq", "c0", "c1"), "h")]
    prop3_extra = [({eng.len_atom("a", "b"): 1,
                     eng.len_atom("c0", "c1"): -1}, 0, ">")]
    return {
        "I.1": dict(hyps=prop1_hyps, rules=AXIOM_MOVES, goal=goal_prop1,
                    max_depth=4, node_budget=20000),
        "I.2": dict(hyps=prop2_hyps, rules=AXIOM_MOVES + ["proposition_1"],
                    goal=goal_prop2, max_depth=8, node_budget=30000,
                    time_budget=600.0),
        "I.3": dict(hyps=prop3_hyps,
                    rules=AXIOM_MOVES + ["proposition_1", "proposition_2'"],
                    goal=goal_prop3, max_depth=4, node_budget=20000,
                    extra_la=prop3_extra),
        # I.4: rules may cite only propositions < 4 (proposition_1-3) plus
        # the superposition inference and its [SMT-bridge] assert rules,
        # which is what the Book's proof uses.
        "I.4": dict(hyps=list(eng.PROP4_HYPS_DATA),
                    rules=AXIOM_MOVES + ["proposition_1", "proposition_2'",
                                         "proposition_3", "superposition",
                                         "assert_point_eq_on_ray",
                                         "assert_point_eq_angle"],
                    goal=goal_prop4,
                    max_depth=4, node_budget=5000, time_budget=60.0),
        # I.5: rules may cite proposition_4 (< 5) and the point/proposition_3
        # constructions the Book's proof uses.
        "I.5": dict(hyps=list(eng.PROP5_HYPS_DATA),
                    rules=AXIOM_MOVES + ["proposition_1", "proposition_2'",
                                         "proposition_3", "extend_point",
                                         "point_between_points_shorter_than",
                                         "proposition_4",
                                         "sum_angles_onlyif"],
                    goal=goal_prop5,
                    max_depth=8, node_budget=100000, time_budget=120.0),
    }


def main():
    for name, kw in prop_searches().items():
        found, path, st = search(**kw)
        print(f"--- {name}: {'FOUND' if found else 'not found'}")
        print(f"    depth={len(path) if found else '-'} nodes={st['nodes']} "
              f"branch(mean/max)={st['branch_mean']:.1f}/{st['branch_max']} "
              f"wall={st['wall_s']:.2f}s entails={st['entails_s']:.2f}s "
              f"(calls={st['entails_calls']}, hits={st['entails_cache_hits']})")
        if found:
            for i, (r, a, b) in enumerate(path, 1):
                print(f"    {i}. {r} {a} {b}")
            extra = kw.get("extra_la")
            verify_path(kw["hyps"], path, kw["goal"], extra)
            print("    replay: OK")


if __name__ == "__main__":
    main()
