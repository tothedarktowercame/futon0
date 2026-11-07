#!/usr/bin/env python3
"""Fast System E engine core: replays LeanEuclid Book I.1-I.3 without SMT/Lean.

State = a set of diagram facts (incidence, betweenness, circle position, ...)
plus a store of exact linear arithmetic constraints over segment lengths
(fractions.Fraction). Each euclid_apply step is a rule application; every
premise of the cited rule must be derived by the engine's own forward closure
(fact rules + linear arithmetic). If a premise cannot be derived, the replay
STOPS and names it — no coordinate model, no diagram trust.

Metric arithmetic (class LinearArithmetic): decides linear equalities and
(strict/non-strict) inequalities over segment-length atoms with rational
coefficients, by Gaussian substitution for equalities and Fourier-Motzkin
elimination for inequalities. It therefore handles I.1-I.3 style reasoning:
radius equalities, betweenness sums (ab + bc = ac), and comparisons like
|a-b| > |c0-c1| combined with equalities. It CANNOT decide: anything nonlinear
(products, e.g. rectangle_area), angle or area arithmetic (those atoms are not
even represented here), or existence facts that need continuity beyond the
construction axioms (which are constructions, not arithmetic). Angles and areas
are out of scope for I.1-I.3.

Every rule cites its LeanEuclid source as file:line so the table can be checked
against ~/code/LeanEuclid/SystemE by reading.
"""

from fractions import Fraction
import itertools
import time

# ---------------------------------------------------------------------------
# Exact linear arithmetic over segment-length atoms
# ---------------------------------------------------------------------------


def len_atom(a, b):
    """Canonical atom for |(a-b)|. Sorting the pair applies segment_symmetric
    (SystemE/Theory/Inferences/Metric.lean:41, |a-b| = |b-a|) as a rewrite."""
    return ("len",) + tuple(sorted((a, b)))


def norm(coeffs, const):
    """Normalize a linear form to a tuple of (sorted) (atom, coeff) pairs + const."""
    items = tuple(sorted((a, c) for a, c in coeffs.items() if c != 0))
    return items, const


class LinearArithmetic:
    """Constraint store: rows (coeffs, const, op) with op in {=, <=, <}."""

    def __init__(self):
        self.rows = []  # each: (norm-items, const, op)

    def add(self, coeffs, const, op):
        if op == ">":
            coeffs, const, op = {k: -v for k, v in coeffs.items()}, -const, "<"
        elif op == ">=":
            coeffs, const, op = {k: -v for k, v in coeffs.items()}, -const, "<="
        # rows are stored as: sum(coeff*atom) + const ~ 0
        items, c = norm(coeffs, -Fraction(const))
        if not items:
            ok = (c == 0) if op == "=" else ((c <= 0) if op == "<=" else (c < 0))
            return ok
        row = (items, c, op)
        if row not in self.rows:
            self.rows.append(row)
        return True

    def entails(self, coeffs, const, op):
        """Is `sum coeffs*atom ~ const` entailed? Decided by checking
        satisfiability of the store plus the negation (Fourier-Motzkin)."""
        if op in ("=",):
            negs = [(coeffs, const, "<"), ({k: -v for k, v in coeffs.items()}, -const, "<")]
        elif op in ("<", "<="):
            negs = [({k: -v for k, v in coeffs.items()}, -const,
                     "<" if op == "<=" else "<=")]
        else:  # > or >=
            return self.entails({k: -v for k, v in coeffs.items()}, -const,
                                "<" if op == ">" else "<=")
        for neg in negs:
            if _fm_sat(self.rows + [_row_of(*neg)]):
                return False
        return True

    def size(self):
        return len(self.rows)


def _row_of(coeffs, const, op):
    items, c = norm(coeffs, -Fraction(const))
    return (items, c, op)


def _check_const(c, op):
    if op == "=":
        return c == 0
    if op == "<=":
        return c <= 0
    return c < 0


def _fm_sat(rows):
    """Fourier-Motzkin satisfiability over rational linear constraints."""
    rows = list(rows)
    # equalities: eliminate by substitution
    changed = True
    while changed:
        changed = False
        for i, (items, c, op) in enumerate(rows):
            if op == "=":
                var = items[0][0]
                co = dict(items)
                if co[var] != 1:
                    div = co[var]
                    co = {k: v / div for k, v in co.items()}
                    c = c / div
                co.pop(var)
                rows = [r for j, r in enumerate(rows) if j != i]
                # var + sum(co) + c = 0  =>  var = -c - sum(co)
                rows = _subst_rows(rows, var, {k: -v for k, v in co.items()}, -c)
                if rows is None:
                    return False
                changed = True
                break
    # inequalities: eliminate variables one at a time
    while True:
        vars_ = sorted({a for items, _, _ in rows for a, _ in items})
        if not vars_:
            return all(_check_const(c, op) for _, c, op in rows)
        x = vars_[0]
        pos, neg, keep = [], [], []
        for items, c, op in rows:
            co = dict(items)
            if x not in co:
                keep.append((items, c, op))
                continue
            a = co.pop(x)
            items2, c2 = norm(co, c)
            if a > 0:
                pos.append((a, items2, c2, op))
            else:
                neg.append((-a, items2, c2, op))
        new = keep
        for a1, i1, c1, op1 in pos:
            for a2, i2, c2, op2 in neg:
                # pos row: a1*x + S1 ~ 0 (a1>0) => x <= -S1/a1 (if <=)
                # neg row: -a2*x + S2 ~ 0 (a2>0) => x >= S2/a2
                # combined: S1/a1 + S2/a2 <= 0
                co = {}
                for at, v in i1:
                    co[at] = co.get(at, 0) + v / a1
                for at, v in i2:
                    co[at] = co.get(at, 0) + v / a2
                strict = op1 == "<" or op2 == "<"
                new.append(_row_of(co, -(c1 / a1 + c2 / a2),
                                   "<" if strict else "<="))
        if len(new) > 400:  # safety valve; I.1-I.3 never approaches this
            return True
        rows = new


def _subst_rows(rows, var, sub_co, sub_c):
    """Substitute var = sum(sub_co) + sub_c in all rows. sub_co: dict."""
    out = []
    for items, c, op in rows:
        co = dict(items)
        if var in co:
            v = co.pop(var)
            for at, w in sub_co.items():
                co[at] = co.get(at, 0) + v * w
            c = c - v * sub_c
        items2, c2 = norm(co, c)
        if items2:
            out.append((items2, c2, op))
        elif not _check_const(c2, op):
            return None
    return out


# ---------------------------------------------------------------------------
# Facts and state
# ---------------------------------------------------------------------------
# Diagram facts are tuples:
#   ("on_line", p, L) ("on_circle", p, C) ("centre", p, C)
#   ("inside", p, C) ("not_outside", p, C) ("outside", p, C)
#   ("between", a, b, c) ("intersects_cc", C, D)
#   ("neq", x, y) ("not_on_line", p, L) ("opposing_sides", p, q, L)
# Metric facts go into the LinearArithmetic store; provenance for a metric
# derivation is ("arithmetic", n_constraints).


class MissingPremise(Exception):
    def __init__(self, step, rule, premise, explanation=""):
        super().__init__(
            f"step {step} ({rule}): premise not derivable: {premise} {explanation}")
        self.step, self.rule, self.premise = step, rule, premise


class State:
    def __init__(self):
        self.facts = {}        # fact tuple -> provenance (rule, [premise facts])
        self.la = LinearArithmetic()
        self.fresh = 0

    def assume(self, fact, why="hypothesis"):
        self.facts[fact] = (why, [])

    def add(self, fact, rule, premises):
        if fact not in self.facts:
            self.facts[fact] = (rule, list(premises))

    def has(self, fact):
        return fact in self.facts

    def new(self, base):
        self.fresh += 1
        return f"{base}${self.fresh}"

    def prov_chain(self, fact, depth=0, seen=None):
        """Textual derivation chain for a diagram fact."""
        if fact not in self.facts:
            return "  " * depth + str(fact) + "  [not present]"
        rule, premises = self.facts[fact]
        if seen is None:
            seen = set()
        if fact in seen or rule == "hypothesis" or rule == "given":
            return "  " * depth + f"{fact}  [{rule}]"
        seen = seen | {fact}
        lines = ["  " * depth + f"{fact}  [{rule}]"]
        for p in premises:
            lines.append(self.prov_chain(p, depth + 1, seen))
        return "\n".join(lines)


def eq_len(la, x, y, u, v):
    """Add |x-y| = |u-v|."""
    la.add({len_atom(x, y): 1, len_atom(u, v): -1}, 0, "=")


def la_entails_eq(la, x, y, u, v):
    return la.entails({len_atom(x, y): 1, len_atom(u, v): -1}, 0, "=")


def la_entails_lt(la, x, y, u, v):
    return la.entails({len_atom(x, y): 1, len_atom(u, v): -1}, 0, "<")


def la_entails_gt(la, x, y, u, v):
    return la.entails({len_atom(x, y): 1, len_atom(u, v): -1}, 0, ">")


# ---------------------------------------------------------------------------
# Forward closure. Every rule cites LeanEuclid SystemE (or Book) file:line.
# Each rule is fn(state) -> iterable of (new_fact, [premise facts]).
# Metric side effects (constraint additions) are keyed by marker facts so the
# fixpoint stays idempotent.
# ---------------------------------------------------------------------------


def r_center_inside(s):
    # Diagrammatic.lean:28 — the centre of a circle is inside it.
    for fact in s.facts:
        if fact[0] == "centre":
            yield ("inside", fact[1], fact[2]), [fact]


def r_not_outside(s):
    # Relations.lean:35 (outsideCircle = not inside and not on).
    for fact in s.facts:
        if fact[0] == "on_circle":
            yield ("not_outside", fact[1], fact[2]), [fact]
        elif fact[0] == "inside":
            yield ("not_outside", fact[1], fact[2]), [fact]


def r_icc2(s):
    # Diagrammatic.lean:319 — a onCircle α, b insideCircle α, a insideCircle β,
    # b onCircle β  ⇒  α intersectsCircle β.
    for f1 in s.facts:
        if f1[0] != "on_circle":
            continue
        a, alpha = f1[1], f1[2]
        for f2 in s.facts:
            if f2[0] != "inside" or f2[2] != alpha or f2[1] == a:
                continue
            b = f2[1]
            for beta in {f3[2] for f3 in s.facts
                         if f3[0] == "inside" and f3[1] == a and f3[2] != alpha}:
                if s.has(("on_circle", b, beta)) and \
                        not s.has(("intersects_cc", alpha, beta)):
                    yield (("intersects_cc", alpha, beta), [
                        f1, f2, ("inside", a, beta), ("on_circle", b, beta)])


def r_icc_symm(s):
    # Diagrammatic.lean:289 — circle intersection is symmetric.
    for fact in s.facts:
        if fact[0] == "intersects_cc":
            yield ("intersects_cc", fact[2], fact[1]), [fact]


def r_dpl(s):
    # Relations.lean:6 — onLine + onLine + ≠ ⇒ distinctPointsOnLine.
    for f1 in s.facts:
        if f1[0] != "on_line":
            continue
        p, L = f1[1], f1[2]
        for f2 in s.facts:
            if f2[0] != "on_line" or f2[2] != L or f2[1] == p:
                continue
            q = f2[1]
            if s.has(("neq", p, q)) and not s.has(("dpl", p, q, L)):
                yield (("dpl", p, q, L), [f1, f2, ("neq", p, q)])


def r_neq_pos(s):
    # Metric.lean:21,25,33 — a ≠ b ⇒ |a-b| > 0 (marker-guarded).
    for fact in s.facts:
        if fact[0] == "neq":
            a, b = fact[1], fact[2]
            for pair in ((a, b), (b, a)):
                marker = ("pos_len", pair[0], pair[1])
                if not s.has(marker):
                    s.la.add({len_atom(*pair): 1}, 0, ">")
                    yield marker, [fact]


def r_between_if(s):
    # Transfer.lean:16 — between a b c ⇒ |ab| + |bc| = |ac| (marker-guarded).
    for fact in s.facts:
        if fact[0] == "between":
            a, b, c = fact[1], fact[2], fact[3]
            marker = ("len_sum", a, b, c)
            if not s.has(marker):
                s.la.add({len_atom(a, b): 1, len_atom(b, c): 1,
                          len_atom(a, c): -1}, 0, "=")
                yield marker, [fact]


def r_poc_onlyif(s):
    # Transfer.lean:39 — centre + two points on circle ⇒ equal radii
    # (marker-guarded, unordered pair via sorted names).
    circles = {}
    for fact in s.facts:
        if fact[0] == "centre":
            circles[fact[2]] = fact[1]
    for fact in s.facts:
        if fact[0] != "on_circle":
            continue
        p, alpha = fact[1], fact[2]
        o = circles.get(alpha)
        if o is None:
            continue
        for f2 in s.facts:
            if f2[0] != "on_circle" or f2[2] != alpha or f2[1] == p:
                continue
            q = f2[1]
            if (q, p) > (p, q):
                continue
            marker = ("radii", alpha, min(p, q), max(p, q))
            if not s.has(marker):
                s.la.add({len_atom(o, p): 1, len_atom(o, q): -1}, 0, "=")
                yield marker, [fact, f2, ("centre", o, alpha)]


def r_classify(s):
    # Transfer.lean:34 (on, if equal), :47 (inside, if smaller), and outside
    # via the contrapositive of :39/:47/:52 with the definition at
    # Relations.lean:35 — compare |centre-p| to the radius exactly.
    circles = {}
    for fact in s.facts:
        if fact[0] == "centre":
            circles[fact[2]] = fact[1]
    for alpha, o in circles.items():
        witnesses = [f[1] for f in s.facts
                     if f[0] == "on_circle" and f[2] == alpha]
        if not witnesses:
            continue
        w = witnesses[0]
        for p in _all_points(s):
            if any(s.has((k, p, alpha)) for k in ("on_circle", "inside", "outside")):
                continue
            if la_entails_eq(s.la, o, p, o, w):
                yield (("on_circle", p, alpha),
                       [("centre", o, alpha), ("on_circle", w, alpha)])
            elif la_entails_lt(s.la, o, p, o, w):
                yield (("inside", p, alpha),
                       [("centre", o, alpha), ("on_circle", w, alpha)])
            elif la_entails_gt(s.la, o, p, o, w):
                yield (("outside", p, alpha),
                       [("centre", o, alpha), ("on_circle", w, alpha)])


def r_pos_neq(s):
    # Metric.lean:21,25 — |a-b| > 0 ⇒ a ≠ b.
    la = s.la
    for a in _all_points(s):
        for b in _all_points(s):
            if a == b or s.has(("neq", a, b)) or s.has(("neq", b, a)):
                continue
            if a > b:
                continue
            if la.entails({len_atom(a, b): 1}, 0, ">"):
                for pair in ((a, b), (b, a)):
                    if not s.has(("neq", *pair)):
                        yield (("neq", *pair), [("pos_len", a, b), ("pos_len", b, a)])


FORWARD_RULES = [
    ("center_inside_circle [Diagrammatic.lean:28]", r_center_inside),
    ("not_outside [Relations.lean:35]", r_not_outside),
    ("intersection_circle_circle_2 [Diagrammatic.lean:319]", r_icc2),
    ("intersection_symm [Diagrammatic.lean:289]", r_icc_symm),
    ("distinct_points_on_line [Relations.lean:6]", r_dpl),
    ("neq_positive_length [Metric.lean:21,25,33]", r_neq_pos),
    ("between_if [Transfer.lean:16]", r_between_if),
    ("point_on_circle_onlyif [Transfer.lean:39]", r_poc_onlyif),
    ("classify_point_vs_circle [Transfer.lean:34,47; Relations.lean:35]", r_classify),
    ("positive_length_neq [Metric.lean:21,25]", r_pos_neq),
]


def close(s):
    """Run forward rules to fixpoint. Returns iteration count."""
    it = 0
    while True:
        it += 1
        changed = False
        for name, fn in FORWARD_RULES:
            for newfact, prem in list(fn(s)):
                if newfact not in s.facts:
                    s.add(newfact, name, prem)
                    changed = True
        if not changed:
            return it


def _all_points(s):
    pts = set()
    for fact in s.facts:
        for x in fact[1:]:
            if isinstance(x, str) and not x.startswith(("L$", "C$")):
                pts.add(x)
    return pts


def r_neq_symm(s):
    for fact in s.facts:
        if fact[0] == "neq" and not s.has(("neq", fact[2], fact[1])):
            yield (("neq", fact[2], fact[1]), [fact])


FORWARD_RULES.insert(0, ("neq_symm [logic]", r_neq_symm))


# ---------------------------------------------------------------------------
# Construction rules (introduce fresh objects). Premises are checked against
# the closed state; a failed check raises MissingPremise. Each entry cites its
# source file:line. Propositions with existential conclusions (I.1, I.2') are
# constructions when cited — they introduce a witness point.
# ---------------------------------------------------------------------------


def _need(s, i, rule, fact):
    if not s.has(fact):
        raise MissingPremise(i, rule, fact, "(no forward rule derives it)")
    return fact


CONSTRUCTIONS = {
    # SystemE/Theory/Constructions/LinesAndCircles.lean:3
    "line_from_points": {
        "cite": "LinesAndCircles.lean:3",
        "args": ("a", "b"),
        "premises": lambda s, a, b: [("neq", a, b)],
        "make": lambda s, a, b: [
            ("L", [("on_line", a, "L"), ("on_line", b, "L")])],
    },
    # LinesAndCircles.lean:6
    "circle_from_points": {
        "cite": "LinesAndCircles.lean:6",
        "args": ("a", "b"),
        "premises": lambda s, a, b: [("neq", a, b)],
        "make": lambda s, a, b: [
            ("C", [("centre", a, "C"), ("on_circle", b, "C")])],
    },
    # Constructions/Intersections.lean:17
    "intersection_circles": {
        "cite": "Intersections.lean:17",
        "args": ("alpha", "beta"),
        "premises": lambda s, a, b: [("intersects_cc", a, b)],
        "make": lambda s, a, b: [
            ("c", [("on_circle", "c", a), ("on_circle", "c", b)])],
    },
    # Intersections.lean:13 — inside point b on line L with c on L
    "intersection_circle_line_extending_points": {
        "cite": "Intersections.lean:13",
        "args": ("alpha", "L", "b", "c"),
        "premises": lambda s, a, L, b, c: [
            ("inside", b, a), ("dpl", b, c, L)],
        "make": lambda s, a, L, b, c: [
            ("g", [("on_circle", "g", a), ("on_line", "g", L),
                   ("between", "g", b, c)])],
    },
    # Intersections.lean:9 — between an inside b and outside c, both on L
    "intersection_circle_line_between_points": {
        "cite": "Intersections.lean:9",
        "args": ("alpha", "L", "b", "c"),
        "premises": lambda s, a, L, b, c: [
            ("inside", b, a), ("on_line", b, L),
            ("outside", c, a), ("on_line", c, L)],
        "make": lambda s, a, L, b, c: [
            ("e", [("on_circle", "e", a), ("on_line", "e", L),
                   ("between", b, "e", c)])],
    },
    # Intersections.lean:24 — circle-circle crossing on the opposite side of L
    "intersection_opposite_side": {
        "cite": "Intersections.lean:24",
        "args": ("alpha", "beta", "b", "c", "d", "L"),
        "premises": lambda s, a, be, b, c, d, L: [
            ("intersects_cc", a, be), ("centre", c, a), ("centre", d, be),
            ("on_line", c, L), ("on_line", d, L), ("not_on_line", b, L)],
        "make": lambda s, a, be, b, c, d, L: [
            ("a", [("on_circle", "a", a), ("on_circle", "a", be),
                   ("opposing_sides", "a", b, L)])],
    },
    # Book/Prop01.lean:5 — equilateral construction (existential conclusion,
    # so a construction when cited)
    "proposition_1": {
        "cite": "Book/Prop01.lean:5",
        "args": ("a", "b", "AB"),
        "premises": lambda s, a, b, AB: [("dpl", a, b, AB)],
        "make": lambda s, a, b, AB: [
            ("d", [("_eq_len", a, "d", a, b), ("_eq_len", "d", b, a, b)])],
    },
    # Book/Prop02.lean:36 — lay off a length from any point a (existential)
    "proposition_2'": {
        "cite": "Book/Prop02.lean:36",
        "args": ("a", "b", "c", "BC"),
        "premises": lambda s, a, b, c, BC: [("dpl", b, c, BC)],
        "make": lambda s, a, b, c, BC: [
            ("d", [("_eq_len", a, "d", b, c)])],
    },
}


def apply_step(s, i, rule, args, bindings):
    """Apply one euclid_apply-style step. Returns a report dict.
    Raises MissingPremise if a premise is not in the closed state."""
    if rule not in CONSTRUCTIONS:
        raise MissingPremise(i, rule, "(unknown rule)",
                             "(not in the engine's rule table)")
    spec = CONSTRUCTIONS[rule]
    prem_specs = spec["premises"](s, *args)
    prem_facts = [_need(s, i, rule, pf) for pf in prem_specs]
    made = spec["make"](s, *args)
    added = []
    for base, facts in made:
        name = bindings[base] if base in bindings else s.new(base)
        for fact in facts:
            fact = tuple(name if x == base else x for x in fact)
            if fact[0] == "_eq_len":
                eq_len(s.la, *fact[1:])
                s.add(("eq_len", *fact[1:]),
                      f"{rule} [{spec['cite']}]", prem_facts)
                added.append(("eq_len", *fact[1:]))
            else:
                s.add(fact, f"{rule} [{spec['cite']}]", prem_facts)
                added.append(fact)
    iters = close(s)
    return {"step": i, "rule": f"{rule} [{spec['cite']}]",
            "premises": prem_facts, "added": added, "closure_iters": iters}


def replay(hyps, steps, verify):
    """Driver. hyps: list of (fact, why) hypotheses (already destructed, as
    euclid_intros would leave them); steps: list of (rule, args, bindings);
    verify: list of facts/queries the conclusion must satisfy (each is either a
    diagram fact tuple or ("_eq_len", x, y, u, v) metric query).
    Returns (state, report). Raises MissingPremise on the first stuck step,
    AssertionError if a final query fails."""
    s = State()
    for fact, why in hyps:
        s.assume(fact, why)
    close(s)
    report = []
    t0 = time.time()
    for i, (rule, args, bindings) in enumerate(steps, start=1):
        r = apply_step(s, i, rule, args, bindings)
        report.append(r)
    for q in verify:
        if q[0] == "_eq_len":
            assert la_entails_eq(s.la, *q[1:]), f"conclusion not entailed: {q}"
        else:
            assert s.has(q), f"conclusion fact missing: {q}"
    report.append({"final": True, "facts": len(s.facts),
                   "constraints": s.la.size(),
                   "seconds": round(time.time() - t0, 4)})
    return s, report


# ---------------------------------------------------------------------------
# Book I.1-I.3 as data (the proofs of Book/Prop01-03.lean, euclid_intros done)
# ---------------------------------------------------------------------------

def _prop1():
    hyps = [(("on_line", "a", "AB"), "h"), (("on_line", "b", "AB"), "h"),
            (("neq", "a", "b"), "h")]
    steps = PROP1_STEPS_DATA
    verify = [("on_circle", "c", "BCD")]
    return hyps, steps, verify


PROP1_STEPS_DATA = [
    ("circle_from_points", ("a", "b"), {"C": "BCD"}),
    ("circle_from_points", ("b", "a"), {"C": "ACE"}),
    ("intersection_circles", ("BCD", "ACE"), {"c": "c"}),
]
PROP2_STEPS_DATA = [
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
]
PROP2_HYPS_DATA = [
    (("on_line", "b", "BC"), "h"), (("on_line", "c", "BC"), "h"),
    (("neq", "b", "c"), "h"), (("neq", "a", "b"), "h"),
]
PROP3_HYPS_DATA = [
    (("on_line", "a", "AB"), "h"), (("on_line", "b", "AB"), "h"),
    (("neq", "a", "b"), "h"),
    (("on_line", "c0", "C"), "h"), (("on_line", "c1", "C"), "h"),
    (("neq", "c0", "c1"), "h"),
]
PROP3_STEPS_DATA = [
    ("proposition_2'", ("a", "c0", "c1", "C"), {"d": "d"}),
    ("circle_from_points", ("a", "d"), {"C": "DEF"}),
    ("intersection_circle_line_between_points", ("DEF", "AB", "a", "b"),
     {"e": "e"}),
]


def main():
    import sys
    which = sys.argv[1] if len(sys.argv) > 1 else "1,2,3"
    for p in which.split(","):
        if p == "1":
            s, rep = replay(_prop1()[0], PROP1_STEPS_DATA,
                            [("on_circle", "c", "BCD")])
        elif p == "2":
            s = State()
            for f, w in PROP2_HYPS_DATA:
                s.assume(f, w)
            close(s)
            t0 = time.time()
            rep = [apply_step(s, i, r, a, b)
                   for i, (r, a, b) in enumerate(PROP2_STEPS_DATA, 1)]
            rep.append({"final": True, "facts": len(s.facts),
                        "constraints": s.la.size(),
                        "seconds": round(time.time() - t0, 4)})
            assert la_entails_eq(s.la, "a", "l", "b", "c"), "I.2 conclusion"
        elif p == "3":
            s = State()
            for f, w in PROP3_HYPS_DATA:
                s.assume(f, w)
            s.la.add({len_atom("a", "b"): 1, len_atom("c0", "c1"): -1},
                     0, ">")
            close(s)
            t0 = time.time()
            rep = [apply_step(s, i, r, a, b)
                   for i, (r, a, b) in enumerate(PROP3_STEPS_DATA, 1)]
            rep.append({"final": True, "facts": len(s.facts),
                        "constraints": s.la.size(),
                        "seconds": round(time.time() - t0, 4)})
            assert s.has(("between", "a", "e", "b")), "I.3 between"
            assert la_entails_eq(s.la, "a", "e", "c0", "c1"), "I.3 length"
        for r in rep:
            print(r)
        print()


if __name__ == "__main__":
    main()



