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


def ang_atom(a, b, c):
    """Canonical atom for ∠(a:b:c) (vertex b). Sorting the legs applies
    angle_symm (SystemE/Theory/Inferences/Metric.lean:46, ∠a:b:c = ∠c:b:a)."""
    x, y = sorted((a, c))
    return ("ang", x, b, y)


RT = ("rt",)  # the right angle, a constant atom in the arithmetic store


def eq_ang(la, a, b, c, d, e, f):
    """Add ∠(a:b:c) = ∠(d:e:f)."""
    la.add({ang_atom(a, b, c): 1, ang_atom(d, e, f): -1}, 0, "=")


def la_entails_eq_ang(la, a, b, c, d, e, f):
    if ang_atom(a, b, c) == ang_atom(d, e, f):
        return True  # same atom: a dict literal would collapse the terms
    return la.entails({ang_atom(a, b, c): 1, ang_atom(d, e, f): -1}, 0, "=")


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
            try:
                if _fm_sat(self.rows + [_row_of(*neg)]):
                    return False
            except FMTooLarge:
                return False  # undecided: do not claim entailment
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


class FMTooLarge(Exception):
    """Fourier-Motzkin elimination grew past its row limit: undecided."""


def _fm_sat(rows):
    """Fourier-Motzkin satisfiability over rational linear constraints.
    All arithmetic is exact: coefficients and constants are coerced to
    Fraction on entry (int/int true division elsewhere in this function
    once produced floats, and float cancellation manufactured rows like
    x = 0 that contradicted x > 0 — a false 'unsat' that made every
    entailment hold vacuously; regression: euclid_search_test.py)."""
    def frac(x):
        return x if isinstance(x, Fraction) else Fraction(x)
    rows = [(tuple((at, frac(co)) for at, co in items), frac(c), op)
            for items, c, op in rows]
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
        if len(new) > 400:
            # Too large to decide. Raising (rather than answering "sat")
            # keeps every caller honest: entailment treats it as "not
            # entailed", and a consistency check must treat it as "cannot
            # certify" (review, claude-9).
            raise FMTooLarge(len(new))
        rows = new


def _subst_rows(rows, var, sub_co, sub_c):
    """Substitute var = sum(sub_co) + sub_c in all rows. sub_co: dict.
    A row v*var + S + c ~ 0 becomes v*(sub_co) + S + (c + v*sub_c) ~ 0.
    (The old code wrote c - v*sub_c — a sign error that only bites once a
    previous substitution makes sub_c nonzero, i.e. exactly in multi-step
    substitution chains; replay constants were all 0, so it hid until the
    search layered circle equalities. Regression: euclid_search_test.py
    test_engine_regression_no_line_names_as_points.)"""
    out = []
    for items, c, op in rows:
        co = dict(items)
        if var in co:
            v = co.pop(var)
            for at, w in sub_co.items():
                co[at] = co.get(at, 0) + v * w
            c = c + v * sub_c
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
        self.rule_cache = {}   # (rule, nrows, query) -> bool; rows only grow

    def assume(self, fact, why="hypothesis"):
        self.facts[fact] = (why, [])

    def add(self, fact, rule, premises):
        if fact not in self.facts:
            self.facts[fact] = (rule, list(premises))

    def has(self, fact):
        return fact in self.facts

    def copy(self):
        """Deep enough copy for by_contra: facts dict + fresh constraint
        store (rows are immutable tuples)."""
        s2 = State()
        s2.facts = dict(self.facts)
        s2.fresh = self.fresh
        s2.la.rows = list(self.la.rows)
        s2.rule_cache = dict(getattr(self, "rule_cache", {}))
        return s2

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
    if len_atom(x, y) == len_atom(u, v):
        return True  # same atom: a dict literal would collapse the terms
    return la.entails({len_atom(x, y): 1, len_atom(u, v): -1}, 0, "=")


def la_entails_lt(la, x, y, u, v):
    if len_atom(x, y) == len_atom(u, v):
        return False
    return la.entails({len_atom(x, y): 1, len_atom(u, v): -1}, 0, "<")


def la_entails_gt(la, x, y, u, v):
    if len_atom(x, y) == len_atom(u, v):
        return False
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


def r_dpl_parts(s):
    # Relations.lean:6 (definition) — dpl p q L is on_line p L ∧ on_line q L
    # ∧ p ≠ q; all three parts are yielded (the p ≠ q part was missing).
    for fact in s.facts:
        if fact[0] == "dpl":
            yield ("on_line", fact[1], fact[3]), [fact]
            yield ("on_line", fact[2], fact[3]), [fact]
            yield ("neq", fact[1], fact[2]), [fact]
            yield ("neq", fact[2], fact[1]), [fact]


def r_between_symm(s):
    # Diagrammatic.lean:45 — between a b c ⇒ between c b a.
    for fact in s.facts:
        if fact[0] == "between":
            yield ("between", fact[3], fact[2], fact[1]), [fact]


def r_between_same_line_out(s):
    # Diagrammatic.lean:53 — between a b c, a and b on L ⇒ c on L.
    lines = {}
    for fact in s.facts:
        if fact[0] == "on_line":
            lines.setdefault(fact[2], set()).add(fact[1])
    for fact in s.facts:
        if fact[0] == "between":
            a, b, c = fact[1], fact[2], fact[3]
            for L, pts in lines.items():
                if a in pts and b in pts and c not in pts:
                    yield ("on_line", c, L), [fact, ("on_line", a, L),
                                              ("on_line", b, L)]


def r_between_same_line_in(s):
    # Diagrammatic.lean:61 — between a b c, a and c on L ⇒ b on L.
    lines = {}
    for fact in s.facts:
        if fact[0] == "on_line":
            lines.setdefault(fact[2], set()).add(fact[1])
    for fact in s.facts:
        if fact[0] == "between":
            a, b, c = fact[1], fact[2], fact[3]
            for L, pts in lines.items():
                if a in pts and c in pts and b not in pts:
                    yield ("on_line", b, L), [fact, ("on_line", a, L),
                                              ("on_line", c, L)]


def r_between_trans_in(s):
    # Diagrammatic.lean:69 — between a b c ∧ between a d b ⇒ between a d c.
    bt = {f for f in s.facts if f[0] == "between"}
    for f1 in bt:
        a, b, c = f1[1], f1[2], f1[3]
        for f2 in bt:
            if f2[0] == "between" and f2[1] == a and f2[3] == b:
                if not s.has(("between", a, f2[2], c)):
                    yield (("between", a, f2[2], c), [f1, f2])


def r_same_side_symm(s):
    # Diagrammatic.lean:112 — same side is symmetric.
    for fact in s.facts:
        if fact[0] == "same_side":
            yield ("same_side", fact[2], fact[1], fact[3]), [fact]


def r_same_side_trans(s):
    # Diagrammatic.lean:127 — a~b and a~c on L ⇒ b~c on L.
    ss = [f for f in s.facts if f[0] == "same_side"]
    for f1 in ss:
        for f2 in ss:
            if f1[1] == f2[1] and f1[3] == f2[3] and f1[2] != f2[2]:
                if not s.has(("same_side", f1[2], f2[2], f1[3])):
                    yield (("same_side", f1[2], f2[2], f1[3]), [f1, f2])


def r_pasch_2(s):
    # Diagrammatic.lean:153 — between a b c, a on L, b not on L ⇒ b~c on L.
    for fact in s.facts:
        if fact[0] != "between":
            continue
        a, b, c = fact[1], fact[2], fact[3]
        for f2 in s.facts:
            if f2[0] == "on_line" and f2[1] == a and \
                    s.has(("not_on_line", b, f2[2])):
                if not s.has(("same_side", b, c, f2[2])):
                    yield (("same_side", b, c, f2[2]),
                           [fact, f2, ("not_on_line", b, f2[2])])


def r_off_line_neq(s):
    # [logic] a point off a line differs from every point on it.
    for f1 in s.facts:
        if f1[0] != "not_on_line":
            continue
        x, L = f1[1], f1[2]
        for f2 in s.facts:
            if f2[0] == "on_line" and f2[2] == L:
                for pair in ((x, f2[1]), (f2[1], x)):
                    if not s.has(("neq", *pair)):
                        yield (("neq", *pair), [f1, f2])


def r_two_lines_meet_once(s):
    # [SMT-bridge, from two_points_determine_line Diagrammatic.lean:14]
    # x on M, L ≠ M, p on both L and M, x ≠ p ⇒ x not on L (two distinct
    # lines share at most one point). Lean's SMT closes such cases; the
    # engine states the contrapositive directly.
    for fL in s.facts:
        if fL[0] != "neq_line":
            continue
        L, M = fL[1], fL[2]
        for fp in s.facts:
            if fp[0] != "on_line" or fp[2] != L or not s.has(("on_line", fp[1], M)):
                continue
            p = fp[1]
            for fx in s.facts:
                if fx[0] == "on_line" and fx[2] == M and fx[1] != p and \
                        s.has(("neq", fx[1], p)) and \
                        not s.has(("not_on_line", fx[1], L)):
                    yield (("not_on_line", fx[1], L),
                           [fL, fp, ("on_line", fx[1], M), ("neq", fx[1], p)])


def r_degenerated_angle(s):
    # Transfer.lean:63 — a,b,c on L, a≠b, a≠c, ¬between b a c ⇒ ∠b:a:c = 0.
    for f1 in s.facts:
        if f1[0] != "between":
            continue
        # order v-x-y on a line: v is not between x and y, so ∠x:v:y = 0
        v, x, y = f1[1], f1[2], f1[3]
        for f2 in s.facts:
            if f2[0] != "on_line":
                continue
            L = f2[2]
            if f2[1] != v:
                continue
            if not (s.has(("on_line", x, L)) and s.has(("on_line", y, L))):
                continue
            if not (s.has(("neq", v, x)) and s.has(("neq", v, y))):
                continue
            marker = ("zero_ang", v, x, y, L)
            if not s.has(marker):
                s.la.add({ang_atom(x, v, y): 1}, 0, "=")
                yield marker, [f1, f2, ("on_line", x, L), ("on_line", y, L),
                               ("neq", v, x), ("neq", v, y)]


def r_angle_ray_eq(s):
    # [SMT-bridge] between v x y (order v-x-y) ⇒ ∠(z,v,x) = ∠(z,v,y) for
    # every z: the rays v→x and v→y coincide, so the angles are equal.
    # LeanEuclid derives this in SMT (the zero angle ∠x:v:y itself is the
    # axiom degenerated_angle_if, Transfer.lean:63; the ray equality is the
    # bridge). Marker-guarded.
    pts = _all_points(s)
    for fact in s.facts:
        if fact[0] != "between":
            continue
        v, x, y = fact[1], fact[2], fact[3]
        for z in pts:
            if z in (v, x, y):
                continue
            marker = ("rayang", v, x, y, z)
            if not s.has(marker):
                s.la.add({ang_atom(z, v, x): 1, ang_atom(z, v, y): -1},
                         0, "=")
                yield marker, [fact]


def r_between_order(s):
    # [SMT-bridge] between a x z ∧ between a y z ∧ |a-x| < |a-y| ⇒
    # between a x y: interior points of a segment are ordered by distance.
    # LeanEuclid's SMT proves this from the coordinate model.
    bt = [f for f in s.facts if f[0] == "between"]
    for fx in bt:
        a, x, z = fx[1], fx[2], fx[3]
        for fy in bt:
            if fy[1] != a or fy[3] != z:
                continue
            y = fy[2]
            if x == y or s.has(("between", a, x, y)):
                continue
            if la_entails_lt(s.la, a, x, a, y):
                yield (("between", a, x, y), [fx, fy])


def r_straight_angle(s):
    # [SMT-bridge] between u v w ∧ x off the line ⇒ ∠u:v:x + ∠x:v:w = 2∟
    # (angles on a straight line). Requires the right-angle constant.
    for fact in s.facts:
        if fact[0] != "between":
            continue
        u, v, w = fact[1], fact[2], fact[3]
        for f2 in s.facts:
            if f2[0] != "not_on_line":
                continue
            x, L = f2[1], f2[2]
            if not (s.has(("on_line", u, L)) and s.has(("on_line", v, L))
                    and s.has(("on_line", w, L))):
                continue
            marker = ("strang", u, v, w, x, L)
            if not s.has(marker):
                s.la.add({ang_atom(u, v, x): 1, ang_atom(x, v, w): 1,
                          RT: -2}, 0, "=")
                yield marker, [fact, f2, ("on_line", u, L),
                               ("on_line", v, L), ("on_line", w, L)]


def r_len_gte_zero(s):
    # Metric.lean:33 — every segment length is ≥ 0. Applied to every length
    # atom present in the store, marker-guarded so the fixpoint stays
    # idempotent (atoms added later pick their row up on the next pass).
    la = s.la
    for a in {at for items, _, _ in la.rows for at, _ in items}:
        if a[0] == "len":
            marker = ("gte", a[1], a[2])
            if not s.has(marker):
                la.add({a: -1}, 0, "<=")
                yield marker, []


def r_peq_subst(s):
    # [derived from zero_segment_if, Metric.lean:21] point equality x = y
    # substitutes into every length/angle atom present in the store.
    la = s.la
    atoms = {a for items, _, _ in la.rows for a, _ in items}
    for fact in s.facts:
        if fact[0] != "peq":
            continue
        x, y = fact[1], fact[2]
        for a in atoms:
            if a[0] == "len" and x in (a[1], a[2]):
                other = tuple(y if t == x else t for t in a[1:3])
                a2 = ("len",) + tuple(sorted(other))
                marker = ("sublen", a, x, y)
                if not s.has(marker):
                    s.la.add({a: 1, a2: -1}, 0, "=")
                    yield marker, [fact]
            elif a[0] == "ang" and x in a[1:]:
                legs, vert = (a[1], a[3]), a[2]
                sub = tuple(y if t == x else t for t in (legs[0], vert, legs[1]))
                a2 = ang_atom(*sub)
                marker = ("subang", a, x, y)
                if not s.has(marker):
                    s.la.add({a: 1, a2: -1}, 0, "=")
                    yield marker, [fact]


def r_same_side_not_on_line(s):
    # Diagrammatic.lean:120 — a.sameSide b L → ¬(a.onLine L); with symm
    # (Diagrammatic.lean:113) both points are off L.
    for fact in s.facts:
        if fact[0] == "same_side":
            x, y, L = fact[1], fact[2], fact[3]
            yield ("not_on_line", x, L), [fact]
            yield ("not_on_line", y, L), [fact]


def r_opposing_parts(s):
    # Relations.lean:13-14 — opposingSides a b L := ¬a.onLine L ∧ ¬b.onLine L
    # ∧ ¬ a.sameSide b L. All three parts are yielded.
    for fact in s.facts:
        if fact[0] == "opposing_sides":
            x, y, L = fact[1], fact[2], fact[3]
            yield ("not_on_line", x, L), [fact]
            yield ("not_on_line", y, L), [fact]


def r_line_neq(s):
    # [logic] a point on L that is not on M forces L ≠ M.
    for f1 in s.facts:
        if f1[0] != "on_line":
            continue
        p, L = f1[1], f1[2]
        for f2 in s.facts:
            if f2[0] == "not_on_line" and f2[1] == p and f2[2] != L:
                if not s.has(("neq_line", L, f2[2])):
                    yield (("neq_line", L, f2[2]), [f1, f2])


def r_angle_pos(s):
    # [SMT-bridge] x off L, u and v on L, u ≠ v, x ≠ v ⇒ ∠(x,v,u) > 0
    # (the angle is strictly between 0 and 2∟). LeanEuclid's SMT supplies
    # angle positivity from the coordinate model; the engine states it as a
    # marker-guarded row.
    for f1 in s.facts:
        if f1[0] != "not_on_line":
            continue
        x, L = f1[1], f1[2]
        for u in s.facts:
            if u[0] != "on_line" or u[2] != L or u[1] == x:
                continue
            if not (s.has(("neq", u[1], x))):
                continue
            for v in s.facts:
                if v[0] != "on_line" or v[2] != L or v[1] in (x, u[1]):
                    continue
                if not s.has(("neq", u[1], v[1])):
                    continue
                a = ang_atom(x, v[1], u[1])
                marker = ("apos", a)
                if not s.has(marker):
                    s.la.add({a: 1}, 0, ">")
                    yield marker, [f1, u, v, ("neq", u[1], x), ("neq", u[1], v[1])]


def _snapshot(la):
    """A query-only copy of a constraint store. Rules that both add rows and
    consult entailment must query a snapshot taken at entry: a row the rule
    itself just added can make the live store unsat, and every later
    entailment check in the same generator then passes vacuously (seen:
    side_gt_angle_gt adding ang1<ang2 and ang2<ang1 in one pass)."""
    l2 = LinearArithmetic()
    l2.rows = list(la.rows)
    return l2


def r_side_gt_angle_gt(s):
    # [SMT-bridge, Euclid I.18] p,q,r pairwise distinct and |p-r| > |p-q|
    # ⇒ ∠(p,q,r) > ∠(q,r,p): the greater side subtends the greater angle.
    # Only for a genuine triangle: r off a line through p and q. It FAILS for
    # collinear q-p-r (both angles 0). Note: this is a LATER proposition used
    # as a rule, so a replay of I.6 that uses it does not mirror Lean's
    # argument (Lean's I.6 closes by the area/part-whole reasoning in SMT).
    pts = _all_points(s)
    snap = _snapshot(s.la)
    on = {}
    for f in s.facts:
        if f[0] == "on_line":
            on.setdefault(f[2], set()).add(f[1])
    lines_pq = {}
    for M, ps in on.items():
        for x in ps:
            for y in ps:
                if x != y:
                    lines_pq.setdefault((x, y), []).append(M)
    live_atoms = {at for items, _, _ in s.la.rows for at, _ in items}
    # rows that directly relate two length atoms: only these can entail a
    # comparison (each FM check is a full Fourier-Motzkin run)
    direct = [set(at for at, _ in items) for items, _, _ in s.la.rows
              if sum(1 for at, _ in items if at[0] == "len") >= 2]
    for p in pts:
        for q in pts:
            for r in pts:
                if len({p, q, r}) < 3:
                    continue
                a1, a2 = ang_atom(p, q, r), ang_atom(q, r, p)
                # only compare angles already under discussion: without
                # this the rule fires for every ordered triple with any
                # entailed length comparison and buries the store in rows
                if a1 not in live_atoms or a2 not in live_atoms:
                    continue
                # cheap filter: some row must relate the two lengths
                if not any(({len_atom(p, r), len_atom(p, q)} <= ats
                            for ats in direct)):
                    continue
                marker = ("angord", p, q, r)
                if s.has(marker):
                    continue
                if not (s.has(("neq", p, q)) and s.has(("neq", q, r))
                        and s.has(("neq", p, r))):
                    continue
                # review fix (claude-9): require a genuine triangle. The
                # collinear claim above is false when p lies between q and r
                # (q-p-r, |pr| > |pq|): both angles are 0 and the rule
                # produced a false contradiction on a realisable state.
                tri = next((M for M in lines_pq.get((p, q), ())
                            if s.has(("not_on_line", r, M))), None)
                if tri is None:
                    continue
                key = ("angord", len(snap.rows), p, q, r)
                if key in s.rule_cache:
                    if not s.rule_cache[key]:
                        continue
                elif not la_entails_gt(snap, p, r, p, q):
                    s.rule_cache[key] = False
                    continue
                s.rule_cache[key] = True
                s.la.add({ang_atom(p, q, r): 1, ang_atom(q, r, p): -1},
                         0, "<")
                yield marker, [("neq", p, q), ("neq", q, r), ("neq", p, r),
                               ("on_line", p, tri), ("on_line", q, tri),
                               ("not_on_line", r, tri)]


def r_off_line_pair_same_side(s):
    # [SMT-bridge] same_side c d L ∧ p on L ∧ c ≠ d ∧ c,d on M ∧ |p-c| = |p-d|
    # (entailed) ⇒ p not on M.
    # Why: if p were on M, then c,d,p would be collinear; p ∈ L and c,d on
    # the same side of L means p is not between c and d (Diagrammatic.lean:164:
    # between c p d ∧ p.onLine L → ¬ c.sameSide d L), so c,d lie on the same
    # ray from p, and |p-c| = |p-d| then gives c = d. Contradiction.
    # (The equal-distance premise is required: same ray alone does not
    # identify c and d — dropping it let the rule conclude nonsense like
    # "a not on AB" from any same-side pair straddling a.)
    snap = _snapshot(s.la)
    direct = [set(at for at, _ in items) for items, _, _ in s.la.rows
              if sum(1 for at, _ in items if at[0] == "len") >= 2]
    for fs in s.facts:
        if fs[0] != "same_side":
            continue
        c, d, L = fs[1], fs[2], fs[3]
        for fp in s.facts:
            if fp[0] != "on_line" or fp[2] != L:
                continue
            p = fp[1]
            if not (s.has(("neq", c, d)) and s.has(("neq", p, c))
                    and s.has(("neq", p, d))):
                continue
            # cheap filter: some row must relate the two distances
            if not any(({len_atom(p, c), len_atom(p, d)} <= ats
                        for ats in direct)):
                continue
            key = ("offpair", len(snap.rows), p, c, d)
            if key in s.rule_cache:
                if not s.rule_cache[key]:
                    continue
            elif not la_entails_eq(snap, p, c, p, d):
                s.rule_cache[key] = False
                continue
            s.rule_cache[key] = True
            for M in {f[2] for f in s.facts if f[0] == "on_line"}:
                if M == L:
                    continue
                if not (s.has(("on_line", c, M)) and s.has(("on_line", d, M))):
                    continue
                if not s.has(("not_on_line", p, M)):
                    yield (("not_on_line", p, M),
                           [fs, fp, ("on_line", c, M), ("on_line", d, M),
                            ("neq", c, d)])


FORWARD_RULES = [
    ("center_inside_circle [Diagrammatic.lean:28]", r_center_inside),
    ("not_outside [Relations.lean:35]", r_not_outside),
    ("intersection_circle_circle_2 [Diagrammatic.lean:319]", r_icc2),
    ("intersection_symm [Diagrammatic.lean:289]", r_icc_symm),
    ("distinct_points_on_line [Relations.lean:6]", r_dpl),
    ("dpl_parts [Relations.lean:6 defn]", r_dpl_parts),    ("neq_positive_length [Metric.lean:21,25,33]", r_neq_pos),
    ("between_if [Transfer.lean:16]", r_between_if),
    ("point_on_circle_onlyif [Transfer.lean:39]", r_poc_onlyif),
    ("classify_point_vs_circle [Transfer.lean:34,47; Relations.lean:35]", r_classify),
    ("positive_length_neq [Metric.lean:21,25]", r_pos_neq),
    ("between_symm [Diagrammatic.lean:45]", r_between_symm),
    ("between_same_line_in [Diagrammatic.lean:61]", r_between_same_line_in),
    ("between_same_line_out [Diagrammatic.lean:53]", r_between_same_line_out),
    ("between_trans_in [Diagrammatic.lean:69]", r_between_trans_in),
    ("same_side_symm [Diagrammatic.lean:112]", r_same_side_symm),
    ("same_side_trans [Diagrammatic.lean:127]", r_same_side_trans),
    ("pasch_2 [Diagrammatic.lean:153]", r_pasch_2),
    ("off_line_neq [logic]", r_off_line_neq),
    ("two_lines_meet_once [SMT-bridge from Diagrammatic.lean:14]",
     r_two_lines_meet_once),
    ("degenerated_angle_if [Transfer.lean:63]", r_degenerated_angle),
    ("angle_ray_eq [SMT-bridge]", r_angle_ray_eq),
    ("between_order [SMT-bridge]", r_between_order),
    ("straight_angle [SMT-bridge]", r_straight_angle),
    ("segment_gte_zero [Metric.lean:33]", r_len_gte_zero),
    ("peq_subst [derived, Metric.lean:21]", r_peq_subst),
    ("same_side_not_on_line [Diagrammatic.lean:120]", r_same_side_not_on_line),
    ("opposing_parts [Relations.lean:13-14]", r_opposing_parts),
    ("line_neq [logic]", r_line_neq),
    ("angle_pos [SMT-bridge]", r_angle_pos),
    ("side_gt_angle_gt [SMT-bridge, Euclid I.18]", r_side_gt_angle_gt),
    ("off_line_pair_same_side [SMT-bridge]", r_off_line_pair_same_side),
]


def close(s):
    """Run forward rules to fixpoint. Returns iteration count.

    Stops early when the linear store is UNSAT: past that point every
    entailment query answers vacuously, and rules gated on entailment
    (side_gt_angle_gt, off_line_pair_same_side) would derive garbage from a
    broken state. FMTooLarge is not an unsat answer, so closure continues."""
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
        try:
            if not _fm_sat(s.la.rows):
                return -it  # store unsat: contradictory(); stop deriving
        except FMTooLarge:
            pass


def _all_points(s):
    """Point names, classified by USAGE (a name in a line or circle slot of
    any fact is not a point — e.g. the given line 'BC' in I.2, or the given
    line 'C' in I.3). The old version scanned fact[1:] of every fact and so
    admitted line names as points; closure then tied them into metric rows
    (classify/poc_onlyif) and could make the store inconsistent, after which
    every entailment holds vacuously. Regression: see euclid_search_test.py
    test_engine_regression_no_line_names_as_points."""
    pts = set()
    for fact in s.facts:
        k = fact[0]
        if k in ("on_line", "not_on_line", "on_circle", "inside", "outside",
                 "not_outside", "centre"):
            pts.add(fact[1])
        elif k in ("between",):
            pts.update(fact[1:4])
        elif k == "neq":
            pts.update(fact[1:3])
        elif k in ("dpl", "opposing_sides"):
            pts.update(fact[1:3])
        elif k in ("pos_len", "len_sum", "eq_len", "lt_len"):
            pts.update(fact[1:])
        elif k == "radii":
            pts.update(fact[2:4])
        elif k in ("not_between",):
            pts.update(fact[1:4])
        elif k in ("peq",):
            pts.update(fact[1:3])
        elif k == "same_side":
            pts.update(fact[1:3])
        elif k in ("eq_ang",):
            pts.update(fact[1:4]); pts.update(fact[4:7])
        elif k in ("sum_ang",):
            pts.update(fact[1:4]); pts.update(fact[4:7])
        elif k in ("subang",):
            pts.update(fact[1][1:])
        # intersects_cc, neq_line: circle/line names only
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
    # SystemE/Theory/Inferences/Superposition.lean:7 — exactly the axiom:
    # formTriangle a b c AB BC AC ∧ dpl d g L ∧ ¬h.onLine L ⇒ ∃ b' c' BC' AC'
    # with the full equality bundle (three angles, three lengths) plus
    # b'.onLine L, ¬between b' d g, c'.sameSide h L, dpl b' c' BC',
    # dpl d c' AC'. formTriangle is destructed into its fact conjuncts.
    "superposition": {
        "cite": "Superposition.lean:7",
        "args": ("a", "b", "c", "d", "g", "h", "AB", "BC", "AC", "L"),
        "premises": lambda s, a, b, c, d, g, h, AB, BC, AC, L: [
            ("dpl", a, b, AB), ("on_line", b, BC), ("on_line", c, BC),
            ("on_line", c, AC), ("on_line", a, AC),
            ("dpl", d, g, L), ("not_on_line", h, L)],
        "make": lambda s, a, b, c, d, g, h, AB, BC, AC, L: [
            ("b'", [("on_line", "b'", L), ("not_between", "b'", d, g)]),
            ("c'", [("same_side", "c'", h, L)]),
            ("BC'", [("dpl", "b'", "c'", "BC'")]),
            ("AC'", [("dpl", d, "c'", "AC'")]),
            (None, [("_eq_ang", b, a, c, "b'", d, "c'"),
                    ("_eq_ang", a, c, b, d, "c'", "b'"),
                    ("_eq_ang", c, b, a, "c'", "b'", d),
                    ("_eq_len", a, b, d, "b'"),
                    ("_eq_len", b, c, "b'", "c'"),
                    ("_eq_len", c, a, "c'", d)])],
    },
    # [SMT-bridge] Lean's euclid_assert b' = e in Prop04.lean, rendered as a
    # premise-checked rule: x, y on line L, ¬between x z y, and the
    # arithmetic entails |z-x| = |z-y| ⇒ x = y (same ray from z, same
    # distance ⇒ coincident).
    "assert_point_eq_on_ray": {
        "cite": "euclid_assert [SMT-bridge]",
        "args": ("x", "y", "z", "L"),
        # z must be ON L: with z off L, two points of L symmetric about the
        # foot of z are equidistant from z and distinct (review, claude-9).
        "premises": lambda s, x, y, z, L: [
            ("on_line", x, L), ("on_line", y, L), ("on_line", z, L),
            ("not_between", x, z, y)],
        "entail": lambda s, x, y, z, L: [("eq_len", (z, x, z, y))],
        "make": lambda s, x, y, z, L: [(None, [("peq", x, y)])],
    },
    # [SMT-bridge] Lean's euclid_assert c' = f in Prop04.lean: equal angle at
    # the apex, equal distance from it, same side ⇒ coincident.
    "assert_point_eq_angle": {
        "cite": "euclid_assert [SMT-bridge]",
        "args": ("x", "y", "w", "z", "L"),
        # The angle's arm z-w must lie on L and z != w, x, y: otherwise
        # "same side of L" says nothing about the side of ray z-w, and the
        # reflections of x across line zw are a counterexample (review,
        # claude-9).
        "premises": lambda s, x, y, w, z, L: [
            ("same_side", x, y, L), ("on_line", z, L), ("on_line", w, L),
            ("neq", z, w), ("neq", z, x), ("neq", z, y)],
        "entail": lambda s, x, y, w, z, L: [
            ("eq_ang", (w, z, x, w, z, y)),
            ("eq_len", (z, x, z, y))],
        "make": lambda s, x, y, w, z, L: [(None, [("peq", x, y)])],
    },
    # SystemE/Theory/Constructions/Points.lean:28
    "point_between_points_shorter_than": {
        "cite": "Points.lean:28",
        "args": ("L", "b", "c", "u", "v"),
        "premises": lambda s, L, b, c, u, v: [("dpl", b, c, L)],
        "entail": lambda s, L, b, c, u, v: [("pos_len", (u, v))],
        "make": lambda s, L, b, c, u, v: [
            ("a", [("on_line", "a", L), ("between", b, "a", c),
                   ("_lt_len", b, "a", u, v)])],
    },
    # Book/Prop03.lean:5 — cut a shorter length off a segment
    "proposition_3": {
        "cite": "Book/Prop03.lean:5",
        "args": ("a", "b", "c0", "c1", "AB", "C"),
        "premises": lambda s, a, b, c0, c1, AB, C: [
            ("dpl", a, b, AB), ("dpl", c0, c1, C)],
        "entail": lambda s, a, b, c0, c1, AB, C: [
            ("gt_len", (a, b, c0, c1))],
        "make": lambda s, a, b, c0, c1, AB, C: [
            ("q", [("between", a, "q", b), ("_eq_len", a, "q", c0, c1)])],
    },
    # Book/Prop04.lean:5 — SAS as a forward rule on existing points. The
    # formTriangle premises are destructed; the three congruence premises are
    # entailment premises; the conclusions are metric facts (no fresh
    # objects).
    "proposition_4": {
        "cite": "Book/Prop04.lean:5",
        "args": ("a", "b", "c", "d", "e", "f", "AB", "BC", "AC",
                 "DE", "EF", "DF"),
        "premises": lambda s, a, b, c, d, e, f, AB, BC, AC, DE, EF, DF: [
            ("dpl", a, b, AB), ("on_line", b, BC), ("on_line", c, BC),
            ("on_line", c, AC), ("on_line", a, AC),
            ("dpl", d, e, DE), ("on_line", e, EF), ("on_line", f, EF),
            ("on_line", f, DF), ("on_line", d, DF)],
        "entail": lambda s, a, b, c, d, e, f, AB, BC, AC, DE, EF, DF: [
            ("eq_len", (a, b, d, e)), ("eq_len", (a, c, d, f)),
            ("eq_ang", (b, a, c, e, d, f))],
        "make": lambda s, a, b, c, d, e, f, AB, BC, AC, DE, EF, DF: [
            (None, [("_eq_len", b, c, e, f),
                    ("_eq_ang", a, b, c, d, e, f),
                    ("_eq_ang", a, c, b, d, f, e)])],
    },
    # SystemE/Theory/Constructions/Points.lean:32
    "extend_point": {
        "cite": "Points.lean:32",
        "args": ("L", "b", "c"),
        "premises": lambda s, L, b, c: [("dpl", b, c, L)],
        "make": lambda s, L, b, c: [
            ("x", [("on_line", "x", L), ("between", b, c, "x")])],
    },
    # SystemE/Theory/Constructions/Points.lean:36
    "extend_point_not_on_line": {
        "cite": "Points.lean:36",
        "args": ("L", "M", "b", "c"),
        "premises": lambda s, L, M, b, c: [("dpl", b, c, L), ("neq_line", L, M)],
        "make": lambda s, L, M, b, c: [
            ("x", [("on_line", "x", L), ("between", b, c, "x"),
                   ("not_on_line", "x", M)])],
    },
    # Book/Prop05.lean (proposition_5) — isosceles base angles plus the
    # exterior-angle equality, as a forward rule. formTriangle destructed;
    # |a-b| = |a-c| is an entailment premise.
    "proposition_5": {
        "cite": "Book/Prop05.lean:5",
        "args": ("a", "b", "c", "d", "e", "AB", "BC", "AC"),
        "premises": lambda s, a, b, c, d, e, AB, BC, AC: [
            ("dpl", a, b, AB), ("on_line", b, BC), ("on_line", c, BC),
            ("on_line", c, AC), ("on_line", a, AC),
            ("between", a, b, d), ("between", a, c, e)],
        "entail": lambda s, a, b, c, d, e, AB, BC, AC: [
            ("eq_len", (a, b, a, c))],
        "make": lambda s, a, b, c, d, e, AB, BC, AC: [
            (None, [("_eq_ang", a, b, c, a, c, b),
                    ("_eq_ang", c, b, d, b, c, e)])],
    },
    # Book/Prop05.lean (proposition_5') — restriction of proposition_5.
    "proposition_5'": {
        "cite": "Book/Prop05.lean:50 (proposition_5')",
        "args": ("a", "b", "c", "AB", "BC", "AC"),
        "premises": lambda s, a, b, c, AB, BC, AC: [
            ("dpl", a, b, AB), ("on_line", b, BC), ("on_line", c, BC),
            ("on_line", c, AC), ("on_line", a, AC)],
        "entail": lambda s, a, b, c, AB, BC, AC: [
            ("eq_len", (a, b, a, c))],
        "make": lambda s, a, b, c, AB, BC, AC: [
            (None, [("_eq_ang", a, b, c, a, c, b)])],
    },
    # SystemE/Theory/Inferences/Transfer.lean:90
    "sum_angles_onlyif": {
        "cite": "Transfer.lean:90",
        "args": ("a", "b", "c", "d", "L", "M"),
        "premises": lambda s, a, b, c, d, L, M: [
            ("on_line", a, L), ("on_line", a, M),
            ("on_line", b, L), ("on_line", c, M),
            ("neq", a, b), ("neq", a, c),
            ("not_on_line", d, L), ("not_on_line", d, M),
            ("neq_line", L, M),
            ("same_side", b, d, M), ("same_side", c, d, L)],
        "make": lambda s, a, b, c, d, L, M: [
            (None, [("_sum_ang", b, a, c, d)])],
    },
}


def _entail_check(s, i, rule, q):
    kind, a = q
    try:
        if _fm_sat(s.la.rows) is False:
            # The store is already inconsistent: the branch is contradictory
            # regardless (contradictory() reports it), so no entailment
            # premise can block the step. Decided unsat only — FMTooLarge
            # still counts as undecided.
            return ("entail",) + q + ("vacuous-store",)
    except FMTooLarge:
        pass
    if kind == "eq_len":
        ok = la_entails_eq(s.la, *a)
    elif kind == "eq_ang":
        ok = la_entails_eq_ang(s.la, *a)
    elif kind == "lt_len":
        ok = la_entails_lt(s.la, *a)
    elif kind == "gt_len":
        ok = la_entails_gt(s.la, *a)
    elif kind == "pos_len":
        ok = s.la.entails({len_atom(*a): 1}, 0, ">")
    else:
        ok = False
    if not ok:
        raise MissingPremise(i, rule, ("entail", kind, a),
                             "(arithmetic does not entail it)")
    return ("entail",) + q


def apply_step(s, i, rule, args, bindings):
    """Apply one euclid_apply-style step. Returns a report dict.
    Raises MissingPremise if a premise is not in the closed state or an
    entailment premise fails the arithmetic."""
    if rule not in CONSTRUCTIONS:
        raise MissingPremise(i, rule, "(unknown rule)",
                             "(not in the engine's rule table)")
    spec = CONSTRUCTIONS[rule]
    prem_specs = spec["premises"](s, *args)
    prem_facts = [_need(s, i, rule, pf) for pf in prem_specs]
    for q in spec.get("entail", lambda s, *a: [])(s, *args):
        prem_facts.append(_entail_check(s, i, rule, q))
    made = spec["make"](s, *args)
    added = []
    for base, facts in made:
        name = bindings.get(base) if base is not None else None
        if base is not None and name is None:
            name = s.new(base)
        for fact in facts:
            fact = tuple((name if x == base else x) if base is not None
                         else x for x in fact)
            if fact[0] == "_eq_len":
                eq_len(s.la, *fact[1:])
                s.add(("eq_len", *fact[1:]),
                      f"{rule} [{spec['cite']}]", prem_facts)
                added.append(("eq_len", *fact[1:]))
            elif fact[0] == "_eq_ang":
                eq_ang(s.la, *fact[1:])
                s.add(("eq_ang", *fact[1:]),
                      f"{rule} [{spec['cite']}]", prem_facts)
                added.append(("eq_ang", *fact[1:]))
            elif fact[0] == "_lt_len":
                s.la.add({len_atom(fact[1], fact[2]): 1,
                          len_atom(fact[3], fact[4]): -1}, 0, "<")
                s.add(("lt_len", *fact[1:]),
                      f"{rule} [{spec['cite']}]", prem_facts)
                added.append(("lt_len", *fact[1:]))
            elif fact[0] == "_sum_ang":
                b, a0, c, d = fact[1:]
                s.la.add({ang_atom(b, a0, c): 1,
                          ang_atom(b, a0, d): -1,
                          ang_atom(d, a0, c): -1}, 0, "=")
                s.add(("sum_ang", *fact[1:]),
                      f"{rule} [{spec['cite']}]", prem_facts)
                added.append(("sum_ang", *fact[1:]))
            else:
                s.add(fact, f"{rule} [{spec['cite']}]", prem_facts)
                added.append(fact)
    iters = close(s)
    return {"step": i, "rule": f"{rule} [{spec['cite']}]",
            "premises": prem_facts, "added": added, "closure_iters": iters}


# ---------------------------------------------------------------------------
# Contradiction detection
# ---------------------------------------------------------------------------
# Exclusive fact pairs (a state holding both is contradictory):
#   peq x y        vs neq x y / neq y x        [logic: x = y ∧ x ≠ y]
#   on_line p L    vs not_on_line p L          [logic]
#   between a b c  vs not_between a b c        [logic]
#   same_side x y L vs opposing_sides x y L    [Relations.lean:13-14:
#       opposingSides a b L := ¬a.onLine L ∧ ¬b.onLine L ∧ ¬ a.sameSide b L]
#       (checked in both argument orders)
#   on_circle p C  vs outside p C              [Relations.lean:35:
#       outsideCircle := ¬ inside ∧ ¬ onCircle]
#   inside p C     vs outside p C              [Relations.lean:35]
# plus: the linear store is unsatisfiable. FMTooLarge counts as UNDECIDED,
# never as contradictory (a too-large store certifies nothing).


def contradictory(s):
    """close(s), then decide. Returns (bool, witness); witness is a pair of
    facts, or ("store-unsat", nrows), or None when no contradiction is found
    (including when the store is FMTooLarge: undecided ≠ contradictory)."""
    close(s)
    try:
        sat = _fm_sat(s.la.rows)
    except FMTooLarge:
        sat = None  # undecided: not certified contradictory
    if sat is False:
        return True, ("store-unsat", len(s.la.rows))
    for fact in list(s.facts):
        k = fact[0]
        if k == "peq":
            for other in (("neq", fact[1], fact[2]), ("neq", fact[2], fact[1])):
                if s.has(other):
                    return True, (fact, other)
        elif k == "on_line":
            other = ("not_on_line", fact[1], fact[2])
            if s.has(other):
                return True, (fact, other)
        elif k == "between":
            other = ("not_between", fact[1], fact[2], fact[3])
            if s.has(other):
                return True, (fact, other)
        elif k == "same_side":
            for other in (("opposing_sides", fact[1], fact[2], fact[3]),
                          ("opposing_sides", fact[2], fact[1], fact[3])):
                if s.has(other):
                    return True, (fact, other)
        elif k in ("on_circle", "inside"):
            other = ("outside", fact[1], fact[2])
            if s.has(other):
                return True, (fact, other)
    return False, None


# ---------------------------------------------------------------------------
# by_contra: assume, run steps, require a contradiction
# ---------------------------------------------------------------------------


def _assume_metric_or_fact(s, fact, why):
    """Add a fact; metric fact tuples also add their row."""
    if fact[0] == "eq_len":
        eq_len(s.la, *fact[1:])
    elif fact[0] == "lt_len":
        s.la.add({len_atom(fact[1], fact[2]): 1,
                  len_atom(fact[3], fact[4]): -1}, 0, "<")
    elif fact[0] == "gt_len":
        s.la.add({len_atom(fact[1], fact[2]): 1,
                  len_atom(fact[3], fact[4]): -1}, 0, ">")
    elif fact[0] == "eq_ang":
        eq_ang(s.la, *fact[1:])
    s.assume(fact, why)


class ByContraFailure(Exception):
    def __init__(self, entry, branch, witness):
        super().__init__(
            f"by_contra branch {branch} did not reach a contradiction "
            f"(witness of non-contradiction: {witness})")
        self.entry, self.branch, self.witness = entry, branch, witness


def _by_contra(s, i, entry):
    """entry = ("by_contra", branches, conclusions).
    branches: list of (assumption-facts, steps). Each branch runs on a copy
    of the current state plus its assumptions and MUST end contradictory;
    classical exhaustiveness of the branch assumptions is the caller's
    responsibility (documented at each use site).
    conclusions: facts added to the OUTER state when every branch
    contradicts (metric fact tuples add their row too)."""
    _, branches, conclusions = entry
    witnesses = []
    for facts, steps in branches:
        sub = s.copy()
        for f in facts:
            _assume_metric_or_fact(sub, f, "by_contra assumption")
        try:
            run_steps(sub, steps, i + 1)
        except MissingPremise as mp:
            # A stuck branch cannot claim a contradiction: report it as a
            # failed by_contra, naming the missing premise.
            raise ByContraFailure(entry, facts, ("stuck", mp.premise)) from mp
        ok, w = contradictory(sub)
        if not ok:
            raise ByContraFailure(entry, facts, w)
        witnesses.append(w)
    for c in conclusions:
        if c == ("false",):
            s.add(("false",), "by_contra", witnesses)
            continue
        _assume_metric_or_fact(s, c, "by_contra")
        s.facts[c] = ("by_contra", witnesses)
    return {"step": i, "rule": "by_contra",
            "branches": len(branches), "witnesses": witnesses,
            "conclusions": conclusions}


def run_steps(s, steps, i0=1):
    """Driver loop shared by replay() and by_contra branches. Steps are
    (rule, args, bindings) or ("by_contra", branches, conclusions)."""
    rep = []
    for i, st in enumerate(steps, i0):
        if st[0] == "by_contra":
            rep.append(_by_contra(s, i, st))
        else:
            rep.append(apply_step(s, i, st[0], st[1], st[2]))
    return rep


def replay(hyps, steps, verify):
    """Driver. hyps: list of (fact, why) hypotheses (already destructed, as
    euclid_intros would leave them); steps: list of (rule, args, bindings);
    verify: list of facts/queries the conclusion must satisfy (each is either a
    diagram fact tuple or ("_eq_len", x, y, u, v) metric query).
    Returns (state, report). Raises MissingPremise on the first stuck step,
    AssertionError if a final query fails."""
    s = State()
    for fact, why in hyps:
        _assume_metric_or_fact(s, fact, why)  # metric hyps also add their row
    close(s)
    report = []
    t0 = time.time()
    report = run_steps(s, steps)
    for q in verify:
        if q == ("_contradictory",):
            ok, w = contradictory(s)
            assert ok, f"final state not contradictory: {w}"
        elif q[0] == "_eq_len":
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

# Book I.4 and I.5 as data (hypotheses destructed; SMT-sourced side/off-line
# facts are stated as explicit inputs — see euclid_engine_test.py docstrings)
PROP4_HYPS_DATA = [
    (("dpl", "a", "b", "AB"), "h"),
    (("on_line", "b", "BC"), "h"), (("on_line", "c", "BC"), "h"),
    (("on_line", "c", "AC"), "h"), (("on_line", "a", "AC"), "h"),
    (("dpl", "d", "e", "DE"), "h"),
    (("on_line", "e", "EF"), "h"), (("on_line", "f", "EF"), "h"),
    (("on_line", "f", "DF"), "h"), (("on_line", "d", "DF"), "h"),
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
PROP5_HYPS_DATA = [
    (("dpl", "a", "b", "AB"), "h"),
    (("on_line", "b", "BC"), "h"), (("on_line", "c", "BC"), "h"),
    (("on_line", "c", "AC"), "h"), (("on_line", "a", "AC"), "h"),
    (("not_on_line", "c", "AB"), "h"),
    (("not_on_line", "b", "AC"), "h"),
    (("not_on_line", "c", "GB"), "h"),
    (("not_on_line", "b", "FC"), "h"),
    (("neq_line", "AB", "AC"), "h"), (("neq_line", "AB", "GB"), "h"),
    (("neq_line", "AC", "FC"), "h"),
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


# Book I.6 (Book/Prop06.lean): by_contra on |ab| ≠ |ac|; the by_cases split
# |ab|>|ac| / |ac|>|ab| is exhaustive by trichotomy of the linear store.
# Each branch: proposition_3 cuts the shorter length, line_from_points joins,
# proposition_4 (SAS) congruence; contradiction via side_gt_angle_gt (I.18)
# against the hypothesis ∠a:b:c = ∠a:c:b.
PROP6_HYPS_DATA = [
    (("dpl", "a", "b", "AB"), "h"),
    (("on_line", "b", "BC"), "h"), (("on_line", "c", "BC"), "h"),
    (("on_line", "c", "AC"), "h"), (("on_line", "a", "AC"), "h"),
    (("neq", "b", "c"), "h"),  # formTriangle non-degeneracy
    (("neq", "a", "c"), "h"),  # formTriangle non-degeneracy
    (("not_on_line", "c", "AB"), "h"),  # formTriangle: c not on AB
    (("eq_ang", "a", "b", "c", "a", "c", "b"), "h"),
]
PROP6_STEPS_DATA = [
    ("by_contra", [
        ([("gt_len", "a", "b", "a", "c")], [
            ("proposition_3", ("b", "a", "a", "c", "AB", "AC"), {"q": "d"}),
            ("line_from_points", ("d", "c"), {"L": "DC"}),
            ("proposition_4", ("b", "d", "c", "c", "a", "b",
                               "AB", "DC", "BC", "AC", "AB", "BC"), {}),
        ]),
        ([("gt_len", "a", "c", "a", "b")], [
            ("proposition_3", ("c", "a", "a", "b", "AC", "AB"), {"q": "d"}),
            ("line_from_points", ("d", "b"), {"L": "DB"}),
            ("proposition_4", ("c", "d", "b", "b", "a", "c",
                               "AC", "DB", "BC", "AB", "AC", "BC"), {}),
        ]),
    ], [("eq_len", "a", "b", "a", "c")]),
]

# Book I.7 (Book/Prop07.lean): conclusion False. After CD is drawn, the two
# nested by_cases (a vs CD, d vs AC) give four branches; a,b are off CD and
# b,d off AC (derived in-state), so sameSide vs opposingSides is exhaustive
# (Relations.lean:13-14). Side facts the branch steps need beyond the case
# fact are carried as explicit branch assumptions (see comments in the test).
PROP7_HYPS_DATA = [
    (("dpl", "a", "b", "AB"), "h"),
    (("dpl", "a", "c", "AC"), "h"),
    (("dpl", "c", "b", "CB"), "h"),
    (("dpl", "a", "d", "AD"), "h"),
    (("dpl", "d", "b", "DB"), "h"),
    (("same_side", "c", "d", "AB"), "h"),
    (("neq", "c", "d"), "h"),
    (("eq_len", "a", "c", "a", "d"), "h"),
    (("eq_len", "c", "b", "d", "b"), "h"),
]


def _prop7_branch(case_a, case_d, extra, steps):
    return ([("same_side" if case_a else "opposing_sides", "a", "b", "CD"),
             ("same_side" if case_d else "opposing_sides", "d", "b", "AC")]
            + extra, steps)


PROP7_STEPS_DATA = [
    ("line_from_points", ("c", "d"), {"L": "CD"}),
    ("by_contra", [
        # branch 1: a sameSide b CD, d sameSide b AC
        _prop7_branch(True, True,
                      [("same_side", "c", "a", "DB")], [
            ("proposition_5'", ("a", "c", "d", "AC", "CD", "AD"), {}),
            ("sum_angles_onlyif", ("d", "c", "b", "a", "CD", "DB"), {}),
            ("proposition_5'", ("b", "c", "d", "CB", "CD", "DB"), {}),
            ("sum_angles_onlyif", ("c", "a", "d", "b", "AC", "CD"), {}),
        ]),
        # branch 2: a sameSide b CD, d opposingSides b AC
        _prop7_branch(True, False,
                      [("same_side", "c", "a", "DB"),
                       ("same_side", "d", "a", "CB"),
                       ("same_side", "c", "b", "AD")], [
            ("proposition_5'", ("a", "d", "c", "AD", "CD", "AC"), {}),
            ("sum_angles_onlyif", ("c", "d", "b", "a", "CD", "CB"), {}),
            ("proposition_5'", ("b", "d", "c", "DB", "CD", "CB"), {}),
            ("sum_angles_onlyif", ("d", "a", "c", "b", "AD", "CD"), {}),
        ]),
        # branch 3: a opposingSides b CD, d sameSide b AC
        _prop7_branch(False, True,
                      [("same_side", "c", "a", "DB"),
                       ("same_side", "e", "b", "CD"),
                       ("same_side", "c", "f", "DB"),
                       ("same_side", "b", "f", "CD")], [
            ("extend_point_not_on_line", ("AC", "CD", "a", "c"), {"x": "e"}),
            ("extend_point_not_on_line", ("AD", "CD", "a", "d"), {"x": "f"}),
            ("proposition_5", ("a", "c", "d", "e", "f", "AC", "CD", "AD"), {}),
            ("sum_angles_onlyif", ("c", "e", "d", "b", "AC", "CD"), {}),
            ("proposition_5'", ("b", "c", "d", "CB", "CD", "DB"), {}),
            ("sum_angles_onlyif", ("d", "c", "b", "f", "CD", "DB"), {}),
        ]),
        # branch 4: a opposingSides b CD, d opposingSides b AC
        _prop7_branch(False, False,
                      [("same_side", "c", "a", "DB"),
                       ("same_side", "f", "b", "CD"),
                       ("same_side", "c", "b", "AD"),
                       ("same_side", "d", "e", "CB"),
                       ("same_side", "b", "e", "CD")], [
            ("extend_point_not_on_line", ("AC", "CD", "a", "c"), {"x": "e"}),
            ("extend_point_not_on_line", ("AD", "CD", "a", "d"), {"x": "f"}),
            ("proposition_5", ("a", "c", "d", "e", "f", "AC", "CD", "AD"), {}),
            ("sum_angles_onlyif", ("d", "f", "c", "b", "AD", "CD"), {}),
            ("proposition_5'", ("b", "d", "c", "DB", "CD", "CB"), {}),
            ("sum_angles_onlyif", ("c", "d", "b", "e", "CD", "CB"), {}),
        ]),
    ], [("false",)]),
]


def _metric_state(hyps):
    s = State()
    for f, w in hyps:
        if f[0] == "eq_len":
            eq_len(s.la, *f[1:])
        elif f[0] == "eq_ang":
            eq_ang(s.la, *f[1:])
        s.assume(f, w)
    close(s)
    return s


def _prop4_data_state():
    return _metric_state(PROP4_HYPS_DATA)


def _prop5_data_state():
    return _metric_state(PROP5_HYPS_DATA)


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
        elif p == "4":
            t0 = time.time()
            s = _prop4_data_state()
            for i, (r, a, b) in enumerate(PROP4_STEPS_DATA, 1):
                apply_step(s, i, r, a, b)
            assert la_entails_eq(s.la, "b", "c", "e", "f"), "I.4 length"
            assert la_entails_eq_ang(s.la, "a", "b", "c", "d", "e", "f")
            assert la_entails_eq_ang(s.la, "a", "c", "b", "d", "f", "e")
            rep = [{"final": True, "facts": len(s.facts),
                    "constraints": s.la.size(),
                    "seconds": round(time.time() - t0, 4)}]
        elif p == "5":
            t0 = time.time()
            s = _prop5_data_state()
            for i, (r, a, b) in enumerate(PROP5_STEPS_DATA, 1):
                apply_step(s, i, r, a, b)
            assert la_entails_eq_ang(s.la, "a", "b", "c", "a", "c", "b")
            assert la_entails_eq_ang(s.la, "c", "b", "d", "b", "c", "e")
            rep = [{"final": True, "facts": len(s.facts),
                    "constraints": s.la.size(),
                    "seconds": round(time.time() - t0, 4)}]
        elif p == "6":
            t0 = time.time()
            s, rep = replay(PROP6_HYPS_DATA, PROP6_STEPS_DATA,
                            [("_eq_len", "a", "b", "a", "c")])
            rep[-1]["seconds"] = round(time.time() - t0, 4)
        elif p == "7":
            t0 = time.time()
            s, rep = replay(PROP7_HYPS_DATA, PROP7_STEPS_DATA,
                            [("false",)])
            rep[-1]["seconds"] = round(time.time() - t0, 4)
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



