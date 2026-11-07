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
        if len(new) > 400:  # safety valve; I.1-I.3 never approaches this
            return True
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


def r_dpl_parts(s):
    # Relations.lean:6 (definition) — dpl p q L gives on_line p L, on_line q L.
    for fact in s.facts:
        if fact[0] == "dpl":
            yield ("on_line", fact[1], fact[3]), [fact]
            yield ("on_line", fact[2], fact[3]), [fact]


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
        "premises": lambda s, x, y, z, L: [
            ("on_line", x, L), ("on_line", y, L),
            ("not_between", x, z, y)],
        "entail": lambda s, x, y, z, L: [("eq_len", (z, x, z, y))],
        "make": lambda s, x, y, z, L: [(None, [("peq", x, y)])],
    },
    # [SMT-bridge] Lean's euclid_assert c' = f in Prop04.lean: equal angle at
    # the apex, equal distance from it, same side ⇒ coincident.
    "assert_point_eq_angle": {
        "cite": "euclid_assert [SMT-bridge]",
        "args": ("x", "y", "w", "z", "L"),
        "premises": lambda s, x, y, w, z, L: [("same_side", x, y, L)],
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



