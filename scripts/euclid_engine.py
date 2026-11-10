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
    la.add(lin((ang_atom(a, b, c), 1), (ang_atom(d, e, f), -1)), 0, "=")


def la_entails_eq_ang(la, a, b, c, d, e, f):
    if ang_atom(a, b, c) == ang_atom(d, e, f):
        return True  # same atom: a dict literal would collapse the terms
    return la.entails(lin((ang_atom(a, b, c), 1), (ang_atom(d, e, f), -1)), 0, "=")


def la_entails_gt_ang(la, a, b, c, d, e, f):
    """Strict angle inequality ∠a:b:c > ∠d:e:f as an LA entailment query."""
    if ang_atom(a, b, c) == ang_atom(d, e, f):
        return False
    return la.entails(lin((ang_atom(a, b, c), 1), (ang_atom(d, e, f), -1)), 0, ">")


def la_entails_lt_sum_rt_ang(la, u, v, w, x, y, z):
    """∠u:v:w + ∠x:y:z < 2∟ (Book I.17's goal shape) as an LA query."""
    return la.entails(lin((ang_atom(u, v, w), 1), (ang_atom(x, y, z), 1),
                          (RT, -2)), 0, "<")


def len_atom(a, b):
    """Canonical atom for |(a-b)|. Sorting the pair applies segment_symmetric
    (SystemE/Theory/Inferences/Metric.lean:41, |a-b| = |b-a|) as a rewrite."""
    return ("len",) + tuple(sorted((a, b)))


def norm(coeffs, const):
    """Normalize a linear form to a tuple of (sorted) (atom, coeff) pairs + const."""
    n = len(coeffs)
    if n > 2:
        items = tuple(sorted((a, c) for a, c in coeffs.items() if c != 0))
    elif n == 2:
        # fast paths (the overwhelmingly common FM cases): 0/1 items need no
        # sort at all, 2 items need only a comparison — sorted() here was
        # 7.7M calls in the search profile
        it = iter(coeffs.items())
        (a1, c1), (a2, c2) = next(it), next(it)
        if c1 == 0 and c2 == 0:
            items = ()
        elif c1 == 0:
            items = ((a2, c2),)
        elif c2 == 0 or a1 < a2:
            items = ((a1, c1), (a2, c2)) if c2 != 0 else ((a1, c1),)
        else:
            items = ((a2, c2), (a1, c1))
    elif n == 1:
        items = tuple((a, c) for a, c in coeffs.items() if c != 0)
    else:
        items = ()
    return items, const


def lin(*pairs):
    """Coefficient dict from (atom, coeff) pairs, ACCUMULATING repeated
    atoms. Review fix (claude-9): rows were written as dict literals
    {X: 1, Y: -1}; when X and Y canonicalise to the same atom (|bc| = |cb|,
    from proposition_4 f b c g c b in I.5) the literal keeps only the last
    value, giving |bc| = 0, and the store went unsat. Every multi-atom row
    is built with lin()."""
    d = {}
    for a, c in pairs:
        d[a] = d.get(a, 0) + c
    return d


_ENT_GLOBAL = {}


class LinearArithmetic:
    """Constraint store: rows (coeffs, const, op) with op in {=, <=, <}."""

    def __init__(self):
        self.rows = []  # each: (norm-items, const, op)
        # entailment memo, keyed (row count, query). Sound per store because
        # rows are append-only, so within one store's lifetime a given row
        # count always names the same row list. Copies/snapshots get their
        # own fresh dict: their rows diverge from the parent's from then on.
        self._ent = {}

    def add(self, coeffs, const, op):
        if op == ">":
            coeffs, const, op = {k: -v for k, v in coeffs.items()}, -const, "<"
        elif op == ">=":
            coeffs, const, op = {k: -v for k, v in coeffs.items()}, -const, "<="
        # rows are stored as: sum(coeff*atom) + const ~ 0 (consts stay int
        # when they are ints: int arithmetic is exact and cheaper than
        # Fraction, and Fraction(n) == n for hashing)
        items, c = norm(coeffs, -const if type(const) is int
                        else -Fraction(const))
        if not items:
            ok = (c == 0) if op == "=" else ((c <= 0) if op == "<=" else (c < 0))
            if not ok:
                # a false constant row (e.g. |x| < |x| after lin() cancels)
                # is a contradiction: record it so _fm_sat sees it, rather
                # than returning False to callers that ignore the value
                row = ((), c, op)
                if row not in self.rows:
                    self.rows.append(row)
            return ok
        row = (items, c, op)
        if row not in self.rows:
            self.rows.append(row)
        return True

    def entails(self, coeffs, const, op):
        """Is `sum coeffs*atom ~ const` entailed? Decided by checking
        satisfiability of the store plus the negation (Fourier-Motzkin).
        Memoised per store on (row count, query); rows only grow. A second,
        global memo keyed on the full row content catches the repeats that
        cross state boundaries (search clones share row prefixes)."""
        if op in (">", ">="):
            coeffs, const, op = ({k: -v for k, v in coeffs.items()}, -const,
                                 "<" if op == ">" else "<=")
        q = (tuple(sorted((a, c) for a, c in coeffs.items() if c != 0)),
             const, op)
        if getattr(self, "_ent_rows", None) is not self.rows:
            # review fix (claude-9): the (row count, query) key assumes this
            # rows list only grows; if it was REPLACED (l.rows = other), a
            # same-count key would answer for the old rows
            self._ent = {}
            self._ent_rows = self.rows
        key = (len(self.rows), q)
        hit = self._ent.get(key)
        if hit is not None:
            return hit
        gkey = (tuple(self.rows), q)
        hit = _ENT_GLOBAL.get(gkey)
        if hit is not None:
            self._ent[key] = hit
            return hit
        # direct single-row witness: if the store contains this very row the
        # entailment holds trivially (an O(R) scan beats a Fourier-Motzkin
        # run; r_classify's radii comparisons hit this constantly)
        items, c = norm(coeffs, -Fraction(const))
        if (items, c, op) in self.rows or \
                (op == "<=" and (items, c, "=") in self.rows):
            self._ent[key] = True
            _ENT_GLOBAL[gkey] = True
            return True
        r = self._entails_run(coeffs, const, op)
        self._ent[key] = r
        if len(_ENT_GLOBAL) > 500000:
            _ENT_GLOBAL.clear()
        _ENT_GLOBAL[gkey] = r
        return r

    def _entails_run(self, coeffs, const, op):
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
    items, c = norm(coeffs, -const if type(const) is int
                    else -Fraction(const))
    return (items, c, op)


def _check_const(c, op):
    if op == "=":
        return c == 0
    if op == "<=":
        return c <= 0
    return c < 0


class FMTooLarge(Exception):
    """Fourier-Motzkin elimination grew past its row limit: undecided."""


def _exdiv(a, b):
    """Exact division that stays int when it can (int/int `/` would be a
    float, and float cancellation once manufactured a false unsat)."""
    if type(a) is int and type(b) is int and a % b == 0:
        return a // b
    return a / b


_FM_CONV_CACHE = {}


def _fm_sat(rows):
    """Fourier-Motzkin satisfiability over rational linear constraints.
    All arithmetic is exact: coefficients and constants are coerced to
    Fraction on entry (int/int true division elsewhere in this function
    once produced floats, and float cancellation manufactured rows like
    x = 0 that contradicted x > 0 — a false 'unsat' that made every
    entailment hold vacuously; regression: euclid_search_test.py)."""
    conv = _FM_CONV_CACHE
    out_rows = []
    for row in rows:
        r2 = conv.get(row)
        if r2 is None:
            items, c, op = row
            # exactness: ints stay ints (int arithmetic is exact and 3x
            # faster than Fraction); anything else must become a Fraction
            r2 = (tuple((at, co if type(co) in (int, Fraction) else Fraction(co))
                        for at, co in items),
                  c if type(c) in (int, Fraction) else Fraction(c), op)
            conv[row] = r2
        out_rows.append(r2)
    rows = out_rows
    # equalities: eliminate by substitution. An atom index sends each
    # substitution only to the rows that mention the variable (the old
    # rescan-from-zero pass visited every row per equality: most of the
    # I.5 replay's runtime).
    rows = _elim_equalities(rows)
    if rows is None:
        return False
    # inequalities: eliminate variables one at a time. The pivot is the
    # variable with the SMALLEST pos*neg product (fewest new rows), not
    # the sorted-first: FM's result is order-independent, but the row
    # count is not — on I.22's chain store the sorted order eliminated
    # ('len','e'','e''') with 22 pos x 41 neg rows (then 572 x 145) and
    # blew past the guard on a query whose store is 31 rows x 7 atoms
    # after equality substitution (E-kimi-task-395).
    while True:
        counts = {}
        for items, _, _ in rows:
            for a, co in items:
                p, n = counts.get(a, (0, 0))
                counts[a] = (p + (co > 0), n + (co < 0))
        if not counts:
            return all(_check_const(c, op) for _, c, op in rows)
        x = min(counts, key=lambda a: (counts[a][0] * counts[a][1], a))
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
                    co[at] = co.get(at, 0) + _exdiv(v, a1)
                for at, v in i2:
                    co[at] = co.get(at, 0) + _exdiv(v, a2)
                strict = op1 == "<" or op2 == "<"
                new.append(_row_of(co, -(_exdiv(c1, a1) + _exdiv(c2, a2)),
                                   "<" if strict else "<="))
        if len(new) > 400:
            # Too large to decide. Raising (rather than answering "sat")
            # keeps every caller honest: entailment treats it as "not
            # entailed", and a consistency check must treat it as "cannot
            # certify" (review, claude-9).
            raise FMTooLarge(len(new))
        rows = new


def _subst_one(row, var, sub_co, sub_c):
    """Substitute var in one row; None = unsat constant, () = drop row,
    else the substituted row."""
    items, c, op = row
    d = dict(items)
    v = d.pop(var)
    for at, w in sub_co.items():
        nw = d.get(at, 0) + v * w
        if nw:
            d[at] = nw
        else:
            d.pop(at, None)
    c = c + v * sub_c
    items2, c2 = norm(d, c)
    if items2:
        return (items2, c2, op)
    return None if not _check_const(c2, op) else ()


def _elim_equalities(rows):
    """Eliminate all equality rows by substitution, choosing each equality's
    first (sorted) atom as its variable — same choice the old rescan loop
    made. Returns the surviving inequality rows, or None for unsat."""
    table = {}
    idx = {}
    nid = 0
    if not any(op == "=" for _, _, op in rows):
        return list(rows)  # nothing to eliminate: skip the index build
    for r in rows:
        table[nid] = r
        for at, _ in r[0]:
            idx.setdefault(at, set()).add(nid)
        nid += 1
    queue = [i for i, r in table.items() if r[2] == "="]
    qi = 0
    while qi < len(queue):
        i = queue[qi]
        qi += 1
        row = table.pop(i, None)
        if row is None:
            continue
        for at, _ in row[0]:
            idx[at].discard(i)
        items, c, _op = row
        co = dict(items)
        var = items[0][0]
        div = co.pop(var)
        if div != 1:
            co = {k: _exdiv(v, div) for k, v in co.items()}
            c = _exdiv(c, div)
        # var + sum(co) + c = 0  =>  var = -c - sum(co)
        sub_co = {k: -v for k, v in co.items()}
        sub_c = -c
        for j in list(idx.get(var, ())):
            r = table.pop(j)
            for at, _ in r[0]:
                idx[at].discard(j)
            nr = _subst_one(r, var, sub_co, sub_c)
            if nr is None:
                return None
            if nr == ():
                continue
            table[nid] = nr
            for at, _ in nr[0]:
                idx.setdefault(at, set()).add(nid)
            if nr[2] == "=":
                queue.append(nid)
            nid += 1
    return list(table.values())


def _subst_rows(rows, var, sub_co, sub_c):
    """Substitute var = sum(sub_co) + sub_c in all rows. sub_co: dict.
    A row v*var + S + c ~ 0 becomes v*(sub_co) + S + (c + v*sub_c) ~ 0.
    (The old code wrote c - v*sub_c — a sign error that only bites once a
    previous substitution makes sub_c nonzero, i.e. exactly in multi-step
    substitution chains; replay constants were all 0, so it hid until the
    search layered circle equalities. Regression: euclid_search_test.py
    test_engine_regression_no_line_names_as_points.)
    Rows not mentioning var pass through untouched (they are already in
    canonical norm form — re-normalising them was 4.4M sorted() calls in
    the I.5 replay)."""
    out = []
    for row in rows:
        items, c, op = row
        d = dict(items)
        if var not in d:
            out.append(row)
            continue
        v = d.pop(var)
        for at, w in sub_co.items():
            nw = d.get(at, 0) + v * w
            if nw:
                d[at] = nw
            else:
                d.pop(at, None)
        c = c + v * sub_c
        items2, c2 = norm(d, c)
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
#   ("not_same_side", p, q, L) — the literal negation of sameSide (Book
#       I.24's second by_cases branch, Prop24.lean:40). The engine's usual
#       rendering of ¬sameSide is the POSITIVE fact opposing_sides
#       (Relations.lean:13-14: opposingSides := both off the line ∧
#       ¬sameSide), used wherever both points are known off the line (hgf
#       in I.24a, pasch_4, ...). In I.24's second branch g is NOT known
#       off EF — that is exactly the nested case split of lines 41-42 — so
#       the negation is kept literal.
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
        # Object-name discipline (P-euclid-binding-clash, claude-9). A name
        # either DECLARED (names an object that exists: introduced by an
        # assume with ordinary hypothesis provenance, or created by a step)
        # or RESERVED (mentioned only by non-hypothesis stated inputs,
        # which may forward-reference objects constructed later: "h-sm"
        # SMT sources like I.5's FC/GB and I.9/I.10's witness/apex points,
        # I.7's by_contra branch facts about e/f, student-diagram trust).
        # A reserved name is NOT an object yet: binding a fresh object to
        # it is allowed exactly once (the binding consumes the reservation
        # and declares the name); a second binding, or a binding to a
        # DECLARED name, is refused by apply_step. "Mentioned in a fact"
        # is deliberately NOT "declared": only hypothesis-provenance
        # assumptions declare, and derived facts never do.
        self.declared = set()  # names of objects that exist
        self.reserved = set()  # names awaiting their one later binding
        # Forward-rule exclusion set (rule NAMES as in FORWARD_RULES):
        # close() skips these. Empty by
        # default; replay(..., exclude_rules=...) sets it, and copies made
        # by by_contra/by_cases inherit it.
        self.exclude_rules = frozenset()

    def assume(self, fact, why="hypothesis"):
        names = {x for x in fact[1:] if isinstance(x, str)}
        if why in ("h", "hypothesis", "given"):
            # A genuine hypothesis asserts facts about objects that exist:
            # every name it mentions is declared.
            self.declared |= names
            self.reserved -= names
        else:
            # Any other stated input ("h-sm" SMT source, a by_contra branch
            # assumption, a student-diagram trust) may forward-reference an
            # object constructed later (I.5's FC/GB, I.7 branch 3's e/f):
            # names it alone introduces are reserved, not declared.
            self.reserved |= names - self.declared - self.reserved
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
        s2.declared = set(self.declared)
        s2.reserved = set(self.reserved)
        s2.la.rows = list(self.la.rows)
        s2.rule_cache = dict(getattr(self, "rule_cache", {}))
        s2.exclude_rules = self.exclude_rules
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
    la.add(lin((len_atom(x, y), 1), (len_atom(u, v), -1)), 0, "=")


def la_entails_eq(la, x, y, u, v):
    if len_atom(x, y) == len_atom(u, v):
        return True  # same atom: a dict literal would collapse the terms
    return la.entails(lin((len_atom(x, y), 1), (len_atom(u, v), -1)), 0, "=")


def la_entails_lt(la, x, y, u, v):
    if len_atom(x, y) == len_atom(u, v):
        return False
    return la.entails(lin((len_atom(x, y), 1), (len_atom(u, v), -1)), 0, "<")


def la_entails_gt(la, x, y, u, v):
    if len_atom(x, y) == len_atom(u, v):
        return False
    return la.entails(lin((len_atom(x, y), 1), (len_atom(u, v), -1)), 0, ">")


def la_entails_gt_sum_len(la, x, y, u, v, p, q):
    """|x─y| + |u─v| > |p─q| (Book I.20's goal shape) as an LA query."""
    return la.entails(lin((len_atom(x, y), 1), (len_atom(u, v), 1),
                          (len_atom(p, q), -1)), 0, ">")


def la_entails_lt_sum_sum_len(la, x, y, u, v, p, q, r, s2):
    """|x─y| + |u─v| < |p─q| + |r─s| (Book I.21's first conjunct shape)
    as an LA query."""
    return la.entails(lin((len_atom(x, y), 1), (len_atom(u, v), 1),
                          (len_atom(p, q), -1), (len_atom(r, s2), -1)),
                      0, "<")


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


def r_icl2(s):
    # Diagrammatic.lean:305 (intersection_circle_line_2) — a insideCircle α,
    # a onLine L  ⇒  L intersectsCircle α. Lean's SMT applies it silently;
    # no Book replay here needed it, but a student does (Joe at I.2,
    # 2026-11-09: the line BC through the centre b of D, refused "line BC
    # meets circle D").
    for f1 in s.facts:
        if f1[0] != "inside":
            continue
        a, alpha = f1[1], f1[2]
        for f2 in s.facts:
            if f2[0] == "on_line" and f2[1] == a and \
                    not s.has(("intersects_lc", f2[2], alpha)):
                yield (("intersects_lc", f2[2], alpha), [f1, f2])


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
                s.la.add(lin((len_atom(a, b), 1), (len_atom(b, c), 1),
                             (len_atom(a, c), -1)), 0, "=")
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
                s.la.add(lin((len_atom(o, p), 1), (len_atom(o, q), -1)), 0, "=")
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
        # Entailments over atoms absent from every row can only be vacuous
        # (an unsat store entails everything); the store-unsat case is what
        # close() early-stops for, so skip those points (three Fourier-Motzkin
        # runs per unrelated point was most of the search's closure cost).
        live = {at for items, _, _ in s.la.rows for at, _ in items}
        for p in _all_points(s):
            if any(s.has((k, p, alpha)) for k in ("on_circle", "inside", "outside")):
                continue
            if len_atom(o, p) not in live or len_atom(o, w) not in live:
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
    live = {at for items, _, _ in la.rows for at, _ in items}
    for a in _all_points(s):
        for b in _all_points(s):
            if a == b or s.has(("neq", a, b)) or s.has(("neq", b, a)):
                continue
            if a > b:
                continue
            if len_atom(a, b) not in live:
                continue  # no row mentions |a-b|: only vacuous (store-unsat)
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
                s.la.add(lin((ang_atom(z, v, x), 1), (ang_atom(z, v, y), -1)),
                         0, "=")
                yield marker, [fact]


def r_angle_same_ray(s):
    # [SMT-bridge] between a v x and between a v y (x, y both beyond v,
    # away from a) ⇒ ∠(z,v,x) = ∠(z,v,y): the rays v→x and v→y coincide
    # whether or not x = y. Lean's SMT closes it silently; kimi-1's
    # playtest (2026-11-10) stalled at I.5 needing ∠bcg = ∠bce with g, e
    # both beyond c and g ≠ e not known. Only for angles the store already
    # mentions (an atom of either side present), so the store does not
    # grow by every point pair. Marker-guarded.
    atoms = {a for row in s.la.rows for a, _k in row[0]}
    beyond = {}
    for fact in s.facts:
        if fact[0] == "between":
            a, v, x = fact[1], fact[2], fact[3]
            beyond.setdefault((a, v), set()).add(x)
    pts = _all_points(s)
    lines_of = {}
    for fact in s.facts:
        if fact[0] == "on_line":
            lines_of.setdefault(fact[1], set()).add(fact[2])
    for (a, v), xs in beyond.items():
        xs = sorted(xs)
        common = lines_of.get(a, set()) & lines_of.get(v, set())
        for i, x in enumerate(xs):
            for y in xs[i + 1:]:
                for z in pts:
                    if z in (a, v, x, y):
                        continue
                    if lines_of.get(z, set()) & common:
                        continue   # z on the ray's own line: degenerate
                    ax, ay = ang_atom(z, v, x), ang_atom(z, v, y)
                    if ax not in atoms and ay not in atoms:
                        continue
                    marker = ("sameray", v, x, y, z)
                    if not s.has(marker):
                        s.la.add(lin((ax, 1), (ay, -1)), 0, "=")
                        yield marker, [("between", a, v, x),
                                       ("between", a, v, y)]


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
        # both substitution directions: linking rows are needed whichever of
        # the two names the existing atoms happen to mention (I.8: peq d c'
        # must rewrite ∠e:c':b' to ∠e:d:b')
        for src, dst in ((x, y), (y, x)):
            for a in atoms:
                if a[0] == "len" and src in (a[1], a[2]):
                    other = tuple(dst if t == src else t for t in a[1:3])
                    a2 = ("len",) + tuple(sorted(other))
                    marker = ("sublen", a, src, dst)
                    if not s.has(marker):
                        s.la.add(lin((a, 1), (a2, -1)), 0, "=")
                        yield marker, [fact]
                elif a[0] == "ang" and src in a[1:]:
                    legs, vert = (a[1], a[3]), a[2]
                    sub = tuple(dst if t == src else t
                                for t in (legs[0], vert, legs[1]))
                    a2 = ang_atom(*sub)
                    marker = ("subang", a, src, dst)
                    if not s.has(marker):
                        s.la.add(lin((a, 1), (a2, -1)), 0, "=")
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
    # ∧ ¬ a.sameSide b L. All three parts are yielded, and the pair itself
    # yields a ≠ b (a point is never on the opposing side of itself).
    for fact in s.facts:
        if fact[0] == "opposing_sides":
            x, y, L = fact[1], fact[2], fact[3]
            yield ("not_on_line", x, L), [fact]
            yield ("not_on_line", y, L), [fact]
            yield ("neq", x, y), [fact]


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


def r_peq_subst_diagram(s):
    # [logic] point equality x = y substitutes into diagrammatic facts:
    # on_line / not_on_line / dpl (either point slot). Mirrors r_peq_subst,
    # which substitutes into length/angle atoms. Needed when a proposition
    # rule's premise names a point that an assert has since identified with
    # another (I.8: superposition's dpl b' c' BC' vs proposition_7's
    # dpl d b DB after b' = f).
    peqs = [f for f in s.facts if f[0] == "peq"]
    if not peqs:
        return
    for f in s.facts:
        k = f[0]
        # "inside" joins for I.22: the by_cases equality branch identifies
        # i with f (the centre of DKL), so inside f DKL (r_center_inside)
        # must rewrite to inside i DKL for the branch conclusion.
        if k in ("on_line", "not_on_line", "inside"):
            x, L = f[1], f[2]
            for pf in peqs:
                if pf[1] == x and not s.has((k, pf[2], L)):
                    yield ((k, pf[2], L), [f, pf])
                if pf[2] == x and not s.has((k, pf[1], L)):
                    yield ((k, pf[1], L), [f, pf])
        elif k == "dpl":
            for pf in peqs:
                for old, new in ((pf[1], pf[2]), (pf[2], pf[1])):
                    for pos in (1, 2):
                        if f[pos] != old:
                            continue
                        t = tuple(new if j == pos else v
                                  for j, v in enumerate(f))
                        if t not in s.facts:
                            yield (t, [f, pf])


FORWARD_RULES = [
    ("center_inside_circle [Diagrammatic.lean:28]", r_center_inside),
    ("not_outside [Relations.lean:35]", r_not_outside),
    ("intersection_circle_circle_2 [Diagrammatic.lean:319]", r_icc2),
    ("intersection_circle_line_2 [Diagrammatic.lean:305]", r_icl2),
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
    ("angle_same_ray [SMT-bridge]", r_angle_same_ray),
    ("between_order [SMT-bridge]", r_between_order),
    # NOTE: "straight_angle" (between u v w, x off the line ⇒
    # ∠u:v:x + ∠x:v:w = 2∟) was REMOVED from the default closure
    # (P-euclid-straight-angle-out): it IS Book I.13, so closing I.1-I.12
    # with it used a later theorem. I.13 is now proved from I.11'
    # (PROP13_STEPS_DATA) and the rule is available to later propositions
    # only as the cited lemma CONSTRUCTIONS["proposition_13"].
    ("segment_gte_zero [Metric.lean:33]", r_len_gte_zero),
    ("peq_subst [derived, Metric.lean:21]", r_peq_subst),
    ("same_side_not_on_line [Diagrammatic.lean:120]", r_same_side_not_on_line),
    ("opposing_parts [Relations.lean:13-14]", r_opposing_parts),
    ("line_neq [logic]", r_line_neq),
    ("angle_pos [SMT-bridge]", r_angle_pos),
    # NOTE: "side_gt_angle_gt" (|p-r| > |p-q| ⇒ ∠p:q:r > ∠q:r:p) was
    # REMOVED from the default closure (E-kimi-task-389): it IS Book
    # I.18, so closing I.1-I.17 with it used a later theorem. I.18 is now
    # proved from I.3/I.16/I.5' (PROP18_STEPS_DATA) and the theorem is
    # available to later propositions only as the cited lemma
    # CONSTRUCTIONS["proposition_18"]. Post-removal reading: the rule's
    # row was also sign-INVERTED (it added ∠p:q:r < ∠q:r:p); every
    # historical use paired it with an angle equality, where either
    # strict direction reaches the same unsat, so the inversion was
    # invisible until the I.18 replay (see test_prop18_hyps_alone...).
    ("off_line_pair_same_side [SMT-bridge]", r_off_line_pair_same_side),
    ("peq_subst_diagram [logic]", r_peq_subst_diagram),
]


def close(s):
    """Run forward rules to fixpoint. Returns iteration count.

    Stops early when the linear store is UNSAT: past that point every
    entailment query answers vacuously, and rules gated on entailment
    (off_line_pair_same_side) would derive garbage from a
    broken state. FMTooLarge is not an unsat answer, so closure continues."""
    it = 0
    while True:
        it += 1
        changed = False
        excluded = getattr(s, "exclude_rules", frozenset())
        for name, fn in FORWARD_RULES:
            if name in excluded:
                continue  # excluded for this replay (replay's exclude_rules)
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
    c = getattr(s, "_apc", None)
    if c is not None and c[0] == len(s.facts):
        return c[1]
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
        elif k in ("same_side", "not_same_side"):
            pts.update(fact[1:3])
        elif k in ("eq_ang",):
            pts.update(fact[1:4]); pts.update(fact[4:7])
        elif k in ("sum_ang",):
            pts.update(fact[1:4]); pts.update(fact[4:7])
        elif k in ("subang",):
            pts.update(fact[1][1:])
        # intersects_cc, neq_line: circle/line names only
    s._apc = (len(s.facts), pts)
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


def _dpl_either(s, p, q, L):
    # distinctPointsOnLine (Relations.lean:6) is symmetric in its points;
    # premise tuples name a direction, so accept whichever the state holds.
    f = ("dpl", p, q, L)
    if s.has(f):
        return f
    return ("dpl", q, p, L)


def _opposing_either(s, p, q, L):
    # opposingSides (Relations.lean:13-14) is symmetric in its points
    # (¬p.onLine L ∧ ¬q.onLine L ∧ ¬p.sameSide q L); premise tuples name a
    # direction, so accept whichever the state holds.
    f = ("opposing_sides", p, q, L)
    if s.has(f):
        return f
    return ("opposing_sides", q, p, L)


def _need(s, i, rule, fact):
    if not s.has(fact):
        raise MissingPremise(i, rule, fact, "(no forward rule derives it)")
    return fact


CONSTRUCTIONS = {
    # SystemE/Theory/Constructions/Points.lean:5 — arbitrary_point
    # verbatim: ∃ _ : Point, true. A fresh point with no facts.
    "arbitrary_point": {
        "cite": "Points.lean:5",
        "args": (),
        "premises": lambda s: [],
        "make": lambda s: [("p", [])],
    },
    # Points.lean:8 — distinct_points verbatim:
    #   ∀ p₁ : Point, ∃ p₂ : Point, p₁ ≠ p₂
    "distinct_points": {
        "cite": "Points.lean:8",
        "args": ("p",),
        "premises": lambda s, p: [],
        "make": lambda s, p: [("q", [("neq", p, "q")])],
    },
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
    # Book/Prop10.lean:7 — bisect a segment, cited as a lemma (the engine's
    # own I.10 replay stands behind the citation). Statement verbatim:
    #   ∀ (a b : Point) (AB : Line), distinctPointsOnLine a b AB →
    #     ∃ d : Point, (between a d b) ∧ (|(a─d)| = |(d─b)|)
    # Existential conclusion, so a construction when cited.
    "proposition_10": {
        "cite": "Book/Prop10.lean:7",
        "args": ("a", "b", "AB"),
        "premises": lambda s, a, b, AB: [_dpl_either(s, a, b, AB)],
        "make": lambda s, a, b, AB: [
            ("d", [("between", a, "d", b),
                   ("_eq_len", a, "d", "d", b)])],
    },
    # Constructions/Intersections.lean:17
    "intersection_circles": {
        "cite": "Intersections.lean:17",
        "args": ("alpha", "beta"),
        "premises": lambda s, a, b: [("intersects_cc", a, b)],
        "make": lambda s, a, b: [
            ("c", [("on_circle", "c", a), ("on_circle", "c", b)])],
    },
    # Constructions/Intersections.lean:20 — intersection_same_side
    # verbatim:
    #   ∀ (α β : Circle) (b c d : Point) (L : Line),
    #     (α.intersectsCircle β) ∧ (c.isCentre α) ∧ (d.isCentre β) ∧
    #     (c.onLine L) ∧ (d.onLine L) ∧ ¬(b.onLine L) →
    #       ∃ a : Point, (a.onCircle α) ∧ (a.onCircle β) ∧
    #         (a.sameSide b L)
    "intersection_same_side": {
        "cite": "Intersections.lean:20",
        "args": ("alpha", "beta", "b", "c", "d", "L"),
        "premises": lambda s, a, b, bp, c, d, L: [
            ("intersects_cc", a, b), ("centre", c, a),
            ("centre", d, b), ("on_line", c, L), ("on_line", d, L),
            ("not_on_line", bp, L)],
        "make": lambda s, a, b, bp, c, d, L: [
            ("a", [("on_circle", "a", a), ("on_circle", "a", b),
                   ("same_side", "a", bp, L)])],
    },
    # SystemE/Theory/Constructions/Points.lean:60 — exists_point_opposite
    # verbatim: ¬(b.onLine L) → ∃ a : Point, a.opposingSides b L
    "exists_point_opposite": {
        "cite": "Points.lean:60",
        "args": ("L", "b"),
        "premises": lambda s, L, b: [("not_on_line", b, L)],
        "make": lambda s, L, b: [
            ("a", [("opposing_sides", "a", b, L)])],
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
    # SystemE/Theory/Inferences/Diagrammatic.lean:312 —
    # intersection_circle_circle_1 verbatim:
    #   (a.onCircle α) ∧ ¬(b.outsideCircle α) ∧ (a.insideCircle β) ∧
    #   (b.outsideCircle β) → α.intersectsCircle β
    # ¬(b.outsideCircle α) is rendered as the POSITIVE fact not_outside
    # b α (the same rendering intersection_circle_line_1 uses: on or
    # inside, Relations.lean:35). Cited per use, not a closure rule (a
    # closure rule would add intersects_cc to earlier replays whose
    # footprints are pinned). First use: Book I.22, where Lean's SMT
    # closes the intersects_cc premise of intersection_circles silently
    # (Prop22.lean:28); a = i (on KLH, inside DKL), b = h (on KLH —
    # hence not outside it — and outside DKL).
    "intersection_circle_circle_1": {
        "cite": "Diagrammatic.lean:312 + Relations.lean:35",
        "args": ("a", "b", "alpha", "beta"),
        "premises": lambda s, a, b, alpha, beta: [
            ("on_circle", a, alpha), ("not_outside", b, alpha),
            ("inside", a, beta), ("outside", b, beta)],
        "make": lambda s, a, b, alpha, beta: [
            (None, [("intersects_cc", alpha, beta)])],
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
    # SystemE/Theory/Inferences/Diagrammatic.lean:297 —
    # intersection_circle_line_1 verbatim: ¬(a.outsideCircle α) ∧
    # ¬(b.outsideCircle α) ∧ (a.opposingSides b L) → L.intersectsCircle α.
    # In the I.12 replay Lean's SMT obtains AB.intersectsCircle EFG from
    # opposingSides d c AB with d on the circle and c its centre (both
    # not-outside); this cites the SystemE axiom that does exactly that.
    "intersection_circle_line_1": {
        "cite": "Diagrammatic.lean:297",
        "args": ("a", "b", "alpha", "L"),
        "premises": lambda s, a, b, alpha, L: [
            ("not_outside", a, alpha), ("not_outside", b, alpha),
            ("opposing_sides", a, b, L)],
        "make": lambda s, a, b, alpha, L: [
            (None, [("intersects_lc", L, alpha)])],
    },
    # SystemE/Theory/Constructions/Intersections.lean:6 —
    # intersections_circle_line verbatim: L.intersectsCircle α →
    # ∃ a b, a.onCircle α ∧ a.onLine L ∧ b.onCircle α ∧ b.onLine L ∧ a ≠ b.
    "intersections_circle_line": {
        "cite": "Intersections.lean:6",
        "args": ("alpha", "L"),
        "premises": lambda s, alpha, L: [("intersects_lc", L, alpha)],
        "make": lambda s, alpha, L: [
            ("a", [("on_circle", "a", alpha), ("on_line", "a", L)]),
            ("b", [("on_circle", "b", alpha), ("on_line", "b", L)]),
            (None, [("neq", "a", "b")])],
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
    # SystemE/Theory/Constructions/Points.lean:43 — extend_point_longer
    # verbatim:
    #   ∀ (L : Line) (b c : Point) (s : Segment), distinctPointsOnLine b c L →
    #     ∃ a : Point, (a.onLine L) ∧ (between b c a) ∧ |(c─a)| > |s|
    # The engine has no Segment objects, so s is passed as its two endpoint
    # arguments (s0, s1) — Lean's call extend_point_longer BE b e (b─e) is
    # ("BE", "b", "e", "b", "e") here.
    "extend_point_longer": {
        "cite": "Points.lean:43",
        "args": ("L", "b", "c", "s0", "s1"),
        "premises": lambda s, L, b, c, s0, s1: [_dpl_either(s, b, c, L)],
        "make": lambda s, L, b, c, s0, s1: [
            ("x", [("on_line", "x", L), ("between", b, c, "x"),
                   ("_gt_len", c, "x", s0, s1)])],
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
    # Book/Prop06.lean:7-9 — proposition_6 verbatim:
    #   ∀ (a b c : Point) (AB BC AC : Line),
    #   formTriangle a b c AB BC AC ∧ (∠ a:b:c = ∠ a:c:b) →
    #   |(a─b)| = |(a─c)|
    # Cited as a lemma (the engine's own I.6 replay, PROP6_STEPS_DATA,
    # is the proof — by_contra on |ab| ≠ |ac|, proposition_3 cut +
    # proposition_4 SAS in each trichotomy branch). formTriangle
    # destructured as at proposition_25 (I.6's own hypothesis list,
    # PROP6_HYPS_DATA, uses dpl a b AB for the first line); the equal
    # base angles are an entailment premise (a metric query, as at
    # proposition_24/25). The conclusion is the metric tuple _eq_len.
    "proposition_6": {
        "cite": "Book/Prop06.lean:7-9 (proposition_6)",
        "args": ("a", "b", "c", "AB", "BC", "AC"),
        "premises": lambda s, a, b, c, AB, BC, AC: [
            _dpl_either(s, a, b, AB),
            ("on_line", b, BC), ("on_line", c, BC),
            ("on_line", c, AC), ("on_line", a, AC),
            ("neq", b, c), ("neq", a, c),
            ("not_on_line", c, AB)],
        "entail": lambda s, a, b, c, AB, BC, AC: [
            ("eq_ang", (a, b, c, a, c, b))],
        "make": lambda s, a, b, c, AB, BC, AC: [
            (None, [("_eq_len", a, b, a, c)])],
    },
    # Book/Prop07.lean:5 — the triangle inequality on a point (SSS falsifies
    # a second triangle on the same base side). Conclusion False, so cited as
    # a rule it ends whatever branch applies it (contradictory() reports the
    # ("false",) fact).
    "proposition_7": {
        "cite": "Book/Prop07.lean:5",
        "args": ("a", "b", "c", "d", "AB", "AC", "CB", "AD", "DB"),
        "premises": lambda s, a, b, c, d, AB, AC, CB, AD, DB: [
            ("dpl", a, b, AB), ("dpl", a, c, AC), ("dpl", c, b, CB),
            ("dpl", a, d, AD), ("dpl", d, b, DB),
            ("same_side", c, d, AB), ("neq", c, d)],
        "entail": lambda s, a, b, c, d, AB, AC, CB, AD, DB: [
            ("eq_len", (a, c, a, d)), ("eq_len", (c, b, d, b))],
        "make": lambda s, a, b, c, d, AB, AC, CB, AD, DB: [
            (None, [("false",)])],
    },
    # Book/Prop01.lean:18 (proposition_1') — equilateral construction on the
    # side of AB opposite x (existential conclusion: construction when
    # cited). Its non-degeneracy premise ¬x.onLine AB mirrors Lean's.
    "proposition_1'": {
        "cite": "Book/Prop01.lean:18",
        "args": ("a", "b", "x", "AB"),
        "premises": lambda s, a, b, x, AB: [
            ("dpl", a, b, AB), ("not_on_line", x, AB)],
        "make": lambda s, a, b, x, AB: [
            ("c", [("_eq_len", "c", a, a, b), ("_eq_len", "c", b, a, b),
                   ("opposing_sides", "c", x, AB)])],
    },
    # Book/Prop08.lean:5 — SSS as a forward rule (the engine's own I.8 replay
    # stands behind the citation). formTriangles destructed; the three length
    # congruences are entailment premises.
    "proposition_8": {
        "cite": "Book/Prop08.lean:5",
        "args": ("a", "b", "c", "d", "e", "f", "AB", "BC", "AC",
                 "DE", "EF", "DF"),
        "premises": lambda s, a, b, c, d, e, f, AB, BC, AC, DE, EF, DF: [
            ("dpl", a, b, AB), ("on_line", b, BC), ("on_line", c, BC),
            ("on_line", c, AC), ("on_line", a, AC),
            ("not_on_line", c, AB),
            ("dpl", d, e, DE), ("on_line", e, EF), ("on_line", f, EF),
            ("on_line", f, DF), ("on_line", d, DF),
            ("not_on_line", f, DE)],
        "entail": lambda s, a, b, c, d, e, f, AB, BC, AC, DE, EF, DF: [
            ("eq_len", (a, b, d, e)), ("eq_len", (a, c, d, f)),
            ("eq_len", (b, c, e, f))],
        "make": lambda s, a, b, c, d, e, f, AB, BC, AC, DE, EF, DF: [
            (None, [("_eq_ang", b, a, c, e, d, f)])],
    },
    # Book/Prop09.lean (proposition_9') — angle bisection point with its
    # side conclusions, as a construction when cited. Statement verbatim:
    #   ∀ (a b c : Point) (AB AC : Line),
    #     formRectilinearAngle b a c AB AC ∧ AB ≠ AC →
    #     ∃ f : Point, (f.sameSide c AB) ∧ (f.sameSide b AC) ∧
    #                  (∠ b:a:f = ∠ c:a:f)
    # formRectilinearAngle b a c AB AC destructures (Relations.lean:64) to
    # dpl b a AB ∧ dpl a c AC. The engine's own I.9 replay (b6407e7) stands
    # behind the citation of the construction; this prime form adds the two
    # sameSide conclusions (Lean's proof: proposition_8 on the SSS pair plus
    # intersection_lines AF DE, finished by SMT).
    "proposition_9'": {
        "cite": "Book/Prop09.lean (proposition_9')",
        "args": ("a", "b", "c", "AB", "AC"),
        "premises": lambda s, a, b, c, AB, AC: [
            _dpl_either(s, b, a, AB), _dpl_either(s, a, c, AC),
            ("neq_line", AB, AC)],
        "make": lambda s, a, b, c, AB, AC: [
            ("f", [("same_side", "f", c, AB), ("same_side", "f", b, AC),
                   ("_eq_ang", b, a, "f", c, a, "f")])],
    },
    # SystemE/Theory/Constructions/Intersections.lean:3 — exactly the axiom:
    # L.intersectsLine M ⇒ ∃ a, a.onLine L ∧ a.onLine M. The premise fact
    # ("intersects_ll", L, M) is produced by the cited rule
    # intersection_lines_opposing (Diagrammatic.lean:267) below.
    "intersection_lines": {
        "cite": "Intersections.lean:3",
        "args": ("L", "M"),
        "premises": lambda s, L, M: [("intersects_ll", L, M)],
        "make": lambda s, L, M: [
            ("a", [("on_line", "a", L), ("on_line", "a", M)])],
    },
    # SystemE/Theory/Inferences/Diagrammatic.lean:267 —
    # intersection_lines_opposing verbatim:
    #   (a.opposingSides b L) ∧ (a.onLine M) ∧ (b.onLine M) →
    #   L.intersectsLine M
    # Cited before intersection_lines (I.10).
    "intersection_lines_opposing": {
        "cite": "Diagrammatic.lean:267",
        "args": ("a", "b", "L", "M"),
        "premises": lambda s, a, b, L, M: [
            ("opposing_sides", a, b, L),
            ("on_line", a, M), ("on_line", b, M)],
        "make": lambda s, a, b, L, M: [
            (None, [("intersects_ll", L, M)])],
    },
    # [SMT-bridge] Lean's euclid_assert-style ray-side fact (Prop10.lean):
    # v, x, u on L (v ≠ x, v ≠ u) and ∠(z,v,x) = ∠(w,v,x) ⇒
    # ∠(z,v,u) = ∠(w,v,u). u on line(v,x) distinct from v lies on the ray
    # v→x or on its opposite; the reflection through v maps an angle θ to
    # 2∟−θ, so the equality is preserved on both rays. Lean's SMT uses this
    # when it feeds proposition_9's angle about d' to proposition_4 about
    # the intersection point d of line(c,d') with the base.
    # neq v x is required: with x = v both premise angles are 0 for every
    # z, w and the rule would force unrelated angles equal.
    "angle_pair_on_line": {
        "cite": "euclid_assert [SMT-bridge]",
        "args": ("z", "w", "v", "x", "u", "L"),
        # review (claude-9): z and w must be off L. System E gives no
        # value to a straight angle (between z v x) or to an angle with
        # z = v; only degenerated_angle_if (Transfer.lean:63) fixes 0 on
        # the ray. With z on L beyond v, ∠z:v:x = ∠w:v:x is satisfiable
        # with w off L, yet for u on z's ray ∠z:v:u = 0 ≠ ∠w:v:u.
        "premises": lambda s, z, w, v, x, u, L: [
            ("on_line", v, L), ("on_line", x, L), ("on_line", u, L),
            ("neq", v, x), ("neq", v, u),
            ("not_on_line", z, L), ("not_on_line", w, L)],
        "entail": lambda s, z, w, v, x, u, L: [
            ("eq_ang", (z, v, x, w, v, x))],
        "make": lambda s, z, w, v, x, u, L: [
            (None, [("_eq_ang", z, v, u, w, v, u)])],
    },
    # SystemE/Theory/Inferences/Diagrammatic.lean:173 — pasch_4 verbatim:
    #   (L ≠ M) ∧ b.onLine L ∧ b.onLine M ∧ distinctPointsOnLine a c M ∧
    #   (a ≠ b) ∧ (c ≠ b) ∧ ¬(a.sameSide c L) → between a b c
    # ¬(a.sameSide c L) is rendered as the positive fact opposing_sides a c L
    # (stronger: it also carries a, c off L — Relations.lean:13-14), because
    # the engine has no negated sameSide fact kind.
    "pasch_4": {
        "cite": "Diagrammatic.lean:173",
        "args": ("a", "b", "c", "L", "M"),
        "premises": lambda s, a, b, c, L, M: [
            ("neq_line", L, M),
            ("on_line", b, L), ("on_line", b, M),
            ("dpl", a, c, M), ("neq", a, b), ("neq", c, b),
            ("opposing_sides", a, c, L)],
        "make": lambda s, a, b, c, L, M: [
            (None, [("between", a, b, c)])],
    },
    # SystemE/Theory/Inferences/Diagrammatic.lean:45-46 — the parts of
    # between_symm besides the reversal (which is the closure rule
    # r_between_symm): between a b c → a ≠ b ∧ a ≠ c ∧ ¬(between b a c).
    # A closure rule would add facts to every earlier replay (footprints
    # are pinned), so this is cited per use instead.
    "between_symm_parts": {
        "cite": "Diagrammatic.lean:45-46",
        "args": ("a", "b", "c"),
        "premises": lambda s, a, b, c: [("between", a, b, c)],
        "make": lambda s, a, b, c: [
            (None, [("neq", a, b), ("neq", a, c),
                    ("not_between", b, a, c)])],
    },
    # SystemE/Theory/Inferences/Diagrammatic.lean:78 — between_trans_out
    # verbatim: between a b c ∧ between b c d → between a b d. A closure
    # rule would add facts to every earlier replay (footprints are
    # pinned), so this is cited per use. First use: proposition_22',
    # where euclid_finish's ¬between g f e is rendered as a by_contra
    # whose branch chains between g f e (assumed) with between f e e''.
    "between_trans_out": {
        "cite": "Diagrammatic.lean:78",
        "args": ("a", "b", "c", "d"),
        "premises": lambda s, a, b, c, d: [
            ("between", a, b, c), ("between", b, c, d)],
        "make": lambda s, a, b, c, d: [
            (None, [("between", a, b, d)])],
    },
    # SystemE/Theory/Inferences/Transfer.lean:102 — perpendicular_if
    # verbatim: (a.onLine L) ∧ (b.onLine L) ∧ (between a c b) ∧
    # ¬(d.onLine L) ∧ (∠ a:c:d = ∠ d:c:b) → ∠ a:c:d = ∟
    # The angle premise is an entailment premise; the conclusion is the
    # row ∠a:c:d = RT with the fact ("right_ang", a, c, d).
    "perpendicular_if": {
        "cite": "Transfer.lean:102",
        "args": ("a", "b", "c", "d", "L"),
        "premises": lambda s, a, b, c, d, L: [
            ("on_line", a, L), ("on_line", b, L), ("between", a, c, b),
            ("not_on_line", d, L)],
        "entail": lambda s, a, b, c, d, L: [
            ("eq_ang", (a, c, d, d, c, b))],
        "make": lambda s, a, b, c, d, L: [
            (None, [("_rt_ang", a, c, d)])],
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
    # SystemE/Theory/Inferences/Transfer.lean:107 — perpendicular_onlyif
    # verbatim: (a.onLine L) ∧ (b.onLine L) ∧ (between a c b) ∧
    # ¬(d.onLine L) ∧ (∠ a:c:d = ∟) → ∠ a:c:d = ∠ d:c:b
    # The right-angle premise is an entailment premise (a "rt_ang" query).
    "perpendicular_onlyif": {
        "cite": "Transfer.lean:107",
        "args": ("a", "b", "c", "d", "L"),
        "premises": lambda s, a, b, c, d, L: [
            ("on_line", a, L), ("on_line", b, L), ("between", a, c, b),
            ("not_on_line", d, L)],
        "entail": lambda s, a, b, c, d, L: [("rt_ang", (a, c, d))],
        "make": lambda s, a, b, c, d, L: [
            (None, [("_eq_ang", a, c, d, d, c, b)])],
    },
    # Book/Prop11.lean:24 — proposition_11' verbatim:
    #   ∀ (a b c x : Point) (AB : Line), (distinctPointsOnLine a b AB) ∧
    #   (between a c b) ∧ ¬(x.onLine AB) →
    #   exists f : Point, (f.sameSide x AB) ∧ (∠ a:c:f = ∟)
    # Cited as a lemma (the engine's own I.11 replay is the proof of the
    # constructible version; Prop11.lean:24 is exactly this existential).
    # Existential conclusion, so a construction when cited.
    "proposition_11'": {
        "cite": "Book/Prop11.lean:24",
        "args": ("a", "b", "c", "x", "AB"),
        "premises": lambda s, a, b, c, x, AB: [
            _dpl_either(s, a, b, AB), ("between", a, c, b),
            ("not_on_line", x, AB)],
        "make": lambda s, a, b, c, x, AB: [
            ("f", [("same_side", "f", x, AB), ("_rt_ang", a, c, "f")])],
    },
    # Book/Prop13.lean:6 — proposition_13 verbatim:
    #   ∀ (a b c d : Point) (AB CD : Line), AB ≠ CD ∧
    #   (distinctPointsOnLine a b AB) ∧ (distinctPointsOnLine c d CD) ∧
    #   (between d b c) → ∠ c:b:a + ∠ a:b:d = ∟ + ∟
    # The engine's own I.13 replay (PROP13_STEPS_DATA) is the proof; this
    # entry is the theorem cited as a lemma by later propositions — it
    # replaces the old "straight_angle [SMT-bridge]" closure rule, which
    # proved exactly this and so let I.1-I.12 use a later theorem
    # (P-euclid-straight-angle-out). The conclusion is the metric tuple
    # _sum_rt_ang (same fact form as the by_cases conclusions and the
    # PROP13_GOAL query): ∠c:b:a + ∠a:b:d = 2∟.
    "proposition_13": {
        "cite": "Book/Prop13.lean:6",
        "args": ("a", "b", "c", "d", "AB", "CD"),
        "premises": lambda s, a, b, c, d, AB, CD: [
            ("neq_line", AB, CD),
            _dpl_either(s, a, b, AB), _dpl_either(s, c, d, CD),
            ("between", d, b, c)],
        "make": lambda s, a, b, c, d, AB, CD: [
            (None, [("_sum_rt_ang", c, b, a, d)])],
    },
    # Book/Prop15.lean:6 — proposition_15 verbatim (vertical angles):
    #   ∀ (a b c d e : Point) (AB CD : Line), distinctPointsOnLine a b AB ∧
    #   distinctPointsOnLine c d CD ∧ e.onLine AB ∧ e.onLine CD ∧
    #   CD ≠ AB ∧ (between d e c) ∧ (between a e b) →
    #   (∠ a:e:c = ∠ d:e:b) ∧ (∠ c:e:b = ∠ a:e:d)
    # Cited as a lemma (the engine's own I.15 replay, PROP15_STEPS_DATA, is
    # the proof); both angle equalities are made as metric facts. No fresh
    # objects, so a forward-style citation like proposition_13.
    "proposition_15": {
        "cite": "Book/Prop15.lean:6",
        "args": ("a", "b", "c", "d", "e", "AB", "CD"),
        "premises": lambda s, a, b, c, d, e, AB, CD: [
            _dpl_either(s, a, b, AB), _dpl_either(s, c, d, CD),
            ("on_line", e, AB), ("on_line", e, CD),
            ("neq_line", CD, AB),
            ("between", d, e, c), ("between", a, e, b)],
        "make": lambda s, a, b, c, d, e, AB, CD: [
            (None, [("_eq_ang", a, e, c, d, e, b),
                    ("_eq_ang", c, e, b, a, e, d)])],
    },
    # Book/Prop16.lean:8 — proposition_16 verbatim (exterior angle):
    #   ∀ (a b c d : Point) (AB BC AC : Line),
    #   formTriangle a b c AB BC AC ∧ (between b c d) →
    #   (∠ a:c:d > ∠ c:b:a) ∧ (∠ a:c:d > ∠ b:a:c)
    # Cited as a lemma (the engine's own I.16a/I.16b replays are the
    # proofs of the two conjuncts); formTriangle destructured exactly as
    # at PROP6/PROP16B. Both strict inequalities are made as metric facts
    # (gt_ang rows). No fresh objects, so a forward-style citation like
    # proposition_15.
    "proposition_16": {
        "cite": "Book/Prop16.lean:8",
        "args": ("a", "b", "c", "d", "AB", "BC", "AC"),
        "premises": lambda s, a, b, c, d, AB, BC, AC: [
            _dpl_either(s, a, b, AB),
            ("on_line", b, BC), ("on_line", c, BC),
            ("on_line", c, AC), ("on_line", a, AC),
            ("neq", b, c), ("neq", a, c),
            ("not_on_line", c, AB),
            ("between", b, c, d)],
        "make": lambda s, a, b, c, d, AB, BC, AC: [
            (None, [("_gt_ang", a, c, d, c, b, a),
                    ("_gt_ang", a, c, d, b, a, c)])],
    },
    # Book/Prop17.lean:6 — proposition_17 verbatim:
    #   ∀ (a b c : Point) (AB BC AC : Line),
    #   formTriangle a b c AB BC AC → ∠ a:b:c + ∠ b:c:a < ∟ + ∟
    # Cited as a lemma (the engine's own I.17 replay, PROP17_STEPS_DATA,
    # is the proof); formTriangle destructured exactly as at
    # PROP6/PROP16B. The conclusion is the metric tuple _lt_sum_rt_ang
    # (LA row ∠a:b:c + ∠b:c:a − 2∟ < 0 plus the lt_sum_rt_ang fact form).
    # No fresh objects, so a forward-style citation like proposition_16.
    "proposition_17": {
        "cite": "Book/Prop17.lean:6",
        "args": ("a", "b", "c", "AB", "BC", "AC"),
        "premises": lambda s, a, b, c, AB, BC, AC: [
            _dpl_either(s, a, b, AB),
            ("on_line", b, BC), ("on_line", c, BC),
            ("on_line", c, AC), ("on_line", a, AC),
            ("neq", b, c), ("neq", a, c),
            ("not_on_line", c, AB)],
        "make": lambda s, a, b, c, AB, BC, AC: [
            (None, [("_lt_sum_rt_ang", a, b, c, b, c, a)])],
    },
    # Book/Prop18.lean:6 — proposition_18 verbatim:
    #   ∀ (a b c : Point) (AB BC AC : Line),
    #   formTriangle a b c AB BC AC ∧ (|(a─c)| > |(a─b)|) →
    #   (∠ a:b:c > ∠ b:c:a)
    # Cited as a lemma (the engine's own I.18 replay, PROP18_STEPS_DATA,
    # is the proof); it replaces the "side_gt_angle_gt [SMT-bridge]"
    # closure rule, which proved exactly this and so let I.1-I.17 use a
    # later theorem (E-kimi-task-389, the c237244 straight_angle/I.13
    # pattern). formTriangle destructured as at proposition_17; the
    # length comparison is an entailment premise. The conclusion is the
    # metric tuple _gt_ang. No fresh objects, forward-style citation.
    "proposition_18": {
        "cite": "Book/Prop18.lean:6",
        "args": ("a", "b", "c", "AB", "BC", "AC"),
        "premises": lambda s, a, b, c, AB, BC, AC: [
            _dpl_either(s, a, b, AB),
            ("on_line", b, BC), ("on_line", c, BC),
            ("on_line", c, AC), ("on_line", a, AC),
            ("neq", b, c), ("neq", a, c),
            ("not_on_line", c, AB)],
        "entail": lambda s, a, b, c, AB, BC, AC: [
            ("gt_len", (a, c, a, b))],
        "make": lambda s, a, b, c, AB, BC, AC: [
            (None, [("_gt_ang", a, b, c, b, c, a)])],
    },
    # Book/Prop19.lean:6 — proposition_19 verbatim:
    #   ∀ (a b c : Point) (AB BC AC : Line),
    #   formTriangle a b c AB BC AC ∧ (∠ a:b:c > ∠ b:c:a) →
    #   (|(a─c)| > |(a─b)|)
    # Cited as a lemma (the engine's own I.19 replay, PROP19_STEPS_DATA,
    # is the proof — by_contra citing proposition_5' and proposition_18).
    # formTriangle destructured as at proposition_18; the angle
    # comparison is an entailment premise. The conclusion is the metric
    # tuple _gt_len. No fresh objects, forward-style citation.
    "proposition_19": {
        "cite": "Book/Prop19.lean:6",
        "args": ("a", "b", "c", "AB", "BC", "AC"),
        "premises": lambda s, a, b, c, AB, BC, AC: [
            _dpl_either(s, a, b, AB),
            ("on_line", b, BC), ("on_line", c, BC),
            ("on_line", c, AC), ("on_line", a, AC),
            ("neq", b, c), ("neq", a, c),
            ("not_on_line", c, AB)],
        "entail": lambda s, a, b, c, AB, BC, AC: [
            ("gt_ang", (a, b, c, b, c, a))],
        "make": lambda s, a, b, c, AB, BC, AC: [
            (None, [("_gt_len", a, c, a, b)])],
    },
    # Book/Prop20.lean:6 — proposition_20 verbatim (triangle inequality):
    #   ∀ (a b c : Point) (AB BC AC : Line),
    #   formTriangle a b c AB BC AC → |(b─a)| + |(a─c)| > |(b─c)|
    # Cited as a lemma (the engine's own I.20 replay, PROP20_STEPS_DATA,
    # is the proof — extend_point_longer + proposition_3 + proposition_5'
    # + sum_angles_onlyif + proposition_19). formTriangle destructured as
    # at proposition_19. The conclusion is the metric tuple _gt_sum_len:
    # the strict LA row |b─a| + |a─c| − |b─c| > 0 plus the gt_sum_len
    # fact form (apply_step's make loop handles it). No fresh objects,
    # forward-style citation.
    "proposition_20": {
        "cite": "Book/Prop20.lean:6",
        "args": ("a", "b", "c", "AB", "BC", "AC"),
        "premises": lambda s, a, b, c, AB, BC, AC: [
            _dpl_either(s, a, b, AB),
            ("on_line", b, BC), ("on_line", c, BC),
            ("on_line", c, AC), ("on_line", a, AC),
            ("neq", b, c), ("neq", a, c),
            ("not_on_line", c, AB)],
        "make": lambda s, a, b, c, AB, BC, AC: [
            (None, [("_gt_sum_len", b, a, a, c, b, c)])],
    },
    # SystemE/Theory/Inferences/Diagrammatic.lean:161 — pasch_3 verbatim:
    #   (between a b c) ∧ (b.onLine L) → ¬(a.sameSide c L)
    # Rendered as the POSITIVE fact opposing_sides a c L (same rendering as
    # pasch_4): opposingSides a c L := ¬a.onLine L ∧ ¬c.onLine L ∧
    # ¬a.sameSide c L (Relations.lean:13-14), so the two off-line parts are
    # added as premises and the conclusion carries exactly pasch_3's
    # content plus them.
    "pasch_3_opposing": {
        "cite": "Diagrammatic.lean:161 + Relations.lean:13-14",
        "args": ("a", "b", "c", "L"),
        "premises": lambda s, a, b, c, L: [
            ("between", a, b, c), ("on_line", b, L),
            ("not_on_line", a, L), ("not_on_line", c, L)],
        "make": lambda s, a, b, c, L: [
            (None, [("opposing_sides", a, c, L)])],
    },
    # [derived: Diagrammatic.lean:127 (sameSide transitivity) read as its
    # contrapositive + Relations.lean:13-14] x opposing y across L and
    # z on y's side of L put x opposing z across L:
    #   opposingSides x y L ∧ sameSide z y L → opposingSides x z L
    # (¬(x~y) ∧ (z~y) ⇒ ¬(x~z), since x~z ∧ z~y would give x~y; the
    # off-line parts ship with the premises).
    "opposing_same_side_trans": {
        "cite": "derived, Diagrammatic.lean:127 contrapositive",
        "args": ("x", "y", "z", "L"),
        "premises": lambda s, x, y, z, L: [
            ("opposing_sides", x, y, L), ("same_side", z, y, L)],
        "make": lambda s, x, y, z, L: [
            (None, [("opposing_sides", x, z, L)])],
    },
    # [derived: Diagrammatic.lean:127 (sameSide transitivity) double
    # contrapositive + Relations.lean:13-14] x and z both opposing y across
    # L are on the SAME side of L:
    #   opposingSides x y L ∧ opposingSides z y L → sameSide x z L
    # (sameSide is an equivalence on the points off L — Diagrammatic.lean:
    # 112,120,127 — so with y off L, ¬(x~y) ∧ ¬(z~y) forces x~z; the
    # off-line parts ship with the opposingSides premises). Cited per use
    # (a closure rule would add facts to every earlier replay; footprints
    # are pinned). First use: Book I.14 (d and e both oppose c across AB).
    "opposing_opposing_same_side": {
        "cite": "derived, Diagrammatic.lean:127 double contrapositive",
        "args": ("x", "y", "z", "L"),
        "premises": lambda s, x, y, z, L: [
            _opposing_either(s, x, y, L), _opposing_either(s, z, y, L)],
        "make": lambda s, x, y, z, L: [
            (None, [("same_side", x, z, L)])],
    },
    # Relations.lean:13-14 — opposingSides a b L := ¬(a.onLine L) ∧
    # ¬(b.onLine L) ∧ ¬(a.sameSide b L). This entry is the definition read
    # FORWARD (intro rule): the three parts give opposingSides. Used where
    # the ¬sameSide half of a classical by_cases is in scope as the literal
    # fact kind not_same_side (Book I.24's second branch: d off EF from the
    # common construction, g off EF from the nested split of lines 41-42).
    "opposing_sides_if": {
        "cite": "Relations.lean:13-14 (defn, intro)",
        "args": ("a", "b", "L"),
        "premises": lambda s, a, b, L: [
            ("not_on_line", a, L), ("not_on_line", b, L),
            ("not_same_side", a, b, L)],
        "make": lambda s, a, b, L: [
            (None, [("opposing_sides", a, b, L)])],
    },
    # Book/Prop21.lean:6 — proposition_21 verbatim:
    #   ∀ (a b c d : Point) (AB BC AC BD DC : Line),
    #   formTriangle a b c AB BC AC ∧ (a.sameSide d BC) ∧
    #   (c.sameSide d AB) ∧ (b.sameSide d AC) ∧
    #   distinctPointsOnLine b d BD ∧ distinctPointsOnLine d c DC →
    #   (|(b─d)| + |(d─c)| < |(b─a)| + |(a─c)|) ∧ (∠ b:d:c > ∠ b:a:c)
    # Cited as a lemma (the engine's own I.21 replay, PROP21_STEPS_DATA,
    # is the proof — intersection at e, then proposition_20 twice and
    # proposition_16 twice). formTriangle destructured as at
    # proposition_20. The conclusions are the metric tuples
    # _lt_sum_sum_len and _gt_ang: the strict LA rows plus the fact forms
    # (apply_step's make loop handles both). No fresh objects,
    # forward-style citation.
    "proposition_21": {
        "cite": "Book/Prop21.lean:6",
        "args": ("a", "b", "c", "d", "AB", "BC", "AC", "BD", "DC"),
        "premises": lambda s, a, b, c, d, AB, BC, AC, BD, DC: [
            _dpl_either(s, a, b, AB),
            ("on_line", b, BC), ("on_line", c, BC),
            ("on_line", c, AC), ("on_line", a, AC),
            ("neq", b, c), ("neq", a, c),
            ("not_on_line", c, AB),
            ("same_side", a, d, BC), ("same_side", c, d, AB),
            ("same_side", b, d, AC),
            ("dpl", b, d, BD), ("dpl", d, c, DC)],
        "make": lambda s, a, b, c, d, AB, BC, AC, BD, DC: [
            (None, [("_lt_sum_sum_len", b, d, d, c, b, a, a, c),
                    ("_gt_ang", b, d, c, b, a, c)])],
    },
    # Book/Prop22.lean:34 — proposition_22' verbatim (the variant I.23
    # cites): four distinctPointsOnLine and the three triangle
    # inequalities ⇒ ∃ k g, g.onLine FE ∧ ¬between g f e ∧
    # |f─k| = |a─a'| ∧ |f─g| = |b─b'| ∧ |k─g| = |c─c'|. An existential
    # construction (like proposition_3): it makes the two witness points.
    # The theorem PROP22P_STEPS_DATA replays. The triangle inequalities
    # are entailment premises (the arithmetic must know them); the first
    # use fires them from gt_sum_len hypothesis rows.
    "proposition_22'": {
        "cite": "Book/Prop22.lean:34 (proposition_22')",
        "args": ("a", "a'", "b", "b'", "c", "c'", "f", "e",
                 "A", "B", "C", "FE"),
        "premises": lambda s, a, a2, b, b2, c, c2, f, e, A, B, C, FE: [
            _dpl_either(s, a, a2, A), _dpl_either(s, b, b2, B),
            _dpl_either(s, c, c2, C), _dpl_either(s, f, e, FE)],
        "entail": lambda s, a, a2, b, b2, c, c2, f, e, A, B, C, FE: [
            ("gt_sum_len", (a, a2, b, b2, c, c2)),
            ("gt_sum_len", (a, a2, c, c2, b, b2)),
            ("gt_sum_len", (b, b2, c, c2, a, a2))],
        "make": lambda s, a, a2, b, b2, c, c2, f, e, A, B, C, FE: [
            ("k", [("_eq_len", f, "k", a, a2),
                   ("_eq_len", "k", "g", c, c2)]),
            ("g", [("on_line", "g", FE), ("not_between", "g", f, e),
                   ("_eq_len", f, "g", b, b2)])],
    },
    # Book/Prop23.lean:8 — proposition_23 verbatim: copy an angle onto a
    # given line at a given point —
    #   ∀ (a b c d e : Point) (AB CD CE : Line),
    #     distinctPointsOnLine a b AB ∧ formRectilinearAngle d c e CD CE →
    #       ∃ f : Point, f ≠ a ∧ (∠ f:a:b = ∠ d:c:e)
    # formRectilinearAngle destructures (Relations.lean:64) to dpl d c CD
    # ∧ dpl c e CE. An existential construction: it makes the witness f.
    # The theorem PROP23_STEPS_DATA replays.
    "proposition_23": {
        "cite": "Book/Prop23.lean:8 (proposition_23)",
        "args": ("a", "b", "c", "d", "e", "AB", "CD", "CE"),
        "premises": lambda s, a, b, c, d, e, AB, CD, CE: [
            _dpl_either(s, a, b, AB), _dpl_either(s, d, c, CD),
            _dpl_either(s, c, e, CE)],
        "make": lambda s, a, b, c, d, e, AB, CD, CE: [
            ("f", [("neq", "f", a),
                   ("_eq_ang", "f", a, b, d, c, e)])],
    },
    # Book/Prop23.lean:35 — proposition_23' verbatim: copy an angle onto
    # a given line at a given point, on either side —
    #   ∀ (a b c d e x : Point) (AB CD CE : Line),
    #     distinctPointsOnLine a b AB ∧ formRectilinearAngle d c e CD CE ∧
    #     ¬(x.onLine AB) →
    #       ∃ f : Point, f ≠ a ∧ (f.onLine AB ∨ f.sameSide x AB) ∧
    #         (∠ f:a:b = ∠ d:c:e)
    # formRectilinearAngle destructures (Relations.lean:64) to dpl d c CD
    # ∧ dpl c e CE. The disjunction is the fact kind "either" (see the
    # PROP23P section): the lemma makes the disjunction itself. First
    # cited by Book I.24 (Prop24.lean:18). The theorem
    # PROP23P_STEPS_DATA replays.
    "proposition_23'": {
        "cite": "Book/Prop23.lean:35 (proposition_23')",
        "args": ("a", "b", "c", "d", "e", "x", "AB", "CD", "CE"),
        "premises": lambda s, a, b, c, d, e, x, AB, CD, CE: [
            _dpl_either(s, a, b, AB), _dpl_either(s, d, c, CD),
            _dpl_either(s, c, e, CE), ("not_on_line", x, AB)],
        "make": lambda s, a, b, c, d, e, x, AB, CD, CE: [
            ("f", [("neq", "f", a),
                   ("either", ("on_line", "f", AB),
                    ("same_side", "f", x, AB)),
                   ("_eq_ang", "f", a, b, d, c, e)])],
    },

    # Book/Prop24.lean:12 — proposition_24 verbatim (the hinge theorem):
    #   forall (a b c d e f : Point) (AB BC AC DE EF DF : Line),
    #   formTriangle a b c AB BC AC /\ formTriangle d e f DE EF DF /\
    #   (|(a-b)| = |(d-e)|) /\ (|(a-c)| = |(d-f)|) /\ (ang b:a:c > ang e:d:f) ->
    #   |(b-c)| > |(e-f)|
    # Cited as a lemma (the engine's own I.24 replay, PROP24_STEPS_DATA, is
    # the proof — BOTH branches of the by_cases on d.sameSide g EF).
    # formTriangle destructured as at proposition_17; the two length
    # equalities and the angle comparison are entailment premises (metric
    # queries, as at proposition_19). The conclusion is the metric tuple
    # _gt_len. No fresh objects, forward-style citation.
    "proposition_24": {
        "cite": "Book/Prop24.lean:12 (proposition_24)",
        "args": ("a", "b", "c", "d", "e", "f",
                 "AB", "BC", "AC", "DE", "EF", "DF"),
        "premises": lambda s, a, b, c, d, e, f, AB, BC, AC, DE, EF, DF: [
            _dpl_either(s, a, b, AB),
            ("on_line", b, BC), ("on_line", c, BC),
            ("on_line", c, AC), ("on_line", a, AC),
            ("neq", b, c), ("neq", a, c),
            ("not_on_line", c, AB),
            _dpl_either(s, d, e, DE),
            ("on_line", e, EF), ("on_line", f, EF),
            ("on_line", f, DF), ("on_line", d, DF),
            ("neq", e, f), ("neq", d, f),
            ("not_on_line", f, DE)],
        "entail": lambda s, a, b, c, d, e, f, AB, BC, AC, DE, EF, DF: [
            ("eq_len", (a, b, d, e)), ("eq_len", (a, c, d, f)),
            ("gt_ang", (b, a, c, e, d, f))],
        "make": lambda s, a, b, c, d, e, f, AB, BC, AC, DE, EF, DF: [
            (None, [("_gt_len", b, c, e, f)])],
    },
    # Book/Prop25.lean:7 — proposition_25 verbatim (converse of the
    # hinge theorem):
    #   forall (a b c d e f : Point) (AB BC AC DE EF DF : Line),
    #   formTriangle a b c AB BC AC /\\ formTriangle d e f DE EF DF /\
    #   (|(a-b)| = |(d-e)|) /\\ (|(a-c)| = |(d-f)|) /\\ (|(b-c)| > |(e-f)|) ->
    #   (ang b:a:c > ang e:d:f)
    # Cited as a lemma (the engine's own I.25 replay, PROP25_STEPS_DATA,
    # is the proof — by_contra citing proposition_4 and proposition_24).
    # formTriangle destructured as at proposition_24; the two length
    # equalities and the length comparison are entailment premises
    # (metric queries, as at proposition_24). The conclusion is the
    # metric tuple _gt_ang. No fresh objects, forward-style citation.
    "proposition_25": {
        "cite": "Book/Prop25.lean:7 (proposition_25)",
        "args": ("a", "b", "c", "d", "e", "f",
                 "AB", "BC", "AC", "DE", "EF", "DF"),
        "premises": lambda s, a, b, c, d, e, f, AB, BC, AC, DE, EF, DF: [
            _dpl_either(s, a, b, AB),
            ("on_line", b, BC), ("on_line", c, BC),
            ("on_line", c, AC), ("on_line", a, AC),
            ("neq", b, c), ("neq", a, c),
            ("not_on_line", c, AB),
            _dpl_either(s, d, e, DE),
            ("on_line", e, EF), ("on_line", f, EF),
            ("on_line", f, DF), ("on_line", d, DF),
            ("neq", e, f), ("neq", d, f),
            ("not_on_line", f, DE)],
        "entail": lambda s, a, b, c, d, e, f, AB, BC, AC, DE, EF, DF: [
            ("eq_len", (a, b, d, e)), ("eq_len", (a, c, d, f)),
            ("gt_len", (b, c, e, f))],
        "make": lambda s, a, b, c, d, e, f, AB, BC, AC, DE, EF, DF: [
            (None, [("_gt_ang", b, a, c, e, d, f)])],
    },
    # Book/Prop22.lean:58 — proposition_22'' verbatim (the same-side
    # variant I.23' cites): proposition_22' plus a point x off FE and the
    # extra conclusion k.sameSide x FE —
    #   ∀ (a a' b b' c c' f e x : Point) (A B C FE : Line),
    #     distinctPointsOnLine a a' A ∧ ... ∧ distinctPointsOnLine f e FE ∧
    #     ¬(x.onLine FE) ∧ (|aa'| + |bb'| > |cc'|) ∧ ... →
    #       ∃ k g, g.onLine FE ∧ ¬(between g f e) ∧ (k.sameSide x FE) ∧
    #       |f─k| = |a─a'| ∧ |f─g| = |b─b'| ∧ |k─g| = |c─c'|
    # The theorem PROP22PP_STEPS_DATA replays.
    "proposition_22''": {
        "cite": "Book/Prop22.lean:58 (proposition_22'')",
        "args": ("a", "a'", "b", "b'", "c", "c'", "f", "e", "x",
                 "A", "B", "C", "FE"),
        "premises": lambda s, a, a2, b, b2, c, c2, f, e, x, A, B, C, FE: [
            _dpl_either(s, a, a2, A), _dpl_either(s, b, b2, B),
            _dpl_either(s, c, c2, C), _dpl_either(s, f, e, FE),
            ("not_on_line", x, FE)],
        "entail": lambda s, a, a2, b, b2, c, c2, f, e, x, A, B, C, FE: [
            ("gt_sum_len", (a, a2, b, b2, c, c2)),
            ("gt_sum_len", (a, a2, c, c2, b, b2)),
            ("gt_sum_len", (b, b2, c, c2, a, a2))],
        "make": lambda s, a, a2, b, b2, c, c2, f, e, x, A, B, C, FE: [
            ("k", [("_eq_len", f, "k", a, a2),
                   ("_eq_len", "k", "g", c, c2),
                   ("same_side", "k", x, FE)]),
            ("g", [("on_line", "g", FE), ("not_between", "g", f, e),
                   ("_eq_len", f, "g", b, b2)])],
    },
    # SystemE/Theory/Inferences/Transfer.lean:124 — flat_angle_onlyif
    # verbatim: between a b c → ∠a:b:c = ∟ + ∟ (the straight angle).
    # Cited per use. First use: Book I.23's degenerate branch, where the
    # angle to copy is straight.
    "flat_angle_onlyif": {
        "cite": "Transfer.lean:124",
        "args": ("a", "b", "c"),
        "premises": lambda s, a, b, c: [("between", a, b, c)],
        "make": lambda s, a, b, c: [(None, [("_flat_ang", a, b, c)])],
    },
    # SystemE/Theory/Inferences/Transfer.lean:63 — degenerated_angle_if
    # verbatim: a≠b ∧ a≠c ∧ a,b,c on L ∧ ¬between b a c → ∠b:a:c = 0
    # (the rays a→b and a→c coincide). Cited per use (the closure rule
    # r_degenerated_angle covers only the between-fed shape; footprints
    # are pinned). First use: Book I.23, ∠g:a:b = 0 from the I.22'
    # conclusion ¬between g a b.
    "degenerated_angle_if": {
        "cite": "Transfer.lean:63",
        "args": ("a", "b", "c", "L"),
        "premises": lambda s, a, b, c, L: [
            ("neq", a, b), ("neq", a, c), ("on_line", a, L),
            ("on_line", b, L), ("on_line", c, L),
            ("not_between", b, a, c)],
        "make": lambda s, a, b, c, L: [(None, [("_zero_ang", b, a, c)])],
    },
    # [SMT-bridge: coordinate model] ∠x:v:x = 0 — the ray v→x against
    # itself. Lean's euclid_finish closes this in SMT; it is
    # degenerated_angle_if (Transfer.lean:63) with c := b, whose
    # ¬between b a b premise is between's irreflexivity (no fact form).
    # First use: Book I.23's degenerate zero branch, ∠b:a:b = 0.
    "angle_self_zero": {
        "cite": "SMT-bridge (degenerated_angle_if Transfer.lean:63, c := b)",
        "args": ("x", "v"),
        "premises": lambda s, x, v: [("neq", x, v)],
        "make": lambda s, x, v: [(None, [("_zero_ang", x, v, x)])],
    },
    # [SMT-bridge: coordinate model] ∠x:v:y = 0 ⇒ ∠z:v:x = ∠z:v:y for
    # every z: the rays v→x and v→y coincide (the same ray-equality
    # bridge as the closure rule r_angle_ray_eq, but fed by a zero-angle
    # fact instead of a between fact). First use: Book I.23,
    # ∠f:a:g = ∠f:a:b from ∠g:a:b = 0.
    "angle_ray_eq_zero": {
        "cite": "SMT-bridge (ray coincidence from a zero angle)",
        "args": ("x", "v", "y", "z"),
        "premises": lambda s, x, v, y, z: [("zero_ang", x, v, y)],
        "make": lambda s, x, v, y, z: [
            (None, [("_eq_ang", z, v, x, z, v, y)])],
    },
    # [SMT-bridge: coordinate model] u, v distinct on L and the three
    # STRICT triangle inequalities among |u─x|, |u─v|, |x─v| ⇒ x off L:
    # for collinear x one of the three is an equality (which one depends
    # on x's position on the line). Lean's SMT closes such cases (e.g.
    # f off AB in Prop23.lean's main branch, where the three
    # inequalities are the proposition_20 rows through the I.22' length
    # equalities). First use: Book I.23.
    "off_line_strict_triangle": {
        "cite": "SMT-bridge (three strict triangle inequalities)",
        "args": ("x", "u", "v", "L"),
        "premises": lambda s, x, u, v, L: [
            ("on_line", u, L), ("on_line", v, L), ("neq", u, v)],
        "entail": lambda s, x, u, v, L: [
            ("gt_sum_len", (u, x, x, v, u, v)),
            ("gt_sum_len", (u, x, u, v, x, v)),
            ("gt_sum_len", (x, v, u, v, u, x))],
        "make": lambda s, x, u, v, L: [(None, [("not_on_line", x, L)])],
    },
    # [SMT-bridge: coordinate model] The apex f of an equilateral
    # triangle on p q (|f─p| = |f─q| = |p─q|), built by proposition_1'
    # on the side of PQ opposite v, is off every line L through p and a
    # second point v with |p─v| = |q─v| and q off L. Soundness: suppose
    # f were on L. L meets PQ only at p (p is on both, q is off L, and
    # two distinct lines meet once), so f opposing v across PQ with both
    # segment ends on L forces p between f and v; then
    #   |f─v| = |f─p| + |p─v| = |p─q| + |q─v| = |f─q| + |q─v|,
    # equality in the triangle inequality for f q v, so q lies between f
    # and v — hence on L, contradicting q off L. Lean's SMT closes this
    # (the between-sum row and the equality case) silently. First use:
    # Book I.9 (f off AB and f off AC).
    "equilateral_apex_off_line": {
        "cite": "SMT-bridge (an equilateral apex is off a sideline "
                "through a base vertex)",
        "args": ("f", "p", "q", "v", "L", "PQ"),
        "premises": lambda s, f, p, q, v, L, PQ: [
            ("on_line", p, L), ("on_line", v, L), ("neq", p, v),
            ("not_on_line", q, L),
            ("dpl", p, q, PQ), ("opposing_sides", f, v, PQ)],
        "entail": lambda s, f, p, q, v, L, PQ: [
            ("eq_len", (f, p, p, q)), ("eq_len", (f, q, p, q)),
            ("eq_len", (p, v, q, v))],
        "make": lambda s, f, p, q, v, L, PQ: [
            (None, [("not_on_line", f, L)])],
    },
    # [logic: Lean's `use`] The existential witness f IS the existing
    # point b — a rename. The premises are exactly the goal's conjuncts
    # about b (so the step asserts nothing the store does not already
    # hold); the make restates them under the witness name f, which the
    # by_cases conclusion then carries to the outer state as the
    # eigenvariable of the case split. The line argument L carries the
    # witness's on-line fact (on_line b L → on_line f L): Book I.23's
    # goal has no such conjunct, but I.23''s does (f.onLine AB ∨
    # f.sameSide x AB, Prop23.lean:37), and Lean's euclid_finish closes
    # that disjunction's left half in the degenerate branches from
    # b.onLine AB — so it is part of what `use` discharges there.
    # First use: Book I.23's degenerate zero branch (`use b`,
    # Prop23.lean:15).
    # logic: Lean `use p` for an ∃ — call an existing point by the
    # witness's name. Joe at I.2 (2026-11-09): in the case a = c, the
    # witness l is just b ("labelling b = l"). The peq lets closure carry
    # every fact of p over to the new name.
    "name_point": {
        "cite": "logic: Lean `use` (an existing point as the witness)",
        "args": ("p",),
        "premises": lambda s, p: [],
        "make": lambda s, p: [("x", [("peq", "x", p)])],
    },
    "use_witness": {
        "cite": "logic: Lean `use` (the witness renamed)",
        "args": ("f", "b", "a", "d", "c", "e", "L"),
        "premises": lambda s, f, b, a, d, c, e, L: [
            ("neq", b, a), ("on_line", b, L)],
        "entail": lambda s, f, b, a, d, c, e, L: [
            ("eq_ang", (b, a, b, d, c, e))],
        "make": lambda s, f, b, a, d, c, e, L: [
            ("f", [("neq", "f", a), ("on_line", "f", L),
                   ("_eq_ang", "f", a, b, d, c, e)])],
    },
    # SystemE/Theory/Inferences/Diagrammatic.lean:187 — triple_incidence_1
    # verbatim:
    #   (a.onLine L) ∧ (a.onLine M) ∧ (a.onLine N) ∧ (b.onLine L) ∧
    #   (c.onLine M) ∧ (d.onLine N) ∧ (c.sameSide d L) ∧
    #   (b.sameSide c N) → ¬(b.sameSide d M)
    # ¬(b.sameSide d M) is rendered as the POSITIVE fact opposing_sides
    # b d M (same rendering as pasch_3_opposing): opposingSides carries
    # the two off-line parts (Relations.lean:13-14), which the axiom does
    # not state, so they are added as premises and the conclusion carries
    # exactly triple_incidence_1's content plus them. Cited per use (a
    # closure rule would add facts to every earlier replay; footprints are
    # pinned). First use: Book I.21 (lines AB, BD, BC meeting at b turn
    # c.sameSide d AB and a.sameSide d BC into opposing_sides a c BD).
    "triple_incidence_1": {
        "cite": "Diagrammatic.lean:187 + Relations.lean:13-14",
        "args": ("L", "M", "N", "a", "b", "c", "d"),
        "premises": lambda s, L, M, N, a, b, c, d: [
            ("on_line", a, L), ("on_line", a, M), ("on_line", a, N),
            ("on_line", b, L), ("on_line", c, M), ("on_line", d, N),
            ("same_side", c, d, L), ("same_side", b, c, N),
            ("not_on_line", b, M), ("not_on_line", d, M)],
        "make": lambda s, L, M, N, a, b, c, d: [
            (None, [("opposing_sides", b, d, M)])],
    },
    # SystemE/Theory/Inferences/Diagrammatic.lean:201 — triple_incidence_2
    # verbatim:
    #   (a.onLine L) ∧ (a.onLine M) ∧ (a.onLine N) ∧ (b.onLine L) ∧
    #   (c.onLine M) ∧ (d.onLine N) ∧ (c.sameSide d L) ∧
    #   ¬(b.sameSide d M) ∧ ¬(d.onLine M) ∧ (b ≠ a) → b.sameSide c N
    # ¬(b.sameSide d M) is rendered as the positive fact opposing_sides
    # b d M (stronger: it also carries b, d off M — Relations.lean:13-14),
    # the same rendering pasch_4 uses, because the engine has no negated
    # sameSide fact kind.
    "triple_incidence_2": {
        "cite": "Diagrammatic.lean:201",
        "args": ("L", "M", "N", "a", "b", "c", "d"),
        "premises": lambda s, L, M, N, a, b, c, d: [
            ("on_line", a, L), ("on_line", a, M), ("on_line", a, N),
            ("on_line", b, L), ("on_line", c, M), ("on_line", d, N),
            ("same_side", c, d, L), ("opposing_sides", b, d, M),
            ("not_on_line", d, M), ("neq", b, a)],
        "make": lambda s, L, M, N, a, b, c, d: [
            (None, [("same_side", b, c, N)])],
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
    elif kind == "gt_sum_len":
        ok = la_entails_gt_sum_len(s.la, *a)
    elif kind == "gt_ang":
        ok = la_entails_gt_ang(s.la, *a)
    elif kind == "pos_len":
        ok = s.la.entails({len_atom(*a): 1}, 0, ">")
    elif kind == "rt_ang":
        ok = s.la.entails(lin((ang_atom(*a), 1), (RT, -1)), 0, "=")
    else:
        ok = False
    if not ok:
        raise MissingPremise(i, rule, ("entail", kind, a),
                             "(arithmetic does not entail it)")
    return ("entail",) + q


def _fact_holds(s, fact):
    """Does `fact` already hold in s? Relational facts: present in the
    (closed) store. Metric facts: entailed by the linear-arithmetic store,
    exactly as the engine's own premise checks do (_entail_check). An
    FMTooLarge verdict is undecided, so it does NOT certify the fact.
    Used by apply_step's identification check (P-euclid-binding-clash-2).
    """
    k = fact[0]
    try:
        if k == "_eq_len":
            return la_entails_eq(s.la, *fact[1:])
        if k == "_eq_ang":
            return la_entails_eq_ang(s.la, *fact[1:])
        if k == "_lt_len":
            return la_entails_lt(s.la, *fact[1:])
        if k == "_gt_len":
            return la_entails_gt(s.la, *fact[1:])
        if k == "_gt_ang":
            return la_entails_gt_ang(s.la, *fact[1:])
        if k == "_gt_sum_len":
            return la_entails_gt_sum_len(s.la, *fact[1:])
        if k == "_lt_sum_sum_len":
            return la_entails_lt_sum_sum_len(s.la, *fact[1:])
        if k == "_lt_sum_rt_ang":
            return la_entails_lt_sum_rt_ang(s.la, *fact[1:])
    except FMTooLarge:
        return False  # undecided is not entailed
    if k == "either":
        # A disjunctive fact (Book I.23': f.onLine AB ∨ f.sameSide x AB,
        # Prop23.lean:37): holds if the disjunction itself is in the
        # store (carried out of a by_cases or made by the cited
        # proposition_23') or if either disjunct holds in this state.
        return fact in s.facts or any(_fact_holds(s, d) for d in fact[1:])
    if k == "_sum_ang":
        return ("sum_ang",) + tuple(fact[1:]) in s.facts
    if k == "_rt_ang":
        return ("right_ang",) + tuple(fact[1:]) in s.facts
    if k == "_sum_rt_ang":
        # ∠(u:v:x) + ∠(x:v:w) = 2∟ — an LA query, not a stored fact
        u, v, x, w = fact[1:5]
        try:
            return s.la.entails(lin((ang_atom(u, v, x), 1),
                                    (ang_atom(x, v, w), 1), (RT, -2)), 0, "=")
        except FMTooLarge:
            return False  # undecided is not entailed
    return fact in s.facts


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
    # Review (claude-9): a made fact names its fresh object by the rule's
    # base name ("d" in proposition_1). If an ARGUMENT has that same name
    # (Book I.11: proposition_1 d e AB as f), substituting the base would
    # also rename the student's own point: |df| = |de| became |ff| = |fe|.
    # Re-make with collision-free placeholders, substitute, then restore.
    bases = {b for b, _ in made if b is not None}
    restore = {}
    if bases & {a for a in args if isinstance(a, str)}:
        ph = []
        for k, a in enumerate(args):
            if isinstance(a, str) and a in bases:
                tok = f"\x00arg{k}"
                restore[tok] = a
                ph.append(tok)
            else:
                ph.append(a)
        made = spec["make"](s, *ph)
    # Resolve each fresh object's name (binding, or an auto-minted one).
    # A binding to a name that already names an object is an
    # IDENTIFICATION claim (P-euclid-binding-clash-2, claude-9): the step
    # is existential, so the claim is sound iff the existing object is a
    # witness — every fact the step would add, with the base replaced by
    # the name, must already hold (relational facts present in the store,
    # metric facts entailed by the arithmetic, via _fact_holds). Accepted:
    # the step adds nothing (the made facts are skipped below). Refused:
    # MissingPremise(("fresh", name)) — without this the new point is
    # silently identified with the old one (|ab| = |bb|-style rows) and
    # the store goes unsat, after which everything is entailed. A name
    # only RESERVED (h-sm input about a not-yet-built object) may take
    # its one binding here.
    # Pass 1: a name for every fresh object (binding, or auto-minted).
    fresh_names = []
    for base, facts in made:
        if base is None:
            fresh_names.append(None)
            continue
        name = bindings.get(base)
        if name is None:
            name = s.new(base)
        fresh_names.append(name)
    # Review (claude-9, after 5dd4348): two fresh objects bound to ONE name
    # (intersections_circle_line as (e, e)) identified them with each other
    # and added neq e e: unsat store, everything entailed. Refused.
    named = [n for n in fresh_names if n is not None]
    for n in named:
        if named.count(n) > 1:
            raise MissingPremise(
                i, rule, ("fresh", n),
                f"(the step makes {named.count(n)} new objects and names "
                f"them all {n!r}; each new object needs its own name)")
    # Substitute EVERY fresh base in every made fact: a rule with two fresh
    # objects (intersections_circle_line: e, g and their a ≠ b) names the
    # other fresh object in a None-base fact, and the own-base-only
    # substitution left those as the rule's literal names (superposition
    # escaped this only because its replays bind b'/c' identically).
    namemap = {base: name for (base, _), name in zip(made, fresh_names)
               if base is not None}

    def _concrete(f):
        # Recurse: a made fact may nest tuples (the "either" disjuncts of
        # proposition_23'), and the fresh base name occurs inside them.
        def _m(x):
            if isinstance(x, tuple):
                return tuple(_m(y) for y in x)
            if isinstance(x, str):
                y = namemap.get(x, x)   # fresh base -> its bound name
                return restore.get(y, y)  # collision placeholder -> arg
            return x
        return tuple(_m(y) for y in f)

    # Pass 2: a binding to a DECLARED name is an identification claim
    # (P-euclid-binding-clash-2). Accepted iff every fact the step states
    # about that object holds already: its own facts AND (review, claude-9)
    # every base-less fact naming it, e.g. a ≠ b when a is reused. All
    # names are concretised with the full namemap, not only the own base.
    identified = [n if n is not None and n in s.declared else None
                  for n in fresh_names]
    ident_set = {n for n in identified if n is not None}
    for (base, facts), name, ident in zip(made, fresh_names, identified):
        if base is None:
            checks = [_concrete(f) for f in facts
                      if ident_set & {namemap.get(x) for x in f
                                      if isinstance(x, str)}]
            who = next((namemap.get(x) for f in facts for x in f
                        if isinstance(x, str) and namemap.get(x) in ident_set),
                       None)
        elif ident is not None:
            checks = [_concrete(f) for f in facts]
            who = name
        else:
            continue
        if not all(_fact_holds(s, f) for f in checks):
            raise MissingPremise(
                i, rule, ("fresh", who),
                f"(the step would identify a fresh object with the "
                f"existing {who!r}, but not every fact it states about "
                f"{who!r} already holds; give the new object a new name)")
    # all names validated: declare the genuinely new ones and consume
    # their reservations
    for name, ident in zip(fresh_names, identified):
        if name is not None and ident is None:
            s.reserved.discard(name)  # h-sm reservation consumed, once
            s.declared.add(name)
    added = []
    for (base, facts), name, ident in zip(made, fresh_names, identified):
        if ident is not None:
            continue  # identification accepted: the step adds nothing
        for fact in facts:
            if base is None and ident_set & {namemap.get(x) for x in fact
                                             if isinstance(x, str)}:
                continue  # checked above: already holds, adds nothing
            fact = _concrete(fact)
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
                s.la.add(lin((len_atom(fact[1], fact[2]), 1),
                             (len_atom(fact[3], fact[4]), -1)), 0, "<")
                s.add(("lt_len", *fact[1:]),
                      f"{rule} [{spec['cite']}]", prem_facts)
                added.append(("lt_len", *fact[1:]))
            elif fact[0] == "_gt_len":
                s.la.add(lin((len_atom(fact[1], fact[2]), 1),
                             (len_atom(fact[3], fact[4]), -1)), 0, ">")
                s.add(("gt_len", *fact[1:]),
                      f"{rule} [{spec['cite']}]", prem_facts)
                added.append(("gt_len", *fact[1:]))
            elif fact[0] == "_gt_ang":
                s.la.add(lin((ang_atom(fact[1], fact[2], fact[3]), 1),
                             (ang_atom(fact[4], fact[5], fact[6]), -1)),
                         0, ">")
                s.add(("gt_ang", *fact[1:]),
                      f"{rule} [{spec['cite']}]", prem_facts)
                added.append(("gt_ang", *fact[1:]))
            elif fact[0] == "_gt_sum_len":
                # |x─y| + |u─v| > |p─q| (Book I.20's conclusion shape):
                # the strict LA row plus the fact form.
                x, y, u, v, p, q = fact[1:7]
                s.la.add(lin((len_atom(x, y), 1), (len_atom(u, v), 1),
                             (len_atom(p, q), -1)), 0, ">")
                s.add(("gt_sum_len", *fact[1:7]),
                      f"{rule} [{spec['cite']}]", prem_facts)
                added.append(("gt_sum_len", *fact[1:7]))
            elif fact[0] == "_lt_sum_sum_len":
                # |x─y| + |u─v| < |p─q| + |r─s| (Book I.21's first
                # conjunct shape): the strict LA row plus the fact form.
                x, y, u, v, p, q, r, s2 = fact[1:9]
                s.la.add(lin((len_atom(x, y), 1), (len_atom(u, v), 1),
                             (len_atom(p, q), -1), (len_atom(r, s2), -1)),
                         0, "<")
                s.add(("lt_sum_sum_len", *fact[1:9]),
                      f"{rule} [{spec['cite']}]", prem_facts)
                added.append(("lt_sum_sum_len", *fact[1:9]))
            elif fact[0] == "_sum_ang":
                b, a0, c, d = fact[1:]
                s.la.add(lin((ang_atom(b, a0, c), 1),
                             (ang_atom(b, a0, d), -1),
                             (ang_atom(d, a0, c), -1)), 0, "=")
                s.add(("sum_ang", *fact[1:]),
                      f"{rule} [{spec['cite']}]", prem_facts)
                added.append(("sum_ang", *fact[1:]))
            elif fact[0] == "_rt_ang":
                s.la.add(lin((ang_atom(*fact[1:4]), 1), (RT, -1)), 0, "=")
                s.add(("right_ang", *fact[1:4]),
                      f"{rule} [{spec['cite']}]", prem_facts)
                added.append(("right_ang", *fact[1:4]))
            elif fact[0] == "_sum_rt_ang":
                u, v, x, w = fact[1:5]
                s.la.add(lin((ang_atom(u, v, x), 1), (ang_atom(x, v, w), 1),
                             (RT, -2)), 0, "=")
                s.add(("sum_rt_ang", *fact[1:5]),
                      f"{rule} [{spec['cite']}]", prem_facts)
                added.append(("sum_rt_ang", *fact[1:5]))
            elif fact[0] == "_lt_sum_rt_ang":
                # ∠u:v:w + ∠x:y:z < 2∟ (Book I.17's conclusion shape):
                # the strict LA row plus the fact form.
                u, v, w, x, y, z = fact[1:7]
                s.la.add(lin((ang_atom(u, v, w), 1), (ang_atom(x, y, z), 1),
                             (RT, -2)), 0, "<")
                s.add(("lt_sum_rt_ang", *fact[1:7]),
                      f"{rule} [{spec['cite']}]", prem_facts)
                added.append(("lt_sum_rt_ang", *fact[1:7]))
            elif fact[0] == "_zero_ang":
                # ∠x:v:y = 0 (degenerated angle): the LA row plus the
                # fact form.
                x, v, y = fact[1:4]
                s.la.add({ang_atom(x, v, y): 1}, 0, "=")
                s.add(("zero_ang", x, v, y),
                      f"{rule} [{spec['cite']}]", prem_facts)
                added.append(("zero_ang", x, v, y))
            elif fact[0] == "_flat_ang":
                # ∠x:v:y = ∟ + ∟ (the straight angle, flat_angle_onlyif
                # Transfer.lean:124): the LA row plus the fact form.
                x, v, y = fact[1:4]
                s.la.add(lin((ang_atom(x, v, y), 1), (RT, -2)), 0, "=")
                s.add(("flat_ang", x, v, y),
                      f"{rule} [{spec['cite']}]", prem_facts)
                added.append(("flat_ang", x, v, y))
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
    if s.has(("false",)):
        # a rule whose conclusion is False (e.g. proposition_7 cited as a
        # rule): the argument it stands for has reached a contradiction
        return True, (("false",),)
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
                          ("opposing_sides", fact[2], fact[1], fact[3]),
                          ("not_same_side", fact[1], fact[2], fact[3]),
                          ("not_same_side", fact[2], fact[1], fact[3])):
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
        s.la.add(lin((len_atom(fact[1], fact[2]), 1),
                     (len_atom(fact[3], fact[4]), -1)), 0, "<")
    elif fact[0] == "gt_len":
        s.la.add(lin((len_atom(fact[1], fact[2]), 1),
                     (len_atom(fact[3], fact[4]), -1)), 0, ">")
    elif fact[0] == "eq_ang":
        eq_ang(s.la, *fact[1:])
    elif fact[0] == "gt_ang":
        s.la.add(lin((ang_atom(fact[1], fact[2], fact[3]), 1),
                     (ang_atom(fact[4], fact[5], fact[6]), -1)), 0, ">")
    elif fact[0] == "gt_sum_len":
        # |x─y| + |u─v| > |p─q| as a hypothesis/assumption (the plain
        # tuple form of Book I.20's conclusion shape).
        x, y, u, v, p, q = fact[1:7]
        s.la.add(lin((len_atom(x, y), 1), (len_atom(u, v), 1),
                     (len_atom(p, q), -1)), 0, ">")
    elif fact[0] == "lt_sum_sum_len":
        # |x─y| + |u─v| < |p─q| + |r─s| as a hypothesis/assumption (the
        # plain tuple form of Book I.21's first conjunct shape).
        x, y, u, v, p, q, r, s2 = fact[1:9]
        s.la.add(lin((len_atom(x, y), 1), (len_atom(u, v), 1),
                     (len_atom(p, q), -1), (len_atom(r, s2), -1)), 0, "<")
    elif fact[0] == "_sum_rt_ang":
        # ∠(u:v:x) + ∠(x:v:w) = 2∟ as a hypothesis/assumption (Book I.14
        # states its angle-sum hypothesis in this shape).
        u, v, x, w = fact[1:5]
        s.la.add(lin((ang_atom(u, v, x), 1), (ang_atom(x, v, w), 1),
                     (RT, -2)), 0, "=")
    elif fact[0] == "_zero_ang":
        # ∠x:v:y = 0 as an assumption (Book I.23's inner by_cases on
        # ∠d:c:e = 0, Prop23.lean:14).
        x, v, y = fact[1:4]
        s.la.add({ang_atom(x, v, y): 1}, 0, "=")
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
        close(sub)  # assumptions must be closed before the steps run
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
    close(s)  # conclusions (e.g. peq) must reach their substitution rules
    return {"step": i, "rule": "by_contra",
            "branches": len(branches), "witnesses": witnesses,
            "conclusions": conclusions}


# ---------------------------------------------------------------------------
# by_cases: case split; every branch must ENTAIL each conclusion
# ---------------------------------------------------------------------------


class ByCasesFailure(Exception):
    def __init__(self, entry, branch, witness):
        super().__init__(
            f"by_cases branch {branch} failed: {witness}")
        self.entry, self.branch, self.witness = entry, branch, witness


def _conclude(s, c, why, witnesses):
    """Add a by_cases conclusion to the state. Metric tuples ("_eq_len",
    "_eq_ang", "_rt_ang", "_sum_rt_ang") add their row plus the fact form;
    anything else is a plain diagram fact."""
    if c[0] == "_eq_len":
        eq_len(s.la, *c[1:])
        s.add(("eq_len", *c[1:]), why, witnesses)
    elif c[0] == "_gt_len":
        # |x─y| > |u─v| carried out of a by_cases (Book I.24a's one-branch
        # split on d.sameSide g EF): the strict LA row plus the fact form.
        x, y, u, v = c[1:5]
        s.la.add(lin((len_atom(x, y), 1), (len_atom(u, v), -1)), 0, ">")
        s.add(("gt_len", *c[1:5]), why, witnesses)
    elif c[0] == "_eq_ang":
        eq_ang(s.la, *c[1:])
        s.add(("eq_ang", *c[1:]), why, witnesses)
    elif c[0] == "_rt_ang":
        s.la.add(lin((ang_atom(*c[1:4]), 1), (RT, -1)), 0, "=")
        s.add(("right_ang", *c[1:4]), why, witnesses)
    elif c[0] == "_sum_rt_ang":
        u, v, x, w = c[1:5]
        s.la.add(lin((ang_atom(u, v, x), 1), (ang_atom(x, v, w), 1),
                     (RT, -2)), 0, "=")
        s.add(("sum_rt_ang", *c[1:5]), why, witnesses)
    else:
        s.add(c, why, witnesses)


def _by_cases(s, i, entry):
    """entry = ("by_cases", branches, conclusions).
    branches: list of (assumption-facts, steps). Each branch runs on a copy
    of the current state plus its assumptions and must ENTAIL every
    conclusion (checked with _fact_holds: relational facts present in the
    closed branch state, metric facts entailed by its arithmetic). Unlike
    by_contra a branch does NOT prove anything by reaching a contradiction:
    a contradictory branch would entail everything vacuously, so it is
    reported as a failure. Classical exhaustiveness of the branch
    assumptions is the caller's responsibility (documented at each use
    site, as for by_contra). The outer state is unchanged unless every
    branch entails every conclusion; then the conclusions are added to the
    OUTER state (metric fact tuples add their row too)."""
    _, branches, conclusions = entry
    witnesses = []
    for facts, steps in branches:
        sub = s.copy()
        for f in facts:
            _assume_metric_or_fact(sub, f, "by_cases assumption")
        close(sub)  # assumptions must be closed before the steps run
        try:
            run_steps(sub, steps, i + 1)
        except MissingPremise as mp:
            # A stuck branch cannot claim the conclusions: report it as a
            # failed by_cases, naming the missing premise.
            raise ByCasesFailure(entry, facts, ("stuck", mp.premise)) from mp
        contra, w = contradictory(sub)
        if contra:
            raise ByCasesFailure(
                entry, facts,
                ("contradictory", w,
                 "a contradictory branch would entail everything vacuously"))
        for c in conclusions:
            if not _fact_holds(sub, c):
                raise ByCasesFailure(entry, facts, ("not-entailed", c))
        witnesses.append((facts, ["entailed: " + repr(c) for c in conclusions]))
    for c in conclusions:
        _conclude(s, c, "by_cases", witnesses)
    close(s)  # conclusions (e.g. metric rows) must reach the closure rules
    return {"step": i, "rule": "by_cases",
            "branches": len(branches), "witnesses": witnesses,
            "conclusions": conclusions}


def run_steps(s, steps, i0=1):
    """Driver loop shared by replay(), by_contra and by_cases branches.
    Steps are (rule, args, bindings), ("by_contra", branches, conclusions),
    or ("by_cases", branches, conclusions)."""
    rep = []
    for i, st in enumerate(steps, i0):
        if st[0] == "by_contra":
            rep.append(_by_contra(s, i, st))
        elif st[0] == "by_cases":
            rep.append(_by_cases(s, i, st))
        else:
            rep.append(apply_step(s, i, st[0], st[1], st[2]))
    return rep


def replay(hyps, steps, verify, exclude_rules=None):
    """Driver. hyps: list of (fact, why) hypotheses (already destructed, as
    euclid_intros would leave them); steps: list of (rule, args, bindings);
    verify: list of facts/queries the conclusion must satisfy (each is either a
    diagram fact tuple or a metric query such as ("_eq_len", x, y, u, v)).
    exclude_rules: optional set of FORWARD_RULES names to disable for this
    replay (threaded through close() on this state and every by_contra /
    by_cases copy of it); default None keeps the full rule set.
    Returns (state, report). Raises MissingPremise on the first stuck step,
    AssertionError if a final query fails."""
    s = State()
    if exclude_rules:
        s.exclude_rules = frozenset(exclude_rules)
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
        elif q[0] == "_eq_ang":
            assert la_entails_eq_ang(s.la, *q[1:]), f"conclusion not entailed: {q}"
        elif q[0] == "_gt_len":
            assert la_entails_gt(s.la, *q[1:]), f"conclusion not entailed: {q}"
        elif q[0] == "_gt_ang":
            assert la_entails_gt_ang(s.la, *q[1:]), f"conclusion not entailed: {q}"
        elif q[0] == "_gt_sum_len":
            assert la_entails_gt_sum_len(s.la, *q[1:]), \
                f"conclusion not entailed: {q}"
        elif q[0] == "_lt_sum_sum_len":
            assert la_entails_lt_sum_sum_len(s.la, *q[1:]), \
                f"conclusion not entailed: {q}"
        elif q[0] == "_lt_sum_rt_ang":
            assert la_entails_lt_sum_rt_ang(s.la, *q[1:]), \
                f"conclusion not entailed: {q}"
        elif q[0] == "_rt_ang":
            assert s.la.entails(lin((ang_atom(*q[1:4]), 1), (RT, -1)),
                                0, "="), f"conclusion not entailed: {q}"
        elif q[0] == "_sum_rt_ang":
            u, v, x, w = q[1:5]
            assert s.la.entails(lin((ang_atom(u, v, x), 1),
                                    (ang_atom(x, v, w), 1), (RT, -2)),
                                0, "="), f"conclusion not entailed: {q}"
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
    # h-sm (not h): these name the lines GB / FC, which the STEPS construct
    # later (line_from_points below); as h-sm their names are reserved for
    # that one binding instead of declared up front (P-euclid-binding-clash).
    (("not_on_line", "c", "GB"), "h-sm"),
    (("not_on_line", "b", "FC"), "h-sm"),
    (("neq_line", "AB", "AC"), "h"), (("neq_line", "AB", "GB"), "h-sm"),
    (("neq_line", "AC", "FC"), "h-sm"),
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
# proposition_4 (SAS) congruence. Lean's euclid_finish then closes by pure
# angle arithmetic: d lies strictly between the endpoints (between b d a,
# resp. between c d a), so the base angle splits around d —
# sum_angles_onlyif (SystemE/Theory/Inferences/Transfer.lean:90) gives
#   branch 1: ∠b:c:a = ∠b:c:d + ∠d:c:a  (same_side b d AC, same_side a d BC
#             are closure from between b d a via between_symm + Pasch_2;
#             not_on_line d AC/BC likewise),
#   branch 2: ∠c:b:a = ∠c:b:d + ∠d:b:a  (mirror),
# while proposition_4 gives ∠b:c:d = ∠c:b:a (resp. ∠c:b:d = ∠b:c:a) which by
# the hypothesis ∠a:b:c = ∠a:c:b equals the whole — forcing the part
# ∠d:c:a (resp. ∠d:b:a) to 0 against angle_pos, store-unsat. This is the
# I.18-free proof: the closure rule side_gt_angle_gt [SMT-bridge, Euclid
# I.18] (a later theorem) was first excluded from this replay via
# exclude_rules (E-kimi-task-388), then removed from default closure
# entirely (E-kimi-task-389) once I.18 itself replayed the Lean way.
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
            # ∠b:c:a = ∠b:c:d + ∠d:c:a (d strictly inside, Transfer.lean:90)
            ("sum_angles_onlyif", ("c", "b", "a", "d", "BC", "AC"), {}),
        ]),
        ([("gt_len", "a", "c", "a", "b")], [
            ("proposition_3", ("c", "a", "a", "b", "AC", "AB"), {"q": "d"}),
            ("line_from_points", ("d", "b"), {"L": "DB"}),
            ("proposition_4", ("c", "d", "b", "b", "a", "c",
                               "AC", "DB", "BC", "AB", "AC", "BC"), {}),
            # ∠c:b:a = ∠c:b:d + ∠d:b:a (d strictly inside, Transfer.lean:90)
            ("sum_angles_onlyif", ("b", "c", "a", "d", "BC", "AB"), {}),
        ]),
    ], [("eq_len", "a", "b", "a", "c")]),
]

# Book I.7 (Book/Prop07.lean): conclusion False. After CD is drawn, the two
# nested by_cases (a vs CD, d vs AC) give four branches; a,b are off CD and
# b,d off AC (derived in-state), so sameSide vs opposingSides is exhaustive
# (Relations.lean:13-14). Each branch's hypotheses are EXACTLY the two split
# facts; every further side fact the angle steps need (what Lean's
# euclid_finish derives silently) is derived inside the branch by cited
# System E diagrammatic axioms — triple_incidence_1/2
# (Diagrammatic.lean:187,201), pasch_3_opposing (Diagrammatic.lean:161) and
# opposing_opposing_same_side (derived, Diagrammatic.lean:127 double
# contrapositive). E-kimi-task-420: the four branches previously ASSUMED
# extra side facts (the old `extra` argument); review found none of them
# entailed. Two of them (same_side c a DB in branches 2 and 3) are worse
# than unentailed: their negation opposing_sides c a DB is DERIVABLE in
# those branches (triple_incidence_1 (CB,DB,AB) from the derived
# same_side d a CB / a d CB), so the old replay trusted the diagram for a
# fact false in the case. No branch step ever needed it; branches 2 and 3
# drop it, branches 1 and 4 derive the facts they do need.
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


def _prop7_branch(case_a, case_d, steps):
    # Hypotheses are EXACTLY the two split facts (a vs CD, d vs AC); every
    # other side fact is derived by a cited step inside the branch.
    return ([("same_side" if case_a else "opposing_sides", "a", "b", "CD"),
             ("same_side" if case_d else "opposing_sides", "d", "b", "AC")],
            steps)


PROP7_STEPS_DATA = [
    ("line_from_points", ("c", "d"), {"L": "CD"}),
    ("by_contra", [
        # branch 1: a sameSide b CD, d sameSide b AC
        _prop7_branch(True, True, [
            # c~a DB: c opposes b across AD (triple_incidence_1 over
            # AC/AD/AB at a: d~b AC is the case fact, c~d AB a hyp), then
            # triple_incidence_2 over CD/AD/DB at d turns a~b CD (case)
            # and c opposing b AD into c~a DB.
            ("triple_incidence_1", ("AC", "AD", "AB", "a", "c", "d", "b"),
             {}),
            ("triple_incidence_2", ("CD", "AD", "DB", "d", "c", "a", "b"),
             {}),
            ("proposition_5'", ("a", "c", "d", "AC", "CD", "AD"), {}),
            ("sum_angles_onlyif", ("d", "c", "b", "a", "CD", "DB"), {}),
            ("proposition_5'", ("b", "c", "d", "CB", "CD", "DB"), {}),
            ("sum_angles_onlyif", ("c", "a", "d", "b", "AC", "CD"), {}),
        ]),
        # branch 2: a sameSide b CD, d opposingSides b AC
        _prop7_branch(True, False, [
            # d~a CB: triple_incidence_2 over CD/AC/CB at c from a~b CD
            # (case) and d opposing b AC (case). c~b AD: opposing c a DB
            # (triple_incidence_1 over CB/DB/AB at b from d~a CB and the
            # c~d AB hyp), then triple_incidence_2 over CD/DB/AD at d.
            ("triple_incidence_2", ("CD", "AC", "CB", "c", "d", "a", "b"),
             {}),
            ("triple_incidence_1", ("CB", "DB", "AB", "b", "c", "d", "a"),
             {}),
            ("triple_incidence_2", ("CD", "DB", "AD", "d", "c", "b", "a"),
             {}),
            ("proposition_5'", ("a", "d", "c", "AD", "CD", "AC"), {}),
            ("sum_angles_onlyif", ("c", "d", "b", "a", "CD", "CB"), {}),
            ("proposition_5'", ("b", "d", "c", "DB", "CD", "CB"), {}),
            ("sum_angles_onlyif", ("d", "a", "c", "b", "AD", "CD"), {}),
        ]),
        # branch 3: a opposingSides b CD, d sameSide b AC
        _prop7_branch(False, True, [
            ("extend_point_not_on_line", ("AC", "CD", "a", "c"), {"x": "e"}),
            ("extend_point_not_on_line", ("AD", "CD", "a", "d"), {"x": "f"}),
            # b~e CD: e opposes a across CD (pasch_3_opposing on
            # between a c e, c on CD); b also opposes a (case), so b~e
            # (opposing_opposing_same_side). b~f CD likewise from
            # between a d f.
            ("pasch_3_opposing", ("a", "c", "e", "CD"), {}),
            ("opposing_opposing_same_side", ("b", "a", "e", "CD"), {}),
            ("pasch_3_opposing", ("a", "d", "f", "CD"), {}),
            ("opposing_opposing_same_side", ("b", "a", "f", "CD"), {}),
            # c~f DB: a~d CB (triple_incidence_2 over AC/CD/CB at c from
            # d~b AC (case) and a opposing b CD (case)), so c opposes a
            # across DB (triple_incidence_1 over CB/DB/AB at b); f also
            # opposes a across DB (pasch_3_opposing on between a d f), so
            # c~f (opposing_opposing_same_side).
            ("triple_incidence_2", ("AC", "CD", "CB", "c", "a", "d", "b"),
             {}),
            ("triple_incidence_1", ("CB", "DB", "AB", "b", "c", "d", "a"),
             {}),
            ("pasch_3_opposing", ("a", "d", "f", "DB"), {}),
            ("opposing_opposing_same_side", ("c", "a", "f", "DB"), {}),
            ("proposition_5", ("a", "c", "d", "e", "f", "AC", "CD", "AD"),
             {}),
            ("sum_angles_onlyif", ("c", "e", "d", "b", "AC", "CD"), {}),
            ("proposition_5'", ("b", "c", "d", "CB", "CD", "DB"), {}),
            ("sum_angles_onlyif", ("d", "c", "b", "f", "CD", "DB"), {}),
        ]),
        # branch 4: a opposingSides b CD, d opposingSides b AC
        _prop7_branch(False, False, [
            ("extend_point_not_on_line", ("AC", "CD", "a", "c"), {"x": "e"}),
            ("extend_point_not_on_line", ("AD", "CD", "a", "d"), {"x": "f"}),
            # b~e CD: e opposes a across CD (pasch_3_opposing), b opposes
            # a (case), so b~e (opposing_opposing_same_side).
            ("pasch_3_opposing", ("a", "c", "e", "CD"), {}),
            ("opposing_opposing_same_side", ("b", "a", "e", "CD"), {}),
            # d~e CB: triple_incidence_2 over CD/AC/CB at c from b~e CD
            # (just derived) and d opposing b AC (case).
            ("triple_incidence_2", ("CD", "AC", "CB", "c", "d", "e", "b"),
             {}),
            # c~b AD: b opposes d across AC (triple_incidence_1 over
            # CB/AC/CD at c from d~e CB and b~e CD) — the case fact in the
            # order triple_incidence_2 needs — then triple_incidence_2 over
            # AB/AC/AD at a with the c~d AB hyp.
            ("triple_incidence_1", ("CB", "AC", "CD", "c", "b", "e", "d"),
             {}),
            ("triple_incidence_2", ("AB", "AC", "AD", "a", "b", "c", "d"),
             {}),
            # b~f CD: f opposes a across CD (pasch_3_opposing on
            # between a d f), b opposes a (case), so b~f.
            ("pasch_3_opposing", ("a", "d", "f", "CD"), {}),
            ("opposing_opposing_same_side", ("b", "a", "f", "CD"), {}),
            ("proposition_5", ("a", "c", "d", "e", "f", "AC", "CD", "AD"),
             {}),
            ("sum_angles_onlyif", ("d", "f", "c", "b", "AD", "CD"), {}),
            ("proposition_5'", ("b", "d", "c", "DB", "CD", "CB"), {}),
            ("sum_angles_onlyif", ("c", "d", "b", "e", "CD", "CB"), {}),
        ]),
    ], [("false",)]),
]


# Book I.8 (Book/Prop08.lean): SSS. Superposition maps triangle (b,c,a) with
# apex b onto (e, b', c') over base EF (b' is the image of c, c' the image of
# a). Lean's euclid_assert c' = f is assert_point_eq_on_ray: b' and f are on
# EF at equal distance and ¬between b' e f ships with the bundle. Lean's
# by_cases d = g becomes a one-branch by_contra on d ≠ c' citing
# proposition_7 (its sameSide premise is exactly superposition's
# c'.sameSide d EF); the conclusion peq d c' then substitutes through the
# bundle's ∠b:a:c = ∠e:c':b' to give ∠b:a:c = ∠e:d:f.
PROP8_HYPS_DATA = [
    (("dpl", "a", "b", "AB"), "h"),
    (("on_line", "b", "BC"), "h"), (("on_line", "c", "BC"), "h"),
    (("on_line", "c", "AC"), "h"), (("on_line", "a", "AC"), "h"),
    (("not_on_line", "c", "AB"), "h"),  # formTriangle a b c
    (("dpl", "d", "e", "DE"), "h"),
    (("on_line", "e", "EF"), "h"), (("on_line", "f", "EF"), "h"),
    (("on_line", "f", "DF"), "h"), (("on_line", "d", "DF"), "h"),
    (("not_on_line", "f", "DE"), "h"),  # formTriangle d e f
    (("eq_len", "a", "b", "d", "e"), "h"),
    (("eq_len", "a", "c", "d", "f"), "h"),
    (("eq_len", "b", "c", "e", "f"), "h"),
]
PROP8_STEPS_DATA = [
    ("superposition", ("b", "c", "a", "e", "f", "d", "BC", "AC", "AB", "EF"),
     {"b'": "b'", "c'": "c'", "BC'": "CG", "AC'": "EG"}),
    # Lean's euclid_assert c' = f: b' and f are both on EF, ¬between b' e f
    # comes with the superposition bundle, and |e-b'| = |e-f| follows from
    # |b-c| = |e-b'| (bundle) and |b-c| = |e-f| (hypothesis).
    ("assert_point_eq_on_ray", ("b'", "f", "e", "EF"), {}),
    # Lean's by_cases d = g: the d ≠ c' case cites proposition_7 on the
    # SSS triangle (e, f | d, c'), whose sameSide premise is exactly the
    # superposition's c'.sameSide d EF.
    ("by_contra", [
        ([("neq", "d", "c'")], [
            ("proposition_7", ("e", "f", "d", "c'", "EF", "DE", "DF",
                               "EG", "CG"), {}),
        ]),
    ], [("peq", "d", "c'")]),
]


# Book I.9 (Book/Prop09.lean): bisect an angle (vertex renamed v: the
# construction point_between_points_shorter_than introduces a base name "a"
# that would collide). d on AB inside with
# |a-d| < |a-c|; e on AC with |a-e| = |a-d| (proposition_3); f the
# equilateral apex on DE opposite a (proposition_1'); proposition_8 (SSS)
# gives ∠d:a:f = ∠e:a:f, and angle_ray_eq transports it to ∠b:a:f = ∠c:a:f.
# SMT-sourced diagrammatic facts Lean's euclid_finish/SMT supplies (flagged
# in the bell-back): the witness d is chosen non-degenerate (a ≠ d) and
# d ≠ e; and Lean's two inline "have ¬(f.onLine AB/AC)" proofs (proposition_5'
# + proposition_5 + ray case analysis) are stated as inputs — the engine's
# bridges do not cover the ray-side split at d.
PROP9_HYPS_DATA = [
    (("dpl", "v", "b", "AB"), "h"),  # formRectilinearAngle b v c AB AC
    (("dpl", "v", "c", "AC"), "h"),
    (("neq_line", "AB", "AC"), "h"),
    (("neq", "v", "d"), "h-sm"),     # witness choice (see comment)
    (("neq", "d", "e"), "h-sm"),     # derivable by contra; SMT-sourced
    (("not_on_line", "f", "AB"), "h-sm"),  # Lean's inline have #1
    (("not_on_line", "f", "AC"), "h-sm"),  # Lean's inline have #2
]
PROP9_STEPS_DATA = [
    ("point_between_points_shorter_than", ("AB", "v", "b", "v", "c"),
     {"a": "d"}),
    ("proposition_3", ("v", "c", "v", "d", "AC", "AB"), {"q": "e"}),
    ("line_from_points", ("d", "e"), {"L": "DE"}),
    ("proposition_1'", ("d", "e", "v", "DE"), {"c": "f"}),
    ("line_from_points", ("f", "e"), {"L": "FE"}),
    ("line_from_points", ("f", "d"), {"L": "FD"}),
    ("line_from_points", ("v", "f"), {"L": "AF"}),
    ("proposition_8", ("v", "d", "f", "v", "e", "f", "AB", "FD", "AF",
                       "AC", "FE", "AF"), {}),
]


# Book I.10 (Book/Prop10.lean): bisect a segment. c is the equilateral apex
# on AB (proposition_1); d' bisects the angle at c with its side conclusions
# (proposition_9'); the same_side branch for (a,b,CD) dies on proposition_7
# after proposition_4 (SAS on c-a-d' / c-b-d') yields |a-d'| = |b-d'|, so a
# and b oppose across CD; intersection_lines (via intersection_lines_opposing)
# lands d on CD and AB; pasch_4 gives between a d b; angle_pair_on_line moves
# the bisector angle from d' to d, and proposition_4 (SAS) closes |a-d|=|b-d|.
# SMT-sourced input Lean derives (flagged in the bell-back): c off AB — the
# equilateral apex is off its base (Lean's proposition_1 concludes only the
# two lengths; the non-collinearity is SMT). Everything else is derived:
# a off BC and b off AC follow from c off AB (two_lines_meet_once at b, the
# sole common point of AB and BC), so AC ≠ BC (line_neq); and once d' is
# constructed, d' off AC/BC (same_side parts) give CD ≠ AC/BC, whence
# a and b off CD (two_lines_meet_once at c). With a, b off CD the
# sameSide/opposingSides split of the by_contra below is exhaustive
# (Relations.lean:13-14, as in the I.7 replay).
PROP10_HYPS_DATA = [
    (("dpl", "a", "b", "AB"), "h"),  # distinctPointsOnLine a b AB
    (("not_on_line", "c", "AB"), "h-sm"),  # equilateral apex off the base
]
PROP10_STEPS_DATA = [
    ("proposition_1", ("a", "b", "AB"), {"d": "c"}),
    ("line_from_points", ("c", "a"), {"L": "AC"}),
    ("line_from_points", ("c", "b"), {"L": "BC"}),
    ("proposition_9'", ("c", "a", "b", "AC", "BC"), {"f": "d'"}),
    # c ≠ d': else d' = c lies on AC, but same_side d' b AC puts d' off AC.
    ("by_contra", [
        ([("peq", "c", "d'")], []),
    ], [("neq", "c", "d'")]),
    ("line_from_points", ("c", "d'"), {"L": "CD"}),
    # a ≠ d' and b ≠ d' for the line constructions below: peq a d' would put
    # a on CD against not_on_line a CD (derived: CD ≠ AC at c, a on AC).
    # Same for b.
    ("by_contra", [
        ([("peq", "a", "d'")], []),
    ], [("neq", "a", "d'")]),
    ("by_contra", [
        ([("peq", "b", "d'")], []),
    ], [("neq", "b", "d'")]),
    ("line_from_points", ("a", "d'"), {"L": "AD'"}),
    ("line_from_points", ("b", "d'"), {"L": "BD'"}),
    # SAS on (c,a,d') vs (c,b,d'): |c-a| = |c-b| (equilateral), |c-d'|
    # common, ∠a:c:d' = ∠b:c:d' (proposition_9') ⇒ |a-d'| = |b-d'|.
    ("proposition_4", ("c", "a", "d'", "c", "b", "d'",
                       "AC", "AD'", "CD", "BC", "BD'", "CD"), {}),
    # a, b off CD (derived: d' off AC/BC give CD ≠ AC/BC, then
    # two_lines_meet_once at c), so same_side a b CD is exhaustive against
    # opposing_sides; in the same_side branch proposition_7 fires on the
    # SSS pair (a, b) over base (c, d'): |c-a| = |c-b|, |a-d'| = |b-d'|.
    ("by_contra", [
        ([("same_side", "a", "b", "CD")], [
            ("proposition_7", ("c", "d'", "a", "b", "CD", "AC", "AD'",
                               "BC", "BD'"), {}),
        ]),
    ], [("opposing_sides", "a", "b", "CD")]),
    # closure now yields neq_line CD AB (line_neq: c on CD, c off AB).
    ("intersection_lines_opposing", ("a", "b", "CD", "AB"), {}),
    ("intersection_lines", ("CD", "AB"), {"a": "d"}),
    # neq a d, neq b d come from off_line_neq (a, b off CD; d on CD).
    ("pasch_4", ("a", "d", "b", "CD", "AB"), {}),
    # carry ∠a:c:d' = ∠b:c:d' onto d (d, d', c on CD; needs neq c d,
    # which off_line_neq gives: c off AB, d on AB).
    ("angle_pair_on_line", ("a", "b", "c", "d'", "d", "CD"), {}),
    ("proposition_4", ("c", "a", "d", "c", "b", "d",
                       "AC", "AB", "CD", "BC", "AB", "CD"), {}),
]


# Book I.11 (Book/Prop11.lean): erect a perpendicular at c on AB. d on AB
# toward a with |c-d| < |c-b|; e on AB toward b with |c-e| = |c-d|
# (proposition_3); f the equilateral apex on de (proposition_1);
# proposition_8 (SSS on c-d-f / c-e-f) gives ∠d:c:f = ∠e:c:f; the rays
# c→d ≡ c→a and c→e ≡ c→b (angle_ray_eq from the two between facts) carry
# it to ∠a:c:f = ∠f:c:b, and perpendicular_if concludes ∠a:c:f = RT.
# Lean cites perpendicular_if as (d e c f AB); the engine cites it as
# (a b c f AB) — the same axiom with the base endpoints instead of the
# equal-distance points, because the engine has no rule for "c between d
# and e" (d, e on opposite rays at equal distance: between c d a,
# between c e b, between a c b), while between a c b is a hypothesis.
# SMT-sourced input Lean derives (flagged in the bell-back):
#   not_on_line f AB — the equilateral apex on ge is off its base (as in
#   I.10). Everything else is derived or cited: the non-degeneracies
#   u≠w, w≠v, w≠g come from between_symm_parts (Diagrammatic.lean:45-46,
#   cited per use so earlier footprints stay pinned), and g≠e follows in
#   the store (between-order rows give |ge| = |gw| + |we| with |gw| > 0).
PROP11_HYPS_DATA = [
    (("dpl", "u", "v", "AB"), "h"),  # distinctPointsOnLine a b AB
    (("between", "u", "w", "v"), "h"),
    (("not_on_line", "f", "AB"), "h-sm"),
]
# Point names are renamed from Lean's (a b c d e f -> u v w g e f): the
# substitution in apply_step replaces the rule's base name in every slot
# of a made fact, and bases "a" (point_between_points_shorter_than,
# proposition_1), "q" (proposition_3) and "d" (proposition_1) would also
# rewrite our own points of the same name (I.9 hit the same collision).
PROP11_STEPS_DATA = [
    ("between_symm_parts", ("u", "w", "v"), {}),   # u≠w, u≠v
    ("between_symm_parts", ("v", "w", "u"), {}),   # w≠v (between v w u via symm)
    ("point_between_points_shorter_than", ("AB", "w", "u", "w", "v"),
     {"a": "g"}),
    ("between_symm_parts", ("w", "g", "u"), {}),   # w≠g, w≠u
    ("proposition_3", ("w", "v", "w", "g", "AB", "AB"), {"q": "e"}),
    ("proposition_1", ("g", "e", "AB"), {"d": "f"}),
    ("line_from_points", ("g", "f"), {"L": "DF"}),
    ("line_from_points", ("f", "e"), {"L": "FE"}),
    ("line_from_points", ("f", "w"), {"L": "FC"}),
    ("proposition_8", ("w", "g", "f", "w", "e", "f",
                       "AB", "DF", "FC", "AB", "FE", "FC"), {}),
    ("perpendicular_if", ("u", "v", "w", "f", "AB"), {}),
]


# Book I.12 (Book/Prop12.lean): drop a perpendicular from c (off AB) onto
# AB. d opposite c across AB (exists_point_opposite); circle EFG centre c
# through d; AB meets EFG at e, g (intersections_circle_line, its
# intersectsCircle premise cited from intersection_circle_line_1 — Lean's
# SMT closes that premise; the engine cites the SystemE axiom, Diagrammatic.
# lean:297, so this replay has NO h-sm input: every premise of every step
# is either a hypothesis, a cited rule, or closure-derived). proposition_10
# bisects eg at h (between e h g, |e-h| = |h-g|); proposition_8 (SSS on
# h-c-g / h-c-e: |h-g| = |h-e| from the bisection, |c-g| = |c-e| radii,
# |h-c| common) gives ∠c:h:g = ∠c:h:e; perpendicular_if concludes
# ∠e:h:c = RT.
# GOAL SUBSTITUTE (stated per the packet): Lean's goal is the DISJUNCTION
# ∠ a:h:c = ∟ ∨ ∠ b:h:c = ∟ — h may coincide with a or b, and System E
# gives an angle with coinciding points no value, so neither disjunct is
# stated unconditionally. The engine has no disjunction fact (none is
# invented here). Instead the replay proves ∠e:h:c = ∟, which implies
# Lean's goal: h and e both lie on AB with e ≠ h, so hc ⊥ AB at h; for
# any point x on AB with x ≠ h the angle ∠x:h:c is the angle between hc
# and a ray of AB, hence ∟. Since a ≠ b at least one of a, b differs
# from h, and that disjunct holds.
# Derived (pinned by test_prop12_derived_facts), NOT inputs: neq c d
# (opposing_parts), dpl e g AB (r_dpl from the made on_lines + neq),
# on_line h AB (between_same_line_in), neq c g/h/e (off_line_neq),
# neq_line CH AB (line_neq), not_on_line g CH / e CH (two_lines_meet_once
# at h; neq e h / neq g h come from pos_neq off the between-order row
# |eg| = |eh| + |hg| with the bisection's |eh| = |hg| and |eg| > 0),
# radii |c-g| = |c-e| (poc_onlyif).
PROP12_HYPS_DATA = [
    (("dpl", "a", "b", "AB"), "h"),  # distinctPointsOnLine a b AB
    (("not_on_line", "c", "AB"), "h"),
]
PROP12_STEPS_DATA = [
    ("exists_point_opposite", ("AB", "c"), {"a": "d"}),
    ("circle_from_points", ("c", "d"), {"C": "EFG"}),
    ("intersection_circle_line_1", ("d", "c", "EFG", "AB"), {}),
    ("intersections_circle_line", ("EFG", "AB"), {"a": "e", "b": "g"}),
    ("proposition_10", ("e", "g", "AB"), {"d": "h"}),
    ("line_from_points", ("c", "g"), {"L": "CG"}),
    ("line_from_points", ("c", "h"), {"L": "CH"}),
    ("line_from_points", ("c", "e"), {"L": "CE"}),
    ("proposition_8", ("h", "c", "g", "h", "c", "e",
                       "CH", "CG", "AB", "CH", "CE", "AB"), {}),
    ("perpendicular_if", ("e", "g", "h", "c", "AB"), {}),
]


# Book I.13 (Book/Prop13.lean:6): angles on a straight line.
# theorem proposition_13 : AB ≠ CD ∧ distinctPointsOnLine a b AB ∧
#   distinctPointsOnLine c d CD ∧ between d b c → ∠ c:b:a + ∠ a:b:d = ∟ + ∟
#
# The old closure rule "straight_angle [SMT-bridge]" was I.13 itself
# (between u v w, x off the line ⇒ ∠u:v:x + ∠x:v:w = 2∟); it is REMOVED
# from the default closure (P-euclid-straight-angle-out) and the theorem
# is available to later propositions only as the cited lemma
# CONSTRUCTIONS["proposition_13"]. The proof here follows Lean's shape
# (by_cases on
# ∠c:b:a = ∠a:b:d; in the ≠ case erect the perpendicular be by I.11' and let
# the angle arithmetic close the goal) rather than taking the equality as an
# SMT fact.
PROP13_HYPS_DATA = [
    # AB ≠ CD in BOTH directions: the engine's neq_line is directed (rules
    # match the stated order; two_lines_meet_once reads CD ≠ AB below).
    (("neq_line", "AB", "CD"), "h"),
    (("neq_line", "CD", "AB"), "h"),
    (("dpl", "a", "b", "AB"), "h"),   # distinctPointsOnLine a b AB
    (("dpl", "c", "d", "CD"), "h"),   # distinctPointsOnLine c d CD
    (("between", "d", "b", "c"), "h"),
]
# The goal row, shared as the conclusions of both by_cases levels:
# ∠c:b:a + ∠a:b:d = 2∟ (checked per branch by LA entailment, then added).
PROP13_GOAL = ("_sum_rt_ang", "c", "b", "a", "d")

# Branch 2 of Lean's split (∠c:b:a ≠ ∠a:b:d): erect the perpendicular.
# proposition_11' d c b a CD as e: same_side e a CD ∧ ∠d:b:e = ∟
# (Prop11.lean:24 with (a,b,c,x,AB) := (d,c,b,a,CD)); line_from_points b e
# as BE. Then a second by_cases on where a lies relative to BE.
# EXHAUSTIVENESS of that inner split (caller's responsibility, as for
# by_contra): the outer branch's hypothesis is ∠c:b:a ≠ ∠a:b:d. If a were
# ON BE, then AB = BE (two lines through a ≠ b), and BE ⊥ CD at b makes
# both angles right — contradicting the hypothesis. So a is off BE, and
# then a is on c's side or on d's side of BE (System E trichotomy; c and d
# are on opposite sides of BE since between d b c with b on BE, pasch_3).
# The engine has no angle-DISEQUALITY fact, so the outer branch-2
# assumption list is empty and the hypothesis is carried by this comment.
def _prop13_branch(side, other):
    # side: the point whose side of BE a is on ("c" or "d"); other: the
    # opposite endpoint of CD. Geometry (drawn for side="c"): e same side of
    # CD as a with ∠d:b:e = ∟; ray ba inside ∠c:b:e, and ray be inside
    # ∠a:b:d. Steps:
    #   1. pasch_3_opposing: between d b c, b on BE, c, d off BE (derived:
    #      two_lines_meet_once at b from BE ≠ CD) ⇒ opposing_sides d c BE.
    #   2. opposing_same_side_trans with same_side a c BE (branch
    #      assumption) ⇒ opposing_sides d a BE.
    #   3. triple_incidence_2 at b over (CD, BE, AB) ⇒ same_side d e AB.
    #      This is the fact Lean's SMT produces silently; System E reaches
    #      it only through the two negation-mediated steps above.
    #   4. sum_angles_onlyif: ∠c:b:e = ∠c:b:a + ∠a:b:e (splitter a).
    #   5. perpendicular_onlyif on between d b c: ∠d:b:e = ∟ ⇒ ∠e:b:c = ∟.
    #   6. sum_angles_onlyif: ∠a:b:d = ∠a:b:e + ∠e:b:d (splitter e; its
    #      side premises are same_side a e CD (I.11') and same_side d e AB
    #      (step 3)).
    # LA closes ∠c:b:a + ∠a:b:d = 2∟ from those rows and ∠d:b:e = ∟.
    # (side="d" is the mirror image: roles of c and d swapped, using
    # between c b d from between_symm closure.)
    return ([("same_side", "a", side, "BE")], [
        # neq b c / neq b d / neq c d are cited from the between (its parts,
        # Diagrammatic.lean:45-46); the engine's closure does not derive
        # neqs from between, and two_lines_meet_once needs them to put c, d
        # off BE.
        ("between_symm_parts", (other, "b", side), {}),
        ("between_symm_parts", (side, "b", other), {}),
        ("pasch_3_opposing", (other, "b", side, "BE"), {}),
        ("opposing_same_side_trans", (other, side, "a", "BE"), {}),
        ("triple_incidence_2", ("CD", "BE", "AB", "b", other, "e", "a"), {}),
        ("sum_angles_onlyif", ("b", side, "e", "a", "CD", "BE"), {}),
        # ∠d:b:e = ∟ is the right angle I.11' constructs (in BOTH branches);
        # perpendicular_onlyif moves it across b: ∠e:b:c = ∟.
        ("perpendicular_onlyif", ("d", "c", "b", "e", "CD"), {}),
        ("sum_angles_onlyif", ("b", "a", other, "e", "AB", "CD"), {}),
    ])


PROP13_STEPS_DATA = [
    ("by_cases", [
        # branch 1: ∠c:b:a = ∠a:b:d (Lean's first by_cases branch:
        # euclid_finish). perpendicular_if (Transfer.lean:102) with
        # between c b d (between_symm of the hypothesis) makes each angle
        # right; LA adds them to 2∟.
        ([("eq_ang", "c", "b", "a", "a", "b", "d")], [
            ("perpendicular_if", ("c", "d", "b", "a", "CD"), {}),
        ]),
        # branch 2: ∠c:b:a ≠ ∠a:b:d — see the exhaustiveness comment above.
        ([], [
            ("proposition_11'", ("d", "c", "b", "a", "CD"), {"f": "e"}),
            ("line_from_points", ("b", "e"), {"L": "BE"}),
            ("by_cases", [
                _prop13_branch("c", "d"),
                _prop13_branch("d", "c"),
            ], [PROP13_GOAL]),
        ]),
    ], [PROP13_GOAL]),
]


# ---------------------------------------------------------------------------
# Book I.14 (Book/Prop14.lean:6) — if ∠a:b:c + ∠a:b:d = 2∟ with c, d on
# opposing sides of AB, then BC = BD (c and d lie on one line through b).
#
# REPRESENTATION of the conclusion "BC = BD": option (a) of the packet —
# the engine has no line-EQUALITY fact kind, so the goal is the by_contra
# whose single branch assumes ("neq_line", "BC", "BD") and must reach a
# contradiction; the outer conclusions are the mutual-incidence facts
# on_line d BC and on_line c BD. For lines through the two distinct points
# b, c (resp. b, d) that is exactly what BC = BD says
# (two_points_determine_line, Diagrammatic.lean:14 — the same theorem the
# neq_line machinery is the contrapositive of). A new "eq_line" fact kind
# (option (b)) was rejected: every forward rule matching on line names
# would have to be quotiented by it, and nothing else in the engine needs
# line equality.
# SOUNDNESS: no forward rule can derive anything from these conclusions
# that BC = BD would not justify — on_line d BC and on_line c BD are
# precisely the facts the state would hold if the lines had been one line
# from the start.
#
# HYPOTHESES: euclid_intros of Prop14.lean:6, destructed. No "h-sm" inputs
# and no extra "h" facts: the neq_lines the proof needs are DERIVED in
# closure — opposing_sides c d AB puts c, d off AB (opposing_parts), so
# line_neq gives BC ≠ AB and BD ≠ AB (c, d resp.), and then
# two_lines_meet_once at b puts a off BC and off BD (a ≠ b from the dpl),
# so line_neq also gives AB ≠ BC and AB ≠ BD (the directed forms
# proposition_13 and sum_angles_onlyif match on).
# The angle-sum hypothesis ∠a:b:c + ∠a:b:d = ∟+∟ is the metric tuple
# ("_sum_rt_ang", "c", "b", "a", "d"): the row is ∠c:b:a + ∠a:b:d = 2∟ and
# ∠c:b:a = ∠a:b:c as canonical atoms (angle_symm, Metric.lean:46).
PROP14_HYPS_DATA = [
    (("dpl", "a", "b", "AB"), "h"),   # distinctPointsOnLine a b AB
    (("dpl", "b", "c", "BC"), "h"),   # distinctPointsOnLine b c BC
    (("dpl", "b", "d", "BD"), "h"),   # distinctPointsOnLine b d BD
    (("opposing_sides", "c", "d", "AB"), "h"),   # c.opposingSides d AB
    (("_sum_rt_ang", "c", "b", "a", "d"), "h"),  # ∠a:b:c + ∠a:b:d = ∟+∟
]

# Lean's shape (Book/Prop14.lean:9-18): by_contra BC ≠ BD; extend_point
# BC c b as e (between c b e); proposition_13 a b e c AB BC
# (∠e:b:a + ∠a:b:c = 2∟); then the case split on a.sameSide e BD, each
# branch closed by sum_angles_onlyif + the arithmetic.
#
# The inner split is rendered as a by_contra (branches = the two cases,
# conclusions = [("false",)]), NOT a by_cases: in Lean both branches are
# finished by euclid_finish reaching False, and the engine's by_cases
# REFUSES a contradictory branch (it would entail everything vacuously).
# (P → ⊥) ∧ (¬P → ⊥) ⊢ ⊥ is exactly a by_contra with False conclusion.
#
# EXHAUSTIVENESS of the split (caller's responsibility, as at I.13): a is
# NOT on BD — closure derives not_on_line a BD (two_lines_meet_once at b
# from BD ≠ AB; a on BD with a ≠ b would make AB = BD and put d on AB,
# contradicting c.opposingSides d AB). With a and e both off BD (e off BD
# from the assumed BC ≠ BD, two_lines_meet_once at b), System E's
# trichotomy on the points off a line leaves exactly sameSide a e BD and
# ¬sameSide a e BD — the latter rendered as the positive fact
# opposing_sides a e BD (Relations.lean:13-14, the same rendering pasch_4
# and triple_incidence_2 use, since the engine has no negated sameSide
# fact kind). There is no third case.
#
# Branch steps before the split (both branches use them):
#   1. extend_point BC c b as e: on_line e BC, between c b e
#      (Points.lean:32).
#   2-3. between_symm_parts on both orientations of between c b e: the
#      engine's closure does not derive neqs from between (see PROP13);
#      sum_angles_onlyif needs neq b e, proposition_13's dpl premise needs
#      neq c e (r_dpl rebuilds dpl c e BC / dpl e c BC from the parts).
#   4. pasch_3_opposing: between c b e with b on AB (c, e off AB) gives
#      opposing_sides c e AB.
#   5. opposing_opposing_same_side: c d AB and c e AB opposing give
#      same_side d e AB (d and e are both on the far side of AB from c).
#      This is the fact Lean's SMT produces silently.
#   6. proposition_13 a b e c AB BC: ∠e:b:a + ∠a:b:c = 2∟
#      (Book/Prop13.lean:6, cited as a lemma; between c b e is the
#      between d b c of the statement).
# Branch 1 (same_side a e BD): sum_angles_onlyif b a d e AB BD gives
#   ∠a:b:d = ∠a:b:e + ∠e:b:d; with the hypothesis and step 6 the
#   arithmetic forces ∠e:b:d = 0, against angle_pos ∠e:b:d > 0
#   (e off BD, b ≠ d, b ≠ e) — the store is unsat.
#   Its side premises are the branch assumption (same_side a e BD) and
#   step 5 (same_side d e AB).
# Branch 2 (opposing_sides a e BD — Lean's "Omitted by Euclid" branch):
#   triple_incidence_2 over (AB, BD, BC) at b with d-witness e turns
#   same_side d e AB and opposing_sides a e BD into same_side a d BC;
#   then sum_angles_onlyif b a e d AB BC gives
#   ∠a:b:e = ∠a:b:d + ∠d:b:e, so ∠d:b:e = 0 against angle_pos
#   (d off BC from the assumed BC ≠ BD) — unsat.
PROP14_STEPS_DATA = [
    ("by_contra", [
        ([("neq_line", "BC", "BD")], [
            ("extend_point", ("BC", "c", "b"), {"x": "e"}),
            ("between_symm_parts", ("c", "b", "e"), {}),
            ("between_symm_parts", ("e", "b", "c"), {}),
            ("pasch_3_opposing", ("c", "b", "e", "AB"), {}),
            ("opposing_opposing_same_side", ("d", "c", "e", "AB"), {}),
            ("proposition_13", ("a", "b", "e", "c", "AB", "BC"), {}),
            ("by_contra", [
                # branch 1: a.sameSide e BD
                ([("same_side", "a", "e", "BD")], [
                    ("sum_angles_onlyif", ("b", "a", "d", "e", "AB", "BD"),
                     {}),
                ]),
                # branch 2: ¬(a.sameSide e BD) — "Omitted by Euclid"
                ([("opposing_sides", "a", "e", "BD")], [
                    ("triple_incidence_2",
                     ("AB", "BD", "BC", "b", "a", "d", "e"), {}),
                    ("sum_angles_onlyif", ("b", "a", "e", "d", "AB", "BC"),
                     {}),
                ]),
            ], [("false",)]),
        ]),
    ], [("on_line", "d", "BC"), ("on_line", "c", "BD")]),
]
# The goal rows the replay verifies: BC = BD as mutual incidence (see the
# representation comment above).
PROP14_GOAL = [("on_line", "d", "BC"), ("on_line", "c", "BD")]


# ---------------------------------------------------------------------------
# Book I.15 as data (Book/Prop15.lean:6 — vertical angles)
# ---------------------------------------------------------------------------
# proposition_15 : dpl a b AB ∧ dpl c d CD ∧ e.onLine AB ∧ e.onLine CD ∧
#   CD ≠ AB ∧ (between d e c) ∧ (between a e b) →
#   (∠ a:e:c = ∠ d:e:b) ∧ (∠ c:e:b = ∠ a:e:d)
# neq_line is given in BOTH directions (the engine's neq_line is directed,
# as at PROP13): Lean's CD ≠ AB premise justifies neq_line CD AB directly,
# and neq_line AB CD is the same inequality read the other way (Lean's ≠
# is symmetric; the directed rendering is the engine's). Call 1 matches on
# AB ≠ CD, calls 2-3 on CD ≠ AB.
PROP15_HYPS_DATA = [
    (("dpl", "a", "b", "AB"), "h"),     # distinctPointsOnLine a b AB
    (("dpl", "c", "d", "CD"), "h"),     # distinctPointsOnLine c d CD
    (("on_line", "e", "AB"), "h"),      # e.onLine AB
    (("on_line", "e", "CD"), "h"),      # e.onLine CD
    (("neq_line", "CD", "AB"), "h"),    # CD ≠ AB
    (("neq_line", "AB", "CD"), "h"),    #   the same ≠, other direction
    (("between", "d", "e", "c"), "h"),  # between d e c
    (("between", "a", "e", "b"), "h"),  # between a e b
]
# Lean's shape (Book/Prop15.lean:10-14): three citations of proposition_13
# then euclid_finish (the arithmetic):
#   proposition_13 a e c d AB CD : ∠c:e:a + ∠a:e:d = 2∟
#   proposition_13 d e a b CD AB : ∠a:e:d + ∠d:e:b = 2∟
#   proposition_13 c e a b CD AB : ∠a:e:c + ∠c:e:b = 2∟
# (with angle_symm, ∠c:e:a = ∠a:e:c as canonical atoms). From the first
# two, ∠a:e:c = 2∟ − ∠a:e:d = ∠d:e:b; from the first and third,
# ∠c:e:b = 2∟ − ∠a:e:c = ∠a:e:d.
# Engine-only steps: the three between_symm_parts citations supply the neqs
# the closure does not derive from between (see PROP13/PROP14) so r_dpl can
# rebuild the dpl premises proposition_13 matches on:
#   (a, e, b) → a ≠ e  for dpl a e AB (call 1)
#   (d, e, c) → d ≠ e  for dpl d e CD (call 2)
#   (c, e, d) → c ≠ e  for dpl c e CD (call 3; between c e d is the closure
#     reversal of the hypothesis between d e c, r_between_symm)
# The reversed between b e a that calls 2-3 need as their between premise
# is likewise closure (r_between_symm), so it is not a cited step.
PROP15_STEPS_DATA = [
    ("between_symm_parts", ("a", "e", "b"), {}),
    ("between_symm_parts", ("d", "e", "c"), {}),
    ("between_symm_parts", ("c", "e", "d"), {}),
    ("proposition_13", ("a", "e", "c", "d", "AB", "CD"), {}),
    ("proposition_13", ("d", "e", "a", "b", "CD", "AB"), {}),
    ("proposition_13", ("c", "e", "a", "b", "CD", "AB"), {}),
]
# Both halves of the conjunction, as LA entailment queries.
PROP15_GOAL = [("_eq_ang", "a", "e", "c", "d", "e", "b"),
               ("_eq_ang", "c", "e", "b", "a", "e", "d")]


# ---------------------------------------------------------------------------
# Book I.16, SECOND conjunct, as data (Book/Prop16.lean:26-32 — the exterior
# angle ∠a:c:d exceeds the interior opposite ∠b:a:c; the part Euclid proves)
# ---------------------------------------------------------------------------
# proposition_16 : formTriangle a b c AB BC AC ∧ (between b c d) →
#   (∠ a:c:d > ∠ c:b:a) ∧ (∠ a:c:d > ∠ b:a:c)
# Only the second conjunct is replayed here (the first, Lean's "Omitted by
# Euclid" bullet, is a separate packet).
# formTriangle destructed exactly as at PROP6 (dpl a b AB + the four
# on_lines + the two non-degeneracy neqs + c off AB); between b c d is the
# extension hypothesis.
PROP16B_HYPS_DATA = [
    (("dpl", "a", "b", "AB"), "h"),      # distinctPointsOnLine a b AB
    (("on_line", "b", "BC"), "h"), (("on_line", "c", "BC"), "h"),
    (("on_line", "c", "AC"), "h"), (("on_line", "a", "AC"), "h"),
    (("neq", "b", "c"), "h"),   # formTriangle non-degeneracy
    (("neq", "a", "c"), "h"),   # formTriangle non-degeneracy
    (("not_on_line", "c", "AB"), "h"),   # formTriangle: c not on AB
    (("between", "b", "c", "d"), "h"),   # BC extended to d
]
# Lean's shape (Book/Prop16.lean:26-32, second bullet):
#   proposition_10 a c AC as e        : between a e c, |a-e| = |e-c|
#   line_from_points b e as BE
#   extend_point_longer BE b e (b─e) as f' : f' on BE, between b e f',
#       |e-f'| > |b-e|   (Points.lean:43; the segment (b─e) is passed as
#       its two endpoints, the engine having no Segment objects)
#   proposition_3 e f' b e BE BE as f : between e f f', |e-f| = |b-e|
#       (argument order EXACTLY as Lean's: engine proposition_3
#       (a,b,c0,c1,AB,C) cuts |c0-c1| off segment a-b; Lean's call has
#       a:=e, b:=f', c0:=b, c1:=e — no remapping needed)
#   line_from_points f c as FC
#   proposition_15 a c b f e AC BE    : ∠a:e:b = ∠f:e:c (vertical angles;
#       the cited lemma added with this replay)
#   proposition_4 e b a e f c BE AB AC BE FC AC
#       (SAS on triangles eba and efc; argument order EXACTLY as Lean's —
#       engine proposition_4 (a,b,c,d,e,f,AB,BC,AC,DE,EF,DF) gets
#       a:=e,b:=b,c:=a, d:=e,e:=f,f:=c. Entailment premises: |e-b|=|e-f|
#       (proposition_3), |e-a|=|e-c| (proposition_10), ∠b:e:a = ∠f:e:c
#       (proposition_15, via canonical atoms). Conclusion used:
#       ∠e:a:b = ∠e:c:f.)
# Engine-only steps: the three between_symm_parts citations supply the neqs
# the closure does not derive from between (as at PROP13/14/15):
#   (a, e, c) → a ≠ e (so two_lines_meet_once puts a off BE, whence
#       line_neq AC BE and f off AC; and angle_ray_eq's ray at a)
#   (c, e, a) → c ≠ e (so two_lines_meet_once puts e off BC — every
#       pasch_2 seed below needs it)
#   (d, c, b) → d ≠ c (for sum_angles_onlyif's neq c d premise and
#       angle_pos; between d c b is the closure reversal of the hypothesis)
# The remaining diagram facts are all CLOSURE, cited here with their rules:
#   e on AC                       between_same_line_in (between a e c)
#   b off AC, a off BC            two_lines_meet_once + line_neq chain
#                                 (seed: c off AB hypothesis)
#   between b f f'                between_trans_in on the reversals
#   between b e f, f e b          between_order (|b-f| = |b-e|+|e-f| > |b-e|
#                                 in the LA store) + between_symm
#   e off BC, d off AC, f off AC  two_lines_meet_once at c / at e
#   f off BC, e~f BC, e~a BC      pasch_2 (between b e f, b on BC, e off BC)
#   a~f BC                        same_side_trans
#   d~f AC                        CITED steps: pasch_3_opposing twice
#                                 (between b c d / between b e f, middles
#                                 c, e on AC → b opposing d AC, b opposing
#                                 f AC), then opposing_opposing_same_side
#                                 d b f AC — the I.14 pattern. (b and d are
#                                 on OPPOSITE sides of AC, so pasch_2
#                                 cannot seed this directly.)
#   ∠f:c:d > 0                    angle_pos (f off BC, c ≠ d, c ≠ f)
#   ∠e:a:b = ∠c:a:b               angle_ray_eq (between a e c, vertex a)
#   ∠e:c:f = ∠a:c:f               angle_ray_eq (between c e a, vertex c)
# The final cited step sum_angles_onlyif c a d f AC BC (Transfer.lean:90)
# gives ∠a:c:d = ∠a:c:f + ∠f:c:d; with ∠a:c:f = ∠b:a:c (proposition_4 +
# the two ray equalities) and ∠f:c:d > 0 the arithmetic entails
# ∠a:c:d > ∠b:a:c strictly — euclid_finish's job.
PROP16B_STEPS_DATA = [
    ("proposition_10", ("a", "c", "AC"), {"d": "e"}),
    ("between_symm_parts", ("a", "e", "c"), {}),
    ("between_symm_parts", ("c", "e", "a"), {}),
    ("line_from_points", ("b", "e"), {"L": "BE"}),
    ("extend_point_longer", ("BE", "b", "e", "b", "e"), {"x": "f'"}),
    ("proposition_3", ("e", "f'", "b", "e", "BE", "BE"), {"q": "f"}),
    ("line_from_points", ("f", "c"), {"L": "FC"}),
    ("between_symm_parts", ("d", "c", "b"), {}),
    ("pasch_3_opposing", ("b", "c", "d", "AC"), {}),
    ("pasch_3_opposing", ("b", "e", "f", "AC"), {}),
    ("opposing_opposing_same_side", ("d", "b", "f", "AC"), {}),
    ("proposition_15", ("a", "c", "b", "f", "e", "AC", "BE"), {}),
    ("proposition_4", ("e", "b", "a", "e", "f", "c", "BE", "AB", "AC",
                       "BE", "FC", "AC"), {}),
    ("sum_angles_onlyif", ("c", "a", "d", "f", "AC", "BC"), {}),
]
# The second conjunct only, as a strict LA entailment query.
PROP16B_GOAL = [("_gt_ang", "a", "c", "d", "b", "a", "c")]


# ---------------------------------------------------------------------------
# Book I.16, FIRST conjunct, as data (Book/Prop16.lean:14-24 — the exterior
# angle ∠a:c:d exceeds the interior opposite ∠c:b:a; the part Lean marks
# "Omitted by Euclid")
# ---------------------------------------------------------------------------
# Same hypotheses as the second conjunct (formTriangle destructured +
# between b c d): PROP16B_HYPS_DATA, reused.
PROP16A_HYPS_DATA = PROP16B_HYPS_DATA
# Lean's shape (Book/Prop16.lean:14-24, first bullet):
#   proposition_10 b c BC as e        : between b e c, |b-e| = |e-c|
#   line_from_points a e as AE
#   extend_point_longer AE a e (a─e) as f' : f' on AE, between a e f',
#       |e-f'| > |a-e|   (the segment (a─e) passed as its two endpoints,
#       as at PROP16B)
#   proposition_3 e f' a e AE AE as f : between e f f', |e-f| = |a-e|
#       (argument order EXACTLY as Lean's, no remapping — as at PROP16B)
#   line_from_points f c as FC
#   proposition_15 b c a f e BC AE    : ∠b:e:a = ∠f:e:c (vertical angles;
#       canonical atoms identify ∠f:e:c with ∠c:e:f)
#   proposition_4 e b a e c f BC AB AE BC FC AE
#       (SAS on triangles eba and ecf; argument order EXACTLY as Lean's.
#       Entailment premises: |e-b| = |e-c| (proposition_10), |e-a| = |e-f|
#       (proposition_3), ∠b:e:a = ∠c:e:f (proposition_15, via canonical
#       atoms). Conclusion used: ∠e:b:a = ∠e:c:f.)
#   extend_point AC a c as g          : g on AC, between a c g
#   proposition_15 a g b d c AC BC    : ∠a:c:b = ∠d:c:g and
#       ∠b:c:g = ∠a:c:d (vertical angles; the second is the one used)
#   euclid_finish: ∠a:c:d = ∠b:c:g (vertical) = ∠b:c:f + ∠f:c:g
#       (f inside ∠b:c:g, sum_angles_onlyif) > ∠b:c:f = ∠e:c:f (ray
#       equality, between b e c reversed) = ∠e:b:a (proposition_4) =
#       ∠c:b:a (ray equality, between b e c).
# Engine-only steps: between_symm_parts citations supply the neqs the
# closure does not derive from between (as at PROP13/14/15/16B):
#   (b, e, c) → b ≠ e  (proposition_4's dpl e b BC)
#   (c, e, b) → c ≠ e  (proposition_4's dpl e c BC; two_lines_meet_once
#       putting e off AC — the pasch_2 seeds below need it)
#   (a, e, f) → a ≠ f  (proposition_15's dpl a f AE; f off AC via
#       two_lines_meet_once at a)
#   (b, c, d) → b ≠ d  (the second proposition_15's dpl b d BC)
#   (g, c, a) → g ≠ c  (sum_angles_onlyif's neq c g premise and angle_pos;
#       between g c a is the closure reversal of extend_point's between
#       a c g)
# The remaining diagram facts are all CLOSURE, cited here with their rules:
#   e on BC                       between_same_line_in (between b e c)
#   a off BC, b off AC            two_lines_meet_once + line_neq chain
#                                 (seed: c off AB hypothesis)
#   between f' e a, between g c a, between c e b
#                                 between_symm (reversals)
#   between a f f', between a e f between_trans_in / between_order
#                                 (|a-f| = |a-e|+|e-f| with |e-f| = |a-e|
#                                 in the LA store; same shape as PROP16B)
#   e off AC, g off BC, f off BC, f off AC
#                                 two_lines_meet_once at c / at e / at a
#   e~b AC, e~f AC                pasch_2 (between c e b with c on AC;
#                                 between a e f with a on AC; e off AC)
#   b~f AC                        same_side_trans — the seed sum_angles_
#                                 onlyif needs on the AC side (b and f
#                                 are on the SAME side of AC here; the
#                                 I.16b configuration had them opposing)
#   g~f BC                        CITED steps: pasch_3_opposing twice
#                                 (between a c g / between a e f, middles
#                                 c, e on BC → a opposing g BC, a opposing
#                                 f BC), then opposing_opposing_same_side
#                                 g a f BC — the I.14 pattern, as at
#                                 PROP16B.
#   ∠f:c:g > 0                    angle_pos (f off AC, c ≠ g, c ≠ f)
#   ∠c:b:a = ∠e:b:a               angle_ray_eq (between b e c, vertex b)
#   ∠e:c:f = ∠b:c:f               angle_ray_eq (between c e b, vertex c)
# The final cited step sum_angles_onlyif c b g f BC AC (Transfer.lean:90)
# gives ∠b:c:g = ∠b:c:f + ∠f:c:g; with ∠b:c:g = ∠a:c:d (the second
# proposition_15), ∠b:c:f = ∠c:b:a (proposition_4 + the two ray
# equalities) and ∠f:c:g > 0 the arithmetic entails ∠a:c:d > ∠c:b:a
# strictly — euclid_finish's job.
PROP16A_STEPS_DATA = [
    ("proposition_10", ("b", "c", "BC"), {"d": "e"}),
    ("between_symm_parts", ("b", "e", "c"), {}),
    ("between_symm_parts", ("c", "e", "b"), {}),
    ("line_from_points", ("a", "e"), {"L": "AE"}),
    ("extend_point_longer", ("AE", "a", "e", "a", "e"), {"x": "f'"}),
    ("proposition_3", ("e", "f'", "a", "e", "AE", "AE"), {"q": "f"}),
    ("line_from_points", ("f", "c"), {"L": "FC"}),
    ("between_symm_parts", ("a", "e", "f"), {}),
    ("between_symm_parts", ("b", "c", "d"), {}),
    ("pasch_3_opposing", ("a", "e", "f", "BC"), {}),
    ("proposition_15", ("b", "c", "a", "f", "e", "BC", "AE"), {}),
    ("proposition_4", ("e", "b", "a", "e", "c", "f", "BC", "AB", "AE",
                       "BC", "FC", "AE"), {}),
    ("extend_point", ("AC", "a", "c"), {"x": "g"}),
    ("between_symm_parts", ("g", "c", "a"), {}),
    ("pasch_3_opposing", ("a", "c", "g", "BC"), {}),
    ("opposing_opposing_same_side", ("g", "a", "f", "BC"), {}),
    ("proposition_15", ("a", "g", "b", "d", "c", "AC", "BC"), {}),
    ("sum_angles_onlyif", ("c", "b", "g", "f", "BC", "AC"), {}),
]
# The first conjunct only, as a strict LA entailment query.
PROP16A_GOAL = [("_gt_ang", "a", "c", "d", "c", "b", "a")]


# ---------------------------------------------------------------------------
# Book I.17, as data (Book/Prop17.lean:6-13 — any two angles of a triangle
# sum to less than two right angles)
# ---------------------------------------------------------------------------
# proposition_17 : formTriangle a b c AB BC AC → ∠a:b:c + ∠b:c:a < ∟+∟.
# formTriangle destructed exactly as at PROP16 (dpl a b AB + the four
# on_lines + the two non-degeneracy neqs + c off AB); no extension
# hypothesis — the replay builds d itself.
PROP17_HYPS_DATA = [
    (("dpl", "a", "b", "AB"), "h"),      # distinctPointsOnLine a b AB
    (("on_line", "b", "BC"), "h"), (("on_line", "c", "BC"), "h"),
    (("on_line", "c", "AC"), "h"), (("on_line", "a", "AC"), "h"),
    (("neq", "b", "c"), "h"),   # formTriangle non-degeneracy
    (("neq", "a", "c"), "h"),   # formTriangle non-degeneracy
    (("not_on_line", "c", "AB"), "h"),   # formTriangle: c not on AB
]
# Lean's shape (Book/Prop17.lean:10-13):
#   extend_point BC b c as d          : d on BC, between b c d
#   proposition_16 a b c d AB BC AC   : ∠a:c:d > ∠c:b:a (the conjunct
#       used here) and ∠a:c:d > ∠b:a:c
#   proposition_13 a c b d AC BC      : ∠b:c:a + ∠a:c:d = ∟+∟ (linear
#       pair; engine proposition_13 (a,b,c,d,AB,CD) with (a,b,c,d,AB,CD)
#       := (a,c,b,d,AC,BC) concludes _sum_rt_ang b c a d)
#   euclid_finish: ∠a:b:c + ∠b:c:a < ∠a:c:d + ∠b:c:a = 2∟, since
#       ∠c:b:a = ∠a:b:c as canonical atoms (angle_symm, Metric.lean:46).
# Engine-only step: between_symm_parts (b, c, d) cites b ≠ d — the
# closure does not derive neqs from between (as at PROP13/14/15/16), and
# proposition_13's dpl b d BC premise needs it (between d c b, its other
# between premise, is the closure reversal of extend_point's fact).
# proposition_16's premises are the formTriangle hypotheses plus between
# b c d; proposition_13's neq_line AC BC premise is closure (c off AB
# seed → two_lines_meet_once/line_neq chain, as at PROP15/16).
PROP17_STEPS_DATA = [
    ("extend_point", ("BC", "b", "c"), {"x": "d"}),
    ("between_symm_parts", ("b", "c", "d"), {}),
    ("proposition_16", ("a", "b", "c", "d", "AB", "BC", "AC"), {}),
    ("proposition_13", ("a", "c", "b", "d", "AC", "BC"), {}),
]
# The goal as an LA entailment query: ∠a:b:c + ∠b:c:a < 2∟.
PROP17_GOAL = [("_lt_sum_rt_ang", "a", "b", "c", "b", "c", "a")]


# ---------------------------------------------------------------------------
# I.18 (the greater side subtends the greater angle) — the theorem that the
# side_gt_angle_gt closure rule short-circuits
# ---------------------------------------------------------------------------
# proposition_18 : formTriangle a b c AB BC AC ∧ (|a─c| > |a─b|) →
# ∠a:b:c > ∠b:c:a. formTriangle destructured exactly as at PROP16/17
# (dpl a b AB + the four on_lines + the two non-degeneracy neqs + c off
# AB); the length comparison is a gt_len metric hypothesis.
PROP18_HYPS_DATA = [
    (("dpl", "a", "b", "AB"), "h"),      # distinctPointsOnLine a b AB
    (("on_line", "b", "BC"), "h"), (("on_line", "c", "BC"), "h"),
    (("on_line", "c", "AC"), "h"), (("on_line", "a", "AC"), "h"),
    (("neq", "b", "c"), "h"),   # formTriangle non-degeneracy
    (("neq", "a", "c"), "h"),   # formTriangle non-degeneracy
    (("not_on_line", "c", "AB"), "h"),   # formTriangle: c not on AB
    (("gt_len", "a", "c", "a", "b"), "h"),   # |a─c| > |a─b|
]
# Lean's shape (Book/Prop18.lean:10-16):
#   proposition_3 a c a b AC AB as d  : d on AC, between a d c, |a-d|=|a-b|
#   line_from_points b d as BD
#   proposition_16 b c d a BC AC BD   : triangle b c d with between c d a;
#       exterior angle ∠b:d:a > ∠d:c:b and ∠b:d:a > ∠c:b:d
#   proposition_5' a b d AB BD AC     : ∠a:b:d = ∠a:d:b (|a-b| = |a-d|)
#   euclid_finish: ∠a:b:c = ∠a:b:d + ∠d:b:c > ∠a:b:d (angle_pos on
#       ∠d:b:c) = ∠a:d:b = ∠b:d:a (canonical atom) > ∠d:c:b = ∠a:c:b
#       (angle_ray_eq, d between a and c) = ∠b:c:a canonically.
# Engine-only step: sum_angles_onlyif b a c d AB BC — NOT Lean's cited
# sum_angles_if b a c d AB BC (Transfer.lean:83). sum_angles_if converts
# a KNOWN angle-sum equality into same-side facts; here both halves of
# its conclusion (same_side a d BC, same_side c d AB) already follow by
# closure from between a d c via Pasch_2, and it makes no metric row, so
# it cannot feed euclid_finish's arithmetic. In Lean, euclid_apply
# discharges sum_angles_if's metric premise (∠b:a:c = ∠b:a:d + ∠d:a:c,
# i.e. ∠a:b:c = ∠a:b:d + ∠d:b:c) by its SMT angle normalization; the
# engine must instead DERIVE that row, which is exactly sum_angles_onlyif
# (Transfer.lean:90) run on the same arguments: same-side premises -> the
# sum row. Its side premises (same_side a d BC, same_side c d AB,
# not_on_line d AB/BC, neq_line AB BC) all close from between a d c, as
# at PROP6's by_contra branch steps (E-kimi-task-388).
PROP18_STEPS_DATA = [
    ("proposition_3", ("a", "c", "a", "b", "AC", "AB"), {"q": "d"}),
    ("line_from_points", ("b", "d"), {"L": "BD"}),
    ("proposition_16", ("b", "c", "d", "a", "BC", "AC", "BD"), {}),
    ("proposition_5'", ("a", "b", "d", "AB", "BD", "AC"), {}),
    ("sum_angles_onlyif", ("b", "a", "c", "d", "AB", "BC"), {}),
]
# The goal as an LA entailment query: ∠a:b:c > ∠b:c:a.
PROP18_GOAL = [("_gt_ang", "a", "b", "c", "b", "c", "a")]


# ---------------------------------------------------------------------------
# I.19 (the greater angle is subtended by the greater side) — the converse
# of I.18, proved by contradiction FROM the cited proposition_18 lemma
# ---------------------------------------------------------------------------
# proposition_19 : formTriangle a b c AB BC AC ∧ (∠a:b:c > ∠b:c:a) →
# |a─c| > |a─b|. formTriangle destructured exactly as PROP18_HYPS_DATA;
# the angle comparison is a gt_ang metric hypothesis (the same plain
# tuple form as PROP18's gt_len hypothesis; _assume_metric_or_fact adds
# the strict angle row).
PROP19_HYPS_DATA = [
    (("dpl", "a", "b", "AB"), "h"),     # distinctPointsOnLine a b AB
    (("on_line", "b", "BC"), "h"), (("on_line", "c", "BC"), "h"),
    (("on_line", "c", "AC"), "h"), (("on_line", "a", "AC"), "h"),
    (("neq", "b", "c"), "h"),   # formTriangle non-degeneracy
    (("neq", "a", "c"), "h"),   # formTriangle non-degeneracy
    (("not_on_line", "c", "AB"), "h"),  # formTriangle: c not on AB
    (("gt_ang", "a", "b", "c", "b", "c", "a"), "h"),  # ∠a:b:c > ∠b:c:a
]
# Lean's shape (Book/Prop19.lean:10-15): by_contra on the goal, then
# by_cases on |a─c| = |a─b|. Branch 1's assumption is the equality;
# branch 2's is Lean's other case (¬(>) ∧ ¬(=)) rendered as
# |a─b| > |a─c|. The two branches split ¬(|a─c| > |a─b|) exactly, and
# their exhaustiveness is trichotomy on the two lengths — the caller's
# responsibility, documented here as at every by_contra use site.
# Branch 1 (|a─c| = |a─b|): proposition_5' a b c AB BC AC gives
#   ∠a:b:c = ∠a:c:b = ∠b:c:a (canonical atom) against the hypothesis —
#   store unsat.
# Branch 2 (|a─b| > |a─c|): proposition_18 a c b AC BC AB (Lean's
#   argument order) — formTriangle a c b AC BC AB closes from the
#   hypotheses (dpl a c AC by r_dpl; b off AC by r_two_lines_meet_once
#   on the neq_line AC AB that the c-on-AC/c-off-AB pair derives; the
#   on_lines and neqs by closure) and the branch assumption is exactly
#   its length premise — gives ∠a:c:b > ∠c:b:a, i.e. ∠b:c:a > ∠a:b:c,
#   against the hypothesis — store unsat. Both contradictions are
#   arithmetic (euclid_finish's SMT in Lean), so no further steps are
#   named. The by_contra conclusion is the goal itself.
PROP19_STEPS_DATA = [
    ("by_contra", [
        # branch 1: |(a─c)| = |(a─b)|
        ([("eq_len", "a", "c", "a", "b")], [
            ("proposition_5'", ("a", "b", "c", "AB", "BC", "AC"), {}),
        ]),
        # branch 2: |(a─b)| > |(a─c)|
        ([("gt_len", "a", "b", "a", "c")], [
            ("proposition_18", ("a", "c", "b", "AC", "BC", "AB"), {}),
        ]),
    ], [("gt_len", "a", "c", "a", "b")]),
]
# The goal as an LA entailment query: |a─c| > |a─b|.
PROP19_GOAL = [("_gt_len", "a", "c", "a", "b")]


# ---------------------------------------------------------------------------
# I.20 (the triangle inequality) — from the cited proposition_19 lemma
# ---------------------------------------------------------------------------
# proposition_20 : formTriangle a b c AB BC AC → |(b─a)| + |(a─c)| > |(b─c)|.
# formTriangle destructured exactly as at PROP18/PROP19 (dpl a b AB + the
# four on_lines + the two non-degeneracy neqs + c off AB); no metric
# hypothesis.
PROP20_HYPS_DATA = [
    (("dpl", "a", "b", "AB"), "h"),     # distinctPointsOnLine a b AB
    (("on_line", "b", "BC"), "h"), (("on_line", "c", "BC"), "h"),
    (("on_line", "c", "AC"), "h"), (("on_line", "a", "AC"), "h"),
    (("neq", "b", "c"), "h"),   # formTriangle non-degeneracy
    (("neq", "a", "c"), "h"),   # formTriangle non-degeneracy
    (("not_on_line", "c", "AB"), "h"),  # formTriangle: c not on AB
]
# Lean's shape (Book/Prop20.lean:10-15), argument orders exactly as Lean's:
#   extend_point_longer AB b a (c─a) as d' : d' on AB, between b a d',
#       |a─d'| > |c─a| (Points.lean:43; the segment (c─a) is passed as its
#       two endpoints, as at PROP16)
#   proposition_3 a d' a c AB AC as d : between a d d', |a─d| = |a─c|
#       (Prop03.lean:5; the |a─d'| > |a─c| entailment premise is step 1's
#       conclusion, |c─a| = |a─c| by the sorted len_atom)
#   line_from_points d c as DC
#   proposition_5' a c d AC DC AB : ∠a:c:d = ∠a:d:c (Prop05.lean:50; the
#       |a─c| = |a─d| entailment premise is step 2's conclusion)
#   sum_angles_onlyif c b d a BC DC : ∠b:c:d = ∠b:c:a + ∠a:c:d
#       (Transfer.lean:90; side premises close from between b a d via
#       Pasch_2, as at PROP18's sum_angles_onlyif step)
#   proposition_19 b c d BC DC AB : |b─d| > |b─c| (Prop19.lean:6). Its
#       angle entailment premise ∠b:c:d > ∠c:d:b closes arithmetically:
#       ∠b:c:d = ∠b:c:a + ∠a:c:d (step 5) > ∠a:c:d (r_angle_pos on
#       ∠b:c:a) = ∠a:d:c (step 4) = ∠b:d:c (r_angle_ray_eq on between
#       d a b) = ∠c:d:b (canonical atom).
# euclid_finish: |b─a| + |a─c| = |b─a| + |a─d| (step 2) = |b─d| (the
#   betweenness sum |b─a| + |a─d| = |b─d| is the closure rule
#   r_between_if, Transfer.lean:16, on between b a d) > |b─c| (step 6).
# Engine-only steps: NONE beyond Lean's — the between facts closure needs
# are derived, not cited: between b d d' from between b a d' ∧ between
# a d d' (r_between_trans_in, Diagrammatic.lean:69, on the reversals),
# then between b a d from r_between_order (between b a d' ∧ between b d d'
# ∧ |b─a| < |b─d|, the comparison entailed by the three betweenness sums
# and |a─d| = |a─c| > 0).
PROP20_STEPS_DATA = [
    ("extend_point_longer", ("AB", "b", "a", "c", "a"), {"x": "d'"}),
    ("proposition_3", ("a", "d'", "a", "c", "AB", "AC"), {"q": "d"}),
    ("line_from_points", ("d", "c"), {"L": "DC"}),
    ("proposition_5'", ("a", "c", "d", "AC", "DC", "AB"), {}),
    ("sum_angles_onlyif", ("c", "b", "d", "a", "BC", "DC"), {}),
    ("proposition_19", ("b", "c", "d", "BC", "DC", "AB"), {}),
]
# The goal as an LA entailment query: |b─a| + |a─c| > |b─c|. The tuple
# ("_gt_sum_len", x, y, u, v, p, q) reads |x─y| + |u─v| > |p─q|.
PROP20_GOAL = [("_gt_sum_len", "b", "a", "a", "c", "b", "c")]


# ---------------------------------------------------------------------------
# I.21 (Book/Prop21.lean) — from the cited proposition_20 and
# proposition_16 lemmas
# ---------------------------------------------------------------------------
# proposition_21 : formTriangle a b c AB BC AC ∧ a.sameSide d BC ∧
#   c.sameSide d AB ∧ b.sameSide d AC ∧ distinctPointsOnLine b d BD ∧
#   distinctPointsOnLine d c DC →
#   (|(b─d)| + |(d─c)| < |(b─a)| + |(a─c)|) ∧ (∠ b:d:c > ∠ b:a:c)
# formTriangle destructured exactly as at PROP18-20; the three sameSide
# hypotheses and the two dpls ship as facts.
PROP21_HYPS_DATA = [
    (("dpl", "a", "b", "AB"), "h"),     # distinctPointsOnLine a b AB
    (("on_line", "b", "BC"), "h"), (("on_line", "c", "BC"), "h"),
    (("on_line", "c", "AC"), "h"), (("on_line", "a", "AC"), "h"),
    (("neq", "b", "c"), "h"),   # formTriangle non-degeneracy
    (("neq", "a", "c"), "h"),   # formTriangle non-degeneracy
    (("not_on_line", "c", "AB"), "h"),  # formTriangle: c not on AB
    (("same_side", "a", "d", "BC"), "h"),   # a.sameSide d BC
    (("same_side", "c", "d", "AB"), "h"),   # c.sameSide d AB
    (("same_side", "b", "d", "AC"), "h"),   # b.sameSide d AC
    (("dpl", "b", "d", "BD"), "h"),   # distinctPointsOnLine b d BD
    (("dpl", "d", "c", "DC"), "h"),   # distinctPointsOnLine d c DC
]
# Lean's shape (Book/Prop21.lean:11-20), argument orders exactly as
# Lean's. Lean's euclid_apply(intersection_lines BD AC) closes the
# premise BD.intersectsLine AC by SMT; the engine renders that premise
# explicitly (as I.10 did with intersection_lines_opposing before
# intersection_lines):
#   1. triple_incidence_1 AB BD BC b a d c (Diagrammatic.lean:187):
#      AB, BD, BC meet at b; d on BD, c on BC, a on AB; d.sameSide c AB
#      (hyp c.sameSide d AB, symm) and a.sameSide d BC (hyp) give
#      opposing_sides a c BD. The off-line premises a, c off BD are
#      closure: d off AB/BC (same_side_not_on_line on the hyps) gives
#      BD ≠ AB and BD ≠ BC (line_neq), then two_lines_meet_once at b
#      puts a and c off BD (a ≠ b from the dpl, b ≠ c hyp).
#   2-3. intersection_lines_opposing a c BD AC (Diagrammatic.lean:267)
#      gives intersects_ll BD AC; intersection_lines BD AC as e
#      (Intersections.lean:3) gives e on BD and on AC.
#   4. pasch_4 BD AC e a c (Diagrammatic.lean:173): a, c oppose across
#      BD, so between a e c. (BD ≠ AC: d on BD off AC, line_neq; the
#      neqs a ≠ e, c ≠ e are off_line_neq from a, c off BD.)
#   5. between b d e: trichotomy between_points (Diagrammatic.lean:87) on
#      the distinct b, d, e on BD leaves three orders; the other two die:
#      between b e d would put b opposing d across AC (pasch_3, e on AC)
#      against hyp b.sameSide d AC; between e b d would put e opposing d
#      across AB (pasch_3, b on AB) against same_side e d AB (closure:
#      pasch_2 on between a e c with a on AB gives e~c AB — e off AB
#      from two_lines_meet_once at a with AB ≠ AC — then
#      same_side_trans with hyp c~d AB). Rendered as a by_contra whose
#      branches are the two dead orders, exactly the I.14 split pattern.
#   6-7. proposition_20 a b e AB BD AC and proposition_20 e d c BD DC AC
#      (Book/Prop20.lean:6, cited lemmas): |b─a|+|a─e|>|b─e| and
#      |d─e|+|e─c|>|d─c|.
#   8-9. proposition_16 c e d b AC BD DC and proposition_16 b a e c
#      AB AC BD (Book/Prop16.lean:8, cited lemmas): ∠c:d:b > ∠d:e:c and
#      ∠b:e:c > ∠e:a:b (the between e d b premise of the first is the
#      reversal of step 5, closure between_symm).
# constructor (Prop21.lean:17) just splits the conjunction goal into two
# goals over the same final state; the engine's goal list is already a
# conjunction, so it needs nothing.
# euclid_finish, conjunct 1: |b─a|+|a─c| = |b─a|+|a─e|+|e─c| (between
#   a e c, r_between_if Transfer.lean:16) > |b─e|+|e─c| (step 6) =
#   |b─d|+|d─e|+|e─c| (between b d e) > |b─d|+|d─c| (step 7).
# euclid_finish, conjunct 2: ∠b:d:c = ∠c:d:b (canonical atom) > ∠d:e:c
#   (step 8) = ∠b:e:c (angle_ray_eq on between e d b) > ∠e:a:b (step 9)
#   = ∠b:a:c (angle_ray_eq on between a e c, canonical atom).
# Engine-only steps: NONE beyond Lean's — steps 1-5 are the explicit
# rendering of premises Lean's SMT closes inside euclid_apply /
# euclid_finish; every rule cited is System E or a Book lemma.
PROP21_STEPS_DATA = [
    ("triple_incidence_1", ("AB", "BD", "BC", "b", "a", "d", "c"), {}),
    ("intersection_lines_opposing", ("a", "c", "BD", "AC"), {}),
    ("intersection_lines", ("BD", "AC"), {"a": "e"}),
    ("pasch_4", ("a", "e", "c", "BD", "AC"), {}),
    ("by_contra", [
        # between b e d ⇒ opposing_sides b d AC (pasch_3, e on AC),
        # against the hypothesis b.sameSide d AC
        ([("between", "b", "e", "d")], [
            ("pasch_3_opposing", ("b", "e", "d", "AC"), {}),
        ]),
        # between e b d ⇒ opposing_sides e d AB (pasch_3, b on AB),
        # against same_side e d AB (closure: pasch_2 + same_side_trans)
        ([("between", "e", "b", "d")], [
            ("pasch_3_opposing", ("e", "b", "d", "AB"), {}),
        ]),
    ], [("between", "b", "d", "e")]),
    ("proposition_20", ("a", "b", "e", "AB", "BD", "AC"), {}),
    ("proposition_20", ("e", "d", "c", "BD", "DC", "AC"), {}),
    ("proposition_16", ("c", "e", "d", "b", "AC", "BD", "DC"), {}),
    ("proposition_16", ("b", "a", "e", "c", "AB", "AC", "BD"), {}),
]
# The goals as LA entailment queries: |b─d| + |d─c| < |b─a| + |a─c| and
# ∠b:d:c > ∠b:a:c. The tuple ("_lt_sum_sum_len", x, y, u, v, p, q, r, s)
# reads |x─y| + |u─v| < |p─q| + |r─s|.
PROP21_GOAL = [("_lt_sum_sum_len", "b", "d", "d", "c", "b", "a", "a", "c"),
               ("_gt_ang", "b", "d", "c", "b", "a", "c")]


# ---------------------------------------------------------------------------
# I.22 (Book/Prop22.lean) — the triangle from three lengths
# ---------------------------------------------------------------------------
# proposition_22 : distinctPointsOnLine a a' A ∧ distinctPointsOnLine b b' B
#   ∧ distinctPointsOnLine c c' C ∧ (|aa'| + |bb'| > |cc'|) ∧
#   (|aa'| + |cc'| > |bb'|) ∧ (|bb'| + |cc'| > |aa'|) →
#   ∃ k f g, |f─k| = |a─a'| ∧ |f─g| = |b─b'| ∧ |k─g| = |c─c'|
# The three dpls ship destructured; the three triangle inequalities ship
# as plain gt_sum_len metric hypotheses (|x─y| + |u─v| > |p─q| rows).
PROP22_HYPS_DATA = [
    (("dpl", "a", "a'", "A"), "h"),   # distinctPointsOnLine a a' A
    (("dpl", "b", "b'", "B"), "h"),   # distinctPointsOnLine b b' B
    (("dpl", "c", "c'", "C"), "h"),   # distinctPointsOnLine c c' C
    # |a─a'| + |b─b'| > |c─c'|  (inequality 1)
    (("gt_sum_len", "a", "a'", "b", "b'", "c", "c'"), "h"),
    # |a─a'| + |c─c'| > |b─b'|  (inequality 2)
    (("gt_sum_len", "a", "a'", "c", "c'", "b", "b'"), "h"),
    # |b─b'| + |c─c'| > |a─a'|  (inequality 3)
    (("gt_sum_len", "b", "b'", "c", "c'", "a", "a'"), "h"),
]
# Lean's shape (Prop22.lean:14-32), argument orders exactly as Lean's.
# d arbitrary; e' distinct from it; DE the line; e'', e''', e extend DE
# past d by more than |aa'|, |bb'|, |cc'| (so |d─e| exceeds each length
# and the three proposition_3 cuts succeed); f, g, h cut |d─f| = |aa'|,
# |f─g| = |bb'|, |g─h| = |cc'| along DE; DKL = circle centre f through d
# (radius |aa'|), KLH = circle centre g through h (radius |cc'|); i is
# the second meet of DE with KLH (between i g h — the far side of g
# from h, toward d); k = the two circles' meet. The ∃ is witnessed by
# the named k, f, g; the goal is the three length equalities.
# The only premise Lean's SMT closes silently (Prop22.lean:28) is
# intersects_cc KLH DKL for intersection_circles. The engine renders it
# explicitly as intersection_circle_circle_1 (Diagrammatic.lean:312)
# with witnesses a = i, b = h:
#   * i on KLH (its construction), h on KLH (circle_from_points), so
#     not_outside h KLH (r_not_outside, Relations.lean:35);
#   * outside h DKL: between f g h (closure: between_order on
#     between f g e, between f h e with |f─g| < |f─h|; the comparison
#     comes from the three betweenness sums and |g─h| = |cc'| > 0) gives
#     |f─h| = |f─g| + |g─h| = |bb'| + |cc'| > |aa'| = |f─d| (r_between_if
#     Transfer.lean:16 + INEQUALITY 3), so r_classify (Transfer.lean:47 /
#     Relations.lean:35) puts h outside DKL;
#   * inside i DKL: |g─i| = |g─h| = |cc'| (radii, poc_onlyif
#     Transfer.lean:39) and |g─f| = |bb'|; i and f both lie on DE on the
#     side of g opposite h (between i g h from i's construction; between
#     f g h closure as above), so between_points trichotomy
#     (Diagrammatic.lean:87) on the collinear g, f, i leaves three
#     cases — the by_cases below. between f g i (g between f and i) is
#     dead: it puts i on h's side of g at distance |cc'| = |g─h|, i.e.
#     i = h, against i ≠ h (between i g h). In each live case
#     r_classify puts i inside DKL:
#       - between g f i: |f─i| = |cc'| − |bb'| < |aa'| (INEQUALITY 1);
#       - between g i f: |f─i| = |bb'| − |cc'| < |aa'| (INEQUALITY 2);
#       - peq f i: i IS the centre of DKL, inside it
#         (r_center_inside Diagrammatic.lean:28 on inside f DKL, then
#         peq_subst_diagram).
#     (icc2, Diagrammatic.lean:319, cannot serve: d is outside KLH —
#     |g─d| = |aa'| + |bb'| > |cc'| — and h outside DKL, so no pair of
#     constructed points fills its a/b slots.)
# The final two point_on_circle_onlyif euclid_apply's (Prop22.lean:30-31)
# are the engine's closure rule r_poc_onlyif (Transfer.lean:39), which
# gives |f─k| = |f─d| and |g─k| = |g─h| once k is on both circles — no
# explicit steps. euclid_finish: |f─k| = |f─d| = |a─a'| (step 7),
# |f─g| = |b─b'| (step 8), |k─g| = |g─k| = |g─h| = |c─c'| (step 9).
# Engine-only steps: NONE beyond Lean's — the by_cases and the icc1
# citation are the explicit rendering of premises Lean's SMT closes
# inside euclid_apply / euclid_finish; every rule cited is System E.
PROP22_STEPS_DATA = [
    ("arbitrary_point", (), {"p": "d"}),
    ("distinct_points", ("d",), {"q": "e'"}),
    ("line_from_points", ("d", "e'"), {"L": "DE"}),
    ("extend_point_longer", ("DE", "d", "e'", "a", "a'"), {"x": "e''"}),
    ("extend_point_longer", ("DE", "d", "e''", "b", "b'"), {"x": "e'''"}),
    ("extend_point_longer", ("DE", "d", "e'''", "c", "c'"), {"x": "e"}),
    # The dpl premises of the proposition_3 cuts and of
    # intersection_circle_line_extending_points need the neqs d ≠ e,
    # f ≠ e, g ≠ e, g ≠ h. The closure's metric route (r_pos_neq,
    # Metric.lean:21,25) asks the linear store |x─y| > 0; on this
    # replay's five-point chain store Fourier-Motzkin blows past its row
    # guard (the between-sum rows make a dense elimination graph), so the
    # neqs are cited diagrammatically instead: between_symm_parts
    # (Diagrammatic.lean:45-46), the same per-use pattern that keeps
    # between_symm's ¬between part out of default closure.
    ("between_symm_parts", ("d", "e'''", "e"), {}),   # neq d e (and d e''')
    ("proposition_3", ("d", "e", "a", "a'", "DE", "A"), {"q": "f"}),
    # between_symm_parts states a ≠ b and a ≠ c of between a b c, so the
    # f/g neqs come off the REVERSALS (closure between_symm) of the makes
    # (neq_symm, logic, flips them for the dpl rule).
    ("between_symm_parts", ("e", "f", "d"), {}),      # neq e f -> f e
    ("proposition_3", ("f", "e", "b", "b'", "DE", "B"), {"q": "g"}),
    ("between_symm_parts", ("e", "g", "f"), {}),      # neq e g -> g e
    ("proposition_3", ("g", "e", "c", "c'", "DE", "C"), {"q": "h"}),
    ("between_symm_parts", ("g", "h", "e"), {}),      # neq g h
    ("circle_from_points", ("f", "d"), {"C": "DKL"}),
    ("circle_from_points", ("g", "h"), {"C": "KLH"}),
    ("intersection_circle_line_extending_points", ("KLH", "DE", "g", "h"),
     {"g": "i"}),
    # inside i DKL by the three-case trichotomy documented above
    ("by_cases", [
        ([("between", "g", "f", "i")], []),
        ([("between", "g", "i", "f")], []),
        ([("peq", "f", "i")], []),
    ], [("inside", "i", "DKL")]),
    ("intersection_circle_circle_1", ("i", "h", "KLH", "DKL"), {}),
    ("intersection_circles", ("KLH", "DKL"), {"c": "k"}),
]
# The goals as LA entailment queries: |f─k| = |a─a'|, |f─g| = |b─b'|,
# |k─g| = |c─c'| over the named witnesses k, f, g.
PROP22_GOAL = [("_eq_len", "f", "k", "a", "a'"),
               ("_eq_len", "f", "g", "b", "b'"),
               ("_eq_len", "k", "g", "c", "c'")]


# ---------------------------------------------------------------------------
# proposition_22' (Prop22.lean:34, the variant I.23 cites):
#   distinctPointsOnLine a a' A ∧ ... ∧ distinctPointsOnLine f e FE ∧
#   (|aa'| + |bb'| > |cc'|) ∧ (|aa'| + |cc'| > |bb'|) ∧ (|bb'| + |cc'| > |aa'|)
#   → ∃ k g, g.onLine FE ∧ ¬(between g f e) ∧
#     |f─k| = |a─a'| ∧ |f─g| = |b─b'| ∧ |k─g| = |c─c'|
# I.22's construction started from the GIVEN segment f─e on FE instead of
# an arbitrary point: e' extends FE past e by more than |bb'|, e'' past e'
# by more than |cc'| (so |f─e''| > |bb'| + |cc'| exceeds each of the three
# lengths — |aa'| via INEQUALITY 3 — and the three proposition_3 cuts
# succeed); d, g, h cut |f─d| = |aa'|, |f─g| = |bb'|, |g─h| = |cc'| along
# FE; DKL = circle centre f through d, KLH = centre g through h; i is the
# second meet of FE with KLH (between i g h); k = the two circles' meet.
# The ∃ is witnessed by the named k, g.
# What Lean's SMT closes silently, the engine renders explicitly, exactly
# as in I.22:
#   * the dpl premises' neqs are cited diagrammatically via
#     between_symm_parts (Diagrammatic.lean:45-46), the f/g neqs off the
#     closure reversals (r_between_symm) as in I.22;
#   * inside i DKL by the same three-case by_cases (between_points
#     trichotomy, Diagrammatic.lean:87, on the collinear g, f, i; the
#     fourth order between f g i is dead — it puts i on h's side of g at
#     distance |cc'| = |g─h|, i.e. i = h, against i ≠ h): between g f i
#     gives |f─i| = |cc'| − |bb'| < |aa'| (INEQUALITY 1), between g i f
#     gives |f─i| = |bb'| − |cc'| < |aa'| (INEQUALITY 2), peq f i puts i
#     at DKL's centre (r_center_inside Diagrammatic.lean:28 +
#     r_peq_subst_diagram);
#   * intersects_cc KLH DKL for intersection_circles (Prop22.lean:52) by
#     the cited intersection_circle_circle_1 (Diagrammatic.lean:312) with
#     witnesses a = i, b = h — outside h DKL from between f g h (closure:
#     between f h e'' by between_symm + between_trans_in on the chain,
#     then between_order with |f─g| < |f─h| = |f─g| + |g─h|), so
#     |f─h| = |bb'| + |cc'| > |aa'| = |f─d| (INEQUALITY 3), r_classify;
#   * ¬between g f e (the goal's second conjunct; euclid_finish,
#     Prop22.lean:56) as a by_contra: assume between g f e; with
#     between f e e'' (closure: between_trans_in on between f e' e'' and
#     between f e e'), the cited between_trans_out (Diagrammatic.lean:78)
#     gives between g f e'', while between_symm_parts on between f g e''
#     (g's proposition_3 cut) gives ¬between g f e'' — the exclusive pair.
#     Exhaustiveness: the single branch is the classical negation of the
#     conclusion. The goal query itself needs NO new goal kind:
#     not_between is a first-class fact kind (superposition's make,
#     Superposition.lean:7) and replay's verify loop checks plain facts
#     with s.has; the positive route between f g e → between_symm_parts
#     cannot serve here because |f─e| vs |bb'| is not fixed by the
#     hypotheses, so the between f g e / between f e g case split would
#     be genuine — the negation is proved directly instead.
# The closing point_on_circle_onlyif steps (Prop22.lean:54-55) are the
# engine's closure rule r_poc_onlyif (Transfer.lean:39) — no explicit
# steps, as in I.22.
PROP22P_HYPS_DATA = [
    (("dpl", "a", "a'", "A"), "h"),   # distinctPointsOnLine a a' A
    (("dpl", "b", "b'", "B"), "h"),   # distinctPointsOnLine b b' B
    (("dpl", "c", "c'", "C"), "h"),   # distinctPointsOnLine c c' C
    (("dpl", "f", "e", "FE"), "h"),   # distinctPointsOnLine f e FE
    # |a─a'| + |b─b'| > |c─c'|  (inequality 1)
    (("gt_sum_len", "a", "a'", "b", "b'", "c", "c'"), "h"),
    # |a─a'| + |c─c'| > |b─b'|  (inequality 2)
    (("gt_sum_len", "a", "a'", "c", "c'", "b", "b'"), "h"),
    # |b─b'| + |c─c'| > |a─a'|  (inequality 3)
    (("gt_sum_len", "b", "b'", "c", "c'", "a", "a'"), "h"),
]
# Lean's shape (Prop22.lean:42-55), argument orders exactly as Lean's,
# with the SMT-closed premises rendered explicitly as documented above.
PROP22P_STEPS_DATA = [
    ("extend_point_longer", ("FE", "f", "e", "b", "b'"), {"x": "e'"}),
    ("between_symm_parts", ("f", "e", "e'"), {}),   # neq f e' (dpl f e' FE)
    ("extend_point_longer", ("FE", "f", "e'", "c", "c'"), {"x": "e''"}),
    # neq f e'' (dpl f e'' FE for the first two proposition_3 cuts)
    ("between_symm_parts", ("f", "e'", "e''"), {}),
    ("proposition_3", ("f", "e''", "a", "a'", "FE", "A"), {"q": "d"}),
    ("proposition_3", ("f", "e''", "b", "b'", "FE", "B"), {"q": "g"}),
    # neq g e'' off the REVERSAL of between f g e'' (closure
    # r_between_symm + neq_symm), for the third cut's dpl g e'' FE
    ("between_symm_parts", ("e''", "g", "f"), {}),
    ("proposition_3", ("g", "e''", "c", "c'", "FE", "C"), {"q": "h"}),
    ("between_symm_parts", ("g", "h", "e''"), {}),  # neq g h (dpl g h FE)
    ("circle_from_points", ("f", "d"), {"C": "DKL"}),
    ("circle_from_points", ("g", "h"), {"C": "KLH"}),
    ("intersection_circle_line_extending_points", ("KLH", "FE", "g", "h"),
     {"g": "i"}),
    # inside i DKL by the three-case trichotomy documented above
    ("by_cases", [
        ([("between", "g", "f", "i")], []),
        ([("between", "g", "i", "f")], []),
        ([("peq", "f", "i")], []),
    ], [("inside", "i", "DKL")]),
    ("intersection_circle_circle_1", ("i", "h", "KLH", "DKL"), {}),
    ("intersection_circles", ("KLH", "DKL"), {"c": "k"}),
    # euclid_finish's ¬between g f e, rendered as a by_contra
    ("by_contra", [
        ([("between", "g", "f", "e")],
         [("between_trans_out", ("g", "f", "e", "e''"), {}),
          ("between_symm_parts", ("f", "g", "e''"), {})]),
    ], [("not_between", "g", "f", "e")]),
]
# The goals as LA entailment queries plus the two diagram facts:
# on_line g FE and not_between g f e over the named witnesses k, g.
PROP22P_GOAL = [("on_line", "g", "FE"), ("not_between", "g", "f", "e"),
                ("_eq_len", "f", "k", "a", "a'"),
                ("_eq_len", "f", "g", "b", "b'"),
                ("_eq_len", "k", "g", "c", "c'")]


# ---------------------------------------------------------------------------
# Book I.23 (Prop23.lean:8, theorem proposition_23): copy an angle onto a
# given line at a given point —
#   distinctPointsOnLine a b AB ∧ formRectilinearAngle d c e CD CE →
#     ∃ f, f ≠ a ∧ ∠f:a:b = ∠d:c:e
# formRectilinearAngle d c e CD CE destructures (Relations.lean:64) to
# dpl d c CD ∧ dpl c e CE. Lean's proof is a by_cases on d.onLine CE
# (Prop23.lean:12); the degenerate branch ("Omitted by Euclid",
# lines 13-21) is a nested by_cases on ∠d:c:e = 0.
PROP23_HYPS_DATA = [
    (("dpl", "a", "b", "AB"), "h"),   # distinctPointsOnLine a b AB
    (("dpl", "d", "c", "CD"), "h"),   # formRectilinearAngle d c e CD CE
    (("dpl", "c", "e", "CE"), "h"),   #  (Relations.lean:64)
]
# The outer by_cases (Prop23.lean:12) splits on on_line d CE /
# not_on_line d CE — a classical negation, exhaustive by logic. Its
# conclusions are the existential's body over the eigenvariable f: each
# branch supplies its own witness (the constructed f in the main branch;
# b via `use` and b' via extend_point in the two degenerate branches)
# and the by_cases carries the renamed conclusions out.
#
# DEGENERATE BRANCH (d on CE, lines 13-21): the nested by_cases on
# ∠d:c:e = 0. c, d, e are collinear with c ≠ d, c ≠ e, so
# ¬between d c e ⇔ ∠d:c:e = 0 (degenerated_angle_if/_onlyif,
# Transfer.lean:63/67) and the split ∠d:c:e = 0 / between d c e is
# exactly Lean's split, the ¬(∠ = 0) branch rendered positively by the
# contrapositive of degenerated_angle_if.
#   * branch ∠d:c:e = 0 (assumed as the metric tuple _zero_ang): the
#     witness is b (`use b`, line 15). ∠b:a:b = 0 is the SMT-bridge
#     angle_self_zero (degenerated_angle_if with c := b); use_witness
#     (logic: `use`) restates the goal's conjuncts about b under the
#     witness name f. euclid_finish closes f ≠ a from dpl a b AB and
#     ∠b:a:b = ∠d:c:e from the two zero rows.
#   * branch between d c e: Lean's `euclid_assert ∠d:c:e = ∟+∟` (line
#     18) is SMT from the coordinate model; the System E citation is
#     flat_angle_onlyif (Transfer.lean:124): between d c e →
#     ∠d:c:e = ∟+∟. (proposition_13 is NOT needed — nothing here goes
#     through a sum of adjacent angles.) extend_point AB b a as f gives
#     between b a f, so ∠f:a:b = ∠b:a:f = ∟+∟ by flat_angle_onlyif
#     again (the legs of ∠f:a:b sort by angle_symm, Metric.lean:46);
#     f ≠ a is between_symm_parts on the closure reversal between f a b.
# MAIN BRANCH (d off CE, lines 22-32), Lean's arg orders:
#   line_from_points d e as DE (d ≠ e: d off CE, e on CE,
#   r_off_line_neq); the three proposition_20 citations give exactly the
#   three gt_sum_len rows |c─d|+|c─e|>|e─d|, |e─d|+|d─c|>|e─c|,
#   |c─e|+|e─d|>|c─d| that proposition_22's entail premises ask for
#   (proposition_22' c d c e e d a b: a,a' = c,d; b,b' = c,e;
#   c,c' = e,d) — no other source; proposition_22' makes f (= Lean's f,
#   the circles' meet k) and g with |a─f| = |c─d|, |f─g| = |e─d|,
#   |a─g| = |c─e|, g on AB, ¬between g a b; line_from_points a f as FA
#   (a ≠ f: |a─f| = |c─d| > 0) and f g as FG (|f─g| = |e─d| > 0);
#   proposition_8 c d e a f g CD DE CE FA FG AB (SSS) gives
#   ∠d:c:e = ∠f:a:g. What Lean's SMT closes silently inside those
#   euclid_apply's, the engine renders explicitly:
#     * e off CD and c off DE (proposition_20's not_on_line premises):
#       r_line_neq from d on CD/DE off CE, then the closure
#       r_two_lines_meet_once through c and e respectively;
#     * g off FA (proposition_8's second not_on_line premise): f is off
#       AB by the cited off_line_strict_triangle (the three strict
#       inequalities among |a─f|, |a─g|, |f─g| come from the
#       proposition_20 rows through the I.22' equalities), then
#       r_line_neq (f on FA, off AB) and r_two_lines_meet_once through a
#       (g ≠ a: |a─g| = |c─e| > 0);
#     * ∠f:a:g = ∠f:a:b (euclid_finish): the cited degenerated_angle_if
#       (Transfer.lean:63) turns ¬between g a b (I.22's conclusion) into
#       ∠g:a:b = 0 — the rays a→g and a→b coincide — and the cited
#       angle_ray_eq_zero bridge rewrites the proposition_8 angle.
PROP23_STEPS_DATA = [
    ("by_cases", [
        # -- degenerate branch: d on CE ("Omitted by Euclid")
        ([("on_line", "d", "CE")], [
            ("by_cases", [
                # ∠d:c:e = 0: the witness is b
                ([("_zero_ang", "d", "c", "e")], [
                    ("angle_self_zero", ("b", "a"), {}),
                    ("use_witness", ("f", "b", "a", "d", "c", "e", "AB"),
                     {"f": "f"}),
                ]),
                # ∠d:c:e ≠ 0, i.e. between d c e: the witness is the
                # extension of AB past a
                ([("between", "d", "c", "e")], [
                    ("flat_angle_onlyif", ("d", "c", "e"), {}),
                    ("extend_point", ("AB", "b", "a"), {"x": "f"}),
                    ("flat_angle_onlyif", ("b", "a", "f"), {}),
                    ("between_symm_parts", ("f", "a", "b"), {}),
                ]),
            ], [("neq", "f", "a"), ("_eq_ang", "f", "a", "b", "d", "c", "e")]),
        ]),
        # -- main branch: d off CE (Euclid's construction)
        ([("not_on_line", "d", "CE")], [
            ("line_from_points", ("d", "e"), {"L": "DE"}),
            ("proposition_20", ("c", "d", "e", "CD", "DE", "CE"), {}),
            ("proposition_20", ("d", "e", "c", "DE", "CE", "CD"), {}),
            ("proposition_20", ("e", "c", "d", "CE", "CD", "DE"), {}),
            ("proposition_22'",
             ("c", "d", "c", "e", "e", "d", "a", "b",
              "CD", "CE", "DE", "AB"), {"k": "f", "g": "g"}),
            ("line_from_points", ("a", "f"), {"L": "FA"}),
            ("line_from_points", ("f", "g"), {"L": "FG"}),
            ("off_line_strict_triangle", ("f", "a", "g", "AB"), {}),
            ("degenerated_angle_if", ("a", "g", "b", "AB"), {}),
            ("angle_ray_eq_zero", ("g", "a", "b", "f"), {}),
            ("proposition_8",
             ("c", "d", "e", "a", "f", "g",
              "CD", "DE", "CE", "FA", "FG", "AB"), {}),
        ]),
    ], [("neq", "f", "a"), ("_eq_ang", "f", "a", "b", "d", "c", "e")]),
]
# The goal: the existential's body over the witness name the by_cases
# carries out — f ≠ a (a plain fact) and ∠f:a:b = ∠d:c:e (an _eq_ang LA
# entailment query; the kind already exists in replay's verify loop).
PROP23_GOAL = [("neq", "f", "a"), ("_eq_ang", "f", "a", "b", "d", "c", "e")]


# ---------------------------------------------------------------------------
# proposition_22'' (Prop22.lean:58, the same-side variant I.23' cites):
#   distinctPointsOnLine a a' A ∧ ... ∧ distinctPointsOnLine f e FE ∧
#   ¬(x.onLine FE) ∧ (|aa'| + |bb'| > |cc'|) ∧ (|aa'| + |cc'| > |bb'|) ∧
#   (|bb'| + |cc'| > |aa'|) →
#     ∃ k g, g.onLine FE ∧ ¬(between g f e) ∧ (k.sameSide x FE) ∧
#     |f─k| = |a─a'| ∧ |f─g| = |b─b'| ∧ |k─g| = |c─c'|
# The construction is proposition_22''s verbatim (Prop22.lean:67-80),
# IDENTICAL to I.22' except the circles' meet: line 76 applies
# intersection_same_side KLH DKL x g f FE as k (in place of
# intersection_circles KLH DKL as k), picking the meet on x's side of
# FE. See the I.22' header above for the SMT-rendered premises the two
# proofs share (between_symm_parts neqs, the three-case inside-i-DKL
# by_cases, icc1's intersects_cc, the by_contra for ¬between g f e).
# intersection_same_side (Intersections.lean:20, cited per use) asks,
# beyond intersects_cc: g.isCentre KLH and f.isCentre DKL (the
# circle_from_points makes), g and f on FE (g from its proposition_3
# cut's between f g e'' with f, e'' on FE; f from dpl f e FE), and
# ¬(x.onLine FE) — the hypothesis this variant adds. Its conclusion
# a.sameSide b L is the goal's extra conjunct, same_side k x FE.
PROP22PP_HYPS_DATA = PROP22P_HYPS_DATA + [
    ((("not_on_line", "x", "FE"), "h")),   # ¬(x.onLine FE)
]
PROP22PP_STEPS_DATA = [
    ("extend_point_longer", ("FE", "f", "e", "b", "b'"), {"x": "e'"}),
    ("between_symm_parts", ("f", "e", "e'"), {}),   # neq f e' (dpl f e' FE)
    ("extend_point_longer", ("FE", "f", "e'", "c", "c'"), {"x": "e''"}),
    # neq f e'' (dpl f e'' FE for the first two proposition_3 cuts)
    ("between_symm_parts", ("f", "e'", "e''"), {}),
    ("proposition_3", ("f", "e''", "a", "a'", "FE", "A"), {"q": "d"}),
    ("proposition_3", ("f", "e''", "b", "b'", "FE", "B"), {"q": "g"}),
    # neq g e'' off the REVERSAL of between f g e'' (closure
    # r_between_symm + neq_symm), for the third cut's dpl g e'' FE
    ("between_symm_parts", ("e''", "g", "f"), {}),
    ("proposition_3", ("g", "e''", "c", "c'", "FE", "C"), {"q": "h"}),
    ("between_symm_parts", ("g", "h", "e''"), {}),  # neq g h (dpl g h FE)
    ("circle_from_points", ("f", "d"), {"C": "DKL"}),
    ("circle_from_points", ("g", "h"), {"C": "KLH"}),
    ("intersection_circle_line_extending_points", ("KLH", "FE", "g", "h"),
     {"g": "i"}),
    # inside i DKL by the three-case trichotomy documented under I.22'
    ("by_cases", [
        ([("between", "g", "f", "i")], []),
        ([("between", "g", "i", "f")], []),
        ([("peq", "f", "i")], []),
    ], [("inside", "i", "DKL")]),
    ("intersection_circle_circle_1", ("i", "h", "KLH", "DKL"), {}),
    # Prop22.lean:76 — the meet on x's side of FE (in place of
    # intersection_circles; Intersections.lean:20)
    ("intersection_same_side", ("KLH", "DKL", "x", "g", "f", "FE"),
     {"a": "k"}),
    # euclid_finish's ¬between g f e, rendered as a by_contra (as in I.22')
    ("by_contra", [
        ([("between", "g", "f", "e")], [
            ("between_trans_out", ("g", "f", "e", "e''"), {}),
            ("between_symm_parts", ("f", "g", "e''"), {}),
        ]),
    ], [("not_between", "g", "f", "e")]),
]
# The goals: I.22''s five conjuncts plus same_side k x FE.
PROP22PP_GOAL = PROP22P_GOAL + [("same_side", "k", "x", "FE")]


# ---------------------------------------------------------------------------
# Book I.23' (Prop23.lean:35, theorem proposition_23'): copy an angle onto
# a given line at a given point, on EITHER side —
#   distinctPointsOnLine a b AB ∧ formRectilinearAngle d c e CD CE ∧
#   ¬(x.onLine AB) →
#     ∃ f, f ≠ a ∧ (f.onLine AB ∨ f.sameSide x AB) ∧ ∠f:a:b = ∠d:c:e
# The construction is proposition_23's verbatim (Prop23.lean:39-59) with
# two changes: the extra hypothesis ¬(x.onLine AB), and the main branch
# citing proposition_22'' c d c e e d a b x CD CE DE AB (line 54; the
# same-side variant) in place of proposition_22', so f lands on x's side
# of AB. The degenerate branches are unchanged from I.23: there the
# witness (b, or the extension of AB past a) is ON AB — the disjunction's
# left half (use_witness now carries the witness's on-line fact; in the
# between branch on_line f AB closes from between b a f).
#
# The disjunctive conclusion is a new goal/fact KIND, "either": one
# tuple ("either", d1, d2). A by_cases branch entails it when EITHER
# disjunct holds in the branch state (_fact_holds); the by_cases carries
# the disjunction itself out to the outer state (_conclude's plain-fact
# path), where neither disjunct needs to hold individually — exactly the
# join of the branch facts. This is the engine's first disjunctive
# conclusion; no earlier replay needed one.
PROP23P_HYPS_DATA = PROP23_HYPS_DATA + [
    ((("not_on_line", "x", "AB"), "h")),   # ¬(x.onLine AB)
]
# The existential's body over the eigenvariable f: the I.23 conjuncts
# plus the disjunction (shared by the outer by_cases, the nested
# by_cases, and the replay's goal).
PROP23P_CONCL_DATA = [
    ("neq", "f", "a"),
    ("either", ("on_line", "f", "AB"), ("same_side", "f", "x", "AB")),
    ("_eq_ang", "f", "a", "b", "d", "c", "e"),
]
PROP23P_STEPS_DATA = [
    ("by_cases", [
        # -- degenerate branch: d on CE, unchanged from I.23
        ([("on_line", "d", "CE")], [
            ("by_cases", [
                ([("_zero_ang", "d", "c", "e")], [
                    ("angle_self_zero", ("b", "a"), {}),
                    ("use_witness", ("f", "b", "a", "d", "c", "e", "AB"),
                     {"f": "f"}),
                ]),
                ([("between", "d", "c", "e")], [
                    ("flat_angle_onlyif", ("d", "c", "e"), {}),
                    ("extend_point", ("AB", "b", "a"), {"x": "f"}),
                    ("flat_angle_onlyif", ("b", "a", "f"), {}),
                    ("between_symm_parts", ("f", "a", "b"), {}),
                ]),
            ], PROP23P_CONCL_DATA),
        ]),
        # -- main branch: d off CE, proposition_22'' in place of 22'
        ([("not_on_line", "d", "CE")], [
            ("line_from_points", ("d", "e"), {"L": "DE"}),
            ("proposition_20", ("c", "d", "e", "CD", "DE", "CE"), {}),
            ("proposition_20", ("d", "e", "c", "DE", "CE", "CD"), {}),
            ("proposition_20", ("e", "c", "d", "CE", "CD", "DE"), {}),
            # Prop23.lean:54 — as (f, g), with f same_side x AB
            ("proposition_22''",
             ("c", "d", "c", "e", "e", "d", "a", "b", "x",
              "CD", "CE", "DE", "AB"), {"k": "f", "g": "g"}),
            ("line_from_points", ("a", "f"), {"L": "AF"}),
            ("line_from_points", ("f", "g"), {"L": "FG"}),
            ("off_line_strict_triangle", ("f", "a", "g", "AB"), {}),
            ("degenerated_angle_if", ("a", "g", "b", "AB"), {}),
            ("angle_ray_eq_zero", ("g", "a", "b", "f"), {}),
            ("proposition_8",
             ("c", "d", "e", "a", "f", "g",
              "CD", "DE", "CE", "AF", "FG", "AB"), {}),
        ]),
    ], PROP23P_CONCL_DATA),
]
PROP23P_GOAL = PROP23P_CONCL_DATA


# ---------------------------------------------------------------------------
# Book I.24a (Prop24.lean:12-39) — the hinge theorem, FIRST HALF ONLY:
# the common construction (lines 18-34) and the one case Euclid treats
# (the d.sameSide g EF branch of line 35's by_cases, lines 36-39). The
# second bullet (lines 40-59, "Omitted by Euclid") is the NEXT packet:
# this replay's closing by_cases has ONE branch, so as a proof of I.24 it
# is INCOMPLETE — it establishes the goal only under the branch
# hypothesis d.sameSide g EF. The name PROP24A (and the test names) say
# so; do not read it as the whole proof.
#
# proposition_24 : formTriangle a b c AB BC AC ∧ formTriangle d e f DE EF
# DF ∧ |a─b| = |d─e| ∧ |a─c| = |d─f| ∧ (∠b:a:c > ∠e:d:f) → |b─c| > |e─f|.
# The two formTriangles destructure exactly as at PROP18/PROP19 (dpl +
# the four on_lines + the two non-degeneracy neqs + the off-line fact),
# once per triangle; the two length equalities and the angle comparison
# are metric hypotheses (the plain tuple forms _assume_metric_or_fact
# installs rows for).
PROP24A_HYPS_DATA = [
    (("dpl", "a", "b", "AB"), "h"),     # formTriangle a b c AB BC AC
    (("on_line", "b", "BC"), "h"), (("on_line", "c", "BC"), "h"),
    (("on_line", "c", "AC"), "h"), (("on_line", "a", "AC"), "h"),
    (("neq", "b", "c"), "h"), (("neq", "a", "c"), "h"),
    (("not_on_line", "c", "AB"), "h"),
    (("dpl", "d", "e", "DE"), "h"),     # formTriangle d e f DE EF DF
    (("on_line", "e", "EF"), "h"), (("on_line", "f", "EF"), "h"),
    (("on_line", "f", "DF"), "h"), (("on_line", "d", "DF"), "h"),
    (("neq", "e", "f"), "h"), (("neq", "d", "f"), "h"),
    (("not_on_line", "f", "DE"), "h"),
    (("eq_len", "a", "b", "d", "e"), "h"),   # |a─b| = |d─e|
    (("eq_len", "a", "c", "d", "f"), "h"),   # |a─c| = |d─f|
    (("gt_ang", "b", "a", "c", "e", "d", "f"), "h"),  # ∠b:a:c > ∠e:d:f
]
# Lean's shape (Prop24.lean:18-39), step by step:
#   18. proposition_23' d e a b c f DE AB AC as g'  (Book/Prop23.lean:35,
#       cited): g' ≠ d, ∠g':d:e = ∠b:a:c, and the DISJUNCTION
#       g'.onLine DE ∨ g'.sameSide f DE (the "either" fact kind).
#   -- The disjunction is RESOLVED BY ELIMINATING ITS LEFT HALF: g' on DE
#      would put the ray d→g' along DE, so ∠g':d:e ∈ {0, 2∟}; but
#      ∠g':d:e = ∠b:a:c is a proper triangle angle, 0 < ∠b:a:c < 2∟.
#      Rendered as a by_contra whose single branch assumes on_line g' DE
#      and whose inner by_contra splits on between g' d e (classical
#      P ∨ ¬P): flat_angle_onlyif (Transfer.lean:124) gives
#      ∠g':d:e = 2∟ against proposition_17 c a b AC AB BC
#      (∠c:a:b + ∠a:b:c < 2∟, with angle_pos on ∠a:b:c);
#      degenerated_angle_if (Transfer.lean:63) gives ∠g':d:e = 0 against
#      angle_pos on ∠b:a:c. The conclusion same_side g' f DE is the
#      disjunction's surviving right half (exhaustiveness: the stored
#      "either" plus the eliminated left — the caller's classical
#      responsibility, as at every by_contra use site).
#   19. line_from_points d g' as DG.
#   20. extend_point_longer DG d g' (a─c) as g''  (Points.lean:43):
#       between d g' g'', |g'─g''| > |a─c|.
#   21. proposition_3 d g'' a c DG AC as g  (Book/Prop03.lean:5, cited):
#       between d g g'', |d─g| = |a─c|; its gt_len premise |d─g''| > |a─c|
#       is |d─g'| + |g'─g''| > |g'─g''| > |a─c| (between_if + the
#       extension, closure).
#   22-23. line_from_points e g as EG, f g as FG. The neq premises are
#       SMT in Lean: e ≠ g closes from e off DG (two_lines_meet_once at d
#       over DG ≠ DE, g' off DE from step 18's resolution); f ≠ g is the
#       by_contra below — peq f g would identify ∠e:d:f with ∠e:d:g
#       (peq_subst), against ∠e:d:g = ∠b:a:c > ∠e:d:f (angle_ray_eq on
#       between d g g'' / d g' g'' plus step 18's equality, all closure).
#   24. proposition_4 a b c d e g AB BC AC DE EG DG  (cited): the SAS
#       angle premise ∠b:a:c = ∠e:d:g is exactly the ray-equality chain
#       above; makes |b─c| = |e─g| (the goal's hinge) and the two angle
#       equalities.
#   25. proposition_5' d g f DG FG DF  (cited): |d─g| = |a─c| = |d─f|,
#       so the base angles agree: ∠d:g:f = ∠d:f:g.
#   28. euclid_assert ∠e:d:g > ∠e:d:f — rendered as a by_contra (angle
#       trichotomy on the two angles, caller-documented): it needs only
#       the angle hypothesis and the ray-equality chain ∠e:d:g =
#       ∠e:d:g' = ∠b:a:c (BEFORE proposition_4's conclusion; Lean's SMT
#       and the engine's LA see the same rows).
#   29. euclid_assert g.sameSide f DE — diagrammatic, CLOSURE ONLY (no
#       cited step): pasch_2 (Diagrammatic.lean:153) on between d g g''
#       and between d g' g'' (d on DE; g, g' off DE — g' from step 18's
#       same_side, g from two_lines_meet_once over DG ≠ DE) puts g and
#       g' on g''s side of DE, and same_side_trans/symm
#       (Diagrammatic.lean:112,127) chain g ~ g'' ~ g' ~ f. The by_contra
#       below only exposes the already-closed fact (its branch assumes
#       opposing_sides g f DE, contradicting the closure-derived
#       same_side; exhaustiveness: g, f both off DE).
#   -- g off DF (needed by hgf's sum_angles_onlyif premise and by the
#      opposing_sides rendering of hgf's conclusion): by_contra on
#      on_line g DF, splitting g's position on DF relative to d and f
#      (g ≠ d since |d─g| = |a─c| > 0; trichotomy plus peq g f,
#      caller-documented): between g d f gives opposing_sides g f DE
#      (pasch_3_opposing) against step 29; between d g f / between d f g
#      identify the rays d→g and d→f (angle_ray_eq), peq g f identifies
#      the points (peq_subst) — each forces ∠e:d:g = ∠e:d:f against
#      step 28.
#   30-33. have hgf : ¬(g.sameSide e DF) — by_contra citing
#       sum_angles_onlyif d e f g DE DF (Transfer.lean:90): the branch
#       assumption (symmetrized) and step 29 (symmetrized) are its side
#       premises, so ∠e:d:f = ∠e:d:g + ∠g:d:f; with ∠e:d:g = ∠b:a:c >
#       ∠e:d:f the arithmetic forces ∠g:d:f < 0, against angle_pos
#       (g off DF — the step above). The conclusion is rendered as the
#       POSITIVE fact opposing_sides e g DF (Relations.lean:13-14: the
#       off-line parts hold — e off DF from the second triangle, g off DF
#       from the step above — and the branch eliminates the sameSide),
#       the same rendering pasch_4 and triple_incidence_2 use, since the
#       engine has no negated sameSide fact kind.
#   34. euclid_assert f.sameSide e DG — diagrammatic: triple_incidence_2
#       (Diagrammatic.lean:201) over (DE, DF, DG) meeting at d with
#       b := e, c := f, d := g: f sameSide g DE (step 29) and
#       opposing_sides e g DF (hgf) give e sameSide f DG; symm is
#       closure.
#   35-39. by_cases (d.sameSide g EF) — FIRST BRANCH ONLY (see the header):
#       36-37. the two angle asserts: triple_incidence_2 over (EF, DF, FG)
#              meeting at f with b := e, c := d, d := g turns the branch
#              hypothesis (d sameSide g EF) and hgf (opposing_sides e g
#              DF) into e sameSide d FG; then sum_angles_onlyif f e g d
#              EF FG gives ∠e:f:g = ∠e:f:d + ∠d:f:g and
#              sum_angles_onlyif g d f e DG FG gives
#              ∠d:g:f = ∠d:g:e + ∠e:g:f. With angle_pos on the two
#              split-off parts and step 25's ∠d:g:f = ∠d:f:g, the
#              arithmetic entails both asserts (∠d:f:g > ∠e:g:f and
#              ∠e:f:g > ∠e:g:f) — each rendered as a by_contra on the
#              angle trichotomy, as at step 28.
#       38. proposition_19 e f g EF FG EG (cited): ∠e:f:g > ∠e:g:f is
#           step 37's row; makes |e─g| > |e─f|.
#       39. euclid_finish: |b─c| = |e─g| (step 24) > |e─f| — the goal,
#           carried out of the branch as a _gt_len conclusion (_conclude
#           gained that case for this replay).
PROP24A_STEPS_DATA = [
    # Prop24.lean:18
    ("proposition_23'", ("d", "e", "a", "b", "c", "f", "DE", "AB", "AC"),
     {"f": "g'"}),
    # resolve the disjunction: eliminate on_line g' DE (see the header)
    ("by_contra", [
        ([("on_line", "g'", "DE")], [
            ("by_contra", [
                ([("between", "g'", "d", "e")], [
                    ("flat_angle_onlyif", ("g'", "d", "e"), {}),
                    ("proposition_17", ("c", "a", "b", "AC", "AB", "BC"),
                     {}),
                ]),
                ([("not_between", "g'", "d", "e")], [
                    ("degenerated_angle_if", ("d", "g'", "e", "DE"), {}),
                ]),
            ], [("false",)]),
        ]),
    ], [("same_side", "g'", "f", "DE")]),
    # Prop24.lean:19-20
    ("line_from_points", ("d", "g'"), {"L": "DG"}),
    ("extend_point_longer", ("DG", "d", "g'", "a", "c"), {"x": "g''"}),
    # neq d g'' (dpl d g'' DG for the cut; closure does not derive neqs
    # from between — the I.13/14 pattern)
    ("between_symm_parts", ("d", "g'", "g''"), {}),
    # Prop24.lean:21-22
    ("proposition_3", ("d", "g''", "a", "c", "DG", "AC"), {"q": "g"}),
    ("between_symm_parts", ("d", "g", "g''"), {}),   # neq d g (dpl d g DG)
    ("line_from_points", ("e", "g"), {"L": "EG"}),
    # f ≠ g (line_from_points f g's premise): peq f g identifies
    # ∠e:d:f with ∠e:d:g (peq_subst closure) against the angle hypothesis
    ("by_contra", [
        ([("peq", "f", "g")], []),
    ], [("neq", "f", "g")]),
    ("line_from_points", ("f", "g"), {"L": "FG"}),
    # Prop24.lean:24-25
    ("proposition_4", ("a", "b", "c", "d", "e", "g",
                       "AB", "BC", "AC", "DE", "EG", "DG"), {}),
    ("proposition_5'", ("d", "g", "f", "DG", "FG", "DF"), {}),
    # Prop24.lean:28 — euclid_assert ∠e:d:g > ∠e:d:f (angle trichotomy)
    ("by_contra", [
        ([("eq_ang", "e", "d", "g", "e", "d", "f")], []),
        ([("gt_ang", "e", "d", "f", "e", "d", "g")], []),
    ], [("gt_ang", "e", "d", "g", "e", "d", "f")]),
    # Prop24.lean:29 — euclid_assert g.sameSide f DE (closure-derived;
    # exposed by contradicting its opposing_sides rendering)
    ("by_contra", [
        ([("opposing_sides", "g", "f", "DE")], []),
    ], [("same_side", "g", "f", "DE")]),
    # g off DF (hgf's premise support; trichotomy on DF — see the header)
    ("by_contra", [
        ([("on_line", "g", "DF"), ("between", "g", "d", "f")], [
            ("pasch_3_opposing", ("g", "d", "f", "DE"), {}),
        ]),
        ([("on_line", "g", "DF"), ("between", "d", "g", "f")], []),
        ([("on_line", "g", "DF"), ("between", "d", "f", "g")], []),
        ([("on_line", "g", "DF"), ("peq", "g", "f")], []),
    ], [("not_on_line", "g", "DF")]),
    # Prop24.lean:30-33 — hgf: ¬(g.sameSide e DF), rendered opposing_sides
    ("by_contra", [
        ([("same_side", "g", "e", "DF")], [
            ("sum_angles_onlyif", ("d", "e", "f", "g", "DE", "DF"), {}),
        ]),
    ], [("opposing_sides", "e", "g", "DF")]),
    # Prop24.lean:34 — euclid_assert f.sameSide e DG
    ("triple_incidence_2", ("DE", "DF", "DG", "d", "e", "f", "g"), {}),
    # Prop24.lean:35-39 — the d.sameSide g EF branch ONLY (the second
    # bullet, lines 40-59, is the next packet; this by_cases is
    # deliberately one-branched and proves I.24 only in Euclid's case)
    ("by_cases", [
        ([("same_side", "d", "g", "EF")], [
            # e sameSide d FG (for the vertex-g angle split)
            ("triple_incidence_2", ("EF", "DF", "FG", "f", "e", "d", "g"),
             {}),
            # ∠e:f:g = ∠e:f:d + ∠d:f:g and ∠d:g:f = ∠d:g:e + ∠e:g:f
            ("sum_angles_onlyif", ("f", "e", "g", "d", "EF", "FG"), {}),
            ("sum_angles_onlyif", ("g", "d", "f", "e", "DG", "FG"), {}),
            # Prop24.lean:36 — euclid_assert ∠d:f:g > ∠e:g:f
            ("by_contra", [
                ([("eq_ang", "d", "f", "g", "e", "g", "f")], []),
                ([("gt_ang", "e", "g", "f", "d", "f", "g")], []),
            ], [("gt_ang", "d", "f", "g", "e", "g", "f")]),
            # Prop24.lean:37 — euclid_assert ∠e:f:g > ∠e:g:f
            ("by_contra", [
                ([("eq_ang", "e", "f", "g", "e", "g", "f")], []),
                ([("gt_ang", "e", "g", "f", "e", "f", "g")], []),
            ], [("gt_ang", "e", "f", "g", "e", "g", "f")]),
            # Prop24.lean:38
            ("proposition_19", ("e", "f", "g", "EF", "FG", "EG"), {}),
        ]),
    ], [("_gt_len", "b", "c", "e", "f")]),
]
# The goal: |b─c| > |e─f| — under the branch hypothesis only (see the
# header); as an LA entailment query, as at I.18/I.19.
PROP24A_GOAL = [("_gt_len", "b", "c", "e", "f")]


# ---------------------------------------------------------------------------
# Book I.24 COMPLETE (Prop24.lean:12-59) — PROP24A's steps with the closing
# by_cases given its SECOND branch (lines 40-59, "Omitted by Euclid"). As a
# proof of I.24 this replay is the whole theorem: the split on
# d.sameSide g EF is now two-branched.
#
# The second-branch hypothesis ¬(d.sameSide g EF) is the NEW fact kind
# ("not_same_side", p, q, L) — the LITERAL negation, not the engine's
# usual opposing_sides rendering (see the fact-kind comment at the top):
# opposingSides would assert g off EF, but g's position on EF is exactly
# the nested split of lines 41-42, and a by_cases branch may not be
# contradictory (it would entail vacuously). Exhaustiveness of the outer
# split: classical P v ~P on the sameSide proposition (Lean's by_cases is
# the same classical split); no off-line side conditions are needed for
# the literal negation.
#
# Line 41's nested by_cases g.onLine EF:
#   - Sub-case on_line g EF (line 42, euclid_finish): the SMT closes the
#     GOAL, not a contradiction: hgf (opposing_sides e g DF,
#     Relations.lean:13-14) puts a point of DF between e and g; e, g on EF
#     force that point onto EF, and EF != DF meet only at f
#     (two_lines_meet_once, Diagrammatic.lean:14), so between e f g, whence
#     |e-g| = |e-f| + |f-g| > |e-f| (between_if, Transfer.lean:16; |f-g| > 0
#     from neq f g) and |b-c| = |e-g| (proposition_4). Rendered as a
#     by_contra deriving between e f g by line trichotomy (g, e, f distinct
#     on EF: neq e g from hgf's opposing parts, neq f g from the common
#     construction; exactly one between order holds, caller-documented as
#     at I.24a's g-on-DF split): between g e f puts f opposing g across DE
#     (pasch_2/pasch_3_opposing closure) against step 29's same_side;
#     between e g f puts g on e's side of DF (pasch_2 closure: between f g e,
#     f on DF) against hgf. Then LA closes the goal.
#   - Sub-case not_on_line g EF (lines 43-59): now d, g are BOTH off EF, so
#     opposing_sides_if (Relations.lean:13-14 intro) renders the branch
#     hypothesis as opposing_sides d g EF. extend_point FG g f as h
#     (Points.lean:32). The four "Not clear why these are needed" asserts:
#       45. ~(g.onLine DF): derived in the common construction (I.24a's
#           step-15 by_contra); needed as pasch_3_opposing's off-line
#           premise for opposing_sides g h DF and as proposition_13's
#           neq_line DF FG support. Exposed by contradicting on_line g DF.
#       46. ~(e.onLine DF): closure (two_lines_meet_once: DE != DF meet
#           only at d, e != d); needed as proposition_17 d f e's off-line
#           premise. Exposed by contradicting on_line e DF.
#       47. g.opposingSides e DF: hgf symmetrized (the defn is symmetric);
#           needed as opposing_opposing_same_side's first premise for
#           assert 48. Exposed by contradicting same_side g e DF.
#       48. h.sameSide e DF: between g f h crosses DF at f
#           (pasch_3_opposing: opposing_sides g h DF, h off DF from
#           two_lines_meet_once at f over FG != DF), so h and e both oppose
#           g across DF (opposing_opposing_same_side, hgf symmetrized).
#           Needed as sum_angles_onlyif f d e h DF EF's same_side e h DF
#           premise (the angle split behind the line-55 assert).
#     Then proposition_13 d f g h DF FG and e f g h EF FG
#     (angle-on-a-line sums = 2r each), proposition_17 d g e DG EG DE and
#     d f e DF EF DE (triangle angle sums < 2r), and the angle asserts.
#     Lines 53-55 (d:g:e < 2r, d:f:e < 2r, and the 4r sum) are LINEAR
#     consequences of the cited rows (angle_pos on g:e:d / f:e:d for the
#     first two; the two proposition_13 rows plus sum_angles_onlyif
#     f d e h DF EF's d:f:e = d:f:h + h:f:e for the third) — the engine's
#     constraint store derives them inside the final assert's check, so
#     they are pinned as LA entailments in the PROP24 tests rather than
#     replay steps (no fact kind carries angle-vs-2r comparisons). The
#     line-56 assert e:f:g > e:g:f is rendered as a by_contra on the angle
#     trichotomy (as at I.24a's step 28): with the 4r sum, proposition_5''s
#     d:g:f = d:f:g, and sum_angles_onlyif g d e f DG EG's
#     d:g:e = d:g:f + f:g:e (premise same_side d f EG from
#     triple_incidence_2 over (DE, EF, EG) at e: same_side f g DE is
#     step 29 symmetrized, opposing_sides d g EF is the branch hypothesis
#     rendered), e:f:g - e:g:f = 4r - d:g:e - d:f:e > 0. Then
#     proposition_19 e f g EF FG EG makes |e-g| > |e-f| and
#     |b-c| = |e-g| (proposition_4) is the goal, carried out of the nested
#     by_cases and then the outer one as _gt_len.
PROP24_STEPS_DATA = PROP24A_STEPS_DATA[:-1] + [
    ("by_cases", [
        PROP24A_STEPS_DATA[-1][1][0],  # the d.sameSide g EF branch, verbatim
        ([("not_same_side", "d", "g", "EF")], [
            ("by_cases", [
                # Prop24.lean:41-42 — g.onLine EF, closed by the goal
                ([("on_line", "g", "EF")], [
                    ("by_contra", [
                        ([("between", "g", "e", "f")], []),
                        ([("between", "e", "g", "f")], []),
                    ], [("between", "e", "f", "g")]),
                ]),
                # Prop24.lean:43-59 — g off EF
                ([("not_on_line", "g", "EF")], [
                    # line 45: assert ~(g.onLine DF) (already derived)
                    ("by_contra", [
                        ([("on_line", "g", "DF")], []),
                    ], [("not_on_line", "g", "DF")]),
                    # line 46: assert ~(e.onLine DF) (closure-derived)
                    ("by_contra", [
                        ([("on_line", "e", "DF")], []),
                    ], [("not_on_line", "e", "DF")]),
                    # line 47: assert g.opposingSides e DF (hgf symm)
                    ("by_contra", [
                        ([("same_side", "g", "e", "DF")], []),
                    ], [("opposing_sides", "g", "e", "DF")]),
                    # line 44
                    ("extend_point", ("FG", "g", "f"), {"x": "h"}),
                    ("between_symm_parts", ("g", "f", "h"), {}),  # neq f h
                    # h off DF / off EF (two_lines_meet_once at f)
                    ("by_contra", [
                        ([("on_line", "h", "DF")], []),
                    ], [("not_on_line", "h", "DF")]),
                    ("by_contra", [
                        ([("on_line", "h", "EF")], []),
                    ], [("not_on_line", "h", "EF")]),
                    # the branch hypothesis rendered positive (both off EF)
                    ("opposing_sides_if", ("d", "g", "EF"), {}),
                    # h across DF/EF from g; line 48: h.sameSide e DF
                    ("pasch_3_opposing", ("g", "f", "h", "DF"), {}),
                    ("opposing_opposing_same_side", ("e", "g", "h", "DF"),
                     {}),
                    ("pasch_3_opposing", ("g", "f", "h", "EF"), {}),
                    ("opposing_opposing_same_side", ("d", "g", "h", "EF"),
                     {}),
                    # same_side d f EG (for the vertex-g angle split)
                    ("triple_incidence_2", ("DE", "EF", "EG", "e",
                                            "d", "f", "g"), {}),
                    # d:g:e = d:g:f + f:g:e and d:f:e = d:f:h + h:f:e
                    ("sum_angles_onlyif", ("g", "d", "e", "f", "DG", "EG"),
                     {}),
                    ("sum_angles_onlyif", ("f", "d", "e", "h", "DF", "EF"),
                     {}),
                    # Prop24.lean:49-52
                    ("proposition_13", ("d", "f", "g", "h", "DF", "FG"), {}),
                    ("proposition_13", ("e", "f", "g", "h", "EF", "FG"), {}),
                    ("proposition_17", ("d", "g", "e", "DG", "EG", "DE"),
                     {}),
                    ("proposition_17", ("d", "f", "e", "DF", "EF", "DE"),
                     {}),
                    # Prop24.lean:56 — euclid_assert e:f:g > e:g:f (angle
                    # trichotomy; lines 53-55 are LA intermediates, header)
                    ("by_contra", [
                        ([("eq_ang", "e", "f", "g", "e", "g", "f")], []),
                        ([("gt_ang", "e", "g", "f", "e", "f", "g")], []),
                    ], [("gt_ang", "e", "f", "g", "e", "g", "f")]),
                    # Prop24.lean:57
                    ("proposition_19", ("e", "f", "g", "EF", "FG", "EG"),
                     {}),
                ]),
            ], [("_gt_len", "b", "c", "e", "f")]),
        ]),
    ], [("_gt_len", "b", "c", "e", "f")]),
]
# The goal: |b-c| > |e-f| — now unconditional (both branches conclude it).
PROP24_GOAL = [("_gt_len", "b", "c", "e", "f")]


# ---------------------------------------------------------------------------
# I.25 (converse of the hinge theorem) — from the cited proposition_4 and
# proposition_24 lemmas
# ---------------------------------------------------------------------------
# Book/Prop25.lean:7-21 — proposition_25 verbatim:
#   ∀ (a b c d e f : Point) (AB BC AC DE EF DF : Line),
#   formTriangle a b c AB BC AC ∧ formTriangle d e f DE EF DF ∧
#   (|(a─b)| = |(d─e)|) ∧ (|(a─c)| = |(d─f)|) ∧ (|(b─c)| > |(e─f)|) →
#   (∠ b:a:c > ∠ e:d:f)
# The two formTriangles destructure exactly as at PROP24A (dpl + the four
# on_lines + the two non-degeneracy neqs + the off-line fact), once per
# triangle; the two length equalities and the length comparison are
# metric hypotheses (the plain tuple forms _assume_metric_or_fact
# installs rows for).
PROP25_HYPS_DATA = [
    (("dpl", "a", "b", "AB"), "h"),     # formTriangle a b c AB BC AC
    (("on_line", "b", "BC"), "h"), (("on_line", "c", "BC"), "h"),
    (("on_line", "c", "AC"), "h"), (("on_line", "a", "AC"), "h"),
    (("neq", "b", "c"), "h"), (("neq", "a", "c"), "h"),
    (("not_on_line", "c", "AB"), "h"),
    (("dpl", "d", "e", "DE"), "h"),     # formTriangle d e f DE EF DF
    (("on_line", "e", "EF"), "h"), (("on_line", "f", "EF"), "h"),
    (("on_line", "f", "DF"), "h"), (("on_line", "d", "DF"), "h"),
    (("neq", "e", "f"), "h"), (("neq", "d", "f"), "h"),
    (("not_on_line", "f", "DE"), "h"),
    (("eq_len", "a", "b", "d", "e"), "h"),   # |a─b| = |d─e|
    (("eq_len", "a", "c", "d", "f"), "h"),   # |a─c| = |d─f|
    (("gt_len", "b", "c", "e", "f"), "h"),   # |b─c| > |e─f|
]
# Lean's shape (Prop25.lean:13-20): by_contra on the goal, then by_cases
# on ∠b:a:c = ∠e:d:f. The negated goal enters exactly as at PROP19:
# Lean's by_contra assumes ¬(∠b:a:c > ∠e:d:f) and the by_cases splits it
# — branch 1's assumption is the equality; branch 2's is Lean's other
# case (¬(>) ∧ ¬(=)) rendered directly as ∠e:d:f > ∠b:a:c. The two
# branches split ¬(∠b:a:c > ∠e:d:f) exactly, and their exhaustiveness is
# trichotomy on the two angles — the caller's responsibility, documented
# here as at every by_contra use site.
# Branch 1 (∠b:a:c = ∠e:d:f): proposition_4 a b c d e f AB BC AC DE EF
#   DF (Lean's argument order, Prop25.lean:15) — the formTriangle
#   premises close from the hypotheses, the two length entailments are
#   the hypotheses, and the angle entailment is the branch assumption —
#   gives |b─c| = |e─f| against the hypothesis |b─c| > |e─f| — store
#   unsat.
# Branch 2 (∠e:d:f > ∠b:a:c): euclid_assert (Prop25.lean:18) IS the
#   branch assumption here — there is nothing to derive, the assumption
#   is already the strict angle row. proposition_24 d e f a b c DE EF DF
#   AB BC AC (Lean's argument order, Prop25.lean:19): the formTriangle
#   premises close from the hypotheses; the length entailments
#   |d─e| = |a─b| and |d─f| = |a─c| are the hypotheses' equalities
#   (symmetric); the angle entailment is the branch assumption exactly —
#   gives |e─f| > |b─c| against the hypothesis — store unsat. Both
#   contradictions are arithmetic (euclid_finish's SMT in Lean), so no
#   further steps are named. The by_contra conclusion is the goal
#   itself.
PROP25_STEPS_DATA = [
    ("by_contra", [
        # branch 1: ∠b:a:c = ∠e:d:f
        ([("eq_ang", "b", "a", "c", "e", "d", "f")], [
            ("proposition_4", ("a", "b", "c", "d", "e", "f",
                               "AB", "BC", "AC", "DE", "EF", "DF"), {}),
        ]),
        # branch 2: ∠e:d:f > ∠b:a:c (Lean's ¬(>) ∧ ¬(=) case)
        ([("gt_ang", "e", "d", "f", "b", "a", "c")], [
            ("proposition_24", ("d", "e", "f", "a", "b", "c",
                                "DE", "EF", "DF", "AB", "BC", "AC"), {}),
        ]),
    ], [("gt_ang", "b", "a", "c", "e", "d", "f")]),
]
# The goal as an LA entailment query: ∠b:a:c > ∠e:d:f.
PROP25_GOAL = [("_gt_ang", "b", "a", "c", "e", "d", "f")]


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
        elif p == "8":
            t0 = time.time()
            s, rep = replay(PROP8_HYPS_DATA, PROP8_STEPS_DATA,
                            [("_eq_ang", "b", "a", "c", "e", "d", "f")])
            rep[-1]["seconds"] = round(time.time() - t0, 4)
        elif p == "9":
            t0 = time.time()
            s, rep = replay(PROP9_HYPS_DATA, PROP9_STEPS_DATA,
                            [("_eq_ang", "b", "v", "f", "c", "v", "f"),
                             ("neq", "v", "f")])
            rep[-1]["seconds"] = round(time.time() - t0, 4)
        elif p == "10":
            t0 = time.time()
            s, rep = replay(PROP10_HYPS_DATA, PROP10_STEPS_DATA,
                            [("between", "a", "d", "b"),
                             ("_eq_len", "a", "d", "d", "b")])
            rep[-1]["seconds"] = round(time.time() - t0, 4)
        elif p == "11":
            t0 = time.time()
            s, rep = replay(PROP11_HYPS_DATA, PROP11_STEPS_DATA,
                            [("not_on_line", "f", "AB"),
                             ("_rt_ang", "u", "w", "f")])
            rep[-1]["seconds"] = round(time.time() - t0, 4)
        elif p == "12":
            t0 = time.time()
            s, rep = replay(PROP12_HYPS_DATA, PROP12_STEPS_DATA,
                            [("_rt_ang", "e", "h", "c"),
                             ("on_line", "h", "AB")])
            rep[-1]["seconds"] = round(time.time() - t0, 4)
        elif p == "13":
            t0 = time.time()
            s, rep = replay(PROP13_HYPS_DATA, PROP13_STEPS_DATA,
                            [PROP13_GOAL])
            rep[-1]["seconds"] = round(time.time() - t0, 4)
        elif p == "14":
            t0 = time.time()
            s, rep = replay(PROP14_HYPS_DATA, PROP14_STEPS_DATA,
                            PROP14_GOAL)
            rep[-1]["seconds"] = round(time.time() - t0, 4)
        elif p == "15":
            t0 = time.time()
            s, rep = replay(PROP15_HYPS_DATA, PROP15_STEPS_DATA,
                            PROP15_GOAL)
            rep[-1]["seconds"] = round(time.time() - t0, 4)
        elif p == "16b":
            t0 = time.time()
            s, rep = replay(PROP16B_HYPS_DATA, PROP16B_STEPS_DATA,
                            PROP16B_GOAL)
            rep[-1]["seconds"] = round(time.time() - t0, 4)
        elif p == "16a":
            t0 = time.time()
            s, rep = replay(PROP16A_HYPS_DATA, PROP16A_STEPS_DATA,
                            PROP16A_GOAL)
            rep[-1]["seconds"] = round(time.time() - t0, 4)
        elif p == "17":
            t0 = time.time()
            s, rep = replay(PROP17_HYPS_DATA, PROP17_STEPS_DATA,
                            PROP17_GOAL)
            rep[-1]["seconds"] = round(time.time() - t0, 4)
        elif p == "18":
            t0 = time.time()
            s, rep = replay(PROP18_HYPS_DATA, PROP18_STEPS_DATA,
                            PROP18_GOAL)
            rep[-1]["seconds"] = round(time.time() - t0, 4)
        elif p == "19":
            t0 = time.time()
            s, rep = replay(PROP19_HYPS_DATA, PROP19_STEPS_DATA,
                            PROP19_GOAL)
            rep[-1]["seconds"] = round(time.time() - t0, 4)
        elif p == "20":
            t0 = time.time()
            s, rep = replay(PROP20_HYPS_DATA, PROP20_STEPS_DATA,
                            PROP20_GOAL)
            rep[-1]["seconds"] = round(time.time() - t0, 4)
        elif p == "21":
            t0 = time.time()
            s, rep = replay(PROP21_HYPS_DATA, PROP21_STEPS_DATA,
                            PROP21_GOAL)
            rep[-1]["seconds"] = round(time.time() - t0, 4)
        elif p == "22":
            t0 = time.time()
            s, rep = replay(PROP22_HYPS_DATA, PROP22_STEPS_DATA,
                            PROP22_GOAL)
            rep[-1]["seconds"] = round(time.time() - t0, 4)
        elif p == "22'":
            t0 = time.time()
            s, rep = replay(PROP22P_HYPS_DATA, PROP22P_STEPS_DATA,
                            PROP22P_GOAL)
            rep[-1]["seconds"] = round(time.time() - t0, 4)
        elif p == "22''":
            t0 = time.time()
            s, rep = replay(PROP22PP_HYPS_DATA, PROP22PP_STEPS_DATA,
                            PROP22PP_GOAL)
            rep[-1]["seconds"] = round(time.time() - t0, 4)
        elif p == "23":
            t0 = time.time()
            s, rep = replay(PROP23_HYPS_DATA, PROP23_STEPS_DATA,
                            PROP23_GOAL)
            rep[-1]["seconds"] = round(time.time() - t0, 4)
        elif p == "23'":
            t0 = time.time()
            s, rep = replay(PROP23P_HYPS_DATA, PROP23P_STEPS_DATA,
                            PROP23P_GOAL)
            rep[-1]["seconds"] = round(time.time() - t0, 4)
        elif p == "24a":
            t0 = time.time()
            s, rep = replay(PROP24A_HYPS_DATA, PROP24A_STEPS_DATA,
                            PROP24A_GOAL)
            rep[-1]["seconds"] = round(time.time() - t0, 4)
        elif p == "24":
            t0 = time.time()
            s, rep = replay(PROP24A_HYPS_DATA, PROP24_STEPS_DATA,
                            PROP24_GOAL)
            rep[-1]["seconds"] = round(time.time() - t0, 4)
        elif p == "25":
            t0 = time.time()
            s, rep = replay(PROP25_HYPS_DATA, PROP25_STEPS_DATA,
                            PROP25_GOAL)
            rep[-1]["seconds"] = round(time.time() - t0, 4)
        elif p == "3":
            s = State()
            for f, w in PROP3_HYPS_DATA:
                s.assume(f, w)
            s.la.add(lin((len_atom("a", "b"), 1), (len_atom("c0", "c1"), -1)),
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
        # review fix (claude-9): an entailment on an unsat store holds
        # vacuously. The I.5 replay "proved" its goal on such a store for
        # weeks (|bc| = 0 from a dict-literal collision). Every replay's
        # final store must be satisfiable, except I.7, whose hypotheses are
        # contradictory by design (Lean's proposition_7 concludes False).
        if p != "7":
            assert _fm_sat(s.la.rows), f"I.{p}: final store is unsat"
        for r in rep:
            print(r)
        print()


if __name__ == "__main__":
    main()



