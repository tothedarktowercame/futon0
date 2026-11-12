# LINE: a smallest geometric rung

Discovery note, 2026-11-12.  This spells out the candidate left open by
`TARSKI-discovery.md`.  The primary source is Michael Beeson, “A constructive
version of Tarski's geometry” (2015), especially §§2.3, 4.1--4.2 and 5.1,
pp. 11--13 and 23--28.  “Axiom” below means Beeson's/Tarski's axiom; “derived”
means a theorem cited there, not an extra source axiom.

## 1. Candidate and boundary

Use one sort, **Point**, intended to lie on a single line, with facts

```text
B(a,b,c)       -- b is strictly between a and c
E(a,b,c,d)     -- segment ab is congruent to cd
a = b
a != b         -- positive apartness/distinctness, not absence of equality
```

and the guarded operation

```text
ext(q,a,b,c)   -- from q through a, extend beyond a by length bc; guard q != a
```

This is the right first implementation fragment, with one qualification:
Beeson's A4-i concludes **non-strict** betweenness `T(q,a,x)`, because `b=c`
makes the result `x=a`.  With only strict `B`, the operation therefore has one
always-valid metric result and one strictly ordered result with an additional
non-null-length guard:

```text
q != a                         => E(a,ext(q,a,b,c),b,c)
q != a, b != c                 => B(q,a,ext(q,a,b,c))
q != a, b = c                  => ext(q,a,b,c) = a
```

The last two rules are the strict-`B` decomposition of A4-i, not its literal
displayed form.  Keeping `T` primitive would reproduce the source more
literally, but strict `B` is the better game rung: it makes progress visible,
prevents a zero-length “construction” from masquerading as a new point, and
matches the independent rational model below.

This is not “one-dimensional Tarski geometry” as a separately sourced complete
theory.  It is the line reduct of Beeson's constructive Tarski axioms.  A8--A10,
Pasch, circles, and continuity are deliberately absent because their intended
models are planes.  Completeness here means complete **for the declared line
game**, not a claim of quantifier elimination or completeness for all truths of
ordered metric lines.

## 2. Rules in engine form

`metagame_eq.rule_statements` represents an unconditional equation as
`(name,left,right)`.  Only **EQ** rules below have that shape.  **HORN**,
**NEG**, and **OR** require engine features not present in `metagame_eq.py`.
Ordinary equality substitution and reflexivity are logic, not listed as
geometric rules.

### Congruence and metric rules

```text
E(a,b,b,a)                                                   [EQ/HORN]
```

A1, reflexivity/symmetry of equidistance (Beeson §4.1, p. 23).  In plain
equational notation this is `distance(a,b) = distance(b,a)`, but with primitive
`E` it is a zero-premise fact, not a term equation.

```text
E(a,b,r,s), E(a,b,p,q) => E(p,q,r,s)                         [HORN]
```

A2, transitivity of equidistance (p. 23).

```text
E(a,b,c,c) => a = b                                         [HORN]
```

A3, identity of equidistance (p. 23).

```text
a != b,
B(a,b,c), B(A,B,C),
E(a,b,A,B), E(b,c,B,C), E(a,d,A,D), E(b,d,B,D)
    => E(c,d,C,D)                                            [HORN]
```

A5, five-segment (p. 23), specialized from non-strict `T` to strict `B`.
This specialization is sound but weaker than the displayed source axiom in
degenerate cases.  It is enough for nondegenerate line-game instances.

### Extension operation

For `x = ext(q,a,b,c)`:

```text
q != a             => E(a,x,b,c)                             [HORN]
q != a, b != c     => B(q,a,x)                               [HORN]
q != a, b = c      => x = a                                  [HORN]
```

These jointly encode Skolemized A4-i (Beeson §4.2, p. 24; motivation §5.1,
p. 26).  The operation is **not offered** until `q != a` is present.  The
source states `T(q,a,x) ∧ E(a,x,b,c)` under that guard.  Strictness for `b!=c`
and collapse for `b=c` follow in the rational model and from A3 plus equality
substitution; their status as the chosen strict-`B` interface should therefore
be recorded as a fragment design decision.

### Betweenness rules

```text
B(a,b,a) => contradiction                                   [NEG]
```

A6-i, written by Beeson as `not B(a,b,a)` (§4.2, p. 24).  This is a constraint,
not a monotone positive conclusion.

```text
B(a,b,c) => B(c,b,a)                                        [HORN]
B(a,b,d), B(b,c,d) => B(a,b,c)                              [HORN]
```

A14-i (symmetry) and A15-i (inner transitivity), §4.2 p. 24.

The following useful order principles are theorems in classical Tarski geometry
listed in §5.3, pp. 28--29.  A14/A15 are reinstated as constructive axioms;
A16--A18 are not.  They are shown here to expose the engine boundary:

```text
B(a,b,c), B(b,c,d), b != c => B(a,b,d)                       [HORN, derived A16]
B(a,b,d), B(a,c,d) => B(a,b,c) OR B(a,c,b)                   [OR, derived A17]
B(a,b,c), B(a,b,d), a != b => B(a,c,d) OR B(a,d,c)           [OR, derived A18]
```

A17/A18 cannot be represented as “facts to one fact” without a branch or a
separate unresolved-disjunction fact.  They should be held out of the first
Horn closure rather than silently choosing an order.

### Distinctness consequences

These are short derived rules, necessary if distinctness is a positive fact:

```text
B(a,b,c) => a != b                                           [HORN, derived]
B(a,b,c) => b != c                                           [HORN, derived]
B(a,b,c) => a != c                                           [HORN, derived]
a != b => b != a                                             [HORN, equality logic]
```

Beeson defines strict betweenness to mean three distinct collinear points with
the middle point specified (§2.3, p. 11); the first three are therefore
definition-elimination rules, not separately numbered axioms.

## 3. Does it need refutation?

| item | Horn? | negative conclusion? | disjunction? |
|---|---|---|---|
| A1--A3 | yes | no | no |
| strict A4-i interface | yes | no | no |
| A5 | yes | no | no |
| A6-i | constraint | yes (`contradiction`) | no |
| A14/A15 | yes | no | no |
| A16 derived | yes | no | no |
| A17/A18 derived | no | no | yes |

**Build-order answer.** Positive existential goals in the initial fragment can
be proved by constructing and closing only.  Full useful reasoning about strict
order cannot: even proving a distinctness consequence semantically from a
negative formulation of `B` needs refutation, and total order/connectivity needs
case splitting.

Construction-and-closure example (segment reflection/doubling):

```text
hypotheses: q != a
construct:  x = ext(q,a,q,a)
goal:       B(q,a,x) AND E(a,x,q,a)
```

Since `q!=a`, the copied segment is non-null; the two A4-i interface rules close
both goals directly.

Assume-and-refute example:

```text
hypothesis: B(a,b,c)
goal:       a != c
assume:     a = c
substitute: B(a,b,a)
A6-i:       contradiction
discharge:  a != c
```

If the game treats `a!=c` as primitive positive data, this proof cannot be
simulated merely by adding facts to one position; it needs a temporary branch
or the derived distinctness rule above.  The recommended order is therefore:
(1) Horn facts and guarded construction, (2) constraints plus derived safe
negative facts, (3) explicit OR branches.

## 4. Candidate held-out theorems

Each target is a regular sequent: hypotheses, constructed points, conjunction
of facts.  None is an axiom instance with no auxiliary construction.

### H1. Double a non-null segment

```text
hypotheses: q != a
construct:  x = ext(q,a,q,a)
target:     B(q,a,x) AND E(q,a,a,x)
```

Uses A4-i and A1/A2 to orient congruence.

### H2. Copy and then repeat an arbitrary non-null length

```text
hypotheses: q != a, b != c
construct:  x = ext(q,a,b,c)
            y = ext(a,x,b,c)
target:     B(q,a,x) AND B(a,x,y)
            AND E(a,x,b,c) AND E(x,y,b,c)
```

This tests two guarded moves and closure across the first fresh point.

### H3. Three equal consecutive segments

```text
hypotheses: q != a
construct:  x = ext(q,a,q,a)
            y = ext(a,x,q,a)
target:     B(q,a,x) AND B(a,x,y)
            AND E(q,a,a,x) AND E(a,x,x,y)
```

This tests congruence orientation/transitivity, not just construction receipts.

### H4. Nested order after two extensions

```text
hypotheses: q != a, b != c, d != e
construct:  x = ext(q,a,b,c)
            y = ext(a,x,d,e)
target:     B(q,a,y) AND B(q,x,y)
```

This needs A14/A15/A16-style order closure in addition to both constructions.
It is a good discriminator between “we stored the move effects” and “we built a
line theory.”

### Midpoints are deliberately excluded

`ext` cannot uniformly construct a point *inside* a segment.  Adding a binary
midpoint operation `mid(a,c)` would make density a one-move theorem,
`a!=c => B(a,mid(a,c),c) ∧ E(a,mid(a,c),mid(a,c),c)`, but midpoint is a derived
construction in full Tarski geometry, not part of this sourced line reduct.
Adding it now would test a second primitive rather than the smallest rung.

## 5. Independent rational-coordinate oracle

Interpret every point as a rational number.  This model is independent of the
closure engine:

```text
B(a,b,c)       iff (a < b < c) OR (c < b < a)
E(a,b,c,d)     iff abs(a-b) = abs(c-d)
a = b          iff rational equality
a != b         iff rational inequality

ext(q,a,b,c)   = a + sign(a-q) * abs(b-c), provided q != a
```

Off the guard `q!=a`, `ext` is undefined and is not a legal move.  When `b=c`,
the formula returns `a`; otherwise it lies strictly beyond `a` on the ray from
`q` through `a`.  Every derived fact can therefore be checked by exact rational
arithmetic, with no appeal to the engine's union-find or closure.

The oracle should check each rule universally over a bounded rational sample
and each produced construction directly.  That is a falsifier, not a proof of
the axiom system: passing samples cannot establish universal validity, while a
single false derived fact is decisive.

## 6. Confirmed and unconfirmed

Confirmed directly against Beeson 2015: the signatures of `B`, `E`, and `ext`;
A1--A6, A14--A18; A4-i's `q!=a` guard; the fact that A14/A15 are constructive
axioms while A16--A18 are listed classical derived theorems; and the relevant
page/section locations.

Unconfirmed as a published named theory: that this exact selected reduct has a
standard completeness theorem under the name “one-dimensional constructive
Tarski geometry.”  No such claim is made here.  Also unconfirmed from a source
is a finite operation-only Pambuccian basis for precisely this reduct; the 2008
full text remained inaccessible.  The strict-`B` split of A4-i and the decision
to expose derived distinctness rules are explicit engine-interface choices,
validated by the rational model rather than quoted as Beeson's displayed
axioms.
