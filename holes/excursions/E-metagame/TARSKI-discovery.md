# Operation-only geometry between the equational game and Euclid

Discovery note, 2026-11-12.  Local code was read at futon0 `998284d8` and
LeanEuclid `33a72c0f`.  The primary papers used were Pambuccian (2008), Beeson
(2015), Beeson--Narboux--Wiedijk (2019), and Avigad--Dean--Mumma (2009).

## Result in one paragraph

There is a real cross-walk, but not a collapse to the present equational game.
Pambuccian's programme shows that useful geometric fragments can be presented
as universal, quantifier-free theories whose vocabulary consists only of
construction operations.  Beeson's constructive Tarski theory supplies the
more direct bridge to Euclid: existential axioms become guarded Skolem
operations, and a logic of partial terms makes line intersection genuinely
partial.  But Beeson deliberately retains the point relations strict
betweenness `B` and segment congruence `E`; the axioms are mostly guarded
equations and relational conclusions, not unconditional equations.  System E
adds an explicit *co-exact* closure for incidence/order/side facts alongside
an *exact* metric closure.  Thus the next complete game rung should be a
monotone **guarded Horn/partial-operation game**, not merely a larger
union-find theory.

## 1. Signatures

### Pambuccian

The 2008 article is a survey of several theories, not one canonical
"Pambuccian system".  Its verified abstract says precisely that the languages
have no relation symbols and that operation symbols denote geometric
constructions (pp. 24--46; DOI 10.1016/j.jal.2007.02.001).  One small system
reported in the cited operation-only series has point variables, three
non-collinear constants, and:

* `M(a,b)` (arity 2), midpoint (with a totalized degenerate value);
* `T(a,b,c)` (arity 3), transport of a distance along a ray (again totalized
  outside its intended domain).

Another reported two-operation presentation uses ternary `R(a,b,c)`, reflection
of `c` in line `ab`, and ternary `U(a,b,c)`, circumcenter of `abc`, with arbitrary
values in degenerate cases.  Relations are encoded by equations between terms:
incidence, perpendicularity, congruence, and betweenness are definitions in the
operation algebra rather than primitive predicates.

**Source limitation.** The publisher/author PDF for the 2008 survey redirected
to an institutional login during this discovery.  The abstract and bibliographic
record were verified, and the operation signatures above were cross-checked in
the accessible descriptions of the underlying papers, but I could not verify
their complete displayed axiom lists from a primary full text.  Consequently I
do **not** give a fabricated axiom count below.  This is the principal unchecked
item in the note.

### Beeson constructive Tarski

The point-only base language has two relations (§2.2, pp. 9--10):

* `B(a,b,c)` (arity 3), **strict** betweenness;
* `E(a,b,c,d)` (arity 4), segment congruence, written `ab = cd`.

Non-strict `T(a,b,c)` and collinearity are defined from these.  The Skolemized
version (§§4, 6) adds constants `α,β,γ` witnessing lower dimension and:

| operation | arity | intended construction / guard |
|---|---:|---|
| `ext(q,a,b,c)` | 4 | extend ray `qa` by length `bc`; guard `q != a` |
| `ip(a,p,c,b,q)` | 5 | strict inner-Pasch intersection; strict-between and non-collinearity guards |
| `center(a,b,c)` | 3 | circumcenter; guard non-collinearity |
| `iℓ(a,b,p,q)` | 4 | intersection of two nonparallel, noncoincident lines; partial in ruler-and-compass Tarski |

The paper discusses Skolem functions for the two line--circle and two
circle--circle intersections (§§6.4, 14.5); they are continuous construction
functions with guarded domains, not unconditional total constructors.  In the
logic of partial terms, `t↓` is the definedness assertion (§4.3, pp. 24--25).

## 2. Axiom forms

### Pambuccian

What is verified is the *form*: universal closures of quantifier-free equations
in operation-only languages, with constructions totalized on degenerate input.
Some surveyed systems are finite equational bases; others use conditional
equations or universal infinitary sentences.  Exact per-system counts and the
requested partition into plain/conditional/negative/disjunctive axioms remain
**unchecked because the primary full text was unavailable**.  The important
game-design point is that arbitrary-value totalization hides partiality; a game
should expose the intended-domain equation as a guard rather than let an
off-domain arbitrary result become a move.

### Beeson (the complete constructive ruler-and-compass theory)

Section 4 (pp. 23--25) lists the classical basis A1--A9, triangle
circumscription A10', continuity, and the constructive changes.  Counting
displayed axiom *schemas*, the intuitionistic theory has:

* exact/equality core: A1--A3 (congruence equivalence/identity), A5
  (five-segment), A9 (upper dimension);
* constructions/order: guarded segment extension A4-i; no-self-between A6-i;
  guarded strict inner Pasch A7-i; lower dimension A8; triangle
  circumscription A10'; symmetry A14-i and inner transitivity A15-i;
* one continuity axiom in the chosen basis: two-point line--circle;
  circle--circle is proved later, not assumed;
* three stability schemas: double-negation stability of `B`, of `E`, and of
  equality.

That is 15 named geometric schemas (A1--A10' with A14/A15 and line--circle,
counting A1--A9 as nine) plus three logical stability schemas.  By syntactic
shape:

| form | schemas |
|---|---|
| plain equation | A1 (`ab=ba`) only |
| equations/relations under positive equational or relational premises | A2, A3, A5, A9, A14, A15 |
| explicit negative/distinctness guard | A4-i, A6-i, A7-i, A8, A10', line--circle; `iℓ` also has a noncoincidence guard |
| disjunctive conclusion | none in the axiom basis; connectivity disjunctions A17/A18 are derived classically and avoided as constructive axioms |
| continuity | two-point line--circle is axiomatic; circle--circle is a theorem (§14) |

The Skolem equations are still conditional: outside their guards a total
Skolem term has an unspecified value.  The `iℓ` extension improves this by
using partial terms: definedness implies both incidences, and uniqueness plus
the intersection guard determines its value (§4.3).

## 3. Exact, co-exact, and the System E translation

Using Manders' distinction as Avigad--Dean--Mumma do in §§2--3:

| Tarski/Beeson item | class | System E counterpart |
|---|---|---|
| A1--A3 congruence laws, A5 five-segment | exact | segment equality and metric inference |
| A4 extension | mixed | construction creates a point; betweenness is co-exact, length equality exact |
| A6, A7 Pasch, A14/A15 | co-exact | `between_*`, `pasch_*` diagram rules |
| A8/A9 dimension | mixed/global | noncollinearity/existence and exact equidistance constraints |
| A10' circumcenter | exact premise/conclusion plus co-exact noncollinearity guard | circle construction plus metric closure |
| line/circle and circle/circle continuity | co-exact existence guarded by exact radius facts | intersection constructions and their incidence/inside/outside side conditions |

This is visible locally in
`SystemE/Theory/Inferences/Diagrammatic.lean`: `between_symm`,
`between_trans_in/out`, `between_points`, `same_side_*`, `pasch_1`--`pasch_4`,
and circle incidence axioms are the co-exact closure.  Metric and transfer
rules live separately in `Inferences/Metric.lean` and `Transfer.lean`.

The ADM completeness proof **does** translate through a Tarski-style theory
`T`, but not by translating only conclusions.  Section 5.3 maps every System E
sequent (points, represented lines/circles, diagram literals, segment and angle
metric expressions) to a regular Tarski sequent; lines and circles become pairs
of defining points and metric sums/angles become positive-primitive Tarski
formulas (Lemma 5.5).  Section 5.4 translates a cut-free T proof back into E
(Lemmas 5.6 onward).  So "finding co-exact facts" is exactly part of the
translation and closure proof, not an omitted preprocessing oracle.

## 4. Case analysis

Beeson does not eliminate all cases; he eliminates *non-constructive* case
splits from the axioms/constructions by demanding unique, continuous outputs.
Strict Pasch excludes the collapsed configuration; segment extension excludes
the null ray.  Derived classical connectivity (`T(a,b,c) -> ... or ...`) is not
silently available intuitionistically.  The paper's constructive results use
uniform perpendicular and arithmetic constructions rather than splitting on a
sign or incidence (§§8--10).  Classical Tarski recovers finite alternative
constructions through Herbrand; constructive proofs yield one uniform term.

The proof-checked Book I still contains explicit natural-deduction cases and
contradictions.  Its I.1 proof has three contradiction blocks to rule out the
three collinear orders; its definition of collinearity exposes a six-way
disjunction (Appendix 1, pp. 41--43).  Thus constructive construction does not
mean "no disjunction in proofs".

## 5. Lemma layers before I.1

The proof-checking paper reports about **70 Book Zero theorems** (§8, p. 24)
and 235 theorems overall for 48 Book I propositions.  It does not publish a
machine-readable dependency depth for I.1--I.3, so exact graph depths are
**not stated by the paper**.

Appendix 1's formal I.1 has 36 displayed proof steps by a count of its
construction/assertion lines (the equilateral conclusion occurs at step 23;
the remainder proves noncollinearity).  Its named pre-I.1 lemma layer includes
inequality symmetry, local extension, congruence flip/symmetry/transitivity,
and part-not-equal-whole, plus common notions, circle postulates, and circle
incidence axioms.  I.2 additionally depends on I.1 and line--circle machinery;
I.3 depends on I.2.  Hence the proposition-level chain is I.3 -> I.2 -> I.1 ->
Book Zero/axioms, depth at least four layers; an exact lemma-edge depth is
unchecked without the repository's master dependency list.

LeanEuclid makes the same layering very compact: local `Book/Prop01.lean` has
five construction/application calls plus `euclid_finish`; Prop02 imports and
uses Prop01, and Prop03 imports and uses Prop02.  This is compact source, not a
claim that the discharged proof has only those logical steps.

## 6. Automation is the co-exact layer

In Beeson's paper, automation is metatheoretic rather than a named tactic:
Skolem terms build witnesses, stability/double-negation imports negative
classical consequences, and the Tarski lemma library supplies order and
incidence consequences.  The Proof-checking Euclid tool checks each explicit
Polish proof step and uses forward reasoning in Coq where ordinary automation
is weak (§§9--11); it does not search arbitrary Euclidean proofs.

LeanEuclid is explicit about the reverse layer.  `euclid_apply` creates the
construction/rule instance, asks `euclid_finish` to solve its antecedent, then
reconstructs the application.  `euclid_finish` runs `simp_all`, splits
conjunctions, translates the local context and goal to SMT, calls Z3 then cvc5,
and closes an UNSAT goal (`SystemE/Meta/Tactics/Solve.lean`, especially
`simpAndSplit`, `solveWithProvers`, and `EuclidApply`).  The facts discharged
include equality/congruence algebra, incidence, between/same-side consequences,
metric inequalities, and transfer facts registered by System E.  This is the
operational analogue of diagram closure: the author states the construction;
automation proves its co-exact and exact side conditions on demand.

## 7. Smallest complete rung for the metagame

The smallest *fully specified from the verified sources* is Beeson's
continuous/ruler-and-compass Tarski fragment with A1--A10', A14--A15,
two-point line--circle, stability, and guarded operations `ext`, `ip`, `center`,
and partial `iℓ`.  It is a game in its own right if positions contain:

1. constructed point terms (only guard-proved operations may be played);
2. monotone `B`, `E`, equality, inequality, and definedness facts;
3. guarded Horn closure plus stability/contradiction checking;
4. a goal literal or conjunction of literals.

It is larger than `metagame_eq.py` in exactly three ways: relations are not all
equations, negative guards matter, and closure sometimes exposes alternatives
rather than merging classes.  It is smaller and cleaner than full System E
because lines and circles remain represented by points and there is no separate
diagram object.

An operation-only Pambuccian `M,T` fragment could be an even closer syntactic
rung to `metagame_eq.py`, but calling a particular finite basis "complete"
requires the unavailable primary axiom list.  The safe implementation order is
therefore: guarded equations/partial terms first; Beeson's point theory as the
acceptance theory; only then test whether a verified Pambuccian basis is a
conservative operation-only presentation of the same game fragment.

## Sources

* Victor Pambuccian, “Axiomatizing geometric constructions,” *Journal of
  Applied Logic* 6 (2008), 24--46, DOI 10.1016/j.jal.2007.02.001. Abstract and
  metadata verified; full text inaccessible during this run.
* Michael Beeson, [“A constructive version of Tarski's geometry”](https://www.michaelbeeson.com/research/papers/ConstructiveTarski.pdf),
  *APAL* 166 (2015), especially §§2, 4--6, 8--10, 12, 14.
* Michael Beeson, Julien Narboux, Freek Wiedijk,
  [“Proof-checking Euclid”](https://www.michaelbeeson.com/research/papers/ProofCheckingEuclid.pdf),
  especially §§1, 5, 8--11 and Appendix 1.
* Jeremy Avigad, Edward Dean, John Mumma,
  [“A formal system for Euclid's Elements”](https://www.andrew.cmu.edu/user/avigad/Papers/formal_system_for_euclids_elements.pdf),
  especially §§2--3 and 5.3--5.4.
* Local LeanEuclid `SystemE/Theory/Inferences/*.lean`,
  `SystemE/Meta/Tactics/Solve.lean`, and `Book/Prop01.lean`--`Prop03.lean` at
  `33a72c0f`.
