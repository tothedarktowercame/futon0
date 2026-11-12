# EQ-3 — signatures and equations as data

Date: 2026-11-12. Implementation: `scripts/metagame_eq.py`.

## Representation

A `Theory` now contains an operation signature `(name, arity)` and `Rule`
values whose two sides are ordinary term trees with explicit `var` leaves.
The packet-1 constructor form, `Theory(frozenset({rule names}), hypotheses)`,
is retained and normalized to the corresponding data rules.  The closure has
no branch for a named law: it indexes the present operation nodes by equality
class and operation, matches a rule side, checks that every node on both sides
is already present, and unions the two result classes.  `rule_statements`
exposes the resulting equations as nested plain tuples.

`monoid` has only `e/0` and `mul/2`; `commutative_group` adds the equation
`mul(X,Y) = mul(Y,X)`.  `random_theory(seed)` deterministically chooses one or
two binary operations, at most one unary operation, at most one constant, and
two to four well-formed equations using at most three variables.

## Equivalence and acceptance

The test file retains the complete packet-1 closure as `_reference_closure`.
For each of `group`, `left_group`, and `exponent_two`, 2,000 seeded runs made
eight uniformly selected moves.  The equality partition from the generic
closure equalled the reference partition after every move: **48,000 positions
compared, 48,000 equal**.

The unchanged free-group and exponent-two parity soundness tests pass.  The
packet-1 engine tests and packet-2 setter tests pass unchanged.  Added tests
cover absence of `inv` from monoid candidates, associativity's four-product
presence condition, commutativity, a repeated pattern variable before and
after its arguments merge, inspectable rule statements, and 50 deterministic
random theories running 200 random legal moves each.  The full gate is 20
tests.

## Timing

The method is the same as EQ-1: one process and one core; a deterministic
group position grown to each recorded size; candidate count is complete;
move timing is the median of copying and public `apply` over the first 30
candidates; closure timing inserts that candidate and times only `closure`.

| elements | candidates | EQ-1 moves/s | EQ-3 moves/s | EQ-1 closure | EQ-3 closure |
|---:|---:|---:|---:|---:|---:|
| 10 | 103 | 2,043 | 2,062 | 0.133 ms | 0.077 ms |
| 20 | 403 | 531 | 390 | 0.358 ms | 0.149 ms |
| 40 | 1,603 | 86 | 35 | 1.366 ms | 0.602 ms |

The generic closure itself is faster in this sample because rule matching
uses indexes for `(class, operation)` and `(operation, argument classes)`.
The complete public move is 2.46 times slower at 40 elements, still below the
packet's three-times finding threshold; most of that measurement is candidate
materialization and membership validation rather than closure.

## Findings

The first generic matcher scanned every present term once per equality class.
It was semantically correct but made the 10,000-move random-theory gate take
more than ten minutes.  Indexing present nodes by class and signature reduced
that test to 72.72 seconds.  No old/new partition differed, no soundness test
failed, and no existing setter behavior required a change.
