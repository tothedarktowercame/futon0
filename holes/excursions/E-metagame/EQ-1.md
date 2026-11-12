# EQ-1 — small equational construction engine

Date: 2026-11-12.  Implementation: `scripts/metagame_eq.py`.

## Acceptance

`scripts/metagame_eq_test.py` exercises congruence and every selectable law
both positively and with a mentioned term absent.  It also pins alias
suppression, removal-minimal proof extraction, one- and two-move shortest
proofs, and the three DEMO derivations.

The seeded soundness run used seed `20261112`, 2,000 runs, two generators and
eight uniformly selected candidate moves per run.  It independently checked
**6,493** equalities reported by the closure with free-word reduction; all
6,493 were valid.

DEMO move-list lengths:

| theory | goal | moves |
|---|---|---:|
| exponent_two | `mul(a,b) = mul(b,a)` | 18 |
| group | `inv(mul(a,b)) = mul(inv(b),inv(a))` | 8 |
| left_group | `mul(a,inv(a)) = e` | 6 |

## Timing

One Python process and one core were used.  For each size, a deterministic
group position was grown to that many recorded terms.  Candidate count is the
complete legal construction list.  The timing is the median over the first 30
candidates: “move/s” copies the position and calls the public `apply`; closure
time copies the position, inserts that candidate's one operation node, and
times `closure` alone.

| recorded elements | equality classes | candidates | moves/s | median closure |
|---:|---:|---:|---:|---:|
| 10 | 10 | 103 | 2,043 | 0.133 ms |
| 20 | 20 | 403 | 531 | 0.358 ms |
| 40 | 40 | 1,603 | 86 | 1.366 ms |

The public move timing includes regenerating the deterministic candidate list
inside `apply`; this is why it grows faster than closure itself.  The closure
uses present multiplication nodes for associativity rather than scanning all
term triples.

## Findings

The packet's model is sufficient as written.  One subtlety is important for
future adapters: optional ground hypotheses are not inserted as terms at
start.  They fire only after all of their mentioned terms have actually been
constructed, matching the same presence condition as the laws.  Goal
subterms, generators and `e` are inserted at start as specified.

`needed` is greedy and inclusion-minimal under removal; it is deliberately not
a minimum-cardinality solver.  `shortest_proof` deduplicates positions by the
set of constructed terms together with their equality partition, so commuting
construction orders do not produce distinct search states.
