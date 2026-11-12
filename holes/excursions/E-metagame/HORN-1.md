# HORN-1 — guarded Horn construction games

Date: 2026-11-12.  Implementation: `scripts/metagame_horn.py` and
`scripts/metagame_horn_test.py`.

## Representation and boundary

The engine has one unsorted domain.  A position contains construction terms,
a union-find equality partition, and relation facts over equality classes.
`neq` has canonical unordered storage and symmetric lookup.  Congruence and
fact rewriting run after every merge.  A rule has relational, equational, or
`neq` premises and one such conclusion (or `contradiction`); matching is only
against present terms, so closure never constructs a node.  An operation is a
name, arity, argument variables, and Horn guards.  It is a legal move only if
its guards hold and the corresponding operation node is not already present.

`start`, `candidates`, `apply`, `entails`, `facts`, `needed`, and
`rule_statements` are the public game operations.  Goals are conjunctions and
may quantify variables existentially over present equality classes.
Inconsistency is position state, rather than an exception raised midway
through closure.

The `line` theory is the Horn portion of `LINE-fragment.md` section 2: A1,
A2, A3, strict A5, the metric/strict/zero cases of guarded `ext/4`, A6 as a
contradiction constraint, A14, A15, derived A16, and the four distinctness
rules.  A17 and A18 remain out because their conclusions are disjunctions.
The strict specialization of A5 and the three-way strict interface for A4-i
remain the fragment decisions documented there; no new source claim is made
here.  `group`, `left_group`, and `exponent_two` translate
`metagame_eq.RULE_LIBRARY` into zero-premise equational rules and unguarded
operations.

## Acceptance campaigns

The equational oracle used the old engine independently.  For each of group,
left-group, and exponent-two, 500 seeded runs made eight moves and compared
the complete equality partition after every move: **12,000 positions compared,
12,000 equal**.

The line oracle interprets points as rational numbers, strict betweenness as
ordered inclusion, congruence as equality of absolute differences, and
`ext(q,a,b,c)` as `a + sign(a-q) * abs(b-c)`.  In 1,000 seeded runs with 3--5
random rational givens and eight guarded extensions, every derived relation
and equality was checked: **94,509 facts checked, no false fact found**.  The
campaign deliberately includes a nonzero first extension and seven
zero-length extensions, exercising both strict and collapsed operation
results without making the generic A2/A5 matcher dominate the test budget.

Focused tests additionally cover the guard with and without positive `neq`,
A15 waiting for its second premise, zero-length collapse, A6 inconsistency,
H1--H4, existential witness search, and dependency removal by `needed`.

Commands and results:

```text
PYTHONPATH=scripts ~/.venvs/openspiel/bin/python -m pytest -q \
  scripts/metagame_horn_test.py -k equational
1 passed; equational positions compared: 12000

PYTHONPATH=scripts ~/.venvs/openspiel/bin/python -m pytest -q \
  scripts/metagame_horn_test.py -k rational
1 passed; line facts checked against Q: 94509

PYTHONPATH=scripts ~/.venvs/openspiel/bin/python -m pytest -q \
  scripts/metagame_horn_test.py -k 'not equational and not rational'
5 passed
```

## Timing

One process and one core were used.  Each recorded move copies a fixed
position, adds the first deterministic legal operation, and closes it; thus
the median closure column is the closure-dominated public move time and
`moves/s` is its reciprocal.  For line, the chosen move is a guarded
zero-length extension, avoiding an accidental benchmark of one particular
nonzero theorem.  Three samples were used at 10 and 20 elements and for all
group rows.  At 40 line elements, two completed samples were 20.869 and
21.985 seconds (median 21.427 seconds).  Candidate counts are
complete; line's count is `n^3(n-1)` without materializing the list.

| theory | elements | candidates | moves/s | median closure |
|---|---:|---:|---:|---:|
| line | 10 | 9,000 | 10.84 | 93.8 ms |
| line | 20 | 152,000 | 0.760 | 1,257 ms |
| line | 40 | 2,496,000 | 0.0467 | 21,427 ms |
| group | 10 | 103 | 12,853 | 0.078 ms |
| group | 20 | 403 | 7,732 | 0.129 ms |
| group | 40 | 1,603 | 4,148 | 0.241 ms |

For context, EQ-3 records 0.602 ms closure at 40 group elements.  The Horn
engine is faster on this particular group sample because it has a small
indexed equality workload, but the line result exposes the expected next
bottleneck: the generic multi-premise relation matcher repeatedly joins large
`E` tables.  Semi-naive/delta rule evaluation and per-relation argument
indexes are the next optimization, not a semantic shortcut.

## Findings

No rule in the accepted line note proved false in the rational campaign.
Two representational points needed to remain explicit.  First, A1 is a
zero-premise relation fact `E(a,b,b,a)`, not a term equation in this engine.
Second, `neq` is positive knowledge; failure to derive it cannot satisfy an
operation guard.  The engine still lacks disjunctive conclusions, branch-local
assumptions/refutation, negation discharge, sorts, and partial-operation
definedness beyond positive Horn guards.  Those omissions are exactly why
A17/A18 and the later refutation rung remain outside HORN-1.
