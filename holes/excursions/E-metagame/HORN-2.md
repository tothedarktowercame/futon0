# HORN-2 — hindsight setting on the line theory

Date: 2026-11-12.  Setter: `scripts/metagame_horn_setter.py`.  Raw records:
`~/code/storage/metagame/horn2/`.

## Method

Each run starts `line` with three or four given points and positive `neq` for
every given pair.  At each step it chooses uniformly from the complete public
`candidates` list and uses public `apply`.  Every final public fact absent
from the opening is a candidate one-atom goal.  Its proof is the
removal-minimal sequence returned by public `needed`; it is replayed with
public `start`, `apply`, and `entails`.

Canonical keys enumerate all renamings of the opening givens.  Relation
argument order is retained; equality and `neq` sides are unordered.

The labels overlap and do not remove records:

- **direct**: the goal is an immediate conclusion of one of the three `ext`
  interface rules for the last needed move (`E(a,x,b,c)`, `B(q,a,x)`, or the
  zero case `x=a`), or the A1 fact newly available for that constructed node.
  Consequences needing a further Horn rule are not direct.
- **symmetric**: the run also contains its A1, A14, or `neq_symm` image.
- **neq**: the goal is positive `Neq`.

“Non-trivial” below means none of those three labels.  This is deliberately
stricter than merely having a needed size greater than one.

## Bounded cells

The six requested cells ran under `futon-testing.slice`, `MemoryMax=8G`, and
`TasksMax=256`, with at most four workers and a 120-second alarm per cell.
No worker was relaunched.  A timeout interrupts the current proof
minimization, so only fully classified runs count.  All timeouts were caused
by the setter's many `needed` replays; separate construction-only probes show
that no closure crossed the 10-second stop threshold.

| givens | K | complete runs | runs/s | median closure | distinct records | direct | symmetric | neq | non-trivial needed 1/2/3/4+ |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 3 | 4 | 2 | 0.0167 | 0.0470 s | 190 | 69 | 177 | 7 | 0/0/1/0 |
| 3 | 6 | 1 | 0.0083 | 0.0418 s | 135 | 49 | 124 | 6 | 0/0/0/0 |
| 3 | 8 | 0 | 0 | 0.0701 s* | 0 | 0 | 0 | 0 | 0/0/0/0 |
| 4 | 4 | 2 | 0.0167 | 0.0597 s | 274 | 96 | 251 | 17 | 0/0/0/0 |
| 4 | 6 | 0 | 0 | 0.0714 s* | 0 | 0 | 0 | 0 | 0/0/0/0 |
| 4 | 8 | 0 | 0 | 0.0776 s* | 0 | 0 | 0 | 0 | 0/0/0/0 |

`*` A cell with no completed classification has no committed timing vector;
the displayed median is from one construction-only run with the same seed,
performed in a fresh bounded scope.  The largest individual closure in those
probes was 0.607 s.  Therefore K was not stopped for the specified >10 s
condition.  The K=8 attempts were started concurrently with K=6; both ended
by the setter's own 120-second alarm, not an external failure.

Across the 720 cell-seconds, exactly one distinct non-trivial non-`neq` goal
was found.  It needs three moves.  Thus observed yield is **5.0 distinct
goals/CPU-hour at needed size >=2** and **5.0/CPU-hour at >=3**.  The sample is
too small for a stable rate estimate; its useful result is that unbiased
hindsight setting is overwhelmingly symmetry/direct-fact production, while
greedy proof minimization is already the throughput bottleneck.

## Fresh replay and rational check

The packet requested ten non-trivial samples, but the bounded experiment
produced only one.  It is reported rather than padding the list with labelled
trivial goals:

```text
goal:
  ext(ext(b,c,a,c),c,c,a) = ext(ext(b,c,a,c),c,a,c)

needed moves:
  x = ext(b,c,a,c)
  y = ext(x,c,c,a)
  z = ext(x,c,a,c)
```

Replaying only those three moves from a fresh three-given opening entails the
goal.  Independently, with rational coordinates `a=-7`, `b=2`, `c=11` and
`ext(q,a,b,c) = a + sign(a-q)|b-c|`, both sides evaluate to the same rational
point.  Thus **1/1 available non-trivial samples re-proved and passed the
rational model check**.  Ten could not honestly be supplied within the
specified six cell budgets.

## Tests and interpretation

```text
PYTHONPATH=scripts ~/.venvs/openspiel/bin/python -m pytest -q \
  scripts/metagame_horn_setter_test.py
4 passed
```

The tests pin canonical-key invariance under renamed givens, fresh replay of
a `needed` set, symmetric-pair labeling, and the boundary between an immediate
extension conclusion and a transitive goal.  The experiment changes no
HORN-1 semantics and performs no semi-naive optimization.
