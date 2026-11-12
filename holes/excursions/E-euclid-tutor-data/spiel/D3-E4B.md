# DESIGN-3 E4b-1: single- and two-fact ZERO training

This is the no-look-ahead arm requested after the exact look-ahead probe was
dropped.  The training data contain no Book proof moves or targets.  Both the
single-fact E1c records and the two-fact E4a records were generated from
rule-uniform random play and are labelled **ZERO**.

## Dataset

The builder at `a090398` compared every record with all three Book goals,
allowing the Book givens and made witnesses to be renamed and treating equal
segments as equivalence classes.  It also removed every example whose exact
logical-state key is a Book evaluation or E3 start state.  The initial
run-seed split was made state-disjoint by moving all held-out copies of a
training state into training.

| Measure | Single | Conjunctive | Total |
|---|---:|---:|---:|
| positions | 3,335 | 26,343 | 29,678 |
| held-out positions | 153 | 433 | 586 |
| training positions | 3,182 | 25,910 | 29,092 |
| source theorem records retained before state exclusions | 1,884 | 10,851 | 12,735 |

Seventy logical-state keys occurred on both sides of the run-seed split, so
641 held-out examples were moved to training.  The final train/held-out key
intersection is empty.  The protected-state test excluded 12,396 candidate
examples.  None of the three Book evaluation states occurs in training.

Ten deduplicated, otherwise eligible theorem records matched a Book goal and
were excluded.  Here are their statements and the generated moves that
witnessed them (the three equivalent two-equation presentations from each
equilateral construction are shown because they are distinct recorded
goals):

| Givens / seed | Excluded statement | Generated moves |
|---|---|---|
| I.3 / 2066111207 | `|b d_5| = |c0 c1|` (I.2 shape) | `circle_from_points(c0,c1)`; `intersections_circle_line(C_1,C)`; `proposition_2'(b,b_2,c0,C)` |
| I.3 / 2066111205 | `|c0 c1| = |c0 c_4|` and `|c0 c1| = |c1 c_4|` | `circle_from_points(c1,c0)`; `circle_from_points(c0,c1)`; `intersection_circles(C_1,C_2)` |
| I.3 / 2066111205 | `|c0 c1| = |c0 c_4|` and `|c0 c_4| = |c1 c_4|` | same three moves |
| I.3 / 2066111205 | `|c0 c1| = |c1 c_4|` and `|c0 c_4| = |c1 c_4|` | same three moves |
| I.2 / 2096111303 | `|ab| = |a c_4|` and `|ab| = |b c_4|` | `circle_from_points(b,a)`; `circle_from_points(a,b)`; `intersection_circles(C_2,C_3)` |
| I.2 / 2096111303 | `|ab| = |a c_4|` and `|a c_4| = |b c_4|` | same three moves |
| I.2 / 2096111303 | `|ab| = |b c_4|` and `|a c_4| = |b c_4|` | same three moves |
| I.2 / 2106111263 | `|ab| = |a c_6|` and `|ab| = |b c_6|` | `circle_from_points(a,b)`; `circle_from_points(b,a)`; `intersection_circles(C_3,C_2)` |
| I.2 / 2106111263 | `|ab| = |a c_6|` and `|a c_6| = |b c_6|` | same three moves |
| I.2 / 2106111263 | `|ab| = |b c_6|` and `|a c_6| = |b c_6|` | same three moves |

The ignored dataset and its audit receipt are
`/home/joe/storage/euclidzero/d3-e4b/mixed.pkl.gz` and
`mixed.pkl.gz.json`.

## Training

One fresh width-64 candidate model was trained with seed `2026111202`, batch
16, learning rate 0.001, fixed padded shapes, no value loss, and no look-ahead
feature.  It ran for the requested 2,400 seconds in
`codex26-d3-e4b-train.scope` with `MemoryMax=24G`, `TasksMax=1024`,
`PYTHONHASHSEED=0`, and the THREADS-1 caps.  The scope completed successfully;
the last live scope receipt recorded a 5,492,277,248-byte (5.12 GiB) memory
peak and 194 tasks.  The final snapshot is
`/home/joe/storage/euclidzero/d3-e4b/train/model`.

| Receipt | Policy cross-entropy |
|---|---:|
| step 1 | 5.5569 |
| first 10 receipts, mean | 3.9071 |
| last 10 receipts, mean | 2.7051 |
| step 2,975 | 2.2188 |

The run completed 2,993 optimizer steps.  Snapshots were also written every
250 steps.

## State-disjoint held-out raw priors

These measurements use raw network priors, without search.  A hit means that
at least one unapplied needed move is in the first one or five candidates.

| Model | Single top 1 | Single top 5 | Conjunctive top 1 | Conjunctive top 5 |
|---|---:|---:|---:|---:|
| generation 0 | 4.58% | 9.15% | 0.00% | 5.08% |
| E2b | 22.22% | **61.44%** | 21.48% | 44.34% |
| E4b mixed | **17.65%** | 50.98% | **29.33%** | **47.34%** |

The mixed model improves conjunctive top-1 by 7.85 percentage points over
E2b, while single-fact top-1 and top-5 are lower.  Over all 586 positions,
the mixed model is 26.28% top-1 and 48.29% top-5; E2b is 21.67% and 48.81%.

## Book transfer

Ranks are overall / rule / within-rule.  These are raw priors with the Book
goal and no search.

| Position | Candidates | Generation 0 | E2b | E4b mixed |
|---|---:|---:|---:|---:|
| I.1 one from solved | 35 | 7 / 4 / 1 | 1 / 1 / 1 | **1 / 1 / 1** |
| I.3 one from solved | 209 | 204 / 14 / 1 | **1 / 1 / 1** | 3 / 3 / 1 |
| I.2 stage 2, best of seven good moves | 532 | **1 / 1 / 1** | 12 / 6 / 1 | **11 / 4 / 1** |

The mixed model retains top-five transfer on I.1 and I.3 and improves E2b's
I.2 stage-2 rank from 12 to 11, but it does not reach the earlier threshold
of rank at most 7.

## I.1 auxiliary-construction probe

The goal is I.1's two equal-length facts.  Probabilities are softmax mass
over the whole premise-certified candidate list.

| Position / desired move | Candidates | Generation 0 | E2b | E4b mixed |
|---|---:|---:|---:|---:|
| after `circle_from_points(b,a)`: `circle_from_points(a,b)` | 31 | rank 1, p=0.03383 | rank 16, p=0.00167 | **rank 4, p=0.02049** |
| after both circles: `intersection_circles(C_1,C_2)` | 35 | rank 7, p=0.02921 | rank 1, p=0.49941 | **rank 1, p=0.49158** |

Thus two-fact training materially repairs the missing second-circle prior
without using a Book proof target, and preserves the correct intersection as
the top move once both circles exist.  It does not yet put the auxiliary
second circle first; that question belongs to the separately reviewed prover
rerun.

## Verification

The reporting extension was checked with:

```
PYTHONHASHSEED=0 python -m pytest -q scripts/euclid_hindsight_train_test.py
6 passed in 0.72s
```

The evaluation ran in a dedicated 12 GiB, 256-task scope and completed
normally.  Its machine-readable result is
`/home/joe/storage/euclidzero/d3-e4b/eval.json`.

## E4b-2: temperature selection and prover rerun

The prover now accepts a positive temperature `T` and uses
`softmax(logits / T)` for path costs.  `T=1` preserves the E3 ordering.  An
explicit optional hindsight goal drives both model features and the terminal
test during temperature selection; omitting it retains the Book behavior.

### Fixed Book-free selection

Selection seed `2026111206` chose 40 state-disjoint held-out theorem records
with needed size at least three: 20 single-fact and 20 conjunctive.  The size
histogram is 26 of size 3, 12 of size 4, one of size 5, and one of size 6.
The fixed records are:

| IDs | Kind | Proposition / run seed / needed size |
|---|---|---|
| 0--2 | conjunctive | I.3 / 2056111224 / 3 |
| 3--4 | conjunctive | I.3 / 2056111251 / 4 |
| 5 | conjunctive | I.3 / 2056111266 / 4 |
| 6--7 | conjunctive | I.3 / 2066111274 / 3 |
| 8 | conjunctive | I.3 / 2106111252 / 3 |
| 9, 11 | conjunctive | I.3 / 2126111259 / 4 |
| 10 | conjunctive | I.3 / 2126111259 / 5 |
| 12--13 | conjunctive | I.3 / 2136111204 / 3 |
| 14 | conjunctive | I.3 / 2136111204 / 4 |
| 15--18 | conjunctive | I.3 / 2136111243 / 3 |
| 19 | conjunctive | I.3 / 2136111243 / 4 |
| 20 | single | I.1 / 2036111235 / 4 |
| 21 | single | I.1 / 2056111224 / 4 |
| 22--23 | single | I.2 / 2036111260 / 3 |
| 24 | single | I.2 / 2096111218 / 3 |
| 25 | single | I.2 / 2136111220 / 6 |
| 26--27 | single | I.2 / 2136111220 / 3 |
| 28--30 | single | I.2 / 2136111271 / 3 |
| 31--32 | single | I.3 / 2026111217 / 3 |
| 33--34 | single | I.3 / 2026111229 / 4 |
| 35--37 | single | I.3 / 2066111207 / 3 |
| 38 | single | I.3 / 2076111206 / 3 |
| 39 | single | I.3 / 2126111210 / 4 |

The complete goals and canonical keys are in the ignored machine-readable
receipt `/home/joe/storage/euclidzero/d3-e4b-2/selection.json`.

### Environmental abort

The 12-worker temperature run was launched once in
`codex26-d3-e4b-temperature.scope` with `MemoryMax=24G`, `TasksMax=1024`,
`PYTHONHASHSEED=0`, and the THREADS-1 environment.  During JAX CPU-client
initialisation, multiple workers aborted with `pthread_create` error 11,
including failures to create `tf_XLAExecute`, `tf_XLACompile`,
`tf_XLAEigen`, and `tf_XLAPjRtCpuClient` threads.  LLVM likewise reported
`pthread_create failed: Resource temporarily unavailable`.  The
multiprocessing pool then repeatedly replaced the aborted workers.  The
recorded parent session was interrupted to stop that respawn loop; the run
was not relaunched, as required.

No search finished and `temperature.jsonl` has zero receipts.  Consequently
the selection table is:

| Temperature | Completed records | Solved | Mean calls on solved |
|---:|---:|---:|---:|
| 1 | 0 / 40 | unavailable | unavailable |
| 2 | 0 / 40 | unavailable | unavailable |
| 4 | 0 / 40 | unavailable | unavailable |

There is therefore no Book-free winning temperature.  The E3 problem matrix
was not run: choosing an arbitrary temperature after this failure would
violate the experiment's selection rule.  The temperature scope is inactive
and no worker survives.

### E4b-2b: capped-worker rerun

The authorized rerun applied THREADS-1's create-small-then-widen sequence to
every prover process.  Each worker was striped onto one CPU before creating
its JAX client and model, then restored to the inherited 32-CPU affinity.
BLAS, OpenMP, TensorFlow, NumExpr, and XLA launch caps were all one.

| Smoke | Workers seen | Tasks per worker | Scope tasks observed | Result |
|---|---:|---:|---:|---|
| small | 2 / 2 | 11, 11 | -- | pass |
| full | 12 / 12 | 11 each | 137 | pass |

This fixes the earlier `pthread_create` failure: all twelve JAX clients were
created and no thread-creation error occurred.

The fixed 40-record selection was then rerun once in
`codex26-d3-e4b-temperature2.scope`, still with `MemoryMax=24G` and
`TasksMax=1024`.  The task count stayed at 137, but twelve simultaneous
best-first searches accumulated large frontiers.  At 12m17s the scope reached
exactly 25,769,803,776 bytes (24.0 GiB), plus a 277.1 MiB swap peak.  systemd
recorded `Result=oom-kill`; aggregate CPU was 2h26m38s.  The run stopped
without agent action and was not relaunched.

Three T=1 searches completed before the OOM; all three solved and freshly
replayed (400 calls / 514.62s, 10 calls / 7.35s, and 6 calls / 1.09s).
These are partial receipts, not a temperature comparison:

| Temperature | Completed records | Solved among completed | Mean calls among completed solves |
|---:|---:|---:|---:|
| 1 | 3 / 40 | 3 | 138.7 |
| 2 | 0 / 40 | unavailable | unavailable |
| 4 | 0 / 40 | unavailable | unavailable |

Because the Book-free selection remains incomplete, no temperature was
chosen and the E3 matrix was not run.  Reducing worker concurrency would be
the direct memory correction, but the packet says not to relaunch after an
uncommanded stop.  The failed scope is inactive and no prover worker remains.
