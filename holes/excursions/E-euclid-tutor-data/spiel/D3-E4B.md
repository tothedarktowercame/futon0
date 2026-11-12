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
