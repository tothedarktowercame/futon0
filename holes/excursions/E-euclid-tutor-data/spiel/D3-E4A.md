# DESIGN-3 E4a: conjunctive hindsight goals

Date: 2026-11-12. Label: **ZERO**. Generation used no Book proof moves,
prefixes, or goals.

## Method and run

The rule-uniform generator can now emit pairs of individually non-trivial
facts which share a point made during the run. Their needed set starts as the
union of the atoms' provenance sets and is minimized by exact joint replay.
The pair key is order-independent and canonical up to renaming made objects.
Pairs which are both direct conclusions of one move are labelled `direct`.

An auxiliary move is an object-producing needed move for which none of its
created objects occurs in either goal atom (or the one atom for a single
record). `euclid_candidate_features.extract` now accepts a list of atoms; an
explicit copy of I.1's two Book atoms produces byte-identical arrays to the
ordinary Book-goal path.

The run used seed `2026111204`, twelve single-threaded workers, six plies,
rule-uniform sampling, `PYTHONHASHSEED=0`, and one scope with
`MemoryMax=24G` and `TasksMax=1024`. Sampling stopped at the fixed 30-minute
deadline. One pre-existing single-goal deletion replay did not return during
the drain; after three minutes with no new record, I stopped only the named
scope and job I started. There was no relaunch. All 98,906 already-streamed
records are the measurement, and the scope and workers are gone.

## Conjunctive yield

| Measure | Count |
|---|---:|
| raw records, single + conjunctive | 98,906 |
| raw single records | 87,509 |
| raw conjunctive records | 11,397 |
| distinct E2-filtered singles | 2,661 |
| distinct non-trivial conjunctions | 10,921 |
| conjunctions labelled direct | 0 |

| Needed moves | Distinct conjunctions |
|---:|---:|
| 2 | 1,327 |
| 3 | 4,709 |
| 4+ | 4,885 |

There are **822** I.1-shaped conjunctions: two object-producing moves
followed by a needed move which uses an output from each. This is a dependency
shape, not a Book-pattern hint; it is computed only after a random run.

## Auxiliary-construction count

Records can contain more than one auxiliary move; the table counts records
with at least one.

| Record kind | Needed size | Records | With auxiliary | Share |
|---|---:|---:|---:|---:|
| single | 2 | 1,241 | 404 | 32.6% |
| single | 3 | 981 | 752 | 76.7% |
| single | 4+ | 439 | 405 | 92.3% |
| conjunctive | 2 | 1,327 | 190 | 14.3% |
| conjunctive | 3 | 4,709 | 1,512 | 32.1% |
| conjunctive | 4+ | 4,885 | 2,899 | 59.3% |

For comparison, the held Book demonstrations have the following counts over
their object-producing construction moves. These rows are labelled **DEMO**
and were not used in generation.

| DEMO | Construction moves | Auxiliary | Share |
|---|---:|---:|---:|
| I.1 | 3 | 2 | 66.7% |
| I.2 | 8 | 7 | 87.5% |
| I.3 | 3 | 2 | 66.7% |

## Ten deterministic examples

Each example lists only its needed moves. Every one was replayed freshly from
the proposition's givens and both atoms held: **10/10 yes**.

1. I.2, size 4, replay **yes**:
   `point_between_points_shorter_than(BC,c,b,c,b) [a=a_1]`;
   `extend_point_longer(BC,c,b,a_1,c) [x=x_3]`;
   `between_symm_parts(c,a_1,x_3)`;
   `proposition_1(a_1,x_3,BC) [d=d_5]`.
   Goal: `|a_1 d_5| < |c x_3|` and `|b c| < |d_5 x_3|`.

2. I.2, size 3, replay **yes**:
   `extend_point_longer(BC,c,b,c,b) [x=x_2]`;
   `proposition_1(b,c,BC) [d=d_3]`;
   `circle_from_points(d_3,b) [C=C_6]`.
   Goal: `inside(d_3,C_6)` and `|b d_3| < |c x_2|`.

3. I.3, size 3, replay **yes**:
   `circle_from_points(b,a) [C=C_1]`;
   `intersections_circle_line(C_1,AB) [a=a_2 b=b_2]`;
   `proposition_1(b,b_2,AB) [d=d_4]`.
   Goal: `not_outside(d_4,C_1)` and `|a_2 b| = |b_2 d_4|`.

4. I.2, size 4, two auxiliary moves, replay **yes**:
   `proposition_1(c,b,BC) [d=d_2]`;
   `line_from_points(d_2,c) [L=L_3]`;
   `off_line_strict_triangle(b,c,d_2,L_3)`;
   `exists_point_opposite(BC,d_2) [a=a_5]`.
   Goal: `gte(a_5,c)` and `gte(a_5,b)`.

5. I.3, size 3, replay **yes**:
   `extend_point_longer(AB,a,b,c0,b) [x=x_1]`;
   `proposition_1(c1,c0,C) [d=d_2]`;
   `proposition_1(c0,c1,C) [d=d_5]`.
   Goal: `|a x_1| > |c1 d_2|` and `|c0 d_2| = |c0 d_5|`.

6. I.2, size 4, one auxiliary move, replay **yes**:
   `point_between_points_shorter_than(BC,c,b,b,c) [a=a_1]`;
   `extend_point_longer(BC,c,b,b,b) [x=x_2]`;
   `point_between_points_shorter_than(BC,b,a_1,x_2,a_1) [a=a_4]`;
   `proposition_1(c,x_2,BC) [d=d_6]`.
   Goal: `|a_1 a_4| < |c d_6|` and `|a_1 b| < |c d_6|`.

7. I.3, size 4, two auxiliary moves, replay **yes**:
   `extend_point_longer(AB,b,a,b,c1) [x=x_1]`;
   `extend_point(C,c0,c1) [x=x_2]`;
   `proposition_2'(b,a,x_1,AB) [d=d_4]`;
   `extend_point_longer(C,c0,x_2,d_4,x_1) [x=x_5]`.
   Goal: `gte(d_4,x_1)` and `|b d_4| < |b x_1|`.

8. I.2, size 3, one auxiliary move, replay **yes**:
   `extend_point_longer(BC,b,c,f_1,x_2) [x=x_3]`;
   `line_from_points(x_3,c) [L=L_5]`;
   `proposition_1(b,x_3,L_5) [d=d_6]`.
   Goal: `|b c| < |b d_6|` and `|b d_6| = |d_6 x_3|`.

9. I.2, size 3, replay **yes**:
   `extend_point_longer(BC,c,b,a,b) [x=x_1]`;
   `extend_point_longer(BC,x_1,b,c,a) [x=x_5]`;
   `extend_point_longer(BC,x_5,b,x_5,c) [x=x_6]`.
   Goal: `|a b| < |x_1 x_5|` and `|a c| < |x_5 x_6|`.

10. I.3, size 4, replay **yes**:
    `extend_point(C,c1,c0) [x=x_2]`;
    `proposition_1(c1,c0,C) [d=d_3]`;
    `extend_point_longer(C,x_2,c1,c0,c1) [x=x_5]`;
    `proposition_1(x_2,x_5,C) [d=d_6]`.
    Goal: `|c1 d_3| < |d_6 x_5|` and `|c1 x_5| < |d_6 x_2|`.

## Verification

`euclid_hindsight_test.py` and `euclid_candidate_features_test.py` pass:
**15 passed in 1.17s**. Exact ignored records and the machine-readable
analysis are under `spiel/loop-smoke/d3-e4a/`.
