# DESIGN-3 E1c: rule-uniform hindsight sampling

Date: 2026-11-12.  Label: **ZERO**.  No Book proof or target was used.

## Change and run

The generator now groups the complete guarded legal list by rule, chooses a
non-empty rule uniformly, then chooses one of that rule's legal groundings
uniformly.  `--sampling move-uniform` retains E1's distribution;
`rule-uniform` is the new default and is written into every record.

The direct filter now compares a goal semantically with the last needed
construction's pre-closure conclusions.  Segment endpoints are canonical,
equality sides are unordered, and reversing `<` produces `>`.  It now labels
both leaked E1b `proposition_2'` examples (`|c0 x_5|=|d_6 x_5|` and
`|b x_3|=|c1 d_4|`) as direct.

E1c used seed `2026111203`, 12 single-threaded processes, six plies, and the
fixed 30-minute window in its own `futon-testing.slice` scope with
`MemoryMax=24G`, `TasksMax=1024`, and `PYTHONHASHSEED=0`.  The deadline
stopped new goals, but an in-progress metric deletion replay did not return;
at 32m19s I stopped only the recorded job and its scope.  Already-streamed
records are the measurement; there was no relaunch or extension.  All E1c
workers and its scope were gone before analysis.

## Side-by-side yield

The tightened E1c filter was applied anew to both datasets.

| Measure | E1 move-uniform | E1c rule-uniform |
|---|---:|---:|
| six-move runs with records | 97 | 833 |
| raw records | 57,303 | 79,824 |
| raw distinct pairs | 57,265 | 73,171 |
| non-trivial distinct pairs | 528 | 3,131 |
| non-trivial size >=3 | 264 | 1,695 |
| non-trivial pairs/hour | 1,056 | 6,262 |
| non-trivial size >=3/hour | 528 | 3,390 |
| circle in needed moves | 17 | 583 |
| circle intersection in needed moves | 8 | 285 |

Both filtered datasets pass DESIGN-3's thresholds of 500/hour for size >=2
and 50/hour for size >=3.  Rule-uniform sampling improves the rates by 5.9x
and 6.4x respectively, while circle involvement rises from 3.2% to 18.6% of
survivors.

| Needed moves | E1 | E1c |
|---:|---:|---:|
| 2 | 264 | 1,436 |
| 3 | 161 | 1,042 |
| 4 | 75 | 482 |
| 5 | 28 | 141 |
| 6 | 0 | 30 |

| Givens | E1 | E1c |
|---|---:|---:|
| I.1 | 12 | 123 |
| I.2 | 177 | 1,242 |
| I.3 | 339 | 1,766 |

## Surviving predicates

| Predicate | E1 | E1c | Predicate | E1 | E1c |
|---|---:|---:|---|---:|---:|
| `_lt_len` | 287 | 490 | `_gt_len` | 108 | 373 |
| `gte` | 42 | 323 | `pos_len` | 38 | 608 |
| `neq` | 36 | 569 | `_eq_len` | 7 | 257 |
| `intersects_lc` | 4 | 156 | `radii` | 2 | 61 |
| `intersects_cc` | 2 | 28 | `on_circle` | 1 | 21 |
| `not_outside` | 1 | 58 | `_eq_ang` | 0 | 23 |
| `_gt_ang` | 0 | 54 | `neq_line` | 0 | 64 |
| `inside` | 0 | 21 | `not_on_line` | 0 | 12 |
| `same_side` | 0 | 11 | `outside` | 0 | 2 |

Rule-uniform sampling did not merely make more collinear length chains: it
produced angle comparisons, circle incidences, side relations, and
non-incidence goals absent from the filtered E1 set.

## Move-rule frequencies

Counts are over one copy of each six-move run, not repeated once per goal.
E1 has 582 moves; E1c has 4,998.

| Rule | E1 | E1c |
|---|---:|---:|
| `extend_point_longer` | 281 | 422 |
| `point_between_points_shorter_than` | 124 | 450 |
| `use_witness` | 71 | 420 |
| `proposition_2'` | 19 | 126 |
| `angle_self_zero` | 18 | 422 |
| `extend_point` | 16 | 418 |
| `circle_from_points` | 13 | 409 |
| `line_from_points` | 11 | 422 |
| `distinct_points` | 9 | 469 |
| `between_symm_parts` | 7 | 187 |
| `proposition_1` | 7 | 256 |
| `flat_angle_onlyif` | 3 | 203 |
| `intersection_circle_line_extending_points` | 1 | 79 |
| `intersections_circle_line` | 1 | 67 |
| `arbitrary_point` | 1 | 435 |
| `angle_ray_eq_zero` | 0 | 85 |
| `off_line_strict_triangle` | 0 | 46 |
| `between_trans_out` | 0 | 25 |
| `degenerated_angle_if` | 0 | 19 |
| `angle_pair_on_line` | 0 | 9 |
| `superposition` | 0 | 7 |
| `exists_point_opposite` | 0 | 6 |
| `proposition_1'` | 0 | 6 |
| `intersection_circle_line_between_points` | 0 | 5 |
| `extend_point_not_on_line` | 0 | 2 |
| `assert_point_eq_on_ray` | 0 | 1 |
| `opposing_opposing_same_side` | 0 | 1 |
| `intersection_circles` | 0 | 1 |

Move-uniform E1 spent 48.3% of moves on `extend_point_longer`; E1c's most
frequent rule was only 9.4%.  `circle_from_points` rose from 2.2% to 8.2%,
and circle-intersection rules from 0.3% to 3.0%.

## Ten random non-trivial E1c examples

Sample seed: `2026111203`.

1. I.3: `extend_point(AB,b,a)->x_2`;
   `proposition_1(b,x_2,AB)->d_6` => `pos_len(x_2,d_6)`.
2. I.2: `proposition_1(c,b,BC)->d_1`; choose `a_3` between `c,b`;
   extend twice to `x_5,x_6` => `|a_3 x_6| > |b d_1|`.
3. I.2: `proposition_1(c,b,BC)->d_1`;
   `off_line_strict_triangle(d_1,b,c,BC)`;
   `superposition(...)->b'_3,c'_3,AC'_3,BC'_3`;
   `proposition_1'(c'_3,b'_3,b,BC'_3)->c_6` => `gte(b'_3,c_6)`.
4. I.2: `proposition_1(c,b,BC)->d_1`;
   `use_witness(a,c,d_1,c,d_1,c,BC)->f_3` => `pos_len(d_1,f_3)`.
5. I.2: the first three moves of example 3, then
   `proposition_1'(c'_3,b'_3,b,BC'_3)->c_6` =>
   `|b'_3 c_6| = |c'_3 c_6|`.
6. I.2: `proposition_1(b,c,BC)->d_2`;
   `proposition_1(b,c,BC)->d_4` => `|b d_4| = |c d_2|`.
7. I.3: `proposition_1(b,a,AB)->d_1`;
   `use_witness(b,b,a,b,a,b,AB)->f_3`;
   `off_line_strict_triangle(d_1,b,a,AB)` => `pos_len(f_3,d_1)`.
8. I.2: extend `BC` to `x_1`; `line_from_points(b,c)->L_2`;
   `circle_from_points(x_1,b)->C_5` => `intersects_lc(L_2,C_5)`.
9. I.2: extend `BC` to `x_2`; `proposition_1(c,b,BC)->d_3`;
   establish its strict triangle; `exists_point_opposite(BC,d_3)->a_6` =>
   `neq(x_2,a_6)`.
10. I.3: construct equilateral apices `d_1,d_3` on `c0 c1`; copy a
    length as `d_4`; `circle_from_points(c1,d_1)->C_5` =>
    `radii(C_5,d_3,d_4)`.

## Tests and receipts

`pytest -q euclid_hindsight_test.py euclid_hindsight_filter_test.py`:
**8 passed in 0.68 s**.  Tests include rule-level balancing, the retained
move-uniform mode, both leaked copied-length examples, all E1b labels, and
the known non-trivial I.1 equal-sides theorem.

Ignored exact receipts are under `spiel/loop-smoke/d3-e1c/`; its
`analysis.json` contains the deterministic example records and full tables.
