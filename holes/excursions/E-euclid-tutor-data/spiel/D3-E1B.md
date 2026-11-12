# DESIGN-3 E1b: non-trivial hindsight yield

Date: 2026-11-12.  This is post-processing of the existing E1 ZERO dataset;
no games, sampling, or training were run.

## Filter

Each record is replayed from fresh givens using only its needed moves.  Labels
are non-exclusive:

- **degenerate**: all goal points lie on one engine-known line, or any angle
  term has three engine-known collinear points;
- **direct**: after normalizing metric spellings (`_eq_len` to `eq_len`, and
  similarly), the goal occurs in the last needed move's `report["added"]`.
  That list is the construction's own conclusions before `close`, so closure
  consequences are not called direct;
- **naming**: a `rayang`/`sameray` marker, a symmetric segment identity, or an
  angle equality whose two atoms are connected solely by the engine's
  `rayang`/`sameray` equivalence graph;
- **zero-length**: a length equality or inequality contains `|pp|`.  This
  fourth rule came from reading 30 random records and is exactly the
  zero-segment identity; no broad rule was added for positivity, distinctness,
  or witnessed intersections because those can be genuine consequences.

Replay states are cached by `(proposition, run seed, needed indices)`.  The
57,303 raw records were processed in 31.48 s at 184 MiB RSS and deduplicated
to the same 57,265 `(givens, canonical key)` pairs used by E1.

## Counts

| Reason | Distinct pairs |
|---|---:|
| degenerate | 50,922 |
| naming | 23,024 |
| direct | 1,051 |
| zero-length | 208 |

Because labels overlap, the reason counts do not sum to the number removed.
After removing every labelled pair, **599 non-trivial pairs remain**.

| Needed moves | Non-trivial pairs |
|---:|---:|
| 2 | 310 |
| 3 | 178 |
| 4 | 83 |
| 5 | 28 |
| **>=2** | **599** |
| **>=3** | **289** |

On E1's 30-minute window this is **1,198 non-trivial pairs/hour** for size
>=2 and **578/hour** for size >=3.  Thus the filtered yield still passes the
original E1 thresholds (500/hour and 50/hour respectively).

| Givens | Non-trivial pairs |
|---|---:|
| I.1 | 13 |
| I.2 | 201 |
| I.3 | 385 |

| Goal predicate | Count | Goal predicate | Count |
|---|---:|---|---:|
| `_lt_len` | 333 | `_gt_len` | 118 |
| `gte` | 42 | `pos_len` | 38 |
| `neq` | 36 | `_eq_len` | 21 |
| `intersects_lc` | 4 | `radii` | 2 |
| `intersects_cc` | 2 | `on_circle` | 1 |
| `not_outside` | 1 | `_eq_ang` | 1 |

The review concern is confirmed: most E1 output was collinear or ray-name
arithmetic.  Only one angle equality survives, while useful segment
comparisons remain plentiful.

## Ten random non-trivial examples

The sample seed is `20261112`; arrows name constructed objects.

1. I.3: `extend_point_longer(AB,a,b,d_3,c1)->x_4`;
   `extend_point_longer(C,c1,c0,x_4,a)->x_5` => `|b x_4| < |c1 x_5|`.
2. I.2: `use_witness(a,c,b,c,b,c,BC)->f_2`;
   `use_witness(b,f_2,b,f_2,b,f_2,BC)->f_3`;
   `use_witness(a,f_3,b,f_3,b,f_3,BC)->f_4`;
   `extend_point_longer(BC,b,f_4,a,f_2)->x_5`;
   `extend_point_longer(BC,b,x_5,c,f_4)->x_6` =>
   `|a f_2| < |f_4 x_6|`.
3. I.3: `point_between_points_shorter_than(AB,b,a,c0,c1)->a_1`;
   `extend_point_longer(AB,a,b,d_3,c1)->x_4` =>
   `|a_1 x_4| > |c1 d_3|`.
4. I.3: `extend_point_longer(C,c1,c0,c0,d_4)->x_5`;
   `proposition_2'(x_5,c0,x_5,C)->d_6` => `|c0 x_5| = |d_6 x_5|`.
5. I.3: `point_between_points_shorter_than(AB,b,a,a,b)->a_1`;
   `extend_point_longer(AB,a_1,a,a,c1)->x_2`;
   `extend_point_longer(AB,b,a,a,x_2)->x_3`;
   `extend_point_longer(AB,x_3,b,b,b)->x_4`;
   `extend_point_longer(AB,x_3,x_4,x_3,a_1)->x_6` =>
   `|a c1| < |a x_6|`.
6. I.3: `extend_point_longer(AB,a,b,c0,a)->x_2`;
   `proposition_2'(a_5,a,x_2,AB)->d_6` => `pos_len(d_6,a_5)`.
7. I.3: `use_witness(b,a,b,a,b,a,AB)->f_1`;
   `extend_point_longer(AB,f_1,b,c0,b)->x_2`;
   `extend_point_longer(AB,x_2,f_1,x_2,f_1)->x_3`;
   `extend_point_longer(AB,b,x_3,x_2,x_2)->x_6` =>
   `|b c0| < |f_1 x_6|`.
8. I.3: `extend_point_longer(AB,a,b,x_1,x_2)->x_3`;
   `proposition_2'(c1,x_3,b,AB)->d_4` => `|b x_3| = |c1 d_4|`.
9. I.3: `extend_point_longer(C,c1,c0,c1,c1)->x_1`;
   `extend_point_longer(C,c1,x_1,b,c0)->x_2`;
   `point_between_points_shorter_than(C,c0,x_1,x_1,c0)->a_4`;
   `proposition_2'(a,x_2,c1,C)->d_6` => `|a d_6| > |a_4 c0|`.
10. I.3: `extend_point_longer(C,c0,c1,a,a)->x_1`;
    `point_between_points_shorter_than(C,c1,c0,x_1,c0)->a_2` =>
    `|ab| > |a_2 c1|`.

## Five examples per trivial reason

### Degenerate

1. I.2: `line_from_points(c,b)->L_1`; extend `L_1` three times; choose
   `a_6` between `c,b` => `∠a_6:x_2:x_5 = ∠c:x_2:x_5` (all on `L_1`).
2. I.1: circle `C_1`, intersect it with `AB` as `a_2,b_2`, choose `a_3`
   on `AB`, extend to `x_5` => `∠a_2:b_2:a_3 = ∠a_2:b_2:x_5`.
3. I.2: create `f_1,x_2,x_5,x_6` successively on `BC` =>
   `∠b:x_5:x_2 = ∠b:x_6:x_2`.
4. I.2: choose `a_1,a_2,x_3,a_5,a_6` successively on `BC` =>
   `∠a_1:b:a_2 = ∠a_5:a_2:a_6`.
5. I.1: extend `AB` to `x_1` and `x_3` =>
   `∠a:b:x_1 = ∠a:b:x_3`.

### Direct

1. I.1: extend `AB` to `x_1`; `use_witness(...,AB)->f_3` =>
   `on_line(f_3,AB)`.
2. I.2: extend `BC` to `x_4`, then beyond it to `x_6` =>
   `between(x_4,c,x_6)`.
3. I.1: extend `AB` to `x_1`; choose `a_3` between `b,a` =>
   `between(b,a_3,a)`.
4. I.2: draw `L_1` through `c,b`; extend it to `x_2` =>
   `|c x_2| > |bb|`.
5. I.3: choose `a_2`; copy a length as `d_3`; choose `a_5` between
   `b,a_2` => `on_line(a_5,AB)`.

### Naming

1. I.1: extend `AB` to `x_1,x_2`, choose `a_3`, extend to `x_4` =>
   `∠a_3:b:x_1 = ∠a_3:b:x_4` by equal rays.
2. I.1: extend to `x_1`, choose `a_2`, extend to `x_3` =>
   `∠a:b:x_3 = ∠a_2:b:x_3` by equal rays.
3. I.2: extend `BC` to `x_2`, choose `a_4` on it =>
   `∠a_4:x_2:b = ∠a_4:x_2:c`.
4. I.1: extend to `x_1,x_4,x_5` =>
   `∠b:x_1:x_4 = ∠x_4:x_1:x_5` by ray markers.
5. I.1: extend to `x_1,x_2`, choose `a_3,a_5` =>
   `∠a_3:b:x_2 = ∠a_5:b:x_1` by ray markers.

### Zero-length

1. I.3: extend `C` to `x_2`; choose `a_4` on `C` =>
   `|a_4 x_2| > |bb|`.
2. I.1: extend to `x_1,x_3,x_5` => `|x_3 x_5| > |x_3 x_3|`.
3. I.1: extend to `x_1,x_6` => `|b x_6| > |aa|`.
4. I.1: extend to `x_1`, choose `a_3`, extend to `x_5` =>
   `|x_1 x_5| > |a_3 a_3|`.
5. I.2: extend `BC` to `x_1,x_6` => `|bb| < |c x_6|`.

## Tests and receipts

`pytest -q euclid_hindsight_filter_test.py euclid_hindsight_test.py`:
**6 passed in 8.58 s**.  The planted tests exercise collinear-angle,
direct-output, ray-identity, and zero-length labels.  The known I.1 theorem
obtained from two circles and their intersection (`|ac| = |bc|`) receives no
label.  Exact classified records and deterministic sample receipts are in
`spiel/loop-smoke/d3-e1/filtered.jsonl` and `filter-analysis.json` (ignored).
