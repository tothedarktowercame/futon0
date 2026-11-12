# EQ-2: hindsight goals from random construction

Each of 18 cells ran for 180 wall seconds, with at most six worker processes. Seeds are `202611120000 + cell*10^7 + run_index`; the gzip records retain each exact seed.

## Cells

| theory | generators | K | runs | runs/s | goals | trivial | distinct non-trivial | needed 1/2/3/4/5/>5 |
|---|---:|---:|---:|---:|---:|---:|---:|---|
| exponent_two | 2 | 6 | 33085 | 183.80 | 143490 | 136495 | 205 | 166/28/8/3/0/0 |
| exponent_two | 2 | 10 | 5765 | 32.02 | 45630 | 41121 | 519 | 383/85/24/19/6/2 |
| exponent_two | 2 | 14 | 1465 | 8.12 | 16181 | 13974 | 547 | 368/79/44/36/17/3 |
| exponent_two | 3 | 6 | 30176 | 167.64 | 98299 | 96097 | 103 | 82/16/4/1/0/0 |
| exponent_two | 3 | 10 | 7585 | 42.14 | 41487 | 39705 | 240 | 178/41/16/5/0/0 |
| exponent_two | 3 | 14 | 2613 | 14.52 | 19606 | 18579 | 242 | 169/53/8/8/4/0 |
| group | 2 | 6 | 48316 | 268.42 | 119550 | 114635 | 165 | 153/9/2/1/0/0 |
| group | 2 | 10 | 10615 | 58.97 | 40845 | 38123 | 359 | 337/18/2/2/0/0 |
| group | 2 | 14 | 3220 | 17.88 | 15508 | 14250 | 341 | 307/28/4/2/0/0 |
| group | 3 | 6 | 37044 | 205.80 | 71738 | 70208 | 75 | 74/1/0/0/0/0 |
| group | 3 | 10 | 9628 | 53.49 | 28012 | 27198 | 96 | 89/7/0/0/0/0 |
| group | 3 | 14 | 3199 | 17.77 | 11969 | 11495 | 105 | 92/13/0/0/0/0 |
| left_group | 2 | 6 | 49279 | 273.77 | 57034 | 56009 | 83 | 68/14/1/0/0/0 |
| left_group | 2 | 10 | 10618 | 58.99 | 17663 | 17016 | 139 | 99/36/4/0/0/0 |
| left_group | 2 | 14 | 3194 | 17.74 | 6514 | 6229 | 116 | 89/25/2/0/0/0 |
| left_group | 3 | 6 | 36834 | 204.63 | 34277 | 34005 | 31 | 24/7/0/0/0/0 |
| left_group | 3 | 10 | 8105 | 45.02 | 10911 | 10730 | 42 | 31/9/2/0/0/0 |
| left_group | 3 | 14 | 2699 | 14.99 | 4553 | 4448 | 38 | 31/7/0/0/0/0 |

## Yield

Distinct goals are deduplicated per theory up to generator renaming and side swapping.

| theory | CPU hours | needed >=2 / CPU-hour | needed >=3 / CPU-hour |
|---|---:|---:|---:|
| exponent_two | 0.300 | 976.3 | 419.8 |
| group | 0.300 | 183.3 | 23.3 |
| left_group | 0.300 | 226.6 | 23.3 |

The Euclid redesign marks were 500 distinct goals/hour needing at least two constructions and 50/hour needing at least three; the table reports the untuned comparison.

## Optimum check

Seeded sample: 200; greedy needed size agreed with exhaustive shortest proof in 168; timeouts: 32; non-agreements (needed, optimum): `[]`. Each call had a 0.5 s wall limit.

## Demo goals and fresh replay

- `exponent_two_commutes`: did not appear.
- `inverse_product`: did not appear.
- `left_group_right_inverse`: did not appear.
- Fresh-start replay proved 20/20 goals sampled with seed 20261112 across every represented needed size (1 through 6) from their recorded needed terms only.

## Sampled goals

- `inv(e) = mul(mul(mul(inv(e),b),b),inv(mul(mul(inv(e),b),b)))` (group, needed 1): `mul(inv(e),e)`
- `a = mul(inv(b),mul(inv(b),a))` (exponent_two, needed 2): `mul(inv(b),inv(b))`, `mul(e,a)`
- `mul(b,a) = mul(mul(mul(b,a),a),mul(mul(mul(mul(b,a),a),b),a))` (exponent_two, needed 3): `mul(e,b)`, `mul(mul(mul(b,a),a),mul(mul(b,a),a))`, `mul(mul(mul(b,a),a),mul(mul(mul(b,a),a),b))`
- `mul(inv(a),e) = mul(mul(a,b),b)` (exponent_two, needed 4): `mul(a,a)`, `mul(b,b)`, `mul(e,a)`, `mul(inv(a),a)`
- `b = mul(mul(a,b),a)` (exponent_two, needed 5): `mul(b,b)`, `mul(e,b)`, `mul(mul(a,b),mul(a,b))`, `mul(mul(a,b),b)`, `mul(a,e)`
- `a = mul(mul(a,inv(inv(b))),b)` (exponent_two, needed 6): `mul(a,e)`, `mul(e,inv(b))`, `mul(inv(b),b)`, `mul(inv(b),inv(b))`, `mul(inv(inv(b)),inv(b))`, `mul(inv(inv(b)),e)`
- `mul(inv(e),a) = mul(mul(inv(e),mul(a,inv(e))),e)` (exponent_two, needed 1): `mul(inv(e),e)`
- `mul(mul(a,b),e) = mul(b,mul(b,mul(a,b)))` (exponent_two, needed 2): `mul(b,b)`, `mul(e,mul(a,b))`
- `mul(inv(e),mul(inv(e),a)) = mul(inv(a),e)` (exponent_two, needed 3): `mul(inv(a),a)`, `mul(mul(inv(e),mul(inv(e),a)),mul(inv(e),mul(inv(e),a)))`, `mul(e,inv(e))`
- `inv(a) = mul(inv(e),mul(inv(e),a))` (exponent_two, needed 4): `mul(inv(a),a)`, `mul(mul(inv(e),mul(inv(e),a)),mul(inv(e),mul(inv(e),a)))`, `mul(e,inv(e))`, `mul(inv(a),e)`
- `inv(a) = mul(b,mul(b,a))` (exponent_two, needed 5): `mul(inv(a),a)`, `mul(inv(a),e)`, `mul(a,a)`, `mul(e,a)`, `mul(b,b)`
- `mul(mul(a,b),a) = mul(a,mul(a,b))` (exponent_two, needed 6): `mul(b,b)`, `mul(e,b)`, `mul(mul(a,b),mul(a,b))`, `mul(mul(a,b),b)`, `mul(a,e)`, `mul(a,a)`
- `mul(inv(e),mul(mul(b,b),inv(e))) = mul(inv(e),mul(b,b))` (group, needed 1): `mul(e,inv(e))`
- `mul(inv(b),mul(c,a)) = mul(mul(mul(inv(b),mul(c,a)),mul(c,a)),mul(c,a))` (exponent_two, needed 2): `mul(inv(b),e)`, `mul(mul(c,a),mul(c,a))`
- `mul(a,b) = mul(e,inv(mul(a,b)))` (exponent_two, needed 3): `mul(mul(a,b),e)`, `mul(mul(a,b),inv(mul(a,b)))`, `mul(inv(mul(a,b)),inv(mul(a,b)))`
- `b = mul(inv(b),inv(e))` (exponent_two, needed 4): `mul(mul(inv(b),inv(e)),b)`, `mul(e,b)`, `mul(mul(inv(b),inv(e)),inv(b))`, `mul(e,inv(e))`
- `mul(b,e) = mul(inv(e),inv(b))` (exponent_two, needed 5): `mul(inv(b),inv(e))`, `mul(mul(inv(b),inv(e)),b)`, `mul(e,b)`, `mul(mul(inv(b),inv(e)),inv(b))`, `mul(e,inv(e))`
- `mul(mul(a,inv(inv(b))),b) = mul(mul(a,inv(inv(b))),inv(inv(b)))` (exponent_two, needed 6): `mul(a,e)`, `mul(e,inv(b))`, `mul(inv(b),b)`, `mul(inv(b),inv(b))`, `mul(inv(inv(b)),inv(b))`, `mul(inv(inv(b)),e)`
- `mul(e,e) = mul(b,mul(mul(b,a),mul(b,mul(b,a))))` (exponent_two, needed 1): `mul(mul(b,mul(b,a)),mul(b,mul(b,a)))`
- `mul(b,a) = mul(mul(mul(b,a),mul(b,a)),inv(mul(b,a)))` (group, needed 2): `mul(mul(b,a),e)`, `mul(mul(b,a),inv(mul(b,a)))`

## Findings

No monotonic-replay assertion failed. The stored proof is removal-minimal in its recorded construction order, not guaranteed minimum; the optimum sample measures that distinction.
