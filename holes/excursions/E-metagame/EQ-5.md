# EQ-5 — non-learning solver baselines

The frozen test set contains 796 goals. Stochastic policies received 20 seeded attempts per goal and budget; exhaustive search had a two-second wall limit per goal.

## Stochastic policies

| theory | needed | policy | budget | goals | mean solve rate | median solved moves |
|---|---:|---|---:|---:|---:|---:|
| exponent_two | 1 | completing | 1n | 60 | 13.1% | 1 |
| exponent_two | 1 | completing | 2n | 60 | 24.9% | 1 |
| exponent_two | 1 | completing | 4n | 60 | 43.4% | 2 |
| exponent_two | 1 | merging | 1n | 60 | 14.6% | 1 |
| exponent_two | 1 | merging | 2n | 60 | 26.1% | 2 |
| exponent_two | 1 | merging | 4n | 60 | 41.8% | 2 |
| exponent_two | 1 | uniform | 1n | 60 | 6.8% | 1 |
| exponent_two | 1 | uniform | 2n | 60 | 11.2% | 1.0 |
| exponent_two | 1 | uniform | 4n | 60 | 18.4% | 2 |
| exponent_two | 2 | completing | 1n | 60 | 1.8% | 2 |
| exponent_two | 2 | completing | 2n | 60 | 6.3% | 3.0 |
| exponent_two | 2 | completing | 4n | 60 | 24.8% | 6 |
| exponent_two | 2 | merging | 1n | 60 | 2.2% | 2.0 |
| exponent_two | 2 | merging | 2n | 60 | 8.4% | 4 |
| exponent_two | 2 | merging | 4n | 60 | 26.5% | 6.0 |
| exponent_two | 2 | uniform | 1n | 60 | 0.6% | 2 |
| exponent_two | 2 | uniform | 2n | 60 | 1.5% | 3.0 |
| exponent_two | 2 | uniform | 4n | 60 | 3.7% | 4.0 |
| exponent_two | 3 | completing | 1n | 60 | 1.1% | 3 |
| exponent_two | 3 | completing | 2n | 60 | 19.8% | 5 |
| exponent_two | 3 | completing | 4n | 60 | 54.8% | 8.0 |
| exponent_two | 3 | merging | 1n | 60 | 1.5% | 3.0 |
| exponent_two | 3 | merging | 2n | 60 | 18.4% | 5 |
| exponent_two | 3 | merging | 4n | 60 | 66.7% | 8.0 |
| exponent_two | 3 | uniform | 1n | 60 | 0.6% | 3 |
| exponent_two | 3 | uniform | 2n | 60 | 4.2% | 5 |
| exponent_two | 3 | uniform | 4n | 60 | 10.2% | 7 |
| exponent_two | 4 | completing | 1n | 60 | 1.6% | 4 |
| exponent_two | 4 | completing | 2n | 60 | 21.1% | 7 |
| exponent_two | 4 | completing | 4n | 60 | 58.2% | 10 |
| exponent_two | 4 | merging | 1n | 60 | 1.2% | 4.0 |
| exponent_two | 4 | merging | 2n | 60 | 22.0% | 7.0 |
| exponent_two | 4 | merging | 4n | 60 | 83.8% | 11.0 |
| exponent_two | 4 | uniform | 1n | 60 | 0.2% | 3.0 |
| exponent_two | 4 | uniform | 2n | 60 | 3.2% | 6 |
| exponent_two | 4 | uniform | 4n | 60 | 6.9% | 10 |
| exponent_two | 5 | completing | 1n | 39 | 2.3% | 5.0 |
| exponent_two | 5 | completing | 2n | 39 | 24.1% | 8.0 |
| exponent_two | 5 | completing | 4n | 39 | 60.3% | 12.0 |
| exponent_two | 5 | merging | 1n | 39 | 2.2% | 5 |
| exponent_two | 5 | merging | 2n | 39 | 29.4% | 8 |
| exponent_two | 5 | merging | 4n | 39 | 96.9% | 13.0 |
| exponent_two | 5 | uniform | 1n | 39 | 0.0% | — |
| exponent_two | 5 | uniform | 2n | 39 | 2.9% | 8 |
| exponent_two | 5 | uniform | 4n | 39 | 4.6% | 9.0 |
| exponent_two | 6 | completing | 1n | 19 | 4.5% | 6 |
| exponent_two | 6 | completing | 2n | 19 | 25.0% | 9 |
| exponent_two | 6 | completing | 4n | 19 | 66.1% | 14 |
| exponent_two | 6 | merging | 1n | 19 | 6.1% | 6 |
| exponent_two | 6 | merging | 2n | 19 | 33.4% | 9 |
| exponent_two | 6 | merging | 4n | 19 | 99.5% | 14.0 |
| exponent_two | 6 | uniform | 1n | 19 | 1.3% | 6 |
| exponent_two | 6 | uniform | 2n | 19 | 2.4% | 9 |
| exponent_two | 6 | uniform | 4n | 19 | 5.3% | 8.5 |
| exponent_two | >6 | completing | 1n | 5 | 0.0% | — |
| exponent_two | >6 | completing | 2n | 5 | 18.0% | 13.0 |
| exponent_two | >6 | completing | 4n | 5 | 77.0% | 18 |
| exponent_two | >6 | merging | 1n | 5 | 0.0% | — |
| exponent_two | >6 | merging | 2n | 5 | 51.0% | 13 |
| exponent_two | >6 | merging | 4n | 5 | 100.0% | 15.0 |
| exponent_two | >6 | uniform | 1n | 5 | 0.0% | — |
| exponent_two | >6 | uniform | 2n | 5 | 0.0% | — |
| exponent_two | >6 | uniform | 4n | 5 | 0.0% | — |
| group | 1 | completing | 1n | 60 | 17.8% | 1.0 |
| group | 1 | completing | 2n | 60 | 33.1% | 1 |
| group | 1 | completing | 4n | 60 | 57.4% | 2 |
| group | 1 | merging | 1n | 60 | 16.8% | 1 |
| group | 1 | merging | 2n | 60 | 36.7% | 1.0 |
| group | 1 | merging | 4n | 60 | 59.2% | 2 |
| group | 1 | uniform | 1n | 60 | 6.3% | 1.0 |
| group | 1 | uniform | 2n | 60 | 12.5% | 1.0 |
| group | 1 | uniform | 4n | 60 | 19.8% | 2 |
| group | 2 | completing | 1n | 60 | 1.8% | 2 |
| group | 2 | completing | 2n | 60 | 7.3% | 3.0 |
| group | 2 | completing | 4n | 60 | 26.2% | 5 |
| group | 2 | merging | 1n | 60 | 2.4% | 2 |
| group | 2 | merging | 2n | 60 | 9.0% | 3.0 |
| group | 2 | merging | 4n | 60 | 35.2% | 6.0 |
| group | 2 | uniform | 1n | 60 | 0.4% | 2 |
| group | 2 | uniform | 2n | 60 | 0.6% | 3 |
| group | 2 | uniform | 4n | 60 | 1.8% | 4.0 |
| group | 3 | completing | 1n | 25 | 1.4% | 3 |
| group | 3 | completing | 2n | 25 | 9.6% | 5.0 |
| group | 3 | completing | 4n | 25 | 41.0% | 8 |
| group | 3 | merging | 1n | 25 | 1.6% | 3.0 |
| group | 3 | merging | 2n | 25 | 12.8% | 6.0 |
| group | 3 | merging | 4n | 25 | 70.0% | 8.0 |
| group | 3 | uniform | 1n | 25 | 0.2% | 3 |
| group | 3 | uniform | 2n | 25 | 1.4% | 4 |
| group | 3 | uniform | 4n | 25 | 2.0% | 5.0 |
| group | 4 | completing | 1n | 7 | 1.4% | 2.5 |
| group | 4 | completing | 2n | 7 | 12.1% | 7 |
| group | 4 | completing | 4n | 7 | 50.7% | 11 |
| group | 4 | merging | 1n | 7 | 0.7% | 3 |
| group | 4 | merging | 2n | 7 | 22.9% | 7.0 |
| group | 4 | merging | 4n | 7 | 97.9% | 11 |
| group | 4 | uniform | 1n | 7 | 0.0% | — |
| group | 4 | uniform | 2n | 7 | 0.0% | — |
| group | 4 | uniform | 4n | 7 | 2.1% | 6 |
| left_group | 1 | completing | 1n | 60 | 14.2% | 1 |
| left_group | 1 | completing | 2n | 60 | 29.7% | 1.0 |
| left_group | 1 | completing | 4n | 60 | 57.7% | 2.0 |
| left_group | 1 | merging | 1n | 60 | 15.9% | 1 |
| left_group | 1 | merging | 2n | 60 | 26.9% | 1 |
| left_group | 1 | merging | 4n | 60 | 59.3% | 2.0 |
| left_group | 1 | uniform | 1n | 60 | 3.8% | 1 |
| left_group | 1 | uniform | 2n | 60 | 3.6% | 2 |
| left_group | 1 | uniform | 4n | 60 | 6.3% | 2.0 |
| left_group | 2 | completing | 1n | 60 | 4.2% | 2 |
| left_group | 2 | completing | 2n | 60 | 25.2% | 4.0 |
| left_group | 2 | completing | 4n | 60 | 73.8% | 6 |
| left_group | 2 | merging | 1n | 60 | 5.1% | 2 |
| left_group | 2 | merging | 2n | 60 | 26.2% | 3 |
| left_group | 2 | merging | 4n | 60 | 84.2% | 6.0 |
| left_group | 2 | uniform | 1n | 60 | 0.2% | 2.0 |
| left_group | 2 | uniform | 2n | 60 | 1.0% | 3.0 |
| left_group | 2 | uniform | 4n | 60 | 1.8% | 4 |
| left_group | 3 | completing | 1n | 60 | 3.6% | 3 |
| left_group | 3 | completing | 2n | 60 | 33.8% | 5 |
| left_group | 3 | completing | 4n | 60 | 83.5% | 7.0 |
| left_group | 3 | merging | 1n | 60 | 2.3% | 3.0 |
| left_group | 3 | merging | 2n | 60 | 35.9% | 5 |
| left_group | 3 | merging | 4n | 60 | 96.8% | 7.0 |
| left_group | 3 | uniform | 1n | 60 | 0.3% | 3.0 |
| left_group | 3 | uniform | 2n | 60 | 1.4% | 5 |
| left_group | 3 | uniform | 4n | 60 | 1.8% | 6.0 |
| left_group | 4 | completing | 1n | 60 | 4.4% | 4 |
| left_group | 4 | completing | 2n | 60 | 52.1% | 6 |
| left_group | 4 | completing | 4n | 60 | 82.8% | 8 |
| left_group | 4 | merging | 1n | 60 | 5.7% | 4.0 |
| left_group | 4 | merging | 2n | 60 | 56.0% | 6.0 |
| left_group | 4 | merging | 4n | 60 | 95.7% | 8.0 |
| left_group | 4 | uniform | 1n | 60 | 0.5% | 3.0 |
| left_group | 4 | uniform | 2n | 60 | 1.0% | 6.0 |
| left_group | 4 | uniform | 4n | 60 | 2.2% | 7 |
| left_group | 5 | completing | 1n | 19 | 4.7% | 5.0 |
| left_group | 5 | completing | 2n | 19 | 55.3% | 8.0 |
| left_group | 5 | completing | 4n | 19 | 64.7% | 8.0 |
| left_group | 5 | merging | 1n | 19 | 2.9% | 5 |
| left_group | 5 | merging | 2n | 19 | 62.9% | 8 |
| left_group | 5 | merging | 4n | 19 | 74.5% | 8 |
| left_group | 5 | uniform | 1n | 19 | 0.0% | — |
| left_group | 5 | uniform | 2n | 19 | 2.4% | 9 |
| left_group | 5 | uniform | 4n | 19 | 2.4% | 8 |
| left_group | 6 | completing | 1n | 10 | 0.0% | — |
| left_group | 6 | completing | 2n | 10 | 10.5% | 9 |
| left_group | 6 | completing | 4n | 10 | 9.0% | 9.0 |
| left_group | 6 | merging | 1n | 10 | 0.0% | — |
| left_group | 6 | merging | 2n | 10 | 8.0% | 10.0 |
| left_group | 6 | merging | 4n | 10 | 26.0% | 15.0 |
| left_group | 6 | uniform | 1n | 10 | 0.0% | — |
| left_group | 6 | uniform | 2n | 10 | 0.0% | — |
| left_group | 6 | uniform | 4n | 10 | 0.0% | — |
| left_group | >6 | completing | 1n | 5 | 0.0% | — |
| left_group | >6 | completing | 2n | 5 | 0.0% | — |
| left_group | >6 | completing | 4n | 5 | 0.0% | — |
| left_group | >6 | merging | 1n | 5 | 0.0% | — |
| left_group | >6 | merging | 2n | 5 | 2.0% | 13.5 |
| left_group | >6 | merging | 4n | 5 | 8.0% | 17.5 |
| left_group | >6 | uniform | 1n | 5 | 0.0% | — |
| left_group | >6 | uniform | 2n | 5 | 0.0% | — |
| left_group | >6 | uniform | 4n | 5 | 0.0% | — |
| monoid | 1 | completing | 1n | 58 | 7.4% | 1.0 |
| monoid | 1 | completing | 2n | 58 | 13.6% | 2.0 |
| monoid | 1 | completing | 4n | 58 | 26.9% | 2.0 |
| monoid | 1 | merging | 1n | 58 | 7.4% | 1.0 |
| monoid | 1 | merging | 2n | 58 | 12.7% | 2 |
| monoid | 1 | merging | 4n | 58 | 29.3% | 3.0 |
| monoid | 1 | uniform | 1n | 58 | 2.0% | 1 |
| monoid | 1 | uniform | 2n | 58 | 5.9% | 2.0 |
| monoid | 1 | uniform | 4n | 58 | 7.5% | 2 |
| monoid | 2 | completing | 1n | 6 | 0.0% | — |
| monoid | 2 | completing | 2n | 6 | 7.5% | 2 |
| monoid | 2 | completing | 4n | 6 | 18.3% | 5.0 |
| monoid | 2 | merging | 1n | 6 | 0.0% | — |
| monoid | 2 | merging | 2n | 6 | 3.3% | 3.0 |
| monoid | 2 | merging | 4n | 6 | 22.5% | 6 |
| monoid | 2 | uniform | 1n | 6 | 0.0% | — |
| monoid | 2 | uniform | 2n | 6 | 0.0% | — |
| monoid | 2 | uniform | 4n | 6 | 2.5% | 3 |

## Goal variability at budget 2n

| theory | policy | every attempt | no attempt | some attempts |
|---|---|---:|---:|---:|
| exponent_two | completing | 1.7% | 20.1% | 78.2% |
| exponent_two | merging | 1.7% | 16.5% | 81.8% |
| exponent_two | uniform | 0.0% | 67.0% | 33.0% |
| group | completing | 0.0% | 17.8% | 82.2% |
| group | merging | 0.0% | 13.8% | 86.2% |
| group | uniform | 0.0% | 59.9% | 40.1% |
| left_group | completing | 6.2% | 7.3% | 86.5% |
| left_group | merging | 6.9% | 5.1% | 88.0% |
| left_group | uniform | 0.0% | 78.8% | 21.2% |
| monoid | completing | 0.0% | 4.7% | 95.3% |
| monoid | merging | 0.0% | 10.9% | 89.1% |
| monoid | uniform | 0.0% | 39.1% | 60.9% |

## Exhaustive search

| needed | goals | solved | timed out | shorter than recorded |
|---:|---:|---:|---:|---:|
| 1 | 238 | 100.0% | 0 | 0 |
| 2 | 186 | 82.8% | 32 | 5 |
| 3 | 145 | 22.8% | 112 | 7 |
| 4 | 127 | 5.5% | 120 | 6 |
| 5 | 58 | 3.4% | 56 | 2 |
| 6 | 29 | 0.0% | 29 | 0 |
| >6 | 10 | 0.0% | 10 | 0 |

Shorter cases (up to 20):
- `mul(b,e) = mul(inv(e),mul(inv(e),b))`: recorded 2, exhaustive 1.
- `a = mul(mul(a,inv(inv(e))),inv(inv(e)))`: recorded 2, exhaustive 1.
- `mul(e,mul(a,b)) = mul(mul(mul(a,b),mul(mul(a,b),inv(e))),mul(mul(a,b),inv(e)))`: recorded 2, exhaustive 1.
- `a = mul(mul(a,inv(e)),inv(inv(e)))`: recorded 2, exhaustive 1.
- `mul(b,b) = mul(inv(e),mul(inv(inv(e)),mul(b,b)))`: recorded 2, exhaustive 1.
- `e = mul(mul(mul(a,b),mul(mul(a,b),b)),b)`: recorded 3, exhaustive 2.
- `mul(b,e) = mul(mul(b,a),mul(b,mul(a,b)))`: recorded 3, exhaustive 2.
- `mul(inv(inv(a)),e) = mul(mul(inv(inv(a)),a),inv(a))`: recorded 3, exhaustive 1.
- `mul(mul(a,b),b) = mul(inv(e),mul(mul(a,b),b))`: recorded 3, exhaustive 2.
- `mul(inv(e),mul(b,inv(e))) = mul(mul(inv(e),inv(e)),mul(b,inv(e)))`: recorded 3, exhaustive 2.
- `mul(e,mul(a,a)) = mul(inv(a),mul(mul(a,a),a))`: recorded 3, exhaustive 2.
- `mul(a,mul(b,inv(e))) = mul(inv(e),mul(a,mul(b,inv(e))))`: recorded 3, exhaustive 2.
- `mul(b,e) = mul(mul(b,inv(mul(b,a))),inv(mul(b,a)))`: recorded 4, exhaustive 1.
- `mul(e,b) = mul(mul(inv(a),mul(a,b)),e)`: recorded 4, exhaustive 1.
- `inv(inv(a)) = mul(mul(inv(inv(a)),a),inv(a))`: recorded 4, exhaustive 2.
- `mul(inv(inv(e)),inv(e)) = mul(inv(e),inv(inv(e)))`: recorded 4, exhaustive 3.
- `mul(mul(b,b),inv(e)) = mul(inv(e),mul(mul(b,b),inv(e)))`: recorded 4, exhaustive 2.
- `mul(mul(inv(b),e),mul(b,mul(inv(b),e))) = mul(inv(b),mul(b,mul(inv(b),e)))`: recorded 4, exhaustive 1.
- `b = mul(mul(b,inv(mul(b,a))),inv(mul(b,a)))`: recorded 5, exhaustive 2.
- `b = mul(mul(inv(a),mul(a,b)),e)`: recorded 5, exhaustive 2.

## Named goals

| goal | policy | n | 2n | 4n |
|---|---|---:|---:|---:|
| exponent_two_commutes | uniform | 0.0% | 0.0% | 0.0% |
| exponent_two_commutes | completing | 0.0% | 20.0% | 85.0% |
| exponent_two_commutes | merging | 0.0% | 95.0% | 100.0% |
| exponent_two_commutes | exhaustive | timeout | — | — |
| inverse_product | uniform | 0.0% | 0.0% | 0.0% |
| inverse_product | completing | 0.0% | 5.0% | 5.0% |
| inverse_product | merging | 0.0% | 0.0% | 100.0% |
| inverse_product | exhaustive | timeout | — | — |
| left_group_right_inverse | uniform | 0.0% | 0.0% | 0.0% |
| left_group_right_inverse | completing | 0.0% | 25.0% | 30.0% |
| left_group_right_inverse | merging | 0.0% | 20.0% | 55.0% |
| left_group_right_inverse | exhaustive | timeout | — | — |

## Fifty-percent frontier at budget 2n

| theory | policy | largest needed size with mean solve rate >= 50% |
|---|---|---:|
| exponent_two | completing | none |
| exponent_two | merging | >6 |
| exponent_two | uniform | none |
| group | completing | none |
| group | merging | none |
| group | uniform | none |
| left_group | completing | 5 |
| left_group | merging | 5 |
| left_group | uniform | none |
| monoid | completing | none |
| monoid | merging | none |
| monoid | uniform | none |

## Findings

At budget 2n, no group policy reached a 50% mean solve rate at any needed size. The preference policies nevertheless improved sharply over uniform: for group size 2, completing/merging/uniform solved 7.3%/9.0%/0.6%; at size 3 they solved 9.6%/12.8%/1.4%. Left-group completing and merging crossed 50% through recorded needed size 5; exponent-two merging crossed it only in the pooled greater-than-six bin.

The “some but not all” fraction—the proposed setter reward—was large for preferred policies at 2n: 82–86% in group, 87–88% in left_group, and 78–82% in exponent_two. Uniform was both weaker and less often variable.

Exhaustive search solved every size-1 goal, 82.8% of size-2 goals, and 22.8% of size-3 goals. Its lower rates at larger sizes are timeout-limited: 120 of 127 size-4 goals and every size-6-or-larger goal hit the two-second limit. It found twenty recorded removal-minimal proofs that were not minimum; the table lists the first twenty shorter cases. Thus “not solved” is not a proof of unreachability.

Uniform play never solved a named goal in 20 attempts at any budget. At 4n, merging solved `exponent_two_commutes` and `inverse_product` in every attempt and `left_group_right_inverse` in 55%; completing reached 85%, 5%, and 30% respectively. All three exhaustive searches timed out at n.

The full 20-attempt grid completed in one scoped run in about eighteen minutes, so no attempts, policies, sizes, or goals were cut. No measurement process was relaunched.
