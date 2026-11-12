# EQ-4 — rule-completing setter policies

Each of 32 cells ran for 120 wall seconds with at most six workers. Every non-uniform choice uses only the current rule statements, their present instances, and the supplied probability.

## Cells

| theory | K | policy | runs | runs/s | trivial share | distinct non-trivial | needed 1/2/3/4/5/>5 |
|---|---:|---|---:|---:|---:|---:|---|
| exponent_two | 10 | completing(0.5) | 2479 | 20.62 | 88.1% | 240 | 142/56/16/18/6/2 |
| exponent_two | 10 | completing(0.8) | 1751 | 14.59 | 87.0% | 142 | 79/33/13/12/5/0 |
| exponent_two | 10 | merging(0.8) | 1804 | 15.03 | 87.4% | 105 | 53/23/12/13/4/0 |
| exponent_two | 10 | uniform | 5163 | 43.02 | 91.0% | 451 | 311/84/28/22/6/0 |
| exponent_two | 14 | completing(0.5) | 478 | 3.97 | 80.9% | 365 | 155/75/33/49/33/20 |
| exponent_two | 14 | completing(0.8) | 313 | 2.61 | 77.2% | 224 | 53/57/34/38/21/21 |
| exponent_two | 14 | merging(0.8) | 208 | 1.73 | 74.2% | 116 | 27/29/19/24/10/7 |
| exponent_two | 14 | uniform | 1240 | 10.33 | 86.6% | 464 | 295/67/39/50/11/2 |
| group | 10 | completing(0.5) | 5603 | 46.68 | 94.9% | 228 | 186/37/4/1/0/0 |
| group | 10 | completing(0.8) | 6152 | 51.26 | 97.4% | 122 | 89/26/5/2/0/0 |
| group | 10 | merging(0.8) | 5068 | 42.23 | 95.5% | 126 | 94/25/5/2/0/0 |
| group | 10 | uniform | 7927 | 66.05 | 94.2% | 247 | 215/29/2/1/0/0 |
| group | 14 | completing(0.5) | 1889 | 15.74 | 92.9% | 270 | 194/56/14/6/0/0 |
| group | 14 | completing(0.8) | 2332 | 19.43 | 95.6% | 203 | 141/46/11/5/0/0 |
| group | 14 | merging(0.8) | 1354 | 11.27 | 91.2% | 176 | 117/44/9/6/0/0 |
| group | 14 | uniform | 2203 | 18.35 | 92.2% | 211 | 184/22/4/1/0/0 |
| left_group | 10 | completing(0.5) | 5232 | 43.60 | 85.2% | 317 | 187/84/30/11/3/2 |
| left_group | 10 | completing(0.8) | 3485 | 29.04 | 77.9% | 249 | 129/73/29/11/3/4 |
| left_group | 10 | merging(0.8) | 3293 | 27.42 | 76.0% | 253 | 124/79/25/17/4/4 |
| left_group | 10 | uniform | 6643 | 55.35 | 96.7% | 102 | 74/28/0/0/0/0 |
| left_group | 14 | completing(0.5) | 1337 | 11.13 | 77.0% | 528 | 235/155/79/42/8/9 |
| left_group | 14 | completing(0.8) | 837 | 6.97 | 66.4% | 517 | 225/125/84/52/14/17 |
| left_group | 14 | merging(0.8) | 723 | 6.02 | 62.8% | 553 | 221/142/100/61/17/12 |
| left_group | 14 | uniform | 2073 | 17.26 | 94.6% | 78 | 57/19/2/0/0/0 |
| monoid | 10 | completing(0.5) | 8230 | 68.58 | 100.0% | 13 | 12/1/0/0/0/0 |
| monoid | 10 | completing(0.8) | 9210 | 76.75 | 100.0% | 8 | 8/0/0/0/0/0 |
| monoid | 10 | merging(0.8) | 9491 | 79.09 | 100.0% | 9 | 9/0/0/0/0/0 |
| monoid | 10 | uniform | 8484 | 70.70 | 99.9% | 13 | 12/1/0/0/0/0 |
| monoid | 14 | completing(0.5) | 3113 | 25.94 | 99.8% | 30 | 27/3/0/0/0/0 |
| monoid | 14 | completing(0.8) | 4301 | 35.84 | 99.8% | 27 | 25/2/0/0/0/0 |
| monoid | 14 | merging(0.8) | 5449 | 45.40 | 99.9% | 24 | 24/0/0/0/0/0 |
| monoid | 14 | uniform | 2340 | 19.50 | 99.9% | 14 | 14/0/0/0/0/0 |

## Yield by theory and policy

Rates deduplicate goals across K. The ‘clean’ columns drop a goal if `e` occurs as an argument of any operation on either side.

| theory | policy | >=2/h | >=3/h | >=4/h | clean >=2/h | clean >=3/h | clean >=4/h |
|---|---|---:|---:|---:|---:|---:|---:|
| exponent_two | completing(0.5) | 3577.2 | 2065.5 | 1511.7 | 1481.8 | 838.2 | 628.6 |
| exponent_two | completing(0.8) | 2578.6 | 1664.1 | 1109.4 | 1064.4 | 704.6 | 464.8 |
| exponent_two | merging(0.8) | 1559.7 | 989.8 | 674.9 | 614.9 | 374.9 | 254.9 |
| exponent_two | uniform | 3538.7 | 1769.4 | 1004.6 | 1664.4 | 764.7 | 434.8 |
| group | completing(0.5) | 1349.7 | 299.9 | 75.0 | 764.8 | 135.0 | 60.0 |
| group | completing(0.8) | 1019.9 | 255.0 | 60.0 | 539.9 | 135.0 | 60.0 |
| group | merging(0.8) | 1004.2 | 209.8 | 60.0 | 434.7 | 74.9 | 45.0 |
| group | uniform | 629.8 | 75.0 | 15.0 | 434.8 | 45.0 | 15.0 |
| left_group | completing(0.5) | 5051.7 | 2233.6 | 899.4 | 704.5 | 284.8 | 89.9 |
| left_group | completing(0.8) | 4872.8 | 2608.8 | 1229.4 | 839.6 | 419.8 | 209.9 |
| left_group | merging(0.8) | 5427.5 | 2908.7 | 1319.4 | 734.7 | 359.8 | 164.9 |
| left_group | uniform | 599.8 | 15.0 | 0.0 | 45.0 | 0.0 | 0.0 |
| monoid | completing(0.5) | 60.0 | 0.0 | 0.0 | 60.0 | 0.0 | 0.0 |
| monoid | completing(0.8) | 30.0 | 0.0 | 0.0 | 30.0 | 0.0 | 0.0 |
| monoid | merging(0.8) | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 |
| monoid | uniform | 15.0 | 0.0 | 0.0 | 15.0 | 0.0 | 0.0 |

## Held-out DEMO goals

- `exponent_two_commutes` appeared under `completing(0.5)` with needed size 8; it is no longer held out.
- `exponent_two_commutes` appeared under `completing(0.8)` with needed size 8; it is no longer held out.
- `exponent_two_commutes` appeared under `merging(0.8)` with needed size 8; it is no longer held out.
- `left_group_right_inverse` appeared under `completing(0.5)` with needed size 6; it is no longer held out.
- `left_group_right_inverse` appeared under `completing(0.8)` with needed size 6; it is no longer held out.
- `left_group_right_inverse` appeared under `merging(0.8)` with needed size 6; it is no longer held out.

## Fresh replay samples

- `completing(0.5)`: 20/20 sampled goals re-proved from their needed terms on a fresh start.
- `completing(0.8)`: 20/20 sampled goals re-proved from their needed terms on a fresh start.
- `merging(0.8)`: 20/20 sampled goals re-proved from their needed terms on a fresh start.
- `uniform`: 20/20 sampled goals re-proved from their needed terms on a fresh start.

## Comparison

Packet 2’s uniform group reference was 183 distinct goals/CPU-hour needing at least two constructions and 23 needing at least three. Its denominator included two- and three-generator cells at K = 6, 10 and 14, whereas EQ-4 fixes two generators and K = 10 or 14, so it is context rather than a like-for-like baseline.

Within EQ-4, group `uniform` yielded 630/75/15 goals per CPU-hour at needed sizes at least 2/3/4. `completing(0.5)` yielded 1,350/300/75: 2.1, 4.0 and 5.0 times the uniform rates. After identity-noise removal the corresponding rates were 435/45/15 and 765/135/60. `completing(0.8)` remained above uniform but below 0.5; `merging(0.8)` was also above uniform but did not beat `completing(0.5)`.

The strongest effect was in `left_group`: uniform yielded 600/15/0, while `completing(0.8)` yielded 4,873/2,609/1,229 and `merging(0.8)` yielded 5,428/2,909/1,319. Much of that is identity-shaped: after the noise filter, their at-least-three rates fell to 420 and 360 respectively. Monoid remained almost entirely trivial under every policy.

The preferred policies found `exponent_two_commutes` with a removal-minimal needed set of eight constructions and `left_group_right_inverse` with six. Both must therefore be removed from the held-out DEMO set for later experiments. `inverse_product` did not appear. No fresh-replay check failed.
