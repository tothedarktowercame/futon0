# DESIGN-3 E6a — hindsight theorems from I.4–I.25 givens

Date: 2026-11-12. Label: **ZERO**. Eight workers sampled rules uniformly for six plies. Generation used only each proposition's givens, guarded rule palette and closure; no Book proof moves or target hints were supplied.

## Per-proposition yield

Record columns are `single/conjunctive` counts grouped by needed-set size.

| proposition | runs | seconds/run | records by kind and needed size | triviality labels | slow replays dropped |
|---:|---:|---:|---|---|---:|
| I.4 | 6 | 411.52 | c2=16, c3=26, c4=1, s2=748, s3=179, s4=189, s5=146 | automatic-predicate=84, degenerate=785, degenerate+direct=13, degenerate+direct+naming=1, degenerate+naming=76, direct=10, naming=262, non-trivial=72, zero-length=2 | 0 |
| I.5 | 5 | 1169.07 | c2=499, c3=1105, c4=666, s2=421, s3=74, s4=15 | automatic-predicate=28, degenerate=288, degenerate+direct=3, degenerate+naming=28, direct=7, direct+naming=1, naming=60, non-trivial=2365 | 0 |
| I.6 | 6 | 235.75 | c2=19, c3=35, c4=12, s2=433, s3=321, s4=94 | automatic-predicate=76, degenerate=579, degenerate+direct=8, degenerate+naming=50, direct=5, direct+naming=2, naming=97, non-trivial=97 | 0 |
| I.7 | 7 | 513.12 | c2=35, c3=32, c4=5, s2=397, s3=103, s4=4 | automatic-predicate=60, degenerate=228, degenerate+direct=18, degenerate+direct+naming=1, degenerate+naming=4, direct=6, direct+zero-length=2, naming=144, non-trivial=113 | 0 |
| I.8 | 8 | 1074.84 | c2=88, c3=63, c4=183, c5=75, s2=765, s3=43, s4=148, s5=81 | automatic-predicate=132, degenerate=524, degenerate+direct=35, degenerate+direct+naming=2, degenerate+naming=46, direct=25, direct+zero-length=2, naming=162, non-trivial=518 | 0 |
| I.9 | 9 | 95.70 | c2=55, c3=2, c4=46, c5=2, s2=405, s3=234, s4=356 | automatic-predicate=112, degenerate=635, degenerate+direct=32, degenerate+naming=52, direct=16, direct+naming=1, direct+zero-length=2, naming=92, non-trivial=158 | 0 |
| I.10 | 10 | 11.23 | c2=17, c3=51, c4=26, c5=4, s2=399, s3=156, s4=37, s5=28 | automatic-predicate=92, degenerate=237, degenerate+direct=44, degenerate+direct+naming=4, degenerate+naming=106, direct=14, direct+naming=2, naming=87, non-trivial=132 | 0 |
| I.11 | 11 | 61.28 | c2=54, c3=202, c4=267, c5=30, s2=984, s3=577, s4=507, s5=18 | automatic-predicate=88, degenerate=1045, degenerate+direct=23, degenerate+direct+naming=10, degenerate+naming=485, direct=14, naming=358, non-trivial=616 | 0 |
| I.12 | 11 | 77.96 | c2=182, c3=248, c4=36, s2=657, s3=281, s4=19 | automatic-predicate=224, degenerate=355, degenerate+direct=35, degenerate+direct+naming=3, degenerate+direct+naming+zero-length=1, degenerate+naming=48, direct=37, direct+zero-length=2, naming=143, non-trivial=573, zero-length=2 | 0 |
| I.13 | 11 | 125.09 | c2=102, c3=295, c4=45, c5=61, s2=850, s3=420, s4=172, s5=71 | automatic-predicate=164, degenerate=874, degenerate+direct=29, degenerate+direct+naming=3, degenerate+naming=150, direct=21, naming=184, non-trivial=591 | 0 |
| I.14 | 11 | 374.09 | c2=43, c3=105, c4=289, c5=102, s2=1008, s3=888, s4=344, s5=10 | automatic-predicate=212, degenerate=1457, degenerate+direct=52, degenerate+direct+naming=4, degenerate+naming=129, direct=16, direct+naming=1, direct+zero-length=4, naming=246, non-trivial=668 | 0 |
| I.15 | 11 | 326.80 | c2=109, c3=45, c4=29, c5=1, s2=1178, s3=849, s4=715, s5=60 | automatic-predicate=168, degenerate=1651, degenerate+direct=32, degenerate+direct+naming=4, degenerate+naming=344, direct=18, direct+naming=1, naming=519, non-trivial=249 | 0 |
| I.16 | 11 | 337.00 | c2=88, c3=223, c4=56, c5=37, s2=1288, s3=652, s4=17, s5=1 | automatic-predicate=132, degenerate=1379, degenerate+direct=6, degenerate+direct+naming=1, degenerate+naming=67, direct=21, direct+naming=2, naming=267, non-trivial=485, zero-length=2 | 0 |
| I.17 | 11 | 195.49 | c2=109, c3=23, c4=36, c5=7, c6=9, s2=574, s3=211, s4=127, s5=23, s6=13 | automatic-predicate=184, degenerate=477, degenerate+direct=32, degenerate+naming=67, direct=17, direct+naming=1, naming=100, non-trivial=254 | 0 |
| I.18 | 11 | 197.23 | c2=178, c3=272, c4=135, s2=728, s3=302, s4=401, s5=14 | automatic-predicate=204, degenerate=786, degenerate+direct=30, degenerate+direct+naming=5, degenerate+naming=38, direct=45, direct+naming=2, naming=212, non-trivial=708 | 0 |
| I.19 | 11 | 361.42 | c2=50, c3=85, c4=14, c5=1, s2=672, s3=962, s4=139, s5=10 | automatic-predicate=140, degenerate=1226, degenerate+direct=43, degenerate+direct+naming=3, degenerate+naming=59, direct=16, direct+naming=1, direct+zero-length=2, naming=193, non-trivial=250 | 0 |
| I.20 | 11 | 207.79 | c2=255, c3=1210, c4=440, s2=874, s3=542, s4=23 | automatic-predicate=144, degenerate=874, degenerate+direct=23, degenerate+direct+naming=5, degenerate+naming=45, direct=23, naming=193, non-trivial=2037 | 0 |
| I.21 | 11 | 670.12 | c2=92, c3=119, s2=432, s3=208, s4=76 | automatic-predicate=116, degenerate=295, degenerate+direct=28, degenerate+direct+naming=2, degenerate+naming=17, direct=33, direct+zero-length=2, naming=117, non-trivial=317 | 0 |
| I.22 | 11 | 55.50 | c3=3, c5=2, s2=326, s3=260, s4=33, s5=101, s6=5 | automatic-predicate=48, degenerate=237, degenerate+direct=21, degenerate+direct+naming=3, degenerate+naming=114, direct=4, naming=290, non-trivial=13 | 0 |
| I.23 | 11 | 156.85 | c2=3, c3=189, c4=334, c5=126, c6=38, s2=511, s3=600, s4=504, s5=38 | automatic-predicate=172, degenerate=640, degenerate+direct=62, degenerate+direct+naming=3, degenerate+direct+zero-length=4, degenerate+naming=222, degenerate+zero-length=5, direct=26, naming=432, non-trivial=777 | 0 |
| I.24 | 6 | 1049.96 | c2=37, c3=127, s2=424, s3=185, s4=5 | automatic-predicate=32, degenerate=398, degenerate+direct=6, degenerate+direct+naming=1, degenerate+naming=32, direct=9, naming=107, non-trivial=192, zero-length=1 | 0 |
| I.25 | 5 | 1299.09 | c2=32, c3=48, s2=394, s3=52, s4=4 | automatic-predicate=64, degenerate=191, degenerate+direct=10, degenerate+naming=12, degenerate+zero-length=1, direct=12, direct+naming=1, naming=128, non-trivial=111 | 0 |

## Book-goal matches

Across all records, 11 state a Book goal among I.1–I.25.

| source givens | matched Book goal | records |
|---:|---:|---:|
| I.8 | I.2 | 1 |
| I.16 | I.1 | 3 |
| I.18 | I.2 | 1 |
| I.20 | I.2 | 1 |
| I.23 | I.2 | 3 |
| I.25 | I.2 | 2 |

## Earlier-proposition citations

Among non-trivial records, 8987 have a needed set containing at least one earlier `proposition_k` move. A record citing several earlier propositions contributes once to each corresponding row.

| cited proposition k | non-trivial records |
|---:|---:|
| I.1 | 5379 |
| I.2 | 407 |
| I.3 | 799 |
| I.9 | 735 |
| I.10 | 3110 |
| I.11 | 596 |
| I.16 | 4 |
| I.22 | 601 |
| I.23 | 6 |

## Ten sampled non-trivial theorems

Reservoir sample seed: `2026111206`.

1. I.18, needed 2, fresh replay **yes**: `superposition(c,a,a,c,b,a,AC,AC,AC,BC) [AC'=AC'_2 BC'=BC'_2 b'=b'_2 c'=c'_2]`; `angle_self_zero(b'_2,a)`. Goal: `[['_gt_ang', 'a', 'c', 'b', "b'_2", 'a', "b'_2"], ['_gt_ang', 'b', "c'_2", 'c', "b'_2", 'a', "b'_2"]]`.
2. I.5, needed 4, fresh replay **yes**: `proposition_1'(e,c,d,AC) [c=c_1]`; `proposition_1'(a,e,c_1,AC) [c=c_3]`; `angle_self_zero(c_1,c_3)`; `extend_point(AB,a,b) [x=x_6]`. Goal: `[['_gt_ang', 'a', 'e', 'x_6', 'c_1', 'c_3', 'c_1'], ['_gt_ang', 'e', 'a', 'x_6', 'c_1', 'c_3', 'c_1']]`.
3. I.18, needed 3, fresh replay **yes**: `use_witness(b,a,c,a,c,a,AC) [f=f_3]`; `proposition_1'(c,f_3,b,AC) [c=c_5]`; `proposition_1'(c,f_3,b,AC) [c=c_6]`. Goal: `[['gte', 'c', 'c_5'], ['_eq_len', 'c', 'c_5', 'c', 'c_6']]`.
4. I.5, needed 3, fresh replay **yes**: `proposition_1'(e,c,d,AC) [c=c_1]`; `proposition_1'(a,e,c_1,AC) [c=c_3]`; `angle_self_zero(c_1,c_3)`. Goal: `[['_gt_ang', 'a', 'b', 'c', 'c_1', 'c_3', 'c_1'], ['_gt_ang', 'b', 'c', 'd', 'c_1', 'c_3', 'c_1']]`.
5. I.12, needed 2, fresh replay **yes**: `superposition(a,b,b,a,b,c,AB,AB,AB,AB) [AC'=AC'_4 BC'=BC'_4 b'=b'_4 c'=c'_4]`; `proposition_9'(a,b,c'_4,AB,AC'_4) [f=f_6]`. Goal: `[['same_side', 'b', 'f_6', "AC'_4"], ['gte', "b'_4", 'f_6']]`.
6. I.11, needed 5, fresh replay **yes**: `proposition_1(u,v,AB) [d=d_1]`; `proposition_1(v,u,AB) [d=d_3]`; `point_between_points_shorter_than(AB,u,v,d_1,u) [a=a_4]`; `extend_point_longer(AB,u,v,d_1,u) [x=x_5]`; `proposition_10(u,x_5,AB) [d=d_6]`. Goal: `[['_gt_len', 'a_4', 'x_5', 'd_3', 'u'], ['_lt_len', 'd_1', 'v', 'd_6', 'x_5']]`.
7. I.25, needed 3, fresh replay **yes**: `extend_point_longer(AB,a,b,b,b) [x=x_2]`; `proposition_3(x_2,a,b,x_2,AB,AB) [q=q_4]`; `proposition_1(a,q_4,AB) [d=d_6]`. Goal: `['_eq_len', 'a', 'b', 'd_6', 'q_4']`.
8. I.5, needed 4, fresh replay **yes**: `proposition_1'(e,c,d,AC) [c=c_1]`; `proposition_1'(a,e,c_1,AC) [c=c_3]`; `angle_self_zero(c_1,c_3)`; `extend_point(AB,a,b) [x=x_6]`. Goal: `[['_gt_ang', 'a', 'x_6', 'e', 'c_1', 'c_3', 'c_1'], ['_gt_ang', 'c', 'e', 'c_3', 'c_1', 'c_3', 'c_1']]`.
9. I.12, needed 3, fresh replay **yes**: `use_witness(a,b,a,b,a,b,AB) [f=f_1]`; `exists_point_opposite(AB,c) [a=a_4]`; `point_between_points_shorter_than(AB,f_1,a,f_1,c) [a=a_6]`. Goal: `[['gte', 'a_4', 'a_6'], ['gte', 'a_6', 'c']]`.
10. I.5, needed 4, fresh replay **yes**: `proposition_1'(e,c,d,AC) [c=c_1]`; `proposition_1'(a,e,c_1,AC) [c=c_3]`; `angle_self_zero(c_1,c_3)`; `extend_point(AB,a,b) [x=x_6]`. Goal: `[['_gt_ang', 'a', 'b', 'e', 'c_1', 'c_3', 'c_1'], ['_gt_ang', 'b', 'x_6', 'e', 'c_1', 'c_3', 'c_1']]`.

## Run receipt

- Runs completed: 205; records: 36801.
- Slow needed-set replays dropped at the 120-second cap: 0.
- Aggregate worker wall time: 70301.1 seconds.
- Output: `/home/joe/storage/euclidzero/d3-e6a/`.
