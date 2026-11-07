# LeanEuclid Book I step motifs

Parsed 72 theorems, 557 euclid_apply steps, 931 steps total (all tactics).

Support = number of distinct propositions (theorem names, primed variants counted separately) the motif occurs in; threshold >= 3. Data-flow chains use order-independent def-use edges.

## ngram (class level), top by support

| motif | support | examples |
|---|---|---|
| cite-proposition -> other | 58 | Prop02.lean:37, Prop05.lean:31, Prop06.lean:16 |
| other -> other | 51 | Prop01.lean:15, Prop01.lean:28, Prop02.lean:23 |
| other -> cite-proposition | 49 | Prop02.lean:36, Prop03.lean:10, Prop06.lean:13 |
| cite-proposition -> cite-proposition | 40 | Prop05.lean:17, Prop09.lean:25, Prop09.lean:30 |
| cite-proposition -> cite-proposition -> other | 31 | Prop09.lean:25, Prop09.lean:30, Prop09.lean:51 |
| construct-line -> cite-proposition | 29 | Prop02.lean:11, Prop05.lean:16, Prop06.lean:15 |
| cite-proposition -> other -> other | 28 | Prop09.lean:26, Prop09.lean:52, Prop10.lean:18 |
| cite-proposition -> construct-line | 25 | Prop02.lean:12, Prop05.lean:14, Prop06.lean:14 |
| other -> other -> cite-proposition | 25 | Prop02.lean:35, Prop06.lean:12, Prop08.lean:14 |
| extend -> cite-proposition | 22 | Prop05.lean:30, Prop07.lean:27, Prop07.lean:35 |
| other -> other -> other | 22 | Prop02.lean:33, Prop02.lean:34, Prop04.lean:12 |
| other -> cite-proposition -> cite-proposition | 21 | Prop09.lean:24, Prop09.lean:29, Prop09.lean:50 |
| cite-proposition -> cite-proposition -> cite-proposition | 20 | Prop15.lean:12, Prop22.lean:20, Prop22.lean:45 |
| other -> cite-proposition -> other | 20 | Prop02.lean:36, Prop08.lean:15, Prop09.lean:32 |
| other -> extend | 20 | Prop05.lean:28, Prop07.lean:24, Prop07.lean:32 |
| cite-proposition -> cite-proposition -> other -> other | 19 | Prop09.lean:25, Prop09.lean:51, Prop21.lean:14 |
| metric/angle/area -> other | 19 | Prop01.lean:14, Prop01.lean:27, Prop02.lean:22 |
| cite-proposition -> other -> cite-proposition | 18 | Prop06.lean:16, Prop09.lean:31, Prop09.lean:57 |
| other -> other -> other -> cite-proposition | 15 | Prop02.lean:34, Prop06.lean:11, Prop08.lean:13 |
| construct-line -> construct-line | 14 | Prop02.lean:13, Prop05.lean:15, Prop09.lean:19 |

## ngram (name level), top by support

| motif | support | examples |
|---|---|---|
| use -> euclid_finish | 21 | Prop01.lean:15, Prop01.lean:28, Prop02.lean:23 |
| line_from_points -> line_from_points | 14 | Prop02.lean:13, Prop05.lean:15, Prop09.lean:19 |
| proposition_3 -> line_from_points | 12 | Prop05.lean:14, Prop06.lean:14, Prop06.lean:18 |
| extend_point_longer -> proposition_3 | 11 | Prop16.lean:18, Prop16.lean:28, Prop20.lean:12 |
| euclid_intros -> extend_point | 9 | Prop05.lean:28, Prop11.lean:45, Prop11.lean:55 |
| euclid_assert -> euclid_assert | 8 | Prop04.lean:12, Prop04.lean:13, Prop04.lean:14 |
| euclid_finish -> euclid_assert | 8 | Prop23.lean:17, Prop23.lean:44, Prop24.lean:33 |
| euclid_intros -> line_from_points | 8 | Prop02.lean:10, Prop07.lean:11, Prop33.lean:15 |
| proposition_31 -> intersection_lines | 8 | Prop37.lean:15, Prop37.lean:17, Prop38.lean:15 |
| euclid_assert -> euclid_finish | 7 | Prop04.lean:15, Prop29.lean:18, Prop29.lean:24 |
| euclid_intros -> by_contra | 7 | Prop06.lean:11, Prop14.lean:11, Prop19.lean:11 |
| line_from_points -> extend_point_longer | 7 | Prop16.lean:17, Prop16.lean:27, Prop22.lean:16 |
| by_contra -> by_cases | 6 | Prop06.lean:12, Prop19.lean:12, Prop25.lean:13 |
| extend_point_longer -> proposition_3 -> line_from_points | 6 | Prop16.lean:18, Prop16.lean:28, Prop20.lean:12 |
| intersection_lines -> proposition_31 | 6 | Prop37.lean:16, Prop38.lean:16, Prop42.lean:23 |
| line_from_points -> extend_point_longer -> proposition_3 | 6 | Prop16.lean:17, Prop16.lean:27, Prop24.lean:19 |
| line_from_points -> line_from_points -> line_from_points | 6 | Prop09.lean:19, Prop09.lean:45, Prop11.lean:16 |
| line_from_points -> proposition_4 | 6 | Prop05.lean:16, Prop06.lean:15, Prop06.lean:19 |
| point_on_circle_onlyif -> point_on_circle_onlyif | 6 | Prop01.lean:13, Prop01.lean:26, Prop02.lean:19 |
| proposition_4 -> euclid_finish | 6 | Prop06.lean:16, Prop06.lean:20, Prop16.lean:32 |

## chain (class level), top by support

| motif | support | examples |
|---|---|---|
| construct-line -> cite-proposition | 37 | Prop02.lean:12, Prop05.lean:17, Prop05.lean:18 |
| cite-proposition -> cite-proposition | 32 | Prop05.lean:17, Prop05.lean:18, Prop06.lean:16 |
| extend -> cite-proposition | 24 | Prop05.lean:31, Prop05.lean:31, Prop07.lean:28 |
| cite-proposition -> construct-line | 23 | Prop02.lean:13, Prop02.lean:14, Prop05.lean:16 |
| cite-proposition -> construct-line -> cite-proposition | 21 | Prop05.lean:17, Prop05.lean:18, Prop06.lean:16 |
| intersect -> cite-proposition | 16 | Prop10.lean:18, Prop21.lean:14, Prop21.lean:15 |
| cite-proposition -> intersect | 15 | Prop02.lean:16, Prop02.lean:18, Prop22.lean:26 |
| cite-proposition -> metric/angle/area | 13 | Prop02.lean:20, Prop02.lean:21, Prop02.lean:22 |
| construct-line -> cite-proposition -> cite-proposition | 13 | Prop09.lean:25, Prop09.lean:30, Prop09.lean:33 |
| cite-proposition -> cite-proposition -> cite-proposition | 11 | Prop09.lean:25, Prop09.lean:30, Prop09.lean:33 |
| cite-proposition -> construct-line -> cite-proposition -> cite-proposition | 11 | Prop09.lean:25, Prop09.lean:30, Prop09.lean:33 |
| cite-proposition -> intersect -> cite-proposition | 11 | Prop37.lean:19, Prop37.lean:20, Prop37.lean:19 |
| extend -> cite-proposition -> cite-proposition | 11 | Prop16.lean:21, Prop16.lean:22, Prop16.lean:31 |
| construct-line -> cite-proposition -> construct-line | 10 | Prop02.lean:13, Prop02.lean:14, Prop09.lean:19 |
| cite-proposition -> cite-proposition -> construct-line | 9 | Prop09.lean:19, Prop09.lean:20, Prop09.lean:21 |
| cite-proposition -> cite-proposition -> construct-line -> cite-proposition | 9 | Prop09.lean:25, Prop09.lean:33, Prop09.lean:30 |
| construct-line -> cite-proposition -> construct-line -> cite-proposition | 9 | Prop09.lean:25, Prop09.lean:33, Prop09.lean:30 |
| construct-line -> extend | 9 | Prop16.lean:18, Prop16.lean:28, Prop22.lean:17 |
| construct-line -> intersect | 9 | Prop02.lean:18, Prop02.lean:16, Prop09.lean:60 |
| intersect -> metric/angle/area | 9 | Prop01.lean:13, Prop01.lean:14, Prop01.lean:26 |

## chain (name level), top by support

| motif | support | examples |
|---|---|---|
| proposition_3 -> line_from_points | 14 | Prop05.lean:16, Prop06.lean:15, Prop06.lean:19 |
| extend_point_longer -> proposition_3 | 11 | Prop16.lean:19, Prop16.lean:29, Prop16.lean:19 |
| proposition_31 -> intersection_lines | 11 | Prop37.lean:16, Prop37.lean:18, Prop38.lean:16 |
| line_from_points -> proposition_4 | 10 | Prop05.lean:17, Prop05.lean:18, Prop05.lean:17 |
| line_from_points -> extend_point_longer | 8 | Prop16.lean:18, Prop16.lean:28, Prop22.lean:17 |
| line_from_points -> extend_point_longer -> proposition_3 | 8 | Prop16.lean:19, Prop16.lean:29, Prop16.lean:19 |
| line_from_points -> proposition_3 | 8 | Prop16.lean:19, Prop16.lean:29, Prop22.lean:20 |
| line_from_points -> proposition_8 | 8 | Prop09.lean:33, Prop09.lean:33, Prop09.lean:33 |
| line_from_points -> intersection_lines | 7 | Prop09.lean:60, Prop09.lean:60, Prop10.lean:17 |
| proposition_3 -> line_from_points -> proposition_4 | 7 | Prop05.lean:17, Prop05.lean:18, Prop06.lean:16 |
| proposition_3 -> proposition_4 | 7 | Prop05.lean:17, Prop05.lean:18, Prop06.lean:16 |
| circle_from_points -> point_on_circle_onlyif | 6 | Prop01.lean:13, Prop01.lean:14, Prop01.lean:26 |
| extend_point_longer -> proposition_3 -> line_from_points | 6 | Prop16.lean:20, Prop16.lean:30, Prop16.lean:20 |
| line_from_points -> proposition_5' | 6 | Prop07.lean:14, Prop07.lean:16, Prop07.lean:20 |
| extend_point_longer -> proposition_3 -> proposition_3 | 5 | Prop22.lean:21, Prop22.lean:22, Prop22.lean:47 |
| line_from_points -> extend_point_longer -> proposition_3 -> line_from_points | 5 | Prop16.lean:20, Prop16.lean:30, Prop16.lean:20 |
| line_from_points -> proposition_3 -> line_from_points | 5 | Prop16.lean:20, Prop16.lean:30, Prop16.lean:20 |
| point_between_points_shorter_than -> line_from_points | 5 | Prop05.lean:15, Prop09.lean:17, Prop09.lean:20 |
| point_between_points_shorter_than -> proposition_3 | 5 | Prop05.lean:14, Prop09.lean:16, Prop09.lean:42 |
| point_between_points_shorter_than -> proposition_3 -> line_from_points | 5 | Prop05.lean:16, Prop09.lean:17, Prop09.lean:19 |

## The I.5 sequence (extend, cut off equal, join, SAS twice)

- not found as ngram name-level 5-gram extend_point -> proposition_3 -> line_from_points -> proposition_4 -> proposition_4
- not found as chain class-level extend -> cite-proposition -> construct-line -> cite-proposition -> cite-proposition
- FOUND as ngram class-level extend -> cite-proposition -> construct-line -> cite-proposition -> cite-proposition: support 3 (proposition_16, proposition_42'', proposition_42''')
- not found as ngram name-level core proposition_3 -> line_from_points -> proposition_4 -> proposition_4
- FOUND as chain class-level core cite-proposition -> construct-line -> cite-proposition -> cite-proposition: support 11 (proposition_16, proposition_24, proposition_42, proposition_42', proposition_42'', proposition_42''', proposition_46, proposition_46', proposition_48, proposition_9, proposition_9')

## Names classed as 'other'

- arbitrary_point
- assumption
- by_cases
- by_contra
- constructor
- euclid_assert
- euclid_finish
- euclid_intros
- exists
- exists_distincts_points_on_line
- exists_point_between_points_on_line
- exists_point_opposite
- have
- line_nonempty
- split_ors
- use

