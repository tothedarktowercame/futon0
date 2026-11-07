# LeanEuclid Book I step motifs

Parsed 72 theorems, 557 euclid_apply steps, 931 steps total (all tactics).

Support = number of distinct propositions (primed variants of one proposition count once) the motif occurs in; threshold >= 3. Data-flow chains follow def-use edges from each use to the latest earlier binder of the name.

## ngram (class level), top by support

| motif | support | examples |
|---|---|---|
| cite-proposition -> other | 40 | Prop02.lean:37, Prop05.lean:31, Prop06.lean:16 |
| other -> cite-proposition | 39 | Prop02.lean:36, Prop03.lean:10, Prop06.lean:13 |
| other -> other | 38 | Prop01.lean:15, Prop01.lean:28, Prop02.lean:23 |
| cite-proposition -> cite-proposition | 29 | Prop05.lean:17, Prop09.lean:25, Prop09.lean:30 |
| other -> other -> cite-proposition | 24 | Prop02.lean:35, Prop06.lean:12, Prop08.lean:14 |
| cite-proposition -> cite-proposition -> other | 23 | Prop09.lean:25, Prop09.lean:30, Prop09.lean:51 |
| construct-line -> cite-proposition | 21 | Prop02.lean:11, Prop05.lean:16, Prop06.lean:15 |
| other -> cite-proposition -> other | 20 | Prop02.lean:36, Prop08.lean:15, Prop09.lean:32 |
| other -> other -> other | 20 | Prop02.lean:33, Prop02.lean:34, Prop04.lean:12 |
| cite-proposition -> other -> other | 19 | Prop09.lean:26, Prop09.lean:52, Prop10.lean:18 |
| cite-proposition -> construct-line | 18 | Prop02.lean:12, Prop05.lean:14, Prop06.lean:14 |
| cite-proposition -> other -> cite-proposition | 16 | Prop06.lean:16, Prop09.lean:31, Prop09.lean:57 |
| other -> cite-proposition -> cite-proposition | 16 | Prop09.lean:24, Prop09.lean:29, Prop09.lean:50 |
| cite-proposition -> cite-proposition -> cite-proposition | 15 | Prop15.lean:12, Prop22.lean:20, Prop22.lean:45 |
| metric/angle/area -> other | 15 | Prop01.lean:14, Prop01.lean:27, Prop02.lean:22 |
| extend -> cite-proposition | 14 | Prop05.lean:30, Prop07.lean:27, Prop07.lean:35 |
| other -> other -> other -> cite-proposition | 14 | Prop02.lean:34, Prop06.lean:11, Prop08.lean:13 |
| other -> extend | 13 | Prop05.lean:28, Prop07.lean:24, Prop07.lean:32 |
| other -> other -> cite-proposition -> other | 13 | Prop02.lean:35, Prop08.lean:14, Prop19.lean:12 |
| cite-proposition -> cite-proposition -> other -> other | 12 | Prop09.lean:25, Prop09.lean:51, Prop21.lean:14 |

## ngram (name level), top by support

| motif | support | examples |
|---|---|---|
| line_from_points -> line_from_points | 10 | Prop02.lean:13, Prop05.lean:15, Prop09.lean:19 |
| proposition_3 -> line_from_points | 10 | Prop05.lean:14, Prop06.lean:14, Prop06.lean:18 |
| use -> euclid_finish | 10 | Prop01.lean:15, Prop01.lean:28, Prop02.lean:23 |
| euclid_assert -> euclid_assert | 8 | Prop04.lean:12, Prop04.lean:13, Prop04.lean:14 |
| euclid_assert -> euclid_finish | 7 | Prop04.lean:15, Prop29.lean:18, Prop29.lean:24 |
| euclid_finish -> euclid_assert | 7 | Prop23.lean:17, Prop23.lean:44, Prop24.lean:33 |
| euclid_intros -> by_contra | 7 | Prop06.lean:11, Prop14.lean:11, Prop19.lean:11 |
| euclid_intros -> line_from_points | 7 | Prop02.lean:10, Prop07.lean:11, Prop33.lean:15 |
| extend_point_longer -> proposition_3 | 7 | Prop16.lean:18, Prop16.lean:28, Prop20.lean:12 |
| by_contra -> by_cases | 6 | Prop06.lean:12, Prop19.lean:12, Prop25.lean:13 |
| proposition_31 -> intersection_lines | 6 | Prop37.lean:15, Prop37.lean:17, Prop38.lean:15 |
| extend_point_longer -> proposition_3 -> line_from_points | 5 | Prop16.lean:18, Prop16.lean:28, Prop20.lean:12 |
| line_from_points -> extend_point_longer | 5 | Prop16.lean:17, Prop16.lean:27, Prop22.lean:16 |
| line_from_points -> proposition_4 | 5 | Prop05.lean:16, Prop06.lean:15, Prop06.lean:19 |
| proposition_13 -> euclid_finish | 5 | Prop15.lean:14, Prop17.lean:14, Prop29.lean:28 |
| proposition_4 -> euclid_finish | 5 | Prop06.lean:16, Prop06.lean:20, Prop16.lean:32 |
| by_cases -> euclid_finish | 4 | Prop08.lean:14, Prop13.lean:11, Prop24.lean:41 |
| euclid_assert -> by_cases | 4 | Prop08.lean:13, Prop24.lean:34, Prop35.lean:30 |
| euclid_intros -> by_cases | 4 | Prop02.lean:33, Prop13.lean:10, Prop23.lean:12 |
| euclid_intros -> by_contra -> by_cases | 4 | Prop06.lean:11, Prop19.lean:11, Prop25.lean:12 |

## chain (class level), top by support

| motif | support | examples |
|---|---|---|
| construct-line -> cite-proposition | 28 | Prop02.lean:12, Prop05.lean:17, Prop05.lean:18 |
| cite-proposition -> cite-proposition | 23 | Prop05.lean:17, Prop05.lean:18, Prop06.lean:16 |
| cite-proposition -> construct-line | 17 | Prop02.lean:13, Prop02.lean:14, Prop05.lean:16 |
| cite-proposition -> construct-line -> cite-proposition | 15 | Prop05.lean:17, Prop05.lean:18, Prop06.lean:16 |
| extend -> cite-proposition | 15 | Prop05.lean:31, Prop05.lean:31, Prop07.lean:28 |
| intersect -> cite-proposition | 13 | Prop10.lean:18, Prop21.lean:14, Prop21.lean:15 |
| cite-proposition -> intersect | 10 | Prop02.lean:16, Prop02.lean:18, Prop22.lean:26 |
| cite-proposition -> metric/angle/area | 10 | Prop02.lean:20, Prop02.lean:21, Prop02.lean:22 |
| cite-proposition -> intersect -> cite-proposition | 8 | Prop37.lean:19, Prop37.lean:20, Prop37.lean:19 |
| construct-line -> cite-proposition -> cite-proposition | 8 | Prop09.lean:25, Prop09.lean:30, Prop09.lean:33 |
| construct-line -> cite-proposition -> construct-line | 8 | Prop02.lean:13, Prop02.lean:14, Prop09.lean:19 |
| construct-line -> cite-proposition -> construct-line -> cite-proposition | 7 | Prop09.lean:25, Prop09.lean:33, Prop09.lean:30 |
| construct-line -> extend | 7 | Prop16.lean:18, Prop16.lean:28, Prop22.lean:17 |
| construct-line -> intersect | 7 | Prop02.lean:18, Prop02.lean:16, Prop09.lean:60 |
| extend -> cite-proposition -> cite-proposition | 7 | Prop16.lean:21, Prop16.lean:22, Prop16.lean:31 |
| cite-proposition -> cite-proposition -> cite-proposition | 6 | Prop09.lean:25, Prop09.lean:30, Prop09.lean:33 |
| cite-proposition -> construct-line -> cite-proposition -> cite-proposition | 6 | Prop09.lean:25, Prop09.lean:30, Prop09.lean:33 |
| cite-proposition -> construct-line -> cite-proposition -> construct-line | 6 | Prop09.lean:19, Prop09.lean:20, Prop09.lean:21 |
| cite-proposition -> construct-line -> extend | 6 | Prop16.lean:18, Prop16.lean:28, Prop24.lean:20 |
| cite-proposition -> extend | 6 | Prop16.lean:18, Prop16.lean:28, Prop24.lean:20 |

## chain (name level), top by support

| motif | support | examples |
|---|---|---|
| proposition_3 -> line_from_points | 11 | Prop05.lean:16, Prop06.lean:15, Prop06.lean:19 |
| line_from_points -> proposition_4 | 9 | Prop05.lean:17, Prop05.lean:18, Prop05.lean:17 |
| proposition_31 -> intersection_lines | 8 | Prop37.lean:16, Prop37.lean:18, Prop38.lean:16 |
| extend_point_longer -> proposition_3 | 7 | Prop16.lean:19, Prop16.lean:29, Prop20.lean:13 |
| line_from_points -> extend_point_longer | 6 | Prop16.lean:18, Prop16.lean:28, Prop22.lean:17 |
| line_from_points -> extend_point_longer -> proposition_3 | 6 | Prop16.lean:19, Prop16.lean:29, Prop22.lean:20 |
| line_from_points -> proposition_3 | 6 | Prop16.lean:19, Prop16.lean:29, Prop22.lean:20 |
| proposition_3 -> line_from_points -> proposition_4 | 6 | Prop05.lean:17, Prop05.lean:18, Prop06.lean:16 |
| proposition_3 -> proposition_4 | 6 | Prop05.lean:17, Prop05.lean:18, Prop06.lean:16 |
| extend_point_longer -> proposition_3 -> line_from_points | 5 | Prop16.lean:20, Prop16.lean:30, Prop20.lean:14 |
| line_from_points -> intersection_lines | 5 | Prop09.lean:60, Prop09.lean:60, Prop10.lean:17 |
| line_from_points -> proposition_5' | 5 | Prop07.lean:14, Prop07.lean:16, Prop07.lean:20 |
| line_from_points -> proposition_8 | 5 | Prop09.lean:33, Prop09.lean:33, Prop09.lean:33 |
| line_from_points -> extend_point_longer -> proposition_3 -> line_from_points | 4 | Prop16.lean:20, Prop16.lean:30, Prop24.lean:22 |
| line_from_points -> proposition_3 -> line_from_points | 4 | Prop16.lean:20, Prop16.lean:30, Prop24.lean:22 |
| proposition_3 -> line_from_points -> proposition_5' | 4 | Prop09.lean:25, Prop09.lean:30, Prop09.lean:25 |
| proposition_3 -> proposition_5' | 4 | Prop09.lean:25, Prop09.lean:30, Prop09.lean:51 |
| circle_from_points -> point_on_circle_onlyif | 3 | Prop01.lean:13, Prop01.lean:14, Prop01.lean:26 |
| extend_point -> proposition_13 | 3 | Prop14.lean:14, Prop17.lean:14, Prop24.lean:50 |
| extend_point_longer -> proposition_3 -> line_from_points -> proposition_4 | 3 | Prop16.lean:22, Prop16.lean:32, Prop24.lean:24 |

## The I.5 sequence (extend, cut off equal, join, SAS twice)

- not found as ngram name-level 5-gram extend_point -> proposition_3 -> line_from_points -> proposition_4 -> proposition_4
- not found as chain class-level extend -> cite-proposition -> construct-line -> cite-proposition -> cite-proposition
- not found as ngram class-level extend -> cite-proposition -> construct-line -> cite-proposition -> cite-proposition
- not found as ngram name-level core proposition_3 -> line_from_points -> proposition_4 -> proposition_4
- FOUND as chain class-level core cite-proposition -> construct-line -> cite-proposition -> cite-proposition: support 6 (proposition_16, proposition_24, proposition_42, proposition_46, proposition_48, proposition_9)

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

