# System E inventory (for scoping a fast search engine)

Read-only survey of `~/code/LeanEuclid/SystemE` (3,544 lines of Lean across the files
below), cross-referenced with the Book I citation counts mined in
`step-motifs.{json,md}`. The Lean code is the authority; section names of Avigad–Dean–Mumma
(2009) are noted where LeanEuclid's comments give them.

System E as LeanEuclid ships it: **101 axioms** (31 constructions: 22 point-rules, 2
line/circle-rules, 7 intersection-rules; 36 diagrammatic, 11 metric, 23 transfer
inferences; 1 superposition) over 3 sorts and 10 primitive relations.

Citation counts are occurrences of the rule name as the head of an `euclid_apply` in
`Book/Prop*.lean` (comments stripped). **Total: 557 euclid_apply steps = 293 citations of
earlier propositions (56 distinct names, primed variants separate) + 264 citations of
System E axioms (30 distinct names). No gap.** The other 71 axioms are never cited by
name in Book I — they are used implicitly by the SMT solver inside `euclid_finish` (§3).

## 1. Sorts and primitive relations

Sorts (`Theory/Sorts/Primitives.lean`):
- `Point` (axiom, Primitives.lean:4), `Line` (:9), `Circle` (:14).
- Defined from points, not primitive: `Segment` = two endpoints with `length : Segment → ℝ`
  (Segments.lean:7, :11); `Angle` = three points with `degree : Angle → ℝ` and constant
  `Right` (Angles.lean:7, :12, :14); `Triangle` = three points with `area : Triangle → ℝ`
  (Triangles.lean:6, :11). Metric quantities are reals; ordering is ordinary `<` on ℝ.

Primitive (opaque) relations (`Theory/Relations.lean`):
| relation | file:line | meaning |
|---|---|---|
| `Point.onLine` | Relations.lean:3 | point incident on line |
| `Point.sameSide` | Relations.lean:10 | two points on one side of a line |
| `collinear` | Relations.lean:18 | (declared, never used anywhere in LeanEuclid) |
| `between` | Relations.lean:25 | `between a b c`: b between a and c |
| `Point.onCircle` | Relations.lean:30 | |
| `Point.insideCircle` | Relations.lean:32 | |
| `Point.isCentre` | Relations.lean:38 | |
| `Line.intersectsLine` | Relations.lean:44 | |
| `Line.intersectsCircle` | Relations.lean:46 | |
| `Circle.intersectsCircle` | Relations.lean:53 | |

Defined abbreviations: `distinctPointsOnLine` (onLine ∧ onLine ∧ ≠, Relations.lean:6),
`opposingSides` (¬onLine ∧ ¬onLine ∧ ¬sameSide, :13), `outsideCircle` (¬inside ∧ ¬on, :35),
`formTriangle` (:58), `formRectilinearAngle` (:64), `formParallelogram` (:68).

## 2. Construction and inference rules

Book I citation count in brackets; `[0]` = never cited by name (SMT uses it implicitly).

### Constructions — points (`Theory/Constructions/Points.lean`)
| rule | file:line | statement (one line) | cites |
|---|---|---|---|
| `arbitrary_point` | Points.lean:5 | some point exists | [1] |
| `distinct_points` | Points.lean:8 | for any point, a distinct one exists | [1] |
| `line_nonempty` | Points.lean:11 | every line has a point on it | [1] |
| `exists_distincts_points_on_line` | Points.lean:14 | on a line, a point distinct from a given one exists | [2] |
| `exists_point_between_points_on_line` | Points.lean:17 | between two distinct points on a line there is a third | [1] |
| `exists_point_between_points_not_on_line` | Points.lean:21 | ...not on a second line | [0] |
| `point_between_points_shorter_than` | Points.lean:28 | point between b,c on L with |b─a| < s (NOT in ADM'09) | [5] |
| `extend_point` | Points.lean:32 | extend beyond c: a on L with between b c a | [34] |
| `extend_point_not_on_line` | Points.lean:36 | ...also off a second line | [0] |
| `extend_point_longer` | Points.lean:43 | extend beyond c by more than s (NOT in ADM'09) | [20] |
| `point_same_side` | Points.lean:47 | a point on the same side of L as b exists | [0] |
| `distinct_point_same_side` | Points.lean:50 | ...distinct from c | [0] |
| `point_on_line_same_side` | Points.lean:56 | stronger form of ADM rule 6 (NOT in ADM'09) | [3] |
| `exists_point_opposite` | Points.lean:60 | a point on the opposite side of L from b exists | [3] |
| `exists_distinct_point_opposite_side` | Points.lean:63 | ...distinct from c | [0] |
| `exists_point_on_circle` | Points.lean:66 | a point on α exists | [0] |
| `exists_distinct_point_on_circle` | Points.lean:69 | ...distinct from b | [0] |
| `exists_point_inside_circle` | Points.lean:72 | a point inside α exists | [0] |
| `exists_distinct_point_inside_circle` | Points.lean:75 | ...distinct from b | [0] |
| `exists_point_outside_circle` | Points.lean:78 | a point outside α exists | [0] |
| `exists_distinct_point_outside_circle` | Points.lean:81 | ...distinct from b | [0] |

### Constructions — lines and circles (`Theory/Constructions/LinesAndCircles.lean`)
| rule | file:line | statement | cites |
|---|---|---|---|
| `line_from_points` | LinesAndCircles.lean:3 | line through two distinct points exists | [85] |
| `circle_from_points` | LinesAndCircles.lean:6 | circle with centre a through b exists | [14] |

### Constructions — intersections (`Theory/Constructions/Intersections.lean`)
| rule | file:line | statement | cites |
|---|---|---|---|
| `intersection_lines` | Intersections.lean:3 | two meeting lines have a common point | [31] |
| `intersections_circle_line` | Intersections.lean:6 | a line meeting a circle crosses it in two points | [1] |
| `intersection_circle_line_between_points` | Intersections.lean:9 | crossing point between an inside and an outside point | [1] |
| `intersection_circle_line_extending_points` | Intersections.lean:13 | crossing point beyond an inside point | [5] |
| `intersection_circles` | Intersections.lean:17 | two meeting circles share a point | [3] |
| `intersection_same_side` | Intersections.lean:20 | circle-circle intersection on a chosen side | [1] |
| `intersection_opposite_side` | Intersections.lean:24 | circle-circle intersection on the opposite side | [1] |

### Diagrammatic inferences (`Theory/Inferences/Diagrammatic.lean`, ADM'09 §3.4)
Forward-chaining facts about between/sameSide/inside, plus intersection-existence
preconditions. All are `[0]` citations — the SMT solver discharges them implicitly.
| rule | file:line | statement |
|---|---|---|
| `two_points_determine_line` | :14 | two distinct points on L and M force L = M |
| `centre_unique` | :20 | a circle has at most one centre |
| `center_inside_circle` | :28 | the centre is inside the circle |
| `inside_not_on_circle` | :35 | inside implies not on |
| `between_symm` | :45 | between a b c ↔ between c b a, plus distinctness and ¬between b a c |
| `between_same_line_out` | :53 | between a b c, a,b on L ⇒ c on L |
| `between_same_line_in` | :61 | between a b c, a,c on L ⇒ b on L |
| `between_trans_in` | :69 | nested betweenness, inner transitivity |
| `between_trans_out` | :78 | nested betweenness, outer transitivity |
| `between_points` | :87 | three distinct collinear points: one of three betweenness orders (CASE SPLIT) |
| `between_not_trans` | :96 | between a b c ∧ between a b d ⇒ ¬between c b d |
| `same_side_rfl` / `_symm` / `_not_on_line` / `_trans` | :105/:112/:119/:127 | sameSide is reflexive/off-line/symmetric/transitive |
| `same_side_pigeon_hole` | :136 | three points off a line: two share a side (CASE SPLIT) |
| `pasch_1`–`pasch_4` | :145/:153/:163/:173 | Pasch-style between/sameSide interactions |
| `triple_incidence_1/2/3` | :187/:199/:212 | three concurrent lines, side relations |
| `circle_line_intersections` | :225 | inside point on chord line is between the two crossing points |
| `circle_points_between` | :236 | between two non-outside points ⇒ inside |
| `circle_points_extend` | :245 | extending past a non-inside point ⇒ outside |
| `circles_intersections_diff_side` | :255 | the two circle-circle crossings are on opposite sides of the centre line |
| `intersection_lines_opposing` | :267 | opposing sides of L along M ⇒ L,M meet |
| `intersection_lines_common_point` | :274 | shared point ⇒ lines intersect (NOT in ADM'09) |
| `parallel_line_unique` | :281 | parallel through a point is unique (NOT in ADM'09) |
| `intersection_symm` | :289 | line intersection is symmetric (NOT in ADM'09) |
| `intersection_circle_line_1/2` | :297/:304 | side/inside conditions ⇒ line meets circle |
| `intersection_circle_circle_1/2` | :312/:319 | in/out conditions ⇒ circles meet |
| `parallelogram_same_side` | :327 | parallelogram corners' sides (NOT in ADM'09) |

### Metric inferences (`Theory/Inferences/Metric.lean`, ADM'09 §3.5) — all `[0]` citations
`zero_segment_if` :21, `zero_segment_onlyif` :25, `segment_gte_zero` :33,
`segment_symmetric` :41, `angle_symm` :46, `angle_range` :54, `degenerated_area` :62,
`area_gte_zero` :69, `area_symm_1` :76, `area_symm_2` :80, `area_congruence` :89
(congruent by SSS+angles ⇒ equal area). Plain real-arithmetic facts about
lengths/degrees/areas.

### Transfer inferences (`Theory/Inferences/Transfer.lean`, ADM'09 §3.6)
Bridge diagrammatic relations and metric quantities.
| rule | file:line | statement | cites |
|---|---|---|---|
| `between_if` | :16 | between a b c ⇒ ab + bc = ac | [2] |
| `equal_circles` | :25 | same centre and radius ⇒ same circle | [0] |
| `point_on_circle_if` | :34 | centre-and-radius equal ⇒ on circle | [0] |
| `point_on_circle_onlyif` | :39 | on circle ⇒ radii equal | [13] |
| `point_in_circle_if` | :47 | radius smaller ⇒ inside | [0] |
| `point_in_circle_onlyif` | :52 | inside ⇒ radius smaller | [0] |
| `degenerated_angle_if` / `_onlyif` | :63/:67 | angle 0 ↔ collinear-and-not-between | [0] |
| `sum_angles_if` | :83 | angle sum + sides ⇒ sameSide conclusions | [1] |
| `sum_angles_onlyif` | :90 | side placement ⇒ angle addition | [16] |
| `perpendicular_if` / `_onlyif` | :102/:107 | right angle ↔ adjacent angles equal | [2] |
| `flat_angle_if` / `_onlyif` | :116/:125 | angle = 2∟ ↔ between (NOT in ADM'09) | [0] |
| `equal_angles` | :138 | vertical/identical rays ⇒ equal angles | [0] |
| `lines_intersect` | :151 | interior-angle sum < 2∟ ⇒ lines meet (parallel postulate) | [0] |
| `degenerated_area_if` / `_onlyif` | :162/:167 | area 0 ↔ collinear | [0] |
| `sum_areas_if` / `_onlyif` | :173/:178 | area additivity ↔ between | [8]/[0] |
| `parallelogram_area` | :189 | parallelogram diagonal area split (NOT in ADM'09) | [3] |
| `sum_parallelograms_area` | :197 | sub-parallelogram decomposition (NOT in ADM'09) | [1] |
| `rectangle_area` | :205 | rectangle area = product of sides (needed by I.47; NOT in ADM'09) | [3] |

### Superposition (`Theory/Inferences/Superposition.lean`)
| rule | file:line | statement | cites |
|---|---|---|---|
| `superposition` | Superposition.lean:7 | copy triangle abc onto segment dg on line L with chosen side; returns congruent copy b'c' with all side/angle equalities | [2] (I.4, I.8) |

### Earlier propositions as rules
293 citations of 56 theorem names (`proposition_1` ... `proposition_47` plus primed
variants). Most-cited: `proposition_3` 34, `proposition_4` 23, `proposition_5'` 14,
`proposition_31` 21, `proposition_13` 14, `proposition_34` 12, ... For the engine these
are just Horn-style rules: hypotheses = the cited proposition's antecedent, effect = its
conclusion.

## 3. How `euclid_apply` and `euclid_finish` work

`euclid_apply` (`Meta/Tactics/Solve.lean`):
- Syntax (Solve.lean:181–185): `euclid_apply t`, `... as i`, `... as (i,+)`.
- `EuclidApply` (Solve.lean:158) elaborates the rule term; if its type is an implication
  whose consequent has no loose binders, it is a rule with premises: `EuclidSolve`
  (Solve.lean:150) = `pushHole` (Solve.lean:48: add the premise-to-prove as a new *goal*
  and as a hypothesis-to-use) → `EuclidFinish true` (discharge that premise) → `finish`
  (Solve.lean:63: once the hole is filled, `obtain` the rule's conclusion, destructing
  conjunctions and existential witnesses via the `as` names). If the rule type is an `∃`,
  it is a pure construction: `obtain ⟨idents, *, h⟩` directly (Solve.lean:171–177).
  `elimAllConjunctions` (Meta/Tactics/Util.lean:64) splits the new facts into atomic
  hypotheses. So: **the *conclusion* of each euclid_apply is handled syntactically
  (obtain/destruct); only the *premises* go to automation/SMT.**

`euclid_finish` (Solve.lean:125–142):
- `preprocess` (Solve.lean:117) unfolds UniGeo-only `Triangle.congruent` (unused by Book);
  then `simp_all; split_ands` (Solve.lean:105) closes what standard simp can.
- For each remaining subgoal, `solveWithProvers` (Solve.lean:90): build the SMT query from
  the whole local context via `getESMTfromContext` and `addGoal`, and call `evalSmt`.
- `evalSmt` (`Meta/Smt/Solver.lean:65`): query = `euclidTheory`
  (`Meta/Smt/EuclidTheory.lean:11`, the System E axioms as SMT-LIB assertions — ~112
  asserts; also unused `euclidConstructionRulesFull/Short` variants at EuclidTheory.lean:944
  and :1340) ++ the context's facts (Esmt structure, `Meta/Smt/Esmt.lean:18`; translation
  through the `ELang.lean` IR and `Translator.lean`), then `(check-sat)`.
  Result `unsat` closes the goal with the **explicitly unsound** `SMT_VERIF` axiom
  (Solver.lean:47, applied at :54) — sound only because unsat really proves it; `sat` is a
  hard failure (Solve.lean:97–99); `unknown` admits a `sorry` (Solver.lean:60).
- `euclid_assert t` is just `have : t := by euclid_finish` (Solve.lean:187, :197).
- `euclid_intros` = intros + conjunction destructuring (`Meta/Tactics/Intros.lean:11`).

**Where the time goes:** the SMT calls. `systemE.solverTime` defaults to 300 s per call
(Solver.lean:11–14; Solve.lean sets it at :203), each remaining subgoal of each
`euclid_apply` premise and each `euclid_finish` spawns a solver run over the full axiom set
plus the accumulated context. That matches the excursion note's 1–90 s per proposition,
dominated by I.35/I.47/I.48.

## 4. Proposed minimal subset for replaying I.1–I.10

Rules actually cited by Book/Prop01–Prop10 (from the mine; propositions cited are
I.1–I.9 themselves). All are in the subset; everything else is deferred.

Relations needed: `onLine`, `distinctPointsOnLine`, `onCircle`, `isCentre`, `insideCircle`,
`between`, `sameSide`/`opposingSides`, `intersectsLine`, `Line.intersectsCircle`,
`Circle.intersectsCircle`, plus metric sorts `Segment.length`, `Angle.degree`, `∟` —
**13 diagrammatic notions + 3 metric ones.**

Cited rules, with engine role:
| rule | cites in I.1–I.10 | role |
|---|---|---|
| `line_from_points` | I.2, I.5–I.10 (18) | construction (introduces a line; needs a ≠ b) |
| `circle_from_points` | I.1 ×4, I.2 ×2, I.3 (7) | construction (centre + point ⇒ circle) |
| `extend_point` | I.5 ×2, I.7 ×4 (6) | construction (introduces a point beyond another) |
| `point_between_points_shorter_than` | I.5, I.9 ×2 (3) | construction, but **needs metric comparison |s|>0 and produces |b─a|<s — real-arithmetic side condition, not pure rule application** |
| `intersection_circles` | I.1 (1) | construction (needs `intersectsCircle` proved first) |
| `intersection_opposite_side` | I.1 (1) | construction (side-selecting variant) |
| `intersection_circle_line_extending_points` | I.2 ×2 (2) | construction (needs an inside point on the line) |
| `intersection_circle_line_between_points` | I.3 (1) | construction (needs inside + outside points) |
| `intersection_lines` | I.9, I.10 (2) | construction (needs `intersectsLine` proved first) |
| `point_on_circle_onlyif` | I.1 ×4, I.2 ×2, I.3 (7) | forward rule: on circle ⇒ equal radii |
| `between_if` | I.2 ×2 (2) | forward rule: between ⇒ segment addition (metric) |
| `sum_angles_onlyif` | I.5 ×2, I.7 ×6 (8) | forward rule: side placement ⇒ angle addition; **its premises are diagrammatic (sameSide) facts the engine must derive** |
| `superposition` | I.4, I.8 (2) | construction + forward: copies a triangle; introduces b', c', BC', AC' with a bundle of equalities |
| propositions 1–9 | the rest of I.1–I.10's cites | forward rules (each earlier proposition is a Horn rule) |

To make the premises of these reachable, the engine also needs the *supporting*
diagrammatic/transfer rules as forward chaining (never cited by name but load-bearing
behind every `euclid_finish`): `center_inside_circle`, `inside_not_on_circle`,
`between_symm`, `between_same_line_in/out`, `between_trans_in/out`, `sameSide` closure
(rfl/symm/trans/not_on_line), `pasch_1–3`, `circle_points_between/extend`,
`circle_line_intersections`, `intersection_lines_opposing`, `intersection_lines_common_point`,
`intersection_circle_line_1/2`, `intersection_circle_circle_1/2`, `circles_intersections_diff_side`,
`two_points_determine_line`, `centre_unique`, `point_in_circle_if/onlyif`, `point_on_circle_if`,
`equal_circles`, `zero_segment_*`, `segment_symmetric`, `angle_symm`, `equal_angles`,
`degenerated_angle_*`, `flat_angle_*`, `between_not_trans`, `same_side_pigeon_hole`,
`between_points`, `pasch_4`. That is ~30 supporting forward rules.
**Case analysis** (search branches, not deduction): `between_points` (3-way),
`same_side_pigeon_hole` (3-way), plus by_contra goals (I.6, I.14, ...) in Book statements.

**Deferred** (first needed later than I.10): `extend_point_longer` (first at I.16; 20
cites), `intersections_circle_line` (I.12), `exists_point_opposite` (I.11, I.12),
`perpendicular_if` (I.11), `sum_angles_if` (I.18, once), `arbitrary_point` and
`distinct_points` (I.22, genericity witnesses), `intersection_same_side` (I.22),
`exists_distincts_points_on_line` and `line_nonempty` (I.30),
`exists_point_between_points_on_line` (I.31), `point_on_line_same_side` (I.30, I.32),
`sum_areas_if/onlyif`, `degenerated_area_*`, `area_*` (I.35+ area propositions),
`lines_intersect` (parallel postulate, I.29+), `parallelogram_area` (I.43),
`rectangle_area` and `sum_parallelograms_area` (I.47),
all existence rules for on/in/outside-circle witnesses, `triple_incidence_*` (I.16+).

**Real geometry, not rule application:** (a) the metric conditions
`|s| > 0`, `|b─a| < s` in `point_between_points_shorter_than` and `extend_point_longer`
require continuity/density of the line — the engine needs a model (e.g. rational or
real coordinates) or an axiomatised length arithmetic to justify these moves;
(b) `superposition` is a rigid motion — treat it as a primitive construction plus the
equality bundle it returns, which is what LeanEuclid does; (c) deciding *which* circle–line
or circle–circle intersection point you get (near/far side) is what the side-selecting
variants encode — the engine's state must track in/on/out and sameSide precisely or it
cannot replay I.1–I.3.

## 5. Open questions

1. **Which solver actually runs, and is it a portfolio?** `solveWithProvers`' comment says
   "call z3 first, then cvc5" (Solve.lean:90–93) but the code makes a single `evalSmt` call;
   `Smt.Solver.create` comes from the external `Smt` package (Solver.lean:2, `import Smt.Solver`),
   so solver choice/portfolioing may live there. I did not trace into that dependency.
2. **The two unused SMT axiom sets** `euclidConstructionRulesFull`/`Short`
   (EuclidTheory.lean:944, :1340) are never referenced by `evalSmt`; presumably experiments
   in encoding constructions vs the flat `euclidTheory`. Whether `euclidTheory` alone is
   complete for Book I or the Lean axioms leak in via the context is unclear.
3. **`collinear` (Relations.lean:18) is dead** — declared, used nowhere. Safe to ignore.
4. **Soundness of `SMT_VERIF`** rests entirely on unsat meaning validity of the negated
   goal over the asserted theory; with an incomplete solver returning `unknown` the tactic
   silently emits `sorry` (Solver.lean:60) — fine for a tutor's referee as long as CI
   checks for sorries.
5. Whether an engine needs `same_side_pigeon_hole`/`between_points` case splits at all for
   I.1–I.10 replays, or whether the specific instantiations are always forced by the
   reference proof — a search-engine design question the inventory cannot settle.
