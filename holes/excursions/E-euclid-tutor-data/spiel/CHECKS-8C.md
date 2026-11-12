# DESIGN-2 packet 8c: reachability, sensitivity, and transfer checks

Date: 2026-11-12.  All measurements used `PYTHONHASHSEED=0` and the
THREADS-1 process caps.  Raw receipts are under
`spiel/loop-smoke/value-8b/checks-8c-*.json` (ignored by Git).

## 1. Stage 2 has seven useful first moves

The rc13 stage-2 prefix produced 532 premise-certified candidates.  Admission
refused five `name_point` candidates that only introduced canonical aliases,
leaving 527 playable first moves.  Exhaustively admitting each playable move
and scanning its resulting premise-certified candidates found **7/527
(1.33%)** that leave a win in one.  The 12-process scan took 625.91 seconds
wall time.

| First move (engine notation) | Winning reply | Packet-7 prior rank | Packet-8b prior rank |
|---|---|---:|---:|
| `circle_from_points(d_2,g_6) -> C_1` | `intersection_circle_line_extending_points(C_1,L_3,a,d_2)` | 90 | 31 |
| `off_line_strict_triangle(a,b,d_2,L_4)` | `superposition(b,c,b,a,b,d_2,BC,BC,BC,L_1)` | 109 | 90 |
| `off_line_strict_triangle(a,d_2,b,L_4)` | same superposition | 110 | 91 |
| `off_line_strict_triangle(b,a,d_2,L_3)` | same superposition | 27 | 33 |
| `off_line_strict_triangle(b,d_2,a,L_3)` | same superposition | 28 | 34 |
| `off_line_strict_triangle(d_2,a,b,L_1)` | same superposition | 102 | 89 |
| `off_line_strict_triangle(d_2,b,a,L_1)` | same superposition | 103 | 88 |

Thus a uniformly selected admitted first move has only a 1.33% chance of
leaving a one-ply finish.  The geometrically familiar continuation is the
circle move; the other six establish a strict triangle ordering and then use
I.4 superposition.

## 2. Missing unit checks

The fact-sensitivity test uses a real I.1 position after two moves.  Repeating
feature extraction is array-for-array identical; deleting the real fact
`on_line(a,AB)` changes the fact hypergraph arrays.

The grounding-overfit test uses the rc13 prefix and the five
`distinct_points` groundings.  It selects the rule's highest-prior grounding,
which is not the network's global top candidate, and trains only toward that
grounding.  It reaches overall rank 1 within the stated limit of 80 Adam
steps; the sibling grounding remains strictly below it.  This checks object
groundings of one rule rather than merely learning a rule bias.

The two feature/model files pass together: **13 passed in 44.34 s**.

## 3. Held-out one-from-solved positions

The vocabulary is global, so the packet-8b snapshot can score I.1 and I.3
without retraining.  The I.1 and I.3 positions use their Book constructions;
the I.2 position moves the independent `circle(b,c)` before the two lines
through `d_2`, changing the object creation order and final fresh name.

| Position; winning move | Candidates | Generation 0 rank (overall/rule/within) | Packet 8b rank (overall/rule/within) | Packet 8b value |
|---|---:|---:|---:|---:|
| I.1 after two circles; `intersection_circles(C_1,C_2)` | 31 | 3 / 2 / 1 | 12 / 8 / 1 | -0.9183 |
| I.3 after copied segment and circle; `intersection_circle_line_between_points(C_2,AB,a,b)` | 184 | 33 / 6 / 1 | 27 / 10 / 1 | -0.9032 |
| reordered I.2; `intersection_circle_line_extending_points(C_7,L_4,a,d_2)` | 545 | 190 / 7 / 9 | **1 / 1 / 1** | +0.7642 |

Packet 8b transfers across an independent reordering of its learned I.2
construction, but it does not transfer the winning-rule preference to the
different I.1/I.3 finishes.

Finally, ten admitted non-winning first-move children, selected by exact
logical keys absent from every packet-8b receipt, have packet-8b value mean
**-0.9307** (range -0.9413 to -0.9226).  The seven children that do leave a
win in one have mean **-0.6733** (range -0.9388 to +0.9192): only the circle
continuation is valued highly (+0.9192), while all six superposition routes
are valued -0.9388.  Generation 0 was essentially uninformative: means
+0.0037 and +0.0101 respectively.
