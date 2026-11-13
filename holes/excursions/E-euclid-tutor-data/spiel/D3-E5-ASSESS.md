# DESIGN-3 after E5: is E6b worth running?

claude-9 with Joe, 2026-11-13. Written after D3-E5 (no proof of I.4, I.9-I.13, I.15 by any arm)
and before the E6a data run finished. Joe asked for a very good argument why a retrain would be
different, given the liabilities; this note records that there is only a weak one, and the cheap
test that decides.

## Where the line stands

- **Successes, all on propositions whose givens were in the training data** (self-made theorems
  from the givens of I.1-I.3): I.1 in 11 opened nodes (gen 0: 26; random 33/187/106); I.2 from its
  givens in 4 moves, by both learned arms, where gen 0 and all random seeds failed (E4b-2c). The
  I.2 proof uses System E's superposition, shorter than Euclid's circle construction.
- **No transfer** (E5): on I.4, I.9-I.13 and I.15 the mixed scorer did no better than gen 0 or
  random: 0/21.

## The argument for E6b (weak)

E6a generates self-made theorems from the givens of I.4-I.25. Every success so far came on
givens that were in the training data, so data from the later givens is the one change with any
evidence behind it.

## The liabilities

1. **Branching.** Start positions have 41 to 6,443 legal moves (I.4: 1,030; I.24: 5,863). A
   400-node best-first search reaches depth about 2; Book proofs are 3-6 moves (I.15: three
   citations of I.13). The scorer would need the right move in its first page of 10, step after
   step. On held-out positions it has the right move in its top 5 about half the time.
2. **The data may be shallow.** E1 needed two rounds of filtering (degenerate and direct goals,
   then rule-uniform sampling) before its theorems were worth anything. Angle and triangle
   theorems from the later givens may be rare.
3. **Case splits.** I.13 needs by_cases; the prover offers no case-split move.
4. **Unequal coverage.** 12 of 21 E5 searches hit the 45-minute safety stop in the final sweep
   (remaining pages of already-opened nodes), so arms covered different amounts.

## The test that decides

Before any further search: for each E5 proposition, walk Euclid's own proof (the engine's
reference replay, `euclid_play.reference_steps`) and record, at each step, the rank and
probability each scorer gives the Book move among the legal candidates. This is a DEMO
diagnostic: the Book proof is used only to measure the scorer, never shown to the search. From
the probabilities, the cost best-first search assigns to the Book path (sum of -log p at the
scorer's temperature) and the page of each step give a forecast of whether a 400-node search can
reach it. E5's outcomes check the forecast on the current scorer.

Decision rule:
- E6a yield: if non-trivial angle/triangle theorems from the later givens are scarce, stop.
- Otherwise retrain, rerun this diagnostic on the new scorer, and run the prover only on
  propositions where every Book step falls on the first page or the Book path's cost is below
  the cost of nodes the search opens within budget.

## Diagnostic result (2026-11-13, claude-9)

Script: `scripts/euclid_book_path_probe.py` (DEMO: Book proofs measure scorers only). Receipts:
`~/storage/euclidzero/d3-e5-probe/{mixed,gen0}.json`. Mixed at T=1.5, gen 0 at T=1. Path cost =
sum over the Book proof's steps of -log p of the Book move.

| Prop | Book steps | mixed cost | gen 0 cost | near-free steps | mixed cost without them | most candidates on the path |
|---|---:|---:|---:|---:|---:|---:|
| I.1 | 3 | 4.3 | 10.2 | 0 | 4.3 | 35 |
| I.2 | 8 | 36.8 | 43.6 | 3 | 23.9 | 670 |
| I.4 | 3 | 15.7 | 23.3 | 0 | 15.7 | 4,329 |
| I.9 | 11 | 97.6 | 83.4 | 7 | 32.3 | 9,561 |
| I.10 | 5 (then a case split) | 23.7 | 24.3 | 3 | 10.1 | 514 |
| I.11 | 12 | 91.0 | 84.7 | 7 | 38.5 | 9,466 |
| I.12 | 10 | 61.2 | 66.6 | 3 | 38.2 | 4,919 |
| I.15 | 6 | 35.4 | 39.7 | 3 | 23.8 | 1,339 |
| I.13 | case split at step 0 | - | - | - | - | - |

"Near-free" = line_from_points, between_symm_parts, off_line_strict_triangle,
equilateral_apex_off_line: drawing a line through two existing points, or restating a
diagram fact.

Readings:

1. **Gen 0 is a uniform prior.** Its costs match the sum of log(candidates) (I.15: 39.7 against
   39.8; I.4: 23.3 against 23.2). Mixed is better than uniform on I.1, I.2, I.4, I.12, I.15, and
   worse on I.9 and I.11 (confidently wrong late in long proofs).
2. **Reachable cost.** I.1 was found at path cost 4.3 (mixed, 11 nodes) and 10.2 (gen 0, 26
   nodes); I.4 at 15.7 was not found within 335 nodes. So a 400-node search reaches paths of cost
   roughly 10-15. Every E5 proposition's Book path costs 16-98 under the mixed scorer.
3. **Citations of earlier propositions rank very low**: proposition_13 at ranks 570-659 of 1,339
   (I.15), proposition_8 at 7,108 of 9,446 (I.11), proposition_3 at 239 of 541 (I.9). The
   self-made training data from I.1-I.3's givens contains almost no such citations.
4. **Branching grows with depth**: thousands of candidates by step 5-10 (9,561 in I.9), most of
   them line constructions through pairs of points.
5. **Near-free steps are a third to two thirds of the path.** Removing their cost alone (not
   counting the smaller branching that would follow) brings I.10 to 10.1, inside the reachable
   range, and I.2, I.15 to about 24.

Verdict on E6b as planned: **not worth running for I.9-I.15.** Better data could plausibly fix
reading 3 (citations), but no plausible scorer brings 6-12-step paths through thousands of
candidates to cost 10-15. I.4 is the only near case (15.7; superposition ranked 80 of 1,030).

What would change the picture is the game, not the scorer: make drawing a line through two
existing points and restating diagram facts part of closure (free), as System E's diagram
inferences already are. That is the "make the co-exact facts free" recipe of the metagame notes,
applied to our own engine. It removes most candidates and a third to two thirds of each Book
path at once. Measurable cheaply: rerun this probe on an engine variant where those moves are
applied automatically.
