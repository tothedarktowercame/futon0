# EuclidZero design 3: self-generated theorems (claude-6 proposal, 2026-11-12)

Joe asked claude-6 for a redesign after run 9 (LEDGER D2-9) and the probe
(futon0 scripts/euclid_train_probe.py) showed the staged I.2 curriculum not
working. Joe, approving: "That sounds like a smart design, let's try that. In
fact it is very much like introducing the 'tutor' into the training mechanic
right away."

## Diagnosis (claude-6; claude-9 agrees)

1. "Lost" positions are artefacts of the move cap: constructions only add
   objects and facts (monotonicity still to be confirmed against the
   contradiction guard).
2. Constructions commute: the search space is a set of positions, not a tree
   (six of the seven good stage-2 first moves reach one position).
3. One theorem gives almost no signal; every closure fact except the goal is
   discarded.
4. The curriculum is the demonstration (Book prefixes), so climbing stages is
   recognising Book positions; I.1/I.3 showed no transfer (D2-8c).

## Design

Keep: engine as authority, notation, game interface, watch pages, receipts,
guarded candidates, DESIGN-2 features and candidate scorer, win-in-one check,
ZERO/DEMO labels.

Replace the training data: from a statement's givens (or small random
givens), play legal constructions (random, later policy-sampled); facts the
closure derives about constructed objects become goals that run achieved
(hindsight goals, as AlphaGeometry's synthetic data; method domain-general).
For each goal, find the needed constructions. Train the scorer, goal-
conditioned, on "is this candidate in the needed set for this goal". No Book
proof or prefix enters training; Book propositions are the held-out test.

Needed sets (claude-9 correction): the engine records provenance for
non-metric facts (`State.facts`: fact -> (rule, premise facts),
euclid_engine.py:548), but metric facts record only ("arithmetic", n)
(:535). Non-metric goals: trace provenance. Metric goals (I.2's goal is one):
replay with one construction removed, or cvc5 unsat cores (SMT-1).
Engine coverage gap (SMT-1's I.4 finding) limits which theorems can be
generated; record it beside results.

Prover (later, E3): budgeted best-first on one growing position, no
backtracking; on success prune to the needed set and compare length with the
Book's.

Other arms: Luna as a separately labelled LLM-prior arm (not ZERO); rules
encoded from their statements (wanted once training spans many goals);
pager/beam and coherent-logic provers later.

## Experiments

- E1 yield (no learning, ~30 min, 12 processes): from the givens of I.1-I.3,
  random legal runs of 6 constructions; collect derived facts mentioning a
  constructed object that are not the direct output of one construction; find
  needed sets. Pass: >=500 distinct (givens, goal) pairs per hour needing >=2
  constructions, >=50 needing >=3, 20 sampled needed sets prove their goal on
  fresh replay. Fail -> policy-guided or backward generation before training.
- E2 learning and transfer (~40 min): train the scorer on E1 data only; raw
  prior, no search. Pass: I.1 and I.3 one-from-solved winning move in the top
  5 and better than generation 0 on both; I.2 stage-2: one of the 7 good first
  moves in the top 7 (run 9: 30/532). No test position's key in training.
- E3: budgeted prover on I.1 from its hypotheses, labelled ZERO
  (self-generated).
