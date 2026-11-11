# Probe 1: OpenSpiel reachability without proof guidance

Date: 2026-11-11.  The probe used `scripts/euclid_spiel.py` and OpenSpiel
1.6.12 under Python 3.12.  The wrapper reads only a proposition's palette,
the current System E facts, and the goal predicate.  It does not read proof
traces, recorded plays, solution orders, or playtest reports.  Actions are
ordered lexicographically by rule name, argument tuple, and generated names.
Case splits (`by_cases` and `by_contra`) are deliberately not actions in this
first game.

## Budget and method

I fixed a ceiling of 30 CPU-minutes before measuring: 5 minutes for structural
counts and step timings, 10 minutes for uniform play, 10 minutes for MCTS, and
5 minutes of margin.  The probe stopped well below that ceiling once repeated
per-game limits established that neither policy could finish a playout.

For each proposition I measured the exact initial legal-action count.  Initial
engine-step time is the mean time to clone and apply each of the first ten
neutrally ordered actions (it excludes legal-action enumeration).  Uniform
play used five deterministic seeds per proposition, a 12-move cap, and a
5-second wall limit per attempt.  The branching-factor distribution includes
every state whose legal actions were completely enumerated before a timeout.

Plain MCTS used OpenSpiel's `MCTSBot`, UCT 1.4, two simulations per move, and
one uniformly random rollout of maximum length one per leaf.  The short rollout
was necessary because even one unbounded rollout did not finish in 60 seconds
on I.1.  Each measured MCTS game had a 10-second wall limit and the same
12-move cap.  Thus these are feasibility measurements, not evidence about a
well-tuned searcher.

## Results

| Proposition | initial branching | initial enumeration | engine step | random completed / attempted | random successes | observed branching factors (n; min / median / mean / max) | MCTS completed / attempted | MCTS successes | moves before timeout |
|---|---:|---:|---:|---:|---:|---|---:|---:|---:|
| I.1 | 29 | 4.4 ms | 0.55 ms | 0 / 5 | 0 | 21; 29 / 143 / 228.8 / 789 | 0 / 1 | 0 | 3 |
| I.2 | 58 | 10.8 ms | 0.59 ms | 0 / 5 | 0 | 24; 58 / 149 / 222.9 / 792 | 0 / 1 | 0 | 3 |
| I.3 | 141 | 27.6 ms | 0.81 ms | 0 / 5 | 0 | 19; 141 / 399 / 454.8 / 1,035 | 0 / 1 | 0 | 2 |
| I.5 | 3,218 | 13.70 s | 21.53 ms | 0 / 5 | 0 | no post-start state completed enumeration in 5 s | 0 / 1 | 0 | 0 |

There is therefore no completed-play success rate to estimate: all 20 uniform
attempts timed out, as did all four deliberately tiny MCTS attempts.  The
observed success count is 0, but reporting it as “0%” without the timeout
denominator would be misleading.  A neutral breadth-first search used only as
an automated test does find an I.1 win within its first 1,000 visited states,
so I.1 is reachable through this action interface and reward 1 is not vacuous.

## What made the wrapper awkward

The browser computes offered moves, while the Python engine exposes rule
application but no legal-move iterator.  The wrapper therefore unifies each
rule's symbolic premises with current facts, fills remaining typed arguments
from current objects, generates deterministic fresh names, and lets the engine
validate rules with entailment premises.  System E also computes geometric
closure during application and entailment; after a few moves both the number
of candidates and the cost of checking them grow sharply.  I.5 shows the
additional cost of treating all earlier propositions as immediately available
rules: its initial palette already expands to 3,218 grounded actions.

This first probe answers the narrow reachability question: the OpenSpiel game
is valid and I.1 has a reachable win, but unguided full-width random rollouts
and plain MCTS are not operational at this action granularity.  A learning
packet will need a principled action representation or progressive expansion;
inserting hand preferences would violate the zero constraint.
