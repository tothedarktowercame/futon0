# Probe 2: exact legal moves with sampled rollouts

Date: 2026-11-11. This repeats Probe 1 after making legal-action discovery
incremental and adding a rollout sampler. The zero constraint is unchanged:
only the palette, current System E facts, and the goal test are used. No proof,
trace, solution order, playtest report, or geometric move preference is read.
Case splits remain outside the game.

## Change and distribution

The full enumerator now indexes facts by predicate, checks each distinct
arithmetic entailment query once per state, and reuses the parent's legal
`(rule, arguments)` pairs in the monotone child state. The Probe-1 enumerator
is retained as `enumerate_actions_reference`; tests compare its exact action
set with the incremental path at three successive random states of I.1, I.2,
and I.3.

Rollouts do not materialise that set. Each proposal chooses a palette rule
uniformly, chooses one distinct matching premise environment uniformly, and
chooses every still-free argument uniformly from existing objects of the
required kind. It validates arithmetic entailments and resamples rejected
proposals. Thus the resulting distribution is the proposal distribution
conditioned on legality. It is neutral but is not uniform over legal grounded
actions: rules receive equal proposal mass regardless of how many groundings
they have.

## Budget and method

I fixed a ceiling of 30 CPU-minutes before measurement: 5 minutes for exact-set
and enumeration measurements, 10 for random play, 10 for MCTS, and 5 margin.
The runs stayed below it. Games use the normal 12-move cap except for the MCTS
rate table, whose explicit four-move cap is explained below.

Before/after is one warm-process measurement of the complete initial legal set
using `enumerate_actions_reference` and the new enumerator, with equality
checked. Random rates use the neutral sampler and completed games: 500 seeded
games for I.1 and 200 each for I.2 and I.3. I.5's 100-game attempt produced no
completed batch within 180 seconds.

## Exact enumeration and random play

| Proposition | legal moves | Probe 1 enumerator | incremental enumerator | speed-up | completed random games | wins | playouts/minute |
|---|---:|---:|---:|---:|---:|---:|---:|
| I.1 | 29 | 3.85 ms | 2.53 ms | 1.5× | 500 | 1 (0.2%) | 335 |
| I.2 | 58 | 8.70 ms | 3.74 ms | 2.3× | 200 | 0 (0%) | 588 |
| I.3 | 141 | 26.41 ms | 10.31 ms | 2.6× | 200 | 0 (0%) | 786 |
| I.5 | 3,218 | 11.709 s | 1.556 s | 7.5× | 0 | — | — |

Unlike Probe 1, I.1–I.3 now finish hundreds of full random games per minute.
The low success rate is therefore a real outcome rather than a timeout
artifact. I.5 is not within reach for rollouts on one machine: exact initial
enumeration is much better, but later closure and entailment checks still make
even a small random batch exceed three minutes.

## MCTS

`NeutralRolloutEvaluator` gives OpenSpiel's `MCTSBot` the sampler above for
leaf rollouts. MCTS still requires the *complete* legal set when it expands a
tree node. With the normal 12-move cap, even two simulations per move did not
finish one I.1 game in 120 seconds. A profile showed the late search spending
time in unmodified engine closure and Fourier–Motzkin entailment: by moves
4–6 the expanded states had 1,027–1,857 legal actions, and a sampled rollout
was also closing increasingly large geometric states.

For a finite rate measurement I therefore used a four-move cap, two
simulations per move, one sampled rollout per leaf (maximum length three), and
20 seeded games per proposition:

| Proposition | completed | wins | games/minute |
|---|---:|---:|---:|
| I.1 | 20 | 0 (0%) | 232 |
| I.2 | 20 | 0 (0%) | 39 |
| I.3 | 20 | 0 (0%) | 27 |

These are genuine completed MCTS games, but the four-move horizon makes their
zero success rates a feasibility baseline, not a useful search assessment.
Increasing to 20 simulations per move took 3.2 s, 13.9 s, and 69.1 s for one
four-move game of I.1, I.2, and I.3 respectively, all without success.

## Boundary of this packet

The wrapper no longer blocks ordinary random playouts for I.1–I.3, and the
legal-set invariant is tested against the old implementation. Full-width MCTS
and I.5 now hit the engine's exact closure/arithmetic cost rather than wrapper
candidate validation. Since this packet forbids changes to the engine, the
next speed step would need an engine-facing design (persistent closure,
incremental arithmetic entailment, or an OpenSpiel search with progressive
action expansion) reviewed as a separate change; silently returning a sampled
subset from `legal_actions` would change the game and is not acceptable.
