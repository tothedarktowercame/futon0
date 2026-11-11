# Probe 13 — root-only mate checks and dead-end terminal states

Date: 2026-11-11.

## Empty-root cause and terminal defence

The Probe 12 empty roots were not caused by exhausting the compact action
space.  Every current Book I compact palette contains the zero-argument
`arbitrary_point` construction, so an ordinary uncapped state always has at
least that legal action.  The actual disagreement was in `EuclidState.clone`:
a perturbed start raises the episode's move cap by the number of perturbation
moves, but a clone reconstructed the game-level, unperturbed cap.  At the
extra allowed ply the played state was non-terminal while MCTS's clone was
terminal.  Its first simulation therefore returned a terminal root with no
children, and packaged AlphaZero failed in `best_child()`.

The clone now copies the episode cap.  A regression replays a perturbed state
to the old boundary and checks that the original and clone remain live with
the same cap.  No `max() iterable argument is empty` failure occurred anywhere
in rc13.

As independent defence, a state whose enumerated compact action set is empty
is terminal with reward zero.  A regression constructs that boundary directly
and checks both `is_terminal()` and the draw return.  The uncached fast path
uses the always-legal `arbitrary_point` fact above, avoiding a full action-space
scan merely to establish that an ordinary state is live.

## Root-only win-in-one

`--win-in-one` now accepts `all`, `root`, or `off`; bare `--win-in-one` retains
the earlier `all` behavior.  Root mode wraps the MCTS bot: it scans terminal
wins once where an actual move is selected, returning a winning root when one
exists, and otherwise delegates to unchanged MCTS with the plain neural
evaluator at every leaf.  Thus geometry and the demonstration do not enter the
search policy.

At the clean stage-2 prefix, a 32-simulation move from the carried Probe 12
network took **22.865 s** in root mode (532 legal actions).  Probe 12 measured
about 70 seconds per move when each of the 32 new tree nodes paid a cold mate
scan.  The root-mode regression wins the untrained stage-1 position 10/10; at
the no-mate I.2 initial position it chooses the same move as plain MCTS under
the same random seed.

## Funded DEMO curriculum

This is the **DEMO** arm: one known eight-move construction selected the start
prefixes and supplied no policy targets.  The run used root-only mate checks,
12 configured actors, 32 simulations, 15-minute stage caps, half perturbed
training starts, and `PYTHONHASHSEED=0`.  Advancement required 8/10 clean and
6/10 fixed-perturbed wins.

| stage | moves remaining | actor games at receipt | training wall | clean | perturbed | advance? |
|---|---:|---:|---:|---:|---:|---|
| 1 | 1 | 2,043 | 930.1 s | 10/10 | 10/10 | yes |
| 2 | 2 | 21 | 930.1 s | 0/10 (10 timeouts) | 0/10 (10 timeouts) | no |

All twenty stage-1 evaluation wins were replayed in a fresh engine state and
accepted by both Spiel's `goal_reached` and `euclid_play`'s checker.  The stage
1 clean evaluation took 26.4 seconds and perturbed evaluation 59.3 seconds.
Clean legal-action counts were ten copies of 545; perturbed counts were
`545, 550, 961, 545, 550, 545, 971, 545, 545, 1250`.

Stage 2 produced checkpoint 1.  Its clean legal-action counts were ten copies
of 532; perturbed counts were
`537, 947, 532, 537, 532, 957, 532, 532, 1235, 672`.  Both evaluations took
about 302.8 seconds.  There were no wins to replay.

The durable `bg.py` scope had 1,947 of its 2,048 task slots occupied when
stage 2 started.  Six JAX actors failed to create their CPU-client threads;
the six survivors completed 27 games in their logs, of which 21 were present
when the stage receipt was sampled and 15 reached the learner before shutdown.
This resource failure reduced the funded stage's effective CPU use; it is not
an empty-root or checkpoint-restore failure.  The surviving actors loaded the
carried network through the plain-array path without Orbax restore errors.

## From-start result

Greedy evaluation from the true I.2 start was **0/10** at stage-2 checkpoint 1.
All ten games completed normally at the move cap in 220.5 seconds total; there
were no timeouts, search failures, or claimed wins.

The run directory `spiel/rc13/` retains only the last useful checkpoint per
stage (stage 1 checkpoint 31 and stage 2 checkpoint 1), the stage-by-stage
logs and receipts, and the final summary.  All owned training and evaluation
workers were absent at completion.
