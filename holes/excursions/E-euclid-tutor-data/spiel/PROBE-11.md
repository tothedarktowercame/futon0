# Probe 11 — win-in-one search and funded Euclid I.2 curriculum

Date: 2026-11-11.

## Win-in-one evaluator

`WinInOneEvaluator` wraps the packaged AlphaZero evaluator.  At a
non-terminal state it applies every legal move to a clone.  If one or more
clones win according to the game's own return, it returns value `[1, -1]`
and divides all prior mass among exactly those moves.  Otherwise it returns
the wrapped evaluator's output unchanged.  The wrapper is installed in both
actor processes and greedy evaluation.  `--win-in-one` enables it;
`--no-win-in-one` preserves the earlier probes.

The check contains no geometric knowledge and receives no demonstration.
Transactionally refused dense actions are skipped: the action enumerator can
advertise such an action, but it is not a successor state.

With `PYTHONHASHSEED=0`, an untrained flat evaluator, and warm process state:

| position | legal moves | winning moves | cold check | repeated median | repeated mean (range) |
|---|---:|---:|---:|---:|---:|
| DEMO stage 1 prefix | 545 | 1 | 4.358 s | 2.492 s | 2.856 s (2.397–4.358) |
| true I.2 start | 49 | 0 | — | 0.0423 s | 0.0425 s (0.0412–0.0452) |

At the stage-1 prefix, untrained greedy MCTS with the check won 10/10.  With
the flag off, the evaluator is unchanged.  At the true I.2 start, where
there is no mate in one, values and priors were exactly equal to the wrapped
evaluator's output.

## Funded DEMO curriculum

This is the **DEMO** arm: the known eight-move construction chooses prefix
states only and is never a policy target.  Training used 12 actors, 32 MCTS
simulations, a 15-minute stage cap, a 60-minute total cap, and the win-in-one
wrapper.  Half of training games enabled a uniformly sampled 0–2 legal-move
perturbation.  Advancement required 8/10 clean and 6/10 fixed-perturbed
greedy wins.  Processes were confined to CPUs 20–31 and every process used
`PYTHONHASHSEED=0`.

| stage | moves remaining | actor games | training wall | clean | perturbed | advance? |
|---|---:|---:|---:|---:|---:|---|
| 1 | 1 | 1,309 | 930.1 s | 10/10 (0 timeouts) | 10/10 (0 timeouts) | yes |
| 2 | 2 | 0 | 930.0 s | 0/10 (10 timeouts) | 0/10 (10 timeouts) | no |

The stage caps first sent an orderly SIGTERM and allowed 30 seconds for
workers to exit; both training processes required forced termination after
that grace period, hence approximately 930 rather than 900 seconds.

Stage 1 reached checkpoint 20.  Its clean position had 545 legal actions in
all ten measurements.  The fixed perturbed positions had respectively
`545, 550, 961, 545, 550, 545, 971, 545, 545, 1250` legal actions.

Stage 2 resumed into checkpoint 0, which contains the carried stage-1
network, but concurrent Orbax restores failed in several actors with
TensorStore read errors while the remaining actors spent the cap restoring;
no actor completed a game.  The clean position had 532 legal actions.  Fixed
perturbed positions had respectively
`537, 947, 532, 537, 532, 957, 532, 532, 1235, 672` legal actions.  Both
evaluations then timed out on all ten games, so the curriculum stopped.

The separately budgeted greedy evaluation from the true I.2 start at the
last checkpoint was **0/10, with all 10 games timing out**, in 303.2 seconds.

## Receipts

All 20 counted stage-1 wins were replayed from fresh states.  The JSONL
receipts record `spiel_goal_reached=true` and `play_goal_reached=true` for
10/10 clean and 10/10 perturbed wins.  Stage 2 and from-start produced no
wins.  `summary.json`, per-stage logs, fixed prefixes, win receipts, and the
last useful checkpoints are under `spiel/rc11/`.

The run exited normally after 48 minutes.  No owned curriculum, actor,
learner, or evaluation process remained afterward.
