# Probe 10 — funded perturbed reverse curriculum on Euclid I.2

Date: 2026-11-11.

## Protocol

This is the **DEMO** arm: the known eight-move construction chooses each
prefix state, but supplies no policy targets.  Training used the point-named
dense action codec, 12 actors, 32 MCTS simulations, a four-minute stage cap,
and a 15-minute total wall cap.  Half of training episodes enabled a uniformly
sampled 0–2 legal-move perturbation; the other half were clean.  The process
and all children were confined to CPUs 20–31.  Every process used
`PYTHONHASHSEED=0`.

Advancement required at least 8/10 clean greedy wins and 6/10 wins on the
fixed perturbed seeds.  Every counted win was configured to be replayed from
a fresh state through both Spiel's `goal_reached` and `euclid_play`'s checker
before logging.  There were no wins, so this verification condition was
vacuous in this run.

## Result

| stage | moves remaining | completed actor games | training wall | clean | perturbed | advance? |
|---|---:|---:|---:|---:|---:|---|
| 1 | 1 | 12 | 240.7 s | 0/10 (4 timeouts) | 0/10 (7 timeouts) | no |

The learner never received enough completed trajectories to make an update;
the only checkpoint is the initial checkpoint 0.  Stage 1 therefore failed
its cap and the curriculum stopped.  Actor logs also report NaN sampling
probabilities after their first completed game; no such trajectory entered a
learner update.

The clean start had 545 legal actions in each of ten measurements.  The ten
fixed perturbed starts had respectively
`545, 550, 961, 545, 550, 545, 971, 545, 545, 1250` legal actions.

Clean and perturbed evaluations completed before the total deadline.  A
ten-game greedy evaluation from the true I.2 start was begun concurrently,
but the fixed 15-minute wall budget expired before it completed.  It was
stopped at the deadline, so the from-start result is **not measured**, rather
than reported as 0/10.

## Operational findings

An initial launch without CPU affinity made all 12 JAX actors abort while
creating CPU-client threads.  Its evidence is retained outside the delivered
`rc10/` directory.  Pinning the retry to the approved 12 CPUs eliminated that
resource failure.

The 371,361-action policy head dominates this experiment.  Sequential
ten-game evaluations took 282.2 seconds clean and 297.3 seconds perturbed;
game alarms cannot interrupt an active JAX call.  This left insufficient wall
time for the requested from-start evaluation.  All owned training and
evaluation processes were stopped and checked absent afterward.
