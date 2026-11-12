# DESIGN-2 packet 8b: mixed-stage value targets

Date: 2026-11-12.  Code commit: `eb27343`.  The run is under
`spiel/loop-smoke/value-8b/train-run` (ignored by Git).  `PYTHONHASHSEED=0`
and the THREADS-1 caps were used throughout.

## Configuration and run

The actors drew stage 1 with weight 0.4 and stage 2 with weight 0.6.  Each
stage used perturbation 0 or 1--2 moves with equal probability.  The replay
sampler reserved at least 30% of each 64-record batch for each stage whenever
both were available.  The starting weights were packet 7's step-39 snapshot.
The run used 12 actors, 32 simulations, root win-in-one at both stages, and
ran for the fixed 1,200 seconds.

| Receipt | Result |
|---|---:|
| Games | 84 |
| Stage-1 games | 37 |
| Stage-2 games | 47 |
| Stage-2 wins | 0 |
| Replay positions at shutdown | 207 |
| Learner steps | 112 |
| Peak service tasks | 1,484 |

All stage-2 games capped out and supplied honest `-1` targets; no move was
forced.  Thus there were no stage-2 wins to replay.  Every actor exited after
the orderly shutdown, and no child of the run parent remained.

## Loss curve

| Learner step | Total | Policy | Value MSE |
|---:|---:|---:|---:|
| 1 | 5.3399 | 2.0546 | 3.1236 |
| 112 | 1.3818 | 1.2657 | 0.0049 |

The learner did reduce both policy and value losses, so the packet's stop
condition did not fire.  The measured final network is checkpoint step 112.

## Value separation

Stage 1 used ten distinct perturbed starts absent from the new receipts,
selected with seeds `2026113002, 2026113003, 2026113004, 2026113005,
2026113007, 2026113009, 2026113010, 2026113011, 2026113012, 2026113015`.
Stage 2 used ten distinct
positions from observed `-1` cap-out receipts.  Their exact logical-state
SHA-256 keys are retained in `loop-smoke/value-8b/measurement.json`.

| Position set | Snapshot | Mean value | Range |
|---|---|---:|---:|
| held-out perturbed stage 1 | packet 7 | +0.99955 | +0.99948 .. +0.99960 |
| held-out perturbed stage 1 | mixed-stage | +0.77595 | +0.35488 .. +0.96370 |
| observed stage-2 cap-outs | packet 7 | +0.99958 | +0.99956 .. +0.99959 |
| observed stage-2 cap-outs | mixed-stage | -0.98209 | -0.99550 .. -0.92829 |

The old head was effectively constant at +1.  The new head cleanly separates
the two measured sets; the lowest stage-1 value remains positive while the
highest stage-2 value remains negative.

## Policy retention and stage-2 search

On packet 7b's same ten held-out starts, the packet-7 snapshot ranked a
winning move first on 9/10 and the mixed-stage snapshot also did so on 9/10.
The per-seed new ranks were `1,1,1,1,1,1,1,1,2,1`; the complete receipt is
`loop-smoke/value-8b/retention.json`.

Ten independent 32-simulation searches at the clean rc13 stage-2 prefix,
with root scan off, each had 530 candidates and visited 7 distinct root
children.  Packet 8a's corresponding result was 8/530; the value separation
did not by itself broaden search.

## Tests

`PYTHONHASHSEED=0 ... python -m pytest -q
euclid_candidate_replay_test.py euclid_candidate_alpha_zero_test.py
euclid_candidate_puct_test.py` passed: **17 passed in 146.38 s**.  New tests
cover seeded weighted stage choice, stage and logical-state provenance,
minimum per-stage replay shares, and the one-stage-available case.
