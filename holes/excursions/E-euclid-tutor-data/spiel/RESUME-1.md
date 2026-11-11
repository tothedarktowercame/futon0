# Resume 1 — reliable carried-network distribution

Date: 2026-11-11.

## Change

The learner is now the only process that restores a carried Orbax checkpoint.
It continues to save canonical Orbax checkpoints, and additionally exports the
inference parameters once as one `.npy` file per JAX tree leaf plus a manifest.
Actors build their model normally, memory-map those plain arrays, validate every
shape and dtype, and install them without entering Orbax or TensorStore.

This applies to the initial carried network and every later learner checkpoint.
The NumPy transport contains parameters only; optimizer state remains solely in
the learner's canonical checkpoint.  A unit test changes a parent model's
weights, sends them through this path, and compares every actor leaf
array-for-array.

## Twelve-actor check

The check resumed rc11 stage 2 from stage 1 checkpoint 20 with 12 actors,
`PYTHONHASHSEED=0`, CPUs 20–31, the clean stage-2 prefix, and two MCTS
simulations.  Two simulations isolate startup and transport from the much
larger cost of the 371,361-action policy head.  Every actor loaded checkpoint
0 and completed a game; no actor log contains a traceback, Orbax restore, or
TensorStore error.

| actor | first game after launch |
|---:|---:|
| 0 | 9.754 s |
| 1 | 10.296 s |
| 2 | 9.927 s |
| 3 | 9.662 s |
| 4 | 10.016 s |
| 5 | 9.901 s |
| 6 | 9.569 s |
| 7 | 9.636 s |
| 8 | 10.108 s |
| 9 | 9.703 s |
| 10 | 10.159 s |
| 11 | 9.052 s |

Before the change, the rc11 stage-2 run completed zero games in its 900-second
cap: several actors raised concurrent Orbax/TensorStore restore errors and the
rest remained in restore.  Thus the measured first-game result changes from
`>900 s / no game` for every actor to 9.052–10.296 seconds.

The compact probe logs are retained in `spiel/rc11-resume-probe/`.  Generated
checkpoints were removed after the check; they are reproducible and exceeded a
gigabyte, while the actor and learner logs are the relevant receipt.
