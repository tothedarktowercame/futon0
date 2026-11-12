# LEARN-0 — planned receipt

The executable receipt is written by `scripts/metagame_learner.py` after the
three leave-one-theory-out splits complete. It records the fixed feature list,
replay drops, model rates, baselines, named goals, frontier comparison,
logistic weights, and wall time. The checked-in implementation keeps model
training CPU-only and uses a 12-hidden-unit MLP plus logistic regression.

## Execution note

The prescribed full evaluation was started in a systemd user scope with the requested 8G/256-task limits and torch CPU threading. It was stopped after 10m13s by the author because the first complete pass was still CPU-bound and had produced no result artifact; no external process stopped it. The learner and focused tests pass, but no performance claim is made from this incomplete run.
