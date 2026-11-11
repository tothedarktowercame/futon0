# Probe 9: corrected Euclid I.2 uniform-random baseline

## Question and fixed experiment

The intended experiment was 100,000 independent games of
`euclid_book1(proposition=2,move_cap=10,dense_actions=true)`, using twelve
worker processes.  At every nonterminal state the driver chose uniformly
from the complete `state.legal_actions()` list.  Game `i` used seed
`9202611110 + i`.  A win would be counted only after the recorded moves
replayed successfully both in a fresh OpenSpiel state and through
`euclid_play.play`, with the latter reporting a solved, noncontradictory
outer state.

The driver and its replay-validation test are commit `e181b4a`.  The known
eight-move I.2 witness passes both validators; its seven-move prefix does not.

## Budget stop and recoverable data

The run was registered as `bg-1794409609098-260`.  It ran from
2026-11-11 15:06:49Z to 15:11:10Z and was stopped through `bg.py kill` at the
owner's request.  The registered parent exited with status 143.  A subsequent
process-table check found neither parent PID 3790128 nor any of its twelve
worker children.

Each worker opened its own `part-NN.jsonl`, but Python's buffered writer had
not flushed a single record when the parent was stopped.  All twelve files
were zero bytes.  Therefore the number of **recorded completed games is 0**.
The workers may have completed games in memory, but reconstructing or
estimating those records would not be a measurement.

## Requested statistics

| Quantity | Result |
|---|---:|
| Recorded completed games | 0 |
| Validated wins | unavailable (no observations) |
| Win rate | unavailable |
| Wilson 95% interval | undefined for `n = 0` |
| Wins by length | unavailable |

There is likewise no branching-factor table from this run, and no persisted
legal-action identities from which to calculate the share of rules at plies
8--10.  In particular, this run can neither confirm nor correct the separate
ply-10 observation of 1,303 legal moves, including 896
`extend_point_longer` and 280 `point_between_points_shorter_than` moves.
Those figures must remain labelled as the separate probe, not as a result of
this experiment.

## Performance finding

A preflight timing run (not part of the stopped sample) completed 24 games in
64.29 seconds on two workers, or 0.373 games/second.  Its start branching
factor was 49 and its legal lists grew into the thousands by later plies.
Linear scaling to twelve workers projected roughly twelve hours for 100,000
games, far beyond the intended budget.  The production workers were all
using a full CPU core when stopped.  A future measurement needs either
explicit per-record flushing plus a fixed completed-game/time budget, or a
faster semantics-preserving legal-action enumerator; reducing the sample
after looking at wins would not be an honest substitute.
