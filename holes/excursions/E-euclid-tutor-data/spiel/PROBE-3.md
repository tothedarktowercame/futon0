# Probe 3: AlphaZero-style self-play on Euclid I.1 and I.2

Date: 2026-11-11. This probe asks whether a learner given only System E's
rules, the current facts, and a terminal goal test can learn Euclid I.1 and
I.2. It does not read LeanEuclid proofs, traces, recorded solutions, playtest
reports, or hand-written move preferences. The reward is exactly 1 at the
goal and 0 otherwise.

## Representation

OpenSpiel requires numeric action ids to be stable across states. A grounded
move is identified by `(rule name, existing argument names)`; fresh output
names are deliberately omitted because they are state-local presentation, not
the mathematical choice. The id is a deterministic 64-bit BLAKE2 digest
reduced into a two-billion-element OpenSpiel action range. Every legal set is
checked for collisions and fails loudly if one occurs. Tests exhibit moves
that remain legal in two states and verify that their ids agree.

The observation is a fixed 4,096-float vector:

- 32 creation-ordered object slots, each with a point/line/circle one-hot;
- 2,048 deterministic feature-hash buckets for the current facts after object
  names are replaced by creation-order indices;
- 1,024 separate feature-hash buckets for the proposition's goal relations;
- a move-count one-hot.

This encoding records what the engine exposes without choosing special
geometric predicates. The object cap is checked rather than silently
truncating. Tests pin the tensor's declared shape and dtype through four plies.

## Learner

OpenSpiel's packaged AlphaZero could not run as installed: its JAX, Flax, and
Optax dependencies were absent. More importantly, its dense policy head would
allocate against the two-billion-id stable action space. I therefore wrote a
small PyTorch AlphaZero-style loop:

- a 64-unit state encoder, a 64-unit action encoder, a dot-product policy
  score for each grounded candidate, and a sigmoid value head;
- PUCT self-play, terminal value backup, replay buffer, cross-entropy against
  root visit counts, and squared terminal-value loss;
- neutral progressive expansion: each node receives six candidates from the
  rules-only sampler documented in Probe 2, and each root receives four PUCT
  simulations; actions are sampled from visit counts during self-play;
- greedy evaluation takes the largest visit count, with 20 games per
  checkpoint.

Progressive expansion is necessary because full legal expansion reaches more
than a thousand actions after a few moves. It does not alter the OpenSpiel
game's legal set; it limits which legal actions a search node considers under
its compute budget. No candidate is selected by a geometric heuristic.

I.1 used a six-move horizon (a solution is reachable within it); I.2 used
eight. Shorter horizons prevent irrelevant late constructions from driving
exact engine closure into the pathological regime seen in Probe 2. The goal
and reward are unchanged.

## Budget

The budget was fixed at about two CPU-hours: nominally 30 CPU-minutes for I.1
and 90 for I.2. Calibration found two configuration problems honestly recorded
here: 12 simulations × 12 candidates did not finish a game in two minutes,
and PyTorch initially consumed 1.8 cores. After changing to 4 × 6 and one
Torch thread, the measured runs held one core. I.1 used 21 final training
minutes plus roughly 13 CPU-minutes of calibration (about 34 total). I.2 used
three independent one-core seeds of 30 minutes each (90 total), run in
parallel. Actual aggregate compute was therefore about 124 CPU-minutes, close
to the stated two-hour budget.

Checkpoints (`.pt`) and raw JSONL logs are retained under `spiel/az/` and are
gitignored. The table below transcribes those logs.

## I.1 curve

| training minutes | self-play games | self-play wins | first win | greedy evaluation |
|---:|---:|---:|---:|---:|
| 0 | 1 | 0 | — | 0/20 |
| 7 | 813 | 0 | — | 0/20 |
| 14 | 1,652 | 0 | — | 0/20 |
| 21 | 2,480 | 0 | — | 0/20 |

Probe 2's neutral 12-move sampler won 1/500 I.1 games (0.2%); claude-9's
independent 30-second rerun won 3/206 (1.5%). The six-move AlphaZero run saw
no win at all. Its loss fell from 1.92 to 0.035 because the value head learned
the dominant zero outcome, not because the policy learned the construction.

**I.1 conclusion:** this learner did not discover or learn I.1 within the
allocated run, despite I.1 being reachable at the chosen horizon.

## I.2 curves

| seed | minutes | self-play games | cumulative wins | first win game | greedy evaluation |
|---:|---:|---:|---:|---:|---:|
| 20261111 | 0 | 1 | 0 | — | 0/20 |
| 20261111 | 10 | 487 | 0 | — | 0/20 |
| 20261111 | 20 | 907 | 0 | — | 0/20 |
| 20261112 | 0 | 1 | 0 | — | 0/20 |
| 20261112 | 10 | 1,591 | 2 | 1,139 | 0/20 |
| 20261112 | 20 | 3,242 | 6 | 1,139 | 0/20 |
| 20261112 | 30 | 4,851 | 7 | 1,139 | 0/20 |
| 20261113 | 0 | 1 | 0 | — | 0/20 |
| 20261113 | 10 | 444 | 0 | — | 0/20 |
| 20261113 | 20 | 882 | 0 | — | 0/20 |
| 20261113 | 30 | 1,339 | 0 | — | 0/20 |

The first seed was stopped at exactly 30 minutes to preserve the aggregate
budget, but its command had originally been given a 90-minute deadline; its
last completed log row is therefore minute 20. The table does not invent its
unlogged final game count. Across recorded rows there are at least 7,097 games
and seven wins, all in seed 20261112. Probe 2's random baseline had zero wins
in 200 I.2 games.

**I.2 conclusion:** rules-only self-play did find I.2. The first win was game
1,139 in one of three seeds, and that seed accumulated seven wins. It did not
learn a reliable solution: every greedy checkpoint, including after those
wins, was 0/20.

## Plain conclusion and limitation

This is a negative learning result with one positive discovery result. Under
sparse terminal reward, neutral progressive expansion, and roughly two
CPU-hours, the learner did not solve I.1 in training and never produced a
greedy policy that solved either proposition. It nevertheless found seven
valid I.2 terminal trajectories without proof data or shaped reward.

The limiting factor is the conjunction of sparse rewards and a huge grounded
action space. Most replay targets are zeros; the value loss can improve while
the policy learns nothing about a successful construction. Raising search
breadth caused exact System E closure and arithmetic entailment to dominate
before games completed. A stronger experiment would need more compute or a
principled structured/factorised policy over rule and argument slots—not a
hand preference or geometric reward—while preserving the same zero-data
constraint.
