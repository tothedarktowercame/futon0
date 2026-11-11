# Probe 7 — reverse curriculum on Euclid I.2

Date: 2026-11-11.

## Question and fixed protocol

This probe tests Joe's backward-curriculum proposal: begin one move before an
I.2 solution, require at least 8 wins in 10 greedy games, then move the start
back one move while carrying the same network forward.  Every stage allows
`k + 2` moves from a start `k` solution moves from the goal.  The compact
action game is unchanged (eight point slots, four line slots, four circle
slots, rules of arity at most four).  A prefix file only chooses the initial
engine state; it is not exposed as a policy target.

The ZERO and DEMO conditions are reported separately.  ZERO may use only
solutions discovered by neutral random compact play.  DEMO uses the known
eight-move construction solely to choose start states.

## Neutral random rate

Four workers completed the full 100,000-game target in 1,129.68 seconds,
before the 30-minute limit.  There were 2 wins: rate `0.00002` (0.002%),
with a Wilson 95% interval `[0.000005485, 0.000072928]` (0.000548% to
0.007293%).  Throughput was 1,327.8 games per core-minute.  Seed was 711.
The workers sampled only actions admitted by the compact dense codec; a test
now checks that every neutral sample is encodable.

Every winning move list is written to `rc7/random-wins.jsonl`.  Each win is
deletion-minimised by replaying after each proposed deletion and retaining it
only when the exact engine still reaches the I.2 goal; results are written to
`rc7/minimized-wins.jsonl`.

The two original lengths were 10 and 9.  Deletion minimisation with
`PYTHONHASHSEED=0` gave lengths 7 and 5 respectively; three separate replay
processes with that fixed hash seed verified both traces.  Without a fixed
hash seed the first trace's closure varied across processes (and therefore so
did its deletion minimum); the five-move trace used by ZERO reproduced in all
checks.  The reported minimisation receipt fixes the hash seed explicitly.

## ZERO arm

ZERO used only the shortest random-play solution above, of length 5.  It used
the same fixed 8/10 advancement threshold, network and training settings as
DEMO.  Stage 1 passed, so its complete network was carried to stage 2; stage
2 failed and stopped the curriculum.

| stage | moves remaining | learner games | training time | greedy from stage | advance? |
|---|---:|---:|---:|---:|---|
| 1 | 1 | 44 | 124.7 s | 10/10 | yes |
| 2 | 2 | 32 | 52.1 s | 0/10 | no |

The stage-2 checkpoint's separate greedy evaluation from the true I.2 initial
state was 0/10, with no timeouts.  ZERO therefore moved one stage backward
without any human construction, but did not learn a policy from the real
start.

## DEMO arm

The demonstration is the eight-move construction stated in the packet.  It
was grounded through the compact game's own legal-action map, and a regression
test checks every prefix: loading the prefix and replaying the suffix reaches
reward 1.

The predeclared advancement threshold was 8/10 greedy wins.  Training used the
packaged NNX MLP (width 32, depth 2), one actor, eight PUCT simulations per
move, and two learner steps.  BLAS and XLA threading were constrained and the
run was pinned to CPUs 12–13 so it did not contend on codex-25's CPUs 0–11.

| stage | moves remaining | learner games | training time | greedy from stage | advance? |
|---|---:|---:|---:|---:|---|
| 1 | 1 | 45 | 97.2 s | 0/10 | no |

Stage 1 did not pass the fixed threshold, so the curriculum correctly stopped
without advancing to stage 2.  The checkpoint's separate greedy evaluation
from the true I.2 initial state was 0/10, with no timeouts.

This is a demonstration-assisted arm, but the demonstration supplied no
supervised targets and was not placed in replay.  The learner still had to
discover the successful final action through AlphaZero self-play.  It found
no winning trajectory in the 45 stage-1 games, which explains why two learner
updates could not create a successful greedy policy.

## Start-state mechanism and gates

`start_file` is an additive game parameter.  Its moves are replayed into a
fresh engine state, while the episode clock and move cap begin at zero.  The
prefix moves remain in creation order for dense action encoding.  A separate
`--resume-from` runner option copies the complete same-shaped I.2 network
between stages; unlike I.1 `--warm-from`, it preserves the I.2 policy head.

`euclid_spiel_test.py` passes (12 tests in 76.23 seconds), including the new
all-prefix suffix-replay test and compact neutral-sampler test.
No Euclid engine, play/referee, server, or codex-25 output/process was changed.
