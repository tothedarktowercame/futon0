# Probe 4 — packaged OpenSpiel AlphaZero on Euclid I.1 and I.2

Date: 2026-11-11.  This probe asks whether OpenSpiel's packaged AlphaZero,
given only the System E rule palette, current facts and terminal goal test, can
learn Euclid I.1 and then I.2.  It does not read proof traces, LeanEuclid,
recorded solutions or playtest reports, and it uses terminal reward 1/0 only.

## Runtime and representation

The Python 3.12 environment uses OpenSpiel 2.0.2, JAX/JAXlib 0.12.0.1, Flax
0.12.12, Optax 0.2.8, Chex 0.1.92 and Orbax Checkpoint 0.12.9.  OpenSpiel's
`model_test.py` and `evaluator_test.py` pass (10 passed, 2 skipped).  JAX was
run on CPU with four-core affinity and `--xla_cpu_multi_thread_eigen=false`;
without affinity, concurrent learner and actors exhausted the session's thread
allowance while constructing CPU thread pools.

Actions have a fixed mixed-radix identity: rule name in lexical order followed
by each argument's creation-order slot among objects of the required kind.
The neutral caps are 8 points, 4 lines and 4 circles.  The packaged dense
policy cannot represent the palette's 10-argument `superposition` rule without
tens of millions of logits, so compact mode uniformly excludes every rule of
arity above four.  The ordinary OpenSpiel wrapper still exposes the complete
legal set.  Compact I.1 has 28,449 actions and compact I.2 has 30,753.

OpenSpiel's packaged implementation rejects games whose reported player count
is not two.  The opt-in AlphaZero facade therefore reports a non-acting second
bookkeeping player and zero-sum returns `[win, -win]`; player 0 still makes
every construction move.  The default Euclid game remains single-player.

The observation remains the neutral 4,096-float tensor from Probe 3: typed
objects in creation order, hashed present facts, hashed goal facts and ply.

## Settings and fixed budget

The I.1 acceptance run uses the packaged NNX MLP (width 32, depth 2), four
actors, 32 PUCT simulations/move, `c=1.41`, root Dirichlet epsilon 0.25 and
alpha 1.0, temperature 1 (greedy after ply 4), replay size 256, reuse 4,
batch 64 and learning rate 0.001.  Greedy evaluation has no root noise.

The fixed ceiling was about three CPU-hours total.  The measured training and
evaluation work used about 2.1 CPU-hours (four cores for I.1, then two cores
per concurrent I.2 arm, plus the fixed-point evaluations).  I.1 was the hard gate:
I.2 would not run unless early I.1 self-play met the measured random baseline
of 21/5,449 = 0.39% at the same six-move horizon.

## Results

Results are filled from `spiel/az4/*/learner.jsonl`; checkpoints remain
gitignored.

| arm / checkpoint | elapsed | self-play games | cumulative wins | greedy evaluation |
|---|---:|---:|---:|---:|
| I.1 step 1 | 1.6 min | 11 | 1 | not run |
| I.1 step 2 | 2.7 min | 22 | 1 | 0/5 |
| I.1 step 10 | 9.8 min | 113 | 12 | 0/10 |
| I.1 step 20 | 13.5 min | 241 | 92 | 10/10 |
| I.1 step 30 | 14.5 min | 411 | 259 | 20/20 |
| I.2 cold | 10.1 min | 22 recorded | 0 | timed out, 0 games in 5 min |
| I.2 I.1-trunk warm | 10.1 min | 66 recorded | 0 | 0/10 |

The two I.2 rows above used `move_cap=6`.  The compact I.2 construction needs
eight moves, so those arms were run at an unwinnable horizon and are retained
only as a record of the mistaken setup.  They are superseded below.

The acceptance gate passed immediately: the first 11 I.1 games contained one
win (9.1%), above the 0.39% random baseline.  More importantly, the no-noise
policy changed from 0/10 at step 10 to 10/10 at step 20 and 20/20 at step 30.
Thus this run both discovered I.1 and consolidated a policy that solves it.

For I.2, the cold and warm arms received the same two-core, ten-minute wall
budget and all other settings were identical.  Warm transfer copied every
shape-compatible parameter from the I.1 checkpoint; because I.2 has 2,304
additional action logits, its policy output layer remained freshly
initialised.  Neither arm ever won.  The warm arm processed more games (66
versus 22), because the cold policy repeatedly selected branches with much
more expensive exact legal-action enumeration.  A ten-game greedy warm
evaluation was 0/10.  The cold greedy policy entered such an expensive branch
that it completed no evaluation game within a separate five-minute limit;
this is reported as a timeout, not as 0/10.

## Conclusion

OpenSpiel's packaged AlphaZero learned a reliable I.1 policy from rules and
terminal reward alone.  The initial I.2 comparison was invalid because its
six-move horizon was too short; the corrected cap-10 comparison follows below.

The result is specifically for the compact action game.  Fifteen palette rules
of arity five through ten are omitted uniformly; none is selected for its
geometric meaning.  That restriction and the expensive exact enumeration on
some policy branches are the principal limits on extrapolating the result to
the complete System E move game.

## Cap-10 I.2 correction

A regression test now replays an eight-move reachability witness in compact
I.2 at `move_cap=10` and obtains terminal reward 1.  The witness is used only
by that reachability test; it is not available to self-play, the observation,
the policy or the reward.  `--move-cap` now controls the training game.  Warm
transfer across caps is shape-safe because the observation tensor has always
had a fixed 4,096 entries, including a fixed allocation for ply one-hots; it
does not resize with `max_game_length`.

An initial cap-10 pilot exposed wasted work: compact mode generated the full
over-cap candidate set and discarded unencodable actions afterwards.  Moving
the same neutral arity and typed-slot filters into candidate generation made
the run feasible.  A test compares the accelerated legal set with the old
reference enumerator filtered through the codec at several I.1 and I.2 states;
they are equal.  The discarded pilot is not counted below.

Cold and warm then ran concurrently for equal budgets: two pinned CPU cores
for 22.5 wall minutes, or 45 CPU-minutes per arm.  Settings otherwise match
the cap-6 probe: 32 simulations, two actors per arm, root epsilon 0.25 and
alpha 1.0, no reward shaping.  Evaluation used the same 32 simulations, no
root noise, and a 20-second per-game limit; timeouts count as non-wins.

| cap-10 arm / checkpoint | elapsed checkpoint time | self-play games | cumulative wins | greedy evaluation |
|---|---:|---:|---:|---:|
| cold step 5 | 10.0 min | 35 | 0 | 0/5 (4 timeouts) |
| cold step 10 / final | 17.3 min | 70 | 0 | 0/5 (0 timeouts) |
| warm step 5 | 7.7 min | 35 | 0 | 0/5 (0 timeouts) |
| warm step 16 / final | 21.0 min | 112 | 0 | 0/5 (0 timeouts) |

The corrected experiment therefore still found no I.2 solution: zero wins in
70 cold and 112 warm self-play games, and zero greedy wins at both fixed
points.  Warm transfer increased throughput but not success.  This is a valid
negative result for the stated 45-CPU-minute arms, unlike the cap-6 run, but it
does not establish that I.2 is unlearnable with a larger search or training
budget.

## Probe 6: wide cap-10 I.2 run

The wider run requested 12 actors and 60 wall minutes for each arm, with the
same cap-10 game, 32 simulations, exploration noise and terminal-only reward
as above.  The host's one-minute load was 25.91 after removing orphaned actors,
so the arms ran sequentially.  Although `c07eca4` sped up the engine after the
cold process had imported it, the warm arm ran from an isolated worktree at
`9518a9a`; thus both arms used the same pre-speedup engine.  Each arm was
pinned to 12 CPU cores.  The intended budget was 720 core-minutes per arm.

Cold stopped after 2,569 seconds (42.8 wall minutes, about 514 core-minutes)
when Orbax could not create a checkpoint-writer thread on the heavily loaded
shared host (`RuntimeError: can't start new thread`).  This was an environment
failure, not a game outcome.  Warm reached its 3,600-second timeout (about 720
core-minutes).  The unequal realised budgets mean the rows below measure the
two runs but are not a controlled equal-budget comparison.

| arm / checkpoint | elapsed | self-play games | cumulative wins | greedy evaluation |
|---|---:|---:|---:|---:|
| cold step 155 | midpoint | 1,085 | 0 | 0/5 (0 timeouts) |
| cold step 311 / final | 42.8 min | 2,177 | 0 | 0/5 (0 timeouts) |
| warm step 394 | midpoint | 2,758 | 0 | 0/5 (0 timeouts) |
| warm step 789 / final | 59.9 min | 5,523 | 0 | 0/5 (0 timeouts) |

Neither arm ever won, so there is no first-win game to report.  I.1 trunk
transfer substantially increased throughput, but it did not produce an I.2
solution among 5,523 warm games.  Combined with the earlier arms, this remains
a bounded negative result: packaged AlphaZero learned I.1, but none of these
compact-game runs discovered I.2.  It does not show that I.2 is unreachable or
unlearnable with more search, a different neutral representation, or a larger
budget.

Before this run, six orphan groups from the earlier probes were identified by
open file descriptors into their `spiel/az4/*/log-actor-N.txt` files and
stopped.  The runner now maps SIGTERM to the packaged loop's KeyboardInterrupt
path, which broadcasts exit and joins its workers.  Both the cold error path
and the warm timeout path left no process with an open descriptor into their
output directories, demonstrating cleanup on both exits.
