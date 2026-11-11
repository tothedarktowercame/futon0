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
terminal reward alone.  Under the much shorter but equal I.2 comparison it did
not discover a single I.2 solution, and I.1 trunk transfer improved throughput
but did not produce a win.  Therefore this probe is positive for I.1 and
negative/inconclusive for I.2: it shows no evidence that I.2 was learned, but
the 10-minute arms are too small to establish that it cannot be learned.

The result is specifically for the compact action game.  Fifteen palette rules
of arity five through ten are omitted uniformly; none is selected for its
geometric meaning.  That restriction and the expensive exact enumeration on
some policy branches are the principal limits on extrapolating the result to
the complete System E move game.
