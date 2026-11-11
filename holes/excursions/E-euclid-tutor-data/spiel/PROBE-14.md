# Probe 14: policy-ordered progressive widening

Date: 2026-11-11.  Proposition: Euclid I.2.  This is the DEMO arm: the
eight-move demonstration chooses curriculum start states, and is never used
as a policy target.  Every process used `PYTHONHASHSEED=0`.

## Search and legality

`euclid_widening.py` ranks the whole dense policy head without asking the
game for a legal mask.  At a non-root node with `N` visits it admits
`ceil(N^0.5)` legal children, checking action ids one at a time in prior
order and skipping illegal ids.  Priors are renormalised over admitted
children.  The root retains the complete game enumeration, root-only
win-in-one check, and Dirichlet noise during training.  Packaged AlphaZero's
learner and visit-count policy targets are unchanged.

The one-id check decodes the compact action and applies it to an isolated
engine-state copy.  Thus the construction premises, arithmetic entailments,
closure, and contradiction guard—not the network—decide legality.  This was
tested after P17 (`b5c93b8`, upstream `2b1c407`) made the enumerator's
transactional consistency check authoritative:

| agreement set | result |
|---|---:|
| 20 seeded random I.2 positions, every enumerated action | exact |
| 500 seeded random action ids at each position (10,000 ids) | exact |
| forced `k=all` at the I.2 start | admitted set equals enumerated legal set |
| 20 recorded rc11 stage-1 wins | winning move chosen in every case |

The last check uses an untrained flat model.  Root win-in-one therefore
produces `20/20` only from the game's terminal reward, not from learned or
demonstration policy information.

## Stage-2 move cost

Fixed state: rc13 stage-2 clean prefix, checkpoint 1, 32 simulations, root
win-in-one mode.  Times are one wall-clock move.  After P17, packaged search
did not complete within 300 seconds; widening completed in 8.593 seconds, a
measured lower-bound speed-up of **34.9x**.

| phase, post-P17 | widening |
|---|---:|
| root complete enumeration + mate scan | 3.718 s |
| descendant one-id legality, 25,746 calls | 2.666 s |
| network inference, 35 calls | 0.488 s |
| cloning | 0.002 s |
| action application | 0.374 s |
| other search work | 1.337 s |
| total | **8.593 s** |

Of the descendant ids considered, 25,710 were illegal and 36 legal: an
illegal share of **99.86%**.  There were zero complete descendant legal-set
enumerations.  For orientation, immediately before P17 the same profiler
measured packaged search at 22.048 s (84 enumerations, 11.456 s; inference
3.946 s; clone/application 0.913 s; root scan 2.814 s; other 2.919 s) and
widening at 7.442 s.  P17 correctly made each packaged enumeration certify
every candidate, which is why the authoritative packaged run now exceeds
300 s.

## DEMO curriculum

Protocol: 12 actors, 32 simulations, `--search widening`, root win-in-one,
thread pool CPUs 20-31, 15 minutes per stage, 60-minute total training
budget, clean threshold `8/10`, perturbed threshold `6/10`.  Training starts
used `start_perturb=2` for half the games.  Evaluation used the fixed clean
start and ten fixed perturbed starts.  Every counted win was replayed in a
fresh Spiel state and through `euclid_play`'s referee.

| stage | moves remaining | games | train wall | checkpoint | clean | perturbed | advanced? |
|---:|---:|---:|---:|---:|---:|---:|:---:|
| 1 | 1 | 1,358 | 930.10 s | 21 | 10/10 | 10/10 | yes |
| 2 | 2 | 24 | 930.07 s | 1 | 0/10 (10 timeouts) | 0/10 (10 timeouts) | no |

Stage 1 legal counts were 545 clean and 545-1,250 perturbed.  Stage 2 legal
counts were 532 clean and 532-1,235 perturbed.  Stage 2 completed one learner
update (65 states from 14 games); all 12 actors remained alive, unlike Probe
13's thread-exhausted cohort.  Its first prior-ranked descendant scans were
nevertheless slow enough that the stage produced only 24 games and no wins.

The required greedy evaluation from the true I.2 start used stage-2
checkpoint 1: **0/10**, ten timeouts, zero search failures, zero verified
wins (302.67 s).

## Gates and artifacts

| gate | result |
|---|---:|
| `euclid_widening_test.py` | 3 passed in 56.86 s |
| `euclid_spiel_test.py` | 24 passed in 577.25 s |
| `euclid_play_test.py` | 99 passed in 178.52 s |

Run artifacts are in `spiel/rc14/`.  They are pruned to the last useful
checkpoint per stage: stage 1 checkpoint 21 and stage 2 checkpoint 1 (plus
its plain actor-weight snapshot).  No rc14 worker survived the driver.

Code commits: `d3d82de` (pager, integration, tests), `9e75e90` (profile
receipt fix), and `a8afce3` (bounded per-search certification cache).  P17
was integrated as `b5c93b8`.
