# Probe 12 — certified mate scans and funded Euclid I.2 curriculum

Date: 2026-11-11.

## Certified win-in-one path

Dense action enumeration has already certified a move's relational and metric
premises.  The win-in-one evaluator now applies that grounded move through a
separate `apply_certified_step` entry point, which omits only those duplicate
premise checks.  The normal transactional copy, closure, arithmetic consistency
test, fresh-name discipline, provenance, and all public engine behavior remain
unchanged.  The evaluator then calls the goal checker once, rather than using
the public action path's pre-move terminal check followed by `is_terminal` and
`returns`.

The mate cache is keyed before enumeration by the exact facts, arithmetic rows,
fresh counter, declared/reserved names, excluded rules, proof-branch bit,
proposition, and ply.  It therefore skips both enumeration and successor scans
when an actor encounters the same logical position in another episode.

With `PYTHONHASHSEED=0` on one pinned CPU:

| stage-1 check | before | after | speed-up |
|---|---:|---:|---:|
| cold median | 2.492 s | 2.165 s | 1.15× |
| warm median across repeated stage starts | 38.65 ms | 0.0191 ms | 2,023× |

The after measurement used seven independently cold evaluators and twenty warm
new game states.  The old warm-key cost was reconstructed with its exact
enumeration-plus-observation key path over twenty new states.  The first after
cold check was 2.989 seconds; the seven-check mean was 2.246 seconds.

Tests compare the public and certified successor for every legal action at the
545-action clean stage-1 position and at two perturbed starts: facts (including
provenance) and linear-arithmetic rows are identical.  A second seeded test
compares cached and fresh mate sets on twenty random positions.

## Funded DEMO curriculum

This is the **DEMO** arm.  The known eight-move construction chooses prefix
states only and supplies no policy targets.  The Probe 11 protocol was retained:
12 actors, 32 simulations, 15-minute stage caps, a 60-minute total training
budget, half perturbed training starts, `PYTHONHASHSEED=0`, and CPUs 20–31.
Advancement required 8/10 clean and 6/10 fixed-perturbed wins.

| stage | moves remaining | actor games | training wall | clean | perturbed | advance? |
|---|---:|---:|---:|---:|---:|---|
| 1 | 1 | 1,469 | 920.5 s | 10/10 | 10/10 | yes |
| 2 | 2 | 2 | 930.0 s | 0/10 (10 timeouts) | 0/10 (10 timeouts) | no |

Stage 1 reached checkpoint 5.  All twenty evaluation wins were replayed from a
fresh state and accepted by both Spiel's `goal_reached` and `euclid_play`'s
checker.  Its clean evaluation took 18.8 seconds and perturbed evaluation 70.2
seconds.  Legal-action counts were 545 for every clean start and
`545, 550, 961, 545, 550, 545, 971, 545, 545, 1250` for perturbed starts.

Stage 2 successfully used the Resume-1 transport: every actor loaded plain
checkpoint-0 parameters, with no Orbax/TensorStore restore.  Two games finished,
but that was insufficient for a learner update.  Several actors subsequently
encountered OpenSpiel's empty-root failure (`max() iterable argument is empty`).
The main cost was otherwise cold mate scans at distinct MCTS tree states.  Clean
legal-action counts were 532; perturbed counts were
`537, 947, 532, 537, 532, 957, 532, 532, 1235, 672`.

The original sequential perturbed evaluator also hit the empty-root exception.
To preserve the fixed evaluation rather than lose its receipts, the ten fixed
seeds were rerun as isolated one-game processes with an OS limit.  Nine recorded
the in-process timeout and one hit the outer limit; all were non-wins.

## From-start result

The separately budgeted greedy evaluation from the true I.2 start was **0/10**:
all ten games timed out.  Nine recorded their in-process timeout and one reached
the outer process limit.  There were no claimed wins, so fresh replay
verification is vacuous for stage 2 and from-start.

The run directory `spiel/rc12/` contains stage-by-stage data, fixed prefixes,
actor and learner logs, per-game isolated evaluation receipts, retained useful
checkpoints, `stages.json`, and `summary.json`.  All owned training and
evaluation workers were absent at completion.
