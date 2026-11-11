# EuclidZero trials ledger

One row per trial: what was run, on what setup, and what came of it. The
detail lives in the probe files beside this one; this ledger is the index.
Kept by claude-9; add a row whenever a run finishes or is stopped.

Labels: **ZERO** = rules only, no solution shown to the learner in any form.
**DEMO** = one demonstration (the 8-move I.2 construction) used only to choose
start states, never as a policy target. **LLM** = a language-model player
through the notation CLI. **ENGINE** = engine/soundness finding, not a learner.

## Setups

| id | game / encoding | engine | notes |
|---|---|---|---|
| S1 | full legal set, neutral sampler | pre-422d139 (FM float bug) | P1-P2 |
| S2 | compact ids, rules of arity <= 4 (I.1 28,449 ids, I.2 30,753) | pre-422d139 | P3-P7; excludes superposition and every earlier proposition from I.4 on |
| S3 | S2 on fixed engine | 422d139 + 2ef5ad2 guard | contradictory moves refused, goal never true in a contradictory state |
| S4 | point-named moves (cab45a1): lines fixed by two point args are derived, cap 6 free slots; I.2 371,361 ids | fixed | admits 5-6 arg rules; at I.2 late positions ~80% of legal moves are extend_point_longer / point_between_points_shorter_than |
| S5 | S4 + goal witnesses may not be given points (057aa5a) | fixed | matches the play page's checklist |

Learner unless stated: OpenSpiel packaged AlphaZero, NNX MLP width 32 depth 2,
replay buffer 256, batch 64, Dirichlet root noise, non-acting second player.

## Trials

| # | date | label | setup | proposition / cap | budget | result | file |
|---|---|---|---|---|---|---|---|
| P1 | 11-10 | - | S1 | I.1 | - | game registered, legal moves enumerate | PROBE-1.md |
| P2 | 11-10 | - | S1 | I.1 | - | incremental enumeration + neutral sampler | PROBE-2.md |
| P3 | 11-10 | ZERO | S2, hand-written AZ | I.1 cap 6 | - | 0/2480 vs random 21/5449: collapsed (prior lock-in, no root noise) | PROBE-3.md |
| P4 | 11-10 | ZERO | S2, packaged AZ | I.1 cap 6 | - | **I.1 learned**: greedy 0/10 -> 10/10 -> 20/20; re-eval 5/5 | PROBE-4.md |
| P4b | 11-10 | ZERO | S2 | I.2 cap 6 | - | invalid: I.2 needs 8 moves | PROBE-4.md |
| P5 | 11-10 | ZERO | S2 | I.2 cap 10, cold vs warm (I.1 net) | 70 / 112 games | 0 wins both | PROBE-4.md |
| P6 | 11-11 | ZERO | S2, pinned pre-c07eca4 engine | I.2 cap 10, cold vs warm | 12 cores x 1 h each | cold 2,177 games (cut at 42.8 min, host task limit), warm 5,523: 0 wins both | PROBE-4.md (222c5cf) |
| P7 | 11-11 | ZERO/DEMO | S2 | I.2 backward curriculum | 1 actor, 8 sims | random 2/100k (both fake, bug); ZERO from a self-found 5-move win: stage 1 10/10, stage 2 0/10; DEMO stage 1 failed on tiny budget | PROBE-7.md |
| E1 | 11-11 | ENGINE | - | - | - | FM `_exdiv` returned float: fake wins; fixed 422d139; guard 2ef5ad2 | SOUNDNESS-1.md |
| P9 | 11-11 | - | S4 | I.2 random rate | stopped at ~15 min (12 h projected) | no data: workers buffered to the end | PROBE-9.md |
| P10 | 11-11 | DEMO | S4 + perturbed starts (bf304f7) | I.2 stages k=1.. | 12 actors, 32 sims, 15 min, 4 min/stage | stage 1 (one move from solved) 0/10 clean, 0/10 perturbed; 12 games; from-start unmeasured. 545 legal moves, 1 wins; 444 from the two existence rules | PROBE-10.md |
| L1 | 11-11 | LLM | Luna (codex-3) on the play page, with drawing | I.1-I.5 | 3 slices | I.1-I.4 solved; stuck at I.5 (no auxiliary construction) | ~/notes/euclid-cs/luna-run-1.md |
| L2 | 11-11 | LLM | Luna, notation CLI (119629b), S5 | I.1 6, I.2 10, I.3 12, I.4 12, I.5 20 | 3 games each | I.1 solved in 3; I.2 confused by goal name `l` | ~/notes/euclid-cs/luna-notation-1.md |
| L3 | 11-11 | LLM | L2 + existential goal wording (1a70ac6) | same | 3 games each | nothing solved: I.2 construction with a and c swapped (2 of 3 games); stops after 1-2 moves; I.5 opened with Pappus superposition(a,b,c,a,c,b) then stopped | same file, Round 2 |
| P11 | 11-11 | DEMO | S5 + perturbed starts + win-in-one check (98d2b69, flag) | I.2 stages | 12 actors, 32 sims, 60 min, 15 min/stage | win-in-one: untrained stage 1 10/10; cost 2.5 s/evaluation at 545 moves, 0.04 s at the start (49). Stage 1: 1,309 games, 10/10 clean, 10/10 perturbed, advanced. Stage 2: **0 games**: concurrent Orbax checkpoint restores failed or ate the cap (infrastructure, not learning); evals 0/10 all timeouts. From start 0/10, all timeouts | PROBE-11.md (39f1e57) |
| WIN1 | 11-11 | ENGINE | profile of one win-in-one check at the 545-move I.2 stage-1 position | - | - | 2.49 s = 1.87 s applying moves (closure, consistency, linear arithmetic) + 0.57 s goal checks (1,096 calls for 545 moves: a pre-move check repeats) + 0.05 s rest. Enumeration already certifies premises that application re-checks | WIN1-PROFILE.md (f7e662a) |
| RESUME-1 | 11-11 | ENGINE | carried-network resume: learner restores Orbax once, exports numpy params, actors memory-map them (7ae0906) | rc11 stage-2 checkpoint | 12 actors, 2 sims | before: no actor finished a game in 900 s; after: all 12 finished game 1 in 9-10 s, 0 restore errors | RESUME-1.md |
| P12 | 11-11 | DEMO | S5 + perturbed starts + win-in-one with certified apply and exact per-actor cache (d1fbed5) + numpy resume | I.2 stages | 12 actors, 32 sims, 60 min, 15 min/stage | check cost cold 2.49 -> 2.17 s; repeated state 38.7 ms -> 0.02 ms. Stage 1: 1,469 games, 10/10, 10/10, advanced. Stage 2: **2 games** in 15 min: every new tree node pays a cold 2.2 s scan; several actors died on OpenSpiel's empty root (`max() iterable argument is empty`: a non-terminal state with no legal moves). From start 0/10, all timeouts | PROBE-12.md (db5ece2) |
| P13 | 11-11 | DEMO | P12 + win-in-one at the root only (b69d090) + episode cap kept on clone (546e7af; the cause of the empty roots) | I.2 stages | 12 actors, 32 sims, 60 min, 15 min/stage | Stage 1: 2,043 games, 10/10, 10/10, advanced. Stage 2: 27 games, 0/10, 0/10, not advanced: **6 of 12 actors could not start** (background scope at 1,947/2,048 tasks; JAX threads), and a stage-2 move costs 22.9 s (532 legal moves). From start 0/10, no timeouts | PROBE-13.md (0f4a7b0) |
| THREADS-1 | 11-11 | ENGINE | per-process JAX/BLAS thread cap (b4112d4, 1b77a5c); limit is TasksMax on futon3c-zone.service | rc13 stage-2 prefix, one-ply episodes | 12 actors x 5 min | actor 67 -> 8 tasks; all 12 alive, 89-93 games each; peak 1,206/2,048. Stage-2 move 20.6 s: enumeration 51% (84 calls), network 19%, root mate scan 14%, MCTS other 13%, apply 4%. Found: enumeration can list a superposition move the contradiction guard then refuses | THREADS-1.md (d057f31) |
| LEGAL-1 | 11-11 | ENGINE | enumeration vs contradiction guard (b5c93b8, codex-21) | rc13 stage-2 prefix + off_line_strict_triangle + name_point(d_2) -> x_2 = d_2 | - | enumeration listed superposition(a,d_2,x_2,...) on a degenerate triangle; guard refuses (store-unsat, 204 rows). Fix: enumeration applies each premise-certified candidate through the guard. Correct, but enumeration 36 -> 1,122 ms (31x) at 532 moves; the search should use unvalidated candidates + guarded single-move checks (asked of codex-26) | spiel tests 24, engine 209 |
| STEP1 | 11-11 | ENGINE | DESIGN-1 step 1: game view quotients peq, earliest name canonical, alias-only moves omitted (a185a6b) | rc13 stage-2 prefix; 10 P13 perturbed starts; 30 random I.2 | - | aliases at curriculum positions: **0** (532 -> 527 moves; 6,007 -> 5,955); in random play 7 aliases, 4,554 -> 3,571 moves (937 alias moves gone), enumeration 189 -> 117 ms. 60 receipt rows + witness still win; LEGAL-1 cannot arise. So aliases inflate random play, not the curriculum: there the two existence rules dominate | STEP1.md |
| SMT-1 | 11-11 | ENGINE | cvc5 + LeanEuclid's 112 SMT axioms (EuclidTheory.lean), givens + move facts only | logged positions | 5 s per query | I.2 witness: 8-move `follows` in 0.12 s, prefixes 0-7 `unknown`; Luna I.1 follows; Luna I.2/I.3/I.5 finals `unknown`; **Luna's four I.4 games (one move: superposition) `follows` in ~4 s where our engine says unsolved**: the engine needs two more deduction moves (assert_point_eq_on_ray, assert_point_eq_angle, euclid_engine.py:3404), LeanEuclid's proof ends superposition + euclid_finish. cvc5 never answered `sat`, so it confirms wins but cannot refute | SMT-1.md (8686325) |

## Standing decisions (Joe)

- 11-11: no random baseline needed ("like a medical trial").
- 11-11: do not leave moves out of the game (the two existence rules stay).
- 11-11: add a mate-in-one (win-in-one) check; it uses only the game's reward.
- 11-11: stage 1 failing one move from solved means it needs more time.
- 11-11: cvc5 "does the goal follow" test, possibly paired with Luna.
- Later: coherent-logic provers (ArgoCLP-style) as a constructive partner;
  Vampire/E; Lean's Duper/Aesop over LeanEuclid statements.

## In flight

(updated as packets go out)

- P11 (codex-26, invoke-1794412025376-17332-452bc781): win-in-one check in the
  search (flag), then DEMO curriculum S5 + perturbed starts, 12 actors, 32 sims,
  60 min, 15 min/stage, separate 20-min from-start eval. Packet
  ~/notes/packets/P-euclid-spiel-11.md.
- SMT-1 (codex-25, invoke-1794412026677-17333-c2d3c913): cvc5 with LeanEuclid's
  System E SMT axioms on logged positions (I.2 witness prefixes, Luna finals) vs
  our engine's verdict. Packet ~/notes/packets/P-euclid-cvc5-1.md.
- P14 (codex-26, invoke-1794417126071-17416-dace9f0c): win-in-one speed (no premise re-check, single goal check, per-actor cache) then the 60-min DEMO rerun -> PROBE-12.md, rc12/.
