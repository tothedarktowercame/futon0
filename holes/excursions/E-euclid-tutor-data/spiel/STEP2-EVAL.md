# DESIGN-2 step 2 acceptance: candidate-policy learning

Date: 2026-11-11/12. `PYTHONHASHSEED=0` throughout. This evaluates the
candidate network and loop from D2-1--D2-6; it adds no search feature.

## A. Stage-1 training

The run used the prefix at `rc11/stage-1/prefix.json`, 12 actors, 32
simulations, root win-in-one enabled, `start_perturb=2` at rate 0.5, a fresh
network, and a 1,200-second fixed window. Other parameters were: move cap 3,
buffer 256 positions, batch 64, Adam at 0.001, L2 coefficient 0.0001, PUCT
1.41, Dirichlet epsilon 0.25/alpha 1.0, and CPUs 19--31 (one pinned CPU per
process). The complete configuration is at
`loop-smoke/step2-eval/train-config.json`; the run is at
`loop-smoke/step2-eval/train-run/` (ignored by git).

The base start seeds, one per actor, were:

```
2026111201, 2027111201, 2028111201, 2029111201,
2030111201, 2031111201, 2032111201, 2033111201,
2034111201, 2035111201, 2036111201, 2037111201
```

The actors completed 951 games, all wins, and durably wrote 949 searched
position records. The learner completed 39 updates; peak service usage was
1,570 of 2,048 tasks. Every actor loaded generation 0 and later trained
generations. The first and last losses were:

| measure | step 1 | step 39 | mean steps 1--10 | mean steps 30--39 |
|---|---:|---:|---:|---:|
| total | 7.5980 | 0.8750 | 6.3494 | 0.9289 |
| policy | 6.4158 | 0.7127 | 5.9746 | 0.7639 |
| value | 0.9896 | 0.0000002 | 0.1871 | 0.0000003 |

The loss criterion therefore passed; there was no tuning or restart. Steps
38--39 were unusually slow under the 12 saturated actors, but the last step
completed during orderly shutdown. The trained network below is the plain
model in `train-run/checkpoints/step-000039/model`; the control is
`train-run/snapshots/generation-000000`.

## B. Held-out raw-prior ranking

The held-out seeds are disjoint from the training seeds. Perturbation rate
was 1.0 for this test; nine starts have one extra legal move and seed
2026111305 has two. For each state, the network scored the premise-certified
candidate list once, before search or root scanning. A candidate was labelled
winning only when authoritative guarded admission produced a successor for
which the game's own `goal_reached()` was true.

| held-out seed | extra moves | candidates | winning | generation 0 rank | trained rank |
|---:|---:|---:|---:|---:|---:|
| 2026111303 | 1 | 551 | 1 | 190 | **1** |
| 2026111304 | 1 | 709 | 1 | 212 | **1** |
| 2026111305 | 2 | 1,072 | 1 | 302 | **1** |
| 2026111306 | 1 | 545 | 1 | 187 | **1** |
| 2026111307 | 1 | 821 | 1 | 244 | 2 |
| 2026111310 | 1 | 549 | 1 | 192 | **1** |
| 2026111311 | 1 | 961 | 1 | 310 | **1** |
| 2026111312 | 1 | 818 | 2 | 246 | 2 |
| 2026111313 | 1 | 546 | 1 | 188 | **1** |
| 2026111314 | 1 | 741 | 1 | 236 | **1** |

Result: the trained network ranks a winning move first on **8/10**, meeting
the acceptance threshold. Generation 0 does so on **0/10**. These are policy
priors learned from search visits and game values; the demonstration selected
the prefix but supplied no policy target.

## C. Stage-2 search cost

The trained step-39 model was measured from `rc13/stage-2/prefix.json` at 32
simulations. Each mode used one unmeasured warm-up search followed by ten
searches through the same search object, so JIT compilation is excluded as in
D2-4. Values are mean seconds per move except the maximum column.

| root scan | mean | max | generation | features | model | admission | root scan | other |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| off | **1.437** | 1.557 | 0.768 | 0.263 | 0.048 | 0.114 | 0.000 | 0.238 |
| on | **3.271** | 3.478 | 0.734 | 0.282 | 0.048 | 1.368 | 1.860 | 0.831 |

The D2-4 untrained reference was 1.47 s off and 3.23 s on, so training did not
materially change search cost. The guarded root scan's admission time is also
included in the admission column; the root-scan column is its total envelope,
so those two columns overlap and should not be summed.

The receipts are `loop-smoke/step2-eval/acceptance.json` and
`loop-smoke/step2-eval/costs-corrected.json`. The measurement helper is
`scripts/euclid_candidate_step2_eval.py`. An additional 10-minute stage-2
actor run was not run: the fixed 20-minute training plus authoritative
held-out labelling and the required cost repetitions consumed this packet's
measurement window. D2-6's 1-simulation transport figure remains explicitly
non-comparable.
