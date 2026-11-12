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

## 7b. Corrected held-out positions and search coverage

This correction uses the same step-39 trained model and generation-0 control;
there was no retraining. The evaluator reconstructed every completed training
start plus each actor's possible in-flight final start from the recorded actor
seed streams. A held-out `logical_state_key` had to be absent from that strict
superset. It also hashed the non-candidate feature tensors of all 949 training
receipts and required the held-out feature hash to be absent. All selected
positions have one or two actual perturb moves and their keys are mutually
distinct. The table lists SHA-256 of the complete `repr(logical_state_key)`;
the full keys are unwieldy fact/linear-store tuples, while the digest is an
exact identifier under the fixed `PYTHONHASHSEED=0` run.

| seed | logical-state key SHA-256 | perturb moves | candidates | winners |
|---:|---|---:|---:|---:|
| 2026111407 | `5dedc2e487a0d8f9cebd45182d37881ef392cfaa3cdd1471a5639c20b97b2004` | 2 | 679 | 1 |
| 2026111410 | `f7305c74e182a22da13c28dd0aa9ccb70579adf5da68ff7373be43ff405244e9` | 1 | 962 | 1 |
| 2026111411 | `3a52a190fc1a501a6a49775c26519acb38400ce19d3967478ebee222b8bf57f1` | 2 | 1,749 | 1 |
| 2026111412 | `1cb83bdcc8254d8f8f8a013f3a40cf904a72d3736ac7610898016ddc32bce49a` | 2 | 1,350 | 1 |
| 2026111413 | `874f1f7c0bfbce26e4cf9a6ebcda45617961c7370d18c27cf75b1c39eee71693` | 2 | 715 | 1 |
| 2026111414 | `69faef524f24a6e90c2ce3ef18811339a766c67780b687204cd215ef69a13e51` | 2 | 715 | 1 |
| 2026111416 | `921d8d22e1754b700153e7d9f1d38978371710619375a619d5e84ae00a626d9d` | 1 | 1,065 | 1 |
| 2026111419 | `d8db1b11fae9c8cc8efd9187d91763f5402e40e667cc0eada58a64ee0db6e9aa` | 2 | 960 | 1 |
| 2026111423 | `f3d3fc4058508ed8c219faf1a22977a16956e374219cfab6319c57380270c5b0` | 1 | 1,343 | 1 |
| 2026111426 | `aa1ea95534e8a4ef0af82602c6b9aae4391faf1a6295c4c09ea23ea003f2fee5` | 2 | 1,343 | 1 |

The next table reports `overall / rule / within-rule` rank. Rule rank orders
rules by their best candidate logit; within-rule rank compares groundings of
the winning rule. `ablated` means `object_creation_order` was zeroed only at
evaluation. In every row the selected winning rule was
`intersection_circle_line_extending_points`.

| seed | generation 0 | generation 0 ablated | trained | trained ablated |
|---:|---:|---:|---:|---:|
| 2026111407 | 188 / 7 / 6 | 191 / 7 / 9 | **1 / 1 / 1** | **1 / 1 / 1** |
| 2026111410 | 311 / 8 / 8 | 316 / 8 / 13 | **1 / 1 / 1** | **1 / 1 / 1** |
| 2026111411 | 553 / 9 / 6 | 563 / 9 / 10 | **1 / 1 / 1** | **1 / 1 / 1** |
| 2026111412 | 382 / 7 / 7 | 384 / 7 / 9 | **1 / 1 / 1** | **1 / 1 / 1** |
| 2026111413 | 212 / 7 / 6 | 216 / 7 / 10 | **1 / 1 / 1** | **1 / 1 / 1** |
| 2026111414 | 213 / 7 / 7 | 215 / 7 / 9 | **1 / 1 / 1** | **1 / 1 / 1** |
| 2026111416 | 299 / 7 / 6 | 305 / 7 / 10 | 2 / **1** / 2 | **1 / 1 / 1** |
| 2026111419 | 311 / 7 / 9 | 314 / 7 / 12 | **1 / 1 / 1** | 2 / **1** / 2 |
| 2026111423 | 380 / 7 / 9 | 386 / 7 / 11 | **1 / 1 / 1** | **1 / 1 / 1** |
| 2026111426 | 380 / 7 / 9 | 386 / 7 / 11 | **1 / 1 / 1** | **1 / 1 / 1** |

Thus the corrected distinct-position result is **9/10** trained versus
**0/10** generation 0. With creation order zeroed it is also **9/10**, but the
miss changes from seed 2026111416 to 2026111419. The trained winning rule ranks
first on all 10 positions both normally and under ablation; each miss is
competition among groundings of that rule, not failure to select the rule.

Finally, ten repeated stage-2 root searches used 32 simulations with the root
scan off. The premise-certified list contained 532 candidates. Two candidates
were guard-refused when selected, leaving 530 aligned root target entries.

| search | non-zero-visit children | aligned candidates | non-zero visits |
|---:|---:|---:|---|
| 1 | 4 | 530 | 10, 10, 9, 3 |
| 2 | 4 | 530 | 10, 10, 9, 3 |
| 3 | 4 | 530 | 10, 10, 9, 3 |
| 4 | 4 | 530 | 10, 10, 9, 3 |
| 5 | 4 | 530 | 10, 10, 9, 3 |
| 6 | 4 | 530 | 10, 10, 9, 3 |
| 7 | 4 | 530 | 10, 10, 9, 3 |
| 8 | 4 | 530 | 10, 10, 9, 3 |
| 9 | 4 | 530 | 10, 10, 9, 3 |
| 10 | 4 | 530 | 10, 10, 9, 3 |

Only four distinct root children receive visits. The result is deterministic
on this fixed state and shows the expected PUCT concentration. The complete
receipt is `loop-smoke/step2-eval/acceptance-7b.json`; the evaluator is
`scripts/euclid_candidate_step2_eval_7b.py`.
