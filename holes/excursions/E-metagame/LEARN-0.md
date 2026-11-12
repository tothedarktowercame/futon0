# LEARN-0 — goal-aware move scorer, leave one theory out

## Features

1. `completing_count`
2. `completes_merge`
3. `merge_goal_left`
4. `merge_goal_right`
5. `arg_has_left_class`
6. `arg_has_right_class`
7. `arg_has_goal_subterm_class`
8. `built_matches_rule_side`
9. `built_rule_side_count`
10. `class_count`
11. `candidate_count`
12. `moves_played`
13. `budget_left`

Softmax temperature `0.75`; inputs standardised with the training mean and deviation. Test goals: the first 20 EQ-5 rows of each needed-size bin of the held-out theory; 5 sampled attempts and one greedy attempt per goal, budget 2n. Baseline columns are the EQ-5 figures (20 attempts) for the same goals.

## Held out: group

- training `held_out`: 1564 records, 22081 examples (2464 positive), 0 dropped as not replayable.
- training `in_theory`: 2081 records, 27231 examples (3038 positive), 0 dropped as not replayable.

| needed | goals | uniform | completing | merging | held_out/logistic sampled | held_out/logistic greedy | held_out/mlp sampled | held_out/mlp greedy | in_theory/logistic sampled | in_theory/logistic greedy |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1 | 20 | 11.8% | 36.2% | 36.8% | 100.0% | 100.0% | 100.0% | 100.0% | 100.0% | 100.0% |
| 2 | 20 | 0.5% | 6.0% | 7.2% | 53.0% | 60.0% | 47.0% | 60.0% | 54.0% | 60.0% |
| 3 | 20 | 1.2% | 10.0% | 14.0% | 61.0% | 85.0% | 60.0% | 85.0% | 70.0% | 85.0% |
| 4 | 7 | 0.0% | 12.1% | 22.9% | 97.1% | 71.4% | 57.1% | 57.1% | 91.4% | 100.0% |

Named goals:

- `inverse_product` at 2n: merging 0.0%, completing 5.0%; held_out/logistic 0/5 sampled, greedy failed; held_out/mlp 0/5 sampled, greedy failed; in_theory/logistic 0/5 sampled, greedy failed.
- `inverse_product` at 4n: merging 100.0%, completing 5.0%; held_out/logistic 5/5 sampled, greedy solved; held_out/mlp 0/5 sampled, greedy solved; in_theory/logistic 5/5 sampled, greedy solved.

Logistic weights (feature order as above, then bias):

- `held_out/logistic`: -0.416, 2.897, 0.733, 1.377, 0.075, -0.418, 0.466, -0.006, -0.668, 0.550, -0.185, 0.018, 0.840, -5.202
- `in_theory/logistic`: -0.247, 2.841, 0.709, 1.343, 0.143, -0.281, 0.833, -0.060, -0.655, 0.732, -0.300, 0.034, 0.802, -5.300

## Held out: left_group

- training `held_out`: 1195 records, 13434 examples (1497 positive), 0 dropped as not replayable.
- training `in_theory`: 2081 records, 27231 examples (3038 positive), 0 dropped as not replayable.

| needed | goals | uniform | completing | merging | held_out/logistic sampled | held_out/logistic greedy | held_out/mlp sampled | held_out/mlp greedy | in_theory/logistic sampled | in_theory/logistic greedy |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1 | 20 | 3.5% | 29.5% | 25.2% | 100.0% | 100.0% | 100.0% | 100.0% | 100.0% | 100.0% |
| 2 | 20 | 1.2% | 29.0% | 29.0% | 63.0% | 85.0% | 63.0% | 75.0% | 77.0% | 75.0% |
| 3 | 20 | 2.5% | 38.5% | 40.2% | 77.0% | 90.0% | 55.0% | 85.0% | 80.0% | 90.0% |
| 4 | 20 | 0.5% | 48.2% | 56.5% | 82.0% | 90.0% | 59.0% | 90.0% | 85.0% | 90.0% |
| 5 | 19 | 2.4% | 55.3% | 62.9% | 69.5% | 68.4% | 32.6% | 52.6% | 70.5% | 73.7% |
| 6 | 10 | 0.0% | 10.5% | 8.0% | 0.0% | 0.0% | 10.0% | 0.0% | 4.0% | 0.0% |
| >6 | 5 | 0.0% | 0.0% | 2.0% | 0.0% | 0.0% | 0.0% | 0.0% | 0.0% | 0.0% |

Named goals:

- `left_group_right_inverse` at 2n: merging 20.0%, completing 25.0%; held_out/logistic 0/5 sampled, greedy failed; held_out/mlp 0/5 sampled, greedy solved; in_theory/logistic 1/5 sampled, greedy solved.
- `left_group_right_inverse` at 4n: merging 55.0%, completing 30.0%; held_out/logistic 1/5 sampled, greedy failed; held_out/mlp 0/5 sampled, greedy solved; in_theory/logistic 5/5 sampled, greedy solved.

Logistic weights (feature order as above, then bias):

- `held_out/logistic`: 0.053, 3.072, 0.865, 1.406, 0.161, -0.081, 0.506, 1.064, -0.616, 1.041, -0.507, 0.057, 0.633, -6.185
- `in_theory/logistic`: -0.247, 2.841, 0.709, 1.343, 0.143, -0.281, 0.833, -0.060, -0.655, 0.732, -0.300, 0.034, 0.802, -5.300

## Held out: exponent_two

- training `held_out`: 1403 records, 18947 examples (2115 positive), 0 dropped as not replayable.
- training `in_theory`: 2081 records, 27231 examples (3038 positive), 0 dropped as not replayable.

| needed | goals | uniform | completing | merging | held_out/logistic sampled | held_out/logistic greedy | held_out/mlp sampled | held_out/mlp greedy | in_theory/logistic sampled | in_theory/logistic greedy |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1 | 20 | 10.2% | 23.5% | 26.0% | 98.0% | 100.0% | 100.0% | 100.0% | 100.0% | 100.0% |
| 2 | 20 | 2.0% | 10.0% | 12.2% | 62.0% | 75.0% | 77.0% | 75.0% | 66.0% | 75.0% |
| 3 | 20 | 2.2% | 12.5% | 12.5% | 64.0% | 60.0% | 62.0% | 60.0% | 61.0% | 60.0% |
| 4 | 20 | 3.8% | 22.0% | 18.0% | 80.0% | 80.0% | 56.0% | 75.0% | 74.0% | 80.0% |
| 5 | 20 | 2.8% | 25.8% | 31.0% | 94.0% | 95.0% | 57.0% | 90.0% | 88.0% | 95.0% |
| 6 | 19 | 2.4% | 25.0% | 33.4% | 93.7% | 84.2% | 36.8% | 78.9% | 89.5% | 78.9% |
| >6 | 5 | 0.0% | 18.0% | 51.0% | 100.0% | 60.0% | 8.0% | 100.0% | 96.0% | 60.0% |

Named goals:

- `exponent_two_commutes` at 2n: merging 95.0%, completing 20.0%; held_out/logistic 5/5 sampled, greedy solved; held_out/mlp 0/5 sampled, greedy solved; in_theory/logistic 5/5 sampled, greedy solved.
- `exponent_two_commutes` at 4n: merging 100.0%, completing 85.0%; held_out/logistic 5/5 sampled, greedy solved; held_out/mlp 0/5 sampled, greedy failed; in_theory/logistic 5/5 sampled, greedy solved.

Logistic weights (feature order as above, then bias):

- `held_out/logistic`: -0.296, 2.804, 0.596, 1.369, 0.203, -0.284, 0.543, 0.190, -0.828, 1.161, -0.732, 0.030, 0.923, -5.479
- `in_theory/logistic`: -0.247, 2.841, 0.709, 1.343, 0.143, -0.281, 0.833, -0.060, -0.655, 0.732, -0.300, 0.034, 0.802, -5.300

Wall time: 383s.

## Review (claude-9, taking over from claude-6, 2026-11-12)

The run above is claude-6's revised evaluation (`bg-1794463128029-351`); it
finished at 06:05Z and was not relaunched. Fixes by claude-6 in review (stale
completion cache, per-candidate move-list recomputation, worker pool, budget
test on an unreachable goal) are committed with this note; 6 tests pass.

Cross-theory overlap. Test goals are excluded from training by
(theory, canonical goal), but the same canonical equation can occur as a
training goal in another theory. For the held-out logistic model at 2n
(sampled attempts, named goals excluded):

| held out | goals also trained in another theory | learned | merging | goals in no training set | learned | merging |
|---|---:|---:|---:|---:|---:|---:|
| group | 32 | 83.8% | 25.9% | 35 | 65.1% | 14.0% |
| left_group | 19 | 85.3% | 56.3% | 95 | 64.6% | 34.1% |
| exponent_two | 12 | 95.0% | 34.6% | 112 | 81.2% | 22.1% |

The overlap raises the learned rate, but on goals no theory trained on the
held-out model still solves 2-4x as often as the merging baseline at the same
budget. Transfer to an unseen theory shows on this test.

Not done: replaying solved learned attempts from seeds (the trained models are
not saved; `learned_attempt` returns `eq.entails` on the final position and
asserts the budget).
