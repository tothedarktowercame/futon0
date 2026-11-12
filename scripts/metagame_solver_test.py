import gzip
import json
import random

import metagame_eq as eq
import metagame_solver as solver


def test_one_move_goal_is_found_and_fresh_replay_proves_it():
    goal = eq.Goal(eq.inv(eq.E), eq.E)
    for policy in ("uniform", "completing", "merging"):
        successes = []
        for seed in range(20):
            result = solver.solve_attempt("group", goal, (), policy, 4, seed)
            if result.solved:
                successes.append(result)
        assert successes, policy
        result = successes[0]
        p, kept = eq.replay("group", goal, result.moves)
        assert kept == result.moves and eq.entails(p, goal)


def test_budget_and_initial_truth():
    hard = eq.Goal(eq.gen("a"), eq.gen("b"))
    result = solver.solve_attempt("group", hard, ("a", "b"), "uniform", 3, 7)
    assert len(result.moves) <= 3
    initial = solver.solve_attempt("group", eq.Goal(eq.E, eq.E), (),
                                   "uniform", 3, 7)
    assert initial.solved and initial.moves == []


def test_testset_build_is_deterministic(tmp_path):
    root = tmp_path / "eq4" / "uniform"
    root.mkdir(parents=True)
    rows = [
        {"theory": "group", "goal": ["mul(a,e)", "a"],
         "needed_size": 1, "seed": seed, "policy": "uniform"}
        for seed in range(5)
    ]
    with gzip.open(root / "group.jsonl.gz", "wt") as stream:
        for row in rows:
            stream.write(json.dumps(row) + "\n")
    assert solver.build_testset(tmp_path / "eq4", seed=9, per_bin=2) == \
        solver.build_testset(tmp_path / "eq4", seed=9, per_bin=2)
