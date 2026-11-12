import inspect
import random

import metagame_eq as eq
import metagame_learner as learner
import metagame_setter as setter


def test_features_are_invariant_under_consistent_renaming():
    goal = eq.Goal(eq.mul(eq.gen("a"), eq.gen("b")), eq.mul(eq.gen("b"), eq.gen("a")))
    p = eq.start("group", goal, ("a", "b")); move = eq.candidates(p)[0]
    renamed = eq.Goal(eq.mul(eq.gen("x"), eq.gen("y")), eq.mul(eq.gen("y"), eq.gen("x")))
    q = eq.start("group", renamed, ("x", "y")); rmove = next(m for m in eq.candidates(q) if str(m) == str(move).replace("a", "x").replace("b", "y"))
    counters = {"class_count": 4, "candidate_count": len(eq.candidates(p)), "moves_played": 0, "budget_left": 3}
    assert learner.feature_vector(p, goal, move, counters) == learner.feature_vector(q, renamed, rmove, {**counters, "candidate_count": len(eq.candidates(q))})


def test_testset_exclusion_catches_deliberate_leak():
    a, b = eq.gen("a"), eq.gen("b")
    goal = eq.Goal(eq.mul(a, b), eq.mul(b, a))
    key = ("group", setter.canonical_goal(goal))
    assert learner.is_test_goal("group", goal, {key})
    renamed = eq.Goal(eq.mul(eq.gen("x"), eq.gen("y")), eq.mul(eq.gen("y"), eq.gen("x")))
    assert learner.is_test_goal("group", renamed, {key})  # deliberate leak, renamed.
    assert not learner.is_test_goal("left_group", goal, {key})


def test_learned_attempt_is_budgeted_and_replayable():
    goal = eq.Goal(eq.mul(eq.gen("a"), eq.gen("b")), eq.mul(eq.gen("a"), eq.gen("b")))
    class One:
        def score(self, x): return 1.0 if x[0] else 0.0
    solved, moves = learner.learned_attempt("group", goal, ("a", "b"), One(), 2, 3, greedy=True)
    assert len(moves) <= 20
    fresh = eq.start("group", goal, ("a", "b"))
    for m in moves: eq.apply(fresh, m)
    assert solved == eq.entails(fresh, goal)


def test_feature_signature_cannot_receive_answer():
    assert list(inspect.signature(learner.feature_vector).parameters) == ["position", "goal", "move", "counters"]


# --- added in review by claude-6 ---

def test_completion_features_follow_the_position_after_a_move():
    """The first version cached completing_moves on the position object and
    kept using the start position's map after moves had been applied."""
    a, b = eq.gen("a"), eq.gen("b")
    goal = eq.Goal(eq.inv(eq.mul(a, b)), eq.mul(eq.inv(b), eq.inv(a)))
    p = eq.start("group", goal, ("a", "b"))
    learner.feature_vector(p, goal, eq.candidates(p)[0])  # fills the cache
    rng = random.Random(0)
    for _ in range(8):
        eq.apply(p, setter.choose_move(p, rng, "merging(1)"))
    live = eq.completing_moves(p)
    differing = 0
    for move in eq.candidates(p):
        vector = learner.feature_vector(p, goal, move)
        assert vector[0] == float(live[move].count)
        assert vector[1] == float(live[move].merges)
        differing += live[move].count > 0
    assert differing  # the check is not vacuous: some move completes a rule


def test_sampled_attempt_keeps_budget_on_a_goal_it_cannot_reach():
    goal = eq.Goal(eq.gen("a"), eq.gen("b"))
    class Flat:
        def score(self, x): return 0.0
    solved, moves = learner.learned_attempt("group", goal, ("a", "b"), Flat(), 3, 5)
    assert not solved and len(moves) == 3
