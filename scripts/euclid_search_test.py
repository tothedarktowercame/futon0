"""Tests for euclid_search.py — uninformed search over the System E engine.
Run from ~/code/futon0: python3 -m pytest scripts/euclid_search_test.py -q
"""

import importlib.util
from pathlib import Path

_spec = importlib.util.spec_from_file_location(
    "euclid_search", Path(__file__).resolve().parents[1] / "scripts/euclid_search.py")
srch = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(srch)
eng = srch.eng

PROP1_HYPS = [(("on_line", "a", "AB"), "h"),
              (("on_line", "b", "AB"), "h"),
              (("neq", "a", "b"), "h")]


def _root():
    s = eng.State()
    for f, w in PROP1_HYPS:
        s.assume(f, w)
    eng.close(s)
    return s


# (i) the I.1 search finds a proof and that proof replays
def test_prop1_search_finds_and_replays():
    found, path, st = srch.search(PROP1_HYPS, srch.AXIOM_MOVES,
                                  srch.goal_prop1, max_depth=4,
                                  node_budget=5000)
    assert found, "I.1 not found"
    assert len(path) == 3
    rules = [p[0] for p in path]
    assert rules.count("circle_from_points") == 2
    assert "intersection_circles" in rules
    # replay through the engine step path, with store-consistency guard
    s, n = srch.verify_path(PROP1_HYPS, path, srch.goal_prop1)
    assert srch.goal_prop1(s)
    assert st["nodes"] > 0 and st["branch_max"] > 0


# (ii) negative: without circle_from_points, I.1 is not findable
def test_prop1_search_negative_without_circles():
    rules = [r for r in srch.AXIOM_MOVES if r != "circle_from_points"]
    found, path, st = srch.search(PROP1_HYPS, rules, srch.goal_prop1,
                                  max_depth=4, node_budget=500)
    assert not found and path is None
    assert st["nodes"] > 0  # the budget was actually spent searching


# (iii) dedup: isomorphic states merge, states differing in a fact do not
def test_dedup_key_isomorphism_and_separation():
    s1 = srch.clone_state(_root())
    eng.apply_step(s1, 1, "line_from_points", ("a", "b"), {})
    s2 = srch.clone_state(_root())
    eng.apply_step(s2, 1, "line_from_points", ("a", "b"), {"L": "L$7"})
    # same construction, differently named fresh line: isomorphic
    assert srch.state_key(s1) == srch.state_key(s2)
    # a circle vs a line is a different fact set: must not merge
    s3 = srch.clone_state(_root())
    eng.apply_step(s3, 1, "circle_from_points", ("a", "b"), {})
    assert srch.state_key(s1) != srch.state_key(s3)
    # two states differing in exactly one fact
    s4 = srch.clone_state(s3)
    s4.assume(("inside", "b", "C$1"), "extra")
    assert srch.state_key(s3) != srch.state_key(s4)


# regression: line names must never enter the metric rules as points.
# This is the exact bad case found by the I.2 search (bogus depth-5 "proof"
# built on a store that entailed |b-c| = 0): run the same five steps and
# assert the store stays consistent and the vacuous goal fails.
def test_engine_regression_no_line_names_as_points():
    hyps = [(("on_line", "b", "BC"), "h"), (("on_line", "c", "BC"), "h"),
            (("neq", "b", "c"), "h"), (("neq", "a", "b"), "h")]
    path = [
        ("circle_from_points", ("b", "c"), {"C": "C$1"}),
        ("circle_from_points", ("c", "b"), {"C": "C$2"}),
        ("intersection_circle_line_extending_points",
         ("C$1", "BC", "b", "c"), {"g": "g$3"}),
        ("intersection_circle_line_extending_points",
         ("C$2", "BC", "c", "g$3"), {"g": "g$4"}),
        ("intersection_circles", ("C$1", "C$2"), {"c": "c$5"}),
    ]
    s = eng.State()
    for f, w in hyps:
        s.assume(f, w)
    eng.close(s)
    for i, (r, a, b) in enumerate(path, 1):
        eng.apply_step(s, i, r, a, b)
    assert srch.store_consistent(s.la), "store became unsatisfiable"
    assert not s.la.entails({eng.len_atom("b", "c"): 1}, 0, "="), \
        "|b-c| = 0 entailed: line names leaked into metric atoms"
    # the bogus goal |a-p| = |b-c| for arbitrary p must now fail
    assert not srch.goal_prop2(s)
