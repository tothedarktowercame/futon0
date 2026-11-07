"""Tests for euclid_search.py — uninformed search over the System E engine.
Run from ~/code/futon0: python3 -m pytest scripts/euclid_search_test.py -q
"""

import importlib.util
import pytest
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


# --- I.4 / I.5 (Book/Prop04-05.lean) ----------------------------------------

def test_I4_book_path_verifies():
    """The Book's I.4 steps (superposition + the two [SMT-bridge] asserts)
    pass verify_path for the new prop_searches entry."""
    kw = srch.prop_searches()["I.4"]
    s, n = srch.verify_path(kw["hyps"], eng.PROP4_STEPS_DATA, kw["goal"])
    assert n == 3
    assert kw["goal"](s)


def test_I5_book_path_verifies_on_a_consistent_store():
    """The Book's I.5 steps pass verify_path, and the store stays
    satisfiable after every step. (zai-2's 1b32dd6 pinned an UNSAT store
    here as correct refusal; the cause was an engine bug: the dict literal
    {len(b,c): 1, len(c,b): -1} collapsed to |bc| = 0 at the second
    proposition_4. Fixed with eng.lin(); claude-9.)"""
    kw = srch.prop_searches()["I.5"]
    s = eng.State()
    for f, w in kw["hyps"]:
        eng._assume_metric_or_fact(s, f, w)
    eng.close(s)
    for i, (r, a, b) in enumerate(eng.PROP5_STEPS_DATA, 1):
        eng.apply_step(s, i, r, a, b)
        assert srch.store_consistent(s.la), f"unsat after step {i}"
    assert kw["goal"](s)
    s2, n = srch.verify_path(kw["hyps"], eng.PROP5_STEPS_DATA, kw["goal"])
    assert n == 8


def test_lin_accumulates_identical_atoms():
    """The collision itself: |bc| = |cb| must add no row (0 = 0), not
    |bc| = 0; |bc| < |cb| must make the store unsat, not |bc| > 0."""
    X, Y = eng.len_atom("b", "c"), eng.len_atom("c", "b")
    assert X == Y
    la = eng.LinearArithmetic()
    la.add(eng.lin((X, 1), (Y, -1)), 0, "=")
    assert la.rows == []
    la.add({X: 1}, 0, ">")
    assert eng._fm_sat(la.rows)
    la.add(eng.lin((X, 1), (Y, -1)), 0, "<")
    assert not eng._fm_sat(la.rows)
    assert eng.la_entails_eq(eng.LinearArithmetic(), "b", "c", "c", "b")

def test_uninformed_search_I5_within_budget():
    """Uninformed search is NOT required to find I.5. It must simply run
    honestly under a budget and report its node count (short budget here;
    the 120 s measurement is reported in the requisition answer)."""
    kw = dict(srch.prop_searches()["I.5"])
    kw.update(node_budget=100000, time_budget=20.0)
    found, path, st = srch.search(**kw)
    assert st["nodes"] > 0
    assert found in (True, False)  # whichever; never an error
