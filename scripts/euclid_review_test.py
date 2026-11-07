#!/usr/bin/env python3
"""Tests for euclid_review.py (E-euclid-tutor engine review).

Real engine runs, no mocks: every fixture replays through the live
euclid_engine/euclid_search modules loaded by euclid_review.
Run from scripts/: python3 -m pytest euclid_review_test.py -q
"""

import importlib.util
import time
from pathlib import Path

spec = importlib.util.spec_from_file_location(
    "euclid_review", Path(__file__).resolve().parent / "euclid_review.py")
rv = importlib.util.module_from_spec(spec)
spec.loader.exec_module(rv)


def _row(rep, i):
    return rep["steps"][i - 1]


def test_fixture_a_book_i1_all_valid_all_progress():
    rep = rv.review("I.1", rv.fixture_book_i1())
    assert rep["invalid_steps"] == 0
    for i in (1, 2, 3):
        r = _row(rep, i)
        assert r["valid"], (i, r)
        assert r["progress"] == "progress", (i, r)
    # d strictly decreases 3 -> 2 -> 1 -> 0 and the goal holds
    assert [_row(rep, i)["d_after"] for i in (1, 2, 3)] == [2, 1, 0]
    assert rep["goal_held"] is True
    assert rep["remaining_distance"] == 0


def test_fixture_b_useless_step_valid_no_progress():
    rep = rv.review("I.1", rv.fixture_i1_useless_step())
    assert rep["invalid_steps"] == 0
    useless = _row(rep, 2)
    assert useless["rule"].startswith("line_from_points")
    assert useless["valid"] is True
    # the label the code could get wrong: d unchanged
    assert useless["progress"] == "no-progress"
    assert useless["d_before"] == useless["d_after"] == 2
    # the other steps are unchanged relative to fixture (a)
    assert _row(rep, 1)["progress"] == "progress"
    assert _row(rep, 3)["progress"] == "progress"
    assert _row(rep, 4)["progress"] == "progress"
    assert [_row(rep, i)["d_after"] for i in (1, 3, 4)] == [2, 1, 0]
    assert rep["goal_held"] is True
    assert rep["remaining_distance"] == 0


def test_fixture_c_diagram_trusting_step_invalid():
    rep = rv.review("I.1", rv.fixture_i1_diagram_trusting())
    assert rep["invalid_steps"] == 1
    bad = _row(rep, 2)
    assert bad["rule"].startswith("intersection_circles")
    assert bad["valid"] is False
    # review fix (claude-9): ACE does not exist yet, and the reason says so
    assert bad["why"] == "uses ACE before it exists"
    assert bad["progress"] == "invalid"
    assert "without it" in bad["state_note"]
    # later steps are still reviewed against the state WITHOUT the bad step
    for i in (3, 4):
        r = _row(rep, i)
        assert r["valid"] is True, (i, r)
        assert r["progress"] == "progress", (i, r)
    assert rep["goal_held"] is True
    assert rep["remaining_distance"] == 0


def test_fixture_d_i3_prop2_omitted_first_invalid_identified():
    rep = rv.review("I.3", rv.fixture_i3_prop2_omitted())
    assert rep["invalid_steps"] >= 1
    first = _row(rep, 1)
    assert first["rule"].startswith("circle_from_points")
    assert first["valid"] is False
    # the point d was never constructed
    assert first["why"] == "uses d before it exists"
    assert rep["goal_held"] is False
    assert rep["remaining_distance"] == 3  # untouched root state


def test_object_used_before_it_exists():
    """Review fix (claude-9): a step naming a circle nobody constructed is
    reported as using it before it exists, not as a missing premise."""
    rep = rv.review("I.1", [
        ("circle_from_points", ("a", "b"), {"C": "BCD"}),
        ("intersection_circles", ("BCD", "ZZZ"), {"c": "c"}),
    ])
    bad = _row(rep, 2)
    assert bad["valid"] is False
    assert bad["why"] == "uses ZZZ before it exists"
    assert _row(rep, 1)["valid"] is True


def test_diagram_trusting_intersection_of_existing_circles():
    """Review addition (claude-9): both circles exist and meet in a typical
    picture of I.2 (centre b through c; centre a through b), but whether they
    meet depends on the lengths, so the engine must refuse the intersection
    and name intersects_cc."""
    rep = rv.review("I.2", [
        ("circle_from_points", ("b", "c"), {"C": "CGH"}),
        ("circle_from_points", ("a", "b"), {"C": "ABX"}),
        ("intersection_circles", ("CGH", "ABX"), {"c": "p"}),
    ], max_depth=1, node_budget=200)
    assert _row(rep, 1)["valid"] and _row(rep, 2)["valid"]
    bad = _row(rep, 3)
    assert bad["valid"] is False
    assert "intersects_cc" in bad["why"], bad["why"]


# -- an engine failure is not a verdict (review fix, claude-9) -----------------

def _patched_engine(monkeypatch, exc, when):
    """Wrap rv.eng.apply_step so that `when(state, i, rule, args)` triggers
    `exc`; the review's Distance BFS calls apply_step too, so gate narrowly."""
    orig = rv.eng.apply_step

    def fake(state, i, rule, args, bindings):
        if when(rule, args):
            raise exc
        return orig(state, i, rule, args, bindings)

    monkeypatch.setattr(rv.eng, "apply_step", fake)


def test_engine_runtime_error_is_not_invalid(monkeypatch):
    """(a) apply_step raising RuntimeError on the reviewed step: the row is
    valid=None / engine-error (never 'invalid'), n_invalid unchanged, the
    error text is visible, and the report is marked incomplete. Injected
    on the useless line step so the other steps stay reviewable."""
    _patched_engine(
        monkeypatch, RuntimeError("engine bug!"),
        when=lambda rule, args: rule == "line_from_points"
        and args == ("a", "b"))
    steps = rv.fixture_i1_useless_step()
    rep = rv.review("I.1", steps)
    row = _row(rep, 2)
    assert row["valid"] is None
    assert row["progress"] == "engine-error"
    assert "engine bug!" in row["why"]
    assert rep["invalid_steps"] == 0
    assert rep["engine_errors"] == 1
    assert rep["incomplete"] is True
    # the OTHER steps are still reviewed (that is why we emit a row)
    assert _row(rep, 1)["valid"] is True
    assert _row(rep, 3)["valid"] is True


def test_undecided_fm_too_large_is_not_invalid(monkeypatch):
    """(b) eng.FMTooLarge (undecided arithmetic): valid=None /
    progress='undecided', counted in undecided_steps, not invalid_steps."""
    _patched_engine(
        monkeypatch, rv.eng.FMTooLarge(999),
        when=lambda rule, args: rule == "line_from_points"
        and args == ("a", "b"))
    rep = rv.review("I.1", rv.fixture_i1_useless_step())
    row = _row(rep, 2)
    assert row["valid"] is None
    assert row["progress"] == "undecided"
    assert "could not decide" in row["why"]
    assert rep["undecided_steps"] == 1
    assert rep["invalid_steps"] == 0
    assert rep["engine_errors"] == 0


def test_formatter_never_shows_none_as_invalid(monkeypatch):
    """The formatted report says UNDECIDED / ENGINE ERROR and declares the
    review incomplete; it can never print INVALID for valid=None."""
    _patched_engine(
        monkeypatch, RuntimeError("boom"),
        when=lambda rule, args: rule == "line_from_points"
        and args == ("a", "b"))
    text = rv.format_report(rv.review("I.1", rv.fixture_i1_useless_step()))
    assert "ENGINE ERROR" in text
    assert "REVIEW INCOMPLETE" in text
    row2 = [l for l in text.splitlines() if l.strip().startswith("2.")][0]
    assert "INVALID" not in row2


def test_distance_exception_is_not_silent_no_progress(monkeypatch):
    """A crash inside the Distance BFS is recorded in dist.errors and
    skipped; it never converts a distance into a fabricated value, and
    None still reads as 'unknown', not 'no-progress'."""
    _patched_engine(
        monkeypatch, KeyError("bogus"),
        when=lambda rule, args: rule == "line_from_points"
        and args == ("a", "b"))
    rep = rv.review("I.1", rv.fixture_i1_useless_step())
    assert rep["invalid_steps"] == 0
    # review (claude-9): this was under `if rep["distance_move_errors"]:`,
    # which passes vacuously if the crash never fires
    assert rep["distance_move_errors"] > 0
    assert rep["incomplete"] is True


def test_tainted_distance_never_labels_no_progress(monkeypatch):
    """Review (claude-9): a crashed move skipped inside the BFS can only
    make a distance too LARGE, so a finite but inflated d must not produce
    "no-progress" (or "progress"). Fake a search that records a crash and
    returns the same finite d everywhere: untainted, every Book I.1 step
    would read no-progress (3 -> 3)."""
    def fake_search(self, state):
        self.errors.append(("circle_from_points", ("b", "a"), "KeyError"))
        return 3

    monkeypatch.setattr(rv.Distance, "_search", fake_search)
    rep = rv.review("I.1", rv.fixture_book_i1())
    labels = [r["progress"] for r in rep["steps"]]
    assert labels == ["unknown"] * 3, labels
    assert rep["incomplete"] is True


def test_step_after_engine_error_is_not_blamed(monkeypatch):
    """Review (claude-9): the engine crashes on Book I.1 step 2 (circle
    b a, which creates ACE). Step 3 uses ACE; before the fix it read
    INVALID "uses ACE before it exists", blaming the student for the
    engine's failure. It must be blocked (not reviewed), not invalid."""
    _patched_engine(monkeypatch, RuntimeError("planted"),
                    when=lambda rule, args: rule == "circle_from_points"
                    and args == ("b", "a"))
    rep = rv.review("I.1", rv.fixture_book_i1())
    rows = rep["steps"]
    assert rows[1]["progress"] == "engine-error"
    assert rows[2]["valid"] is None and rows[2]["progress"] == "blocked"
    assert rep["invalid_steps"] == 0 and rep["blocked_steps"] == 1
    text = rv.format_report(rep)
    assert "INVALID" not in text and "not reviewed" in text


def test_binding_clash_reads_as_a_name_clash():
    """Review (claude-9, after 6342e2f): proposition_1 a b AB named b is a
    clash; the reason names it in words, not as "('fresh', 'b')"."""
    rep = rv.review("I.1", [("proposition_1", ("a", "b", "AB"), {"d": "b"})])
    row = _row(rep, 1)
    assert row["valid"] is False and row["progress"] == "invalid"
    assert row["why"].startswith("names its new object b, but b already "
                                 "exists"), row["why"]
    assert "fresh" not in row["why"]


# -- I.10 / I.11: review() runs them (P-euclid-review-I10-I11) -----------------

# The Lean-cited steps of the engine replays (trace prop10.json/prop11.json:
# steps whose lean.line is non-null). I.10: 7 of 17 (the rest are
# engine-only: by_contra splits, auxiliary lines, pasch_4, ...). I.11: 8 of
# 11 (engine-only: the three between_symm_parts, which Lean gets by SMT).
I10_LEAN_STEPS = [rv.eng.PROP10_STEPS_DATA[i - 1]
                  for i in (1, 2, 3, 4, 6, 14, 17)]
I11_LEAN_STEPS = [rv.eng.PROP11_STEPS_DATA[i - 1]
                  for i in (3, 5, 6, 7, 8, 9, 10, 11)]

# Small budgets on purpose: the distance BFS cannot find these proofs (most
# cited rules have no candidate enumerator), so every distance must read
# None/"unknown" after a couple of seconds, not after minutes.
I10_I11_BUDGET = dict(max_depth=2, node_budget=300, time_budget=2.0)


def test_i10_full_engine_replay_all_valid():
    """(a) review() of the full I.10 engine replay: every step valid (the
    by_contra tactics included), goal_held True, in well under 2 minutes."""
    import time
    t0 = time.perf_counter()
    rep = rv.review("I.10", rv.eng.PROP10_STEPS_DATA, **I10_I11_BUDGET)
    wall = time.perf_counter() - t0
    assert wall < 120, f"I.10 review took {wall:.1f}s"
    assert rep["invalid_steps"] == 0
    assert rep["undecided_steps"] == 0
    assert rep["engine_errors"] == 0
    assert rep["blocked_steps"] == 0
    for r in rep["steps"]:
        assert r["valid"] is True, r
    assert rep["goal_held"] is True
    assert rep["remaining_distance"] == 0


def test_i11_full_engine_replay_all_valid():
    """(a) review() of the full I.11 engine replay: every step valid,
    goal_held True, in well under 2 minutes."""
    import time
    t0 = time.perf_counter()
    rep = rv.review("I.11", rv.eng.PROP11_STEPS_DATA, **I10_I11_BUDGET)
    wall = time.perf_counter() - t0
    assert wall < 120, f"I.11 review took {wall:.1f}s"
    assert rep["invalid_steps"] == 0
    assert rep["undecided_steps"] == 0
    assert rep["engine_errors"] == 0
    assert rep["blocked_steps"] == 0
    for r in rep["steps"]:
        assert r["valid"] is True, r
    assert rep["goal_held"] is True
    assert rep["remaining_distance"] == 0


def test_i10_lean_cited_steps_step6_stays_invalid():
    """(b) I.10's Lean proof, Lean-cited steps only (P-euclid-review-smt-
    repair): step 6 (intersection_lines CD AB) stays INVALID. Decision:
    its gap (intersects_ll CD AB) is closed in Lean by SMT alone (no case
    split in Prop10.lean), and in the engine by the
    by_contra at replay step 12 plus intersection_lines_opposing — a proof
    tactic with case splits and new lines, NOT a fact-only forward
    derivation, so no repair exists and the firm rule (never valid without
    a found derivation) keeps it invalid with the missing premise.
    "undecided: needs case split" was rejected: it would claim the
    fact-only search space still might close it, which it cannot.
    Step 7 then names d, which step 6 would have constructed."""
    t0 = time.perf_counter()
    rep = rv.review("I.10", I10_LEAN_STEPS, **I10_I11_BUDGET)
    wall = time.perf_counter() - t0
    assert wall < 60, f"I.10 Lean-cited review took {wall:.1f}s"
    assert rep["goal_held"] is False
    assert rep["invalid_steps"] == 2
    bad = _row(rep, 6)
    assert bad["rule"].startswith("intersection_lines")
    assert bad["valid"] is False and bad["progress"] == "invalid"
    assert bad["why"] == "missing premise: ('intersects_ll', 'CD', 'AB')", \
        bad["why"]
    after = _row(rep, 7)
    assert after["valid"] is False and after["progress"] == "invalid"
    assert "uses d" in after["why"] and "before it exists" in after["why"]
    # the steps before it (the Lean-visible prefix) are all valid
    for i in (1, 2, 3, 4, 5):
        assert _row(rep, i)["valid"] is True, (i, _row(rep, i))


def test_i11_lean_cited_steps_valid_via_smt_repair():
    """(b) I.11's Lean proof, Lean-cited steps only (P-euclid-review-smt-
    repair): every step valid, goal_held True. Steps 1-2 are
    valid-needs-smt: Lean gets w != u, w != v and w != g by SMT from the
    between facts; the repair replays the engine-only between_symm_parts
    moves (fact-only, bindings {}) that the Lean proof never cites."""
    t0 = time.perf_counter()
    rep = rv.review("I.11", I11_LEAN_STEPS, **I10_I11_BUDGET)
    wall = time.perf_counter() - t0
    assert wall < 60, f"I.11 Lean-cited review took {wall:.1f}s"
    assert rep["invalid_steps"] == 0
    assert rep["goal_held"] is True
    first = _row(rep, 1)
    assert first["rule"].startswith("point_between_points_shorter_than")
    assert first["valid"] is True
    assert first["progress"] == "valid-needs-smt"
    assert "Lean's SMT supplies dpl w u AB" in first["why"]
    assert "between_symm_parts u w v" in first["why"]
    second = _row(rep, 2)
    assert second["valid"] is True
    assert second["progress"] == "valid-needs-smt"
    assert "between_symm_parts w g u" in second["why"]
    for i in range(3, len(I11_LEAN_STEPS) + 1):
        r = _row(rep, i)
        assert r["valid"] is True, (i, r)
        assert r["progress"] != "valid-needs-smt", (i, r)
    assert rep["smt_repairs"] == 2


# -- repair bad cases (P-euclid-review-smt-repair) ----------------------------

def test_repair_never_repairs_a_false_premise():
    """Bad case (a): the missing premise is FALSE in the state — f is
    stated off AB (hypothesis), so dpl f u AB is not merely absent, it is
    contradicted. No fact-only move can add on_line f AB, so the step
    stays invalid."""
    rep = rv.review("I.11", [
        ("point_between_points_shorter_than", ("AB", "f", "u", "w", "v"),
         {"a": "g"}),
    ], **I10_I11_BUDGET)
    bad = _row(rep, 1)
    assert bad["valid"] is False and bad["progress"] == "invalid"
    assert bad["why"] == "missing premise: ('dpl', 'f', 'u', 'AB')", \
        bad["why"]
    assert rep["smt_repairs"] == 0


def test_repair_never_creates_an_object():
    """Bad case (b): the student drops I.11's proposition_1 (which makes
    the equilateral apex f). Repair adds facts, never objects, and f's
    name is only RESERVED (the h-sm hypothesis not_on_line f AB), so the
    omission surfaces where f's missing facts are needed: proposition_8's
    entailment premise |g-f| = |e-f| fails, then perpendicular_if's angle
    entailment fails. Entailment premises are never repaired (they are
    arithmetic, not facts), and no fact-only move can conjure the
    equilateral's equalities, so both steps stay invalid and the goal is
    not reached."""
    steps = [s for s in I11_LEAN_STEPS if s[0] != "proposition_1"]
    rep = rv.review("I.11", steps, **I10_I11_BUDGET)
    p8 = [r for r in rep["steps"] if r["rule"].startswith("proposition_8")]
    assert len(p8) == 1
    assert p8[0]["valid"] is False and p8[0]["progress"] == "invalid"
    assert "entail" in p8[0]["why"], p8[0]["why"]
    perp = [r for r in rep["steps"]
            if r["rule"].startswith("perpendicular_if")]
    assert len(perp) == 1
    assert perp[0]["valid"] is False
    assert rep["goal_held"] is False
    # the repairable prefix is still repaired (repair is per-step), and
    # nothing past the omitted construction is
    assert rep["smt_repairs"] == 2
    assert _row(rep, 1)["progress"] == "valid-needs-smt"


def test_i12_still_keyerror():
    """(c) I.12 has no prop_searches entry yet: review raises KeyError,
    not a silently empty report."""
    import pytest
    with pytest.raises(KeyError):
        rv.review("I.12", [])


def test_i10_goal_needs_the_last_proposition_4():
    """Review fix (claude-9): goal_prop10 is not vacuous. Without the final
    proposition_4 (which gives |a-d| = |d-b|) every step still applies but
    the goal does not hold."""
    steps = list(rv.eng.PROP10_STEPS_DATA)
    last = max(i for i, s in enumerate(steps) if s[0] == "proposition_4")
    rep = rv.review("I.10", steps[:last] + steps[last + 1:], **I10_I11_BUDGET)
    assert rep["invalid_steps"] == 0
    assert rep["goal_held"] is False


def test_default_review_uses_the_props_time_budget():
    """Review fix (claude-9): review("I.10", steps) with no budget given took
    361 s for three steps; Distance now falls back to the prop's own."""
    assert rv.Distance("I.10").time_budget == 5.0
    assert rv.Distance("I.11").time_budget == 5.0
    assert rv.Distance("I.10", time_budget=1.0).time_budget == 1.0
