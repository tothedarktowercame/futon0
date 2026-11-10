"""Tests for euclid_play.py (E-kimi-task-413, P-euclid-play-backend).

Run from scripts/: ~/code/futon6/.venv/bin/python -m pytest -q euclid_play_test.py
"""

import copy
import math
import pytest
import re
import json
import os
import signal
import subprocess
import sys
import time
import urllib.request

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import euclid_engine as eng
import euclid_trace as tr
import euclid_play as ep

PORT = 8123
SCRIPT = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                      "euclid_play.py")


def _move(rule, args, bindings):
    return {"rule": rule, "args": list(args), "bindings": dict(bindings)}


def _engine_moves(n):
    return [_move(r, a, b) for r, a, b in ep.reference_steps(n)]


def _straight_line(n):
    return all(r not in ("by_contra", "by_cases")
               for r, _a, _b in ep.reference_steps(n))


def _engine_step_to_move(st):
    """An engine step tuple as a play() move dict; case moves become
    {rule, branches, conclusion, open: False}."""
    if st[0] in ("by_contra", "by_cases"):
        return {"rule": st[0],
                "branches": [{"hyps": [list(f) for f in h],
                              "moves": [_engine_step_to_move(x)
                                        for x in steps]}
                             for h, steps in st[1]],
                "conclusion": [list(c) for c in st[2]],
                "open": False}
    return _move(st[0], st[1], st[2])


def test_case_props_reach_goal():
    """I.6, 7, 10, 13, 14, 19, 21 played with the engine's own steps (case
    moves as closed blocks) reach the goal."""
    reached = []
    for n in (6, 7, 8, 10, 13, 14, 19, 21, 22, 23):
        steps = ep.reference_steps(n)
        res = ep.play(n, [_engine_step_to_move(st) for st in steps])
        assert res["goal_reached"], \
            "I.%d: engine replay did not reach the goal: %s" % (n, res)
        assert all(m["ok"] for m in res["moves"]), \
            "I.%d: a move failed: %s" % (n, res["moves"])
        reached.append(n)
    print("\ncase props reached:", reached)


def test_open_block_reports_branch():
    """I.21 with its by_contra's last branch open and its moves missing
    reports branch_contradictory false; with them, true."""
    steps = [_engine_step_to_move(st) for st in tr.hyps_and_steps(21)[1]]
    bc = next(m for m in steps if m["rule"] == "by_contra")
    pre = steps[:steps.index(bc)]
    # last branch open, its moves missing
    open_bc = json.loads(json.dumps(bc))
    open_bc["open"] = True
    open_bc["branches"][-1]["moves"] = []
    res = ep.play(21, pre + [open_bc])
    ent = res["moves"][-1]
    assert ent["ok"] and ent["open"]
    assert ent["branches"][0]["closed"] is True
    last = ent["branches"][-1]
    assert last["closed"] is False
    assert last["branch_contradictory"] is False
    assert last["branch_goal_reached"] is False
    assert not res["goal_reached"]
    # with the branch's moves, the branch contradicts
    open_bc2 = json.loads(json.dumps(bc))
    open_bc2["open"] = True
    res = ep.play(21, pre + [open_bc2])
    ent = res["moves"][-1]
    assert ent["ok"]
    assert ent["branches"][-1]["branch_contradictory"] is True
    assert not res["goal_reached"]  # block still open: no conclusion yet


def test_closed_by_contra_without_contradiction_refused():
    """A closed by_contra whose split IS exhaustive but whose branch does
    not contradict is refused with the engine's own wording. (A split
    with a case dropped is now refused earlier, as "missing case" — see
    test_missing_case_refusals.)"""
    mv = {"rule": "by_contra",
          "branches": [{"hyps": [["between", "b", "e", "d"]],
                        "moves": []},  # covered, but does not contradict
                       {"hyps": [["between", "e", "b", "d"]],
                        "moves": []}],
          "conclusion": [["between", "b", "d", "e"]],
          "open": False}
    # after the engine's own construction, so b, d, e are on BD (the
    # between trichotomy needs a common line; at the start it does not
    # apply — see test_between_split_needs_a_line)
    steps = [_engine_step_to_move(st) for st in tr.hyps_and_steps(21)[1]]
    bc = next(m for m in steps if m["rule"] == "by_contra")
    pre = steps[:steps.index(bc)]
    res = ep.play(21, pre + [mv])
    ent = res["moves"][-1]
    assert not ent["ok"]
    assert "by_contra branch" in ent["error"]
    assert "did not reach a contradiction" in ent["error"]
    assert not res["goal_reached"]


def test_open_block_earlier_branch_must_close():
    """An open by_contra whose first branch does not close is an error,
    not an ok open block."""
    steps = [_engine_step_to_move(st) for st in tr.hyps_and_steps(21)[1]]
    bc = next(m for m in steps if m["rule"] == "by_contra")
    pre = steps[:steps.index(bc)]
    bad = json.loads(json.dumps(bc))
    bad["open"] = True
    bad["branches"][0]["moves"] = []  # first branch no longer closes
    res = ep.play(21, pre + [bad])
    ent = res["moves"][-1]
    assert not ent["ok"]
    assert "earlier case" in ent["error"]


def _walk_case_blocks(n, s, steps, refuted, found):
    """Replay engine step tuples on s, collecting (rule, move-dict,
    state-copy, refuted) for every case block at the state where it
    stands, nested blocks included (mirrors _coverage_check)."""
    for i, st in enumerate(steps, 1):
        if st[0] in ("by_contra", "by_cases"):
            found.append((st[0], _engine_step_to_move(st), s.copy(),
                          tuple(refuted)))
            for hyps, bsteps in st[1]:
                sub = s.copy()
                for f in hyps:
                    eng._assume_metric_or_fact(sub, f, st[0])
                eng.close(sub)
                sub_ref = tuple(refuted)
                if st[0] == "by_cases" and not hyps:
                    sub_ref += tuple(h for h, _s in st[1] if h)
                _walk_case_blocks(n, sub, bsteps, sub_ref, found)
            for c in st[2]:
                if c[0] != "false":
                    eng._assume_metric_or_fact(s, c, "walk")
            eng.close(s)
        else:
            eng.apply_step(s, i, st[0], st[1], st[2])
            eng.close(s)


def test_catalogue_splits_covered():
    """Every engine case split in the catalogue (I.10, 13, 14, 19, 21,
    nested blocks included) is covered; record the family each matched."""
    table = []
    for n in (6, 7, 8, 10, 13, 14, 19, 21, 22, 23):
        s = ep._start_state(n)
        found = []
        _walk_case_blocks(n, s, ep.reference_steps(n), (), found)
        for rule, mv, bs, refuted in found:
            cov = ep.coverage(
                bs, rule,
                [[tuple(f) for f in b["hyps"]] for b in mv["branches"]],
                [tuple(c) for c in mv["conclusion"]], refuted)
            assert cov["covered"], (n, rule, mv["branches"], cov)
            table.append((n, rule, cov["family"]))
        # and the engine's steps still reach Solved under the referee
        res = ep.play(n, [_engine_step_to_move(st)
                          for st in ep.reference_steps(n)])
        assert res["goal_reached"], n
    print("\ncoverage table:", *["  I.%d %s -> %s" % t for t in table],
          sep="\n")


def test_missing_case_refusals():
    """I.21 with either refuting case dropped is refused, naming exactly
    the dropped between fact; I.19 with one case dropped names the
    missing comparison."""
    steps21 = [_engine_step_to_move(st)
               for st in tr.hyps_and_steps(21)[1]]
    bc = next(m for m in steps21 if m["rule"] == "by_contra")
    pre = steps21[:steps21.index(bc)]
    for drop, missing in ((0, ["between", "b", "e", "d"]),
                          (1, ["between", "d", "b", "e"])):
        bad = json.loads(json.dumps(bc))
        del bad["branches"][drop]
        res = ep.play(21, pre + [bad])
        ent = res["moves"][-1]
        assert not ent["ok"] and ent["error"] == "missing case"
        assert ent["missing_case"]["missing"] == [missing], ent
        assert ent["missing_case"]["family"] is not None
        assert not res["goal_reached"]
    # I.19: drop the |a-b| > |a-c| case
    steps19 = [_engine_step_to_move(st)
               for st in tr.hyps_and_steps(19)[1]]
    bad19 = json.loads(json.dumps(steps19[0]))
    del bad19["branches"][1]
    res = ep.play(19, [bad19])
    ent = res["moves"][-1]
    assert not ent["ok"] and ent["error"] == "missing case"
    assert ent["missing_case"]["missing"] == [["gt_len", "a", "b", "a", "c"]]
    assert not res["goal_reached"]


def test_coverage_family_edge_cases():
    """A by_cases on same_side d g EF alone misses the opposing and
    on_line members; a split of facts outside every family -> None."""
    s = eng.State()
    eng.close(s)
    cov = ep.coverage(s, "by_cases", [[("same_side", "d", "g", "EF")]],
                      [])
    assert not cov["covered"]
    assert cov["family"] is not None
    # the referee reports the family with the fewest missing members:
    # since b3ef275's follow-up that is "same side or not" (P or not P),
    # one member, rather than the four-way sides family's three
    assert [("not_same_side", "d", "g", "EF")] == \
        [tuple(m) for m in cov["missing"]]
    cov2 = ep.coverage(s, "by_cases", [[("dpl", "a", "b", "L")]],
                       [("on_line", "x", "M")])
    assert not cov2["covered"]
    assert cov2["family"] is None and cov2["missing"] == []


def test_engine_steps_reach_goal():
    """For every straight-line catalogue prop, replaying the engine's own
    steps reaches the goal."""
    reached, skipped = [], []
    for entry in ep.catalogue():
        n = entry["n"]
        if not _straight_line(n) or n in ep.AWAITING_SM:
            skipped.append(n)
            continue
        res = ep.play(n, _engine_moves(n))
        assert res["goal_reached"], \
            "I.%d: engine replay did not reach the goal: %s" % (n, res)
        assert all(m["ok"] for m in res["moves"])
        reached.append(n)
    print("\nreached:", reached, "skipped (not straight-line):", skipped)


def test_all_but_last_step_does_not_reach_goal():
    for entry in ep.catalogue():
        n = entry["n"]
        if not _straight_line(n) or n in ep.AWAITING_SM:
            continue
        moves = _engine_moves(n)[:-1]
        res = ep.play(n, moves)
        assert not res["goal_reached"], \
            "I.%d: goal already reached without the last step" % n


def test_palette_5():
    pal = ep.palette(5)
    assert "proposition_4" in pal
    assert "proposition_5" not in pal
    assert "proposition_5'" not in pal
    assert "proposition_6" not in pal
    assert "proposition_10" not in pal
    assert "line_from_points" in pal  # a construction, always available


def test_catalogue_lists_6_and_7():
    """I.6 and I.7 join the catalogue between 5 and 10 (packet
    P-euclid-play-config-I6-I7)."""
    ns = [e["n"] for e in ep.catalogue()]
    assert ns.index(5) < ns.index(6) < ns.index(7) < ns.index(10)
    assert ep.GOALS[6] == [("_eq_len", "a", "b", "a", "c")]
    assert ep.GOALS[7] == [("false",)]


def test_future_proposition_refused():
    res = ep.play(5, [_move("proposition_6", (), {})])
    assert res["moves"][0]["error"] == "I.6 is not proved yet"
    assert not res["moves"][0]["ok"]
    assert not res["goal_reached"]


def test_missing_premise_returns_the_fact():
    # I.1: intersection_circles of BCD with itself is not shown to
    # intersect, so it fails with the missing premise as the error. (A
    # circle that does not exist yet is now refused earlier, by name.)
    steps = tr.hyps_and_steps(1)[1]
    res = ep.play(1, [_move(*steps[0]),
                      _move("intersection_circles", ("BCD", "BCD"),
                            {"c": "c"})])
    assert len(res["moves"]) == 2
    assert res["moves"][0]["ok"]
    last = res["moves"][-1]
    assert not last["ok"]
    assert isinstance(last["error"], list)
    assert last["error"] == ["intersects_cc", "BCD", "BCD"]
    assert not res["goal_reached"]


def test_rule_info():
    # constructions: args and the base names the make introduces
    li = ep.rule_info("line_from_points")
    assert li["args"] == ["a", "b"]
    assert li["binds"] == ["L"]
    assert li["cite"] == "LinesAndCircles.lean:3"
    ci = ep.rule_info("circle_from_points")
    assert ci["args"] == ["a", "b"]
    assert ci["binds"] == ["C"]
    ii = ep.rule_info("intersection_circles")
    assert ii["args"] == ["alpha", "beta"]
    assert ii["binds"] == ["c"]
    # a proposition: proposition_1 binds d
    pi = ep.rule_info("proposition_1")
    assert pi["args"] == ["a", "b", "AB"]
    assert pi["binds"] == ["d"]
    assert pi["cite"] == "Book/Prop01.lean:5"


def test_catalogue_palette_carries_rule_info():
    cat = ep.catalogue()
    p1 = next(e for e in cat if e["n"] == 1)
    assert all(set(e) == {"name", "args", "cite", "binds", "makes_nothing", "label",
                          "group", "arg_hints", "needs", "gives",
                          "arg_kinds"}
               for e in p1["palette"])
    circ = next(e for e in p1["palette"]
                if e["name"] == "circle_from_points")
    assert circ["needs"] == ["[a] ≠ [b]"]   # the rule in words
    names = [e["name"] for e in p1["palette"]]
    assert names == ep.palette(1)  # constructions first, then propositions
    lfp = next(e for e in p1["palette"] if e["name"] == "line_from_points")
    assert lfp["binds"] == ["L"]
    # nothing citable from the future: no proposition entries at I.1
    assert not any(n.startswith("proposition_") for n in names)
    p5 = next(e for e in cat if e["n"] == 5)
    p4 = next(e for e in p5["palette"] if e["name"] == "proposition_4")
    assert p4["cite"].startswith("Book/Prop04")


def _server_pid(port):
    out = subprocess.check_output(["ss", "-ltnp"]).decode()
    for line in out.splitlines():
        if ":%d " % port in line:
            pid = line.split("pid=")[1].split(",")[0]
            return int(pid)
    return None


def test_http_round_trip():
    proc = subprocess.Popen([sys.executable, SCRIPT, "--port", str(PORT)],
                            stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT)
    try:
        base = "http://127.0.0.1:%d" % PORT
        for _ in range(100):
            try:
                with urllib.request.urlopen(base + "/api/catalogue",
                                            timeout=2) as r:
                    cat = json.loads(r.read())
                break
            except Exception:
                time.sleep(0.2)
        else:
            raise AssertionError("server did not come up on %d" % PORT)
        assert [e["n"] for e in cat] == sorted(tr.FREE)
        assert cat[0]["title"].startswith("I.1")
        assert cat[0]["statement"].startswith("theorem proposition_1")
        moves = _engine_moves(1)
        req = urllib.request.Request(
            base + "/api/play",
            data=json.dumps({"n": 1, "moves": moves}).encode(),
            headers={"Content-Type": "application/json"})
        with urllib.request.urlopen(req, timeout=30) as r:
            res = json.loads(r.read())
        assert res["goal_reached"] is True
        assert all(m["ok"] for m in res["moves"])
        assert res["contradictory"] is False
    finally:
        pid = _server_pid(PORT)
        if pid is not None:
            os.kill(pid, signal.SIGKILL)
        proc.kill()
        proc.wait()
        assert _server_pid(PORT) is None, "server on %d not killed" % PORT


def test_case_block_with_no_cases_is_refused():
    # Review (claude-9): the engine leaves exhaustiveness of a split to the
    # caller, so a by_contra with ZERO branches checked nothing and asserted
    # its conclusion outright: I.21 reached Solved with its by_contra
    # emptied. play() refuses an empty block, also nested inside a branch.
    moves = [_engine_step_to_move(st) for st in tr.hyps_and_steps(21)[1]]
    i = next(k for k, m in enumerate(moves) if m["rule"] == "by_contra")
    empty = copy.deepcopy(moves)
    empty[i]["branches"] = []
    r = ep.play(21, empty)
    assert r["moves"][i]["ok"] is False
    assert "needs at least one case" in r["moves"][i]["error"]
    assert r["goal_reached"] is False
    nested = copy.deepcopy(moves)
    nested[i]["branches"][0]["moves"].append(
        {"rule": "by_cases", "branches": [], "conclusion": [["false"]],
         "open": False})
    r = ep.play(21, nested)
    assert r["moves"][i]["ok"] is False
    assert r["goal_reached"] is False


def test_crossed_split_missing_case_is_the_pair():
    # I.7's split is a vs CD crossed with d vs AC (four cases). Review
    # (claude-9): with 4b7a2ea its cases are exactly the split facts and
    # it is covered as a crossed family; dropping one case reports the
    # missing CONJUNCTION, which the page opens as one two-fact case.
    moves = [_engine_step_to_move(st) for st in tr.hyps_and_steps(7)[1]]
    i = next(k for k, m in enumerate(moves) if m["rule"] == "by_contra")
    x = copy.deepcopy(moves)
    dropped = x[i]["branches"].pop(1)["hyps"]
    r = ep.play(7, x)
    assert r["moves"][i]["error"] == "missing case"
    (pair,) = r["moves"][i]["missing_case"]["missing"]
    norm = lambda f: (f[0], *sorted(f[1:3]), f[3])
    assert sorted(map(norm, pair)) == sorted(map(norm, dropped))
    x[i]["branches"][0]["hyps"].append(["same_side", "c", "a", "DB"])
    x[i]["branches"].insert(1, {"hyps": dropped, "moves":
                                moves[i]["branches"][1]["moves"]})
    r = ep.play(7, x)
    assert r["moves"][i]["error"] == "missing case"  # the old extra fact


def test_case_with_extra_assumption_does_not_cover():
    # Review (claude-9): {between b e d, X} must not count as covering
    # "between b e d" -- it refutes only the part where X also holds.
    moves = [_engine_step_to_move(st) for st in tr.hyps_and_steps(21)[1]]
    i = next(k for k, m in enumerate(moves) if m["rule"] == "by_contra")
    x = copy.deepcopy(moves)
    x[i]["branches"][0]["hyps"].append(["opposing_sides", "a", "c", "AB"])
    r = ep.play(21, x)
    assert r["moves"][i]["ok"] is False
    assert r["moves"][i]["error"] == "missing case"
    assert r["goal_reached"] is False


def test_palette_7():
    """With CONSTRUCTIONS[\"proposition_6\"] (E-kimi-task-420), the I.7
    student palette includes the I.6 lemma; later propositions stay out."""
    pal = ep.palette(7)
    assert "proposition_6" in pal
    assert "proposition_5" in pal
    assert "proposition_7" not in pal
    assert "proposition_10" not in pal
    assert "line_from_points" in pal


def test_engine_i24_i25_splits_covered():
    # Review (claude-9): I.24 and I.25 are not in the play catalogue yet,
    # but their engine splits must pass the referee too: I.24's
    # elimination of proposition_23''s left disjunct is covered by the
    # stored "either" fact, and its closing same_side / not_same_side
    # by_cases by the P-or-not-P family.
    for n, hyps, steps in ((24, eng.PROP24A_HYPS_DATA, eng.PROP24_STEPS_DATA),
                           (25, eng.PROP25_HYPS_DATA, eng.PROP25_STEPS_DATA)):
        s = eng.State()
        for f, w in hyps:
            eng._assume_metric_or_fact(s, f, w)
        eng.close(s)
        found = []
        _walk_case_blocks(n, s, steps, (), found)
        assert found
        for rule, mv, bs, refuted in found:
            cov = ep.coverage(
                bs, rule, [[tuple(f) for f in b["hyps"]]
                           for b in mv["branches"]],
                [tuple(c) for c in mv["conclusion"]], refuted)
            assert cov["covered"], (n, rule, mv["branches"], cov)


def test_catalogue_lists_8_and_9():
    ns = [e["n"] for e in ep.catalogue()]
    assert ns.index(7) < ns.index(8) < ns.index(9) < ns.index(10)
    assert ep.GOALS[8] == [("_eq_ang", "b", "a", "c", "e", "d", "f")]
    assert ep.GOALS[9] == [("_eq_ang", "b", "v", "f", "c", "v", "f"),
                           ("neq", "v", "f")]


def test_palette_9():
    """palette(9) contains proposition_8 (CONSTRUCTIONS lemma,
    Book/Prop08.lean:5, cited) and proposition_6; proposition_9 itself
    and later propositions stay out."""
    pal = ep.palette(9)
    assert "proposition_8" in pal
    assert "proposition_6" in pal
    assert "proposition_9" not in pal
    assert "proposition_9'" not in pal
    assert "proposition_10" not in pal


def test_play_9_reaches_goal():
    """E-kimi-task-421/428: play(9) with the reference steps (the engine's
    own plus the explicit trusted-input moves of PLAY_EXTRA_STEPS)
    reaches the goal (also covered by test_engine_steps_reach_goal, I.9
    being straight-line)."""
    steps = ep.reference_steps(9)
    res = ep.play(9, [_engine_step_to_move(st) for st in steps])
    assert res["goal_reached"], \
        "I.9: engine replay did not reach the goal: %s" % res
    assert all(m["ok"] for m in res["moves"]), \
        "I.9: a move failed: %s" % res["moves"]


# ------------------------------------------- student wording (E-kimi-task-423)

def _assert_student_text(t):
    """Plain refusal text must exist and carry no Python repr."""
    assert t and isinstance(t, str)
    assert "('" not in t and "[(" not in t
    assert "None" not in t


def test_rule_labels_cover_every_rule():
    """Every CONSTRUCTIONS name (every palette entry) has a student label
    in a known group — a new rule fails this test until labelled."""
    groups = {"constructions", "diagrammatic", "metric", "propositions"}
    for name in eng.CONSTRUCTIONS:
        assert name in ep.RULE_LABELS, "unlabelled rule: %s" % name
        label, group = ep.RULE_LABELS[name]
        assert label and group in groups, name
    # and rule_info carries the wording to the page
    info = ep.rule_info("intersection_circles")
    assert info["label"] == "where two circles meet"
    assert info["group"] == "constructions"
    assert info["arg_hints"] == {"alpha": "first circle",
                                 "beta": "second circle"}


def test_plain_unknown_rule():
    res = ep.play(1, [_move("squircle", (), {})])
    m = res["moves"][0]
    assert "unknown rule" in m["error"]
    _assert_student_text(m["error_plain"])
    assert "squircle" in m["error_plain"]


def test_plain_future_proposition():
    res = ep.play(5, [_move("proposition_6", (), {})])
    t = res["moves"][0]["error_plain"]
    _assert_student_text(t)
    assert "I.6" in t and "earlier propositions" in t


def test_plain_bad_arity():
    res = ep.play(1, [_move("circle_from_points", ("a",), {"C": "C1"})])
    m = res["moves"][0]
    assert m["error"].startswith("bad arity")
    _assert_student_text(m["error_plain"])
    assert "circle_from_points" in m["error_plain"]


def test_plain_missing_premise():
    steps = tr.hyps_and_steps(1)[1]
    res = ep.play(1, [_move(*steps[0]),
                      _move("intersection_circles", ("BCD", "BCD"),
                            {"c": "c"})])
    m = res["moves"][-1]
    assert isinstance(m["error"], list)
    t = m["error_plain"]
    _assert_student_text(t)
    assert t.startswith("Not yet shown: ") and "BCD" in t


def test_plain_fresh_name():
    res = ep.play(1, [_move("circle_from_points", ("a", "b"), {"C": "a"})])
    m = res["moves"][0]
    assert m["error"] == ["fresh", "a"]
    t = m["error_plain"]
    _assert_student_text(t)
    assert t == ("a already exists; choose a new name, or show that the "
                 "new point is a")


def test_plain_by_contra_without_contradiction():
    mv = {"rule": "by_contra",
          "branches": [{"hyps": [["neq", "a", "b"]], "moves": []}],
          "conclusion": [["false"]], "open": False}
    res = ep.play(1, [mv])
    m = res["moves"][0]
    assert "did not reach a contradiction" in m["error"]
    t = m["error_plain"]
    _assert_student_text(t)
    assert t == ("This case does not lead to a contradiction yet: keep "
                 "going inside it, or check its assumptions.")


def test_plain_by_cases_without_conclusion():
    mv = {"rule": "by_cases",
          "branches": [{"hyps": [["peq", "a", "b"]], "moves": []},
                       {"hyps": [["neq", "a", "b"]], "moves": []}],
          "conclusion": [["between", "a", "b", "a"]], "open": False}
    res = ep.play(1, [mv])
    m = res["moves"][0]
    assert m["error"] and m["error"] != "missing case"
    t = m["error_plain"]
    _assert_student_text(t)
    assert "does not reach what you are proving" in t


def test_plain_empty_case_block():
    res = ep.play(1, [{"rule": "by_contra", "branches": [],
                       "conclusion": [["false"]], "open": False}])
    t = res["moves"][0]["error_plain"]
    _assert_student_text(t)
    assert "at least one case" in t


def test_metric_and_either_refusals_in_plain_words():
    """claude-9 review of 17adff0: a metric MissingPremise arrives as
    ("entail", kind, (args)), and either-facts nest; both came out as
    Python reprs."""
    cases = [
        (("entail", "gt_sum_len", ("a", "b", "b", "c", "a", "c")),
         "|ab| + |bc| > |ac|"),
        (("entail", "eq_len", ("a", "b", "c", "d")), "|ab| = |cd|"),
        (["either", [["on_line", "f", "AB"]],
          [["same_side", "f", "x", "AB"]]],
         "either f is on line AB, or f and x are on the same side of AB"),
    ]
    for err, words in cases:
        text = ep.student_error(err)
        assert words in text, text
        for bad in ("('", "[(", "[[", "None", "entail"):
            assert bad not in text, text


def test_catalogue_lists_22_and_23():
    """I.22 and I.23 join the catalogue after 21 (E-kimi-task-422)."""
    ns = [e["n"] for e in ep.catalogue()]
    assert ns.index(21) < ns.index(22) < ns.index(23)
    assert ep.GOALS[22] == list(eng.PROP22_GOAL)
    assert ep.GOALS[23] == list(eng.PROP23_GOAL)


def test_palette_23():
    """palette(23) contains proposition_22's primed variants (the I.23
    proof cites proposition_22', proposition_20 and proposition_8);
    proposition_22 itself is replay-only (not in CONSTRUCTIONS), and
    proposition_23 itself stays out."""
    pal = ep.palette(23)
    # proposition_22 itself is replay-only (not in CONSTRUCTIONS); its
    # citable variants are
    assert "proposition_22" not in pal
    assert "proposition_22'" in pal
    assert "proposition_22''" in pal
    assert "proposition_20" in pal
    assert "proposition_8" in pal
    assert "proposition_23" not in pal
    assert "proposition_23'" not in pal


def test_play_22_23_splits_covered_and_drops_refused():
    """E-kimi-task-422 found the referee refusing the engine's own I.22
    and I.23 splits (now in test_case_props_reach_goal). Review fixes
    (claude-9): between_not_trans (Diagrammatic.lean:96) refutes I.22's
    fourth order between f g i; ∠x:v:y = 0 is a _holds query, so I.23's
    zero-angle case covers the same-ray orders and d = e. Bad cases:
    dropping any case is refused — including I.22's i = f, which the
    between family did not ask for before (it assumed f, g, i distinct).
    """
    steps22 = [_engine_step_to_move(st) for st in tr.hyps_and_steps(22)[1]]
    k = next(j for j, m in enumerate(steps22) if m["rule"] == "by_cases")
    expect = [[["between", "g", "f", "i"]], [["between", "f", "i", "g"]],
              [["peq", "f", "i"]]]
    for j in range(3):
        mv = copy.deepcopy(steps22)
        del mv[k]["branches"][j]
        res = ep.play(22, mv)
        ent = res["moves"][-1]
        assert not res["goal_reached"]
        assert ent["error"] == "missing case", ent
        assert ent["missing_case"]["missing"] == expect[j]
    steps23 = [_engine_step_to_move(st) for st in tr.hyps_and_steps(23)[1]]
    for j in range(2):
        mv = copy.deepcopy(steps23)
        del mv[0]["branches"][j]
        ent = ep.play(23, mv)["moves"][0]
        assert ent["error"] == "missing case", ent


def test_between_split_needs_a_line():
    """between_points needs the three points on one line: at I.21's
    start nothing puts b, d, e on a common line, so a split on their
    order is refused, not covered."""
    mv = {"rule": "by_contra",
          "branches": [{"hyps": [["between", "b", "e", "d"]], "moves": []},
                       {"hyps": [["between", "e", "b", "d"]], "moves": []}],
          "conclusion": [["between", "b", "d", "e"]],
          "open": False}
    ent = ep.play(21, [mv])["moves"][-1]
    assert ent["error"] == "missing case"


def test_catalogue_lists_24_and_25():
    """I.24 and I.25 join the catalogue after 23 (E-kimi-task-424)."""
    ns = [e["n"] for e in ep.catalogue()]
    assert ns.index(23) < ns.index(24) < ns.index(25)
    assert ep.GOALS[24] == list(eng.PROP24_GOAL)
    assert ep.GOALS[25] == list(eng.PROP25_GOAL)


def test_palette_25():
    """palette(25) contains proposition_24 (citable: I.25's branch 2
    cites it) and proposition_23'; proposition_25 itself stays out."""
    pal = ep.palette(25)
    assert "proposition_24" in pal
    assert "proposition_23'" in pal
    assert "proposition_25" not in pal


def test_play_24_25_splits_covered_and_drops_refused():
    """E-kimi-task-424: I.24's closing by_cases (same_side d g EF or
    not — the P-or-not-P family) and I.25's by_contra (the angle
    trichotomy family) are covered by the referee; dropping a branch
    names the missing member."""
    steps24 = [_engine_step_to_move(st) for st in tr.hyps_and_steps(24)[1]]
    k = next(j for j, m in enumerate(steps24) if m["rule"] == "by_cases")
    expect = [[["same_side", "d", "g", "EF"]],
              [["not_same_side", "d", "g", "EF"]]]
    for j in range(2):
        mv = copy.deepcopy(steps24)
        del mv[k]["branches"][j]
        res = ep.play(24, mv)
        ent = res["moves"][k]
        assert not res["goal_reached"]
        assert ent["error"] == "missing case", ent
        assert ent["missing_case"]["missing"] == expect[j], ent
    # I.24's nested split (g on EF or not) inside branch 2
    mv = copy.deepcopy(steps24)
    del mv[k]["branches"][1]["moves"][0]["branches"][1]
    ent = ep.play(24, mv)["moves"][k]
    assert ent["error"] == "missing case", ent
    assert ent["missing_case"]["missing"] == [["not_on_line", "g", "EF"]]
    # I.25: drop either branch of the by_contra
    steps25 = [_engine_step_to_move(st) for st in tr.hyps_and_steps(25)[1]]
    expect25 = [[["eq_ang", "e", "d", "f", "b", "a", "c"]],
                [["gt_ang", "e", "d", "f", "b", "a", "c"]]]
    for j in range(2):
        mv = copy.deepcopy(steps25)
        del mv[0]["branches"][j]
        res = ep.play(25, mv)
        ent = res["moves"][0]
        assert not res["goal_reached"]
        assert ent["error"] == "missing case", ent
        assert ent["missing_case"]["missing"] == expect25[j], ent


def test_blank_and_unknown_inputs_refused_plainly():
    """Joe's first move (2026-11-09) sent circle_from_points with both
    inputs blank and got "Not yet shown:  ≠ "."""
    ent = ep.play(1, [{"rule": "circle_from_points", "args": ["", ""],
                       "bindings": {"C": "C"}}])["moves"][-1]
    assert not ent["ok"] and "Fill in every box" in ent["error_plain"]
    ent = ep.play(1, [{"rule": "circle_from_points", "args": ["a", "z"],
                       "bindings": {"C": "C"}}])["moves"][-1]
    assert not ent["ok"] and "nothing called z" in ent["error_plain"]
    res = ep.play(1, [{"rule": "circle_from_points", "args": ["a", "b"],
                       "bindings": {"C": "C"}}])
    assert res["moves"][-1]["ok"]


def test_goal_witnesses_take_any_name():
    """Joe 2026-11-09: I.1 built with circles C, D never showed Solved
    (the goal named the engine's circle BCD). Lean's ∃ witnesses may be
    any point the student made, under any name."""
    mv = [{"rule": "circle_from_points", "args": ["a", "b"],
           "bindings": {"C": "C"}},
          {"rule": "circle_from_points", "args": ["b", "a"],
           "bindings": {"C": "D"}},
          {"rule": "intersection_circles", "args": ["C", "D"],
           "bindings": {"c": "p"}}]
    assert ep.play(1, mv)["goal_reached"]
    assert not ep.play(1, mv[:2])["goal_reached"]
    # a witness must still satisfy the goal: one circle's point is not
    # equidistant from both ends
    one = mv[:1] + [{"rule": "point_on_circle", "args": ["C"],
                     "bindings": {}}] if "point_on_circle" in \
        ep.palette(1) else mv[:1]
    assert not ep.play(1, one)["goal_reached"]


def test_line_through_inside_point_meets_circle():
    """Joe at I.2 (2026-11-09): BC passes through b, the centre of D, so
    it meets D (intersection_circle_line_2, Diagrammatic.lean:305); the
    engine lacked the rule and refused "line BC meets circle D"."""
    mv = [{"rule": "circle_from_points", "args": ["b", "c"],
           "bindings": {"C": "D"}},
          {"rule": "intersections_circle_line", "args": ["D", "BC"],
           "bindings": {"a": "d", "b": "e"}}]
    assert ep.play(2, mv)["moves"][-1]["ok"]
    # a line through a point ON the circle (not inside) may only touch
    # it: still refused
    mv2 = [{"rule": "circle_from_points", "args": ["a", "b"],
            "bindings": {"C": "E"}},
           {"rule": "intersections_circle_line", "args": ["E", "BC"],
            "bindings": {"a": "d", "b": "e"}}]
    ent = ep.play(2, mv2)["moves"][-1]
    assert not ent["ok"] and ent["error"] == ["intersects_lc", "BC", "E"]


def test_i2_split_on_a_equals_c_with_named_witness():
    """Joe at I.2 (2026-11-09): split on a = c; in that case the witness
    l is just b (name_point, Lean's `use b`), so |al| = |ab| = |bc|; in
    the other case the usual construction. The plain-form case conclusion
    is checked by arithmetic, the split is covered, the goal is reached.
    name_point alone does not prove I.2, and dropping a case is refused."""
    eng_steps = [_engine_step_to_move(st) for st in tr.hyps_and_steps(2)[1]]
    blk = {"rule": "by_cases", "branches": [
        {"hyps": [["peq", "a", "c"]],
         "moves": [{"rule": "name_point", "args": ["b"],
                    "bindings": {"x": "l"}}]},
        {"hyps": [["neq", "a", "c"]], "moves": eng_steps}],
        "conclusion": [["eq_len", "a", "l", "b", "c"]], "open": False}
    res = ep.play(2, [blk])
    assert res["moves"][-1]["ok"] and res["goal_reached"]
    alone = ep.play(2, [{"rule": "name_point", "args": ["b"],
                         "bindings": {"x": "l"}}])
    assert alone["moves"][0]["ok"] and not alone["goal_reached"]
    one = copy.deepcopy(blk)
    del one["branches"][1]
    ent = ep.play(2, [one])["moves"][-1]
    assert ent["error"] == "missing case"
    assert ent["missing_case"]["missing"] == [["neq", "a", "c"]]


def test_case_try_snapped_cases_find_existing_witness():
    """Joe at I.2 (2026-11-09): dragging a onto c, or onto circle D
    (centre b through c), each makes b a witness for l (|ab| = |bc|); a
    on line BC does not settle it by itself."""
    r = ep.case_try(2, [], [["peq", "a", "c"]])
    assert r["holds"] and r["witness"] == {"l": "b"}
    assert r["moves"] == [{"rule": "name_point", "args": ["b"],
                           "bindings": {"x": "l"}}]
    mv = [{"rule": "circle_from_points", "args": ["b", "c"],
           "bindings": {"C": "D"}}]
    r = ep.case_try(2, mv, [["on_circle", "a", "D"]])
    assert r["holds"] and r["witness"] == {"l": "b"}
    assert not ep.case_try(2, mv, [["on_line", "a", "BC"]])["holds"]
    # the returned moves close the case in a real block
    blk = {"rule": "by_cases", "branches": [
        {"hyps": [["peq", "a", "c"]],
         "moves": ep.case_try(2, [], [["peq", "a", "c"]])["moves"]}],
        "conclusion": [["eq_len", "a", "l", "b", "c"]], "open": True}
    assert ep.play(2, [blk])["moves"][-1]["ok"]


def test_open_block_second_snapped_case_keeps_first_closed():
    """Joe at I.2 (2026-11-09): after snapping a = c (closed by l := b),
    snapping a onto D was refused "an earlier case must close first": the
    open-block path checked the plain eq_len conclusion by lookup."""
    mv = [{"rule": "circle_from_points", "args": ["b", "c"],
           "bindings": {"C": "D"}},
          {"rule": "by_cases", "branches": [
              {"hyps": [["peq", "a", "c"]],
               "moves": [{"rule": "name_point", "args": ["b"],
                          "bindings": {"x": "l"}}]},
              {"hyps": [["on_circle", "a", "D"]],
               "moves": [{"rule": "name_point", "args": ["b"],
                          "bindings": {"x": "l"}}]}],
           "conclusion": [["eq_len", "a", "l", "b", "c"]], "open": True}]
    assert ep.play(2, mv)["moves"][-1]["ok"]
    mv[-1]["open"] = False
    ent = ep.play(2, mv)["moves"][-1]
    assert ent["error"] == "missing case"
    assert ent["missing_case"]["missing"] == [["neq", "a", "c"]]


def test_case_conclusion_witness_takes_any_name():
    """Joe at I.2 (2026-11-10): inside a case the constructed point has
    the student's name, not the theorem's l. The case's points may serve
    as the witness (name_point added for an earlier case; the current
    case's tick uses the same search). An unfinished case still fails."""
    steps = [_engine_step_to_move(st) for st in tr.hyps_and_steps(2)[1]]
    steps[-1]["bindings"] = {"g": "z"}
    blk = {"rule": "by_cases", "branches": [
        {"hyps": [["peq", "a", "c"]],
         "moves": [{"rule": "name_point", "args": ["b"],
                    "bindings": {"x": "l"}}]},
        {"hyps": [["neq", "a", "c"]], "moves": steps}],
        "conclusion": [["eq_len", "a", "l", "b", "c"]], "open": True}
    ent = ep.play(2, [blk])["moves"][-1]
    assert ent["ok"] and ent["branches"][-1]["branch_goal_reached"]
    blk["open"] = False
    res = ep.play(2, [blk])
    assert res["moves"][-1]["ok"] and res["goal_reached"]
    short = copy.deepcopy(blk)
    short["branches"][1]["moves"] = steps[:-1]
    assert not ep.play(2, [short])["moves"][-1]["ok"]


def test_both_points_line_circle_records_drawn_order():
    """Joe at I.2 (2026-11-10): "I don't want to have to 'nurse' a clearly
    valid construction into health". The two meeting points of a line and
    a circle are interchangeable when made, so the order the drawing shows
    is recorded (via the 'beyond' move) and Euclid's construction proves
    I.2 with the plain both-points move. Building on the near point instead
    stays unproved without a case split (f may lie between d and b or
    beyond d)."""
    base = [{"rule": "circle_from_points", "args": ["b", "c"],
             "bindings": {"C": "D"}},
            {"rule": "circle_from_points", "args": ["a", "b"],
             "bindings": {"C": "E"}},
            {"rule": "circle_from_points", "args": ["b", "a"],
             "bindings": {"C": "F"}},
            {"rule": "intersection_circles", "args": ["F", "E"],
             "bindings": {"c": "d"}},
            {"rule": "line_from_points", "args": ["d", "b"],
             "bindings": {"L": "DB"}},
            {"rule": "intersections_circle_line", "args": ["D", "DB"],
             "bindings": {"a": "e", "b": "f"}}]
    far = base + [{"rule": "circle_from_points", "args": ["d", "e"],
                   "bindings": {"C": "G"}},
                  {"rule": "line_from_points", "args": ["d", "a"],
                   "bindings": {"L": "DA"}},
                  {"rule": "intersections_circle_line", "args": ["G", "DA"],
                   "bindings": {"a": "g", "b": "h"}}]
    res = ep.play(2, far)
    assert all(m["ok"] for m in res["moves"]) and res["goal_reached"]
    assert ["between", "e", "b", "d"] in res["moves"][5]["added"]
    near = base + [{"rule": "circle_from_points", "args": ["d", "f"],
                    "bindings": {"C": "G"}}]
    assert not ep.play(2, near)["moves"][-1]["ok"]   # d = f not ruled out


def test_length_marks_show_known_equalities_and_goal():
    """Joe (2026-11-10): the figure should read as solved. After I.1 at
    I.2, ab = ad = bd is one class; the goal's bc is shown as the length
    to carry until l exists; at I.1's end the goal segments are joined."""
    mv = [{"rule": "line_from_points", "args": ["a", "b"],
           "bindings": {"L": "AB"}},
          {"rule": "proposition_1", "args": ["a", "b", "AB"],
           "bindings": {"d": "d"}}]
    mk = ep.play(2, mv)["marks"]
    assert [["a", "b"], ["a", "d"], ["b", "d"]] in mk["classes"]
    assert mk["goal"] == {"segments": [["b", "c"]], "joined": False,
                          "partial": True}
    i1 = [{"rule": "circle_from_points", "args": ["a", "b"],
           "bindings": {"C": "C"}},
          {"rule": "circle_from_points", "args": ["b", "a"],
           "bindings": {"C": "D"}},
          {"rule": "intersection_circles", "args": ["C", "D"],
           "bindings": {"c": "p"}}]
    g = ep.play(1, i1)["marks"]["goal"]
    assert g["joined"] and g["segments"] == [["p", "a"], ["a", "b"]]
    part = ep.play(1, i1[:2])["marks"]["goal"]
    assert part["partial"] and not part["joined"]


def test_both_points_order_records_between_for_i3():
    """kimi-1's playtest (2026-11-10): at I.3 the circle about a meets AB
    in two points; the drawn order must record that the near one lies
    between a and b (Intersections.lean:9, b outside the circle), not just
    which side of a it is on."""
    mv = [{"rule": "proposition_2'", "args": ["a", "c0", "c1", "C"],
           "bindings": {"d": "d"}},
          {"rule": "circle_from_points", "args": ["a", "d"],
           "bindings": {"C": "D"}},
          {"rule": "intersections_circle_line", "args": ["D", "AB"],
           "bindings": {"a": "e", "b": "f"}}]
    res = ep.play(3, mv)
    assert res["goal_reached"]
    assert ["between", "a", "e", "b"] in res["moves"][-1]["added"]


def test_same_ray_angles_without_knowing_the_points_differ():
    """kimi-1's playtest (2026-11-10): at I.5, g and e both lie beyond c
    from a; angle bcg = angle bce whether or not g = e (angle_same_ray).
    Kimi's own moves now reach I.5's goal."""
    import euclid_engine as eng
    s = ep._start_state(5)
    t = s.copy()
    eng._assume_metric_or_fact(t, ("between", "a", "c", "g"), "test")
    eng.close(t)
    t.la.add({eng.ang_atom("b", "c", "g"): 1}, 0, ">")   # mention it
    eng.close(t)
    assert eng._fact_holds(t, ("_eq_ang", "b", "c", "g", "b", "c", "e"))


def test_fact_rule_without_drawing_does_not_crash():
    """kimi-1's playtest (2026-11-10): flat_angle_onlyif at I.5 crashed
    the request ('no numeric placement'), the page showed 'Failed to
    fetch' and the move was lost. A move that adds no objects needs no
    drawing."""
    res = ep.play(5, [{"rule": "flat_angle_onlyif", "args": ["a", "b", "d"],
                       "bindings": {}}])
    ent = res["moves"][-1]
    assert "error" in ent   # accepted or refused, but answered
    assert not (ent.get("error") or "").startswith("cannot draw")


def test_trusted_inputs_are_not_in_the_game():
    """Found from kimi-1's I.5 (2026-11-10): the engine's "h-sm" inputs
    (facts Lean's SMT supplies about objects built later) were in the
    game's start state, so a student's own line GB, or point d, inherited
    unproved facts. They are dropped; each proposition that needed them
    now carries explicit moves in PLAY_EXTRA_STEPS: I.5 f != b
    (between_symm_parts); I.9 v != d (between_symm_parts; d != e then
    follows in closure) and f off AB / f off AC (the cited
    equilateral_apex_off_line SMT-bridge); I.10 c off AB and I.11 f off
    AB (the cited off_line_strict_triangle). The reference steps reach
    each goal; the engine's bare steps (the trusted inputs simply
    dropped) do not."""
    s = ep._start_state(5)
    assert not s.has(("not_on_line", "c", "GB"))
    assert ep.play(5, _engine_moves(5))["goal_reached"]
    plain5 = [_move(r, a, b) for r, a, b in tr.hyps_and_steps(5)[1]]
    assert not ep.play(5, plain5)["goal_reached"]
    assert ep.AWAITING_SM == ()
    for n in (9, 10, 11):
        res = ep.play(n, [_engine_step_to_move(st)
                          for st in ep.reference_steps(n)])
        assert res["goal_reached"], n
        assert all(m["ok"] for m in res["moves"]), n
        plain = [_engine_step_to_move(st)
                 for st in tr.hyps_and_steps(n)[1]]
        assert not ep.play(n, plain)["goal_reached"], n


def test_equilateral_apex_off_line_refused_without_equilateral():
    """The I.9 bridge move is REFUSED when its apex is only isosceles
    (|x─p| = |x─q| but not = |p─q|): the soundness argument needs the
    equality |f─v| = |f─q| + |q─v| to force q between f and v. Same
    diagram premises as I.9's use, one length row short."""
    s = eng.State()
    for f in [("on_line", "p", "L"), ("on_line", "v", "L"),
              ("neq", "p", "v"), ("not_on_line", "q", "L"),
              ("on_line", "p", "PQ"), ("on_line", "q", "PQ"),
              ("neq", "p", "q"), ("opposing_sides", "x", "v", "PQ")]:
        eng._assume_metric_or_fact(s, f, "test")
    # isosceles only: |x─p| = |x─q| and |p─v| = |q─v|, no |x─p| = |p─q|
    eng._assume_metric_or_fact(s, ("eq_len", "x", "p", "x", "q"), "test")
    eng._assume_metric_or_fact(s, ("eq_len", "p", "v", "q", "v"), "test")
    eng.close(s)
    try:
        eng.apply_step(s, 0, "equilateral_apex_off_line",
                       ("x", "p", "q", "v", "L", "PQ"), {})
        assert False, "accepted an isosceles-but-not-equilateral apex"
    except eng.MissingPremise as e:
        assert "entail" in str(e), e
    # and with the equilateral rows it is accepted
    eng._assume_metric_or_fact(s, ("eq_len", "x", "p", "p", "q"), "test")
    eng._assume_metric_or_fact(s, ("eq_len", "x", "q", "p", "q"), "test")
    eng.close(s)
    eng.apply_step(s, 0, "equilateral_apex_off_line",
                   ("x", "p", "q", "v", "L", "PQ"), {})
    eng.close(s)
    assert s.has(("not_on_line", "x", "L"))


def test_objects_made_in_a_case_stay_usable_when_undrawn():
    """codex-1's playtest (2026-11-10): inside the case |ab| > |ac| of
    I.6 (drawn with |ab| = |ac|), I.3's d could not be drawn, was left
    out of the objects, and every later use of d was refused. It is now
    listed (undrawn) and usable; I.3's line argument is a line."""
    mv = [{"rule": "by_cases", "branches": [
        {"hyps": [["gt_len", "a", "b", "a", "c"]],
         "moves": [{"rule": "proposition_3",
                    "args": ["b", "a", "a", "c", "AB", "AC"],
                    "bindings": {"q": "d"}},
                   {"rule": "line_from_points", "args": ["d", "c"],
                    "bindings": {"L": "DC"}}]}],
        "conclusion": [["eq_len", "a", "b", "a", "c"]], "open": True}]
    res = ep.play(6, mv)
    br = res["moves"][-1]["branches"][-1]
    assert all(m["ok"] for m in br["moves"])
    und = {o["name"] for o in res["objects"] if o.get("undrawn")}
    assert {"d", "DC"} <= und
    assert ep.rule_info("proposition_3")["arg_kinds"]["C"] == "line"


def test_goal_checklist_i5_progress():
    """E-kimi-task-430 (P-euclid-play-goal-checklist): every goal clause
    carries its state. I.5's two angle equalities both start pending;
    playing the reference moves, the first proposition_4 that turns a
    clause over leaves exactly one shown and the other pending (before
    this, proving one of the two changed nothing visible — codex-1,
    codex-shots/i5-progress-invisible.png); at the end both are shown."""
    moves = _engine_moves(5)
    res = ep.play(5, [])
    assert [g["state"] for g in res["marks"]["goals"]] == \
        ["pending", "pending"]
    seen = None
    for i, st in enumerate(ep.reference_steps(5)):
        if st[0] != "proposition_4":
            continue
        r = ep.play(5, moves[:i + 1])
        states = [g["state"] for g in r["marks"]["goals"]]
        if "shown" in states:
            seen = states
            break
    assert seen == ["pending", "shown"], seen   # ∠cbd = ∠bce lands first
    res = ep.play(5, moves)
    assert res["goal_reached"]
    assert [g["state"] for g in res["marks"]["goals"]] == ["shown", "shown"]


def test_goal_checklist_covers_every_clause():
    """I.1's two clauses both appear, pending at the start (c := b would
    satisfy |ca| = |ab| trivially and must not count as shown) and shown
    at the end; a shown _eq_ang joins its two angles into one class."""
    res = ep.play(1, [])
    goals = res["marks"]["goals"]
    assert len(goals) == 2
    assert all(g["kind"] == "_eq_len" for g in goals)
    assert [g["state"] for g in goals] == ["pending", "pending"]
    res = ep.play(1, _engine_moves(1))
    assert [g["state"] for g in res["marks"]["goals"]] == ["shown", "shown"]
    res = ep.play(5, _engine_moves(5))
    assert any(sorted([["a", "b", "c"], ["a", "c", "b"]]) == sorted(cls)
               for cls in res["marks"]["angle_classes"]), \
        res["marks"]["angle_classes"]


def test_angle_marks_skip_names_of_the_same_angle():
    """Joe's I.2 (2026-11-10): a, d, f lie on one ray from g, so ∠agb,
    ∠bgd and ∠bgf are one angle under three names; the store's
    equalities between them got arcs at g that "don't seem to help with
    anything". No class may hold two names of one drawn angle, and this
    figure has no equal angles worth marking."""
    moves = [
        {"rule": "circle_from_points", "args": ["b", "c"], "bindings": {"C": "C"}},
        {"rule": "circle_from_points", "args": ["b", "a"], "bindings": {"C": "D"}},
        {"rule": "circle_from_points", "args": ["a", "b"], "bindings": {"C": "E"}},
        {"rule": "line_from_points", "args": ["a", "b"], "bindings": {"L": "AB"}},
        {"rule": "intersection_circles", "args": ["D", "E"], "bindings": {"c": "d"}},
        {"rule": "line_from_points", "args": ["a", "d"], "bindings": {"L": "AD"}},
        {"rule": "line_from_points", "args": ["d", "b"], "bindings": {"L": "DB"}},
        {"rule": "intersection_circle_line_extending_points",
         "args": ["C", "DB", "b", "d"], "bindings": {"g": "e"}},
        {"rule": "circle_from_points", "args": ["d", "e"], "bindings": {"C": "F"}},
        {"rule": "intersections_circle_line", "args": ["F", "AD"],
         "bindings": {"a": "f", "b": "g"}}]
    res = ep.play(2, moves)
    assert res["goal_reached"]
    assert res["marks"]["angle_classes"] == [], res["marks"]["angle_classes"]
    # still marked: I.5's base angles, two different angles
    res = ep.play(5, _engine_moves(5))
    assert any(sorted([["a", "b", "c"], ["a", "c", "b"]]) == sorted(cls)
               for cls in res["marks"]["angle_classes"])


def _i7_codex_slice5(last_move):
    """codex-1's slice-5 I.7 move list (2026-11-10), with the innermost
    opposing_sides b g CD branch holding just last_move."""
    import json
    moves = json.load(open(os.path.join(
        os.path.dirname(__file__), "testdata",
        "codex-i7-slice5-moves.json")))["moves"]

    def inner(ms):
        for m in ms:
            if m.get("rule") == "by_cases":
                for b in m["branches"]:
                    if b["hyps"] == [["opposing_sides", "b", "g", "CD"]]:
                        return b["moves"]
                    r = inner(b["moves"])
                    if r is not None:
                        return r
    inner(moves)[:] = [last_move] if last_move else []
    return moves


def _find_entry(entries, rule):
    for e in entries:
        if e.get("rule") == rule:
            return e
        for b in e.get("branches") or []:
            r = _find_entry(b.get("moves") or [], rule)
            if r:
                return r


def test_opposing_premise_accepts_either_order():
    """codex-1 slice 5: the case said b and g are on opposite sides of
    CD; opposing_same_side_trans g b h CD was refused "Not yet shown: g
    and b are on opposite sides of CD". opposingSides is symmetric."""
    res = ep.play(7, _i7_codex_slice5(
        {"rule": "opposing_same_side_trans", "args": ["g", "b", "h", "CD"],
         "bindings": {}}))
    e = _find_entry(res["moves"], "opposing_same_side_trans")
    assert e["ok"], e
    assert ["opposing_sides", "g", "h", "CD"] in e["added"]


def test_student_extension_off_line_is_drawn():
    """codex-1 slice 5: g, h from extend_point_not_on_line were usable but
    'Made but not drawn'. They are drawn past their point, on the line."""
    res = ep.play(7, _i7_codex_slice5(None))
    o = {x["name"]: x for x in res["objects"]}
    for p in ("g", "h"):
        assert not o[p].get("undrawn") and o[p]["xy"], o[p]
    a, d, g = (o[p]["xy"] for p in ("a", "d", "g"))
    assert abs((d[0] - a[0]) * (g[1] - a[1])
               - (d[1] - a[1]) * (g[0] - a[0])) < 1e-9   # g on AD
    assert math.dist(a, g) > math.dist(a, d)             # past d


def test_nested_closed_contradiction_block_lets_next_case_open():
    """codex-1 live loop (I.7): a closed split whose cases are all
    impossible (conclusion: a contradiction), nested inside the case
    'c and g on the same side of DB'. That case showed impossible; then
    adding the next case was refused "An earlier case does not work
    yet", because the engine's by_cases refuses contradictory branches.
    Such a block is the engine's by_contra with conclusion False."""
    import json
    rec = json.load(open(os.path.join(os.path.dirname(__file__),
                                      "testdata",
                                      "codex-i7-live-nested-close.json")))
    res = ep.play(7, rec["moves"])
    errs = []

    def walk(es):
        for e in es:
            if e.get("error"):
                errs.append(e["error"])
            for b in e.get("branches") or []:
                walk(b.get("moves") or [])
    walk(res["moves"])
    assert errs == [], errs
    # and an inner case that is NOT finished still blocks: keep only the
    # first move of the 'b, g opposite sides of CD' case
    import copy
    mv = copy.deepcopy(rec["moves"])

    def strip(ms):
        for m in ms:
            for b in m.get("branches") or []:
                if b["hyps"] == [["opposing_sides", "b", "g", "CD"]]:
                    b["moves"] = b["moves"][:1]
                    return True
                if strip(b["moves"]):
                    return True
    assert strip(mv)
    errs.clear()
    walk(ep.play(7, mv)["moves"])
    assert any("did not reach a contradiction" in str(e) for e in errs), errs


def test_goal_checklist_all_props_have_words():
    """Every catalogue prop reports every goal clause with a state and
    student-readable words (never a Python repr)."""
    for entry in ep.catalogue():
        res = ep.play(entry["n"], [])
        goals = res["marks"]["goals"]
        folded = len(ep._GOAL_FOLD.get(entry["n"], (0, ()))[1])
        assert len(goals) == len(ep.GOALS[entry["n"]]) - folded, entry["n"]
        for g in goals:
            assert g["state"] in ("shown", "pending")
            assert g["words"] and "(" not in g["words"], g


def test_impossible_case_finishes_a_by_cases_split():
    """codex-1's playtest slice 3 (2026-11-10): at I.6 the case
    |ab| > |ac| became contradictory; the tick said it held, but adding
    the next case was refused ('an earlier case does not work yet'),
    because the engine's by_cases rejects a contradictory case as
    vacuous. In the game an impossible case is finished (the referee
    checks coverage), and the page says why it is impossible."""
    case1 = [{"rule": "proposition_3", "args": ["b", "a", "a", "c", "AB",
                                                "AC"], "bindings": {"q": "d"}},
             {"rule": "line_from_points", "args": ["d", "c"],
              "bindings": {"L": "DC"}},
             {"rule": "proposition_4",
              "args": ["b", "d", "c", "c", "a", "b", "AB", "DC", "BC", "AC",
                       "AB", "BC"], "bindings": {}},
             {"rule": "sum_angles_onlyif",
              "args": ["c", "a", "b", "d", "AC", "BC"], "bindings": {}}]
    blk = {"rule": "by_cases", "branches": [
        {"hyps": [["gt_len", "a", "b", "a", "c"]], "moves": case1}],
        "conclusion": [["eq_len", "a", "b", "a", "c"]], "open": True}
    br = ep.play(6, [blk])["moves"][-1]["branches"][-1]
    assert br["branch_contradictory"] and br["why_impossible"]
    two = copy.deepcopy(blk)
    two["branches"].append({"hyps": [["eq_len", "a", "b", "a", "c"]],
                            "moves": []})
    ent = ep.play(6, [two])["moves"][-1]
    assert ent["ok"] and ent["branches"][0]["closed"]


def _open_case_move(hyps, inner=None, rule="by_cases"):
    return {"rule": rule,
            "branches": [{"hyps": [list(f) for f in hyps],
                          "moves": inner or []}],
            "conclusion": [], "open": True}


def test_case_figure_peq_drawn_and_coincident():
    """I.2, open case peq a c (E-kimi-task-431): the case figure is
    drawn, a lands ON c numerically, and the givens (b, c distinct and
    on BC, a distinct from b) still hold."""
    res = ep.play(2, [_open_case_move([("peq", "a", "c")])])
    cf = res["case_figure"]
    assert cf["drawn"] is True
    assert cf["hyps"] == [["peq", "a", "c"]]
    o = {x["name"]: x["xy"] for x in res["objects"]
            if x["kind"] == "point"}
    assert math.dist(o["a"], o["c"]) < 1e-6, o
    assert math.dist(o["b"], o["c"]) > 0.05   # neq b c, visibly
    assert math.dist(o["a"], o["b"]) > 0.05   # neq a b


def test_case_figure_impossible_case_drawn_with_broken_given():
    """I.6, open case |a-b| > |a-c| (E-kimi-task-432): no placement
    satisfies the givens AND the case (the base angles are equal), but
    the case alone is a figure -- so it is drawn with the equal-base-
    angles given relaxed, and 'broken' says how the angles now stand.
    Euclid's reasoning is visual: were ab longer than ac, the base
    angles could not be equal."""
    t0 = time.perf_counter()
    res = ep.play(6, [_open_case_move([("gt_len", "a", "b", "a", "c")])])
    assert time.perf_counter() - t0 < 2.0
    cf = res["case_figure"]
    assert cf["drawn"] is True
    assert cf["relaxed"] == [["eq_ang", "a", "b", "c", "a", "c", "b"]]
    assert len(cf["broken"]) == 1 and "∠abc" in cf["broken"][0]
    o = {x["name"]: x["xy"] for x in res["objects"]
            if x["kind"] == "point"}
    scale = max(math.dist(o[p], o[q])
                for p in o for q in o) or 1.0
    ab, ac = math.dist(o["a"], o["b"]), math.dist(o["a"], o["c"])
    assert ab > ac + 0.04 * scale, (ab, ac, scale)
    abc = ep._angle_at(o["a"], o["b"], o["c"])
    acb = ep._angle_at(o["a"], o["c"], o["b"])
    assert abs(abc - acb) > 0.05, (abc, acb)
    # the other givens still hold: c off AB, the incidences intact
    assert ep._dist_line(o["c"], o["a"], o["b"]) > 0.04 * scale


def test_case_figure_possible_case_not_relaxed():
    """I.2, open case peq a c is POSSIBLE: drawn as before, with no
    'relaxed'/'broken' keys (relaxation only fires when the givens and
    the case cannot all hold)."""
    res = ep.play(2, [_open_case_move([("peq", "a", "c")])])
    cf = res["case_figure"]
    assert cf["drawn"] is True
    assert "relaxed" not in cf and "broken" not in cf


def test_case_figure_unsatisfiable_even_relaxed_not_drawn():
    """Two contradicting case facts (a on BC and a off BC): leaving out
    any one GIVEN cannot help -- the case facts are never relaxed -- so
    the case stays undrawn."""
    hyps = [("on_line", "a", "BC"), ("not_on_line", "a", "BC")]
    res = ep.play(2, [_open_case_move(hyps)])
    cf = res["case_figure"]
    assert cf["drawn"] is False
    assert "no placement satisfies" in cf["why"]


def test_case_figure_on_line_drawn():
    """I.2, open case on_line a BC: drawn, with a numerically on the
    line through b and c."""
    res = ep.play(2, [_open_case_move([("on_line", "a", "BC")])])
    cf = res["case_figure"]
    assert cf["drawn"] is True, cf
    o = {x["name"]: x["xy"] for x in res["objects"]
            if x["kind"] == "point"}
    a, b, c = o["a"], o["b"], o["c"]
    d = (abs((b[0] - a[0]) * (c[1] - a[1]) - (b[1] - a[1]) * (c[0] - a[0]))
         / math.dist(b, c))
    assert d < 1e-6, (a, b, c)
    assert math.dist(o["b"], o["c"]) > 0.05


def test_no_open_case_no_case_figure():
    """Without an open case block there is no 'case_figure' key and the
    drawing is identical to the FREE[n] placement."""
    t0 = time.perf_counter()
    res = ep.play(2, [])
    assert time.perf_counter() - t0 < 2.0
    assert "case_figure" not in res
    o = {x["name"]: x["xy"] for x in res["objects"]
            if x["kind"] == "point"}
    for p, p0 in tr.FREE[2].items():
        assert math.dist(o[p], p0) < 1e-12, (p, o[p], p0)


def test_case_figure_timing():
    """play() on the case-figure cases stays under 2 s each."""
    for n, hyps in ((2, [("peq", "a", "c")]),
                    (2, [("on_line", "a", "BC")]),
                    (6, [("gt_len", "a", "b", "a", "c")])):
        t0 = time.perf_counter()
        ep.play(n, [_open_case_move(hyps)])
        assert time.perf_counter() - t0 < 2.0, (n, hyps)


def test_case_figure_nested_innermost():
    """Nested open blocks: the innermost open case's hypotheses are
    added to the enclosing open case's."""
    inner = _open_case_move([("on_line", "a", "BC")])
    outer = _open_case_move([("neq", "a", "c")], inner=[inner])
    res = ep.play(2, [outer])
    cf = res["case_figure"]
    assert cf["hyps"] == [["neq", "a", "c"], ["on_line", "a", "BC"]]
    assert cf["drawn"] is True
    o = {x["name"]: x["xy"] for x in res["objects"]
            if x["kind"] == "point"}
    a, b, c = o["a"], o["b"], o["c"]
    d = (abs((b[0] - a[0]) * (c[1] - a[1]) - (b[1] - a[1]) * (c[0] - a[0]))
         / math.dist(b, c))
    assert d < 1e-6
    assert math.dist(a, c) > 0.05


def test_case_figure_moves_only_the_point_the_case_is_about():
    """claude-9's review of fa06b11 (2026-11-10): supposing a = c should
    move a onto c, as a drag would, not nudge a, b and c; 'a on BC' moves
    a to the nearest point of BC."""
    def blk(h):
        return [{"rule": "by_cases", "branches": [{"hyps": [h],
                                                    "moves": []}],
                 "conclusion": [["eq_len", "a", "l", "b", "c"]],
                 "open": True}]
    r = ep.play(2, blk(["peq", "a", "c"]))
    assert r["case_figure"]["moved"] == ["a"]
    P = {o["name"]: o["xy"] for o in r["objects"] if o.get("xy")}
    assert P["b"] == [0.0, 0.0] and P["c"] == [1.0, 0.0]
    r = ep.play(2, blk(["on_line", "a", "BC"]))
    P = {o["name"]: o["xy"] for o in r["objects"] if o.get("xy")}
    assert abs(P["a"][0] + 0.5) < 1e-3 and abs(P["a"][1]) < 1e-6


def test_extension_labels_match_what_the_move_gives():
    """codex-1's slice 4 (2026-11-10): 'extend past' / 'from' were the
    wrong way round, so following the labels extended the wrong end.
    The move gives '[c] is between [b] and x': from [b], past [c]."""
    for r in ("extend_point", "extend_point_not_on_line",
              "extend_point_longer"):
        info = ep.rule_info(r)
        assert "[c] is between [b] and x" in info["gives"], r
        assert info["arg_hints"]["b"] == "from", r
        assert info["arg_hints"]["c"] == "extend past", r


def test_off_line_refusal_names_the_missing_distinctness():
    # codex-1, live loop slice 7: e beyond b on AB; pasch_3 a b e CB is
    # refused for "e off CB", which the engine sees by itself once e != b
    # (two lines meet once). The refusal must say that, and the step that
    # supplies e != b (between_symm_parts e b a) must let the move through.
    base = [{"rule": "extend_point", "args": ["AB", "a", "b"],
             "bindings": {"x": "e"}}]
    pasch = {"rule": "pasch_3_opposing", "args": ["a", "b", "e", "CB"],
             "bindings": {}}
    case = lambda ms: [{"rule": "by_cases", "branches": [
        {"hyps": [["opposing_sides", "a", "d", "CB"]], "moves": ms}],
        "conclusion": [["false"]], "open": True}]
    res = ep.play(7, case(base + [pasch]))
    inner = res["moves"][0]["branches"][0]["moves"]
    err = inner[-1]["error_plain"]
    assert "e lies off line CB" in err
    assert "e and b are different points" in err
    sym = {"rule": "between_symm_parts", "args": ["e", "b", "a"],
           "bindings": {}}
    res = ep.play(7, case(base + [sym, pasch]))
    inner = res["moves"][0]["branches"][0]["moves"]
    assert all(m["ok"] for m in inner), inner[-1]


def test_fact_rule_offers_no_name_and_makes_no_phantom_point():
    # codex-1, live loop slice 7: intersection_lines_opposing a b CD AB
    # was given the name f; the move only states that CD and AB meet,
    # but f was listed as a made (undrawn) point no fact was about.
    assert ep.rule_info("intersection_lines_opposing")["makes_nothing"]
    assert not ep.rule_info("intersection_lines")["makes_nothing"]
    assert not ep.rule_info("extend_point")["makes_nothing"]
    moves = [{"rule": "line_from_points", "args": ["c", "d"],
              "bindings": {"L": "CD"}},
             {"rule": "by_cases", "branches": [
                 {"hyps": [["opposing_sides", "a", "b", "CD"]], "moves": [
                     {"rule": "intersection_lines_opposing",
                      "args": ["a", "b", "CD", "AB"],
                      "bindings": {"x": "f"}}]}],
              "conclusion": [["false"]], "open": True}]
    res = ep.play(7, moves)
    inner = res["moves"][-1]["branches"][0]["moves"]
    assert inner[-1]["ok"], inner[-1]
    assert "f" not in {o["name"] for o in res["objects"]}


def test_goal_neq_clause_not_ticked_for_a_stray_witness():
    # codex-1, live loop slice 8: mid-way through I.9 the checklist read
    # "☐ ∠bvf = ∠cvf | ☑ v ≠ d": the ≠ clause found its own witness d,
    # which the angle clause is not about. Both clauses share one f.
    here = os.path.dirname(os.path.abspath(__file__))
    moves = json.load(open(os.path.join(
        here, "testdata", "codex-i9-slice8-partial.json")))
    goals = ep.play(9, moves)["marks"]["goals"]
    assert [g["state"] for g in goals] == ["pending", "pending"], goals
    assert goals[1]["words"] == "v ≠ f"
    # solved, both clauses still tick (and I.23, the other ≠ goal)
    for n in (9, 23):
        res = ep.play(n, [_engine_step_to_move(st)
                          for st in ep.reference_steps(n)])
        assert res["goal_reached"], n
        assert all(g["state"] == "shown" for g in res["marks"]["goals"]), \
            (n, res["marks"]["goals"])


def test_circle_meeting_on_a_side_is_drawn():
    # codex-1, live loop slice 9, I.9: intersection_opposite_side D E v b e
    # BE made f, but f was "made but not drawn" (no placement for the
    # side variants). f is drawn, on the other side of be from v.
    here = os.path.dirname(os.path.abspath(__file__))
    moves = json.load(open(os.path.join(
        here, "testdata", "codex-i9-slice9-apex.json")))
    res = ep.play(9, moves)
    assert all(m["ok"] for m in res["moves"])
    o = {x["name"]: x for x in res["objects"]}
    assert o["f"].get("xy") and not o["f"].get("undrawn"), o["f"]
    v, b, e, f = (o[k]["xy"] for k in "vbef")
    side = lambda x: ((x[0] - b[0]) * (e[1] - b[1])
                      - (x[1] - b[1]) * (e[0] - b[0]))
    assert side(f) * side(v) < 0
    # the same-side variant picks the other meeting point
    moves[-1] = dict(moves[-1], rule="intersection_same_side")
    res = ep.play(9, moves)
    o = {x["name"]: x for x in res["objects"]}
    assert side(o["f"]["xy"]) * side(v) > 0


def test_lines_meet_premise_accepts_either_order():
    # codex-1, live loop slice 9, I.10: intersection_lines AB CD refused
    # "lines AB and CD meet" with intersects_ll CD AB shown
    # (intersection_symm, Diagrammatic.lean:289).
    here = os.path.dirname(os.path.abspath(__file__))
    moves = json.load(open(os.path.join(
        here, "testdata", "codex-i10-slice9-meet-order.json")))
    res = ep.play(10, moves)
    assert all(m["ok"] for m in res["moves"]), res["moves"][-1]
    o = {x["name"]: x for x in res["objects"]}
    assert o["e"]["xy"] == [0.0, 0.0]   # where CD crosses AB
    # with no meeting shown at all, still refused, naming the asked order
    res = ep.play(10, moves[:7] + moves[8:])
    assert res["moves"][-1]["error_plain"] == \
        "Not yet shown: lines AB and CD meet"


def test_goal_checklist_uses_one_witness_for_all_clauses():
    # codex-1, live loop slice 9, I.10: "☑ e is between a and b |
    # ☑ |ac| = |cb|" while unsolved; each clause had found its own point.
    here = os.path.dirname(os.path.abspath(__file__))
    moves = json.load(open(os.path.join(
        here, "testdata", "codex-i10-slice9-two-witnesses.json")))
    res = ep.play(10, moves)
    assert not res["goal_reached"]
    goals = res["marks"]["goals"]
    assert [g["state"] for g in goals] == ["shown", "pending"], goals
    assert [g["words"] for g in goals] == ["e is between a and b",
                                           "|ae| = |eb|"]


def test_right_angle_square_only_when_proved():
    """codex-1, live loop slice 10, I.11: before the last move, ∠uwf and
    ∠dwf were equal and measured 90°, and the page drew the right-angle
    square while '☐ angle uwf is right' was pending. marks.right_angles
    lists only angles the store proves right; the page squares only
    those."""
    case = json.load(open(os.path.join(os.path.dirname(__file__),
                                       "testdata",
                                       "codex-i11-slice10-solved.json")))
    moves = case["moves"]
    before = ep.play(11, moves[:-1])
    assert not before["goal_reached"]
    assert before["marks"]["right_angles"] == []
    assert before["marks"]["angle_classes"]   # the equal pair is still marked
    after = ep.play(11, moves)
    assert after["goal_reached"]
    assert ["f", "w", "u"] in after["marks"]["right_angles"]


def test_i12_goal_names_only_the_theorem_points_and_needs_e_on_ab():
    """codex-1, live loop slice 10, I.12: the checklist read '∠ehc is
    right' with no e in the statement, and e was any point at all. The
    goal now needs e ON AB and not h (equivalent to Lean's ∠ahc ∨ ∠bhc
    right), and the checklist line says it in the theorem's names."""
    assert ("on_line", "e", "AB") in ep.GOALS[12]
    assert ("neq", "e", "h") in ep.GOALS[12]
    words = [g["words"] for g in ep.play(12, [])["marks"]["goals"]]
    assert len(words) == 2
    for w in words:      # no point e: not alone, not inside an angle
        assert not re.search(r"\be\b|∠e|∠\we\w|∠\w\we", w), w
    assert "∠ahc or ∠bhc is right" in words[1]
    mv = [_move(r, a, b) for r, a, b in ep.reference_steps(12)]
    res = ep.play(12, mv)
    assert res["goal_reached"]
    assert all(g["state"] == "shown" for g in res["marks"]["goals"])


def test_line_inequality_is_read_in_either_order():
    """codex-1, live loop slice 10, I.12: pasch_4 c e d AB CD was refused
    'lines AB and CD are different' with CD ≠ AB known (c on CD, c off
    AB). The engine's neq_line_symm closes the other order."""
    case = json.load(open(os.path.join(os.path.dirname(__file__),
                                       "testdata",
                                       "codex-i12-slice10-pasch-lines.json")))
    res = ep.play(12, case["moves"])
    last = res["moves"][-1]
    assert last["rule"] == "pasch_4" and last["ok"], last.get("error")
    assert ["between", "c", "e", "d"] in last["added"]
