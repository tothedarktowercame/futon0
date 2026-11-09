"""Tests for euclid_play.py (E-kimi-task-413, P-euclid-play-backend).

Run from scripts/: ~/code/futon6/.venv/bin/python -m pytest -q euclid_play_test.py
"""

import copy
import pytest
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
    _h, steps = tr.hyps_and_steps(n)
    return [_move(r, a, b) for r, a, b in steps]


def _straight_line(n):
    return all(r not in ("by_contra", "by_cases")
               for r, _a, _b in tr.hyps_and_steps(n)[1])


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
        steps = tr.hyps_and_steps(n)[1]
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
        _walk_case_blocks(n, s, tr.hyps_and_steps(n)[1], (), found)
        for rule, mv, bs, refuted in found:
            cov = ep.coverage(
                bs, rule,
                [[tuple(f) for f in b["hyps"]] for b in mv["branches"]],
                [tuple(c) for c in mv["conclusion"]], refuted)
            assert cov["covered"], (n, rule, mv["branches"], cov)
            table.append((n, rule, cov["family"]))
        # and the engine's steps still reach Solved under the referee
        res = ep.play(n, [_engine_step_to_move(st)
                          for st in tr.hyps_and_steps(n)[1]])
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
        if not _straight_line(n):
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
        if not _straight_line(n):
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
    assert all(set(e) == {"name", "args", "cite", "binds", "label",
                          "group", "arg_hints"}
               for e in p1["palette"])
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
    """E-kimi-task-421: play(9) with the engine's own steps reaches the
    goal (also covered by test_engine_steps_reach_goal, I.9 being
    straight-line). play(8) awaits a referee fix — see the
    E-kimi-task-421 bell: the play referee's _holds does not
    symmetrize peq, so I.8's by_contra conclusion peq d c' (Lean's
    `by_cases d = g` order) is not matched to the normalized family
    member peq c' d."""
    steps = tr.hyps_and_steps(9)[1]
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
