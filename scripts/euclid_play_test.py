"""Tests for euclid_play.py (E-kimi-task-413, P-euclid-play-backend).

Run from scripts/: ~/code/futon6/.venv/bin/python -m pytest -q euclid_play_test.py
"""

import copy
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
    """I.10, 13, 14, 19, 21 played with the engine's own steps (case
    moves as closed blocks) reach the goal."""
    reached = []
    for n in (10, 13, 14, 19, 21):
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
    """A closed by_contra whose branch does not contradict is refused
    with the engine's own wording."""
    mv = {"rule": "by_contra",
          "branches": [{"hyps": [["between", "b", "e", "d"]],
                        "moves": []}],
          "conclusion": [["between", "b", "d", "e"]],
          "open": False}
    res = ep.play(21, [mv])
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


def test_future_proposition_refused():
    res = ep.play(5, [_move("proposition_6", (), {})])
    assert res["moves"][0]["error"] == "I.6 is not proved yet"
    assert not res["moves"][0]["ok"]
    assert not res["goal_reached"]


def test_missing_premise_returns_the_fact():
    # I.1: intersection_circles BCD ACE before the second circle (ACE)
    # exists must fail with the missing premise as the error.
    steps = tr.hyps_and_steps(1)[1]
    res = ep.play(1, [_move(*steps[0]), _move(*steps[2])])
    assert len(res["moves"]) == 2
    assert res["moves"][0]["ok"]
    last = res["moves"][-1]
    assert not last["ok"]
    assert isinstance(last["error"], list)
    assert "ACE" in last["error"]  # the missing premise names ACE
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
    assert all(set(e) == {"name", "args", "cite", "binds"}
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
