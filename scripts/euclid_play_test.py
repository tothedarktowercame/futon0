"""Tests for euclid_play.py (E-kimi-task-413, P-euclid-play-backend).

Run from scripts/: ~/code/futon6/.venv/bin/python -m pytest -q euclid_play_test.py
"""

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
