import subprocess
import sys
from pathlib import Path

import euclid_notation_cli as cli


WITNESS = [
    ("line_from_points", ("a", "b")),
    ("proposition_1", ("a", "b", "L_1")),
    ("line_from_points", ("d_2", "a")),
    ("line_from_points", ("d_2", "b")),
    ("circle_from_points", ("b", "c")),
    ("intersection_circle_line_extending_points", ("C_5", "L_4", "b", "d_2")),
    ("circle_from_points", ("d_2", "g_6")),
    ("intersection_circle_line_extending_points", ("C_7", "L_3", "a", "d_2")),
]


def invoke(tmp_path, *args):
    return subprocess.run(
        [sys.executable, str(Path(cli.__file__)), "--log-dir", str(tmp_path), *args],
        text=True, capture_output=True, env={**dict(__import__("os").environ), "PYTHONHASHSEED": "0"},
    )


def start(tmp_path, game_id="g", cap=10):
    result = invoke(tmp_path, "new", "--prop", "2", "--cap", str(cap), "--id", game_id)
    assert result.returncode == 0, result.stderr


def current(tmp_path, game_id):
    records = cli._read(tmp_path / f"{game_id}.jsonl")
    return records, cli._replay(records)[2]


def action_text(state, rule, args):
    matches = []
    for action in state.legal_actions():
        grounded = state._action_by_id[action]
        if grounded[0] == rule and grounded[1] == tuple(args):
            matches.append(state.action_to_string(0, action))
    assert len(matches) == 1, (rule, args, matches)
    return matches[0]


def play_witness(tmp_path, game_id, count=8):
    start(tmp_path, game_id)
    for rule, args in WITNESS[:count]:
        _records, state = current(tmp_path, game_id)
        result = invoke(tmp_path, "move", "--id", game_id, action_text(state, rule, args))
        assert result.returncode == 0, result.stderr
    return current(tmp_path, game_id)[1]


def test_eight_move_witness_is_solved_and_prefix_is_not(tmp_path):
    assert play_witness(tmp_path, "full").goal_reached()
    assert not play_witness(tmp_path, "prefix", 7).goal_reached()


def test_illegal_move_is_refused_without_changing_game_state(tmp_path):
    start(tmp_path)
    before_records, before = current(tmp_path, "g")
    result = invoke(tmp_path, "move", "--id", "g", "not_a_move(a,b)")
    assert result.returncode == 2
    assert result.stderr.strip() == "unknown or illegal move"
    after_records, after = current(tmp_path, "g")
    assert after.history_moves == before.history_moves
    assert after.engine_state.facts == before.engine_state.facts
    assert len(after_records) == len(before_records) + 1
    assert after_records[-1]["accepted"] is False


def test_number_and_string_select_the_same_move(tmp_path):
    start(tmp_path, "number")
    start(tmp_path, "string")
    shown = invoke(tmp_path, "show", "--id", "number")
    assert shown.returncode == 0
    records, state = current(tmp_path, "string")
    text = action_text(state, "line_from_points", ("a", "b"))
    show_record = cli._read(tmp_path / "number.jsonl")[-1]
    number = show_record["legal_moves"].index(text) + 1
    assert invoke(tmp_path, "move", "--id", "number", str(number)).returncode == 0
    assert invoke(tmp_path, "move", "--id", "string", text).returncode == 0
    assert current(tmp_path, "number")[1].history_moves == current(tmp_path, "string")[1].history_moves


def test_move_cap_ends_game(tmp_path):
    start(tmp_path, cap=1)
    _records, state = current(tmp_path, "g")
    text = action_text(state, "line_from_points", ("a", "b"))
    assert invoke(tmp_path, "move", "--id", "g", text).returncode == 0
    _records, state = current(tmp_path, "g")
    assert state.is_terminal()
    assert not state.goal_reached()
    refused = invoke(tmp_path, "move", "--id", "g", text)
    assert refused.returncode == 2
    assert refused.stderr.strip() == "game is over"
