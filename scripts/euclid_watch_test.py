import json
from pathlib import Path

import euclid_play as ep
import euclid_spiel  # noqa: F401
import pyspiel


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


def _write_log(root: Path, game_id, move_texts=(), cap=10, player=None):
    first = {"timestamp": "2026-11-11T12:00:00+00:00", "command": "new",
             "accepted": True, "proposition": 2, "move_cap": cap,
             "legal_move_count": 49, "solved": False}
    if player:
        first["player"] = player
    rows = [first]
    for index, text in enumerate(move_texts, 1):
        rows.append({"timestamp": f"2026-11-11T12:00:{index:02d}+00:00",
                     "command": "move", "accepted": True, "move": text,
                     "legal_move_count": 0, "solved": index == len(move_texts)})
    path = root / f"{game_id}.jsonl"
    path.write_text("".join(json.dumps(row) + "\n" for row in rows))
    return path


def _witness_strings_and_check_conversion():
    state = pyspiel.load_game(
        "euclid_book1(proposition=2,move_cap=10,dense_actions=true)").new_initial_state()
    texts = []
    for rule, args in WITNESS:
        matches = [action for action in state.legal_actions()
                   if state._action_by_id[action][0] == rule
                   and state._action_by_id[action][1] == args]
        assert len(matches) == 1
        action = matches[0]
        grounded = state._action_by_id[action]
        move = ep.notation_action_to_play_move(state, action)
        assert (move["rule"], tuple(move["args"]), tuple(sorted(move["bindings"].items()))) == grounded
        texts.append(state.action_to_string(0, action))
        state.apply_action(action)
    assert state.goal_reached()
    return texts


def test_watch_list_shows_planted_game(tmp_path, monkeypatch):
    monkeypatch.setattr(ep, "_NOTATION_LOG_DIR", str(tmp_path))
    _write_log(tmp_path, "luna-I2-1", player="reader")
    page = ep._watch_index_html()
    assert "href='./luna-I2-1'" in page
    assert "reader" in page and "I.2" in page and "0 / 10" in page


def test_witness_conversion_round_trips_and_final_watch_is_solved(tmp_path, monkeypatch):
    monkeypatch.setattr(ep, "_NOTATION_LOG_DIR", str(tmp_path))
    texts = _witness_strings_and_check_conversion()
    _write_log(tmp_path, "luna-I2-witness", texts)
    watched = ep._notation_replay("luna-I2-witness")
    assert watched["solved"] and watched["over"]
    assert watched["shown_step"] == 8
    assert len(watched["moves"]) == 8
    assert len(watched["drawing"]["moves"]) == 8


def test_undrawable_move_is_listed_instead_of_failing(tmp_path, monkeypatch):
    monkeypatch.setattr(ep, "_NOTATION_LOG_DIR", str(tmp_path))
    state = pyspiel.load_game(
        "euclid_book1(proposition=2,move_cap=10,dense_actions=true)").new_initial_state()
    text = next(state.action_to_string(0, action) for action in state.legal_actions()
                if state._action_by_id[action][0] == "arbitrary_point")
    _write_log(tmp_path, "luna-undrawn", [text])
    watched = ep._notation_replay("luna-undrawn")
    assert watched["moves"] == [{"number": 1, "text": text, "not_drawn": True,
                                 "error": None}]
    assert watched["drawing"]["objects"]
