#!/usr/bin/env python3
"""Replayable terminal interface to the dense Euclid OpenSpiel game."""

from __future__ import annotations

import argparse
import json
import os
import sys
from datetime import datetime, timezone
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
    sys.path.insert(0, str(SCRIPT_DIR))

import euclid_play as play  # noqa: E402
import euclid_spiel  # noqa: E402,F401
import pyspiel  # noqa: E402

DEFAULT_LOG_DIR = Path.home() / "notes" / "euclid-cs" / "notation-games"


class CliError(Exception):
    pass


def _timestamp():
    return datetime.now(timezone.utc).isoformat()


def _path(log_dir: Path, game_id: str) -> Path:
    if not game_id or game_id in {".", ".."} or Path(game_id).name != game_id:
        raise CliError("invalid game id")
    return log_dir / f"{game_id}.jsonl"


def _read(path: Path):
    if not path.exists():
        raise CliError(f"unknown game id: {path.stem}")
    with path.open(encoding="utf-8") as stream:
        return [json.loads(line) for line in stream if line.strip()]


def _append(path: Path, record):
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("a", encoding="utf-8") as stream:
        stream.write(json.dumps({"timestamp": _timestamp(), **record},
                                sort_keys=True, separators=(",", ":")) + "\n")


def _game_spec(prop: int, cap: int) -> str:
    return f"euclid_book1(proposition={prop},move_cap={cap},dense_actions=true)"


def _replay(records):
    first = records[0]
    if first.get("command") != "new":
        raise CliError("corrupt game log: first command is not new")
    prop, cap = int(first["proposition"]), int(first["move_cap"])
    state = pyspiel.load_game(_game_spec(prop, cap)).new_initial_state()
    moves = []
    for record in records[1:]:
        if record.get("command") != "move" or not record.get("accepted"):
            continue
        if state.is_terminal():
            raise CliError("corrupt game log: accepted move follows terminal state")
        legal = state.legal_actions()
        by_string = {state.action_to_string(0, action): action for action in legal}
        move = record["move"]
        if move not in by_string:
            raise CliError(f"corrupt game log: recorded move is no longer legal: {move}")
        state.apply_action(by_string[move])
        moves.append(move)
    return prop, cap, state, moves


def _legal(state):
    if state.is_terminal():
        return []
    return [(action, state.action_to_string(0, action))
            for action in state.legal_actions()]


def _base_record(command, state, legal_count, accepted=None, move=None, reason=None):
    record = {"command": command, "legal_move_count": legal_count,
              "solved": bool(state.goal_reached())}
    if accepted is not None:
        record["accepted"] = accepted
    if move is not None:
        record["move"] = move
    if reason is not None:
        record["reason"] = reason
    return record


def command_new(args):
    path = _path(args.log_dir, args.id)
    if path.exists():
        raise CliError(f"game id already exists: {args.id}")
    if args.prop < 1 or args.cap < 1:
        raise CliError("proposition and move cap must be positive")
    state = pyspiel.load_game(_game_spec(args.prop, args.cap)).new_initial_state()
    legal_count = len(_legal(state))
    _append(path, {**_base_record("new", state, legal_count, accepted=True),
                   "proposition": args.prop, "move_cap": args.cap})
    print(f"started {args.id}: Euclid I.{args.prop}, move cap {args.cap}")


def _loaded(args):
    path = _path(args.log_dir, args.id)
    records = _read(path)
    return path, records, _replay(records)


def command_show(args):
    path, _records, (prop, cap, state, moves) = _loaded(args)
    legal = _legal(state)
    given = play.tr.objects_from_hyps(prop)
    print(f"Proposition: I.{prop}")
    print("Given objects: " + ", ".join(f"{o['name']} ({o['kind']})" for o in given))
    print("Goal:")
    for goal in play.GOALS[prop]:
        print(f"  - {play.fact_words(goal)}")
    print("Moves so far:")
    if moves:
        for index, move in enumerate(moves, 1):
            print(f"  {index}. {move}")
    else:
        print("  (none)")
    print(f"Moves left: {max(0, cap - len(moves))}")
    print(f"Solved: {'yes' if state.goal_reached() else 'no'}")
    print(f"Over: {'yes' if state.is_terminal() else 'no'}")
    print("Legal moves:")
    if legal:
        for index, (_action, text) in enumerate(legal, 1):
            print(f"  {index}. {text}")
    else:
        print("  (none)")
    _append(path, {**_base_record("show", state, len(legal), accepted=True),
                   "legal_moves": [text for _action, text in legal]})


def command_facts(args):
    path, _records, (_prop, _cap, state, _moves) = _loaded(args)
    legal_count = len(_legal(state))
    for fact in sorted(state.engine_state.facts, key=repr):
        print(play.fact_words(fact))
    _append(path, _base_record("facts", state, legal_count, accepted=True))


def command_move(args):
    path, records, (_prop, _cap, state, _moves) = _loaded(args)
    if state.is_terminal():
        reason = "game is over"
        _append(path, _base_record("move", state, 0, accepted=False,
                                   move=args.move, reason=reason))
        raise CliError(reason)
    legal = _legal(state)
    by_string = {text: action for action, text in legal}
    requested = args.move
    chosen = None
    if requested.isdecimal():
        shows = [record for record in records if record.get("command") == "show"]
        if not shows:
            reason = "numbered moves require a preceding show command"
        else:
            shown = shows[-1].get("legal_moves", [])
            current = [text for _action, text in legal]
            number = int(requested)
            if shown != current:
                reason = "the last numbered move list is stale; run show again"
            elif not 1 <= number <= len(shown):
                reason = f"move number must be between 1 and {len(shown)}"
            else:
                chosen = shown[number - 1]
                reason = None
    elif requested in by_string:
        chosen, reason = requested, None
    else:
        reason = "unknown or illegal move"
    if chosen is None:
        _append(path, _base_record("move", state, len(legal), accepted=False,
                                   move=requested, reason=reason))
        raise CliError(reason)
    state.apply_action(by_string[chosen])
    next_legal_count = len(_legal(state))
    _append(path, _base_record("move", state, next_legal_count, accepted=True,
                               move=chosen))
    print(f"accepted: {chosen}")
    print(f"solved: {'yes' if state.goal_reached() else 'no'}")


def _parser():
    parser = argparse.ArgumentParser()
    parser.add_argument("--log-dir", type=Path, default=DEFAULT_LOG_DIR)
    sub = parser.add_subparsers(dest="command", required=True)
    new = sub.add_parser("new")
    new.add_argument("--prop", type=int, required=True)
    new.add_argument("--cap", type=int, required=True)
    new.add_argument("--id", required=True)
    new.set_defaults(function=command_new)
    for name, function in (("show", command_show), ("facts", command_facts)):
        item = sub.add_parser(name)
        item.add_argument("--id", required=True)
        item.set_defaults(function=function)
    move = sub.add_parser("move")
    move.add_argument("--id", required=True)
    move.add_argument("move")
    move.set_defaults(function=command_move)
    return parser


def main(argv=None):
    if os.environ.get("PYTHONHASHSEED") != "0" and argv is None:
        env = dict(os.environ)
        env["PYTHONHASHSEED"] = "0"
        os.execve(sys.executable, [sys.executable, str(Path(__file__).resolve()), *sys.argv[1:]], env)
    args = _parser().parse_args(argv)
    try:
        args.function(args)
    except CliError as error:
        print(str(error), file=sys.stderr)
        return 2
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
