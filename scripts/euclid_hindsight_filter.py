#!/usr/bin/env python3
"""Post-process E1 records with explicit triviality labels."""
from __future__ import annotations

import argparse
import glob
import json
from pathlib import Path
import random

import euclid_engine as eng
import euclid_hindsight as hindsight
import euclid_spiel as spiel


def record_moves(record):
    return [(m["tuple"][0], tuple(m["tuple"][1]),
             tuple(sorted(m["tuple"][2].items()))) for m in record["moves"]]


def replay_with_reports(proposition, moves, needed):
    state, _kinds = hindsight.initial_state(proposition)
    reports = {}
    for index in sorted(needed):
        rule, args, bindings = moves[index]
        reports[index] = eng.apply_step(
            state, index + 1, rule, args, dict(bindings))
    return state, reports


def _points(state, proposition, goal):
    initial = {o["name"]: o["kind"]
               for o in hindsight.play.tr.objects_from_hyps(proposition)}
    kinds = spiel._objects(state, initial)
    return [x for x in goal[1:] if isinstance(x, str)
            and kinds.get(x) == "point"]


def _line_sets(state):
    lines = {}
    for fact in state.facts:
        if fact and fact[0] == "on_line":
            lines.setdefault(fact[2], set()).add(fact[1])
    return lines


def _angle_terms(goal):
    kind = goal[0]
    if kind in ("_eq_ang", "eq_ang", "_gt_ang", "gt_ang"):
        return (tuple(goal[1:4]), tuple(goal[4:7]))
    if kind in ("zero_ang", "flat_ang", "right_ang", "_rt_ang"):
        return (tuple(goal[1:4]),)
    return ()


def degenerate(state, proposition, goal):
    lines = _line_sets(state)
    points = set(_points(state, proposition, goal))
    if points and any(points <= members for members in lines.values()):
        return True
    return any(any(set(term) <= members for members in lines.values())
               for term in _angle_terms(goal))


def _stored_goal(goal):
    mapping = {"_eq_len": "eq_len", "_lt_len": "lt_len",
               "_gt_len": "gt_len", "_eq_ang": "eq_ang",
               "_gt_ang": "gt_ang"}
    return (mapping.get(goal[0], goal[0]),) + tuple(goal[1:])


def _metric_relation(fact):
    kind = fact[0].lstrip("_")
    if kind in ("eq_len", "lt_len", "gt_len"):
        left, right = eng.len_atom(*fact[1:3]), eng.len_atom(*fact[3:5])
    elif kind in ("eq_ang", "gt_ang"):
        left, right = eng.ang_atom(*fact[1:4]), eng.ang_atom(*fact[4:7])
    else:
        return None
    if kind.startswith("eq_"):
        return (kind, frozenset((left, right)))
    if kind.startswith("lt_"):
        return (kind.replace("lt_", "gt_"), right, left)
    return (kind, left, right)


def direct(goal, reports, needed):
    if not needed:
        return False
    target = _metric_relation(goal)
    for conclusion in reports[max(needed)]["added"]:
        if _stored_goal(goal) == conclusion:
            return True
        if target is not None and target == _metric_relation(conclusion):
            return True
    return False


def _angle_equivalence(state, left, right):
    if eng.ang_atom(*left) == eng.ang_atom(*right):
        return True
    edges = {}
    for fact in state.facts:
        if fact[0] not in ("rayang", "sameray"):
            continue
        v, x, y, z = fact[1:5]
        one, two = eng.ang_atom(z, v, x), eng.ang_atom(z, v, y)
        edges.setdefault(one, set()).add(two)
        edges.setdefault(two, set()).add(one)
    start, target = eng.ang_atom(*left), eng.ang_atom(*right)
    todo, seen = [start], set()
    while todo:
        atom = todo.pop()
        if atom == target:
            return True
        if atom not in seen:
            seen.add(atom)
            todo.extend(edges.get(atom, ()))
    return False


def naming(state, goal):
    kind = goal[0]
    if kind in ("rayang", "sameray"):
        return True
    if kind in ("_eq_len", "eq_len"):
        return eng.len_atom(*goal[1:3]) == eng.len_atom(*goal[3:5])
    if kind in ("_eq_ang", "eq_ang"):
        return _angle_equivalence(state, goal[1:4], goal[4:7])
    return False


def zero_length(goal):
    """A length comparison containing the identically-zero segment |pp|."""
    if goal[0] not in ("_eq_len", "eq_len", "_lt_len", "lt_len",
                       "_gt_len", "gt_len"):
        return False
    return goal[1] == goal[2] or goal[3] == goal[4]


def classify(record, cache=None):
    moves = record_moves(record)
    needed = tuple(record["needed_move_indices"])
    key = (record["proposition"], record["run_seed"], needed)
    if cache is not None and key in cache:
        state, reports = cache[key]
    else:
        state, reports = replay_with_reports(record["proposition"], moves, needed)
        if cache is not None:
            cache[key] = (state, reports)
    goal = tuple(record["goal"])
    reasons = []
    if degenerate(state, record["proposition"], goal):
        reasons.append("degenerate")
    if direct(goal, reports, needed):
        reasons.append("direct")
    if naming(state, goal):
        reasons.append("naming")
    if zero_length(goal):
        reasons.append("zero-length")
    return reasons


def process(inputs, output):
    cache = {}
    with Path(output).open("w") as stream:
        for path in sorted(glob.glob(inputs)):
            for line in open(path):
                record = json.loads(line)
                record["trivial_reasons"] = classify(record, cache)
                stream.write(json.dumps(record, separators=(",", ":")) + "\n")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--inputs", required=True)
    parser.add_argument("--output", required=True)
    args = parser.parse_args()
    process(args.inputs, args.output)


if __name__ == "__main__":
    main()
