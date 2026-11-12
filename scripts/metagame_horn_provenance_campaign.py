#!/usr/bin/env python3
"""Seeded HORN-3 provenance/oracle campaign; one shard per process."""

import argparse
import json
import random
import time
from pathlib import Path

import metagame_horn as horn
import metagame_horn_setter as setter


def run(kind, first, count):
    deltas = {}
    facts_checked = runs = 0
    started = time.monotonic()
    for index in range(first, first + count):
        rng = random.Random(202611120000 + index + (0 if kind == "line" else 1000000))
        k = rng.randint(4, 6)
        if kind == "line":
            n = rng.randint(3, 4); opening = setter.opening(n)
        else:
            opening = horn.start("group", (), ("a", "b"))
        position = opening.copy(); moves = []
        for _ in range(k):
            choices = horn.candidates(position)
            if not choices: break
            move = rng.choice(choices); horn.apply(position, move); moves.append(move)
        initial = set(horn.facts(opening))
        new_facts = [fact for fact in horn.facts(position) if fact not in initial]
        for fact in new_facts:
            members = [fact]; goal = horn.Goal((fact,))
            new = horn.needed(kind, opening, moves, goal)
            old = horn.needed_by_replay(kind, opening, moves, goal)
            proved, _ = horn.replay(kind, opening, new)
            assert all(horn.entails(proved, horn.Goal((member,)))
                       for member in members), (index, fact, new)
            for move in new:
                trial = [x for x in new if x != move]
                trial_position, _ = horn.replay(kind, opening, trial)
                assert all(not horn.entails(trial_position, horn.Goal((member,)))
                           for member in members), (index, fact, move)
            delta = len(new) - len(old)
            deltas[str(delta)] = deltas.get(str(delta), 0) + len(members)
            facts_checked += len(members)
        runs += 1
    return {"kind": kind, "first": first, "runs": runs,
            "facts_checked": facts_checked, "size_delta": deltas,
            "elapsed": time.monotonic() - started}


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--kind", choices=("line", "group"), required=True)
    parser.add_argument("--first", type=int, required=True)
    parser.add_argument("--count", type=int, required=True)
    parser.add_argument("--out", type=Path, required=True)
    args = parser.parse_args()
    result = run(args.kind, args.first, args.count)
    args.out.parent.mkdir(parents=True, exist_ok=True)
    args.out.write_text(json.dumps(result, indent=2, sort_keys=True) + "\n")
    print(json.dumps(result, sort_keys=True))


if __name__ == "__main__":
    main()
