#!/usr/bin/env python3
"""Hindsight theorem setter for the guarded-Horn line game.

Only the public metagame_horn game calls are used to construct, replay, and
test positions.  The small amount of syntax handling here is for stable keys,
labels, and JSON records; it does not participate in closure.
"""

from __future__ import annotations

import argparse
import json
import random
import signal
import time
from dataclasses import dataclass
from itertools import permutations
from pathlib import Path

import metagame_horn as horn


@dataclass
class GoalRecord:
    givens: int
    k: int
    seed: int
    goal: horn.Atom
    needed: list[horn.Move]
    labels: tuple[str, ...]

    def json(self):
        return {"givens": self.givens, "K": self.k, "seed": self.seed,
                "goal": atom_data(self.goal),
                "needed": [move_data(x) for x in self.needed],
                "needed_size": len(self.needed), "labels": list(self.labels),
                "canonical_key": canonical_key(self.goal, self.givens)}


class CellDeadline(Exception):
    pass


def _deadline(_signum, _frame):
    raise CellDeadline


def givens(n):
    return tuple(horn.gen(chr(ord("a") + i)) for i in range(n))


def hypotheses(points):
    return tuple(horn.Neq(a, b) for i, a in enumerate(points)
                 for b in points[i + 1:])


def opening(n):
    points = givens(n)
    return horn.start("line", hypotheses(points), points)


def term_data(term):
    if term.op == "gen":
        return {"op": "gen", "name": str(term.args[0])}
    return {"op": term.op, "args": [term_data(x) for x in term.args]}


def atom_data(atom):
    if isinstance(atom, horn.Rel):
        return {"kind": "relation", "name": atom.name,
                "args": [term_data(x) for x in atom.args]}
    if isinstance(atom, horn.Neq):
        return {"kind": "neq", "left": term_data(atom.left),
                "right": term_data(atom.right)}
    return {"kind": "equation", "left": term_data(atom.left),
            "right": term_data(atom.right)}


def move_data(move):
    return {"op": move.op, "args": [term_data(x) for x in move.args]}


def term_from_data(value):
    if value["op"] == "gen":
        return horn.gen(value["name"])
    return horn.Term(value["op"], tuple(term_from_data(x) for x in value["args"]))


def atom_from_data(value):
    if value["kind"] == "relation":
        return horn.Rel(value["name"], tuple(term_from_data(x) for x in value["args"]))
    left, right = term_from_data(value["left"]), term_from_data(value["right"])
    return horn.Neq(left, right) if value["kind"] == "neq" else horn.Equation(left, right)


def move_from_data(value):
    return horn.Move(value["op"], tuple(term_from_data(x) for x in value["args"]))


def _serial_term(term, rename):
    if term.op == "gen":
        return rename[str(term.args[0])]
    return f"{term.op}(" + ",".join(_serial_term(x, rename)
                                     for x in term.args) + ")"


def _serial_atom(atom, rename):
    if isinstance(atom, horn.Rel):
        return f"{atom.name}(" + ",".join(_serial_term(x, rename)
                                           for x in atom.args) + ")"
    sides = sorted((_serial_term(atom.left, rename),
                    _serial_term(atom.right, rename)))
    return ("neq" if isinstance(atom, horn.Neq) else "eq") + \
        f"({sides[0]},{sides[1]})"


def canonical_key(atom, n):
    """Canonicalize a ground goal under every renaming of its n givens."""
    names = sorted(_generator_names(atom))
    # A goal need not mention every opening point.  Canonicalizing the names
    # it does mention is exactly the quotient induced by all n-point
    # permutations, without manufacturing irrelevant names.
    return min(_serial_atom(atom, dict(zip(names, order)))
               for order in permutations((f"g{i}" for i in range(n)), len(names)))


def _generator_names(atom):
    names = set()
    terms = atom.args if isinstance(atom, horn.Rel) else (atom.left, atom.right)
    def visit(term):
        if term.op == "gen":
            names.add(str(term.args[0]))
        else:
            for child in term.args:
                visit(child)
    for term in terms:
        visit(term)
    return names


def replay(n, moves):
    position = opening(n)
    for move in moves:
        horn.apply(position, move)
    return position


def _is_image(goal, other):
    if isinstance(goal, horn.Neq) and isinstance(other, horn.Neq):
        return goal.left == other.right and goal.right == other.left
    if isinstance(goal, horn.Rel) and isinstance(other, horn.Rel):
        if goal.name == other.name == "B":
            return goal.args == tuple(reversed(other.args))
        if goal.name == other.name == "E":
            a, b, c, d = other.args
            return goal.args in ((b, a, a, b), (b, a, d, c),
                                 (c, d, a, b), (d, c, b, a))
    return False


def symmetric_keys(goals, n):
    out = set()
    for goal in goals:
        if any(goal != other and _is_image(goal, other) for other in goals):
            out.add(canonical_key(goal, n))
    return out


def _plain_ground(term):
    if term.op == "gen":
        return ("gen", term.args[0])
    return (term.op, *(_plain_ground(x) for x in term.args))


def _plain_ground_atom(atom):
    if isinstance(atom, horn.Rel):
        return (atom.name, *(_plain_ground(x) for x in atom.args))
    if isinstance(atom, horn.Neq):
        return ("neq", _plain_ground(atom.left), _plain_ground(atom.right))
    return ("=", _plain_ground(atom.left), _plain_ground(atom.right))


def _unify(pattern, ground, env):
    if not isinstance(pattern, tuple) or not isinstance(ground, tuple):
        return env if pattern == ground else None
    if pattern and pattern[0] == "var":
        name = str(pattern[1])
        if name in env and env[name] != ground:
            return None
        return {**env, name: ground}
    if len(pattern) != len(ground) or pattern[0] != ground[0]:
        return None
    state = env
    for left, right in zip(pattern[1:], ground[1:]):
        state = _unify(left, right, state)
        if state is None:
            return None
    return state


def _rule_instance(rule, goal, known):
    """Whether one displayed Horn rule has a ground instance for goal.

    This is labeling only.  Closure remains wholly in metagame_horn.
    """
    _name, premises, conclusion = rule
    env = _unify(conclusion, _plain_ground_atom(goal), {})
    if env is None:
        return False
    states = [env]
    for premise in premises:
        next_states = []
        for state in states:
            for fact in known:
                found = _unify(premise, fact, state)
                if found is not None:
                    next_states.append(found)
        states = next_states
        if not states:
            return False
    return bool(states)


def direct_label(n, goal, needed):
    """Whether goal is an immediate rule conclusion of the last needed move.

    These are precisely the three strict-interface rules whose conclusion
    mentions the newly constructed ``ext`` term.  A1/A14/neq symmetry is a
    separate label; consequences requiring another Horn rule are theorem
    yield even when their proof has only one construction.
    """
    if not needed:
        return False
    move = needed[-1]
    q, a, b, c = move.args
    result = move.result
    if isinstance(goal, horn.Rel) and goal.name == "E":
        if goal.args == (a, result, b, c):
            return True
        u, v, w, z = goal.args
        return (u, v) == (z, w) and result in goal.args  # A1 at the new node
    if isinstance(goal, horn.Rel) and goal.name == "B":
        return goal.args == (q, a, result)
    if isinstance(goal, horn.Equation) and b == c:
        return {goal.left, goal.right} == {result, a}
    return False


def _walk_terms(value):
    if not isinstance(value, tuple):
        return
    if value and value[0] in ("gen", "ext"):
        yield value
        for child in value[1:]:
            yield from _walk_terms(child)
    else:
        for child in value[1:]:
            yield from _walk_terms(child)


def classify_run(n, k, seed):
    rng = random.Random(seed)
    position = opening(n)
    initial = {canonical_key(x, n) for x in horn.facts(position)}
    moves, closure_times = [], []
    for _ in range(k):
        choices = horn.candidates(position)
        if not choices:
            break
        move = rng.choice(choices)
        started = time.perf_counter()
        horn.apply(position, move)
        closure_times.append(time.perf_counter() - started)
        moves.append(move)
    new_goals = [x for x in horn.facts(position)
                 if canonical_key(x, n) not in initial]
    symmetric = symmetric_keys(new_goals, n)
    records = []
    for goal in new_goals:
        proof = horn.needed("line", opening(n), moves, horn.Goal((goal,)))
        labels = []
        if direct_label(n, goal, proof): labels.append("direct")
        if canonical_key(goal, n) in symmetric: labels.append("symmetric")
        if isinstance(goal, horn.Neq): labels.append("neq")
        records.append(GoalRecord(n, k, seed, goal, proof, tuple(labels)))
    return records, closure_times


def run_cell(n, k, seconds, base_seed, out):
    deadline = time.monotonic() + seconds
    old = signal.signal(signal.SIGALRM, _deadline)
    signal.setitimer(signal.ITIMER_REAL, seconds)
    runs = 0
    all_records = {}
    closures = []
    timed_out = False
    started = time.monotonic()
    try:
        while time.monotonic() < deadline:
            records, measured = classify_run(n, k, base_seed + runs)
            closures.extend(measured)
            for record in records:
                all_records.setdefault(canonical_key(record.goal, n), record)
            runs += 1
            if measured and max(measured) > 10:
                break
    except CellDeadline:
        timed_out = True
    finally:
        signal.setitimer(signal.ITIMER_REAL, 0)
        signal.signal(signal.SIGALRM, old)
    elapsed = time.monotonic() - started
    result = {"givens": n, "K": k, "runs": runs, "elapsed": elapsed,
              "timed_out": timed_out,
              "closure_seconds": closures,
              "stopped_over_10s": any(x > 10 for x in closures),
              "records": [x.json() for x in all_records.values()]}
    out.parent.mkdir(parents=True, exist_ok=True)
    out.write_text(json.dumps(result, indent=2, sort_keys=True) + "\n")
    return result


def main(argv=None):
    parser = argparse.ArgumentParser()
    parser.add_argument("--givens", type=int, choices=(3, 4), required=True)
    parser.add_argument("--k", type=int, choices=(4, 6, 8), required=True)
    parser.add_argument("--seconds", type=float, default=120)
    parser.add_argument("--seed", type=int, default=2026111200)
    parser.add_argument("--out", type=Path, required=True)
    args = parser.parse_args(argv)
    result = run_cell(args.givens, args.k, args.seconds, args.seed, args.out)
    print(json.dumps({k: v for k, v in result.items() if k != "records"},
                     sort_keys=True))


if __name__ == "__main__":
    main()
