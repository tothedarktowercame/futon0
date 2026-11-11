#!/usr/bin/env python3
"""Ask cvc5 whether a Euclid-game goal follows from raw move facts.

The inference axioms are extracted verbatim from LeanEuclid's
SystemE/Meta/Smt/EuclidTheory.lean.  Crucially, this module feeds the solver
only theorem hypotheses and each construction's explicit ``added`` facts;
the Python engine's forward closure is used to validate/replay moves but is
not asserted in SMT.
"""

from __future__ import annotations

import argparse
import json
import re
import subprocess
import sys
import time
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
    sys.path.insert(0, str(SCRIPT_DIR))

import euclid_engine as eng  # noqa: E402
import euclid_play as play  # noqa: E402
import euclid_spiel  # noqa: E402,F401
import euclid_trace as trace  # noqa: E402
import pyspiel  # noqa: E402

LEAN_THEORY = Path.home() / "code/LeanEuclid/SystemE/Meta/Smt/EuclidTheory.lean"
CVC5 = Path.home() / ".local/bin/cvc5"


def _q(name):
    return "|" + str(name).replace("|", "_") + "|"


def lean_euclid_axioms(path=LEAN_THEORY):
    """Extract the literal SMT assertions used by LeanEuclid, unchanged."""
    source = Path(path).read_text(encoding="utf-8")
    literals = re.findall(r'Smt\.Term\.literalT\s*"(.*?)"\s*\)', source, re.S)
    # The file also uses literalT for the five sort abbreviations above
    # euclidTheory.  Only parenthesised literals are SMT commands/axioms.
    return [x for x in literals if x.lstrip().startswith("(")]


def _expand_fact(fact):
    k, *a = fact
    if k == "dpl":
        p, q, line = a
        return [("neq", p, q), ("on_line", p, line), ("on_line", q, line)]
    if k == "opposing_sides":
        p, q, line = a
        return [("not_on_line", p, line), ("not_on_line", q, line),
                ("not_same_side", p, q, line)]
    if k == "outside":
        p, circle = a
        return [("not_inside", p, circle), ("not_on_circle", p, circle)]
    return [tuple(fact)]


def fact_smt(fact):
    k, *a = fact
    q = [_q(x) for x in a]
    rel = {
        "on_line": "OnL", "on_circle": "OnC", "inside": "Inside",
        "centre": "Centre", "same_side": "SameSide",
        "intersects_ll": "IntersectsLL", "intersects_lc": "IntersectsLC",
        "intersects_cc": "IntersectsCC", "between": "Between",
    }
    if k in rel:
        return f"({rel[k]} {' '.join(q)})"
    if k == "neq": return f"(not (= {q[0]} {q[1]}))"
    if k == "peq": return f"(= {q[0]} {q[1]})"
    if k == "neq_line": return f"(not (= {q[0]} {q[1]}))"
    if k == "peq_line": return f"(= {q[0]} {q[1]})"
    if k == "not_on_line": return f"(not (OnL {q[0]} {q[1]}))"
    if k == "not_on_circle": return f"(not (OnC {q[0]} {q[1]}))"
    if k == "not_inside": return f"(not (Inside {q[0]} {q[1]}))"
    if k == "not_same_side": return f"(not (SameSide {q[0]} {q[1]} {q[2]}))"
    if k == "not_between": return f"(not (Between {q[0]} {q[1]} {q[2]}))"
    if k == "not_outside":
        return f"(or (Inside {q[0]} {q[1]}) (OnC {q[0]} {q[1]}))"
    if k in ("eq_len", "_eq_len"):
        return f"(= (SegmentPP {q[0]} {q[1]}) (SegmentPP {q[2]} {q[3]}))"
    if k in ("lt_len", "_lt_len", "gt_len", "_gt_len"):
        op = "<" if "lt" in k else ">"
        return f"({op} (SegmentPP {q[0]} {q[1]}) (SegmentPP {q[2]} {q[3]}))"
    if k == "pos_len": return f"(> (SegmentPP {q[0]} {q[1]}) 0.0)"
    if k in ("eq_ang", "_eq_ang", "gt_ang", "_gt_ang"):
        op = "=" if "eq" in k else ">"
        return f"({op} (AnglePPP {' '.join(q[:3])}) (AnglePPP {' '.join(q[3:6])}))"
    if k in ("right_ang", "rt_ang", "_rt_ang"):
        return f"(= (AnglePPP {' '.join(q[:3])}) RightAngle)"
    if k in ("zero_ang", "_zero_ang"):
        return f"(= (AnglePPP {' '.join(q[:3])}) 0.0)"
    if k == "flat_ang":
        return f"(= (AnglePPP {' '.join(q[:3])}) (+ RightAngle RightAngle))"
    raise ValueError(f"unsupported fact for SMT: {fact!r}")


def _initial_raw(prop):
    hyps, _ = trace.hyps_and_steps(prop)
    return [tuple(f) for f, why in hyps if why != "h-sm"]


def new_state(prop, cap):
    game = pyspiel.load_game(
        f"euclid_book1(proposition={prop},move_cap={cap},dense_actions=true)")
    return game.new_initial_state()


def apply_grounded(state, grounded):
    rule, args, bindings = grounded
    report = eng.apply_step(state.engine_state, len(state.history_moves) + 1,
                            rule, args, dict(bindings))
    state.history_moves.append((rule, args, tuple(bindings)))
    state._actions = state._action_by_id = None
    return [tuple(f) for f in report["added"]]


def action_by_text(state, text):
    legal = state.legal_actions()
    matches = [state._action_by_id[a] for a in legal
               if state.action_to_string(0, a) == text]
    if len(matches) != 1:
        raise ValueError(f"recorded action is not uniquely legal: {text}")
    return matches[0]


def replay_log(path):
    records = [json.loads(x) for x in Path(path).read_text().splitlines() if x.strip()]
    first = records[0]
    prop, cap = int(first["proposition"]), int(first["move_cap"])
    state, raw = new_state(prop, cap), _initial_raw(prop)
    for row in records[1:]:
        if row.get("command") == "move" and row.get("accepted"):
            raw += apply_grounded(state, action_by_text(state, row["move"]))
    return prop, state, raw


def replay_grounded(prop, moves, cap=20):
    state, raw = new_state(prop, cap), _initial_raw(prop)
    for rule, args in moves:
        state.legal_actions()
        matches = [g for g in state._action_by_id.values()
                   if g[0] == rule and g[1] == tuple(args)]
        if len(matches) != 1:
            raise ValueError((rule, args, len(matches)))
        raw += apply_grounded(state, matches[0])
    return state, raw


def build_smt(prop, state, raw_facts):
    objects = {x["name"]: x["kind"] for x in state._object_list()}
    givens = {x["name"] for x in trace.objects_from_hyps(prop)}
    witness = []
    for goal in play.GOALS[prop]:
        for x in goal[1:]:
            if isinstance(x, str) and x not in givens and x not in witness:
                witness.append(x)
    lines = ["(set-logic ALL)", "(declare-sort Point 0)",
             "(declare-sort Line 0)", "(declare-sort Circle 0)",
             "(declare-fun Between (Point Point Point) Bool)",
             "(declare-fun OnL (Point Line) Bool)",
             "(declare-fun OnC (Point Circle) Bool)",
             "(declare-fun Inside (Point Circle) Bool)",
             "(declare-fun Centre (Point Circle) Bool)",
             "(declare-fun SameSide (Point Point Line) Bool)",
             "(declare-fun IntersectsLL (Line Line) Bool)",
             "(declare-fun IntersectsLC (Line Circle) Bool)",
             "(declare-fun IntersectsCC (Circle Circle) Bool)",
             "(declare-fun SegmentPP (Point Point) Real)",
             "(declare-fun AnglePPP (Point Point Point) Real)",
             "(declare-fun AreaPPP (Point Point Point) Real)",
             "(declare-const RightAngle Real)"]
    lines += [f"(assert {axiom})" for axiom in lean_euclid_axioms()]
    for name, kind in sorted(objects.items()):
        lines.append(f"(declare-const {_q(name)} {kind.title()})")
    seen = set()
    for fact in raw_facts:
        for expanded in _expand_fact(fact):
            try: assertion = fact_smt(expanded)
            except ValueError: continue
            if assertion not in seen:
                lines.append(f"(assert {assertion})")
                seen.add(assertion)
    goal_parts = [fact_smt(g) for g in play.GOALS[prop]]
    if witness:
        # LeanEuclid's quantified theory routinely makes cvc5 return
        # unknown when another existential is added.  The notation game's
        # goal checker ranges witnesses over constructed, non-given points,
        # so enumerate that same finite domain (one witness in Book I.1--5).
        if len(witness) != 1:
            raise ValueError("finite witness enumeration currently supports one point")
        w = witness[0]
        candidates = sorted(name for name, kind in objects.items()
                            if kind == "point" and name not in givens)
        cases = []
        for candidate in candidates:
            instantiated = [tuple(candidate if x == w else x for x in g)
                            for g in play.GOALS[prop]]
            parts = [fact_smt(g) for g in instantiated]
            cases.append(parts[0] if len(parts) == 1
                         else "(and " + " ".join(parts) + ")")
        goal = "false" if not cases else (cases[0] if len(cases) == 1
                                           else "(or " + " ".join(cases) + ")")
    else:
        goal = goal_parts[0] if len(goal_parts) == 1 else "(and " + " ".join(goal_parts) + ")"
    lines += [f"(assert (not {goal}))", "(check-sat)"]
    return "\n".join(lines) + "\n"


def run_solver(text, solver=CVC5, timeout=20):
    started = time.monotonic()
    try:
        result = subprocess.run([str(solver), "--lang=smt2", f"--tlimit={timeout*1000}"],
                                input=text, text=True, capture_output=True,
                                timeout=timeout + 2)
    except subprocess.TimeoutExpired:
        return "timeout", time.monotonic() - started, ""
    answer = (result.stdout.strip().splitlines() or ["unknown"])[0]
    verdict = "follows" if answer == "unsat" else answer
    return verdict, time.monotonic() - started, result.stderr.strip()


I2_WITNESS = [
    ("line_from_points", ("a", "b")),
    ("proposition_1", ("a", "b", "L_1")),
    ("line_from_points", ("d_2", "a")),
    ("line_from_points", ("d_2", "b")),
    ("circle_from_points", ("b", "c")),
    ("intersection_circle_line_extending_points", ("C_5", "L_4", "b", "d_2")),
    ("circle_from_points", ("d_2", "g_6")),
    ("intersection_circle_line_extending_points", ("C_7", "L_3", "a", "d_2")),
]


def main(argv=None):
    parser = argparse.ArgumentParser()
    parser.add_argument("--log", type=Path)
    parser.add_argument("--i2-prefix", type=int)
    parser.add_argument("--out", type=Path)
    parser.add_argument("--timeout", type=int, default=20)
    args = parser.parse_args(argv)
    if args.log:
        prop, state, raw = replay_log(args.log)
    elif args.i2_prefix is not None:
        prop = 2
        state, raw = replay_grounded(2, I2_WITNESS[:args.i2_prefix], 10)
    else:
        parser.error("give --log or --i2-prefix")
    text = build_smt(prop, state, raw)
    if args.out:
        args.out.write_text(text)
    verdict, elapsed, error = run_solver(text, timeout=args.timeout)
    print(json.dumps({"proposition": prop, "moves": len(state.history_moves),
                      "engine": state.goal_reached(), "smt": verdict,
                      "seconds": round(elapsed, 4), "stderr": error}))


if __name__ == "__main__":
    main()
