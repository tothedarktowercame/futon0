"""Independent soundness checks for Euclid's linear-arithmetic decider."""

from fractions import Fraction
import itertools
import json
from pathlib import Path
import random

import pyspiel

import euclid_engine as eng
import euclid_play as play
import euclid_spiel as spiel  # registers euclid_book1


def _f(x):
    return x if isinstance(x, Fraction) else Fraction(x)


def _norm(coeffs, const):
    return ({a: _f(c) for a, c in coeffs.items() if c}, _f(const))


def _substitute(rows, variable, expression, constant):
    out = []
    for coeffs, c, op in rows:
        coeffs = dict(coeffs)
        multiplier = coeffs.pop(variable, 0)
        if multiplier:
            for atom, coefficient in expression.items():
                coeffs[atom] = coeffs.get(atom, 0) + multiplier * coefficient
            c += multiplier * constant
        coeffs, c = _norm(coeffs, c)
        if coeffs:
            out.append((coeffs, c, op))
        elif not (c == 0 if op == "=" else c <= 0 if op == "<=" else c < 0):
            return None
    return out


def _exact_sat(encoded_rows):
    """Small independent rational feasibility oracle.

    Equalities are eliminated by a simple full rescan, then inequalities by
    textbook Fourier--Motzkin.  It deliberately shares neither the engine's
    indexed equality eliminator nor its division helper/cache.
    """
    rows = [({a: _f(c) for a, c in items}, _f(const), op)
            for items, const, op in encoded_rows]
    while True:
        equality = next((i for i, row in enumerate(rows) if row[2] == "="), None)
        if equality is None:
            break
        coeffs, const, _ = rows.pop(equality)
        variable = sorted(coeffs)[0]
        divisor = coeffs.pop(variable)
        expression = {a: -c / divisor for a, c in coeffs.items()}
        rows = _substitute(rows, variable, expression, -const / divisor)
        if rows is None:
            return False
    while any(coeffs for coeffs, _const, _op in rows):
        variable = min(a for coeffs, _const, _op in rows for a in coeffs)
        positive, negative, keep = [], [], []
        for coeffs, const, op in rows:
            coeffs = dict(coeffs)
            coefficient = coeffs.pop(variable, 0)
            if coefficient > 0:
                positive.append((coefficient, coeffs, const, op))
            elif coefficient < 0:
                negative.append((-coefficient, coeffs, const, op))
            else:
                keep.append((coeffs, const, op))
        rows = keep
        for pc, p, pk, po in positive:
            for nc, n, nk, no in negative:
                coeffs = {a: c / pc for a, c in p.items()}
                for atom, coefficient in n.items():
                    coeffs[atom] = coeffs.get(atom, 0) + coefficient / nc
                coeffs, const = _norm(coeffs, pk / pc + nk / nc)
                rows.append((coeffs, const, "<" if "<" in (po, no) else "<="))
    return all(const <= 0 if op == "<=" else const < 0
               for _coeffs, const, op in rows)


def _negations(coeffs, const, op):
    if op == "=":
        return [(coeffs, const, "<"),
                ({a: -c for a, c in coeffs.items()}, -const, "<")]
    if op in (">", ">="):
        coeffs, const, op = ({a: -c for a, c in coeffs.items()}, -const,
                             "<" if op == ">" else "<=")
    return [({a: -c for a, c in coeffs.items()}, -const,
             "<" if op == "<=" else "<=")]


def _oracle_confirms(rows, coeffs, const, op):
    return all(not _exact_sat(rows + [eng._row_of(*negation)])
               for negation in _negations(coeffs, const, op))


def _minimal_failure_store():
    # Ten-row reduction of Probe 7's pre-step-5 store.  The two queried
    # lengths are algebraically equal.  Before the fix, integer thirds became
    # nearby floats during equality elimination and `<` incorrectly held.
    L, A = eng.len_atom, eng.ang_atom
    return [
        eng._row_of(eng.lin((L("b", "c"), 1), (L("b", "g"), -1),
                            (L("c", "g"), 1)), 0, "="),
        eng._row_of(eng.lin((L("b", "c"), -1), (L("c", "g"), 1)), 0, "="),
        eng._row_of(eng.lin((L("b", "g"), 1), (L("b", "h"), 1),
                            (L("g", "h"), -1)), 0, "="),
        eng._row_of(eng.lin((L("b", "c"), -1), (L("b", "h"), 1)), 0, "="),
        eng._row_of({A("b", "h", "g"): 1}, 0, "="),
        eng._row_of(eng.lin((L("c", "g"), 1), (L("c", "h"), 1),
                            (L("g", "h"), -1)), 0, "="),
        eng._row_of({A("c", "h", "g"): 1}, 0, "="),
        eng._row_of({L("c", "h"): -1}, 0, "<"),
        eng._row_of(eng.lin((L("b", "e"), 1), (L("b", "g"), -1),
                            (L("e", "g"), 1)), 0, "="),
        eng._row_of(eng.lin((L("b", "e"), -1), (L("b", "h"), 1)), 0, "="),
    ]


def test_fractional_equality_elimination_does_not_invent_strict_order():
    la = eng.LinearArithmetic()
    la.rows = _minimal_failure_store()
    assert eng.la_entails_eq(la, "g", "c", "g", "e")
    assert not eng.la_entails_lt(la, "g", "c", "g", "e")


def _replay(record):
    state = pyspiel.load_game(
        "euclid_book1(proposition=2,move_cap=12,dense_actions=true)"
    ).new_initial_state()
    for rule, args, bindings in record["moves"]:
        state.apply_action_tuple(
            (rule, tuple(args), tuple(tuple(binding) for binding in bindings)))
    return state


def _probe7_records(name):
    path = (Path(__file__).resolve().parents[1] /
            "holes/excursions/E-euclid-tutor-data/spiel/rc7" / name)
    return [json.loads(line) for line in path.read_text().splitlines()]


def test_probe7_five_move_false_win_is_now_consistent_and_unsolved():
    record = min(_probe7_records("minimized-wins.jsonl"),
                 key=lambda item: len(item["moves"]))
    state = _replay(record)
    assert eng.contradictory(state.engine_state) == (False, None)
    assert not state.goal_reached()


def test_probe7_second_original_false_win_is_now_consistent_and_unsolved():
    state = _replay(_probe7_records("random-wins.jsonl")[1])
    assert eng.contradictory(state.engine_state) == (False, None)
    assert not state.goal_reached()


def test_seeded_move_stores_entail_only_oracle_certified_queries():
    rng = random.Random(20261111)
    checked = 0
    for proposition in (1, 2, 3, 5):
        state = play._start_state(proposition)
        kinds = {obj["name"]: obj["kind"]
                 for obj in play.tr.objects_from_hyps(proposition)}
        for ply in range(3):
            state_action = spiel.sample_action(
                proposition, state, kinds, ply, rng=rng)
            eng.apply_step(state, ply + 1, state_action[0], state_action[1],
                           dict(state_action[2]))
            atoms = sorted({atom for row in state.la.rows for atom, _ in row[0]
                            if atom[0] == "len"})
            for left, right in itertools.islice(itertools.combinations(atoms, 2), 6):
                coeffs = eng.lin((left, 1), (right, -1))
                for op in ("=", "<", "<="):
                    if state.la.entails(coeffs, 0, op):
                        assert _oracle_confirms(state.la.rows, coeffs, 0, op)
                        checked += 1
    assert checked >= 20


def test_independent_oracle_detects_the_pre_fix_false_entailment(monkeypatch):
    def broken_divide(a, b):
        if type(a) is int and type(b) is int and a % b == 0:
            return a // b
        return a / b

    monkeypatch.setattr(eng, "_exdiv", broken_divide)
    eng._ENT_GLOBAL.clear()
    la = eng.LinearArithmetic()
    la.rows = _minimal_failure_store()
    coeffs = eng.lin((eng.len_atom("g", "c"), 1),
                     (eng.len_atom("g", "e"), -1))
    assert la.entails(coeffs, 0, "<")
    assert not _oracle_confirms(la.rows, coeffs, 0, "<")
