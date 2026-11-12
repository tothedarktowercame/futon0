import random
from pathlib import Path

import metagame_eq as eq
import metagame_lemma as lemma
import metagame_solver as solver


def _sound(theory, checker, runs=200):
    rng = random.Random(20261112)
    a, b = eq.gen("a"), eq.gen("b")
    checked = 0
    for _ in range(runs):
        p = eq.start(theory, eq.Goal(a, b), (a, b))
        for _ in range(8):
            choices = eq.candidates(p)
            if not choices:
                break
            eq.apply(p, rng.choice(choices))
        for fact in eq.facts(p):
            checked += 1
            if not checker(fact.left, fact.right):
                return False, checked
    return True, checked


def _parity(term):
    if term.op == "e":
        return (0, 0)
    if term.op == "gen":
        return (1, 0) if term.args[0] == "a" else (0, 1)
    if term.op == "inv":
        return _parity(term.args[0])
    x, y = _parity(term.args[0]), _parity(term.args[1])
    return ((x[0] + y[0]) % 2, (x[1] + y[1]) % 2)


def test_derived_lemmas_are_sound_and_a_bad_lemma_is_caught():
    for spec in lemma.derived_theories():
        checker = ((lambda x, y: eq.free_group_equal(x, y))
                   if spec.base in ("group", "left_group")
                   else (lambda x, y: _parity(x) == _parity(y)))
        sound, checked = _sound(spec.theory, checker)
        assert sound and checked > 0, spec.name

    bad = lemma._extend("group", "group+commutes",
                        eq.RULE_LIBRARY["commutative"])
    a, b = eq.gen("a"), eq.gen("b")
    p = eq.start(bad.theory, eq.Goal(a, b), (a, b))
    eq.apply(p, eq.Move("mul", (a, b)))
    eq.apply(p, eq.Move("mul", (b, a)))
    assert any(not eq.free_group_equal(f.left, f.right) for f in eq.facts(p))


def test_closure_with_a_lemma_never_adds_a_term():
    a, b = eq.gen("a"), eq.gen("b")
    terms = [eq.mul(a, eq.inv(a)), eq.mul(a, eq.E), eq.mul(a, b),
             eq.inv(eq.mul(a, b)), eq.inv(a), eq.inv(b),
             eq.mul(eq.inv(b), eq.inv(a)), eq.mul(b, a)]
    for spec in lemma.derived_theories():
        p = eq.start(spec.theory, eq.Goal(a, b), (a, b))
        for term in terms:
            for child in reversed(list(eq.subterms(term))):
                if child not in p.ids:
                    p.add_term(child)
        before = len(p.terms)
        eq.closure(p)
        assert len(p.terms) == before, spec.name


def test_base_runner_reproduces_three_eq5_rows():
    root = Path("/home/joe/code/storage/metagame/eq5")
    rows = lemma.load_jsonl(root / "testset.jsonl.gz")
    recorded = {r["index"]: r for r in
                lemma.load_jsonl(root / "results.jsonl.gz")}
    chosen = [(i, row) for i, row in enumerate(rows)
              if not row.get("named")][:3]
    for index, row in chosen:
        base = eq.THEORIES[row["theory"]]
        spec = lemma.Derived("base", row["theory"], base)
        actual = lemma.run_row(spec, index, row)["attempts"]
        assert actual == recorded[index]["attempts"]
