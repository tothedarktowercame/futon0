import random
from fractions import Fraction

import metagame_eq as eq
import metagame_horn as horn


def partition(position):
    groups = {}
    for i, term in enumerate(position.terms):
        groups.setdefault(position.find(i), set()).add(term)
    return frozenset(frozenset(x) for x in groups.values())


def test_equational_closure_matches_legacy_12000_positions():
    compared = 0
    for name, seed in (("group", 301), ("left_group", 302), ("exponent_two", 303)):
        rng = random.Random(seed)
        for _ in range(500):
            old = eq.start(name, eq.Goal(eq.gen("a"), eq.gen("b")))
            new = horn.start(name, (), ("a", "b"))
            for _ in range(8):
                move = rng.choice(eq.candidates(old))
                eq.apply(old, move); horn.apply(new, move)
                assert partition(old) == partition(new)
                compared += 1
    print(f"equational positions compared: {compared}")
    assert compared == 12000


def rational_ext(q, a, b, c):
    return a + (1 if a > q else -1) * abs(b - c)


def true_atom(atom, coordinates):
    value = lambda t: coordinates[t]
    if isinstance(atom, horn.Equation): return value(atom.left) == value(atom.right)
    if isinstance(atom, horn.Neq): return value(atom.left) != value(atom.right)
    args = tuple(map(value, atom.args))
    if atom.name == "B":
        a,b,c = args; return a < b < c or c < b < a
    if atom.name == "E":
        a,b,c,d = args; return abs(a-b) == abs(c-d)
    raise AssertionError(atom)


def test_line_soundness_1000_seeded_runs():
    rng = random.Random(20261112)
    checked = 0
    for run in range(1000):
        count = rng.randint(3, 5)
        values = rng.sample(range(-20, 21), count)
        givens = tuple(horn.gen(f"p{run}_{i}") for i in range(count))
        coordinates = dict(zip(givens, map(Fraction, values)))
        hypotheses = tuple(horn.Neq(a,b) for i,a in enumerate(givens)
                           for b in givens[i+1:])
        position = horn.start("line", hypotheses, givens)
        for step in range(8):
            reps = [position.terms[i] for i in range(len(position.terms))
                    if position.find(i) == i]
            while True:
                q, a = rng.sample(reps, 2)
                if step == 0:
                    b, c = rng.sample(reps, 2)
                else:
                    b = c = rng.choice(reps)
                args = (q, a, b, c)
                move = horn.Move("ext", args)
                if horn._valid_move(position, move): break
            coordinates[move.result] = rational_ext(*(coordinates[x] for x in args))
            horn.apply(position, move)
        for fact in horn.facts(position):
            checked += 1
            assert true_atom(fact, coordinates), (run, fact)
    print(f"line facts checked against Q: {checked}")
    assert checked > 90000


def test_guard_and_zero_length_extension():
    q,a,b = map(horn.gen, "qab")
    p = horn.start("line", (), (q,a,b))
    assert not any(m.op == "ext" for m in horn.candidates(p))
    p = horn.start("line", (horn.Neq(q,a),), (q,a,b))
    assert horn.Move("ext", (q,a,b,b)) in horn.candidates(p)
    horn.apply(p, horn.Move("ext", (q,a,b,b)))
    assert p.find(p.ids[horn.Term("ext", (q,a,b,b))]) == p.find(p.ids[a])


def test_A15_waits_for_both_premises():
    a,b,c,d = map(horn.gen, "abcd")
    first = horn.Rel("B", (a,b,d)); second = horn.Rel("B", (b,c,d))
    p = horn.start("line", (first,), (a,b,c,d))
    assert horn.Rel("B", (a,b,c)) not in horn.facts(p)
    p.relations.add(("B", tuple(p.find(p.ids[x]) for x in second.args)))
    horn.closure(p)
    assert horn.Rel("B", (a,b,c)) in horn.facts(p)


def test_inconsistency_is_recorded_not_raised():
    a,b = map(horn.gen, "ab")
    p = horn.start("line", (horn.Rel("B", (a,b,a)),), (a,b))
    assert p.inconsistent


def do_move(p, args):
    move = horn.Move("ext", args); assert move in horn.candidates(p)
    assert horn.apply(p, move)
    return move.result


def test_H1_to_H4_and_existential_goal_and_needed():
    q,a,b,c,d,e = map(horn.gen, "qabcde")
    hypotheses = (horn.Neq(q,a), horn.Neq(b,c), horn.Neq(d,e))
    opening = horn.start("line", hypotheses, (q,a,b,c,d,e))
    p = opening.copy()
    xterm = horn.Term("ext", (q,a,b,c))
    h1 = horn.Goal((horn.Rel("B", (q,a,xterm)), horn.Rel("E", (a,xterm,b,c))))
    assert not horn.entails(p, h1)
    x = do_move(p, (q,a,b,c)); assert horn.entails(p, h1)
    yterm = horn.Term("ext", (a,x,d,e))
    h4 = horn.Goal((horn.Rel("B", (q,a,yterm)), horn.Rel("B", (q,x,yterm))))
    assert not horn.entails(p, h4)
    y = do_move(p, (a,x,d,e)); assert horn.entails(p, h4)

    witness = horn.var("w")
    assert horn.entails(p, horn.Goal((horn.Rel("B", (q,a,witness)),
                                     horn.Rel("E", (a,witness,b,c))), ("w",)))

    # H2/H3 are the same two-extension pattern with equal copied lengths.
    p2 = horn.start("line", (horn.Neq(q,a), horn.Neq(b,c)), (q,a,b,c))
    x2 = do_move(p2, (q,a,b,c)); y2 = do_move(p2, (a,x2,b,c))
    assert horn.entails(p2, horn.Goal((horn.Rel("B", (q,a,x2)),
                                      horn.Rel("B", (a,x2,y2)),
                                      horn.Rel("E", (x2,y2,b,c)))))
    p3 = horn.start("line", (horn.Neq(q,a),), (q,a))
    x3 = do_move(p3, (q,a,q,a)); y3 = do_move(p3, (a,x3,q,a))
    assert horn.entails(p3, horn.Goal((horn.Rel("E", (q,a,a,x3)),
                                      horn.Rel("E", (a,x3,x3,y3)))))

    irrelevant = horn.Move("ext", (q,a,q,a))
    moves = [horn.Move("ext", (q,a,b,c)), irrelevant,
             horn.Move("ext", (a,x,d,e))]
    assert horn.needed("line", opening, moves, h4) == [moves[0], moves[2]]


def test_rule_statements_and_A17_A18_absent():
    names = {row[0] for row in horn.rule_statements("line")}
    assert "A16" in names and "A17" not in names and "A18" not in names
