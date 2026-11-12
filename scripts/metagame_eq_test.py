import random

import metagame_eq as eq


a, b, c = map(eq.gen, "abc")


def bare(rules, *terms):
    p = eq.Position(eq.Theory(frozenset(rules)))
    for t in terms:
        eq._add_tree(p, t)
    eq.closure(p)
    return p


def test_congruence_fires_and_requires_both_operation_terms():
    p = bare([], a, b, eq.inv(a), eq.inv(b))
    p.union(p.ids[a], p.ids[b]); eq.closure(p)
    assert p.find(p.ids[eq.inv(a)]) == p.find(p.ids[eq.inv(b)])
    q = bare([], a, b, eq.inv(a))
    q.union(q.ids[a], q.ids[b]); eq.closure(q)
    assert eq.inv(b) not in q.ids


def test_each_law_fires_and_does_not_create_missing_terms():
    cases = [
        ("left_identity", eq.mul(eq.E, a), a, [eq.E, a]),
        ("right_identity", eq.mul(a, eq.E), a, [eq.E, a]),
        ("left_inverse", eq.mul(eq.inv(a), a), eq.E, [eq.E, a, eq.inv(a)]),
        ("right_inverse", eq.mul(a, eq.inv(a)), eq.E, [eq.E, a, eq.inv(a)]),
        ("square_is_identity", eq.mul(a, a), eq.E, [eq.E, a]),
    ]
    for rule, lhs, rhs, absent_terms in cases:
        p = bare([rule], *absent_terms, lhs)
        assert p.find(p.ids[lhs]) == p.find(p.ids[rhs])
        q = bare([rule], *absent_terms)
        assert lhs not in q.ids
    lhs, rhs = eq.mul(eq.mul(a, b), c), eq.mul(a, eq.mul(b, c))
    p = bare(["assoc"], eq.E, a, b, c, eq.mul(a, b), eq.mul(b, c), lhs, rhs)
    assert p.find(p.ids[lhs]) == p.find(p.ids[rhs])
    q = bare(["assoc"], eq.E, a, b, c, eq.mul(a, b), lhs)
    assert eq.mul(b, c) not in q.ids and rhs not in q.ids


def test_alias_result_is_not_a_candidate():
    p = bare(["left_identity"], eq.E, a, eq.mul(eq.E, a))
    assert eq.Move("mul", (eq.E, a)) not in eq.candidates(p)
    assert all(m.result != eq.inv(eq.mul(eq.E, a)) for m in eq.candidates(p)
               if m.op == "inv" and m.args == (a,))


def test_group_closure_is_sound_against_free_group():
    rng = random.Random(20261112)
    checked = 0
    goal = eq.Goal(a, b)
    for _ in range(2000):
        p = eq.start("group", goal)
        for _ in range(8):
            cs = eq.candidates(p)
            if not cs:
                break
            eq.apply(p, rng.choice(cs))
        for fact in eq.facts(p):
            checked += 1
            assert eq.free_group_equal(fact.left, fact.right), fact
    print(f"independently checked {checked} closure equalities")
    assert checked > 1000


def test_needed_removes_irrelevant_moves_and_replays():
    x, y = eq.gen("x"), eq.gen("y")
    ee, ie = eq.mul(eq.E, eq.E), eq.inv(eq.E)
    th = eq.Theory(frozenset(),
                   (eq.Goal(a, ee), eq.Goal(ee, ie), eq.Goal(ie, b)))
    goal = eq.Goal(a, b)
    p = eq.start(th, goal, [a, b])
    moves = [eq.Move("inv", (a,)), eq.Move("mul", (a, a)),
             eq.Move("mul", (eq.E, eq.E)), eq.Move("inv", (eq.E,))]
    keep = eq.needed(th, p, moves, goal)
    assert keep == moves[2:]
    fresh, kept = eq.replay(th, goal, keep, [a, b])
    assert kept == keep and eq.entails(fresh, goal)


def test_shortest_proof_one_and_two_moves():
    ie, ee = eq.inv(eq.E), eq.mul(eq.E, eq.E)
    one = eq.Theory(frozenset(), (eq.Goal(a, ie), eq.Goal(ie, b)))
    assert eq.shortest_proof(one, eq.Goal(a, b), 2)[0] == 1
    two = eq.Theory(frozenset(),
                    (eq.Goal(a, ee), eq.Goal(ee, ie), eq.Goal(ie, b)))
    assert eq.shortest_proof(two, eq.Goal(a, b), 2)[0] == 2


# DEMO fixtures: hand-entered construction lists, not search output.
ab, ba = eq.mul(a, b), eq.mul(b, a)
ia, ib, iab = eq.inv(a), eq.inv(b), eq.inv(ab)
rhs = eq.mul(ib, ia)
DEMO = {
    "exponent_two_commutes": (
        "exponent_two", eq.Goal(ab, ba), [
            eq.Move("inv", (a,)), eq.Move("mul", (a, a)),
            eq.Move("mul", (ia, a)), eq.Move("mul", (eq.E, a)),
            eq.Move("mul", (ia, eq.E)), eq.Move("inv", (b,)),
            eq.Move("mul", (b, b)), eq.Move("mul", (ib, b)),
            eq.Move("mul", (eq.E, b)), eq.Move("mul", (ib, eq.E)),
            eq.Move("mul", (ab, ba)), eq.Move("mul", (b, ba)),
            eq.Move("inv", (ab,)), eq.Move("mul", (iab, ab)),
            eq.Move("mul", (eq.E, ba)), eq.Move("mul", (iab, eq.E)),
            eq.Move("mul", (ab, ab)), eq.Move("mul", (eq.E, ab)),
        ]),
    "inverse_product": (
        "group", eq.Goal(iab, rhs), [
            eq.Move("mul", (ia, a)), eq.Move("mul", (eq.E, b)),
            eq.Move("mul", (ia, ab)), eq.Move("mul", (ib, b)),
            eq.Move("mul", (rhs, ab)), eq.Move("mul", (ab, iab)),
            eq.Move("mul", (rhs, eq.E)), eq.Move("mul", (eq.E, iab)),
        ]),
    "left_group_right_inverse": (
        "left_group", eq.Goal(eq.mul(a, ia), eq.E), [
            eq.Move("inv", (ia,)), eq.Move("mul", (eq.inv(ia), ia)),
            eq.Move("mul", (eq.E, a)), eq.Move("mul", (ia, a)),
            eq.Move("mul", (eq.inv(ia), eq.E)), eq.Move("mul", (eq.E, ia)),
        ]),
}


def test_demo_move_lists_are_engine_checked():
    for name, (th, goal, moves) in DEMO.items():
        p, kept = eq.replay(th, goal, moves)
        assert kept == moves, name
        assert eq.entails(p, goal), name
