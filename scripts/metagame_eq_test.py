import random
from itertools import product

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


# Added in review (claude-6): the packet's soundness test covered `group` only.
def _random_facts(theory_name, seed, runs=600, moves=8):
    rng = random.Random(seed)
    for _ in range(runs):
        p = eq.start(theory_name, eq.Goal(a, b))
        for _ in range(moves):
            cs = eq.candidates(p)
            if not cs:
                break
            eq.apply(p, rng.choice(cs))
        yield from eq.facts(p)


def _parity(t):
    """Generator-count parity: decides equality in exponent-two groups."""
    if t.op == "e":
        return frozenset()
    if t.op == "gen":
        return frozenset([t.args[0]])
    out = frozenset()
    for s in t.args:
        out = out ^ _parity(s)
    return out


def test_exponent_two_closure_is_sound_against_parity():
    checked = 0
    for fact in _random_facts("exponent_two", 7):
        checked += 1
        assert _parity(fact.left) == _parity(fact.right), fact
    assert checked > 1000


def test_left_group_closure_is_sound_against_free_group():
    checked = 0
    for fact in _random_facts("left_group", 11):
        checked += 1
        assert eq.free_group_equal(fact.left, fact.right), fact
    assert checked > 300


def test_assoc_waits_for_the_fourth_product():
    lhs = eq.mul(eq.mul(a, b), c)
    p = bare(["assoc"], eq.E, a, b, c, eq.mul(a, b), eq.mul(b, c), lhs,
             eq.mul(c, a))
    assert eq.facts(p) == []
    eq._add_tree(p, eq.mul(a, eq.mul(b, c)))
    eq.closure(p)
    assert [(f.left, f.right) for f in eq.facts(p)] == [
        (lhs, eq.mul(a, eq.mul(b, c)))]


def _reference_closure(p):
    """Frozen packet-1 closure, used only as the migration oracle."""
    names = {r.name for r in p.theory.rules}
    while True:
        changed = False
        signatures = {}
        for i, t in enumerate(p.terms):
            if t.op not in ("mul", "inv"):
                continue
            sig = (t.op, tuple(p.find(p.ids[x]) for x in t.args))
            if sig in signatures:
                changed |= p.union(signatures[sig], i)
            else:
                signatures[sig] = i
        for h in p.theory.hypotheses:
            changed |= eq._merge_if_present(p, h.left, h.right)
        e_id = p.ids.get(eq.E)
        reps = [p.terms[p.find(i)] for i in range(len(p.terms))]
        for i in range(len(p.terms)):
            x = reps[i]
            if "left_identity" in names:
                z = eq._lookup(p, "mul", (eq.E, x))
                if z is not None:
                    changed |= p.union(z, i)
            if "right_identity" in names:
                z = eq._lookup(p, "mul", (x, eq.E))
                if z is not None:
                    changed |= p.union(z, i)
            xi = eq._lookup(p, "inv", (x,))
            if xi is not None and e_id is not None:
                ix = p.terms[p.find(xi)]
                if "left_inverse" in names:
                    z = eq._lookup(p, "mul", (ix, x))
                    if z is not None:
                        changed |= p.union(z, e_id)
                if "right_inverse" in names:
                    z = eq._lookup(p, "mul", (x, ix))
                    if z is not None:
                        changed |= p.union(z, e_id)
            if "square_is_identity" in names and e_id is not None:
                z = eq._lookup(p, "mul", (x, x))
                if z is not None:
                    changed |= p.union(z, e_id)
        if "assoc" in names:
            decompositions, mul_nodes = {}, []
            for i, t in enumerate(p.terms):
                if t.op == "mul":
                    decompositions.setdefault(p.find(i), []).append(t.args)
                    mul_nodes.append((i, t))
            for lhs, outer in mul_nodes:
                left, z = outer.args
                for x, y in decompositions.get(p.find(p.ids[left]), ()):
                    yz = eq._lookup(p, "mul", (y, z))
                    if yz is None:
                        continue
                    rhs = eq._lookup(p, "mul", (x, p.terms[p.find(yz)]))
                    if rhs is not None:
                        changed |= p.union(lhs, rhs)
        if not changed:
            return


def _partition(p):
    classes = {}
    for i, t in enumerate(p.terms):
        classes.setdefault(p.find(i), set()).add(t)
    return frozenset(frozenset(xs) for xs in classes.values())


def test_data_rules_match_packet_one_closure_after_every_move():
    compared = 0
    for theory_name, seed in (("group", 101), ("left_group", 102),
                              ("exponent_two", 103)):
        rng = random.Random(seed)
        for _ in range(2000):
            new = eq.start(theory_name, eq.Goal(a, b))
            old = new.copy()
            for _ in range(8):
                move = rng.choice(eq.candidates(new))
                eq.apply(new, move)
                args = tuple(old.representative_term(x) for x in move.args)
                old.add_term(eq.Term(move.op, args))
                _reference_closure(old)
                compared += 1
                assert _partition(new) == _partition(old)
    print(f"compared {compared} old/new closure positions")
    assert compared == 48000


def test_monoid_signature_and_associativity_presence_condition():
    p = eq.start("monoid", eq.Goal(a, b), (a, b, c))
    assert all(move.op != "inv" for move in eq.candidates(p))
    ab, bc = eq.mul(a, b), eq.mul(b, c)
    lhs, rhs = eq.mul(ab, c), eq.mul(a, bc)
    for term in (ab, bc, lhs):
        eq._add_tree(p, term)
    eq.closure(p)
    assert rhs not in p.ids
    eq._add_tree(p, rhs); eq.closure(p)
    assert p.find(p.ids[lhs]) == p.find(p.ids[rhs])


def test_commutative_group_rule_is_data():
    p = eq.start("commutative_group", eq.Goal(a, b))
    ab, ba = eq.mul(a, b), eq.mul(b, a)
    eq._add_tree(p, ab); eq._add_tree(p, ba); eq.closure(p)
    assert p.find(p.ids[ab]) == p.find(p.ids[ba])
    assert any(name == "commutative" for name, _, _ in
               eq.rule_statements("commutative_group"))


def test_repeated_pattern_variable_requires_one_class():
    repeated = eq.Rule("square", eq.mul(eq.X, eq.X), eq.E)
    th = eq.Theory((repeated,))
    aa, ab = eq.mul(a, a), eq.mul(a, b)
    p = eq.start(th, eq.Goal(a, b))
    eq._add_tree(p, aa); eq._add_tree(p, ab); eq.closure(p)
    assert p.find(p.ids[aa]) == p.find(p.ids[eq.E])
    assert p.find(p.ids[ab]) != p.find(p.ids[eq.E])
    p.union(p.ids[a], p.ids[b]); eq.closure(p)
    assert p.find(p.ids[ab]) == p.find(p.ids[eq.E])


def test_random_theories_are_deterministic_and_run_200_moves():
    for seed in range(50):
        th = eq.random_theory(seed)
        assert th == eq.random_theory(seed)
        p = eq.Position(th)
        for op, arity in th.signature:
            if arity == 0:
                p.add_term(eq.Term(op))
        p.add_term(a); p.add_term(b); eq.closure(p)
        rng = random.Random(seed)
        for _ in range(200):
            reps = [p.terms[i] for i in range(len(p.terms)) if p.find(i) == i]
            operations = [(op, arity) for op, arity in th.signature if arity]
            attempts = 0
            while True:
                op, arity = rng.choice(operations)
                move = eq.Move(op, tuple(rng.choice(reps) for _ in range(arity)))
                attempts += 1
                if eq._is_candidate(p, move):
                    eq._apply_candidate(p, move)
                    break
                assert attempts < 10000


def _brute_instances(p):
    """Independent complete-instance enumerator for completing_moves tests."""
    classes = sorted({p.find(i) for i in range(len(p.terms))})

    def variables(pattern):
        if pattern.op == "var":
            return {str(pattern.args[0])}
        return set().union(*(variables(x) for x in pattern.args), set())

    def evaluate(pattern, env):
        if pattern.op == "var":
            return env[str(pattern.args[0])]
        args = []
        for child in pattern.args:
            value = evaluate(child, env)
            if value is None:
                return None
            args.append(value)
        for i, term in enumerate(p.terms):
            if term.op == pattern.op and tuple(p.find(p.ids[x]) for x in term.args) == tuple(args):
                return p.find(i)
        return None

    out = {}
    for rule_index, rule in enumerate(p.theory.rules):
        names = sorted(variables(rule.left) | variables(rule.right))
        for values in product(classes, repeat=len(names)):
            env = dict(zip(names, values))
            left, right = evaluate(rule.left, env), evaluate(rule.right, env)
            if left is not None and right is not None:
                out[(rule_index, tuple(values))] = (left, right)
    return out


def _brute_completing_moves(p):
    before = _brute_instances(p)
    before_classes = len({p.find(i) for i in range(len(p.terms))})
    answer = {}
    for move in eq.candidates(p):
        q = p.copy()
        q.add_term(eq.Term(move.op, move.args))
        after = _brute_instances(q)
        added = set(after) - set(before)
        eq.closure(q)
        after_classes = len({q.find(i) for i in range(len(q.terms))})
        answer[move] = eq.MoveCompletion(len(added),
                                         after_classes < before_classes + 1)
    return answer


def test_completing_moves_matches_brute_force_on_300_positions():
    sources = [("group", 501), ("monoid", 502),
               (eq.random_theory(17), 503), (eq.random_theory(23), 504)]
    compared = 0
    for theory_, seed in sources:
        rng = random.Random(seed)
        for _ in range(75):
            th = eq.theory(theory_)
            p = eq.Position(th)
            for op, arity in th.signature:
                if arity == 0:
                    p.add_term(eq.Term(op))
            p.add_term(a); p.add_term(b); eq.closure(p)
            for _ in range(3):
                choices = eq.candidates(p)
                if choices:
                    eq.apply(p, rng.choice(choices))
            assert eq.completing_moves(p) == _brute_completing_moves(p)
            compared += 1
    assert compared == 300


def test_only_the_missing_associative_product_completes_assoc():
    p = eq.start("group", eq.Goal(a, b), (a, b, c))
    ab, bc = eq.mul(a, b), eq.mul(b, c)
    lhs, rhs = eq.mul(ab, c), eq.mul(a, bc)
    for term in (ab, bc, lhs):
        eq._add_tree(p, term)
    eq.closure(p)
    information = eq.completing_moves(p)
    target = eq.Move("mul", (a, bc))
    unrelated = eq.Move("inv", (c,))
    assert information[target].count >= 1
    assert information[unrelated].count == 0
