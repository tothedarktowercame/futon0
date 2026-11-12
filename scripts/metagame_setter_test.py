import metagame_eq as eq
import metagame_setter as setter


a, b = eq.gen("a"), eq.gen("b")


def test_classifies_trivial_and_exactly_two_term_goal():
    ee, ie = eq.mul(eq.E, eq.E), eq.inv(eq.E)
    th = eq.Theory(frozenset(),
                   (eq.Goal(a, ee), eq.Goal(ee, ie), eq.Goal(ie, b)))
    # ee = ie is immediate when its own subterms form the solver start.
    assert eq.entails(eq.start(th, eq.Goal(ee, ie), (a, b)), eq.Goal(ee, ie))
    goal = eq.Goal(a, b)
    assert not eq.entails(eq.start(th, goal, (a, b)), goal)
    keep = setter.needed_terms(th, goal, (a, b), [ee, ie])
    assert keep == [ee, ie]


def test_replay_tracks_an_argument_that_later_becomes_nonrepresentative():
    x, z = eq.mul(a, a), eq.inv(eq.E)
    th = eq.Theory(frozenset(), (eq.Goal(x, z), eq.Goal(z, a)))
    p = eq.start(th, eq.Goal(eq.E, b), (a, b))
    mapping = setter._initial_mapping(p)
    assert setter.add_term_by_classes(p, x, mapping)
    before_merge = eq.inv(x)
    assert setter.add_term_by_classes(p, before_merge, mapping)
    assert setter.add_term_by_classes(p, z, mapping)
    assert p.find(p.ids[x]) == p.find(p.ids[a])
    # A later recorded operation still naming x is added using a's class.
    later = eq.mul(before_merge, x)
    assert setter.add_term_by_classes(p, later, mapping)
    assert mapping[x] == p.representative_term(a)
    assert eq._lookup(p, "mul", (p.representative_term(before_merge),
                                 p.representative_term(a))) is not None


def test_canonical_goal_ignores_generator_names_and_side_order():
    x, y = eq.gen("x"), eq.gen("y")
    first = eq.Goal(eq.mul(a, eq.inv(b)), eq.inv(a))
    renamed_swapped = eq.Goal(eq.inv(x), eq.mul(x, eq.inv(y)))
    assert setter.canonical_goal(first) == setter.canonical_goal(renamed_swapped)


def test_seeded_run_needed_terms_prove_on_fresh_start():
    found = None
    for seed in range(100):
        run = setter.random_run("exponent_two", 2, 10, seed, seed)
        _, records = setter.classify_run(run)
        if records:
            found = records[0]
            break
    assert found is not None
    p, added, _ = setter.replay_terms(found.theory, found.goal,
                                      setter.generators(found.generator_count),
                                      found.needed)
    assert added == found.needed
    assert eq.entails(p, found.goal)


def test_stratified_sample_covers_needed_sizes():
    records = []
    for size in (1, 2, 3):
        records.append(setter.GoalRecord("group", 2, 3, eq.Goal(a, b),
                                         [eq.inv(eq.E)] * size, 1, size))
    sample = setter.stratified_sample(records, 3, 7)
    assert {len(record.needed) for record in sample} == {1, 2, 3}


def test_probability_one_chooses_only_completing_moves():
    c = eq.gen("c")
    p = eq.start("group", eq.Goal(a, b), (a, b, c))
    ab, bc = eq.mul(a, b), eq.mul(b, c)
    for term in (ab, bc, eq.mul(ab, c)):
        eq._add_tree(p, term)
    eq.closure(p)
    information = eq.completing_moves(p)
    assert any(item.count for item in information.values())
    rng = __import__("random").Random(20261112)
    for _ in range(200):
        move = setter.choose_move(p, rng, "completing(1)")
        assert information[move].count > 0
