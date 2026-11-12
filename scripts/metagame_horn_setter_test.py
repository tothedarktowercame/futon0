import metagame_horn as horn
import metagame_horn_setter as setter


def test_canonical_key_is_invariant_under_renaming_givens():
    a, b, c = map(horn.gen, "abc")
    x, y, z = map(horn.gen, "xyz")
    first = horn.Rel("B", (a, horn.Term("ext", (a, b, b, c)), c))
    renamed = horn.Rel("B", (y, horn.Term("ext", (y, z, z, x)), x))
    assert setter.canonical_key(first, 3) == setter.canonical_key(renamed, 3)


def test_needed_moves_replay_on_fresh_start_and_entail_goal():
    a, b, c = setter.givens(3)
    move = horn.Move("ext", (a, b, a, c))
    position = setter.opening(3)
    horn.apply(position, move)
    goal = horn.Rel("B", (a, b, move.result))
    needed = horn.needed("line", setter.opening(3), [move], horn.Goal((goal,)))
    fresh = setter.replay(3, needed)
    assert needed == [move]
    assert horn.entails(fresh, horn.Goal((goal,)))


def test_symmetric_pair_is_labelled():
    a, b, c = setter.givens(3)
    forward = horn.Rel("B", (a, b, c))
    reverse = horn.Rel("B", (c, b, a))
    keys = setter.symmetric_keys([forward, reverse], 3)
    assert setter.canonical_key(forward, 3) in keys
    assert setter.canonical_key(reverse, 3) in keys


def test_immediate_extension_conclusion_is_direct_but_transitive_goal_is_not():
    a, b, c = setter.givens(3)
    move = horn.Move("ext", (a, b, a, c))
    assert setter.direct_label(3, horn.Rel("B", (a, b, move.result)), [move])
    assert not setter.direct_label(3, horn.Rel("B", (a, c, move.result)), [move])


# --- added in review by claude-9 ---

def test_mirror_pair_is_one_orbit_and_keeps_a_representative():
    """The first version labelled both members of a mirror pair symmetric,
    so a theorem with a mirror image was dropped entirely."""
    a, b, c = setter.givens(3)
    forward = horn.Rel("B", (a, b, c))
    reverse = horn.Rel("B", (c, b, a))
    assert setter.orbit_key(forward, 3) == setter.orbit_key(reverse, 3)
    e1 = horn.Rel("E", (a, b, b, c))
    e2 = horn.Rel("E", (c, b, a, b))
    assert setter.orbit_key(e1, 3) == setter.orbit_key(e2, 3)
    x = horn.Term("ext", (a, b, a, c))
    assert setter.orbit_key(horn.Rel("B", (a, b, x)), 3) != \
        setter.orbit_key(horn.Rel("B", (b, a, x)), 3)


def test_reflexive_congruence_is_labelled_but_a_real_one_is_not():
    a, b, c = setter.givens(3)
    assert setter.reflexive_label(horn.Rel("E", (a, b, a, b)))
    assert setter.reflexive_label(horn.Rel("E", (a, b, b, a)))
    assert setter.reflexive_label(horn.Rel("E", (a, a, c, c)))
    assert not setter.reflexive_label(horn.Rel("E", (a, b, b, c)))


def test_a_run_keeps_one_record_per_orbit_with_a_representative():
    records, _ = setter.classify_run(3, 4, 2026111200)
    keys = [setter.orbit_key(r.goal, 3) for r in records]
    assert len(keys) == len(set(keys))
    kept = [r for r in records if not r.labels]
    assert kept, "a 3-point, 4-move run should leave some unlabelled theorem"
    for r in kept[:3]:
        fresh = setter.replay(3, r.needed)
        assert horn.entails(fresh, horn.Goal((r.goal,)))
