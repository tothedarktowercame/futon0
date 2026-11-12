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
