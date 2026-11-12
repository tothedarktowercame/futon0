import random

import metagame_horn as horn
import metagame_horn_setter as setter


def test_first_of_two_independent_derivations_is_recorded():
    x = horn.var("x")
    p, r, q = (horn.Rel(name, (x,)) for name in "PRQ")
    theory = horn.Theory((horn.Rule("from-P", (p,), q),
                          horn.Rule("from-R", (r,), q)), ())
    a = horn.gen("a")
    position = horn.start(theory, (horn.Rel("P", (a,)), horn.Rel("R", (a,))), (a,))
    stored, reason = horn.derivation(position, horn.Rel("Q", (a,)))
    assert stored == horn.Rel("Q", (a,))
    assert reason.rule == "from-P"
    assert reason.premises == (horn.Rel("P", (a,)),)


def test_congruence_merge_records_argument_equalities():
    a, b = horn.var("a"), horn.var("b")
    mark = horn.Term("mark", (a, b))
    theory = horn.Theory((
        horn.Rule("mark-present", (), horn.Rel("T", (mark,))),
        horn.Rule("merge", (horn.Rel("T", (mark,)),), horn.Equation(a, b))),
        (horn.Operation("f", 1, ("x",)), horn.Operation("mark", 2, ("a", "b"))))
    ga, gb = horn.gen("a"), horn.gen("b")
    source = horn.start(theory, (), (ga, gb))
    horn.apply(source, horn.Move("f", (ga,)))
    horn.apply(source, horn.Move("f", (gb,)))
    horn.apply(source, horn.Move("mark", (ga, gb)))
    reasons = [reason for fact, reason in source.derivations.items()
               if isinstance(fact, horn.Equation) and reason.rule == "congruence"]
    assert reasons
    assert all(all(isinstance(x, horn.Equation) for x in reason.premises)
               for reason in reasons)


def test_provenance_needed_and_replay_oracle_agree_on_entailment():
    position = setter.opening(3)
    rng = random.Random(20261112)
    moves = []
    for _ in range(4):
        move = rng.choice(horn.candidates(position))
        horn.apply(position, move); moves.append(move)
    opening = setter.opening(3)
    initial = set(horn.facts(opening))
    for fact in [x for x in horn.facts(position) if x not in initial][:20]:
        goal = horn.Goal((fact,))
        new = horn.needed("line", opening, moves, goal)
        old = horn.needed_by_replay("line", opening, moves, goal)
        assert horn.entails(horn.replay("line", opening, new)[0], goal)
        assert horn.entails(horn.replay("line", opening, old)[0], goal)
        for move in new:
            trial = [x for x in new if x != move]
            assert not horn.entails(horn.replay("line", opening, trial)[0], goal)
