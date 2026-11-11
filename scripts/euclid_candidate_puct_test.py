import json
from pathlib import Path
from unittest import mock

import numpy as np
import pyspiel

import euclid_candidate_puct as candidate_puct
import euclid_spiel


ROOT = Path(__file__).resolve().parents[1]
SPIEL = ROOT / "holes/excursions/E-euclid-tutor-data/spiel"


class FlatModel:
    def __call__(self, arrays):
        batch, candidates = arrays["candidate_mask"].shape
        return np.zeros((batch, candidates), dtype=np.float32), np.zeros(
            batch, dtype=np.float32)


def _bot(**kwargs):
    return candidate_puct.CandidatePUCT(
        FlatModel(), simulations=kwargs.pop("simulations", 8),
        jit_model=False, **kwargs)


def _stage_one_state():
    prefix = SPIEL / "rc11/stage-1/prefix.json"
    return pyspiel.load_game(
        f"euclid_book1(proposition=2,move_cap=3,dense_actions=true,"
        f"alpha_zero_facade=true,start_file={prefix})"
    ).new_initial_state()


def _receipt_state(receipt):
    game = pyspiel.load_game(
        "euclid_book1(proposition=2,move_cap=3,dense_actions=true,"
        "alpha_zero_facade=true)"
    )
    engine_state = euclid_spiel.play._start_state(2)
    initial = []
    for ply, encoded in enumerate(receipt["initial_moves"], 1):
        rule, args, bindings = encoded
        action = (rule, tuple(args), tuple((a, b) for a, b in bindings))
        euclid_spiel.eng.apply_step(
            engine_state, ply, rule, tuple(args), dict(bindings))
        initial.append(action)
    return euclid_spiel.EuclidState(
        game, engine_state=engine_state, initial_moves=initial)


def test_root_win_in_one_wins_clean_ten_times_and_recorded_twenty():
    for seed in range(10):
        state = _stage_one_state()
        result = _bot(seed=seed).search(state)
        assert result.candidate is not None
        state.apply_action_tuple(result.candidate)
        assert state.goal_reached()

    receipts = []
    for name in ("eval-clean-wins.jsonl", "eval-perturbed-wins.jsonl"):
        receipts.extend(json.loads(line) for line in
                        (SPIEL / "rc11/stage-1" / name).read_text().splitlines())
    assert len(receipts) == 20
    for receipt in receipts:
        state = _receipt_state(receipt)
        result = _bot().search(state)
        assert result.candidate == (
            receipt["moves"][0][0], tuple(receipt["moves"][0][1]),
            tuple((a, b) for a, b in receipt["moves"][0][2]))
        state.apply_action_tuple(result.candidate)
        assert state.goal_reached()


def test_guard_refusal_is_never_a_child_and_is_cached():
    prefix = SPIEL / "rc13/stage-2/prefix.json"
    state = pyspiel.load_game(
        f"euclid_book1(proposition=2,move_cap=4,dense_actions=true,"
        f"alpha_zero_facade=true,start_file={prefix})"
    ).new_initial_state()
    state.apply_action_tuple(("off_line_strict_triangle",
                              ("d_2", "b", "a", "L_1"), ()))
    state.apply_action_tuple(("name_point", ("d_2",), (("x", "x_2"),)))
    refused = (
        "superposition",
        ("a", "d_2", "x_2", "a", "b", "d_2",
         "L_3", "L_3", "L_3", "L_1"),
        (("AC'", "AC'_3"), ("BC'", "BC'_3"),
         ("b'", "b'_3"), ("c'", "c'_3")),
    )
    bot = _bot(simulations=1, root_win_in_one=False)
    bot._stats = bot._new_stats()
    node = candidate_puct.Node(
        state, expanded=True, candidates=[refused],
        priors=np.asarray([1.0]))
    bot._simulation(node)
    assert not node.children
    assert not node.candidates
    assert node.dead_end
    assert bot._stats["admission_refusals"] == 1
    assert bot._admit(state, refused) is None
    assert bot._stats["admission_cache_hits"] == 1
    assert bot._stats["admission_calls"] == 1


def test_fixed_seed_visit_counts_and_aligned_policy_target():
    state = pyspiel.load_game(
        "euclid_book1(proposition=1,move_cap=5,dense_actions=true,"
        "alpha_zero_facade=true)"
    ).new_initial_state()
    first = _bot(simulations=12, root_win_in_one=False, seed=71).search(state)
    second = _bot(simulations=12, root_win_in_one=False, seed=71).search(state)
    assert first.candidate_keys == second.candidate_keys
    assert first.visit_counts == second.visit_counts
    assert len(first.candidate_keys) == len(first.visit_counts)
    assert sum(first.visit_counts) == 12
    assert first.candidate in first.candidate_keys


def test_empty_candidate_node_is_dead_end_value_zero():
    state = pyspiel.load_game(
        "euclid_book1(proposition=1,move_cap=5,dense_actions=true,"
        "alpha_zero_facade=true)"
    ).new_initial_state()
    bot = _bot(simulations=3, root_win_in_one=False)
    with mock.patch.object(candidate_puct, "premise_certified_candidates",
                           return_value=[]):
        result = bot.search(state)
    assert result.candidate is None
    assert result.candidate_keys == ()
    assert result.visit_counts == ()
    assert result.root_value == 0.0
