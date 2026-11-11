import json
from pathlib import Path
import random

import numpy as np
import pyspiel

import euclid_open_spiel_alpha_zero as runner
import euclid_spiel
import euclid_widening


HERE = Path(__file__).resolve().parent
SPIEL = HERE.parent / "holes/excursions/E-euclid-tutor-data/spiel"


class _FlatModel:
    def inference(self, observation, mask):
        del observation
        policy = np.asarray(mask, dtype=float)
        return 0.0, policy / policy.sum()


class _FlatEvaluator:
    def __init__(self, action_count):
        self._model = _FlatModel()
        self.action_count = action_count

    def evaluate(self, state):
        del state
        return np.array([0.0, 0.0])

    def prior(self, state):
        actions = state.legal_actions()
        return [(a, 1 / len(actions)) for a in actions]

    def cache_info(self):
        return None

    def clear_cache(self):
        pass


def _game(start_file=None, move_cap=10):
    start = f",start_file={start_file}" if start_file else ""
    return pyspiel.load_game(
        f"euclid_book1(proposition=2,move_cap={move_cap},dense_actions=true,"
        f"alpha_zero_facade=true{start})")


def test_single_action_legality_matches_enumeration_on_seeded_positions():
    rng = random.Random(1401)
    game = _game()
    state = game.new_initial_state()
    for position in range(20):
        legal = set(state.legal_actions())
        for action in legal:
            assert (euclid_widening.certify_action(state, action) is not None)
        for action in rng.sample(range(game.num_distinct_actions()), 500):
            assert ((euclid_widening.certify_action(state, action) is not None)
                    == (action in legal))
        if position % 4 == 3:
            state = game.new_initial_state()
        else:
            state.apply_action(rng.choice(tuple(legal)))


def test_force_all_admits_exactly_the_enumerated_legal_set():
    game = _game()
    state = game.new_initial_state()
    wrapped = euclid_widening.WideningEvaluator(
        _FlatEvaluator(game.num_distinct_actions()),
        game.num_distinct_actions())
    bot = euclid_widening.ProgressiveWideningBot(
        game, 1.41, 2, wrapped, force_all=True)
    node = runner.mcts.SearchNode(None, state.current_player(), 1.0)
    data = euclid_widening._NodeData()
    bot._admit(node, state, data, game.num_distinct_actions())
    assert {child.action for child in node.children} == set(state.legal_actions())


def test_root_mate_wrapper_wins_stage_one_for_recorded_wins():
    prefix = SPIEL / "rc11/stage-1/prefix.json"
    game = _game(prefix, move_cap=3)
    base = _FlatEvaluator(game.num_distinct_actions())
    widening = euclid_widening.WideningEvaluator(
        base, game.num_distinct_actions())
    bot = euclid_widening.ProgressiveWideningBot(
        game, 1.41, 32, widening, random_state=np.random.RandomState(0))
    bot = runner.root_win_in_one_bot(bot, widening)
    records = []
    for path in sorted((SPIEL / "rc11/stage-1").glob("*wins.jsonl")):
        records.extend(json.loads(line) for line in path.read_text().splitlines())
    assert len(records) >= 20
    for _record in records[:20]:
        state = game.new_initial_state()
        state.apply_action(bot.step(state))
        assert state.goal_reached()

