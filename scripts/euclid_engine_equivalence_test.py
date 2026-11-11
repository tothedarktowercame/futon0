import random

import pytest

import euclid_engine as engine
import euclid_engine_reference as reference
import euclid_play as play
import euclid_spiel as spiel


PROPOSITIONS = (1, 2, 3, 5)
SEEDS = range(8)
PLIES = 5


def _outcome(state, ply, action):
    rule, args, bindings = action
    trial = state.copy()
    try:
        report = engine.apply_step(trial, ply, rule, args, dict(bindings))
    except engine.MissingPremise as exc:
        return ("refused", exc.premise), trial
    return ("accepted", tuple(report["added"])), trial


@pytest.mark.parametrize("proposition", PROPOSITIONS)
@pytest.mark.parametrize("seed", SEEDS)
def test_optimized_entailment_matches_frozen_engine_on_seeded_sequences(
        proposition, seed):
    """Old/new stores and refusal witnesses agree along neutral random play."""
    rng = random.Random(10_000 * proposition + seed)
    fast = play._start_state(proposition)
    old = reference.from_state(fast)
    initial_kinds = {
        obj["name"]: obj["kind"]
        for obj in play.tr.objects_from_hyps(proposition)
    }

    for ply in range(PLIES):
        # Probe several grounded candidates, including refusals, before
        # advancing along one accepted neutral action.
        candidates = list(spiel._candidate_actions(
            proposition, fast, initial_kinds, ply))
        rng.shuffle(candidates)
        for action in candidates[:8]:
            fast_outcome, _ = _outcome(fast, ply + 1, action)
            old_outcome, _ = _outcome(old, ply + 1, action)
            assert fast_outcome == old_outcome

        action = spiel.sample_action(
            proposition, fast, initial_kinds, ply, rng=rng)
        fast_outcome, fast = _outcome(fast, ply + 1, action)
        old_outcome, old = _outcome(old, ply + 1, action)
        assert fast_outcome == old_outcome
        assert fast_outcome[0] == "accepted"
        assert set(fast.facts) == set(old.facts)
        assert fast.la.rows == old.la.rows
        assert fast.declared == old.declared
        assert fast.reserved == old.reserved

