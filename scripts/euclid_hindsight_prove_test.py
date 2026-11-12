import euclid_candidate_model as models
import euclid_hindsight_prove as prove
import euclid_hindsight_train as train


def test_finds_i1_from_one_move_before_goal():
    _label, state, _legal, _wins = train.book_positions()[0]
    model = models.CandidateModel(seed=2026111202)
    result = prove.prove(state, model=model, max_calls=1, wall_seconds=30)
    assert result["solved"]
    assert result["proof_length"] == 1
    assert prove.replay(state, result)


def test_random_order_is_reproducible():
    _label, state, _legal, _wins = train.book_positions()[0]
    left = prove.prove(state, random_seed=17, max_calls=2, wall_seconds=30)
    right = prove.prove(state, random_seed=17, max_calls=2, wall_seconds=30)
    for key in ("solved", "network_calls", "nodes_expanded", "states_seen",
                "admitted", "refused", "proof_length", "proof_tuples"):
        assert left[key] == right[key]
