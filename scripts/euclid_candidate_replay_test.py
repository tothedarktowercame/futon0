import json
from pathlib import Path
from types import SimpleNamespace

from flax import nnx
import jax
import jax.numpy as jnp
import numpy as np
import optax
import pyspiel

import euclid_candidate_features as feature_encoder
import euclid_candidate_model as candidate_model
import euclid_candidate_replay as replay
import euclid_spiel


def _state(after_line=False):
    state = pyspiel.load_game(
        "euclid_book1(proposition=1,move_cap=6,dense_actions=true,"
        "alpha_zero_facade=true)"
    ).new_initial_state()
    if after_line:
        action = next(candidate for candidate in _candidates(state)
                      if candidate[0] == "line_from_points"
                      and candidate[1] == ("a", "b"))
        state.apply_action_tuple(action)
    return state


def _candidates(state):
    codec = state._euclid_game.action_codec
    rules = set(codec.rules)
    by_kind = {
        kind: set(names[:euclid_spiel.DENSE_KIND_CAPS[kind]])
        for kind, names in codec._by_kind(state).items()
    }
    free = {rule: set(codec.layouts[rule][1]) for rule in rules}
    return euclid_spiel.enumerate_actions(
        state._proposition, state.engine_state, state.initial_kinds,
        len(state.history_moves), state._parent_legal_keys,
        rules, by_kind, free, state._canonical_names(), validate=False)


def _record(state, count=4, value=1.0, seed=7, stage="default"):
    candidates = _candidates(state)[:count]
    arrays = feature_encoder.extract(state, candidates).arrays
    visits = tuple(range(count, 0, -1))
    result = SimpleNamespace(candidate_keys=tuple(candidates),
                             visit_counts=visits)
    return replay.ReplayRecord.from_search(
        arrays, result, value, game_id="euclid-i1", ply=len(state.history_moves),
        start_kind="clean", seed=seed, stage=stage,
        logical_state_key_sha256=f"key-{seed}")


def _assert_record_equal(left, right):
    assert left.candidate_keys == right.candidate_keys
    assert left.visits == right.visits
    assert left.value == right.value
    assert (left.game_id, left.ply, left.start_kind, left.seed, left.stage,
            left.logical_state_key_sha256) == (
        right.game_id, right.ply, right.start_kind, right.seed, right.stage,
        right.logical_state_key_sha256)
    assert left.features.keys() == right.features.keys()
    for name in left.features:
        assert np.array_equal(left.features[name], right.features[name]), name


def test_receipt_round_trip_is_exact_and_written_immediately(tmp_path):
    records = [_record(_state(), seed=1), _record(_state(True), seed=2)]
    path = replay.actor_receipt_path(tmp_path, 3)
    with replay.ReceiptWriter(path) as writer:
        writer.append(records[0])
        assert len(path.read_text().splitlines()) == 1
        writer.append(records[1])
        assert len(path.read_text().splitlines()) == 2
    loaded = replay.read_receipts(path)
    assert len(loaded) == 2
    for expected, actual in zip(records, loaded):
        _assert_record_equal(expected, actual)


def test_padding_invariance_and_padded_logits_are_ignored():
    small = _record(_state(), count=3)
    large = _record(_state(True), count=20)
    alone = replay.collate([small])
    together = replay.collate([small, large])
    model = candidate_model.CandidateModel(seed=1)
    alone_logits, alone_values = model(alone.features)
    together_logits, together_values = model(together.features)
    padded_first = replay.ReplayBatch(
        {name: value[:1] for name, value in together.features.items()},
        together.policy_target[:1], together.policy_row_mask[:1],
        together.value_target[:1])
    alone_loss = replay.loss_from_outputs(alone_logits, alone_values, alone)
    padded_loss = replay.loss_from_outputs(
        together_logits[:1], together_values[:1], padded_first)
    assert all(np.allclose(np.asarray(left), np.asarray(right), atol=1e-6)
               for left, right in zip(alone_loss, padded_loss))

    planted_high = together_logits[:1].at[:, 3:].set(1e9)
    planted_low = together_logits[:1].at[:, 3:].set(-1e9)
    high = replay.loss_from_outputs(planted_high, together_values[:1], padded_first)
    low = replay.loss_from_outputs(planted_low, together_values[:1], padded_first)
    assert np.allclose(np.asarray(high[0]), np.asarray(low[0]), atol=1e-6)


def test_version_mismatch_is_refused_clearly(tmp_path):
    path = replay.actor_receipt_path(tmp_path, 0)
    encoded = _record(_state()).to_json()
    encoded["receipt_format_version"] = 1
    path.write_text(json.dumps(encoded) + "\n")
    try:
        replay.read_receipts(path)
    except ValueError as error:
        assert "receipt format" in str(error)
    else:
        raise AssertionError("old 0/1 receipt format was accepted")

    encoded = _record(_state()).to_json()
    encoded["schema_version"] += 1
    path.write_text(json.dumps(encoded) + "\n")
    try:
        replay.read_receipts(path)
    except ValueError as error:
        assert "schema version" in str(error)
    else:
        raise AssertionError("mismatched replay schema was accepted")

    encoded = _record(_state()).to_json()
    encoded["vocabulary_version"] = "0" * 64
    path.write_text(json.dumps(encoded) + "\n")
    try:
        replay.read_receipts(path)
    except ValueError as error:
        assert "vocabulary version" in str(error)
    else:
        raise AssertionError("mismatched replay vocabulary was accepted")


def test_seeded_bounded_buffer_is_uniform_and_evicts_oldest():
    records = [_record(_state(bool(index % 2)), seed=index)
               for index in range(5)]
    first = replay.ReplayBuffer(capacity=3, seed=19)
    second = replay.ReplayBuffer(capacity=3, seed=19)
    first.extend(records)
    second.extend(records)
    second.extend(records)
    assert len(first) == 3
    assert [record.seed for record in first.records] == [2, 3, 4]
    assert [record.seed for record in first.sample(2)] == [
        record.seed for record in second.sample(2)]


def test_seeded_buffer_reserves_minimum_share_for_each_available_stage():
    records = ([_record(_state(), seed=index, stage="stage-1")
                for index in range(10)] +
               [_record(_state(True), seed=100 + index, stage="stage-2")
                for index in range(10)])
    buffer = replay.ReplayBuffer(capacity=30, seed=23)
    buffer.extend(records)
    sample = buffer.sample(
        10, min_stage_share=0.3, stages=("stage-1", "stage-2"))
    assert sum(record.stage == "stage-1" for record in sample) >= 3
    assert sum(record.stage == "stage-2" for record in sample) >= 3

    one_stage = replay.ReplayBuffer(capacity=30, seed=23)
    one_stage.extend(records[:10])
    assert {record.stage for record in one_stage.sample(
        10, min_stage_share=0.3, stages=("stage-1", "stage-2"))} == {
            "stage-1"}


def test_zero_candidate_record_is_value_only():
    state = _state()
    arrays = feature_encoder.extract(state, []).arrays
    record = replay.ReplayRecord.from_search(
        arrays, SimpleNamespace(candidate_keys=(), visit_counts=()), -1.0,
        game_id="dead", ply=0, start_kind="dead-end", seed=0)
    batch = replay.collate([record])
    model = candidate_model.CandidateModel(seed=2)
    logits, values = model(batch.features)
    policy, value = replay.loss_from_outputs(logits, values, batch)
    assert logits.shape == (1, 0)
    assert float(policy) == 0.0
    assert np.isfinite(float(value))


def test_fifty_optimizer_steps_reduce_policy_and_value_losses():
    records = [
        _record(_state(), count=8, value=1.0, seed=1),
        _record(_state(True), count=8, value=-1.0, seed=2),
    ]
    # Give the second position a different preferred move.
    records[1] = replay.ReplayRecord(
        records[1].features, records[1].candidate_keys,
        (0, 0, 0, 8, 0, 0, 0, 0), records[1].value,
        records[1].game_id, records[1].ply, records[1].start_kind,
        records[1].seed)
    batch = replay.collate(records)
    model = candidate_model.CandidateModel(seed=3)
    optimizer = nnx.Optimizer(model, optax.adam(3e-3), wrt=nnx.Param)
    _total, before = replay.model_loss(model, batch, l2_coefficient=0.0)
    before = {name: float(value) for name, value in before.items()}
    for _ in range(50):
        (loss, _components), gradients = nnx.value_and_grad(
            replay.model_loss, has_aux=True)(model, batch, 0.0)
        optimizer.update(model, gradients)
        assert np.isfinite(float(loss))
    _total, after = replay.model_loss(model, batch, l2_coefficient=0.0)
    assert float(after["policy"]) < before["policy"]
    assert float(after["value"]) < before["value"]
