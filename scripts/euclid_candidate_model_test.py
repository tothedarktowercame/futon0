from pathlib import Path

from flax import nnx
import jax
import jax.numpy as jnp
import numpy as np
import optax
import pyspiel

import euclid_candidate_features as feature_encoder
import euclid_candidate_model as candidate_model
import euclid_spiel


ROOT = Path(__file__).resolve().parents[1]
PREFIX = ROOT / "holes/excursions/E-euclid-tutor-data/spiel/rc13/stage-2/prefix.json"


def _state(proposition=1, prefix=None):
    start = f",start_file={prefix}" if prefix else ""
    return pyspiel.load_game(
        f"euclid_book1(proposition={proposition},move_cap=6,"
        f"dense_actions=true,alpha_zero_facade=true{start})"
    ).new_initial_state()


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


def _arrays(state, candidates=None):
    if candidates is None:
        candidates = _candidates(state)
    return feature_encoder.extract(state, candidates).arrays


def _loss(model, batch, policy_target, value_target):
    logits, values = model(batch)
    masked_logits = jnp.where(batch["candidate_mask"], logits, -1e9)
    policy = -jnp.sum(policy_target * jax.nn.log_softmax(masked_logits), axis=1)
    value = jnp.square(values - value_target)
    return jnp.mean(policy + value)


def test_shapes_and_masks_for_zero_one_and_1200_candidates():
    state = _state()
    candidates = _candidates(state)
    model = candidate_model.CandidateModel(seed=1)
    for count in (0, 1, 1200):
        selected = ((candidates * (count // len(candidates) + 1))[:count]
                    if count else [])
        batch = candidate_model.batch_features([_arrays(state, selected)])
        logits, value = model(batch)
        assert logits.shape == (1, count)
        assert value.shape == (1,)
        assert np.isfinite(np.asarray(value)).all()
        assert batch["candidate_mask"].shape == (1, count)


def test_padding_changes_no_real_output():
    small = _arrays(_state(), _candidates(_state())[:3])
    large_state = _state(2, PREFIX)
    large = _arrays(large_state)
    model = candidate_model.CandidateModel(seed=2)
    small_logits, small_value = model(candidate_model.batch_features([small]))
    padded_logits, padded_value = model(
        candidate_model.batch_features([small, large]))
    assert np.allclose(np.asarray(small_logits[0]),
                       np.asarray(padded_logits[0, :3]), atol=1e-6)
    assert np.allclose(np.asarray(small_value[0]),
                       np.asarray(padded_value[0]), atol=1e-6)
    assert np.all(np.asarray(padded_logits[0, 3:]) == -1e9)


def test_candidate_permutation_permutes_logits():
    state = _state()
    candidates = _candidates(state)[:7]
    permutation = np.asarray([5, 2, 0, 6, 1, 4, 3])
    model = candidate_model.CandidateModel(seed=3)
    original, value = model(candidate_model.batch_features([
        _arrays(state, candidates)]))
    permuted, permuted_value = model(candidate_model.batch_features([
        _arrays(state, [candidates[i] for i in permutation])]))
    assert np.allclose(np.asarray(permuted[0]),
                       np.asarray(original[0])[permutation], atol=1e-6)
    assert np.allclose(np.asarray(value), np.asarray(permuted_value), atol=1e-6)


def test_plain_array_round_trip(tmp_path):
    state = _state()
    batch = candidate_model.batch_features([_arrays(state, _candidates(state)[:4])])
    source = candidate_model.CandidateModel(seed=4)
    expected = source(batch)
    candidate_model.save_plain_arrays(source, tmp_path / "weights")
    restored = candidate_model.CandidateModel(seed=99)
    candidate_model.load_plain_arrays(restored, tmp_path / "weights")
    for left, right in zip(candidate_model.parameter_arrays(source),
                           candidate_model.parameter_arrays(restored)):
        assert np.array_equal(left, right)
    actual = restored(batch)
    assert all(np.array_equal(np.asarray(left), np.asarray(right))
               for left, right in zip(expected, actual))


def test_gradients_reach_embeddings_and_value_and_step_reduces_loss():
    state = _state()
    candidates = _candidates(state)[:8]
    first = _arrays(state, candidates)
    second = _arrays(state, list(reversed(candidates)))
    batch = candidate_model.batch_features([first, second])
    targets = jnp.zeros((2, 8)).at[0, 0].set(1).at[1, 3].set(1)
    values = jnp.asarray([1.0, -1.0])
    model = candidate_model.CandidateModel(seed=5)
    optimizer = nnx.Optimizer(model, optax.adam(3e-3), wrt=nnx.Param)

    loss, gradients = nnx.value_and_grad(_loss)(
        model, batch, targets, values)
    paths_and_leaves, _ = jax.tree_util.tree_flatten_with_path(gradients)
    nonzero = {str(path): bool(np.any(np.asarray(leaf)))
               for path, leaf in paths_and_leaves}
    for name in ("rule_embedding", "role_embedding", "kind_embedding",
                 "predicate_embedding", "value_output"):
        assert any(name in path and active for path, active in nonzero.items()), name
    optimizer.update(model, gradients)
    after = _loss(model, batch, targets, values)
    assert float(after) < float(loss)


def test_overfit_one_grounding_without_promoting_its_sibling():
    state = _state(2, PREFIX)
    candidates = _candidates(state)
    by_rule = {}
    for index, candidate in enumerate(candidates):
        by_rule.setdefault(candidate[0], []).append(index)
    initial_model = candidate_model.CandidateModel(seed=83)
    arrays = _arrays(state, candidates)
    batch = candidate_model.batch_features([arrays])
    initial_logits, _ = initial_model(batch)
    global_top = int(np.argmax(np.asarray(initial_logits[0])))
    rule = "distinct_points"
    indices = by_rule[rule]
    assert len(indices) >= 5 and global_top not in indices
    model = initial_model
    ordered = sorted(indices, key=lambda index: -float(initial_logits[0, index]))
    target = ordered[0]
    sibling = ordered[1]
    assert target != global_top
    policy = jnp.zeros((1, len(candidates))).at[0, target].set(1)
    optimizer = nnx.Optimizer(model, optax.adam(1e-2), wrt=nnx.Param)
    reached = None
    for step in range(1, 81):
        _value, gradients = nnx.value_and_grad(_loss)(
            model, batch, policy, jnp.asarray([0.0]))
        optimizer.update(model, gradients)
        logits, _ = model(batch)
        if int(np.argmax(np.asarray(logits[0]))) == target:
            reached = step
            break
    assert reached is not None, (rule, target)
    final = np.asarray(logits[0])
    assert final[target] > final[sibling]


def test_bucketed_padding_preserves_outputs_and_bounds_shapes():
    """D3-E2 OOM: per-batch padded shapes recompiled the jitted step every
    batch.  Bucketing must leave every real logit and value unchanged and map
    batches of different real sizes onto the same padded shape."""
    state = _state(2, PREFIX)
    candidates = _candidates(state)
    small = _arrays(state, candidates[:37])
    large = _arrays(state, candidates[:53])
    model = candidate_model.CandidateModel(seed=7)
    for items in ([small], [large], [small, large]):
        plain_logits, plain_values = model(candidate_model.batch_features(items))
        bucket_logits, bucket_values = model(
            candidate_model.batch_features(items, bucket=True))
        for row, item in enumerate(items):
            width = len(item["candidate_mask"])
            np.testing.assert_allclose(
                np.asarray(bucket_logits[row, :width]),
                np.asarray(plain_logits[row, :width]), rtol=1e-5, atol=1e-5)
        np.testing.assert_allclose(np.asarray(bucket_values),
                                   np.asarray(plain_values), rtol=1e-5, atol=1e-5)
    a = candidate_model.batch_features([small], bucket=True)
    b = candidate_model.batch_features([large], bucket=True)
    # 37 and 53 candidates share the 64 bucket; every padded axis is a power of two
    assert a["candidate_mask"].shape == b["candidate_mask"].shape == (1, 64)
    for batch in (a, b):
        for value in batch.values():
            assert all(size & (size - 1) == 0 for size in value.shape[1:])
