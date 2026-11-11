#!/usr/bin/env python3
"""Flax NNX policy/value model over grounded Euclid candidates."""

from __future__ import annotations

import json
from pathlib import Path

from flax import nnx
import jax
import jax.numpy as jnp
import numpy as np

import euclid_candidate_features as feature_schema


WIDTH = 64


class MLP(nnx.Module):
    def __init__(self, inputs, hidden, outputs, *, rngs):
        self.first = nnx.Linear(inputs, hidden, rngs=rngs)
        self.second = nnx.Linear(hidden, outputs, rngs=rngs)

    def __call__(self, values):
        return self.second(nnx.relu(self.first(values)))


def _masked_mean(values, mask, axis):
    weights = mask.astype(values.dtype)[..., None]
    total = jnp.sum(values * weights, axis=axis)
    count = jnp.sum(weights, axis=axis)
    return total / jnp.maximum(count, 1)


def _gather_objects(objects, indices):
    batch = jnp.arange(objects.shape[0])
    return objects[batch.reshape((-1,) + (1,) * (indices.ndim - 1)), indices]


class CandidateModel(nnx.Module):
    """Two-round fact/object encoder with shared candidate and value heads."""

    def __init__(self, *, width=WIDTH, seed=0):
        self.width = width
        rngs = nnx.Rngs(seed)
        vocab = feature_schema.VOCABULARY
        self.rule_embedding = nnx.Embed(len(vocab.rules), width, rngs=rngs)
        self.predicate_embedding = nnx.Embed(
            len(vocab.predicates), width, rngs=rngs)
        self.role_embedding = nnx.Embed(len(vocab.roles), width, rngs=rngs)
        self.kind_embedding = nnx.Embed(len(vocab.kinds), width, rngs=rngs)
        self.creation_embedding = nnx.Embed(
            feature_schema.CREATION_ORDER_CLIP + 1, width, rngs=rngs)
        self.given_embedding = nnx.Embed(2, width, rngs=rngs)
        self.constant_type_embedding = nnx.Embed(7, width, rngs=rngs)
        self.constant_bucket_embedding = nnx.Embed(
            feature_schema.CONSTANT_BUCKETS + 1, width, rngs=rngs)

        self.object_initial = MLP(3 * width, 128, width, rngs=rngs)
        self.fact_updates = nnx.List([
            MLP(2 * width, 128, width, rngs=rngs) for _ in range(2)])
        self.object_updates = nnx.List([
            MLP(2 * width, 128, width, rngs=rngs) for _ in range(2)])
        self.candidate_head = MLP(6 * width, 128, width, rngs=rngs)
        self.candidate_logit = nnx.Linear(width, 1, rngs=rngs)
        self.value_hidden = nnx.Linear(2 * width, width, rngs=rngs)
        self.value_output = nnx.Linear(width, 1, rngs=rngs)

    def __call__(self, arrays):
        kind = self.kind_embedding(arrays["object_kind"])
        creator = self.rule_embedding(arrays["object_creator_rule"])
        result_role = self.role_embedding(arrays["object_result_role"])
        creation = self.creation_embedding(arrays["object_creation_order"])
        given = self.given_embedding(arrays["object_given"].astype(jnp.int32))
        base_object = kind + creator + result_role + creation + given

        goal = (self.predicate_embedding(arrays["object_goal_predicate"])
                + self.role_embedding(arrays["object_goal_role"]))
        goal = _masked_mean(goal, arrays["object_goal_mask"], axis=2)
        parents = _gather_objects(base_object, arrays["object_parent"])
        parents = _masked_mean(parents, arrays["object_parent_mask"], axis=2)
        objects = nnx.tanh(self.object_initial(
            jnp.concatenate((base_object, goal, parents), axis=-1)))
        objects *= arrays["object_mask"][..., None]

        fact_state = self.predicate_embedding(arrays["fact_predicate"])
        fact_mask = arrays["fact_mask"]
        for fact_update, object_update in zip(
                self.fact_updates, self.object_updates):
            arguments = _gather_objects(objects, arrays["fact_object"])
            arguments += self.role_embedding(arrays["fact_role"])
            arguments += self.constant_type_embedding(
                arrays["fact_constant_type"])
            arguments += self.constant_bucket_embedding(
                arrays["fact_constant_bucket"])
            argument_mask = arrays["fact_role"] != 0
            argument_pool = _masked_mean(arguments, argument_mask, axis=2)
            fact_state = nnx.tanh(fact_update(
                jnp.concatenate((fact_state, argument_pool), axis=-1)))
            fact_state *= fact_mask[..., None]

            incidence = (fact_state[:, :, None, :]
                         + self.role_embedding(arrays["fact_role"]))
            incidence_mask = arrays["fact_object_mask"] & fact_mask[:, :, None]
            incidence *= incidence_mask[..., None]
            messages = jnp.zeros_like(objects)
            counts = jnp.zeros(objects.shape[:-1] + (1,), dtype=objects.dtype)
            batch = jnp.arange(objects.shape[0])[:, None, None]
            messages = messages.at[batch, arrays["fact_object"]].add(incidence)
            counts = counts.at[batch, arrays["fact_object"]].add(
                incidence_mask[..., None])
            messages /= jnp.maximum(counts, 1)
            objects = nnx.tanh(object_update(
                jnp.concatenate((objects, messages), axis=-1)))
            objects *= arrays["object_mask"][..., None]

        object_global = _masked_mean(
            objects, arrays["object_mask"], axis=1)
        fact_global = _masked_mean(fact_state, fact_mask, axis=1)
        global_state = jnp.concatenate((object_global, fact_global), axis=-1)

        candidate_arguments = _gather_objects(
            objects, arrays["candidate_object"])
        candidate_arguments += self.role_embedding(
            arrays["candidate_argument_role"])
        candidate_arguments += self.kind_embedding(
            arrays["candidate_argument_kind"])
        argument_pool = _masked_mean(
            candidate_arguments, arrays["candidate_argument_mask"], axis=2)
        premise_pool = _masked_mean(
            self.predicate_embedding(arrays["candidate_premise_predicate"]),
            arrays["candidate_premise_mask"], axis=2)
        conclusion_pool = _masked_mean(
            self.predicate_embedding(arrays["candidate_conclusion_predicate"]),
            arrays["candidate_conclusion_mask"], axis=2)
        rule = self.rule_embedding(arrays["candidate_rule"])
        repeated_global = jnp.broadcast_to(
            global_state[:, None, :],
            rule.shape[:-1] + (global_state.shape[-1],))
        candidate = self.candidate_head(jnp.concatenate((
            rule, argument_pool, premise_pool, conclusion_pool,
            repeated_global), axis=-1))
        logits = self.candidate_logit(nnx.relu(candidate))[..., 0]
        logits = jnp.where(arrays["candidate_mask"], logits, -1e9)

        value = self.value_output(nnx.relu(
            self.value_hidden(global_state)))[..., 0]
        return logits, nnx.tanh(value)


def batch_features(items):
    """Pad feature dictionaries and add a leading position dimension."""
    if not items:
        raise ValueError("at least one position is required")
    keys = items[0].keys()
    if any(item.keys() != keys for item in items[1:]):
        raise ValueError("feature dictionaries have different schemas")
    result = {}
    for key in keys:
        arrays = [np.asarray(item[key]) for item in items]
        rank = arrays[0].ndim
        if any(array.ndim != rank for array in arrays):
            raise ValueError(f"rank mismatch for {key}")
        shape = tuple(max(array.shape[axis] for array in arrays)
                      for axis in range(rank))
        padded = np.zeros((len(arrays),) + shape, dtype=arrays[0].dtype)
        for index, array in enumerate(arrays):
            slices = (index,) + tuple(slice(0, size) for size in array.shape)
            padded[slices] = array
        result[key] = jnp.asarray(padded)
    return result


def parameter_arrays(model):
    return [np.asarray(value) for value in
            jax.tree_util.tree_leaves(nnx.state(model, nnx.Param))]


def parameter_count(model):
    return sum(array.size for array in parameter_arrays(model))


def save_plain_arrays(model, destination):
    destination = Path(destination)
    destination.mkdir(parents=True, exist_ok=True)
    manifest = []
    for index, array in enumerate(parameter_arrays(model)):
        filename = f"{index:04d}.npy"
        np.save(destination / filename, array, allow_pickle=False)
        manifest.append({"file": filename, "shape": list(array.shape),
                         "dtype": str(array.dtype)})
    (destination / "manifest.json").write_text(
        json.dumps(manifest, indent=2) + "\n")


def load_plain_arrays(model, source):
    source = Path(source)
    manifest = json.loads((source / "manifest.json").read_text())
    state = nnx.state(model, nnx.Param)
    target, treedef = jax.tree_util.tree_flatten(state)
    if len(target) != len(manifest):
        raise ValueError("parameter leaf count does not match model")
    loaded = []
    for expected, entry in zip(target, manifest):
        array = np.load(source / entry["file"], mmap_mode="r",
                        allow_pickle=False)
        if array.shape != expected.shape or array.dtype != expected.dtype:
            raise ValueError("parameter shape or dtype does not match model")
        loaded.append(jnp.asarray(array))
    nnx.update(model, jax.tree_util.tree_unflatten(treedef, loaded))
