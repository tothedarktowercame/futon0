#!/usr/bin/env python3
"""Variable-length replay records and masked AlphaZero losses."""

from __future__ import annotations

import base64
from collections import deque
from dataclasses import dataclass
import json
import os
from pathlib import Path
import random
import math

from flax import nnx
import jax
import jax.numpy as jnp
import numpy as np

import euclid_candidate_features as feature_schema
import euclid_candidate_model as candidate_model


RECEIPT_FORMAT_VERSION = 2


def _candidate_to_json(candidate):
    rule, args, bindings = candidate
    return [rule, list(args), [list(binding) for binding in bindings]]


def _candidate_from_json(encoded):
    rule, args, bindings = encoded
    return rule, tuple(args), tuple(tuple(binding) for binding in bindings)


def _array_to_json(array):
    array = np.ascontiguousarray(array)
    return {
        "dtype": array.dtype.str,
        "shape": list(array.shape),
        "data": base64.b64encode(array.tobytes()).decode("ascii"),
    }


def _array_from_json(encoded):
    dtype = np.dtype(encoded["dtype"])
    shape = tuple(encoded["shape"])
    raw = base64.b64decode(encoded["data"], validate=True)
    expected = int(np.prod(shape, dtype=np.int64)) * dtype.itemsize
    if len(raw) != expected:
        raise ValueError("replay array byte count does not match its shape")
    return np.frombuffer(raw, dtype=dtype).reshape(shape).copy()


@dataclass(frozen=True)
class ReplayRecord:
    features: dict[str, np.ndarray]
    candidate_keys: tuple
    visits: tuple[int, ...]
    value: float
    game_id: str
    ply: int
    start_kind: str
    seed: int
    stage: str = "default"
    logical_state_key_sha256: str = ""
    schema_version: int = feature_schema.SCHEMA_VERSION
    vocabulary_version: str = feature_schema.VOCABULARY.vocabulary_version

    def __post_init__(self):
        feature_version = int(np.asarray(
            self.features["schema_version"]).reshape(-1)[0])
        feature_vocabulary = bytes(np.asarray(
            self.features["vocabulary_version"], dtype=np.uint8)).hex()
        if feature_version != self.schema_version:
            raise ValueError("record metadata and feature schema versions differ")
        if feature_vocabulary != self.vocabulary_version:
            raise ValueError("record metadata and feature vocabulary versions differ")
        candidates = int(np.asarray(self.features["candidate_mask"]).sum())
        if len(self.candidate_keys) != candidates or len(self.visits) != candidates:
            raise ValueError("candidate keys, visits, and feature mask are not aligned")
        if any(visit < 0 for visit in self.visits):
            raise ValueError("visit counts must be nonnegative")
        if self.schema_version != feature_schema.SCHEMA_VERSION:
            raise ValueError(
                f"replay schema version {self.schema_version} does not match "
                f"{feature_schema.SCHEMA_VERSION}")
        if self.vocabulary_version != feature_schema.VOCABULARY.vocabulary_version:
            raise ValueError(
                "replay vocabulary version does not match the current vocabulary")

    @classmethod
    def from_search(cls, feature_arrays, result, eventual_value, *, game_id,
                    ply, start_kind, seed, stage="default",
                    logical_state_key_sha256=""):
        return cls(
            {name: np.asarray(value).copy()
             for name, value in feature_arrays.items()},
            tuple(result.candidate_keys), tuple(result.visit_counts),
            float(eventual_value), str(game_id), int(ply), str(start_kind),
            int(seed), str(stage), str(logical_state_key_sha256))

    def to_json(self):
        return {
            "receipt_format_version": RECEIPT_FORMAT_VERSION,
            "schema_version": self.schema_version,
            "vocabulary_version": self.vocabulary_version,
            "provenance": {"game_id": self.game_id, "ply": self.ply,
                           "start_kind": self.start_kind, "seed": self.seed,
                           "stage": self.stage,
                           "logical_state_key_sha256":
                               self.logical_state_key_sha256},
            "candidate_keys": [_candidate_to_json(key)
                               for key in self.candidate_keys],
            "visits": list(self.visits),
            "value": self.value,
            "features": {name: _array_to_json(value)
                         for name, value in sorted(self.features.items())},
        }

    @classmethod
    def from_json(cls, encoded):
        if encoded.get("receipt_format_version") != RECEIPT_FORMAT_VERSION:
            raise ValueError("unsupported replay receipt format version")
        schema = encoded.get("schema_version")
        vocabulary = encoded.get("vocabulary_version")
        if schema != feature_schema.SCHEMA_VERSION:
            raise ValueError(
                f"replay schema version {schema} does not match "
                f"{feature_schema.SCHEMA_VERSION}")
        if vocabulary != feature_schema.VOCABULARY.vocabulary_version:
            raise ValueError(
                "replay vocabulary version does not match the current vocabulary")
        provenance = encoded["provenance"]
        return cls(
            {name: _array_from_json(value)
             for name, value in encoded["features"].items()},
            tuple(_candidate_from_json(value)
                  for value in encoded["candidate_keys"]),
            tuple(int(value) for value in encoded["visits"]),
            float(encoded["value"]), provenance["game_id"],
            int(provenance["ply"]), provenance["start_kind"],
            int(provenance["seed"]), provenance.get("stage", "default"),
            provenance.get("logical_state_key_sha256", ""),
            int(schema), vocabulary)


class ReceiptWriter:
    """Append one durable JSON line as each actor result arrives."""

    def __init__(self, path):
        self.path = Path(path)
        self.path.parent.mkdir(parents=True, exist_ok=True)
        self.stream = self.path.open("a", encoding="utf-8")

    def append(self, record):
        self.stream.write(json.dumps(record.to_json(), separators=(",", ":"))
                          + "\n")
        self.stream.flush()
        os.fsync(self.stream.fileno())

    def close(self):
        self.stream.close()

    def __enter__(self):
        return self

    def __exit__(self, *_exc):
        self.close()


def actor_receipt_path(directory, actor_id):
    return Path(directory) / f"actor-{int(actor_id)}.jsonl"


def read_receipts(path):
    records = []
    with Path(path).open(encoding="utf-8") as stream:
        for line_number, line in enumerate(stream, 1):
            if not line.strip():
                continue
            try:
                records.append(ReplayRecord.from_json(json.loads(line)))
            except (KeyError, TypeError, ValueError, json.JSONDecodeError) as error:
                raise ValueError(
                    f"invalid replay receipt at line {line_number}: {error}") from error
    return records


class ReplayBuffer:
    def __init__(self, capacity=256, seed=0):
        if capacity <= 0:
            raise ValueError("replay capacity must be positive")
        self.records = deque(maxlen=int(capacity))
        self.random = random.Random(seed)

    def append(self, record):
        self.records.append(record)

    def extend(self, records):
        self.records.extend(records)

    def sample(self, count, *, min_stage_share=0.0, stages=()):
        if count < 0 or count > len(self.records):
            raise ValueError("sample size exceeds the replay buffer")
        records = list(self.records)
        if not stages or min_stage_share <= 0:
            return self.random.sample(records, count)
        target = math.ceil(count * min_stage_share)
        available = {
            stage: [record for record in records if record.stage == stage]
            for stage in stages
        }
        active = [stage for stage in stages if available[stage]]
        if target * len(active) > count:
            raise ValueError("minimum replay stage shares exceed the batch")
        selected = []
        for stage in active:
            selected.extend(self.random.sample(
                available[stage], min(target, len(available[stage]))))
        selected_ids = {id(record) for record in selected}
        remainder = [record for record in records
                     if id(record) not in selected_ids]
        selected.extend(self.random.sample(remainder, count - len(selected)))
        self.random.shuffle(selected)
        return selected

    def __len__(self):
        return len(self.records)


@dataclass(frozen=True)
class ReplayBatch:
    features: dict[str, jax.Array]
    policy_target: jax.Array
    policy_row_mask: jax.Array
    value_target: jax.Array


def collate(records):
    if not records:
        raise ValueError("cannot collate an empty replay batch")
    features = candidate_model.batch_features(
        [record.features for record in records])
    candidate_count = features["candidate_mask"].shape[1]
    targets = np.zeros((len(records), candidate_count), dtype=np.float32)
    row_mask = np.zeros(len(records), dtype=np.bool_)
    for row, record in enumerate(records):
        visits = np.asarray(record.visits, dtype=np.float32)
        total = float(visits.sum())
        if total > 0:
            targets[row, :len(visits)] = visits / total
            row_mask[row] = True
    return ReplayBatch(
        features, jnp.asarray(targets), jnp.asarray(row_mask),
        jnp.asarray([record.value for record in records], dtype=jnp.float32))


def loss_from_outputs(logits, values, batch):
    """Return policy/value components with all padding exactly masked."""
    if logits.shape[1] == 0:
        policy_rows = jnp.zeros(logits.shape[0], dtype=values.dtype)
    else:
        masked = jnp.where(batch.features["candidate_mask"], logits, -1e30)
        policy_rows = -jnp.sum(
            batch.policy_target * jax.nn.log_softmax(masked, axis=1), axis=1)
    policy_weight = batch.policy_row_mask.astype(values.dtype)
    policy = (jnp.sum(policy_rows * policy_weight)
              / jnp.maximum(jnp.sum(policy_weight), 1))
    value = jnp.mean(jnp.square(values - batch.value_target))
    return policy, value


def model_loss(model, batch, l2_coefficient=1e-4):
    logits, values = model(batch.features)
    policy, value = loss_from_outputs(logits, values, batch)
    leaves = jax.tree_util.tree_leaves(nnx.state(model, nnx.Param))
    l2 = sum(jnp.sum(jnp.square(leaf)) for leaf in leaves)
    total = policy + value + l2_coefficient * l2
    return total, {"policy": policy, "value": value, "l2": l2,
                   "total": total}
