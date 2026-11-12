import json

from flax import nnx
import numpy as np
import optax

import euclid_candidate_alpha_zero as loop
import euclid_candidate_features as schema
import euclid_candidate_model as candidate_model
import euclid_candidate_replay as replay


def test_config_round_trip_records_every_field(tmp_path):
    config = loop.Config(
        proposition=2, move_cap=7, start_file="prefix.json",
        start_perturb=2, start_perturb_rate=0.5, start_seed=11,
        actors=3, actor_restart_games=2, simulations=9, c_puct=1.7,
        dirichlet_epsilon=0.2,
        dirichlet_alpha=0.4, temperature=0.8, temperature_drop=3,
        root_win_in_one=False, first_play_reduction=0.2,
        stage_mix=[{"name": "near", "weight": 0.4,
                    "start_file": "prefix.json", "start_perturb": 1}],
        replay_min_stage_share=0.3, initial_snapshot="model",
        buffer_size=99, batch_size=5,
        learning_rate=0.002, l2_coefficient=0.003,
        snapshot_cadence=4, checkpoint_cadence=7, duration_seconds=12,
        poll_seconds=0.2, thread_cpus="20-21", seed=13,
        evaluation_games=6, evaluation_game_seconds=17)
    path = tmp_path / "config.json"
    config.save(path)
    assert loop.Config.load(path) == config
    assert set(json.loads(path.read_text())) == set(config.__dataclass_fields__)


def test_game_boundary_releases_successors_and_jax_compilations(monkeypatch):
    called = []
    class Bot:
        def clear_game_cache(self):
            called.append("successors")
    import jax
    monkeypatch.setattr(jax, "clear_caches", lambda: called.append("jax"))
    loop._release_game_resources(Bot())
    assert called == ["successors", "jax"]


def test_stage_mix_is_seeded_weighted_and_names_receipt_provenance():
    config = loop.Config(stage_mix=[
        {"name": "stage-1", "weight": 1},
        {"name": "stage-2", "weight": 3},
    ])
    entries = loop._stage_entries(config)
    import random
    chosen = [loop._choose_stage(entries, random.Random(seed))["name"]
              for seed in range(1000)]
    assert 700 <= chosen.count("stage-2") <= 800

    state_features = {
        "schema_version": np.asarray([schema.SCHEMA_VERSION], np.int32),
        "vocabulary_version": np.frombuffer(
            bytes.fromhex(schema.VOCABULARY.vocabulary_version), np.uint8),
        "candidate_mask": np.zeros(0, np.bool_),
    }
    record = replay.ReplayRecord(
        state_features, (), (), -1.0, "game", 2, "perturbed", 17,
        "stage-2", "abc123")
    encoded = record.to_json()
    assert encoded["provenance"]["stage"] == "stage-2"
    assert encoded["provenance"]["logical_state_key_sha256"] == "abc123"
    restored = replay.ReplayRecord.from_json(encoded)
    assert (restored.stage, restored.logical_state_key_sha256) == (
        "stage-2", "abc123")


def test_snapshot_generation_is_verified_and_loads_array_for_array(tmp_path):
    source = candidate_model.CandidateModel(seed=1)
    metadata = loop._publish_snapshot(source, tmp_path, 3, 8)
    assert metadata["schema_version"] == schema.SCHEMA_VERSION
    target = candidate_model.CandidateModel(seed=2)
    generation, loaded = loop._load_snapshot_verified(target, tmp_path)
    assert generation == 3
    assert loaded["learner_step"] == 8
    for expected, actual in zip(candidate_model.parameter_arrays(source),
                                candidate_model.parameter_arrays(target)):
        assert np.array_equal(expected, actual)
    assert loop._load_snapshot_verified(target, tmp_path, generation) == (
        generation, None)


def test_checkpoint_contains_optimizer_and_version_metadata(tmp_path):
    model = candidate_model.CandidateModel(seed=3)
    optimizer = nnx.Optimizer(model, optax.adam(1e-3), wrt=nnx.Param)
    destination = loop._save_checkpoint(
        model, optimizer, tmp_path, step=4, generation=2)
    metadata = json.loads((destination / "metadata.json").read_text())
    assert metadata == {
        "generation": 2, "schema_version": schema.SCHEMA_VERSION,
        "step": 4,
        "vocabulary_version": schema.VOCABULARY.vocabulary_version,
    }
    manifest = json.loads(
        (destination / "optimizer/manifest.json").read_text())
    assert manifest
    assert all((destination / "optimizer" / entry["file"]).exists()
               for entry in manifest)


def test_receipt_tailer_waits_for_complete_line(tmp_path):
    state_features = {
        "schema_version": np.asarray([schema.SCHEMA_VERSION], np.int32),
        "vocabulary_version": np.frombuffer(
            bytes.fromhex(schema.VOCABULARY.vocabulary_version), np.uint8),
        "candidate_mask": np.zeros(0, np.bool_),
    }
    # The record constructor only requires the version and candidate arrays;
    # collation is not involved in this transport-level test.
    record = replay.ReplayRecord(
        state_features, (), (), 0.0, "dead", 0, "clean", 1)
    encoded = json.dumps(record.to_json())
    receipt_dir = tmp_path / "receipts"
    receipt_dir.mkdir()
    path = receipt_dir / "actor-0.jsonl"
    path.write_text(encoded)
    offsets = {}
    assert loop._tail_receipts(receipt_dir, offsets) == []
    with path.open("a") as stream:
        stream.write("\n")
    loaded = loop._tail_receipts(receipt_dir, offsets)
    assert len(loaded) == 1
    assert loaded[0].game_id == "dead"
