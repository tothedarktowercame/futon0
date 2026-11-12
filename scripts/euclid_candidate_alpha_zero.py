#!/usr/bin/env python3
"""Actor/learner loop for the grounded-candidate EuclidZero network."""

from __future__ import annotations

import argparse
from dataclasses import asdict, dataclass, replace
import hashlib
import json
import os
from pathlib import Path
import random
import signal
import subprocess
import sys
import time


@dataclass
class Config:
    proposition: int = 2
    move_cap: int = 4
    start_file: str = ""
    start_perturb: int = 0
    start_perturb_rate: float = 0.0
    start_seed: int = 0
    actors: int = 2
    simulations: int = 4
    c_puct: float = 1.41
    dirichlet_epsilon: float = 0.25
    dirichlet_alpha: float = 1.0
    temperature: float = 1.0
    temperature_drop: int = 4
    root_win_in_one: bool = True
    first_play_reduction: float = 0.0
    stage_mix: list | None = None
    replay_min_stage_share: float = 0.0
    initial_snapshot: str = ""
    buffer_size: int = 256
    batch_size: int = 8
    learning_rate: float = 0.001
    l2_coefficient: float = 0.0001
    snapshot_cadence: int = 1
    checkpoint_cadence: int = 10
    duration_seconds: int = 60
    poll_seconds: float = 0.1
    thread_cpus: str = "20-31"
    seed: int = 0
    evaluation_games: int = 10
    evaluation_game_seconds: int = 60

    @classmethod
    def load(cls, path):
        values = json.loads(Path(path).read_text())
        unknown = set(values) - set(cls.__dataclass_fields__)
        if unknown:
            raise ValueError(f"unknown config fields: {sorted(unknown)}")
        return cls(**values)

    def save(self, path):
        Path(path).write_text(json.dumps(asdict(self), indent=2,
                                        sort_keys=True) + "\n")


def _cpu_pool(encoded):
    result = []
    for part in encoded.split(","):
        if not part:
            continue
        if "-" in part:
            first, last = map(int, part.split("-", 1))
            result.extend(range(first, last + 1))
        else:
            result.append(int(part))
    if not result:
        raise ValueError("thread_cpus must name at least one CPU")
    return result


def _thread_environment():
    environment = dict(os.environ, PYTHONHASHSEED="0")
    for name in ("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS",
                 "MKL_NUM_THREADS", "NUMEXPR_NUM_THREADS",
                 "TF_NUM_INTRAOP_THREADS", "TF_NUM_INTEROP_THREADS"):
        environment[name] = "1"
    return environment


def _log(path, event, **fields):
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("a") as stream:
        stream.write(json.dumps({"time": time.time(), "event": event,
                                 **fields}, separators=(",", ":")) + "\n")
        stream.flush()
        os.fsync(stream.fileno())


def _game(config, *, seed_offset=0):
    import pyspiel
    import euclid_spiel  # noqa: F401

    start = f",start_file={Path(config.start_file).resolve()}" if config.start_file else ""
    return pyspiel.load_game(
        f"euclid_book1(proposition={config.proposition},move_cap={config.move_cap},"
        f"dense_actions=true,alpha_zero_facade=true{start},"
        f"start_perturb={config.start_perturb},"
        f"start_perturb_rate={config.start_perturb_rate},"
        f"start_seed={config.start_seed + seed_offset})")


def _stage_entries(config):
    entries = config.stage_mix or [{
        "name": "default", "weight": 1.0,
        "start_file": config.start_file,
        "start_perturb": config.start_perturb,
        "start_perturb_rate": config.start_perturb_rate,
        "move_cap": config.move_cap,
    }]
    allowed = {"name", "weight", "start_file", "start_perturb",
               "start_perturb_rate", "move_cap", "start_seed"}
    names = set()
    result = []
    for raw in entries:
        unknown = set(raw) - allowed
        if unknown:
            raise ValueError(f"unknown stage fields: {sorted(unknown)}")
        entry = {
            "name": str(raw["name"]), "weight": float(raw["weight"]),
            "start_file": str(raw.get("start_file", config.start_file)),
            "start_perturb": int(raw.get("start_perturb", config.start_perturb)),
            "start_perturb_rate": float(raw.get(
                "start_perturb_rate", config.start_perturb_rate)),
            "move_cap": int(raw.get("move_cap", config.move_cap)),
            "start_seed": int(raw.get("start_seed", config.start_seed)),
        }
        if not entry["name"] or entry["name"] in names:
            raise ValueError("stage names must be nonempty and unique")
        if entry["weight"] <= 0:
            raise ValueError("stage weights must be positive")
        names.add(entry["name"])
        result.append(entry)
    return result


def _choose_stage(entries, rng):
    point = rng.random() * sum(entry["weight"] for entry in entries)
    for entry in entries:
        point -= entry["weight"]
        if point < 0:
            return entry
    return entries[-1]


def _stage_config(config, entry):
    return replace(
        config, start_file=entry["start_file"],
        start_perturb=entry["start_perturb"],
        start_perturb_rate=entry["start_perturb_rate"],
        move_cap=entry["move_cap"], start_seed=entry["start_seed"])


def _logical_state_key_sha256(state):
    return hashlib.sha256(
        repr(state.logical_state_key()).encode("utf-8")).hexdigest()


def _current_snapshot(out):
    path = Path(out) / "snapshots/current.json"
    return json.loads(path.read_text()) if path.exists() else None


def _publish_snapshot(model, out, generation, learner_step):
    import euclid_candidate_features as schema
    import euclid_candidate_model as candidate_model

    root = Path(out) / "snapshots"
    destination = root / f"generation-{generation:06d}"
    candidate_model.save_plain_arrays(model, destination)
    metadata = {
        "generation": generation, "learner_step": learner_step,
        "snapshot": str(destination.resolve()),
        "schema_version": schema.SCHEMA_VERSION,
        "vocabulary_version": schema.VOCABULARY.vocabulary_version,
    }
    temporary = root / f"current.{os.getpid()}.tmp"
    temporary.write_text(json.dumps(metadata, sort_keys=True) + "\n")
    os.replace(temporary, root / "current.json")
    return metadata


def _load_snapshot_verified(model, out, previous_generation=-1):
    import euclid_candidate_features as schema
    import euclid_candidate_model as candidate_model

    metadata = _current_snapshot(out)
    if metadata is None or metadata["generation"] <= previous_generation:
        return previous_generation, None
    if metadata["schema_version"] != schema.SCHEMA_VERSION:
        raise ValueError("snapshot schema version mismatch")
    if metadata["vocabulary_version"] != schema.VOCABULARY.vocabulary_version:
        raise ValueError("snapshot vocabulary version mismatch")
    candidate_model.load_plain_arrays(model, metadata["snapshot"])
    confirmed = _current_snapshot(out)
    if confirmed is None or confirmed["generation"] != metadata["generation"]:
        return previous_generation, None
    return metadata["generation"], metadata


def _optimizer_arrays(optimizer):
    from flax import nnx
    import jax
    return [value for value in jax.tree_util.tree_leaves(
        nnx.state(optimizer, nnx.OptState))]


def _save_checkpoint(model, optimizer, out, step, generation):
    import numpy as np
    import euclid_candidate_features as schema
    import euclid_candidate_model as candidate_model

    destination = Path(out) / "checkpoints" / f"step-{step:06d}"
    candidate_model.save_plain_arrays(model, destination / "model")
    optimizer_dir = destination / "optimizer"
    optimizer_dir.mkdir(parents=True, exist_ok=True)
    manifest = []
    for index, leaf in enumerate(_optimizer_arrays(optimizer)):
        array = np.asarray(leaf)
        filename = f"{index:04d}.npy"
        np.save(optimizer_dir / filename, array, allow_pickle=False)
        manifest.append({"file": filename, "shape": list(array.shape),
                         "dtype": str(array.dtype)})
    (optimizer_dir / "manifest.json").write_text(
        json.dumps(manifest, indent=2) + "\n")
    (destination / "metadata.json").write_text(json.dumps({
        "step": step, "generation": generation,
        "schema_version": schema.SCHEMA_VERSION,
        "vocabulary_version": schema.VOCABULARY.vocabulary_version,
    }, indent=2, sort_keys=True) + "\n")
    return destination


def _tail_receipts(receipt_dir, offsets):
    import euclid_candidate_replay as replay

    records = []
    for path in sorted(Path(receipt_dir).glob("actor-*.jsonl")):
        offset = offsets.get(path, 0)
        with path.open("rb") as stream:
            stream.seek(offset)
            while True:
                before = stream.tell()
                line = stream.readline()
                if not line:
                    break
                if not line.endswith(b"\n"):
                    stream.seek(before)
                    break
                records.append(replay.ReplayRecord.from_json(
                    json.loads(line.decode("utf-8"))))
            offsets[path] = stream.tell()
    return records


def learner_main(config_path, out):
    config = Config.load(config_path)
    log = Path(out) / "logs/learner.jsonl"
    stop = False

    def request_stop(_signum, _frame):
        nonlocal stop
        stop = True

    signal.signal(signal.SIGTERM, request_stop)
    signal.signal(signal.SIGINT, request_stop)
    from flax import nnx
    import optax
    import euclid_candidate_model as candidate_model
    import euclid_candidate_replay as replay

    model = candidate_model.CandidateModel(seed=config.seed)
    if config.initial_snapshot:
        candidate_model.load_plain_arrays(model, config.initial_snapshot)
    optimizer = nnx.Optimizer(
        model, optax.adam(config.learning_rate), wrt=nnx.Param)
    buffer = replay.ReplayBuffer(config.buffer_size, config.seed)
    stage_names = [entry["name"] for entry in _stage_entries(config)]
    generation = step = 0
    metadata = _publish_snapshot(model, out, generation, step)
    _log(log, "snapshot", **metadata)
    offsets = {}
    while not stop:
        new_records = _tail_receipts(Path(out) / "receipts", offsets)
        buffer.extend(new_records)
        if len(buffer) >= config.batch_size:
            batch = replay.collate(buffer.sample(
                config.batch_size,
                min_stage_share=config.replay_min_stage_share,
                stages=stage_names))
            (loss, components), gradients = nnx.value_and_grad(
                replay.model_loss, has_aux=True)(
                    model, batch, config.l2_coefficient)
            optimizer.update(model, gradients)
            step += 1
            _log(log, "learner_step", step=step, records=len(buffer),
                 loss=float(loss), **{name: float(value)
                                      for name, value in components.items()})
            if step % config.snapshot_cadence == 0:
                generation += 1
                metadata = _publish_snapshot(model, out, generation, step)
                _log(log, "snapshot", **metadata)
            if step % config.checkpoint_cadence == 0:
                checkpoint = _save_checkpoint(
                    model, optimizer, out, step, generation)
                _log(log, "checkpoint", step=step,
                     checkpoint_path=str(checkpoint))
        time.sleep(config.poll_seconds)
    checkpoint = _save_checkpoint(model, optimizer, out, step, generation)
    _log(log, "shutdown", step=step, generation=generation,
         records=len(buffer), checkpoint=str(checkpoint))


def _fresh_referee(config, state):
    import euclid_spiel
    all_moves = state.initial_moves + state.history_moves
    fresh = euclid_spiel.play._start_state(config.proposition)
    for ply, (rule, args, bindings) in enumerate(all_moves, 1):
        euclid_spiel.eng.apply_step(fresh, ply, rule, args, dict(bindings))
    checked_state = euclid_spiel.EuclidState(state._euclid_game, fresh)
    return (checked_state.goal_reached()
            and euclid_spiel.play._goal_reached(
                config.proposition, fresh, checked_state._object_list()))


def actor_main(config_path, out, actor_id):
    config = Config.load(config_path)
    actor_id = int(actor_id)
    log = Path(out) / f"logs/actor-{actor_id}.jsonl"
    stop = False

    def request_stop(_signum, _frame):
        nonlocal stop
        stop = True

    signal.signal(signal.SIGTERM, request_stop)
    signal.signal(signal.SIGINT, request_stop)
    import euclid_candidate_features as feature_encoder
    import euclid_candidate_model as candidate_model
    import euclid_candidate_puct as candidate_puct
    import euclid_candidate_replay as replay

    model = candidate_model.CandidateModel(seed=config.seed + actor_id + 1)
    generation = -1
    while generation < 0 and not stop:
        generation, metadata = _load_snapshot_verified(model, out, generation)
        if metadata is None:
            time.sleep(config.poll_seconds)
    if stop:
        return
    if any(name.startswith("orbax") for name in sys.modules):
        raise RuntimeError("actor imported Orbax")
    _log(log, "snapshot_loaded", generation=generation,
         learner_step=metadata["learner_step"], orbax_imported=False)
    stage_entries = _stage_entries(config)
    stage_rng = random.Random(config.seed + actor_id * 1_000_000)
    stage_games = {
        entry["name"]: _game(
            _stage_config(config, entry), seed_offset=actor_id * 1_000_000)
        for entry in stage_entries
    }
    bot = candidate_puct.CandidatePUCT(
        model, simulations=config.simulations, uct_c=config.c_puct,
        root_win_in_one=config.root_win_in_one,
        first_play_reduction=config.first_play_reduction,
        dirichlet=(config.dirichlet_epsilon, config.dirichlet_alpha),
        temperature=config.temperature, seed=config.seed + actor_id)
    receipt_path = replay.actor_receipt_path(Path(out) / "receipts", actor_id)
    games = 0
    with replay.ReceiptWriter(receipt_path) as writer:
        while not stop:
            loaded, metadata = _load_snapshot_verified(model, out, generation)
            if loaded != generation:
                generation = loaded
                _log(log, "snapshot_loaded", generation=generation,
                     learner_step=metadata["learner_step"], orbax_imported=False)
            stage = _choose_stage(stage_entries, stage_rng)
            game = stage_games[stage["name"]]
            state = game.new_initial_state()
            pending = []
            while not stop and not state.is_terminal():
                candidates = candidate_puct.premise_certified_candidates(state)
                arrays = feature_encoder.extract(state, candidates).arrays
                bot.temperature = (config.temperature
                                   if len(state.history_moves) < config.temperature_drop
                                   else 0.0)
                result = bot.search(state)
                if result.candidate is None:
                    break
                # Root validation can remove refused candidates, so align the
                # stored tensors to the search's final deterministic keys.
                arrays = feature_encoder.extract(
                    state, list(result.candidate_keys)).arrays
                pending.append((arrays, result, len(state.history_moves),
                                _logical_state_key_sha256(state)))
                state.apply_action_tuple(result.candidate)
            value = 1.0 if state.goal_reached() else -1.0
            for arrays, result, ply, state_key in pending:
                writer.append(replay.ReplayRecord.from_search(
                    arrays, result, value, game_id=f"euclid-i{config.proposition}",
                    ply=ply, start_kind=("perturbed" if
                        len(state.initial_moves) > len(game.start_moves) else "clean"),
                    seed=config.seed + actor_id, stage=stage["name"],
                    logical_state_key_sha256=state_key))
            games += 1
            _log(log, "game", game=games, positions=len(pending), value=value,
                 generation=generation, moves=len(state.history_moves),
                 stage=stage["name"], initial_moves=state.initial_moves,
                 history_moves=state.history_moves)
            bot.clear_game_cache()
    _log(log, "shutdown", games=games, generation=generation)


def _service_tasks_path():
    for line in Path("/proc/self/cgroup").read_text().splitlines():
        if line.startswith("0::"):
            relative = Path(line[3:].lstrip("/"))
            parts = relative.parts
            service = next((index for index, part in enumerate(parts)
                            if part == "futon3c-zone.service"), None)
            if service is not None:
                relative = Path(*parts[:service + 1])
            path = Path("/sys/fs/cgroup") / relative
            candidate = path / "pids.current"
            return candidate if candidate.exists() else None
    return None


def run_main(config_path, out):
    config_path, out = Path(config_path).resolve(), Path(out).resolve()
    config = Config.load(config_path)
    out.mkdir(parents=True, exist_ok=True)
    config.save(out / "config.json")
    cpus = _cpu_pool(config.thread_cpus)
    environment = _thread_environment()
    processes = []
    stopping = False

    def launch(role, identity, cpu):
        command = ["taskset", "-c", str(cpu), sys.executable,
                   str(Path(__file__).resolve()), role,
                   "--config", str(config_path), "--out", str(out)]
        if identity is not None:
            command += ["--actor-id", str(identity)]
        process = subprocess.Popen(command, env=environment)
        processes.append(process)
        return process

    def stop_all(_signum=None, _frame=None):
        nonlocal stopping
        if stopping:
            return
        stopping = True
        for process in processes:
            if process.poll() is None:
                process.send_signal(signal.SIGTERM)

    signal.signal(signal.SIGTERM, stop_all)
    signal.signal(signal.SIGINT, stop_all)
    learner = launch("learner", None, cpus[0])
    deadline = time.monotonic() + 60
    while _current_snapshot(out) is None:
        if learner.poll() is not None:
            raise RuntimeError("learner exited before publishing generation 0")
        if time.monotonic() > deadline:
            stop_all()
            raise TimeoutError("learner did not publish generation 0")
        time.sleep(0.1)
    for actor_id in range(config.actors):
        # Reserve the first configured CPU for the learner when the pool has
        # enough entries; actors otherwise wrap deterministically.
        launch("actor", actor_id, cpus[(actor_id + 1) % len(cpus)])
    tasks_path = _service_tasks_path()
    peak_tasks = 0
    finish = time.monotonic() + config.duration_seconds
    while not stopping and time.monotonic() < finish:
        if tasks_path:
            peak_tasks = max(peak_tasks, int(tasks_path.read_text()))
        failed = [process for process in processes
                  if process.poll() not in (None, 0)]
        if failed:
            stop_all()
            raise RuntimeError(f"worker exited with {failed[0].returncode}")
        time.sleep(0.5)
    stop_all()
    grace = time.monotonic() + 30
    for process in processes:
        remaining = max(0.1, grace - time.monotonic())
        try:
            process.wait(timeout=remaining)
        except subprocess.TimeoutExpired:
            # This PID was launched and recorded above; escalation is confined
            # to our own worker after an orderly SIGTERM grace period.
            process.kill()
            process.wait()
    summaries = {}
    for actor_id in range(config.actors):
        path = out / f"logs/actor-{actor_id}.jsonl"
        events = [json.loads(line) for line in path.read_text().splitlines()]
        summaries[str(actor_id)] = {
            "games": sum(event["event"] == "game" for event in events),
            "generations": sorted({event["generation"] for event in events
                                   if event["event"] == "snapshot_loaded"}),
        }
    learner_events = [json.loads(line) for line in
                      (out / "logs/learner.jsonl").read_text().splitlines()]
    result = {"peak_tasks": peak_tasks, "actors": summaries,
              "learner_steps": sum(event["event"] == "learner_step"
                                   for event in learner_events),
              "duration_seconds": config.duration_seconds}
    (out / "summary.json").write_text(
        json.dumps(result, indent=2, sort_keys=True) + "\n")
    print(json.dumps(result, sort_keys=True))


def evaluate_main(config_path, out, snapshot, games=None, game_seconds=None):
    config = Config.load(config_path)
    games = games or config.evaluation_games
    game_seconds = game_seconds or config.evaluation_game_seconds
    cpus = _cpu_pool(config.thread_cpus)
    os.sched_setaffinity(0, {cpus[0]})
    import euclid_candidate_model as candidate_model
    import euclid_candidate_puct as candidate_puct
    import euclid_candidate_replay as replay

    model = candidate_model.CandidateModel(seed=config.seed)
    candidate_model.load_plain_arrays(model, snapshot)
    os.sched_setaffinity(0, set(cpus))
    game = _game(config)
    wins = timeouts = 0
    records = []

    def timeout(_signum, _frame):
        raise TimeoutError("evaluation game exceeded its time limit")

    previous = signal.signal(signal.SIGALRM, timeout)
    try:
        for index in range(games):
            signal.alarm(game_seconds)
            try:
                state = game.new_initial_state()
                bot = candidate_puct.CandidatePUCT(
                    model, simulations=config.simulations, uct_c=config.c_puct,
                    root_win_in_one=config.root_win_in_one,
                    first_play_reduction=config.first_play_reduction,
                    temperature=0.0, seed=config.seed + index)
                while not state.is_terminal():
                    result = bot.search(state)
                    if result.candidate is None:
                        break
                    state.apply_action_tuple(result.candidate)
                if state.goal_reached():
                    if not _fresh_referee(config, state):
                        raise AssertionError("evaluation win failed fresh referees")
                    wins += 1
                    records.append({
                        "initial_moves": state.initial_moves,
                        "moves": state.history_moves,
                        "spiel_goal_reached": True,
                        "play_goal_reached": True,
                    })
            except TimeoutError:
                timeouts += 1
            finally:
                signal.alarm(0)
    finally:
        signal.signal(signal.SIGALRM, previous)
    output = Path(out)
    output.mkdir(parents=True, exist_ok=True)
    with (output / "evaluation-wins.jsonl").open("w") as stream:
        for record in records:
            stream.write(json.dumps(record) + "\n")
    result = {"games": games, "wins": wins, "timeouts": timeouts}
    (output / "evaluation.json").write_text(json.dumps(result, indent=2) + "\n")
    print(json.dumps(result, sort_keys=True))


def main():
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(dest="command", required=True)
    for command in ("run", "learner", "actor", "eval"):
        child = subparsers.add_parser(command)
        child.add_argument("--config", type=Path, required=True)
        child.add_argument("--out", type=Path, required=True)
        if command == "actor":
            child.add_argument("--actor-id", type=int, required=True)
        if command == "eval":
            child.add_argument("--snapshot", type=Path, required=True)
            child.add_argument("--games", type=int)
            child.add_argument("--game-seconds", type=int)
    args = parser.parse_args()
    if args.command == "run":
        run_main(args.config, args.out)
    elif args.command == "learner":
        learner_main(args.config, args.out)
    elif args.command == "actor":
        actor_main(args.config, args.out, args.actor_id)
    else:
        evaluate_main(args.config, args.out, args.snapshot,
                      args.games, args.game_seconds)


if __name__ == "__main__":
    main()
