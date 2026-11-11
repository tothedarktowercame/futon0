#!/usr/bin/env python3
"""Small AlphaZero-style learner for the rules-only Euclid OpenSpiel game.

OpenSpiel's bundled AlphaZero requires JAX/Flax and a dense policy vector over
all action ids. Euclid has a large structured action space, so this learner
uses a PyTorch state/action scorer and neutral progressive expansion. Nothing
outside the game state, rule palette, and terminal reward enters the model.
"""

from __future__ import annotations

import argparse
from collections import deque
import hashlib
import json
from pathlib import Path
import random
import sys
import time

import numpy as np
import pyspiel
import torch
from torch import nn

sys.path.insert(0, str(Path(__file__).resolve().parent))
import euclid_spiel  # noqa: E402


ACTION_FEATURES = 256


def _bucket(token, size=ACTION_FEATURES):
    return int.from_bytes(hashlib.blake2b(repr(token).encode(), digest_size=8).digest(), "big") % size


def action_features(state, action):
    rule, args, _bindings = action
    order = state._object_order()
    object_index = {name: i for i, name in enumerate(order)}
    kinds = euclid_spiel._objects(state.engine_state, state.initial_kinds)
    out = np.zeros(ACTION_FEATURES, dtype=np.float32)
    out[_bucket(("rule", rule))] += 1
    for position, arg in enumerate(args):
        identity = object_index.get(arg, ("name", arg))
        out[_bucket(("arg", position, identity))] += 1
        out[_bucket(("kind", position, kinds.get(arg, "unknown")))] += 1
    return out


class PolicyValueNet(nn.Module):
    def __init__(self, hidden=64):
        super().__init__()
        self.state = nn.Sequential(nn.Linear(euclid_spiel.OBSERVATION_SIZE, hidden), nn.ReLU())
        self.action = nn.Sequential(nn.Linear(ACTION_FEATURES, hidden), nn.ReLU())
        self.policy_bias = nn.Linear(ACTION_FEATURES, 1)
        self.value = nn.Sequential(nn.Linear(hidden, hidden), nn.ReLU(), nn.Linear(hidden, 1), nn.Sigmoid())

    def forward(self, observation, actions):
        state = self.state(observation)
        encoded_actions = self.action(actions)
        logits = encoded_actions @ state + self.policy_bias(actions).squeeze(-1)
        return logits, self.value(state).squeeze(-1)


class Node:
    def __init__(self):
        self.actions = None
        self.priors = None
        self.visits = None
        self.values = None
        self.children = None


def _candidate_sample(state, rng, count):
    found = {}
    for _ in range(count * 20):
        action = state.sample_action_tuple(rng)
        found[(action[0], action[1])] = action
        if len(found) >= count:
            break
    return [found[key] for key in sorted(found)]


def _expand(model, state, node, rng, candidates):
    node.actions = _candidate_sample(state, rng, candidates)
    obs = torch.from_numpy(state.observation_tensor())
    feats = torch.from_numpy(np.stack([action_features(state, action) for action in node.actions]))
    with torch.no_grad():
        logits, value = model(obs, feats)
        node.priors = torch.softmax(logits, 0).numpy()
    node.visits = np.zeros(len(node.actions), dtype=np.int32)
    node.values = np.zeros(len(node.actions), dtype=np.float32)
    node.children = [Node() for _ in node.actions]
    return float(value)


def _simulate(model, state, node, rng, candidates, c_puct=1.5):
    if state.is_terminal():
        return state.returns()[0]
    if node.actions is None:
        return _expand(model, state, node, rng, candidates)
    total = max(1, int(node.visits.sum()))
    q = np.divide(node.values, node.visits, out=np.zeros_like(node.values), where=node.visits > 0)
    scores = q + c_puct * node.priors * np.sqrt(total) / (1 + node.visits)
    choice = int(np.argmax(scores))
    child_state = state.clone()
    child_state.apply_action_tuple(node.actions[choice])
    value = _simulate(model, child_state, node.children[choice], rng, candidates, c_puct)
    node.visits[choice] += 1
    node.values[choice] += value
    return value


def search(model, state, rng, simulations, candidates):
    root = Node()
    for _ in range(simulations):
        _simulate(model, state.clone(), root, rng, candidates)
    policy = root.visits.astype(np.float64)
    if policy.sum() == 0:
        policy = root.priors.astype(np.float64)
    policy /= policy.sum()
    return root.actions, policy


def self_play(model, game, rng, simulations, candidates):
    state = game.new_initial_state()
    examples = []
    while not state.is_terminal():
        actions, policy = search(model, state, rng, simulations, candidates)
        examples.append((state.observation_tensor(),
                         np.stack([action_features(state, a) for a in actions]), policy.astype(np.float32)))
        choice = rng.choices(range(len(actions)), weights=policy, k=1)[0]
        state.apply_action_tuple(actions[choice])
    reward = state.returns()[0]
    return [(obs, acts, policy, reward) for obs, acts, policy in examples], int(reward)


def train_steps(model, optimizer, replay, rng, steps=8, batch=16):
    if not replay:
        return 0.0
    losses = []
    model.train()
    for _ in range(steps):
        optimizer.zero_grad()
        loss = 0
        for sample in rng.sample(list(replay), min(batch, len(replay))):
            obs, acts, target_policy, target_value = sample
            logits, value = model(torch.from_numpy(obs), torch.from_numpy(acts))
            policy_loss = -(torch.from_numpy(target_policy) * torch.log_softmax(logits, 0)).sum()
            value_loss = (value - float(target_value)) ** 2
            loss = loss + policy_loss + value_loss
        loss = loss / min(batch, len(replay))
        loss.backward()
        optimizer.step()
        losses.append(float(loss.detach()))
    return sum(losses) / len(losses)


def evaluate(model, game, seed, games, simulations, candidates):
    rng = random.Random(seed)
    wins = 0
    model.eval()
    for _ in range(games):
        state = game.new_initial_state()
        while not state.is_terminal():
            actions, policy = search(model, state, rng, simulations, candidates)
            state.apply_action_tuple(actions[int(np.argmax(policy))])
        wins += int(state.returns()[0] > 0)
    return wins


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--proposition", type=int, required=True)
    parser.add_argument("--minutes", type=float, required=True)
    parser.add_argument("--out", type=Path, required=True)
    parser.add_argument("--seed", type=int, default=20261111)
    parser.add_argument("--simulations", type=int, default=12)
    parser.add_argument("--candidates", type=int, default=12)
    parser.add_argument("--eval-games", type=int, default=20)
    parser.add_argument("--eval-every", type=float, default=600)
    parser.add_argument("--move-cap", type=int, default=6)
    args = parser.parse_args()

    torch.set_num_threads(1)
    torch.manual_seed(args.seed + args.proposition)
    rng = random.Random(args.seed + args.proposition)
    game = pyspiel.load_game(
        f"euclid_book1(proposition={args.proposition},move_cap={args.move_cap})"
    )
    model = PolicyValueNet()
    optimizer = torch.optim.Adam(model.parameters(), lr=1e-3)
    replay = deque(maxlen=4000)
    args.out.mkdir(parents=True, exist_ok=True)
    log_path = args.out / f"i{args.proposition}.jsonl"
    checkpoint = args.out / f"i{args.proposition}.pt"
    start = time.monotonic()
    next_eval = start
    deadline = start + args.minutes * 60
    games = wins = 0
    first_win = None
    with log_path.open("w") as log:
        while time.monotonic() < deadline:
            trajectory, won = self_play(model, game, rng, args.simulations, args.candidates)
            replay.extend(trajectory)
            games += 1
            wins += won
            if won and first_win is None:
                first_win = games
            loss = train_steps(model, optimizer, replay, rng)
            now = time.monotonic()
            if now >= next_eval or now >= deadline:
                eval_wins = evaluate(model, game, args.seed + games, args.eval_games,
                                     args.simulations, args.candidates)
                row = {"minutes": (now - start) / 60, "self_play_games": games,
                       "self_play_wins": wins, "first_win_game": first_win,
                       "eval_games": args.eval_games, "eval_wins": eval_wins, "loss": loss}
                print(json.dumps(row), flush=True)
                log.write(json.dumps(row) + "\n"); log.flush()
                torch.save({"model": model.state_dict(), "row": row, "args": vars(args)}, checkpoint)
                next_eval += args.eval_every


if __name__ == "__main__":
    main()
