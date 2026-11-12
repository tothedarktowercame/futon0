#!/usr/bin/env python3
"""Guarded PUCT over position-local grounded Euclid candidates."""

from __future__ import annotations

from dataclasses import dataclass, field
import math
import time

from flax import nnx
import numpy as np

import euclid_candidate_features as feature_encoder
import euclid_candidate_model as candidate_model
import euclid_spiel


_REFUSED = object()


@dataclass
class Node:
    state: object
    prior: float = 1.0
    visits: int = 0
    value_sum: float = 0.0
    network_value: float = 0.0
    expanded: bool = False
    dead_end: bool = False
    candidates: list = field(default_factory=list)
    priors: np.ndarray = field(default_factory=lambda: np.empty(0))
    children: dict = field(default_factory=dict)

    @property
    def value(self):
        return self.value_sum / self.visits if self.visits else 0.0

    @property
    def value_estimate(self):
        return self.value if self.visits else self.network_value


@dataclass(frozen=True)
class SearchResult:
    candidate: tuple | None
    candidate_keys: tuple
    visit_counts: tuple
    root_value: float
    stats: dict


def premise_certified_candidates(state):
    """The dense game's deterministic candidate list without its guard."""
    codec = state._euclid_game.action_codec
    allowed_rules = allowed_by_kind = free_variables = None
    if codec is not None:
        allowed_rules = set(codec.rules)
        allowed_by_kind = {
            kind: set(names[:euclid_spiel.DENSE_KIND_CAPS[kind]])
            for kind, names in codec._by_kind(state).items()
        }
        free_variables = {
            rule: set(codec.layouts[rule][1]) for rule in allowed_rules
        }
    return euclid_spiel.enumerate_actions(
        state._proposition, state.engine_state, state.initial_kinds,
        len(state.history_moves), state._parent_legal_keys,
        allowed_rules, allowed_by_kind, free_variables,
        state._canonical_names(), validate=False)


class CandidatePUCT:
    def __init__(self, model, *, simulations=32, uct_c=1.41,
                 root_win_in_one=True, dirichlet=None, temperature=0.0,
                 widening=None, first_play_reduction=0.0, seed=0,
                 jit_model=True):
        self.model = model
        self.simulations = simulations
        self.uct_c = uct_c
        self.root_win_in_one = root_win_in_one
        self.dirichlet = dirichlet
        self.temperature = temperature
        self.widening = widening
        self.first_play_reduction = float(first_play_reduction)
        self.random = np.random.RandomState(seed)
        self.admission_cache = {}
        self.last_stats = {}
        self._forward = (nnx.jit(lambda network, batch: network(batch))
                         if jit_model else lambda network, batch: network(batch))

    @staticmethod
    def _softmax(logits):
        logits = np.asarray(logits, dtype=np.float64)
        if not len(logits):
            return logits
        shifted = logits - np.max(logits)
        probabilities = np.exp(shifted)
        total = probabilities.sum()
        return (probabilities / total if total > 0
                else np.full(len(logits), 1.0 / len(logits)))

    def _time(self, phase, started):
        self._stats[f"{phase}_seconds"] += time.perf_counter() - started
        self._stats[f"{phase}_calls"] += 1

    def _expand(self, node, *, root=False):
        started = time.perf_counter()
        candidates = premise_certified_candidates(node.state)
        self._time("candidate_generation", started)

        started = time.perf_counter()
        encoded = feature_encoder.extract(node.state, candidates).arrays
        batch = candidate_model.batch_features([encoded])
        self._time("features", started)

        started = time.perf_counter()
        logits, values = self._forward(self.model, batch)
        logits, value = np.asarray(logits[0, :len(candidates)]), float(values[0])
        self._time("model", started)

        node.network_value = value
        node.candidates = list(candidates)
        node.priors = self._softmax(logits)
        node.expanded = True
        if not node.candidates:
            node.dead_end = True
            return 0.0, ()

        winning = ()
        if root and self.root_win_in_one:
            scan_started = time.perf_counter()
            accepted = []
            accepted_priors = []
            wins = []
            for candidate, prior in zip(node.candidates, node.priors):
                child = self._admit(node.state, candidate)
                if child is None:
                    continue
                accepted.append(candidate)
                accepted_priors.append(prior)
                if child.goal_reached():
                    wins.append(candidate)
            node.candidates = accepted
            node.priors = np.asarray(accepted_priors, dtype=np.float64)
            self._renormalize(node)
            self._stats["root_scan_seconds"] += time.perf_counter() - scan_started
            self._stats["root_scan_calls"] += 1
            winning = tuple(wins)
            if not accepted:
                node.dead_end = True

        if root and self.dirichlet and node.candidates:
            epsilon, alpha = self.dirichlet
            noise = self.random.dirichlet([alpha] * len(node.candidates))
            node.priors = (1 - epsilon) * node.priors + epsilon * noise
        return value, winning

    def _renormalize(self, node):
        if not len(node.priors):
            return
        total = float(node.priors.sum())
        node.priors = (node.priors / total if total > 0 else
                       np.full(len(node.priors), 1.0 / len(node.priors)))

    def _admit(self, state, candidate):
        key = (state.logical_state_key(), candidate)
        if key in self.admission_cache:
            self._stats["admission_cache_hits"] += 1
            stored = self.admission_cache[key]
            return None if stored is _REFUSED else self._child_state(
                state, candidate, stored)
        started = time.perf_counter()
        successor, refusal = euclid_spiel.admit_candidate(
            state.engine_state, len(state.history_moves), candidate)
        self._time("admission", started)
        if refusal is not None:
            self.admission_cache[key] = _REFUSED
            self._stats["admission_refusals"] += 1
            return None
        self.admission_cache[key] = successor.copy()
        return self._child_state(state, candidate, successor)

    @staticmethod
    def _child_state(parent, candidate, engine_state):
        child = parent.clone()
        child.engine_state = engine_state.copy()
        child.history_moves.append(candidate)
        child._actions = child._action_by_id = None
        child._parent_legal_keys = frozenset()
        return child

    @staticmethod
    def _remove(node, index):
        candidate = node.candidates.pop(index)
        node.priors = np.delete(node.priors, index)
        node.children.pop(candidate, None)

    def _eligible(self, node):
        if self.widening is None:
            return range(len(node.candidates))
        c, alpha = self.widening
        count = min(len(node.candidates),
                    max(1, math.ceil(c * max(node.visits, 1) ** alpha)))
        ranked = np.argsort(-node.priors, kind="stable")
        return ranked[:count]

    def _select(self, node):
        eligible = list(self._eligible(node))
        parent = max(node.visits, 1)
        best = None
        for index in eligible:
            candidate = node.candidates[index]
            child = node.children.get(candidate)
            visits = child.visits if child is not None else 0
            value = (child.value if child is not None and child.visits
                     else node.value_estimate - self.first_play_reduction)
            score = (value + self.uct_c * node.priors[index]
                     * math.sqrt(parent) / (1 + visits))
            key = (score, -index)
            if best is None or key > best[0]:
                best = key, index
        return None if best is None else best[1]

    def _terminal_value(self, state):
        if state.goal_reached():
            return 1.0
        if len(state.history_moves) >= state._move_cap:
            return -1.0
        return None

    @staticmethod
    def _new_stats():
        return {
            "candidate_generation_seconds": 0.0,
            "candidate_generation_calls": 0,
            "features_seconds": 0.0,
            "features_calls": 0,
            "model_seconds": 0.0,
            "model_calls": 0,
            "admission_seconds": 0.0,
            "admission_calls": 0,
            "admission_cache_hits": 0,
            "admission_refusals": 0,
            "root_scan_seconds": 0.0,
            "root_scan_calls": 0,
        }

    def _simulation(self, root):
        node = root
        path = [root]
        while True:
            terminal = self._terminal_value(node.state)
            if terminal is not None:
                value = terminal
                break
            if not node.expanded:
                value, _winning = self._expand(node)
                if node.dead_end:
                    value = -1.0
                break
            if node.dead_end:
                value = -1.0
                break

            while True:
                index = self._select(node)
                if index is None:
                    node.dead_end = True
                    value = -1.0
                    break
                candidate = node.candidates[index]
                child = node.children.get(candidate)
                if child is None:
                    child_state = self._admit(node.state, candidate)
                    if child_state is None:
                        self._remove(node, index)
                        self._renormalize(node)
                        if not node.candidates:
                            node.dead_end = True
                            value = -1.0
                            break
                        continue
                    child = Node(child_state, prior=float(node.priors[index]))
                    node.children[candidate] = child
                node = child
                path.append(node)
                break
            if node is not path[-1] or node.dead_end:
                break

        for visited in reversed(path):
            visited.visits += 1
            visited.value_sum += value

    def search(self, state):
        total_started = time.perf_counter()
        self.admission_cache.clear()
        self._stats = self._new_stats()
        root = Node(state.clone())
        terminal = self._terminal_value(root.state)
        if terminal is not None:
            root.dead_end = True
            return self._result(root, None, terminal, total_started)
        root_value, winning = self._expand(root, root=True)
        if root.dead_end:
            return self._result(root, None, -1.0, total_started)
        if winning:
            chosen = winning[0]
            counts = {candidate: 0 for candidate in root.candidates}
            counts[chosen] = self.simulations
            root.visits = self.simulations
            root.value_sum = float(self.simulations)
            return self._result(root, chosen, 1.0, total_started, counts)

        for _ in range(self.simulations):
            self._simulation(root)
        counts = {candidate: root.children[candidate].visits
                  if candidate in root.children else 0
                  for candidate in root.candidates}
        chosen = self._choose(root.candidates, counts)
        return self._result(root, chosen, root.value or root_value,
                            total_started, counts)

    def _choose(self, candidates, counts):
        if not candidates:
            return None
        visits = np.asarray([counts[candidate] for candidate in candidates],
                            dtype=np.float64)
        if self.temperature <= 0:
            return candidates[int(np.argmax(visits))]
        weights = np.power(visits, 1.0 / self.temperature)
        if weights.sum() <= 0:
            weights = np.ones_like(weights)
        return candidates[int(self.random.choice(
            len(candidates), p=weights / weights.sum()))]

    def _result(self, root, chosen, value, total_started, counts=None):
        counts = counts or {candidate: 0 for candidate in root.candidates}
        keys = tuple(root.candidates)
        visits = tuple(int(counts[candidate]) for candidate in keys)
        self._stats["total_seconds"] = time.perf_counter() - total_started
        accounted = sum(self._stats[key] for key in (
            "candidate_generation_seconds", "features_seconds",
            "model_seconds", "admission_seconds"))
        self._stats["other_seconds"] = max(
            0.0, self._stats["total_seconds"] - accounted)
        self.last_stats = dict(self._stats)
        return SearchResult(chosen, keys, visits, float(value),
                            dict(self.last_stats))
