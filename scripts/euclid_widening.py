"""Policy-ordered progressive-widening search for the Euclid OpenSpiel game.

Only the root enumerates the full legal set.  Descendants ask the game about
individual action ids in network-prior order and admit ``ceil(c*N**alpha)``
legal children after N visits.
"""

from __future__ import annotations

from dataclasses import dataclass, field
import math
import time

import numpy as np
import pyspiel
from open_spiel.python.algorithms import mcts

import euclid_spiel


def certify_action(state, action_id):
    """Return the grounded action iff this one compact id is legal.

    Decoding enforces the compact object's slots and point-determined lines;
    applying to an isolated engine copy checks premises, arithmetic
    entailments, closure, and the outer-state consistency guard.
    """
    codec = state._euclid_game.action_codec
    grounded = codec.decode(state, int(action_id)) if codec is not None else None
    if grounded is None:
        return None
    rule, args, bindings = grounded
    trial = state.engine_state.copy()
    try:
        euclid_spiel.eng.apply_step(
            trial, len(state.history_moves) + 1, rule, args, dict(bindings))
    except (euclid_spiel.eng.MissingPremise, TypeError, ValueError):
        return None
    return grounded


@dataclass
class _NodeData:
    ranked: np.ndarray | None = None
    probabilities: np.ndarray | None = None
    cursor: int = 0
    illegal: int = 0
    exhausted: bool = False
    actions: set[int] = field(default_factory=set)


class WideningEvaluator(mcts.Evaluator):
    """Add unmasked whole-action inference to an AlphaZero evaluator."""

    def __init__(self, wrapped, action_count):
        self.wrapped = wrapped
        self.action_count = action_count
        self._raw_cache = {}
        source = wrapped
        while not hasattr(source, "_model") and hasattr(source, "wrapped"):
            source = source.wrapped
        self._model = getattr(source, "_model", None)

    def all_action_inference(self, state):
        obs = np.asarray(state.observation_tensor())
        key = obs.tobytes()
        cached = self._raw_cache.get(key)
        if cached is None:
            winning = getattr(self.wrapped, "_winning_actions", None)
            wins = winning(state) if winning is not None else ()
            if wins:
                policy = np.zeros(self.action_count, dtype=float)
                policy[list(wins)] = 1.0 / len(wins)
                cached = (np.array([1.0, -1.0]), policy)
                self._raw_cache[key] = cached
                return cached
            model = self._model
            if model is None:
                raise TypeError("widening requires an AlphaZero evaluator")
            mask = np.ones(self.action_count, dtype=np.float32)
            value, policy = model.inference(obs, mask)
            cached = (np.array([value, -value]), np.asarray(policy))
            self._raw_cache[key] = cached
        return cached

    def evaluate(self, state):
        return self.wrapped.evaluate(state)

    def prior(self, state):
        return self.wrapped.prior(state)

    def cache_info(self):
        return self.wrapped.cache_info()

    def clear_cache(self):
        self._raw_cache.clear()
        return self.wrapped.clear_cache()


class ProgressiveWideningBot(pyspiel.Bot):
    """PUCT with a lazily checked, policy-ranked action space."""

    def __init__(self, game, uct_c, max_simulations, evaluator, *,
                 c=1.0, alpha=0.5, dirichlet_noise=None,
                 random_state=None, force_all=False):
        super().__init__()
        self.game = game
        self.uct_c = uct_c
        self.max_simulations = max_simulations
        self.evaluator = evaluator
        self.c = c
        self.alpha = alpha
        self.dirichlet_noise = dirichlet_noise
        self.random = random_state or np.random.RandomState()
        self.force_all = force_all
        self.last_stats = {}
        self._certified = {}
        self._timing = None

    def restart_at(self, state):
        del state

    def _raw_inference(self, state):
        """Infer over every action id, without asking for a legal mask."""
        started = time.perf_counter()
        result = self.evaluator.all_action_inference(state)
        if self._timing is not None:
            self._timing["network_seconds"] += time.perf_counter() - started
            self._timing["network_calls"] += 1
        return result

    def _rank(self, state, data):
        if data.ranked is None:
            _value, policy = self._raw_inference(state)
            # Stable ordering makes equal/flat priors reproducible.
            data.ranked = np.argsort(-policy, kind="stable")
            data.probabilities = policy

    def _single_legal(self, state, action):
        started = time.perf_counter()
        key = (state.logical_state_key(), int(action))
        grounded = self._certified.get(key)
        if grounded is None:
            grounded = certify_action(state, action)
            if grounded is not None:
                self._certified[key] = grounded
        if self._timing is not None:
            self._timing["legality_seconds"] += time.perf_counter() - started
            self._timing["legality_calls"] += 1
        return grounded is not None

    def _apply(self, state, action):
        started = time.perf_counter()
        key = (state.logical_state_key(), int(action))
        grounded = self._certified.get(key)
        if grounded is None:
            # Root actions came from full enumeration rather than the lazy
            # checker.  Use its certified fast path and cache no duplicate.
            state.apply_certified_action(int(action))
        else:
            state.apply_action_tuple(grounded)
        if self._timing is not None:
            self._timing["application_seconds"] += time.perf_counter() - started

    def _renormalize(self, node, data):
        total = sum(float(data.probabilities[c.action]) for c in node.children)
        if total <= 0:
            probability = 1.0 / len(node.children)
            for child in node.children:
                child.prior = probability
        else:
            for child in node.children:
                child.prior = float(data.probabilities[child.action]) / total

    def _admit(self, node, state, data, target):
        self._rank(state, data)
        if self.force_all:
            target = self.game.num_distinct_actions()
        while len(node.children) < target and data.cursor < len(data.ranked):
            action = int(data.ranked[data.cursor])
            data.cursor += 1
            if not self._single_legal(state, action):
                data.illegal += 1
                continue
            data.actions.add(action)
            node.children.append(mcts.SearchNode(
                action, state.current_player(),
                float(data.probabilities[action])))
        data.exhausted = data.cursor == len(data.ranked)
        if node.children:
            self._renormalize(node, data)

    def _expand_root(self, root, state, data):
        _value, policy = self._raw_inference(state)
        started = time.perf_counter()
        actions = state.legal_actions()
        if self._timing is not None:
            self._timing["root_enumeration_seconds"] += time.perf_counter() - started
        priors = np.asarray([policy[action] for action in actions], dtype=float)
        total = priors.sum()
        priors[:] = priors / total if total > 0 else 1.0 / len(actions)
        if self.dirichlet_noise and actions:
            epsilon, alpha = self.dirichlet_noise
            noise = self.random.dirichlet([alpha] * len(actions))
            priors = (1 - epsilon) * priors + epsilon * noise
        root.children = [mcts.SearchNode(action, state.current_player(), prior)
                         for action, prior in zip(actions, priors)]
        data.actions.update(actions)
        key = state.logical_state_key()
        for action in actions:
            self._certified[(key, int(action))] = state._action_by_id[action]

    def mcts_search(self, state):
        search_started = time.perf_counter()
        self._certified.clear()
        self._timing = {
            "root_enumeration_seconds": 0.0,
            "legality_seconds": 0.0,
            "legality_calls": 0,
            "network_seconds": 0.0,
            "network_calls": 0,
            "clone_seconds": 0.0,
            "application_seconds": 0.0,
        }
        root = mcts.SearchNode(None, state.current_player(), 1.0)
        metadata = {id(root): _NodeData()}
        illegal_checks = legal_checks = 0
        for _ in range(self.max_simulations):
            started = time.perf_counter()
            working = state.clone()
            self._timing["clone_seconds"] += time.perf_counter() - started
            path = [root]
            node = root
            while not working.is_terminal() and node.explore_count > 0:
                data = metadata.setdefault(id(node), _NodeData())
                before_illegal = data.illegal
                before_children = len(node.children)
                if node is root and not node.children:
                    self._expand_root(node, working, data)
                elif node is not root:
                    target = max(1, math.ceil(self.c * node.explore_count ** self.alpha))
                    self._admit(node, working, data, target)
                    illegal_checks += data.illegal - before_illegal
                    legal_checks += len(node.children) - before_children
                if not node.children:
                    # Ranked list exhausted without a legal continuation.
                    break
                chosen = max(node.children,
                             key=lambda child: child.puct_value(
                                 node.explore_count, self.uct_c))
                self._apply(working, chosen.action)
                node = chosen
                metadata.setdefault(id(node), _NodeData())
                path.append(node)

            if working.is_terminal():
                returns = working.returns()
                node.outcome = returns
            elif node.explore_count > 0 and not node.children:
                returns = np.array([0.0, 0.0])
                node.outcome = returns
            else:
                returns, _policy = self._raw_inference(working)
            for visited in reversed(path):
                target = returns[visited.player]
                visited.total_reward += target
                visited.explore_count += 1

        total_seconds = time.perf_counter() - search_started
        self.last_stats = dict(self._timing, **{
            "single_legal": legal_checks,
            "single_illegal": illegal_checks,
            "illegal_share": (illegal_checks / (legal_checks + illegal_checks)
                              if legal_checks + illegal_checks else 0.0),
            "total_seconds": total_seconds,
        })
        accounted = sum(self.last_stats[key] for key in (
            "root_enumeration_seconds", "legality_seconds", "network_seconds",
            "clone_seconds", "application_seconds"))
        self.last_stats["other_seconds"] = max(0.0, total_seconds - accounted)
        self._timing = None
        return root

    def step(self, state):
        return self.mcts_search(state).best_child().action

    def step_with_policy(self, state):
        root = self.mcts_search(state)
        chosen = root.best_child().action
        total = sum(child.explore_count for child in root.children)
        return ([(child.action, child.explore_count / total)
                 for child in root.children], chosen)
