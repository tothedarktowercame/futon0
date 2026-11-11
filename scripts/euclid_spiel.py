#!/usr/bin/env python3
"""OpenSpiel wrapper for the rule-only Euclid Book I construction game.

Actions are enumerated from the proposition palette and the current engine
state.  No proof trace, recorded play, or move preference is read.  Case
splits are deliberately absent from this first wrapper.
"""

from __future__ import annotations

import itertools
import json
import os
import sys

import pyspiel
from open_spiel.python.observation import IIGObserverForPublicInfoGame

sys.path.insert(0, os.path.dirname(__file__))
import euclid_engine as eng  # noqa: E402
import euclid_play as play  # noqa: E402


_GAME_TYPE = pyspiel.GameType(
    short_name="euclid_book1",
    long_name="Euclid Book I (System E rules)",
    dynamics=pyspiel.GameType.Dynamics.SEQUENTIAL,
    chance_mode=pyspiel.GameType.ChanceMode.DETERMINISTIC,
    information=pyspiel.GameType.Information.PERFECT_INFORMATION,
    utility=pyspiel.GameType.Utility.IDENTICAL,
    reward_model=pyspiel.GameType.RewardModel.TERMINAL,
    max_num_players=1,
    min_num_players=1,
    provides_information_state_string=True,
    provides_information_state_tensor=False,
    provides_observation_string=True,
    provides_observation_tensor=False,
    parameter_specification={"proposition": 1, "move_cap": 12},
)


def _game_info(move_cap):
    return pyspiel.GameInfo(
        num_distinct_actions=1_000_000,
        max_chance_outcomes=0,
        num_players=1,
        min_utility=0.0,
        max_utility=1.0,
        utility_sum=None,
        max_game_length=move_cap,
    )


def _objects(state, initial):
    kinds = dict(initial)
    for fact in state.facts:
        if not fact:
            continue
        k = fact[0]
        if k in ("on_line", "not_on_line") and len(fact) >= 3:
            kinds[fact[1]] = "point"; kinds[fact[2]] = "line"
        elif k == "dpl" and len(fact) >= 4:
            kinds[fact[1]] = kinds[fact[2]] = "point"; kinds[fact[3]] = "line"
        elif k in ("on_circle", "centre", "inside", "outside", "not_outside") and len(fact) >= 3:
            kinds[fact[1]] = "point"; kinds[fact[2]] = "circle"
        elif k == "intersects_cc":
            kinds[fact[1]] = kinds[fact[2]] = "circle"
        elif k == "intersects_lc":
            kinds[fact[1]] = "line"; kinds[fact[2]] = "circle"
        elif k in ("neq_line", "peq_line", "intersects_ll"):
            kinds[fact[1]] = kinds[fact[2]] = "line"
    for name in state.declared:
        kinds.setdefault(name, "point")
    return kinds


def _unify(pattern, fact, variables, env):
    if not isinstance(pattern, (tuple, list)) or not isinstance(fact, (tuple, list)) or len(pattern) != len(fact):
        return None
    out = dict(env)
    for p, value in zip(pattern, fact):
        if isinstance(p, (tuple, list)):
            out = _unify(p, value, variables, out)
            if out is None:
                return None
        elif p in variables:
            if p in out and out[p] != value:
                return None
            out[p] = value
        elif p != value:
            return None
    return out


def _premise_environments(spec, state):
    variables = tuple(spec["args"])
    try:
        premises = spec["premises"](eng.State(), *variables)
    except Exception:
        premises = []
    envs = [{}]
    facts = sorted(state.facts, key=repr)
    for pattern in premises:
        if not isinstance(pattern, (tuple, list)) or not pattern or pattern[0] in ("either", "entail"):
            continue
        matches = []
        for env in envs:
            for fact in facts:
                if fact and fact[0] == pattern[0]:
                    found = _unify(pattern, fact, set(variables), env)
                    if found is not None:
                        matches.append(found)
        envs = matches
        if not envs:
            break
    return envs


def _fresh_bindings(spec, args, state, ply):
    try:
        made = spec["make"](state, *args)
    except Exception:
        return {}
    used = set(state.declared)
    bindings = {}
    for base, _facts in made:
        if base is None or base in bindings:
            continue
        stem = f"{base}_{ply + 1}"
        name, suffix = stem, 2
        while name in used:
            name, suffix = f"{stem}_{suffix}", suffix + 1
        bindings[base] = name
        used.add(name)
    return bindings


def enumerate_actions(n, state, initial_kinds, ply):
    """All accepted non-case moves, sorted by rule, args, and fresh names."""
    kinds = _objects(state, initial_kinds)
    by_kind = {kind: sorted(name for name, k in kinds.items() if k == kind)
               for kind in ("point", "line", "circle")}
    actions = []
    for rule in sorted(play.palette(n)):
        spec = eng.CONSTRUCTIONS[rule]
        info = play.rule_info(rule)
        variables = tuple(spec["args"])
        for env in _premise_environments(spec, state):
            missing = [v for v in variables if v not in env]
            domains = [by_kind.get(info["arg_kinds"].get(v, "point"), []) for v in missing]
            for values in itertools.product(*domains):
                full = dict(env, **dict(zip(missing, values)))
                args = tuple(full[v] for v in variables)
                bindings = _fresh_bindings(spec, args, state, ply)
                if "entail" in spec:
                    trial = state.copy()
                    try:
                        eng.apply_step(trial, ply + 1, rule, args, bindings)
                    except (eng.MissingPremise, TypeError, ValueError):
                        continue
                actions.append((rule, args, tuple(sorted(bindings.items()))))
    return sorted(set(actions), key=lambda action: (action[0], action[1], action[2]))


class EuclidGame(pyspiel.Game):
    def __init__(self, params=None):
        params = params or {}
        self.proposition = int(params.get("proposition", 1))
        self.move_cap = int(params.get("move_cap", 12))
        super().__init__(_GAME_TYPE, _game_info(self.move_cap), params)

    def new_initial_state(self):
        return EuclidState(self)

    def make_py_observer(self, iig_obs_type=None, params=None):
        return IIGObserverForPublicInfoGame(
            iig_obs_type or pyspiel.IIGObservationType(perfect_recall=False), params
        )


class EuclidState(pyspiel.State):
    def __init__(self, game, engine_state=None, history=None):
        super().__init__(game)
        self._euclid_game = game
        self._proposition = game.proposition
        self._move_cap = game.move_cap
        self.engine_state = engine_state or play._start_state(self._proposition)
        self.history_moves = list(history or [])
        self.initial_kinds = {o["name"]: o["kind"] for o in play.tr.objects_from_hyps(self._proposition)}
        self._actions = None

    def clone(self):
        copy = EuclidState(self._euclid_game, self.engine_state.copy(), self.history_moves)
        copy._actions = self._actions
        return copy

    def _object_list(self):
        return [{"name": name, "kind": kind} for name, kind in _objects(self.engine_state, self.initial_kinds).items()]

    def goal_reached(self):
        return play._goal_reached(self._proposition, self.engine_state, self._object_list(), None)

    def is_terminal(self):
        return self.goal_reached() or len(self.history_moves) >= self._move_cap

    def current_player(self):
        return pyspiel.PlayerId.TERMINAL if self.is_terminal() else 0

    def _legal_actions(self, player):
        assert player == 0
        if self._actions is None:
            self._actions = enumerate_actions(self._proposition, self.engine_state,
                                              self.initial_kinds, len(self.history_moves))
        return list(range(len(self._actions)))

    def _apply_action(self, action):
        if self._actions is None:
            self._legal_actions(0)
        rule, args, bindings_tuple = self._actions[action]
        eng.apply_step(self.engine_state, len(self.history_moves) + 1, rule, args, dict(bindings_tuple))
        self.history_moves.append((rule, args, bindings_tuple))
        self._actions = None

    def _action_to_string(self, player, action):
        if self._actions is None:
            self._legal_actions(0)
        rule, args, bindings = self._actions[action]
        made = ",".join(f"{k}={v}" for k, v in bindings)
        return f"{rule}({','.join(args)})" + (f" -> {made}" if made else "")

    def returns(self):
        return [1.0 if self.is_terminal() and self.goal_reached() else 0.0]

    def information_state_string(self, player=None):
        return str(self)

    def observation_string(self, player=None):
        return str(self)

    def __str__(self):
        return json.dumps(self.history_moves, sort_keys=True)


pyspiel.register_game(_GAME_TYPE, EuclidGame)
