#!/usr/bin/env python3
"""OpenSpiel wrapper for the rule-only Euclid Book I construction game.

Actions are enumerated from the proposition palette and the current engine
state.  No proof trace, recorded play, or move preference is read.  Case
splits are deliberately absent from this first wrapper.
"""

from __future__ import annotations

import hashlib
import itertools
import json
import os
import random
import sys

import pyspiel
import numpy as np
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
    provides_observation_tensor=True,
    parameter_specification={
        "proposition": 1, "move_cap": 12, "dense_actions": False,
        "alpha_zero_facade": False, "start_file": "",
        "start_perturb": 0, "start_perturb_rate": 1.0, "start_seed": 0,
    },
)

_ALPHA_ZERO_GAME_TYPE = pyspiel.GameType(
    short_name="euclid_book1",
    long_name="Euclid Book I (System E rules; AlphaZero facade)",
    dynamics=pyspiel.GameType.Dynamics.SEQUENTIAL,
    chance_mode=pyspiel.GameType.ChanceMode.DETERMINISTIC,
    information=pyspiel.GameType.Information.PERFECT_INFORMATION,
    utility=pyspiel.GameType.Utility.ZERO_SUM,
    reward_model=pyspiel.GameType.RewardModel.TERMINAL,
    max_num_players=2,
    min_num_players=2,
    provides_information_state_string=True,
    provides_information_state_tensor=False,
    provides_observation_string=True,
    provides_observation_tensor=True,
    parameter_specification=_GAME_TYPE.parameter_specification,
)


def _game_info(move_cap, num_actions=2_000_000_000, alpha_zero_facade=False):
    return pyspiel.GameInfo(
        num_distinct_actions=num_actions,
        max_chance_outcomes=0,
        num_players=2 if alpha_zero_facade else 1,
        min_utility=-1.0 if alpha_zero_facade else 0.0,
        max_utility=1.0,
        utility_sum=None,
        max_game_length=move_cap,
    )


MAX_OBJECTS = 32
OBSERVATION_SIZE = 4096
DENSE_KIND_CAPS = {"point": 8, "line": 4, "circle": 4}
DENSE_MAX_ARITY = 6


def _determined_line_args(spec, arg_kinds):
    """Map line arguments to the two point arguments that determine them.

    This is read from the construction's own ``dpl``/``on_line`` premises;
    names such as AB carry no meaning here.
    """
    variables = tuple(spec["args"])
    try:
        premises = spec["premises"](eng.State(), *variables)
    except Exception:
        return {}
    points_on = {v: [] for v in variables if arg_kinds.get(v) == "line"}
    for premise in premises:
        if not isinstance(premise, (tuple, list)) or not premise:
            continue
        if premise[0] == "dpl" and len(premise) == 4 and premise[3] in points_on:
            points_on[premise[3]].extend((premise[1], premise[2]))
        elif premise[0] == "on_line" and len(premise) == 3 and premise[2] in points_on:
            points_on[premise[2]].append(premise[1])
    result = {}
    point_variables = {v for v in variables if arg_kinds.get(v, "point") == "point"}
    for line, points in points_on.items():
        distinct = []
        for point in points:
            if point in point_variables and point not in distinct:
                distinct.append(point)
        if len(distinct) >= 2:
            result[line] = tuple(distinct[:2])
    return result


class DenseActionCodec:
    """Mixed-radix ids over neutral typed creation-order object slots.

    A line fixed by two point arguments in the rule's own premises is recovered
    from the state rather than occupying an action slot.  The arity cap applies
    to the remaining free arguments.  The ordinary game is unchanged.
    """

    def __init__(self, proposition):
        self.rules = []
        self.offsets = {}
        self.layouts = {}
        offset = 0
        for rule in sorted(play.palette(proposition)):
            spec = eng.CONSTRUCTIONS[rule]
            info = play.rule_info(rule)
            arg_kinds = info["arg_kinds"]
            determined = _determined_line_args(spec, arg_kinds)
            free = tuple(arg for arg in spec["args"] if arg not in determined)
            if len(free) > DENSE_MAX_ARITY:
                continue
            kinds = tuple(arg_kinds.get(arg, "point") for arg in free)
            radices = tuple(DENSE_KIND_CAPS[kind] for kind in kinds)
            size = int(np.prod(radices, dtype=np.int64)) if radices else 1
            self.offsets[rule] = (offset, kinds, radices, size)
            self.layouts[rule] = (tuple(spec["args"]), free, determined)
            self.rules.append(rule)
            offset += size
        self.size = offset

    @staticmethod
    def _by_kind(state):
        kinds = _objects(state.engine_state, state.initial_kinds)
        result = {kind: [] for kind in DENSE_KIND_CAPS}
        for name in state._object_order():
            kind = kinds.get(name)
            if kind in result:
                result[kind].append(name)
        return result

    @staticmethod
    def _line_through(state, p, q):
        if p == q:
            return None
        facts = state.engine_state.facts
        kinds = _objects(state.engine_state, state.initial_kinds)
        for name in state._object_order():
            if (kinds.get(name) == "line"
                    and ("on_line", p, name) in facts
                    and ("on_line", q, name) in facts):
                return name
        return None

    def _full_args(self, state, rule, free_args):
        variables, free, determined = self.layouts[rule]
        env = dict(zip(free, free_args))
        for line, (p, q) in determined.items():
            found = self._line_through(state, env[p], env[q])
            if found is None:
                return None
            env[line] = found
        return tuple(env[v] for v in variables)

    def encode(self, state, action):
        rule, args, _bindings = action
        if rule not in self.offsets:
            return None
        offset, kinds, radices, _size = self.offsets[rule]
        variables, free, _determined = self.layouts[rule]
        env = dict(zip(variables, args))
        free_args = tuple(env[v] for v in free)
        # Equal-line aliases do not get duplicate compact actions: the points
        # name the canonical existing line in creation order.
        if self._full_args(state, rule, free_args) != tuple(args):
            return None
        by_kind = self._by_kind(state)
        rank = 0
        for arg, kind, radix in zip(free_args, kinds, radices):
            try:
                slot = by_kind[kind].index(arg)
            except ValueError:
                return None
            if slot >= radix:
                return None
            rank = rank * radix + slot
        return offset + rank

    def decode(self, state, action_id):
        for rule in self.rules:
            offset, kinds, radices, size = self.offsets[rule]
            if not offset <= action_id < offset + size:
                continue
            rank = action_id - offset
            slots = []
            for radix in reversed(radices):
                slots.append(rank % radix)
                rank //= radix
            slots.reverse()
            by_kind = self._by_kind(state)
            try:
                free_args = tuple(by_kind[kind][slot] for kind, slot in zip(kinds, slots))
            except IndexError:
                return None
            args = self._full_args(state, rule, free_args)
            if args is None:
                return None
            spec = eng.CONSTRUCTIONS[rule]
            bindings = _fresh_bindings(spec, args, state.engine_state, len(state.history_moves))
            return rule, args, tuple(sorted(bindings.items()))
        return None


def stable_action_id(action):
    """A state-independent id for a grounded `(rule, arguments)` move."""
    rule, args, _bindings = action
    payload = json.dumps([rule, list(args)], separators=(",", ":"), ensure_ascii=True).encode()
    return int.from_bytes(hashlib.blake2b(payload, digest_size=8).digest(), "big") % 2_000_000_000


def _stable_bucket(value, size):
    payload = repr(value).encode("utf-8", "backslashreplace")
    return int.from_bytes(hashlib.blake2b(payload, digest_size=8).digest(), "big") % size


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


def _premise_environments(spec, state, facts_by_head=None):
    variables = tuple(spec["args"])
    try:
        premises = spec["premises"](eng.State(), *variables)
    except Exception:
        premises = []
    envs = [{}]
    if facts_by_head is None:
        facts_by_head = {}
        for fact in sorted(state.facts, key=repr):
            if fact:
                facts_by_head.setdefault(fact[0], []).append(fact)
    variable_set = set(variables)
    for pattern in premises:
        if not isinstance(pattern, (tuple, list)) or not pattern or pattern[0] in ("either", "entail"):
            continue
        matches = []
        for env in envs:
            for fact in facts_by_head.get(pattern[0], ()):
                found = _unify(pattern, fact, variable_set, env)
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


def _candidate_actions(n, state, initial_kinds, ply, allowed_rules=None,
                       allowed_by_kind=None, free_variables=None):
    """Yield grounded candidates before arithmetic entailment checks."""
    kinds = _objects(state, initial_kinds)
    by_kind = {kind: sorted(name for name, k in kinds.items() if k == kind)
               for kind in ("point", "line", "circle")}
    facts_by_head = {}
    for fact in sorted(state.facts, key=repr):
        if fact:
            facts_by_head.setdefault(fact[0], []).append(fact)
    for rule in sorted(play.palette(n)):
        if allowed_rules is not None and rule not in allowed_rules:
            continue
        spec = eng.CONSTRUCTIONS[rule]
        info = play.rule_info(rule)
        variables = tuple(spec["args"])
        for env in _premise_environments(spec, state, facts_by_head):
            missing = [v for v in variables if v not in env]
            domains = []
            for v in missing:
                kind = info["arg_kinds"].get(v, "point")
                domain = by_kind.get(kind, [])
                if (allowed_by_kind is not None
                        and (free_variables is None or v in free_variables[rule])):
                    domain = [name for name in domain if name in allowed_by_kind[kind]]
                domains.append(domain)
            for values in itertools.product(*domains):
                full = dict(env, **dict(zip(missing, values)))
                args = tuple(full[v] for v in variables)
                if allowed_by_kind is not None and any(
                    (free_variables is None or variable in free_variables[rule]) and
                    arg not in allowed_by_kind[info["arg_kinds"].get(variable, "point")]
                    for variable, arg in zip(variables, args)
                ):
                    continue
                bindings = _fresh_bindings(spec, args, state, ply)
                yield rule, args, tuple(sorted(bindings.items()))


def _entailments_hold(spec, state, args, cache):
    """Check each distinct arithmetic query once in this monotone state."""
    for query in spec.get("entail", lambda _state, *_args: ())(state, *args):
        key = repr(query)
        if key not in cache:
            try:
                eng._entail_check(state, 0, "open_spiel_enumeration", query)
            except (eng.MissingPremise, TypeError, ValueError):
                cache[key] = False
            else:
                cache[key] = True
        if not cache[key]:
            return False
    return True


def enumerate_actions_reference(n, state, initial_kinds, ply):
    """Probe-1 oracle: validate each entailment candidate by applying it."""
    accepted = []
    for action in _candidate_actions(n, state, initial_kinds, ply):
        rule, args, bindings = action
        if "entail" in eng.CONSTRUCTIONS[rule]:
            trial = state.copy()
            try:
                eng.apply_step(trial, ply + 1, rule, args, dict(bindings))
            except (eng.MissingPremise, TypeError, ValueError):
                continue
        accepted.append(action)
    return sorted(set(accepted), key=lambda action: (action[0], action[1], action[2]))


def enumerate_actions(n, state, initial_kinds, ply, parent_legal_keys=(),
                      allowed_rules=None, allowed_by_kind=None, free_variables=None):
    """All accepted non-case moves, with repeated entailments and parent moves reused."""
    accepted = []
    query_cache = {}
    parent_legal_keys = set(parent_legal_keys)
    for action in _candidate_actions(n, state, initial_kinds, ply,
                                     allowed_rules, allowed_by_kind, free_variables):
        rule, args, _bindings = action
        if ((rule, args) not in parent_legal_keys
                and not _entailments_hold(eng.CONSTRUCTIONS[rule], state, args, query_cache)):
            continue
        accepted.append(action)
    return sorted(set(accepted), key=lambda action: (action[0], action[1], action[2]))


def sample_action(n, state, initial_kinds, ply, rng=None, max_attempts=10_000,
                  allowed_rules=None, allowed_by_kind=None, free_variables=None):
    """Sample without materialising the legal set.

    Each attempt chooses a rule uniformly, then a matching premise environment
    uniformly, then each still-free typed argument uniformly. Rejection and
    resampling conditions this neutral proposal distribution on legality; it
    is not uniform over legal grounded moves.
    """
    rng = rng or random.Random()
    kinds = _objects(state, initial_kinds)
    by_kind = {kind: sorted(name for name, k in kinds.items() if k == kind)
               for kind in ("point", "line", "circle")}
    facts_by_head = {}
    for fact in sorted(state.facts, key=repr):
        if fact:
            facts_by_head.setdefault(fact[0], []).append(fact)
    rules = sorted(rule for rule in play.palette(n)
                   if allowed_rules is None or rule in allowed_rules)
    env_cache = {}
    query_cache = {}
    for _ in range(max_attempts):
        rule = rng.choice(rules)
        spec = eng.CONSTRUCTIONS[rule]
        info = play.rule_info(rule)
        variables = tuple(spec["args"])
        if rule not in env_cache:
            envs = _premise_environments(spec, state, facts_by_head)
            # A fact may satisfy two syntactic joins. Do not weight that action
            # twice merely because it has two derivations.
            env_cache[rule] = [dict(items) for items in sorted(
                {tuple(sorted(env.items())) for env in envs}
            )]
        envs = env_cache[rule]
        if not envs:
            continue
        env = rng.choice(envs)
        full = dict(env)
        refused = False
        for variable in variables:
            if variable in full:
                continue
            kind = info["arg_kinds"].get(variable, "point")
            domain = by_kind.get(kind, ())
            if (allowed_by_kind is not None
                    and (free_variables is None or variable in free_variables[rule])):
                domain = tuple(name for name in domain if name in allowed_by_kind[kind])
            if not domain:
                refused = True
                break
            full[variable] = rng.choice(domain)
        if refused:
            continue
        args = tuple(full[v] for v in variables)
        if not _entailments_hold(spec, state, args, query_cache):
            continue
        bindings = _fresh_bindings(spec, args, state, ply)
        return rule, args, tuple(sorted(bindings.items()))
    raise RuntimeError(f"no legal action sampled in {max_attempts} neutral attempts")


class EuclidGame(pyspiel.Game):
    def __init__(self, params=None):
        params = params or {}
        self._configure(params)
        game_type = _ALPHA_ZERO_GAME_TYPE if self.alpha_zero_facade else _GAME_TYPE
        num_actions = self.action_codec.size if self.action_codec else 2_000_000_000
        super().__init__(game_type, _game_info(self.move_cap + self.start_perturb,
                                               num_actions, self.alpha_zero_facade), params)

    def _configure(self, params):
        self.proposition = int(params.get("proposition", 1))
        self.move_cap = int(params.get("move_cap", 12))
        self.dense_actions = bool(params.get("dense_actions", False))
        self.alpha_zero_facade = bool(params.get("alpha_zero_facade", False))
        self.start_file = str(params.get("start_file", ""))
        self.start_perturb = int(params.get("start_perturb", 0))
        self.start_perturb_rate = float(params.get("start_perturb_rate", 1.0))
        self.start_seed = int(params.get("start_seed", 0))
        self._start_episode = itertools.count()
        self.action_codec = DenseActionCodec(self.proposition) if self.dense_actions else None
        self.start_moves = []
        self.start_state = None
        if self.start_file:
            with open(self.start_file) as stream:
                encoded = json.load(stream)
            self.start_state = play._start_state(self.proposition)
            for ply, (rule, args, bindings) in enumerate(encoded, 1):
                grounded = (rule, tuple(args), tuple(sorted(dict(bindings).items())))
                eng.apply_step(self.start_state, ply, grounded[0], grounded[1],
                               dict(grounded[2]))
                self.start_moves.append(grounded)

    def _ensure_configured(self):
        # pyspiel pickles the C++ base and can omit Python instance attributes
        # when an AlphaZero actor is spawned.
        if not hasattr(self, "proposition"):
            self._configure(self.get_parameters())

    def new_initial_state(self):
        self._ensure_configured()
        engine_state = self.start_state.copy() if self.start_state is not None else None
        state = EuclidState(self, engine_state=engine_state,
                            initial_moves=self.start_moves)
        episode = next(self._start_episode)
        rng = random.Random(self.start_seed + episode)
        enabled = self.start_perturb and rng.random() < self.start_perturb_rate
        count = rng.randint(0, self.start_perturb) if enabled else 0
        perturb_moves = []
        for _ in range(count):
            if state.is_terminal():
                break
            action = state.sample_action_tuple(rng)
            state.apply_action_tuple(action)
            perturb_moves.append(action)
        state.initial_moves = list(self.start_moves) + perturb_moves
        state.history_moves = []
        state._move_cap = self.move_cap + len(perturb_moves)
        state._actions = state._action_by_id = None
        state._parent_legal_keys = frozenset()
        return state

    def make_py_observer(self, iig_obs_type=None, params=None):
        return IIGObserverForPublicInfoGame(
            iig_obs_type or pyspiel.IIGObservationType(perfect_recall=False), params
        )

    def observation_tensor_shape(self):
        return [OBSERVATION_SIZE]


class EuclidState(pyspiel.State):
    def __init__(self, game, engine_state=None, history=None, parent_legal_keys=(),
                 initial_moves=()):
        super().__init__(game)
        self._euclid_game = game
        self._proposition = game.proposition
        self._move_cap = game.move_cap
        self.engine_state = engine_state or play._start_state(self._proposition)
        self.history_moves = list(history or [])
        self.initial_moves = list(initial_moves)
        self.initial_kinds = {o["name"]: o["kind"] for o in play.tr.objects_from_hyps(self._proposition)}
        self._actions = None
        self._action_by_id = None
        self._parent_legal_keys = frozenset(parent_legal_keys)

    def clone(self):
        copy = EuclidState(
            self._euclid_game, self.engine_state.copy(), self.history_moves,
            self._parent_legal_keys, self.initial_moves
        )
        copy._actions = self._actions
        copy._action_by_id = self._action_by_id
        return copy

    def _object_order(self):
        ordered = []
        for obj in play.tr.objects_from_hyps(self._proposition):
            if obj["name"] not in ordered:
                ordered.append(obj["name"])
        for _rule, _args, bindings in self.initial_moves + self.history_moves:
            for _base, name in bindings:
                if name not in ordered:
                    ordered.append(name)
        for name in sorted(self.engine_state.declared):
            if name not in ordered:
                ordered.append(name)
        if len(ordered) > MAX_OBJECTS:
            raise ValueError(f"observation object cap {MAX_OBJECTS} exceeded")
        return ordered

    def observation_tensor(self, player=None):
        """Neutral fixed-size encoding of object kinds, facts, and the goal."""
        del player
        tensor = np.zeros(OBSERVATION_SIZE, dtype=np.float32)
        order = self._object_order()
        index = {name: i for i, name in enumerate(order)}
        kinds = _objects(self.engine_state, self.initial_kinds)
        kind_index = {"point": 0, "line": 1, "circle": 2}
        for i, name in enumerate(order):
            if kinds.get(name) in kind_index:
                tensor[i * 3 + kind_index[kinds[name]]] = 1

        def canonical(value):
            if isinstance(value, (tuple, list)):
                return tuple(canonical(x) for x in value)
            return ("object", index[value]) if value in index else value

        fact_offset, fact_size = MAX_OBJECTS * 3, 2048
        for fact in self.engine_state.facts:
            slot = fact_offset + _stable_bucket(canonical(fact), fact_size)
            tensor[slot] = min(1.0, tensor[slot] + 0.25)
        goal_offset, goal_size = fact_offset + fact_size, 1024
        for goal in play.GOALS[self._proposition]:
            tensor[goal_offset + _stable_bucket(canonical(goal), goal_size)] = 1
        tensor[goal_offset + goal_size + min(len(self.history_moves), self._move_cap)] = 1
        return tensor

    def _object_list(self):
        return [{"name": name, "kind": kind} for name, kind in _objects(self.engine_state, self.initial_kinds).items()]

    def goal_reached(self):
        if eng.contradictory_outer_state(self.engine_state)[0]:
            return False
        given = {o["name"] for o in play.tr.objects_from_hyps(self._proposition)}
        # same rule as the student checklist (euclid_play.py:1227): an
        # existential witness may not be one of the given points
        return play._goal_witness(self._proposition, self.engine_state, self._object_list(),
                                  None, given=given, wit_not_in=given) is not None

    def logical_state_key(self):
        """Exact search key for the logical store and name discipline."""
        state = self.engine_state
        return (
            self._proposition,
            tuple(sorted(state.facts)),
            tuple(sorted(state.la.rows)),
            state.fresh,
            tuple(sorted(state.declared)),
            tuple(sorted(state.reserved)),
            tuple(sorted(state.exclude_rules)),
            state.proof_branch,
            len(self.history_moves),
        )

    def is_terminal(self):
        return self.goal_reached() or len(self.history_moves) >= self._move_cap

    def current_player(self):
        return pyspiel.PlayerId.TERMINAL if self.is_terminal() else 0

    def _legal_actions(self, player):
        assert player == 0
        if self._actions is None:
            allowed_rules = allowed_by_kind = None
            if self._euclid_game.action_codec:
                allowed_rules = set(self._euclid_game.action_codec.rules)
                allowed_by_kind = {
                    kind: set(names[:DENSE_KIND_CAPS[kind]])
                    for kind, names in self._euclid_game.action_codec._by_kind(self).items()
                }
            self._actions = enumerate_actions(self._proposition, self.engine_state,
                                              self.initial_kinds, len(self.history_moves),
                                              self._parent_legal_keys,
                                              allowed_rules, allowed_by_kind,
                                              ({rule: set(self._euclid_game.action_codec.layouts[rule][1])
                                                for rule in allowed_rules}
                                               if allowed_rules is not None else None))
            self._action_by_id = {}
            for grounded in self._actions:
                action_id = (self._euclid_game.action_codec.encode(self, grounded)
                             if self._euclid_game.action_codec else stable_action_id(grounded))
                if action_id is None:
                    continue
                previous = self._action_by_id.setdefault(action_id, grounded)
                if previous != grounded:
                    raise RuntimeError(f"stable action-id collision: {previous!r} and {grounded!r}")
        return sorted(self._action_by_id)

    def apply_action_tuple(self, action):
        """Apply a grounded action returned by the neutral sampler."""
        rule, args, bindings_tuple = action
        eng.apply_step(self.engine_state, len(self.history_moves) + 1, rule, args, dict(bindings_tuple))
        self.history_moves.append((rule, args, bindings_tuple))
        self._actions = None
        self._action_by_id = None
        self._parent_legal_keys = frozenset()

    def apply_certified_action(self, action):
        """Apply an action returned by this state's latest enumeration."""
        if self._actions is None:
            self._legal_actions(0)
        rule, args, bindings_tuple = self._action_by_id[action]
        parent_keys = frozenset((r, a) for r, a, _b in self._actions)
        eng.apply_certified_step(
            self.engine_state, len(self.history_moves) + 1,
            rule, args, dict(bindings_tuple))
        self.history_moves.append((rule, args, bindings_tuple))
        self._actions = None
        self._action_by_id = None
        self._parent_legal_keys = parent_keys

    def sample_action_tuple(self, rng=None):
        allowed_rules = None
        allowed_by_kind = None
        if self._euclid_game.action_codec:
            allowed_rules = set(self._euclid_game.action_codec.rules)
            allowed_by_kind = {
                kind: set(names[:DENSE_KIND_CAPS[kind]])
                for kind, names in self._euclid_game.action_codec._by_kind(self).items()
            }
        free_variables = ({rule: set(self._euclid_game.action_codec.layouts[rule][1])
                           for rule in allowed_rules}
                          if allowed_rules is not None else None)
        for _ in range(100):
            action = sample_action(
                self._proposition, self.engine_state, self.initial_kinds,
                len(self.history_moves), rng, allowed_rules=allowed_rules,
                allowed_by_kind=allowed_by_kind, free_variables=free_variables)
            if (self._euclid_game.action_codec is None
                    or self._euclid_game.action_codec.encode(self, action) is not None):
                return action
        raise RuntimeError("no encodable action sampled in 100 neutral samples")

    def _apply_action(self, action):
        if self._actions is None:
            self._legal_actions(0)
        rule, args, bindings_tuple = self._action_by_id[action]
        parent_keys = frozenset((r, a) for r, a, _b in self._actions)
        eng.apply_step(self.engine_state, len(self.history_moves) + 1, rule, args, dict(bindings_tuple))
        self.history_moves.append((rule, args, bindings_tuple))
        self._actions = None
        self._action_by_id = None
        self._parent_legal_keys = parent_keys

    def _action_to_string(self, player, action):
        if self._actions is None:
            self._legal_actions(0)
        rule, args, bindings = self._action_by_id[action]
        shown_args = args
        codec = self._euclid_game.action_codec
        if codec is not None:
            variables, free, _determined = codec.layouts[rule]
            env = dict(zip(variables, args))
            shown_args = tuple(env[v] for v in free)
        made = ",".join(f"{k}={v}" for k, v in bindings)
        return f"{rule}({','.join(shown_args)})" + (f" -> {made}" if made else "")

    def returns(self):
        won = 1.0 if self.is_terminal() and self.goal_reached() else 0.0
        return [won, -won] if self._euclid_game.alpha_zero_facade else [won]

    def information_state_string(self, player=None):
        return str(self)

    def observation_string(self, player=None):
        return str(self)

    def __str__(self):
        return json.dumps(self.history_moves, sort_keys=True)


class NeutralRolloutEvaluator:
    """MCTS evaluator whose rollouts use the rejection sampler, not legal_actions."""

    def __init__(self, n_rollouts=1, seed=0, max_length=None):
        self.n_rollouts = n_rollouts
        self.max_length = max_length
        self.rng = random.Random(seed)

    def evaluate(self, state):
        result = np.zeros(1)
        for _ in range(self.n_rollouts):
            working = state.clone()
            length = 0
            while not working.is_terminal():
                working.apply_action_tuple(working.sample_action_tuple(self.rng))
                length += 1
                if self.max_length is not None and length >= self.max_length:
                    break
            result += np.array(working.returns())
        return result / self.n_rollouts

    def prior(self, state):
        actions = state.legal_actions(state.current_player())
        return [(action, 1.0 / len(actions)) for action in actions]


pyspiel.register_game(_GAME_TYPE, EuclidGame)
