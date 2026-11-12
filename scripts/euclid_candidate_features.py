#!/usr/bin/env python3
"""Deterministic, domain-neutral tensors for grounded Euclid candidates."""

from __future__ import annotations

from dataclasses import dataclass
from fractions import Fraction
from collections import OrderedDict
import hashlib
import json
from pathlib import Path

import numpy as np

import euclid_engine as eng
import euclid_play as play
import euclid_spiel as spiel


SCHEMA_VERSION = 1
VOCABULARY_EPOCH = 2
CREATION_ORDER_CLIP = 31
CONSTANT_BUCKETS = 64
KIND_IDS = {"unknown": 0, "point": 1, "line": 2, "circle": 3}
HINDSIGHT_GOAL_ARITIES = {
    "_eq_ang": 6, "_eq_len": 4, "_gt_ang": 6, "_gt_len": 4,
    "_lt_len": 4, "gte": 2, "inside": 2, "intersects_cc": 2,
    "intersects_lc": 2, "neq": 2, "neq_line": 2,
    "not_on_line": 2, "not_outside": 2, "on_circle": 2,
    "outside": 2, "pos_len": 2, "radii": 3, "same_side": 3,
}


def _symbolic_rule_syntax(rule):
    spec = eng.CONSTRUCTIONS[rule]
    args = tuple(spec["args"])
    state = eng.State()
    premises = []
    conclusions = []
    for key in ("premises", "entail"):
        try:
            premises.extend(tuple(fact) for fact in spec.get(key, lambda *_: [])(
                state, *args))
        except Exception:
            pass
    try:
        for _base, facts in spec["make"](state, *args):
            conclusions.extend(tuple(fact) for fact in facts)
    except Exception:
        pass
    return args, premises, conclusions


def _head(fact):
    return str(fact[0]) if fact else "<empty>"


def _fact_arguments(fact):
    if len(fact) == 2 and isinstance(fact[1], (tuple, list)):
        return tuple(fact[1])
    return tuple(fact[1:])


def _make_vocabulary():
    rules = sorted(eng.CONSTRUCTIONS)
    predicates = {"<empty>"}
    roles = {"<none>"}
    syntax = {}
    role_kind_votes = {}
    for rule in rules:
        args, premises, conclusions = _symbolic_rule_syntax(rule)
        arg_kinds = play.rule_info(rule)["arg_kinds"]
        predicates.update(_head(fact) for fact in premises + conclusions)
        roles.update(f"rule:{rule}:arg:{arg}" for arg in args)
        for base in play.rule_info(rule)["binds"]:
            roles.add(f"rule:{rule}:result:{base}")
        syntax[rule] = (args, premises, conclusions)
        for fact in premises + conclusions:
            for position, argument in enumerate(_fact_arguments(fact)):
                if argument in arg_kinds:
                    votes = role_kind_votes.setdefault(
                        (_head(fact), position), {})
                    kind = arg_kinds[argument]
                    votes[kind] = votes.get(kind, 0) + 1
    for goals in play.GOALS.values():
        predicates.update(_head(goal) for goal in goals)
        for goal in goals:
            roles.update(f"goal:{_head(goal)}:{i}"
                         for i in range(max(0, len(goal) - 1)))
    max_arity = max((len(fact) - 1
                     for _rule, (_args, premises, conclusions) in syntax.items()
                     for fact in premises + conclusions), default=0)
    roles.update(f"fact:{i}" for i in range(max_arity))

    def ids(values, reserved):
        return {name: i for i, name in enumerate(reserved + sorted(values))}

    rule_ids = ids(rules, ["<pad>", "<none>"])
    predicate_ids = ids(predicates, ["<pad>", "<unk>"])
    base_predicates = frozenset(predicate_ids)
    role_ids = ids(roles, ["<pad>", "<unk>"])
    # Preserve every pre-E2 id so default feature tensors remain unchanged;
    # append the synthetic-goal vocabulary at the end.
    for predicate, arity in sorted(HINDSIGHT_GOAL_ARITIES.items()):
        if predicate not in predicate_ids:
            predicate_ids[predicate] = len(predicate_ids)
        for position in range(arity):
            role = f"goal:{predicate}:{position}"
            if role not in role_ids:
                role_ids[role] = len(role_ids)
    role_kinds = {
        key: sorted(votes, key=lambda kind: (-votes[kind], kind))[0]
        for key, votes in role_kind_votes.items()
    }
    return rule_ids, predicate_ids, role_ids, syntax, role_kinds, base_predicates


(_RULE_IDS, _PREDICATE_IDS, _ROLE_IDS, _RULE_SYNTAX,
 _PREDICATE_ROLE_KINDS, _BASE_PREDICATE_NAMES) = _make_vocabulary()


@dataclass(frozen=True)
class Vocabulary:
    schema_version: int
    vocabulary_version: str
    rules: dict[str, int]
    predicates: dict[str, int]
    roles: dict[str, int]
    kinds: dict[str, int]

    def to_dict(self):
        return {
            "schema_version": self.schema_version,
            "vocabulary_version": self.vocabulary_version,
            "rules": self.rules,
            "predicates": self.predicates,
            "roles": self.roles,
            "kinds": self.kinds,
        }

    def save(self, path):
        Path(path).write_text(json.dumps(self.to_dict(), indent=2,
                                         sort_keys=True) + "\n")


def _vocabulary():
    payload = {
        "schema_version": SCHEMA_VERSION,
        "vocabulary_epoch": VOCABULARY_EPOCH,
        "rules": _RULE_IDS,
        "predicates": _PREDICATE_IDS,
        "roles": _ROLE_IDS,
        "kinds": KIND_IDS,
    }
    version = hashlib.sha256(json.dumps(
        payload, sort_keys=True, separators=(",", ":")).encode()).hexdigest()
    return Vocabulary(SCHEMA_VERSION, version, _RULE_IDS, _PREDICATE_IDS,
                      _ROLE_IDS, KIND_IDS)


VOCABULARY = _vocabulary()


@dataclass(frozen=True)
class CandidateFeatures:
    arrays: dict[str, np.ndarray]
    vocabulary: Vocabulary = VOCABULARY

    def save(self, path):
        """Save arrays plus the schema/vocabulary beside them."""
        path = Path(path)
        np.savez(path, **self.arrays)
        self.vocabulary.save(path.with_suffix(path.suffix + ".vocabulary.json"))


def _canonical_facts(state, canonical, object_index):
    def replace(value):
        if isinstance(value, (tuple, list)):
            return tuple(replace(item) for item in value)
        return canonical.get(value, value)

    facts = set()
    for fact in state.engine_state.facts:
        fact = replace(fact)
        if fact and fact[0] == "peq" and fact[1] == fact[2]:
            continue
        if (fact and fact[0] in ("sublen", "subang")
                and fact[-2] == fact[-1]):
            continue
        if fact and fact[0] == "subang1" and fact[-3] == fact[-2]:
            continue
        facts.add(fact)
    def structural(value):
        if isinstance(value, tuple):
            return tuple(structural(item) for item in value)
        if value in object_index:
            return ("object", object_index[value])
        return ("constant", repr(value))

    return sorted(facts, key=structural)


def _constant(value):
    if isinstance(value, bool):
        kind, rendered = 1, str(int(value))
    elif isinstance(value, int):
        kind, rendered = 1, str(value)
    elif isinstance(value, Fraction):
        kind, rendered = 2, f"{value.numerator}/{value.denominator}"
    elif isinstance(value, float):
        kind, rendered = 3, value.hex()
    elif isinstance(value, str):
        kind, rendered = 4, value
    elif isinstance(value, (tuple, list)):
        kind, rendered = 5, repr(tuple(value))
    else:
        kind, rendered = 6, repr(value)
    bucket = int.from_bytes(hashlib.sha256(rendered.encode()).digest()[:4],
                            "big") % CONSTANT_BUCKETS + 1
    return kind, bucket


_LOOKAHEAD_CACHE = OrderedDict()
_LOOKAHEAD_CACHE_LIMIT = 100_000


def _feature_candidates(state):
    generated = spiel.enumerate_actions(
        state._proposition, state.engine_state, state.initial_kinds,
        len(state.history_moves), canonical_names=state._canonical_names(),
        validate=False)
    objects = set(state._object_order())
    return [action for action in generated
            if all(not isinstance(arg, str) or arg in objects
                   for arg in action[1])]


def candidate_join_counts(state, candidates, cache=None):
    """Count child moves joining a new result to a pre-existing object."""
    cache = _LOOKAHEAD_CACHE if cache is None else cache
    before = frozenset(state._object_order())
    logical = state.logical_state_key()
    counts = []
    for action in candidates:
        key = (logical, action)
        if key in cache:
            counts.append(cache[key])
            if cache is _LOOKAHEAD_CACHE:
                cache.move_to_end(key)
            continue
        successor, _refusal = spiel.admit_candidate(
            state.engine_state, len(state.history_moves), action)
        count = 0
        if successor is not None:
            child = state.clone()
            child.engine_state = successor
            child.history_moves.append(state._canonical_action(action))
            child._actions = None
            child._action_by_id = None
            child._parent_legal_keys = frozenset()
            made = {name for _base, name in action[2]}
            for offered in _feature_candidates(child):
                arguments = set(offered[1])
                if arguments & made and arguments & before:
                    count += 1
        cache[key] = count
        if cache is _LOOKAHEAD_CACHE and len(cache) > _LOOKAHEAD_CACHE_LIMIT:
            cache.popitem(last=False)
        counts.append(count)
    return np.asarray(counts, dtype=np.float32)


def extract(state, candidates, goal=None, lookahead=False):
    """Encode ``state`` and its ordered premise-certified ``candidates``."""
    canonical = state._canonical_names()
    objects = state._object_order()
    object_index = {name: i for i, name in enumerate(objects)}
    kinds = spiel._objects(state.engine_state, state.initial_kinds)
    given = {canonical.get(obj["name"], obj["name"])
             for obj in play.tr.objects_from_hyps(state._proposition)}

    provenance = {}
    for rule, args, bindings in state.initial_moves + state.history_moves:
        parents = tuple(dict.fromkeys(
            canonical.get(arg, arg) for arg in args
            if canonical.get(arg, arg) in object_index))
        for base, name in bindings:
            name = canonical.get(name, name)
            if name in object_index and name not in provenance:
                provenance[name] = (rule, base, parents)

    max_parents = max((len(entry[2]) for entry in provenance.values()), default=0)
    object_parent = np.zeros((len(objects), max_parents), dtype=np.int32)
    object_parent_mask = np.zeros_like(object_parent, dtype=np.bool_)
    arrays = {
        "schema_version": np.asarray([SCHEMA_VERSION], dtype=np.int32),
        "vocabulary_version": np.frombuffer(
            bytes.fromhex(VOCABULARY.vocabulary_version), dtype=np.uint8).copy(),
        "object_mask": np.ones(len(objects), dtype=np.bool_),
        "object_kind": np.asarray([
            KIND_IDS.get(kinds.get(name, "unknown"), 0) for name in objects
        ], dtype=np.int32),
        "object_given": np.asarray([name in given for name in objects],
                                   dtype=np.bool_),
        "object_creation_order": np.asarray([
            min(i, CREATION_ORDER_CLIP) for i in range(len(objects))
        ], dtype=np.int32),
        "object_creator_rule": np.asarray([
            _RULE_IDS.get(provenance.get(name, ("<none>",))[0],
                          _RULE_IDS["<none>"])
            for name in objects
        ], dtype=np.int32),
        "object_result_role": np.asarray([
            _ROLE_IDS.get(
                f"rule:{provenance[name][0]}:result:{provenance[name][1]}",
                _ROLE_IDS["<none>"]) if name in provenance else _ROLE_IDS["<none>"]
            for name in objects
        ], dtype=np.int32),
        "object_parent": object_parent,
        "object_parent_mask": object_parent_mask,
    }
    for i, name in enumerate(objects):
        for j, parent in enumerate(provenance.get(name, (None, None, ()))[2]):
            object_parent[i, j] = object_index[parent]
            object_parent_mask[i, j] = True

    if goal is None:
        goals = play.GOALS[state._proposition]
    elif goal and isinstance(goal[0], (tuple, list)):
        goals = tuple(tuple(atom) for atom in goal)
    else:
        goals = (tuple(goal),)
    given_names = {obj["name"] for obj in play.tr.objects_from_hyps(state._proposition)}
    existential = {arg for goal in goals for arg in goal[1:]
                   if isinstance(arg, str) and arg not in given_names}
    max_goal_roles = sum(len(goal) - 1 for goal in goals)
    goal_predicate = np.zeros((len(objects), max_goal_roles), dtype=np.int32)
    goal_role = np.zeros_like(goal_predicate)
    goal_mask = np.zeros_like(goal_predicate, dtype=np.bool_)
    for i, name in enumerate(objects):
        slot = 0
        for goal in goals:
            predicate = _head(goal)
            for position, argument in enumerate(goal[1:]):
                literal = canonical.get(argument, argument) == name
                witness = (argument in existential
                           and kinds.get(name) == _PREDICATE_ROLE_KINDS.get(
                               (predicate, position), "point"))
                if literal or witness:
                    goal_predicate[i, slot] = _PREDICATE_IDS.get(
                        predicate, _PREDICATE_IDS["<unk>"])
                    goal_role[i, slot] = _ROLE_IDS.get(
                        f"goal:{predicate}:{position}", _ROLE_IDS["<unk>"])
                    goal_mask[i, slot] = True
                slot += 1
    arrays.update({"object_goal_predicate": goal_predicate,
                   "object_goal_role": goal_role,
                   "object_goal_mask": goal_mask})

    facts = _canonical_facts(state, canonical, object_index)
    max_fact_args = max((len(fact) - 1 for fact in facts), default=0)
    fact_object = np.zeros((len(facts), max_fact_args), dtype=np.int32)
    fact_object_mask = np.zeros_like(fact_object, dtype=np.bool_)
    fact_role = np.zeros_like(fact_object)
    fact_constant_type = np.zeros_like(fact_object)
    fact_constant_bucket = np.zeros_like(fact_object)
    for i, fact in enumerate(facts):
        for j, argument in enumerate(fact[1:]):
            fact_role[i, j] = _ROLE_IDS.get(f"fact:{j}", _ROLE_IDS["<unk>"])
            if argument in object_index:
                fact_object[i, j] = object_index[argument]
                fact_object_mask[i, j] = True
            else:
                fact_constant_type[i, j], fact_constant_bucket[i, j] = _constant(argument)
    arrays.update({
        "fact_mask": np.ones(len(facts), dtype=np.bool_),
        "fact_predicate": np.asarray([
            (_PREDICATE_IDS.get(_head(fact), _PREDICATE_IDS["<unk>"])
             if _head(fact) in _BASE_PREDICATE_NAMES
             else _PREDICATE_IDS["<unk>"])
            for fact in facts], dtype=np.int32),
        "fact_object": fact_object,
        "fact_object_mask": fact_object_mask,
        "fact_role": fact_role,
        "fact_constant_type": fact_constant_type,
        "fact_constant_bucket": fact_constant_bucket,
    })

    max_candidate_args = max((len(action[1]) for action in candidates), default=0)
    max_premises = max((len(_RULE_SYNTAX[action[0]][1])
                        for action in candidates), default=0)
    max_conclusions = max((len(_RULE_SYNTAX[action[0]][2])
                           for action in candidates), default=0)
    candidate_object = np.zeros((len(candidates), max_candidate_args), dtype=np.int32)
    candidate_argument_role = np.zeros_like(candidate_object)
    candidate_argument_kind = np.zeros_like(candidate_object)
    candidate_argument_mask = np.zeros_like(candidate_object, dtype=np.bool_)
    candidate_premise = np.zeros((len(candidates), max_premises), dtype=np.int32)
    candidate_premise_mask = np.zeros_like(candidate_premise, dtype=np.bool_)
    candidate_conclusion = np.zeros((len(candidates), max_conclusions), dtype=np.int32)
    candidate_conclusion_mask = np.zeros_like(candidate_conclusion, dtype=np.bool_)
    for i, (rule, args, _bindings) in enumerate(candidates):
        variables, premises, conclusions = _RULE_SYNTAX[rule]
        arg_kinds = play.rule_info(rule)["arg_kinds"]
        for j, (variable, argument) in enumerate(zip(variables, args)):
            argument = canonical.get(argument, argument)
            candidate_object[i, j] = object_index[argument]
            candidate_argument_role[i, j] = _ROLE_IDS[
                f"rule:{rule}:arg:{variable}"]
            candidate_argument_kind[i, j] = KIND_IDS.get(
                arg_kinds.get(variable, "point"), 0)
            candidate_argument_mask[i, j] = True
        for j, fact in enumerate(premises):
            candidate_premise[i, j] = _PREDICATE_IDS.get(
                _head(fact), _PREDICATE_IDS["<unk>"])
            candidate_premise_mask[i, j] = True
        for j, fact in enumerate(conclusions):
            candidate_conclusion[i, j] = _PREDICATE_IDS.get(
                _head(fact), _PREDICATE_IDS["<unk>"])
            candidate_conclusion_mask[i, j] = True
    arrays.update({
        "candidate_mask": np.ones(len(candidates), dtype=np.bool_),
        "candidate_rule": np.asarray([
            _RULE_IDS[action[0]] for action in candidates], dtype=np.int32),
        "candidate_object": candidate_object,
        "candidate_argument_role": candidate_argument_role,
        "candidate_argument_kind": candidate_argument_kind,
        "candidate_argument_mask": candidate_argument_mask,
        "candidate_premise_predicate": candidate_premise,
        "candidate_premise_mask": candidate_premise_mask,
        "candidate_conclusion_predicate": candidate_conclusion,
        "candidate_conclusion_mask": candidate_conclusion_mask,
    })
    if lookahead:
        arrays["candidate_join_count"] = candidate_join_counts(state, candidates)
    return CandidateFeatures(arrays)
