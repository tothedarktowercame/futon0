#!/usr/bin/env python3
"""Monotone guarded-Horn construction game over one unsorted domain.

Elements are construction terms.  Equality is union-find; relation facts are
stored over equality classes.  Rules never create terms: every term pattern in
a firing instance must already be represented.  Operations are the only way to
add terms, and are offered only when their guards hold.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from itertools import product
from typing import Iterable

import metagame_eq as eq

Term, Move = eq.Term, eq.Move
gen, var = eq.gen, eq.var


@dataclass(frozen=True)
class Rel:
    name: str
    args: tuple[Term, ...]


@dataclass(frozen=True)
class Equation:
    left: Term
    right: Term


@dataclass(frozen=True)
class Neq:
    left: Term
    right: Term


@dataclass(frozen=True)
class Contradiction:
    pass


CONTRADICTION = Contradiction()
Atom = Rel | Equation | Neq


@dataclass(frozen=True)
class Rule:
    name: str
    premises: tuple[Atom, ...]
    conclusion: Atom | Contradiction


@dataclass(frozen=True)
class Operation:
    name: str
    arity: int
    variables: tuple[str, ...]
    guard: tuple[Atom, ...] = ()


@dataclass(frozen=True)
class Theory:
    rules: tuple[Rule, ...]
    operations: tuple[Operation, ...]

    @property
    def arities(self):
        return {op.name: op.arity for op in self.operations}


@dataclass(frozen=True)
class Goal:
    atoms: tuple[Atom, ...]
    variables: tuple[str, ...] = ()


@dataclass
class Position:
    theory: Theory
    terms: list[Term] = field(default_factory=list)
    ids: dict[Term, int] = field(default_factory=dict)
    parent: list[int] = field(default_factory=list)
    relations: set[tuple[str, tuple[int, ...]]] = field(default_factory=set)
    inconsistent: bool = False
    initial_givens: tuple[Term, ...] = ()
    initial_hypotheses: tuple[Atom, ...] = ()

    def add_term(self, term: Term):
        if term in self.ids:
            return self.ids[term]
        if term.op == "var":
            raise ValueError("variables are not position elements")
        if term.op != "gen":
            arity = self.theory.arities.get(term.op)
            if arity != len(term.args):
                raise ValueError(f"unknown operation {term.op}/{len(term.args)}")
            if any(arg not in self.ids for arg in term.args):
                raise ValueError("operation arguments must already be present")
        idx = len(self.terms)
        self.terms.append(term); self.ids[term] = idx; self.parent.append(idx)
        return idx

    def find(self, idx):
        while self.parent[idx] != idx:
            self.parent[idx] = self.parent[self.parent[idx]]
            idx = self.parent[idx]
        return idx

    def union(self, left, right):
        left, right = self.find(left), self.find(right)
        if left == right:
            return False
        early, late = sorted((left, right))
        self.parent[late] = early
        return True

    def representative_term(self, term):
        return self.terms[self.find(self.ids[term])]

    def copy(self):
        return Position(self.theory, list(self.terms), dict(self.ids),
                        list(self.parent), set(self.relations), self.inconsistent,
                        self.initial_givens, self.initial_hypotheses)


def _vars_term(term):
    if term.op == "var":
        return {str(term.args[0])}
    if term.op == "gen":
        return set()
    return set().union(*(_vars_term(x) for x in term.args), set())


def _terms_atom(atom):
    if isinstance(atom, Rel): return atom.args
    if isinstance(atom, (Equation, Neq)): return (atom.left, atom.right)
    return ()


def _vars_atom(atom):
    return set().union(*(_vars_term(x) for x in _terms_atom(atom)), set())


def _index(position):
    by_class = {}
    signatures = {}
    for idx, term in enumerate(position.terms):
        cls = position.find(idx)
        by_class.setdefault((cls, term.op), []).append(term)
        args = ((term.args[0],) if term.op == "gen" else
                tuple(position.find(position.ids[x]) for x in term.args))
        signatures.setdefault((term.op, args), idx)
    return by_class, signatures


def _match(position, pattern, cls, env, by_class):
    cls = position.find(cls)
    if pattern.op == "var":
        name = str(pattern.args[0])
        if name in env:
            if position.find(env[name]) == cls: yield env
        else:
            yield {**env, name: cls}
        return
    for term in by_class.get((cls, pattern.op), ()):
        if pattern.op == "gen":
            if term == pattern: yield env
            continue
        states = [env]
        for child, actual in zip(pattern.args, term.args):
            states = [e2 for e in states
                      for e2 in _match(position, child, position.ids[actual], e, by_class)]
        yield from states


def _eval(position, pattern, env, signatures):
    if pattern.op == "var": return position.find(env[str(pattern.args[0])])
    if pattern.op == "gen":
        node = position.ids.get(pattern)
        return None if node is None else position.find(node)
    args = []
    for child in pattern.args:
        value = _eval(position, child, env, signatures)
        if value is None: return None
        args.append(value)
    node = signatures.get((pattern.op, tuple(args)))
    return None if node is None else position.find(node)


def _holds(position, atom, env, signatures):
    if isinstance(atom, Rel):
        args = tuple(_eval(position, x, env, signatures) for x in atom.args)
        return None not in args and (atom.name, args) in position.relations
    left, right = (_eval(position, x, env, signatures)
                   for x in (atom.left, atom.right))
    if left is None or right is None: return False
    if isinstance(atom, Neq):
        return ("neq", tuple(sorted((left, right)))) in position.relations
    return left == right


def _seed_envs(position, rule, by_class):
    patterns = [t for atom in (*rule.premises, rule.conclusion)
                for t in _terms_atom(atom) if t.op != "var"]
    if not patterns: return [{}]
    anchor = max(patterns, key=lambda t: (len(_vars_term(t)), len(t.args)))
    out = []
    classes = sorted({position.find(i) for i in range(len(position.terms))})
    for cls in classes: out.extend(_match(position, anchor, cls, {}, by_class))
    return out


def _environments(position, rule, by_class, signatures):
    classes = sorted({position.find(i) for i in range(len(position.terms))})
    variables = set().union(*(_vars_atom(a) for a in rule.premises),
                            _vars_atom(rule.conclusion))
    seen = set()
    for seed in _seed_envs(position, rule, by_class):
        # Relation premises are indexes, not a Cartesian product.  Bind their
        # variables directly from matching stored tuples.  Pick the next join
        # per partial environment: A5 is tractable only when E(a,b,A,B) is
        # joined before the otherwise independent second betweenness tuple.
        relation_premises = [atom for atom in rule.premises
                             if not isinstance(atom, Equation)]
        counts = {}
        for name, _args in position.relations:
            counts[name] = counts.get(name, 0) + 1

        def join(state, remaining):
            if not remaining:
                yield state
                return
            atom = max(remaining, key=lambda item: (
                len(_vars_atom(item) & state.keys()),
                -counts.get(item.name if isinstance(item, Rel) else "neq", 0)))
            name = atom.name if isinstance(atom, Rel) else "neq"
            patterns = atom.args if isinstance(atom, Rel) else (atom.left, atom.right)
            indexed_facts = list(position.relations)
            if name == "neq":
                indexed_facts += [(n, tuple(reversed(xs))) for n, xs in position.relations
                                  if n == "neq" and xs[0] != xs[1]]
            for fact_name, fact_args in indexed_facts:
                if fact_name != name or len(fact_args) != len(patterns): continue
                partials = [state]
                for pattern, cls in zip(patterns, fact_args):
                    partials = [e2 for e in partials
                                for e2 in _match(position, pattern, cls, e, by_class)]
                for partial in partials:
                    yield from join(partial, tuple(x for x in remaining if x is not atom))

        for state in join(seed, tuple(relation_premises)):
            missing = sorted(variables - state.keys())
            for values in product(classes, repeat=len(missing)):
                env = {**state, **dict(zip(missing, values))}
                key = tuple(sorted((k, position.find(v)) for k, v in env.items()))
                if key in seen: continue
                seen.add(key)
                # Presence is checked for every term, including the conclusion.
                if all(_eval(position, term, env, signatures) is not None
                       for atom in (*rule.premises, rule.conclusion)
                       for term in _terms_atom(atom)) and all(
                           _holds(position, atom, env, signatures)
                           for atom in rule.premises):
                    yield env


def _canonical_relations(position):
    return {(name, tuple(position.find(x) for x in args))
            for name, args in position.relations}


def closure(position):
    """Close congruence and Horn rules; record inconsistency, never raise it."""
    while True:
        changed = False
        signatures = {}
        for idx, term in enumerate(position.terms):
            if term.op == "gen": continue
            sig = (term.op, tuple(position.find(position.ids[x]) for x in term.args))
            if sig in signatures: changed |= position.union(signatures[sig], idx)
            else: signatures[sig] = idx
        rewritten = _canonical_relations(position)
        if rewritten != position.relations:
            position.relations = rewritten; changed = True
        if any(name == "neq" and args[0] == args[1]
               for name, args in position.relations):
            position.inconsistent = True
        by_class, signatures = _index(position)
        for rule in position.theory.rules:
            for env in _environments(position, rule, by_class, signatures):
                conclusion = rule.conclusion
                if isinstance(conclusion, Contradiction):
                    position.inconsistent = True
                elif isinstance(conclusion, Equation):
                    left = _eval(position, conclusion.left, env, signatures)
                    right = _eval(position, conclusion.right, env, signatures)
                    changed |= position.union(left, right)
                else:
                    terms = conclusion.args if isinstance(conclusion, Rel) else (
                        conclusion.left, conclusion.right)
                    args = tuple(_eval(position, x, env, signatures) for x in terms)
                    name = conclusion.name if isinstance(conclusion, Rel) else "neq"
                    fact = (name, args)
                    if name == "neq": fact = (name, tuple(sorted(args)))
                    if fact not in position.relations:
                        position.relations.add(fact); changed = True
        if not changed: return position


def start(theory_, hypotheses=(), givens=()):
    th = theory(theory_)
    actual_givens = tuple(gen(x) if isinstance(x, str) else x for x in givens)
    position = Position(th, initial_givens=actual_givens,
                        initial_hypotheses=tuple(hypotheses))
    for op in th.operations:
        if op.arity == 0: position.add_term(Term(op.name))
    for given in actual_givens: position.add_term(given)
    # Hypotheses do not create elements; their terms must be among the givens.
    _, signatures = _index(position)
    env = {}
    for atom in hypotheses:
        if isinstance(atom, Equation):
            left, right = (_eval(position, x, env, signatures)
                           for x in (atom.left, atom.right))
            if left is None or right is None: raise ValueError("unknown hypothesis term")
            position.union(left, right)
        else:
            terms = atom.args if isinstance(atom, Rel) else (atom.left, atom.right)
            args = tuple(_eval(position, x, env, signatures) for x in terms)
            if None in args: raise ValueError("unknown hypothesis term")
            name = atom.name if isinstance(atom, Rel) else "neq"
            if name == "neq": args = tuple(sorted(args))
            position.relations.add((name, args))
    return closure(position)


def candidates(position):
    reps = [position.terms[i] for i in range(len(position.terms))
            if position.find(i) == i]
    _, signatures = _index(position)
    result = []
    for op in position.theory.operations:
        if not op.arity: continue
        for args in product(reps, repeat=op.arity):
            classes = tuple(position.find(position.ids[x]) for x in args)
            if (op.name, classes) in signatures: continue
            env = dict(zip(op.variables, classes))
            if all(_holds(position, atom, env, signatures) for atom in op.guard):
                result.append(Move(op.name, args))
    return sorted(result, key=lambda m: (m.op, tuple(map(str, m.args))))


def _valid_move(position, move):
    op = next((x for x in position.theory.operations if x.name == move.op), None)
    if op is None or len(move.args) != op.arity or any(x not in position.ids for x in move.args):
        return False
    classes = tuple(position.find(position.ids[x]) for x in move.args)
    _, signatures = _index(position)
    if (op.name, classes) in signatures:
        return False
    env = dict(zip(op.variables, classes))
    return all(_holds(position, atom, env, signatures) for atom in op.guard)


def apply(position, move):
    canonical = Move(move.op, tuple(position.representative_term(x) for x in move.args))
    if not _valid_move(position, canonical): raise ValueError(f"not a candidate: {move}")
    position.add_term(canonical.result)
    closure(position)
    return not position.inconsistent


def entails(position, goal):
    goal = goal if isinstance(goal, Goal) else Goal(tuple(goal))
    _, signatures = _index(position)
    variables = set(goal.variables) | set().union(*(_vars_atom(a) for a in goal.atoms), set())
    classes = sorted({position.find(i) for i in range(len(position.terms))})
    for values in product(classes, repeat=len(variables)):
        env = dict(zip(sorted(variables), values))
        if all(_holds(position, atom, env, signatures) for atom in goal.atoms): return True
    return False


def facts(position):
    out = []
    for name, args in sorted(_canonical_relations(position)):
        terms = tuple(position.terms[position.find(x)] for x in args)
        out.append(Neq(*terms) if name == "neq" else Rel(name, terms))
    for i, left in enumerate(position.terms):
        for right in position.terms[i + 1:]:
            if position.find(position.ids[left]) == position.find(position.ids[right]):
                out.append(Equation(left, right))
    return out


def replay(theory_, opening, moves):
    position = start(theory_, opening.initial_hypotheses, opening.initial_givens)
    kept = []
    for move in moves:
        mapped = Move(move.op, tuple(position.representative_term(x)
                                     for x in move.args if x in position.ids))
        if len(mapped.args) != len(move.args) or mapped not in candidates(position): continue
        apply(position, mapped); kept.append(move)
    return position, kept


def needed(theory_, opening, moves, goal):
    keep = list(moves)
    for move in list(moves):
        trial = [x for x in keep if x != move]
        position, _ = replay(theory_, opening, trial)
        if entails(position, goal): keep = trial
    return keep


def _plain(term):
    return ("var", term.args[0]) if term.op == "var" else (term.op, *map(_plain, term.args))


def _plain_atom(atom):
    if isinstance(atom, Rel): return (atom.name, *map(_plain, atom.args))
    if isinstance(atom, Neq): return ("neq", _plain(atom.left), _plain(atom.right))
    if isinstance(atom, Equation): return ("=", _plain(atom.left), _plain(atom.right))
    return ("contradiction",)


def rule_statements(theory_):
    return tuple((r.name, tuple(map(_plain_atom, r.premises)), _plain_atom(r.conclusion))
                 for r in theory(theory_).rules)


V = {x: var(x) for x in "qabcxypdrsABCD"}
def R(name, *xs): return Rel(name, tuple(V[x] if isinstance(x, str) else x for x in xs))
def N(a, b): return Neq(V[a], V[b])


def _equational(name):
    source = eq.theory(name)
    rules = tuple(Rule(r.name, (), Equation(r.left, r.right)) for r in source.rules)
    operations = tuple(Operation(op, arity, tuple(f"v{i}" for i in range(arity)))
                       for op, arity in source.signature)
    return Theory(rules, operations)


def _line():
    q,a,b,c,x,y,p,d = (V[k] for k in "qabcxypd")
    ex = Term("ext", (q,a,b,c))
    rules = [
        Rule("A1", (), Rel("E", (a,b,b,a))),
        Rule("A2", (R("E","a","b","r","s"), R("E","a","b","p","q")), R("E","p","q","r","s")),
        Rule("A3", (R("E","a","b","c","c"),), Equation(a,b)),
        Rule("A5", (N("a","b"), R("B","a","b","c"), R("B","A","B","C"),
                    R("E","a","b","A","B"), R("E","b","c","B","C"),
                    R("E","a","d","A","D"), R("E","b","d","B","D")), R("E","c","d","C","D")),
        Rule("ext_metric", (N("q","a"),), Rel("E", (a,ex,b,c))),
        Rule("ext_strict", (N("q","a"), N("b","c")), Rel("B", (q,a,ex))),
        Rule("ext_zero", (N("q","a"), Equation(b,c)), Equation(ex,a)),
        Rule("A6", (Rel("B", (a,b,a)),), CONTRADICTION),
        Rule("A14", (Rel("B", (a,b,c)),), Rel("B", (c,b,a))),
        Rule("A15", (Rel("B", (a,b,d)), Rel("B", (b,c,d))), Rel("B", (a,b,c))),
        Rule("A16", (Rel("B", (a,b,c)), Rel("B", (b,c,d)), Neq(b,c)), Rel("B", (a,b,d))),
        Rule("B_neq_left", (Rel("B", (a,b,c)),), Neq(a,b)),
        Rule("B_neq_right", (Rel("B", (a,b,c)),), Neq(b,c)),
        Rule("B_neq_ends", (Rel("B", (a,b,c)),), Neq(a,c)),
        Rule("neq_symm", (Neq(a,b),), Neq(b,a)),
    ]
    return Theory(tuple(rules), (Operation("ext", 4, ("q","a","b","c"), (N("q","a"),)),))


THEORIES = {name: _equational(name) for name in ("group", "left_group", "exponent_two")}
THEORIES["line"] = _line()
# A17/A18 are intentionally absent: their conclusions are disjunctions.


def theory(value): return THEORIES[value] if isinstance(value, str) else value
