#!/usr/bin/env python3
"""A small monotone equational-construction game.

``needed`` computes a proof minimal under removal by a greedy replay.  It does
not promise a minimum-cardinality proof.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from collections import deque
from itertools import product
import random


@dataclass(frozen=True, order=True)
class Term:
    op: str
    args: tuple["Term", ...] = ()

    def __str__(self):
        if self.op == "var":
            return str(self.args[0])
        if self.op == "e":
            return "e"
        if self.op == "gen":
            return str(self.args[0])
        return f"{self.op}(" + ",".join(map(str, self.args)) + ")"


def gen(name: str) -> Term:
    # A string in args is intentional: generator labels are leaves, not terms.
    return Term("gen", (name,))  # type: ignore[arg-type]


E = Term("e")


def var(name: str) -> Term:
    return Term("var", (name,))  # type: ignore[arg-type]


def mul(x: Term, y: Term) -> Term:
    return Term("mul", (x, y))


def inv(x: Term) -> Term:
    return Term("inv", (x,))


@dataclass(frozen=True, order=True)
class Move:
    op: str
    args: tuple[Term, ...]

    @property
    def result(self) -> Term:
        return Term(self.op, self.args)

    def __str__(self):
        return str(self.result)


@dataclass(frozen=True)
class MoveCompletion:
    count: int
    merges: bool


@dataclass(frozen=True)
class Goal:
    left: Term
    right: Term


@dataclass(frozen=True)
class Rule:
    name: str
    left: Term
    right: Term


X, Y, Z = var("X"), var("Y"), var("Z")
RULE_LIBRARY = {
    rule.name: rule for rule in (
        Rule("left_identity", mul(E, X), X),
        Rule("right_identity", mul(X, E), X),
        Rule("left_inverse", mul(inv(X), X), E),
        Rule("right_inverse", mul(X, inv(X)), E),
        Rule("assoc", mul(mul(X, Y), Z), mul(X, mul(Y, Z))),
        Rule("square_is_identity", mul(X, X), E),
        Rule("commutative", mul(X, Y), mul(Y, X)),
    )
}

DEFAULT_SIGNATURE = (("e", 0), ("inv", 1), ("mul", 2))


@dataclass(frozen=True)
class Theory:
    rules: tuple[Rule, ...] | frozenset[str]
    hypotheses: tuple[Goal, ...] = ()
    signature: tuple[tuple[str, int], ...] = DEFAULT_SIGNATURE

    def __post_init__(self):
        if self.rules and isinstance(next(iter(self.rules)), str):
            names = sorted(self.rules)  # legacy packet-1 constructor
            object.__setattr__(self, "rules", tuple(RULE_LIBRARY[n] for n in names))
        elif not isinstance(self.rules, tuple):
            object.__setattr__(self, "rules", tuple(self.rules))
        signature = tuple(sorted(self.signature))
        if len(dict(signature)) != len(signature):
            raise ValueError("operation names in a signature must be unique")
        object.__setattr__(self, "signature", signature)
        arities = dict(signature)

        def validate(pattern):
            if pattern.op == "var":
                return
            if pattern.op not in arities or arities[pattern.op] != len(pattern.args):
                raise ValueError(f"invalid rule pattern operation {pattern.op}/{len(pattern.args)}")
            for child in pattern.args:
                validate(child)

        for rule in self.rules:
            validate(rule.left)
            validate(rule.right)

    @property
    def arities(self):
        return dict(self.signature)


THEORIES = {
    "group": Theory(frozenset({"left_identity", "right_identity",
                                "left_inverse", "right_inverse", "assoc"})),
    "left_group": Theory(frozenset({"left_identity", "left_inverse", "assoc"})),
    "exponent_two": Theory(frozenset({"left_identity", "right_identity",
                                       "left_inverse", "right_inverse", "assoc",
                                       "square_is_identity"})),
    "monoid": Theory(frozenset({"left_identity", "right_identity", "assoc"}),
                     signature=(("e", 0), ("mul", 2))),
    "commutative_group": Theory(frozenset({"left_identity", "right_identity",
                                            "left_inverse", "right_inverse",
                                            "assoc", "commutative"})),
}


def theory(value: str | Theory) -> Theory:
    return THEORIES[value] if isinstance(value, str) else value


def _plain_pattern(t: Term):
    if t.op == "var":
        return ("var", t.args[0])
    return (t.op, *(_plain_pattern(a) for a in t.args))


def rule_statements(theory_: str | Theory):
    """Return rules as plain ``(name, left, right)`` nested tuples."""
    return tuple((r.name, _plain_pattern(r.left), _plain_pattern(r.right))
                 for r in theory(theory_).rules)


def random_theory(seed: int, min_rules=2, max_rules=4,
                  max_pattern_depth=2) -> Theory:
    """Make a deterministic, well-formed small equational theory."""
    rng = random.Random(seed)
    signature = []
    for i in range(rng.randint(1, 2)):
        signature.append((f"b{i}", 2))
    if rng.randrange(2):
        signature.append(("u0", 1))
    if rng.randrange(2):
        signature.append(("c0", 0))
    binary = [op for op, arity in signature if arity == 2]
    unary = [op for op, arity in signature if arity == 1]

    def pattern(leaves):
        nodes = list(leaves)
        rng.shuffle(nodes)
        while len(nodes) > 1:
            index = rng.randrange(len(nodes) - 1)
            combined = Term(rng.choice(binary), (nodes[index], nodes[index + 1]))
            nodes[index:index + 2] = [combined]
        result = nodes[0]
        for _ in range(rng.randint(0, max_pattern_depth) if unary else 0):
            result = Term(rng.choice(unary), (result,))
        return result

    rules = []
    for i in range(rng.randint(min_rules, max_rules)):
        count = rng.randint(3, 4)
        variable_count = rng.randint(1, min(3, count))
        leaves = tuple((X, Y, Z)[j % variable_count] for j in range(count))
        left = pattern(leaves)
        right = pattern(leaves)
        while right == left:
            right = pattern(leaves)
        rules.append(Rule(f"r{i}", left, right))
    return Theory(tuple(rules), signature=tuple(signature))


def subterms(t: Term):
    yield t
    if t.op not in ("gen", "var"):
        for arg in t.args:
            yield from subterms(arg)


@dataclass
class Position:
    theory: Theory
    terms: list[Term] = field(default_factory=list)
    ids: dict[Term, int] = field(default_factory=dict)
    parent: list[int] = field(default_factory=list)

    def add_term(self, t: Term) -> int:
        if t in self.ids:
            return self.ids[t]
        if t.op == "var":
            raise ValueError("pattern variables are not position terms")
        if t.op != "gen":
            arity = self.theory.arities.get(t.op)
            if arity is None or arity != len(t.args):
                raise ValueError(f"{t.op}/{len(t.args)} is outside the signature")
        if t.op != "gen" and any(a not in self.ids for a in t.args):
            raise ValueError(f"arguments must be present before {t}")
        i = len(self.terms)
        self.terms.append(t)
        self.ids[t] = i
        self.parent.append(i)
        return i

    def find(self, i: int) -> int:
        while self.parent[i] != i:
            self.parent[i] = self.parent[self.parent[i]]
            i = self.parent[i]
        return i

    def union(self, i: int, j: int) -> bool:
        i, j = self.find(i), self.find(j)
        if i == j:
            return False
        early, late = sorted((i, j))
        self.parent[late] = early
        return True

    def representative_term(self, t: Term) -> Term:
        return self.terms[self.find(self.ids[t])]

    def copy(self) -> "Position":
        return Position(self.theory, list(self.terms), dict(self.ids), list(self.parent))


def _add_tree(p: Position, t: Term):
    if t.op not in ("gen", "var"):
        for a in t.args:
            _add_tree(p, a)
    p.add_term(t)


def start(theory_: str | Theory, goal: Goal, generators=()) -> Position:
    th = theory(theory_)
    p = Position(th)
    for op, arity in th.signature:
        if arity == 0:
            p.add_term(Term(op))
    for name in generators:
        p.add_term(gen(name) if isinstance(name, str) else name)
    _add_tree(p, goal.left)
    _add_tree(p, goal.right)
    closure(p)
    return p


def _lookup(p: Position, op: str, args: tuple[Term, ...]):
    """An operation node with these arguments, modulo current equality."""
    reps = tuple(p.find(p.ids[a]) for a in args)
    for i, t in enumerate(p.terms):
        if t.op == op and tuple(p.find(p.ids[a]) for a in t.args) == reps:
            return i
    return None


def _lookup_classes(p: Position, op: str, classes: tuple[int, ...]):
    classes = tuple(p.find(i) for i in classes)
    for i, t in enumerate(p.terms):
        if t.op == op and tuple(p.find(p.ids[a]) for a in t.args) == classes:
            return i
    return None


def _merge_if_present(p: Position, left: Term, right: Term) -> bool:
    if left not in p.ids or right not in p.ids:
        return False
    return p.union(p.ids[left], p.ids[right])


def _matches(p: Position, pattern: Term, class_id: int, env: dict[str, int],
             index: dict[tuple[int, str], list[Term]]):
    """Yield extensions matching a pattern against one equality class."""
    class_id = p.find(class_id)
    if pattern.op == "var":
        name = str(pattern.args[0])
        if name in env:
            if p.find(env[name]) == class_id:
                yield env
        else:
            yield {**env, name: class_id}
        return
    for term in index.get((class_id, pattern.op), ()):
        states = [env]
        for child, actual in zip(pattern.args, term.args):
            states = [new for state in states
                      for new in _matches(p, child, p.ids[actual], state, index)]
        yield from states


def _eval_pattern(p: Position, pattern: Term, env: dict[str, int],
                  signatures: dict[tuple[str, tuple[int, ...]], int]):
    if pattern.op == "var":
        return p.find(env[str(pattern.args[0])])
    args = []
    for child in pattern.args:
        value = _eval_pattern(p, child, env, signatures)
        if value is None:
            return None
        args.append(value)
    node = signatures.get((pattern.op, tuple(args)))
    return None if node is None else p.find(node)


def _pattern_variables(pattern: Term):
    if pattern.op == "var":
        return {str(pattern.args[0])}
    return set().union(*(_pattern_variables(a) for a in pattern.args), set())


def _rule_merges(p: Position, rule: Rule):
    anchor = rule.left if rule.left.op != "var" else rule.right
    changed = False
    seen = set()
    classes = sorted({p.find(i) for i in range(len(p.terms))})
    index: dict[tuple[int, str], list[Term]] = {}
    signatures = {}
    for i, term in enumerate(p.terms):
        index.setdefault((p.find(i), term.op), []).append(term)
        signatures.setdefault(
            (term.op, tuple(p.find(p.ids[a]) for a in term.args)
             if term.op != "gen" else ()), i)
    variables = _pattern_variables(rule.left) | _pattern_variables(rule.right)
    anchor_classes = (classes if anchor.op == "var" else
                      sorted({class_id for class_id, op in index if op == anchor.op}))
    merges = []
    for class_id in anchor_classes:
        for partial in _matches(p, anchor, class_id, {}, index):
            missing = sorted(variables - partial.keys())
            for values in product(classes, repeat=len(missing)):
                env = {**partial, **dict(zip(missing, values))}
                key = tuple(sorted((name, p.find(value))
                                   for name, value in env.items()))
                if key in seen:
                    continue
                seen.add(key)
                left = _eval_pattern(p, rule.left, env, signatures)
                right = _eval_pattern(p, rule.right, env, signatures)
                if left is not None and right is not None:
                    merges.append((left, right))
    for left, right in merges:
        changed |= p.union(left, right)
    return changed


def closure(p: Position) -> None:
    """Apply generic congruence, hypotheses, and data rules to fixpoint."""
    while True:
        changed = False
        signatures = {}
        for i, term in enumerate(p.terms):
            if not term.args or term.op == "gen":
                continue
            sig = (term.op, tuple(p.find(p.ids[a]) for a in term.args))
            if sig in signatures:
                changed |= p.union(signatures[sig], i)
            else:
                signatures[sig] = i
        for hypothesis in p.theory.hypotheses:
            changed |= _merge_if_present(p, hypothesis.left, hypothesis.right)
        for rule in p.theory.rules:
            changed |= _rule_merges(p, rule)
        if not changed:
            return


def candidates(p: Position) -> list[Move]:
    reps = [p.terms[i] for i in range(len(p.terms)) if p.find(i) == i]
    out = []
    for op, arity in p.theory.signature:
        if not arity:
            continue
        for args in product(reps, repeat=arity):
            if _lookup(p, op, args) is None:
                out.append(Move(op, args))
    return sorted(out, key=lambda m: (m.op, tuple(map(str, m.args))))


def _present_signatures(p: Position):
    signatures = {}
    for i, term in enumerate(p.terms):
        if term.op == "gen":
            continue
        signatures.setdefault(
            (term.op, tuple(p.find(p.ids[a]) for a in term.args)), p.find(i))
    return signatures


def _one_missing(pattern: Term, env: dict[str, int], signatures):
    """Evaluate a pattern that may lack its root, but no deeper node."""
    if pattern.op == "var":
        return env[str(pattern.args[0])], None
    args = []
    for child in pattern.args:
        value, missing = _one_missing(child, env, signatures)
        if missing is not None:
            return None, ("blocked",)
        args.append(value)
    signature = (pattern.op, tuple(args))
    if signature in signatures:
        return signatures[signature], None
    return ("new", signature), signature


def completing_moves(p: Position) -> dict[Move, MoveCompletion]:
    """Count direct rule instances completed by every legal construction.

    Variables range over classes already present in ``p``.  A construction
    completes an instance when it is the sole absent operation node across
    the two equation patterns.  The calculation indexes missing signatures;
    it does not apply candidates to copied positions.
    """
    moves = candidates(p)
    by_signature = {
        (move.op, tuple(p.find(p.ids[a]) for a in move.args)): move
        for move in moves
    }
    counts = {move: 0 for move in moves}
    merges = {move: False for move in moves}
    classes = sorted({p.find(i) for i in range(len(p.terms))})
    signatures = _present_signatures(p)
    for rule in p.theory.rules:
        names = sorted(_pattern_variables(rule.left) |
                       _pattern_variables(rule.right))
        for values in product(classes, repeat=len(names)):
            env = dict(zip(names, values))
            left, left_missing = _one_missing(rule.left, env, signatures)
            right, right_missing = _one_missing(rule.right, env, signatures)
            missing = {x for x in (left_missing, right_missing)
                       if x is not None}
            if ("blocked",) in missing or len(missing) != 1:
                continue
            signature = next(iter(missing))
            move = by_signature.get(signature)
            if move is None:
                continue
            counts[move] += 1
            merges[move] |= left != right
    return {move: MoveCompletion(counts[move], merges[move]) for move in moves}


def _is_candidate(p: Position, move: Move):
    arity = p.theory.arities.get(move.op)
    if arity is None or not arity or len(move.args) != arity:
        return False
    if any(arg not in p.ids or p.find(p.ids[arg]) != p.ids[arg]
           for arg in move.args):
        return False
    return _lookup(p, move.op, move.args) is None


def _apply_candidate(p: Position, move: Move) -> Term:
    args = tuple(p.representative_term(a) for a in move.args)
    result = Term(move.op, args)
    p.add_term(result)
    closure(p)
    return result


def apply(p: Position, move: Move) -> Term:
    if move not in candidates(p):
        raise ValueError(f"not a candidate: {move}")
    return _apply_candidate(p, move)


def entails(p: Position, goal: Goal) -> bool:
    return (goal.left in p.ids and goal.right in p.ids
            and p.find(p.ids[goal.left]) == p.find(p.ids[goal.right]))


def facts(p: Position) -> list[Goal]:
    out = []
    for i, left in enumerate(p.terms):
        for j in range(i + 1, len(p.terms)):
            if p.find(i) == p.find(j):
                out.append(Goal(left, p.terms[j]))
    return out


def replay(theory_: str | Theory, goal: Goal, moves: list[Move], generators=()):
    p = start(theory_, goal, generators)
    kept = []
    for move in moves:
        if all(a in p.ids for a in move.args) and move in candidates(p):
            apply(p, move)
            kept.append(move)
    return p, kept


def needed(theory_: str | Theory, start_: Position, moves: list[Move], goal: Goal):
    """Return a subproof minimal under removal, not necessarily a minimum."""
    keep = list(moves)

    def from_start(candidate_moves):
        p = start_.copy()
        accepted = []
        for move in candidate_moves:
            # A removed construction also removes every later construction
            # whose argument was its result: those arguments are simply no
            # longer present in this replay.
            if all(a in p.ids for a in move.args) and move in candidates(p):
                apply(p, move)
                accepted.append(move)
        return p, accepted

    i = 0
    while i < len(keep):
        trial = keep[:i] + keep[i + 1:]
        p, actually_kept = from_start(trial)
        if entails(p, goal):
            keep = actually_kept
        else:
            i += 1
    return keep


def _word(t: Term):
    if t.op == "e":
        return []
    if t.op == "gen":
        return [(t.args[0], 1)]
    if t.op == "mul":
        return _word(t.args[0]) + _word(t.args[1])
    if t.op == "inv":
        return [(x, -s) for x, s in reversed(_word(t.args[0]))]
    raise ValueError(t)


def free_group_equal(t1: Term, t2: Term) -> bool:
    def reduce(word):
        stack = []
        for letter in word:
            if stack and stack[-1] == (letter[0], -letter[1]):
                stack.pop()
            else:
                stack.append(letter)
        return stack
    return reduce(_word(t1)) == reduce(_word(t2))


def _state_key(p: Position):
    classes = {}
    for i, term in enumerate(p.terms):
        classes.setdefault(p.find(i), set()).add(term)
    # Independent of construction order: commuting move sequences reach one
    # search state when they have the same terms and equality partition.
    return (frozenset(p.terms),
            frozenset(frozenset(part) for part in classes.values()))


def shortest_proof(theory_: str | Theory, goal: Goal, max_moves=3):
    initial = start(theory_, goal)
    if entails(initial, goal):
        return 0, []
    queue = deque([(initial, [])])
    seen = {_state_key(initial)}
    while queue:
        p, path = queue.popleft()
        if len(path) >= max_moves:
            continue
        for move in candidates(p):
            q = p.copy()
            apply(q, move)
            new_path = path + [move]
            if entails(q, goal):
                return len(new_path), new_path
            key = _state_key(q)
            if key not in seen:
                seen.add(key)
                queue.append((q, new_path))
    return None
