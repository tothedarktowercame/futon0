#!/usr/bin/env python3
"""A small monotone equational-construction game.

``needed`` computes a proof minimal under removal by a greedy replay.  It does
not promise a minimum-cardinality proof.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from collections import deque


@dataclass(frozen=True, order=True)
class Term:
    op: str
    args: tuple["Term", ...] = ()

    def __str__(self):
        if self.op == "e":
            return "e"
        if self.op == "gen":
            return str(self.args[0])
        if self.op == "inv":
            return f"inv({self.args[0]})"
        return f"mul({self.args[0]},{self.args[1]})"


def gen(name: str) -> Term:
    # A string in args is intentional: generator labels are leaves, not terms.
    return Term("gen", (name,))  # type: ignore[arg-type]


E = Term("e")


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
class Goal:
    left: Term
    right: Term


@dataclass(frozen=True)
class Theory:
    rules: frozenset[str]
    hypotheses: tuple[Goal, ...] = ()


THEORIES = {
    "group": Theory(frozenset({"left_identity", "right_identity",
                                "left_inverse", "right_inverse", "assoc"})),
    "left_group": Theory(frozenset({"left_identity", "left_inverse", "assoc"})),
    "exponent_two": Theory(frozenset({"left_identity", "right_identity",
                                       "left_inverse", "right_inverse", "assoc",
                                       "square_is_identity"})),
}


def theory(value: str | Theory) -> Theory:
    return THEORIES[value] if isinstance(value, str) else value


def subterms(t: Term):
    yield t
    if t.op in ("mul", "inv"):
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
        if t.op in ("mul", "inv") and any(a not in self.ids for a in t.args):
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
    if t.op in ("mul", "inv"):
        for a in t.args:
            _add_tree(p, a)
    p.add_term(t)


def start(theory_: str | Theory, goal: Goal, generators=()) -> Position:
    th = theory(theory_)
    p = Position(th)
    p.add_term(E)
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


def _merge_if_present(p: Position, left: Term, right: Term) -> bool:
    if left not in p.ids or right not in p.ids:
        return False
    return p.union(p.ids[left], p.ids[right])


def closure(p: Position) -> None:
    """Apply congruence, ground hypotheses, and selected laws to fixpoint."""
    while True:
        changed = False
        # Congruence: operation nodes with equal argument classes coincide.
        signatures = {}
        for i, t in enumerate(p.terms):
            if t.op not in ("mul", "inv"):
                continue
            sig = (t.op, tuple(p.find(p.ids[a]) for a in t.args))
            if sig in signatures:
                changed |= p.union(signatures[sig], i)
            else:
                signatures[sig] = i

        for h in p.theory.hypotheses:
            changed |= _merge_if_present(p, h.left, h.right)

        e_id = p.ids.get(E)
        reps = [p.terms[p.find(i)] for i in range(len(p.terms))]
        for i, x in enumerate(p.terms):
            rx = reps[i]
            if "left_identity" in p.theory.rules:
                z = _lookup(p, "mul", (E, rx))
                if z is not None:
                    changed |= p.union(z, i)
            if "right_identity" in p.theory.rules:
                z = _lookup(p, "mul", (rx, E))
                if z is not None:
                    changed |= p.union(z, i)
            xi = _lookup(p, "inv", (rx,))
            if xi is not None and e_id is not None:
                ix = p.terms[p.find(xi)]
                if "left_inverse" in p.theory.rules:
                    z = _lookup(p, "mul", (ix, rx))
                    if z is not None:
                        changed |= p.union(z, e_id)
                if "right_inverse" in p.theory.rules:
                    z = _lookup(p, "mul", (rx, ix))
                    if z is not None:
                        changed |= p.union(z, e_id)
            if "square_is_identity" in p.theory.rules and e_id is not None:
                z = _lookup(p, "mul", (rx, rx))
                if z is not None:
                    changed |= p.union(z, e_id)

        if "assoc" in p.theory.rules:
            # Inspect only present multiplication nodes.  For an outer node
            # (xy)z, each multiplication node in the class of its left
            # argument supplies x,y; the other two mentioned products must
            # still be present before the law fires.
            decompositions = {}
            mul_nodes = []
            for i, t in enumerate(p.terms):
                if t.op == "mul":
                    decompositions.setdefault(p.find(i), []).append(t.args)
                    mul_nodes.append((i, t))
            for lhs, outer in mul_nodes:
                left, z = outer.args
                for x, y in decompositions.get(p.find(p.ids[left]), ()):
                    yz = _lookup(p, "mul", (y, z))
                    if yz is None:
                        continue
                    rhs = _lookup(p, "mul", (x, p.terms[p.find(yz)]))
                    if rhs is not None:
                        changed |= p.union(lhs, rhs)
        if not changed:
            return


def candidates(p: Position) -> list[Move]:
    reps = [p.terms[i] for i in range(len(p.terms)) if p.find(i) == i]
    out = []
    for x in reps:
        if _lookup(p, "inv", (x,)) is None:
            out.append(Move("inv", (x,)))
    for x in reps:
        for y in reps:
            if _lookup(p, "mul", (x, y)) is None:
                out.append(Move("mul", (x, y)))
    return sorted(out, key=lambda m: (m.op, tuple(map(str, m.args))))


def apply(p: Position, move: Move) -> Term:
    if move not in candidates(p):
        raise ValueError(f"not a candidate: {move}")
    args = tuple(p.representative_term(a) for a in move.args)
    result = Term(move.op, args)
    p.add_term(result)
    closure(p)
    return result


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
