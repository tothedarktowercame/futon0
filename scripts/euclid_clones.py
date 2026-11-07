#!/usr/bin/env python3
"""Clone envelope v0 for E-euclid-tutor: predict + prune.

A Clone is the proof engine with some techniques removed and a search-depth
limit: predict(prop, state) returns the set of first moves (rule, canonical
args) over ALL shortest goal paths the clone's own search finds from `state`
(its rules = the proposition's rules minus `removed_rules`, budget
`max_depth`/`node_budget`). Fresh names in args are replaced by their role
(the construction base name), so clones that minted different fresh objects
still agree. Empty set = the clone finds no path within its budget.

An Envelope is the set of clones still consistent with what the student has
done. observe(prop, state, step) scores every clone (hits / observations for
a prediction-accuracy score), keeps only clones whose predict() contained
the step, and RECORDS the ruled-out ones (they keep being scored). If every
clone is ruled out, all clones are kept and the event is recorded with
"no clone predicted this step" — silent emptying is not allowed.

euclid_engine / euclid_search / euclid_review are imported read-only by
path (the euclid_review.py importlib shape); none are edited here. Nothing
here reads the Book step data — prediction is search-only (tested).
"""

import importlib.util
from pathlib import Path

_SCRIPTS = Path(__file__).resolve().parent


def _load(name, filename):
    spec = importlib.util.spec_from_file_location(name, _SCRIPTS / filename)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


rv = _load("euclid_review_clone", "euclid_review.py")
srch, eng = rv.srch, rv.eng


def _canon(x):
    """Fresh object name -> its role (construction base name).

    apply_step mints fresh names as f"{base}${n}"; the part before '$' is
    the role, so two clones that minted different fresh objects agree.
    """
    if isinstance(x, str) and "$" in x:
        return x.split("$", 1)[0]
    return x


def canon_step(rule, args):
    if rule == "intersection_circles":
        # symmetric rule: (alpha, beta) and (beta, alpha) are the same move
        # (r_icc_symm derives the premise both ways); canonicalize the order
        args = tuple(sorted(args))
    return (rule, tuple(_canon(a) for a in args))


class Clone:
    """The engine minus `removed_rules`, searching to `max_depth`."""

    def __init__(self, name, removed_rules=frozenset(), max_depth=3,
                 node_budget=3000):
        self.name = name
        self.removed_rules = frozenset(removed_rules)
        self.max_depth = max_depth
        self.node_budget = node_budget
        self.hits = 0
        self.observations = 0

    def rules(self, prop):
        return [r for r in srch.prop_searches()[prop]["rules"]
                if r not in self.removed_rules]

    def _goal(self, prop):
        return srch.prop_searches()[prop]["goal"]

    def distance(self, prop, state):
        """Shortest depth to the goal within the clone's budget, or None."""
        rules, goal = self.rules(prop), self._goal(prop)
        if goal(state):
            return 0
        seen = {srch.state_key(state)}
        queue = [(state, 0)]
        nodes = 0
        while queue:
            if nodes >= self.node_budget:
                return None
            s, depth = queue.pop(0)
            nodes += 1
            if depth >= self.max_depth:
                continue
            for rname, args in srch.legal_moves(s, rules):
                s2 = srch.clone_state(s)
                try:
                    eng.apply_step(s2, 1, rname, args, {})
                except eng.MissingPremise:
                    continue
                k = srch.state_key(s2)
                if k in seen:
                    continue
                seen.add(k)
                if goal(s2) and srch.store_consistent(s2.la):
                    return depth + 1
                queue.append((s2, depth + 1))
        return None

    def predict(self, prop, state):
        """Set of canonical first moves over all shortest goal paths the
        clone finds; empty if it finds no path (or the goal already holds).
        The search returns one path, so all shortest first moves are
        enumerated by trying each legal first move and searching the rest."""
        rules, goal = self.rules(prop), self._goal(prop)
        d = self.distance(prop, state)
        if not d:  # None (no path in budget) or 0 (goal already holds)
            return set()
        moves = set()
        for rname, args in srch.legal_moves(state, rules):
            s2 = srch.clone_state(state)
            try:
                eng.apply_step(s2, 1, rname, args, {})
            except eng.MissingPremise:
                continue
            if d == 1:
                ok = goal(s2) and srch.store_consistent(s2.la)
            else:
                # the rest of the shortest path must fit in d-1 more moves
                sub = Clone(self.name, self.removed_rules, d - 1,
                            self.node_budget)
                ok = sub.distance(prop, s2) == d - 1
            if ok:
                moves.add(canon_step(rname, args))
        return moves


class Envelope:
    """The set of clones still consistent with the student's play."""

    def __init__(self, clones):
        self.clones = list(clones)
        self.active = list(self.clones)
        self.events = []  # one record per observation, never pruned

    def observe(self, prop, state, step):
        """Observe the student's `step` = (rule, args[, bindings]) at
        `state`. Scores every clone, prunes the active set to the clones
        that predicted it, records the ruled-out ones. If every clone is
        ruled out, ALL clones stay active and the event says so."""
        rule, args = step[0], step[1]
        canonical = canon_step(rule, args)
        predicted, missed = {}, {}
        for c in self.clones:
            pred = c.predict(prop, state)
            c.observations += 1
            if canonical in pred:
                c.hits += 1
                predicted[c.name] = sorted(map(str, pred))
            else:
                missed[c.name] = sorted(map(str, pred))
        keep = [c for c in self.active if c.name in predicted]
        no_clone = bool(self.active) and not keep
        if no_clone:
            keep = list(self.active)  # silent emptying is not allowed
        self.active = keep
        self.events.append({
            "prop": prop,
            "step": f"{rule} {tuple(args)}",
            "predicted_by": sorted(predicted),
            "ruled_out": sorted(missed),
            "no_clone_predicted_this_step": no_clone,
        })
        return self.events[-1]

    def accuracy(self):
        """Per clone, hits / observations (None before any observation)."""
        return {c.name: (c.hits / c.observations if c.observations else None)
                for c in self.clones}


def standard_envelope():
    """The starting envelope for I.1-I.3: the full engine; a student who
    has not got circles; one without intersection_circles; and a student
    who looks only one move ahead."""
    return Envelope([
        Clone("full-engine"),
        Clone("no-circle_from_points", removed_rules={"circle_from_points"}),
        Clone("no-intersection_circles",
              removed_rules={"intersection_circles"}),
        Clone("depth-1", max_depth=1),
    ])


def main():
    env = standard_envelope()
    state = rv.initial_state("I.1")
    print("clone            root-predictions (I.1)")
    for c in env.clones:
        print(f"  {c.name:24s} {sorted(map(str, c.predict('I.1', state)))}")
    print("\nplaying the Book's I.1:")
    for step in rv.fixture_book_i1():
        ev = env.observe("I.1", state, step)
        print(f"  {ev['step']}: predicted_by={ev['predicted_by']} "
              f"ruled_out={ev['ruled_out']}")
        trial = srch.clone_state(state)
        eng.apply_step(trial, 1, step[0], step[1], step[2])
        state = trial
    print("\naccuracy:", env.accuracy())
    print("active:", [c.name for c in env.active])


if __name__ == "__main__":
    main()
