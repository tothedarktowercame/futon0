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
import math
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
                 node_budget=3000, eps=0.05):
        self.name = name
        self.removed_rules = frozenset(removed_rules)
        self.max_depth = max_depth
        self.node_budget = node_budget
        self.eps = eps  # slice-3 field: mass a wrong move gets (not a literal)
        self.hits = 0
        self.observations = 0
        self.loglik = 0.0  # cumulative log-likelihood of the observed steps

    def rules(self, prop):
        return [r for r in srch.prop_searches()[prop]["rules"]
                if r not in self.removed_rules]

    def legal_set(self, prop, state):
        """Canonical set of moves legal for this clone at `state` (base:
        euclid_search.legal_moves under the clone's rules)."""
        return {canon_step(r, a)
                for r, a in srch.legal_moves(state, self.rules(prop))}

    def legal_count(self, prop, state):
        """Number of moves legal for this clone at `state`."""
        return len(self.legal_set(prop, state))

    def probability(self, prop, state, canonical):
        """Probability this clone assigns to the observed (canonical) step.
        In its non-empty predicted set P: (1-eps)/|P|. Otherwise
        eps/(|legal|-|P|) — the clone thought something else was right.
        Empty P: 1/|legal| if the step is one of the clone's legal moves
        (it cannot see any path and is guessing uniformly over the moves it
        could make); if the step is not even in its action space the clone
        cannot produce it at all and gets eps/|legal| — the same
        smoothing a miss gets. (Literal 1/|legal| for every step, legal or
        not, would let a technique-less clone outrank an informed hit —
        measured on the I.2 fixture, where no-circle_from_points would
        win with posterior 0.71 while the trusting clone scored 0.06.)"""
        P = self.predict(prop, state)
        L = self.legal_count(prop, state)
        if not P:
            if not L:
                return 0.0
            if canonical in self.legal_set(prop, state):
                return 1.0 / L
            return self.eps / L
        if canonical in P:
            return (1 - self.eps) / len(P)
        return self.eps / max(L - len(P), 1)

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
    """The set of clones still consistent with the student's play.

    Scoring (slice 3): each clone gives the observed step a probability
    (see Clone.probability); the envelope keeps a cumulative log-likelihood
    per clone and a normalised posterior under a uniform prior. A clone is
    RULED OUT when its posterior falls below `threshold` (recorded, never
    deleted); if every clone falls below, all are kept and the event says
    so — silent emptying is not allowed. hits/observations stay as a
    reported statistic next to the posterior."""

    def __init__(self, clones, threshold=0.01):
        self.clones = list(clones)
        self.active = list(self.clones)
        self.threshold = threshold
        self.events = []  # one record per observation, never pruned

    def _posterior(self):
        """Uniform prior over clones, likelihood = exp(cumulative loglik).
        All-zero likelihood (e.g. eps=0 misses) yields all-zero posteriors;
        the caller keeps every clone in that case."""
        ws = [math.exp(c.loglik) if c.loglik != float("-inf") else 0.0
              for c in self.clones]
        total = sum(ws)
        if total <= 0.0:
            return {c.name: 0.0 for c in self.clones}
        return {c.name: w / total for c, w in zip(self.clones, ws)}

    def observe(self, prop, state, step):
        """Observe the student's `step` = (rule, args[, bindings]) at
        `state`. Scores every clone, updates posteriors, records clones
        below threshold. If every clone is below threshold (or every
        likelihood is zero), ALL clones stay active and the event says so."""
        rule, args = step[0], step[1]
        canonical = canon_step(rule, args)
        probabilities = {}
        predicted_by, missed_prediction = [], []
        for c in self.clones:
            pred = c.predict(prop, state)
            c.observations += 1
            p = c.probability(prop, state, canonical)
            probabilities[c.name] = p
            c.loglik += math.log(p) if p > 0 else float("-inf")
            if canonical in pred:
                c.hits += 1
                predicted_by.append(c.name)
            else:
                missed_prediction.append(c.name)
        posterior = self._posterior()
        below = [c.name for c in self.clones
                 if posterior[c.name] < self.threshold]
        keep = [c for c in self.active if c.name not in below]
        all_below = bool(self.active) and not keep
        if all_below:
            keep = list(self.active)  # silent emptying is not allowed
        self.active = keep
        self.events.append({
            "prop": prop,
            "step": f"{rule} {tuple(args)}",
            "probabilities": probabilities,
            "posterior": posterior,
            "predicted_by": sorted(predicted_by),
            "missed_prediction": sorted(missed_prediction),
            "below_threshold": sorted(below),
            "no_clone_predicted_this_step": not predicted_by,
            "all_below_threshold": all_below,
        })
        # 'ruled_out' kept as an alias for the below-threshold record
        self.events[-1]["ruled_out"] = sorted(below)
        return self.events[-1]

    def accuracy(self):
        """Per clone, hits / observations (None before any observation)."""
        return {c.name: (c.hits / c.observations if c.observations else None)
                for c in self.clones}


def standard_envelope(eps=0.05):
    """The starting envelope for I.1-I.3: the full engine; a student who
    has not got circles; one without intersection_circles; and a student
    who looks only one move ahead."""
    return Envelope([
        Clone("full-engine", eps=eps),
        Clone("no-circle_from_points", removed_rules={"circle_from_points"},
              eps=eps),
        Clone("no-intersection_circles",
              removed_rules={"intersection_circles"}, eps=eps),
        Clone("depth-1", max_depth=1, eps=eps),
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
              f"below_threshold={ev['below_threshold']}")
        print(f"    posterior={ev['posterior']}")
        trial = srch.clone_state(state)
        eng.apply_step(trial, 1, step[0], step[1], step[2])
        state = trial
    print("\naccuracy:", env.accuracy())
    print("active:", [c.name for c in env.active])


if __name__ == "__main__":
    main()
