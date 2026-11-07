#!/usr/bin/env python3
"""Diagram-trusting clone for E-euclid-tutor (student model slice 2).

A TrustingClone is a Clone that believes extra things: a student who reads
facts off the picture. apply_step works like the engine's, but on a
MissingPremise whose premise kind is trusted (default: intersects_cc,
between, same_side, inside) AND which holds in the picture, it assumes the
premise and retries, recording "trusted from the picture: <fact>". A
premise false in the picture is still refused (the student believes the
picture, not anything at all), and untrusted premise kinds are refused.

The picture is numeric placement from euclid_trace (objects_from_hyps,
place_step) — no new geometry: between/opposing_sides are decided by
trace.fact_holds, circle-circle meeting by trace.circle_circle, and
same_side/inside by trace's own vector helpers on the same coordinates.

predict is deliberately simple (per the slice spec): it returns the set of
moves LEGAL for this clone — engine-legal moves plus candidate moves whose
only obstacle is a trusted, picture-true premise — with args canonicalized
by euclid_clones.canon_step. Unlike Clone.predict it is not filtered to
shortest-path first moves. TrustingClone is an ordinary member of
euclid_clones.Envelope; its picture is advanced with note_step after each
observed step (the envelope itself does not track placements).

euclid_engine / euclid_search / euclid_review / euclid_clones /
euclid_trace are imported read-only by path; none are edited here.
"""

import importlib.util
from pathlib import Path

_SCRIPTS = Path(__file__).resolve().parent


def _load(name, filename):
    spec = importlib.util.spec_from_file_location(name, _SCRIPTS / filename)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


ec = _load("euclid_clones_trust", "euclid_clones.py")
tr = _load("euclid_trace_trust", "euclid_trace.py")
rv, srch, eng = ec.rv, ec.srch, ec.eng

DEFAULT_TRUSTED = frozenset({"intersects_cc", "between", "same_side",
                             "inside"})


class TrustingClone(ec.Clone):
    """A Clone that also believes trusted facts read off the picture."""

    def __init__(self, name, prop, xy=None, objects=None,
                 trusted_kinds=DEFAULT_TRUSTED, removed_rules=frozenset(),
                 max_depth=3, node_budget=3000):
        super().__init__(name, removed_rules=removed_rules,
                         max_depth=max_depth, node_budget=node_budget)
        self.prop = prop
        self.prop_n = self._prop_number(prop)
        self.trusted_kinds = frozenset(trusted_kinds)
        # picture: euclid_trace placement, defaulting to the traditional
        # diagram for the proposition (FREE points + hypothesis objects).
        # Fail LOUDLY here if euclid_trace has no placement support for the
        # proposition (review, claude-9: a silent fallback meant the picture
        # never advanced for I.4 onward).
        if xy is not None:
            self.xy = dict(xy)
        else:
            if self.prop_n not in tr.FREE:
                raise ValueError(
                    "euclid_trace has no placement (FREE) for %s" % prop)
            self.xy = dict(tr.FREE[self.prop_n])
        if objects is not None:
            self.objects = list(objects)
        else:
            try:
                self.objects = tr.objects_from_hyps(self.prop_n)
            except Exception as exc:
                raise ValueError(
                    "euclid_trace has no hypothesis objects for %s: %r"
                    % (prop, exc))
        # derived hypothesis points (I.4's e/f, I.5's d/e): placed by
        # euclid_trace at build_trace time; the default picture must match
        # or place_step would KeyError on them (seen as a picture error)
        if xy is None and objects is None:
            try:
                tr.derived_free_points(self.prop_n, self.xy)
            except KeyError as exc:
                raise ValueError(
                    "euclid_trace cannot place the hypothesis points "
                    "for %s: %r" % (prop, exc))
        self.trust_log = []  # "trusted from the picture: <fact>" records
        self.picture_errors = []  # (rule, args, repr(exc)) from place_step
        self.picture_stale = False

    @staticmethod
    def _prop_number(prop):
        if not (isinstance(prop, str) and prop.startswith("I.")):
            raise ValueError("prop must be 'I.n', got %r" % (prop,))
        try:
            return int(prop[2:])
        except ValueError:
            raise ValueError("prop must be 'I.n', got %r" % (prop,))

    # --- picture checks (trace primitives, no new geometry) ---------------

    def _circle_geo(self, C):
        o = next(o for o in self.objects
                 if o["name"] == C and o["kind"] == "circle")
        centre = o["jsx"]["center"]
        through = o["jsx"]["through"]
        r = tr._norm(tr._sub(self.xy[through], self.xy[centre]))
        return self.xy[centre], r

    def _line_geo(self, L):
        o = next(o for o in self.objects
                 if o["name"] == L and o["kind"] == "line"
                 and o.get("jsx", {}).get("p"))
        p1, p2 = o["jsx"]["p"]
        return self.xy[p1], self.xy[p2]

    def holds_in_picture(self, fact):
        """True / False for the trusted diagram kinds, using euclid_trace's
        placement and geometry; None when the fact is not a picture fact.
        Review fixes (claude-9):
          * while the picture is STALE (a place_step failure was recorded),
            no picture fact is trusted: None (no trust), never False/True
            asserted from a lagging picture;
          * a missing point/object is None ("not in the picture"), not
            False ("false in the picture")."""
        if self.picture_stale:
            return None
        kind = fact[0]
        if kind in ("between", "opposing_sides"):
            try:
                return tr.fact_holds(fact, self.xy) is True
            except KeyError:
                return None
        if kind == "intersects_cc":
            try:
                (c1, r1), (c2, r2) = (self._circle_geo(fact[1]),
                                      self._circle_geo(fact[2]))
            except (StopIteration, KeyError):
                return None  # not in the picture
            try:
                tr.circle_circle(c1, r1, c2, r2)
                return True
            except AssertionError:
                return False  # genuinely non-meeting circles
        if kind == "same_side":
            p, q, L = fact[1], fact[2], fact[3]
            try:
                (x1, y1), (x2, y2) = self._line_geo(L)

                def side(pt):
                    return ((pt[0] - x1) * (y2 - y1)
                            - (pt[1] - y1) * (x2 - x1))
                sp, sq = side(self.xy[p]), side(self.xy[q])
            except (StopIteration, KeyError):
                return None  # not in the picture
            return sp * sq > 0
        if kind == "inside":
            try:
                (c, r) = self._circle_geo(fact[2])
                d = tr._norm(tr._sub(self.xy[fact[1]], c))
            except (StopIteration, KeyError):
                return None  # not in the picture
            return d < r
        return None

    # --- trusting application ---------------------------------------------

    def apply_step(self, state, i, rule, args, bindings,
                   advance_picture=True):
        """eng.apply_step, but a MissingPremise of a trusted kind that
        holds in the picture is assumed (and recorded) and the step
        retried. Anything else still raises."""
        # Review fix (claude-9): work on a copy, so a step that is refused
        # after some premises were trusted leaves neither the trusted facts
        # in the caller's state nor entries in trust_log.
        work = srch.clone_state(state)
        pending = []
        while True:
            try:
                rep = eng.apply_step(work, i, rule, args, bindings)
                break
            except eng.MissingPremise as e:
                p = e.premise
                if (isinstance(p, tuple) and p and isinstance(p[0], str)
                        and p[0] in self.trusted_kinds
                        and self.holds_in_picture(p) is True):
                    work.assume(p, "student diagram trust")
                    pending.append(f"trusted from the picture: {p}")
                else:
                    raise
        state.__dict__.update(work.__dict__)
        self.trust_log.extend(pending)
        if advance_picture:
            try:
                tr.place_step(self.prop_n, rule, args, bindings,
                              rep["added"], self.xy, self.objects)
            except Exception as exc:
                # Review fix (claude-9): never hide a picture failure. The
                # engine step already succeeded and is valid — record the
                # placement failure and mark the picture stale; while stale,
                # holds_in_picture answers None (no trust) so the clone
                # refuses trusted premises for a reason anyone can see.
                self.picture_errors.append((rule, args, repr(exc)))
                self.picture_stale = True
        return rep

    def note_step(self, prop, state, step):
        """Advance the clone's picture past an observed `step` (the engine
        side is applied to a scratch copy of `state`)."""
        assert prop == self.prop
        trial = srch.clone_state(state)
        return self.apply_step(trial, 1, step[0], step[1], step[2],
                               advance_picture=True)

    # --- prediction ---------------------------------------------------------

    def predict(self, prop, state):
        """Legal moves FOR THIS CLONE: every candidate move of its rules
        whose application succeeds outright or after trusting a
        picture-true premise of a trusted kind. Not shortest-path
        filtered (slice spec: keep it simple)."""
        assert prop == self.prop
        moves = set()
        saved_log = list(self.trust_log)  # predictions are hypothetical
        for rname in self.rules(prop):
            for args in srch.CANDIDATES[rname](state):
                trial = srch.clone_state(state)
                try:
                    self.apply_step(trial, 1, rname, args, {},
                                    advance_picture=False)
                except eng.MissingPremise:
                    continue
                moves.add(ec.canon_step(rname, args))
        self.trust_log = saved_log
        return moves

    def legal_set(self, prop, state):
        """The trusting clone's legal set IS its predicted set (moves that
        apply outright or after trusting a picture-true premise)."""
        return self.predict(prop, state)

    def legal_count(self, prop, state):
        return len(self.legal_set(prop, state))


def main():
    # demo: the I.2 fixture — full engine refuses, trusting clone accepts
    steps = [("circle_from_points", ("b", "c"), {"C": "CGH"}),
             ("circle_from_points", ("a", "b"), {"C": "ABX"}),
             ("intersection_circles", ("CGH", "ABX"), {"c": "p"})]
    tc = TrustingClone("trusting", "I.2")
    state = rv.initial_state("I.2")
    for i, step in enumerate(steps, start=1):
        try:
            eng.apply_step(srch.clone_state(state), i, *step[:2],
                           step[2])
            print(f"  {i}. {step[0]} {step[1]}: engine OK")
        except eng.MissingPremise as e:
            print(f"  {i}. {step[0]} {step[1]}: engine refuses ({e})")
        rep = tc.apply_step(state, i, step[0], step[1], step[2])
        print(f"     trusting: applied, added {len(rep['added'])} facts")
    print("trust log:", tc.trust_log)


if __name__ == "__main__":
    main()
