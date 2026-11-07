#!/usr/bin/env python3
"""Engine review of a student's step list (E-euclid-tutor).

Like a Go engine reviewing a game: for each step of a construction, report
(1) valid or invalid (and why), and (2) progress or no progress towards the
goal (with the numbers, not only the label).

- valid: euclid_engine.apply_step succeeds on the state so far. Invalid: it
  raises MissingPremise (the missing fact is named) or another engine error.
  After an invalid step the remaining steps are reviewed against the state
  WITHOUT the invalid step (later steps may still be fine); the row says so.
- distance d(state): the depth at which a breadth-first search — the same
  move generator, dedup key, goal test and store-consistency gate as
  euclid_search.search — first finds the goal FROM that state, with a small
  budget (default max_depth 4, node budget 5000). None = not found in
  budget. Results are cached per state key (the goal, rules and budget are
  fixed within one review).
- progress: d(after) < d(before). no-progress: d(after) >= d(before).
  unknown: either distance is None. Invalid steps carry no distances.

euclid_engine.py and euclid_search.py are imported read-only by path (the
same importlib shape euclid_certify.py uses); neither is edited here.
"""

import importlib.util
from pathlib import Path

_SCRIPTS = Path(__file__).resolve().parent


def _load(name, filename):
    spec = importlib.util.spec_from_file_location(name, _SCRIPTS / filename)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


srch = _load("euclid_search_review", "euclid_search.py")
eng = srch.eng  # the engine instance euclid_search itself loaded and patched

MAX_DEPTH = 4
NODE_BUDGET = 5000


def initial_state(prop):
    """Root state for a proposition key of srch.prop_searches(): hypotheses
    assumed, extra metric rows added, forward closure run once."""
    kw = srch.prop_searches()[prop]
    s = eng.State()
    for fact, why in kw["hyps"]:
        eng._assume_metric_or_fact(s, fact, why)  # metric hyps add their row
    for coeffs, const, op in (kw.get("extra_la") or []):
        s.la.add(coeffs, const, op)
    eng.close(s)
    return s


class Distance:
    """d(state) with a per-review cache keyed by the search dedup key."""

    def __init__(self, prop, max_depth=MAX_DEPTH, node_budget=NODE_BUDGET,
                 time_budget=None):
        kw = srch.prop_searches()[prop]
        self.rules, self.goal = kw["rules"], kw["goal"]
        self.max_depth, self.node_budget = max_depth, node_budget
        # wall-clock cap per distance call (P-euclid-review-I10-I11): on
        # large states (the I.10/I.11 replays) even a small node budget
        # costs minutes, because each node applies every legal move with
        # full closure. A cut-off returns None = "unknown", never a
        # fabricated distance.
        self.time_budget = time_budget
        self._cache = {}
        self._tainted = set()
        self.errors = []  # (rule, args, repr(exc)) of moves that CRASHED:
        # they are skipped, and a distance computed with skips is at best
        # an over-estimate — an exception here must never read as
        # "no-progress" (None stays None = unknown)

    def __call__(self, state):
        key = srch.state_key(state)
        if key in self._cache:
            return self._cache[key]
        n0 = len(self.errors)
        d = self._search(state)
        self._cache[key] = d
        if len(self.errors) > n0:
            self._tainted.add(key)
        return d

    def tainted(self, state):
        """True if d(state) was computed with crashed moves skipped: the
        value may be too large, so a progress label built on it is unsafe
        (review fix, claude-9)."""
        return srch.state_key(state) in self._tainted

    def _search(self, state):
        """BFS from `state` over legal moves (same generator/dedup/goal/
        consistency gate as srch.search); depth of the first goal state."""
        if self.goal(state):
            return 0
        import time as _time
        t0 = _time.perf_counter()
        seen = {srch.state_key(state)}
        queue = [(state, 0)]
        nodes = 0
        while queue:
            if nodes >= self.node_budget:
                return None
            if self.time_budget and _time.perf_counter() - t0 > \
                    self.time_budget:
                return None
            s, depth = queue.pop(0)
            nodes += 1
            if depth >= self.max_depth:
                continue
            for rname, args in srch.legal_moves(s, self.rules):
                if self.time_budget and _time.perf_counter() - t0 > \
                        self.time_budget:
                    # a single node's expansion can itself take minutes on
                    # a large state (every legal move is applied with full
                    # closure): cut mid-expansion too. None = unknown.
                    return None
                s2 = srch.clone_state(s)
                try:
                    eng.apply_step(s2, 1, rname, args, {})
                except eng.MissingPremise:
                    continue  # premise withdrawn under us: cannot happen
                except eng.FMTooLarge as e:
                    # undecided arithmetic inside the BFS: skip the move,
                    # record it — the distance can only get less reliable
                    # (never silently "no-progress": None means unknown)
                    self.errors.append((rname, args, repr(e)))
                    continue
                except Exception as e:
                    self.errors.append((rname, args, repr(e)))
                    continue
                k = srch.state_key(s2)
                if k in seen:
                    continue
                seen.add(k)
                if self.goal(s2) and srch.store_consistent(s2.la):
                    return depth + 1
                queue.append((s2, depth + 1))
        return None


def _fmt_step(rule, args):
    return f"{rule} {args}"


def _missing_why(premise):
    """Student-facing reason for a MissingPremise. A ("fresh", name)
    premise is a binding clash (P-euclid-binding-clash-2): the step names
    its new object after an existing one that it is not shown to equal.
    Review (claude-9): this read "missing premise: ('fresh', 'b')"."""
    if isinstance(premise, tuple) and len(premise) == 2 \
            and premise[0] == "fresh":
        return (f"names its new object {premise[1]}, but {premise[1]} "
                f"already exists and the new object is not shown to be "
                f"the same; use a new name")
    return f"missing premise: {premise}"


def review(prop, steps, max_depth=MAX_DEPTH, node_budget=NODE_BUDGET,
           time_budget=None):
    """Review `steps` = [(rule, args, bindings?), ...] against proposition
    `prop` (a key of srch.prop_searches()). Returns a report dict.

    Verdicts vs engine failures (review fix, claude-9):
      * MissingPremise IS a verdict: the step is invalid.
      * eng.FMTooLarge means the engine could not DECIDE (undecided
        arithmetic): the row is valid=None / progress="undecided" and is
        counted in undecided_steps, never in invalid_steps.
      * Any other exception is an engine bug, not a judgement on the
        student's step: the row is valid=None / progress="engine-error"
        with the traceback summary, counted in engine_errors, and the
        report is marked incomplete. We emit a row rather than re-raise
        so the tutor still sees the fate of the student's OTHER steps
        (re-raising would discard the whole review); with valid=None the
        formatter prints ENGINE ERROR, so this can never be shown as
        "invalid"."""
    kw = srch.prop_searches()[prop]
    goal = kw["goal"]
    dist = Distance(prop, max_depth, node_budget, time_budget)
    state = initial_state(prop)
    d_before = dist(state)
    rows = []
    n_invalid = 0
    n_undecided = 0
    n_engine_errors = 0
    n_blocked = 0
    unapplied = []  # steps the ENGINE could not apply (error/undecided)

    def blocked_row(i, fmt, reason):
        # review fix (claude-9): once the engine failed to apply an earlier
        # step, the state is not the student's; a later "missing" object or
        # premise may be one that step would have supplied. Not a verdict.
        return {"step": i, "rule": fmt, "valid": None,
                "why": (f"{reason}; may depend on step(s) {unapplied}, "
                        "which the engine could not apply"),
                "progress": "blocked", "d_before": None, "d_after": None,
                "state_note": "step not applied"}

    for i, step in enumerate(steps, start=1):
        rule, args = step[0], step[1]
        bindings = step[2] if len(step) > 2 else {}
        if rule == "by_contra":
            # by_contra is a tactic, not a CONSTRUCTIONS rule: the engine
            # driver run_steps special-cases it, and review must too (the
            # I.10 replay cites it). Its "args" are branch (facts, steps)
            # pairs, not object names, so the unknown-object check below
            # does not apply.
            unknown = []
            fmt = (f"by_contra ({len(step[1])} branch(es) "
                   f"-> {list(step[2]) if len(step) > 2 else []})")
        else:
            pts, lns, crs = srch.object_kinds(state)
            unknown = [x for x in args if x not in pts | lns | crs]
            fmt = _fmt_step(rule, args)
        if unknown and unapplied:
            n_blocked += 1
            rows.append(blocked_row(i, fmt, f"uses {', '.join(unknown)}"))
            continue
        if unknown:  # review fix (claude-9): say the object does not exist,
            # rather than reporting a missing premise that mentions it
            n_invalid += 1
            rows.append({
                "step": i, "rule": fmt,
                "valid": False,
                "why": f"uses {', '.join(unknown)} before it exists",
                "progress": "invalid",
                "d_before": None, "d_after": None,
                "state_note": ("step not applied; later steps are reviewed "
                               "against the state without it"),
            })
            continue
        trial = srch.clone_state(state)
        try:
            if rule == "by_contra":
                eng._by_contra(trial, i, step)
            else:
                eng.apply_step(trial, i, rule, args, bindings)
        except eng.ByContraFailure as e:
            # a by_contra whose branch fails to contradict IS a verdict:
            # the case split does not close. Same shape as MissingPremise.
            why = f"by_contra failed: {e}"
            if unapplied:
                n_blocked += 1
                rows.append(blocked_row(i, fmt, why))
                continue
            n_invalid += 1
            rows.append({
                "step": i, "rule": fmt,
                "valid": False, "why": why,
                "progress": "invalid",
                "d_before": None, "d_after": None,
                "state_note": ("step not applied; later steps are reviewed "
                               "against the state without it"),
            })
            continue
        except eng.MissingPremise as e:
            why = _missing_why(e.premise)
            if unapplied:
                n_blocked += 1
                rows.append(blocked_row(i, fmt, why))
                continue
            n_invalid += 1
            rows.append({
                "step": i, "rule": fmt,
                "valid": False, "why": why,
                "progress": "invalid",
                "d_before": None, "d_after": None,
                "state_note": ("step not applied; later steps are reviewed "
                               "against the state without it"),
            })
            continue  # state unchanged
        except eng.FMTooLarge as e:
            n_undecided += 1
            unapplied.append(i)
            rows.append({
                "step": i, "rule": fmt,
                "valid": None,
                "why": f"engine could not decide: {type(e).__name__}: {e}",
                "progress": "undecided",
                "d_before": None, "d_after": None,
                "state_note": ("step not applied (undecided); later steps "
                               "are reviewed against the state without it"),
            })
            continue
        except Exception as e:  # engine bug: NOT a verdict on the step
            import traceback
            n_engine_errors += 1
            unapplied.append(i)
            rows.append({
                "step": i, "rule": fmt,
                "valid": None,
                "why": (f"engine error: {type(e).__name__}: {e} — "
                        + traceback.format_exc().strip().splitlines()[-1]),
                "progress": "engine-error",
                "d_before": None, "d_after": None,
                "state_note": ("step not applied (engine error); later "
                               "steps are reviewed against the state "
                               "without it"),
            })
            continue
        d_after = dist(trial)
        if d_before is None or d_after is None:
            label = "unknown"
        elif dist.tainted(state) or dist.tainted(trial):
            # a skipped (crashed) move can only inflate a distance, so
            # neither "progress" nor "no-progress" can be trusted here
            label = "unknown"
        elif d_after < d_before:
            label = "progress"
        else:
            label = "no-progress"
        rows.append({
            "step": i, "rule": fmt,
            "valid": True, "why": "apply_step succeeded",
            "progress": label,
            "d_before": d_before, "d_after": d_after,
        })
        state, d_before = trial, d_after
    goal_held = goal(state) and srch.store_consistent(state.la)
    return {
        "prop": prop,
        "steps": rows,
        "invalid_steps": n_invalid,
        "undecided_steps": n_undecided,
        "engine_errors": n_engine_errors,
        "blocked_steps": n_blocked,
        "incomplete": bool(n_undecided or n_engine_errors or n_blocked
                           or dist.errors),
        "distance_move_errors": len(dist.errors),
        "goal_held": bool(goal_held),
        "remaining_distance": d_before,
        "max_depth": max_depth,
        "node_budget": node_budget,
    }


def format_report(rep):
    lines = [f"review {rep['prop']}  (d-budget: depth {rep['max_depth']}, "
             f"{rep['node_budget']} nodes)"]
    for r in rep["steps"]:
        verdict = {True: "VALID", False: "INVALID", None: None}
        label = verdict[r["valid"]] or (
            {"undecided": "UNDECIDED", "blocked": "NOT REVIEWED"}.get(
                r["progress"], "ENGINE ERROR"))
        lines.append(
            f"  {r['step']}. {r['rule']}: "
            f"{label} "
            f"({r['why']}) | {r['progress']}"
            + (f" d {r['d_before']}->{r['d_after']}"
               if r["d_before"] is not None or r["d_after"] is not None
               else ""))
        if r["valid"] is not True:
            lines.append(f"     ({r['state_note']})")
    lines.append(f"goal {'HOLDS' if rep['goal_held'] else 'NOT reached'}; "
                 f"remaining distance {rep['remaining_distance']}; "
                 f"{rep['invalid_steps']} invalid step(s)")
    if rep.get("incomplete"):
        lines.append(
            "REVIEW INCOMPLETE: "
            + "; ".join(x for x in (
                f"{rep['undecided_steps']} undecided step(s)"
                if rep.get("undecided_steps") else None,
                f"{rep['engine_errors']} engine error(s)"
                if rep.get("engine_errors") else None,
                f"{rep['blocked_steps']} later step(s) not reviewed"
                if rep.get("blocked_steps") else None,
                f"{rep.get('distance_move_errors', 0)} distance move(s) "
                f"skipped after engine errors"
                if rep.get("distance_move_errors") else None,
            ) if x))
    return "\n".join(lines)


# --- demo fixtures -----------------------------------------------------------

def fixture_book_i1():
    """The Book's I.1 steps (engine PROP1_STEPS_DATA)."""
    return [("circle_from_points", ("a", "b"), {"C": "BCD"}),
            ("circle_from_points", ("b", "a"), {"C": "ACE"}),
            ("intersection_circles", ("BCD", "ACE"), {"c": "c"})]


def fixture_i1_useless_step():
    """I.1 with a redundant line_from_points a b inserted after the first
    circle (valid but useless)."""
    return [("circle_from_points", ("a", "b"), {"C": "BCD"}),
            ("line_from_points", ("a", "b"), {"L": "AB"}),
            ("circle_from_points", ("b", "a"), {"C": "ACE"}),
            ("intersection_circles", ("BCD", "ACE"), {"c": "c"})]


def fixture_i1_diagram_trusting():
    """I.1 trusting the diagram: intersection_circles before the second
    circle exists."""
    return [("circle_from_points", ("a", "b"), {"C": "BCD"}),
            ("intersection_circles", ("BCD", "ACE"), {"c": "c"}),
            ("circle_from_points", ("b", "a"), {"C": "ACE"}),
            ("intersection_circles", ("BCD", "ACE"), {"c": "c2"})]


def fixture_i3_prop2_omitted():
    """I.3 with proposition_2' omitted: the circle at a through the point d
    that was never constructed (engine PROP3_STEPS_DATA minus step 1)."""
    return [("circle_from_points", ("a", "d"), {"C": "DEF"}),
            ("intersection_circle_line_between_points",
             ("DEF", "AB", "a", "b"), {"e": "e"})]


FIXTURES = {
    "book-i1": fixture_book_i1,
    "i1-useless-step": fixture_i1_useless_step,
    "i1-diagram-trusting": fixture_i1_diagram_trusting,
    "i3-prop2-omitted": fixture_i3_prop2_omitted,
}
FIXTURE_PROP = {"book-i1": "I.1", "i1-useless-step": "I.1",
                "i1-diagram-trusting": "I.1", "i3-prop2-omitted": "I.3"}


def main():
    import sys
    names = sys.argv[1:] or list(FIXTURES)
    for name in names:
        rep = review(FIXTURE_PROP[name], FIXTURES[name]())
        print(format_report(rep))
        print()


if __name__ == "__main__":
    main()
