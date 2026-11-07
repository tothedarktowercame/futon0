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
        s.assume(fact, why)
    for coeffs, const, op in (kw.get("extra_la") or []):
        s.la.add(coeffs, const, op)
    eng.close(s)
    return s


class Distance:
    """d(state) with a per-review cache keyed by the search dedup key."""

    def __init__(self, prop, max_depth=MAX_DEPTH, node_budget=NODE_BUDGET):
        kw = srch.prop_searches()[prop]
        self.rules, self.goal = kw["rules"], kw["goal"]
        self.max_depth, self.node_budget = max_depth, node_budget
        self._cache = {}

    def __call__(self, state):
        key = srch.state_key(state)
        if key in self._cache:
            return self._cache[key]
        d = self._search(state)
        self._cache[key] = d
        return d

    def _search(self, state):
        """BFS from `state` over legal moves (same generator/dedup/goal/
        consistency gate as srch.search); depth of the first goal state."""
        if self.goal(state):
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
            for rname, args in srch.legal_moves(s, self.rules):
                s2 = srch.clone_state(s)
                try:
                    eng.apply_step(s2, 1, rname, args, {})
                except eng.MissingPremise:
                    continue  # premise withdrawn under us: cannot happen
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


def review(prop, steps, max_depth=MAX_DEPTH, node_budget=NODE_BUDGET):
    """Review `steps` = [(rule, args, bindings?), ...] against proposition
    `prop` (a key of srch.prop_searches()). Returns a report dict."""
    kw = srch.prop_searches()[prop]
    goal = kw["goal"]
    dist = Distance(prop, max_depth, node_budget)
    state = initial_state(prop)
    d_before = dist(state)
    rows = []
    n_invalid = 0
    for i, step in enumerate(steps, start=1):
        rule, args = step[0], step[1]
        bindings = step[2] if len(step) > 2 else {}
        pts, lns, crs = srch.object_kinds(state)
        unknown = [x for x in args if x not in pts | lns | crs]
        if unknown:  # review fix (claude-9): say the object does not exist,
            # rather than reporting a missing premise that mentions it
            n_invalid += 1
            rows.append({
                "step": i, "rule": _fmt_step(rule, args),
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
            eng.apply_step(trial, i, rule, args, bindings)
        except eng.MissingPremise as e:
            n_invalid += 1
            rows.append({
                "step": i, "rule": _fmt_step(rule, args),
                "valid": False, "why": f"missing premise: {e.premise}",
                "progress": "invalid",
                "d_before": None, "d_after": None,
                "state_note": ("step not applied; later steps are reviewed "
                               "against the state without it"),
            })
            continue  # state unchanged
        except Exception as e:  # other engine errors are invalid too
            n_invalid += 1
            rows.append({
                "step": i, "rule": _fmt_step(rule, args),
                "valid": False, "why": f"engine error: {type(e).__name__}: {e}",
                "progress": "invalid",
                "d_before": None, "d_after": None,
                "state_note": ("step not applied; later steps are reviewed "
                               "against the state without it"),
            })
            continue
        d_after = dist(trial)
        if d_before is None or d_after is None:
            label = "unknown"
        elif d_after < d_before:
            label = "progress"
        else:
            label = "no-progress"
        rows.append({
            "step": i, "rule": _fmt_step(rule, args),
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
        "goal_held": bool(goal_held),
        "remaining_distance": d_before,
        "max_depth": max_depth,
        "node_budget": node_budget,
    }


def format_report(rep):
    lines = [f"review {rep['prop']}  (d-budget: depth {rep['max_depth']}, "
             f"{rep['node_budget']} nodes)"]
    for r in rep["steps"]:
        lines.append(
            f"  {r['step']}. {r['rule']}: "
            f"{'VALID' if r['valid'] else 'INVALID'} "
            f"({r['why']}) | {r['progress']}"
            + (f" d {r['d_before']}->{r['d_after']}"
               if r["d_before"] is not None or r["d_after"] is not None
               else ""))
        if not r["valid"]:
            lines.append(f"     ({r['state_note']})")
    lines.append(f"goal {'HOLDS' if rep['goal_held'] else 'NOT reached'}; "
                 f"remaining distance {rep['remaining_distance']}; "
                 f"{rep['invalid_steps']} invalid step(s)")
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
