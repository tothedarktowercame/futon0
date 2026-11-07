#!/usr/bin/env python3
"""Engine review of a student's step list (E-euclid-tutor).

Like a Go engine reviewing a game: for each step of a construction, report
(1) valid or invalid (and why), and (2) progress or no progress towards the
goal (with the numbers, not only the label).

- valid: euclid_engine.apply_step succeeds on the state so far — directly,
  or after a fact-only repair (depth ≤ 2, bindings {}) standing in for
  Lean's SMT; repaired rows are valid with progress "valid-needs-smt" and
  their why names the helper step(s). Invalid: apply_step raises
  MissingPremise (the missing fact is named) that no repair closes, or
  another engine error.
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
        # review fix (claude-9): with no explicit budget, use the prop's own
        # (I.10/I.11: 5 s). Unbudgeted, review("I.10", steps[:3]) took 361 s.
        self.time_budget = (time_budget if time_budget is not None
                            else kw.get("time_budget"))
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


# --- fact-only premise repair (P-euclid-review-smt-repair) ------------------
#
# Lean's SMT closes small FACT premises of cited rules (e.g. w ≠ u from
# between u w v in I.11) that the engine only derives by citing a forward
# rule explicitly. When a reviewed step fails with a missing FACT premise,
# search for a short sequence of FACT-ONLY engine moves (depth ≤ 2, seconds
# not minutes) after which the step applies; if found, the step is valid
# with progress label "valid-needs-smt" and the state carries the helper
# facts forward — exactly what Lean's state has after its SMT step.
#
# Fact-only means the rule's make introduces no fresh object (every make
# entry has base None). The candidate move set (from CONSTRUCTIONS):
#   between_symm_parts          neq / not_between from a between
#   intersection_lines_opposing intersects_ll from opposing_sides
#   intersection_circle_line_1  intersects_lc from opposing_sides
#   pasch_4                     between from opposing_sides
#   perpendicular_if            right_ang
#   sum_angles_onlyif           sum_ang
#   angle_pair_on_line          eq_ang along a line
#   assert_point_eq_on_ray      peq
#   assert_point_eq_angle       peq
# Excluded: proposition_7 derives ("false",) — repairing by explosion is
# not what Lean's SMT does; proposition_4/5/5'/8 are fact-only but metric
# (8–12 arguments, congruence value) — no cheap candidate set, and the
# observed SMT gaps are diagrammatic. A repair NEVER creates an object
# (bindings are {} throughout), so a step whose gap is a missing object
# stays invalid.
REPAIR_RULES = [
    "between_symm_parts",
    "intersection_lines_opposing",
    "intersection_circle_line_1",
    "pasch_4",
    "perpendicular_if",
    "sum_angles_onlyif",
    "angle_pair_on_line",
    "assert_point_eq_on_ray",
    "assert_point_eq_angle",
]

REPAIR_MAX_DEPTH = 2
REPAIR_NODE_BUDGET = 200
REPAIR_TIME_BUDGET = 2.0


def _is_fact_premise(p):
    """A MissingPremise premise that repair may supply: a fact tuple, not
    a ("fresh", name) binding clash, not an ("entail", ...) arithmetic
    failure, not the "(unknown rule)" string."""
    return (isinstance(p, tuple) and len(p) >= 2
            and isinstance(p[0], str) and p[0] not in ("fresh", "entail"))


def _on_line_index(s):
    on = {}
    for f in s.facts:
        if f[0] == "on_line":
            on.setdefault(f[2], set()).add(f[1])
    return on


def _repair_candidates(s, rname):
    """Rough candidate argument tuples for a fact-only repair rule, drawn
    from facts already in the state (premise filtering happens in
    repair_premise, mirroring srch.legal_moves)."""
    pts, lns, crs = srch.object_kinds(s)
    facts = sorted(s.facts)
    if rname == "between_symm_parts":
        for f in facts:
            if f[0] == "between":
                yield (f[1], f[2], f[3])
    elif rname == "intersection_lines_opposing":
        for f in facts:
            if f[0] != "opposing_sides":
                continue
            _, a, b, L = f
            for M in sorted(lns):
                if s.has(("on_line", a, M)) and s.has(("on_line", b, M)):
                    yield (a, b, L, M)
    elif rname == "intersection_circle_line_1":
        for f in facts:
            if f[0] != "opposing_sides":
                continue
            _, a, b, L = f
            for al in sorted(crs):
                if s.has(("not_outside", a, al)) and \
                        s.has(("not_outside", b, al)):
                    yield (a, b, al, L)
    elif rname == "pasch_4":
        on = _on_line_index(s)
        for f in facts:
            if f[0] != "opposing_sides":
                continue
            _, a, c, L = f
            for M in sorted(lns):
                if M == L or not s.has(("dpl", a, c, M)):
                    continue
                for b in sorted(on.get(L, ()) & on.get(M, ())):
                    yield (a, b, c, L, M)
    elif rname == "perpendicular_if":
        for f in facts:
            if f[0] != "between":
                continue
            _, a, c, b = f
            for L in sorted(lns):
                if not (s.has(("on_line", a, L))
                        and s.has(("on_line", b, L))):
                    continue
                for d in sorted(pts):
                    if s.has(("not_on_line", d, L)):
                        yield (a, b, c, d, L)
    elif rname == "sum_angles_onlyif":
        on = _on_line_index(s)
        ss = [f for f in facts if f[0] == "same_side"]
        for _, b, d, M in ss:
            for _, c, d2, L in ss:
                if d2 != d or L == M or not s.has(("neq_line", L, M)):
                    continue
                for a in sorted(on.get(L, ()) & on.get(M, ())):
                    yield (a, b, c, d, L, M)
    elif rname == "angle_pair_on_line":
        on = _on_line_index(s)
        for L in sorted(lns):
            P = sorted(on.get(L, ()))
            off = [p for p in sorted(pts) if s.has(("not_on_line", p, L))]
            for v in P:
                for x in P:
                    if x == v or not s.has(("neq", v, x)):
                        continue
                    for u in P:
                        if u in (v, x) or not s.has(("neq", v, u)):
                            continue
                        for z in off:
                            for w in off:
                                yield (z, w, v, x, u, L)
    elif rname == "assert_point_eq_on_ray":
        on = _on_line_index(s)
        for L in sorted(lns):
            P = sorted(on.get(L, ()))
            for z in P:
                for x in P:
                    if x == z:
                        continue
                    for y in P:
                        if y in (x, z):
                            continue
                        if s.has(("not_between", x, z, y)):
                            yield (x, y, z, L)
    elif rname == "assert_point_eq_angle":
        on = _on_line_index(s)
        for f in facts:
            if f[0] != "same_side":
                continue
            _, x, y, L = f
            P = sorted(on.get(L, ()))
            for z in P:
                for w in P:
                    if w == z or not s.has(("neq", z, w)):
                        continue
                    if s.has(("neq", z, x)) and s.has(("neq", z, y)):
                        yield (x, y, w, z, L)


def repair_premise(state, i, rule, args, bindings,
                   max_depth=REPAIR_MAX_DEPTH, node_budget=REPAIR_NODE_BUDGET,
                   time_budget=REPAIR_TIME_BUDGET):
    """BFS over fact-only moves from `state`; returns (helpers, trial) where
    helpers is a non-empty list of (rule, args) moves (bindings {}) and
    trial is a clone of `state` with the helpers AND the original step
    applied — or None when no repair is found in budget. A helper that
    changes nothing (same dedup key) is skipped: it cannot unblock the
    step. Seconds, not minutes: the deadline is checked per candidate."""
    import time as _time
    t0 = _time.perf_counter()

    def over_budget():
        return _time.perf_counter() - t0 > time_budget

    def attempt(s):
        t = srch.clone_state(s)
        try:
            eng.apply_step(t, i, rule, args, bindings)
        except Exception:
            return None
        return t

    seen = {srch.state_key(state)}
    frontier = [(state, ())]
    nodes = 0
    while frontier:
        if nodes >= node_budget or over_budget():
            return None
        s, path = frontier.pop(0)
        nodes += 1
        if len(path) >= max_depth:
            continue
        k0 = srch.state_key(s)
        for rname in REPAIR_RULES:
            spec = eng.CONSTRUCTIONS[rname]
            for cargs in _repair_candidates(s, rname):
                if over_budget():
                    return None
                try:
                    prems = spec["premises"](s, *cargs)
                except Exception:
                    continue
                if not all(s.has(pf) for pf in prems):
                    continue
                s2 = srch.clone_state(s)
                try:
                    eng.apply_step(s2, i, rname, cargs, {})
                except Exception:
                    continue  # entailment premise failed, or engine error
                k = srch.state_key(s2)
                if k == k0 or k in seen:
                    continue
                seen.add(k)
                t = attempt(s2)
                if t is not None:
                    return (list(path) + [(rname, cargs)], t)
                frontier.append((s2, path + ((rname, cargs),)))
    return None


def review(prop, steps, max_depth=MAX_DEPTH, node_budget=NODE_BUDGET,
           time_budget=None, repair=True,
           repair_max_depth=REPAIR_MAX_DEPTH,
           repair_node_budget=REPAIR_NODE_BUDGET,
           repair_time_budget=REPAIR_TIME_BUDGET):
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
    n_smt = 0  # steps valid only via a fact-only SMT repair
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
        smt_helpers = None
        e_premise = None
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
            # Fact-only premise repair (P-euclid-review-smt-repair): Lean's
            # SMT closes small fact premises the engine only derives by
            # citing a forward rule. Repair searches for a fact-only
            # derivation; the firm rule is that a step is marked valid via
            # repair ONLY when such a derivation was found and replayed.
            # I.10 step 6 (intersection_lines CD AB) decision: it stays
            # INVALID with the missing premise — its gap (intersects_ll
            # CD AB) is closed in Lean by SMT alone (Prop10.lean has no
            # case split; review fix, claude-9) and in the engine by new
            # lines plus by_contra (replay steps 5-13); that is a proof tactic
            # with new objects, not a fact-only forward derivation, so no
            # repair exists and "undecided" would claim we still might
            # find one in this search space, which we cannot.
            if repair and _is_fact_premise(e.premise):
                fixed = repair_premise(state, i, rule, args, bindings,
                                       repair_max_depth, repair_node_budget,
                                       repair_time_budget)
                if fixed is not None:
                    smt_helpers, trial = fixed
                    e_premise = e.premise
            if smt_helpers is None:
                n_invalid += 1
                rows.append({
                    "step": i, "rule": fmt,
                    "valid": False, "why": why,
                    "progress": "invalid",
                    "d_before": None, "d_after": None,
                    "state_note": ("step not applied; later steps are "
                                   "reviewed against the state without it"),
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
        if smt_helpers is not None:
            n_smt += 1
            helper_txt = "; ".join(
                f"{r} {' '.join(str(x) for x in a)}" for r, a in smt_helpers)
            premise_txt = " ".join(str(x) for x in e_premise)
            why = (f"valid; Lean's SMT supplies {premise_txt} "
                   f"(engine: {helper_txt})")
            label = "valid-needs-smt"
        else:
            why = "apply_step succeeded"
        rows.append({
            "step": i, "rule": fmt,
            "valid": True, "why": why,
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
        "smt_repairs": n_smt,
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
