#!/usr/bin/env python3
"""Export reviewed student attempts as viewer traces (E-euclid-tutor).

For each demo fixture of euclid_review (imported read-only), run review()
and write traces/review-<name>.json: the review rows (valid / progress /
why / distances — exactly what review() returned) plus what the viewer
needs to DRAW the steps that were applied.

Drawing reuses euclid_trace's numeric placement (also imported read-only):
the state sequence is review()'s own — a step is placed iff review() marked
it valid=True (it advanced the state); invalid / undecided / engine-error /
blocked steps draw nothing, they only appear in the step list with a badge.

Usage: python3 scripts/euclid_review_export.py [--out DIR] [fixture ...]
"""

import importlib.util
import json
import os
import sys

_SCRIPTS = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, _SCRIPTS)


def _load(name, filename):
    spec = importlib.util.spec_from_file_location(
        name, os.path.join(_SCRIPTS, filename))
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


rv = _load("euclid_review_export_review", "euclid_review.py")
et = _load("euclid_review_export_trace", "euclid_trace.py")
ec = _load("euclid_review_export_clones", "euclid_clones.py")
cdg = _load("euclid_review_export_clone_diagram", "euclid_clone_diagram.py")

ROOT = os.path.dirname(_SCRIPTS)
OUT_DIR = os.path.join(ROOT, "holes", "excursions", "E-euclid-tutor-data",
                       "viewer", "traces")

# viewer badge classes, keyed by (valid, progress)
BADGE = "badge-"


def badge_of(row):
    """Badge name for one review row (viewer class names mirror this)."""
    if row["valid"] is False:
        return "invalid"
    if row["valid"] is None:
        return "unknown"  # undecided / engine-error / blocked: not decided
    if row["progress"] == "progress":
        return "valid-progress"
    if row["progress"] == "no-progress":
        return "valid-no-progress"
    return "unknown"  # valid but distances unknown


def make_envelope(prop):
    """standard_envelope() plus the TrustingClone for this proposition.
    euclid_clones.py / euclid_clone_diagram.py are called, never edited."""
    env = ec.standard_envelope()
    tc = cdg.TrustingClone("trusting", prop)
    env.clones.append(tc)
    env.active.append(tc)
    return env


def export_fixture(name):
    """Review one fixture and build its viewer trace dict."""
    prop = rv.FIXTURE_PROP[name]
    steps = rv.FIXTURES[name]()
    rep = rv.review(prop, steps)

    # state sequence exactly as review() walked it: only valid=True steps
    # advance the state, so only those get a placement
    state = rv.initial_state(prop)
    env = make_envelope(prop)
    p = int(prop.split(".")[1])  # euclid_trace keys placements by prop number
    objects = et.objects_from_hyps(p)
    xy = dict(et.FREE[p])
    et.derived_free_points(p, xy)
    rows = []
    n_drawn = 0
    for i, (step, row) in enumerate(zip(steps, rep["steps"]), start=1):
        bindings = step[2] if len(step) > 2 else {}
        drawn = False
        # envelope: observe BEFORE any state/picture advance, at the state
        # the review used for this step (same shape as euclid_clones_test:
        # observe, then note_step, then advance). Exactly what observe()
        # returned — no re-computation, no rounding.
        ev = env.observe(prop, state, step)
        envelope = {
            "posterior": ev["posterior"],
            "probabilities": ev["probabilities"],
            "below_threshold": ev["below_threshold"],
            "all_below_threshold": ev["all_below_threshold"],
        }
        if row["valid"] is True:
            pre = state  # note_step applies the step itself; give it the
            # state BEFORE the step, like the engine replay in euclid_clones
            trial = rv.srch.clone_state(state)
            step_rep = rv.eng.apply_step(trial, i, step[0], step[1],
                                         bindings)
            why = et.place_step(prop, step[0], step[1], bindings,
                                step_rep["added"], xy, objects)
            for o in objects:
                if o["def_step"] is None:
                    o["def_step"] = i
            state = trial
            drawn = True
            n_drawn += 1
            placement_note = why
            # the trusting clone's picture follows the applied steps only,
            # like the review's state (an invalid step never happened)
            tc = env.clones[-1]
            if tc.name == "trusting":
                tc.note_step(prop, pre, step)
        else:
            placement_note = "step not applied — nothing drawn for it"
        rows.append({
            "n": i,
            "rule": row["rule"],
            "valid": row["valid"],
            "progress": row["progress"],
            "why": row["why"],
            "d_before": row["d_before"],
            "d_after": row["d_after"],
            "state_note": row.get("state_note"),
            "badge": badge_of(row),
            "drawn": drawn,
            "envelope": envelope,
            "placement_decision": placement_note,
        })

    placement = {k: (list(v) if v is not None else None)
                 for k, v in xy.items()}
    return {
        "review": True,
        "prop": prop,
        "title": "review: %s — student attempt on I.%s" % (
            name, prop.split(".")[-1]),
        "name": name,
        "objects": objects,
        "steps": rows,          # same key the stepper walks
        "placement": placement,
        "summary": {
            "invalid_steps": rep["invalid_steps"],
            "undecided_steps": rep["undecided_steps"],
            "engine_errors": rep["engine_errors"],
            "blocked_steps": rep["blocked_steps"],
            "incomplete": rep["incomplete"],
            "goal_held": rep["goal_held"],
            "remaining_distance": rep["remaining_distance"],
            "steps_drawn": n_drawn,
        },
    }


def main():
    out = OUT_DIR
    argv = sys.argv[1:]
    if "--out" in argv:
        out = argv[argv.index("--out") + 1]
        argv = [a for a in argv if not a.startswith("--")]
    names = argv or list(rv.FIXTURES)
    os.makedirs(out, exist_ok=True)
    for name in names:
        t = export_fixture(name)
        path = os.path.join(out, "review-%s.json" % name)
        with open(path, "w") as fh:
            json.dump(t, fh, indent=1)
        print("wrote %s (%d steps, %d drawn, %d invalid)" %
              (path, len(t["steps"]), t["summary"]["steps_drawn"],
               t["summary"]["invalid_steps"]))


if __name__ == "__main__":
    main()
