#!/usr/bin/env python3
"""euclid_play.py — Witness-style play backend for Book I (E-kimi-task-413,
packet P-euclid-play-backend from claude-9).

A student plays the propositions IN ORDER: proposition N's palette is the
constructions plus proposition_K (and primed variants) for K < N only;
citing proposition_N or later is refused ("I.N is not proved yet").

  catalogue()      ordered entries for every N in euclid_trace.FREE
  palette(N)       rule names citable at N
  play(N, moves)   STATELESS replay of a student's moves from the
                   proposition's hypothesis state

HTTP: python3 euclid_play.py --port P  (binds 127.0.0.1) serves the viewer
directory statically plus GET /api/catalogue and POST /api/play.

Stdlib only; imports euclid_engine as eng and euclid_trace as tr.
Neither is modified.

GOALS: the same per-prop goal queries euclid_trace.build_trace / the
engine's own main() already use. Where the engine names them
(PROP13_GOAL, PROP14_GOAL, PROP15_GOAL, PROP16B_GOAL, PROP17_GOAL,
PROP18_GOAL, PROP19_GOAL, PROP20_GOAL, PROP21_GOAL, and _prop1()'s verify
list) the constant is REFERENCED, not copied. The 1-5 and 10-12 queries
exist only INLINE in eng.main()/tr.build_trace (no module-level name), so
they are restated here verbatim — flagged to claude-9 in the reply bell.
"""

import argparse
import functools
import json
import os
import re
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer

import euclid_engine as eng
import euclid_trace as tr

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
VIEWER_DIR = os.path.join(REPO_ROOT, "holes", "excursions",
                          "E-euclid-tutor-data", "viewer")
INDEX_HTML = os.path.join(VIEWER_DIR, "index.html")

# The goal queries, per prop. Referenced where the engine names them;
# restated verbatim (with provenance) where it does not.
GOALS = {
    1: eng._prop1()[2],                     # engine's own verify list
    2: [("_eq_len", "a", "l", "b", "c")],   # eng.main() p=="2" assert
    3: [("between", "a", "e", "b"),         # eng.main() p=="3" asserts
        ("_eq_len", "a", "e", "c0", "c1")],
    4: [("_eq_len", "b", "c", "e", "f"),    # eng.main() p=="4" asserts
        ("_eq_ang", "a", "b", "c", "d", "e", "f"),
        ("_eq_ang", "a", "c", "b", "d", "f", "e")],
    5: [("_eq_ang", "a", "b", "c", "a", "c", "b"),  # eng.main() p=="5"
        ("_eq_ang", "c", "b", "d", "b", "c", "e")],
    10: [("between", "a", "d", "b"),        # tr.build_trace queries dict
         ("_eq_len", "a", "d", "d", "b")],
    11: [("not_on_line", "f", "AB"),        # tr.build_trace queries dict
         ("_rt_ang", "u", "w", "f")],
    12: [("_rt_ang", "e", "h", "c"),        # tr.build_trace queries dict
         ("on_line", "h", "AB")],           # (I.12's goal substitute)
    13: [eng.PROP13_GOAL],
    14: eng.PROP14_GOAL,
    15: eng.PROP15_GOAL,
    16: eng.PROP16B_GOAL,
    17: eng.PROP17_GOAL,
    18: eng.PROP18_GOAL,
    19: eng.PROP19_GOAL,
    20: eng.PROP20_GOAL,
    21: eng.PROP21_GOAL,
}

_PROP_RE = re.compile(r"^proposition_(\d+)'*$")


def palette(n):
    """Rule names a student may use at proposition N: every
    eng.CONSTRUCTIONS entry that is not a proposition, plus
    proposition_K and its primed variants for K < N only."""
    constructions, propositions = [], []
    for name in eng.CONSTRUCTIONS:
        m = _PROP_RE.match(name)
        if m:
            if int(m.group(1)) < n:
                propositions.append(name)
        else:
            constructions.append(name)
    return constructions + propositions


def _titles():
    """The viewer's option text per numeric proposition (index.html)."""
    titles = {}
    with open(INDEX_HTML) as fh:
        for m in re.finditer(r'<option value="(\d+)">([^<]+)</option>',
                             fh.read()):
            titles[int(m.group(1))] = m.group(2).strip()
    return titles


def _lean_statement(n):
    """Book/PropNN.lean from 'theorem' to ':='."""
    path = os.path.join(tr.LEAN_DIR, "Book", "Prop%02d.lean" % n)
    lines, collecting = [], False
    with open(path) as fh:
        for line in fh:
            if not collecting and line.lstrip().startswith("theorem"):
                collecting = True
            if collecting:
                stop = line.rstrip().endswith(":=")
                lines.append(line.rstrip() if not stop
                             else line.rstrip()[:-2].rstrip())
                if stop:
                    break
    return "\n".join(lines)


def rule_info(name):
    """{args, cite, binds} for a rule. binds are the base names the rule's
    make introduces, read off the make lambda's returned pairs on a
    throwaway state (euclid_engine.py is NOT modified). If the make
    lambda cannot be run that way, binds is [] — the page then shows one
    free "name" field instead of per-bind inputs."""
    spec = eng.CONSTRUCTIONS[name]
    binds = []
    try:
        made = spec["make"](eng.State(),
                            *["#arg%d" % k for k in range(len(spec["args"]))])
        binds = [b for b, _facts in made if b is not None]
    except Exception:
        binds = []
    return {"args": list(spec["args"]), "cite": spec["cite"],
            "binds": binds}


def catalogue():
    """Ordered list, one entry per proposition N in tr.FREE."""
    titles = _titles()
    out = []
    for n in sorted(tr.FREE):
        hyps, _steps = tr.hyps_and_steps(n)
        out.append({
            "n": n,
            "title": titles.get(n, "I.%d" % n),
            "statement": _lean_statement(n),
            "hyps": [list(f) for f, _w in hyps],
            "goals": [list(g) for g in GOALS[n]],
            "palette": [dict({"name": nm}, **rule_info(nm))
                        for nm in palette(n)],
        })
    return out


def _start_state(n):
    """The start state exactly as euclid_trace.build_trace builds it:
    metric hyps for 4/5, the prop-3 row, then close."""
    hyps, _steps = tr.hyps_and_steps(n)
    if n in (4, 5):
        s = eng._metric_state(hyps)
    else:
        s = eng.State()
        for f, w in hyps:
            # as eng.replay() does (tr.build_trace's path for 10-21):
            # metric hyps (e.g. I.18's gt_len) also add their LA row
            eng._assume_metric_or_fact(s, f, w)
    if n == 3:
        s.la.add({eng.len_atom("a", "b"): 1,
                  eng.len_atom("c0", "c1"): -1}, 0, ">")
    eng.close(s)
    return s


def play(n, moves):
    """Stateless: rebuild N's start state, apply each move
    {rule, args, bindings} with eng.apply_step + close, stop at the first
    failure. by_contra/by_cases moves go through eng.run_steps (the same
    tuples the engine uses)."""
    pal = set(palette(n))
    s = _start_state(n)
    objects = tr.objects_from_hyps(n)
    xy = {k: v for k, v in tr.FREE[n].items()}
    tr.derived_free_points(n, xy)
    out_moves = []
    for i, mv in enumerate(moves, 1):
        rule = mv["rule"]
        args = tuple(mv.get("args") or ())
        bindings = dict(mv.get("bindings") or {})
        entry = {"n": i, "rule": rule, "ok": False, "added": [],
                 "error": None}
        m = _PROP_RE.match(rule)
        if m and rule not in pal:
            entry["error"] = "I.%d is not proved yet" % int(m.group(1))
        elif rule not in pal and rule not in ("by_contra", "by_cases"):
            entry["error"] = ("unknown rule %r (not in I.%d's palette)"
                              % (rule, n))
        else:
            try:
                if rule in ("by_contra", "by_cases"):
                    rep = eng.run_steps(s, [(rule, list(args), bindings)])[0]
                    added = rep.get("added", [])
                else:
                    rep = eng.apply_step(s, i, rule, args, bindings)
                    eng.close(s)
                    added = rep["added"]
                    tr.place_step(n, rule, args, bindings, added, xy,
                                  objects)
                    for o in objects:
                        if o["def_step"] is None:
                            o["def_step"] = i
                entry["ok"] = True
                entry["added"] = [list(f) for f in added]
            except eng.MissingPremise as e:
                entry["error"] = (list(e.premise)
                                  if isinstance(e.premise, tuple)
                                  else str(e.premise))
            except TypeError as e:
                entry["error"] = ("bad arity for %s: %s" % (rule, e))
        out_moves.append(entry)
        if not entry["ok"]:
            break
    goal_reached = all(eng._fact_holds(s, g) for g in GOALS[n])
    contradict = eng.contradictory(s)[0]
    out_objects = []
    for o in objects:
        d = dict(o)
        if o["kind"] == "point" and o["name"] in xy:
            d["xy"] = list(xy[o["name"]]) if xy[o["name"]] else None
        out_objects.append(d)
    return {"n": n, "moves": out_moves, "goal_reached": goal_reached,
            "objects": out_objects, "contradictory": contradict}


class _Handler(SimpleHTTPRequestHandler):
    def __init__(self, *a, **kw):
        super().__init__(*a, directory=VIEWER_DIR, **kw)

    def _send_json(self, obj, code=200):
        body = json.dumps(obj).encode()
        self.send_response(code)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def do_GET(self):
        if self.path == "/api/catalogue":
            self._send_json(catalogue())
        else:
            super().do_GET()

    def do_POST(self):
        if self.path == "/api/play":
            try:
                length = int(self.headers.get("Content-Length", 0))
                data = json.loads(self.rfile.read(length) or b"{}")
                self._send_json(play(int(data["n"]),
                                     data.get("moves") or []))
            except (KeyError, ValueError, TypeError) as e:
                self._send_json({"error": str(e)}, code=400)
        else:
            self.send_error(404)

    def log_message(self, *a):
        pass


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=8123)
    args = ap.parse_args()
    srv = ThreadingHTTPServer(("127.0.0.1", args.port), _Handler)
    print("serving %s on http://127.0.0.1:%d" % (VIEWER_DIR, args.port))
    srv.serve_forever()


if __name__ == "__main__":
    main()
