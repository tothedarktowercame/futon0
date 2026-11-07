#!/usr/bin/env python3
"""Certify System E engine step sequences as LeanEuclid proofs.

Given a found step sequence in the engine's (rule, args, bindings) format,
emit a LeanEuclid proof (theorem statement copied verbatim from
~/code/LeanEuclid/Book/PropNN.lean, `euclid_intros`, one `euclid_apply ...`
per step, `use <witness>`, `euclid_finish`), check it with
`lake env lean` from ~/code/LeanEuclid, and report certified / not certified
with the reason and wall time.

Certification rule: certified iff `lake env lean` exits 0 AND the output
contains neither an error ("error:") nor the sorry warning
("declaration uses 'sorry'"). Both halves matter:
  - solver answers SAT on a false goal  -> "error: failed", exit 1;
  - solver answers unknown (timeout)     -> "error: Could not prove", exit 1;
  - a `sorry` left anywhere in the script (the failure mode the review
    flagged: euclid_finish's `admitGoal` on solver-unknown) can exit 0
    with only the warning, and only the sorry check catches it.
Every SMT success in LeanEuclid goes through the axiom SMT_VERIF; that axiom
in the dependency list is expected and is not itself a defect of the proof.

LeanEuclid is used read-only except the scratch directory tmp-certify/ under
it, which this tool creates and the caller deletes; nothing there is ever
committed. PATH must include LeanEuclid/venv/bin and ~/.local/bin (z3/cvc5),
LD_LIBRARY_PATH the elan Lean shared-library dir — env() sets both up.

Usage (from ~/code/futon0):
  python3 scripts/euclid_certify.py book        # Book replay-data controls
  python3 scripts/euclid_certify.py found       # search-found sequences
  python3 scripts/euclid_certify.py negatives   # negative controls
"""

import importlib.util
import os
import subprocess
import time
from pathlib import Path

FUTON0 = Path(__file__).resolve().parents[1]
LEAN_EUCLID = Path("~/code/LeanEuclid").expanduser()
SCRATCH = LEAN_EUCLID / "tmp-certify"

LEAN_NAME = {  # engine identifier -> Lean identifier (unicode hypothesis names)
    "c0": "c₀",
    "c1": "c₁",
}


def lean(x):
    return LEAN_NAME.get(x, x)


# ---------------------------------------------------------------------------
# Theorems: statements copied verbatim from ~/code/LeanEuclid/Book/PropNN.lean
# ---------------------------------------------------------------------------

PROP1_STATEMENT = """\
theorem proposition_1 : ∀ (a b : Point) (AB : Line),
  distinctPointsOnLine a b AB →
  ∃ c : Point, |(c─a)| = |(a─b)| ∧ |(c─b)| = |(a─b)| :="""

PROP2_STATEMENT = """\
theorem proposition_2 : ∀ (a b c : Point) (BC : Line),
  (distinctPointsOnLine b c BC) ∧ (a ≠ b) →
  ∃ l : Point, |(a─l)| = |(b─c)| :="""

PROP3_STATEMENT = """\
theorem proposition_3 : ∀ (a b c₀ c₁ : Point) (AB C : Line),
  distinctPointsOnLine a b AB ∧ distinctPointsOnLine c₀ c₁ C ∧ |(a─b)| > |(c₀─c₁)| →
  ∃ e : Point, between a e b ∧ |(a─e)| = |(c₀─c₁)| :="""

# Book proof step sequences (the euclid_apply lines of Book/PropNN.lean, in
# the engine's (rule, args, bindings) format; forward rules carry no
# bindings). These are the replay-data control on the emitter.
PROP1_BOOK_STEPS = [
    ("circle_from_points", ("a", "b"), {"C": "BCD"}),
    ("circle_from_points", ("b", "a"), {"C": "ACE"}),
    ("intersection_circles", ("BCD", "ACE"), {"c": "c"}),
    ("point_on_circle_onlyif", ("a", "b", "c", "BCD"), {}),
    ("point_on_circle_onlyif", ("b", "a", "c", "ACE"), {}),
]
PROP2_BOOK_STEPS = [
    ("line_from_points", ("a", "b"), {"L": "AB"}),
    ("proposition_1", ("a", "b", "AB"), {"d": "d"}),
    ("line_from_points", ("d", "a"), {"L": "AE"}),
    ("line_from_points", ("d", "b"), {"L": "BF"}),
    ("circle_from_points", ("b", "c"), {"C": "CGH"}),
    ("intersection_circle_line_extending_points", ("CGH", "BF", "b", "d"),
     {"g": "g"}),
    ("circle_from_points", ("d", "g"), {"C": "GKL"}),
    ("intersection_circle_line_extending_points", ("GKL", "AE", "a", "d"),
     {"g": "l"}),
    ("point_on_circle_onlyif", ("b", "c", "g", "CGH"), {}),
    ("point_on_circle_onlyif", ("d", "l", "g", "GKL"), {}),
    ("between_if", ("l", "a", "d"), {}),
    ("between_if", ("g", "b", "d"), {}),
]
PROP3_BOOK_STEPS = [
    ("proposition_2'", ("a", "c0", "c1", "C"), {"d": "d"}),
    ("circle_from_points", ("a", "d"), {"C": "DEF"}),
    ("intersection_circle_line_between_points", ("DEF", "AB", "a", "b"),
     {"e": "e"}),
    ("point_on_circle_onlyif", ("a", "d", "e"), {}),
]

PROP5_STATEMENT = """\
theorem proposition_5 : ∀ (a b c d e : Point) (AB BC AC : Line),
  formTriangle a b c AB BC AC ∧ (|(a─b)| = |(a─c)|) ∧
  (between a b d) ∧ (between a c e) →
  (∠ a:b:c = ∠ a:c:b) ∧ (∠ c:b:d = ∠ b:c:e) :="""

# Copied verbatim from euclid_engine.PROP5_STEPS_DATA (zai-11 owns the engine
# file and may have it mid-edit; the copy is pinned and test-asserted equal to
# the engine's data whenever the engine file parses). The final rule takes a
# Segment argument: engine args ("c", "e") render as the Lean segment (c─e).
PROP5_BOOK_STEPS = [
    ("point_between_points_shorter_than", ("AB", "b", "d", "c", "e"),
     {"a": "f"}),
    ("proposition_3", ("a", "e", "f", "a", "AC", "AB"), {"q": "g"}),
    ("line_from_points", ("c", "f"), {"L": "FC"}),
    ("line_from_points", ("b", "g"), {"L": "GB"}),
    ("proposition_4", ("a", "f", "c", "a", "g", "b", "AB", "FC", "AC",
                       "AC", "GB", "AB"), {}),
    ("proposition_4", ("f", "b", "c", "g", "c", "b", "AB", "BC", "FC",
                       "AC", "BC", "GB"), {}),
    ("sum_angles_onlyif", ("b", "a", "g", "c", "AB", "GB"), {}),
    ("sum_angles_onlyif", ("c", "a", "f", "b", "AC", "FC"), {}),
]

# Rules whose Lean signature ends in a Segment built from the engine's last
# two (Point) args (SystemE Theory/Constructions/Points.lean:28).
SEGMENT_TAIL_RULES = {"point_between_points_shorter_than"}

PROPS = {
    "I.1": dict(imports=["SystemE"], statement=PROP1_STATEMENT,
                book_steps=PROP1_BOOK_STEPS, witness="c"),
    "I.2": dict(imports=["SystemE", "Book.Prop01"], statement=PROP2_STATEMENT,
                book_steps=PROP2_BOOK_STEPS, witness="l"),
    "I.3": dict(imports=["SystemE", "Book.Prop02"], statement=PROP3_STATEMENT,
                book_steps=PROP3_BOOK_STEPS, witness="e"),
    # I.5 has no existential witness (goal is a conjunction of angle
    # equalities), so witness=None: emit_proof emits no `use` line.
    "I.5": dict(imports=["SystemE", "Book.Prop03", "Book.Prop04"],
                statement=PROP5_STATEMENT, book_steps=PROP5_BOOK_STEPS,
                witness=None),
}


# ---------------------------------------------------------------------------
# Proof emission
# ---------------------------------------------------------------------------

def _render_args(rule, args, term=lean):
    """Render engine args for one euclid_apply, gluing segment-tail rules.

    point_between_points_shorter_than ends in a Segment (Points.lean:28); the
    engine carries it as its two endpoint Points, Lean wants (x─y).
    """
    if rule in SEGMENT_TAIL_RULES and len(args) >= 2:
        head = " ".join(term(a) for a in args[:-2])
        seg = f"({term(args[-2])}─{term(args[-1])})"
        return " ".join(x for x in (head, seg) if x)
    return " ".join(term(a) for a in args)


def emit_step(rule, args, bindings):
    """One euclid_apply line from an engine step."""
    terms = _render_args(rule, args)
    names = " ".join(lean(v) for v in bindings.values())
    if names:
        return f"  euclid_apply ({rule} {terms}) as {names}"
    return f"  euclid_apply ({rule} {terms})"


def emit_proof(prop, steps, witness, statement=None, finish="euclid_finish"):
    cfg = PROPS[prop]
    # Search-found steps carry engine fresh names containing '$' (illegal in
    # Lean); rename them to v1, v2, ... in order of first appearance.
    rename = {}

    def nm(x):
        if isinstance(x, str) and "$" in x and x not in rename:
            rename[x] = f"v{len(rename) + 1}"
        return rename.get(x, x)

    def term(x):
        return lean(nm(x))

    lines = [f"import {cfg['imports'][0]}"]
    for imp in cfg["imports"][1:]:
        lines.append(f"import {imp}")
    lines.append("")
    lines.append("namespace Elements.Book1")
    lines.append("")
    lines.append(statement if statement is not None else cfg["statement"])
    lines.append("by")
    lines.append("  euclid_intros")
    for rule, args, bindings in steps:
        terms = _render_args(rule, args, term=term)
        names = " ".join(term(v) for v in bindings.values())
        if names:
            lines.append(f"  euclid_apply ({rule} {terms}) as {names}")
        else:
            lines.append(f"  euclid_apply ({rule} {terms})")
    if witness is not None:
        lines.append(f"  use {term(witness)}")
    lines.append(f"  {finish}")
    lines.append("")
    lines.append("end Elements.Book1")
    return "\n".join(lines) + "\n"


# ---------------------------------------------------------------------------
# Lean checking
# ---------------------------------------------------------------------------

def env():
    e = dict(os.environ)
    e["PATH"] = (f"{LEAN_EUCLID / 'venv' / 'bin'}:{Path('~/.local/bin').expanduser()}"
                 f":{e.get('PATH', '')}")
    libdir = Path("~/.elan/toolchains/leanprover--lean4---v4.19.0/lib/lean")
    e["LD_LIBRARY_PATH"] = str(libdir.expanduser())
    return e


def check_lean(source, name, timeout=600):
    """Run `lake env lean` on the emitted proof.

    Returns dict(certified, rc, wall_s, reason, output). Certified iff
    rc == 0 and no "error:" line and no "declaration uses 'sorry'".
    """
    SCRATCH.mkdir(exist_ok=True)
    path = SCRATCH / f"{name}.lean"
    path.write_text(source)
    t0 = time.perf_counter()
    try:
        proc = subprocess.run(
            ["lake", "env", "lean", str(path)],
            cwd=str(LEAN_EUCLID), env=env(), capture_output=True, text=True,
            timeout=timeout)
    except subprocess.TimeoutExpired as e:
        # review (claude-9): a solver that never answers is a refusal to
        # certify, not a crash of the certifier
        out = ((e.stdout or b"").decode(errors="replace")
               if isinstance(e.stdout, bytes) else (e.stdout or ""))
        return {"certified": False, "rc": None,
                "wall_s": round(time.perf_counter() - t0, 2),
                "reason": f"timeout after {timeout} s", "output": out,
                "path": str(path)}
    wall = time.perf_counter() - t0
    out = (proc.stdout + proc.stderr).strip()
    reasons = []
    if "declaration uses 'sorry'" in out:
        reasons.append("declaration uses 'sorry'")
    for line in out.splitlines():
        if ": error:" in line or line.startswith("error:"):
            reasons.append(line.strip())
    if proc.returncode != 0:
        reasons.append(f"exit code {proc.returncode}")
    return {"certified": not reasons, "rc": proc.returncode,
            "wall_s": round(wall, 2), "reason": "; ".join(dict.fromkeys(reasons)),
            "output": out, "path": str(path)}


# ---------------------------------------------------------------------------
# Modes
# ---------------------------------------------------------------------------

def certify_book():
    results = {}
    for prop, cfg in PROPS.items():
        r = check_lean(emit_proof(prop, cfg["book_steps"], cfg["witness"]),
                       f"book_{prop.replace('.', '')}")
        results[prop] = r
        _report(f"book {prop}", r)
    return results


def certify_found():
    """Certify the sequences found by euclid_search (BFS).

    Only runs when scripts/euclid_search.py is committed (the requisition's
    pending rule: zai-11 owns that file while it is uncommitted).
    """
    r = subprocess.run(["git", "-C", str(FUTON0), "ls-files",
                        "--error-unmatch", "scripts/euclid_search.py"],
                       capture_output=True)
    if r.returncode != 0:
        print("found-sequence: PENDING — scripts/euclid_search.py is not "
              "committed yet (zai-11 owns it); only the Book control "
              "sequences were certified.")
        return None
    spec = importlib.util.spec_from_file_location(
        "euclid_search", FUTON0 / "scripts" / "euclid_search.py")
    srch = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(srch)
    results = {}
    for prop, kw in srch.prop_searches().items():
        found, path, st = srch.search(**kw)
        if not found:
            results[prop] = {"certified": False,
                             "reason": "search did not find a sequence"}
            _report(f"found {prop}", results[prop])
            continue
        srch.verify_path(kw["hyps"], path, kw["goal"], kw.get("extra_la"))
        # witness: the point introduced by the final construction step
        witness = list(path[-1][2].values())[-1]
        res = check_lean(emit_proof(prop, path, witness),
                         f"found_{prop.replace('.', '')}")
        results[prop] = res
        _report(f"found {prop}", res)
    return results


def negatives():
    """(a) drop one construction step; (b) unprovable conclusion with
    exit-0/`sorry`-only variants."""
    out = {}
    # (a) I.1 without the second circle: intersection_circles loses its premise.
    steps = [s for s in PROP1_BOOK_STEPS
             if not (s[0] == "circle_from_points" and s[1] == ("b", "a"))]
    r = check_lean(emit_proof("I.1", steps, "c"), "neg_missing_step")
    out["drop-second-circle"] = r
    _report("negative drop-second-circle", r)
    # (b1) false conclusion the SMT solver refutes (SAT): "error: failed".
    bad = PROP1_STATEMENT.replace(
        "∃ c : Point, |(c─a)| = |(a─b)| ∧ |(c─b)| = |(a─b)|",
        "∃ c : Point, |(c─a)| = |(a─b)| ∧ ¬(|(c─b)| = |(a─b)|)")
    r = check_lean(emit_proof("I.1", PROP1_BOOK_STEPS, "c", statement=bad),
                   "neg_sat")
    out["sat-refuted"] = r
    _report("negative sat-refuted", r)
    # (b2) the exit-0 trap the review flagged: euclid_finish's solver-unknown
    # path closes with `admitGoal` (sorry). A hand-substituted sorry stands in
    # for it (in this LeanEuclid build solver-unknown also logs an error, so
    # the rc-0 case is demonstrated directly): only the sorry check catches it.
    r = check_lean(emit_proof("I.1", PROP1_BOOK_STEPS, "c", finish="sorry"),
                   "neg_sorry")
    out["sorry-only"] = r
    _report("negative sorry-only", r)
    return out


def _report(label, r):
    print(f"{label}: {'CERTIFIED' if r['certified'] else 'NOT certified'}"
          f" ({r.get('wall_s', '-')}s)"
          + ("" if r["certified"] else f" — {r['reason']}"))


def main():
    import sys
    mode = sys.argv[1] if len(sys.argv) > 1 else "book"
    if mode == "book":
        certify_book()
    elif mode == "found":
        certify_found()
    elif mode == "negatives":
        negatives()
    elif mode == "all":
        certify_book()
        certify_found()
        negatives()
    else:
        raise SystemExit(f"unknown mode {mode!r}")


if __name__ == "__main__":
    main()
