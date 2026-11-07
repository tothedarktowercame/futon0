"""Tests for euclid_certify.py — LeanEuclid certification of engine sequences.
Run from ~/code/futon0: python3 -m pytest scripts/euclid_certify_test.py -q

Integration tests invoke `lake env lean` against ~/code/LeanEuclid (read-only
except the tmp-certify/ scratch dir, which is not committed). They are skipped
when LeanEuclid or its built oleans are absent.
"""

import importlib.util
from pathlib import Path
import shutil
import subprocess
import pytest

spec = importlib.util.spec_from_file_location(
    "euclid_certify", Path(__file__).resolve().parents[1] / "scripts/euclid_certify.py")
cert = importlib.util.module_from_spec(spec)
spec.loader.exec_module(cert)

LEAN_AVAILABLE = (cert.LEAN_EUCLID / ".lake").exists() and shutil.which("lake")


# --- emission (unit) --------------------------------------------------------

def test_emit_book_prop1_golden():
    src = cert.emit_proof("I.1", cert.PROP1_BOOK_STEPS, "c")
    assert src == """import SystemE

namespace Elements.Book1

theorem proposition_1 : ∀ (a b : Point) (AB : Line),
  distinctPointsOnLine a b AB →
  ∃ c : Point, |(c─a)| = |(a─b)| ∧ |(c─b)| = |(a─b)| :=
by
  euclid_intros
  euclid_apply (circle_from_points a b) as BCD
  euclid_apply (circle_from_points b a) as ACE
  euclid_apply (intersection_circles BCD ACE) as c
  euclid_apply (point_on_circle_onlyif a b c BCD)
  euclid_apply (point_on_circle_onlyif b a c ACE)
  use c
  euclid_finish

end Elements.Book1
"""


def test_emit_renames_fresh_dollar_names():
    steps = [("circle_from_points", ("a", "b"), {"C": "C$1"}),
             ("intersection_circles", ("C$1", "C$2"), {"c": "c$3"})]
    src = cert.emit_proof("I.1", steps, "c$3")
    assert "$" not in src
    assert "  euclid_apply (intersection_circles v1 v2) as v3" in src
    assert "  use v3" in src


def test_emit_unicode_hypothesis_names():
    # engine c0/c1 must become the Book's c₀/c₁ in citable-proposition args
    src = cert.emit_proof("I.3", cert.PROP3_BOOK_STEPS, "e")
    assert "(proposition_2' a c₀ c₁ C) as d" in src


def test_emit_prop5_segment_arg_and_no_witness():
    # (1) point_between_points_shorter_than ends in a Segment: the engine's
    # two endpoint args render as (c─e); (2) I.5 has no existential witness,
    # so emit_proof emits no `use` line.
    src = cert.emit_proof("I.5", cert.PROP5_BOOK_STEPS, None)
    assert "  euclid_apply (point_between_points_shorter_than AB b d (c─e)) as f" in src
    assert "\n  use " not in src


def test_emit_prop5_matches_book_proof_lines():
    # the emitted euclid_apply lines are the Book's own proof
    # (Book/Prop05.lean lines 13-20) modulo whitespace
    src = cert.emit_proof("I.5", cert.PROP5_BOOK_STEPS, None)
    book = (cert.LEAN_EUCLID / "Book" / "Prop05.lean").read_text().splitlines()
    book_steps = [l.strip() for l in book[12:20] if l.strip().startswith("euclid_apply")]
    ours = [l.strip() for l in src.splitlines() if l.strip().startswith("euclid_apply")]
    assert ours == book_steps


def test_emit_prop4_asserts_and_grouped_bindings():
    # I.4: euclid_assert steps emitted verbatim; superposition's four names
    # in the Book's grouped form `as (b', c', BC', DC')`; no witness.
    src = cert.emit_proof("I.4", cert.PROP4_BOOK_STEPS, None)
    assert "  euclid_apply (superposition a b c d e f AB BC AC DE) as (b', c', BC', DC')" in src
    assert "  euclid_assert b' = e" in src
    assert "  euclid_assert ∠ e:d:c' = ∠ e:d:f" in src
    assert "  euclid_assert DC' = DF" in src
    assert "  euclid_assert c' = f" in src
    assert "\n  use " not in src


def test_emit_prop4_matches_book_proof_block():
    # the emitted statement and proof block are the Book's own, byte-identical
    book = (cert.LEAN_EUCLID / "Book" / "Prop04.lean").read_text().splitlines()

    def block(lines):
        return lines[lines.index("by"):lines.index("end Elements.Book1")]

    src = cert.emit_proof("I.4", cert.PROP4_BOOK_STEPS, None).splitlines()
    assert cert.PROP4_STATEMENT == "\n".join(book[4:8])
    assert block(src) == block(book)


def test_prop5_steps_pinned_to_engine_data():
    # PROP5_BOOK_STEPS is a verbatim copy of euclid_engine.PROP5_STEPS_DATA;
    # keep them in sync (skipped while zai-11 has the engine mid-edit).
    try:
        spec = importlib.util.spec_from_file_location(
            "euclid_engine", Path(__file__).resolve().parents[1] / "scripts" / "euclid_engine.py")
        eng = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(eng)
    except SyntaxError:
        pytest.skip("euclid_engine.py currently unparseable (concurrent edit)")
    assert cert.PROP5_BOOK_STEPS == eng.PROP5_STEPS_DATA


class _FakePopen:
    """Stands in for subprocess.Popen in check_lean unit tests."""
    def __init__(self, out="", rc=0, hang=False):
        self.out, self.returncode, self.hang, self.pid = out, rc, hang, -1

    def __call__(self, *a, **kw):
        return self

    def communicate(self, timeout=None):
        if self.hang and timeout is not None:
            raise subprocess.TimeoutExpired(cmd="lean", timeout=timeout)
        return self.out, ""


def test_check_lean_rule_catches_sorry_at_rc0(monkeypatch):
    # the exit-0 trap the review flagged: only the sorry check catches it
    monkeypatch.setattr(cert.subprocess, "Popen",
                        _FakePopen("warning: declaration uses 'sorry'\n"))
    r = cert.check_lean("x", "t")
    assert not r["certified"]
    assert "declaration uses 'sorry'" in r["reason"]


# --- Lean checking (integration) ---------------------------------------------

@pytest.mark.skipif(not LEAN_AVAILABLE, reason="LeanEuclid not available")
@pytest.mark.parametrize("prop", ["I.1", "I.2", "I.3", "I.4", "I.5"])
def test_book_sequences_certify(prop):
    cfg = cert.PROPS[prop]
    r = cert.check_lean(cert.emit_proof(prop, cfg["book_steps"], cfg["witness"]),
                        f"test_book_{prop.replace('.', '')}")
    assert r["certified"], r["output"]
    assert r["wall_s"] > 0


@pytest.mark.skipif(not LEAN_AVAILABLE, reason="LeanEuclid not available")
def test_negative_missing_construction_step():
    # (a) drop the second circle: the intersection loses its premise
    steps = [s for s in cert.PROP1_BOOK_STEPS
             if not (s[0] == "circle_from_points" and s[1] == ("b", "a"))]
    r = cert.check_lean(cert.emit_proof("I.1", steps, "c"), "test_neg_drop")
    assert not r["certified"]
    assert "error" in r["reason"].lower()


@pytest.mark.skipif(not LEAN_AVAILABLE, reason="LeanEuclid not available")
def test_negative_false_conclusion_sat():
    # (b1) a conclusion the SMT solver refutes: "error: failed", exit 1
    bad = cert.PROP1_STATEMENT.replace(
        "∃ c : Point, |(c─a)| = |(a─b)| ∧ |(c─b)| = |(a─b)|",
        "∃ c : Point, |(c─a)| = |(a─b)| ∧ ¬(|(c─b)| = |(a─b)|)")
    r = cert.check_lean(cert.emit_proof("I.1", cert.PROP1_BOOK_STEPS, "c",
                                        statement=bad), "test_neg_sat")
    assert not r["certified"]


@pytest.mark.skipif(not LEAN_AVAILABLE, reason="LeanEuclid not available")
def test_negative_sorry_only_warning():
    # (b2) exit code 0 with only the sorry warning (the trap: solver-unknown
    # closes with admitGoal): the sorry check is what reports not-certified
    r = cert.check_lean(cert.emit_proof("I.1", cert.PROP1_BOOK_STEPS, "c",
                                        finish="sorry"), "test_neg_sorry")
    assert r["rc"] == 0
    assert not r["certified"]
    assert "declaration uses 'sorry'" in r["reason"]


@pytest.mark.skipif(not LEAN_AVAILABLE, reason="LeanEuclid not available")
def test_engine_i4_certifies():
    # the ENGINE's own PROP4_STEPS_DATA, bridges as euclid_assert of their
    # peq conclusions, AC' renamed DC' at emit time (P-euclid-certify-engine-I4)
    r = cert.check_lean(
        cert.emit_proof("I.4-engine", cert.engine_steps("I.4"), None),
        "test_engine_I4")
    assert r["certified"], r["output"]


def test_engine_i4_asserts_come_from_engine_data(monkeypatch):
    # monkeypatch the engine's PROP4_STEPS_DATA with swapped bridge args and
    # show the emitted assert text follows the data (no hand copy)
    eng = cert._engine()
    swapped = [list(s) for s in eng.PROP4_STEPS_DATA]
    swapped[1] = ("assert_point_eq_on_ray", ("c'", "f", "d", "DE"), {})
    monkeypatch.setattr(eng, "PROP4_STEPS_DATA", swapped)
    src = cert.emit_proof("I.4-engine", cert.engine_steps("I.4"), None)
    assert "  euclid_assert c' = f" in src
    assert "  euclid_assert b' = e" not in src


def test_unknown_bridge_rule_raises():
    # neither citable in Lean nor a known bridge: refuse, never silently drop
    steps = [("assert_point_eq_line", ("b'", "DE"), {})]
    with pytest.raises(ValueError, match="assert_point_eq_line"):
        cert.translate_engine_steps(steps)


# --- I.11-engine (E-kimi-task-377) ------------------------------------------

def _i11_src(steps=None):
    if steps is None:
        steps = cert.engine_steps("I.11")
    cfg = cert.PROPS["I.11-engine"]
    return cert.emit_proof("I.11-engine", steps, "f",
                           finish=cfg.get("finish", "euclid_finish"))


def test_emit_engine_i11_emission_pinned():
    # Engine names renamed back to Lean's (u v w g -> a b c d); the h-sm
    # input is a CHECKED euclid_assert right after proposition_1 creates f;
    # between_symm_parts emits as euclid_apply of between_symm (axiom cite,
    # not assert); no trailing euclid_finish (use f closes the goal).
    src = _i11_src()
    assert "  euclid_apply (between_symm a c b)\n" in src
    assert "  euclid_apply (point_between_points_shorter_than AB c a (c─b)) as d\n" in src
    assert ("  euclid_apply (proposition_1 d e AB) as f\n"
            "  euclid_assert ¬(f.onLine AB)\n") in src
    assert "  euclid_apply (perpendicular_if a b c f AB)\n  use f\n" in src
    assert "euclid_finish" not in src
    assert " u " not in src and " v " not in src and " w " not in src
    import hashlib
    assert hashlib.sha256(src.encode()).hexdigest()[:16] == "bcb60be9b3dcdff4"


def test_engine_i11_steps_come_from_engine_data(monkeypatch):
    # mutate the engine's live PROP11_STEPS_DATA and show the emission follows
    eng = cert._engine()
    swapped = [list(s) for s in eng.PROP11_STEPS_DATA]
    swapped[9] = ("proposition_8", ("w", "e", "f", "w", "g", "f",
                                    "AB", "DF", "FC", "AB", "FE", "FC"), {})
    monkeypatch.setattr(eng, "PROP11_STEPS_DATA", swapped)
    src = _i11_src()
    assert "(proposition_8 c e f c d f AB DF FC AB FE FC)" in src
    assert "(proposition_8 c d f c e f AB DF FC AB FE FC)" not in src


@pytest.mark.skipif(not LEAN_AVAILABLE, reason="LeanEuclid not available")
def test_engine_i11_certifies():
    # the ENGINE's own PROP11_STEPS_DATA incl. its SMT-sourced h-sm input,
    # certified against LeanEuclid
    r = cert.check_lean(_i11_src(), "test_engine_I11")
    assert r["certified"], r["output"]


@pytest.mark.skipif(not LEAN_AVAILABLE, reason="LeanEuclid not available")
def test_negative_engine_i11_false_hsm_assert():
    # Replace the h-sm assert ¬(f.onLine AB) with its negation (asserting the
    # equilateral apex ON its base — false). The assert must be CHECKED, so
    # this must NOT certify, and no lean process may survive the refusal.
    steps = [("euclid_assert", ("f.onLine AB",), {})
             if r_ == "euclid_assert" else (r_, a, b)
             for (r_, a, b) in cert.engine_steps("I.11")]
    assert any(r_ == "euclid_assert" for (r_, _a, _b) in steps)
    r = cert.check_lean(_i11_src(steps), "test_neg_I11_hsm", timeout=120)
    assert not r["certified"], r
    import time
    for _ in range(50):
        left = subprocess.run(["pgrep", "-f", "test_neg_I11_hsm.lean"],
                              capture_output=True, text=True).stdout.strip()
        if not left:
            break
        time.sleep(0.1)
    assert left == "", (left, r["reason"])


# --- I.12-engine (E-kimi-task-376) ------------------------------------------

def test_emit_engine_i12_emission_pinned():
    # The engine's own I.12 steps emit as the Book proof plus the two
    # engine-only SystemE axioms as euclid_apply (Book precedent for
    # euclid_apply on an axiom: Prop11.lean:20,37 applies perpendicular_if).
    steps = cert.engine_steps("I.12")
    src = cert.emit_proof("I.12-engine", steps, "h")
    assert "  euclid_apply (intersection_circle_line_1 d c EFG AB)\n" in src
    assert "  euclid_apply (perpendicular_if e g h c AB)\n" in src
    assert "  euclid_apply (intersections_circle_line EFG AB) as (e, g)\n" in src
    assert "  use h\n" in src
    import hashlib
    assert hashlib.sha256(src.encode()).hexdigest()[:16] == "d2261316dae9cd24"


def test_engine_i12_steps_come_from_engine_data(monkeypatch):
    # mutate the engine's live PROP12_STEPS_DATA and show the emission follows
    eng = cert._engine()
    swapped = [list(s) for s in eng.PROP12_STEPS_DATA]
    swapped[4] = ("proposition_10", ("e", "c", "AB"), {"d": "h"})
    monkeypatch.setattr(eng, "PROP12_STEPS_DATA", swapped)
    src = cert.emit_proof("I.12-engine", cert.engine_steps("I.12"), "h")
    assert "(proposition_10 e c AB) as h" in src
    assert "(proposition_10 e g AB) as h" not in src


@pytest.mark.skipif(not LEAN_AVAILABLE, reason="LeanEuclid not available")
def test_engine_i12_certifies():
    # the ENGINE's own PROP12_STEPS_DATA certified against LeanEuclid
    r = cert.check_lean(
        cert.emit_proof("I.12-engine", cert.engine_steps("I.12"), "h"),
        "test_engine_I12")
    assert r["certified"], r["output"]


@pytest.mark.skipif(not LEAN_AVAILABLE, reason="LeanEuclid not available")
def test_negative_engine_i12_perpendicular_if_wrong_foot():
    # perpendicular_if with foot e (not h): claims between e e g — false.
    # Must NOT certify, and no lean process may survive the timeout.
    steps = [("perpendicular_if", ("e", "g", "e", "c", "AB"), b)
             if r_ == "perpendicular_if" else (r_, a, b)
             for (r_, a, b) in cert.engine_steps("I.12")]
    r = cert.check_lean(cert.emit_proof("I.12-engine", steps, "h"),
                        "test_neg_I12_foot", timeout=120)
    assert not r["certified"], r
    import time
    for _ in range(50):
        left = subprocess.run(["pgrep", "-f", "test_neg_I12_foot.lean"],
                              capture_output=True, text=True).stdout.strip()
        if not left:
            break
        time.sleep(0.1)
    assert left == "", (left, r["reason"])



def test_book_emission_i1_to_i5_unchanged():
    # Review (claude-9): the first version only counted euclid_intros, which
    # any emission change passes. Pin the exact text: sha256 prefixes of the
    # Book I.1-I.5 emission, checked equal to the emitter before the engine
    # path existed (HEAD 18f28d8's euclid_certify.py, emitted side by side).
    import hashlib
    pins = {"I.1": "3ab49c6eb35a0fda", "I.2": "fd0f8cb642ab2ebd",
            "I.3": "ed76e074b17775f9", "I.4": "40bc26d1cde59686",
            "I.5": "6231e666b3c26e7a"}
    for prop, pin in pins.items():
        cfg = cert.PROPS[prop]
        src = cert.emit_proof(prop, cfg["book_steps"], cfg["witness"])
        assert hashlib.sha256(src.encode()).hexdigest()[:16] == pin, prop


@pytest.mark.skipif(not LEAN_AVAILABLE, reason="LeanEuclid not available")
def test_negative_prop4_false_assert():
    # asserting b' = d (not true of the superposed triangle) is refused.
    # Measured: the SMT solver hangs on the false assert, so this comes back
    # as a timeout refusal, not an error line — both are accepted, and the
    # reason records which happened.
    steps = [("euclid_assert", ("b' = d",), {}) if i == 1 else s
             for i, s in enumerate(cert.PROP4_BOOK_STEPS)]
    r = cert.check_lean(cert.emit_proof("I.4", steps, None), "test_neg_prop4",
                        timeout=180)
    assert not r["certified"]
    assert ("timeout after 180 s" in r["reason"]
            or "error" in r["reason"].lower())


@pytest.mark.skipif(not LEAN_AVAILABLE, reason="LeanEuclid not available")
def test_negative_prop5_drop_proposition_3():
    # dropping step 2 (proposition_3) leaves `g` unbound: Lean refuses with
    # an unknown-identifier error
    steps = cert.PROP5_BOOK_STEPS[:1] + cert.PROP5_BOOK_STEPS[2:]
    r = cert.check_lean(cert.emit_proof("I.5", steps, None), "test_neg_prop5")
    assert not r["certified"]
    assert "unknown identifier 'g'" in r["reason"]


@pytest.mark.skipif(not LEAN_AVAILABLE, reason="LeanEuclid not available")
def test_found_sequence_i1_certifies():
    """The I.1 sequence found by uninformed search, emitted and certified
    (fresh '$' names renamed). I.2/I.3 found runs are exercised by the CLI
    (`python3 scripts/euclid_certify.py found`); I.2's search budget makes it
    too slow for the unit suite."""
    spec2 = importlib.util.spec_from_file_location(
        "euclid_search", Path(__file__).resolve().parents[1] / "scripts/euclid_search.py")
    srch = importlib.util.module_from_spec(spec2)
    spec2.loader.exec_module(srch)
    kw = srch.prop_searches()["I.1"]
    found, path, _ = srch.search(**kw)
    assert found, "I.1 search unexpectedly failed"
    srch.verify_path(kw["hyps"], path, kw["goal"], kw.get("extra_la"))
    witness = list(path[-1][2].values())[-1]
    r = cert.check_lean(cert.emit_proof("I.1", path, witness), "test_found_I1")
    assert r["certified"], r["output"]



def test_check_lean_timeout_is_not_certified(monkeypatch):
    # review (claude-9): a false step can leave the SMT search running past
    # the timeout; that must come back as a refusal, not an exception
    killed = []
    monkeypatch.setattr(cert.subprocess, "Popen", _FakePopen(hang=True))
    monkeypatch.setattr(cert.os, "killpg", lambda pid, sig: killed.append(pid))
    r = cert.check_lean("-- anything", "test_timeout", timeout=7)
    assert r["certified"] is False
    assert r["reason"] == "timeout after 7 s"
    assert killed == [-1]          # the whole process group was killed


@pytest.mark.skipif(not LEAN_AVAILABLE, reason="LeanEuclid not available")
def test_negative_prop5_swapped_sas_times_out_and_leaves_no_lean():
    # review (claude-9): proposition_4 a c f a g b (f and c swapped) claims
    # triangle acf ~ agb, which needs |ac| = |ag|: false. Lean's SMT search
    # does not refute it within 10 min; it must not certify, and no lean
    # process may survive the timeout (before the fix one did).
    steps = list(cert.PROP5_BOOK_STEPS)
    rule, args, b = steps[4]
    args = list(args); args[1], args[2] = args[2], args[1]
    steps[4] = (rule, tuple(args), b)
    r = cert.check_lean(cert.emit_proof("I.5", steps, None),
                        "test_neg_prop5_swap", timeout=30)
    assert not r["certified"], r
    # SIGKILL delivery is asynchronous: allow a few seconds for the group to
    # go (one suite run saw a failure here that did not reproduce in 3 reruns)
    import time
    for _ in range(50):
        left = subprocess.run(["pgrep", "-f", "test_neg_prop5_swap.lean"],
                              capture_output=True, text=True).stdout.strip()
        if not left:
            break
        time.sleep(0.1)
    assert left == "", (left, r["reason"])


# --- I.13-engine (E-kimi-task-381) ------------------------------------------

def test_emit_engine_i13_emission_pinned():
    # The engine's nested by_cases tree emits as nested Lean by_cases blocks:
    # outer split on the angle equality, inner on a.sameSide c BE; the mirror
    # branch's case assumption is a CHECKED euclid_assert; the derived
    # opposing_same_side_trans emits as an assert of the fact its consumer
    # (triple_incidence_2) needs; every branch ends with euclid_finish.
    src = cert.emit_proof13()
    assert "  by_cases ∠ c:b:a = ∠ a:b:d\n" in src
    assert "    by_cases a.sameSide c BE\n" in src
    assert "  . euclid_apply (perpendicular_if c d b a CD)\n" in src
    assert "euclid_apply (proposition_11' d c b a CD) as e\n" in src
    assert "euclid_apply (pasch_3 d b c BE)\n" in src
    assert "euclid_assert ¬(d.sameSide a BE)\n" in src
    assert "    . euclid_assert a.sameSide d BE\n" in src
    assert "euclid_apply (pasch_3 c b d BE)\n" in src
    assert "euclid_assert ¬(c.sameSide a BE)\n" in src
    assert src.count("euclid_finish") == 3  # one per leaf branch
    import hashlib
    assert hashlib.sha256(src.encode()).hexdigest()[:16] == "52be0771bab15180"


def test_engine_i13_steps_come_from_engine_data(monkeypatch):
    # mutate the engine's live PROP13_STEPS_DATA and show the emission follows
    eng = cert._engine()
    data = eng.PROP13_STEPS_DATA
    _, branches, conclusions = data[0]
    facts, steps = branches[0]
    mutated = (facts, [("perpendicular_if", ("c", "d", "b", "a", "AB"), b)
                       if r_ == "perpendicular_if" else (r_, a, b)
                       for (r_, a, b) in steps])
    monkeypatch.setattr(eng, "PROP13_STEPS_DATA",
                        [("by_cases", [mutated] + branches[1:], conclusions)])
    src = cert.emit_proof13()
    assert "(perpendicular_if c d b a AB)" in src
    assert "(perpendicular_if c d b a CD)" not in src


@pytest.mark.skipif(not LEAN_AVAILABLE, reason="LeanEuclid not available")
def test_engine_i13_certifies():
    # the ENGINE's own PROP13_STEPS_DATA (nested by_cases) certified against
    # LeanEuclid
    r = cert.check_lean(cert.emit_proof13(), "test_engine_I13")
    assert r["certified"], r["output"]


@pytest.mark.skipif(not LEAN_AVAILABLE, reason="LeanEuclid not available")
def test_negative_engine_i13_false_case_fact():
    # The packet's first bad-case suggestion (drop branch-1 perpendicular_if)
    # still certifies: euclid_finish closes that branch alone, as in
    # Book/Prop13.lean:12. So: assert a FALSE case fact — the mirror branch
    # claims a.sameSide c BE, contradicting the by_cases negative-branch
    # hypothesis. Must NOT certify (the SMT solver answers unknown ->
    # timeout), and no lean process may survive.
    src = cert.emit_proof13()
    bad = src.replace("euclid_assert a.sameSide d BE",
                      "euclid_assert a.sameSide c BE")
    assert bad != src
    r = cert.check_lean(bad, "test_neg_I13_casefact", timeout=120)
    assert not r["certified"], r
    import time
    for _ in range(50):
        left = subprocess.run(["pgrep", "-f", "test_neg_I13_casefact.lean"],
                              capture_output=True, text=True).stdout.strip()
        if not left:
            break
        time.sleep(0.1)
    assert left == "", (left, r["reason"])


# --- I.14-engine (E-kimi-task-387) ------------------------------------------

def test_emit_engine_i14_emission_pinned():
    # The engine's outer by_contra emits as Lean's `by_contra`; the nested
    # engine by_contra over same_side/opposing_sides a e BD emits as
    # `by_cases a.sameSide e BD`; the derived opposing_opposing_same_side
    # emits as a CHECKED euclid_assert of its conclusion; branch 2's
    # opposing_sides fact is dropped (Lean's negative branch supplies
    # exactly ¬(a.sameSide e BD), triple_incidence_2's premise shape);
    # each by_cases branch ends with euclid_finish, and there is no
    # trailing finish after the by_cases (the branches close the False
    # goal by_contra installed).
    src = cert.emit_proof14()
    assert "  by_contra\n" in src
    assert "  by_cases a.sameSide e BD\n" in src
    assert "  euclid_apply (extend_point BC c b) as e\n" in src
    assert "  euclid_apply (between_symm c b e)\n" in src
    assert "  euclid_apply (pasch_3 c b e AB)\n" in src
    assert "  euclid_assert d.sameSide e AB\n" in src
    assert "  euclid_apply (proposition_13 a b e c AB BC)\n" in src
    assert "  . euclid_apply (sum_angles_onlyif b a d e AB BD)\n" in src
    assert "  . euclid_apply (triple_incidence_2 AB BD BC b a d e)\n" in src
    assert "    euclid_apply (sum_angles_onlyif b a e d AB BC)\n" in src
    assert "opposingSides a e BD" not in src  # branch-2 fact dropped
    assert src.count("euclid_finish") == 2  # one per by_cases branch
    assert not src.rstrip().endswith("euclid_finish")
    import hashlib
    assert hashlib.sha256(src.encode()).hexdigest()[:16] == "aa4b6085a2d3b502"


def test_engine_i14_steps_come_from_engine_data(monkeypatch):
    # mutate the engine's live PROP14_STEPS_DATA and show the emission follows
    eng = cert._engine()
    (kind, branches, conclusions), = eng.PROP14_STEPS_DATA
    (facts, steps), = branches
    mutated = (facts, [(s[0], tuple("BD" if a == "AB" else a for a in s[1]),
                        s[2])
                       if s[0] == "pasch_3_opposing" else s
                       for s in steps])
    monkeypatch.setattr(eng, "PROP14_STEPS_DATA",
                        [(kind, [mutated], conclusions)])
    src = cert.emit_proof14()
    assert "(pasch_3 c b e BD)" in src
    assert "(pasch_3 c b e AB)" not in src


@pytest.mark.skipif(not LEAN_AVAILABLE, reason="LeanEuclid not available")
def test_engine_i14_certifies():
    # the ENGINE's own PROP14_STEPS_DATA (by_contra + case split) certified
    # against LeanEuclid
    r = cert.check_lean(cert.emit_proof14(), "test_engine_I14")
    assert r["certified"], r["output"]


@pytest.mark.skipif(not LEAN_AVAILABLE, reason="LeanEuclid not available")
def test_negative_engine_i14_false_asserted_fact():
    # The packet's bad case: replace the DERIVED same-side assert
    # (opposing_opposing_same_side's conclusion d.sameSide e AB) with the
    # false e.sameSide c AB — c and e are on OPPOSING sides of AB
    # (pasch_3). Must NOT certify (the SMT solver answers unknown ->
    # timeout), and no lean process may survive.
    src = cert.emit_proof14()
    bad = src.replace("euclid_assert d.sameSide e AB",
                      "euclid_assert e.sameSide c AB")
    assert bad != src
    r = cert.check_lean(bad, "test_neg_I14_assert", timeout=120)
    assert not r["certified"], r
    import time
    for _ in range(50):
        left = subprocess.run(["pgrep", "-f", "test_neg_I14_assert.lean"],
                              capture_output=True, text=True).stdout.strip()
        if not left:
            break
        time.sleep(0.1)
    assert left == "", (left, r["reason"])


@pytest.mark.skipif(not LEAN_AVAILABLE, reason="LeanEuclid not available")
def test_negative_engine_i14_no_by_contra():
    # My own bad case: drop `by_contra` — the goal stays BC = BD and the
    # by_cases branches' euclid_finish (closing False) cannot close it.
    # Must NOT certify, and no lean process may survive.
    src = cert.emit_proof14()
    bad = src.replace("  by_contra\n", "")
    assert bad != src
    r = cert.check_lean(bad, "test_neg_I14_nocontra", timeout=120)
    assert not r["certified"], r
    import time
    for _ in range(50):
        left = subprocess.run(["pgrep", "-f", "test_neg_I14_nocontra.lean"],
                              capture_output=True, text=True).stdout.strip()
        if not left:
            break
        time.sleep(0.1)
    assert left == "", (left, r["reason"])
