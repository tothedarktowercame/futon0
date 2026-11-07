"""Tests for euclid_certify.py — LeanEuclid certification of engine sequences.
Run from ~/code/futon0: python3 -m pytest scripts/euclid_certify_test.py -q

Integration tests invoke `lake env lean` against ~/code/LeanEuclid (read-only
except the tmp-certify/ scratch dir, which is not committed). They are skipped
when LeanEuclid or its built oleans are absent.
"""

import importlib.util
from pathlib import Path
import shutil
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


def test_check_lean_rule_catches_sorry_at_rc0(monkeypatch):
    # the exit-0 trap the review flagged: only the sorry check catches it
    def fake_run(*a, **kw):
        class P:
            returncode = 0
            stdout = "warning: declaration uses 'sorry'\n"
            stderr = ""
        return P()
    monkeypatch.setattr(cert.subprocess, "run", fake_run)
    r = cert.check_lean("x", "t")
    assert not r["certified"]
    assert "declaration uses 'sorry'" in r["reason"]


# --- Lean checking (integration) ---------------------------------------------

@pytest.mark.skipif(not LEAN_AVAILABLE, reason="LeanEuclid not available")
@pytest.mark.parametrize("prop", ["I.1", "I.2", "I.3"])
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
