"""Tests for euclid_step_motifs.py — run from ~/code/futon0:

    python3 -m pytest scripts/euclid_step_motifs_test.py -q
"""

import importlib.util
from pathlib import Path
import pytest

spec = importlib.util.spec_from_file_location(
    "euclid_step_motifs",
    Path(__file__).resolve().parents[1] / "scripts/euclid_step_motifs.py")
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)

BOOK = Path.home() / "code" / "LeanEuclid" / "Book"


def test_proposition_5_steps_in_order():
    """Book/Prop05.lean's proposition_5 yields its euclid_apply steps in
    order with the right cited names (8 steps on disk; the requisition
    said 9 — see the requisition reply for the reconciliation)."""
    theorems = module.parse_theorems((BOOK / "Prop05.lean").read_text(),
                                     str(BOOK / "Prop05.lean"))
    names = [t["name"] for t in theorems]
    assert names[0] == "proposition_5"
    p5 = theorems[0]
    applies = [s for s in p5["steps"] if s["tactic"] == "euclid_apply"]
    assert [s["name"] for s in applies] == [
        "point_between_points_shorter_than",
        "proposition_3",
        "line_from_points",
        "line_from_points",
        "proposition_4",
        "proposition_4",
        "sum_angles_onlyif",
        "sum_angles_onlyif",
    ]
    # parenthesised and bare forms both parsed; bindings recorded
    prop3 = applies[1]
    assert prop3["bindings"] == ["g"]
    assert "AC" in prop3["uses"] and "f" in prop3["uses"]
    assert applies[4]["bindings"] == []  # bare-form proposition_4, no `as`


def test_book_euclid_apply_count_matches_557():
    theorems = module.parse_book(BOOK)
    count = sum(1 for t in theorems for s in t["steps"]
                if s["tactic"] == "euclid_apply")
    assert count == 557, f"expected 557 euclid_apply steps, parsed {count}"


def _fixture_text():
    """A synthetic book where the data-flow chain
       point_on_circle_onlyif -> circle_from_points -> arbitrary_point
       (in class terms: metric/angle/area -> construct-circle -> other)
       occurs in 3 theorems (with shuffled interleaving and distinct
       variable names), and the chain
       point_on_line_same_side -> intersection_lines -> line_from_points
       occurs in only 2 theorems and must NOT be reported."""
    parts = []
    # good chain, three theorems, different source orders.
    good_orders = [
        # order 1: producer-first
        ["euclid_apply (arbitrary_point a b) as x",
         "euclid_apply (circle_from_points x y) as c",
         "euclid_apply (point_on_circle_onlyif x y z c)"],
        # order 2: consumer of x written before circle, chain still by data flow
        ["euclid_apply (arbitrary_point m n) as u",
         "euclid_apply (point_on_circle_onlyif u w z cc)",
         "euclid_apply (circle_from_points u v) as cc"],
        # order 3: the using step comes first in source
        ["euclid_apply (point_on_circle_onlyif p q r k)",
         "euclid_apply (arbitrary_point p s) as k0",
         "euclid_apply (circle_from_points k0 t) as k"],
    ]
    for i, order in enumerate(good_orders, start=1):
        body = "\n".join("  " + ln for ln in order)
        parts.append(f"theorem planted{i} : goal :=\nby\n  euclid_intros\n{body}\n")
    # bad chain: only two theorems
    bad = ["euclid_apply (line_from_points l m) as lm",
           "euclid_apply (intersection_lines lm ln) as p",
           "euclid_apply (point_on_line_same_side p lm n)"]
    for i in (1, 2):
        body = "\n".join("  " + ln for ln in bad)
        parts.append(f"theorem sparse{i} : goal :=\nby\n  euclid_intros\n{body}\n")
    return "\n".join(parts)


def test_planted_chain_found_at_support_3_and_sparse_chain_excluded():
    theorems = module.parse_theorems(_fixture_text(), "<fixture>")
    assert len(theorems) == 5
    motifs = module.mine(theorems, min_support=3)
    # name level: each theorem uses distinct names, so no name-level chain
    # reaches support 3 — the class-level abstraction is what must fire.
    # Chains are directed producer-first: A binds what B uses, B binds what
    # C uses.
    good = "other -> construct-circle -> metric/angle/area"
    info = motifs["chain"]["class"].get(good)
    assert info is not None, f"planted chain {good!r} not reported"
    assert info["support"] == 3
    assert sorted(info["propositions"]) == ["planted1", "planted2", "planted3"]
    # the sparse chain (2 theorems only) must NOT be reported
    bad = "construct-line -> intersect -> metric/angle/area"
    assert motifs["chain"]["class"].get(bad) is None, (
        "chain occurring in only 2 propositions was reported — support "
        "threshold not enforced"
    )
    # and no chain motif of any shape from the sparse theorems survives
    for motif, mi in motifs["chain"]["class"].items():
        assert "sparse1" not in mi["propositions"] and \
               "sparse2" not in mi["propositions"], motif


def test_fixture_bad_case_actually_constructed():
    """Guard that the fixture really contains the bad chain as edges —
    the exclusion test above is only meaningful if the bad chain exists
    in the data at support 2."""
    theorems = module.parse_theorems(_fixture_text(), "<fixture>")
    sparse = [t for t in theorems if t["name"].startswith("sparse")]
    assert len(sparse) == 2
    for th in sparse:
        edges = module.dataflow_edges(th["steps"])
        assert edges, "sparse theorems unexpectedly have no def-use edges"
    motifs2 = module.mine(theorems, min_support=2)
    bad = "construct-line -> intersect -> metric/angle/area"
    info = motifs2["chain"]["class"].get(bad)
    assert info is not None and info["support"] == 2
