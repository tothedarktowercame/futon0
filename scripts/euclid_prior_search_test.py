#!/usr/bin/env python3
"""Tests for euclid_prior_search: leakage control and prior behaviour.

Run: python3 scripts/euclid_prior_search_test.py -v
(pytest is not installed in this environment; stdlib unittest is used.)
"""

import importlib.util
import re
import unittest
from pathlib import Path

_HERE = Path(__file__).resolve().parent


def _load(name, path):
    spec = importlib.util.spec_from_file_location(name, Path(path))
    m = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(m)
    return m


eps = _load("euclid_prior_search", _HERE / "euclid_prior_search.py")
motifs = _load("euclid_step_motifs", _HERE / "euclid_step_motifs.py")


def _prop_num(prop):
    m = re.match(r"proposition_(\d+)", prop)
    return int(m.group(1)) if m else 0


class LeakageTest(unittest.TestCase):
    def test_prior_for_prop3_uses_only_earlier_theorems(self):
        _, prov = eps.build_prior(3)
        for th in prov["theorems"]:
            n = eps.prop_number(th)
            self.assertIsNotNone(n)
            self.assertLess(n, 3, th)

    def test_prior_for_prop3_has_no_motif_supported_only_by_prop3_or_later(self):
        _, prov = eps.build_prior(3)
        allowed = {"proposition_1", "proposition_2"}
        for level in ("name", "class"):
            for pair, props in prov[level].items():
                self.assertLessEqual(set(props), allowed, (level, pair, props))

    def test_bad_case_full_mine_has_lateonly_motifs_absent_from_restricted(self):
        """The exact bad case the leakage control is named for: motifs whose
        ONLY support is proposition_3 or later exist in a full-book mine, and
        none of them contribute a nonzero count to the restricted I.3 prior."""
        full = motifs.mine(motifs.parse_book(), min_support=1)
        prior3, _ = eps.build_prior(3)
        late_only = set()
        for pair, info in full["chain"]["name"].items():
            parts = tuple(k.strip() for k in pair.split("->"))
            if len(parts) != 2:
                continue
            if info["propositions"] and all(
                    _prop_num(p) >= 3 for p in info["propositions"]):
                late_only.add(parts)
        self.assertTrue(late_only,
                        "expected at least one late-only motif in full mine")
        for parts in late_only:
            self.assertEqual(prior3["name_bigram"].get(parts, 0), 0, parts)


class PriorBehaviourTest(unittest.TestCase):
    def test_prior_for_prop2_ranks_circle_first_above_late_intersection(self):
        """I.2's prior is mined only from I.1, whose proof draws circles
        before intersecting them; the first-move score must reflect that."""
        prior, _ = eps.build_prior(2)
        self.assertGreater(
            eps.move_score(prior, None, "circle_from_points"),
            eps.move_score(prior, None, "intersection_circles"))

    def test_empty_prior_gives_zero_scores(self):
        prior, _ = eps.build_prior(1)  # no propositions before I.1
        for r in ("circle_from_points", "line_from_points",
                  "intersection_circles"):
            self.assertEqual(eps.move_score(prior, None, r), 0.0)

    def test_prior_search_i1_matches_uninformed_order(self):
        """With the empty prior (I.1) all scores are 0 and FIFO tie-breaking
        reproduces the uninformed BFS: same result, depth, node count."""
        kw = eps.es.prop_searches()["I.1"]
        f0, p0, s0 = eps.es.search(**kw)
        prior, _ = eps.build_prior(1)
        f1, p1, s1 = eps.search_prior(prior=prior, **kw)
        self.assertTrue(f0 and f1)
        self.assertEqual(len(p0), len(p1))
        self.assertEqual(s1["nodes"], s0["nodes"])

    def test_prior_search_i3_registers_first_move_misranking(self):
        """The regression, encoded: under the I.3 prior the first move
        circle_from_points outscores proposition_2' (I.1/I.2 draw circles
        first; I.3's Book proof cites I.2 first), so best-first ordering
        pushes the found-by-BFS solution aside. The search respects its
        budget; the mis-ranking is asserted at the prior level."""
        prior, _ = eps.build_prior(3)
        self.assertGreater(
            eps.move_score(prior, None, "circle_from_points"),
            eps.move_score(prior, None, "proposition_2'"))
        kw = dict(eps.es.prop_searches()["I.3"])
        kw["node_budget"] = 200
        kw["time_budget"] = 60.0
        found, path, st = eps.search_prior(prior=prior, **kw)
        self.assertLessEqual(st["nodes"], 200)

    def test_prior_search_i2_respects_small_budget(self):
        """Smoke: the I.2 run terminates within a small node budget (not the
        600 s production run) without crashing; leakage tested above."""
        kw = dict(eps.es.prop_searches()["I.2"])
        kw["node_budget"] = 300
        kw["time_budget"] = 20.0
        prior, _ = eps.build_prior(2)
        found, path, st = eps.search_prior(prior=prior, **kw)
        self.assertLessEqual(st["nodes"], 300)
        if found:
            eps.es.verify_path(kw["hyps"], path, kw["goal"])


class StructuralPriorTest(unittest.TestCase):
    def test_bonus_grows_with_recency(self):
        self.assertGreater(eps.structural_bonus("proposition_2'"),
                           eps.structural_bonus("proposition_1"))
        for r in ("line_from_points", "circle_from_points",
                  "intersection_circles"):
            self.assertEqual(eps.structural_bonus(r), 0.0)
        # parse works for higher cites too — but they must never be offered
        self.assertEqual(eps.structural_bonus("proposition_3"), 3.0)

    def test_i3_rule_universe_has_no_proposition_3_or_higher(self):
        """Leakage: the structural prior consults only the NAMES of
        propositions k < n; the rule list for I.3 offers proposition_1 and
        proposition_2' only, so nothing >= 3 can ever be proposed."""
        rules = eps.es.prop_searches()["I.3"]["rules"]
        for r in rules:
            m = re.match(r"proposition_(\d+)", r)
            if m:
                self.assertLess(int(m.group(1)), 3, r)

    def test_i3_structural_run_never_proposes_high_cites(self):
        """Watched bad case: wrap legal_moves and record every rule offered
        during a real (small-budget) structural run for I.3."""
        offered = []
        orig = eps.es.legal_moves

        def spy(s, rules):
            mv = orig(s, rules)
            offered.extend(r for r, _ in mv)
            return mv

        eps.es.legal_moves = spy
        try:
            kw = dict(eps.es.prop_searches()["I.3"])
            kw["node_budget"] = 50
            kw["time_budget"] = 60.0
            eps.search_prior(prior=eps.structural_prior(), **kw)
        finally:
            eps.es.legal_moves = orig
        self.assertTrue(offered)
        for r in set(offered):
            m = re.match(r"proposition_(\d+)", r)
            self.assertFalse(m and int(m.group(1)) >= 3, r)

    def test_structural_i1_matches_uninformed(self):
        """I.1's rules are axioms only, so the structural bonus is 0 for
        every move and the search reproduces the uninformed BFS."""
        kw = eps.es.prop_searches()["I.1"]
        f0, p0, s0 = eps.es.search(**kw)
        f1, p1, s1 = eps.search_prior(prior=eps.structural_prior(), **kw)
        self.assertTrue(f0 and f1)
        self.assertEqual(len(p0), len(p1))
        self.assertEqual(s1["nodes"], s0["nodes"])

    def test_combined_finds_i3_and_verifies(self):
        """The one positive result: motif + structural finds I.3 (the
        structural term rescues the proposition_2' first move that the
        motif prior alone starved). Slow (~45 s) — pinned deliberately."""
        kw = eps.es.prop_searches()["I.3"]
        mined, _ = eps.build_prior(3)
        comb = dict(mined)
        comb["structural"] = True
        found, path, st = eps.search_prior(prior=comb, **kw)
        self.assertTrue(found, st)
        eps.es.verify_path(kw["hyps"], path, kw["goal"], kw.get("extra_la"))


if __name__ == "__main__":
    unittest.main(verbosity=2)
