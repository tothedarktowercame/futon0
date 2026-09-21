"""Run with: uv run --with edn-format python -m unittest discover -s analysis/audits -p test_pattern_stages.py."""
import contextlib
import io
import json
from pathlib import Path
import tempfile
import unittest

import pattern_stage_evidence as evidence
import pattern_stage_review as review


class EvidenceJoinTests(unittest.TestCase):
    def test_real_join_excludes_resume_ambiguity_and_nonunique_rank(self):
        # These are actual serialized inputs to the production join, not mocked classification.
        def turn(identifier, text):
            return {"evidence/id": identifier, "evidence/session-id": "s",
                    "evidence/at": "2026-09-01T10:00:00Z",
                    "evidence/body": {"event": "chat-turn", "role": "user", "text": text}}
        def retrieval(identifier, query, results):
            return {"evidence/id": identifier, "evidence/session-id": "s",
                    "evidence/at": "2026-09-01T10:01:00Z",
                    "evidence/body": {"query": query, "results": results}}
        rank1 = [{"id": "p/one", "rank": 1}, {"id": "p/two", "rank": 2}]
        rows = [retrieval("good", "Build the thing", rank1),
                retrieval("resume", "--- resumed: parked dependencies complete", rank1),
                retrieval("ambiguous", "Yes", rank1),
                retrieval("invalid", "Review the thing", rank1 + [{"id": "p/other", "rank": 1}])]
        rows.append(rows[0].copy())  # Exact evidence duplicate must not add a hit.
        with tempfile.TemporaryDirectory() as directory:
            p = Path(directory)
            (p / "joe.json").write_text(json.dumps([
                turn("a", "Build the thing"), turn("b", "--- resumed: parked dependencies complete"),
                turn("c", "Yes"), turn("d", "Yes"), turn("e", "Review the thing")]))
            (p / "retrieval.json").write_text(json.dumps(rows))
            with contextlib.redirect_stdout(io.StringIO()):
                evidence.replay(p)
            self.assertEqual(json.loads((p / "hits.json").read_text()), {"p/one": 1})
            statuses = {r["retrieval-id"]: r["status"] for r in json.loads((p / "joins.json").read_text())}
            self.assertEqual(statuses, {"good": "matched", "resume": "park-resume-marker",
                                       "ambiguous": "ambiguous-text-prefix", "invalid": "invalid-rank1"})
            rows[-1]["evidence/body"] = {"query": "Different"}
            (p / "retrieval.json").write_text(json.dumps(rows))
            with self.assertRaisesRegex(ValueError, "Conflicting evidence"):
                evidence.replay(p)

    def test_operator_quoting_agent_header_is_not_agent_origin(self):
        text = "--- CURRENT TURN ---\nFrom: joe\nOrigin: operator\n---\nPlease review this:\nOrigin: agent"
        self.assertIsNone(evidence.exclusion(text))

    def test_embedded_edn_and_resume_envelope(self):
        self.assertEqual(evidence.body({"evidence/body": '{"results" [{:id "x" :rank 1}]}'}),
                         {"results": [{"id": "x", "rank": 1}]})
        self.assertEqual(evidence.exclusion("--- CURRENT TURN ---\nFrom: joe\nOrigin: operator\n---\n"
                                           "--- resumed: parked dependencies complete"), "park-resume-marker")
        self.assertEqual(evidence.exclusion("WAKE CHECKLIST: inspect the record"), "wake-payload")


class HumanReviewTests(unittest.TestCase):
    def test_no_agreement_until_complete_and_save_round_trip(self):
        key = {"a": {"kind": "practice", "stage": "act"}, "b": {"kind": "subject", "stage": "believe"}}
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "joe.json"
            labels = review.load_labels(path, key)
            self.assertFalse(path.exists())
            self.assertEqual(review.agreement_report(key, labels)["status"], "awaiting labels")
            labels["a"] = key["a"]
            review.save_labels(path, labels, key)
            self.assertEqual(review.load_labels(path, key), labels)
            self.assertEqual(review.agreement_report(key, labels)["complete"], 1)
            review.save_labels(path, key, key)
            report = review.agreement_report(key, review.load_labels(path, key))
            self.assertEqual(report["kind"]["kappa"], 1)
            self.assertEqual(report["stage"]["kappa"], 1)
            with self.assertRaises(ValueError):
                review.save_labels(path, {"wrong-id": key["a"]}, key)
            self.assertEqual(review.load_labels(path, key), key)

    def test_kappa_and_constant_marginals(self):
        result = review.confusion_and_kappa(["a", "a", "b", "b"], ["a", "b", "a", "b"], ("a", "b"))
        self.assertEqual(result["agreement"], 0.5)
        self.assertEqual(result["kappa"], 0)
        self.assertIsNone(review.confusion_and_kappa(["a"], ["a"], ("a", "b"))["kappa"])


if __name__ == "__main__":
    unittest.main()
