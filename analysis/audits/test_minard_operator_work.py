"""Tests of real input aggregation, smoothing, missing telemetry, and relabelling."""
from pathlib import Path
import tempfile
import unittest

from edn_format import dumps, Keyword, loads
from minard_operator_work import build_data, HERE


class MinardTests(unittest.TestCase):
    def build(self, labels):
        return build_data(labels, HERE / 'pattern-stage-joins-2026-09-21.jsonl',
                          HERE / 'FORENSIC-autopilot-2026-09-21.md',
                          HERE / 'pattern-stage-manifest-2026-09-21.json')

    def test_actual_data_and_relabel_change_the_integral(self):
        path = HERE / 'pattern-stages-2026-09-21.edn'
        baseline = self.build(path)
        self.assertEqual((baseline['hits'], baseline['turns']), (2882, 2879))
        self.assertEqual(sum(d['total'] for d in baseline['days']), 2882)
        self.assertEqual(baseline['totals']['assurance'], 689)
        for i, day in enumerate(baseline['days']):
            window = baseline['days'][max(0, i-2):i+1]
            for stage in baseline['stages']:
                self.assertAlmostEqual(day['mean'][stage], sum(d['counts'][stage] for d in window)/len(window))
        gaps = {g['rank']: g for g in baseline['gaps']}
        self.assertEqual(gaps[1]['tokens'], 831983803)
        self.assertTrue(gaps[20]['clipped'])
        self.assertIsNone(gaps[46]['tokens'])
        # Change one actual EDN stage and rerun against the real join ledger.
        doc = dict(loads(path.read_text()))
        patterns = [dict(r) for r in doc[Keyword('patterns')]]
        row = next(r for r in patterns if r[Keyword('id')] == 'memory/verify-in-the-serving-process')
        row[Keyword('stage')] = Keyword('act')
        doc[Keyword('patterns')] = patterns
        with tempfile.TemporaryDirectory() as directory:
            revised = Path(directory) / 'relabelled.edn'
            revised.write_text(dumps(doc))
            changed = self.build(revised)
        self.assertEqual(changed['totals']['assurance'], 689 - 66)
        self.assertEqual(changed['totals']['act'], 391 + 66)
        self.assertEqual(changed['hits'], baseline['hits'])
        self.assertNotEqual(changed['days'], baseline['days'])


if __name__ == '__main__':
    unittest.main()
