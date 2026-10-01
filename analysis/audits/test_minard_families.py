"""Tests of the family breakdown: panel sums vs the main figure, and a hand-worked fixture."""
import json
import tempfile
from datetime import date
from pathlib import Path
import unittest

from edn_format import dumps, Keyword

from minard_families import build_data, build_families
from minard_operator_work import build_data as main_build_data, HERE

FULL = HERE / 'pattern-stage-joins-2026-10-01-fullwindow.jsonl'
LABELS = HERE / 'pattern-stages-2026-10-01.edn'


class FamilyPanelTests(unittest.TestCase):
    def test_family_daily_sums_match_main_figure_exactly(self):
        main = main_build_data(LABELS, FULL, HERE / 'FORENSIC-autopilot-2026-09-21.md',
                               HERE / 'pattern-stage-manifest-2026-10-01-fullwindow.json',
                               date(2026, 8, 22), date(2026, 10, 1))
        fam = build_data(FULL, LABELS)
        self.assertEqual(fam['days'], [d['date'] for d in main['days']])
        for panel in fam['panels']:
            stage = panel['stage']
            for i, day in enumerate(panel['days']):
                self.assertEqual(sum(day['counts'].values()), main['days'][i]['counts'][stage],
                                 f'{stage} {day["date"]}: families must sum to the stage daily count')
            self.assertEqual(sum(f['hits'] for f in panel['families']), main['totals'][stage])
            # trailing mean of the family sum equals the stage daily count
            for i in range(len(panel['days'])):
                w = range(max(0, i - 2), i + 1)
                self.assertAlmostEqual(sum(f['means'][i] for f in panel['families']),
                                       sum(main['days'][j]['counts'][stage] for j in w) / len(w))

    def test_three_day_fixture_panel_sums_and_other_grouping(self):
        # perceive: f1 -> day0 x2, day2 x1 ; f3 -> day0 x1
        # believe : f2 -> day1 x3 (before cutoff) + day1 x1 (at/after cutoff)
        # Hand result (top_n=1): perceive f1 [2,0,1], other(f3) [1,0,0], totals [3,0,1];
        # believe f2 [0,4,0], no other. Means use available days at the left edge.
        rows = [
            {'status': 'matched', 'pattern-id': 'f1/a', 'turn-at': '2026-08-22T09:00:00Z', 'retrieval-id': 'r1'},
            {'status': 'matched', 'pattern-id': 'f1/a', 'turn-at': '2026-08-22T10:00:00Z', 'retrieval-id': 'r2'},
            {'status': 'matched', 'pattern-id': 'f1/b', 'turn-at': '2026-08-24T09:00:00Z', 'retrieval-id': 'r3'},
            {'status': 'matched', 'pattern-id': 'f2/a', 'turn-at': '2026-08-23T09:00:00Z', 'retrieval-id': 'r4'},
            {'status': 'matched', 'pattern-id': 'f2/a', 'turn-at': '2026-08-23T09:30:00Z', 'retrieval-id': 'r5'},
            {'status': 'matched', 'pattern-id': 'f2/a', 'turn-at': '2026-08-23T11:00:00Z', 'retrieval-id': 'r6'},
            {'status': 'matched', 'pattern-id': 'f3/a', 'turn-at': '2026-08-22T12:00:00Z', 'retrieval-id': 'r7'},
            {'status': 'matched', 'pattern-id': 'f2/b', 'turn-at': '2026-08-23T12:00:00Z', 'retrieval-id': 'r8'},
            {'status': 'unmatched', 'pattern-id': None, 'turn-at': None, 'retrieval-id': 'r9'},
        ]
        doc = {Keyword('patterns'): [
            {Keyword('id'): pid, Keyword('stage'): Keyword(stage)}
            for pid, stage in [('f1/a', 'perceive'), ('f1/b', 'perceive'),
                               ('f2/a', 'believe'), ('f2/b', 'believe'), ('f3/a', 'perceive')]]}
        with tempfile.TemporaryDirectory() as directory:
            p = Path(directory)
            (p / 'joins.jsonl').write_text('\n'.join(json.dumps(r) for r in rows))
            (p / 'labels.edn').write_text(dumps(doc))
            fam = build_families(p / 'joins.jsonl', p / 'labels.edn',
                                 date(2026, 8, 22), date(2026, 8, 24), top_n=1)
        by_stage = {panel['stage']: panel for panel in fam['panels']}
        perceive, believe = by_stage['perceive'], by_stage['believe']
        self.assertEqual([f['name'] for f in perceive['families']], ['f1', '(other families)'])
        self.assertEqual(perceive['families'][0]['counts'], [2, 0, 1])
        self.assertEqual(perceive['families'][1]['counts'], [1, 0, 0])
        self.assertEqual(perceive['total'], 4)
        self.assertEqual([f['name'] for f in believe['families']], ['f2'])
        self.assertEqual(believe['families'][0]['counts'], [0, 4, 0])
        # hand-checked means: day0 = own value; day1 = mean of days 0-1; day2 = mean of days 0-2
        with tempfile.TemporaryDirectory() as directory:
            p = Path(directory)
            (p / 'joins.jsonl').write_text('\n'.join(json.dumps(r) for r in rows))
            (p / 'labels.edn').write_text(dumps(doc))
            data = build_data(p / 'joins.jsonl', p / 'labels.edn',
                              date(2026, 8, 22), date(2026, 8, 24),
                              cutoff2='2026-08-23T12:00:00Z', top_n=1)
        f1 = data['panels'][0]['families'][0]  # perceive/f1
        self.assertEqual(f1['means'], [2.0, 1.0, 1.0])
        other = data['panels'][0]['families'][1]
        self.assertEqual(other['means'], [1.0, 0.5, 1 / 3])
        # window split at the fixture cutoff: f1 before 2 / after 1; other before 1 / after 0
        self.assertEqual((f1['hitsBefore'], f1['hitsAfter']), (2, 1))
        self.assertEqual((other['hitsBefore'], other['hitsAfter']), (1, 0))
        self.assertEqual((data['panels'][1]['families'][0]['hitsBefore'],
                          data['panels'][1]['families'][0]['hitsAfter']), (3, 1))


if __name__ == '__main__':
    unittest.main()
