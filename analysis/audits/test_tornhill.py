"""Tests for tornhill.py and tornhill_chat.py on a synthetic repository and transcripts.

Each check is also fed the bad case it exists to catch: a tampered report must
fail `check`, a side-branch commit must still count, merges must not, and a
transcript row written after as_of must not be counted.
"""
import json
import os
import subprocess
import tempfile
import unittest
from pathlib import Path

import tornhill
import tornhill_chat

ENV = dict(os.environ, GIT_AUTHOR_NAME='t', GIT_AUTHOR_EMAIL='t@t', GIT_COMMITTER_NAME='t',
           GIT_COMMITTER_EMAIL='t@t')


def git(repo, *args, when=None):
    env = dict(ENV)
    if when is not None:
        env['GIT_AUTHOR_DATE'] = env['GIT_COMMITTER_DATE'] = f'@{when} +0000'
    return subprocess.run(['git', '-C', str(repo), *args], check=True, capture_output=True,
                          text=True, env=env).stdout


def commit(repo, files, msg, when, trailer=None):
    for name, text in files.items():
        path = Path(repo) / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text)
        git(repo, 'add', name)
    body = msg + (f'\n\nCo-Authored-By: {trailer}' if trailer else '')
    git(repo, 'commit', '-q', '-m', body, when=when)
    return git(repo, 'rev-parse', 'HEAD').strip()


class Unit(unittest.TestCase):
    def test_indentation_uses_language_unit(self):
        clj = '(defn f []\n  (let [x 1]\n    x))\n\n'
        py = 'def f():\n    if x:\n        return 1\n'
        self.assertEqual(tornhill.indentation(clj, '.clj')['total'], 3)   # 0+1+2
        self.assertEqual(tornhill.indentation(py, '.py')['total'], 3)     # 0+1+2
        self.assertEqual(tornhill.indentation(clj, '.clj')['loc'], 3)     # blank line skipped

    def test_test_twins_share_a_module_key(self):
        for a, b in [('src/x/http.clj', 'test/x/http_test.clj'),
                     ('emacs/session-mode.el', 'test/session-mode-test.el'),
                     ('scripts/cas.py', 'tests/test_cas.py')]:
            self.assertEqual(tornhill.module_key(a), tornhill.module_key(b))
        self.assertNotEqual(tornhill.module_key('a/zai_api.clj'),
                            tornhill.module_key('a/kimi_api_test.clj'))

    def test_model_trailer_normalised(self):
        self.assertEqual(tornhill.model_of(['Claude Opus 5 (1M context) <n@a.com>']), 'Claude Opus 5')
        self.assertIsNone(tornhill.model_of([]))

    def test_generated_paths_are_not_code(self):
        self.assertFalse(tornhill.is_code('lab/r03_cyberants_files/vega5.js'))
        self.assertFalse(tornhill.is_code('target/classes/x.clj'))
        self.assertTrue(tornhill.is_code('src/futon3c/transport/http.clj'))

    def test_operator_rule_matches_census(self):
        op = 'Surface: emacs-repl\nFrom: joe\nOrigin: operator\n'
        self.assertTrue(tornhill_chat.is_operator(op))
        self.assertFalse(tornhill_chat.is_operator(op + '--- resumed: parked ---'))
        self.assertFalse(tornhill_chat.is_operator(op + 'WAKE CHECKLIST'))
        self.assertFalse(tornhill_chat.is_operator('From: claude-8\nOrigin: agent'))

    def test_attribution_prefers_printed_sha_then_earlier(self):
        printed = {'match': 'sha-printed', 'at': '2026-09-10'}
        subject = {'match': 'subject+time', 'at': '2026-09-01'}
        later = {'match': 'sha-printed', 'at': '2026-09-12'}
        self.assertEqual(min([subject, later, printed], key=tornhill_chat.attribution_rank), printed)


class Repo(unittest.TestCase):
    """A repo with a hotspot, a coupled pair, a big sweep, a side branch and a merge."""

    @classmethod
    def setUpClass(cls):
        cls.tmp = tempfile.TemporaryDirectory()
        cls.root = Path(cls.tmp.name)
        r = cls.repo = cls.root / 'futonX'
        r.mkdir()
        git(r, 'init', '-q', '-b', 'main')
        import time
        now = int(time.time())
        old = now - 200 * 86400
        commit(r, {'src/old.clj': '(ns old)\n'}, 'old file', old)
        body = '(defn f []\n  (let [x 1]\n    x))\n'
        for i in range(6):                                      # a and b always together
            commit(r, {'src/a.clj': body * (i + 1), 'src/b.clj': f'(ns b) ;{i}\n'},
                   f'ab {i}', now - (60 - i) * 86400, trailer='Claude Fable 5 <n@a.com>')
        commit(r, {'src/a.clj': body * 9}, 'a alone', now - 40 * 86400)
        sweep = {f'src/s{i}.clj': '(ns s)\n' for i in range(35)}
        sweep['src/a.clj'] = body * 10
        sweep['src/b.clj'] = '(ns b) ;sweep\n'
        commit(r, sweep, 'sweep', now - 35 * 86400)
        git(r, 'checkout', '-q', '-b', 'side')
        cls.side = commit(r, {'src/c.clj': '(ns c)\n  x\n'}, 'side c', now - 30 * 86400)
        git(r, 'checkout', '-q', 'main')
        commit(r, {'src/d.clj': '(ns d)\n'}, 'main d', now - 29 * 86400)
        git(r, 'merge', '-q', '--no-ff', '-m', 'merge side', 'side', when=now - 28 * 86400)
        commit(r, {'src/old.clj': '(ns old)\n  (def y 2)\n'}, 'touch old', now - 10 * 86400)
        commit(r, {'src/old.clj': '(ns old)\n  (def y 2)\n  (def z 3)\n'}, 'grow old',
               now - 5 * 86400)
        cls.out = cls.root / 'report.json'
        tornhill.main(['collect', '--root', str(cls.root), '--repos', 'futonX', 'missing-repo',
                       '--days', '90', '--trend-top', '5', '--out', str(cls.out)])
        cls.report = json.loads(cls.out.read_text())
        cls.files = {f['path']: f for f in cls.report['repos']['futonX']['files']}

    @classmethod
    def tearDownClass(cls):
        cls.tmp.cleanup()

    def test_missing_repo_is_reported_not_zero(self):
        self.assertEqual(self.report['missing'][0]['repo'], 'missing-repo')
        self.assertNotIn('missing-repo', self.report['repos'])

    def test_revisions_exclude_merges_and_include_side_branch(self):
        self.assertEqual(self.files['src/a.clj']['revs'], 8)    # 6 + alone + sweep
        self.assertEqual(self.files['src/c.clj']['revs'], 1)    # side-branch commit counts once

    def test_hotspot_is_revisions_times_complexity(self):
        a = self.files['src/a.clj']
        self.assertEqual(a['hotspot'], round(a['revs'] * a['complexity']['total']))
        self.assertEqual(max(self.files.values(), key=lambda f: f['hotspot'])['path'], 'src/a.clj')

    def test_coupling_skips_sweeps(self):
        pairs = {(p['a'], p['b']): p for p in self.report['repos']['futonX']['coupling']}
        ab = pairs[('src/a.clj', 'src/b.clj')]
        self.assertEqual(ab['shared'], 6)                       # the 36-file sweep is not counted
        self.assertEqual(self.report['repos']['futonX']['changesets_skipped'], 1)
        self.assertFalse(any('src/s1.clj' in k for k in pairs))

    def test_trend_ratio_only_for_files_that_predate_window(self):
        self.assertTrue(self.files['src/a.clj']['born_in_window'])
        self.assertNotIn('trend_ratio', self.files['src/a.clj'])
        old = self.files['src/old.clj']
        self.assertFalse(old['born_in_window'])
        self.assertEqual(old['trend_ratio'], 2.0)   # 1 indent at the first touch, 2 at the last

    def test_check_passes_and_catches_a_tampered_report(self):
        good = self.root / 'good.txt'
        self.assertEqual(tornhill.main(['check', '--root', str(self.root), '--report', str(self.out),
                                        '--out', str(good)]), 0)
        bad = json.loads(self.out.read_text())
        bad['repos']['futonX']['files'][0]['revs'] += 1
        tampered = self.root / 'tampered.json'
        tampered.write_text(json.dumps(bad))
        self.assertEqual(tornhill.main(['check', '--root', str(self.root), '--report', str(tampered),
                                        '--out', str(self.root / 'bad.txt')]), 1)


class Transcript(unittest.TestCase):
    def rows(self, path, rows):
        # Compact separators, as Claude Code writes them; the reader pre-filters
        # lines on the literal '"type":"user"'.
        path.write_text('\n'.join(json.dumps(r, separators=(',', ':')) for r in rows) + '\n')

    def test_claude_reader_counts_census_rule_and_respects_as_of(self):
        with tempfile.TemporaryDirectory() as d:
            op = 'Surface: emacs-repl\nFrom: joe\nTo: claude-7\nOrigin: operator\n\nhello'
            rows = [
                {'type': 'user', 'uuid': 'u1', 'timestamp': '2026-09-20T10:00:00.000Z',
                 'message': {'content': op}},
                {'type': 'user', 'uuid': 'u2', 'timestamp': '2026-09-20T11:00:00.000Z',
                 'message': {'content': [{'type': 'text', 'text': 'From: codex-3\nTo: claude-7\nOrigin: agent'}]}},
                {'type': 'user', 'uuid': 'u3', 'timestamp': '2026-09-20T12:00:00.000Z',
                 'message': {'content': op + '\n--- resumed: parked ---'}},
                {'type': 'user', 'uuid': 'u4', 'timestamp': '2026-09-20T13:00:00.000Z',
                 'entrypoint': 'cli', 'message': {'content': 'typed directly'}},
                {'type': 'user', 'uuid': 'late', 'timestamp': '2026-09-26T00:00:00.000Z',
                 'message': {'content': op}},
            ]
            live = Path(d) / 's.jsonl'
            snap = Path(d) / 's.jsonl.pre-compact-1'
            self.rows(live, rows)
            self.rows(snap, rows[:1])                  # same uuid in a snapshot: counted once
            got = tornhill_chat.read_claude([str(live), str(snap)], {'u1'}, '2026-09-25T00:00:00.000Z')
            self.assertEqual(got['seat'], 'claude-7')
            self.assertEqual(got['operator_turns'], 1)  # not the park resume, not the late row
            self.assertEqual(got['bell_turns'], 1)
            self.assertEqual(got['direct_turns'], 1)
            self.assertEqual(got['compactions'], 1)
            self.assertEqual(got['labelled'], ['u1'])


if __name__ == '__main__':
    unittest.main()
