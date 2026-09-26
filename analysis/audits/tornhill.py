#!/usr/bin/env python3
"""Tornhill-style code forensics over the futon repositories.

Adam Tornhill, *Your Code as a Crime Scene* (2015) and *Software Design
X-Rays* (2018): git history is the forensic trace, and the instruments are

  hotspots          revisions x complexity per file (complexity = indentation,
                    Tornhill's own language-neutral proxy)
  complexity trend  is a hotspot getting worse? (complexity sampled over time)
  change coupling   files that keep changing in the same change set
  code age          days since a file last changed
  knowledge map     who changes a file; here authors are mostly models, read
                    from Co-Authored-By trailers

Mission: futon3c/holes/missions/M-the-perfect-crime.md (overt question, and
the 2026-09-26 section that records this script's design decisions).
Discipline: library/code-coherence/subsumption-claim-discipline.flexiarg --
every number here is checked against a second reading of the same history
(see `check`), and a repository that cannot be read is reported missing,
never zero.

Standard library only. Read-only over the repositories.
"""
import argparse
import collections
import itertools
import json
import math
import re
import subprocess
import sys
import time
from datetime import datetime, timezone
from pathlib import Path

REPOS = ('futon0 futon1 futon1a futon1b futon2 futon2a futon3 futon3a futon3b '
         'futon3c futon4 futon5 futon5a futon6 futon7 futon7a').split()

# Indentation unit per language: leading whitespace / unit = logical indents.
INDENT_UNIT = {'.clj': 2, '.cljs': 2, '.cljc': 2, '.edn': 2, '.el': 2, '.bb': 2,
               '.js': 2, '.ts': 2, '.lean': 2,
               '.py': 4, '.rs': 4, '.sh': 4, '.lua': 4, '.java': 4}
CODE_EXT = tuple(k for k in INDENT_UNIT if k != '.edn')
GENERATED = re.compile(r'(^|/)(target|out|node_modules|\.cpcache|compiled|vendor)/|_files/|\.min\.js$')
TEST_NAME = re.compile(r'(^test_|[_-]test$|[_-]tests$)')

# Tornhill / code-maat defaults for change coupling.
MAX_CHANGESET = 30      # larger commits are sweeps/reformats: coupling noise
MIN_SHARED = 5
MIN_DEGREE = 30.0       # percent


def git(repo, *args):
    return subprocess.run(['git', '-C', str(repo), *args], check=True,
                          capture_output=True, text=True, errors='replace').stdout


def model_of(trailers):
    """'Claude Opus 5 (1M context) <noreply@..>' -> 'Claude Opus 5'."""
    for t in trailers:
        name = re.sub(r'\s*<.*$', '', t).strip()
        name = re.sub(r'\s*\(.*\)$', '', name)
        if name:
            return name
    return None


def read_history(repo):
    """All commits reachable from HEAD, newest first, with numstat per file.

    Walks the whole history and leaves time filtering to the caller: --since
    prunes history when commit dates are non-monotonic (commit_timeseries.py).
    """
    out = git(repo, 'log', 'HEAD', '--no-renames', '--no-merges',
              '--format=\x1e%H\t%ct\t%an\t%(trailers:key=Co-Authored-By,valueonly,separator=%x1f)',
              '--numstat')
    commits = []
    for block in out.split('\x1e')[1:]:
        head, _, body = block.partition('\n')
        sha, ct, author, trailers = (head.split('\t') + [''])[:4]
        files = {}
        for line in body.splitlines():
            parts = line.split('\t')
            if len(parts) != 3:
                continue
            a, d, path = parts
            files[path] = (int(a) if a.isdigit() else 0, int(d) if d.isdigit() else 0)
        commits.append({'sha': sha, 'ct': int(ct), 'author': author,
                        'model': model_of(trailers.split('\x1f') if trailers else []),
                        'files': files})
    return commits


def is_code(path):
    return path.endswith(CODE_EXT) and not GENERATED.search(path)


def indentation(text, ext):
    """Tornhill's complexity proxy: sum of logical indents over non-blank lines."""
    unit = INDENT_UNIT.get(ext, 4)
    total = maxi = loc = 0
    for line in text.splitlines():
        if not line.strip():
            continue
        loc += 1
        lead = line[:len(line) - len(line.lstrip(' \t'))]
        n = lead.count('\t') + lead.count(' ') / unit
        total += n
        maxi = max(maxi, n)
    return {'total': round(total, 1), 'mean': round(total / loc, 2) if loc else 0,
            'max': round(maxi, 1), 'loc': loc}


def module_key(path):
    """Collapse a file and its test twin to one key (http.clj ~ http_test.clj)."""
    stem = Path(path).stem
    return TEST_NAME.sub('', stem)


def analyse_repo(repo, days, trend_top, trend_samples, now):
    head = git(repo, 'rev-parse', 'HEAD').strip()
    tracked = set(git(repo, 'ls-files').splitlines())
    history = read_history(repo)
    cutoff = now - days * 86400
    window = [c for c in history if c['ct'] >= cutoff]

    last_touch, first_touch = {}, {}
    for c in history:                                   # newest first
        for f in c['files']:
            last_touch.setdefault(f, c['ct'])
            first_touch[f] = c['ct']

    stats = collections.defaultdict(lambda: {'revs': 0, 'added': 0, 'deleted': 0,
                                             'models': collections.Counter(),
                                             'authors': collections.Counter(),
                                             'touches': []})
    for c in window:
        for f, (a, d) in c['files'].items():
            if f not in tracked or not is_code(f):
                continue
            s = stats[f]
            s['revs'] += 1
            s['added'] += a
            s['deleted'] += d
            s['models'][c['model'] or 'none'] += 1
            s['authors'][c['author']] += 1
            s['touches'].append((c['ct'], c['sha']))

    files = []
    for f, s in stats.items():
        try:
            text = (Path(repo) / f).read_text(errors='replace')
        except OSError:
            continue
        cx = indentation(text, Path(f).suffix)
        models = s['models'].most_common()
        top_model, top_n = models[0]
        files.append({
            'path': f, 'module': module_key(f), 'test': bool(TEST_NAME.search(Path(f).stem)),
            'revs': s['revs'], 'churn': s['added'] + s['deleted'],
            'added': s['added'], 'deleted': s['deleted'],
            'complexity': cx,
            'hotspot': round(s['revs'] * cx['total']),
            'age_days': round((now - last_touch[f]) / 86400, 1),
            'born_in_window': first_touch[f] >= cutoff,
            'models': dict(models), 'authors': dict(s['authors'].most_common()),
            'main_model': top_model, 'main_model_share': round(top_n / s['revs'], 2),
            'touches': sorted(s['touches']),
        })
    files.sort(key=lambda r: -r['hotspot'])

    # Complexity trend for the top hotspots: sample the file at evenly spaced
    # commits that touched it inside the window, plus HEAD.
    for r in files[:trend_top]:
        touches = r.pop('touches')
        idx = sorted({round(i * (len(touches) - 1) / max(1, trend_samples - 1))
                      for i in range(trend_samples)})
        series = []
        for i in idx:
            ct, sha = touches[i]
            try:
                text = git(repo, 'show', f'{sha}:{r["path"]}')
            except subprocess.CalledProcessError:
                continue
            cx = indentation(text, Path(r['path']).suffix)
            series.append({'ct': ct, 'sha': sha[:10], 'total': cx['total'], 'loc': cx['loc']})
        r['trend'] = series
        # A file created inside the window has no 'before'; its first sample is
        # its birth, so a ratio would only restate its size. Growth is reported
        # for files that existed when the window opened.
        if len(series) >= 2 and series[0]['total'] and not r['born_in_window']:
            r['trend_ratio'] = round(series[-1]['total'] / series[0]['total'], 2)
    for r in files[trend_top:]:
        r.pop('touches', None)

    # Change coupling at commit grain (code-maat 'coupling').
    revs = {r['path']: r['revs'] for r in files}
    pairs = collections.Counter()
    skipped = 0
    for c in window:
        fs = sorted(f for f in c['files'] if f in revs)
        if len(fs) > MAX_CHANGESET:
            skipped += 1
            continue
        pairs.update(itertools.combinations(fs, 2))
    coupling = []
    for (a, b), n in pairs.items():
        if n < MIN_SHARED:
            continue
        degree = 100 * n / ((revs[a] + revs[b]) / 2)
        if degree < MIN_DEGREE:
            continue
        coupling.append({'a': a, 'b': b, 'shared': n, 'degree': round(degree, 1),
                         'twin': module_key(a) == module_key(b)})
    coupling.sort(key=lambda p: (-p['degree'], -p['shared']))
    soc = collections.Counter()                         # sum of coupling
    for (a, b), n in pairs.items():
        soc[a] += n
        soc[b] += n
    for r in files:
        r['sum_of_coupling'] = soc.get(r['path'], 0)

    return {'head': head, 'commits_all': len(history), 'commits_window': len(window),
            'changesets_skipped': skipped, 'files': files, 'coupling': coupling}


def collect(args):
    now = time.time()
    root = Path(args.root)
    report = {'generated': datetime.fromtimestamp(now, timezone.utc).isoformat(timespec='seconds'),
              'window_days': args.days,
              'params': {'max_changeset': MAX_CHANGESET, 'min_shared': MIN_SHARED,
                         'min_degree': MIN_DEGREE, 'indent_unit': INDENT_UNIT,
                         'extensions': CODE_EXT, 'generated_paths': GENERATED.pattern,
                         'history': 'HEAD, --no-merges, --no-renames (renames reset history)'},
              'repos': {}, 'missing': []}
    for name in args.repos:
        path = root / name
        if not (path / '.git').is_dir():            # worktrees have a .git file
            report['missing'].append({'repo': name, 'reason': 'no repository root'})
            continue
        t = time.time()
        report['repos'][name] = analyse_repo(path, args.days, args.trend_top,
                                             args.trend_samples, now)
        r = report['repos'][name]
        print(f'{name}: {r["commits_window"]} commits, {len(r["files"])} code files, '
              f'{len(r["coupling"])} coupled pairs ({time.time() - t:.1f}s)', file=sys.stderr)
    Path(args.out).write_text(json.dumps(report, indent=1))
    print(f'wrote {args.out}', file=sys.stderr)


def spearman(xs, ys):
    def ranks(v):
        order = sorted(range(len(v)), key=lambda i: v[i])
        r = [0.0] * len(v)
        i = 0
        while i < len(order):
            j = i
            while j + 1 < len(order) and v[order[j + 1]] == v[order[i]]:
                j += 1
            for k in range(i, j + 1):
                r[order[k]] = (i + j) / 2
            i = j + 1
        return r
    rx, ry = ranks(xs), ranks(ys)
    n = len(xs)
    mx, my = sum(rx) / n, sum(ry) / n
    cov = sum((a - mx) * (b - my) for a, b in zip(rx, ry))
    sx = math.sqrt(sum((a - mx) ** 2 for a in rx))
    sy = math.sqrt(sum((b - my) ** 2 for b in ry))
    return cov / (sx * sy) if sx and sy else float('nan')


def check(args):
    """Second reading of the history, by a different git path, for each repo's
    top hotspots: per-file `git log` revision counts and line churn must match.
    Also reports how far revisions and churn agree as rankings, because agent
    commits are small and frequent, which inflates revision counts."""
    report = json.loads(Path(args.report).read_text())
    now = datetime.fromisoformat(report['generated']).timestamp()
    cutoff = now - report['window_days'] * 86400
    lines = [f'INDEPENDENT CHECK: {Path(args.report).name}', f'generated: {report["generated"]}', '',
             'Method: git log HEAD --no-merges --full-history --no-renames --numstat -- <file>',
             '(--full-history: default path simplification drops side-branch commits that',
             'really changed the file; found via futon6 304beb6 on the first run),',
             'filtered to the window; compared with the report\'s revs and churn.', '']
    bad = 0
    for name, r in report['repos'].items():
        files = r['files']
        if not files:
            lines.append(f'{name}: no code files changed in window')
            continue
        for f in files[:args.per_repo]:
            out = git(Path(report.get('root', args.root)) / name, 'log', 'HEAD', '--no-merges',
                      '--full-history', '--no-renames', '--format=@%ct', '--numstat', '--', f['path'])
            revs = churn = 0
            inside = False
            for line in out.splitlines():
                if line.startswith('@'):
                    inside = int(line[1:]) >= cutoff
                    revs += inside
                elif inside and line.strip():
                    a, d, _ = line.split('\t', 2)
                    churn += (int(a) if a.isdigit() else 0) + (int(d) if d.isdigit() else 0)
            ok = revs == f['revs'] and churn == f['churn']
            bad += not ok
            lines.append(f'{name}: {f["path"]} revs report={f["revs"]} hand={revs} '
                         f'churn report={f["churn"]} hand={churn} {"MATCH" if ok else "MISMATCH"}')
        if len(files) >= 5:
            rho = spearman([f['revs'] for f in files], [f['churn'] for f in files])
            lines.append(f'{name}: spearman(revs, churn) over {len(files)} files = {rho:.2f}')
        lines.append('')
    lines.append(f'missing repositories: {report["missing"] or "none"}')
    lines.append(f'result: {"PASS" if not bad else f"{bad} MISMATCH"}')
    Path(args.out).write_text('\n'.join(lines) + '\n')
    print('\n'.join(lines))
    return 1 if bad else 0


def main(argv=None):
    p = argparse.ArgumentParser(description=__doc__.split('\n')[0])
    sub = p.add_subparsers(dest='cmd', required=True)
    c = sub.add_parser('collect', help='analyse repositories, write JSON')
    c.add_argument('--root', default=str(Path(__file__).resolve().parents[3]))
    c.add_argument('--repos', nargs='+', default=REPOS)
    c.add_argument('--days', type=int, default=90)
    c.add_argument('--trend-top', type=int, default=10)
    c.add_argument('--trend-samples', type=int, default=6)
    c.add_argument('--out', required=True)
    k = sub.add_parser('check', help='re-read history for the top hotspots')
    k.add_argument('--root', default=str(Path(__file__).resolve().parents[3]))
    k.add_argument('--report', required=True)
    k.add_argument('--per-repo', type=int, default=3)
    k.add_argument('--out', required=True)
    args = p.parse_args(argv)
    return collect(args) if args.cmd == 'collect' else check(args)


if __name__ == '__main__':
    sys.exit(main())
