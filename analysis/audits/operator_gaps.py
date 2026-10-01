#!/usr/bin/env python3
"""Reproduce and extend the operator-gap / Codex-token table behind the
Minard figure's lower panel (02 WORK DURING RECORDED OPERATOR GAPS).

This replaces the hand-run procedure behind FORENSIC-autopilot-2026-09-21.md
("## All observed activity-bearing gaps") with a committed script, per
M-aif-over-the-operator.  It takes the window as parameters:

    python3 operator_gaps.py --start 2026-08-22 --end 2026-10-01T21:12:00Z

Gap rule (inferred from the report table and stated here as the settled rule):

  * Qualified operator rows are evidence GET from :7073 with author=joe,
    body.event=chat-turn and body.role=user, plus Marimo rows with
    transport=marimo and direction=inbound.  Rows whose text contains the
    literal '--- resumed: parked dependencies complete' are excluded, as are
    rows that *are* agent/harness dispatches (an 'Origin: agent|harness' or
    'Surface: bell|whistle' header in the first lines of the text).  Operator
    texts that merely *mention* bells/auto-bellback are retained.
  * Consecutive qualified turn timestamps delimit maximal gaps; a gap is
    listed when it is at least MIN_GAP_HOURS (6) long and is
    "activity-bearing": it contains at least one qualifying Codex token event
    or at least one commit in the repository scope.  Gaps with neither are
    not listed (they do not appear in the report table either).

Token rule (per the report's "Sources and counting limits"):

  * Source: ~/.codex/sessions/**/*.jsonl, event_msg payload.type=token_count.
  * Each qualifying event contributes last_token_usage input+output.
  * Within one rollout file, in file order: events with null info are skipped
    ("no-info"); an event whose cumulative total_token_usage equals the
    previous retained cumulative snapshot is skipped ("unchanged snapshot");
    a decreased cumulative counter starts a new segment but the event still
    contributes its last_token_usage.  Exact duplicate events (same
    timestamp, same cumulative and last usage) are skipped.  No cumulative
    counters are summed; cached input and reasoning output are subsets and
    are never added separately.

Commit rule:

  * Repositories: every top-level /home/joe/code/* directory with a real .git
    directory (linked worktrees have a .git *file* and are not counted), the
    same discovery the report used on 2026-09-21.
  * `git log --all --format='%H%x09%ct'`, unique SHA per repo, UTC committer
    date, filtered to each gap's full-precision bounds; mathlib4 is
    restricted to commits touching DarkTower/WarMachine/.

Events and commits are assigned to a gap g when g.start <= t < g.end using
full-precision bounds (the report's displayed seconds are rounded).

Read-only: the evidence API is only GET, session logs and git repos are only
read.  Output is a single JSON file.
"""

import argparse
import json
import re
import subprocess
import sys
import urllib.parse
import urllib.request
from concurrent.futures import ProcessPoolExecutor
from datetime import datetime, timedelta, timezone
from pathlib import Path

API = 'http://localhost:7073/api/alpha/evidence'
SESSIONS_ROOT = Path.home() / '.codex' / 'sessions'
CODE_ROOT = Path('/home/joe/code')
MIN_GAP_HOURS = 6.0
RESUMED = '--- resumed: parked dependencies complete'
HEADER_RE = re.compile(r'^(Origin:\s*(agent|harness)\b|Surface:\s*(bell|whistle)\b)')
MATHLIB = 'mathlib4'
MATHLIB_PATHS = ['DarkTower/WarMachine/']

HERE = Path(__file__).resolve().parent


# ---------------------------------------------------------------- timestamps

def parse_ts(value):
    """Parse an ISO-8601 UTC timestamp, tolerating nanosecond fractions."""
    v = value.strip()
    if v.endswith('Z'):
        v = v[:-1] + '+00:00'
    if '.' in v:
        head, tail = v.split('.', 1)
        frac, _, rest = tail.partition('+')
        v = head + '.' + frac[:6].ljust(6, '0') + (('+' + rest) if rest else '')
    return datetime.fromisoformat(v).astimezone(timezone.utc)


def iso(dt):
    return dt.strftime('%Y-%m-%dT%H:%M:%S.') + f'{dt.microsecond:06d}Z'


def iso_second(dt):
    """Report-style display: truncated to whole seconds (the report's table
    truncates; verified against all 54 rows on 2026-10-01)."""
    return dt.replace(microsecond=0).strftime('%Y-%m-%dT%H:%M:%SZ')


# ------------------------------------------------------- operator turns

def _get(url):
    req = urllib.request.Request(url, headers={'Accept': 'application/json'})
    with urllib.request.urlopen(req, timeout=120) as r:
        return json.load(r)


def qualifies(body):
    """The report's qualified-operator-row rule."""
    event = body.get('event')
    transport = body.get('transport')
    if not ((event == 'chat-turn' and body.get('role') == 'user')
            or (transport == 'marimo' and body.get('direction') == 'inbound')):
        return False
    text = body.get('text') or ''
    if RESUMED in text:
        return False
    # Rows that *are* agent/harness dispatches carry the surface header at the
    # top of the text; operator texts that merely mention these words later
    # in the body are retained (verified against 2026-09-15..22 data).
    for line in text.splitlines()[:5]:
        if HEADER_RE.match(line.strip()):
            return False
    return True


def fetch_operator_turns(since, before, api=API, page_limit=1000, log=sys.stderr):
    """Page the evidence API (descending) and return qualified turn times."""
    params = {'author': 'joe', 'since': iso(since), 'before': iso(before),
              'limit': str(page_limit)}
    turns, scanned, excluded = [], 0, 0
    cursor = None
    while True:
        q = dict(params)
        if cursor:
            q['cursor-at'] = cursor['at']
            q['cursor-id'] = cursor['id']
        url = api + '?' + urllib.parse.urlencode(q)
        d = _get(url)
        entries = d.get('entries', [])
        for e in entries:
            scanned += 1
            body = e.get('evidence/body')
            if not isinstance(body, dict):
                body = {}
            if qualifies(body):
                turns.append(parse_ts(e['evidence/at']))
            else:
                excluded += 1
        cursor = d.get('next-cursor')
        if not cursor or not entries:
            break
        print(f'  ... {scanned} evidence rows paged', file=log)
    turns.sort()
    print(f'evidence: {scanned} rows scanned, {len(turns)} qualified turns, '
          f'{excluded} excluded', file=log)
    return turns


# ------------------------------------------------------- codex token events

def scan_rollout(path):
    """Token events of one rollout file after the report's dedup rules.

    Returns (events, diagnostics) where events is a list of
    (epoch_seconds, input_plus_output) in file order.
    """
    events = []
    diag = {'no-info': 0, 'unchanged': 0, 'resets': 0, 'duplicates': 0,
            'parse-errors': 0}
    prev_cum = None
    seen = set()
    try:
        f = open(path, 'r', encoding='utf-8', errors='replace')
    except OSError:
        return events, diag
    with f:
        for line in f:
            if '"token_count"' not in line:
                continue
            try:
                row = json.loads(line)
            except json.JSONDecodeError:
                diag['parse-errors'] += 1
                continue
            payload = row.get('payload') or {}
            if payload.get('type') != 'token_count':
                continue
            info = payload.get('info')
            if not info:
                diag['no-info'] += 1
                continue
            total = info.get('total_token_usage') or {}
            last = info.get('last_token_usage') or {}
            cum = (total.get('input_tokens'), total.get('output_tokens'),
                   total.get('total_tokens'))
            key = (row.get('timestamp'), cum,
                   last.get('input_tokens'), last.get('output_tokens'))
            if key in seen:
                diag['duplicates'] += 1
                continue
            seen.add(key)
            if prev_cum is not None:
                if cum == prev_cum:
                    diag['unchanged'] += 1
                    continue
                if (cum[2] is not None and prev_cum[2] is not None
                        and cum[2] < prev_cum[2]):
                    diag['resets'] += 1  # new segment; still counted below
            prev_cum = cum
            ts = row.get('timestamp')
            if not ts:
                diag['no-info'] += 1
                continue
            tokens = (last.get('input_tokens') or 0) + (last.get('output_tokens') or 0)
            events.append((parse_ts(ts).timestamp(), tokens))
    return events, diag


def _scan_one(path):
    return str(path), *scan_rollout(path)


def scan_sessions(root=SESSIONS_ROOT, workers=8, log=sys.stderr):
    """All token events across retained rollout files, sorted by time."""
    files = sorted(Path(root).glob('**/*.jsonl'))
    print(f'codex sessions: scanning {len(files)} rollout files', file=log)
    events, diags = [], []
    with ProcessPoolExecutor(max_workers=workers) as pool:
        for name, evs, diag in pool.map(_scan_one, files, chunksize=8):
            events.extend(evs)
            diags.append((name, diag))
    events.sort()
    totals = {}
    for _, d in diags:
        for k, v in d.items():
            totals[k] = totals.get(k, 0) + v
    print(f'codex sessions: {len(events)} token events retained; {totals}',
          file=log)
    return events


# ------------------------------------------------------- commits

def discover_repos(code_root=CODE_ROOT):
    repos = []
    for d in sorted(Path(code_root).iterdir()):
        if d.is_dir() and (d / '.git').is_dir():
            repos.append(d.name)
    return repos


def repo_commits(repo, code_root=CODE_ROOT):
    """Unique (sha, committer-epoch) pairs reachable from --all."""
    cmd = ['git', '-C', str(Path(code_root) / repo), 'log', '--all',
           '--format=%H%x09%ct']
    if repo == MATHLIB:
        cmd += ['--'] + MATHLIB_PATHS
    try:
        out = subprocess.run(cmd, capture_output=True, text=True,
                             timeout=600).stdout
    except (subprocess.TimeoutExpired, OSError) as exc:
        print(f'warning: git log failed for {repo}: {exc}', file=sys.stderr)
        return []
    seen, rows = set(), []
    for line in out.splitlines():
        sha, _, ct = line.partition('\t')
        if sha and sha not in seen:
            seen.add(sha)
            rows.append((int(ct), sha))
    return rows


def count_commits(repos, gaps, code_root=CODE_ROOT, log=sys.stderr):
    """commits[gap_index][repo] = n, by UTC committer date in gap bounds."""
    counts = [{} for _ in gaps]
    for repo in repos:
        rows = repo_commits(repo, code_root)
        if not rows:
            continue
        rows.sort()
        times = [r[0] for r in rows]
        import bisect
        for i, g in enumerate(gaps):
            lo = bisect.bisect_left(times, g['start'].timestamp())
            hi = bisect.bisect_left(times, g['end'].timestamp())
            n = hi - lo
            if n:
                counts[i][repo] = n
        print(f'  commits: {repo} ({len(rows)} unique)', file=log)
    return counts


# ------------------------------------------------------- gaps

def compute_gaps(turns, min_hours=MIN_GAP_HOURS):
    """Maximal gaps of at least min_hours between consecutive turns."""
    gaps = []
    for a, b in zip(turns, turns[1:]):
        hours = (b - a).total_seconds() / 3600.0
        if hours >= min_hours:
            gaps.append({'start': a, 'end': b, 'hours': hours})
    return gaps


def assign_tokens(gaps, events):
    """Sum events with g.start <= t < g.end; None when no event (NR)."""
    import bisect
    times = [e[0] for e in events]
    for g in gaps:
        lo = bisect.bisect_left(times, g['start'].timestamp())
        hi = bisect.bisect_left(times, g['end'].timestamp())
        if hi > lo:
            g['tokens'] = sum(e[1] for e in events[lo:hi])
        else:
            g['tokens'] = None
    return gaps


def activity_bearing(gaps, commit_counts):
    out = []
    for g, cc in zip(gaps, commit_counts):
        if g['tokens'] or cc:
            g = dict(g)
            g['commits'] = cc
            out.append(g)
    return out


def gap_row(g):
    return {'start': iso(g['start']), 'end': iso(g['end']),
            'start-display': iso_second(g['start']),
            'end-display': iso_second(g['end']),
            'hours': round(g['hours'], 3), 'tokens': g['tokens'],
            'commits': g.get('commits', {})}


# ------------------------------------------------------- main

def build(start, end, turns_since, api=API, sessions_root=SESSIONS_ROOT,
          code_root=CODE_ROOT, workers=8, log=sys.stderr):
    turns = fetch_operator_turns(turns_since, end, api=api, log=log)
    turns = [t for t in turns if t <= end]
    events = scan_sessions(sessions_root, workers=workers, log=log)
    events = [e for e in events if e[0] < end.timestamp()]
    gaps = compute_gaps(turns)
    assign_tokens(gaps, events)
    gaps = [g for g in gaps if g['end'] > start and g['start'] < end]
    repos = discover_repos(code_root)
    print(f'repos in scope: {len(repos)}', file=log)
    counts = count_commits(repos, gaps, code_root, log=log)
    rows = [gap_row(g) for g in activity_bearing(gaps, counts)]
    rows.sort(key=lambda r: r['start'])
    return {'window': {'start': iso(start), 'end': iso(end),
                       'turns-since': iso(turns_since)},
            'rule': {'min-gap-hours': MIN_GAP_HOURS,
                     'activity-bearing': '>=1 codex token event or >=1 commit'},
            'gaps': rows}


def main(argv=None):
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--start', default='2026-08-22T00:00:00Z')
    p.add_argument('--end', default='2026-10-01T21:12:00Z')
    p.add_argument('--turns-since', default=None,
                   help='how far back to page operator turns so that a gap '
                        'spanning --start is still delimited '
                        '(default: start - 40 days)')
    p.add_argument('--output', default=str(HERE / 'operator-gaps-2026-10-01.json'))
    p.add_argument('--workers', type=int, default=8)
    args = p.parse_args(argv)
    start = parse_ts(args.start)
    end = parse_ts(args.end)
    turns_since = (parse_ts(args.turns_since) if args.turns_since
                   else start - timedelta(days=40))
    data = build(start, end, turns_since, workers=args.workers)
    out = Path(args.output)
    out.write_text(json.dumps(data, indent=1) + '\n')
    print(f'{out}: {len(data["gaps"])} activity-bearing gaps', file=sys.stderr)
    return data


if __name__ == '__main__':
    main()
