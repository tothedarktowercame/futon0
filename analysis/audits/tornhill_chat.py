#!/usr/bin/env python3
"""Your agent chat as a crime scene: Tornhill's instruments joined to the chat record.

tornhill.py reads git alone. This script joins each commit to the agent session
that made it (futon6 data/session-commit-index.json), and each session to its
transcript, so that a file's history can be read in terms of who worked on it
and what that work cost the operator:

  knowledge map      which agent seat changes a file (claude-N, codex-N), not just
                     which model signed the commit -- most Codex commits carry no
                     Co-Authored-By trailer at all
  operator attention Joe's turns in the sessions that changed a file, raw and
                     apportioned across the code files each session touched
  knowledge loss     sessions that changed a file and were compacted
  stance             labelled replies (operator-reply pilot) inside those sessions
  session coupling   files changed in the same session; pairs that never share a
                     commit are coupling that agents split across commits, which
                     commit-grain coupling cannot see

Mission: futon3c/holes/missions/M-the-perfect-crime.md, third sweep ("the agent
chat as crime scene") and the 2026-09-26 section.

Operator turn rule is claude_operator_census.py's: user rows containing
"From: joe" and "Origin: operator", excluding park resumes and WAKE CHECKLIST,
deduped by row uuid across the live transcript and its .pre-compact snapshots.
Codex rollouts: response_item user messages under the same text rule.

Output contains session ids and seat names, no turn text; it is written outside
the repository (~/.local/share/futon-audits/). Standard library only; read-only.
"""
import argparse
import collections
import csv
import glob
import itertools
import json
import os
import re
import subprocess
import sys
from datetime import datetime, timezone
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import tornhill  # noqa: E402

SEAT = re.compile(r'(?m)^(?:To|Agent): ((?:claude|codex|kimi|zai|fable)-\d+)\b')
CORRECTION = {'amend', 'redirect', 'reject'}
SESSION_MAX_FILES = 40      # a session that touched more is a sweep, like a big commit
SESSION_MIN_SHARED = 3


def row_text(content):
    if isinstance(content, str):
        return content
    if isinstance(content, list):
        return ' '.join(x.get('text', '') for x in content if isinstance(x, dict))
    return ''


def is_operator(text):
    return ('From: joe' in text and 'Origin: operator' in text
            and 'resumed: parked' not in text and 'WAKE CHECKLIST' not in text)


def transcript_files():
    claude = collections.defaultdict(list)
    for f in glob.glob(os.path.expanduser('~/.claude/projects/*/*.jsonl*')):
        claude[os.path.basename(f).split('.jsonl')[0]].append(f)
    codex = {}
    for f in glob.glob(os.path.expanduser('~/.codex/sessions/*/*/*/*.jsonl')):
        m = re.search(r'([0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12})\.jsonl$', f)
        if m:
            codex[m.group(1)] = f
    return claude, codex


def read_claude(files, wanted_uuids, as_of):
    """Seat, operator/bell turns (census rule), compactions, labelled uuids seen.
    Rows stamped after as_of are ignored: live seats keep writing while this runs."""
    seen, seats = set(), collections.Counter()
    op = bell = direct = 0
    hits = set()
    for f in files:
        for line in open(f, errors='ignore'):
            if '"type":"user"' not in line:
                continue
            try:
                e = json.loads(line)
            except ValueError:
                continue
            u = e.get('uuid')
            if u in seen or e.get('timestamp', '') > as_of:
                continue
            seen.add(u)
            text = row_text(e.get('message', {}).get('content'))
            seats.update(SEAT.findall(text))
            if is_operator(text):
                op += 1
                if u in wanted_uuids:
                    hits.add(u)
            elif 'Origin: agent' in text:
                bell += 1
            elif (e.get('entrypoint') == 'cli' and text.strip() and not text.startswith('<')
                  and 'From: ' not in text):
                # Typed straight into an interactive CLI session: no Agency
                # envelope, so the census rule cannot see it. Counted apart.
                direct += 1
    compactions = sum('.pre-compact-' in os.path.basename(f) for f in files)
    return {'seat': seats.most_common(1)[0][0] if seats else None,
            'operator_turns': op, 'direct_turns': direct, 'bell_turns': bell,
            'compactions': compactions, 'labelled': sorted(hits)}


def read_codex(path, as_of):
    seats = collections.Counter()
    op = bell = 0
    for line in open(path, errors='ignore'):
        if '"response_item"' not in line or '"role":"user"' not in line:
            continue
        try:
            e = json.loads(line)
        except ValueError:
            continue
        p = e.get('payload', {})
        if p.get('type') != 'message' or p.get('role') != 'user' or e.get('timestamp', '') > as_of:
            continue
        text = row_text(p.get('content'))
        seats.update(SEAT.findall(text))
        if is_operator(text):
            op += 1
        elif 'Origin: agent' in text:
            bell += 1
    return {'seat': seats.most_common(1)[0][0] if seats else None,
            'operator_turns': op, 'direct_turns': 0, 'bell_turns': bell,
            'compactions': None, 'labelled': []}


def load_labels(pilot_dir, joe_labels):
    """uuid -> (label, labeller); Joe's blind label wins over codex-14's."""
    labels = {}
    for path in (Path(pilot_dir) / 'stance_labels.csv', Path(joe_labels)):
        if not path.exists():
            continue
        for row in csv.DictReader(open(path)):
            uuid = row['pair_id'].split(':', 1)[-1]
            labels[uuid] = (row['label'], row['labeller'])
    return labels


def attribution_rank(c):
    """One commit, two sessions (22 cases in the 2026-09-25 index): the session
    that printed the sha beats a subject+time match, and the earlier session
    beats a later one that printed the same sha from a log or rebase."""
    return (0 if c['match'] == 'sha-printed' else 1, c['at'] or '')


def resolve_commits(root, index_commits, repos):
    """Short sha -> full sha, timestamp, code files, per repo; unresolvable are counted."""
    by_repo = collections.defaultdict(list)
    for c in index_commits:
        if c['repo'] in repos:
            by_repo[c['repo']].append(c)
    resolved, unresolved, ambiguous = [], collections.Counter(), []
    for repo, cs in by_repo.items():
        path = Path(root) / repo
        batch = subprocess.run(['git', '-C', str(path), 'cat-file', '--batch-check'],
                               input='\n'.join(c['sha'] for c in cs) + '\n',
                               capture_output=True, text=True).stdout.splitlines()
        full = {}
        for c, line in zip(cs, batch):
            parts = line.split()
            if len(parts) == 3 and parts[1] == 'commit':
                prev = full.get(parts[0])
                if prev is None or attribution_rank(c) < attribution_rank(prev):
                    full[parts[0]] = c
                if prev is not None and prev['session'] != c['session']:
                    ambiguous.append({'repo': repo, 'sha': parts[0][:10],
                                      'sessions': sorted({prev['session'], c['session']})})
            else:
                unresolved[repo] += 1
        if not full:
            continue
        out = subprocess.run(['git', '-C', str(path), 'log', '--no-walk=unsorted', '--stdin',
                              '--no-renames', '--format=\x1e%H\t%ct', '--numstat'],
                             input='\n'.join(full) + '\n', capture_output=True, text=True,
                             errors='replace').stdout
        for block in out.split('\x1e')[1:]:
            head, _, body = block.partition('\n')
            sha, ct = head.split('\t')
            files = [ln.split('\t')[2] for ln in body.splitlines()
                     if ln.count('\t') == 2 and tornhill.is_code(ln.split('\t')[2])]
            c = full[sha]
            resolved.append({'repo': repo, 'sha': sha, 'ct': int(ct), 'session': c['session'],
                             'agent_kind': c['agent_kind'], 'match': c['match'], 'files': files})
    return resolved, dict(unresolved), ambiguous


def collect(args):
    as_of = datetime.now(timezone.utc).strftime('%Y-%m-%dT%H:%M:%S.%fZ')[:-4] + 'Z'
    report = json.loads(Path(args.report).read_text())
    index = json.loads(Path(args.index).read_text())
    root = args.root
    repos = set(report['repos'])
    commits, unresolved, ambiguous = resolve_commits(root, index['commits'], repos)
    start = min(c['ct'] for c in commits)
    labels = load_labels(args.pilot, args.joe_labels)

    claude, codex = transcript_files()
    sessions = {}
    for kind, sid in sorted({(c['agent_kind'], c['session']) for c in commits}):
        if kind == 'claude' and sid in claude:
            info = read_claude(claude[sid], labels, as_of)
        elif kind == 'codex' and sid in codex:
            info = read_codex(codex[sid], as_of)
        else:
            info = {'seat': None, 'operator_turns': None, 'direct_turns': None, 'bell_turns': None,
                    'compactions': None, 'labelled': [], 'transcript': 'none'}
        info['kind'] = kind
        # No Agency envelope: in the 2026-09-26 sample these are build-loop
        # invocations ("ROW TO DO THIS INVOCATION ...") and codex exec jobs.
        info['seat'] = info['seat'] or f'{kind}:unrouted'
        info['stance'] = dict(collections.Counter(labels[u][0] for u in info['labelled']))
        sessions[sid] = info
        print(f'{kind} {sid[:12]} seat={info["seat"]} op={info["operator_turns"]}',
              file=sys.stderr)

    # Per-file join.
    session_files = collections.defaultdict(set)
    file_commits = collections.defaultdict(list)
    for c in commits:
        for f in c['files']:
            key = (c['repo'], f)
            file_commits[key].append(c)
            session_files[c['session']].add(key)
    # Denominator: every commit that touched the file since the index starts.
    total_since = collections.Counter()
    for repo in repos:
        for c in tornhill.read_history(Path(root) / repo):
            if c['ct'] >= start:
                for f in c['files']:
                    if tornhill.is_code(f):
                        total_since[(repo, f)] += 1

    hot = {(repo, f['path']): f for repo, r in report['repos'].items() for f in r['files']}
    files = []
    for key, cs in file_commits.items():
        if key not in hot:              # deleted since, or not code in the report
            continue
        sids = sorted({c['session'] for c in cs})
        seats = collections.Counter(sessions[c['session']]['seat'] for c in cs)
        kinds = collections.Counter(c['agent_kind'] for c in cs)
        op_raw = sum(sessions[s]['operator_turns'] or 0 for s in sids)
        op_share = sum((sessions[s]['operator_turns'] or 0) / len(session_files[s]) for s in sids)
        stance = collections.Counter()
        for s in sids:
            stance.update(sessions[s]['stance'])
        main_seat, main_n = seats.most_common(1)[0]
        h = hot[key]
        files.append({
            'repo': key[0], 'path': key[1],
            'hotspot': h['hotspot'], 'revs_90d': h['revs'],
            'attributed': len(cs), 'revs_since_start': total_since[key],
            'coverage': round(len(cs) / total_since[key], 2) if total_since[key] else None,
            'kinds': dict(kinds), 'seats': dict(seats.most_common()),
            'main_seat': main_seat, 'main_seat_share': round(main_n / len(cs), 2),
            'n_seats': len(seats), 'sessions': len(sids),
            'operator_turns': op_raw, 'operator_share': round(op_share, 1),
            'direct_turns': sum(sessions[s]['direct_turns'] or 0 for s in sids),
            'bell_turns': sum(sessions[s]['bell_turns'] or 0 for s in sids),
            'compacted_sessions': sum(1 for s in sids if sessions[s]['compactions']),
            'no_transcript_sessions': sum(1 for s in sids if sessions[s].get('transcript') == 'none'),
            'stance': dict(stance), 'corrections': sum(stance[k] for k in CORRECTION),
        })
    files.sort(key=lambda r: -r['operator_share'])

    # Session-grain coupling versus commit-grain coupling on the same commits.
    commit_pairs = collections.Counter()
    for c in commits:
        fs = sorted({(c['repo'], f) for f in c['files']})
        if len(fs) <= tornhill.MAX_CHANGESET:
            commit_pairs.update(itertools.combinations(fs, 2))
    sess_count = collections.Counter()
    sess_pairs = collections.Counter()
    for s, fs in session_files.items():
        fs = sorted(fs)
        sess_count.update(fs)
        if len(fs) <= SESSION_MAX_FILES:
            sess_pairs.update(itertools.combinations(fs, 2))
    coupling = []
    for (a, b), n in sess_pairs.items():
        if n < SESSION_MIN_SHARED or a[0] != b[0]:
            continue
        degree = 100 * n / ((sess_count[a] + sess_count[b]) / 2)
        if degree < tornhill.MIN_DEGREE:
            continue
        coupling.append({'repo': a[0], 'a': a[1], 'b': b[1], 'sessions': n,
                         'degree': round(degree, 1), 'commits_shared': commit_pairs[(a, b)],
                         'twin': tornhill.module_key(a[1]) == tornhill.module_key(b[1])})
    coupling.sort(key=lambda p: (-p['sessions'], -p['degree']))

    seats = collections.defaultdict(lambda: {'commits': 0, 'sessions': set(), 'files': set(),
                                             'operator_turns': 0, 'compactions': 0})
    for c in commits:
        s = sessions[c['session']]
        k = s['seat']
        seats[k]['commits'] += 1
        seats[k]['files'].update((c['repo'], f) for f in c['files'])
        if c['session'] not in seats[k]['sessions']:
            seats[k]['sessions'].add(c['session'])
            seats[k]['operator_turns'] += s['operator_turns'] or 0
            seats[k]['compactions'] += s['compactions'] or 0
    seat_rows = sorted(({'seat': k, 'commits': v['commits'], 'sessions': len(v['sessions']),
                         'files': len(v['files']), 'operator_turns': v['operator_turns'],
                         'compactions': v['compactions']} for k, v in seats.items()),
                       key=lambda r: -r['commits'])

    out = {'generated': datetime.now(timezone.utc).isoformat(timespec='seconds'), 'as_of': as_of,
           'report': args.report, 'index': args.index,
           'index_generated': index.get('generated'),
           'window': {'start': datetime.fromtimestamp(start, timezone.utc).isoformat(),
                      'commits_resolved': len(commits),
                      'index_commits_in_repos': sum(1 for c in index['commits'] if c['repo'] in repos),
                      'unresolved_by_repo': unresolved,
                      'index_commits_without_repo': sum(1 for c in index['commits'] if not c['repo']),
                      'ambiguous_attribution': ambiguous},
           'labels': {'pairs': len(labels), 'found_in_sessions': sum(len(s['labelled']) for s in sessions.values())},
           'sessions': sessions, 'seats': seat_rows, 'files': files, 'session_coupling': coupling}
    Path(args.out).write_text(json.dumps(out, indent=1))
    print(f'wrote {args.out}: {len(commits)} commits, {len(sessions)} sessions, '
          f'{len(files)} files, {len(coupling)} session-coupled pairs', file=sys.stderr)


def check(args):
    """Re-read the sources for a sample: (1) the operator count for sampled Claude
    sessions by claude_operator_census.py's own loop, restricted to the session's
    files; (2) for sampled commits, that the session transcript mentions the
    commit's short sha or subject; (3) file lists by `git show --name-only`."""
    out = json.loads(Path(args.report).read_text())
    index = json.loads(Path(out['index']).read_text())
    claude, codex = transcript_files()
    lines = [f'INDEPENDENT CHECK: {Path(args.report).name}', f'generated: {out["generated"]}', '']
    bad = 0
    sample = sorted((s for s, v in out['sessions'].items() if v['kind'] == 'claude'),
                    key=lambda s: -(out['sessions'][s]['operator_turns'] or 0))[:args.n]
    lines.append('(1) operator turns, census loop re-run per session:')
    for sid in sample:
        seen, n = set(), 0
        for f in claude[sid]:
            for line in open(f, errors='ignore'):
                if '"type":"user"' not in line or 'From: joe' not in line:
                    continue
                try:
                    e = json.loads(line)
                except ValueError:
                    continue
                if e.get('uuid') in seen or e.get('timestamp', '') > out['as_of']:
                    continue
                seen.add(e.get('uuid'))
                c = e.get('message', {}).get('content')
                tx = c if isinstance(c, str) else ' '.join(x.get('text', '') for x in c if isinstance(x, dict)) if isinstance(c, list) else ''
                if 'Origin: operator' in tx and 'resumed: parked' not in tx and 'WAKE CHECKLIST' not in tx:
                    n += 1
        got = out['sessions'][sid]['operator_turns']
        ok = n == got
        bad += not ok
        lines.append(f'  {sid[:12]} report={got} census={n} {"MATCH" if ok else "MISMATCH"}')
    lines.append('')
    lines.append('(2) commit named in its session transcript (sha prefix or subject):')
    by_sha = {c['sha'][:8]: c for c in index['commits'] if c['repo']}
    picked = collections.Counter()
    for c in index['commits']:
        if not c['repo'] or picked[c['agent_kind']] >= args.n:
            continue
        sid, kind = c['session'], c['agent_kind']
        files = claude.get(sid, []) if kind == 'claude' else [codex[sid]] if sid in codex else []
        if not files:
            continue
        picked[kind] += 1
        subject = c['subject'].split('\\n')[0][:60]
        found = any(c['sha'][:7] in open(f, errors='ignore').read() or subject in open(f, errors='ignore').read()
                    for f in files)
        bad += not found
        lines.append(f'  {kind} {c["repo"]} {c["sha"][:8]} in {sid[:12]}: {"FOUND" if found else "NOT FOUND"}')
    lines.append('')
    lines.append('(3) file lists, git show --name-only vs report (top files by operator share):')
    for f in out['files'][:args.n]:
        repo = Path(args.root) / f['repo']
        n = 0
        seen = set()
        for c in index['commits']:
            if c['repo'] != f['repo'] or c['sha'][:8] in seen:
                continue
            seen.add(c['sha'][:8])
            # Merges are excluded, as in tornhill.py (code-maat does the same):
            # `git show` lists a merge's combined-diff files, numstat does not.
            r = subprocess.run(['git', '-C', str(repo), 'show', '--no-renames', '--name-only',
                                '--format=%P', c['sha']], capture_output=True, text=True)
            lines_ = r.stdout.splitlines()
            merge = bool(lines_) and len(lines_[0].split()) > 1
            n += r.returncode == 0 and not merge and f['path'] in lines_[1:]
        ok = n == f['attributed']
        bad += not ok
        lines.append(f'  {f["repo"]}/{f["path"]} report={f["attributed"]} hand={n} {"MATCH" if ok else "MISMATCH"}')
    lines.append('')
    lines.append(f'result: {"PASS" if not bad else f"{bad} MISMATCH/NOT FOUND"}')
    Path(args.out).write_text('\n'.join(lines) + '\n')
    print('\n'.join(lines))
    return 1 if bad else 0


def main(argv=None):
    here = Path(__file__).resolve()
    p = argparse.ArgumentParser(description=__doc__.split('\n')[0])
    sub = p.add_subparsers(dest='cmd', required=True)
    c = sub.add_parser('collect')
    c.add_argument('--root', default=str(here.parents[3]))
    c.add_argument('--report', required=True, help='tornhill.py collect output')
    c.add_argument('--index', default=str(here.parents[3] / 'futon6/data/session-commit-index.json'))
    c.add_argument('--pilot', default=str(here.parent / 'operator-reply-pilot-2026-09-21'))
    c.add_argument('--joe-labels', default=os.path.expanduser(
        '~/.local/share/futon-audits/operator-reply-pilot-2026-09-21/joe_labels.csv'))
    c.add_argument('--out', required=True)
    k = sub.add_parser('check')
    k.add_argument('--root', default=str(here.parents[3]))
    k.add_argument('--report', required=True, help='tornhill_chat.py collect output')
    k.add_argument('--n', type=int, default=4)
    k.add_argument('--out', required=True)
    args = p.parse_args(argv)
    return collect(args) if args.cmd == 'collect' else check(args)


if __name__ == '__main__':
    sys.exit(main())
