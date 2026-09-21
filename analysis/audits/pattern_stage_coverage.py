"""Compare the guarded snapshot to claude-5's UUID-deduplicated census.

Same selection as claude_operator_census.py at 870497b, with the full timestamp
cutoff applied before counting. The census uses substring envelope detection;
it is a requested comparison population, not a new claim of perfect attribution.
Coverage is a conservative cross-source confirmation: exact normalized payload,
same session, timestamps within five minutes, unique in both directions. It never
changes the retrieval join. Text stays in memory and is not written to the report.

Usage: python pattern_stage_coverage.py /path/to/raw-snapshot --output report.json
"""
import argparse
from collections import Counter, defaultdict
from datetime import datetime
import glob
import hashlib
import json
from pathlib import Path
import re
import subprocess

from pattern_stage_evidence import normalize, textbody

PRIOR = 'cd03ab1c92157b0b442932d7e913b0e7799b28b6'
HERE = Path(__file__).resolve().parent


def seconds(at):
    return datetime.fromisoformat(at.replace('Z', '+00:00')).timestamp()


def census(since, before):
    seen, rows, files = set(), [], []
    # Match the original census's file traversal and first-UUID-copy convention.
    for filename in glob.glob('/home/joe/.claude/projects/*/*.jsonl*'):
        data = Path(filename).read_bytes()
        files.append({'path': filename, 'sha256': hashlib.sha256(data).hexdigest()})
        for line in data.decode(errors='ignore').splitlines():
            if '"type":"user"' not in line or 'From: joe' not in line:
                continue
            try:
                row = json.loads(line)
            except ValueError:
                continue
            uid = row.get('uuid')
            if uid in seen:
                continue
            seen.add(uid)
            content = row.get('message', {}).get('content')
            text = content if isinstance(content, str) else ' '.join(
                b.get('text', '') for b in content if isinstance(b, dict)) if isinstance(content, list) else ''
            if 'Origin: operator' not in text or 'resumed: parked' in text or 'WAKE CHECKLIST' in text:
                continue
            at = row.get('timestamp', '')
            if since <= at < before:
                payload = re.sub(r'^Agent: [^\n]+\n\s*User message:\s*\n', '', textbody(text)).strip()
                rows.append({'uuid': uid, 'at': at, 'session': row.get('sessionId', Path(filename).name.split('.jsonl')[0]),
                             'norm': normalize(payload)})
    return rows, files


def build(snapshot):
    window = json.loads((snapshot / 'window.json').read_text())
    turns = json.loads((snapshot / 'turns.json').read_text())
    joins = json.loads((snapshot / 'joins.json').read_text())
    previous = [json.loads(line) for line in subprocess.check_output([
        'git', '-C', str(HERE.parents[1]), 'show',
        PRIOR + ':analysis/audits/pattern-stage-joins-2026-09-21.jsonl'], text=True).splitlines()]
    matched = {j['turn-id'] for j in joins if j['status'] == 'matched'}
    old = {j['turn-id'] for j in previous if j['status'] == 'matched'}
    operators, files = census(window['since'], window['before'])
    by = defaultdict(Counter)
    for j in joins:
        by[j['at'][:10]]['retrieval-emissions'] += 1
        if j['status'] == 'matched':
            by[j['turn-at'][:10]]['joined-hits'] += 1
    index = defaultdict(list)
    for t in turns:
        index[(t['session'], t['norm'])].append(t)
        day = by[t['at'][:10]]
        if not t['excluded']:
            day['eligible-store-turns'] += 1
        if t['id'] in matched:
            day['joined-store-turns'] += 1
        if t['id'] in old:
            day['old-joined-store-turns'] += 1
    ledger = []
    for row in operators:
        candidates = [t for t in index[(row['session'], row['norm'])]
                      if abs(seconds(row['at']) - t['time']) <= 300]
        ledger.append({k: row[k] for k in ('uuid', 'at', 'session')} | {
            'candidate-store-turns': [t['id'] for t in candidates]})
    # Reject repeated transcript copies with different UUIDs claiming one store turn.
    inverse = Counter(t for r in ledger for t in r['candidate-store-turns'])
    for row in ledger:
        candidates = row['candidate-store-turns']
        unique = len(candidates) == 1 and inverse[candidates[0]] == 1
        row['status'] = 'unique' if unique else 'ambiguous' if candidates else 'unmatched'
        row['covered'] = unique and candidates[0] in matched
        row['previously-covered'] = unique and candidates[0] in old
        day = by[row['at'][:10]]
        day['transcript-turns'] += 1
        day['transcript-' + row['status']] += 1
        day['covered-transcript-turns'] += int(row['covered'])
        day['old-covered-transcript-turns'] += int(row['previously-covered'])
    return {'window': window, 'census-source-commit': '870497b', 'previous-ledger-commit': PRIOR,
            'matching': 'exact normalized payload; same session; absolute timestamp difference <=300 seconds; unique both directions',
            'days': dict(sorted(by.items())), 'transcript-records': ledger, 'source-files': files}


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('snapshot', type=Path)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    report = build(args.snapshot)
    args.output.write_text(json.dumps(report, indent=2) + '\n')
    for date, row in report['days'].items():
        if date >= '2026-09-08':
            n, d = row.get('covered-transcript-turns', 0), row.get('transcript-turns', 0)
            print(date, row.get('joined-store-turns', 0), n, d, f'{100*n/d:.1f}%' if d else 'n/a')
