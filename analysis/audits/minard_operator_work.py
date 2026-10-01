# /// script
# requires-python = ">=3.10"
# dependencies = ["edn-format==0.7.5"]
# ///
"""Regenerate the standalone Minard-style figure from stage EDN and evidence joins.

Run using marimo-zone/.venv/bin/python (edn-format==0.7.5), or uv run this script.
The fixed-width separators and linear interpolation are visual presentation;
exact integral shares always come from unsmoothed counts.
"""
import argparse
from collections import Counter
from datetime import date, datetime, timedelta, timezone
import hashlib
import json
from pathlib import Path
import re

from edn_format import loads
from pattern_stage_evidence import plain

HERE = Path(__file__).resolve().parent
STAGES = ['perceive', 'believe', 'evaluate', 'select', 'act', 'assurance', 'coordination', 'none']
START = date(2026, 8, 22)
END = date(2026, 9, 21)


def timestamp(value):
    return datetime.fromisoformat(value.replace('Z', '+00:00'))


def gap_rows(report, cutoff, start=START):
    section = report.split('## All observed activity-bearing gaps', 1)[1].split('\n## ', 1)[0]
    rows = []
    for line in section.splitlines():
        if not re.match(r'^\|\s*\d+\s*\|', line):
            continue
        columns = [c.strip() for c in line.strip('|').split('|')]
        rank, start_, end, hours, tokens = columns[:5]
        a, b = timestamp(start_), timestamp(end)
        if b <= datetime.combine(start, datetime.min.time(), timezone.utc) or a >= cutoff:
            continue
        rows.append({'rank': int(rank), 'start': start_, 'end': end, 'hours': float(hours),
                     'tokens': None if tokens == 'NR' else int(tokens.replace(',', '')),
                     'clipped': a.date() < start or b > cutoff})
    if not rows:
        raise ValueError('Forensic source table yielded no overlapping windows.')
    return sorted(rows, key=lambda r: r['start'])


def build_data(labels_path, joins_path, report_path, manifest_path,
               start=START, end=END):
    labels_doc = plain(loads(labels_path.read_text()))
    labels = labels_doc['patterns']
    index = {r['id']: r for r in labels}
    if len(index) != len(labels) or any(r['stage'] not in STAGES for r in labels):
        raise ValueError('Duplicate IDs or stage outside the rubric.')
    manifest = json.loads(manifest_path.read_text())
    cutoff = timestamp(manifest['window']['before'])
    days = [{'date': (start + timedelta(days=i)).isoformat(), 'counts': {s: 0 for s in STAGES}}
            for i in range((end - start).days + 1)]
    totals = Counter()
    turns, retrievals = set(), set()
    for line in joins_path.read_text().splitlines():
        row = json.loads(line)
        if row['status'] != 'matched':
            continue
        if row['retrieval-id'] in retrievals:
            raise ValueError('Duplicate accepted retrieval ID.')
        retrievals.add(row['retrieval-id'])
        label = index[row['pattern-id']]
        if row['rank1-pattern-ids'] != [row['pattern-id']]:
            raise ValueError('Accepted event does not have exactly one rank-one result.')
        day = timestamp(row['turn-at']).date()  # Operator day, not delayed retrieval day.
        if not start <= day <= end:
            raise ValueError('Matched operator turn lies outside the chart window.')
        days[(day - start).days]['counts'][label['stage']] += 1
        totals[label['stage']] += 1
        turns.add(row['turn-id'])
    if len(retrievals) != manifest['matched-retrievals'] or len(turns) != manifest['unique-matched-turns']:
        raise ValueError('Join ledger does not match the published coverage denominator.')
    for i, day in enumerate(days):
        window = days[max(0, i-2):i+1]
        day['mean'] = {s: sum(d['counts'][s] for d in window) / len(window) for s in STAGES}
        day['total'] = sum(day['counts'].values())
    return {'stages': STAGES, 'days': days, 'totals': dict(totals), 'hits': len(retrievals),
            'turns': len(turns), 'cutoff': cutoff.isoformat(),
            'eligible': manifest['statistics']['eligible-user-turns'],
            'gaps': gap_rows(report_path.read_text(), cutoff, start),
            'sourceHashes': {p.name: hashlib.sha256(p.read_bytes()).hexdigest()
                             for p in (labels_path, joins_path, report_path, manifest_path)}}


def generate(output, labels=HERE / 'pattern-stages-2026-09-21.edn',
             joins=HERE / 'pattern-stage-joins-2026-09-21.jsonl',
             report=HERE / 'FORENSIC-autopilot-2026-09-21.md',
             manifest=HERE / 'pattern-stage-manifest-2026-09-21.json',
             template_path=HERE / 'minard_operator_work.template.html',
             start=START, end=END):
    data = build_data(labels, joins, report, manifest, start, end)
    template = template_path.read_text()
    encoded = json.dumps(data, ensure_ascii=False).replace('<', '\\u003c')
    output.write_text(template.replace('/*DATA*/', encoded))
    return data


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', type=Path, default=HERE / 'minard-operator-work-2026-09-21.html')
    parser.add_argument('--labels', type=Path, default=HERE / 'pattern-stages-2026-09-21.edn')
    parser.add_argument('--joins', type=Path, default=HERE / 'pattern-stage-joins-2026-09-21.jsonl')
    parser.add_argument('--report', type=Path, default=HERE / 'FORENSIC-autopilot-2026-09-21.md')
    parser.add_argument('--manifest', type=Path, default=HERE / 'pattern-stage-manifest-2026-09-21.json')
    parser.add_argument('--template', type=Path, default=HERE / 'minard_operator_work.template.html')
    parser.add_argument('--start', type=lambda v: date.fromisoformat(v), default=START)
    parser.add_argument('--end', type=lambda v: date.fromisoformat(v), default=END)
    args = parser.parse_args()
    data = generate(args.output, args.labels, args.joins, args.report, args.manifest,
                    args.template, args.start, args.end)
    print(f'{args.output}: {data["hits"]} hits; {data["turns"]} turns; {len(data["gaps"])} gap windows')
