# /// script
# requires-python = ">=3.10"
# dependencies = ["edn-format==0.7.5"]
# ///
"""Family breakdown of the Minard stage stream, one panel per stage.

Reuses the window, smoothing and label loading conventions of
minard_operator_work.py. Family = the pattern id before the first '/'.
Writes the standalone page plus a small JSON of the counts, and prints the
PATTERN-STAGES 'Families' markdown section (families above 5% of their
stage's hits in either window).
"""
import argparse
from collections import Counter, defaultdict
from datetime import date, datetime, timedelta
import colorsys
import hashlib
import json
from pathlib import Path

from edn_format import Keyword, loads

from minard_operator_work import STAGES, timestamp

HERE = Path(__file__).resolve().parent
START = date(2026, 8, 22)
END = date(2026, 10, 1)
CUTOFF2 = '2026-09-21T17:19:12.718167Z'  # labels before: 689 | after: +97 provisional
STAGE_COLORS = {'perceive': '#3987e5', 'believe': '#d95926', 'evaluate': '#199e70',
                'select': '#c98500', 'act': '#d55181', 'assurance': '#008300',
                'coordination': '#9085e9', 'none': '#898d91'}
OTHER = '#898d91'


def shades(base, n):
    """n perceptually spread shades within the stage's own hue."""
    r, g, b = (int(base[i:i + 2], 16) / 255 for i in (1, 3, 5))
    h, l, s = colorsys.rgb_to_hls(r, g, b)
    out = []
    for i in range(n):
        li = max(0.16, min(0.9, 0.32 + 0.5 * i / max(1, n - 1)))
        si = max(0.25, s * (1.0 - 0.35 * i / max(1, n - 1)))
        rr, gg, bb = colorsys.hls_to_rgb(h, li, si)
        out.append('#%02x%02x%02x' % (round(rr * 255), round(gg * 255), round(bb * 255)))
    return out


def load_labels(labels_path):
    doc = loads(labels_path.read_text())
    labels = {}
    for row in doc[Keyword('patterns')]:
        pid = str(row[Keyword('id')])
        labels[pid] = {'stage': str(row[Keyword('stage')]).lstrip(':'),
                       'family': pid.split('/', 1)[0],
                       'provisional': str(row.get(Keyword('label-source'), '')) == 'additions-2026-10-01'}
    return labels


def build_families(joins_path, labels_path, start=START, end=END,
                   cutoff2=CUTOFF2, top_n=8):
    labels = load_labels(labels_path)
    days = [(start + timedelta(days=i)).isoformat() for i in range((end - start).days + 1)]
    di = {d: i for i, d in enumerate(days)}
    cut = timestamp(cutoff2)
    stage_family_day = {s: defaultdict(lambda: [0] * len(days)) for s in STAGES}
    pattern_hits = Counter()
    for line in joins_path.read_text().splitlines():
        row = json.loads(line)
        if row['status'] != 'matched':
            continue
        label = labels[row['pattern-id']]
        day = timestamp(row['turn-at'])
        if not start <= day.date() <= end:
            raise ValueError('Matched operator turn lies outside the chart window.')
        stage_family_day[label['stage']][label['family']][di[day.date().isoformat()]] += 1
        pattern_hits[row['pattern-id']] += 1
    panels = []
    for stage in STAGES:
        fam_day = stage_family_day[stage]
        fam_total = {f: sum(c) for f, c in fam_day.items()}
        top = sorted(fam_total, key=lambda f: (-fam_total[f], f))[:top_n]
        other = [f for f in fam_total if f not in top]
        fams = top + (['(other families)'] if other else [])
        colors = shades(STAGE_COLORS[stage], len(top)) + ([OTHER] if other else [])
        series = []
        for fam in fams:
            counts = fam_day.get(fam, [0] * len(days)) if fam != '(other families)' \
                else [sum(fam_day[f][i] for f in other) for i in range(len(days))]
            series.append({'name': fam, 'counts': counts})
        panels.append({'stage': stage, 'total': sum(fam_total.values()),
                       'other-families': other,
                       'families': [{'name': fam, 'color': color, 'counts': s['counts'],
                                     'hits': sum(s['counts'])} for fam, color, s in zip(fams, colors, series)]})
        panels[-1]['family-day'] = fam_day
    return {'days': days, 'panels': panels, 'pattern-hits': dict(pattern_hits)}


def window_split(joins_path, labels_path, start=START, end=END, cutoff2=CUTOFF2, top_n=8):
    """Exact before/after split per (stage, family), by turn timestamp."""
    labels = load_labels(labels_path)
    cut = timestamp(cutoff2)
    hits = defaultdict(lambda: [0, 0])
    top_patterns = defaultdict(Counter)
    for line in joins_path.read_text().splitlines():
        row = json.loads(line)
        if row['status'] != 'matched':
            continue
        label = labels[row['pattern-id']]
        when = timestamp(row['turn-at'])
        if not start <= when.date() <= end:
            continue
        side = 0 if when < cut else 1
        hits[(label['stage'], label['family'])][side] += 1
        top_patterns[(label['stage'], label['family'])][row['pattern-id']] += 1
    return hits, top_patterns


def build_data(joins_path, labels_path, start=START, end=END, cutoff2=CUTOFF2, top_n=8):
    fam = build_families(joins_path, labels_path, start, end, cutoff2, top_n)
    split, top_patterns = window_split(joins_path, labels_path, start, end, cutoff2, top_n)
    days = fam['days']
    out_panels = []
    for panel in fam['panels']:
        p = {'stage': panel['stage'], 'total': panel['total'], 'families': []}
        for f in panel['families']:
            if f['name'] == '(other families)':
                members = panel['other-families']
                before = sum(split.get((panel['stage'], m), [0, 0])[0] for m in members)
                after = sum(split.get((panel['stage'], m), [0, 0])[1] for m in members)
                merged = Counter()
                for m in members:
                    merged.update(top_patterns.get((panel['stage'], m), Counter()))
                tops = [{'id': pid, 'hits': n} for pid, n in merged.most_common(3)]
            else:
                key = (panel['stage'], f['name'])
                before, after = split.get(key, [0, 0])
                tops = [{'id': pid, 'hits': n} for pid, n in
                        top_patterns[key].most_common(3)] if key in top_patterns else []
            p['families'].append({**f, 'hitsBefore': before, 'hitsAfter': after, 'topPatterns': tops})
        p['days'] = [{'date': d, 'counts': {f['name']: f['counts'][i] for f in panel['families']}}
                     for i, d in enumerate(days)]
        out_panels.append(p)
    for p in out_panels:  # trailing 3-day mean, available-days rule at the left edge
        for i in range(len(days)):
            w = range(max(0, i - 2), i + 1)
            for f in p['families']:
                f.setdefault('means', [None] * len(days))
                f['means'][i] = sum(p['days'][j]['counts'][f['name']] for j in w) / len(w)
            p['days'][i]['total'] = sum(p['days'][i]['counts'].values())
    return {'stages': STAGES, 'days': days, 'panels': out_panels, 'cutoff2': cutoff2,
            'markerLabel': 'labels before / after: 689 original | +97 provisional',
            'sourceHashes': {p.name: hashlib.sha256(p.read_bytes()).hexdigest()
                             for p in (joins_path, labels_path)}}


def families_markdown(data, threshold=0.05):
    lines = []
    for p in data['panels']:
        rows = []
        for f in p['families']:
            if not f['hits']:
                continue
            tb, ta = p['total'] or 1, p['total'] or 1
            before_all = sum(g['hitsBefore'] for g in p['families'])
            after_all = sum(g['hitsAfter'] for g in p['families'])
            sb = f['hitsBefore'] / before_all if before_all else 0
            sa = f['hitsAfter'] / after_all if after_all else 0
            if sb >= threshold or sa >= threshold:
                rows.append((f, sb, sa))
        if not rows:
            continue
        lines.append(f"\n**{p['stage']}** ({p['total']} hits): " + '; '.join(
            f"{f['name']} {f['hitsBefore']} ({sb:.0%}) → {f['hitsAfter']} ({sa:.0%})"
            for f, sb, sa in rows))
    return '\n'.join(lines)


def generate(output, counts, joins=HERE / 'pattern-stage-joins-2026-10-01-fullwindow.jsonl',
             labels=HERE / 'pattern-stages-2026-10-01.edn',
             template_path=HERE / 'minard_families.template.html',
             start=START, end=END, cutoff2=CUTOFF2):
    data = build_data(joins, labels, start, end, cutoff2)
    template = template_path.read_text()
    encoded = json.dumps(data, ensure_ascii=False).replace('<', '\\u003c')
    output.write_text(template.replace('/*DATA*/', encoded))
    counts.write_text(json.dumps({'panels': [{'stage': p['stage'], 'total': p['total'],
                                              'families': [{k: v for k, v in f.items() if k != 'means'}
                                                           for f in p['families']]} for p in data['panels']]},
                                 indent=1))
    return data


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', type=Path, default=HERE / 'minard-families-2026-10-01.html')
    parser.add_argument('--counts', type=Path, default=HERE / 'minard-families-2026-10-01.json')
    parser.add_argument('--joins', type=Path, default=HERE / 'pattern-stage-joins-2026-10-01-fullwindow.jsonl')
    parser.add_argument('--labels', type=Path, default=HERE / 'pattern-stages-2026-10-01.edn')
    parser.add_argument('--template', type=Path, default=HERE / 'minard_families.template.html')
    parser.add_argument('--start', type=lambda v: date.fromisoformat(v), default=START)
    parser.add_argument('--end', type=lambda v: date.fromisoformat(v), default=END)
    args = parser.parse_args()
    data = generate(args.output, args.counts, args.joins, args.labels, args.template,
                    args.start, args.end)
    print(f'{args.output}: {len(data["panels"])} panels; ' +
          ', '.join(f'{p["stage"]}={p["total"]}' for p in data['panels']))
    print(families_markdown(data))
