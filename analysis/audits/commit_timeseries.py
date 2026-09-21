#!/usr/bin/env python3
"""Count unique reachable commits by UTC committer date; render CSV, SVG and HTML.

Uses only the Python standard library. Missing repositories remain blank in CSV
and are disclosed in the figure and page, never silently treated as zero.
"""
import argparse
import csv
from datetime import date, datetime, timedelta, timezone
from html import escape
from pathlib import Path
import subprocess

REPOS = 'futon0 futon1 futon1a futon1b futon1bi futon2 futon2a futon3 futon3a futon3b futon3c futon4 futon5 futon5a futon6 futon7 futon7a futonY mathlib4 p4ng'.split()
SERIES = [('war_machine', 'War Machine · futon2', '#2463a6'),
          ('war_machine_lean', 'War Machine Lean · mathlib4 path', '#b44d19'),
          ('everything_else', 'Other repositories', '#32764c')]


def collect(root, start, end):
    days = [start + timedelta(days=i) for i in range((end - start).days + 1)]
    counts = {day: {} for day in days}
    missing = []
    for repo in REPOS:
        path = root / repo
        check = subprocess.run(['git', '-C', str(path), 'rev-parse', '--show-toplevel'],
                               capture_output=True, text=True)
        if check.returncode:
            missing.append(repo)
            for day in days:
                counts[day][repo] = None
            continue
        if Path(check.stdout.strip()).resolve() != path.resolve():
            raise RuntimeError(f'{path} is not a repository root')
        command = ['git', '-C', str(path), 'log', '--all', '--format=%H%x09%ct']
        if repo == 'mathlib4':
            command += ['--', 'DarkTower/WarMachine/']
        # Walk all history, then filter timestamps: --since can prune history
        # with non-monotonic commit dates. Git's normal path-history semantics
        # (including merge simplification) apply to the Mathlib path filter.
        result = subprocess.run(command, check=True, capture_output=True, text=True)
        for day in days:
            counts[day][repo] = 0
        seen = set()
        for line in result.stdout.splitlines():
            sha, timestamp = line.split('\t')
            if sha in seen:
                continue
            seen.add(sha)
            day = datetime.fromtimestamp(int(timestamp), timezone.utc).date()
            if day in counts:
                counts[day][repo] += 1
    rows = []
    for day in days:
        values = counts[day]
        rows.append({'date': day.isoformat(), **values,
                     'war_machine': values['futon2'],
                     'war_machine_lean': values['mathlib4'],
                     'everything_else': sum(v for k, v in values.items()
                                            if k not in ('futon2', 'mathlib4') and v is not None)})
    if any(rows[0][key] is None for key in ('war_machine', 'war_machine_lean')):
        raise RuntimeError('A War Machine source repository is unavailable')
    return rows, missing


def render_svg(rows, missing):
    width, height = 1200, 650
    left, right, top, bottom = 88, 1160, 165, 532
    peak = max(row[key] for row in rows for key, _, _ in SERIES)
    step = max(1, ((peak + 4) // 5 + 9) // 10 * 10)
    ymax = step * 5
    x = lambda i: left + i * (right-left) / max(1, len(rows)-1)
    y = lambda v: bottom - v * (bottom-top) / ymax
    parts = [f'<svg xmlns="http://www.w3.org/2000/svg" width="{width}" height="{height}" viewBox="0 0 {width} {height}" role="img" aria-labelledby="title description">',
             '<title id="title">FUTON commit activity: War Machine and other repositories</title>',
             '<desc id="description">Faint dots are daily counts; lines are trailing seven-day means within the plotted window. Missing repositories are disclosed below.</desc>',
             '<rect width="1200" height="650" fill="#fff"/>',
             '<g font-family="system-ui, sans-serif" fill="#243343">',
             '<text x="88" y="40" font-size="26" font-weight="650">FUTON commit activity</text>',
             f'<text x="88" y="68" font-size="16">{rows[0]["date"]} – {rows[-1]["date"]} · UTC committer dates · inclusive</text>',
             '<text x="88" y="94" font-size="14">Lines: trailing 7-day mean · Faint dots: daily counts · First 6 means use available days</text>']
    for j, (_, label, color) in enumerate(SERIES):
        lx = 88 + j * 365
        parts += [f'<line x1="{lx}" y1="128" x2="{lx+26}" y2="128" stroke="{color}" stroke-width="3"/>',
                  f'<text x="{lx+34}" y="133" font-size="15">{escape(label)}</text>']
    for value in range(0, ymax+1, step):
        yy = y(value)
        parts += [f'<line x1="{left}" y1="{yy:.2f}" x2="{right}" y2="{yy:.2f}" stroke="#e1e6eb"/>',
                  f'<text x="{left-14}" y="{yy+5:.2f}" text-anchor="end" font-size="13">{value}</text>']
    for i in range(0, len(rows), 7):
        parts.append(f'<text x="{x(i):.2f}" y="557" text-anchor="middle" font-size="13">{rows[i]["date"][5:]}</text>')
    for key, _, color in SERIES:
        values = [r[key] for r in rows]
        means = [sum(values[max(0, i-6):i+1])/min(i+1, 7) for i in range(len(values))]
        for i, value in enumerate(values):
            parts.append(f'<circle cx="{x(i):.2f}" cy="{y(value):.2f}" r="3" fill="{color}" opacity="0.25"><title>{rows[i]["date"]}: {value} commits</title></circle>')
        points = ' '.join(f'{x(i):.2f},{y(v):.2f}' for i, v in enumerate(means))
        parts.append(f'<polyline points="{points}" fill="none" stroke="{color}" stroke-width="3" stroke-linejoin="round"/>')
    coverage = ('Unavailable: ' + ', '.join(missing) + ' (not included in totals).') if missing else 'All requested repositories available.'
    parts += ['<text x="624" y="589" text-anchor="middle" font-size="15">Commit date (UTC, month-day)</text>',
              '<text transform="translate(26 348) rotate(-90)" text-anchor="middle" font-size="15">Commits per day</text>',
              f'<text x="88" y="625" font-size="13">{escape(coverage)} Window end may be a partial day; counts reflect refs at generation.</text>',
              '</g></svg>']
    return '\n'.join(parts) + '\n'


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--start', type=date.fromisoformat, default=date(2026, 7, 21))
    parser.add_argument('--end', type=date.fromisoformat, default=date(2026, 9, 21))
    parser.add_argument('--root', type=Path, default=Path('/home/joe/code'))
    parser.add_argument('--output-dir', type=Path, default=Path(__file__).resolve().parent)
    args = parser.parse_args()
    if args.end < args.start:
        parser.error('--end must be on or after --start')
    rows, missing = collect(args.root, args.start, args.end)
    args.output_dir.mkdir(parents=True, exist_ok=True)
    stem = f'commit-timeseries-{args.end}'
    csv_path = args.output_dir / (stem + '.csv')
    with csv_path.open('w', newline='') as file:
        writer = csv.DictWriter(file, fieldnames=['date', *REPOS, *(s[0] for s in SERIES)])
        writer.writeheader()
        writer.writerows(rows)
    (args.output_dir / (stem + '.svg')).write_text(render_svg(rows, missing))
    totals = {key: sum(row[key] for row in rows) for key, _, _ in SERIES}
    generated = datetime.now(timezone.utc).isoformat(timespec='seconds')
    caption = (f'Commit activity from {args.start} through {args.end}, inclusive, binned by UTC committer date '
               '(Git %ct, not author date). Each canonical repository is queried with git log --all; '
               'commit SHAs are deduplicated within that repository, and worktrees are not counted separately. '
               'War Machine is futon2. War Machine Lean is mathlib4 filtered by -- DarkTower/WarMachine/ '
               'using Git’s normal path-history simplification; other Mathlib commits are excluded. '
               'Other repositories sums the remaining available canonical repositories. Lines show a trailing '
               '7-day mean and faint dots show daily counts; the first six means use available window days. '
               'The CSV retains daily per-repository counts (mathlib4 is path-filtered). '
               + (f'Unavailable: {", ".join(missing)}; its CSV cells are blank, and totals exclude it. ' if missing else '')
               + f'Generated {generated}; the last day is partial if still in progress. '
               'These are commit counts, not estimates of effort or agent attribution.')
    summary = ' · '.join(f'{label}: {totals[key]:,}' for key, label, _ in SERIES)
    page = ('<!doctype html>\n<html lang="en"><meta charset="utf-8">'
            '<meta name="viewport" content="width=device-width,initial-scale=1">'
            '<title>FUTON commit activity</title><style>'
            'body{max-width:1200px;margin:32px auto;padding:0 20px;font:16px/1.6 system-ui;color:#243343;background:#fff}'
            'img{width:100%;height:auto}figcaption{max-width:1000px}figure{margin:0}a{color:#2463a6}</style>'
            '<main><figure>'
            f'<img src="{stem}.svg" alt="Commit activity: War Machine, War Machine Lean and other repositories">'
            f'<figcaption><p>{escape(caption)}</p></figcaption></figure>'
            f'<p><strong>Window totals:</strong> {escape(summary)}</p>'
            f'<p><a href="{stem}.csv">Download daily CSV</a> · <a href="{stem}.svg">Open SVG</a></p>'
            '</main></html>\n')
    (args.output_dir / (stem + '.html')).write_text(page)
    print(summary)
    print('Unavailable:', ', '.join(missing) or 'none')
    print(csv_path)


if __name__ == '__main__':
    main()
