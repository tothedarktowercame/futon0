# /// script
# requires-python = ">=3.10"
# dependencies = ["edn-format==0.7.5"]
# ///
"""Merged Minard page: the main 2026-10-01 operator-work figure plus the
per-stage pattern-family breakdown as one interactive page.

Main-figure data comes from minard_operator_work.build_data; family data comes
from minard_families.build_data (not recomputed here). The template is the
10-01 operator-work template extended with an in-page family panel.
"""
import argparse
from datetime import date
import json
from pathlib import Path

from minard_operator_work import build_data as build_main
from minard_families import build_data as build_families

HERE = Path(__file__).resolve().parent
START = date(2026, 8, 22)
END = date(2026, 10, 1)


def generate(output, labels=HERE / 'pattern-stages-2026-10-01.edn',
             joins=HERE / 'pattern-stage-joins-2026-10-01-fullwindow.jsonl',
             report=HERE / 'FORENSIC-autopilot-2026-09-21.md',
             manifest=HERE / 'pattern-stage-manifest-2026-10-01-fullwindow.json',
             template_path=HERE / 'minard_merged_2026_10_01.template.html',
             start=START, end=END):
    main = build_main(labels, joins, report, manifest, start, end)
    families = build_families(joins, labels, start, end)
    main['sourceHashes'].update({'families.' + k: v for k, v in families['sourceHashes'].items()})
    template = template_path.read_text()
    page = template.replace('/*DATA*/', json.dumps(main, ensure_ascii=False).replace('<', '\\u003c'))
    page = page.replace('/*FAMILY_DATA*/', json.dumps(families, ensure_ascii=False).replace('<', '\\u003c'))
    output.write_text(page)
    return main, families


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', type=Path, default=HERE / 'minard-operator-work-2026-10-01.html')
    parser.add_argument('--labels', type=Path, default=HERE / 'pattern-stages-2026-10-01.edn')
    parser.add_argument('--joins', type=Path, default=HERE / 'pattern-stage-joins-2026-10-01-fullwindow.jsonl')
    parser.add_argument('--report', type=Path, default=HERE / 'FORENSIC-autopilot-2026-09-21.md')
    parser.add_argument('--manifest', type=Path, default=HERE / 'pattern-stage-manifest-2026-10-01-fullwindow.json')
    parser.add_argument('--template', type=Path, default=HERE / 'minard_merged_2026_10_01.template.html')
    parser.add_argument('--start', type=lambda v: date.fromisoformat(v), default=START)
    parser.add_argument('--end', type=lambda v: date.fromisoformat(v), default=END)
    args = parser.parse_args()
    main, families = generate(args.output, args.labels, args.joins, args.report,
                              args.manifest, args.template, args.start, args.end)
    print(f'{args.output}: {main["hits"]} hits; {len(families["panels"])} family panels (' +
          ', '.join(f'{p["stage"]}={len(p["families"])} families' for p in families['panels']) + ')')
