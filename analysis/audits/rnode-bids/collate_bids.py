"""Collate <NODE>-links.edn bids: per pattern, its bidders; single / contested / unbid;
cross-stage bids (bidder's stage vs the pattern's stage label). Writes BIDS-collated-2026-10-01.{md,json}."""
import json, re
from collections import defaultdict
from pathlib import Path
import edn_format
from edn_format import Keyword as K

HERE = Path(__file__).resolve().parent


def load():
    nodes = {n['node']: n for n in json.loads((HERE / 'nodes.json').read_text())}
    pats = {}
    for m in re.finditer(r'^- `([^`]+)` \| (\w+) \| (\d+) \| ([^|]*)\|', (HERE / 'patterns.md').read_text(), re.M):
        pats[m.group(1)] = {'stage': m.group(2), 'hits': int(m.group(3)), 'title': m.group(4).strip()}
    bids = []
    for node in nodes:
        d = edn_format.loads((HERE / f'{node}-links.edn').read_text())
        for b in d.get(K('bids')) or []:
            c = b[K('code')]
            bids.append({'node': node, 'pattern': b[K('pattern')], 'kind': str(b[K('kind')]).lstrip(':'),
                         'strength': str(b[K('strength')]).lstrip(':'), 'how': b[K('how')],
                         'symbol': c.get(K('symbol')), 'file': c.get(K('file')), 'line': c.get(K('line'))})
    return nodes, pats, bids


def collate(nodes, pats, bids):
    by_pat = defaultdict(list)
    for b in bids:
        by_pat[b['pattern']].append(b)
    rows = []
    for pid, p in sorted(pats.items(), key=lambda kv: -kv[1]['hits']):
        bs = by_pat.get(pid, [])
        func = [b for b in bs if b['kind'] == 'functional']
        status = 'unbid' if not func else 'single' if len({b['node'] for b in func}) == 1 else 'contested'
        rows.append({'pattern': pid, **p, 'status': status,
                     'functional': [b['node'] for b in func], 'topic': [b['node'] for b in bs if b['kind'] == 'topic'],
                     'cross-stage': sorted({b['node'] for b in func
                                            if nodes[b['node']]['stage'].lower() != p['stage'].lower()})})
    per_node = {n: {'stage': nodes[n]['stage'], 'band': nodes[n]['band'],
                    'functional': sum(1 for b in bids if b['node'] == n and b['kind'] == 'functional'),
                    'topic': sum(1 for b in bids if b['node'] == n and b['kind'] == 'topic')} for n in nodes}
    return rows, per_node


def main():
    nodes, pats, bids = load()
    rows, per_node = collate(nodes, pats, bids)
    count = lambda s: sum(1 for r in rows if r['status'] == s)
    hits = lambda s: sum(r['hits'] for r in rows if r['status'] == s)
    out = {'bids': bids, 'patterns': rows, 'nodes': per_node}
    (HERE / 'BIDS-collated-2026-10-01.json').write_text(json.dumps(out, indent=1, ensure_ascii=False))
    md = ['# R-node bids on the top 150 functional patterns, collated (2026-10-01)\n',
          f'{len(bids)} bids from {len(nodes)} nodes. Functional bids only decide status; topic bids are listed.\n',
          f'- single bidder: {count("single")} patterns ({hits("single")} hits)',
          f'- contested: {count("contested")} patterns ({hits("contested")} hits)',
          f'- unbid: {count("unbid")} patterns ({hits("unbid")} hits)\n',
          '## Per node\n', '| node | stage | band | functional | topic |', '|---|---|---|---:|---:|']
    md += [f'| {n} | {v["stage"]} | {v["band"]} | {v["functional"]} | {v["topic"]} |' for n, v in per_node.items()]
    md += ['\n## Bid patterns\n', '| pattern | stage label | hits | status | functional bidders | cross-stage | topic |',
           '|---|---|---:|---|---|---|---|']
    md += [f'| `{r["pattern"]}` | {r["stage"]} | {r["hits"]} | {r["status"]} | {", ".join(r["functional"])} | '
           f'{", ".join(r["cross-stage"])} | {", ".join(r["topic"])} |' for r in rows if r['functional'] or r['topic']]
    md += ['\n## Bids\n']
    md += [f'- **{b["node"]}** → `{b["pattern"]}` ({b["kind"]}, {b["strength"]}) at `{b["symbol"]}` '
           f'{b["file"]}:{b["line"]} — {b["how"]}' for b in bids]
    (HERE / 'BIDS-collated-2026-10-01.md').write_text('\n'.join(md) + '\n')
    print(f'{len(bids)} bids; single {count("single")}/{hits("single")} hits; contested {count("contested")}/{hits("contested")}; '
          f'unbid {count("unbid")}/{hits("unbid")}')


if __name__ == '__main__':
    main()
