"""Held-out detection with and without machine-cues.edn, overall and for the six revised nodes:
per node, held-out turns where the node is detected, and how many of those have that node among
the R-node turn bids. Live-turn counts per node too. Writes DETECT-machine-2026-10-02.md."""
import json
from collections import defaultdict
from pathlib import Path
import detect

HERE = Path(__file__).resolve().parent
SIX = ['R10', 'R8', 'R19', 'R17', 'R20', 'R15']


def run(machine):
    detect.MACHINE = machine
    cs = detect.cues()
    pats = {n: [(c, rx) for c in sorted(v) if (rx := detect.compile_cue(c))] for n, v in cs.items()}
    test = set(json.loads((HERE / 'test-ids.json').read_text()))
    turns = {t['id']: t['operator-turn'] for t in json.loads((HERE.parent / 'rnode-turn-bids/turns.json').read_text()) if t['id'] in test}
    bids = defaultdict(set)
    for b in json.loads((HERE.parent / 'rnode-turn-bids/TURN-BIDS-collated-2026-10-01.json').read_text())['bids']:
        bids[b['turn']].add(b['node'])
    held = {t: set(detect.detect(x, pats)) for t, x in turns.items()}
    per = {}
    for n in SIX:
        det = [t for t in held if n in held[t]]
        per[n] = (len(det), sum(1 for t in det if n in bids.get(t, ())), sum(1 for t in turns if n in bids.get(t, ())))
    live = []
    for f in detect.LIVE.glob('turn-*.json'):
        if f.name.endswith(('.analysis.json', '.candidates.json')): continue
        try: r = json.loads(f.read_text())
        except Exception: continue
        if r.get('origin', 'operator') == 'operator' and (r.get('source_text') or '').strip(): live.append(r['source_text'])
    lc = {n: sum(1 for x in live if n in detect.detect(x, {n: pats[n]})) for n in SIX}
    both = [t for t in held if held[t] and bids.get(t)]
    return per, lc, len(both), sum(1 for t in both if held[t] & bids[t]), len(live)


if __name__ == '__main__':
    a, b = run(False), run(True)
    md = ['# Machine-side cues for six nodes: held-out detection with and without (2026-10-02)\n',
          f'Overall held-out agreement with turn bids: without {a[3]}/{a[2]}; with {b[3]}/{b[2]}.\n',
          '| node | held-out detected (without → with) | of those, node also bid | turns where agents bid this node | live turns detected (of %d) |' % a[4],
          '|---|---|---|---:|---|']
    for n in SIX:
        (d0, h0, nb), (d1, h1, _) = a[0][n], b[0][n]
        md.append(f'| {n} | {d0} → {d1} | {h0} → {h1} | {nb} | {a[1][n]} → {b[1][n]} |')
    (HERE / 'DETECT-machine-2026-10-02.md').write_text('\n'.join(md) + '\n')
    print('\n'.join(md))
