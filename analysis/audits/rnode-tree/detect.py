"""Do the keywords detect anything? A plain matcher: every cue from rnode-tree.edn (leaf cues
only) plus every kept cue from ELAB-collated.json, matched case-insensitively at word
boundaries ("..." in a cue matches up to 40 characters). Runs over (a) the 150 held-out turns
and (b) every operator turn in ~/.emacs-graph/session-turn-analysis, >>> quotes removed.
On (a) compares with the R-node turn bids (independent agent judgements).
Writes DETECT-2026-10-02.{md,json}."""
import json, re
from collections import Counter, defaultdict
from pathlib import Path
import edn_format
from edn_format import Keyword as K

HERE = Path(__file__).resolve().parent
LIVE = Path.home() / '.emacs-graph/session-turn-analysis'


def strip_quotes(text):
    return re.split(r'^\s*>>>', text, maxsplit=1, flags=re.M)[0]


def cues():
    out = defaultdict(set)
    tree = edn_format.loads((HERE / 'rnode-tree.edn').read_text())
    def walk(n):
        if K('leaf') in n:
            out[n[K('leaf')]].update(n.get(K('cues')) or [])
        for c in n.get(K('children')) or []:
            walk(c)
    for b in tree[K('branches')]:
        walk(b)
    for c in json.loads((HERE / 'ELAB-collated.json').read_text())['kept-cues']:
        if c['node'] in out:            # leaf cues only; branch cues do not name a node
            out[c['node']].add(c['cue'])
    if MACHINE:
        for node, cl in edn_format.loads((HERE / 'machine-cues.edn').read_text())[K('cues')].items():
            out[node].update(cl)
    return out


def compile_cue(c):
    parts = [re.escape(p.strip()) for p in re.split(r'\.\.\.|…', c.lower()) if p.strip()]
    if not parts:
        return None
    return re.compile(r'(?<!\w)' + r'.{0,40}?'.join(parts) + r'(?!\w)', re.S)


def detect(text, pats):
    t = strip_quotes(text).lower()
    return {node: [c for c, rx in ps if rx.search(t)] for node, ps in pats.items() if any(rx.search(t) for _, rx in ps)}


STOP = set()
MACHINE = False   # add machine-cues.edn (codex-10 narrative) when True


def main():
    cs = cues()
    cs = {n: {c for c in v if c.lower() not in STOP} for n, v in cs.items()}
    pats = {n: [(c, rx) for c in sorted(v) if (rx := compile_cue(c))] for n, v in cs.items()}
    # (a) held-out turns vs turn bids
    test_ids = set(json.loads((HERE / 'test-ids.json').read_text()))
    turns = {t['id']: t['operator-turn'] for t in json.loads((HERE.parent / 'rnode-turn-bids/turns.json').read_text())
             if t['id'] in test_ids}
    bids = defaultdict(set)
    for b in json.loads((HERE.parent / 'rnode-turn-bids/TURN-BIDS-collated-2026-10-01.json').read_text())['bids']:
        bids[b['turn']].add(b['node'])
    held = {t: detect(x, pats) for t, x in turns.items()}
    both = [t for t in held if held[t] and bids.get(t)]
    hit = sum(1 for t in both if set(held[t]) & bids[t])
    # chance: keyword node sets shuffled across the same turns
    import random
    rng, sims = random.Random(1), []
    sets = [set(held[t]) for t in both]
    for _ in range(2000):
        rng.shuffle(sets)
        sims.append(sum(1 for s, t in zip(sets, both) if s & bids[t]))
    chance = sum(sims) / len(sims)
    # (b) all live operator turns
    live = []
    for f in LIVE.glob('turn-*.json'):
        if f.name.endswith(('.analysis.json', '.candidates.json')):
            continue
        try:
            r = json.loads(f.read_text())
        except Exception:
            continue
        if r.get('origin', 'operator') == 'operator' and (r.get('source_text') or '').strip():
            live.append(r['source_text'])
    live_hits = [detect(x, pats) for x in live]
    node_turns, cue_turns, nhits = Counter(), Counter(), Counter()
    for h in live_hits:
        nhits[min(len(h), 5)] += 1
        for n, cl in h.items():
            node_turns[n] += 1
            for c in cl:
                cue_turns[(n, c)] += 1
    out = {'cues-per-node': {n: len(v) for n, v in cs.items()},
           'held-out': {'turns': len(held), 'with-detection': sum(1 for h in held.values() if h),
                        'with-bids': sum(1 for t in held if bids.get(t)), 'both': len(both),
                        'detected-node-among-bids': hit, 'chance': chance},
           'live': {'turns': len(live), 'nodes-detected-histogram': dict(sorted(nhits.items())),
                    'turns-per-node': dict(node_turns.most_common()),
                    'top-cues': [[n, c, k] for (n, c), k in cue_turns.most_common(40)]}}
    (HERE / 'DETECT-2026-10-02.json').write_text(json.dumps(out, indent=1, ensure_ascii=False))
    L = len(live)
    md = ['# Do the R-node keywords detect anything? (2026-10-02)\n',
          f'{sum(len(v) for v in cs.values())} cues over {len(cs)} leaves (tree + three elaborations).\n',
          '## Held-out turns (150, never shown to the elaborating agents)\n',
          f'- turns with at least one detection: {out["held-out"]["with-detection"]} of {len(held)}',
          f'- turns with both a detection and an R-node turn bid: {len(both)}; a detected node is among the bid nodes '
          f'on {hit} ({hit / max(len(both), 1):.0%}); chance (detections shuffled across those turns) {chance:.1f} '
          f'({chance / max(len(both), 1):.0%})\n',
          f'## All live operator turns ({L})\n',
          '- nodes detected per turn: ' + ', '.join(f'{k}{"+" if k == 5 else ""}: {v} ({v / L:.0%})' for k, v in sorted(nhits.items())),
          '']
    md += ['| node | turns | share |', '|---|---:|---:|']
    md += [f'| {n} | {k} | {k / L:.1%} |' for n, k in node_turns.most_common()]
    md += [f'| {n} | 0 | 0% |' for n in sorted(set(cs) - set(node_turns))]
    md += ['\n## Cues that fire most (live turns)\n'] + [f'- {n} "{c}": {k} ({k / L:.1%})' for (n, c), k in cue_turns.most_common(40)]
    (HERE / 'DETECT-2026-10-02.md').write_text('\n'.join(md) + '\n')
    print('\n'.join(md[:9]))


if __name__ == '__main__':
    main()
