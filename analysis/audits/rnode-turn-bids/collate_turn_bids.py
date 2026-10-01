"""Collate R-node bids on the 300 blinded turns, unblind with key.json, and compare with two
graph-distance associations from the mined pattern graph. Writes TURN-BIDS-collated-2026-10-01.{md,json}.

Sources kept separate:
  turn   - R-node agents' bids on blinded operator turns (bids whose quote fails the check are dropped)
  graph18 - R-nodes whose catalogue-pegged aif/ pattern (futon3 2fbb58a, @holds-at) is within 2 steps
  graph52 - the same from all 52 @holds-at patterns in the library"""
import json, math, random, re, subprocess
from collections import Counter, defaultdict
from pathlib import Path
import edn_format
from edn_format import Keyword as K

HERE = Path(__file__).resolve().parent
LIB = Path('/home/joe/code/futon3/library')
GRAPH = Path('/home/joe/code/storage/operator-turns/mined-pattern-graph.json')
LOOP = {'PERCEIVE', 'BELIEVE', 'EVALUATE', 'SELECT', 'ACT'}


def nodes():
    return {n['node']: n for n in json.loads((HERE.parent / 'rnode-bids' / 'nodes.json').read_text())}


def bids(turn_text):
    out, dropped = [], 0
    for f in sorted(HERE.glob('*-turns.edn')):
        d = edn_format.loads(f.read_text())
        for b in d.get(K('bids')) or []:
            t, q = b.get(K('turn')), ' '.join(str(b.get(K('quote')) or '').split())
            if t not in turn_text or not q or q not in turn_text[t]:
                dropped += 1
                continue
            out.append({'node': d[K('node')], 'turn': t, 'strength': str(b[K('strength')]).lstrip(':'),
                        'quote': q, 'why': b.get(K('why'))})
    return out, dropped


def holds_at():
    pegged = set(subprocess.check_output(
        ['git', '-C', '/home/joe/code/futon3', 'show', '--name-only', '--format=', '2fbb58a'], text=True).split())
    all_, cat = defaultdict(set), defaultdict(set)
    for f in LIB.rglob('*.flexiarg'):
        s = f.read_text(errors='replace')
        m, h = re.search(r'^@flexiarg (\S+)', s, re.M), re.search(r'^@holds-at (\S+)', s, re.M)
        if m and h:
            all_[h.group(1)].add(m.group(1))
            if str(f.relative_to('/home/joe/code/futon3')) in pegged:
                cat[h.group(1)].add(m.group(1))
    return cat, all_


def near(adj, tags, pattern, radius=2):
    dist, frontier = {pattern: 0}, [pattern]
    for x in frontier:
        if dist[x] < radius:
            for y in adj.get(x, ()):
                if y not in dist:
                    dist[y] = dist[x] + 1
                    frontier.append(y)
    return {r for r, ps in tags.items() if any(p in dist for p in ps)}


def main():
    ns = nodes()
    turns = {t['id']: ' '.join(t['operator-turn'].split()) for t in json.loads((HERE / 'turns.json').read_text())}
    key = json.loads((HERE / 'key.json').read_text())
    bs, dropped = bids(turns)
    for b in bs:
        b['pattern'], b['stage-label'] = key[b['turn']]['pattern'], str(key[b['turn']]['stage'] or 'none').upper()
    adj = defaultdict(set)
    for e in json.loads(GRAPH.read_text())['edges']:
        adj[e['a']].add(e['b']); adj[e['b']].add(e['a'])
    cat, all_ = holds_at()

    # stage consistency: bidder's stage (loop stages; assurance-band nodes count as ASSURANCE) vs pattern label
    node_stage = {n: (v['stage'] if v['band'] == 'loop' else 'ASSURANCE') for n, v in ns.items()}
    same = sum(1 for b in bs if node_stage[b['node']] == b['stage-label'])
    ms, ls = Counter(node_stage[b['node']] for b in bs), Counter(b['stage-label'] for b in bs)
    chance = sum(ms[s] * ls[s] for s in ms) / len(bs) ** 2
    by_label = defaultdict(Counter)
    for b in bs:
        by_label[b['stage-label']][b['node']] += 1

    # per pattern
    pats = sorted({v['pattern'] for v in key.values()})
    turns_of = defaultdict(list)
    for t, v in key.items():
        turns_of[v['pattern']].append(t)
    rows, agree = [], Counter()
    rng = random.Random(1)
    for p in pats:
        c = Counter(b['node'] for b in bs if b['pattern'] == p)
        g18, g52 = near(adj, cat, p), near(adj, all_, p)
        top = [n for n, k in c.items() if k == max(c.values())] if c else []
        row = {'pattern': p, 'stage-label': str(key[turns_of[p][0]]['stage']), 'turn-bids': dict(c.most_common()),
               'top': top, 'graph18': sorted(g18), 'graph52': sorted(g52)}
        rows.append(row)
        for name, g in (('graph18', g18), ('graph52', g52)):
            if top and g:
                agree[name + '-both'] += 1
                agree[name + '-hit'] += any(t in g for t in top)
                # chance: probability that a random node set of the same size as `top` meets g
                N, k, m = len(ns), len(top), len(g & set(ns))
                agree[name + '-chance'] += 1 - math.comb(N - m, k) / math.comb(N, k)
    covered = len({b['turn'] for b in bs})
    per_node = Counter(b['node'] for b in bs)
    out = {'bids': bs, 'dropped-bids': dropped, 'turns-covered': covered, 'patterns': rows,
           'stage-consistency': {'same': same, 'bids': len(bs), 'chance': chance},
           'by-stage-label': {k: dict(v.most_common()) for k, v in by_label.items()},
           'per-node': dict(per_node.most_common()), 'graph-agreement': dict(agree)}
    (HERE / 'TURN-BIDS-collated-2026-10-01.json').write_text(json.dumps(out, indent=1, ensure_ascii=False))

    md = ['# R-node bids on 300 blinded operator turns, collated (2026-10-01)\n',
          f'{len(bs)} bids kept from {len(ns)} nodes ({dropped} dropped: quote not in the turn). '
          f'{covered} of 300 turns have at least one bid.\n',
          f'**Stage consistency:** {same} of {len(bs)} bids ({same / len(bs):.1%}) come from a node in the pattern\'s '
          f'stage label; chance from the marginals {chance:.1%}.\n',
          '**Graph agreement** (pattern\'s top turn-bid node is among the R-nodes within 2 graph steps; '
          'chance = a random node of the same count):\n']
    for name in ('graph18', 'graph52'):
        n = agree[name + '-both']
        if n:
            md.append(f'- {name}: {agree[name + "-hit"]} of {n} patterns ({agree[name + "-hit"] / n:.0%}); '
                      f'chance expectation {agree[name + "-chance"]:.1f} ({agree[name + "-chance"] / n:.0%})')
    md += ['\n## Bids per node\n', ' · '.join(f'{n} {k}' for n, k in per_node.most_common()),
           '\n## R-nodes by pattern stage label\n']
    md += [f'- **{k}**: ' + ', '.join(f'{n} {c}' for n, c in v.most_common()) for k, v in sorted(by_label.items())]
    md += ['\n## Per pattern (3 turns each)\n', '| pattern | label | turn bids | graph18 | graph52 |', '|---|---|---|---|---|']
    md += [f'| `{r["pattern"]}` | {r["stage-label"]} | ' + ', '.join(f'{n} {k}' for n, k in r['turn-bids'].items())
           + f' | {", ".join(r["graph18"])} | {", ".join(r["graph52"])} |' for r in rows]
    (HERE / 'TURN-BIDS-collated-2026-10-01.md').write_text('\n'.join(md) + '\n')
    print('\n'.join(md[:12]))


if __name__ == '__main__':
    main()
