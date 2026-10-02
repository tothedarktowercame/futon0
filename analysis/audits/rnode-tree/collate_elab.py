"""Collate elab-<agent>.edn files: drop cues whose quote is not in the cited dev turn; then
slices -- cues per node per agent, cues proposed by >=2 agents, collisions by node pair,
placement flags, Max-Neef verdicts per cell. Writes ELAB-collated.{md,json}."""
import json, re
from collections import defaultdict
from pathlib import Path
import edn_format
from edn_format import Keyword as K

HERE = Path(__file__).resolve().parent
norm = lambda s: ' '.join(str(s).split()).lower()
TURNS = {t['id']: norm(t['operator-turn']) for t in json.loads((HERE / 'dev-turns.json').read_text())}


def leaves(tree):
    out = set()
    def walk(n):
        if K('leaf') in n: out.add(n[K('leaf')])
        for c in n.get(K('children')) or []: walk(c)
    for b in tree[K('branches')]: walk(b)
    return out


def s(v):
    return str(v).lstrip(':') if v is not None else None


def cue_key(c):
    return re.sub(r'[^a-z0-9 ]', '', norm(c)).strip()


def main():
    tree = edn_format.loads((HERE / 'rnode-tree.edn').read_text())
    valid = leaves(tree) | {s(b[K('beer')]) for b in tree[K('branches')]} | {'none'}
    agents, cues, dropped, bad_node = [], [], defaultdict(int), defaultdict(int)
    colls, flags, mn = [], [], defaultdict(dict)
    for f in sorted(HERE.glob('elab-*.edn')):
        d = edn_format.loads(f.read_text())
        a = d.get(K('agent')) or f.stem[5:]
        agents.append(a)
        for c in d.get(K('cues')) or []:
            node = s(c.get(K('node')))
            if node not in valid:
                bad_node[a] += 1; continue
            t, q = c.get(K('turn')), c.get(K('quote'))
            general = s(c.get(K('from'))) == 'general'
            if not general and (t not in TURNS or not q or norm(q) not in TURNS[t]):
                dropped[a] += 1; continue
            cues.append({'agent': a, 'node': node, 'cue': c.get(K('cue')), 'turn': t, 'quote': q, 'general': general})
        for c in d.get(K('collisions')) or []:
            colls.append({'agent': a, 'cue': c.get(K('cue')), 'nodes': sorted(s(n) for n in c.get(K('nodes')) or []),
                          'why': c.get(K('why'))})
        for p in d.get(K('placement-flags')) or []:
            flags.append({'agent': a, 'node': s(p.get(K('node'))), 'suggest': p.get(K('suggest')), 'why': p.get(K('why'))})
        for m in d.get(K('max-neef')) or []:
            cell = '·'.join(s(x) for x in m.get(K('cell')) or [])
            mn[cell][a] = {'ok': m.get(K('pairing-ok')), 'conf': s(m.get(K('confidence'))),
                           'terms': list(m.get(K('terms')) or []), 'note': m.get(K('note'))}
    by_cue = defaultdict(set)
    for c in cues:
        by_cue[(c['node'], cue_key(c['cue']))].add(c['agent'])
    shared = sorted(((n, k, sorted(v)) for (n, k), v in by_cue.items() if len(v) >= 2), key=lambda x: x[0])
    per = defaultdict(lambda: defaultdict(int))
    for c in cues: per[c['node']][c['agent']] += 1
    pairs = defaultdict(list)
    for c in colls:
        ns = c['nodes']
        for i in range(len(ns)):
            for j in range(i + 1, len(ns)):
                pairs[(ns[i], ns[j])].append((c['agent'], c['cue']))
    flag_nodes = defaultdict(set)
    for p in flags: flag_nodes[p['node']].add(p['agent'])
    # turn-level comparison: which node(s) each agent cites a given dev turn for
    turn_nodes = defaultdict(lambda: defaultdict(set))
    for c in cues:
        if c['turn']: turn_nodes[c['turn']][c['agent']].add(c['node'])
    multi = {t: v for t, v in turn_nodes.items() if len(v) >= 2}
    same_turn_agree = {t: v for t, v in multi.items() if set.intersection(*v.values())}
    same_turn_differ = {t: v for t, v in multi.items() if not set.intersection(*v.values())}
    out = {'agents': agents, 'kept-cues': cues, 'dropped-quote': dict(dropped), 'bad-node': dict(bad_node),
           'turns-cited-by-2+': len(multi), 'turn-agree': {t: {a: sorted(n) for a, n in v.items()} for t, v in same_turn_agree.items()},
           'turn-differ': {t: {a: sorted(n) for a, n in v.items()} for t, v in same_turn_differ.items()},
           'shared-cues': shared, 'collisions': colls, 'flags': flags, 'max-neef': mn}
    (HERE / 'ELAB-collated.json').write_text(json.dumps(out, indent=1, ensure_ascii=False))
    md = [f'# R-node tree elaboration, collated ({", ".join(agents)})\n',
          f'Kept {len(cues)} cues. Dropped (quote not in cited turn): {dict(dropped)}; unknown node: {dict(bad_node)}.\n',
          f'**Turn level:** {len(multi)} dev turns are cited by 2+ agents; on {len(same_turn_agree)} they share a node, '
          f'on {len(same_turn_differ)} they name only different nodes.\n',
          '## Turns cited by 2+ agents for different nodes\n'] + [
          f'- {t}: ' + '; '.join(f'{a} {"/".join(sorted(n))}' for a, n in v.items()) for t, v in sorted(same_turn_differ.items())] + [
          f'\n## Cues proposed by 2+ agents, same node and wording ({len(shared)})\n'] + [f'- {n}: "{k}" — {", ".join(v)}' for n, k, v in shared]
    md += ['\n## Cues per node\n', '| node | ' + ' | '.join(agents) + ' |', '|---' * (len(agents) + 1) + '|']
    md += [f'| {n} | ' + ' | '.join(str(per[n].get(a, 0)) for a in agents) + ' |' for n in sorted(per)]
    md += ['\n## Collisions by node pair (agents naming the pair)\n']
    md += [f'- {a} / {b}: ' + '; '.join(f'{ag}: "{cu}"' for ag, cu in v)
           for (a, b), v in sorted(pairs.items(), key=lambda kv: -len({x[0] for x in kv[1]}))]
    md += ['\n## Placement flags\n'] + [f'- **{p["node"]}** ({p["agent"]}; flagged by {len(flag_nodes[p["node"]])} agent(s)): '
                                         f'suggest {p["suggest"]} — {p["why"]}' for p in sorted(flags, key=lambda p: p['node'])]
    md += ['\n## Max-Neef cells\n', '| cell | ' + ' | '.join(agents) + ' |', '|---' * (len(agents) + 1) + '|']
    for cell in sorted(mn):
        md.append(f'| {cell} | ' + ' | '.join(
            (f'{"ok" if v["ok"] else "NOT ok" if v["ok"] is False else "?"} ({v["conf"]}): ' + ', '.join(v['terms'][:6]))
            if (v := mn[cell].get(a)) else '—' for a in agents) + ' |')
    (HERE / 'ELAB-collated.md').write_text('\n'.join(md) + '\n')
    print(md[1].strip()); print(md[2].strip()); print(f'shared cues {len(shared)}; collisions {len(colls)}; flags {len(flags)}; cells {len(mn)}')


if __name__ == '__main__':
    main()
