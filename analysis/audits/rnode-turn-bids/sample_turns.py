"""Blinded turn sample for R-node bidding on turns. Top 100 patterns by hits (all kinds),
3 turns each (seeded random), each turn shown with the tail of the agent message it
answers. Writes turns.json (what bidders see: opaque T-ids, no pattern) and key.json
(T-id -> evidence id, pattern, stage label; NOT shown to bidders)."""
import json, random, urllib.request
from collections import defaultdict
from pathlib import Path

HERE = Path(__file__).resolve().parent
AUD = HERE.parent
JOINS = AUD / 'pattern-stage-joins-2026-10-01-fullwindow.jsonl'
SEED, TOP, PER = 20261001, 100, 3


def evidence(eid):
    req = urllib.request.Request(f'http://localhost:7073/api/alpha/evidence/{eid}', headers={'Accept': 'application/json'})
    with urllib.request.urlopen(req, timeout=30) as r:
        return json.load(r)


def text(ev):
    b = ev.get('evidence/body') or {}
    return b.get('text') or b.get('content') or ''


def main():
    import sys
    sys.path.insert(0, str(AUD))
    from minard_families import load_labels
    labels = load_labels(AUD / 'pattern-stages-2026-10-01.edn')
    by_pat = defaultdict(list)
    for line in JOINS.read_text().splitlines():
        r = json.loads(line)
        if r.get('status') == 'matched':
            by_pat[r['pattern-id']].append(r)
    top = sorted(by_pat, key=lambda p: (-len(by_pat[p]), p))[:TOP]
    rng = random.Random(SEED)
    picked = []
    for p in top:
        rows = sorted(by_pat[p], key=lambda r: r['turn-id'])
        picked += [(p, r) for r in rng.sample(rows, min(PER, len(rows)))]
    rng.shuffle(picked)
    turns, key = [], {}
    for i, (p, r) in enumerate(picked, 1):
        ev = evidence(r['turn-id'])
        prior = ''
        if ev.get('evidence/in-reply-to'):
            try:
                prior = text(evidence(ev['evidence/in-reply-to']))
            except Exception:
                prior = ''
        tid = f'T{i:03d}'
        turns.append({'id': tid, 'at': r['turn-at'][:16], 'agent-said-before (tail)': prior[-500:],
                      'operator-turn': text(ev)[:1500]})
        st = labels.get(p)
        key[tid] = {'evidence-id': r['turn-id'], 'pattern': p,
                    'stage': st if isinstance(st, str) else (st or {}).get('stage')}
    (HERE / 'turns.json').write_text(json.dumps(turns, indent=1, ensure_ascii=False))
    (HERE / 'key.json').write_text(json.dumps(key, indent=1, ensure_ascii=False))
    print(len(turns), 'turns from', len(top), 'patterns;', sum(1 for t in turns if t['agent-said-before (tail)']), 'with prior agent text')


if __name__ == '__main__':
    main()
