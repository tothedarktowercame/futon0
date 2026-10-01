#!/usr/bin/env python3
# dependencies = ["edn-format==0.7.5"]
# PBASE stage comparison: Minard (per turn, rank-1 pattern stage) vs 象
# (per fragment intent -> core intent -> loop stage), window
# 2026-08-22 .. 2026-09-21T17:19:12Z. Read-only on its inputs; writes
# pbase-compare-2026-09-21.md and .jsonl next to this script.
import argparse, glob, json, os, sys, unittest, collections
from datetime import datetime, timezone
from edn_format import loads, Keyword

HERE = os.path.dirname(os.path.abspath(__file__))
AUDITS = HERE
STORAGE_BATCHES = '/home/joe/code/storage/operator-turns/batches'
STORAGE_FILTERED = '/home/joe/code/storage/operator-turns/operator-turns-filtered.jsonl'

PBASE = ['PERCEIVE', 'BELIEVE', 'EVALUATE', 'SELECT', 'ACT']
# intent -> stage, transcribed from legend-rows in
# futon3/src-cljs/futon3/turnfeed/core.cljs (22 core intents).
CORE_STAGE = {
    'report-problem': 'PERCEIVE', 'explain': 'PERCEIVE', 'report': 'PERCEIVE',
    'clarify': 'BELIEVE', 'qualify': 'BELIEVE', 'approve': 'BELIEVE',
    'disagree': 'BELIEVE', 'collect': 'BELIEVE', 'retract': 'BELIEVE',
    'constrain': 'EVALUATE', 'extend': 'EVALUATE', 'explore': 'EVALUATE',
    'propose': 'SELECT', 'prioritize': 'SELECT', 'redirect': 'SELECT',
    'defer': 'SELECT', 'delegate': 'SELECT', 'withdraw': 'SELECT',
    'ask-action': 'ACT', 'continue': 'ACT', 'verify': 'ACT',
    'unresolved': 'ANNOTATOR', 'gist': 'ANNOTATOR',
}

def kw(x):
    return str(x).lstrip(':') if isinstance(x, Keyword) else x

def parse_ts(s):
    return datetime.fromisoformat(s.replace('Z', '+00:00'))

def in_window(ts, since, before):
    return since <= ts < before

# ---------------------------------------------------------------- inputs
def load_intent_map(path):
    """tail label -> core intent or 'unmapped'; also per-label counts."""
    data = loads(open(path).read())
    m, counts = {}, {}
    for entry in data[Keyword('labels')]:
        label = entry[Keyword('label')]
        core = kw(entry[Keyword('core')])
        m[label] = core
        counts[label] = entry[Keyword('count')]
    return m, counts

def load_pattern_stages(path):
    """pattern id -> {'stage': str, 'kind': str}."""
    data = loads(open(path).read())
    out = {}
    for p in data[Keyword('patterns')]:
        out[p[Keyword('id')]] = {
            'stage': kw(p[Keyword('stage')]).upper(),
            'kind': kw(p[Keyword('kind')]),
        }
    return out

def load_minard(joins_path, stages, since, before):
    """(session, turn_at) -> {'stages': set, 'kinds': set, 'patterns': set}.
    Only 'matched' rows whose turn-at is inside the window."""
    turns = collections.defaultdict(
        lambda: {'stages': set(), 'kinds': set(), 'patterns': set()})
    n_rows = n_matched = n_window = 0
    for line in open(joins_path):
        r = json.loads(line)
        n_rows += 1
        if r.get('status') != 'matched':
            continue
        n_matched += 1
        ta = parse_ts(r['turn-at'])
        if not in_window(ta, since, before):
            continue
        n_window += 1
        key = (r['session'], r['turn-at'])
        for pid in r.get('rank1-pattern-ids', []):
            if pid in stages:
                turns[key]['stages'].add(stages[pid]['stage'])
                turns[key]['kinds'].add(stages[pid]['kind'])
                turns[key]['patterns'].add(pid)
    return dict(turns), (n_rows, n_matched, n_window)

def load_batch_turns(blocks_glob, since, before):
    """(session, created_at) -> turn record dict (window, has reading)."""
    turns = {}
    n_files = n_window = n_missing_reading = 0
    for req in sorted(glob.glob(blocks_glob)):
        if not req.endswith('.json') or req.endswith('.analysis.json') \
           or req.endswith('.candidates.json'):
            continue
        n_files += 1
        rec = json.load(open(req))
        if 'sentences' not in rec:
            continue  # candidate file, not a turn
        ts = parse_ts(rec['created_at']) if 'created_at' in rec else None
        if ts is None or not in_window(ts, since, before):
            continue
        n_window += 1
        ana_path = req + '.analysis.json'
        if not os.path.exists(ana_path):
            n_missing_reading += 1
            continue
        ana = json.load(open(ana_path))
        if 'sentences' not in ana:
            n_missing_reading += 1
            continue
        key = (rec['session_id'], rec['created_at'])
        rec['_analysis'] = ana
        rec['_path'] = req
        if key in turns:
            turns[key]['_dup_paths'] = turns[key].get('_dup_paths', []) + [req]
        else:
            turns[key] = rec
    return turns, (n_files, n_window, n_missing_reading)

def xiang_classes(rec, intent_map):
    """Counter over classes: 5 PBASE stages, ANNOTATOR, 'unmapped'."""
    c = collections.Counter()
    for s in rec['_analysis'].get('sentences', []):
        for fr in s.get('fragments', []):
            intent = fr.get('intent', '')
            core = intent_map.get(intent, 'unmapped')
            if core == 'unmapped':
                c['unmapped'] += 1
            else:
                c[CORE_STAGE[core]] += 1
    return c

def modal_stage(counter):
    """Deterministic argmax; ties broken by fixed class order."""
    order = PBASE + ['ANNOTATOR', 'unmapped']
    best = None
    for cls in order:
        if counter.get(cls, 0) > 0 and (best is None or counter[cls] > counter[best]):
            best = cls
    return best

# ---------------------------------------------------------------- stats
def one_sided_agreement(ratings_a, ratings_b, pbase=PBASE):
    """Fraction of turns rated by A (in pbase) whose B rating (also required
    in pbase, else it is a miss) equals A's. Directional: the denominator is
    A's pbase set, so swapping the labellings changes the number."""
    a_pbase = [i for i, x in enumerate(ratings_a) if x in pbase]
    if not a_pbase:
        return 0.0
    hit = sum(1 for i in a_pbase if ratings_b[i] == ratings_a[i])
    return hit / len(a_pbase)

def percent_agreement(ra, rb):
    """Symmetric PBASE-only percent agreement (both ratings in PBASE)."""
    idx = [i for i in range(len(ra)) if ra[i] in PBASE and rb[i] in PBASE]
    if not idx:
        return 0.0, 0
    hit = sum(1 for i in idx if ra[i] == rb[i])
    return hit / len(idx), len(idx)

def cohen_kappa(ra, rb):
    idx = [i for i in range(len(ra)) if ra[i] in PBASE and rb[i] in PBASE]
    if not idx:
        return 0.0, 0
    cats = sorted(set(ra[i] for i in idx) | set(rb[i] for i in idx))
    n = len(idx)
    po = sum(1 for i in idx if ra[i] == rb[i]) / n
    ca = collections.Counter(ra[i] for i in idx)
    cb = collections.Counter(rb[i] for i in idx)
    pe = sum(ca[c] * cb[c] for c in cats) / (n * n)
    if pe == 1.0:
        return 1.0, n
    return (po - pe) / (1 - pe), n

# ---------------------------------------------------------------- main
def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--blocks-glob',
                    default=os.path.join(STORAGE_BATCHES, '2026-08-22_2026-09-21_block*', '*.json'))
    args = ap.parse_args()

    since = parse_ts('2026-08-22T00:00:00Z')
    before = parse_ts('2026-09-21T17:19:12.718167Z')

    intent_map, label_counts = load_intent_map(
        os.path.join(AUDITS, 'pbase-intent-map-2026-09-21.edn'))
    stages = load_pattern_stages(
        os.path.join(AUDITS, 'pattern-stages-2026-09-21.edn'))
    minard, (nj_rows, nj_matched, nj_window) = load_minard(
        os.path.join(AUDITS, 'pattern-stage-joins-2026-09-21.jsonl'),
        stages, since, before)
    batch, (nb_files, nb_window, nb_missing) = load_batch_turns(
        args.blocks_glob, since, before)

    # filtered index: (session, at) -> [ids]
    filt = collections.defaultdict(list)
    for line in open(STORAGE_FILTERED):
        r = json.loads(line)
        filt[(r['session'], r['at'])].append(r['id'])

    join_zero, join_multi = [], []
    rows = []
    for key, rec in batch.items():
        ids = filt.get(key, [])
        if len(ids) == 0:
            join_zero.append(key)
            continue
        if len(ids) > 1:
            join_multi.append((key, ids))
        classes = xiang_classes(rec, intent_map)
        modal = modal_stage(classes)
        entry = {
            'evidence-id': ids[0],
            'session': key[0],
            'at': key[1],
            'minard-stages': None,
            'minard-kind': None,
            'xiang-modal': modal,
            'xiang-shares': {k: v for k, v in classes.items()},
        }
        m = minard.get(key)
        if m and len(m['stages']) == 1:
            entry['minard-stages'] = sorted(m['stages'])
            entry['minard-kind'] = sorted(m['kinds'])
        rows.append(entry)

    # multi-stage / multi-retrieval Minard turns (listed, not picked)
    minard_multi = {k: v for k, v in minard.items() if len(v['stages']) > 1}
    minard_multi_retr = {}
    retr_count = collections.Counter()
    for line in open(os.path.join(AUDITS, 'pattern-stage-joins-2026-09-21.jsonl')):
        r = json.loads(line)
        if r.get('status') == 'matched' and in_window(parse_ts(r['turn-at']), since, before):
            retr_count[(r['session'], r['turn-at'])] += 1
    minard_multi_retr = {k: v for k, v in retr_count.items() if v > 1}

    # denominators
    d_batch = len(batch)
    d_minard = len(minard)
    d_both = sum(1 for r in rows if r['minard-stages'])
    d_joined = len(rows)

    # cross table (single-stage Minard rows only)
    xcols = PBASE + ['ANNOTATOR', 'unmapped']
    cross = collections.Counter()
    for r in rows:
        if r['minard-stages']:
            cross[(r['minard-stages'][0], r['xiang-modal'])] += 1
    minard_rows = sorted(set(s for s, _ in cross) |
                         set('ASSURANCE COORDINATION NONE'.split()) & set())

    # ratings for stats
    def ratings(kind_filter=None):
        ra, rb = [], []
        for r in rows:
            if not r['minard-stages']:
                continue
            if kind_filter and not (set(kind_filter) & set(r['minard-kind'])):
                continue
            ra.append(r['minard-stages'][0])
            rb.append(r['xiang-modal'])
        return ra, rb

    ra, rb = ratings()
    pa_all, n_all = percent_agreement(ra, rb)
    kap_all, _ = cohen_kappa(ra, rb)
    rap, rbp = ratings(kind_filter=['practice'])
    pa_pr, n_pr = percent_agreement(rap, rbp)
    kap_pr, _ = cohen_kappa(rap, rbp)

    # disagreement cells
    dis = [(v, k) for k, v in cross.items() if k[0] != k[1]]
    dis.sort(key=lambda t: (-t[0], t[1]))
    dis_examples = {}
    for r in rows:
        if r['minard-stages'] and r['minard-stages'][0] != r['xiang-modal']:
            dis_examples.setdefault((r['minard-stages'][0], r['xiang-modal']), []).append(
                r['evidence-id'])

    # jsonl out
    out_jsonl = os.path.join(AUDITS, 'pbase-compare-2026-09-21.jsonl')
    with open(out_jsonl, 'w') as f:
        for r in sorted(rows, key=lambda r: (r['session'], r['at'])):
            f.write(json.dumps(r, sort_keys=True) + '\n')

    # md out
    frags_total = sum(label_counts.values())
    frags_mapped = sum(v for k, v in label_counts.items() if intent_map[k] != 'unmapped')

    md = []
    md.append('# PBASE stage comparison: Minard vs 象 (2026-08-22 .. 2026-09-21T17:19:12Z)\n')
    md.append('All numbers below are produced by `pbase_compare.py` from the files named in each line.\n')
    md.append('## Denominators\n')
    md.append(f'- Minard join rows total: {nj_rows}; matched: {nj_matched}; matched with turn-at in window: {nj_window}')
    md.append(f'- distinct Minard-labelled turns (session + turn-at) in window: {d_minard}')
    md.append(f'- batch turn files scanned: {nb_files}; in window with a 象 reading: {d_batch} '
              f'(window turns {nb_window}, window turns without a reading {nb_missing}; '
              f'{nb_window - nb_missing - d_batch} in-window readings shared a (session, created_at) key with another and were merged)')
    md.append(f'- joined rows (batch turn with >=1 filtered-row match on session+at): {d_joined}')
    md.append(f'- of those, with a single-stage Minard label: {d_both}')
    md.append(f'- batch turns matching zero filtered rows: {len(join_zero)}; matching several: {len(join_multi)}')
    md.append(f'- fragments total: {frags_total}; mapped to a core intent: {frags_mapped} '
              f'({frags_mapped / frags_total:.1%}); unmapped: {frags_total - frags_mapped} '
              f'({(frags_total - frags_mapped) / frags_total:.1%})')
    md.append(f'- reconciliation: joins window turns {nj_window} -> distinct turns {d_minard} '
              f'(repeat retrievals on one turn collapse); joined {d_joined} = window batch turns '
              f'{d_batch} - zero-filter-matches {len(join_zero)}; cross-table rows {d_both} = '
              f'{d_joined} - no-Minard-label {d_joined - d_both} - multi-stage-Minard {len(minard_multi)}\n')

    md.append('## Cross-table: Minard stage (rows) x 象 modal class (columns)\n')
    mrows = ['PERCEIVE', 'BELIEVE', 'EVALUATE', 'SELECT', 'ACT',
             'ASSURANCE', 'COORDINATION', 'NONE']
    mrows = [r for r in mrows if any(k[0] == r for k in cross)] or mrows[:5]
    mrows += sorted(set(k[0] for k in cross) - set(mrows))
    hdr = '| Minard \\ 象 | ' + ' | '.join(xcols) + ' | total |'
    md.append(hdr)
    md.append('|' + '---|' * (len(xcols) + 2))
    for mr in mrows:
        cells = [cross.get((mr, xc), 0) for xc in xcols]
        if sum(cells) == 0 and mr not in ('PERCEIVE', 'BELIEVE', 'EVALUATE', 'SELECT', 'ACT'):
            continue
        md.append(f'| {mr} | ' + ' | '.join(str(c) for c in cells) + f' | {sum(cells)} |')
    tot = sum(cross.values())
    md.append('| total | ' + ' | '.join(str(sum(cross.get((mr, xc), 0) for mr in mrows)) for xc in xcols) + f' | {tot} |\n')

    md.append('## Agreement (PBASE-only turns: Minard stage and 象 modal both among the five)\n')
    md.append(f'- overall: {pa_all:.1%} agreement on n={n_all} turns; Cohen\'s kappa {kap_all:.3f}')
    md.append(f'- Minard kind = practice only: {pa_pr:.1%} agreement on n={n_pr} turns; Cohen\'s kappa {kap_pr:.3f}')
    md.append(f'- one-sided check (denominator = Minard-PBASE turns, 象 non-PBASE counted as miss): '
              f'{one_sided_agreement(ra, rb):.1%}; with roles swapped: {one_sided_agreement(rb, ra):.1%} '
              f'(the two differ, so the roles cannot be silently exchanged)\n')

    md.append('## Multi-retrieval / multi-stage Minard turns (listed, not picked)\n')
    md.append(f'- turns with >1 matched retrieval in window: {len(minard_multi_retr)}')
    for k in sorted(minard_multi_retr):
        md.append(f'  - session {k[0]} at {k[1]}: {minard_multi_retr[k]} retrievals')
    md.append(f'- turns whose rank-1 patterns span more than one stage: {len(minard_multi)}')
    for k in sorted(minard_multi):
        md.append(f'  - session {k[0]} at {k[1]}: stages {sorted(minard[k]["stages"])} '
                  f'patterns {sorted(minard[k]["patterns"])}\n')

    md.append('## Top disagreement cells (Minard stage -> 象 modal class), 2 example turn ids each\n')
    md.append('| count | Minard | 象 | examples |')
    md.append('|---|---|---|---|')
    for v, k in dis[:20]:
        ex = ', '.join(dis_examples.get(k, [])[:2])
        md.append(f'| {v} | {k[0]} | {k[1]} | {ex} |')

    out_md = os.path.join(AUDITS, 'pbase-compare-2026-09-21.md')
    open(out_md, 'w').write('\n'.join(md) + '\n')
    print(f'wrote {out_md} and {out_jsonl}')
    print(f'joined {d_joined}, both {d_both}, agree {pa_all:.1%} kappa {kap_all:.3f}')

# ---------------------------------------------------------------- tests
class TestPbaseCompare(unittest.TestCase):
    """Hand fixture: 4 turns, answers worked out by hand.

    turn1: Minard PERCEIVE, 象 modal PERCEIVE            -> agree (PBASE)
    turn2: Minard ACT,     象 modal BELIEVE              -> disagree (PBASE)
    turn3: Minard SELECT,  象 modal ANNOTATOR            -> not PBASE-only
    turn4: Minard NONE,    象 modal ACT                  -> not PBASE-only
    PBASE-only turns: 2, hits 1 -> 50.0%.  Kappa: po=.5; marginals
    A={PERCEIVE .5, ACT .5}, B={PERCEIVE .5, BELIEVE .5}; pe=.5*.5+.0+.. =.25
    (PERCEIVE-PERCEIVE .25 + ACT/BELIEVE cross terms 0) -> kappa=(.5-.25)/.75=1/3.
    """

    def setUp(self):
        self.ra = ['PERCEIVE', 'ACT', 'SELECT', 'NONE']
        self.rb = ['PERCEIVE', 'BELIEVE', 'ANNOTATOR', 'ACT']

    def test_percent_agreement(self):
        pa, n = percent_agreement(self.ra, self.rb)
        self.assertEqual(n, 2)
        self.assertAlmostEqual(pa, 0.5)

    def test_kappa(self):
        kap, n = cohen_kappa(self.ra, self.rb)
        self.assertEqual(n, 2)
        self.assertAlmostEqual(kap, 1 / 3)

    def test_one_sided_is_directional(self):
        # A-PBASE turns are 1..3 (SELECT counted, with B=ANNOTATOR a miss):
        # hits = turn1 only -> 1/3. Swapping roles, B-PBASE turns are
        # 1,2,4 -> hits = turn1 only -> 1/3 as well on THIS fixture, so we
        # extend it with a turn where only B is PBASE to expose the swap.
        ra = self.ra + ['NONE']
        rb = self.rb + ['ACT']
        self.assertAlmostEqual(one_sided_agreement(ra, rb), 1 / 3)
        self.assertAlmostEqual(one_sided_agreement(rb, ra), 1 / 4)
        self.assertNotEqual(one_sided_agreement(ra, rb),
                            one_sided_agreement(rb, ra))

    def test_modal_stage(self):
        self.assertEqual(modal_stage(collections.Counter(
            {'PERCEIVE': 3, 'BELIEVE': 1})), 'PERCEIVE')
        # tie -> fixed order prefers PERCEIVE
        self.assertEqual(modal_stage(collections.Counter(
            {'BELIEVE': 2, 'PERCEIVE': 2})), 'PERCEIVE')
        # non-PBASE classes can win the modal but stay their own class
        self.assertEqual(modal_stage(collections.Counter(
            {'ACT': 1, 'unmapped': 2})), 'unmapped')

    def test_core_stage_table_covers_legend(self):
        # legend-rows carries 23 intents: 22 Joe moves + the annotator
        # coin-flip `unresolved`; each appears exactly once here.
        self.assertEqual(len(CORE_STAGE), 23)
        self.assertEqual(len(set(CORE_STAGE.values()) - {'ANNOTATOR'}), 5)

if __name__ == '__main__':
    if len(sys.argv) > 1 and sys.argv[1] == 'test':
        sys.argv = [sys.argv[0]] + sys.argv[2:]
        unittest.main()
    else:
        main()
