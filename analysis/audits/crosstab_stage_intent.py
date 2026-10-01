#!/usr/bin/env python3
# dependencies = ["edn-format==0.7.5"]
# Cross-tab: pattern stage (rank-1 retrieved pattern) x 象 intent stage/R-node
# for operator turns, 2026-08-22 .. 2026-10-01 full window. Read-only on its
# inputs; writes CROSSTAB-stage-intent-2026-10-01.md/.json next to this file.
#
# Join key (stated): pattern-stage join row `turn-id`  ==  turn-analysis base
# record `evidence_id` (both are the filtered-store evidence id, e.g.
# "emacs-<hash>"). Fallback, exact and counted separately: session_id +
# created_at == session + turn-at compared as parsed instants. No text
# matching anywhere.
import argparse, glob, json, os, re, sys, unittest, collections
from datetime import datetime
from edn_format import loads, Keyword

HERE = os.path.dirname(os.path.abspath(__file__))
AUDITS = HERE
TURNS_DIR = '/home/joe/.emacs-graph/session-turn-analysis'
CLJS_LEGEND = '/home/joe/code/futon3/src-cljs/futon3/turnfeed/core.cljs'
CONTROL_STAGES = '/home/joe/code/p4ng/empirics-futon/control-stages.edn'

def kw(x):
    return str(x).lstrip(':') if isinstance(x, Keyword) else x

def parse_ts(s):
    return datetime.fromisoformat(str(s).replace('Z', '+00:00'))

# ---------------------------------------------------------------- legend
def parse_legend(path=CLJS_LEGEND):
    """Parse (def legend-rows ...) out of the cljs source. Returns
    {intent: {'stage': str, 'r': str|None}}. The :r field is '—' for the
    two ANNOTATOR rows; those become None."""
    src = open(path).read()
    m = re.search(r'\(def legend-rows\s*(\[.*)\n\n', src, re.S)
    block = m.group(1)
    out = {}
    # brace-scan each {...} map inside the vector
    depth, start = 0, None
    for i, ch in enumerate(block):
        if ch == '{':
            if depth == 0:
                start = i
            depth += 1
        elif ch == '}':
            depth -= 1
            if depth == 0:
                body = block[start:i + 1]
                sm = re.search(r':stage\s+"([^"]+)"', body)
                im = re.search(r':intents\s+\[([^\]]*)\]', body)
                rm = re.search(r':r\s+"([^"]*)"', body)
                if not (sm and im):
                    continue
                stage = sm.group(1)
                rnode = rm.group(1) if rm else None
                rnode = rnode.split()[0] if rnode else None
                if rnode in ('—', ''):
                    rnode = None
                for intent in re.findall(r'"([^"]+)"', im.group(1)):
                    out[intent] = {'stage': stage, 'r': rnode}
    return out

def load_control_stages(path=CONTROL_STAGES):
    data = loads(open(path).read())
    return {n[Keyword('node')]: {'stage': n[Keyword('stage')],
                                 'band': kw(n[Keyword('band')])}
            for n in data[Keyword('nodes')]}

# ---------------------------------------------------------------- sides
def load_pattern_side(joins_path, stages_path):
    """turn-id -> {'stages': set, 'nodes': set, 'patterns': set} for matched
    rows whose rank-1 pattern has a stage label."""
    data = loads(open(stages_path).read())
    labels = {p[Keyword('id')]: {
        'stage': str(kw(p[Keyword('stage')])).upper(),
        'node': kw(p[Keyword('node')]) or None,
        'kind': kw(p[Keyword('kind')])}
        for p in data[Keyword('patterns')]}
    turns = collections.defaultdict(
        lambda: {'stages': set(), 'nodes': set(), 'patterns': set(), 'kinds': set()})
    n_rows = n_matched = 0
    for line in open(joins_path):
        r = json.loads(line)
        n_rows += 1
        if r.get('status') != 'matched':
            continue
        n_matched += 1
        t = turns[r['turn-id']]
        for pid in r.get('rank1-pattern-ids', []):
            if pid in labels:
                t['stages'].add(labels[pid]['stage'])
                t['patterns'].add(pid)
                t['kinds'].add(labels[pid]['kind'])
                if labels[pid]['node']:
                    t['nodes'].add(labels[pid]['node'])
    return dict(turns), labels, (n_rows, n_matched)

def load_intent_side():
    """Returns (by_evidence, by_session_ts, stats).
    by_evidence: evidence_id -> {'intents': [...], 'cue_labels': [...], 'text'}
    by_session_ts: (session, parsed created_at) -> evidence_id-ish record."""
    by_ev, by_st = {}, {}
    n_turns = n_ana = n_evkey = 0
    for req in sorted(glob.glob(os.path.join(TURNS_DIR, 'turn-*.json'))):
        if req.endswith(('.analysis.json', '.candidates.json')):
            continue
        rec = json.load(open(req))
        n_turns += 1
        ana_path = req + '.analysis.json'
        intents, cue_labels = [], []
        if os.path.exists(ana_path):
            ana = json.load(open(ana_path))
            if 'sentences' in ana:
                n_ana += 1
                for s in ana['sentences']:
                    for fr in s.get('fragments', []):
                        intents.append(fr.get('intent', ''))
        for s in rec.get('sentences') or []:
            for c in s.get('cues') or []:
                if isinstance(c, dict) and c.get('label'):
                    cue_labels.append(c['label'])
                elif isinstance(c, str):
                    cue_labels.append(c)
        entry = {'intents': intents, 'cue_labels': cue_labels,
                 'text': rec.get('source_text', ''),
                 'turn_id': rec.get('turn_id'),
                 'session': rec.get('session_id'),
                 'created_at': rec.get('created_at')}
        ev = rec.get('evidence_id')
        if ev:
            n_evkey += 1
            by_ev[ev] = entry
        by_st[(rec.get('session_id'), parse_ts(rec['created_at']))] = entry
    return by_ev, by_st, (n_turns, n_ana, n_evkey)

# ---------------------------------------------------------------- stats
def stage_agreement(pairs):
    """pairs: list of (pattern_stage, intent_stage). Returns
    (matrix Counter, per-stage agreement dict, overall agreement)."""
    matrix = collections.Counter(p for p in pairs)
    agree = sum(1 for a, b in pairs if a == b)
    overall = agree / len(pairs) if pairs else 0.0
    per = {}
    for stage in sorted(set(a for a, _ in pairs)):
        rows = [b for a, b in pairs if a == stage]
        per[stage] = (sum(1 for b in rows if b == stage), len(rows))
    return matrix, per, overall

def fixture_matrix(pairs):
    """Pure helper used by the test: returns the matrix Counter only."""
    return collections.Counter(pairs)

# ---------------------------------------------------------------- main
def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--joins', default=os.path.join(
        AUDITS, 'pattern-stage-joins-2026-10-01-fullwindow.jsonl'))
    ap.add_argument('--stages', default=os.path.join(
        AUDITS, 'pattern-stages-2026-10-01.edn'))
    args = ap.parse_args()

    legend = parse_legend()
    nodes = load_control_stages()
    pturns, labels, (nj_rows, nj_matched) = load_pattern_side(args.joins, args.stages)
    by_ev, by_st, (nt_turns, nt_ana, nt_evkey) = load_intent_side()

    # join
    joined, join_by_ev, join_by_ts, join_fail = {}, [], [], []
    for tid, t in pturns.items():
        rec = by_ev.get(tid)
        key = 'evidence_id'
        if rec is None:
            rec = by_st.get((None, None))  # never; placeholder
        if rec is None:
            # exact fallback: session + parsed created_at vs join row
            # needs the join row's session/turn-at, which we did not keep;
            # re-scan joins for this turn-id
            rec = _fallback_lookup(tid, args.joins, by_st)
            key = 'session+timestamp'
        if rec is None:
            join_fail.append(tid)
            continue
        (join_by_ev if key == 'evidence_id' else join_by_ts).append(tid)
        joined[tid] = {'pattern': t, 'record': rec}

    n_multi = [t for t, j in joined.items() if len(j['pattern']['stages']) > 1]

    # pair- and turn-level tables
    pairs, turn_pairs = [], []
    cue_pairs = []  # secondary: cue-only intents (base-record cues[].label)
    unknown_intents = collections.Counter()
    rnode_counts = collections.defaultdict(collections.Counter)
    patnode_counts = collections.defaultdict(collections.Counter)
    node_equal = [0, 0]
    per_turn_rows = []
    for tid, j in joined.items():
        if len(j['pattern']['stages']) > 1:
            continue  # ambiguous pattern stage; counted separately
        pstage = next(iter(j['pattern']['stages']))
        staged = [(i, legend[i]) for i in j['record']['intents']
                  if i in legend]
        for i in (j['record']['intents']):
            if i not in legend:
                unknown_intents[i] += 1
        for i, lg in staged:
            pairs.append((pstage, lg['stage']))
            if pstage == lg['stage']:
                rnode_counts[pstage][lg['r'] or 'none'] += 1
                pnode = next(iter(j['pattern']['nodes'])) if len(j['pattern']['nodes']) == 1 else None
                if pnode:
                    patnode_counts[pstage][pnode] += 1
                    node_equal[1] += 1
                    if pnode == lg['r']:
                        node_equal[0] += 1
        for c in j['record']['cue_labels']:
            if c in legend:
                cue_pairs.append((pstage, legend[c]['stage']))
        if staged:
            first = staged[0][1]
            turn_pairs.append((pstage, first['stage']))
        per_turn_rows.append({
            'turn-id': tid, 'pattern-stage': pstage,
            'pattern-nodes': sorted(j['pattern']['nodes']),
            'intents': j['record']['intents'],
            'intent-stages': [lg['stage'] for _, lg in staged],
            'first-intent-stage': first['stage'] if staged else None,
        })

    matrix, per, overall = stage_agreement(pairs)
    tmatrix, tper, toverall = stage_agreement(turn_pairs)
    cmatrix, cper, coverall = stage_agreement(cue_pairs)

    istages = sorted(set(b for _, b in pairs)) or ['PERCEIVE', 'BELIEVE', 'EVALUATE', 'SELECT', 'ACT', 'ANNOTATOR']
    pstages = sorted(set(a for a, _ in pairs) | {'ASSURANCE', 'COORDINATION', 'NONE'})
    pstages = [s for s in ['PERCEIVE', 'BELIEVE', 'EVALUATE', 'SELECT', 'ACT', 'ASSURANCE', 'COORDINATION', 'NONE'] if s in pstages]

    # disagreement cells
    dis = sorted(((v, k) for k, v in matrix.items() if k[0] != k[1]),
                 key=lambda t: (-t[0], t[1]))
    dis_ex = collections.defaultdict(list)
    for tid, j in joined.items():
        if len(j['pattern']['stages']) != 1:
            continue
        pstage = next(iter(j['pattern']['stages']))
        for i in j['record']['intents']:
            if i in legend and legend[i]['stage'] != pstage and len(dis_ex[(pstage, legend[i]['stage'])]) < 3:
                dis_ex[(pstage, legend[i]['stage'])].append(
                    (j['record']['text'][:120].replace('\n', ' '),
                     next(iter(j['pattern']['patterns'])), i))

    out = {
        'join': {
            'key-primary': 'join turn-id == analysis evidence_id',
            'key-fallback': 'session + parsed created_at == session + parsed turn-at (exact instants, no text matching)',
            'joins-rows': nj_rows, 'joins-matched': nj_matched,
            'pattern-turns': len(pturns),
            'analysis-turns': nt_turns, 'analysis-with-reading': nt_ana,
            'analysis-with-evidence-id': nt_evkey,
            'joined': len(joined), 'via-evidence-id': len(join_by_ev),
            'via-session-timestamp': len(join_by_ts),
            'join-failed': len(join_fail), 'join-failed-ids': join_fail,
            'multi-stage-pattern-turns': len(n_multi),
        },
        'legend-intents': len(legend),
        'pairs': len(pairs), 'turn-pairs': len(turn_pairs),
        'unknown-intent-fragments': sum(unknown_intents.values()),
        'unknown-intents-top': unknown_intents.most_common(10),
        'matrix-pairs': {f'{a}|{b}': v for (a, b), v in sorted(matrix.items())},
        'matrix-per-turn-first-intent': {f'{a}|{b}': v for (a, b), v in sorted(tmatrix.items())},
        'cue-only-pairs': len(cue_pairs),
        'cue-only-matrix': {f'{a}|{b}': v for (a, b), v in sorted(cmatrix.items())},
        'cue-only-agreement-overall': coverall,
        'agreement-overall-pairs': overall,
        'agreement-overall-per-turn': toverall,
        'agreement-per-stage': {k: {'hits': v[0], 'n': v[1],
                                    'rate': v[0] / v[1]}
                                for k, v in per.items()},
        'rnodes-in-agreeing-pairs': {s: dict(c) for s, c in rnode_counts.items()},
        'pattern-nodes-in-agreeing-pairs': {s: dict(c) for s, c in patnode_counts.items()},
        'node-equality': {'both-present': node_equal[1], 'equal': node_equal[0]},
        'disagreement-examples': {f'{k[0]}->{k[1]}': v for k, v in dis_ex.items()},
    }
    json.dump(out, open(os.path.join(AUDITS, 'CROSSTAB-stage-intent-2026-10-01.json'), 'w'),
              indent=1, ensure_ascii=False)

    md = ['# Cross-tab: pattern stage x 象 intent stage (2026-08-22 .. 2026-10-01)\n']
    md.append('All numbers are produced by `crosstab_stage_intent.py`.\n')
    j = out['join']
    md.append('## Join\n')
    md.append(f"- join rows: {j['joins-rows']}, matched: {j['joins-matched']}, "
              f"distinct pattern-labelled turns: {j['pattern-turns']}")
    md.append(f"- analysis records: {j['analysis-turns']} (with a reading {j['analysis-with-reading']}; "
              f"with evidence_id {j['analysis-with-evidence-id']})")
    md.append(f"- primary key (join turn-id == evidence_id): {j['via-evidence-id']} turns")
    md.append(f"- exact fallback (session + parsed instant): {j['via-session-timestamp']} turns; "
              f"text matching used: none")
    md.append(f"- joined: {j['joined']}; could not join: {j['join-failed']} "
              f"(pattern turn with no analysis record under either key); "
              f"multi-stage pattern turns set aside: {j['multi-stage-pattern-turns']}")
    md.append(f"- fragments with an intent outside legend-rows: {out['unknown-intent-fragments']} "
              f"(top: {', '.join(f'{k} x{v}' for k, v in out['unknown-intents-top'][:5])})\n")

    md.append('## 1. Stage agreement matrix (per (turn, intent) pair)\n')
    md.append('| pattern \\ intent | ' + ' | '.join(istages) + ' | total | cue-only |')
    md.append('|' + '---|' * (len(istages) + 3))
    for ps in pstages:
        cells = [matrix.get((ps, i), 0) for i in istages]
        md.append(f'| {ps} | ' + ' | '.join(map(str, cells)) +
                  f' | {sum(cells)} | {sum(cmatrix.get((ps, i), 0) for i in istages)} |')
    md.append(f'\nCue-only intents (secondary column, base-record cues[].label '
              f'mapped through legend-rows): {len(cue_pairs)} pairs, '
              f'agreement {coverall:.1%}.\n')
    md.append('\nPer-turn (first staged intent) overall agreement: '
              f'{toverall:.1%} on n={len(turn_pairs)} turns; per-pair agreement '
              f'{overall:.1%} on n={len(pairs)} pairs.\n')
    md.append('Per-pattern-stage agreement (pairs):')
    md.append('| pattern stage | agreeing | total | rate |')
    md.append('|---|---|---|---|')
    for ps in pstages:
        if ps in per:
            h, n = per[ps]
            md.append(f'| {ps} | {h} | {n} | {h / n:.1%} |')

    md.append('\n## 2. R-nodes of agreeing pairs, per stage\n')
    md.append('| stage | intent R-node counts | pattern :node counts (where present) |')
    md.append('|---|---|---|')
    for ps in pstages:
        if ps in rnode_counts or ps in patnode_counts:
            md.append(f"| {ps} | {dict(rnode_counts.get(ps, {}))} | {dict(patnode_counts.get(ps, {}))} |")

    md.append('\n## 3. Pattern :node vs intent R-node equality (both present)\n')
    md.append(f"- both present: {node_equal[1]}; equal: {node_equal[0]}"
              f" ({node_equal[0] / node_equal[1]:.1%})" if node_equal[1] else
              '  - both present: 0')

    md.append('\n## 4. Examples for the 4 largest disagreement cells\n')
    for v, k in dis[:4]:
        md.append(f"### {k[0]} -> {k[1]} ({v} pairs)")
        for text, pid, intent in dis_ex[k]:
            md.append(f'- `{pid}` / intent `{intent}`: "{text}"')
    open(os.path.join(AUDITS, 'CROSSTAB-stage-intent-2026-10-01.md'), 'w').write(
        '\n'.join(md) + '\n')
    print('pairs', len(pairs), 'agreement %.1f%% kappa-less' % (100 * overall),
          'joined', len(joined), 'via-ev', len(join_by_ev), 'via-ts', len(join_by_ts))

_FALLBACK_INDEX = None
def _fallback_lookup(tid, joins_path, by_st):
    """Exact fallback join: session + parsed instant. Builds an index of
    turn-id -> (session, instant) from the join rows on first use."""
    global _FALLBACK_INDEX
    if _FALLBACK_INDEX is None:
        _FALLBACK_INDEX = {}
        for line in open(joins_path):
            r = json.loads(line)
            if r.get('status') == 'matched':
                _FALLBACK_INDEX[r['turn-id']] = (
                    r['session'], parse_ts(r['turn-at']))
    key = _FALLBACK_INDEX.get(tid)
    if key is None:
        return None
    return by_st.get(key)

# ---------------------------------------------------------------- tests
class TestCrosstab(unittest.TestCase):
    """Hand fixture (worked out by hand, per-pair matrix):

    turn1: pattern BELIEVE, intents [report(PERCEIVE), clarify(BELIEVE)]
           -> pairs (B,P),(B,B): one agree, one disagree
    turn2: pattern ACT,     intents [continue(ACT)]
           -> pair (A,A): agree
    turn3: pattern PERCEIVE,intents [gist(ANNOTATOR)]
           -> pair (P,ANNOT): disagree (intent stage is a column of its own)
    turn4: pattern SELECT,  intents []  (no staged intents)
           -> no pairs, excluded from per-turn table

    pair matrix: BELIEVE|BELIEVE=1, BELIEVE|PERCEIVE=1, ACT|ACT=1,
                 PERCEIVE|ANNOTATOR=1.  Agreement = 2/4 = 50%.
    Per-stage agreement: BELIEVE 1/2, ACT 1/1, PERCEIVE 0/1.
    Per-turn (first staged intent): turns 1-3 -> (B,P),(A,A),(P,ANNOT):
    agreement 1/3.
    R-node equality: give turn2's pattern :node R16 and continue's R-node
    R16 -> 1/1 equal; turn1's pattern has no :node -> not counted.
    """

    def test_matrix_and_agreement(self):
        legend = {
            'report': {'stage': 'PERCEIVE', 'r': 'R2'},
            'clarify': {'stage': 'BELIEVE', 'r': 'R7'},
            'continue': {'stage': 'ACT', 'r': 'R16'},
            'gist': {'stage': 'ANNOTATOR', 'r': None},
        }
        turns = [
            ({'stages': {'BELIEVE'}, 'nodes': set(), 'patterns': {'p/x'}},
             ['report', 'clarify']),
            ({'stages': {'ACT'}, 'nodes': {'R16'}, 'patterns': {'p/y'}},
             ['continue']),
            ({'stages': {'PERCEIVE'}, 'nodes': set(), 'patterns': {'p/z'}},
             ['gist']),
            ({'stages': {'SELECT'}, 'nodes': set(), 'patterns': {'p/w'}},
             []),
        ]
        pairs = []
        for pat, intents in turns:
            if len(pat['stages']) != 1:
                continue
            pstage = next(iter(pat['stages']))
            for i in intents:
                pairs.append((pstage, legend[i]['stage']))
        self.assertEqual(fixture_matrix(pairs), collections.Counter({
            ('BELIEVE', 'BELIEVE'): 1, ('BELIEVE', 'PERCEIVE'): 1,
            ('ACT', 'ACT'): 1, ('PERCEIVE', 'ANNOTATOR'): 1}))
        matrix, per, overall = stage_agreement(pairs)
        self.assertAlmostEqual(overall, 0.5)
        self.assertEqual(per['BELIEVE'], (1, 2))
        self.assertEqual(per['ACT'], (1, 1))
        self.assertEqual(per['PERCEIVE'], (0, 1))
        # per-turn first-intent table
        tp = []
        for pat, intents in turns:
            if len(pat['stages']) == 1 and intents:
                tp.append((next(iter(pat['stages'])), legend[intents[0]]['stage']))
        _, _, toverall = stage_agreement(tp)
        self.assertAlmostEqual(toverall, 1 / 3)

    def test_parse_legend(self):
        legend = parse_legend()
        # 23 legend intents: 22 Joe moves + annotator `unresolved`
        self.assertEqual(len(legend), 23)
        self.assertEqual(legend['report']['stage'], 'PERCEIVE')
        self.assertEqual(legend['report']['r'], 'R2')
        self.assertEqual(legend['verify'], {'stage': 'ACT', 'r': 'R9'})
        self.assertEqual(legend['gist'], {'stage': 'ANNOTATOR', 'r': None})
        self.assertEqual(legend['delegate'], {'stage': 'SELECT', 'r': 'R11'})

if __name__ == '__main__':
    if len(sys.argv) > 1 and sys.argv[1] == 'test':
        sys.argv = [sys.argv[0]] + sys.argv[2:]
        unittest.main()
    else:
        main()
