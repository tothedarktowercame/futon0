#!/usr/bin/env python3
"""Freeze full-text, UUID-deduplicated transcript pairs; never modify transcripts."""
import argparse
from collections import Counter
from datetime import datetime, timezone
import hashlib
import json
from pathlib import Path
import random
import re

SESSIONS = {
    'claude-12': 'd158cebc-06aa-4763-8704-e216a5a39f5c',
    'claude-4': 'af24caa1-54d3-4f19-9d73-c8183eb9cb65',
    'claude-5': 'de4c2047-bf32-4b18-bd55-8f97e94c6252',
}
BLIND_SEED = 20260922


def text_of(row):
    content = row.get('message', {}).get('content', '')
    if isinstance(content, str):
        return content
    return '\n\n'.join(b.get('text', '') for b in content if b.get('type') == 'text')


def incoming(row):
    text = text_of(row)
    if not text.strip() or text.startswith('[compacted: tool result'):
        return 'tool_result_or_empty', ''
    if row.get('isSidechain'):
        return 'sidechain', text
    # Inspect the current envelope, never a quoted envelope in a tool result.
    match = re.match(r'\s*--- CURRENT TURN ---\s*\n(.*?)\n---\s*\n(.*)', text, re.S)
    fields = {}
    body = text
    if match:
        fields = dict(re.findall(r'^([A-Za-z ]+):\s*(.*)$', match[1], re.M))
        body = match[2].strip()
        body = re.sub(r'^Agent: [^\n]+\n\s*User message:\s*\n', '', body).strip()
    if re.search(r'resumed:\s*parked dependencies', body, re.I):
        return 'park_resume_marker', body
    if re.match(r'\s*(?:WAKE(?: CHECKLIST)?\s*:|WAKE CHECKLIST\b|ON WAKE\b|STOOD DOWN FOR THE NIGHT\b)', body, re.I):
        return 'wake_checklist', body
    if fields.get('Origin') == 'agent' or fields.get('Surface') in ('bell', 'whistle', 'auto-bellback'):
        return 'agent_bell', body
    if row.get('isMeta') or row.get('isCompactSummary'):
        return 'harness_housekeeping', body
    if fields.get('Origin') == 'operator' and fields.get('From') == 'joe':
        if body.startswith('Surface: marimo') or fields.get('Surface') == 'marimo':
            return 'notebook_display_unverified', body
        return 'operator', body
    return 'untyped_or_housekeeping', body


def extract(root, cutoff):
    all_pairs = []
    dialogue = []
    manifests = {}
    for agent, sid in SESSIONS.items():
        paths = sorted(root.glob('*/' + sid + '.jsonl*'))
        if not paths:
            raise FileNotFoundError(sid)
        records = {}; occurrences = Counter(); conflicts = set(); files = []
        for path in paths:
            data = path.read_bytes()
            files.append({'path': str(path), 'bytes': len(data), 'sha256': hashlib.sha256(data).hexdigest()})
            for lineno, line in enumerate(data.splitlines(), 1):
                row = json.loads(line)
                if row.get('type') not in ('user', 'assistant') or not row.get('uuid'):
                    continue
                if row.get('timestamp', '') >= cutoff:
                    continue
                uid = row['uuid']; occurrences[uid] += 1
                old = records.get(uid)
                if old and text_of(old) != text_of(row):
                    conflicts.add(uid)
                # Prefer the longest full-text copy, deterministic first path on ties.
                if old is None or (not text_of(row).startswith('[compacted:'), len(text_of(row))) > (not text_of(old).startswith('[compacted:'), len(text_of(old))):
                    records[uid] = {**row, '_path': str(path), '_line': lineno}
        rows = sorted(records.values(), key=lambda r: (r.get('timestamp', ''), r['uuid']))
        excluded = Counter(); exclusion_rows=[]; pairs=[]; finals=[]; last_input=None
        for row in rows:
            if row['type'] == 'assistant':
                content=row.get('message',{}).get('content',[])
                if (not row.get('isSidechain') and row.get('message',{}).get('stop_reason') == 'end_turn'
                        and text_of(row).strip() and not any(b.get('type')=='tool_use' for b in content if isinstance(b,dict))):
                    finals.append(row)
                    dialogue.append({'session':agent, 'uuid':row['uuid'], 'timestamp':row['timestamp'], 'kind':'assistant_final', 'text':text_of(row)})
                continue
            kind, body = incoming(row)
            if kind != 'operator':
                excluded[kind] += 1
                exclusion_rows.append({'uuid':row['uuid'],'timestamp':row.get('timestamp'), 'class':kind,
                                       'excerpt':body[:160], 'path':row['_path'],'line':row['_line']})
                if kind not in ('tool_result_or_empty', 'harness_housekeeping'):last_input=row
                continue
            dialogue.append({'session':agent, 'uuid':row['uuid'], 'timestamp':row['timestamp'], 'kind':'operator', 'text':body})
            # A fresh final must occur after the last non-tool user input. This
            # refuses interrupted turns instead of pairing a stale final across them.
            eligible=[r for r in finals if not last_input or r.get('timestamp','') > last_input.get('timestamp','')]
            if not eligible:
                excluded['operator_without_preceding_final'] += 1
                exclusion_rows.append({'uuid':row['uuid'],'timestamp':row.get('timestamp'), 'class':'operator_without_preceding_final','excerpt':body[:160]})
                last_input=row
                continue
            previous=eligible[-1]
            pair={'pair_id':agent+':'+row['uuid'],'session':agent,'session_id':sid,
                  'operator_uuid':row['uuid'],'operator_at':row['timestamp'],'operator_text':body,
                  'agent_uuid':previous['uuid'],'agent_at':previous['timestamp'],'agent_text':text_of(previous),
                  'operator_source':{'path':row['_path'],'line':row['_line']},
                  'agent_source':{'path':previous['_path'],'line':previous['_line']},
                  'session_order':len(pairs)+1}
            pairs.append(pair);last_input=row
        all_pairs.extend(pairs)
        manifests[agent]={'session_id':sid,'source_files':files,'unique_user_rows':sum(r['type']=='user' for r in rows),
                          'uuid_occurrences':sum(occurrences.values()),'unique_message_uuids':len(records),
                          'conflicting_text_uuids':sorted(conflicts),'eligible_pairs':len(pairs),
                          'exclusions':dict(excluded),'excluded_rows':exclusion_rows,
                          'first_pair_at':pairs[0]['operator_at'] if pairs else None,
                          'last_pair_at':pairs[-1]['operator_at'] if pairs else None}
    return all_pairs,manifests,dialogue


def select_sample(pairs, total=100):
    grouped={s:[p for p in pairs if p['session']==s] for s in SESSIONS}
    quotas={s:0 for s in SESSIONS}
    while sum(quotas.values()) < min(total,len(pairs)):
        for s in SESSIONS:
            if quotas[s]<len(grouped[s]) and sum(quotas.values())<total:quotas[s]+=1
    selected=[]
    for s,rows in grouped.items():
        n=quotas[s]
        # Prefix sampling makes every earlier eligible correction part of the
        # labelled set, rather than treating unlabelled earlier turns as absent.
        selected.extend(rows[:n])
    return sorted(selected,key=lambda p:(list(SESSIONS).index(p['session']),p['operator_at']))


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root',type=Path,default=Path('/home/joe/.claude/projects'))
    parser.add_argument('--out',type=Path,default=Path(__file__).parent)
    parser.add_argument('--cutoff',default=datetime.now(timezone.utc).isoformat().replace('+00:00','Z'))
    a=parser.parse_args();pairs,manifest,dialogue=extract(a.root,a.cutoff);sample=select_sample(pairs)
    a.out.mkdir(parents=True,exist_ok=True)
    for name,rows in [('all_pairs.jsonl',pairs),('sample_pairs.jsonl',sample),('dialogue_context.jsonl',dialogue)]:
        (a.out/name).write_text(''.join(json.dumps(r,ensure_ascii=False)+'\n' for r in rows))
    blind=random.Random(BLIND_SEED).sample([p['pair_id'] for p in sample],min(20,len(sample)))
    (a.out/'blind_ids.json').write_text(json.dumps({'seed':BLIND_SEED,'pair_ids':blind},indent=2)+'\n')
    (a.out/'manifest.json').write_text(json.dumps({'cutoff':a.cutoff,'sampling':'session-stratified chronological prefixes; round-robin quotas to 100','sessions':manifest},indent=2)+'\n')
    for agent,m in manifest.items():print(agent,m['eligible_pairs'],m['exclusions'],'conflicts',len(m['conflicting_text_uuids']))
    print('sample',Counter(p['session'] for p in sample))

if __name__=='__main__':main()
