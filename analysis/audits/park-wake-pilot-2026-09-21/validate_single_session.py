#!/usr/bin/env python3
"""Independent direct-source recount of today's claude-5 session; no extractor imports."""
from collections import Counter
import json
from pathlib import Path
import re

DATA=Path(__file__).parent
SID='de4c2047-bf32-4b18-bd55-8f97e94c6252'
summary=json.loads((DATA/'summary.json').read_text());cutoff=summary['cutoff']
records={}
for path in sorted(Path('/home/joe/.claude/projects').glob('*/'+SID+'.jsonl*')):
    for line in path.open():
        row=json.loads(line)
        if row.get('type') not in ('assistant','user') or row.get('timestamp','z')>=cutoff:continue
        if row.get('isSidechain'):continue
        uid=row['uuid']
        if uid not in records or len(json.dumps(row))>len(json.dumps(records[uid])):records[uid]=row
seen=set();kind='other';totals={c:Counter() for c in ['wake','bell-in','operator','other']}
for row in sorted(records.values(),key=lambda r:(r['timestamp'],r['uuid'])):
    message=row['message'];content=message['content'];text=content if isinstance(content,str) else '\n'.join(b.get('text','') for b in content)
    if row['type']=='user':
        if (isinstance(content,list) and any(b.get('type')=='tool_result' for b in content)) or text.startswith('[compacted: tool result'):continue
        env=re.match(r'\s*--- CURRENT TURN ---\s*\n(.*?)\n---\s*\n(.*)',text,re.S)
        header=env[1] if env else '';body=env[2].strip() if env else text
        body=re.sub(r'^Agent: [^\n]+\n\s*User message:\s*\n','',body)
        if '\n--- resumed: parked dependencies' in body or body.startswith('WAKE CHECKLIST'):kind='wake'
        elif 'Origin: agent' in header:kind='bell-in'
        elif 'Origin: operator' in header or 'From: joe' in header:kind='operator'
        else:kind='other'
    else:
        uid=message.get('id') or 'uuid:'+row['uuid']
        if uid in seen:continue
        seen.add(uid)
        if not summary['start']<=row['timestamp'][:10]<=summary['end']:continue
        for k in ['input_tokens','cache_read_input_tokens','cache_creation_input_tokens','output_tokens']:
            totals[kind][k]+=message.get('usage',{}).get(k,0)
expected=next(s['totals'] for s in summary['sessions'] if s['session_id']==SID)
print('Differences:',{c:{k:(totals[c][k],expected[c].get(k,0)) for k in ['input_tokens','cache_read_input_tokens','cache_creation_input_tokens','output_tokens'] if totals[c][k]!=expected[c].get(k,0)} for c in totals})
assert all(totals[c][k]==expected[c].get(k,0) for c in totals for k in ['input_tokens','cache_read_input_tokens','cache_creation_input_tokens','output_tokens'])
print(json.dumps({'session':SID,'direct_source_totals':totals,'matches_summary':True},indent=2))
