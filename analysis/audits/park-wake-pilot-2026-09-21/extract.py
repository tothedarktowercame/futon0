#!/usr/bin/env python3
"""Read-only Claude transcript union. Freeze usage events and observable turn actions."""
import argparse
from collections import Counter, defaultdict
from datetime import datetime, timezone
import hashlib
import gzip
import json
from pathlib import Path
import re
import shlex

COMPONENTS=('input_tokens','cache_read_input_tokens','cache_creation_input_tokens','output_tokens')


def text_of(row):
    c=row.get('message',{}).get('content',[])
    return c if isinstance(c,str) else '\n'.join(b.get('text','') for b in c if b.get('type')=='text')


def tool_result(row):
    c=row.get('message',{}).get('content',[])
    return (isinstance(c,list) and any(b.get('type')=='tool_result' for b in c)) or text_of(row).startswith('[compacted: tool result')


def command_actions(command):
    # Drop heredoc payloads before inspecting executed shell command tokens.
    lines=command.splitlines();kept=[];delimiter=None
    for line in lines:
        if delimiter:
            if line.strip()==delimiter:delimiter=None
            continue
        kept.append(line)
        m=re.search(r"<<-?\s*['\"]?([A-Za-z_][A-Za-z_0-9]*)['\"]?",line)
        if m:delimiter=m.group(1)
    try:
        lexer=shlex.shlex('\n'.join(kept),posix=True,punctuation_chars=';&|()\n');lexer.whitespace=' \t\r';lexer.whitespace_split=True
        tokens=list(lexer)
    except ValueError:return {'kind':'unknown','park':False,'dispatch':False}
    parts=[];part=[]
    for token in tokens:
        if token and all(c in ';&|()\n' for c in token):
            if part:parts.append(part);part=[]
        else:part.append(token)
    if part:parts.append(part)
    park=dispatch=False;status=False;unknown=False
    for part in parts:
        first=Path(part[0]).name
        if first in ('echo','printf','cat'):unknown=True;continue
        scripts=[i for i,t in enumerate(part) if Path(t).name in ('agency_send.py','agency_send')]
        if scripts and (scripts[0]==0 or first in ('python','python3','uv','timeout','env')):
            park |= '--park' in part
            dispatch |= '--to' in part and ('--kind' not in part or part[part.index('--kind')+1:part.index('--kind')+2] in (['bell'],['whistle']))
            continue
        if first=='curl':
            method='GET'
            for i,token in enumerate(part):
                if token in ('-X','--request') and i+1<len(part):method=part[i+1].upper()
                if token in ('-d','--data','--data-raw','--data-binary'):method='POST' if method=='GET' else method
            # Inspect URL arguments, not URLs quoted inside a JSON prompt payload.
            urls=[t for t in part if re.match(r'^(?:https?://|\$[A-Za-z_{])',t)]
            park |= method=='POST' and any('/api/alpha/park' in t for t in urls)
            dispatch |= method=='POST' and any(re.search(r'/api/alpha/(?:agents/[^ /]+/invoke|invoke(?:$|[?]))',t) for t in urls)
            if method=='GET' and any('/api/alpha/invoke/jobs' in t for t in urls):status=True
            elif method!='POST' or not (park or dispatch):unknown=True
            continue
        if first not in ('jq','head','tail','true'):unknown=True
    return {'kind':'status_check' if status and not unknown and not park and not dispatch else 'coordination' if park or dispatch else 'unknown','park':park,'dispatch':dispatch}


def usage(row):
    u=row.get('message',{}).get('usage')
    return {k:u.get(k,0) for k in COMPONENTS} if isinstance(u,dict) else None


def extract(root,cutoff):
    groups=defaultdict(list)
    for path in sorted(root.glob('*/*.jsonl*')):
        if path.is_file():groups[path.name.split('.jsonl')[0]].append(path)
    turns=[];events=[];manifest={'cutoff':cutoff,'scope':str(root/'*/*.jsonl*'),'sessions':{},'excluded_files':[]}
    for sid,paths in sorted(groups.items()):
        records={};files=[];counts=Counter();conflicts=set()
        for path in paths:
            digest=hashlib.sha256();n=0
            with path.open('rb') as source:
                for line in source:
                    digest.update(line);n+=1
                    try:r=json.loads(line)
                    except (ValueError,UnicodeDecodeError):counts['invalid_json_rows']+=1;continue
                    if r.get('type') not in ('user','assistant') or not r.get('uuid'):continue
                    if not r.get('timestamp') or r['timestamp']>=cutoff:continue
                    if r.get('isSidechain'):counts['sidechain_occurrences']+=1;continue
                    counts['row_occurrences']+=1
                    uid=r['uuid'];old=records.get(uid)
                    if old and (text_of(old)!=text_of(r) or usage(old)!=usage(r)):conflicts.add(uid)
                    score=lambda q:(not text_of(q).startswith('[compacted:'),len(json.dumps(q.get('message',{}))),q.get('timestamp',''))
                    if old is None or score(r)>score(old):records[uid]={**r,'_source':{'path':str(path),'line':n}}
            files.append({'path':str(path),'lines':n,'sha256':digest.hexdigest()})
        if not records:continue
        ordered=sorted(records.values(),key=lambda r:(r['timestamp'],r['uuid']))
        current=None;api={};tool_flags={};agent_names=Counter()
        def new_turn(r,orphan=False):
            text=text_of(r) if not orphan else ''
            match=re.match(r'\s*--- CURRENT TURN ---\s*\n(.*?)\n---\s*\n(.*)',text,re.S)
            header=dict(re.findall(r'^([A-Za-z -]+):\s*(.*)$',match[1],re.M)) if match else {}
            body=match[2].strip() if match else text
            body=re.sub(r'^Agent: [^\n]+\n\s*User message:\s*\n','',body)
            # Retain the resume marker but omit returned dependency transcripts.
            resume=re.search(r'^--- resumed:.*$',body,re.M)
            rule_text=body[:resume.end()] if resume else body
            agent=header.get('To','')
            if re.fullmatch(r'claude-\d+',agent):agent_names[agent]+=1
            return {'turn_id':sid+':'+r['uuid'],'session_id':sid,'started_at':r['timestamp'],
                    'header':header,'resume_job_ids':re.findall(r'^• (invoke-[0-9]+-[0-9]+-[a-f0-9]+)',body,re.M),'trigger_text':rule_text,'trigger_source':r['_source'],'orphan':orphan,
                    'assistant_rows':0,'missing_usage_rows':0,'tool_calls':0,'status_checks':0,'unknown_tools':0,
                    'park_attempts':0,'dispatch_attempts':0,'confirmed_park_ids':[],'park_job_ids':[],
                    'final_text_chars':0,'final_preview':'','actions':[]}
        for r in ordered:
            if r['type']=='user' and not tool_result(r):
                current=new_turn(r);turns.append(current);continue
            if current is None:
                current=new_turn(r,True);turns.append(current)
            content=r.get('message',{}).get('content',[])
            if r['type']=='user':
                for block in content if isinstance(content,list) else []:
                    flag=tool_flags.get(block.get('tool_use_id'))
                    if flag and flag['park'] and not block.get('is_error'):
                        result=str(block.get('content',''))
                        current['confirmed_park_ids']+=re.findall(r'park-[a-f0-9-]{8,}',result)
                        current['park_job_ids']+=re.findall(r'invoke-[0-9]+-[0-9]+-[a-f0-9]+',result)
                continue
            current['assistant_rows']+=1
            u=usage(r)
            if u is not None:
                for component in COMPONENTS:
                    if component not in r['message']['usage']:counts['missing_component_'+component]+=1
            if u is None:current['missing_usage_rows']+=1
            else:
                key=r['message'].get('id') or 'uuid:'+r['uuid']
                event={'session_id':sid,'turn_id':current['turn_id'],'message_id':key,'timestamp':r['timestamp'],
                       'usage':u,'model':r['message'].get('model'),'source':r['_source'],'uuids':[r['uuid']],
                       'row_events':[{'timestamp':r['timestamp'],'usage':u,'uuid':r['uuid'],'turn_id':current['turn_id']}], 'usage_variants':1}
                if key not in api:api[key]=event
                else:
                    old=api[key]
                    if old['usage']!=u:old['usage_variants']+=1
                    old['usage']=u;old['uuids'].append(r['uuid']);old['row_events']+=event['row_events']
                    if old['turn_id']!=current['turn_id']:counts['api_ids_crossing_turn_boundaries']+=1
            for block in content if isinstance(content,list) else []:
                if block.get('type')!='tool_use':continue
                current['tool_calls']+=1;name=block.get('name','');args=block.get('input',{})
                flag=command_actions(args.get('command','')) if name in ('Bash','bash') else {'kind':'unknown','park':False,'dispatch':False}
                tool_flags[block.get('id')]=flag
                current['status_checks']+=flag['kind']=='status_check';current['unknown_tools']+=flag['kind']=='unknown'
                current['park_attempts']+=flag['park'];current['dispatch_attempts']+=flag['dispatch']
                if flag['park'] or flag['dispatch'] or flag['kind']=='status_check':
                    current['actions'].append({'at':r['timestamp'],'tool':name,**flag,'source':r['_source']})
            if r.get('message',{}).get('stop_reason')=='end_turn' and text_of(r).strip():
                current['final_text_chars']+=len(text_of(r));current['final_preview']=text_of(r)[:300]
        events.extend(api.values())
        manifest['sessions'][sid]={'files':files,'unique_rows':len(records),'conflict_uuids':sorted(conflicts),'counts':dict(counts),
                                   'agents':dict(agent_names),'first_at':ordered[0]['timestamp'],'last_at':ordered[-1]['timestamp'],
                                   'api_messages':len(api),'usage_conflicting_api_ids':sum(e['usage_variants']>1 for e in api.values())}
    return turns,events,manifest


def main():
    p=argparse.ArgumentParser(description=__doc__);p.add_argument('--root',type=Path,default=Path('/home/joe/.claude/projects'))
    p.add_argument('--out',type=Path,default=Path(__file__).parent)
    p.add_argument('--cutoff',default=datetime.now(timezone.utc).isoformat().replace('+00:00','Z'))
    a=p.parse_args();turns,events,manifest=extract(a.root,a.cutoff);a.out.mkdir(parents=True,exist_ok=True)
    for name,rows in [('turns.jsonl',turns),('usage-events.jsonl',events)]:
        (a.out/(name+'.gz')).write_bytes(gzip.compress(''.join(json.dumps(r,ensure_ascii=False)+'\n' for r in rows).encode(),mtime=0))
    (a.out/'manifest.json').write_text(json.dumps(manifest,indent=2)+'\n')
    print(json.dumps({'sessions':len(manifest['sessions']),'turns':len(turns),'API_messages':len(events),'cutoff':a.cutoff}))

if __name__=='__main__':main()
