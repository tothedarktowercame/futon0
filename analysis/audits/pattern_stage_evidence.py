# /// script
# requires-python = ">=3.10"
# dependencies = ["edn-format==0.7.5"]
# ///
"""Replay the conservative rank-one/operator evidence join.

Raw snapshots and full queries stay in the chosen local snapshot directory.
Uses read-only evidence GETs. No deep health probe. The publication contains
only IDs, source hashes, join decisions and aggregate counts.
"""
import argparse
import urllib.request
import urllib.parse
import sys,json,re,collections
from edn_format import loads,Keyword,ImmutableDict,ImmutableList
from pathlib import Path
from datetime import datetime
def plain(x):
 if isinstance(x,(dict,ImmutableDict)):return {str(k).lstrip(':'):plain(v) for k,v in x.items()}
 if isinstance(x,(list,tuple,ImmutableList)):return [plain(v) for v in x]
 if isinstance(x,Keyword):return str(x).lstrip(':')
 return x
def body(r):
 x=r.get('evidence/body',{});return plain(loads(x)) if isinstance(x,str) else x

def normalize(t):return ' '.join(t.split())
def textbody(t):
 m=re.match(r'\s*--- CURRENT TURN ---\s*\n(.*?)\n---\s*\n(.*)',t,re.S)
 return m[2].strip() if m else t.strip()
def exclusion(t):
 raw=t
 if re.match(r'\s*--- CURRENT TURN ---',raw) and re.search(r'^Origin: agent\s*$',raw.split('\n---\n',1)[0],re.M):return 'agent-envelope'
 t=textbody(t)
 if re.match(r'(?s)^(?:F\d+|JIT|APM)[^\n]{0,120}CONTINUATION:',t):return 'continuation-payload'
 if re.search(r'resumed:\s*parked dependencies complete',t,re.I):return 'park-resume-marker'
 if re.match(r'\s*(?:WAKE(?: CHECKLIST)?\s*:|WAKE CHECKLIST\b|ON WAKE\b|STOOD DOWN FOR THE NIGHT\b)',t,re.I):return 'wake-payload'
 return None

def fetch_snapshot(out, window, endpoint):
 for name,filters in [('retrieval',{'tags':'context-retrieval'}),('joe',{'author':'joe'})]:
  rows=[];cursor={};seen=set();pages=[]
  while True:
   url=endpoint+'/api/alpha/evidence?'+urllib.parse.urlencode({**window,**filters,'limit':1000,**cursor})
   with urllib.request.urlopen(urllib.request.Request(url,headers={'Accept':'application/json'}),timeout=60) as r:d=json.load(r)
   rows.extend(d['entries']); pages.append({k:v for k,v in d.items() if k!='entries'})
   print(name,len(pages),len(rows),'incomplete',d.get('incomplete'),flush=True)
   c=d.get('next-cursor')
   if not c:
    if d.get('incomplete'):raise RuntimeError('Incomplete response without cursor')
    break
   key=(c['at'],c['id']);assert key not in seen;seen.add(key)
   cursor={'cursor-at':c['at'],'cursor-id':c['id']}
  (out/(name+'.json')).write_text(json.dumps(rows))
  (out/(name+'-pages.json')).write_text(json.dumps(pages))
 (out/'window.json').write_text(json.dumps(window))

def replay(P):
 retrieval=json.loads((P/'retrieval.json').read_text());joe=json.loads((P/'joe.json').read_text())

 for source in (retrieval,joe):
  identities={}
  for record in source:
   key=record['evidence/id']
   if key in identities and record!=identities[key]:
    raise ValueError('Conflicting evidence records for '+key)
   identities[key]=record

 turns=[];by_session=collections.defaultdict(list);stats=collections.Counter();seen=set()
 for r in joe:
  if r['evidence/id'] in seen:stats['duplicate-joe-evidence']+=1;continue
  seen.add(r['evidence/id']);b=body(r)
  if not ((b.get('event')=='chat-turn' and b.get('role')=='user') or (b.get('transport')=='marimo' and b.get('direction')=='inbound')):stats['joe-not-user-turn']+=1;continue
  t={'id':r['evidence/id'],'session':r.get('evidence/session-id'),'at':r['evidence/at'],'text':b.get('text',''),'turn-id':b.get('turn-id')}
  t['norm']=normalize(textbody(t['text']));t['excluded']=exclusion(t['text']);t['time']=datetime.fromisoformat(t['at'].replace('Z','+00:00')).timestamp()
  stats['user-turns']+=1
  if '--- resumed: parked dependencies complete' in t['text']:stats['exact-resume-marker-turns']+=1
  if t['excluded']:stats[t['excluded']+'-turns']+=1
  else:stats['eligible-user-turns']+=1
  turns.append(t);by_session[t['session']].append(t)
 for ts in by_session.values():ts.sort(key=lambda t:t['time'])
 results=[];seen=set()
 for r in sorted(retrieval,key=lambda r:(r['evidence/at'],r['evidence/id'])):
  if r['evidence/id'] in seen:stats['duplicate-retrieval-evidence']+=1;continue
  seen.add(r['evidence/id']);b=body(r);q=b.get('query','');qn=normalize(q);time=datetime.fromisoformat(r['evidence/at'].replace('Z','+00:00')).timestamp()
  out={'retrieval-id':r['evidence/id'],'session':r.get('evidence/session-id'),'at':r['evidence/at'],'query':q,'rank1':[v for v in b.get('results',[]) if v.get('rank')==1]}
  ts=[t for t in by_session[out['session']] if 0<=time-t['time']<=21600]
  # Direct query prefix matching, also allowing short prompts followed by response preview.
  matches=[t for t in ts if qn and t['norm'] and (t['norm'].startswith(qn) or qn.startswith(t['norm']+' ') or qn==t['norm'])]
  method='session-text-prefix'
  if not matches and re.match(r'\s*--- CURRENT TURN ---',q) and re.search(r'\nFrom: joe\n',q) and re.search(r'\nOrigin: operator\n',q):
   # Require a nonempty payload suffix and exactly one matching user turn.
   # An envelope alone or multiple matching short prefixes is insufficient.
   tail=normalize(q.split('\n---\n',1)[1]) if '\n---\n' in q else ''
   matches=[t for t in ts if tail and t['norm'].startswith(tail)];method='operator-envelope-unique-payload-prefix'
  if not matches:
   out['status']='unmatched'
  elif len(matches)>1:
   out['candidate-turns']=[t['id'] for t in matches]
   if all(t['excluded'] for t in matches):out['status']='excluded-all-prefix-candidates-automatic'
   else:out['status']='ambiguous-text-prefix'
  else:
   t=matches[0];out.update({'turn-id':t['id'],'turn-at':t['at'],'delay-seconds':time-t['time'],'join-method':method})
   out['status']=t['excluded'] or ('matched' if len(out['rank1'])==1 else 'invalid-rank1')
   if out['status']=='matched':out['pattern-id']=out['rank1'][0]['id']
  stats[out['status']]+=1;results.append(out)
 (P/'joins.json').write_text(json.dumps(results));(P/'turns.json').write_text(json.dumps(turns));(P/'join-stats.json').write_text(json.dumps(stats))
 hits=collections.Counter(r['pattern-id'] for r in results if r['status']=='matched')
 (P/'hits.json').write_text(json.dumps(hits))
 print(json.dumps(stats,indent=2));print('patterns',len(hits),'hits',sum(hits.values()),'top50',sum(n for _,n in hits.most_common(50)))
 print('methods',collections.Counter(r.get('join-method') for r in results if r['status']=='matched'))
 print('top',hits.most_common(15))


def main():
 parser=argparse.ArgumentParser(description=__doc__)
 parser.add_argument("snapshot",type=Path)
 parser.add_argument("--fetch",action="store_true")
 parser.add_argument("--endpoint",default="http://localhost:7073")
 parser.add_argument("--since",default="2026-08-22")
 parser.add_argument("--before",default="2026-09-21T17:19:12.718167Z")
 args=parser.parse_args()
 args.snapshot.mkdir(parents=True,exist_ok=True)
 if args.fetch:
  fetch_snapshot(args.snapshot, {"since":args.since,"before":args.before},args.endpoint.rstrip("/"))
 replay(args.snapshot)

if __name__ == "__main__":
 main()
