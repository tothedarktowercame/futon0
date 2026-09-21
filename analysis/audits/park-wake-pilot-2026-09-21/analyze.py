#!/usr/bin/env python3
"""Reclassify frozen Claude usage with editable ordered trigger rules; stdlib only."""
import argparse
from collections import Counter,defaultdict
from datetime import date,datetime,timedelta
from html import escape
import csv,gzip,json,math,re,statistics
from pathlib import Path

DATA=Path(__file__).parent
COMPONENTS=('input_tokens','cache_read_input_tokens','cache_creation_input_tokens','output_tokens')
CLASSES=('wake','bell-in','operator','other')
COLORS=dict(zip(CLASSES,('#ab5b20','#7650a0','#267d6d','#8b939c')))
DEFAULT_RULES={
 'wake':r'(?im)^--- resumed:\s*parked dependenc|\A\s*(?:WAKE(?: CHECKLIST)?\b|ON WAKE\b|STOOD DOWN FOR THE NIGHT\b|park (?:deadline|timeout) (?:reached|expired)|deadline (?:wake|expired))',
 'bell-in':r'(?im)^Origin: agent\s*$',
 'operator':r'(?im)^Origin: operator\s*$|^From: joe\s*$'}


def load(data=DATA):
    read=lambda n:[json.loads(s) for s in gzip.decompress((data/(n+'.gz')).read_bytes()).decode().splitlines()]
    return read('turns.jsonl'),read('usage-events.jsonl'),json.loads((data/'manifest.json').read_text())


def classify(turn,rules):
    # Only the envelope is searched for origin/identity, never a quoted body envelope.
    for c in CLASSES[:-1]:
        text=turn['trigger_text'] if c=='wake' else '\n'.join(f'{k}: {v}' for k,v in turn['header'].items())
        if re.search(rules[c],text):return c
    return 'other'


def quantiles(values):
    if not values:return {'n':0,'min':None,'p50':None,'p90':None,'p95':None,'max':None}
    ordered=sorted(values)
    def q(p):
        k=(len(ordered)-1)*p;i=int(k);return ordered[i]+(ordered[min(i+1,len(ordered)-1)]-ordered[i])*(k-i)
    return {'n':len(values),'min':ordered[0],'p50':q(.5),'p90':q(.9),'p95':q(.95),'max':ordered[-1]}


def ranks(values):
    ordered=sorted(enumerate(values),key=lambda x:x[1]);result=[0.]*len(values);i=0
    while i<len(ordered):
        j=i+1
        while j<len(ordered) and ordered[j][1]==ordered[i][1]:j+=1
        for k in range(i,j):result[ordered[k][0]]=(i+j-1)/2
        i=j
    return result


def spearman(x,y):
    if len(x)<3 or len(set(x))<2 or len(set(y))<2:return None
    return statistics.correlation(ranks(x),ranks(y))


def analyze(turns,events,manifest,start='2026-09-07',end='2026-09-21',rules=None,unit='api'):
    rules=rules or DEFAULT_RULES
    start_day=date.fromisoformat(start);end_day=date.fromisoformat(end)
    if end_day<start_day:raise ValueError('End must not precede start')
    for c in CLASSES[:-1]:re.compile(rules[c])
    if unit=='uuid':
        selected=[{**e, **row, 'message_id':row['uuid'], 'row_events':[row]} for e in events for row in e['row_events'] if start<=row['timestamp'][:10]<=end]
    else:
        selected=[e for e in events if start<=e['timestamp'][:10]<=end]
    by_id={t['turn_id']:t for t in turns};total={c:Counter() for c in CLASSES};daily=defaultdict(lambda:{c:Counter() for c in CLASSES})
    sessions={};turn_usage={};global_ids=set();cross_session_duplicates=0
    for event in selected:
        if unit=='api':
            if event['message_id'] in global_ids:cross_session_duplicates+=1;continue
            global_ids.add(event['message_id'])
        turn=by_id[event['turn_id']];kind=classify(turn,rules);sid=event['session_id']
        if sid not in sessions:sessions[sid]={'session_id':sid,'agents':manifest['sessions'][sid]['agents'],'totals':{c:Counter() for c in CLASSES},'api_messages':0,'wake_api_messages':0}
        s=sessions[sid];s['api_messages']+=1;s['wake_api_messages']+=kind=='wake'
        if turn['turn_id'] not in turn_usage:
            turn_usage[turn['turn_id']]={**turn,'class':kind,'usage':Counter(),'api_messages':0,'first_cache_read':event['usage']['cache_read_input_tokens'],
                                         'first_context_tokens':sum(event['usage'][k] for k in COMPONENTS[:3]),'first_usage_at':event['timestamp']}
        t=turn_usage[turn['turn_id']];t['api_messages']+=1
        rows=[{'timestamp':event['timestamp'],'usage':event['usage']}] if unit=='api' else event['row_events']
        for row in rows:
            if not start<=row['timestamp'][:10]<=end:continue
            for k in COMPONENTS:
                value=row['usage'][k]
                total[kind][k]+=value;daily[row['timestamp'][:10]][kind][k]+=value;s['totals'][kind][k]+=value;t['usage'][k]+=value
    wake=[t for t in turn_usage.values() if t['class']=='wake']
    activity=Counter()
    for t in wake:
        t['dispatched_work']=t['dispatch_attempts']>0
        t['reported_final_text']=t['final_text_chars']>0
        t['only_status_checks']=t['tool_calls']>0 and t['status_checks']==t['tool_calls'] and not t['dispatch_attempts'] and not t['park_attempts'] and t['final_text_chars']<=600
        for k in ('dispatched_work','reported_final_text','only_status_checks'):activity[k]+=t[k]
        bucket='dispatch' if t['dispatched_work'] else 'status-only' if t['only_status_checks'] else 'final-without-dispatch' if t['reported_final_text'] else 'other-or-no-final'
        activity['exclusive_'+bucket]+=1
    x=[t['first_cache_read'] for t in wake]
    context={'n':len(wake),'spearman_cache_read_total':spearman(x,[t['usage']['cache_read_input_tokens'] for t in wake]),
             'spearman_output_total':spearman(x,[t['usage']['output_tokens'] for t in wake]),
             'single_api_wakes':sum(t['api_messages']==1 for t in wake),
             'spearman_cache_vs_api_count':spearman(x,[t['api_messages'] for t in wake])}
    prefixes=Counter(re.sub(r'\s+',' ',t['trigger_text']).strip()[:100] or '<no trigger: orphan assistant>' for t in turn_usage.values() if t['class']=='other')
    other_envelopes=Counter((t['header'].get('Origin','<absent>'),t['header'].get('Surface','<absent>')) for t in turn_usage.values() if t['class']=='other')
    dispatch_turns=[t for t in turn_usage.values() if t['class'] in ('operator','bell-in') and t['park_attempts']]
    dispatch_totals={c:sum(t['usage'][c] for t in dispatch_turns) for c in COMPONENTS}
    return {'start':start,'end':end,'unit':unit,'rules':rules,'cutoff':manifest['cutoff'],'totals':total,'daily':dict(daily),
            'sessions':list(sessions.values()),'turns':list(turn_usage.values()),'wake_count':len(wake),'activity':activity,'context':context,
            'wake_distribution':{k:quantiles([t['usage'][k] for t in wake]) for k in COMPONENTS},
            'other_envelopes':[{'origin':key[0],'surface':key[1],'turns':count} for key,count in other_envelopes.most_common()],
            'other_prefixes':prefixes.most_common(20),'park_dispatch_turns':len(dispatch_turns),'park_dispatch_totals':dispatch_totals,
            'cross_session_api_duplicates_removed':cross_session_duplicates}


def share(result,component,kind):
    n=sum(result['totals'][c][component] for c in CLASSES)
    return result['totals'][kind][component]/n if n else 0


def session_ranking(result,component,min_calls=0):
    rows=[]
    for s in result['sessions']:
        total=sum(s['totals'][c].get(component,0) for c in CLASSES)
        if total and s['api_messages']>=min_calls:
            rows.append({'session':s['session_id'],'agents':', '.join(s['agents']) or 'unknown','wake_share':s['totals']['wake'].get(component,0)/total,
                         'wake_tokens':s['totals']['wake'].get(component,0),'total_tokens':total,'api_messages':s['api_messages'],'wake_api_messages':s['wake_api_messages']})
    return sorted(rows,key=lambda r:(-r['wake_share'],-r['total_tokens']))


def table(headers,rows):
    return '<div style="overflow:auto"><table style="font:14px system-ui;border-collapse:collapse"><tr>'+''.join('<th style="padding:8px;text-align:left;border-bottom:2px solid #aaa">'+escape(str(x))+'</th>' for x in headers)+'</tr>'+''.join('<tr>'+''.join('<td style="padding:7px;border-bottom:1px solid #ddd">'+escape(str(x))+'</td>' for x in row)+'</tr>' for row in rows)+'</table></div>'


def svg_start(title,height):return [f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 1100 {height}" style="width:100%;height:auto" role="img"><title>{escape(title)}</title><rect width="1100" height="{height}" fill="white"/><g font-family="system-ui,sans-serif" fill="#23313f"><text x="30" y="32" font-size="22">{escape(title)}</text>']
def svg_end(parts):return ''.join(parts)+'</g></svg>'


def daily_plot(result,component):
    p=svg_start('Daily share by trigger · '+component,440)
    for i,c in enumerate(CLASSES):p.append(f'<circle cx="{35+i*225}" cy="63" r="5" fill="{COLORS[c]}"/><text x="{47+i*225}" y="68" font-size="15">{c}</text>')
    start=date.fromisoformat(result['start']);n=(date.fromisoformat(result['end'])-start).days+1;w=945/n
    for i in range(n):
        d=(start+timedelta(days=i)).isoformat();values=result['daily'].get(d,{c:Counter() for c in CLASSES});total=sum(values[c][component] for c in CLASSES);y=340;x=100+i*w
        for c in CLASSES:
            h=230*values[c][component]/total if total else 0;y-=h
            p.append(f'<rect x="{x+1}" y="{y}" width="{max(.5,w-2)}" height="{h}" fill="{COLORS[c]}"><title>{d} {c}: {values[c][component]:,} / {total:,}</title></rect>')
        if not total:p.append(f'<text x="{x+w/2}" y="330" font-size="10" text-anchor="middle" transform="rotate(-90 {x+w/2} 330)">no records</text>')
        if i%max(1,n//12)==0:p.append(f'<text x="{x+w/2}" y="365" font-size="12" text-anchor="middle">{d[5:]}</text>')
        if d=='2026-08-30':p.append(f'<line x1="{x+w/2}" x2="{x+w/2}" y1="100" y2="340" stroke="black" stroke-dasharray="5 3"/><text x="{x+w/2+4}" y="97" font-size="12">Aug 30 · facade discovery</text>')
    for v in (0,.25,.5,.75,1):p.append(f'<text x="85" y="{345-230*v}" font-size="13" text-anchor="end">{v:.0%}</text>')
    p.append('<text x="570" y="408" text-anchor="middle" font-size="14">API message date (UTC, month-day) · shares within each recorded day</text>');return svg_end(p)


def scatter_plot(result,component):
    wake=[t for t in result['turns'] if t['class']=='wake'];p=svg_start('Wake cost versus starting cached context · '+component,485)
    xmax=max([math.log10(1+t['first_cache_read']) for t in wake]+[1]);ymax=max([math.log10(1+t['usage'][component]) for t in wake]+[1])
    x=lambda v:110+math.log10(1+v)/xmax*925;y=lambda v:380-math.log10(1+v)/ymax*295
    for exp in range(int(xmax)+1):
        v=10**exp-1;p.append(f'<line x1="{x(v)}" x2="{x(v)}" y1="85" y2="380" stroke="#eee"/><text x="{x(v)}" y="405" text-anchor="middle" font-size="12">{v:,}</text>')
    for exp in range(int(ymax)+1):
        v=10**exp-1;p.append(f'<line x1="110" x2="1035" y1="{y(v)}" y2="{y(v)}" stroke="#eee"/><text x="98" y="{y(v)+5}" text-anchor="end" font-size="12">{v:,}</text>')
    for t in wake:
        p.append(f'<circle cx="{x(t["first_cache_read"])}" cy="{y(t["usage"][component])}" r="3" opacity=".35" fill="#ab5b20"><title>{escape(t["turn_id"])} · API calls {t["api_messages"]} · cache {t["first_cache_read"]:,} · cost {t["usage"][component]:,}</title></circle>')
    p.append(f'<text x="570" y="440" text-anchor="middle" font-size="14">First API message cache-read tokens (log₁₀(1+x) scale)</text><text transform="translate(23 235) rotate(-90)" text-anchor="middle" font-size="14">Total wake {component} (log₁₀(1+y))</text><text x="70" y="472" font-size="13">Each dot is one wake. Total cache-read includes its first read; this correlation is partly mechanical.</text>');return svg_end(p)


def session_plot(result,component,min_calls=0):
    rows=session_ranking(result,component,min_calls)[:10];p=svg_start('Top 10 sessions by wake share · '+component,470)
    for j,r in enumerate(rows):
        y=72+j*33;label=r['agents']+' / '+r['session'][:8]
        p.append(f'<text x="285" y="{y+17}" text-anchor="end" font-size="13">{escape(label)}</text><rect x="300" y="{y}" width="{650*r["wake_share"]}" height="23" fill="#ab5b20"/><text x="{310+650*r["wake_share"]}" y="{y+17}" font-size="13">{r["wake_share"]:.1%}</text>')
    p.append('<text x="620" y="435" text-anchor="middle" font-size="14">Wake tokens / all tokens in session (selected component)</text><text x="50" y="461" font-size="13">Small sessions can rank 100%; inspect call and token denominators in the table.</text>');return svg_end(p)


def main():
    p=argparse.ArgumentParser(description=__doc__);p.add_argument('--start',default='2026-09-07');p.add_argument('--end',default='2026-09-21');p.add_argument('--out',type=Path,default=DATA)
    a=p.parse_args();data=load();result=analyze(*data,start=a.start,end=a.end);literal=analyze(*data,start=a.start,end=a.end,unit='uuid')
    a.out.mkdir(parents=True,exist_ok=True)
    small={k:v for k,v in result.items() if k not in ('turns','daily')};small['literal_uuid_totals']=literal['totals']
    (a.out/'summary.json').write_text(json.dumps(small,indent=2)+'\n')
    (a.out/'rules.json').write_text(json.dumps(DEFAULT_RULES,indent=2)+'\n')
    with (a.out/'turn-costs.csv').open('w') as f:
        fields=['turn_id','session_id','started_at','class','api_messages',*COMPONENTS,'first_cache_read','park_attempts','dispatch_attempts','status_checks','tool_calls','final_text_chars']
        w=csv.DictWriter(f,fieldnames=fields,lineterminator='\n');w.writeheader()
        for t in result['turns']:w.writerow({k:t['usage'].get(k,0) if k in COMPONENTS else t[k] for k in fields})
    with (a.out/'session-costs.csv').open('w') as f:
        fields=['session_id','agents','trigger','api_messages',*COMPONENTS,*[k+'_share' for k in COMPONENTS]]
        writer=csv.DictWriter(f,fieldnames=fields,lineterminator='\n');writer.writeheader()
        for session in result['sessions']:
            for trigger in CLASSES:
                row={'session_id':session['session_id'],'agents':', '.join(session['agents']),'trigger':trigger,'api_messages':session['api_messages']}
                for component in COMPONENTS:
                    total=sum(session['totals'][c][component] for c in CLASSES)
                    row[component]=session['totals'][trigger][component]
                    row[component+'_share']=row[component]/total if total else ''
                writer.writerow(row)
    for component in COMPONENTS:
        for name,fn in [('daily',daily_plot),('scatter',scatter_plot),('sessions',session_plot),('histogram',histogram_plot)]:(a.out/f'{name}-{component}.svg').write_text(fn(result,component)+'\n')
    print('usage shares',json.dumps({k:{c:round(share(result,k,c)*100,3) for c in CLASSES} for k in COMPONENTS}))
    print('wake count',result['wake_count'],'context',result['context'],'activities',result['activity'])



def histogram_plot(result,component):
    values=[t['usage'][component] for t in result['turns'] if t['class']=='wake'];p=svg_start('Per-wake token distribution · '+component,380)
    maximum=max([math.log10(1+v) for v in values]+[1]);counts=[0]*20
    for v in values:counts[min(19,int(math.log10(1+v)/maximum*20))]+=1
    peak=max(counts+[1])
    for i,count in enumerate(counts):
        x=95+i*47;h=230*count/peak
        lo=10**(maximum*i/20)-1;hi=10**(maximum*(i+1)/20)-1
        p.append(f'<rect x="{x}" y="{300-h}" width="44" height="{h}" fill="#ab5b20"><title>{lo:.0f}–{hi:.0f} tokens: {count} wakes</title></rect>')
    for i in range(0,21,4):p.append(f'<text x="{95+i*47}" y="324" text-anchor="middle" font-size="12">{10**(maximum*i/20)-1:,.0f}</text>')
    for f in (0,.5,1):p.append(f'<text x="82" y="{305-230*f}" text-anchor="end" font-size="13">{peak*f:.0f}</text>')
    p.append('<text x="560" y="365" text-anchor="middle" font-size="14">Tokens per wake (equal-width bins in log₁₀(1+tokens))</text><text transform="translate(25 180) rotate(-90)" text-anchor="middle" font-size="14">Number of wakes</text>');return svg_end(p)


if __name__=='__main__':main()
