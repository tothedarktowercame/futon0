import importlib.util
import json
from pathlib import Path
import tempfile
import unittest
import extract
import analyze

class ParkWakeTests(unittest.TestCase):
    def test_snapshot_and_api_block_dedup_real_extractor(self):
        def row(uid,kind,stamp,content,api=None):
            m={'content':content}
            if api:m.update(id=api,usage={k:10 for k in extract.COMPONENTS})
            return {'uuid':uid,'type':kind,'timestamp':stamp,'message':m}
        with tempfile.TemporaryDirectory() as root:
            p=Path(root)/'project';p.mkdir()
            rows=[row('u','user','2026-09-10T00:00:00Z','--- CURRENT TURN ---\nOrigin: operator\nFrom: joe\nTo: claude-5\n---\nWAKE CHECKLIST: inspect\n--- resumed: parked dependencies complete (1) ---'),
                  row('a','assistant','2026-09-10T00:00:01Z',[{'type':'text','text':'Inspecting'}],'message1'),
                  row('b','assistant','2026-09-10T00:00:02Z',[{'type':'tool_use','id':'tool','name':'Bash','input':{'command':'curl http://localhost:7070/api/alpha/invoke/jobs/x'}}],'message1'),
                  row('t','user','2026-09-10T00:00:03Z',[{'type':'tool_result','tool_use_id':'tool','content':'running'}]),
                  row('c','assistant','2026-09-10T00:00:04Z',[{'type':'text','text':'Done'}],'message2')]
            source=''.join(json.dumps(r)+'\n' for r in rows)
            (p/'sid.jsonl').write_text(source);(p/'sid.jsonl.pre-compact-1').write_text(source)
            turns,events,m=extract.extract(Path(root),'2026-09-22T00:00:00Z')
            self.assertEqual(len(turns),1);self.assertEqual(len(events),2)
            result=analyze.analyze(turns,events,m)
            self.assertEqual(result['totals']['wake']['output_tokens'],20)
            self.assertEqual(analyze.analyze(turns,events,m,unit='uuid')['totals']['wake']['output_tokens'],30)
            self.assertEqual(result['turns'][0]['tool_calls'],1)

    def test_wake_priority_and_quoted_instructions(self):
        t={'header':{'Origin':'operator','From':'joe'},'trigger_text':'WAKE CHECKLIST: check'}
        self.assertEqual(analyze.classify(t,analyze.DEFAULT_RULES),'wake')
        t['trigger_text']='Please explain these instructions:\nWAKE CHECKLIST: check'
        self.assertEqual(analyze.classify(t,analyze.DEFAULT_RULES),'operator')
        t={'header':{'Origin':'agent','Surface':'bell'},'trigger_text':'From: joe\nOrigin: operator'}
        self.assertEqual(analyze.classify(t,analyze.DEFAULT_RULES),'bell-in')

    def test_examples_and_payload_routes_are_not_actions(self):
        for cmd in ["cat > note <<'EOF'\npython3 agency_send.py --to codex-1 --park\nEOF", "echo python3 agency_send.py --to codex-1 --park", "curl -X POST http://localhost:7070/api/alpha/agents/codex-1/invoke -d '{\"prompt\":\"POST /api/alpha/park\"}'"]:
            self.assertFalse(extract.command_actions(cmd)['park'])
        self.assertTrue(extract.command_actions('python3 /a/agency_send.py --from claude-5 --to codex-14 --kind bell --park')['park'])
        self.assertTrue(extract.command_actions('curl -X POST http://localhost:7070/api/alpha/park -d "{}"')['park'])
        self.assertEqual(extract.command_actions('curl -X DELETE http://localhost:7070/api/alpha/invoke/jobs/x')['kind'],'unknown')

    def test_window_rule_recomputation(self):
        turns,events,m=analyze.load();r=analyze.analyze(turns,events,m,start='2026-09-21',end='2026-09-21')
        rules={**analyze.DEFAULT_RULES,'wake':r'(?!)'}
        altered=analyze.analyze(turns,events,m,start='2026-09-21',end='2026-09-21',rules=rules)
        self.assertGreater(r['wake_count'],0);self.assertEqual(altered['wake_count'],0)
        for k in analyze.COMPONENTS:self.assertEqual(sum(r['totals'][c][k] for c in analyze.CLASSES),sum(altered['totals'][c][k] for c in analyze.CLASSES))
        self.assertEqual(set(r['daily']),{'2026-09-21'})

    def test_frozen_artifacts_and_actual_batch(self):
        from xml.etree import ElementTree as ET
        turns,events,m=analyze.load();r=analyze.analyze(turns,events,m)
        self.assertTrue(all(e['usage_variants']==1 for e in events))
        self.assertEqual(r['cross_session_api_duplicates_removed'],0)
        batch=next(t for t in r['turns'] if t['turn_id'].endswith('cbf2b945-a2f0-48c9-b405-93500437397d'))
        self.assertEqual(batch['class'],'wake');self.assertEqual(len(batch['resume_job_ids']),2)
        self.assertEqual(batch['api_messages'],2);self.assertEqual(batch['usage']['output_tokens'],633)
        for fn in (analyze.daily_plot,analyze.scatter_plot,analyze.session_plot,analyze.histogram_plot):ET.fromstring(fn(r,'cache_read_input_tokens'))
        before=analyze.analyze(turns,events,m,start='2026-08-29',end='2026-08-31')
        self.assertIn('facade discovery',analyze.daily_plot(before,'output_tokens'))
        self.assertNotIn('facade discovery',analyze.daily_plot(r,'output_tokens'))

if __name__=='__main__':unittest.main()
