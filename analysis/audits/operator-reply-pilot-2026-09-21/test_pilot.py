"""Data invariants and the real MiniLM tokenizer/embedding path; no transcript writes."""
import csv
import hashlib
import json
from pathlib import Path
import unittest
import extract_pairs
import measure

DATA=Path(__file__).parent

class PilotTests(unittest.TestCase):
    def test_operator_envelope_does_not_make_resume_a_human_turn(self):
        row={'message':{'content':'--- CURRENT TURN ---\nSurface: emacs-repl\nOrigin: operator\nFrom: joe\n---\nresumed: parked dependencies ready'}}
        self.assertEqual(extract_pairs.incoming(row)[0],'park_resume_marker')
        row['message']['content']=row['message']['content'].replace('resumed: parked dependencies ready','WAKE CHECKLIST: inspect jobs')
        self.assertEqual(extract_pairs.incoming(row)[0],'wake_checklist')

    def test_quoted_operator_in_bell_is_not_operator(self):
        row={'message':{'content':'--- CURRENT TURN ---\nSurface: bell\nOrigin: agent\nFrom: claude-4\n---\nJoe said: yes'}}
        self.assertEqual(extract_pairs.incoming(row)[0],'agent_bell')

    def test_sampling_is_prefix_and_complete_labels(self):
        pairs=measure.read_jsonl(DATA/'all_pairs.jsonl');sample=measure.read_jsonl(DATA/'sample_pairs.jsonl')
        self.assertEqual(extract_pairs.select_sample(pairs),sample)
        labels=list(csv.DictReader((DATA/'stance_labels.csv').open()))
        self.assertEqual(len(sample),100)
        self.assertEqual({p['pair_id'] for p in sample},{p['pair_id'] for p in labels})
        for session in extract_pairs.SESSIONS:
            ids=[p['session_order'] for p in sample if p['session']==session]
            self.assertEqual(ids,list(range(1,len(ids)+1)))
        for p in sample:self.assertLess(p['agent_at'],p['operator_at'])

    def test_metrics_provenance_and_no_future_correction(self):
        result=json.loads((DATA/'metrics-minilm.json').read_text());seen={s:set() for s in extract_pairs.SESSIONS}
        for name,digest in result['provenance']['inputs'].items():self.assertEqual(hashlib.sha256((DATA/name).read_bytes()).hexdigest(),digest)
        for r in result['rows']:
            self.assertEqual(len(r['history_max_cosine']),r['unit_count'])
            self.assertTrue(all(h+1e-5>=a for h,a in zip(r['history_max_cosine'],r['agent_max_cosine'])))
            if r['earlier_correction_id']:self.assertIn(r['earlier_correction_id'],seen[r['session']])
            if r['label'] in ('redirect','reject'):
                self.assertEqual(r['repeated_correction_cosine'] is None,not seen[r['session']]);seen[r['session']].add(r['pair_id'])
            else:self.assertIsNone(r['repeated_correction_cosine'])
            self.assertLessEqual(measure.contribution(r['history_max_cosine'],.55),measure.contribution(r['history_max_cosine'],.85))

    def test_real_encoder_long_input_and_exact_repeat(self):
        import numpy as np
        import torch
        from sentence_transformers import SentenceTransformer
        torch.set_num_threads(2)
        model=SentenceTransformer(measure.DEFAULT_MODEL,local_files_only=True,device='cpu')
        limit=model.max_seq_length-model.tokenizer.num_special_tokens_to_add(pair=False)
        source=' '.join(['coordination']*600)+' unique ending'
        pieces=measure.chunks(source,model.tokenizer,limit)
        self.assertGreater(len(pieces),1);self.assertTrue(pieces[-1].endswith('unique ending'))
        self.assertTrue(all(len(model.tokenizer(p)['input_ids'])<=model.max_seq_length for p in pieces))
        vectors=model.encode(['The operator approves the proposed work.']*2,normalize_embeddings=True)
        maximum=measure.max_cosine(vectors[:1],vectors[1:]).tolist()
        self.assertAlmostEqual(maximum[0],1.0,places=5)
        self.assertEqual(measure.contribution(maximum,.75),0)
        self.assertEqual(measure.contribution(measure.max_cosine(vectors[:1],vectors[:0]).tolist(),.75),1)

if __name__=='__main__':unittest.main()
