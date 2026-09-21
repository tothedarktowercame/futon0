#!/usr/bin/env python3
"""Recompute semantic contribution and repeated correction from frozen full text.
Run with futon3a/.venv/bin/python. Stance labels are read, never generated.
"""
import argparse
import csv
import hashlib
import importlib.metadata
import json
import os
from pathlib import Path
import re
from datetime import datetime, timezone

DEFAULT_MODEL = 'sentence-transformers/all-MiniLM-L6-v2'
CATEGORIES = ('continue', 'accept', 'amend', 'redirect', 'reject', 'new')


def read_jsonl(path):
    return [json.loads(line) for line in Path(path).read_text().splitlines() if line.strip()]


def chunks(text, tokenizer, max_tokens):
    """Sentence/newline boundaries, subdivided at tokenizer offsets before truncation."""
    result = []
    for sentence in re.split(r'(?<=[.!?])\s+|\n+', text):
        sentence = sentence.strip()
        if not sentence:
            continue
        offsets = tokenizer(sentence, add_special_tokens=False, return_offsets_mapping=True)['offset_mapping']
        for start in range(0, len(offsets), max_tokens):
            end = min(start + max_tokens, len(offsets))
            result.append(sentence[offsets[start][0]:offsets[end-1][1]])
    return result


def contribution(maxima, threshold):
    return sum(v < threshold for v in maxima) / len(maxima) if maxima else None


def max_cosine(query, reference):
    import numpy as np
    if len(reference) == 0:
        return np.full(len(query), -1.0)
    return np.clip((query @ reference.T).max(axis=1), -1, 1)


def compute(data, model_name=DEFAULT_MODEL, revision=None):
    import numpy as np
    import torch
    from sentence_transformers import SentenceTransformer
    torch.set_num_threads(2)
    model = SentenceTransformer(model_name, revision=revision, trust_remote_code=False, device='cpu')
    tokenizer = model.tokenizer
    # Allow the encoder's special tokens while retaining all long-turn text.
    limit = int(model.max_seq_length) - tokenizer.num_special_tokens_to_add(pair=False)
    if limit < 1:
        raise ValueError('Encoder has no usable token capacity')
    pairs = read_jsonl(data / 'sample_pairs.jsonl')
    labels = {r['pair_id']: r for r in csv.DictReader((data / 'stance_labels.csv').open())}
    assert set(labels) == {p['pair_id'] for p in pairs}
    assert all(l['label'] in CATEGORIES for l in labels.values())
    latest = {s: max(p['operator_at'] for p in pairs if p['session']==s) for s in {p['session'] for p in pairs}}
    context = [r for r in read_jsonl(data/'dialogue_context.jsonl') if r['timestamp'] <= latest[r['session']]]
    units = {}; corpus = []; corpus_index = {}
    def register(uid, text):
        ids=[]
        for unit in chunks(text,tokenizer,limit):
            if unit not in corpus_index:
                corpus_index[unit]=len(corpus);corpus.append(unit)
            ids.append(corpus_index[unit])
        units[uid]=ids
    for r in context: register(r['uuid'],r['text'])
    for p in pairs:
        register(p['operator_uuid'],p['operator_text']); register(p['agent_uuid'],p['agent_text'])
    vectors = model.encode(corpus, normalize_embeddings=True, batch_size=64, show_progress_bar=False)
    by_session={s:[r for r in context if r['session']==s] for s in latest}
    earlier={s:[] for s in latest};output=[]
    for p in pairs:
        q=vectors[units[p['operator_uuid']]]
        if not len(q):raise ValueError('Empty operator tokenization: '+p['pair_id'])
        agent=vectors[units[p['agent_uuid']]]
        past_ids=sorted({i for r in by_session[p['session']] if r['timestamp'] < p['operator_at'] for i in units[r['uuid']]})
        history=vectors[past_ids] if past_ids else vectors[:0]
        agent_cos=max_cosine(q,agent).tolist();history_cos=max_cosine(q,history).tolist()
        centre=q.mean(axis=0);norm=np.linalg.norm(centre);centre=centre/norm if norm else centre
        label=labels[p['pair_id']]['label'];repeat=None;prior_id=None
        if label in ('redirect','reject'):
            candidates=earlier[p['session']]
            if candidates:
                similarities=[float(np.clip(centre @ old,-1,1)) for _,old in candidates]
                best=int(np.argmax(similarities));repeat=similarities[best];prior_id=candidates[best][0]
            candidates.append((p['pair_id'],centre))
        output.append({'pair_id':p['pair_id'],'session':p['session'],'operator_at':p['operator_at'],
                       'label':label,'unit_count':len(q),'operator_units':[corpus[i] for i in units[p['operator_uuid']]],
                       'agent_max_cosine':agent_cos,'history_max_cosine':history_cos,
                       'contribution_075':contribution(history_cos,.75),
                       'repeated_correction_cosine':repeat,'earlier_correction_id':prior_id})
    config=model[0].auto_model.config
    return {'provenance':{'model':model_name,'requested_revision':revision,'resolved_revision':getattr(config,'_commit_hash',None),
                          'encoder_max_sequence_length':model.max_seq_length,'chunk_token_limit':limit,
                          'dimension':int(vectors.shape[1]),'unique_chunks':len(corpus),
                          'context_rows':len(context),'created_at':datetime.now(timezone.utc).isoformat(),
                          'packages':{n:importlib.metadata.version(n) for n in ['sentence-transformers','transformers','torch','numpy']},
                          'inputs':{name:hashlib.sha256((data/name).read_bytes()).hexdigest() for name in ['sample_pairs.jsonl','stance_labels.csv','dialogue_context.jsonl']},
                          'script_sha256':hashlib.sha256(Path(__file__).read_bytes()).hexdigest()},'rows':output}


def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--data',type=Path,default=Path(__file__).parent)
    p.add_argument('--model',default=DEFAULT_MODEL);p.add_argument('--revision',default=None)
    p.add_argument('--output',type=Path,required=True)
    a=p.parse_args();result=compute(a.data,a.model,a.revision)
    a.output.parent.mkdir(parents=True,exist_ok=True)
    temp=a.output.with_suffix(a.output.suffix+'.tmp-'+str(os.getpid()))
    temp.write_text(json.dumps(result,ensure_ascii=False,indent=2)+'\n');temp.replace(a.output)
    print(json.dumps(result['provenance']))

if __name__=='__main__':main()
