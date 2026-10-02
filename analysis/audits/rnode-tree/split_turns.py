"""Split the 300 blinded turns into a development half (shown to the elaborating agents) and a
held-out test half (never shown). Text after a >>> marker is quoted agent text and is removed
(the one >>> in the sample runs to the end of its turn). Seeded; writes dev-turns.json, test-ids.json."""
import json, random, re
from pathlib import Path

HERE = Path(__file__).resolve().parent
TURNS = HERE.parent / 'rnode-turn-bids' / 'turns.json'


def strip_quotes(text):
    return re.split(r'^\s*>>>', text, maxsplit=1, flags=re.M)[0].rstrip()


turns = json.loads(TURNS.read_text())
ids = sorted(t['id'] for t in turns)
random.Random(20261002).shuffle(ids)
dev = set(ids[:150])
out = [{'id': t['id'], 'agent-said-before (tail)': t['agent-said-before (tail)'],
        'operator-turn': strip_quotes(t['operator-turn'])} for t in turns if t['id'] in dev]
(HERE / 'dev-turns.json').write_text(json.dumps(out, indent=1, ensure_ascii=False))
(HERE / 'test-ids.json').write_text(json.dumps(sorted(set(ids) - dev)))
print(len(out), 'dev turns;', 300 - len(out), 'held out')
