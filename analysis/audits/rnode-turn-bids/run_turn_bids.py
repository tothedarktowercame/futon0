"""One `codex exec` per R-node over the blinded turns; writes <NODE>-turns.edn, then runs
check_turn_bids.py on it. The node's priming notes come from ../rnode-bids/<NODE>-links.edn.
Usage: python3 run_turn_bids.py [NODE ...] [--jobs 4]"""
import json, subprocess, sys
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
import edn_format
from edn_format import Keyword as K

HERE = Path(__file__).resolve().parent
PRIOR = HERE.parent / 'rnode-bids'
LOGS = HERE.parents[2] / 'data' / 'audits' / 'rnode-turn-bids' / 'logs'


def notes(node):
    d = edn_format.loads((PRIOR / f'{node["node"]}-links.edn').read_text())
    claims = [f'- {p[K("claim")]} ({p.get(K("symbol"))} at {p.get(K("file"))}:{p.get(K("line"))})'
              for p in d.get(K('priming')) or []]
    return json.dumps(node, indent=1, ensure_ascii=False) + '\n\nNotes from the code reading:\n' + '\n'.join(claims)


def run(node):
    out, log = HERE / f'{node["node"]}-turns.edn', LOGS / f'{node["node"]}.jsonl'
    if out.exists() and out.stat().st_size:
        return node['node'], 'exists'
    prompt = (HERE / 'prompt.md').read_text().replace('{NODE}', node['node']) + notes(node)
    with open(log, 'w') as lf:
        r = subprocess.run(['codex', 'exec', '--ephemeral', '--skip-git-repo-check', '-s', 'read-only',
                            '-C', '/home/joe/code', '--json', '-o', str(out), '-'],
                           input=prompt, text=True, stdout=lf, stderr=subprocess.STDOUT)
    chk = subprocess.run([sys.executable, str(HERE / 'check_turn_bids.py'), str(out)], capture_output=True, text=True)
    return node['node'], f'exit {r.returncode}; {chk.stdout.strip() or chk.stderr.strip()}'


if __name__ == '__main__':
    args = sys.argv[1:]
    jobs = 4
    if '--jobs' in args:
        i = args.index('--jobs'); jobs = int(args[i + 1]); del args[i:i + 2]
    nodes = json.loads((PRIOR / 'nodes.json').read_text())
    if args:
        nodes = [n for n in nodes if n['node'] in args]
    LOGS.mkdir(parents=True, exist_ok=True)
    with ThreadPoolExecutor(jobs) as ex:
        for name, status in ex.map(run, nodes):
            print(name, status, flush=True)
