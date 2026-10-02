"""Run one `codex exec` per R-node (prompt.md + the node's row from nodes.json), a few at a
time; each writes <NODE>-links.edn, then is checked by check_bids.py. Usage:
  python3 run_bids.py [NODE ...] [--jobs 4]"""
import json, subprocess, sys
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

HERE = Path(__file__).resolve().parent
LOGS = HERE.parents[2] / 'data' / 'audits' / 'rnode-bids' / 'logs'


def run(node):
    out, log = HERE / f'{node["node"]}-links.edn', LOGS / f'{node["node"]}.jsonl'
    if out.exists() and out.stat().st_size:
        return node['node'], 'exists'
    prompt = (HERE / 'prompt.md').read_text().replace('{NODE}', node['node']) + json.dumps(node, indent=1, ensure_ascii=False)
    with open(log, 'w') as lf:
        r = subprocess.run(['codex', 'exec', '--ephemeral', '--skip-git-repo-check', '-s', 'read-only',
                            '-C', '/home/joe/code', '--json', '-o', str(out), '-'],
                           input=prompt, text=True, stdout=lf, stderr=subprocess.STDOUT)
    chk = subprocess.run([sys.executable, str(HERE / 'check_bids.py'), str(out)], capture_output=True, text=True)
    return node['node'], f'exit {r.returncode}; {chk.stdout.strip() or chk.stderr.strip()}'


if __name__ == '__main__':
    args = sys.argv[1:]
    jobs = 4
    if '--jobs' in args:
        i = args.index('--jobs'); jobs = int(args[i + 1]); del args[i:i + 2]
    nodes = json.loads((HERE / 'nodes.json').read_text())
    if args:
        nodes = [n for n in nodes if n['node'] in args]
    LOGS.mkdir(parents=True, exist_ok=True)
    with ThreadPoolExecutor(jobs) as ex:
        for name, status in ex.map(run, nodes):
            print(name, status, flush=True)
