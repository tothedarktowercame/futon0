"""Check a <NODE>-links.edn: parses, has the fields, every cited file exists and its cited
line mentions the cited symbol's last segment within 3 lines, every bid names a pattern
in patterns.md, at most 15 bids. Prints one summary line; exit 1 on any failure."""
import re, sys
from pathlib import Path
import edn_format
from edn_format import Keyword as K

HERE = Path(__file__).resolve().parent
IDS = set(re.findall(r'^- `([^`]+)`', (HERE / 'patterns.md').read_text(), re.M))


def cite_ok(c):
    f, line, sym = c.get(K('file')), c.get(K('line')), str(c.get(K('symbol')) or '')
    if not f or not Path(f).exists() or not isinstance(line, int):
        return False
    lines = Path(f).read_text(errors='replace').splitlines()
    leaf = re.split(r'[./]', sym)[-1] if sym else ''
    window = '\n'.join(lines[max(0, line - 4):line + 3])
    return bool(leaf) and leaf in window


def check(path):
    problems = []
    try:
        d = edn_format.loads(Path(path).read_text())
    except Exception as e:
        return [f'unparseable: {e}'], None
    if not hasattr(d, 'get') or K('node') not in d:
        return ['not a map with :node'], None
    if Path(path).name != f'{d[K("node")]}-links.edn':
        problems.append('node does not match file name')
    bids, prim = list(d.get(K('bids')) or []), list(d.get(K('priming')) or [])
    if len(bids) > 15: problems.append(f'{len(bids)} bids > 15')
    if not prim: problems.append('no priming claims')
    for p in prim:
        if not cite_ok(p): problems.append(f'priming cite fails: {p.get(K("symbol"))} {p.get(K("file"))}:{p.get(K("line"))}')
    for b in bids:
        pid = b.get(K('pattern'))
        if pid not in IDS: problems.append(f'unknown pattern {pid}')
        if b.get(K('kind')) not in (K('functional'), K('topic')): problems.append(f'{pid}: bad :kind')
        if b.get(K('strength')) not in (K('strong'), K('partial')): problems.append(f'{pid}: bad :strength')
        if not (b.get(K('how')) or '').strip(): problems.append(f'{pid}: no :how')
        if not cite_ok(b.get(K('code')) or {}): problems.append(f'{pid}: code cite fails')
    return problems, d


if __name__ == '__main__':
    problems, d = check(sys.argv[1])
    n = d[K('node')] if d else '?'
    nb = len(d.get(K('bids')) or []) if d else 0
    print(f'{n}: {nb} bids; ' + ('OK' if not problems else 'FAIL ' + ' | '.join(problems)))
    sys.exit(1 if problems else 0)
