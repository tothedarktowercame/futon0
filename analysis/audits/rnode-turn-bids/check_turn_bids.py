"""Check a <NODE>-turns.edn: parses; node matches file name; <=30 bids; every turn id exists;
every quote occurs verbatim (whitespace-normalised) in that turn's operator text; strength valid."""
import json, sys
from pathlib import Path
import edn_format
from edn_format import Keyword as K

HERE = Path(__file__).resolve().parent
TURNS = {t['id']: ' '.join(t['operator-turn'].split()) for t in json.loads((HERE / 'turns.json').read_text())}


def check(path):
    try:
        d = edn_format.loads(Path(path).read_text())
    except Exception as e:
        return [f'unparseable: {e}'], None
    if not hasattr(d, 'get') or K('node') not in d:
        return ['not a map with :node'], None
    problems = []
    if Path(path).name != f'{d[K("node")]}-turns.edn': problems.append('node does not match file name')
    bids = list(d.get(K('bids')) or [])
    if len(bids) > 30: problems.append(f'{len(bids)} bids > 30')
    for b in bids:
        t, q = b.get(K('turn')), ' '.join(str(b.get(K('quote')) or '').split())
        if t not in TURNS: problems.append(f'unknown turn {t}'); continue
        if not q or q not in TURNS[t]: problems.append(f'{t}: quote not in turn')
        if b.get(K('strength')) not in (K('strong'), K('partial')): problems.append(f'{t}: bad :strength')
    return problems, d


if __name__ == '__main__':
    problems, d = check(sys.argv[1])
    n = d[K('node')] if d else '?'
    nb = len(d.get(K('bids')) or []) if d else 0
    print(f'{n}: {nb} bids; ' + ('OK' if not problems else 'FAIL ' + ' | '.join(problems[:6])))
    sys.exit(1 if problems else 0)
