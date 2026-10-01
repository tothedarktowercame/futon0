"""Build the inputs for the R-node bidding round: nodes.json (one row per R-node with
its Lean modules and code basis) and patterns.md (the top 150 functional patterns by hits)."""
import json, re, subprocess
from pathlib import Path
import edn_format
from edn_format import Keyword as K

HERE = Path(__file__).resolve().parent
CODE = Path('/home/joe/code')
STAGES = CODE / 'p4ng/empirics-futon/control-stages.edn'
DAG = CODE / 'p4ng/empirics-futon/aif-lean-dag.edn'
LABELS = HERE.parent / 'pattern-stages-2026-10-01.edn'
LEAN = CODE / 'mathlib4'
TOP_N = 150


def nodes():
    cat = edn_format.loads(STAGES.read_text())
    mods = json.loads(subprocess.check_output(
        ['bb', '-e', '(println (cheshire.core/generate-string (:node-modules (:join (clojure.edn/read-string (slurp "%s"))))))' % DAG]))
    out = []
    for n in cat[K('nodes')]:
        node = n[K('node')]
        if node == 'TRACE':          # retired 2026-09-10 (C498)
            continue
        files = [str(LEAN / (m.replace('.', '/') + '.lean')) for m in mods.get(node, [])]
        assert all(Path(f).exists() for f in files), (node, files)
        out.append({'node': node, 'stage': n[K('stage')], 'band': str(n[K('band')]).lstrip(':'),
                    'label': n[K('label')], 'basis': n.get(K('basis')), 'lean-files': files})
    return out


def field(text, name):
    m = re.search(r'\+ %s:\s*\n(.*?)(?=\n\s*\+ [A-Z-]+:|\Z)' % re.escape(name), text, re.S)
    return ' '.join(m.group(1).split()) if m else ''


def patterns():
    doc = edn_format.loads(LABELS.read_text())
    rows = [p for p in doc[K('patterns')]
            if str(p.get(K('kind'))) in (':practice', ':mixed') and (p.get(K('hits')) or 0) > 0]
    rows.sort(key=lambda p: (-(p.get(K('hits')) or 0), p[K('id')]))
    out = []
    for p in rows[:TOP_N]:
        path = CODE / p[K('path')]
        text = path.read_text() if path.exists() else ''
        concl = re.search(r'! conclusion:\s*(.*)', text)
        out.append({'id': p[K('id')], 'path': str(path), 'stage': str(p[K('stage')]).lstrip(':'),
                    'hits': p.get(K('hits')), 'title': p.get(K('title')) or '',
                    'conclusion': (concl.group(1) if concl else '')[:240],
                    'then': field(text, 'THEN')[:240]})
    return out


if __name__ == '__main__':
    ns, ps = nodes(), patterns()
    (HERE / 'nodes.json').write_text(json.dumps(ns, indent=1, ensure_ascii=False))
    lines = ['# Top %d functional patterns by hits (22 Aug - 1 Oct 2026)\n' % len(ps),
             'Columns: id | stage label | hits | title | conclusion (truncated) | THEN (truncated). Open `path` for the full text.\n']
    for p in ps:
        lines.append('- `%s` | %s | %s | %s | %s | THEN: %s | path: %s' % (
            p['id'], p['stage'], p['hits'], p['title'], p['conclusion'], p['then'], p['path']))
    (HERE / 'patterns.md').write_text('\n'.join(lines) + '\n')
    print(len(ns), 'nodes;', sum(1 for n in ns if n['lean-files']), 'with Lean files;', len(ps), 'patterns;',
          'hits covered', sum(p['hits'] for p in ps))
