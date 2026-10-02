"""Check rnode-tree.edn: every catalogue node except TRACE is exactly one leaf; every split's
children differ on the split key; every branch and leaf has cues; prints the leaf paths."""
import sys
from pathlib import Path
import edn_format
from edn_format import Keyword as K

HERE = Path(__file__).resolve().parent
CATALOGUE = Path('/home/joe/code/p4ng/empirics-futon/control-stages.edn')


def walk(node, path, out, problems):
    if not node.get(K('cues')) and K('object') not in node and K('scope') not in node and path:
        problems.append(f'no cues at {path}')
    if K('leaf') in node:
        out.append((node[K('leaf')], path))
        return
    split, kids = node.get(K('split')), node.get(K('children')) or []
    if not split or len(kids) < 2:
        problems.append(f'internal node without split or with <2 children at {path}')
    vals = [str(k.get(split)) for k in kids]
    if len(set(vals)) != len(vals) or 'None' in vals:
        problems.append(f'children not disambiguated by {split} at {path}: {vals}')
    for k in kids:
        walk(k, path + [f'{str(split).lstrip(":")}=' + ('·'.join(str(v).lstrip(':') for v in k.get(split)) if (hasattr(k.get(split), '__iter__') and not isinstance(k.get(split), str)) else str(k.get(split)).lstrip(':'))], out, problems)


def main(path=HERE / 'rnode-tree.edn'):
    tree = edn_format.loads(Path(path).read_text())
    leaves, problems = [], []
    for b in tree[K('branches')]:
        walk(b, [f'beer={b[K("beer")]}'], leaves, problems)
    cat = {n[K('node')] for n in edn_format.loads(CATALOGUE.read_text())[K('nodes')]} - {'TRACE'}
    names = [l for l, _ in leaves]
    dup = {n for n in names if names.count(n) > 1}
    if dup: problems.append(f'nodes on more than one leaf: {sorted(dup)}')
    if cat - set(names): problems.append(f'catalogue nodes missing: {sorted(cat - set(names))}')
    if set(names) - cat: problems.append(f'leaves not in catalogue: {sorted(set(names) - cat)}')
    for leaf, p in leaves:
        print(f'{leaf:11} ' + ' > '.join(p))
    print('OK' if not problems else 'FAIL\n' + '\n'.join(problems))
    return 0 if not problems else 1


if __name__ == '__main__':
    sys.exit(main(*sys.argv[1:]))
