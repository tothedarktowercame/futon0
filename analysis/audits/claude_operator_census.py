"""Daily count of genuine Joe operator turns in Claude transcripts (claude-5's census, 2026-09-21).

Rule: every ~/.claude/projects/*/*.jsonl* file (current logs AND .pre-compact-* snapshots);
rows with type "user" whose text contains "From: joe" AND "Origin: operator";
exclude park resumes ("resumed: parked") and "WAKE CHECKLIST"; dedupe by row uuid across all files;
date = UTC date of the row timestamp. Tool-result rows never match (no envelope text).
"""
import glob, json, sys
since, until = (sys.argv[1:3] + ["2026-09-08", "2026-09-21"])[:2]
seen, ops = set(), {}
for f in glob.glob('/home/joe/.claude/projects/*/*.jsonl*'):
    for line in open(f, errors='ignore'):
        if '"type":"user"' not in line or 'From: joe' not in line:
            continue
        try:
            e = json.loads(line)
        except ValueError:
            continue
        u = e.get('uuid')
        if u in seen:
            continue
        seen.add(u)
        c = e.get('message', {}).get('content')
        tx = c if isinstance(c, str) else ' '.join(x.get('text', '') for x in c if isinstance(x, dict)) if isinstance(c, list) else ''
        if 'Origin: operator' not in tx or 'resumed: parked' in tx or 'WAKE CHECKLIST' in tx:
            continue
        d = e.get('timestamp', '')[:10]
        if since <= d <= until:
            ops[d] = ops.get(d, 0) + 1
for d in sorted(ops):
    print(d, ops[d])
