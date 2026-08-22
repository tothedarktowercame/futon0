#!/usr/bin/env python3
"""Windowed term-attention over OPERATOR turns.

Extends the commit-per-repo prototype (zone.hyperreal.enterprises/attention.html,
"Where the attention went") along two axes:

  prototype        this
  -----------      ----------------------------------------
  unit: commit     unit: operator turn   (commits are mostly agent-authored,
                                          so they measure system output; turns
                                          measure the operator directly)
  dim: repo        dim: search term      (finer than a repo, and not tied to
                                          where a file happens to live)

Backed by M-text-sidecar's FTS5 surface, which is DONE and live:
  GET /api/alpha/evidence/text-search?q=&author=&limit=&hydrate=false

Contract hazard (API-CONTRACT.md): a 503 :expensive-read-busy is NOT an empty
result. Status is checked before any row is counted.

Usage:  attention_terms.py [--weeks 16] [--author joe] TERM [TERM ...]
        attention_terms.py --json ...          machine-readable series
"""
import argparse, collections, datetime, json, re, sys, urllib.request, urllib.error

BASE = "http://localhost:7073/api/alpha/evidence/text-search"
BARS = " ▁▂▃▄▅▆▇█"


def fetch(term, author, limit=3000):
    """Return the :evidence/at timestamps for `term`, or raise on a busy store."""
    url = f"{BASE}?q={urllib.parse.quote(term)}&limit={limit}&hydrate=false&author={author}"
    try:
        with urllib.request.urlopen(url, timeout=90) as r:
            if r.status != 200:                      # never count rows from a non-200
                raise RuntimeError(f"HTTP {r.status} for {term!r}")
            body = r.read().decode()
    except urllib.error.HTTPError as e:
        raise RuntimeError(f"HTTP {e.code} for {term!r} "
                           f"({'store busy, retry with backoff' if e.code == 503 else e.reason})")
    if ":ok false" in body:
        raise RuntimeError(f"store returned :ok false for {term!r}")
    ats = re.findall(r':evidence/at "([^"]+)"', body)
    m = re.search(r':count (\d+)', body)
    n = int(m.group(1)) if m else len(ats)
    if n >= limit:
        print(f"  ! {term}: count {n} hit the limit — raise --limit", file=sys.stderr)
    return ats


def series(ats, weeks, today):
    """Weekly counts, oldest-first, ending in the current week."""
    c = collections.Counter()
    for a in ats:
        w = (today - datetime.date.fromisoformat(a[:10])).days // 7
        if 0 <= w < weeks:
            c[w] += 1
    return [c.get(weeks - 1 - i, 0) for i in range(weeks)]


def spark(vals):
    mx = max(vals) or 1
    return "".join(BARS[min(8, round(v * 8 / mx))] for v in vals)


def shape(vals):
    """Burst vs thread: what a raw total hides.

    A topic worked in one sitting and a topic carried for months can have
    similar totals and mean completely different things, so report the
    concentration, not just the sum."""
    tot = sum(vals)
    if not tot:
        return "silent"
    active = sum(1 for v in vals if v)
    top = max(vals) / tot
    if top >= .5 and active <= 3:
        return "burst"
    if active >= len(vals) * .4 and top < .4:
        return "thread"
    return "mixed"


def main():
    p = argparse.ArgumentParser()
    p.add_argument("terms", nargs="+")
    p.add_argument("--weeks", type=int, default=16)
    p.add_argument("--author", default="joe")
    p.add_argument("--limit", type=int, default=3000)
    p.add_argument("--today", default=None, help="YYYY-MM-DD; defaults to today")
    p.add_argument("--json", action="store_true")
    a = p.parse_args()
    today = datetime.date.fromisoformat(a.today) if a.today else datetime.date.today()

    out = []
    for t in a.terms:
        ats = fetch(t, a.author, a.limit)
        vals = series(ats, a.weeks, today)
        out.append({"term": t, "values": vals, "total": len(ats),
                    "windowed": sum(vals), "peak": max(vals) if vals else 0,
                    "shape": shape(vals)})

    if a.json:
        print(json.dumps({"author": a.author, "weeks": a.weeks,
                          "as_of": today.isoformat(), "series": out}, indent=1))
        return
    print(f"\n{a.author}'s turns per week, last {a.weeks} weeks "
          f"(rightmost = week of {today})\n")
    for s in out:
        print(f"  {s['term']:22s} |{spark(s['values'])}|  peak={s['peak']:4d}"
              f"  {a.weeks}wk={s['windowed']:5d}  all={s['total']:5d}  {s['shape']}")
    print()


if __name__ == "__main__":
    main()
