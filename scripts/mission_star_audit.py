#!/usr/bin/env python3
"""Mission / capability-star audit (2026-10-25, approved by Joe; read-only).

Stages:
  collect   - scan <code-root>/*/holes/missions/M-*.md, read status lines the
              way futon2.aif.mission-registry does (first 20 lines,
              status-line-pattern, lead-token classification), last commit
              date via git log -1, declared star links from the star-map
              graph (:missions :produces and each star's :minted-by), and
              stated wants from unchecked "- [ ]" task lines.
  infer     - emit missions lacking a declared link as batches for external
              inference, and/or verify an inference JSON (quotes are
              string-checked against the doc head).
  report    - write the audit markdown.

Usage:
  python3 mission_star_audit.py collect  --out missions.json
  python3 mission_star_audit.py infer    --missions missions.json --emit-batches dir
  python3 mission_star_audit.py infer    --missions missions.json --verify inferences.json
  python3 mission_star_audit.py report   --missions missions.json --out AUDIT.md
"""

from __future__ import annotations

import argparse
import datetime as dt
import json
import re
import subprocess
from pathlib import Path

CODE_ROOT = Path("/home/joe/code")
GRAPH = CODE_ROOT / "futon0/holes/missions/M-capability-star-map.graph.edn"
TODAY = dt.date(2026, 10, 25)

STATUS_LINE = re.compile(r"^\s*(?:[-*]\s*)?(?:#+\s*)?(?:\*\*)?Status:?(?:\*\*)?\s*:?\s*(.+)$", re.I)
UNCHECKED = re.compile(r"^\s*[-*]\s+\[\s\]\s+(\S.*)$")
# A stated want is "admitted" when its text names a mechanically checkable
# artifact: a path, a script/test invocation, an exit code, a sha, or a
# countable/regex-checkable claim.  This is a documented heuristic standing in
# for the registry's runtime locator classes (:C3 :C4 :C5 :C6 :C8); it is
# stated as such in the report.
CHECKABLE = re.compile(
    r"(?:/[A-Za-z0-9_.-]+/|\b(?:bb|clojure|python3?|make|lake|git|curl|emacs)\b"
    r"|\.clj\b|\.py\b|\.el\b|\.edn\b|\.lean\b|exits?\s+0|sha256|JSON|grep|regex"
    r"|\btests?\b|\bchecks?\b|\bemits?\b|\bcount\b|\d+)",
    re.I,
)


def classify_status(line):
    if not line:
        return "no-status"
    upper = line.upper()
    if "SPECIFIED, NOT YET IMPLEMENTED" in upper:
        return "draft"
    lead = re.sub(r"^[\s>*#`_~-]+", "", upper).strip()
    m = re.match(r"[A-Z][A-Z-]*", lead)
    head = m.group(0) if m else ""
    if re.match(r"^(?:OPEN|ACTIVE|PARTIAL|IDENTIFY|HEAD|INSTANTIATE|MAP|DERIVE|ARGUE|VERIFY)\s*(?:[-\u2014\u2013:/]|\()\s*(?:PARKED|FROZEN|DEFERRED)\b", lead):
        return "inactive"
    if any(head.startswith(p) for p in ("ARCHIVED", "PARKED", "SUPERSEDED", "ABANDONED", "DEFERRED", "FROZEN")):
        return "inactive"
    if any(head.startswith(p) for p in ("COMPLETE", "COMPLETED", "CLOSED", "DONE", "DISCHARGED", "ANSWERED", "DISSOLVED")):
        return "complete"
    for tok, cls in (("ACTIVE", "active"), ("OPEN", "open"), ("PARTIAL", "partial"), ("DRAFT", "draft")):
        if head == tok:
            return cls
    return "unknown"


def repo_of(path):
    for p in [path.parent] + list(path.parents):
        if (p / ".git").exists():
            return p
    return path.parent


def last_commit(path):
    repo = repo_of(path)
    try:
        out = subprocess.run(
            ["git", "-C", str(repo), "log", "-1", "--format=%cI", "--", str(path)],
            capture_output=True, text=True, timeout=30,
        )
        return out.stdout.strip() or None
    except Exception:
        return None


def parse_graph(text):
    stars, missions = {}, {}
    cap_m = re.search(r":capabilities\s*(\{.*?\n :missions)", text, re.S)
    cap_text = cap_m.group(1) if cap_m else ""
    mis_m = re.search(r":missions\s*(\{.*)", text, re.S)
    mis_text = mis_m.group(1) if mis_m else ""
    for m in re.finditer(r"\n\s*:([a-z0-9-]+)\s*\{", cap_text):
        key = m.group(1)
        # a star block ends at the next star key header, not at the first
        # closing brace (inner maps close at deeper indent and truncate it)
        nxt = cap_text.find("\n  :", m.end())
        end = cap_text.find("\n }", m.end())
        stop = min(x for x in (nxt, end, len(cap_text)) if x > 0)
        block = cap_text[m.start(): stop]
        title = re.search(r':title\n?\s+"((?:[^"\\]|\\.)*)"', block)
        status = re.search(r":status\s+:([a-z]+)", block)
        mb = re.search(r":minted-by\n?\s*\[[^\]]*\]", block)
        minted = re.findall(r'"(M-[^"]+)"', mb.group(0)) if mb else []
        stars[key] = {"title": title.group(1) if title else "",
                      "status": status.group(1) if status else "?",
                      "minted-by": minted}
    for m in re.finditer(r'"(M-[^"]+)"\s*\{', mis_text):
        mid = m.group(1)
        end = mis_text.find("},", m.end())
        block = mis_text[m.end(): end if end > 0 else len(mis_text)]
        pm = re.search(r":produces\s*\[[^\]]*\]", block)
        produces = re.findall(r":([a-z0-9-]+)", pm.group(0).split("[", 1)[1]) if pm else []
        missions[mid] = {"produces": produces}
    return stars, missions


def collect(out):
    files = sorted(CODE_ROOT.glob("*/holes/missions/M-*.md"))
    stars, missions = parse_graph(GRAPH.read_text())
    rows = []
    for f in files:
        text = f.read_text(errors="replace")
        lines = text.splitlines()
        status_line = next((m.group(1) for m in map(STATUS_LINE.match, lines[:20]) if m), None)
        tasks = [m.group(1) for m in map(UNCHECKED.match, lines) if m]
        mid = f.stem
        declared = sorted(set(missions.get(mid, {}).get("produces", []))
                          | {s for s, v in stars.items() if mid in v["minted-by"]})
        want = ("admitted" if tasks and any(CHECKABLE.search(t) for t in tasks)
                else "stated-no-locator" if tasks else "none")
        rows.append({"id": mid, "path": str(f), "repo": repo_of(f).name,
                     "status": classify_status(status_line), "status_line": status_line,
                     "last_commit": last_commit(f), "declared_stars": declared,
                     "unchecked_tasks": len(tasks), "want": want, "head": text[:1500]})
    out.parent.mkdir(parents=True, exist_ok=True)
    out.write_text(json.dumps({"stars": stars, "rows": rows}, indent=1))
    print(f"collected {len(rows)} missions, {len(stars)} stars -> {out}")


def emit_batches(missions, batches):
    data = json.loads(Path(missions).read_text())
    todo = [r for r in data["rows"] if not r["declared_stars"]]
    Path(batches).mkdir(parents=True, exist_ok=True)
    titles = {k: v["title"] for k, v in data["stars"].items()}
    for i in range(0, len(todo), 10):
        chunk = todo[i:i + 10]
        (Path(batches) / f"batch-{i//10:03d}.json").write_text(json.dumps(
            {"stars": titles,
             "missions": [{"id": r["id"], "head": r["head"]} for r in chunk]}, indent=1))
    print(f"{len(todo)} missions without declared stars -> {batches} ({(len(todo)+9)//10} batches)")


def verify(missions, inferences):
    p = Path(missions)
    data = json.loads(p.read_text())
    inf = json.loads(Path(inferences).read_text())
    byid = {r["id"]: r for r in data["rows"]}
    kept, dropped = 0, []
    for mid, picks in inf.items():
        r = byid.get(mid)
        if not r:
            continue
        good = []
        for pick in picks or []:
            q = (pick.get("quote") or "").strip()
            star = pick.get("star")
            if star in data["stars"] and len(q.split()) >= 8 and q in r["head"]:
                good.append(star)
        if good:
            r["inferred_stars"] = sorted(set(good))[:2]
            kept += len(r["inferred_stars"])
        else:
            dropped.append(mid)
    p.write_text(json.dumps(data, indent=1))
    print(f"kept {kept} inferred links; dropped {len(dropped)} (quote failed/star unknown): {sorted(dropped)[:10]}")


def fmt_date(iso):
    return iso[:10] if iso else "never"


def days_ago(iso):
    if not iso:
        return 9999
    return (TODAY - dt.date.fromisoformat(iso[:10])).days


def report(missions, inferences, out):
    data = json.loads(Path(missions).read_text())
    rows, stars = data["rows"], data["stars"]

    def live(r):
        return days_ago(r["last_commit"]) <= 30 and r["status"] not in ("complete", "inactive")

    by_status = {}
    for r in rows:
        by_status[r["status"]] = by_status.get(r["status"], 0) + 1
    declared = [r for r in rows if r["declared_stars"]]
    inferred = [r for r in rows if not r["declared_stars"] and r.get("inferred_stars")]
    nostar = [r for r in rows if not r["declared_stars"] and not r.get("inferred_stars")]

    L = []
    A = L.append
    A("# Mission / capability-star audit - 2026-10-25")
    A("")
    A(f"Scope: `~/code/*/holes/missions/M-*.md` ({len(rows)} docs across {len(set(r['repo'] for r in rows))} repos). "
      "Read-only audit approved by Joe 2026-10-25. Script: `futon0/scripts/mission_star_audit.py` "
      "(`collect`, then `infer --emit-batches` / `infer --verify`, then `report`).")
    A("")
    A("Status parsing mirrors `futon2.aif.mission-registry`: the first 20 lines are matched against its "
      "`status-line-pattern`, and the lead token is classified (draft / inactive / complete / active / open / "
      "partial / unknown). Docs with no Status line in the first 20 lines are counted `no-status`.")
    A("")
    A("Wants are read as unchecked `- [ ]` task lines. **admitted** uses a documented text-level heuristic standing "
      "in for the registry's runtime locator classes (:C3 :C4 :C5 :C6 :C8 of `futon2.aif.cascade-problems`): the "
      "task text names a mechanically checkable artifact (a path, a script/test invocation, an exit code, a sha, a "
      "count, or an explicit check/test/emit word). **stated-no-locator** = unchecked tasks matching none of those. "
      "This is not a run of `futon2.aif.outcome-wants/wants` against substrate-2; it is the same question asked of "
      "the doc text alone.")
    A("")
    A(f"Live = a commit within 30 days of {TODAY} and status not complete/inactive.")
    A("")
    A("Inferred links were produced by馬-sonnet (batched, 10 missions per whistle, star titles + first 1500 "
      "characters each, at most 2 stars per mission, each with a verbatim quote of >= 8 words); quotes were "
      "string-checked against the mission text and picks whose quote failed were dropped. Every such link is "
      "marked `inferred` and is not a declaration.")
    A("")
    A("## Counts")
    A("")
    A("| Measure | Count |")
    A("|---|---:|")
    for k in sorted(by_status):
        A(f"| status: {k} | {by_status[k]} |")
    A(f"| declared star link | {len(declared)} |")
    A(f"| inferred star only | {len(inferred)} |")
    A(f"| no star (declared or inferred) | {len(nostar)} |")
    for w in ("admitted", "stated-no-locator", "none"):
        A(f"| want: {w} | {sum(1 for r in rows if r['want'] == w)} |")
    A(f"| no commit in 30 days | {sum(1 for r in rows if days_ago(r['last_commit']) > 30)} |")
    A("")
    A("## Per star")
    A("")
    A("| Star | Status | Declared | Inferred | Live among them |")
    A("|---|---|---:|---:|---:|")
    for k, v in sorted(stars.items()):
        dec = [r for r in rows if k in r["declared_stars"]]
        inf = [r for r in rows if r.get("inferred_stars") and k in r["inferred_stars"]]
        A(f"| `{k}` | {v['status']} | {len(dec)} | {len(inf)} | {sum(1 for r in dec + inf if live(r))} |")
    A("")
    A("Stars with **no live mission** (no declared or inferred mission with a commit in 30 days and not closed):")
    for k, v in sorted(stars.items()):
        dec = [r for r in rows if k in r["declared_stars"]]
        inf = [r for r in rows if r.get("inferred_stars") and k in r["inferred_stars"]]
        if not any(live(r) for r in dec + inf):
            A(f"- `{k}` ({v['status']}) - {v['title'][:90]}")
    A("")
    A("Held stars of note:")
    for key in ("wm-overnight-unsupervised", "efe-trustworthy-over-starmap", "ai-passes-prelims"):
        v = stars.get(key)
        if not v:
            A(f"- `{key}` NOT FOUND in graph")
            continue
        dec = [r for r in rows if key in r["declared_stars"]]
        inf = [r for r in rows if r.get("inferred_stars") and key in r["inferred_stars"]]
        A(f"- `{key}` - status {v['status']}, declared {len(dec)}, inferred {len(inf)}, "
          f"live {sum(1 for r in dec + inf if live(r))}. {v['title'][:80]}")
    A("")
    A("## The 20 live missions with no star")
    A("")
    A("| Mission | Repo | Status | Last commit |")
    A("|---|---|---|---|")
    for r in sorted((r for r in nostar if live(r)), key=lambda r: r["last_commit"] or "", reverse=True)[:20]:
        A(f"| `{r['id']}` | {r['repo']} | {r['status']} | {fmt_date(r['last_commit'])} |")
    A("")
    Path(out).write_text("\n".join(L) + "\n")
    print(f"report -> {out}")


def main():
    p = argparse.ArgumentParser()
    sub = p.add_subparsers(dest="cmd", required=True)
    c = sub.add_parser("collect"); c.add_argument("--out", type=Path, required=True)
    i = sub.add_parser("infer"); i.add_argument("--missions", type=Path, required=True)
    i.add_argument("--emit-batches", type=Path); i.add_argument("--verify", type=Path)
    r = sub.add_parser("report"); r.add_argument("--missions", type=Path, required=True)
    r.add_argument("--out", type=Path, required=True)
    a = p.parse_args()
    if a.cmd == "collect":
        collect(a.out)
    elif a.cmd == "infer":
        if a.emit_batches:
            emit_batches(a.missions, a.emit_batches)
        if a.verify:
            verify(a.missions, a.verify)
    else:
        report(a.missions, None, a.out)


if __name__ == "__main__":
    main()
