#!/usr/bin/env python3
"""Have Kimi seats read unlabelled patterns into the pattern-stage rubric.

Joe (2026-10-01): label the 97 patterns retrieved for his turns after
2026-09-21 that pattern-stages-2026-09-21.edn does not cover, the same way the
689 were labelled: a reader takes the pattern's own text, picks kind and stage,
and quotes the line that warrants it.  Packs of ten, one seat per pack.

  python3 pattern_stage_read_loop.py --seats kimi-1,kimi-2 [--minutes 90]

Each answer is checked here, not trusted: kind/stage/mode/confidence from the
closed lists, the id one that was asked, and the quote a verbatim (whitespace-
normalised) substring of the source.  Accepted rows append to
pattern-stage-additions-2026-10-01.jsonl with the source path and sha256;
refused ones are logged and the pattern is retried once.
Progress: tail -f pattern-stage-read-loop.log.jsonl
Stop early: touch /tmp/claude17/pattern-stage-read.STOP
"""
import argparse, datetime, glob, hashlib, json, os, re, subprocess, threading, time, urllib.request

HERE = os.path.dirname(os.path.abspath(__file__))
CODE = "/home/joe/code"
UNLABELLED = os.path.join(HERE, "pattern-stage-unlabelled-2026-10-01.json")
OUT = os.path.join(HERE, "pattern-stage-additions-2026-10-01.jsonl")
LOG = os.path.join(HERE, "pattern-stage-read-loop.log.jsonl")
WORK = "/tmp/claude17/pattern-stage-read"
STOP = "/tmp/claude17/pattern-stage-read.STOP"
API = "http://localhost:7070/api/alpha"
CALLER = "pattern-stage-read-loop"   # not on the roster: no bellbacks
KINDS = {"practice", "subject", "mixed"}
STAGES = {"perceive", "believe", "evaluate", "select", "act", "assurance", "coordination", "none"}
MODES = {"functional", "topical", "functional-and-topical"}
CONF = {"high", "medium", "low"}
lock = threading.Lock()

RUBRIC = """You are labelling design patterns for a count of what stage of an Active Inference control loop Joe's work touched. Label each pattern below from ITS OWN TEXT only (IF / HOWEVER / THEN / BECAUSE, else the `! conclusion`), never from its filename or directory.

kind (one):
- practice: a way of working a person or agent performs (review, plan, record, route, check).
- subject: what the work is about; content, a design decision, a domain technique (math formalisation techniques are always subject, even if imperative).
- mixed: genuinely both.

stage (one), with the gloss the existing 689 labels use:
- perceive: observes or exposes state
- believe: records, represents, or revises what is known
- evaluate: weighs consequences, options, or limits
- select: sets a choice, priority, or admission condition
- act: implements or executes the work
- assurance: makes a claim independently checkable or preserves its trace
- coordination: routes work, roles, or communication between parties
- none: the text states no control-stage operation (a lookup/encoding record, a bare meta-tag)

stage-mode: functional (the pattern PERFORMS that stage), topical (it is ABOUT that stage), or functional-and-topical.
confidence: high (clear from the text), medium, low.
node: optional, only if the text itself names an R-node (e.g. "R9"); else omit.
quote-field: IF, HOWEVER, THEN, BECAUSE or CONCLUSION.
quote: copy a span of 6-40 words EXACTLY from that field of the source shown (it is checked character for character after collapsing whitespace). Do not paraphrase, do not add ellipses.
rationale: one sentence: why this stage, citing the quote.

Answer: write {answer} as a JSON array, one object per pattern:
  {{"id": ..., "kind": ..., "stage": ..., "stage-mode": ..., "confidence": ..., "node": optional, "quote-field": ..., "quote": ..., "rationale": ...}}
Rewrite the file after each pattern and check it parses: python3 -c "import json;print(len(json.load(open('{answer}'))))"
Do all {n}. Do not write anywhere else, do not commit, do not publish. Nobody needs belling; end with one line: the answer path and the count.
"""


def now():
    return datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def log(row):
    with lock, open(LOG, "a") as fh:
        fh.write(json.dumps(row, ensure_ascii=False) + "\n")


def norm(t):
    return " ".join((t or "").split())


def source_of(pid):
    """(path, sha256 of the file, text of this pattern's section)."""
    path = os.path.join(CODE, "futon3/library", pid + ".flexiarg")
    if not os.path.exists(path):
        hits = subprocess.run(["grep", "-rlE", r"^@arg\s+" + re.escape(pid) + r"\s*$",
                               os.path.join(CODE, "futon3/library")],
                              capture_output=True, text=True).stdout.split()
        if not hits:
            return None, None, None
        path = hits[0]
    raw = open(path, encoding="utf-8").read()
    text = raw
    if path.endswith(".multiarg"):
        parts = re.split(r"(?m)^(?=@arg\s)", raw)
        text = next((p for p in parts if re.match(r"@arg\s+" + re.escape(pid) + r"\s*$", p.splitlines()[0] if p else "")), "")
    return os.path.relpath(path, CODE), hashlib.sha256(raw.encode()).hexdigest(), text


def done_ids():
    if not os.path.exists(OUT):
        return set()
    return {json.loads(l)["id"] for l in open(OUT, encoding="utf-8") if l.strip()}


def check(ans, want, srcs):
    """Accepted rows and {id: reason} for refusals."""
    ok, bad = [], {}
    for a in ans if isinstance(ans, list) else []:
        pid = a.get("id")
        if pid not in want:
            continue
        why = None
        if a.get("kind") not in KINDS: why = f"kind {a.get('kind')!r}"
        elif a.get("stage") not in STAGES: why = f"stage {a.get('stage')!r}"
        elif a.get("stage-mode") not in MODES: why = f"stage-mode {a.get('stage-mode')!r}"
        elif a.get("confidence") not in CONF: why = f"confidence {a.get('confidence')!r}"
        elif len(norm(a.get("quote")).split()) < 4: why = "quote too short"
        elif norm(a.get("quote")) not in norm(srcs[pid][2]): why = "quote not verbatim in source"
        if why:
            bad[pid] = why
            continue
        path, sha, _ = srcs[pid]
        row = {k: a[k] for k in ("id", "kind", "stage", "stage-mode", "confidence",
                                 "quote-field", "quote", "rationale") if k in a}
        if a.get("node"):
            row["node"] = a["node"]
        row.update({"hits": want[pid], "path": path, "sha256": sha, "reader": None})
        ok.append(row)
    for pid in want:
        if pid not in bad and pid not in {r["id"] for r in ok}:
            bad[pid] = "missing from answer"
    return ok, bad


def get(path):
    with urllib.request.urlopen(API + path, timeout=30) as r:
        return json.load(r)


def wait(job, limit_s=45 * 60):
    t0 = time.time()
    while time.time() - t0 < limit_s:
        try:
            j = get(f"/invoke/jobs/{job}")["job"]
            if j["state"] in ("done", "failed", "cancelled"):
                return j
        except Exception:
            pass
        time.sleep(20)
    return {"state": "timeout"}


queue, tries = [], {}


def claim(n=10):
    with lock:
        take, rest = queue[:n], queue[n:]
        queue[:] = rest
        for pid, _ in take:
            tries[pid] = tries.get(pid, 0) + 1
        return take


def seat_loop(seat, srcs, end_at):
    while time.time() < end_at and not os.path.exists(STOP):
        pack = claim()
        if not pack:
            return
        want = dict(pack)
        tag = f"{seat}-{int(time.time())}"
        answer, cover = f"{WORK}/{tag}.answer.json", f"{WORK}/{tag}.md"
        body = [RUBRIC.format(answer=answer, n=len(pack))]
        for pid, hits in pack:
            path, sha, text = srcs[pid]
            body.append(f"\n---\n## {pid}\nsource: {path}\n\n```\n{text.strip()}\n```\n")
        open(cover, "w").write("\n".join(body))
        out = subprocess.run(["bash", os.path.join(CODE, "futon3c/scripts/kimi-task.sh"), "--from", CALLER,
                              "--to", seat, "--purpose", f"pattern-stage reading {tag} ({len(pack)} patterns)", cover],
                             capture_output=True, text=True).stdout
        m = re.search(r"task=(\S+) .*job=(\S+)", out)
        row = {"at": now(), "seat": seat, "ids": list(want)}
        if not m:
            row.update({"error": "dispatch failed", "detail": out[-300:]})
            log(row)
            with lock:
                queue.extend(pack)
            time.sleep(60)
            continue
        req, job = m.group(1), m.group(2)
        started = time.time()
        j = wait(job)
        subprocess.run(["bash", os.path.join(CODE, "futon3c/scripts/kimi-task.sh"), "--complete", req, job],
                       capture_output=True, text=True)
        row.update({"job": job, "task": req, "state": j.get("state"),
                    "minutes": round((time.time() - started) / 60, 1)})
        try:
            ans = json.load(open(answer))
        except (OSError, ValueError) as e:
            ans, row["error"] = [], f"answer: {e}"
        ok, bad = check(ans, want, srcs)
        with lock:
            with open(OUT, "a", encoding="utf-8") as fh:
                for r in ok:
                    r["reader"] = f"{seat} {job}"
                    fh.write(json.dumps(r, ensure_ascii=False) + "\n")
            for pid, why in bad.items():
                if tries.get(pid, 0) < 2:
                    queue.append((pid, want[pid]))
        row.update({"accepted": len(ok), "refused": bad})
        log(row)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--seats", required=True)
    ap.add_argument("--minutes", type=float, default=90)
    a = ap.parse_args()
    os.makedirs(WORK, exist_ok=True)
    have = done_ids()
    srcs = {}
    for u in json.load(open(UNLABELLED))["unlabelled"]:
        pid = u["pattern-id"]
        if pid in have:
            continue
        srcs[pid] = source_of(pid)
        if srcs[pid][0] is None:
            log({"at": now(), "id": pid, "error": "no source"})
            continue
        queue.append((pid, u["hits"]))
    end_at = time.time() + a.minutes * 60
    log({"at": now(), "start": True, "seats": a.seats, "patterns": len(queue)})
    threads = [threading.Thread(target=seat_loop, args=(s, srcs, end_at)) for s in a.seats.split(",")]
    for t in threads:
        t.start(); time.sleep(5)
    for t in threads:
        t.join()
    log({"at": now(), "end": True, "accepted": len(done_ids()), "left": [p for p, _ in queue]})


if __name__ == "__main__":
    main()
