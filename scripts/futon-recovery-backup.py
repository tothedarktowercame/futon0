#!/usr/bin/env python3
"""Copy all Zone Git branches and per-worktree WIP to two recovery hubs.

Recovery refs are deliberately separate from publication refs.  Repositories
are discovered from every immediate Git checkout under ~/code (except storage
and lean4), then de-duplicated by Git common directory.  Linked worktrees from
any location are included by `git worktree list`.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import tempfile
from datetime import datetime, timezone
from urllib.parse import urlparse

DEFAULT_HUBS = [
    "ssh://joe@172.236.108.82:2222",
    "ssh://joe@172.236.28.208:2222",
]
EXCLUDE = {"storage", "lean4"}
MAX_UNTRACKED_BYTES = 20 * 1024 * 1024


def run(args, cwd=None, env=None, check=False):
    p = subprocess.run(args, cwd=cwd, env=env, text=True,
                       stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if check and p.returncode:
        raise RuntimeError(f"{' '.join(args)}: {p.stderr.strip()}")
    return p


def git(path: Path, *args, env=None, check=False):
    return run(["git", "-C", str(path), *args], env=env, check=check)


def discover(code: Path):
    groups = {}
    for path in sorted(code.iterdir()):
        if not path.is_dir() or path.name in EXCLUDE:
            continue
        common = git(path, "rev-parse", "--path-format=absolute", "--git-common-dir")
        if common.returncode:
            continue
        key = str(Path(common.stdout.strip()).resolve())
        groups.setdefault(key, path)
    return list(groups.values())


def worktrees(repo: Path):
    out = git(repo, "worktree", "list", "--porcelain", check=True).stdout
    return [Path(line[9:]) for line in out.splitlines() if line.startswith("worktree ")]


def ref_key(path: Path):
    slug = re.sub(r"[^A-Za-z0-9._-]+", "-", path.name).strip("-") or "root"
    digest = hashlib.sha256(str(path.resolve()).encode()).hexdigest()[:12]
    return f"{slug}-{digest}"


def dirty(path: Path):
    return bool(git(path, "status", "--porcelain=v1", "-uall").stdout)


def snapshot(path: Path, ref: str, cutoff: int):
    head = git(path, "rev-parse", "-q", "--verify", "HEAD")
    if head.returncode:
        return {"status": "held", "reason": "head-missing"}
    with tempfile.NamedTemporaryFile() as idx:
        env = dict(os.environ, GIT_INDEX_FILE=idx.name)
        git(path, "read-tree", "HEAD", env=env, check=True)
        git(path, "add", "-u", ".", env=env, check=True)
        raw = git(path, "ls-files", "-z", "-o", "--exclude-standard", check=True).stdout
        included, excluded = [], []
        for name in filter(None, raw.split("\0")):
            candidate = path / name
            if candidate.is_file() and candidate.stat().st_size <= cutoff:
                included.append(name)
            else:
                excluded.append(name)
        if included:
            git(path, "add", "-f", "--", *included, env=env, check=True)
        tree = git(path, "write-tree", env=env, check=True).stdout.strip()
    old = git(path, "rev-parse", "-q", "--verify", ref)
    if old.returncode == 0:
        old_tree = git(path, "rev-parse", f"{old.stdout.strip()}^{{tree}}", check=True).stdout.strip()
        if old_tree == tree:
            return {"status": "unchanged", "ref": ref, "sha": old.stdout.strip(),
                    "included": len(included), "excluded": len(excluded)}
    message = f"zone wip snapshot {datetime.now(timezone.utc).isoformat()} {path}"
    made = subprocess.run(["git", "-C", str(path), "commit-tree", tree, "-p", head.stdout.strip()],
                          input=message + "\n", text=True, stdout=subprocess.PIPE,
                          stderr=subprocess.PIPE, check=True).stdout.strip()
    git(path, "update-ref", ref, made, check=True)
    return {"status": "written", "ref": ref, "sha": made,
            "included": len(included), "excluded": len(excluded)}


def ensure_hub(hub: str, name: str):
    if "://" not in hub:
        bare = Path(hub) / f"{name}.git"
        if not bare.exists():
            run(["git", "init", "--bare", str(bare)], check=True)
        return str(bare)
    u = urlparse(hub)
    target = f"{u.username + '@' if u.username else ''}{u.hostname}"
    port = str(u.port or 22)
    remote_dir = f"git/zone-backup/{name}.git"
    run(["ssh", "-o", "BatchMode=yes", "-p", port, target,
         f"[ -d {remote_dir} ] || git init -q --bare {remote_dir}"], check=True)
    return f"{hub}/~/git/zone-backup/{name}.git"


def backup_repo(repo: Path, hubs, cutoff):
    name = repo.name
    common = Path(git(repo, "rev-parse", "--path-format=absolute", "--git-common-dir",
                      check=True).stdout.strip())
    hook = common / "hooks" / "post-commit"
    rows, active_refs = [], []
    for wt in worktrees(repo):
        row = {"path": str(wt), "key": ref_key(wt)}
        ref = f"refs/backup/wip/{row['key']}"
        if not wt.exists():
            row.update(status="unavailable")
        elif dirty(wt):
            row.update(snapshot(wt, ref, cutoff))
            active_refs.append(ref)
        else:
            git(repo, "update-ref", "-d", ref)
            row.update(status="clean")
        rows.append(row)
    outcomes = []
    specs = ["+refs/heads/*:refs/backup/branches/*",
             "+refs/tags/*:refs/backup/tags/*",
             "+refs/backup/wip/*:refs/backup/wip/*"]
    for hub in hubs:
        try:
            remote = ensure_hub(hub, name)
            p = git(repo, "push", "--prune", remote, *specs)
            outcomes.append({"hub": hub, "ok": p.returncode == 0,
                             "error": p.stderr.strip() if p.returncode else None})
        except Exception as exc:
            outcomes.append({"hub": hub, "ok": False, "error": str(exc)})
    return {"repo": name, "path": str(repo), "worktrees": rows,
            "hubs": outcomes, "ok": all(x["ok"] for x in outcomes),
            "untracked-cutoff-bytes": cutoff,
            "post-commit-hook-installed": hook.exists() and os.access(hook, os.X_OK)}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--code", type=Path, default=Path.home() / "code")
    ap.add_argument("--hub", action="append", dest="hubs")
    ap.add_argument("--receipt", type=Path)
    ap.add_argument("--max-untracked-bytes", type=int, default=MAX_UNTRACKED_BYTES)
    args = ap.parse_args()
    hubs = args.hubs or DEFAULT_HUBS
    rows = [backup_repo(repo, hubs, args.max_untracked_bytes)
            for repo in discover(args.code)]
    report = {"ok": all(r["ok"] for r in rows),
              "at": datetime.now(timezone.utc).isoformat(),
              "scope": "all-immediate-code-git-repositories-plus-linked-worktrees",
              "repos": rows}
    encoded = json.dumps(report, indent=2, sort_keys=True)
    if args.receipt:
        args.receipt.parent.mkdir(parents=True, exist_ok=True)
        args.receipt.write_text(encoded + "\n")
    print(encoded)
    raise SystemExit(0 if report["ok"] else 1)


if __name__ == "__main__":
    main()
