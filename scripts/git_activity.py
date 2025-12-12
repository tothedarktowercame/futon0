#!/usr/bin/env python3
"""Aggregate git activity across the FUTON stack.

Reads `data/git_sources.json` and emits daily commit counts per repo plus
an aggregate view suitable for a HUD heatmap.
"""

from __future__ import annotations

import argparse
import datetime as dt
import json
import subprocess
import sys
from collections import Counter, defaultdict
from pathlib import Path
from typing import Dict, List

DEFAULT_CONFIG = Path(__file__).resolve().parent.parent / "data" / "git_sources.json"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Summarise git contributions across repos")
    parser.add_argument(
        "--config",
        type=Path,
        default=DEFAULT_CONFIG,
        help="Path to git_sources.json (default: %(default)s)",
    )
    parser.add_argument(
        "--days",
        type=int,
        default=30,
        help="Window length in days (default: %(default)s)",
    )
    parser.add_argument(
        "--output",
        type=Path,
        help="Optional path to write JSON output (stdout when omitted)",
    )
    parser.add_argument(
        "--author",
        help="Optional git --author filter (case-insensitive substring)",
    )
    parser.add_argument(
        "--quiet",
        action="store_true",
        help="Suppress per-repo warnings (still returns partial results)",
    )
    return parser.parse_args()


def load_config(path: Path) -> List[Dict[str, str]]:
    if not path.exists():
        raise SystemExit(f"Config not found: {path}")
    with path.open() as handle:
        data = json.load(handle)
    repos = data.get("repos", [])
    base = path.parent
    for repo in repos:
        raw_path = Path(repo["path"])
        repo["path"] = (base / raw_path).resolve()
    return repos


def git_dates(repo_path: Path, since: str, author: str | None = None) -> List[str]:
    cmd = ["git", "-C", str(repo_path), "log", "--since", since, "--date=short", "--format=%ad"]
    if author:
        cmd.extend(["--author", author])
    proc = subprocess.run(cmd, capture_output=True, text=True)
    if proc.returncode != 0:
        raise RuntimeError(proc.stderr.strip() or f"git log failed for {repo_path}")
    lines = [line.strip() for line in proc.stdout.splitlines() if line.strip()]
    return lines


def summarise_repo(meta: Dict[str, str], dates: List[str]) -> Dict:
    counts = Counter(dates)
    if counts:
        sorted_days = dict(sorted(counts.items()))
        first = min(sorted_days)
        last = max(sorted_days)
    else:
        sorted_days = {}
        first = last = None
    return {
        "label": meta.get("label", meta["path"].name),
        "path": str(meta["path"]),
        "sphere": meta.get("sphere"),
        "total_commits": sum(counts.values()),
        "days_active": len(counts),
        "first_day": first,
        "last_day": last,
        "by_day": sorted_days,
    }


def aggregate_counts(per_repo: List[Dict]) -> Dict[str, Dict[str, int]]:
    aggregate = Counter()
    by_sphere = defaultdict(int)
    for repo in per_repo:
        for day, count in repo["by_day"].items():
            aggregate[day] += count
        if repo["sphere"]:
            by_sphere[repo["sphere"]] += repo["total_commits"]
    return {
        "by_day": dict(sorted(aggregate.items())),
        "by_sphere": dict(sorted(by_sphere.items())),
        "total_commits": sum(aggregate.values()),
    }


def main() -> None:
    args = parse_args()
    repos = load_config(args.config)
    today = dt.date.today()
    since_date = (today - dt.timedelta(days=args.days - 1)).isoformat()
    per_repo = []
    for repo in repos:
        try:
            dates = git_dates(repo["path"], since_date, args.author)
            per_repo.append(summarise_repo(repo, dates))
        except Exception as exc:  # pylint: disable=broad-except
            if not args.quiet:
                print(f"[git-activity] skipped {repo.get('label')} ({exc})", file=sys.stderr)
    summary = {
        "generated_at": dt.datetime.now().isoformat(),
        "window": {
            "days": args.days,
            "since": since_date,
            "through": today.isoformat(),
            "author": args.author,
        },
        "repos": per_repo,
        "aggregate": aggregate_counts(per_repo),
    }
    if args.output:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(json.dumps(summary, indent=2))
    else:
        json.dump(summary, sys.stdout, indent=2)
        sys.stdout.write("\n")


if __name__ == "__main__":
    main()
