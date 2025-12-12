#!/usr/bin/env python3
"""Emit a lightweight vitality snapshot for cron/systemd ingestion."""

from __future__ import annotations

import argparse
import datetime as dt
import json
import os
import sys
from collections import Counter, deque
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Sequence

REPO_ROOT = Path(__file__).resolve().parent.parent
DEFAULT_CONFIG = REPO_ROOT / "data" / "vitality_scanner.json"
DEFAULT_OUTPUT = REPO_ROOT / "data" / "vitality" / "latest_scan.json"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Generate a FUTON0 vitality snapshot")
    parser.add_argument("--config", type=Path, default=DEFAULT_CONFIG,
                        help="Path to vitality_scanner.json (default: %(default)s)")
    parser.add_argument("--output", type=Path,
                        help="Optional override for the JSON output path")
    parser.add_argument("--quiet", action="store_true", help="Suppress stdout dump")
    return parser.parse_args()


def load_config(path: Path) -> Dict:
    if path.exists():
        with path.open() as handle:
            return json.load(handle)
    # Fall back to a minimal inline config when file is missing.
    return {
        "lookback_hours": 24,
        "filesystem": [],
        "tatami": None,
        "output": str(DEFAULT_OUTPUT),
    }


def expand_path(raw: str) -> Path:
    return Path(os.path.expandvars(os.path.expanduser(raw)))


def summarize_imports(entry: Dict) -> Optional[Dict[str, object]]:
    index_path = entry.get("import_index")
    if not index_path:
        return None
    path = expand_path(index_path)
    if not path.exists():
        return None
    try:
        with path.open() as handle:
            payload = json.load(handle)
    except (OSError, json.JSONDecodeError):
        return None
    entries: Sequence[Dict[str, str]] = payload.get("entries") or []
    total = len(entries)
    if total == 0:
        return None
    limit = int(entry.get("import_limit", 5))
    sorted_entries = sorted(
        entries,
        key=lambda item: item.get("ingested_at") or item.get("recorded_date") or "",
        reverse=True,
    )
    recent: List[Dict[str, str]] = []
    for item in sorted_entries[:limit]:
        display = item.get("title") or item.get("base_name") or os.path.basename(
            item.get("copied_to") or item.get("source") or ""
        )
        info = {
            "title": display,
            "recorded_date": item.get("recorded_date"),
            "ingested_at": item.get("ingested_at"),
            "mp3": item.get("mp3"),
            "copied_to": item.get("copied_to"),
        }
        recent.append({k: v for k, v in info.items() if v})
    return {"total": total, "recent": recent}


def scan_filesystem(entry: Dict, now: dt.datetime, default_lookback: int) -> Dict:
    label = entry.get("label") or entry.get("path")
    root = expand_path(entry["path"])
    lookback_hours = entry.get("lookback_hours", default_lookback)
    cutoff = now - dt.timedelta(hours=lookback_hours)
    summary: Dict[str, object] = {
        "label": label,
        "path": str(root),
        "exists": root.exists(),
        "lookback_hours": lookback_hours,
    }
    imports = summarize_imports(entry)
    if imports:
        summary["imports"] = imports
    if not root.exists():
        return summary
    start = dt.datetime.now()
    max_depth = int(entry.get("max_depth", 2))
    recent_files = 0
    latest_ts: Optional[float] = None
    children = Counter()
    queue: deque[tuple[Path, int]] = deque([(root, 0)])
    while queue:
        current, depth = queue.popleft()
        try:
            for child in current.iterdir():
                if child.is_symlink():
                    continue
                if child.is_dir():
                    if depth < max_depth:
                        queue.append((child, depth + 1))
                    continue
                try:
                    mtime = child.stat().st_mtime
                except OSError:
                    continue
                if mtime >= cutoff.timestamp():
                    recent_files += 1
                    latest_ts = mtime if latest_ts is None else max(latest_ts, mtime)
                    rel = child.relative_to(root)
                    head = rel.parts[0] if rel.parts else child.name
                    children[head] += 1
        except OSError:
            continue
    summary["recent_files"] = recent_files
    summary["latest_mtime"] = (
        dt.datetime.fromtimestamp(latest_ts).isoformat() if latest_ts else None
    )
    summary["top_children"] = [
        {"name": name, "recent_files": count}
        for name, count in children.most_common(entry.get("top_n", 5))
    ]
    summary["scan_duration_seconds"] = round((dt.datetime.now() - start).total_seconds(), 3)
    return summary


def parse_timestamp(raw: str) -> Optional[dt.datetime]:
    text = raw.strip()
    if not text:
        return None
    if text.endswith("Z"):
        text = text[:-1] + "+00:00"
    try:
        parsed = dt.datetime.fromisoformat(text)
        if parsed.tzinfo is None:
            parsed = parsed.replace(tzinfo=dt.timezone.utc)
        return parsed
    except ValueError:
        return None


def scan_tatami(entry: Dict, now: dt.datetime, default_lookback: int) -> Dict:
    log_path = expand_path(entry.get("log_path", ""))
    summary: Dict[str, object] = {
        "log_path": str(log_path),
        "exists": log_path.exists(),
        "lookback_hours": entry.get("lookback_hours", default_lookback),
    }
    if not log_path.exists():
        return summary
    timestamp_field = entry.get("timestamp_field", "timestamp")
    fmt = entry.get("format", "auto")
    events: List[dt.datetime] = []
    try:
        with log_path.open() as handle:
            for raw_line in handle:
                line = raw_line.strip()
                if not line:
                    continue
                timestamp: Optional[str] = None
                if fmt == "jsonl" or (fmt == "auto" and line.startswith("{")):
                    try:
                        payload = json.loads(line)
                        value = payload.get(timestamp_field)
                        if isinstance(value, str):
                            timestamp = value
                    except json.JSONDecodeError:
                        timestamp = line
                else:
                    timestamp = line
                if not timestamp:
                    continue
                parsed = parse_timestamp(timestamp)
                if parsed:
                    events.append(parsed)
    except OSError as exc:
        summary["error"] = str(exc)
        return summary
    events.sort()
    summary["event_count"] = len(events)
    lookback_delta = dt.timedelta(hours=summary["lookback_hours"])
    threshold = now - lookback_delta
    summary["events_in_lookback"] = sum(1 for event in events if event >= threshold)
    if events:
        last_event = events[-1]
        summary["last_event"] = last_event.isoformat()
        summary["hours_since_last"] = round((now - last_event).total_seconds() / 3600, 2)
        gap_limit = entry.get("gap_warning_hours")
        if gap_limit is not None:
            summary["gap_warning"] = (now - last_event) > dt.timedelta(hours=gap_limit)
    return summary


def main() -> None:
    args = parse_args()
    config = load_config(args.config)
    now = dt.datetime.now(dt.timezone.utc)
    lookback_hours = int(config.get("lookback_hours", 24))
    fs_entries = config.get("filesystem") or []
    filesystem = [scan_filesystem(entry, now, lookback_hours) for entry in fs_entries]
    tatami_cfg = config.get("tatami") or None
    tatami = scan_tatami(tatami_cfg, now, lookback_hours) if tatami_cfg else None
    summary = {
        "generated_at": now.isoformat(),
        "lookback_hours": lookback_hours,
        "filesystem": filesystem,
        "tatami": tatami,
    }
    output_path = args.output or config.get("output") or DEFAULT_OUTPUT
    if output_path:
        output = expand_path(str(output_path)) if isinstance(output_path, str) else output_path
        output.parent.mkdir(parents=True, exist_ok=True)
        output.write_text(json.dumps(summary, indent=2))
    if not args.quiet:
        json.dump(summary, sys.stdout, indent=2)
        sys.stdout.write("\n")


if __name__ == "__main__":
    main()
