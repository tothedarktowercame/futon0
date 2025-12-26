#!/usr/bin/env python3
"""Sync Zoom R4 WAV files into the FUTON media tree with optional MP3 + title metadata."""

from __future__ import annotations

import argparse
import datetime as dt
import errno
import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
import time
from collections import defaultdict
from contextlib import redirect_stdout, redirect_stderr
from pathlib import Path
from typing import Dict, Iterable, List, Tuple

DEFAULT_SOURCE = Path("/media") / os.environ.get("USER", "") / "ZOOMR4"
DEFAULT_DEST = Path(os.path.expanduser(os.environ.get("ZOOM_SYNC_DEST") or str(Path.home() / "code" / "storage" / "zoomr4")))
DEFAULT_META_DIR = Path(os.path.expanduser(os.environ.get("ZOOM_SYNC_META_DIR") or str(DEFAULT_DEST / "meta")))


def env_path(name: str, fallback: Path) -> Path:
    value = os.environ.get(name)
    if value:
        return Path(os.path.expanduser(value))
    return fallback


DEFAULT_LOG = env_path("ZOOM_SYNC_INDEX", DEFAULT_META_DIR / "zoom_sync_index.json")
DEFAULT_RUN_LOG = env_path("ZOOM_SYNC_RUN_LOG", DEFAULT_META_DIR / "zoom_sync.log")
DEFAULT_TITLES = env_path("ZOOM_SYNC_TITLES", DEFAULT_META_DIR / "zoom_titles.json")
DEFAULT_TITLE_FILE = "TITLES.TXT"
CATALOG_SCHEMA_VERSION = 1
PROJECT_FOLDER_RE = re.compile(r"\d{8}_\d{3}")
DEFAULT_STATUS = "hold"
VALID_STATUSES = {"hold", "archive", "trash"}
STATUS_ALIASES = {
    "keep": "hold",
    "stay": "hold",
    "hold": "hold",
    "archive": "archive",
    "skip": "archive",
    "trash": "trash",
    "delete": "trash",
    "drop": "trash",
}
USER_FIELDS = {"title", "status", "artist", "album", "notes"}


def normalize_status(value: str | None) -> str | None:
    if not value:
        return None
    slug = value.strip().lower()
    if slug in VALID_STATUSES:
        return slug
    return STATUS_ALIASES.get(slug)


def notify(title: str, body: str) -> None:
    exe = shutil.which("notify-send")
    if exe:
        subprocess.run([exe, title, body], check=False)
def log(msg: str) -> None:
    print(f"[zoom-sync] {msg}")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Copy WAV files from a Zoom R4 and convert to MP3")
    parser.add_argument("--source", type=Path, default=DEFAULT_SOURCE,
                        help="Mount point for the Zoom recorder (default: %(default)s)")
    parser.add_argument("--dest", type=Path, default=DEFAULT_DEST,
                        help="Destination root for archived audio (default: %(default)s)")
    parser.add_argument("--log", type=Path, default=DEFAULT_LOG,
                        help="JSON index recording ingested files (default: %(default)s, env: ZOOM_SYNC_INDEX)")
    parser.add_argument("--run-log", type=Path, default=DEFAULT_RUN_LOG,
                        help="Append run output to this log file (default: %(default)s, env: ZOOM_SYNC_RUN_LOG)")
    parser.add_argument("--titles", type=Path, default=DEFAULT_TITLES,
                        help="Friendly title index (default: %(default)s, env: ZOOM_SYNC_TITLES)")
    parser.add_argument("--ffmpeg", default=shutil.which("ffmpeg") or "ffmpeg",
                        help="ffmpeg binary used for MP3 conversion")
    parser.add_argument("--ffprobe", default=shutil.which("ffprobe") or "ffprobe",
                        help="ffprobe binary used to gather duration/channel metadata")
    parser.add_argument("--no-convert", action="store_true",
                        help="Skip MP3 generation even if ffmpeg is present")
    parser.add_argument("--dry-run", action="store_true",
                        help="Report actions without copying or converting")
    parser.add_argument("--delete-from-recorder", action="store_true",
                        help="Delete recorder files whose catalog status is archive or trash")
    parser.add_argument("--rename-hold", action="store_true",
                        help="Rename recorder files for hold entries using the catalog title")
    parser.add_argument("--pattern", default="*.WAV",
                        help="Glob for files to ingest (default: %(default)s)")
    parser.add_argument("--title-file-name", default=DEFAULT_TITLE_FILE,
                        help="Recorder filename (e.g. TITLES.TXT) for synced titles")
    parser.add_argument("--no-title-file", action="store_true",
                        help="Skip writing the recorder title file")
    parser.add_argument("--title", action="append", default=[], metavar="SHA=Title",
                        help="Assign a friendly title and exit (may be repeated)")
    parser.add_argument("--status", action="append", default=[], metavar="SHA=STATUS",
                        help="Set the catalog status (hold/archive/trash) and exit; may be repeated")
    parser.add_argument("--tag", action="append", default=[], metavar="SHA=TAG",
                        help="Add TAG to the catalog entry (may be repeated)")
    parser.add_argument("--untag", action="append", default=[], metavar="SHA=TAG",
                        help="Remove TAG from the catalog entry (may be repeated)")
    parser.add_argument("--list", action="store_true",
                        help="List catalog entries sorted by recorded date and exit")
    parser.add_argument("--list-status", choices=sorted(VALID_STATUSES),
                        help="Filter --list output by status (requires --list)")
    parser.add_argument("--list-limit", type=int,
                        help="Optional limit for --list output")
    parser.add_argument("--list-missing-titles", action="store_true",
                        help="List recordings missing titles and exit")
    parser.add_argument("--refresh-titles", action="store_true",
                        help="Rewrite only the recorder title file from existing metadata")
    parser.add_argument("--title-manifest-status", default="all",
                        choices=["all", *sorted(VALID_STATUSES)],
                        help="Which catalog status to include in the recorder title file "
                             "(default: %(default)s)")
    parser.add_argument("--auto-title-missing", action="store_true",
                        help="Fill missing titles from base names/timestamps and exit")
    parser.add_argument("--scan-attempts", type=int, default=10,
                        help="How many times to retry scanning the recorder (default: %(default)s)")
    parser.add_argument("--scan-delay", type=float, default=1.0,
                        help="Seconds to wait between scan attempts (default: %(default)s)")
    return parser.parse_args()


def load_index(path: Path) -> Dict[str, Dict]:
    if not path.exists():
        return {}
    with path.open() as handle:
        data = json.load(handle)
    entries = data.get("entries", [])
    catalog: Dict[str, Dict] = {}
    for entry in entries:
        sha = entry.get("sha256")
        if not sha:
            continue
        normalized = dict(entry)
        normalize_catalog_entry(normalized)
        catalog[sha] = normalized
    return catalog


def save_index(path: Path, entries: Dict[str, Dict]) -> None:
    payload = {
        "schema_version": CATALOG_SCHEMA_VERSION,
        "updated_at": dt.datetime.now().isoformat(),
        "entries": sorted(entries.values(), key=entry_sort_key),
    }
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w") as handle:
        json.dump(payload, handle, indent=2)


def load_titles(path: Path) -> Dict[str, Dict]:
    if not path.exists():
        return {}
    with path.open() as handle:
        data = json.load(handle)
    entries = data.get("entries", [])
    return {entry["sha256"]: entry for entry in entries}


def save_titles(path: Path, entries: Dict[str, Dict]) -> None:
    payload = {
        "updated_at": dt.datetime.now().isoformat(),
        "entries": sorted(entries.values(), key=lambda e: e["sha256"]),
    }
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w") as handle:
        json.dump(payload, handle, indent=2)


def entry_sort_key(entry: Dict[str, Dict]) -> Tuple[str, str]:
    recorded = entry.get("recorded_at") or entry.get("recorded_date") or entry.get("ingested_at") or ""
    return recorded, entry.get("sha256") or ""


def derive_project_metadata(source: str | None) -> Tuple[str | None, str | None]:
    if not source:
        return None, None
    try:
        path = Path(source)
    except TypeError:
        return None, None
    folder: str | None = None
    project_path: str | None = None
    parts = path.parts
    for idx, part in enumerate(parts):
        if PROJECT_FOLDER_RE.fullmatch(part):
            folder = part
            parent = parts[idx - 1] if idx and parts[idx - 1].lower().startswith("r4_project") else None
            if parent:
                project_path = str(Path(parent) / part)
            else:
                project_path = part
            break
    return project_path, folder


def ensure_recorded_metadata(entry: Dict[str, Dict]) -> bool:
    base = entry.get("base_name")
    if not base:
        source = entry.get("source")
        if source:
            base = Path(source).stem
    if not base:
        return False
    match = re.match(r"(\d{4})(\d{2})(\d{2})_(\d{2})(\d{2})(\d{2})_(.+)", base)
    if not match:
        return False
    year, month, day, hour, minute, second, _tail = match.groups()
    try:
        recorded = dt.datetime(int(year), int(month), int(day), int(hour), int(minute), int(second))
    except ValueError:
        return False
    changed = False
    if not entry.get("recorded_at"):
        entry["recorded_at"] = recorded.isoformat()
        changed = True
    if not entry.get("recorded_date"):
        entry["recorded_date"] = f"{year}/{month}-{day}"
        changed = True
    return changed


def merge_tags(existing: Iterable[str] | None, extra: Iterable[str]) -> List[str]:
    tags = {tag for tag in (existing or []) if tag}
    tags.update(tag for tag in extra if tag)
    return sorted(tags)


def remove_tags(existing: Iterable[str] | None, to_remove: Iterable[str]) -> List[str]:
    remove = {tag for tag in to_remove if tag}
    return sorted(tag for tag in (existing or []) if tag and tag not in remove)


def parse_assignments(pairs: Iterable[str]) -> List[Tuple[str, str]]:
    assignments: List[Tuple[str, str]] = []
    for raw in pairs:
        if "=" not in raw:
            raise SystemExit(f"invalid assignment '{raw}', expected SHA=VALUE")
        sha, value = raw.split("=", 1)
        sha = sha.strip()
        value = value.strip()
        if not sha or not value:
            raise SystemExit(f"invalid assignment '{raw}', expected SHA=VALUE")
        assignments.append((sha, value))
    return assignments


def assign_tags(catalog: Dict[str, Dict], pairs: Iterable[str], remove: bool = False) -> List[str]:
    updated: List[str] = []
    assignments = parse_assignments(pairs)
    for sha, tag in assignments:
        entry = catalog.get(sha)
        if not entry:
            continue
        existing = entry.get("tags")
        if remove:
            merged = remove_tags(existing, [tag])
        else:
            merged = merge_tags(existing, [tag])
        if merged != existing:
            entry["tags"] = merged
            normalize_catalog_entry(entry)
            updated.append(sha)
    return updated


def normalize_catalog_entry(entry: Dict[str, Dict]) -> bool:
    changed = False
    desired_status = normalize_status(entry.get("status")) or DEFAULT_STATUS
    if entry.get("status") != desired_status:
        entry["status"] = desired_status
        changed = True

    tags = entry.get("tags")
    if tags is None:
        entry["tags"] = []
        changed = True
    elif not isinstance(tags, list):
        entry["tags"] = list(tags)
        changed = True

    if ensure_recorded_metadata(entry):
        changed = True

    project_path, project_folder = derive_project_metadata(entry.get("source"))
    if project_path and entry.get("project_path") != project_path:
        entry["project_path"] = project_path
        changed = True
    if project_folder and entry.get("project_folder") != project_folder:
        entry["project_folder"] = project_folder
        changed = True
    if project_folder and entry.get("recorder_project") != project_folder:
        entry["recorder_project"] = project_folder
        changed = True

    source = entry.get("source")
    if source:
        recorder_file = Path(source).name
        if entry.get("recorder_file") != recorder_file:
            entry["recorder_file"] = recorder_file
            changed = True

    base = (entry.get("base_name") or "").upper()
    recorder_file_name = (entry.get("recorder_file") or "").upper()
    derived_tags: List[str] = []
    if project_folder:
        derived_tags.append(f"project:{project_folder}")
    if "BOUNCE" in recorder_file_name or "BOUNCE" in base:
        derived_tags.append("bounce")
    elif "TRACK" in recorder_file_name or "TRACK" in base:
        derived_tags.append("track")
    merged = merge_tags(entry.get("tags"), derived_tags)
    if merged != entry.get("tags"):
        entry["tags"] = merged
        changed = True

    return changed


def merge_legacy_titles(catalog: Dict[str, Dict], titles_path: Path) -> bool:
    if not titles_path.exists():
        return False
    legacy = load_titles(titles_path)
    changed = False
    for sha, legacy_entry in legacy.items():
        entry = catalog.setdefault(sha, {"sha256": sha})
        if legacy_entry.get("title") and entry.get("title") != legacy_entry["title"]:
            entry["title"] = legacy_entry["title"]
            changed = True
        for key in ("base_name", "source"):
            if legacy_entry.get(key) and not entry.get(key):
                entry[key] = legacy_entry[key]
                changed = True
        normalize_catalog_entry(entry)
    return changed


def export_titles_view(path: Path, catalog: Dict[str, Dict]) -> None:
    snapshot: Dict[str, Dict] = {}
    for sha, entry in catalog.items():
        payload = {"sha256": sha}
        for key in ("base_name", "source", "title"):
            value = entry.get(key)
            if value:
                payload[key] = value
        snapshot[sha] = payload
    save_titles(path, snapshot)


def persist_catalog(args: argparse.Namespace, catalog: Dict[str, Dict]) -> None:
    save_index(args.log, catalog)
    export_titles_view(args.titles, catalog)


def assign_titles(catalog: Dict[str, Dict], assignments: List[str]) -> List[str]:
    updated = []
    for item in assignments:
        if "=" not in item:
            raise SystemExit(f"Invalid --title value '{item}'. Use SHA=Title format.")
        sha, title = item.split("=", 1)
        sha = sha.strip()
        title = title.strip()
        if not sha:
            raise SystemExit("Missing SHA in --title assignment.")
        entry = catalog.get(sha)
        if not entry:
            entry = {"sha256": sha}
            catalog[sha] = entry
        entry["title"] = title
        normalize_catalog_entry(entry)
        updated.append(sha)
    return updated


def assign_statuses(catalog: Dict[str, Dict], assignments: List[str]) -> List[str]:
    updated = []
    for item in assignments:
        if "=" not in item:
            raise SystemExit(f"Invalid --status value '{item}'. Use SHA=Status format.")
        sha, status = item.split("=", 1)
        sha = sha.strip()
        status = status.strip()
        if not sha:
            raise SystemExit("Missing SHA in --status assignment.")
        normalized = normalize_status(status)
        if not normalized:
            raise SystemExit(f"Unknown status '{status}'. Use one of: {', '.join(sorted(VALID_STATUSES))}.")
        entry = catalog.get(sha)
        if not entry:
            entry = {"sha256": sha}
            catalog[sha] = entry
        if entry.get("status") != normalized:
            entry["status"] = normalized
            updated.append(sha)
        normalize_catalog_entry(entry)
    return updated


def missing_titles(catalog: Dict[str, Dict]) -> Dict[str, Dict]:
    return {sha: entry for sha, entry in catalog.items() if not entry.get("title")}


def slugify(name: str) -> str:
    slug = re.sub(r"[^a-z0-9]+", "-", name.lower()).strip("-")
    return slug or "track"


def friendly_title(entry: Dict[str, str]) -> str | None:
    base = entry.get("base_name") or Path(entry.get("source", "")).stem
    if not base:
        return None
    match = re.match(r"(\d{4})(\d{2})(\d{2})_(\d{2})(\d{2})(\d{2})_(.+)", base)
    description = entry.get("title")
    if match:
        year, month, day, hour, minute, _second, tail = match.groups()
        slug = tail.replace("-", " ").replace("_", " ").strip()
        timestamp = f"{year}-{month}-{day} {hour}:{minute}"
        description = f"{timestamp} — {slug or 'untitled'}"
    else:
        description = base.replace("_", " ").strip()
    return description


def compute_sha(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1 << 20), b""):
            digest.update(chunk)
    return digest.hexdigest()


def unique_path(path: Path) -> Path:
    candidate = path
    counter = 1
    while candidate.exists():
        candidate = path.with_name(f"{path.stem}-{counter}{path.suffix}")
        counter += 1
    return candidate


def safe_recorder_filename(title: str, suffix: str) -> str:
    name = title.strip()
    if not name:
        return ""
    name = name.replace("/", "-")
    if os.altsep:
        name = name.replace(os.altsep, "-")
    # Keep filenames FAT-safe: ASCII only, strip control chars and reserved symbols.
    name = name.replace("\u2014", "-").replace("\u2013", "-")
    name = name.encode("ascii", "ignore").decode("ascii")
    name = re.sub(r"[<>:\"/\\\\|?*]", "", name)
    name = re.sub(r"[\x00-\x1f]", "", name)
    name = re.sub(r"\s+", " ", name).strip(" .")
    return f"{name}{suffix}"


def copy_file(src: Path, dest: Path, dry_run: bool) -> Path:
    dest.parent.mkdir(parents=True, exist_ok=True)
    target = unique_path(dest)
    if dry_run:
        log(f"would copy {src} → {target}")
    else:
        log(f"copying {src} → {target}")
        shutil.copy2(src, target)
    return target


def convert_to_mp3(ffmpeg: str, wav_path: Path, mp3_path: Path, dry_run: bool) -> Path:
    mp3_path = unique_path(mp3_path)
    mp3_path.parent.mkdir(parents=True, exist_ok=True)
    cmd = [ffmpeg, "-y", "-i", str(wav_path), "-codec:a", "libmp3lame", "-qscale:a", "2", str(mp3_path)]
    if dry_run:
        log("would run: " + " ".join(cmd))
        return mp3_path
    proc = subprocess.run(cmd, capture_output=True, text=True)
    if proc.returncode != 0:
        raise RuntimeError(proc.stderr or "ffmpeg failed")
    log(f"converted {wav_path} → {mp3_path}")
    return mp3_path


def find_wavs(source: Path, pattern: str) -> List[Path]:
    if not source.exists():
        raise FileNotFoundError(errno.ENOENT, f"Source mount not found: {source}")
    return sorted(source.rglob(pattern))


def find_wavs_with_retry(source: Path, pattern: str, attempts: int, delay: float) -> List[Path]:
    """Scan the recorder, retrying briefly if the mount isn't ready yet."""
    last_error: OSError | None = None
    for attempt in range(1, attempts + 1):
        try:
            return find_wavs(source, pattern)
        except OSError as exc:
            if exc.errno not in {errno.EIO, errno.ESTALE, errno.ENODEV, errno.ENOENT}:
                raise
            last_error = exc
            if attempt < attempts:
                log(f"recorder not ready ({exc}); retrying in {delay:.1f}s [{attempt}/{attempts}]")
                time.sleep(delay)
    raise SystemExit(f"Failed to scan recorder at {source}: {last_error}")


def write_title_manifest(
    source: Path,
    catalog: Dict[str, Dict],
    file_name: str,
    dry_run: bool,
    status_filter: str = "all",
) -> None:
    if not source.exists():
        return
    lines = [
        "Zoom R4 Track Titles",
        f"Updated: {dt.datetime.now().isoformat()}",
        f"Status filter: {status_filter}",
        "",
    ]
    grouped: Dict[str, List[Tuple[str, str]]] = defaultdict(list)
    for entry in catalog.values():
        if status_filter != "all":
            status = entry.get("status") or DEFAULT_STATUS
            if status != status_filter:
                continue
        src = entry.get("source")
        if not src:
            continue
        project = entry.get("recorder_project")
        if not project:
            _project_path, project = derive_project_metadata(src)
        recorder_file = entry.get("recorder_file") or Path(src).name
        if not recorder_file:
            continue
        title = entry.get("title") or "UNTITLED"
        grouped[project or "(unknown project)"].append((recorder_file, title))
    if not grouped:
        lines.append("(no recordings indexed yet)")
    else:
        for project in sorted(grouped):
            lines.append(project)
            for recorder_file, title in sorted(grouped[project]):
                lines.append(f"  {recorder_file} :: {title}")
            lines.append("")
    target = source / file_name
    contents = "\n".join(lines) + "\n"
    if dry_run:
        log(f"would write {target}")
        return
    try:
        target.write_text(contents)
        log(f"wrote titles to {target}")
    except OSError as exc:
        print(f"[zoom-sync] failed to write {target}: {exc}", file=sys.stderr)


def fill_missing_titles(catalog: Dict[str, Dict]) -> int:
    updated = 0
    for entry in catalog.values():
        if entry.get("title"):
            continue
        title = friendly_title(entry)
        if title:
            entry["title"] = title
            normalize_catalog_entry(entry)
            updated += 1
    return updated


def probe_audio(ffprobe: str | None, wav_path: Path) -> Dict[str, float | int]:
    if not ffprobe:
        return {}
    cmd = [
        ffprobe,
        "-v",
        "error",
        "-select_streams",
        "a:0",
        "-show_entries",
        "format=duration:stream=channels,bit_rate",
        "-of",
        "json",
        str(wav_path),
    ]
    proc = subprocess.run(cmd, capture_output=True, text=True)
    if proc.returncode != 0:
        return {}
    result: Dict[str, float | int] = {}
    try:
        payload = json.loads(proc.stdout or "{}")
    except json.JSONDecodeError:
        return result
    duration = payload.get("format", {}).get("duration")
    if duration:
        try:
            result["duration"] = float(duration)
        except (TypeError, ValueError):
            pass
    streams = payload.get("streams") or []
    if streams:
        stream = streams[0]
        channels = stream.get("channels")
        if channels is not None:
            try:
                result["channel_count"] = int(channels)
            except (TypeError, ValueError):
                pass
        bitrate = stream.get("bit_rate") or payload.get("format", {}).get("bit_rate")
        if bitrate:
            try:
                result["bitrate"] = int(bitrate)
            except (TypeError, ValueError):
                try:
                    result["bitrate"] = int(float(bitrate))
                except (TypeError, ValueError):
                    pass
    return result


def truncate(text: str, width: int) -> str:
    if len(text) <= width:
        return text
    if width <= 3:
        return text[:width]
    return text[: width - 3] + "..."


def format_recorded(entry: Dict[str, Dict]) -> str:
    recorded = entry.get("recorded_at")
    if recorded:
        try:
            return dt.datetime.fromisoformat(recorded).strftime("%Y-%m-%d %H:%M")
        except ValueError:
            return recorded[:16]
    if entry.get("recorded_date"):
        return entry["recorded_date"]
    return "?"


def sorted_catalog_entries(catalog: Dict[str, Dict]) -> List[Dict[str, Dict]]:
    return sorted(catalog.values(), key=entry_sort_key)


def list_catalog(catalog: Dict[str, Dict], status_filter: str | None = None, limit: int | None = None) -> None:
    rows: List[Dict[str, Dict]] = []
    for entry in sorted_catalog_entries(catalog):
        if status_filter and entry.get("status") != status_filter:
            continue
        rows.append(entry)
    if not rows:
        if status_filter:
            log(f"no catalog entries with status '{status_filter}'")
        else:
            log("catalog is empty")
        return
    header = f"{'Recorded':16} {'Status':8} {'Project':14} {'Title':40} MP3"
    print(header)
    print("-" * len(header))
    if limit is None:
        selected = rows
    else:
        selected = rows[: max(limit, 0)]
    for entry in selected:
        recorded = format_recorded(entry)
        status = entry.get("status") or DEFAULT_STATUS
        project = entry.get("project_folder") or "-"
        title = entry.get("title") or entry.get("base_name") or entry.get("sha256", "")[:12]
        title_display = truncate(title, 40)
        project_display = truncate(project, 14)
        mp3_path = entry.get("mp3") or entry.get("copied_to") or ""
        print(f"{recorded:16} {status:8} {project_display:14} {title_display:40} {mp3_path}")


def ingest(args: argparse.Namespace, catalog: Dict[str, Dict]) -> None:
    path_cache = {}
    fingerprint_cache = {}
    for entry in catalog.values():
        src = entry.get("source")
        if src:
            path_cache[src] = entry
        project_folder = entry.get("project_folder") or entry.get("recorder_project")
        recorder_file = entry.get("recorder_file")
        source_size = entry.get("source_size")
        source_mtime = entry.get("source_mtime")
        if project_folder and recorder_file and source_size is not None and source_mtime is not None:
            fingerprint_cache[(project_folder, recorder_file, source_size, source_mtime)] = entry
    log(f"scanning {args.source} for {args.pattern}")
    files = find_wavs_with_retry(args.source, args.pattern, args.scan_attempts, args.scan_delay)
    if not files:
        log(f"no files matching {args.pattern} under {args.source}")
        notify("Zoom ingest", "No files found on the recorder")
        return
    candidates: List[Tuple[Path, os.stat_result]] = []
    for wav in files:
        stat = wav.stat()
        cache_entry = path_cache.get(str(wav))
        if not cache_entry:
            _project_path, project_folder = derive_project_metadata(str(wav))
            if project_folder:
                cache_entry = fingerprint_cache.get((project_folder, wav.name, stat.st_size, stat.st_mtime))
        if cache_entry and cache_entry.get("source_mtime") == stat.st_mtime and cache_entry.get("source_size") == stat.st_size:
            continue
        candidates.append((wav, stat))
    candidate_projects: Dict[str, List[Tuple[Path, os.stat_result]]] = defaultdict(list)
    for wav, stat in candidates:
        _project_path, project_folder = derive_project_metadata(str(wav))
        project_key = project_folder or "(unknown project)"
        candidate_projects[project_key].append((wav, stat))
    project_names = sorted(candidate_projects, key=lambda name: (name == "(unknown project)", name))
    existing_projects = {
        entry.get("project_folder")
        for entry in catalog.values()
        if entry.get("project_folder")
    }
    new_project_count = sum(
        1 for name in project_names if name != "(unknown project)" and name not in existing_projects
    )
    log(
        f"found {len(files)} candidate file(s); {len(candidates)} new/updated across {len(project_names)} project(s)"
    )
    notify(
        "Zoom ingest",
        f"Found {len(files)} files ({len(candidates)} new/updated) across {len(project_names)} projects ({new_project_count} new)",
    )
    ffmpeg_available = not args.no_convert and shutil.which(args.ffmpeg)
    if not ffmpeg_available and not args.no_convert:
        log(f"ffmpeg not found at {args.ffmpeg}; skipping conversion")
    ffprobe_binary: str | None = None
    if not args.dry_run:
        probe_candidate = shutil.which(args.ffprobe) if args.ffprobe else None
        if probe_candidate:
            ffprobe_binary = probe_candidate
        else:
            log(f"ffprobe not found at {args.ffprobe}; duration/channel metadata will be skipped")
    new_entries = 0
    updated_existing = 0
    had_candidates = bool(candidates)
    if not candidates:
        log("no new files to ingest")
        notify("Zoom ingest", "No new tracks detected")
    for idx, project in enumerate(project_names, start=1):
        project_files = sorted(candidate_projects[project], key=lambda item: item[0])
        new_project_files: List[Tuple[Path, os.stat_result, str]] = []
        for wav, stat in project_files:
            cache_entry = path_cache.get(str(wav))
            if cache_entry and cache_entry.get("source_mtime") == stat.st_mtime and cache_entry.get("source_size") == stat.st_size:
                sha = cache_entry["sha256"]
            else:
                sha = compute_sha(wav)
            existing_entry = catalog.get(sha)
            if existing_entry:
                updated = False
                if existing_entry.get("source_mtime") != stat.st_mtime:
                    existing_entry["source_mtime"] = stat.st_mtime
                    updated = True
                if existing_entry.get("source_size") != stat.st_size:
                    existing_entry["source_size"] = stat.st_size
                    updated = True
                if existing_entry.get("source") != str(wav):
                    existing_entry["source"] = str(wav)
                    updated = True
                if updated:
                    normalize_catalog_entry(existing_entry)
                    catalog[sha] = existing_entry
                    path_cache[str(wav)] = existing_entry
                    updated_existing += 1
                log(f"skip existing {wav} (already ingested)")
                continue
            new_project_files.append((wav, stat, sha))
        if not new_project_files:
            continue
        log(f"syncing project {idx}/{len(project_names)}: {project} ({len(new_project_files)} files)")
        notify("Zoom ingest", f"Syncing project {idx}/{len(project_names)}: {project}")
        for wav, stat, sha in new_project_files:
            ts = dt.datetime.fromtimestamp(stat.st_mtime)
            day = ts.strftime("%Y/%m-%d")
            slug = slugify(wav.stem)
            base_name = f"{ts.strftime('%Y%m%d_%H%M%S')}_{slug}"
            wav_dest = args.dest / "incoming" / day / f"{base_name}.wav"
            mp3_dest = args.dest / "mp3" / day / f"{base_name}.mp3"
            copied = copy_file(wav, wav_dest, args.dry_run)
            mp3_path = None
            if ffmpeg_available:
                try:
                    mp3_path = convert_to_mp3(args.ffmpeg, copied, mp3_dest, args.dry_run)
                except Exception as exc:  # pylint: disable=broad-except
                    print(f"[zoom-sync] mp3 conversion failed for {copied}: {exc}", file=sys.stderr)
            recorded_at = dt.datetime.fromtimestamp(stat.st_mtime)
            project_path, project_folder = derive_project_metadata(str(wav))
            derived_tags: List[str] = []
            if project_folder:
                derived_tags.append(f"project:{project_folder}")
            name_upper = wav.name.upper()
            if "BOUNCE" in name_upper:
                derived_tags.append("bounce")
            elif "TRACK" in name_upper:
                derived_tags.append("track")
            existing = catalog.get(sha, {})
            entry_data = dict(existing)
            entry_data["sha256"] = sha
            entry_data["source"] = str(wav)
            entry_data["copied_to"] = str(copied)
            if mp3_path:
                entry_data["mp3"] = str(mp3_path)
            entry_data["recorded_date"] = day
            entry_data["recorded_at"] = recorded_at.isoformat()
            entry_data["ingested_at"] = dt.datetime.now().isoformat()
            entry_data["base_name"] = base_name
            entry_data["source_mtime"] = stat.st_mtime
            entry_data["source_size"] = stat.st_size
            entry_data["recorder_file"] = wav.name
            if project_path:
                entry_data["project_path"] = project_path
            if project_folder:
                entry_data["project_folder"] = project_folder
                entry_data["recorder_project"] = project_folder
            if ffprobe_binary and not args.dry_run:
                metadata = probe_audio(ffprobe_binary, copied)
                entry_data.update(metadata)
            entry_data["tags"] = merge_tags(existing.get("tags"), derived_tags)
            if existing.get("title"):
                entry_data["title"] = existing["title"]
            if existing.get("status"):
                entry_data["status"] = existing["status"]
            normalize_catalog_entry(entry_data)
            catalog[sha] = entry_data
            path_cache[str(wav)] = entry_data
            new_entries += 1
    if new_entries:
        if args.dry_run:
            log(f"dry run complete ({new_entries} new files detected)")
            notify("Zoom ingest", f"Dry run: {new_entries} files would be copied")
        else:
            persist_catalog(args, catalog)
            log(f"ingested {new_entries} new file(s); index updated at {args.log}")
            notify("Zoom ingest", f"Copied {new_entries} new track(s)")
    elif updated_existing and not args.dry_run:
        persist_catalog(args, catalog)
        log(f"updated {updated_existing} catalog entry mtime/size fields")
    elif had_candidates:
        log("no new files detected")
        notify("Zoom ingest", "No new tracks detected")
    missing = missing_titles(catalog)
    if missing:
        log(f"{len(missing)} recording(s) missing titles. Use --title SHA=Name to label them.")
    if not args.no_title_file:
        write_title_manifest(args.source, catalog, args.title_file_name, args.dry_run, args.title_manifest_status)


def list_missing(catalog: Dict[str, Dict]) -> None:
    missing = missing_titles(catalog)
    if not missing:
        log("all recordings have titles")
        return
    log(f"{len(missing)} recording(s) missing titles:")
    for sha, entry in missing.items():
        base = entry.get("base_name") or sha
        print(f"  {sha} :: {base}")


def rename_hold_titles(args: argparse.Namespace, catalog: Dict[str, Dict]) -> int:
    if not args.source.exists():
        log(f"recorder not mounted at {args.source}; skipping renames")
        return 0
    source_root = args.source.resolve()
    updated = 0
    for entry in catalog.values():
        status = entry.get("status") or DEFAULT_STATUS
        if status != "hold":
            continue
        title = entry.get("title")
        if not title:
            continue
        src = entry.get("source")
        if not src:
            continue
        try:
            src_path = Path(src).resolve()
        except OSError:
            continue
        if source_root not in src_path.parents:
            continue
        if not src_path.is_file():
            continue
        suffix = src_path.suffix or ".WAV"
        new_name = safe_recorder_filename(title, suffix)
        if not new_name:
            continue
        if src_path.name == new_name:
            continue
        target = src_path.with_name(new_name)
        if target.exists() and target != src_path:
            target = unique_path(target)
        if args.dry_run:
            log(f"would rename {src_path.name} → {target.name}")
            continue
        try:
            src_path.rename(target)
            entry["source"] = str(target)
            entry["recorder_file"] = target.name
            normalize_catalog_entry(entry)
            updated += 1
            log(f"renamed {src_path.name} → {target.name}")
        except OSError as exc:
            print(f"[zoom-sync] failed to rename {src_path}: {exc}", file=sys.stderr)
    return updated


def delete_from_recorder(args: argparse.Namespace, catalog: Dict[str, Dict]) -> int:
    if not args.source.exists():
        log(f"recorder not mounted at {args.source}; skipping deletions")
        return 0
    source_root = args.source.resolve()
    targets: List[Path] = []
    for entry in catalog.values():
        status = entry.get("status") or DEFAULT_STATUS
        if status not in {"archive", "trash"}:
            continue
        src = entry.get("source")
        if not src:
            continue
        try:
            src_path = Path(src).resolve()
        except OSError:
            continue
        if source_root not in src_path.parents:
            continue
        if not src_path.is_file():
            continue
        targets.append(src_path)
    if not targets:
        log("no recorder files eligible for deletion")
        return 0
    deleted = 0
    for target in sorted(set(targets)):
        if args.dry_run:
            log(f"would delete {target}")
            continue
        try:
            target.unlink()
            log(f"deleted {target}")
            deleted += 1
        except OSError as exc:
            print(f"[zoom-sync] failed to delete {target}: {exc}", file=sys.stderr)
    return deleted


def cleanup_empty_projects(args: argparse.Namespace) -> int:
    if not args.source.exists():
        return 0
    projects_root = args.source / "R4_Project"
    if not projects_root.exists():
        return 0
    removed = 0
    for project_dir in sorted(projects_root.iterdir()):
        if not project_dir.is_dir():
            continue
        try:
            contents = list(project_dir.iterdir())
        except OSError:
            continue
        has_wav = any(item.is_file() and item.suffix.lower() == ".wav" for item in contents)
        if has_wav:
            continue
        if args.dry_run:
            log(f"would remove project with no wavs {project_dir}")
            continue
        for item in contents:
            if item.is_file():
                try:
                    item.unlink()
                except OSError as exc:
                    print(f"[zoom-sync] failed to remove {item}: {exc}", file=sys.stderr)
        try:
            project_dir.rmdir()
            removed += 1
            log(f"removed project with no wavs {project_dir}")
        except OSError as exc:
            print(f"[zoom-sync] failed to remove {project_dir}: {exc}", file=sys.stderr)
    return removed


def main() -> None:
    args = parse_args()
    catalog = load_index(args.log)
    if merge_legacy_titles(catalog, args.titles):
        persist_catalog(args, catalog)

    args.run_log.parent.mkdir(parents=True, exist_ok=True)
    with args.run_log.open("a", encoding="utf-8") as handle, redirect_stdout(handle), redirect_stderr(handle):
        log(f"run started at {dt.datetime.now().isoformat()}")
        if args.title:
            updated = assign_titles(catalog, args.title)
            if updated:
                persist_catalog(args, catalog)
                log(f"updated titles for {len(updated)} recording(s)")
                if not args.no_title_file:
                    write_title_manifest(args.source, catalog, args.title_file_name, args.dry_run, args.title_manifest_status)
            else:
                log("no catalog entries matched the provided titles")
            return

        if args.status:
            updated = assign_statuses(catalog, args.status)
            if updated:
                persist_catalog(args, catalog)
                log(f"updated status for {len(updated)} recording(s)")
            else:
                log("no catalog entries matched the provided status assignments")
            return

        if args.tag:
            updated = assign_tags(catalog, args.tag, remove=False)
            if updated:
                persist_catalog(args, catalog)
                log(f"tagged {len(updated)} recording(s)")
            else:
                log("no catalog entries matched the provided tags")
            return

        if args.untag:
            updated = assign_tags(catalog, args.untag, remove=True)
            if updated:
                persist_catalog(args, catalog)
                log(f"untagged {len(updated)} recording(s)")
            else:
                log("no catalog entries matched the provided tags")
            return

        if args.list or args.list_status or args.list_limit:
            list_catalog(catalog, args.list_status, args.list_limit)
            return

        if args.list_missing_titles:
            list_missing(catalog)
            return

        if args.refresh_titles:
            if not args.no_title_file:
                write_title_manifest(args.source, catalog, args.title_file_name, args.dry_run, args.title_manifest_status)
            else:
                log("--refresh-titles requested but --no-title-file also set; nothing to do")
            return

        if args.auto_title_missing:
            filled = fill_missing_titles(catalog)
            if filled:
                persist_catalog(args, catalog)
                log(f"auto-filled titles for {filled} recording(s)")
                if not args.no_title_file:
                    write_title_manifest(args.source, catalog, args.title_file_name, args.dry_run, args.title_manifest_status)
            else:
                log("no missing titles detected; nothing to auto-fill")
            return

        ingest(args, catalog)
        renamed = 0
        if args.rename_hold:
            renamed = rename_hold_titles(args, catalog)
            if renamed:
                persist_catalog(args, catalog)
                if not args.no_title_file:
                    write_title_manifest(args.source, catalog, args.title_file_name, args.dry_run, args.title_manifest_status)
            summary = f"{'Dry run: ' if args.dry_run else ''}renamed {renamed} hold track(s)"
            notify("Zoom rename", summary)
        if args.delete_from_recorder:
            deleted = delete_from_recorder(args, catalog)
            summary = f"{'Dry run: ' if args.dry_run else ''}deleted {deleted} track(s)"
            notify("Zoom delete", summary)
            removed_projects = cleanup_empty_projects(args)
            summary = f"{'Dry run: ' if args.dry_run else ''}removed {removed_projects} empty project(s)"
            notify("Zoom cleanup", summary)


if __name__ == "__main__":
    main()
