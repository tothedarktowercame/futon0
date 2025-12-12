#!/usr/bin/env python3
"""Sync Zoom R4 WAV files into the FUTON media tree with optional MP3 + title metadata."""

from __future__ import annotations

import argparse
import datetime as dt
import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
from pathlib import Path
from typing import Dict, List

DEFAULT_SOURCE = Path("/media") / os.environ.get("USER", "") / "ZOOMR4"
DEFAULT_DEST = Path.home() / "media" / "zoomr4"
DEFAULT_LOG = Path(__file__).resolve().parent.parent / "data" / "zoom_sync_index.json"
DEFAULT_TITLES = Path(__file__).resolve().parent.parent / "data" / "zoom_titles.json"
DEFAULT_TITLE_FILE = "TITLES.TXT"




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
                        help="JSON index recording ingested files (default: %(default)s)")
    parser.add_argument("--titles", type=Path, default=DEFAULT_TITLES,
                        help="Friendly title index (default: %(default)s)")
    parser.add_argument("--ffmpeg", default=shutil.which("ffmpeg") or "ffmpeg",
                        help="ffmpeg binary used for MP3 conversion")
    parser.add_argument("--no-convert", action="store_true",
                        help="Skip MP3 generation even if ffmpeg is present")
    parser.add_argument("--dry-run", action="store_true",
                        help="Report actions without copying or converting")
    parser.add_argument("--pattern", default="*.WAV",
                        help="Glob for files to ingest (default: %(default)s)")
    parser.add_argument("--title-file-name", default=DEFAULT_TITLE_FILE,
                        help="Recorder filename (e.g. TITLES.TXT) for synced titles")
    parser.add_argument("--no-title-file", action="store_true",
                        help="Skip writing the recorder title file")
    parser.add_argument("--title", action="append", default=[], metavar="SHA=Title",
                        help="Assign a friendly title and exit (may be repeated)")
    parser.add_argument("--list-missing-titles", action="store_true",
                        help="List recordings missing titles and exit")
    parser.add_argument("--refresh-titles", action="store_true",
                        help="Rewrite only the recorder title file from existing metadata")
    parser.add_argument("--auto-title-missing", action="store_true",
                        help="Fill missing titles from base names/timestamps and exit")
    return parser.parse_args()


def load_index(path: Path) -> Dict[str, Dict]:
    if not path.exists():
        return {}
    with path.open() as handle:
        data = json.load(handle)
    entries = data.get("entries", [])
    return {entry["sha256"]: entry for entry in entries}


def save_index(path: Path, entries: Dict[str, Dict]) -> None:
    payload = {
        "updated_at": dt.datetime.now().isoformat(),
        "entries": sorted(entries.values(), key=lambda e: e["ingested_at"]),
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


def assign_titles(titles: Dict[str, Dict], assignments: List[str]) -> List[str]:
    updated = []
    for item in assignments:
        if "=" not in item:
            raise SystemExit(f"Invalid --title value '{item}'. Use SHA=Title format.")
        sha, title = item.split("=", 1)
        sha = sha.strip()
        title = title.strip()
        if not sha:
            raise SystemExit("Missing SHA in --title assignment.")
        entry = titles.get(sha)
        if not entry:
            entry = {"sha256": sha}
            titles[sha] = entry
        entry["title"] = title
        updated.append(sha)
    return updated


def missing_titles(titles: Dict[str, Dict]) -> Dict[str, Dict]:
    return {sha: entry for sha, entry in titles.items() if not entry.get("title")}


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
        raise SystemExit(f"Source mount not found: {source}")
    return sorted(source.rglob(pattern))


def write_title_manifest(source: Path, titles: Dict[str, Dict], file_name: str, dry_run: bool) -> None:
    if not source.exists():
        return
    lines = ["Zoom R4 Track Titles", f"Updated: {dt.datetime.now().isoformat()}", ""]
    entries = sorted(titles.values(), key=lambda e: e.get("base_name") or e.get("sha256"))
    if not entries:
        lines.append("(no recordings indexed yet)")
    else:
        for entry in entries:
            base = entry.get("base_name") or entry["sha256"]
            title = entry.get("title") or "UNTITLED"
            lines.append(f"{base} :: {title}")
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


def fill_missing_titles(titles: Dict[str, Dict]) -> int:
    updated = 0
    for entry in titles.values():
        if entry.get("title"):
            continue
        title = friendly_title(entry)
        if title:
            entry["title"] = title
            updated += 1
    return updated


def sync_index_titles(log_path: Path, titles: Dict[str, Dict]) -> int:
    if not log_path.exists():
        return 0
    known = load_index(log_path)
    updated = 0
    for sha, entry in known.items():
        title = titles.get(sha, {}).get("title")
        if title and entry.get("title") != title:
            entry["title"] = title
            updated += 1
    if updated:
        save_index(log_path, known)
    return updated


def ingest(args: argparse.Namespace, titles: Dict[str, Dict]) -> None:
    known = load_index(args.log)
    files = find_wavs(args.source, args.pattern)
    if not files:
        log(f"no files matching {args.pattern} under {args.source}")
        notify("Zoom ingest", "No files found on the recorder")
        return
    log(f"found {len(files)} candidate file(s) to scan")
    notify("Zoom ingest", f"Scanning {len(files)} files on the recorder")
    ffmpeg_available = not args.no_convert and shutil.which(args.ffmpeg)
    if not ffmpeg_available and not args.no_convert:
        log(f"ffmpeg not found at {args.ffmpeg}; skipping conversion")
    new_entries = 0
    for wav in files:
        sha = compute_sha(wav)
        if sha in known:
            log(f"skip existing {wav} (already ingested)")
            continue
        ts = dt.datetime.fromtimestamp(wav.stat().st_mtime)
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
        known[sha] = {
            "sha256": sha,
            "source": str(wav),
            "copied_to": str(copied),
            "mp3": str(mp3_path) if mp3_path else None,
            "recorded_date": day,
            "ingested_at": dt.datetime.now().isoformat(),
            "base_name": base_name,
            "title": titles.get(sha, {}).get("title"),
        }
        entry = titles.get(sha) or {"sha256": sha}
        entry.setdefault("base_name", base_name)
        entry.setdefault("source", str(wav))
        titles[sha] = entry
        new_entries += 1
    if new_entries:
        if args.dry_run:
            log(f"dry run complete ({new_entries} new files detected)")
            notify("Zoom ingest", f"Dry run: {new_entries} files would be copied")
        else:
            save_index(args.log, known)
            save_titles(args.titles, titles)
            log(f"ingested {new_entries} new file(s); index updated at {args.log}")
            notify("Zoom ingest", f"Copied {new_entries} new track(s)")
    else:
        log("no new files detected")
        notify("Zoom ingest", "No new tracks detected")
    missing = missing_titles(titles)
    if missing:
        log(f"{len(missing)} recording(s) missing titles. Use --title SHA=Name to label them.")
    if not args.no_title_file:
        write_title_manifest(args.source, titles, args.title_file_name, args.dry_run)


def list_missing(titles: Dict[str, Dict]) -> None:
    missing = missing_titles(titles)
    if not missing:
        log("all recordings have titles")
        return
    log(f"{len(missing)} recording(s) missing titles:")
    for sha, entry in missing.items():
        base = entry.get("base_name") or sha
        print(f"  {sha} :: {base}")


def main() -> None:
    args = parse_args()
    titles = load_titles(args.titles)

    if args.title:
        updated = assign_titles(titles, args.title)
        save_titles(args.titles, titles)
        log(f"updated titles for {len(updated)} recording(s)")
        if not args.no_title_file:
            write_title_manifest(args.source, titles, args.title_file_name, args.dry_run)
        sync_count = sync_index_titles(args.log, titles)
        if sync_count:
            log(f"refreshed {sync_count} index entr(ies) with new titles")
        return

    if args.list_missing_titles:
        list_missing(titles)
        return

    if args.refresh_titles:
        if not args.no_title_file:
            write_title_manifest(args.source, titles, args.title_file_name, args.dry_run)
        else:
            log("--refresh-titles requested but --no-title-file also set; nothing to do")
        return

    if args.auto_title_missing:
        filled = fill_missing_titles(titles)
        if filled:
            save_titles(args.titles, titles)
            log(f"auto-filled titles for {filled} recording(s)")
            sync_count = sync_index_titles(args.log, titles)
            if sync_count:
                log(f"refreshed {sync_count} index entr(ies) with new titles")
            if not args.no_title_file:
                write_title_manifest(args.source, titles, args.title_file_name, args.dry_run)
        else:
            log("no missing titles detected; nothing to auto-fill")
        return

    ingest(args, titles)


if __name__ == "__main__":
    main()
