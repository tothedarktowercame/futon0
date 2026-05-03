#!/usr/bin/env python3
"""
End-to-end pipeline: transcript -> commentary -> TTS -> stitched podcast mp3.
"""

from __future__ import annotations

import argparse
import os
import shlex
import subprocess
import tempfile
import time
from datetime import datetime
from pathlib import Path

from audio_pipeline_lib import collect_segment_files, run_checked, stitch_segments, update_latest_symlink


DEFAULT_MODEL = os.environ.get("OPENAI_MODEL", "gpt-5.2-chat-latest")
DEFAULT_WORDS_RATIO = float(os.environ.get("WORDS_RATIO", "1.0"))
DEFAULT_MIN_WORDS = int(os.environ.get("MIN_WORDS", "800"))
DEFAULT_MAX_WORDS = int(os.environ.get("MAX_WORDS", "8000"))
DEFAULT_SEGMENTS = int(os.environ.get("COMMENTARY_SEGMENTS", "1"))
DEFAULT_PAUSE = float(os.environ.get("PODCAST_PAUSE_SECONDS", "1.0"))

DEFAULT_PIPER_MODEL = os.environ.get("PIPER_MODEL", "")
DEFAULT_PIPER_CONFIG = os.environ.get("PIPER_CONFIG", "")
DEFAULT_PIPER_BIN = os.environ.get("PIPER_BIN", "")

def resolve_output_dir(transcript_path: Path, out_dir: str | None) -> Path:
    return Path(out_dir).expanduser().resolve() if out_dir else transcript_path.parent / "commentary"


def default_output_mp3(audio_dir: Path) -> Path:
    stamp = datetime.now().strftime("%Y-%m-%d")
    return audio_dir / f"commentary_{stamp}_full.mp3"

def parse_cmd(cmd: str) -> list[str]:
    return shlex.split(cmd)


def sweeten_with_easyeffects(
    input_mp3: Path,
    output_mp3: Path,
    preset_name: str,
    easyeffects_cmd: str,
    pw_play_cmd: str,
    pw_record_cmd: str,
    sink_name: str,
    monitor_name: str,
) -> None:
    service_proc = subprocess.Popen([*parse_cmd(easyeffects_cmd), "--service-mode"])
    tmp_wav_path: Path | None = None
    try:
        time.sleep(0.5)
        run_checked([*parse_cmd(easyeffects_cmd), "-l", preset_name])
        with tempfile.NamedTemporaryFile(suffix=".wav", delete=False) as tmp_wav:
            tmp_wav_path = Path(tmp_wav.name)
        record_proc = subprocess.Popen([*parse_cmd(pw_record_cmd), "--target", monitor_name, str(tmp_wav_path)])
        try:
            run_checked([*parse_cmd(pw_play_cmd), "--target", sink_name, str(input_mp3)])
        finally:
            record_proc.terminate()
            try:
                record_proc.wait(timeout=5)
            except subprocess.TimeoutExpired:
                record_proc.kill()
        run_checked(
            [
                "ffmpeg",
                "-y",
                "-i",
                str(tmp_wav_path),
                "-codec:a",
                "libmp3lame",
                "-q:a",
                "2",
                str(output_mp3),
            ]
        )
    finally:
        if tmp_wav_path and tmp_wav_path.exists():
            try:
                tmp_wav_path.unlink()
            except OSError:
                pass
        service_proc.terminate()
        try:
            service_proc.wait(timeout=5)
        except subprocess.TimeoutExpired:
            service_proc.kill()


def main() -> None:
    ap = argparse.ArgumentParser(description="Transcript -> commentary -> TTS -> stitched podcast mp3.")
    ap.add_argument("transcript", help="Path to transcript text file (UTF-8).")
    ap.add_argument("--out-dir", default=None, help="Output directory (default: <transcript_dir>/commentary).")
    ap.add_argument("--model", default=DEFAULT_MODEL, help="OpenAI model (default: env OPENAI_MODEL).")
    ap.add_argument("--commentary-segments", type=int, default=DEFAULT_SEGMENTS, help="Number of commentary turns.")
    ap.add_argument("--chunk-words", type=int, default=1200, help="Words per chunk.")
    ap.add_argument("--overlap-words", type=int, default=0, help="Overlap words between chunks.")
    ap.add_argument("--words-ratio", type=float, default=DEFAULT_WORDS_RATIO, help="Commentary-to-transcript word ratio.")
    ap.add_argument("--min-words", type=int, default=DEFAULT_MIN_WORDS, help="Minimum words for total commentary.")
    ap.add_argument("--max-words", type=int, default=DEFAULT_MAX_WORDS, help="Maximum words for total commentary.")
    ap.add_argument("--notes-prompt", default=None, help="Producer notes prompt file.")
    ap.add_argument("--commentary-prompt", default=None, help="Commentary prompt file.")
    ap.add_argument("--futon5-dir", default=None, help="Path to futon5 repo (defaults to sibling).")
    ap.add_argument("--clj-bin", default="clj", help="Clojure CLI binary (default: clj).")
    ap.add_argument("--piper-model", default=DEFAULT_PIPER_MODEL, help="Piper model path (env PIPER_MODEL).")
    ap.add_argument("--piper-config", default=DEFAULT_PIPER_CONFIG, help="Piper config path (env PIPER_CONFIG).")
    ap.add_argument("--piper-bin", default=DEFAULT_PIPER_BIN, help="Piper binary (env PIPER_BIN).")
    ap.add_argument("--pause-seconds", type=float, default=DEFAULT_PAUSE, help="Silence between segments.")
    ap.add_argument("--output-mp3", default=None, help="Final stitched mp3 path.")
    ap.add_argument("--easyeffects-preset", default="Podcast", help="EasyEffects output preset name.")
    ap.add_argument(
        "--easyeffects-mode",
        choices=["replace", "sidecar"],
        default="sidecar",
        help="Replace the stitched mp3 or write a sidecar sweetened mp3.",
    )
    ap.add_argument("--easyeffects-suffix", default="_sweet", help="Sidecar filename suffix before .mp3.")
    ap.add_argument("--easyeffects-bin", default="easyeffects", help="EasyEffects command (supports flatpak).")
    ap.add_argument("--pw-play-bin", default="pw-play", help="pw-play command.")
    ap.add_argument("--pw-record-bin", default="pw-record", help="pw-record command.")
    ap.add_argument("--easyeffects-sink", default="Easy Effects Sink", help="EasyEffects sink name.")
    ap.add_argument("--easyeffects-monitor", default="Easy Effects Sink Monitor", help="EasyEffects monitor name.")
    args = ap.parse_args()

    transcript_path = Path(args.transcript).expanduser().resolve()
    if not transcript_path.exists():
        raise SystemExit(f"Transcript not found: {transcript_path}")

    out_dir = resolve_output_dir(transcript_path, args.out_dir)
    audio_dir = out_dir / "audio"
    output_mp3 = Path(args.output_mp3).expanduser().resolve() if args.output_mp3 else default_output_mp3(audio_dir)

    commentary_cmd = [
        "python3",
        str(Path(__file__).resolve().parent / "transcript_commentary.py"),
        str(transcript_path),
        "--model",
        args.model,
        "--commentary-segments",
        str(args.commentary_segments),
        "--chunk-words",
        str(args.chunk_words),
        "--overlap-words",
        str(args.overlap_words),
        "--words-ratio",
        str(args.words_ratio),
        "--min-words",
        str(args.min_words),
        "--max-words",
        str(args.max_words),
        "--clj-bin",
        args.clj_bin,
        "--tts",
        "--piper-model",
        args.piper_model,
        "--piper-config",
        args.piper_config,
    ]
    if args.out_dir:
        commentary_cmd.extend(["--out-dir", str(out_dir)])
    if args.notes_prompt:
        commentary_cmd.extend(["--notes-prompt", args.notes_prompt])
    if args.commentary_prompt:
        commentary_cmd.extend(["--commentary-prompt", args.commentary_prompt])
    if args.futon5_dir:
        commentary_cmd.extend(["--futon5-dir", args.futon5_dir])
    if args.piper_bin:
        commentary_cmd.extend(["--piper-bin", args.piper_bin])

    run_checked(commentary_cmd)

    segments = collect_segment_files(audio_dir, "commentary", "wav")
    stitch_segments(segments, args.pause_seconds, output_mp3)
    if args.easyeffects_preset:
        if args.easyeffects_mode == "replace":
            sweetened_mp3 = output_mp3.with_name(f"{output_mp3.stem}_sweetening_tmp{output_mp3.suffix}")
        else:
            sweetened_mp3 = output_mp3.with_name(f"{output_mp3.stem}{args.easyeffects_suffix}{output_mp3.suffix}")
        sweeten_with_easyeffects(
            input_mp3=output_mp3,
            output_mp3=sweetened_mp3,
            preset_name=args.easyeffects_preset,
            easyeffects_cmd=args.easyeffects_bin,
            pw_play_cmd=args.pw_play_bin,
            pw_record_cmd=args.pw_record_bin,
            sink_name=args.easyeffects_sink,
            monitor_name=args.easyeffects_monitor,
        )
        if args.easyeffects_mode == "replace":
            os.replace(sweetened_mp3, output_mp3)
        else:
            print(f"Wrote sweetened podcast: {sweetened_mp3}")
    if output_mp3.parent == audio_dir:
        update_latest_symlink(audio_dir, output_mp3, "commentary_full.mp3")
    print(f"Wrote stitched podcast: {output_mp3}")


if __name__ == "__main__":
    main()
