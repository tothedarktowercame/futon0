#!/usr/bin/env python3
"""
Shared helpers for chunked text -> model -> TTS/audio pipelines.
"""

from __future__ import annotations

import json
import os
import re
import shutil
import subprocess
import wave
import tempfile
from pathlib import Path


def run_capture(cmd: list[str], cwd: Path | None = None, input_text: str | None = None) -> str:
    try:
        result = subprocess.run(
            cmd,
            cwd=str(cwd) if cwd else None,
            input=input_text.encode("utf-8") if input_text is not None else None,
            check=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
    except subprocess.CalledProcessError as exc:
        stdout = (exc.stdout or b"").decode("utf-8", errors="replace").strip()
        stderr = (exc.stderr or b"").decode("utf-8", errors="replace").strip()
        details = []
        if stdout:
            details.append(f"stdout:\n{stdout}")
        if stderr:
            details.append(f"stderr:\n{stderr}")
        if details:
            raise SystemExit("Command failed:\n" + "\n\n".join(details)) from exc
        raise
    return result.stdout.decode("utf-8")


def run_checked(cmd: list[str], cwd: Path | None = None) -> None:
    subprocess.run(cmd, cwd=str(cwd) if cwd else None, check=True)


def extract_response_text(raw: str) -> str:
    try:
        payload = json.loads(raw)
    except json.JSONDecodeError:
        return raw

    if isinstance(payload, dict):
        if "choices" in payload:
            texts: list[str] = []
            for choice in payload.get("choices", []):
                message = choice.get("message") if isinstance(choice, dict) else None
                if isinstance(message, dict):
                    content = message.get("content")
                    if content:
                        texts.append(content)
            return "\n".join(texts).strip() or raw
        if "output_text" in payload:
            return str(payload.get("output_text", "")).strip() or raw
    return raw


def strip_markdown_for_tts(md: str) -> str:
    md = re.sub(r"```.*?```", "", md, flags=re.S)
    md = re.sub(r"^\s*#{1,6}\s*", "", md, flags=re.M)
    md = re.sub(r"^\s*[-*+]\s+", "", md, flags=re.M)
    md = re.sub(r"\n{3,}", "\n\n", md)
    return md.strip() + "\n"


def chunk_words(text: str, chunk_words: int, overlap_words: int) -> list[str]:
    if chunk_words <= 0:
        raise ValueError("chunk_words must be > 0")
    if overlap_words < 0:
        raise ValueError("overlap_words must be >= 0")
    if overlap_words >= chunk_words:
        raise ValueError("overlap_words must be < chunk_words")

    words = re.findall(r"\S+", text)
    if not words:
        return []

    step = chunk_words - overlap_words
    chunks: list[str] = []
    for start in range(0, len(words), step):
        chunk = words[start : start + chunk_words]
        if not chunk:
            break
        chunks.append(" ".join(chunk))
        if start + chunk_words >= len(words):
            break
    return chunks


def resolve_piper_bin(piper_bin: str) -> str:
    if piper_bin:
        return piper_bin
    if Path("bin/piper").is_file() and os.access("bin/piper", os.X_OK):
        return str(Path("bin/piper"))
    workspace_root = Path(__file__).resolve().parents[2]
    tts_candidate = workspace_root / "tts" / "bin" / "piper"
    if tts_candidate.is_file() and os.access(tts_candidate, os.X_OK):
        return str(tts_candidate)
    return shutil.which("piper") or ""


def collect_segments(audio_dir: Path, stem: str) -> list[Path]:
    segments = sorted(audio_dir.glob(f"{stem}_segment_*.mp3"))
    if not segments:
        raise SystemExit(f"No {stem}_segment_*.mp3 found in {audio_dir}")
    return segments


def collect_segment_files(audio_dir: Path, stem: str, suffix: str) -> list[Path]:
    segments = sorted(audio_dir.glob(f"{stem}_segment_*.{suffix}"))
    if not segments:
        raise SystemExit(f"No {stem}_segment_*.{suffix} found in {audio_dir}")
    return segments


def normalize_wav_in_place(path: Path) -> None:
    """
    Piper occasionally leaves a trailing byte in the WAV container.
    Rewriting through ffmpeg enforces a valid PCM container before any
    downstream encoding or concatenation.
    """
    tmp_path = path.with_name(f"{path.stem}.normalized{path.suffix}")
    run_checked(
        [
            "ffmpeg",
            "-y",
            "-v",
            "error",
            "-i",
            str(path),
            "-c:a",
            "pcm_s16le",
            str(tmp_path),
        ]
    )
    os.replace(tmp_path, path)


def wav_stream_params(path: Path) -> tuple[int, str]:
    with wave.open(str(path), "rb") as wav_file:
        channels = wav_file.getnchannels()
        sample_rate = wav_file.getframerate()
    if channels == 1:
        return sample_rate, "mono"
    if channels == 2:
        return sample_rate, "stereo"
    raise SystemExit(f"Unsupported wav channel count for stitching: {channels} ({path})")


def split_tts_text(text: str, max_words: int = 25) -> list[str]:
    text = text.strip()
    if not text:
        return []
    if max_words <= 0:
        raise ValueError("max_words must be > 0")

    def hard_split(part: str) -> list[str]:
        words = part.split()
        return [" ".join(words[i : i + max_words]) for i in range(0, len(words), max_words)]

    sentence_parts = re.split(r'(?<=[.!?])(?:["”’)\]]+)?\s+', text)
    normalized_parts: list[str] = []
    for sentence in sentence_parts:
        sentence = sentence.strip()
        if not sentence:
            continue
        if len(sentence.split()) <= max_words:
            normalized_parts.append(sentence)
            continue

        clause_parts = re.split(r"(?<=[,;:])\s+", sentence)
        for clause in clause_parts:
            clause = clause.strip()
            if not clause:
                continue
            if len(clause.split()) <= max_words:
                normalized_parts.append(clause)
            else:
                normalized_parts.extend(hard_split(clause))
    for chunk in normalized_parts:
        if len(chunk.split()) > max_words:
            raise ValueError("split_tts_text produced an oversized chunk")
    return normalized_parts


def synthesize_text_with_piper(
    text: str,
    output_wav: Path,
    piper_bin: str,
    piper_model: str,
    piper_config: str,
    sentence_silence: float = 0.25,
    max_words_per_chunk: int = 25,
    join_pause_seconds: float = 0.0,
) -> None:
    chunks = split_tts_text(text, max_words=max_words_per_chunk)
    if not chunks:
        raise SystemExit("No TTS text to synthesize.")

    with tempfile.TemporaryDirectory(prefix="piper_chunks_") as tmp_dir_raw:
        tmp_dir = Path(tmp_dir_raw)
        wav_parts: list[Path] = []
        for idx, chunk in enumerate(chunks, start=1):
            part_path = tmp_dir / f"part_{idx:03d}.wav"
            run_capture(
                [
                    piper_bin,
                    "--model",
                    piper_model,
                    "--config",
                    piper_config,
                    "--sentence_silence",
                    str(sentence_silence),
                    "--output_file",
                    str(part_path),
                ],
                input_text=chunk + "\n",
            )
            normalize_wav_in_place(part_path)
            wav_parts.append(part_path)

        stitch_segments(wav_parts, join_pause_seconds, output_wav)


def stitch_segments(segments: list[Path], pause_seconds: float, output_path: Path) -> None:
    sample_rate: int | None = None
    channel_layout = "mono"
    if segments and segments[0].suffix.lower() == ".wav":
        sample_rate, channel_layout = wav_stream_params(segments[0])

    inputs: list[str] = []
    for idx, segment in enumerate(segments):
        if idx > 0 and pause_seconds > 0:
            pause_src = f"anullsrc=r={sample_rate or 48000}:cl={channel_layout}"
            inputs.extend(["-f", "lavfi", "-t", f"{pause_seconds}", "-i", pause_src])
        inputs.extend(["-i", str(segment)])

    pause_inputs = max(len(segments) - 1, 0) if pause_seconds > 0 else 0
    total_inputs = len(segments) + pause_inputs
    labels = "".join([f"[{i}:a]" for i in range(total_inputs)])
    filter_complex = f"{labels}concat=n={total_inputs}:v=0:a=1[out]"

    cmd = [
        "ffmpeg",
        "-y",
        "-v",
        "error",
        *inputs,
        "-filter_complex",
        filter_complex,
        "-map",
        "[out]",
    ]
    if output_path.suffix.lower() == ".mp3":
        cmd.extend(["-codec:a", "libmp3lame", "-b:a", "64k"])
    cmd.append(str(output_path))
    run_checked(cmd)


def update_latest_symlink(audio_dir: Path, output_path: Path, link_name: str) -> None:
    link_path = audio_dir / link_name
    try:
        if link_path.exists() or link_path.is_symlink():
            link_path.unlink()
    except OSError:
        return
    if output_path.is_absolute():
        target = output_path
    else:
        target = (audio_dir / output_path).resolve()
    try:
        link_path.symlink_to(target)
    except OSError:
        return
