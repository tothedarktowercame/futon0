#!/usr/bin/env python3
"""
Batch transcribe Zoom R4 audio using faster-whisper.
Splits audio into fixed-length chunks, transcribes each, and stitches a full text file.
"""

from __future__ import annotations

import argparse
import os
import shutil
import subprocess
import sys
from pathlib import Path

try:
    from faster_whisper import WhisperModel
except ImportError as exc:
    raise SystemExit(
        "faster-whisper is not installed. Use the voice-typing venv or install it."
    ) from exc


DEFAULT_MODEL_PATH = Path("/home/joe/opt/voice-typing-linux/faster-whisper-small")


def resolve_model_path(user_value: str | None) -> str:
    if user_value:
        return user_value
    env_value = os.environ.get("FASTER_WHISPER_MODEL")
    if env_value:
        return env_value
    if DEFAULT_MODEL_PATH.exists():
        return str(DEFAULT_MODEL_PATH)
    return "small"


def require_ffmpeg() -> None:
    if shutil.which("ffmpeg") is None:
        raise SystemExit("ffmpeg not found in PATH.")


def split_audio(
    input_path: Path,
    chunk_dir: Path,
    chunk_seconds: int,
    prefix: str,
) -> list[Path]:
    require_ffmpeg()
    chunk_dir.mkdir(parents=True, exist_ok=True)
    pattern = chunk_dir / f"{prefix}_%03d.wav"
    cmd = [
        "ffmpeg",
        "-y",
        "-i",
        str(input_path),
        "-f",
        "segment",
        "-segment_time",
        str(chunk_seconds),
        "-ar",
        "16000",
        "-ac",
        "1",
        str(pattern),
    ]
    subprocess.run(cmd, check=True)
    return sorted(chunk_dir.glob(f"{prefix}_*.wav"))


def transcribe_chunks(
    model: WhisperModel,
    chunks: list[Path],
    text_dir: Path,
    full_out: Path,
    language: str,
    beam_size: int,
    best_of: int,
    temperature: float,
    vad_filter: bool,
    min_silence_ms: int,
    speech_pad_ms: int,
) -> None:
    text_dir.mkdir(parents=True, exist_ok=True)
    all_lines: list[str] = []

    for idx, chunk in enumerate(chunks, start=1):
        out_path = text_dir / f"{chunk.stem}.txt"
        print(f"[{idx}/{len(chunks)}] {chunk.name}")
        segments, info = model.transcribe(
            str(chunk),
            language=language,
            beam_size=beam_size,
            best_of=best_of,
            temperature=temperature,
            vad_filter=vad_filter,
            vad_parameters=dict(
                min_silence_duration_ms=min_silence_ms,
                speech_pad_ms=speech_pad_ms,
            ),
        )
        lines: list[str] = []
        for segment in segments:
            text = segment.text.strip()
            if text:
                lines.append(text)
        out_path.write_text("\n".join(lines) + ("\n" if lines else ""), encoding="utf-8")
        all_lines.extend(lines)
        print(f"  language={info.language} duration={info.duration} wrote={out_path}")

    full_out.write_text("\n".join(all_lines) + ("\n" if all_lines else ""), encoding="utf-8")
    print(f"full transcript: {full_out}")


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Transcribe an audio file with faster-whisper using 5-minute chunks."
    )
    parser.add_argument("input", help="Audio file to transcribe")
    parser.add_argument(
        "--output-dir",
        default=None,
        help="Base output directory (default: data/zoomr4_transcripts/<stem>)",
    )
    parser.add_argument(
        "--model",
        default=None,
        help="Model name or path (default: $FASTER_WHISPER_MODEL or local small model)",
    )
    parser.add_argument(
        "--device",
        default="cpu",
        choices=["cpu", "cuda"],
        help="Device (default: cpu)",
    )
    parser.add_argument(
        "--compute-type",
        default="int8",
        help="Compute type (default: int8)",
    )
    parser.add_argument(
        "--language",
        default="en",
        help="Language code (default: en)",
    )
    parser.add_argument(
        "--chunk-seconds",
        type=int,
        default=300,
        help="Chunk length in seconds (default: 300)",
    )
    parser.add_argument("--beam-size", type=int, default=5, help="Beam size (default: 5)")
    parser.add_argument("--best-of", type=int, default=3, help="Best-of (default: 3)")
    parser.add_argument(
        "--temperature",
        type=float,
        default=0.0,
        help="Temperature (default: 0.0)",
    )
    parser.add_argument(
        "--no-vad",
        action="store_true",
        help="Disable VAD filtering",
    )
    parser.add_argument(
        "--min-silence-ms",
        type=int,
        default=300,
        help="VAD min silence duration in ms (default: 300)",
    )
    parser.add_argument(
        "--speech-pad-ms",
        type=int,
        default=400,
        help="VAD speech padding in ms (default: 400)",
    )

    args = parser.parse_args()
    input_path = Path(args.input).expanduser().resolve()
    if not input_path.exists():
        raise SystemExit(f"Input not found: {input_path}")

    stem = input_path.stem
    if args.output_dir:
        base_dir = Path(args.output_dir).expanduser().resolve()
    else:
        base_dir = Path("data/zoomr4_transcripts") / stem
    chunk_dir = base_dir / "chunks"
    text_dir = base_dir / "chunks_txt"
    full_out = base_dir / f"{stem}_full.txt"

    model_path = resolve_model_path(args.model)
    print(f"Using model: {model_path}")

    if args.chunk_seconds <= 0:
        chunks = [input_path]
        text_dir.mkdir(parents=True, exist_ok=True)
    else:
        chunks = split_audio(input_path, chunk_dir, args.chunk_seconds, stem)
        if not chunks:
            raise SystemExit("No chunks produced; check ffmpeg output.")

    model = WhisperModel(
        model_path,
        device=args.device,
        compute_type=args.compute_type,
    )

    transcribe_chunks(
        model=model,
        chunks=chunks,
        text_dir=text_dir,
        full_out=full_out,
        language=args.language,
        beam_size=args.beam_size,
        best_of=args.best_of,
        temperature=args.temperature,
        vad_filter=not args.no_vad,
        min_silence_ms=args.min_silence_ms,
        speech_pad_ms=args.speech_pad_ms,
    )


if __name__ == "__main__":
    main()
