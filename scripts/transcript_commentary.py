#!/usr/bin/env python3
"""
Chunk a transcript, gather producer notes across chunks, then request
podcast-style commentary with optional multi-turn continuation and Piper TTS.
"""

from __future__ import annotations

import argparse
import json
import math
import os
import re
import shutil
import subprocess
from pathlib import Path


DEFAULT_CHUNK_WORDS = 1200
DEFAULT_OVERLAP_WORDS = 0
DEFAULT_MODEL = os.environ.get("OPENAI_MODEL", "gpt-5.2-chat-latest")
DEFAULT_WORDS_RATIO = float(os.environ.get("WORDS_RATIO", "1.0"))
DEFAULT_MIN_WORDS = int(os.environ.get("MIN_WORDS", "800"))
DEFAULT_MAX_WORDS = int(os.environ.get("MAX_WORDS", "8000"))

DEFAULT_PIPER_MODEL = os.environ.get("PIPER_MODEL", "")
DEFAULT_PIPER_CONFIG = os.environ.get("PIPER_CONFIG", "")
DEFAULT_PIPER_BIN = os.environ.get("PIPER_BIN", "")

NOTES_INPUT_TEMPLATE = """Transcript section {index} of {total}:
\"\"\"\n{transcript}\n\"\"\"
"""

COMMENTARY_START_TEMPLATE = """You have already received the full transcript in prior turns.
Begin the commentary now. Target length: {words_target} words total.
If it does not fit in one response, continue when I say "continue" without restarting.
"""


def run(cmd: list[str], cwd: Path | None = None, input_text: str | None = None) -> str:
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


def words_target_for_transcript(
    transcript: str,
    ratio: float,
    min_words: int,
    max_words: int,
) -> int:
    count = len(re.findall(r"\S+", transcript))
    target = int(math.ceil(count * ratio))
    target = max(min_words, min(max_words, target))
    return max(target, 1)


def resolve_futon5_dir() -> Path | None:
    env_value = os.environ.get("FUTON5_DIR")
    if env_value:
        candidate = Path(env_value).expanduser()
        if candidate.is_dir():
            return candidate
    root = Path(__file__).resolve().parents[2]
    candidate = root / "futon5"
    if candidate.is_dir():
        return candidate
    return None


def resolve_notes_prompt() -> Path:
    return Path(__file__).resolve().parents[1] / "resources" / "prompts" / "commentary_system.prompt"


def resolve_commentary_prompt() -> Path:
    return Path(__file__).resolve().parents[1] / "resources" / "prompts" / "commentary_followup.prompt"


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


def main() -> None:
    ap = argparse.ArgumentParser(description="Generate commentary + audio from a transcript.")
    ap.add_argument("transcript", help="Path to transcript text file (UTF-8).")
    ap.add_argument("--out-dir", default=None, help="Output directory (default: <transcript_dir>/commentary).")
    ap.add_argument("--chunk-words", type=int, default=DEFAULT_CHUNK_WORDS, help="Words per chunk.")
    ap.add_argument("--overlap-words", type=int, default=DEFAULT_OVERLAP_WORDS, help="Overlap words between chunks.")
    ap.add_argument("--model", default=DEFAULT_MODEL, help="OpenAI model (default: env OPENAI_MODEL).")
    ap.add_argument("--words-ratio", type=float, default=DEFAULT_WORDS_RATIO, help="Commentary-to-transcript word ratio.")
    ap.add_argument("--min-words", type=int, default=DEFAULT_MIN_WORDS, help="Minimum words for total commentary.")
    ap.add_argument("--max-words", type=int, default=DEFAULT_MAX_WORDS, help="Maximum words for total commentary.")
    ap.add_argument("--commentary-segments", type=int, default=1, help="Number of commentary segments to request.")
    ap.add_argument("--notes-prompt", default=str(resolve_notes_prompt()), help="Producer notes prompt file.")
    ap.add_argument("--commentary-prompt", default=str(resolve_commentary_prompt()), help="Commentary prompt file.")
    ap.add_argument("--futon5-dir", default=None, help="Path to futon5 repo (defaults to sibling).")
    ap.add_argument("--clj-bin", default="clj", help="Clojure CLI binary (default: clj).")
    ap.add_argument("--dry-run", action="store_true", help="Write payload JSON instead of calling OpenAI.")
    ap.add_argument("--tts", action="store_true", help="Run Piper TTS on commentary output.")
    ap.add_argument("--piper-model", default=DEFAULT_PIPER_MODEL, help="Piper model path (env PIPER_MODEL).")
    ap.add_argument("--piper-config", default=DEFAULT_PIPER_CONFIG, help="Piper config path (env PIPER_CONFIG).")
    ap.add_argument("--piper-bin", default=DEFAULT_PIPER_BIN, help="Piper binary (env PIPER_BIN).")
    args = ap.parse_args()

    transcript_path = Path(args.transcript).expanduser().resolve()
    if not transcript_path.exists():
        raise SystemExit(f"Transcript not found: {transcript_path}")

    transcript_text = transcript_path.read_text(encoding="utf-8").strip()
    sections = chunk_words(transcript_text, args.chunk_words, args.overlap_words)
    if not sections:
        raise SystemExit("Transcript is empty after parsing.")

    out_dir = Path(args.out_dir).expanduser().resolve() if args.out_dir else transcript_path.parent / "commentary"
    sections_dir = out_dir / "sections"
    prompts_dir = out_dir / "prompts"
    responses_dir = out_dir / "responses"
    audio_dir = out_dir / "audio"
    for d in (sections_dir, prompts_dir, responses_dir):
        d.mkdir(parents=True, exist_ok=True)
    if args.tts:
        audio_dir.mkdir(parents=True, exist_ok=True)

    futon5_dir = Path(args.futon5_dir).expanduser().resolve() if args.futon5_dir else resolve_futon5_dir()
    if futon5_dir is None or not futon5_dir.is_dir():
        raise SystemExit("futon5 repo not found; set --futon5-dir or FUTON5_DIR.")

    notes_prompt_path = Path(args.notes_prompt).expanduser().resolve()
    if not notes_prompt_path.exists():
        raise SystemExit(f"Notes prompt not found: {notes_prompt_path}")

    commentary_prompt_path = Path(args.commentary_prompt).expanduser().resolve()
    if not commentary_prompt_path.exists():
        raise SystemExit(f"Commentary prompt not found: {commentary_prompt_path}")

    if args.tts:
        if not args.piper_model or not args.piper_config:
            raise SystemExit("Set --piper-model/--piper-config or PIPER_MODEL/PIPER_CONFIG.")
        piper_bin = resolve_piper_bin(args.piper_bin)
        if not piper_bin:
            raise SystemExit("piper not found. Set --piper-bin or add it to PATH.")

    full_md_parts: list[str] = []
    full_txt_parts: list[str] = []
    notes_parts: list[str] = []

    messages: list[dict[str, str]] = []
    messages_path = responses_dir / "messages.json"

    def write_messages() -> None:
        messages_path.write_text(
            json.dumps(messages, ensure_ascii=True, indent=2) + "\n",
            encoding="utf-8",
        )

    total = len(sections)
    messages.append({"role": "system", "content": notes_prompt_path.read_text(encoding="utf-8")})

    for idx, section in enumerate(sections, start=1):
        section_path = sections_dir / f"section_{idx:03d}.txt"
        section_path.write_text(section + "\n", encoding="utf-8")

        notes_prompt = NOTES_INPUT_TEMPLATE.format(
            index=idx,
            total=total,
            transcript=section,
        )
        notes_prompt_path_out = prompts_dir / f"notes_prompt_{idx:03d}.txt"
        notes_prompt_path_out.write_text(notes_prompt, encoding="utf-8")

        messages.append({"role": "user", "content": notes_prompt})
        write_messages()

        notes_cmd = [
            args.clj_bin,
            "-M",
            "-m",
            "futon5.llm.relay",
            "--messages",
            str(messages_path),
            "--model",
            args.model,
        ]
        if args.dry_run:
            notes_cmd.append("--dry-run")

        notes_output = run(notes_cmd, cwd=futon5_dir)
        if args.dry_run:
            payload_path = responses_dir / f"payload_notes_{idx:03d}.json"
            payload_path.write_text(notes_output, encoding="utf-8")
            raise SystemExit("Dry-run stops after the first payload; rerun without --dry-run.")

        notes_output = extract_response_text(notes_output).strip()
        messages.append({"role": "assistant", "content": notes_output})
        write_messages()

        notes_path = responses_dir / f"producer_notes_{idx:03d}.txt"
        notes_path.write_text(notes_output + "\n", encoding="utf-8")
        notes_parts.append(f"--- notes {idx} ---\n{notes_output}\n")

    notes_full_path = out_dir / "producer_notes_full.txt"
    notes_full_path.write_text("\n".join(notes_parts).strip() + "\n", encoding="utf-8")

    messages.append({"role": "system", "content": commentary_prompt_path.read_text(encoding="utf-8")})
    total_words_target = words_target_for_transcript(
        transcript_text,
        args.words_ratio,
        args.min_words,
        args.max_words,
    )
    start_prompt = COMMENTARY_START_TEMPLATE.format(words_target=total_words_target)
    messages.append({"role": "user", "content": start_prompt})
    write_messages()

    def request_commentary_segment() -> str:
        cmd = [
            args.clj_bin,
            "-M",
            "-m",
            "futon5.llm.relay",
            "--messages",
            str(messages_path),
            "--model",
            args.model,
        ]
        output = extract_response_text(run(cmd, cwd=futon5_dir)).strip()
        messages.append({"role": "assistant", "content": output})
        write_messages()
        return output

    segment_total = max(args.commentary_segments, 1)
    for segment_idx in range(1, segment_total + 1):
        output = request_commentary_segment()
        md_path = responses_dir / f"commentary_segment_{segment_idx:03d}.md"
        md_path.write_text(output + "\n", encoding="utf-8")

        tts_text = strip_markdown_for_tts(output)
        txt_path = responses_dir / f"commentary_segment_{segment_idx:03d}.txt"
        txt_path.write_text(tts_text, encoding="utf-8")

        full_md_parts.append(output)
        full_txt_parts.append(tts_text.strip())

        if args.tts:
            wav_path = audio_dir / f"commentary_segment_{segment_idx:03d}.wav"
            run(
                [
                    piper_bin,
                    "--model",
                    args.piper_model,
                    "--config",
                    args.piper_config,
                    "--sentence_silence",
                    "0.35",
                    "--output_file",
                    str(wav_path),
                ],
                input_text=tts_text,
            )
            if shutil.which("ffmpeg"):
                mp3_path = audio_dir / f"commentary_segment_{segment_idx:03d}.mp3"
                run(["ffmpeg", "-y", "-i", str(wav_path), "-af", "loudnorm", str(mp3_path)])

        if segment_idx < segment_total:
            messages.append({"role": "user", "content": "continue"})
            write_messages()

    if not args.dry_run:
        full_md_path = out_dir / "commentary_full.md"
        full_md_path.write_text("\n".join(full_md_parts).strip() + "\n", encoding="utf-8")
        full_txt_path = out_dir / "commentary_full.txt"
        full_txt_path.write_text("\n\n".join(full_txt_parts).strip() + "\n", encoding="utf-8")

    print(f"Wrote commentary output to: {out_dir}")


if __name__ == "__main__":
    main()
