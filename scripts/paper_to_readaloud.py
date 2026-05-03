#!/usr/bin/env python3
"""
Generate a spoken read-aloud version of a paper from plain text/markdown input.
Defaults to a literal source-text read-aloud; LLM rendering is opt-in.
"""

from __future__ import annotations

import argparse
import json
import os
import shutil
from datetime import datetime
from pathlib import Path

from audio_pipeline_lib import (
    chunk_words,
    collect_segment_files,
    extract_response_text,
    resolve_piper_bin,
    run_capture,
    stitch_segments,
    strip_markdown_for_tts,
    synthesize_text_with_piper,
    update_latest_symlink,
)


DEFAULT_MODEL = os.environ.get("OPENAI_MODEL", "gpt-5.2-chat-latest")
DEFAULT_CHUNK_WORDS = 1600
DEFAULT_OVERLAP_WORDS = 120
DEFAULT_PAUSE = float(os.environ.get("READALOUD_PAUSE_SECONDS", "0.6"))

DEFAULT_PIPER_MODEL = os.environ.get("PIPER_MODEL", "")
DEFAULT_PIPER_CONFIG = os.environ.get("PIPER_CONFIG", "")
DEFAULT_PIPER_BIN = os.environ.get("PIPER_BIN", "")

SECTION_TEMPLATE = """Paper section {index} of {total}:
\"\"\"\n{section}\n\"\"\"
"""

START_TEMPLATE = """You have already rendered any earlier sections of this paper in prior turns.
Render section {index} of {total} now.
Do not restart the paper. Do not repeat earlier sections except where a short bridge is needed for continuity.
"""


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


def resolve_prompt() -> Path:
    return Path(__file__).resolve().parents[1] / "resources" / "prompts" / "paper_readaloud_system.prompt"


def default_output_dir(paper_path: Path, out_dir: str | None) -> Path:
    return Path(out_dir).expanduser().resolve() if out_dir else paper_path.parent / "readaloud"


def default_output_mp3(audio_dir: Path) -> Path:
    stamp = datetime.now().strftime("%Y-%m-%d")
    return audio_dir / f"paper_readaloud_{stamp}_full.mp3"


def main() -> None:
    ap = argparse.ArgumentParser(description="Generate a spoken read-aloud version of a paper.")
    ap.add_argument("paper", help="Path to paper text/markdown file (UTF-8).")
    ap.add_argument("--out-dir", default=None, help="Output directory (default: <paper_dir>/readaloud).")
    ap.add_argument("--chunk-words", type=int, default=DEFAULT_CHUNK_WORDS, help="Words per chunk.")
    ap.add_argument("--overlap-words", type=int, default=DEFAULT_OVERLAP_WORDS, help="Overlap words between chunks.")
    ap.add_argument(
        "--render-through-llm",
        action="store_true",
        help="Rewrite each paper section into spoken prose with the relay before TTS.",
    )
    ap.add_argument("--model", default=DEFAULT_MODEL, help="OpenAI model (default: env OPENAI_MODEL).")
    ap.add_argument("--prompt", default=str(resolve_prompt()), help="Paper read-aloud prompt file.")
    ap.add_argument("--futon5-dir", default=None, help="Path to futon5 repo (defaults to sibling).")
    ap.add_argument("--clj-bin", default="clj", help="Clojure CLI binary (default: clj).")
    ap.add_argument("--dry-run", action="store_true", help="Write the first relay payload and stop.")
    ap.add_argument("--tts", action="store_true", help="Run Piper TTS on rendered sections.")
    ap.add_argument("--piper-model", default=DEFAULT_PIPER_MODEL, help="Piper model path (env PIPER_MODEL).")
    ap.add_argument("--piper-config", default=DEFAULT_PIPER_CONFIG, help="Piper config path (env PIPER_CONFIG).")
    ap.add_argument("--piper-bin", default=DEFAULT_PIPER_BIN, help="Piper binary (env PIPER_BIN).")
    ap.add_argument("--pause-seconds", type=float, default=DEFAULT_PAUSE, help="Silence between spoken sections.")
    ap.add_argument("--output-mp3", default=None, help="Final stitched mp3 path.")
    args = ap.parse_args()

    paper_path = Path(args.paper).expanduser().resolve()
    if not paper_path.exists():
        raise SystemExit(f"Paper not found: {paper_path}")

    paper_text = paper_path.read_text(encoding="utf-8").strip()
    sections = chunk_words(paper_text, args.chunk_words, args.overlap_words)
    if not sections:
        raise SystemExit("Paper is empty after parsing.")

    out_dir = default_output_dir(paper_path, args.out_dir)
    sections_dir = out_dir / "sections"
    prompts_dir = out_dir / "prompts"
    responses_dir = out_dir / "responses"
    audio_dir = out_dir / "audio"
    for directory in (sections_dir, prompts_dir, responses_dir):
        directory.mkdir(parents=True, exist_ok=True)
    if args.tts:
        audio_dir.mkdir(parents=True, exist_ok=True)

    futon5_dir: Path | None = None
    prompt_path: Path | None = None
    if args.render_through_llm:
        futon5_dir = Path(args.futon5_dir).expanduser().resolve() if args.futon5_dir else resolve_futon5_dir()
        if futon5_dir is None or not futon5_dir.is_dir():
            raise SystemExit("futon5 repo not found; set --futon5-dir or FUTON5_DIR.")

        prompt_path = Path(args.prompt).expanduser().resolve()
        if not prompt_path.exists():
            raise SystemExit(f"Prompt not found: {prompt_path}")

    if args.tts:
        if not args.piper_model or not args.piper_config:
            raise SystemExit("Set --piper-model/--piper-config or PIPER_MODEL/PIPER_CONFIG.")
        piper_bin = resolve_piper_bin(args.piper_bin)
        if not piper_bin:
            raise SystemExit("piper not found. Set --piper-bin or add it to PATH.")
        if not shutil.which("ffmpeg"):
            raise SystemExit("ffmpeg not found. Add it to PATH before using --tts.")

    messages: list[dict[str, str]] = []
    messages_path = responses_dir / "messages.json"

    def write_messages() -> None:
        messages_path.write_text(json.dumps(messages, ensure_ascii=True, indent=2) + "\n", encoding="utf-8")

    if args.render_through_llm:
        messages.append({"role": "system", "content": prompt_path.read_text(encoding="utf-8")})
    write_messages()

    full_md_parts: list[str] = []
    full_txt_parts: list[str] = []
    total = len(sections)

    for idx, section in enumerate(sections, start=1):
        section_path = sections_dir / f"section_{idx:03d}.txt"
        section_path.write_text(section + "\n", encoding="utf-8")

        if args.render_through_llm:
            prompt_text = START_TEMPLATE.format(index=idx, total=total) + "\n" + SECTION_TEMPLATE.format(
                index=idx,
                total=total,
                section=section,
            )
            prompt_out_path = prompts_dir / f"readaloud_prompt_{idx:03d}.txt"
            prompt_out_path.write_text(prompt_text, encoding="utf-8")

            messages.append({"role": "user", "content": prompt_text})
            write_messages()

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
            if args.dry_run:
                cmd.append("--dry-run")

            output = run_capture(cmd, cwd=futon5_dir)
            if args.dry_run:
                payload_path = responses_dir / f"payload_readaloud_{idx:03d}.json"
                payload_path.write_text(output, encoding="utf-8")
                raise SystemExit("Dry-run stops after the first payload; rerun without --dry-run.")

            output = extract_response_text(output).strip()
            messages.append({"role": "assistant", "content": output})
            write_messages()
        else:
            prompt_text = SECTION_TEMPLATE.format(index=idx, total=total, section=section)
            prompt_out_path = prompts_dir / f"readaloud_prompt_{idx:03d}.txt"
            prompt_out_path.write_text(prompt_text, encoding="utf-8")
            output = section.strip()

        md_path = responses_dir / f"paper_readaloud_segment_{idx:03d}.md"
        md_path.write_text(output + "\n", encoding="utf-8")

        tts_text = strip_markdown_for_tts(output)
        txt_path = responses_dir / f"paper_readaloud_segment_{idx:03d}.txt"
        txt_path.write_text(tts_text, encoding="utf-8")

        full_md_parts.append(output)
        full_txt_parts.append(tts_text.strip())

        if args.tts:
            wav_path = audio_dir / f"paper_readaloud_segment_{idx:03d}.wav"
            synthesize_text_with_piper(
                text=tts_text,
                output_wav=wav_path,
                piper_bin=piper_bin,
                piper_model=args.piper_model,
                piper_config=args.piper_config,
                sentence_silence=0.25,
            )
            mp3_path = audio_dir / f"paper_readaloud_segment_{idx:03d}.mp3"
            run_capture(["ffmpeg", "-y", "-v", "error", "-i", str(wav_path), "-af", "loudnorm", str(mp3_path)])

    full_md_path = out_dir / "paper_readaloud_full.md"
    full_md_path.write_text("\n\n".join(full_md_parts).strip() + "\n", encoding="utf-8")
    full_txt_path = out_dir / "paper_readaloud_full.txt"
    full_txt_path.write_text("\n\n".join(full_txt_parts).strip() + "\n", encoding="utf-8")

    if not args.tts:
        print(f"Wrote paper read-aloud text output to: {out_dir}")
        return

    output_mp3 = Path(args.output_mp3).expanduser().resolve() if args.output_mp3 else default_output_mp3(audio_dir)
    segments = collect_segment_files(audio_dir, "paper_readaloud", "wav")
    stitch_segments(segments, args.pause_seconds, output_mp3)
    if output_mp3.parent == audio_dir:
        update_latest_symlink(audio_dir, output_mp3, "paper_readaloud_full.mp3")
    print(f"Wrote paper read-aloud output to: {out_dir}")
    print(f"Wrote stitched read-aloud mp3: {output_mp3}")


if __name__ == "__main__":
    main()
