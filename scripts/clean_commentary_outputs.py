#!/usr/bin/env python3
"""
Normalize commentary outputs by extracting text from JSON responses.
"""

from __future__ import annotations

import argparse
import json
from pathlib import Path


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


def rewrite_file(path: Path) -> bool:
    raw = path.read_text(encoding="utf-8")
    cleaned = extract_response_text(raw)
    if cleaned == raw:
        return False
    path.write_text(cleaned.strip() + "\n", encoding="utf-8")
    return True


def main() -> None:
    ap = argparse.ArgumentParser(description="Clean JSON-wrapped commentary outputs.")
    ap.add_argument("responses_dir", help="Path to commentary responses directory.")
    args = ap.parse_args()

    responses_dir = Path(args.responses_dir).expanduser().resolve()
    if not responses_dir.is_dir():
        raise SystemExit(f"Responses directory not found: {responses_dir}")

    patterns = [
        "commentary_segment_*.md",
        "commentary_segment_*.txt",
        "commentary_*.md",
        "commentary_*.txt",
        "producer_notes_*.txt",
    ]

    changed = 0
    for pattern in patterns:
        for path in sorted(responses_dir.glob(pattern)):
            if rewrite_file(path):
                changed += 1
                print(f"cleaned: {path}")

    if changed == 0:
        print("No JSON-wrapped files found.")


if __name__ == "__main__":
    main()
