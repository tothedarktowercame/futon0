# Podcast Workflow

This document covers the machine-augmented podcast workflow built around Zoom R4
recordings, transcript chunking, LLM commentary generation, optional Piper TTS,
and final MP3 stitching/sweetening.

## Files

- `scripts/zoom_transcribe.py`
  - Takes an audio file, optionally chunks it with `ffmpeg`, runs
    `faster-whisper`, and writes both per-chunk transcripts and a stitched text
    transcript.
- `scripts/transcript_commentary.py`
  - Takes a transcript, chunks it into sections, generates producer notes across
    all sections, then requests one or more commentary turns using the full
    accumulated context.
- `scripts/transcript_to_podcast.py`
  - End-to-end wrapper: commentary generation, Piper TTS, MP3 stitching, and
    optional EasyEffects sweetening.
- `scripts/transcript_to_podcast_sweeten.sh`
  - Convenience wrapper around `transcript_to_podcast.py` that auto-detects an
    `easyeffects` command and defaults to the `Podcast` preset.
- `scripts/clean_commentary_outputs.py`
  - Cleanup helper for older JSON-wrapped model outputs.
- `resources/prompts/commentary_system.prompt`
  - System prompt for producer notes.
- `resources/prompts/commentary_followup.prompt`
  - Follow-up prompt for the actual podcast-style commentary.

## Prerequisites

- `ffmpeg` for chunking and MP3 stitching.
- `faster-whisper` for transcription.
- OpenAI CLI/API access for commentary generation.
- `futon5` checkout available as a sibling repo, or pass `--futon5-dir`.
- Optional: Piper plus `PIPER_MODEL` / `PIPER_CONFIG` for TTS.
- Optional: EasyEffects plus PipeWire (`pw-play`, `pw-record`) for sweetening.

## Typical flow

1. Transcribe a recording.

```bash
python3 scripts/zoom_transcribe.py \
  ../storage/zoomr4/incoming/2025/12-30/20251230_105836_track1-01.wav
```

This writes output under `data/zoomr4_transcripts/<stem>/` by default. Use
`--output-dir` to override. Use `--chunk-seconds 0` to skip chunking.

2. Generate commentary from the transcript.

```bash
python3 scripts/transcript_commentary.py \
  data/zoomr4_transcripts/<stem>/<stem>.txt
```

Default output is `<transcript_dir>/commentary/` with these subdirectories:

- `sections/`
- `prompts/`
- `responses/`
- `audio/` when `--tts` is enabled

Useful options:

- `--commentary-segments N` for multi-turn continuation
- `--chunk-words`, `--overlap-words` to control transcript chunking
- `--dry-run` to write payloads without calling the model
- `--tts` to synthesize commentary audio with Piper
- `--notes-prompt` / `--commentary-prompt` to override the default prompt files

3. Run the end-to-end pipeline.

```bash
python3 scripts/transcript_to_podcast.py \
  data/zoomr4_transcripts/<stem>/<stem>.txt \
  --commentary-segments 1
```

This wraps `transcript_commentary.py`, stitches `commentary_segment_*.mp3` into
a final MP3, and writes a dated output file under `commentary/audio/` unless
`--output-mp3` is supplied.

4. Sweeten the stitched output with EasyEffects.

```bash
scripts/transcript_to_podcast_sweeten.sh \
  data/zoomr4_transcripts/<stem>/<stem>.txt
```

By default this uses the `Podcast` EasyEffects preset and writes a sidecar
sweetened MP3. Pass `--easyeffects-mode replace` if you want the stitched file
replaced instead.

## Output notes

- `transcript_commentary.py` stores the evolving message history in
  `responses/messages.json`.
- `transcript_to_podcast.py` updates `commentary/audio/commentary_full.mp3` as a
  symlink to the latest dated stitched MP3 when writing into the default audio
  directory.
- `clean_commentary_outputs.py` is useful if older runs captured raw JSON model
  responses instead of plain text.

## Prompt files

The default prompt path wiring is local to this repo:

- `resources/prompts/commentary_system.prompt`
- `resources/prompts/commentary_followup.prompt`

If prompt behavior drifts, update those files first before changing the Python
pipeline.
