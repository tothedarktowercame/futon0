# Paper Read-Aloud Workflow

This workflow adapts the chunked Piper pipeline for paper narration rather than
podcast commentary. It expects plain text or markdown extracted from a paper,
reads each chunk from the source text by default, optionally routes chunks
through the relay for a spoken rendering, and stitches a final MP3.

## Files

- `scripts/paper_to_readaloud.py`
  - End-to-end wrapper for chunking paper text, default literal read-aloud,
    optional relay-based spoken rendering, optional Piper TTS, and final MP3
    stitching.
- `scripts/audio_pipeline_lib.py`
  - Shared chunking, relay output parsing, Piper lookup, and MP3 stitching
    helpers used by both the podcast and paper workflows.
- `resources/prompts/paper_readaloud_system.prompt`
  - System prompt for faithful spoken rendering of a paper section.

## Prerequisites

- Plain text or markdown input for the paper.
- OpenAI CLI/API access through `futon5.llm.relay`.
- `futon5` checkout available as a sibling repo, or pass `--futon5-dir`.
- Optional: Piper plus `PIPER_MODEL` / `PIPER_CONFIG` for TTS.
- Optional: `ffmpeg` for MP3 normalization and stitching.

If the paper is only available as PDF, extract UTF-8 text first and review the
result before narration.

## Typical flow

1. Generate a read-aloud script from the paper text.

```bash
python3 scripts/paper_to_readaloud.py \
  /path/to/paper.txt
```

By default this keeps the source wording rather than paraphrasing it.

This writes output under `<paper_dir>/readaloud/` by default with these
subdirectories:

- `sections/`
- `prompts/`
- `responses/`
- `audio/` when `--tts` is enabled

Useful options:

- `--chunk-words`, `--overlap-words` to control sectioning
- `--render-through-llm` to rewrite each section into spoken prose before TTS
- `--dry-run` to write the first relay payload without calling the model
- `--prompt` to override the default paper read-aloud prompt

2. Add TTS and stitched MP3 output.

```bash
python3 scripts/paper_to_readaloud.py \
  /path/to/paper.txt \
  --tts
```

This synthesizes `paper_readaloud_segment_*.mp3`, stitches them into a dated
output file under `readaloud/audio/`, and updates
`readaloud/audio/paper_readaloud_full.mp3` as a symlink to the latest dated
output when using the default audio directory.

If you explicitly want a spoken adaptation instead of a literal read-aloud, add
`--render-through-llm`.

## Output notes

- `responses/messages.json` stores the evolving message history passed to the
  relay when `--render-through-llm` is used.
- `paper_readaloud_full.md` stores the assembled read-aloud script.
- `paper_readaloud_full.txt` stores the TTS-cleaned version.
