# FUTON0 utility scripts

Two automation hooks live here while FUTON1 stabilises:

## Devmap

The prototype roadmap lives in `../futon3/holes/futon0.devmap`. This README
tracks the concrete scripts and operational notes that back those entries.

## Git activity manifest
1. Run `scripts/git_vitality_sync.sh` to regenerate `git_activity.json` and the downstream `git_summary.edn` HUD feed in `../futon3/resources/vitality/`.
2. The helper script wraps `python3 scripts/git_activity.py` (configurable via `data/git_sources.json`) followed by `clojure -M:vitality/git-summary` inside the `futon3` repo, so one call refreshes the entire pipeline.
3. Optional automation: copy `systemd/user-git-vitality.service` and `systemd/user-git-vitality.timer` into `~/.config/systemd/user/` as `git-vitality.service`/`git-vitality.timer`, then run `systemctl --user daemon-reload && systemctl --user enable --now git-vitality.timer` for an hourly refresh.

## Zoom R4 ingest
- `scripts/zoom_sync.py --source /media/$USER/ZOOMR4 --dest ~/code/storage/zoomr4` copies WAV recordings off the Zoom R4, converts to MP3 when `ffmpeg` is present, and now normalizes `~/code/storage/zoomr4/meta/zoom_sync_index.json` as the single catalog (schema fields include SHA, recorder project, derived tags, mp3 path, duration/channel counts via `ffprobe`, and the curation status). The helper still regenerates `~/code/storage/zoomr4/meta/zoom_titles.json` for legacy callers but every edit flows through the catalog. Override the locations with `ZOOM_SYNC_META_DIR` or `ZOOM_SYNC_INDEX`/`ZOOM_SYNC_TITLES`.
- Add `--delete-from-recorder` to remove recorder files whose catalog status is `archive` or `trash` and clean up any empty recorder project directories. This honors `--dry-run`, so you can see the deletion list without touching the SD card.
- Add `--rename-hold` to rename recorder files for `hold` entries using the catalog title (sanitized, extension preserved). This also respects `--dry-run`.
- Update metadata inline via `python3 scripts/zoom_sync.py --title SHA=MySong` and `--status SHA=hold|archive|trash`. Each edit persists to the catalog, rewrites per-project `TITLES.TXT` entries on the recorder (`TRACK1_01.WAV :: My Chorus`), and keeps `~/code/storage/zoomr4` in sync with the recorder library; use `hold` for “Zoom + disk”, `archive` for “disk only”, and `trash` once you’re ready to delete from both.
- `python3 scripts/zoom_sync.py --list` (plus `--list-status hold` or `--list-limit 20`) prints the library sorted by recorded date so you can skim takes from the terminal. `--list-missing-titles` and `--auto-title-missing` still help fill friendly timestamp titles for recorder files that haven’t been curated yet.
- Enable automatic ingest via the sample `systemd/zoom-sync.path` + `.service` units documented in `systemd/README.md`.

## Zoom R4 transcription (on demand)
- `python3 scripts/zoom_transcribe.py ../storage/zoomr4/incoming/2025/12-30/20251230_105836_track1-01.wav` splits the file into 5-minute 16 kHz mono chunks, runs faster-whisper, and writes per-chunk transcripts plus a stitched full transcript under `data/zoomr4_transcripts/<stem>/`.
- Set `--output-dir` to control the output location and `--chunk-seconds 0` to skip chunking.
- The script uses `$FASTER_WHISPER_MODEL` or `/home/joe/opt/voice-typing-linux/faster-whisper-small` when present; pass `--model` to override.

## Transcript commentary (LLM + TTS)
- `python3 scripts/transcript_commentary.py /path/to/transcript.txt` chunks a transcript, gathers producer notes across all chunks, then requests a podcast-style commentary using the full conversation context. Outputs land under `<transcript_dir>/commentary`.
- Defaults: `--chunk-words 1200`, `--overlap-words 0`, `--model gpt-5.2-chat-latest`, `--words-ratio 1.0`, `--min-words 800`, `--max-words 8000`, `--commentary-segments 1`.
- Add `--commentary-segments N` to request multiple commentary turns (the script appends `continue` between turns).
- Add `--tts` plus `--piper-model`/`--piper-config` (or `PIPER_MODEL`/`PIPER_CONFIG`) to synthesize audio output.
- The producer-notes prompt defaults to `resources/prompts/commentary_system.prompt`; the commentary prompt defaults to `resources/prompts/commentary_followup.prompt`.
- For an end-to-end run (commentary + TTS + stitched mp3), use `python3 scripts/transcript_to_podcast.py /path/to/transcript.txt`. Defaults: `--commentary-segments 1`, `--pause-seconds 1.0`, `--model gpt-5.2-chat-latest`.

## Vitality scanner stub
- `clojure -Sdeps '{:paths ["scripts"] :deps {org.clojure/data.json {:mvn/version "2.5.0"}}}' -M -m futon0.vitality.scanner` (source: `scripts/futon0/vitality/scanner.clj`) inspects directories listed in `~/code/storage/futon0/vitality/vitality_scanner.json`, counts files touched during the current lookback window, optionally parses Tatami logs, and writes a summary JSON payload (default `~/code/storage/futon0/vitality/latest_scan.json`).
- Adjust `~/code/storage/futon0/vitality/vitality_scanner.json` to point at the directories/Tatami export you want to monitor, then call the scanner from cron/systemd alongside `git_vitality_sync.sh` to start the 30–45 day baseline run mentioned in the devmap.
- The sample `systemd/user-vitality-scanner.service` + `.timer` units run the scanner hourly and write straight into `../futon3/resources/vitality/latest_scan.json`, so Futon3’s Stack HUD stays live without manual copies. For more responsive updates, enable `systemd/user-vitality-scanner.path` to trigger scans on futon directory changes (throttled); details live in `systemd/README.md`.
- Each filesystem entry now supports an optional `import_index`/`import_limit`. Point `import_index` at `~/code/storage/zoomr4/meta/zoom_sync_index.json` (or any JSON index with `entries`) and the Stack HUD will report how many assets were already imported even if the drive is offline; this is how `zoomr4` shows “| 57 imported” along with the latest track titles.
- The vitality JSON data stores are a practical prototype for now; revisit this later for real-time, concurrent state management instead of periodic JSON snapshots.
- `~/code/storage/futon0/vitality/vitality_scanner.json` advertises `storage_status` so the futon0 vitality HUD raises a warning (“storage not backed up”) until the storage roots in `~/code/storage` are mirrored to offline media.
- Until the dedicated Stack HUD surface lands, Futon3 renders this telemetry inside the
  Tatami context pane (to keep the data visible); treat that integration as a preview
  rather than the final UI.
- **How to read the HUD:**
  - *Vitality* rows summarize how many files changed inside each watched directory during the scanner’s 24‑hour lookback window. A zero means “no edits today”, while a ⚠️ after “Tatami” warns that no sessions were logged during the configured gap window.
  - *Boundary gaps* come from `resources/boundary.edn` and report “missing evidence” counts per futon layer. These numbers increase when devmap entries go stale and shrink when you land corresponding artifacts; treat them as “proof debt” gauges for each layer.
  - *Reminders* are simple countdowns generated by `stack/status.clj` (weekly review, Tai Chi, etc.) so the HUD keeps both operational and calendar nudges in one place.

## Backup summary snapshots
- `clojure -Sdeps '{:paths ["scripts"] :deps {org.clojure/data.json {:mvn/version "2.5.0"}}}' -M -m futon0.backup.snapshot --write` writes a snapshot under `~/code/backups/` with `summary.json` (+ `summary.md` by default). It pulls vitality data, recording ingest stats, and git summary when available. Run with `--validate <path>` to check a summary file against the minimal schema.

## Rhythm envelope aggregation
- `clojure -Sdeps '{:paths ["scripts"] :deps {org.clojure/data.json {:mvn/version "2.5.0"}}}' -M -m futon0.rhythm.envelope --write` reads Stack HUD `YYYY-MM-DD.summary.json` files from `~/code/storage/futon0/vitality/stack-hud`, emits `envelopes.jsonl`, plus weekly/monthly aggregates, and can optionally write an EDN payload for FUTON1 via `--futon1-out <path>`.

## Weekly salients snapshot
- `clojure -Sdeps '{:paths ["scripts"] :deps {org.clojure/data.json {:mvn/version "2.5.0"}}}' -M -m futon0.rhythm.salients --org ~/code/futon3/holes/aob.org --write` merges Org headlines with optional FUTON1 tag summaries into `salients.json`. Use `--futon1-tags <path>` to include a tags summary (EDN/JSON), and `--futon1-out`/`--futon3-out` to publish a handoff payload.

## Rhythm dry run (boundary checks)
- `clojure -Sdeps '{:paths ["scripts"] :deps {org.clojure/data.json {:mvn/version "2.5.0"}}}' -M -m futon0.rhythm.dry-run` creates a temporary HUD summary, validates the backup cadence/report examples, runs envelope aggregation, and writes a salients snapshot to ensure M1→M2→M3 transitions stay coherent.

## Experiment harness + leaderboard
- `clojure -Sdeps '{:paths ["scripts"] :deps {org.clojure/data.json {:mvn/version "2.5.0"}}}' -M -m futon0.rhythm.experiments --write` reads `envelopes.jsonl`, applies `data/backup_experiments.json`, and writes `experiments.json` plus a Markdown leaderboard into `~/code/backups/`.

## Quarterly rhythm analysis
- `clojure -Sdeps '{:paths ["scripts"] :deps {org.clojure/data.json {:mvn/version "2.5.0"}}}' -M -m futon0.rhythm.quarterly --write` correlates envelope summaries with affect markers (see `data/affect_markers.json`) and writes `quarterly.json` into `~/code/backups/`. Provide `--affect <path>` to point at a local `affect.jsonl`.

## Affect transitions ingest (FUTON1)
- `clojure -Sdeps '{:paths ["scripts"] :deps {org.clojure/data.json {:mvn/version "2.5.0"}}}' -M -m futon0.rhythm.affect --api-base http://localhost:8080 --actor-id :open-world-ingest.nlp/ego --write` fetches FUTON1 affect transitions and emits `~/code/storage/futon0/vitality/affect.jsonl` (or `--output`) for quarterly correlation. Use `--append` for incremental daily runs and pass `--lookback-hours`, `--since`, or `--until` to control the window.

## Voice typing bridge

- `opt/voice-typing-linux/voice` launches `enhanced-voice-typing.py`, a faster-whisper + VAD loop tuned for this stack. Pass `--layout dvorak` (or keep the default `qwerty`) and optional `--enter-key KEYWORD` to control how transcripts are mapped and when Enter is sent.
- The script prefers a per-user ydotool socket (`/run/user/$UID/ydotoold/socket`) and advertises that path via `YDOTOOL_SOCKET_PATH`. Make sure the `ydotoold` binary can access `/dev/uinput` (either join the `uinput` group or apply the udev rule in `docs/systemd/README.md`).
- Futon3’s Stack HUD exposes “Voice typing: Start/Stop” buttons that launch both `ydotoold` (using `my-chatgpt-shell-ydotoold-command`) and the voice client, so you can toggle dictation without touching a terminal. Output lands in the `*Stack Voice Typing*` buffer for debugging.
- When the HUD reports “Voice typing: ON | pid …” it means both `ydotoold` and the recognizer are live; if it shows “unconfigured” double‑check `my-chatgpt-shell-voice-command` or the udev permissions on `/dev/uinput`. The hot reload block above it works the same way for Emacs auto‑eval.
- TODO: Trace why toggling voice typing sometimes opens `*Tatami Context*` and log the initiating command so it can be suppressed.

## Elisp tests

Run the futon0 contrib ERT tests:
```bash
emacs -Q --batch \
  -L ~/code/futon0/contrib \
  -L ~/code/futon0/contrib/test \
  -l ~/code/futon0/contrib/test/stack-hud-test.el \
  -l ~/code/futon0/contrib/test/stack-doc-test.el \
  -l ~/code/futon0/contrib/test/voice-typing-test.el \
  --eval "(ert-run-tests-batch-and-exit t)"
```
