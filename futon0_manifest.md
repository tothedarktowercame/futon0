# FUTON0 Information Source Manifest

## Purpose
Map the existing (and near-future) digital traces available to FUTON0 so we can build vitality indicators and epistemic-rhythm monitors without introducing new manual lifelogging. Each entry highlights what signal it offers, how to access it, friction cost, and potential integrations with FUTON3/FUTON4.

## Legend
- **Channel** – subsystem emitting the trace.
- **Signal** – what kind of vitality/epistemic cue it provides.
- **Access** – command, repo, or service where the trace lives today.
- **Cadence** – how often the trace changes; useful for threshold setting.
- **Friction** – estimated overhead to query/maintain (Low = automated, Medium = some manual review, High = avoid unless essential).
- **Notes** – integration ideas or caveats.

## Core Code Repositories

| Channel | Signal | Access | Cadence | Friction | Notes |
| --- | --- | --- | --- | --- | --- |
| `futon0` repo | Devmap revisions, manifests, scanner code | Git checkout at `/home/joe/code/futon0` | Daily-weekly during FUTON0 sprints | Low | Use git history + file mtimes to detect vitality work on the base layer. |
| `futon1` repo | Session tags, WIP states, sphere coverage | `/home/joe/code/futon1` | Multi-session per day | Low | Already covers >90% sessions; can parse Org/JSON exports for track counts and futon moments. |
| `futon3` / `futon4` repos (when refreshed) | Devmaps, inference scripts | `/home/joe/code/futon3`, `/home/joe/code/futon4` | Weekly-monthly | Low-Medium | Track narrative refresh cadence; treat as symbolic health metric. |

## Version Control + Builder Signals

| Channel | Signal | Access | Cadence | Friction | Notes |
| --- | --- | --- | --- | --- | --- |
| Git commits (all repos) | Movement + rhythm of coding work | `git log --since=...` | In bursts; multiple/day when active | Low | Aggregate per day/week to infer energy + focus; cross-check with FUTON1 tags. |
| Branch/PR metadata | Scope + WIP count | `git branch`, hosted forge if any | Weekly | Low | Helps enforce WIP caps; detect lingering branches. |
| Build/test artifacts | Technical progress, failure loops | Local build logs | Per run | Medium | Could be sampled via cron to note streaks of passes/fails. |

## Filesystem Ambient Traces

| Channel | Signal | Access | Cadence | Friction | Notes |
| --- | --- | --- | --- | --- | --- |
| File modification times | Ambient creative activity | `find`/`stat` under project dirs | Minute-hour | Low | Cron scanner can summarise top-level directories touched per day. |
| Recording directories (audio/video) | Micro-recording cadence | e.g. `~/recordings`, `~/tatami` | Daily | Low | Use `ls -lt` to count new files; indicates reflective/creative rhythm. |
| Notes/Org trees | Planning + review cadence | `~/org`, `~/Dropbox/org` | Daily-weekly | Medium | Parse headings for weekly review evidence, stale nodes. |
| Shell history | Ops load, context switching | `~/.bash_history` | Continuous | Medium | Sampled summaries reveal multi-tasking or focus. |

## Communication + Session Streams

| Channel | Signal | Access | Cadence | Friction | Notes |
| --- | --- | --- | --- | --- | --- |
| Tatami/API chat logs | Ground-truth FUTON clock, reflective density | Tatami export API / local cache | Daily | Low | Already timestamped; scanner can detect gaps >24h as low-vitality warnings. |
| Calendar (AOB/Ambitions, Tai Chi intention) | Epistemic rhythm + weekly prompts | CalDAV/iCal feed | Weekly | Medium | Hook HUD prompts into FUTON3; log Y/N responses automatically. |
| Email labels / consulting ticket queues | External demand vs. response | IMAP API or service export | Daily | Medium | Use counts/latencies, not content, to avoid info overload. |
| Messaging apps (Signal/Matrix/Slack) | Collaboration energy | Export or notifications | Daily | Medium-High | Only if export automation exists; otherwise treat as optional. |

## System & Environment Telemetry

| Channel | Signal | Access | Cadence | Friction | Notes |
| --- | --- | --- | --- | --- | --- |
| Cron vitality scanner (to build) | Aggregated daily summary | `/var/log/cron`, scanner output in repo | Daily | Low | Emits JSON/CSV for FUTON3 ingestion; includes filesystem counts, git stats, Tatami gaps. |
| Local machine uptime & battery cycles | Physical strain / rest cycles | `uptime`, `pmset -g batt` (macOS) | Hourly | Low | Long uptimes without rest hint at imbalance; integrate into scanner. |
| Terminal process logs (`~/.local/share/fish` etc.) | Tool diversity, heavy workloads | Shell analytics | Daily | Medium | Optional; only if data already logged. |

## Potential Biometrics (No New Manual Logging)

| Channel | Signal | Access | Cadence | Friction | Notes |
| --- | --- | --- | --- | --- | --- |
| Wearable (Apple Watch, Oura, WHOOP) | HRV, sleep, activity load | Vendor export / HealthKit share sheet | Continuous | Medium | Leverage automatic sync; ingest only summary metrics (daily HRV, sleep debt) to avoid data sprawl. |
| Phone screen-time stats | Cognitive load / distraction | iOS/Android weekly report | Daily-weekly | Low | Already auto-captured; parse PDFs/emails into structured trend. |
| Location significant-places | Travel rhythm | Apple/Google timeline | Daily | Medium | Use only aggregated “distance/places per week” metrics. |

## HUD / Prompt Channels

| Channel | Signal | Access | Cadence | Friction | Notes |
| --- | --- | --- | --- | --- | --- |
| Weekly Tai Chi intention prompt | Embodied balance check | FUTON3 HUD config | Weekly | Low | Record Y/N automatically; tie to vitality indicator thresholds. |
| Review reminders (weekly/monthly) | Epistemic rhythm adherence | FUTON3 HUD + Org agenda | Weekly/monthly | Low | Event log becomes evidence for rhythm stability. |
| Spontaneous futon tags | Wonder/insight capture | FUTON1 tagging flow | Ad hoc | Low | Keep minimal text tags to mark resonance/dissonance moments. |

## External Knowledge Bases

| Channel | Signal | Access | Cadence | Friction | Notes |
| --- | --- | --- | --- | --- | --- |
| Personal wiki / Notion (if any) | Reference snapshots | Hosted | Weekly | Medium | Mirror critical entries into Org/FUTON3 to avoid drift. |
| Consulting portals / client repos | Demand pulses | Client-specific | Weekly | Medium | Count issue movements, not full content. |
| Finance/budget trackers | Resource strain | YNAB/Sheets exports | Weekly-monthly | Medium | Use aggregated deltas to see stress signals; optional but useful for vitality context. |

## Implementation Hooks

1. **Manifest registry** – Keep this file in `futon0` repo; update when new channels open/close.
2. **Scanner config** – YAML/JSON referencing channels with queries, so FUTON3 can request summaries programmatically.
3. **Privacy/effort gate** – Before wiring a channel, confirm it requires no new manual entry and respects attention limits.
4. **Pilot telemetry** – 30–45 day run collecting minimal fields (timestamps, counts) to establish baseline thresholds for alerts.
5. **Git vitality snapshot** – `data/git_sources.json` plus `scripts/git_activity.py` reproduce GitHub-style grids with semantic tags (`--days 60 --output /tmp/git.json`). Feed the JSON into FUTON3/FUTON4 HUD widgets.
6. **Zoom R4 ingest** – `scripts/zoom_sync.py --source /media/$USER/ZOOMR4 --dest ~/code/storage/zoomr4` watches for new WAVs, copies them into `~/code/storage/zoomr4`, converts to MP3 (when ffmpeg is available), and records hashes in `~/code/storage/zoomr4/meta/zoom_sync_index.json`. Friendly names live in `~/code/storage/zoomr4/meta/zoom_titles.json` (use `--title SHA=Name` to label tracks) and the script writes `TITLES.TXT` back to the recorder on every run. Override the metadata paths with `ZOOM_SYNC_META_DIR` or `ZOOM_SYNC_INDEX`/`ZOOM_SYNC_TITLES`. Optional user-level systemd units live in `systemd/zoom-sync.*` so the ingest runs automatically whenever the recorder mounts.
7. **Vitality scanner stub** – `scripts/vitality_scanner.py` reads `data/vitality_scanner.json`, counts recent filesystem touches, inspects Tatami exports when configured, and emits `data/vitality/latest_scan.json` for FUTON3 ingestion.

## Immediate Next Steps
- Prioritise high-signal/low-friction channels (Tatami, git, filesystem, FUTON1 tags) for the first scanner iteration.
- Decide wearable/phone data opt-in policy; if unavailable, mark as “future option” but keep interface slots ready.
- Draft the YAML config schema linking each channel to collection scripts, storage paths, and alert thresholds.
- Review this manifest weekly during the epistemic rhythm sync to ensure sources remain accurate and healthy.
