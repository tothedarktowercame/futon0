# Futon0 Rhythm (Time Dimension)

This document complements `futon0_manifest.md` (space dimension). It captures
the time dimension: cadence, retention, and the minimal reporting surface that
lets Futon0 track epistemic rhythm without committing raw biometrics.

## Backup cadence (time index)

Snapshots act as the rhythm anchor and define the reporting intervals.

- Daily snapshots: keep last 7.
- Weekly snapshots: keep last 4 Sundays.
- Monthly snapshots: keep last 12 first Sundays.

Runnable cadence config: `futon0/data/backup_cadence.json`.

Backups live under `~/code/backups/`. Each snapshot includes a lightweight
summary stats page that summarizes recent activity and biometric proxies
without storing raw data in git. These summary pages can themselves be tracked
over time as long-horizon vitality indicators.

## Example report schema (no raw data)

Store snapshots outside the repo; only a sample report may be committed.

```json
{
  "generated_at": "2026-01-05T12:00:00Z",
  "window": {
    "lookback_hours": 24,
    "weekly_anchor": "2026-01-04",
    "monthly_anchor": "2026-01-04"
  },
  "vitality": {
    "filesystem": [
      {"label": "code", "recent_files": 12, "latest_mtime": "2026-01-05T11:40:00Z"},
      {"label": "zoomr4", "recent_imports": 3}
    ],
    "tatami": {
      "events_in_lookback": 6,
      "hours_since_last": 2.1,
      "gap_warning": false
    }
  },
  "recording_ingest": {
    "recent_count": 3,
    "recent_minutes": 42
  },
  "git_activity": {
    "commit_count": 8,
    "repos_touched": ["futon0", "futon1", "futon3"]
  },
  "storage_status": {
    "needs_backup": true,
    "note": "Local storage roots are not mirrored offline yet."
  },
  "derivatives_exchange": {
    "summary": "Optional computed indicators from other futons",
    "signals": []
  }
}
```

Derivation notes (summary only; raw data stays in storage):
- `generated_at`: timestamp of the backup run.
- `window`: anchors derived from cadence config (weekly/monthly Sundays).
- `vitality.filesystem`: from `~/code/storage/futon0/vitality/latest_scan.json`.
- `vitality.tatami`: from `~/code/futon3/resources/tatami/logs/sessions.jsonl`.
- `recording_ingest`: from `~/code/storage/zoomr4/meta/zoom_sync_index.json`.
- `git_activity`: from `~/code/futon3/resources/vitality/git_summary.edn`.
- `storage_status`: from `~/code/storage/futon0/vitality/vitality_scanner.json`.
- `derivatives_exchange`: optional rollups computed from other futons.

Example report: `futon0/data/backup_report_example.json`.

## Evidence guidance

Use this rhythm report to document Prototype 1 (Epistemic Rhythm and Salients):

- Link a sample report to the devmap evidence ledger.
- Keep raw data in storage; only commit an example report and the cadence spec.

## Stack HUD logging cadence and envelope

Each `M-x stack-hud` invocation logs a snapshot (see
`futon0/contrib/stack-hud.el`) to
`~/code/storage/futon0/vitality/stack-hud/YYYY-MM-DD.jsonl`. On day rollover,
the logger compacts the prior day into a single `YYYY-MM-DD.summary.json`
containing only the first and last snapshots. This first/last pair defines a
daily "envelope" of the HUD state without retaining every intermediate refresh.

The envelope is intended as a rhythm marker rather than a forensic log: it
captures the day’s opening and closing state for quick comparisons and trend
tracking.

Envelope aggregation script: `futon0/scripts/futon0/rhythm/envelope.clj`.
Outputs `envelopes.jsonl`, `weekly.json`, and `monthly.json` into the configured
output directory; use `--futon1-out <path>` to emit an EDN payload for optional
FUTON1 ingestion.

## Weekly salients snapshot

Weekly salients merge Org headlines with FUTON1 tag summaries into a single
snapshot. Use `futon0/scripts/futon0/rhythm/salients.clj` to emit
`salients.json` under `~/code/backups/` and optionally publish an EDN payload
via `--futon1-out` or `--futon3-out` when handoff ownership is decided.

## Persistence vs logging

Raw activity and biometrics stay in storage (or upstream systems). When needed,
derived summaries can be persisted into FUTON1; otherwise they remain in the
HUD logs and backup summaries. The goal is to preserve a stable, inspectable
trace of rhythm without committing sensitive data to git.

## Rhythm buildout plan

### M0 — Cadence + report spec [0/3]
### M0 — Cadence + report spec [3/3]
- [X] Document the backup cadence and retention policy in a runnable form.
- [X] Define the summary stats schema (fields + derivation notes).
- [X] Create one synthetic example report (committed).

### M1 — Backup runner + summary page [4/4]
- [X] Implement snapshot creation under `~/code/backups/`.
- [X] Emit summary stats per snapshot (JSON + optional Markdown).
- [X] Add a small validator to keep summary files consistent with the schema.
- [X] Write a minimal README snippet showing how to run the backup job.

### M2 — Rhythm envelope aggregation [3/3]
- [X] Parse HUD first/last summaries into a daily envelope stream.
- [X] Roll up envelopes into weekly/monthly aggregates.
- [X] Persist derived envelope summaries to FUTON1 (optional).

### M3 — Salients merge + handoff [3/3]
- [X] Merge Org + FUTON1 metadata into a weekly salients snapshot.
- [X] Publish the snapshot to FUTON1 (or FUTON3 if ownership shifts).
- [X] Record the handoff target and completed steps for traceability.

### M4 — Experimental harness + leaderboard [3/3]
- [X] Define experiment configs (retention windows, thresholds, weights).
- [X] Compute outcome metrics and rank configs.
- [X] Render a lightweight leaderboard report per cycle.

### M5 — Quarterly rhythm analysis [2/2]
- [X] Correlate cadence stability with affect markers (energy, focus, lightness).
- [X] Archive quarterly summaries as input to future cadence tuning.

Update this plan as milestones advance; keep the checklist honest.
