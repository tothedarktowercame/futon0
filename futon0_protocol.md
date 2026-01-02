# Futon0 Protocol (Decisions + Open Questions)

This document captures open decisions and configuration questions for the
Futon0 rhythm/backup pipeline. It is intended for slow, deliberate completion.

## Immediate decisions

- Backup output root (default `~/code/backups/`): ___________________________
- Org sources for weekly salients (paths): _________________________________
- FUTON1 handoff now? (`--futon1-out` enabled) [yes/no]: ____________________
- Affect source (open-world ingest): _______________________________________
- Experiment config updates needed? (weights/windows) _______________________

## Affect markers from open-world ingest

- Source of ingest data:
  - [ ] XTDB
  - [ ] JSONL/EDN log
  - [ ] Other: ___________________________
- Path or endpoint for source: _____________________________________________
- Preferred heuristic signals (if any): ____________________________________
- Acceptable lag (e.g., hourly/daily): _____________________________________

## Scheduling / automation

- Systemd timers to enable (daily/weekly/monthly): __________________________
- Manual trigger cadence (if not automated): ________________________________
- Where should outputs be mirrored (if any): ________________________________

## Extensions / future work

- Additional affect markers beyond energy/focus/lightness: _________________
- Additional summary fields for backup reports: _____________________________
- Handoff target if FUTON3 owns salients merge: _____________________________
