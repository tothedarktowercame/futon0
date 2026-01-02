# Futon0 Protocol (Decisions + Open Questions)

This document captures decisions and configuration for the Futon0 rhythm/vitality
pipeline. Updated 2026-01-02 toward Prototype 1 readiness.

## Decided

| Item | Decision |
|------|----------|
| Backup output root | `~/code/backups/` |
| FUTON1 handoff | yes — open-world capture enabled |
| Affect source | infer from open-world ingest (no new lifelogging) |
| Sync cadence | fixed weekly time |
| WIP cap | >3 active tracks triggers pause |
| Tai Chi intention prompt | removed — no value without evidence |
| Wearable data | future option — no device yet, interface slots ready |

## Org sources for salients

- Legacy paths (reference only):
  - `~/code/futon3/holes/aob.org` (contains 🥨 sections)
  - `~/code/futon3/holes/orpm.org`
- Migration target: `futon3/library/workflow-coherence/*.flexiarg`
- Seed patterns created:
  - `weekly-rhythm.flexiarg` — 7-day sync check
  - `sphere-equilibrium.flexiarg` — 5 spheres balance
  - `wip-cap.flexiarg` — ≤3 active threads
- Future work: migrate remaining salients from aob.org as patterns

## Affect markers from open-world ingest

- Source: FUTON1 open-world capture (XTDB or equivalent)
- Path/endpoint: TBD — needs futon1 documentation check
- Heuristic: infer from typing activity / session patterns
- Code update: may be required to emit affect-relevant signals
- Acceptable lag: daily (batch with vitality scanner)

## Progress signal

- Source: fulab/fubar events (futon3)
  - Session completions, pattern applications, evidence landing
  - Links to Prototype 4 (Trail & Proof-State Journal)
- Integration: futon0 reads `resources/fulab-events.edn` or equivalent

## Scheduling / automation

- Systemd timers: daily vitality, weekly salients, monthly audit
- Manual trigger: `scripts/futon0/rhythm/*.clj` when not automated

## Work bounds (time envelope)

### Sleep/wake
- Wake: ~07:30
- Sleep: ~23:30

### Day job (M-F)
- Hours: 09:30-17:30
- Light stack activity permitted (ChatGPT turns, pattern notes, ZoomR4 import)
- Heavy coding: **not during these hours**

### Stack work window
- Hours: 17:30-23:00 (possible, not obliged)
- Tai chi: 2 nights/week (optional attendance)

### Morning routine
- 4-track recording: 1 hour
- Walking commute: 30 min

### Correlation
- ChatGPT turns and XTDB hits are **timestamped**
- Cross-correlate activity vs. time envelope to detect:
  - Heavy work during day job hours (warning)
  - Sleep/wake drift
  - Time-of-day productivity patterns

## Experiment config

- Based on **fulab pattern clock-ins** — no separate config needed
- Patterns you clock in on become the active experiment set
- Weights/windows derive from pattern metadata (sigils, factors)

### Unclocked activity warning

- ChatGPT turns / XTDB hits without an active clock-in = **drift signal**
- Threshold: N unclocked turns triggers HUD warning ("100 turns not clocked")
- Not punitive — just awareness that work isn't landing against a pattern

## Dependencies on other layers

- [ ] **futon1 ticket**: expose XTDB endpoint/path for affect inference
  - Needed for timestamp correlation with time envelope
  - Blocks: unclocked activity detection, affect heuristics

## Future directions

### Affect inference ↔ Active inference integration

- futon2/futon3 have active inference models (virtual hunger signals, etc.)
- futon0 affect inference could *ground* those models with real behavioral traces
- futon1 was intended to do some of this — may need another iteration
- Potential: build out a model of actual affects/perceptions that feeds the predictive layer
- Cross-reference: futon2/futon3 active inference work
