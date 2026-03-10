# Futon0 Protocol (Decisions + Open Questions)

This document captures decisions and configuration for the Futon0 rhythm/vitality
pipeline. Updated 2026-03-10 after the end-to-end quarterly demo using Stack HUD
summaries plus git-log fallback evidence.

## Decided

| Item | Decision |
|------|----------|
| Backup output root | `~/code/backups/` |
| FUTON1 handoff | yes — open-world capture enabled |
| Affect source | infer from open-world ingest (no new lifelogging) |
| Derivation discipline | derivative only — no new life logging added for these reports |
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

- Long-term source of truth: Evidence Landscape material rooted in `futon1a`
- Current demo/runtime path: `futon0.rhythm.affect`
  - `--entries-file` for exported Evidence Landscape JSON/JSONL
  - `--evidence-url` for live `futon3c` evidence queries
  - `--git-repo` fallback for retrospective demos before the Evidence Landscape is fully mapped
- Heuristic: infer affect-laden turns or commit episodes from existing text plus novelty-within-window follow-ons
- Acceptable lag: daily batch is fine; real-time is optional
- Constraint: this remains derivative telemetry, not a new self-reporting channel

## Progress signal

- Source: fulab/fubar events (futon3)
  - Session completions, pattern applications, evidence landing
  - Links to Prototype 4 (Trail & Proof-State Journal)
- Integration: futon0 reads `resources/fulab-events.edn` or equivalent

## Scheduling / automation

- Systemd timers: daily vitality, weekly salients, monthly audit
- Manual trigger: `scripts/futon0/rhythm/*.clj` when not automated

## Change tracking

- Keep a repo-local `CHANGELOG.md` for the reporting surface itself
- Record when new evidence sources come online, when report sections become operational, and when derivation logic changes materially
- Treat changelog entries as part of the protocol evidence trail: the system should be able to show not just current outputs, but how its attestation capacity improved over time

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

- [X] **futon1 ticket**: expose XTDB endpoint/path for affect inference
  - Needed for timestamp correlation with time envelope
  - Blocks: unclocked activity detection, affect heuristics
  - Now served via `futon0.rhythm.affect` (API-backed ingest to `affect.jsonl`)

## Future directions

### Derivative attestation targets

The report series should be able to attest to the following without any new life
logging. Each item must be derived from already-existing signals, not from a new
manual diary or dedicated tracking burden.

- Activity
  - Question: what do I actually do, day to day?
  - Likely sources: git history, HUD envelopes, recording ingest, session/evidence timestamps, salients snapshots
- Attention
  - Question: what kind of attention is my life already requiring and rewarding?
  - Likely sources: evidence turns, task/mission references, repo mix, file clusters, repeated topic terms
- Time-shape
  - Question: what rhythms am I actually living inside?
  - Likely sources: daily envelopes, weekly/monthly rollups, gaps, burst patterns, clock-time distributions
- Speech mode
  - Question: what kinds of speech am I already using most naturally and most often?
  - Likely sources: Evidence Landscape turns, commit language, Org headlines, prompt/response genre markers
- Social role
  - Question: what roles do I in fact keep enacting?
  - Likely sources: coordination turns, agent routing, review/implementation/teaching language, mission ownership patterns
- Energy effect
  - Question: what leaves me clearer, scattered, depleted, sharpened, boxed in?
  - Likely sources: affect transitions, pre/post envelope changes, follow-on artifact production, abandonment vs continuation
- Compatibility
  - Question: what in my present life already supports the continuation of the underlying practice?
  - Likely sources: recurring conditions that correlate with sustained work, stable rhythms, low-friction task re-entry, artifact completion
- Distortion
  - Question: what in my present life already asks me to suppress, fake, or overdevelop something?
  - Likely sources: repeated workaround language, fragmentation, defensive coordination, suppression/conflict markers, stalled or over-elaborated threads

Operational rule:

- These are attestation targets, not free-floating introspective themes.
- Every claim in the report should be traceable back to inspectable derivative evidence.
- If a target cannot be supported by existing traces, the right move is to state that the evidence is insufficient, not to add new logging pressure.

### Affect inference ↔ Active inference integration

- futon2/futon3 have active inference models (virtual hunger signals, etc.)
- futon0 affect inference could *ground* those models with real behavioral traces
- futon1 was intended to do some of this — may need another iteration
- Potential: build out a model of actual affects/perceptions that feeds the predictive layer
- Cross-reference: futon2/futon3 active inference work

## Optional experimental harness ticket (naturalistic protocol sketch)

Instrument two streams:

- Stream A: affect events / transitions
  - Explicit tags (fast)
  - Optional candidate detection via novelty-within-10-minutes
- Stream E: effectiveness signals
  - “New term” events (PlanetMath proxy)
  - “New artifact label” events (function names, namespaces, pattern IDs, commit tokens, TODO headings)
  - Lightweight activity markers (run, jump rope, rehearsal, etc.) if already logged

Define windows:

- Primary: very short (e.g., 10 minutes) to respect momentariness
- Secondary: medium (e.g., same day) for robustness checks only

Compute correlations with confounds in mind:

- Compare minutes/episodes with at least one affect event vs minutes/episodes without
- Rate of effectiveness events per unit time
- Within-person comparisons over weeks (baseline shifts)

Make “affect-surface online” measurable:

- Affect events per 100 turns
- Transitions with certificates per week
- Proportion of affect events that lead to a consequence candidate (even if curated later)

Interpretation discipline:

- You’re not proving “affect causes effectiveness.”
- You’re testing whether articulation of affect is associated with downstream actionable consequences and predicts better progress metrics.

What this buys you:

- Observational, longitudinal, within-subject (no lab)
- Compatible with the Spinoza/AIF stance:
  - Affect surface is a controllable interface layer
  - Effect side operationalized via novelty + artifacts
  - Link inspectable via certificates, not just statistics
