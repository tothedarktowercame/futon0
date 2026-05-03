# Excursion: REPL Evidence Turns

**Date:** 2026-04-07
**Entry point:** E-Vasilopita residue → M-repl-wins-over-cli → Joe HUD evidence pane

## What This Is

E-Vasilopita reopened inhabitation for claude/codex sessions, but the HUD and
mission schema still treat "REPL evidence" as anecdotal. This excursion closes
the sorry around **turn-count telemetry** so that the Agency evidence store,
Joe HUD, and the portfolio conductor all see the same statistic. It is the
work needed before Click 2 (recording → creativity) can trust its priors.

## Moves

1. **Advance M-repl-wins-over-cli → TESTING**
   - Confirm the claude (`cr`) + codex (`cx`) launchers now satisfy the entry
     friction condition documented in [E-Vasilopita](/home/joe/code/futon0/analysis/excursions/E-Vasilopita.md).
   - Update mission metadata (status, `:verification-notes`) so mission control
     stops treating the CLI fallback as blocking evidence capture.

2. **Thread turn statistics into Agency evidence**
   - Extend the claude/codex REPL instrumentation ( `futon0/scripts/cr`,
     `futon0/scripts/cx`, `futon0/agent-chat/claude-repl.el`,
     `futon0/agent-chat/codex-repl.el`) so every turn emits `{session-id,
     agent-id, surface, turn-index, timestamp}`.
   - Ensure the evidence forwarder writes per-session turn counts plus rolling
     24h / 7d aggregates (needed later for Joe HUD + Joe HUD’s per-timescale
     cards).

3. **Surface the metric on Joe HUD + conductor inputs**
   - Update [`joe_hud.clj`](/home/joe/code/futon0/scripts/futon0/report/joe_hud.clj)
     so the Evidence column shows: (a) current session turns; (b) sessions today
     vs. rolling window; (c) pointer into the Evidence Landscape for quick
     review.
   - Expose the same metric via the Agency API and record it as an observation
     channel for `futon3c/portfolio/observe.clj` so the conductor sees evidence
     throughput as a signal when ranking REPL-surface missions.

## Tests

- Mission control inventory shows M-repl-wins-over-cli in TESTING with notes
  referencing `cr`/`cx` launch behaviour.
- Agency evidence explorer lists turn-count documents keyed by session ID and
  surface.
- Joe HUD renders the new stat without degrading the other three HUD columns.
- Portfolio logs show the added observation channel, even if policy logic
  ignores it initially.

## Residue

- Structured telemetry schema for turn counts, ready for Click 2 modelling.
- Shared vocabulary for "inhabitation rate" at the REPL layer (sessions per
  day, turns per session, last turn timestamp).
- Confidence that the REPL-vs-CLI sorry is permanently closed because the data
  that proves it stays visible to both humans (HUD) and the conductor.
