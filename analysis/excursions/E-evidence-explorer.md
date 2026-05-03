# Excursion: Evidence Explorer

**Date:** 2026-04-09
**Entry point:** M-repl-wins-over-cli session → context retrieval notifications → "how do I query this back?"

## What This Is

The REPL now logs `context-retrieval` evidence entries on every A->B turn:
each entry records the turn number, the protopattern query (user prompt +
agent response), and the top-k futon3a pattern matches with scores. These
entries are certificates — proof that the turn was observed and its patterns
identified.

This excursion covers the **read side**: querying the evidence chain to
inspect past turns, their retrieved patterns, and the trajectory of pattern
selection over time. The goal is to make the evidence landscape navigable
from within the REPL, completing the write/read loop.

## Moves

1. **Evidence query function (Emacs-side)**
   - `C-c C-e` or similar keybinding in claude-repl / codex-repl.
   - Completing-read over turn numbers or pattern IDs.
   - Results render in a `*context*` buffer in the HUD frame (sliding
     blackboard pattern, cyborg layer).

2. **Query modes**
   - **By turn**: "show patterns retrieved for turn N" -> lookup
     `context-retrieval` evidence where `turn=N`.
   - **By pattern**: "which turns surfaced `tension-detection`?" -> filter
     evidence entries where results contain that ID.
   - **Reverse lookup**: "given this pattern, show the A->B exchanges it
     matched" -> training data for pre-hoc selection.
   - **Timeline**: "what patterns came up today?" -> filter by timestamp,
     visualise pattern frequency.

3. **Pre-hoc pattern selection (future)**
   - Once enough post-hoc observations accumulate, build a predictive model:
     given the conversation trajectory so far, which patterns are likely
     relevant for the *next* turn?
   - This is the transition from observation (post-hoc) to active inference
     (pre-hoc) in the AIF loop.
   - Blocked by: sufficient data volume from post-hoc retrieval.

## Connection to Click 2

The `context-retrieval` evidence entries are the observation channel that
Click 2 needs for the `recording -> P(creative-boost)` edge. Each turn's
retrieved patterns are an affective/cognitive fingerprint — the system's
best guess at what happened in the exchange. Over time, the correlation
between retrieved patterns and session outcomes (commits, evidence quality,
session duration) becomes the training signal for the Bayesian model.

## Tests

- Evidence store contains `context-retrieval` entries with correct structure
  (turn number, query, results with scores).
- Query function retrieves entries by turn number and by pattern ID.
- `*context*` HUD panel renders query results without stealing focus.
- Pre-hoc prediction (when built) surfaces patterns *before* the turn with
  measurable relevance improvement over random baseline.

## Residue

- Navigable evidence landscape accessible from the REPL.
- Training dataset for pre-hoc pattern selection.
- Shared `*context*` HUD panel infrastructure reusable for other retrieval
  surfaces (e.g., git history, mission state, code search).
