# Joe's Active Context

## Current Mission

**M-structural-law** (futon3c) — Universal invariants as self-representing stack layer.
Three phases, ten Codex handoffs. All coding via GitHub issues.

## Active Patterns (PSR 2026-03-10)

Sigil string: `代内示功令马`

### Per-Handoff Patterns

| Sigil | Pattern | Guidance |
|-------|---------|----------|
| 代 | handoff-preserves-context | Each GitHub issue carries full context: :in/:out files, shapes, test expectations. No "you know what I mean." |
| 内 | scope-before-action | Sharp boundaries on every handoff. Codex works inside the box, not around it. |
| 示 | evidence-over-assertion | PRs come back with test counts and violation outputs, not "I think this works." |

### Coordination Patterns

| Sigil | Pattern | Guidance |
|-------|---------|----------|
| 功 | coordination-has-cost | Claude calls are expensive this week. Strategy only, no code review that tests can cover. |
| 令 | student-dispatch | Codex handoffs ARE student-dispatch: context, dead ends, directions, report format. |
| 马 | mission-lifecycle | Running M-structural-law through its phases — carry the pattern being instantiated. |

## Operational State

- Claude token budget: 93% used, resets in ~3 days
- Mode: Codex-primary, Claude for strategy only
- All 14 repos clean and pushed (futon-sync status: 2026-03-10)
- Server running refactored code (futon3c, port 6768 Drawbridge)
- Tickle + FM conductor default OFF at boot (use --start-tickle / --start-fm)

## What Landed This Session (2026-03-10)

- `proof_logic.clj` — core.logic invariant layer for Proof Peripheral (13 tests, 29 assertions)
- `agency/logic.clj` — core.logic invariant layer for Agency (10 tests, 20 assertions)
- `M-structural-law.md` — mission file for universal invariants
- `M-invariant-violations.md` — violation ledger (V-1..V-5 peripheral asymmetries)
- `README-done.md` — agent completion signalling documentation
- Bootstrap defaults changed: tickle/FM conductor off unless --start-tickle/--start-fm
- FM-001b proof state moved to futon6 (canonical location)
- All repos cleaned, committed, pushed
