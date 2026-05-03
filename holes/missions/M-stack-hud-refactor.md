# Mission: Stack HUD Refactor — ant's-eye view, posframe-native

**Date:** 2026-04-25
**Status:** PARTIAL (Phase 1 shipped 2026-04-25; Phases 2–4 deferred)
**Owner:** Joe
**Cross-ref:**
- `futon0/analysis/excursions/E-stack-hud-cleanup.md` — full audit + port manifest (working doc)
- `futon3c/holes/missions/M-repl-wins-over-cli.md` Checkpoint 3 — entry point and excursion ledger
- `futon3c/src/futon3c/evidence/invariant.clj` (`I-evidence-per-turn`) — the persistence guarantee the HUD's evidence-probe surfaces

## 1. Motivation

The legacy Stack HUD (`futon0/contrib/stack-hud.el`, ~2343 lines, with
`stack-doc.el` ~416 lines for sidewindow line-docs) had drifted: it
accumulated 14 blocks, several of which rendered stale or broken data
(`boundary`, `musn`, `pattern-sync`), some of which had no clear decision
they helped Joe make, and all of which were rendered into a *side window
inside the active Emacs frame*. The doc system was a centralised
line-property lookup that drifted from the code that produced the line.

The single trigger for this refactor was the M-repl-wins-over-cli
Checkpoint 3 incident: evidence had been silently going to a volatile
in-memory atom for days while a "happy" stack-hud-1 displayed nothing
that would have caught the failure. The HUD was rendering, looked fine,
and contributed zero to detecting that the system underneath had quietly
stopped doing the thing it was supposed to do. **A HUD that does not help
catch silent failures is not a HUD; it is decoration.**

This mission reframes the Stack HUD around a single question — *what
decisions does it help me make?* — and ports each surviving block into
a posframe-native, frame-isolated, widget-owned architecture that
collocates "what does this show?" with the code that shows it.

## 2. Scope

### 2.1 Scope in

- A new HUD surface (`stack-hud-2`) rendered in a dedicated full-screen
  Sway-floated Emacs frame, triggered by ⁂ (U+2042 ASTERISM).
- A widget library (`stack-hud-widgets.el`) where each widget owns its
  data fetcher, render function, doc string, and tunable defcustoms in
  one place; testable in isolation via `M-x stack-hud-widget-render-NAME`.
- Per-widget docs displayed via posframe child frames, replacing
  `stack-doc.el`'s sidewindow lookup.
- A decision-driven audit (E-stack-hud-cleanup §1–6) that scores every
  legacy block against six decision-categories and produces a port
  manifest with priorities.
- A live evidence-accumulation probe (`futon0/scripts/futon0/vitality/scanner.bb`,
  committed in `8ea799a`) that surfaces XTDB writes per session
  without any LLM cost.
- The legacy HUD (`stack-hud.el` / "stack-hud-1") preserved unchanged
  during the transition so material can be ported deliberately.

### 2.2 Scope out

- **Per-agent HUDs.** Affect, AIF signals, mana, EFE candidates — these
  are agent-side ant's-eye views and re-enter M-aif-head territory.
  Deferred to a follow-up excursion. The decision-category framing
  (§3) is intentionally projectable onto the per-agent surface so that
  follow-up work is an instantiation, not a redesign.
- **War Machine modifications.** War Machine is the strategic-overview
  surface; the HUD is the operational ant's-eye view. They cross-reference
  but do not duplicate. No changes to War Machine in this mission.
- **A "great unification" of futon* HUDs.** futon0 is by design the
  cross-futon interface layer; the HUD legitimately depends on data from
  many futons. A wiring diagram of those dependencies is a deliverable
  here, not a single-store architecture.
- **Renaming `stack-hud.el` → `stack-hud-1.el` on disk.** The rename is
  conceptual; coexistence is intentional during the port window. The
  physical rename happens once stack-hud-2 has subsumed enough material.

## 3. The decision-category framework (audit §1)

The HUD earns its place by answering, at a glance, questions Joe asks
himself between sessions. The six categories:

1. **Did the basics happen since I last looked?** Sanity check —
   evidence accumulating, sessions running, services up, git active.
2. **Where is recent work landing?** Which futons saw activity, which
   sessions are alive, what got committed.
3. **What patterns are firing?** Top-N most-activated patterns from
   futon3a semantic search — surfaces emergent themes.
4. **Which surface is silent that shouldn't be?** The alarm dimension
   — reazon-checked relations like "evidence delta = 0 in active turn
   window."
5. **What's queued for me?** Reminders, blockers, mission gates.
6. **Is something rotting?** Stale data, dead links, sections that
   haven't moved. The HUD spotting itself becoming unreliable.

Every block is scored against these six. Blocks that serve no category
are dropped, regardless of plumbing health.

## 4. Three-level cross-check

Each block is evaluated at three levels:

| Level         | Surface               | Audience      | Question                                      |
|---------------|-----------------------|---------------|-----------------------------------------------|
| Joe-HUD       | stack-hud-2           | Joe           | Does this answer a §3 decision for Joe?       |
| War Machine   | `make war-machine`    | Joe + agents  | Is this already covered by the overview?      |
| M-aif-head    | (deferred per-agent)  | each agent    | Would this same shape help a single agent?    |

The cross-check is bookkeeping, not a gate: it surfaces whether a block
is general infrastructure (suitable for per-agent reuse) or Joe-specific.
This keeps M-aif-head's deferred Phase 2 in view as we work, so the
per-agent HUD can be an instantiation of the same widget shapes rather
than a re-architecture.

## 5. Architecture

### 5.1 Files

| File | Role |
|------|------|
| `futon0/contrib/stack-hud-2.el` | Frame management, block dispatch, ⁂ binding, Sway window-rule documented in commentary |
| `futon0/contrib/stack-hud-widgets.el` | Widgets (data fetcher + render + doc per widget); posframe popup mechanism; `?` keybinding |
| `futon0/contrib/stack-hud.el` | Legacy HUD (stack-hud-1), preserved unchanged |
| `futon0/contrib/stack-doc.el` | Legacy line-doc sidewindow, preserved while -1 is alive |
| `futon0/scripts/futon0/vitality/scanner.bb` (and `.clj`) | Vitality scanner with the evidence-accumulation probe |
| `futon0/analysis/excursions/E-stack-hud-cleanup.md` | The audit + port manifest |

### 5.2 Composition

```
                  ⁂ (global key)
                       │
                       ▼
              stack-hud-2-toggle
                       │
                       ▼
           ┌──────────────────────────┐
           │  Dedicated Emacs frame   │
           │  name = "stack-hud-2"    │
           │  fullscreen = fullboth   │
           │  Sway: floating + fs     │
           └────────────┬─────────────┘
                        │
                        ▼
                 *stack-hud-2* buffer
                        │
              renders blocks in order:
                        │
       ┌────────────────┼─────────────────┐
       │                │                 │
       ▼                ▼                 ▼
  briefing-summary  sessions  patterns  session-activity
   (W4, no-LLM)     (W3, ✓)   (W1)      (W2)
       │                │                 │
       ▼ data           ▼ data            ▼ data
   Arxana cache     XTDB probe       XTDB probe
   + W2 deltas      + Arxana cache   /api/alpha/evidence
                                     since=...
```

At any block, `?` calls a posframe with the block's doc string.
`stack-hud-widgets.el` ships docs for all four widgets keyed by an
invisible text property attached to each headline.

### 5.3 Sway integration

```sway
# ~/.config/sway/config
for_window [title="^stack-hud-2$"] floating enable, fullscreen enable
```

Match by Emacs frame title. The HUD floats above the tile layout so
opening or closing it does not disturb existing window numbering or
tiling.

### 5.4 Cross-futon dependencies (wiring map)

| From      | To             | What         |
|-----------|----------------|--------------|
| futon0 (scanner) | futon1a (HTTP :7071) | evidence count + latest_at probes |
| futon0 (HUD widgets) | futon1a (HTTP :7071) | per-pattern, per-session, sessions-in-flight live data |
| futon0 (HUD widgets) | futon4 (Arxana cache) | session summary cache pull (no LLM) |
| futon0 (HUD widgets) | futon3c (REPL buffers in same Emacs) | fallback open-session enumeration when cache empty |
| futon0 (HUD widgets) | futon3a (indirect, via context-retrieval evidence) | pattern IDs and titles |
| futon0 (legacy HUD) | futon3 / git / filesystem | direct fs reads (vitality, git, tatami, …) |
| futon0 (legacy HUD) | futon5a | reminder tracker (when alive) |
| futon0 (legacy HUD) | futon3c (HTTP :7070) | services check |

## 6. Status snapshot — what shipped 2026-04-25 (Phase 1)

Already in stack-hud-2:

- ⁂ globally bound to `stack-hud-2-toggle`; opens/raises/closes a dedicated
  fullscreen frame. Sway window-rule documented in
  `stack-hud-2.el` commentary.
- Four widgets live with real data:
  - `Patterns` — top-N activated patterns from context-retrieval
  - `Session Activity` — evidence delta decomposed by session-id
  - `Recent Sessions` — top-3 most recently active, with cached
    Arxana summaries plus a per-session live probe for accurate
    count + `[since-first, since-latest]` ages
  - `Briefing Summary` — synthesised across cached sessions weighted
    by recent-evidence × age decay, no LLM per refresh
- `?` toggles a posframe doc popup keyed off a text property on each
  headline; navigates to the right widget regardless of where the
  cursor is.
- Posframe self-resolves on load if not on `load-path`
  (`stack-hud-widget--ensure-posframe`).
- Posframe colour: dark navy background (`#0b1a33`), lighter navy
  border (`#1f3a66`), 10px internal padding. All defcustoms.
- Audit doc complete (`E-stack-hud-cleanup.md` §0–§7).

## 7. Completion criteria

The mission is complete when:

1. **All blocks have a §3 decision** — every visible block in the new
   HUD points at a category from §3 in its own doc.
2. **Drops landed** — boundary, musn, pattern-sync removed from the
   stack-hud-1 defcustom (rendered or not, gone).
3. **Voice and hot-reload ported** with toggle-button support added
   to `stack-hud-2-mode-map`.
4. **Services, git, vitality, liveness, reminders ported** as widgets
   with the same widget-owned doc shape.
5. **`focus` resolved** — either ported (data populated) or dropped
   (data dead).
6. **Layer 2 alarm aggregator** registers at least one reazon check
   (e.g. `evidence-monotonic-since-scan`) and renders a top-of-HUD
   ribbon when violations fire.
7. **stack-hud.el → stack-hud-1.el rename completed** and consumer
   `(require)` calls updated.
8. **stack-doc.el retired** for widget blocks; remains only for any
   non-widget warnings still produced by the legacy paths during
   transition.
9. **Cross-futon dependency map (§5.4) committed alongside the
   completed remediation** so the wiring is documented at the point
   of stabilisation.
10. **Per-agent HUD excursion logged** as the natural follow-up,
    referencing M-aif-head's deferred Phase 2.

## 8. Phases and remaining work

Phase 1 (DONE 2026-04-25): Audit, framing, four widgets, posframe docs,
frame management, ⁂ binding, evidence-accumulation probe, this mission
file.

Phase 2 (next session, sprint 1 of E-stack-hud-cleanup §6):
services + git + liveness + verify-or-drop focus + execute the dead-block
drops + war-machine cross-link replacing boundary. ~2–3 hours.
Architecturally trivial.

Phase 3 (sprint 2): button support in stack-hud-2-mode, then voice and
hot-reload ports. ~2 hours.

Phase 4 (sprint 3): vitality + reminders. ~2–3 hours.

Phase 5 (sprint 4): full briefing as a separate command, alarm
aggregator with reazon. ~half-day.

Phase 6 (close): rename stack-hud.el → stack-hud-1.el, retire
stack-doc.el for widget blocks, log per-agent HUD excursion, mark
mission COMPLETE.

## 9. Relationship to other missions

- **M-repl-wins-over-cli** — entry point. Checkpoint 3 reported the
  evidence-pipeline incident that motivated the audit. This mission
  resolves the third excursion logged off that checkpoint
  (E-stack-hud-cleanup).
- **M-aif-head** — deferred Phase 2 of M-aif-head was per-agent AIF
  heads (observation channels, default-mode tier). The decision-category
  framing here is intentionally portable to that surface; once Phase 6
  closes, a sibling excursion can project these widget shapes onto
  per-agent HUDs.
- **E-evidence-viewer-deep-dive** (sibling excursion off M-repl-wins-
  over-cli Checkpoint 3) — covers the evidence-viewer at
  `:7071/evidence-viewer` and the Arxana row-cache freshness leak.
  Independent of this mission but shares data sources.
- **War Machine** — explicit non-overlap. The strategic synthesis
  surface remains as-is; the HUD links to it via a one-line block
  rather than duplicating.

## 10. Notes for the next session

- The new HUD frame's title is `stack-hud-2`. Sway rule already
  documented in `stack-hud-2.el` commentary; if Joe hasn't added it
  yet, the frame opens but tiles instead of floating.
- Widget data fetchers all rely on `FUTON1A_URL` (default
  `http://localhost:7071`). If the daemon needs to talk to a remote
  futon1a, set the env var before opening Emacs.
- The session probe limit defaults to 200 entries. Sessions with more
  show `200+ turns`; bump `stack-hud-widget-session-probe-limit` if
  needed.
- Per-block doc strings live in `stack-hud-widgets.el` under
  `stack-hud-widget-docs`; new blocks should add an entry there at the
  same time as their render function.
- `stack-hud-2-blocks` defcustom controls render order. Rearrange in
  init if the default order doesn't match the morning glance pattern.
