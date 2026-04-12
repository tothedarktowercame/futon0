# War Machine — Upgrade Guide

The War Machine is an assemblage, not a finished product. It breaks down and
rebuilds as the stack evolves. This document explains how to extend it.

## Architecture

```
scan-* functions  →  observe  →  judge  →  render
   (data)          (12-channel)  (priorities,   (markdown,
                    vector)       losses, G)     hex views)
```

**Engine** (`scripts/futon0/report/war_machine.clj`): scans, observation
vector, judgement layer, markdown renderer.

**Visualiser** (`scripts/futon0/report/war_machine_visual.clj`): Swing hex
grid with four views (Stack, Missions, Invariants, Patterns).

**Emacs** (`emacs/war-machine.el`): `M-x war-machine` runs the engine via
Babashka and renders markdown in a buffer.

## Adding a new scan source

Every scan function follows the same pattern:

```clojure
(defn scan-X
  "Scan [description]. Returns {:key data ...}."
  [days]  ;; or [] if no window needed
  (let [raw (fetch-or-read-data)]
    {:summary ...
     :details ...}))
```

Steps:
1. Add `scan-X` to `war_machine.clj` (after existing scans, before the renderer)
2. Call it in `generate-war-machine` and add to `scan-data` map
3. If it should feed the observation vector, add a channel to `observation-channels`
   and update `observe`
4. If it should produce priorities, add a `X-priorities` function and include
   it in `judge`

## Adding a new AIF head

The war machine reads AIF heads — it doesn't run them. To integrate a new head:

1. Expose the head's state via an HTTP endpoint (e.g. `GET /api/alpha/X/state`)
2. Add a query in `scan-aif-heads` that reads the endpoint
3. The head's judgement (mode, urgency, recommendation) appears in the
   priorities list and the AIF Heads section

Currently integrated:
- Portfolio Inference (`/api/alpha/portfolio/state`)
- Mission AIF Head (code exists, no HTTP endpoint yet)
- Session AIF Head (not yet built)

## Adding a new visualiser view

The visualiser cycles through views via a toggle button.

1. Add `assign-X-layout` function that returns `[{:node {...} :q q :r r} ...]`
2. Add sprite type(s) to `draw-hex-sprite` (fill, border, label, badge)
3. Add detail text to `node-detail-text`
4. Add the view mode keyword to the toggle cycle in `visualize`
5. Add the view branch to `paintComponent`'s `case` on `view-mode`

Current views: `:stack`, `:missions`, `:invariants`, `:patterns`

## Adding invariant checks

The war machine queries the invariant runner via Drawbridge `/eval`. To add
a new domain:

1. Create `futon3c/src/futon3c/X/logic.clj` with `build-db` and
   `query-violations` functions
2. Add the domain to `invariant-domain-registry` in `http.clj`
3. The war machine's `load-invariant-inventory` will pick it up automatically

To add a candidate invariant family to the model:
1. Add the family to `futon4/futon-stack-invariant-model.edn` under `:families`
2. The Arxana Browser and the visualiser's Invariants view will show it

## Connecting a structural graph (future)

When a code-review-graph style structural graph exists:

1. Add `scan-structural-graph` that queries the graph (via MCP or HTTP)
2. The graph nodes become a fifth visualiser view
3. PSR/PUR annotations on graph edges would connect patterns to code paths
4. The judge could trace invariant violations through the graph to the
   responsible pattern selection

## Key design properties

**Read-only observer** (WM-I1): The war machine never writes to the stack.
It reads and displays. Sovereignty remains with the operator.

**Stacked judgement** (not fiat): The judge composes signals from multiple
AIF heads. It doesn't override them — it surfaces where they agree, disagree,
or are missing.

**Extensible without redesign**: New scans, heads, views, and invariant
domains slot in via the existing patterns. No central coordinator decides
what's included — the war machine reads whatever is available.

**The machine breaks down**: Per Deleuze and Guattari, the war machine is
not a State apparatus. It doesn't aim for perfection or completion. It
observes flows, detects capture, and reports. When it breaks (a scan fails,
an endpoint goes dark, an invariant runner throws), the other parts keep
working. The gaps are visible as dark hexes and missing data — which is
itself useful information.

## Files

| File | Purpose |
|------|---------|
| `scripts/futon0/report/war_machine.clj` | Engine: scans, observe, judge, render |
| `scripts/futon0/report/war_machine_visual.clj` | Swing hex visualiser (4 views) |
| `emacs/war-machine.el` | `M-x war-machine` Emacs command |
| `futon5a/data/war-machine-exotype.edn` | AIF+ wiring diagram |
| `futon5a/data/war-machine-terminal-vocabulary.edn` | Preferences (C), free energy (G) |
| `futon4/futon-stack-invariant-model.edn` | Invariant layer/family model |
| `futon3c/docs/structural-law-inventory.sexp` | Candidate invariant registry |
| `futon3c/holes/missions/M-war-machine.md` | Mission document with checkpoints |
