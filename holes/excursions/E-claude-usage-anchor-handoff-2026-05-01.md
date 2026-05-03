# Claude Review Handoff: usage-report Claude anchors

Date: 2026-05-01
Owner: Codex
Target reviewer: Claude

## What changed

Implemented a manual-anchor calibration path for Claude usage estimation in
`futon0/contrib/usage-report.el`.

Primary changes:

- multi-window snapshot caching so the same code can query both the current
  5-hour window and the weekly 7-day window
- manual anchor capture via:
  `M-x usage-report-capture-claude-anchor`
- local anchor storage in:
  `usage-report-claude-anchor-file`
  defaulting to `~/.emacs.d/usage-report-claude-anchors.eld`
- Claude estimated `% used` derived from the median implied cap over stored
  anchors
- headline / HUD rendering now prefers:
  `Claude est <current>% cur / <week>% week`
  when anchors exist

## Key code locations

- snapshot/cache refactor:
  `futon0/contrib/usage-report.el:64-113`
- anchor read/write and estimate helpers:
  `futon0/contrib/usage-report.el:119-239`
- manual capture command:
  `futon0/contrib/usage-report.el:241-273`
- headline fragment and main headline:
  `futon0/contrib/usage-report.el:299-343`
- HUD rendering:
  `futon0/contrib/usage-report.el:369-436`

Tests added:

- `futon0/contrib/test/usage-report-test.el`

## Current estimator assumption

The estimate uses `:weighted-input-equiv` by default
(`usage-report-claude-estimate-metric`), not `:output`.

Reasoning:

- output-only felt too narrow given Claude’s heavy cache traffic
- `weighted-input-equiv` is at least structurally closer to “resource burden”
  than raw output alone

This is still an assumption and is the main thing to challenge.

## Verified locally

Structural checks:

- `check-parens` passed for:
  - `futon0/contrib/usage-report.el`
  - `futon0/contrib/test/usage-report-test.el`
- batch load of `usage-report.el` succeeded

ERT:

- 5 tests passed in `usage-report-test.el`

Operator-path check:

- exercised `usage-report-capture-claude-anchor` in batch with a temp anchor
  file and stubbed snapshots
- initial empty-file bug was found and fixed
- seeded one real anchor into `~/.emacs.d/usage-report-claude-anchors.eld`
  using the operator-reported settings values `24% current`, `20% week`

Live headline after seeding:

```text
Codex 98% free · Claude est 24% cur / 20% week · resets 19:07Z
```

Sample rendered HUD output with one anchor:

```text
Usage:
  Codex 92% free · Claude est 24% cur / 20% week · resets 18:58Z
  Codex: 8.0% used  (max across 1 sess, resets 18:58Z)
    codex-se  used 8.0%  total 3.2M
  Claude: 1.2M out  (2 sess, 756 msgs, weighted 39.3M)
    est current: 24.0% used from 1 weighted anchor (cap 163.8M)
    est week:    20.0% used from 1 weighted anchor (cap 1271.3M)
    claude-s  out 1.2M  msgs 756
  Claude estimates use weighted caps derived from 1+ manual anchor in <temp>
```

## What I want checked

1. Is `:weighted-input-equiv` the right default estimator basis, or is
   `:output` actually more defensible/stable given Claude’s real usage page?
2. Is the median-cap approach structurally sane, or should it use a different
   fit rule once multiple anchors exist?
3. Does the operator UX read clearly, especially:
   - anchor capture command name and prompts
   - headline wording
   - HUD explanatory lines
4. Any hidden breakage from changing the cache model from a single cons cell to
   a per-window hash table?
5. Any reason the anchor file should be JSON/EDN instead of printed Lisp data?

## Known limitations

- No automatic scrape/API for `claude.ai/settings/usage`; anchors are manual.
- No live UI verification inside a normal Emacs session yet; only batch checks.
- Estimates are shown with as few as 1 anchor
  (`usage-report-claude-anchor-min-samples = 1`) so the feature is usable
  immediately after the first capture. This may be too eager.

## Suggested review actions

1. Read the changed code paths listed above.
2. Challenge the estimator basis (`weighted` vs `output`).
3. If the structure seems sound, try the command live:
   `M-x usage-report-capture-claude-anchor`
4. Confirm the stack HUD wording is understandable in practice.
