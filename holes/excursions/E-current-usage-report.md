# Excursion: Current Usage Report — Claude + Codex token burndown

**Date opened:** 2026-04-25
**Entry point:** alongside `algorithms/apm-daily-batch.md`, the recurring
question "do I have budget headroom to slip in another APM problem?" had no
local answer. The Codex CLI publishes an authoritative `used_percent` per
session; Claude Code does not. Both write JSONL transcripts containing
enough raw token data to reconstruct the picture without invoking either
agent.

## What we built

Three layers, intentionally thin, all reading the same source-of-truth
JSONL files under `~/.claude/projects/-home-joe-code/` and
`~/.codex/sessions/YYYY/MM/DD/`:

1. **bb script** — `futon0/contrib/current-usage-report.bb`.
   Reads JSONL, sums per-session usage in a rolling window (default 300
   min = Codex's 5h cap), aggregates totals (Codex used-pct max across
   active sessions; Claude output sum). Emits human text by default,
   JSON via `--json`. Optional `--push` writes per-session burndown rows
   to `/api/alpha/mission-control` + `/api/alpha/evidence` on futon3c.
   No agent invocation, no JVM execution — pure local file parse.

2. **elisp glue** — `futon0/contrib/usage-report.el`.
   Caches the bb snapshot (`usage-report-cache-seconds`, default 30 s),
   exposes:
   - `usage-report-snapshot` — parsed plist, force-refreshable.
   - `usage-report-headline-string` — one-line `% free` summary.
   - `usage-report-headline-prefix` — the headline plus ` · ` for
     prepending to existing Arxana Browser headlines.
   - `usage-report-insert-stack-hud-block` — multi-line block for stack-hud.
   - `usage-report-show` — `M-x` interactive popup buffer.

3. **Two surfaces:**
   - **Stack HUD** `usage` block (registered in `stack-hud-blocks`,
     wired in `stack-hud--block-render-fn`): headline line + per-session
     detail + soft-cap footer.
   - **Arxana Browser** session views (`evidence-sessions`,
     `evidence-open-sessions`, `lab-sessions-active`,
     `lab-sessions-recent`): usage prefix prepended to the
     header-line, copying the `docbook-contents` non-editable headline
     pattern. This required teaching `arxana-browser--decorate-header-line`
     about a new constant
     `arxana-browser--description-in-header-views` so non-docbook views
     can also push their description into `header-line-format` rather
     than `mode-line-format` (the old default for non-docbook views,
     which left the description below the buffer where horizontal-clip
     hid the appended usage text).

## Comparability problem (and current resolution)

First headline shape was "Codex 3.0% used · Claude 227.7k out". Joe
flagged it as two non-comparable numbers floating in space — Codex was
fraction-of-cap, Claude was absolute count.

Resolution: render both as `% free`. Codex remains authoritative
(`100 - used_percent_max`). Claude is rendered against
`usage-report-claude-output-soft-cap` (defcustom, default 200 000) and
flagged with `*` to mark it as a heuristic, user-tunable reference rather
than an Anthropic-published cap. The stack-hud detail row spells out the
soft cap and how to change it, so the asymmetry is honest rather than
hidden.

The soft cap is the obvious open question. The default 200 k is a
ballpark guess; once a real cap-hit incident happens the calibration
target becomes concrete. Better-than-soft-cap would be: parse a
trailing-window of *successful* runs (last N days where no rate-limit
error fired) and use the 90th-percentile output as the cap. Deferred —
the heuristic is good enough to make the headline useful right now.

## Scoping decisions

- **No scheduling.** The bb script is cheap (≈1 s read, no API calls),
  the surfaces pull on demand, the cache keeps repeated views fast.
  A periodic agent would add operational surface area for no real win.
- **Per-session counts in stack-hud detail, not in Arxana row columns.**
  The Arxana session rows come from `/fulab/lab/sessions/active` whose
  session-id namespace doesn't necessarily line up with the bb script's
  (Claude file-uuid vs. Codex `session_meta` id vs. fulab's own
  enumeration). Bridging that requires a merge step that wasn't worth
  the cost for "should I dispatch?". Headline + stack-hud detail
  together cover the question.
- **`--push` left in but not relied on.** Pull-on-demand from both
  surfaces makes the burndown-as-evidence path redundant. The flag
  stays for the per-turn-evidence follow-on (next section) where the
  same shape will land in XTDB directly.

## Deferred / next

- **Per-turn usage annotations to the Evidence Landscape.** Joe's
  framing: "direct-to-XTDB API operation that doesn't need to run on
  the server, but could be coordinated from the server as we build
  Evidence units." The current bb script already has the per-session
  aggregator; the per-turn shape lives unaggregated in the same JSONL
  files. A futon1a `POST /entity` (or evidence-create) per turn, tagged
  with session-id and turn ordinal, gives the Evidence Landscape the
  data it needs to project burn rate per mission, per agent, per
  thread. This is where the soft-cap heuristic should be replaced by
  per-window posteriors from observed history.
- **Cap calibration from history.** Replace
  `usage-report-claude-output-soft-cap` with a function that reads
  recent transcripts and returns the 90th-percentile 5h-window output
  among runs that didn't hit a cap warning.
- **Per-session columns in the Arxana session table.** Bridge fulab
  session ids ↔ Claude/Codex JSONL session ids and add `out`, `used%`
  columns. Defer until the bridge is needed for another reason — the
  headline is enough on its own.

## Files

- `futon0/contrib/current-usage-report.bb`
- `futon0/contrib/usage-report.el`
- `futon0/contrib/futon-config.el` (require + defcustom-aware splice
  into `stack-hud-blocks`, plus inclusion in
  `my-chatgpt-shell-hot-reload-files`)
- `futon0/contrib/stack-hud.el` (block list + dispatch for `usage`)
- `futon4/dev/arxana-browser-core.el`
  (`arxana-browser--description-in-header-views` constant, new
  evidence-sessions / evidence-open-sessions / lab-sessions-* clauses
  in `arxana-browser--header-line`, mirrored branch in
  `arxana-browser--decorate-header-line`)
- `algorithms/current-usage-report.md` — algorithm doc, Step 1–6 spec.

## References

- `algorithms/apm-daily-batch.md` — sibling algorithm; the budget
  question this excursion answers gates its Step 3.
- `~/.claude/projects/-home-joe-code/*.jsonl` — Claude per-message
  `usage` blocks (input / cache_creation / cache_read / output).
- `~/.codex/sessions/YYYY/MM/DD/rollout-*.jsonl` —
  `event_msg/token_count` events with `rate_limits.primary`.
