# Algorithm: Current Usage Report — Per-Session Token Burndown

Date: 2026-04-25
Status: active
Owner mission: M-apm-solutions (futon3c) — sibling to apm-daily-batch
Related: algorithms/apm-daily-batch.md
Implementation: `~/code/futon0/contrib/current-usage-report.bb`

## Purpose

Quickly answer: **how much Claude and Codex budget do I have left in the
current rate-limit window, and which sessions are spending it?** so that
another APM problem (or any other compute-heavy task) can be slipped in
at a suitable juncture without hitting a hard cap mid-batch.

Per-session granularity — not just an aggregate — so that successive
runs can attribute burn to specific sessions and the rate of consumption
becomes visible across the day rather than being a single point estimate.

## Design

The computation is **purely local file parsing**. Both Claude Code and
Codex CLI write JSONL session transcripts containing token usage numbers
that are sufficient to answer the question. No model invocation, no
slash command, no JVM execution — Babashka reads the files directly.

The Arxana Browser → Sessions section is reached through HTTP to
futon3c's existing endpoints (`/api/alpha/mission-control` and
`/api/alpha/evidence`). The bb script is the HTTP client; the JVM is
just the receiving side. This keeps the read path independent of the
JVM (still works if futon3c is down) and the write path optional (`--push`).

Per-turn evidence-landscape annotations (direct-to-XTDB) are deferred —
this algorithm is the "per-session report" predecessor that proves out
the data shape and burndown UX before that work begins.

## Prerequisites

1. `bb` (Babashka) on PATH. Verify: `bb --version` ≥ 1.12.
2. `~/.claude/projects/-home-joe-code/*.jsonl` — Claude Code 2.x default
   session transcripts.
3. `~/.codex/sessions/YYYY/MM/DD/rollout-*.jsonl` — Codex CLI 0.12x
   default session rollouts.
4. (For `--push` only) futon3c HTTP reachable on `http://localhost:7070`.
   Probe: `curl -sS http://localhost:7070/health`.

## Inputs

- `~/.claude/projects/-home-joe-code/*.jsonl` — each assistant message
  carries `.message.usage.{input_tokens, cache_creation_input_tokens,
  cache_read_input_tokens, output_tokens}`. The session id is the
  filename (UUID).
- `~/.codex/sessions/YYYY/MM/DD/rollout-*.jsonl` — `event_msg` events
  with `payload.type=="token_count"` carry
  `payload.rate_limits.primary.{used_percent, window_minutes, resets_at}`
  and `payload.info.total_token_usage.{input_tokens, cached_input_tokens,
  output_tokens, reasoning_output_tokens, total_tokens}`. Session id
  comes from the `session_meta` event's `payload.id`.

## Procedure

### Step 1: Run the bb script

Read-only (default — prints to stdout):

```bash
~/code/futon0/contrib/current-usage-report.bb
```

With burndown push to Arxana Browser → Sessions:

```bash
~/code/futon0/contrib/current-usage-report.bb --push
```

Other flags:

- `--window MIN` — rolling window in minutes (default 300 = the Codex
  5-hour window).
- `--verbose` — include all fields in stdout.
- `--help` — usage banner.

### Step 2: Read the report

Output structure:

```
=== Current Usage Report — <iso-now> ===
Window: last 300 min  (since <iso-since>)

Codex sessions in window (N):
  codex   <short-id>  used=X.X%  resets=<iso>  total=<n>  (<last-iso>)
  ...

Claude sessions in window (M):
  claude  <short-id>  msgs=<n>  in=<n>  cw=<n>  cr=<n>  out=<n>  weighted=<n>  (<last-iso>)
  ...
```

Codex `used=X.X%` is authoritative — directly from the API rate-limit
header. `resets` is the wall-clock time at which the 5-hour window
clears. `total` is total tokens accounted for in this Codex session.

Claude has no `used_percent` — the columns are raw sums:
- `msgs` — assistant turns in window.
- `in` — fresh input tokens.
- `cw` — cache-write (billed at 1.25×).
- `cr` — cache-read (billed at 0.1×).
- `out` — output tokens.
- `weighted` — `cw·1.25 + cr·0.1 + in`, a structural budget feel (not
  a price quote).

### Step 3: Decide

- **Codex `used_pct` ≥ 80%** anywhere: do not dispatch a Codex sorry-
  cleanup pass. Defer to `resets_at`. Claude work can still proceed.
- **Codex `used_pct` ≤ 50%** *and* Claude `out` total < 30k in window:
  safe to slip in another APM problem (≈ 1 Claude turn + 1 Codex turn).
- **Claude `out` total ≥ 50k** *or* `msgs` ≥ 60 across all sessions in
  window: heavy interactive day. Defer batch work and let the cap relax.
  Heuristic — refine after a week of burndown data.
- **No Codex `token_count` events in window**: Codex idle. Claude
  decision applies alone.

### Step 4: (with `--push`) Verify the Sessions row

The push step does, per active session:

1. `POST /api/alpha/mission-control` with `action: start`,
   `session-id: usage:<source>:<short-id>`, idempotent — already-existing
   sessions are reused, not duplicated.
2. `POST /api/alpha/evidence` with `session-id` matching the burndown
   session, `claim-type: usage-snapshot`, `tags: [usage burndown
   <source>]`, body = the full per-session record.

After push, the Arxana Browser → Sessions tab shows one row per active
upstream session, prefixed `usage:claude:` or `usage:codex:`. Each row's
evidence list is the time series of snapshots; the body of each
evidence entry is the full record so the detail view can render a
sparkline of `used_pct` over time without further schema work.

Verify the latest run landed:

```bash
curl -sS -X POST http://localhost:7070/api/alpha/mission-control \
  -H "Content-Type: application/json" \
  -d '{"action":"sessions"}' \
  | jq -c '.sessions[] | select(."session-id" | startswith("usage:"))'
```

## Decision Rules

(See Step 3 — kept inline so the algorithm stays single-pass-readable.)

## Outputs

Per run:
- One stdout report (always) with one row per Claude and Codex session
  active in the window.
- (with `--push`) One mission-control session per active upstream
  session in the Arxana Sessions tab, plus one evidence entry per run
  inside each, body containing the full snapshot.

## Cadence

Run on demand before deciding to dispatch a heavy task. For continuous
visibility, schedule via `/loop` or `/schedule` at 15-minute intervals
during active hours — that gives ~20 burndown points per Codex window,
enough for a per-session sparkline.

The script is cheap (no model calls, no JVM execution) so cadence can be
aggressive without budget concern.

## Stop Conditions

- This is a per-invocation algorithm; it has no completion criterion of
  its own.
- Retire and rewrite when either (a) Claude Code adds an authoritative
  rate-limit endpoint comparable to Codex's `rate_limits.primary`, or
  (b) per-turn usage annotations are landed directly to XTDB (the
  deferred follow-on), at which point the Sessions-tab snapshot pattern
  is replaced by live evidence-landscape queries.

## Emacs Entry Point (sketched)

A minimal `M-x current-usage-report` binding shells out to the bb
script and pops the report into a side buffer. Pseudocode:

```elisp
(defun current-usage-report (&optional push)
  (interactive "P")
  (let* ((args (if push '("--push") '()))
         (cmd (mapconcat #'shell-quote-argument
                         (cons "/home/joe/code/futon0/contrib/current-usage-report.bb"
                               args)
                         " "))
         (buf (get-buffer-create "*usage-report*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (shell-command-to-string cmd))
      (goto-char (point-min)))
    (display-buffer buf)))
```

Placement to be decided when wiring the binding — likely
`futon3c/emacs/usage-report.el` alongside other operator HUDs.

## References

- `~/code/futon0/contrib/current-usage-report.bb` — implementation.
- `futon3c/src/futon3c/transport/http.clj:3109` — mission-control RPC
  (start/step/stop/sessions).
- `futon3c/src/futon3c/transport/http.clj:1099` — evidence-create endpoint.
- `~/.codex/sessions/YYYY/MM/DD/rollout-*.jsonl` — Codex `token_count`
  events with `rate_limits.primary`.
- `~/.claude/projects/-home-joe-code/*.jsonl` — Claude per-message
  `usage` blocks.
- `algorithms/apm-daily-batch.md` — sibling algorithm; this one is a
  precondition check before dispatching its Step 3.
