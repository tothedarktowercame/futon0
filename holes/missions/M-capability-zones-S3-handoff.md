# M-capability-zones S3 handoff — readiness that wakes, not shrugs

**From:** claude-3 (owner seat) → **To:** (first free of codex-1/codex-2; set at dispatch)
**Evidence base:** ledger kinds `agent-unavailable` ×3 (attempts 004, 012, 025/027 window)
and `substrate-unavailable` ×1 (attempt-028); plus 2026-07-21: post-OOM agents in Agency
status `restored` answered a manual whistle-ping immediately and went `idle`, but the
runner's readiness had already recorded `agent-unavailable` sorries end-to-end
(cohort-41 attempt-001). The failure family is UNCLAIMED by any catalog pattern; this
slice is its mechanical repair and its future candidate-pattern's first revision cycle.
**Scope:** futon2 runner readiness phases ONLY (`full_loop_runner.clj` agent-readiness /
substrate-preflight and their helpers). No changes to selection, EFE, tripwires,
contracts, cohort protocol.

## Goal

The readiness phase should attempt the cheap recovery it can prove works before
declaring unavailability — and record what it tried, typed.

### D1 — wake-before-fail for agents
- When a required agent's roster status is `restored` (invoke-ready but not claiming
  availability), send ONE whistle ping (`POST /api/alpha/whistle`, short timeout ~30s,
  same payload shape as a manual ping) and re-check status. `idle` after ping ⇒ proceed.
- Statuses `idle`/`invoking` behave exactly as today. Unreachable/no-reply after the
  single ping ⇒ today's unavailable path, unchanged.
- The phase payload records the attempt, typed: `:readiness/wake-attempted true`,
  `:readiness/wake-result :woken | :no-reply | :not-needed` — evidence, not narration.

### D2 — bounded retry for substrate preflight
- On substrate health-check failure: retry twice with 5s spacing before the sorry.
- Success on retry ⇒ proceed, but record `:readiness/substrate-transient true` in the
  phase payload (a transient recovered is DATA — do not hide it; the ledger and any
  future tripwire may want the rate).
- Still failing after retries ⇒ today's `substrate-unavailable` path, unchanged.

### D3 — detail without vocabulary breakage
- The failure KINDS (`agent-unavailable`, `substrate-unavailable`) are ledger vocabulary;
  do NOT rename or split them (empirics compatibility). Add a `:failure-detail` field on
  the sorry payload: `:unreachable | :restored-unwoken | :busy | :transient-exhausted`.

## Acceptance bar (demonstrate in bell-back)
1. Unit test with a mocked roster: `restored` + successful ping ⇒ phase :ok with
   `:wake-result :woken`; `restored` + no reply ⇒ unavailable with
   `:failure-detail :restored-unwoken`.
2. Unit test mocked substrate: fail-fail-succeed ⇒ :ok + `:substrate-transient true`;
   fail×3 ⇒ sorry with `:failure-detail :transient-exhausted`.
3. Idle-agent and healthy-substrate paths byte-identical (regression fixture).
4. No live-store writes; no whistles to real agents from tests (mock the send fn —
   inject it like the runner's other `-fn opts`).
5. clj-kondo clean; check-parens OK
   (`emacs -Q --batch -l futon4/dev/check-parens.el --eval "(arxana-check-parens-cli)" -- --no-defaults FILES`);
   existing runner tests green.

## Constraints
Injection over hardcoding: the ping/health calls go through opts-overridable fns like
the runner's existing `:repair-*-fn` pattern, so tests never touch the mesh. ONE ping,
TWO retries — no loops, no unbounded waits (a readiness phase must stay seconds-scale).
Nothing outside this spec; questions come back as a bell.

## Delivery
Commit to futon2 (branch `M-propagators-ant-gate`). Bell claude-3 back with summary,
shas, test outputs, spec gaps.
