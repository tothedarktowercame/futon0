# M-capability-zones S2b handoff — discharge contracts declare artifact shape

**Status:** CLOSED — Artifact-shaped discharge contracts, backward compatibility, independent validation, and tests landed in futon2 commit `03d341d12`.

**From:** claude-3 (owner seat) → **To:** codex-2
**Context:** `:artifact-binding-mismatch` recurred across attempts 014/021/037/039 and has
never been the direct target of a landed repair (external Field Desk scan, 2026-07-21,
confirmed by checkpoints 3–4 of `futon0/holes/missions/M-capability-zones.md`). Two causes
observed: vague spec (014/021/037 — nothing authorable) and shape mismatch (039 — the
repair was a DATA deposit; the contract demands an in-attempt code commit, so an honest
author has nothing bindable and a dishonest one invents code; see reverted futon2 `63fcdb3`).
**Scope:** futon2 `repair_obligation.clj` + the runner's discharge-contract construction
(`full_loop_runner.clj` `discharge-contract` fn ONLY — nothing else in the runner).
Backward compatibility is the hard bar.

## Goal

Discharge contracts gain a declared artifact shape, so data- and spec-shaped repairs can
bind honest evidence instead of demanding code.

### D1 — contract field
- `:discharge-contract` gains `:artifact-shape` ∈ `#{:code-commit :data-deposit
  :spec-document}`. Absent field ⇒ `:code-commit` (every existing contract behaves
  byte-identically).

### D2 — shape-appropriate evidence validation
- `record-implementation!` and `resolve!` currently require `commit` (+ attempt-window
  binding downstream). Generalize the evidence field per shape:
  - `:code-commit` — exactly today's checks, unchanged.
  - `:data-deposit` — instead of a commit: `{:store-url ... :record-type ...
    :count-before N :count-after M :deposit-run-id ...}` — all required, counts must
    differ, and the validator READS the store to confirm the current count matches
    `:count-after` (read-only; that is the independent check).
  - `:spec-document` — instead of a commit: `{:path ... :git-sha ...}` — validator
    confirms the path exists and the sha is an ancestor commit touching it.
- The witness discipline (`:resolved?` `:dial-moved?`) is unchanged for all shapes.

### D3 — the runner writes shapes
- `discharge-contract` in the runner: repair classes whose findings come from data/spec
  faults may declare non-code shapes IF the failure-kind implies it; default remains
  `:code-commit`. Keep this minimal — a lookup map with today's kinds all `:code-commit`
  is acceptable for v1; the point is the FIELD exists end-to-end.

## Acceptance bar (demonstrate in bell-back)
1. Every existing test green, byte-identical behavior for contracts without the field.
2. New tests: each shape validates its evidence and rejects the others' (e.g. a
   `:data-deposit` contract rejects a bare commit; a `:code-commit` contract rejects
   a deposit map).
3. Read-only store verification test for `:data-deposit` (mock or live read — NO writes).
4. clj-kondo clean, check-parens OK
   (`emacs -Q --batch -l futon4/dev/check-parens.el --eval "(arxana-check-parens-cli)" -- --no-defaults FILES`).

## Constraints
No changes to `selection-discrimination`, stop-line preemption, or validation-line
auto-resolution. No live-store writes. Nothing outside this spec — questions come back
as a bell, not as improvisation.

## Delivery
Commit to futon2 (branch `M-propagators-ant-gate`). Bell claude-3 back with summary,
shas, test outputs, spec gaps.
