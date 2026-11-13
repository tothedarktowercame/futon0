# Algorithm: Close Verified Mission and Delegate Deferred Work

Date: 2026-03-10
Status: active

## Authority routing

- mission creation/checklist owner:
  `docs/mission-creation-checklist.md`
- upstream lifecycle owner:
  `futon4/holes/mission-lifecycle.md`
- local operator-verification gate:
  `library/social/operator-verification-gate-after-agent-triage-or-fix.flexiarg`
- commit/checkpoint owner:
  `algorithms/conduct-commit-phase-with-diff-and-total-dirty-closure.md`
- constraint-evaluation owner:
  `algorithms/how-to-evaluate-constraints.md`

## Purpose

Use this when a mission appears ready to close and the system needs to mark it
complete without over-claiming.

This algorithm owns the missing closure boundary between:

1. evidence that the mission's acceptance criteria were actually satisfied
2. explicit operator approval to treat that evidence as enough for closure
3. honest delegation of remaining work to parent, child, sibling, or follow-on
   missions instead of silently leaving open scope behind

It exists to prevent two common closure failures:
- closing the wrong mission in a parent/child neighborhood
- marking a mission complete while leaving still-in-scope work disguised as
  vague future notes

## Companion Flexiarg Resolution (2026-03-14)

Governing companion flexiargs for this algorithm:
- `library/social/operator-verification-gate-after-agent-triage-or-fix.flexiarg`
  - governs the requirement that mission closure waits for explicit operator
    agreement on the evidence
- `library/social/stable-mission-files-with-sibling-artifact-trees.flexiarg`
  - governs mission-local artifact topology and helps preserve honest closure
    routing across mission-owned children and sibling artifacts

Inspected but non-owning candidates:
- `library/social/reverse-morphogenesis-for-retroactive-mission-reconstruction.flexiarg`
  - governs retroactive mission creation and chronology honesty, not the direct
    close/delegate boundary once a mission already exists

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

## Inputs

- candidate mission path
- explicit mission neighborhood:
  - parent missions
  - child missions
  - direct dependencies / feeds-into edges
  - mission-owned docs or runbooks, if any
- current acceptance criteria and live status text
- concrete evidence:
  - tests
  - receipts
  - docs/run logs
  - operator observations
- operator close approval

## Preconditions

1. The candidate mission already exists and is the current owner being
   discussed.
2. The explicit mission neighborhood has been inspected before changing
   mission status.
3. The operator has either already verified the evidence or is available to
   do so now.
4. The mission is not being closed merely because the current coding wave
   wants a checkpoint.

## Procedure

1. Reconfirm the closure owner.
   - Inspect the explicit mission neighborhood first.
   - Decide whether the mission being discussed is actually:
     - the mission that should close now
     - a child that should close while the parent stays open
     - a parent that can close only after one or more children do
     - not closeable yet because the active open work is still in-scope here
   - Do not close a parent merely because one implementation child completed.
2. Re-read the mission against its own scope and acceptance criteria.
   - Read:
     - `Goal`
     - `Scope In`
     - `Scope Out`
     - `Acceptance Criteria`
     - current status / checkpoint sections
   - Check each acceptance criterion against concrete evidence, not memory.
   - Compose:
     `algorithms/how-to-evaluate-constraints.md`
   - For each acceptance criterion, determine whether it is:
     - satisfied
     - partially satisfied
     - unsatisfied
   - If a criterion is partially satisfied or unsatisfied but still aligned
     with the mission's purpose/scope, stop closure and route back out of this
     algorithm into implementation/evidence work rather than weakening the
     criterion.
   - If a criterion appears misaligned with the mission's purpose/scope, do
     not change it here; stop and ask the operator whether they want to change
     the constraint.
3. Classify remaining work honestly.
   - `blocking` if the work is still inside this mission's scope/acceptance.
   - `deferred-nonblocking` if the work is explicitly out-of-scope or already
     owned by another mission/parent/follow-on.
   - `ambiguous` if ownership is unclear.
   - If any remaining work is `blocking`, stop and keep the mission open.
4. Obtain or confirm operator verification.
   - Do not change the mission to complete until the operator explicitly
     agrees that the evidence is enough for closure.
   - Preserve the operator identity/date in the closure note when that
     verification is what justifies status change.
5. Update the mission file explicitly.
   - Change the status line to a close form such as:
     - `Status: Complete (verified by <operator>, YYYY-MM-DD)`
     or the local simplified equivalent already used in that mission lane.
   - Add a `Close Decision` section when one does not already exist.
   - Record:
     - what evidence was accepted
     - why the mission is closeable now
     - who verified the close decision, when relevant
   - Add a `Deferred Items` section when non-blocking remainder exists.
   - Keep deferred items explicit enough that a later agent can tell why they
     did not block closure and where they belong now.
6. Update neighboring owner surfaces when needed.
   - If a child mission closed but the parent remains open, update the parent
     so it explicitly says:
     - the child is complete
     - the parent remains open for the broader remainder
   - If mission-owned docs or runbooks were acting as closure/status owners by
     accident, point them back to the mission instead of duplicating the close
     judgment.
7. Preserve reopen legibility.
   - If the mission had a prior false close or a meaningful reopen event, keep
     that reopen history explicit instead of rewriting the mission as though it
     closed cleanly the first time.
   - If later evidence shows the mission was closed too early, reopen it
     honestly rather than hiding the earlier mistake.
8. Record reused vs new and perform the local orthogonality pass.
   - Name which close-owner pattern or prior mission shape was reused.
   - Name any new closure algorithm, doc note, or neighboring updates added.
   - Use:
     `algorithms/conduct-knowledge-orthogonalization-pass.md`
   - Tighten surrounding surfaces so one place owns the close decision instead
     of repeating status prose in multiple artifacts.
   - If the remaining question is specifically devmap wiring or floating-vs-
     wired mission topology, route that narrower follow-on into:
     `algorithms/decide-follow-on-orthogonality-and-devmap-wiring-after-mission-reconstruction.md`
     instead of treating it as the general owner.
9. If the operator wants a checkpoint now, route into the commit algorithm.
   - Use:
     `algorithms/conduct-commit-phase-with-diff-and-total-dirty-closure.md`
   - Mission closure is not itself the commit algorithm; it composes it when a
     checkpoint is wanted.

## Outputs

- mission file updated to an honest closed state, or an explicit decision that
  the mission remains open
- explicit deferred-item list for non-blocking remainder
- adjacent parent/child/doc surfaces updated when closure changes ownership
  interpretation
- optional checkpoint continuation via the governed commit algorithm

## Stop Conditions

Stop and ask the operator if:

- it is unclear whether the current mission or one of its neighbors should
  close
- at least one acceptance criterion still lacks concrete evidence
- the remaining work cannot be clearly classified as blocking versus delegated
- the closure would rely on hand-wavy “probably good enough” reasoning rather
  than explicit operator verification

## References

- `docs/mission-creation-checklist.md`
- `holes/missions/mission-template.md`
- `futon4/holes/mission-lifecycle.md`
- `library/social/operator-verification-gate-after-agent-triage-or-fix.flexiarg`
- `algorithms/conduct-knowledge-orthogonalization-pass.md`
- `algorithms/decide-follow-on-orthogonality-and-devmap-wiring-after-mission-reconstruction.md`
- `algorithms/conduct-commit-phase-with-diff-and-total-dirty-closure.md`
- `algorithms/how-to-evaluate-constraints.md`
- `algorithms/complete-futon-issue-mission-through-pr-open-and-local-proxy-finalization.md`
- `holes/missions/M-eal-language-spec/M-eal-session-stack-runtime-model/M-vscode-client-irc-parity-implementation.md`

## Notes

- This algorithm is the canonical mission-closure owner in this repo.
- For futon-origin runs that also need publication and local proxy PR-open
  synchronization, compose this algorithm through:
  `algorithms/complete-futon-issue-mission-through-pr-open-and-local-proxy-finalization.md`
  rather than duplicating mission-close semantics inline.
