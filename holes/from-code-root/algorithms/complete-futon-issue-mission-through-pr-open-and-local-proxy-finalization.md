# Algorithm: Complete Futon Issue Mission Through PR-Open and Local Proxy Finalization

Date: 2026-03-11
Status: active

## Authority routing

- futon-origin intake/write-up owner:
  `algorithms/create-futon-issue-before-triage.md`
- futon-origin triage/handoff owner:
  `algorithms/triage-futon-issue-and-decide-local-work-handoff.md`
- publication owner:
  `algorithms/publish-branch-pr-with-human-approved-evidence-comment.md`
- post-merge continuation owner:
  `algorithms/complete-publication-continuation-after-pr-merge.md`
- mission-closure owner:
  `algorithms/close-verified-mission-and-delegate-deferred-work.md`
- commit-phase checkpoint owner:
  `algorithms/conduct-commit-phase-with-diff-and-total-dirty-closure.md`
- issue/case system owner:
  `holes/missions/M-futon-full-system-operation-and-onboarding/M-world-backed-local-issue-case-system.md`

## Purpose

Use this when a futon-scoped mission has reached a locally verified repair
checkpoint and you want one composed routine that:

1. closes the mission record early in the completion pass
2. confirms triage state is complete for this futon-origin issue
3. publishes the repair branch to PR-open through the publication algorithm
4. associates the PR with the originating upstream issue
5. updates the local futon-origin proxy to PR-open publication state with
   triage and publication receipts
6. checkpoints mission/proxy updates in `gh`

This algorithm composes the publication algorithm; it does not replace it.

## Companion Flexiarg Resolution (2026-03-14)

Governing companion flexiargs for this algorithm:
- `library/social/operator-verification-gate-after-agent-triage-or-fix.flexiarg`
  - governs the locally verified repair checkpoint required before composed
    mission closure and publication
- `library/social/chat-derived-pr-evidence-with-human-preflight-review.flexiarg`
  - inherited from the PR-open publication family owner:
    `algorithms/publish-branch-pr-with-human-approved-evidence-comment.md`
- `library/social/futon-self-description-defers-to-installation-authority.flexiarg`
  - governs deference to the originating upstream futon issue while the local
    proxy records only local handoff/publication state

Inspected but non-owning candidates:
- `library/social/merged-pr-completes-publication-via-local-integration-reanchor.flexiarg`
  - governs the later merged-PR continuation, not the PR-open proxy-finalizing
    completion path owned here

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

## Inputs

- futon target repo name or path
- target branch
- base branch (`main` or `master`)
- publication title/summary and optional evidence-comment plan
- originating upstream issue reference (repo + issue number)
- mission file path for the futon-scoped repair
- local futon-origin proxy path under `world/futon-issues/<futon>/...`
- mission execution receipt (branch tip commit + local verification evidence)
- triage receipt or triage-complete marker in the local proxy

## Preconditions

1. The futon-origin proxy already exists and links the active futon-scoped
   mission.
2. The mission execution/acceptance checkpoint already exists on the target
   branch tip.
3. The operator has decided this mission is ready for completion closure.
4. The publication algorithm preconditions are satisfied.
5. The local proxy is still open for PR-open publication/follow-on tracking.

## Procedure

1. Re-open mission and proxy state before mutation.
   - confirm the branch tip commit and local verification receipt
   - confirm the mission file path and current mission status
   - confirm the originating upstream issue and local proxy path
2. Close the mission record early in this completion pass.
   - run:
     - `algorithms/close-verified-mission-and-delegate-deferred-work.md`
   - use the active mission path and current acceptance evidence from this run
   - if that algorithm classifies closure as blocked, stop the line and resolve
     the blocking acceptance gap before continuing
   - this algorithm does not re-own mission close semantics; it composes the
     canonical mission-closure owner
3. Confirm triage completion state in the local proxy.
   - verify the proxy has an explicit triage outcome and handoff record
   - if triage completion is missing, run:
     - `algorithms/triage-futon-issue-and-decide-local-work-handoff.md`
   - append/update local triage-completion receipt before publication updates
4. Publish through:
   - `algorithms/publish-branch-pr-with-human-approved-evidence-comment.md`
   - capture: branch URL/ref, PR URL/number, and association receipts
5. Verify PR-open publication state.
   - confirm PR exists and is open on the expected base/head
   - if PR surface is ambiguous, stop the line before proxy mutation
6. Update the local futon-origin proxy to PR-open state with publication
   receipts.
   - update `issue.md`:
     - current status/publication state = PR-open (not locally closed)
     - publication/association receipt summary
     - next continuation after merge (if merge has not occurred yet)
     - PR reference under related evidence
   - update `links.edn`:
     - canonical upstream issue link
     - concrete upstream PR link
   - append `events.ndjson`:
     - triage-complete confirmation event (if newly recorded in this run)
     - publication-progress event with PR-open receipt
7. Keep merge-time continuation explicit.
   - preserve the later continuation through:
     - `algorithms/complete-publication-continuation-after-pr-merge.md`
   - for futon repos, that continuation should explicitly include:
     - switch the local repo from the feature branch back to `main` or
       `master` after merge is observed
     - run:
       `algorithms/pull-futons-and-reconcile-active-branches.md`
     from that integration-branch posture
8. Checkpoint mission/proxy updates in `gh`.
   - run:
     - `algorithms/conduct-commit-phase-with-diff-and-total-dirty-closure.md`
   - include only intended mission/proxy artifacts in this checkpoint group
9. Record completion receipts.
   - mission closure receipt (mission path + status change)
   - triage completion receipt
   - publication receipt (PR URL/number)
   - proxy PR-open update receipt (paths updated + current status)
   - `gh` checkpoint commit id
   - next continuation condition (PR merged vs not merged)

## Decision Rule

Use this algorithm when all are true:

- the work is a futon-origin issue routed to a futon-scoped mission
- mission execution is locally complete enough to publish
- the local futon-origin proxy should stay synchronized with PR-open state

Use only the publication algorithm when:

- there is no futon-origin local proxy to finalize in this run
- publication is the only required operation boundary

## Outputs

- published branch + open PR with issue association
- mission record closed for this completed repair pass
- triage state explicitly complete in the local proxy
- local futon-origin proxy updated to PR-open state
- explicit post-merge continuation retained
- `gh` checkpoint commit for mission/proxy artifacts

## References

- publication owner:
  `algorithms/publish-branch-pr-with-human-approved-evidence-comment.md`
- merge continuation owner:
  `algorithms/complete-publication-continuation-after-pr-merge.md`
- mission closure owner:
  `algorithms/close-verified-mission-and-delegate-deferred-work.md`
- commit checkpoint owner:
  `algorithms/conduct-commit-phase-with-diff-and-total-dirty-closure.md`

## Notes

- This algorithm intentionally keeps duplicate-issue closure out of scope.
- Duplicate management should be handled by its own dedicated routine.
- This algorithm owns futon-origin proxy synchronization to PR-open state, not
  generic mission closure semantics.
