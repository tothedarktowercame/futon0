# Algorithm: Check Approved PR and Dispatch Merge Continuation

Date: 2026-03-10
Status: active

## Authority routing

- publication-open owner:
  `algorithms/publish-branch-pr-with-human-approved-evidence-comment.md`
- merged-PR continuation owner:
  `algorithms/complete-publication-continuation-after-pr-merge.md`
- preferred PR surface inspection helper:
  `agent_skills/gh-pr-status.bat`
- upstream halt-on-ambiguity discipline:
  `futon3/library/futon-theory/stop-the-line.flexiarg`

## Purpose

Use this when you learn that a previously published PR has been approved and
you want a governed next-step check instead of ad hoc guessing.

This algorithm does not treat approval as merge. Its job is:

1. confirm the PR review state from an authoritative source
2. check whether the PR has actually merged yet
3. if merged, invoke the merged-PR continuation
4. if not merged, record that the PR is approved-but-still-open and stop cleanly

## Companion Flexiarg Resolution (2026-03-14)

Governing companion flexiargs for this algorithm:
- `library/social/merged-pr-completes-publication-via-local-integration-reanchor.flexiarg`
  - inherited from the merged-PR continuation family owner:
    `algorithms/complete-publication-continuation-after-pr-merge.md`
- `library/system-architecture/deterministic-evidence-authority.flexiarg`
  - local dispatcher delta: approval does not imply merge, so the continuation
    trigger must be checked against authoritative PR state

Inspected but non-owning candidates:
- `library/social/chat-derived-pr-evidence-with-human-preflight-review.flexiarg`
  - same non-owner read as the merged-PR continuation family owner

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

## Inputs

- target repo name or path
- PR selector or published branch name
- approval evidence source
- canonical integration branch if already known (`main` or `master`)

## Preconditions

1. A PR-based publication episode already exists.
2. Approval should be observed from an authoritative source:
   - GitHub review UI/notification
   - `gh` query
   - future GitHub webhook ingress
3. Do not assume approval implies merge.

## Procedure

1. Observe the approval event.
   - Record how the approval was observed.
   - Confirm which repo and PR/branch the approval applies to.
2. Query authoritative PR status.
   - Preferred helper surface:
     - `agent_skills/gh-pr-status.bat <repo-name-or-path> [branch]`
   - Confirm whether the PR is:
     - still open
     - approved or otherwise review-ready
     - merged
     - merely closed without merge
3. Classify the result conservatively.
   - If the PR is merged:
     - run:
       `algorithms/complete-publication-continuation-after-pr-merge.md`
   - If the PR is approved but still open:
     - record that the publication episode remains at PR-open/approved
     - leave the merge continuation pending
     - stop without local branch mutation
   - If the PR is closed without merge, or the review state is not actually
     approved, stop and ask the operator.
4. Record the continuation state.
   - approval evidence source
   - authoritative PR status observed
   - whether the merged-PR continuation was invoked
   - if not invoked, what exact condition is still pending

## Stop Conditions

Stop and ask the operator if any of these happen:

- PR status is ambiguous
- the approval event cannot be matched to a single published PR
- the PR is closed without merge
- the status source disagrees across surfaces

## Decision Rule: When This Algorithm Is The Right Tool

Use this algorithm when all of the following are true:

- the branch was already published to PR-open
- you have just learned that the PR is approved or appears approved
- the immediate question is whether the merged-PR continuation should run now

Use the merged-PR continuation algorithm instead when:

- the PR is already known to be merged from an authoritative source

Use the publication algorithm instead when:

- the branch has not yet been published to PR-open

## Outputs

- authoritative classification of the approved PR as:
  - approved and still open
  - merged
  - ambiguous / closed-without-merge
- invocation of the merged-PR continuation when appropriate
- explicit retention of the merge continuation when approval alone is all that
  happened

## Notes

- Approval is a useful observation event, but merge is the mutation trigger for
  local re-anchor.
- This algorithm is intentionally a dispatcher/check step, not a second
  competing publication-completion algorithm.
