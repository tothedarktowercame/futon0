# Algorithm: Publish Verified Repair and Close World-Backed Issue

Date: 2026-03-08
Status: active

## Authority routing

- issue/case owner:
  `holes/missions/M-futon-full-system-operation-and-onboarding/M-world-backed-local-issue-case-system.md`
- issue triage/repair flow owner:
  `algorithms/process-world-backed-issue-through-triage-and-scheduled-repair.md`
- publication flow owner:
  `algorithms/publish-branch-pr-with-human-approved-evidence-comment.md`
- post-merge publication continuation:
  `algorithms/complete-publication-continuation-after-pr-merge.md`
- operator-verification gate:
  `library/social/operator-verification-gate-after-agent-triage-or-fix.flexiarg`
- post-mortem gate:
  `library/social/post-mortem-for-novel-learning-capture.flexiarg`

## Purpose

Use this when a repair mission has reached operator-verified fix status and you
want to close the world-backed issue and repair mission without over-claiming.

This algorithm owns the missing closure boundary between:

1. operator verification that the lived problem is fixed
2. publication of the fix when code-bearing branches must be published
3. post-mortem capture when the run produced novel learning

It intentionally reuses the existing publication algorithms rather than
re-describing PR flow inline.

The issue-local publication field in this algorithm is intentionally scoped as
`repair_publication_state`. It tracks where the case stands relative to repair
publication obligations. It is not a generic cross-lane publication contract
and does not replace concrete PR/branch/evidence artifacts or the GitHub
command request/response schemas.

## Companion Flexiarg Resolution (2026-03-14)

Governing companion flexiargs for this algorithm:
- `library/social/operator-verification-gate-after-agent-triage-or-fix.flexiarg`
  - inherited from the local world-backed issue lifecycle family owner:
    `algorithms/process-world-backed-issue-through-triage-and-scheduled-repair.md`
- `library/social/post-mortem-for-novel-learning-capture.flexiarg`
  - inherited from the local world-backed issue lifecycle family owner:
    `algorithms/process-world-backed-issue-through-triage-and-scheduled-repair.md`

Inspected but non-owning candidates:
- `library/social/extract-classical-automation-from-novel-evolver-work.flexiarg`
  - governs promotion of stable subprocedures revealed by the run, but not the
    direct verified-repair closure boundary
- `library/social/merged-pr-completes-publication-via-local-integration-reanchor.flexiarg`
  - governs the later merged-PR continuation, not the PR-open repair-closure
    procedure itself

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

## Inputs

- world-backed issue/case path
- repair mission path
- verification evidence from the operator
- set of affected repos and branches, if any
- decision about whether publication is required for this repair
- publication evidence once branch/PR publishing occurs

## Preconditions

1. The repair has already reached the operator-verification boundary.
2. If the repair changes publishable repo state, assume publication remains
   locally held by the active repair lane until branch publication has actually
   been executed to PR-open or equivalent accepted review surface.
3. For routine same-lane code publication, compose the publication algorithm
   directly from this algorithm. Treat that composition like a method call, not
   like a deferred continuation.
4. The only continuation that normally remains after closure is the later
   post-merge re-anchor step.
5. If publication is required and the target repo is a futon repo, the futon
   sync/reconcile gate should already have been satisfied for the active branch.

## Procedure

1. Observe the verified repair state.
   - Confirm the issue has reached operator-confirmed repair success, not only
     agent-side plausibility.
2. Classify publication need.
   - `not-needed` when the resolution was explanation-only, local-only, or
     otherwise does not require repo publication.
   - `required` when the repair lives on one or more code branches that should
     be published for review.
3. If publication is required and is still locally held, record the waiting
   state and invoke publication directly.
   - Set `current_status` to:
     - `operator-verified-awaiting-publication`
   - Set `repair_publication_state` to:
     - `awaiting-publication`
   - Keep `verification_state` at:
     - `operator-verified-fixed`
   - Keep the repair mission open.
   - Preserve the pending step:
     - publish each required branch and open the corresponding PR
   - If the target repo is a futon repo, the direct publication call must
     satisfy:
     - `algorithms/pull-futons-and-reconcile-active-branches.md`
     before push unless that effect was already achieved in the current session.
4. If publication is required now, reuse the publication algorithm directly.
   - For each affected repo/branch, run:
     - `algorithms/publish-branch-pr-with-human-approved-evidence-comment.md`
   - Let that publication algorithm own the concrete helper composition:
     - repo-aware transport via `agent_skills/git-github-repo.bat`
     - branch push + PR creation via `agent_skills/gh-pr-create.bat`
     - reviewed evidence-comment posting via
       `agent_skills/gh-pr-comment-file.bat`
   - Treat multi-repo publication recursively:
     - each required branch must satisfy the publication step
      - the overall repair is not fully published until all required branches
        have an open PR or equivalent accepted review surface
5. If publication was already executed earlier in the same run, normalize the
   case to the publication outputs rather than reopening the waiting state.
   - Collect the existing PR URL, branch URL/ref, and evidence-comment URL, if
     available.
6. Record publication outputs in the world-backed case.
   - PR URL
   - branch URL/ref
   - evidence-comment URL, if one was posted
   - `repair_publication_state`:
     - `pr-open`
7. Decide closure completion from the current lane.
   - If publication was not needed:
     - keep `repair_publication_state = not-needed`
   - If publication was required and is still locally held:
     - keep the repair mission open
     - keep `current_status = operator-verified-awaiting-publication`
     - do not mark the issue or mission complete yet
   - If publication was already executed:
     - record `repair_publication_state = pr-open`
8. Run the post-mortem novelty gate.
   - If the run was routine replay, record that ordinary evidence is enough.
   - If novel learning occurred, promote the lesson into algorithms, docs,
     missions, flexiargs, or other durable artifacts before claiming the lane
     learned from the run.
9. Mark the repair mission and case closure state.
   - repair mission may now become `Completed` when one of these is true:
     - publication was not needed
     - publication was already executed to PR-open or equivalent
   - world-backed issue may now claim:
     - `current_status = operator-verified-fixed`
   - `operator-verified-fixed` does not by itself imply that publication is
     complete
   - if publication was executed, keep:
     - `repair_publication_state = pr-open`
10. Leave the post-merge continuation explicit.
   - If a PR was opened, keep the next continuation:
     - `algorithms/complete-publication-continuation-after-pr-merge.md`
   - PR-open publication does not erase the later local re-anchor continuation

## Outputs

- world-backed issue with explicit `repair_publication_state`
- repair mission that stays open while publication is still locally held
- PR-open publication evidence when publication is required
- explicit post-merge continuation once publication has reached PR-open
- post-mortem decision recorded as:
  - routine replay, or
  - novel learning with promoted artifacts

## Notes

- This algorithm does not replace the publication algorithm; it composes it.
- This algorithm also does not restate raw helper invocation details; those now
  live in the publication algorithm and its governed helper surfaces.
- `repair_publication_state` is issue-lane coordination metadata, not the
  authoritative publication fact model for every lane.
- The point is to keep the issue-system closure rule honest:
  - operator verification proves the lived fix
  - if the same repair lane still owns commit/push/PR work, it must invoke the
    publication algorithm directly before claiming completion
  - publication proves the fix has actually entered review/publication flow
  - post-mortem proves the novel lesson was either promoted or intentionally
    left as ordinary evidence
- For routine code-bearing repairs, `repair_publication_state =
  awaiting-publication` means the lane is not yet closeable.
- The later PR merge / local re-anchor step remains a continuation. The initial
  branch publication step does not.
