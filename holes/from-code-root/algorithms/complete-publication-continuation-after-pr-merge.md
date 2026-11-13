# Algorithm: Complete Publication Continuation After PR Merge

Date: 2026-03-08
Status: active

## Authority routing

- publication flow owner:
  `algorithms/publish-branch-pr-with-human-approved-evidence-comment.md`
- approval-time dispatcher:
  `algorithms/check-approved-pr-and-dispatch-merge-continuation.md`
- broader futon sync/reconciliation owner:
  `algorithms/pull-futons-and-reconcile-active-branches.md`
- publication completion policy:
  `library/social/merged-pr-completes-publication-via-local-integration-reanchor.flexiarg`
- upstream halt-on-ambiguity discipline:
  `futon3/library/futon-theory/stop-the-line.flexiarg`
- preferred repo-aware transport helper:
  `agent_skills/git-github-repo.bat`
- future event-plane neighbor:
  `holes/missions/M-mfuton-familiar-typed-surface-world-and-memory/M-my-familiar-coordination-forge/M-github-webhook-typed-surface-ingress.md`

## Purpose

Use this when a branch you previously published as a PR has now been accepted
and merged, and you want to complete the stored publication continuation by
returning the local repo to the canonical integration head.

For the straightforward case, this means:

1. observe that the PR really merged into `main` or `master`
2. switch the local repo back to that integration branch
3. for futon repos, run the normal futon pull/reconcile algorithm from that
   posture; otherwise fast-forward the integration branch so the local repo now
   reflects upstream truth

This algorithm owns the post-merge continuation for one publication episode.
For futon repos it composes the broader pull/reconcile algorithm; for other
repos it may use the narrower direct fetch/fast-forward path.

## Companion Flexiarg Resolution (2026-03-14)

Governing companion flexiargs for this algorithm:
- `library/social/merged-pr-completes-publication-via-local-integration-reanchor.flexiarg`
  - governs the core claim that PR-open publication is incomplete until the
    local repo is re-anchored on the canonical integration branch
- `library/social/extract-classical-automation-from-novel-evolver-work.flexiarg`
  - governs promotion of the repeatable post-merge local re-anchor step into a
    reusable algorithm/helper path

Inspected but non-owning candidates:
- `library/social/chat-derived-pr-evidence-with-human-preflight-review.flexiarg`
  - governs PR-open evidence posting, not the later merged-PR continuation

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

Publication-continuation family note:
- this algorithm is the shared family owner for the merged-PR continuation
  posture
- approval-time dispatchers that decide whether to invoke this continuation
  should inherit this merged-PR companion and name only their additional local
  delta explicitly

## Inputs

- target repo path
- PR identifier or merge evidence link
- canonical integration branch (`main` or `master`)
- current local branch state
- operator policy for whether old feature branches should be retained, deleted,
  or left untouched

## Preconditions

1. The PR merge should be observed from an authoritative source:
   - GitHub UI/notification
   - `gh` query
   - future GitHub webhook event ingress
2. Do not assume "closed PR" means "merged PR"; confirm the merge result.
3. Do not discard local branch tips or uncommitted work implicitly.

## Procedure

1. Observe the merge event.
   - Confirm the PR merged, not merely closed.
   - Confirm the destination integration branch.
2. Run the local complexity gate.
   - If the local repo has uncommitted changes, a detached HEAD, ambiguous base
     branch, stacked dependent branches, or a force-updated remote history,
     perform a call with continuation and stop for operator review.
3. Switch to the canonical integration branch.
   - Usually `main` or `master`.
4. Refresh remote truth from the integration-branch posture.
   - For futon repos, prefer the canonical local sync posture:
     `algorithms/pull-futons-and-reconcile-active-branches.md`
   - For other repos, the narrower direct refresh path is still acceptable:
     - fetch the repo's remote state
     - preferred helper surface:
       `agent_skills/git-github-repo.bat <repo-name-or-path> fetch --all --prune`
     - then fast-forward only:
       `merge --ff-only origin/<integration-branch>`
     - do not create an incidental merge commit here
5. Verify the local re-anchor result.
   - Confirm local `main`/`master` now includes the merged PR result.
   - Confirm the local branch is clean.
   - Confirm the branch tracks the intended upstream integration head.
6. Decide what to do with the published feature branch.
   - Leave it in place if further evidence, backport, or branch-local work is
     still live.
   - Delete or archive it only with explicit intent.
7. Record the continuation completion.
   - publication episode now closed through post-merge local re-anchor
   - repo path
   - integration branch
   - merge evidence source
   - any retained/deleted feature branch decision

## Stop Conditions

Stop and ask the operator if any of these happen:

- PR status is ambiguous (`closed` vs `merged`)
- unclear integration branch
- dirty working tree
- stacked branch topology where local branch pointers may need backup/repointing
- force-updated or rewound remote branch history
- fast-forward is not possible

## Decision Rule: When This Algorithm Is The Right Tool

Use this algorithm when all of the following are true:

- a PR-based publication episode already happened
- the later merge event is the continuation trigger
- the immediate goal is to return the local repo to the upstream integration
  head cleanly, usually by first switching back to `main` or `master`

Use the broader pull/reconcile algorithm instead when:

- the work is a general futon refresh episode rather than a post-merge
  continuation for one published branch
- multiple unrelated active futon branches need a broader judgment pass

## Call with continuation note

For this post-merge boundary, a call with continuation means:

1. observe the merge event but stop before mutation because local topology is
   ambiguous or risky
2. preserve the pending step:
   - `switch local repo to <integration-branch> and fast-forward to upstream merged head`
3. record the exact blocker and resume condition
4. later resume that same re-anchor step rather than treating the repo as a
   fresh unrelated sync episode

## Outputs

- local repo re-anchored to `main` or `master`
- explicit completion of the publication continuation
- retained/deleted/archived branch decision recorded
- explicit halt if branch topology makes the re-anchor unsafe

## References

- publication-side case study:
  `docs/futon3-pr-publication-continuation-case-study.md`
- stacked realignment case study:
  `docs/futon1-stacked-pr-sync-case-study.md`

## Notes

- This algorithm treats PR merge as a continuation event, not as unrelated new
  work.
- "Accepted" here means merged into the canonical integration branch, not only
  approved by a reviewer.
- For futon repos, the default practical move after merge is:
  - switch back to `main` or `master`
  - run `algorithms/pull-futons-and-reconcile-active-branches.md`
- That means the post-merge continuation and the futon refresh/reconcile step
  are composed here rather than treated as unrelated work.
- When the narrower single-repo re-anchor path is sufficient, prefer the
  repo-aware transport helper for the fetch step rather than reconstructing
  ad hoc shell transport setup.
- Future GitHub event ingress can detect the merge event, but the downstream
  local action should still remain typed and explicit.
