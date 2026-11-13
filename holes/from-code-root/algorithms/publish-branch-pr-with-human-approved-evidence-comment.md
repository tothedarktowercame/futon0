# Algorithm: Publish Branch + PR + Human-Approved Evidence Comment

Date: 2026-03-06
Status: active

## Authority routing

- automation extraction and post-mortem promotion:
  `library/social/extract-classical-automation-from-novel-evolver-work.flexiarg`
- publication completion after merge:
  `library/social/merged-pr-completes-publication-via-local-integration-reanchor.flexiarg`
- routine control should move away from ad hoc interpretation:
  `library/social/typed-command-surface-over-eval.flexiarg`
- architecture authority for routine control:
  `library/system-architecture/typed-command-control-plane-authority.flexiarg`
- LLM/client role is adapter, not authority:
  `library/system-architecture/uniform-service-fabric-spine.flexiarg`
- evidence ordering:
  `library/system-architecture/deterministic-evidence-authority.flexiarg`
- upstream path discipline:
  `futon3/library/futon-theory/proof-path.flexiarg`
- upstream halt-on-ambiguity discipline:
  `futon3/library/futon-theory/stop-the-line.flexiarg`
- post-merge completion routine:
  `algorithms/complete-publication-continuation-after-pr-merge.md`
- approval-time status-check routine:
  `algorithms/check-approved-pr-and-dispatch-merge-continuation.md`
- futon-origin composed mission/proxy completion owner:
  `algorithms/complete-futon-issue-mission-through-pr-open-and-local-proxy-finalization.md`
- preferred helper surfaces:
  - `agent_skills/gh-pr-status.bat`
  - `agent_skills/gh-pr-create.bat`
  - `agent_skills/gh-pr-comment-file.bat`
  - `agent_skills/git-github-repo.bat`
- post-open reviewer request should stay in the same GitHub PR publication
  `agent_skill` family, preferably through an MCP-backed surface when that
  lane supports it

## Purpose

Use this when a code branch is ready to publish and you want a repeatable,
reviewer-friendly submission flow with human approval before posting a detailed
PR evidence comment.

## Companion Flexiarg Resolution (2026-03-14)

Governing companion flexiargs for this algorithm:
- `library/social/chat-derived-pr-evidence-with-human-preflight-review.flexiarg`
  - governs the reviewer-facing evidence-comment posture and human preflight
    requirement
- `library/system-architecture/deterministic-evidence-authority.flexiarg`
  - governs evidence ordering and the separation between observed facts and the
    narrative posted to the PR surface
- `library/system-architecture/typed-command-control-plane-authority.flexiarg`
  - governs preference for helper/typed command surfaces over ad hoc
    publication mutation

Inspected but non-owning candidates:
- `library/social/merged-pr-completes-publication-via-local-integration-reanchor.flexiarg`
  - governs the later merge continuation, not the PR-open publication step
- `library/social/extract-classical-automation-from-novel-evolver-work.flexiarg`
  - explains why this bounded publication flow should exist, but not the direct
    reviewer-evidence policy surface

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

PR-open publication family note:
- this algorithm is the shared family owner for the PR-open publication
  posture
- composed publication finalizers should inherit the reviewer-evidence
  companion posture from here and name only their additional local issue,
  mission, or proxy delta explicitly

## Inputs

- target repo name or path (for example `futon1`)
- target branch name
- target base branch (usually `main` or `master`)
- summary of what changed
- evidence available from the current work session
- optional originating issue reference for association (repo + issue number)

## Preconditions

1. Confirm there is a governing submission policy/flexiarg for this flow.
2. Confirm branch working tree is clean.
3. Confirm branch diff scope is understandable and reviewable.
4. Confirm the intended repair changes are already committed on the target
   branch tip before publication starts.
5. If the target repo is a futon repo, run
   `algorithms/pull-futons-and-reconcile-active-branches.md` first, or
   explicitly confirm that its sync effect has already been achieved in the
   current session before pushing.

## Procedure

1. Review branch state.
   - Check current branch, ahead/behind, and changed files vs base.
   - Read the real diff (not only commit messages).
   - Confirm the branch tip already contains the intended repair checkpoint.
   - Treat this as the observation phase before publication mutation.
2. Decide whether existing submission flexiarg is sufficient.
   - If a suitable flexiarg already exists, reuse it.
   - If missing, create one before continuing.
3. For futon repos, call the pull/reconcile algorithm before publication.
   - Run:
     - `algorithms/pull-futons-and-reconcile-active-branches.md`
   - If its sync effect has already been achieved in the current session and
     that fact is explicit, this step may be treated as already satisfied.
   - If the pull/reconcile path halts on ambiguity, stop the publication flow
     there rather than bypassing it.
4. Publish the branch.
   - For futon repos, this assumes the pre-push sync gate from the prior step
     has already passed.
   - Preferred composed helper for branch push + PR creation:
     - `agent_skills/gh-pr-create.bat <repo-name-or-path> <base-branch> <title> [body-file]`
   - Inside that helper, the repo-aware transport step should prefer:
     - `agent_skills/git-github-repo.bat <repo-name-or-path> push -u origin <branch>`
   - Helper-governed requirement:
     - run publication mutation through `gh-pr-create.bat`
     - do not bypass this with ad hoc direct push/PR commands
     - if the helper fails, stop the line and fix the helper/algorithm path
       before continuing publication
   - If access is blocked or pending, perform a call with continuation and
     record:
     - blocked reason
     - expected access path
     - exact resume sequence
   - If access later changes, treat the resume as a continuation event and
     record how the state change was observed.
   - If conflicts, auth ambiguity, or unsafe branch state appear, stop the line
     and return to observation/continuation handling rather than forcing the
     push.
5. Create PR.
   - Before creating a new PR, inspect whether the branch already has an open
     PR review surface.
   - Preferred helper surface:
     - `agent_skills/gh-pr-status.bat <repo-name-or-path> [branch]`
   - If an open PR already exists for the branch, reuse that PR instead of
     attempting duplicate creation.
   - Base = canonical integration branch.
   - Title/body = concise scope, boundaries, and no-overclaim summary.
   - When step 3 used `agent_skills/gh-pr-create.bat`, this PR creation is
     already satisfied by that same helper call, including the existing-PR
     reuse check.
6. Verify and capture the canonical PR review surface.
   - Obtain and store the concrete PR URL (and PR number if available).
   - Preferred helper surface:
     - `agent_skills/gh-pr-status.bat --url-only <repo-name-or-path> [branch]`
   - If publication surface output says a PR exists but no concrete PR URL can
     be recovered, stop the line and resolve the ambiguity before continuing.
7. After the PR exists, request the default reviewer.
   - Request `tothedarktowercame` as a reviewer on the PR.
   - Route that mutation through the GitHub PR publication `agent_skill`
     family.
   - Prefer the mounted official GitHub MCP lane when it exposes the needed
     reviewer-request mutation.
   - If no governed reviewer-request `agent_skill` surface exists yet, stop
     the line and extend that lane rather than mutating reviewer state through
     an ad hoc direct command inside this algorithm.
8. If an originating issue exists, associate the PR to that issue.
   - Keep this step bounded to issue association, not issue closure.
   - For GitHub-side issue association mutation, prefer the mounted official
     GitHub MCP lane when it exposes the needed issue write surface.
   - Add a PR-to-issue association marker in the PR surface (for example
     `refs #<issue-number>` in PR body) unless already present.
   - Add a short issue comment linking the PR URL when that link is not
     already explicit in issue history.
   - If association updates fail on either surface, perform a call with
     continuation and preserve the pending association step.
9. Decide whether a thorough PR comment is needed.
   - Needed when reviewers would benefit from provenance, patch rationale, and
     verification guidance beyond the PR body.
10. Draft the thorough PR comment.
   - Include: scope, why, patch-by-file summary, non-goals, reviewer quick-check.
   - Keep claims bounded to evidence already established.
11. Human preflight review.
   - Show full draft comment text to human operator.
   - Wait for explicit approval before posting.
12. Post comment with full body.
   - Prefer a body-file path (not one-line CLI body) to avoid truncation and
     fragile quoting.
   - If the approved comment lives inside a larger draft file, extract the
     exact approved block into a temporary body file first.
   - Preferred helper surface:
     - `agent_skills/gh-pr-comment-file.bat <repo-name-or-path> [pr-selector|current] <body-file>`
   - Helper-governed requirement:
     - post through `gh-pr-comment-file.bat`
     - do not bypass this with direct `gh pr comment` inside this algorithm
     - if helper posting fails, stop the line and fix the helper path before
       posting
13. Record outputs.
   - branch URL, PR URL, reviewer-request receipt, comment URL (if posted),
     originating issue association receipt, and any follow-on tasks.
14. If the PR later merges, complete the stored continuation.
   - treat the merge as a continuation event
   - run:
     `algorithms/complete-publication-continuation-after-pr-merge.md`
   - return the local repo to the canonical integration branch rather than
     leaving publication operationally unfinished
15. If you later learn that the PR is approved and want to know whether the
    merge continuation should run now, check that through:
    `algorithms/check-approved-pr-and-dispatch-merge-continuation.md`

## Decision Rule: When To Post A Thorough PR Comment

Post one when at least one is true:

- branch changes are non-trivial and span multiple files/behaviors
- reviewers need explicit rationale to map changes to intent
- there is a receipt/evidence contract or verification method to communicate
- prior discussion contained important constraints not obvious from diff alone

Skip or keep minimal when:

- change is tiny and self-evident
- PR body already fully carries reviewer context

## Outputs

- published branch
- opened PR
- verified PR URL/number capture for the review surface
- reviewer-request mutation routed through the GitHub PR publication
  `agent_skill` lane
- originating issue association receipt when a source issue exists
- optional reviewer evidence comment (human-approved before posting)
- when the PR later merges, a clear post-merge continuation path for returning
  the local repo to the integration branch
- references for future audit:
  - commit(s)
  - PR link
  - posted comment link

## Call with continuation note

For the publication boundary, a call with continuation means:

1. stop at the access, approval, or safety boundary before publication
2. preserve the pending publication step rather than discarding it
3. record the exact blocked reason and exact resume sequence
4. later resume the same publication step when the continuation event occurs

## Stop Conditions

Stop and ask the operator if:

- `gh-pr-create.bat` cannot complete push/PR mutation
- `gh-pr-status.bat` output is ambiguous or cannot recover a concrete PR URL
- no governed GitHub PR publication `agent_skill` surface exists for reviewer
  request when that step is reached
- `gh-pr-comment-file.bat` cannot post the approved comment body
- helper invocation semantics are broken by environment/quoting/tooling drift
  and would require an ad hoc fallback to proceed

When these occur, treat the helper path as the thing to repair; do not perform
publication/comment mutation through a bypass command inside this algorithm.

## English/Human-Programming Cross-References

This algorithm is a constrained English operational program. It should be read
as executable intent, not casual prose.

Related idea artifacts:

- `ideas/categorical-state-transition-model-for-english-mission-programming.md`
  - framing: English statements as state-transition specs
- `ideas/workflows-as-repeatable-mission-main-methods.md`
  - framing: workflows as reusable "main methods" in English-as-EAL form
- `ideas/text-base-lineage-and-technology-mapping.md`
  - framing: language substrate + lineage-aware organization of procedural text

## Notes

- This algorithm complements (does not replace) submission flexiargs.
- Keep it basic and operational; avoid theory-heavy prose in execution-time use.
- The reason this exists as an algorithm rather than free-form agent handling is
  explicit system policy:
  - routine control should move into governed procedures instead of ad hoc LLM
    interpretation
  - stable publication evidence should be grounded in deterministic git/gh
    outputs, with narrative only explaining the result
- Case-study refinement anchor:
  `docs/futon3-pr-publication-continuation-case-study.md`
- In this environment, repo-scoped wrapper paths and body-file comment posting
  are more reliable than fragile nested shell quoting.
- Preferred composition:
  - branch PR surface inspection through `agent_skills/gh-pr-status.bat`
  - branch push + PR create through `agent_skills/gh-pr-create.bat`
  - detailed reviewed comment posting through
    `agent_skills/gh-pr-comment-file.bat`
- This algorithm is intentionally non-fallback:
  - when any required helper fails, stop and repair helper execution rather
    than mutating publication state through ad hoc direct commands
- This algorithm includes issue association when an originating issue exists,
  but does not own duplicate-issue closure policy.
- This algorithm also does not own futon-origin local proxy state mutation.
  When the run must close a futon-scoped mission and update the futon-origin
  proxy to PR-open in one composed flow, route through:
  `algorithms/complete-futon-issue-mission-through-pr-open-and-local-proxy-finalization.md`
- The algorithm is intentionally proof-path-shaped:
  observe branch state first, mutate later, and stop-the-line on ambiguity.
- This algorithm is also a working-example anchor for the EAL control form
  later named as `CALL WITH CONTINUATION`.
- Publication is not fully complete at PR-open time; when the PR later merges,
  the local continuation should normally run the post-merge re-anchor
  algorithm.
- Approval is not merge:
  - reviewer approval does not by itself trigger the publication-completion
  continuation
  - use the approved-PR check algorithm when the next question is whether the
    PR has actually merged yet
