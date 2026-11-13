# Algorithm: Pull Futons and Reconcile Active Branches (Installation-Wide by Default)

Date: 2026-03-07
Status: active

## Purpose

Use this when you want to refresh the local futon installation set with:

1. the canonical multi-repo pull script
2. safe follow-on reconciliation of the checked-out futon branches currently
   carried in that installation

This algorithm is intentionally designed as a global "bring the futons to
present" procedure. Its default job is to pull the local futon installation set
forward and merge those upstream results into the local checked-out work
regardless of the narrower coding context of the moment.

Narrowing rule:
- narrower-than-global scope is an operator override, not the default
- if the operator explicitly asks for a narrower subset, follow that direction
- if the evolver believes narrowing is prudent, the evolver must stop and ask
  for operator approval before narrowing the scope
- without that operator approval, continue with the installation-wide scope

This algorithm exists to isolate the repeatable, classical part of the repo-sync
workflow from the live evolver/operator conversation.

Publication rule:
- before pushing a futon repo branch, run this algorithm or explicitly confirm
  that its effect has already been achieved in the current session
- "branch remote is current" is not sufficient; the checked-out branch should
  first contain the latest canonical integration head (`origin/main` or
  `origin/master`) when that is the obvious upstream base

## Companion Flexiarg Resolution (2026-03-14)

Governing companion flexiargs for this algorithm:
- `library/social/extract-classical-automation-from-novel-evolver-work.flexiarg`
  - governs promotion of the stabilized pull/reconcile lane into a reusable
    algorithm and helper composition
- `library/social/merged-pr-completes-publication-via-local-integration-reanchor.flexiarg`
  - governs the post-merge re-anchor expectation that this algorithm often
    composes for futon repos

Inspected but non-owning candidates:
- `library/social/chat-derived-pr-evidence-with-human-preflight-review.flexiarg`
  - governs reviewer-facing publication evidence, not the pre-push sync and
    installation-wide reconciliation lane

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

## Inputs

- canonical pull script path:
  - `pull-futons.bat`
- preferred Windows clean-env wrapper:
  - `gh/mfuton/scripts/windows/pull-futons-clean-windows.bat`
- preferred repo-aware GitHub transport helper for follow-on fetch/push
  substeps:
  - `gh/mfuton/agent_skills/git-github-repo.bat`
- lower-level transport helper when the repo root is already explicit:
  - `gh/mfuton/agent_skills/git-github.bat`
- full local futon installation set covered by the canonical pull regime
- for any checked-out branch that is already published, whether it already has
  an open PR or equivalent review surface that should stay current
- operator policy for whether merge mutation is allowed in the current session
- optional narrower scope only when:
  - the operator explicitly asks for it
  - or the evolver asks and the operator explicitly approves it

Typical default installation scope:
- every local futon repo covered by `pull-futons.bat`
- includes repos that are not part of the current feature focus
- default branch surface is the checked-out branch in each repo, not every
  parked local branch

## Preconditions

1. Operator is willing to let the pull script mutate local repo state.
2. There is no unresolved merge or rebase already in progress in a repo being
   reconciled.
3. The default reconciliation scope is the full local futon installation set;
   do not narrow based on current task context unless the operator explicitly
   directs it or explicitly approves an evolver request to narrow it.
4. The default branch surface is the checked-out branch in each repo; do not
   silently widen to parked non-checked-out branches unless the operator asked.

## Procedure

1. Run the canonical pull step.
   - Preferred from this workspace/shell:
     - `gh/mfuton/scripts/windows/pull-futons-clean-windows.bat`
   - Direct upstream entrypoint:
     - `pull-futons.bat`
   - The clean-env wrapper exists because some local shells inherit broken git
     transport env (`HTTP_PROXY`, `HTTPS_PROXY`, `ALL_PROXY`,
     `GIT_SSH_COMMAND`) that make the canonical script fail even though the
     script itself is correct.
2. Record the outcome by repo.
   - Which repos were already up to date.
   - Which repos fast-forwarded.
   - Which repos were skipped because they had no tracking branch.
3. Run the complexity gate.
   - If the reconciliation path is straightforward, continue directly to sync.
   - If the reconciliation path is complex, perform a call with continuation:
     talk to the operator and schedule the sync step after the ambiguity is
     resolved.
   - Complexity includes:
     - no obvious upstream/base branch
     - dirty working tree whose temporary stash/restore path is not straightforward
     - force-updated or rewound remote history
     - branch divergence that needs a judgment call
     - non-trivial merge conflict
4. Establish reconciliation scope.
   - Start from the full local futon installation set covered by the pull
     regime.
   - Include repos even when they seem unrelated to the current coding focus.
   - Use the checked-out branch in each repo as the default branch surface.
   - If the operator already directed a narrower scope, use that narrower
     scope.
   - If the evolver believes narrowing is prudent, stop and ask for operator
     approval; narrow only after that approval is granted.
5. For each futon repo in scope, inspect branch state.
   - current branch
   - tracking branch, if any
   - whether the branch is clean
   - whether the canonical integration head (`origin/main` or `origin/master`)
     moved
   - whether the checked-out branch already contains the latest canonical
     integration head, even if its own tracked feature-branch remote is current
6. Choose the reconciliation target conservatively.
   - If the checked-out branch has an obvious tracked upstream and is simply
     behind it, reconcile against that tracked upstream.
   - Else if the branch is a feature branch based on `main` or `master` and the
     integration head advanced, reconcile by merging that updated integration
     head into the branch.
   - Treat "branch remote is current" and "branch contains latest integration
     head" as separate checks. A feature branch can be up to date with
     `origin/<feature-branch>` and still be behind `origin/main` or
     `origin/master`.
   - Else if there is no obvious merge base or tracking relationship, stop and
     ask the operator.
7. Perform only the safe sync step.
   - Accept straightforward merge/update only.
   - Straightforward dirty worktrees may be temporarily autostashed and
     restored when that is what lets the checked-out branch safely absorb the
     updated upstream head.
   - If a non-trivial conflict appears, stop and talk to the operator.
8. Re-check branch state after the sync.
   - confirm branch tip now includes the intended upstream head
   - confirm the checked-out branch now contains the latest canonical
     integration head (`origin/main` or `origin/master`) when that was the
     intended sync target
   - confirm no unresolved conflict remains
9. If a checked-out branch was already published and the sync step created new
   local commits, push the branch so the published review surface stays
   current.
   - Typical case:
     - an open PR already exists
     - the branch was behind the canonical integration head
     - the reconcile step merged that head into the branch
   - Treat this as the normal completion of the same sync/publication episode,
     not as a separate deferred continuation.
   - Preferred helper surface:
     - `agent_skills/git-github-repo.bat <repo> push -u origin <branch>`
   - If the repo root is already explicit and helper composition would add no
     clarity, the lower-level transport helper is still acceptable:
     - `agent_skills/git-github.bat -C <repo-root> push -u origin <branch>`
   - If the push path is blocked by auth, shell, or safety ambiguity, perform a
     call with continuation and preserve the pending push step.
10. Record results.
   - repo
   - branch
   - merge target used
   - whether the merge was automatic or halted
   - whether a post-reconcile push was required and whether it happened
   - any follow-on conflicts or operator decisions required

## Stop Conditions

Stop and ask the operator if any of these happen:

- non-trivial merge conflict
- ambiguous choice of merge target
- force-pushed/rewound remote branch that changes the meaning of "up to date"
- dirty working tree that should not be auto-stashed or whose restore is likely
  to conflict
- branch lacks a trustworthy upstream/base relationship

## Decision Rule: What Counts as Safe Automation Here

Safe to automate:
- running `pull-futons.bat`
- reading current branch/upstream/ahead-behind state
- detecting whether a checked-out branch is behind an obvious upstream base
- detecting whether a checked-out branch is missing commits from the canonical
  integration head even when its tracked feature-branch remote is current
- straightforward merge with no conflict and no semantic choice

Not safe to automate blindly:
- conflict resolution
- choosing between multiple plausible merge bases
- history rewriting
- non-trivial stash/restore behavior on dirty trees
- semantic judgment about overlapping changes

High-level rule:
1. default to the full local futon installation set
2. only narrow that scope after explicit operator direction or explicit
   operator approval of an evolver request to narrow it
3. if complex, perform a call with continuation to the operator while
   scheduling the sync step
4. sync changes

Call with continuation note:
- In this algorithm, a call with continuation means the durable open-world
  continuation form: the flow intentionally suspends at the operator boundary,
  preserves the pending sync step, and resumes that same step after the
  operator resolves the ambiguity.

## Outputs

- refreshed local futon repo set
- checked-out futon branches across the installation set reconciled when safe
- already-published checked-out branches pushed when the reconcile step created
  new local commits
- explicit halt/report when the situation exceeds safe automation
- when successful, a futon-branch state that is ready for a later push step
  when no already-published branch needed immediate push

## References

- exact pull entrypoint:
  - `pull-futons.bat`
- preferred local wrapper:
  - `gh/mfuton/scripts/windows/pull-futons-clean-windows.bat`
- automation-extraction policy:
  - `library/social/extract-classical-automation-from-novel-evolver-work.flexiarg`
- post-mortem promotion gate:
  - `library/social/post-mortem-for-novel-learning-capture.flexiarg`
- preferred repo-aware transport helper:
  - `agent_skills/git-github-repo.bat`
- lower-level transport helper:
  - `agent_skills/git-github.bat`
- overlap/conflict policy:
  - `library/social/upstream-supersession-via-semantic-venn-diagram.flexiarg`
- case anchor:
  - `docs/futon1-stacked-pr-sync-case-study.md`

## Notes

- Scope is intentionally limited to futon repos; do not widen this algorithm to
  `gh`, `development`, or other non-futon repos unless a separate algorithm
  explicitly owns that transport/upstream regime.
- Scope clarification added from the 2026-03-26 run:
  - this algorithm is global across the local futon installation set by default
  - do not narrow it just because the current coding context is narrower
  - if the operator wants a narrower scope, follow that
  - if the evolver believes narrowing is prudent, ask for operator approval and
    narrow only if that approval is explicitly granted
- Observed failure mode on 2026-03-07:
  - the ambient shell inherited:
    - `HTTP_PROXY=http://127.0.0.1:9`
    - `HTTPS_PROXY=http://127.0.0.1:9`
    - `ALL_PROXY=http://127.0.0.1:9`
    - `GIT_SSH_COMMAND=cmd /c exit 1`
  - this broke both HTTPS and SSH futon remotes even though
    `pull-futons.bat` itself was fine
  - the classical fix is the clean-env wrapper, not ad hoc per-run shell
    surgery
- Clarification added from the 2026-03-10 run:
  - the preferred Windows wrapper now performs the follow-on checked-out-branch
    reconcile pass after the bulk pull step
  - for straightforward dirty repos, it may temporarily autostash local changes,
    absorb the latest upstream head, and then restore those changes
  - dirty repos are therefore no longer a blanket skip at the preferred wrapper
    layer, even though the upstream bulk pull script itself remains narrower
- Publication is a separate step from sync. If a follow-on push is desired and
  the local shell blocks ordinary `git push` with subprocess/signal-pipe
  failures, prefer `agent_skills/git-github-repo.bat <repo> push ...`; fall
  back to a repo-scoped `pwsh -NoProfile` push invocation under the approved
  unrestricted path only when the helper surface is itself blocked.
- Clarification added from the 2026-03-08 run:
  - if a checked-out futon feature branch already has an open PR or equivalent
    published review surface, and reconcile merges new upstream commits into
    that branch, push the branch before considering the review surface current
    again
  - this is a direct composed step of the same sync/publication episode, not a
    new deferred continuation
- But publication is not independent of sync ordering:
  - for futon repos, the default rule is `run this algorithm first, then push`
  - if that order is intentionally skipped, the operator should say so
    explicitly rather than leaving it implicit
- Clarification added from the 2026-03-07 run:
  - "up to date" for this algorithm means the checked-out branch contains the
    latest canonical upstream base (`origin/main` or `origin/master`) when that
    base is the obvious integration head
  - it is not enough for the checked-out branch to merely match its own
    `origin/<feature-branch>` remote
- Observed case on 2026-03-07:
  - ordinary `git -C futon3c push origin merge_http_async_windows_cache`
    failed in the constrained shell with Git `sh.exe` signal-pipe/Win32 error 5
  - nested PowerShell retries were not sufficient while still inside the same
    constrained environment
  - the successful publication path was an unrestricted repo-scoped push
    launched through PowerShell without profile loading
- This algorithm complements future scripts; it does not require that all steps
  are already scripted.
- If a later batch wrapper is built for the safe substeps, that wrapper should
  preserve the same stop conditions instead of trying to automate away
  ambiguity.
- If a later push/publication is desired, treat successful completion of this
  algorithm as the normal pre-push gate for futon repos.
  - do not push first and reconcile later
  - if the algorithm could not complete because a call with continuation was
    required, the push should stay blocked until the continuation resolves
    and sync resumes
