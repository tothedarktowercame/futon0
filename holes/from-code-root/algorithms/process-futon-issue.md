# Algorithm: Process Futon Issue

Date: 2026-03-27
Status: active

## Authority routing

- issue/case owner:
  `holes/missions/M-futon-full-system-operation-and-onboarding/M-world-backed-local-issue-case-system.md`
- mission-creation owner:
  `algorithms/create-mission.md`
- publication owner:
  `algorithms/publish-branch-pr-with-human-approved-evidence-comment.md`
- pull/reconcile owner:
  `algorithms/pull-futons-and-reconcile-active-branches.md`
- mission-closure owner:
  `algorithms/close-verified-mission-and-delegate-deferred-work.md`
- current native GitLab issue-intake owner:
  `holes/missions/M-mfuton-familiar-typed-surface-world-and-memory/M-my-familiar-distributed-resource-plane-extensions/M-gitlab-internal-familiar-component/M-first-native-gitlab-issue-intake-for-superpod-handoff-recovery.md`
- current grouped GitLab issue method family owner:
  `holes/missions/M-mfuton-familiar-typed-surface-world-and-memory/M-my-familiar-distributed-resource-plane-extensions/M-gitlab-internal-familiar-component/M-gitlab-operator-facing-agent-skill-method-family.md`
- grouped GitLab command entry surfaces:
  `agent_skills/gitlab/gitlab-issue-create-from-packet.bat`
  `agent_skills/gitlab/gitlab-issue-note-from-file.bat`
  `agent_skills/gitlab/gitlab-issue-close.bat`

## Purpose

Use this when a futon-origin issue should be handled locally as a full `mfuton`
work item rather than staying only as an upstream proxy or a futon-scoped
mission.

This composed routine owns the local processing spine:

1. create the local repair mission first under `holes/missions/issues/`
2. create the corresponding native `mfuton` GitLab issue
3. pause for local discussion/triage before implementation
4. update the mission from that triage
5. do the fix
6. call
   `algorithms/publish-branch-pr-with-human-approved-evidence-comment.md`
   to publish the futon-side repair on a branch and open the PR
7. associate the PR back to the local GitLab issue
8. update the upstream futon issue with the PR reference and a short
   solved/handled note
9. close the local mission and the local GitLab issue once the work is handed
   off through PR-open publication

Boundary note:

- this algorithm is for the local execution lane where `mfuton` owns the live
  working mission and the local GitLab issue
- it does not replace the futon-origin proxy algorithms that preserve the
  upstream futon issue as the only local carrier
- if a local futon-origin proxy already exists, keep it linked as provenance
  and supporting context
- but this algorithm does not require creating or finalizing a
  `world/futon-issues/<futon>/...` proxy as its primary closure carrier

## Companion Flexiarg Resolution (2026-03-27)

Governing companion flexiargs for this algorithm:
- `library/social/futon-self-description-defers-to-installation-authority.flexiarg`
  - governs deference to the upstream futon issue as the canonical upstream
    problem statement even while the local mission and GitLab issue own the
    local execution lane
- `library/social/operator-verification-gate-after-agent-triage-or-fix.flexiarg`
  - governs the verification checkpoint before local mission and GitLab issue
    closure
- `library/social/chat-derived-pr-evidence-with-human-preflight-review.flexiarg`
  - inherited from the PR publication family for reviewer-facing publication
    evidence

Inspected but non-owning candidates:
- `library/social/merged-pr-completes-publication-via-local-integration-reanchor.flexiarg`
  - governs the later merged-PR integration continuation, not the local
    mission-and-GitLab closure checkpoint at PR-open handoff owned here

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

## Inputs

- upstream futon issue reference
  - futon repo
  - issue number or URL
- intended local repair summary/title
- target futon repo for the code change
- base branch (`main` or `master`)
- target local GitLab project
- any current repro notes, receipts, or discussion context already gathered

## Preconditions

1. The issue really should become local execution work instead of staying
   proxy-only.
2. The strongest local owner is a repair mission under:
   - `holes/missions/issues/`
3. The native local GitLab issue surface is available for this installation.
4. The futon target repo can be worked on through the normal branch + PR
   publication lane.
5. If the stronger truthful move is still a futon-origin proxy or a direct
   futon-scoped mission, do not force this algorithm; use the existing
   futon-origin issue algorithms instead.

## Procedure

1. Re-state the local handling decision before writeback.
   - Confirm that this issue should be processed locally through:
     - one repair mission in `holes/missions/issues/`
     - one native local GitLab issue
   - Check whether a local futon-origin proxy already exists.
     - if yes, preserve and link it as provenance/context
     - if no, do not force creation of one unless the operator wants the extra
       proxy record explicitly
   - If that decision is still not settled, stop and discuss before creating
     artifacts.
2. Create the local repair mission first.
   - Run:
     - `algorithms/create-mission.md`
   - Place the new mission under:
     - `holes/missions/issues/`
   - Start with a mission state that is honest for pre-repair planning, usually:
     - `Status: Proposed`
   - Record the upstream futon issue as provenance in the mission.
3. Create the corresponding native local GitLab issue.
   - Use the current GitLab issue-create surface to open the issue in the
     target local GitLab project.
   - Reuse the same core problem statement as the mission, while keeping the
     upstream futon issue reference explicit.
   - Link:
     - GitLab issue -> mission
     - mission -> GitLab issue
     - GitLab issue -> upstream futon issue
4. Pause for local discussion and triage.
   - Do not jump straight from issue creation into implementation.
   - Use the mission plus GitLab issue as the current local planning surfaces.
   - Decide:
     - the actual local scope
     - any acceptance boundary
     - whether the intended fix is still the right one
5. Update the mission from that triage.
   - Refine scope, acceptance criteria, derivation path, and any immediate
     implementation plan.
   - If the triage changes the local execution shape materially, update the
     GitLab issue summary/body as needed too.
6. Perform the repair and local verification.
   - Do the implementation work under the mission owner.
   - Keep the GitLab issue current enough to remain the tracked issue surface,
     but do not treat it as the main design carrier.
7. Prepare the futon repo for publication.
   - Ensure the work is on a non-integration branch.
   - The publication call in the next step is the governing owner for the
     futon pre-push sync lane.
8. Call the publication algorithm and open the PR.
   - Run:
     - `algorithms/publish-branch-pr-with-human-approved-evidence-comment.md`
   - For futon repos, that publication algorithm should itself call:
     - `algorithms/pull-futons-and-reconcile-active-branches.md`
     unless its sync effect has already been achieved in the current session.
   - Capture the resulting PR URL/number as the local publication receipt.
9. Associate the PR back to the local GitLab issue.
   - Add the PR reference to the GitLab issue through the current note/edit
     surface.
   - Make the PR the concrete publication/handoff artifact for the issue.
10. Update the upstream futon issue with the PR receipt.
   - Add a short upstream issue note that:
     - links the PR URL/number
     - says the issue is handled/solved through that PR-open publication
   - Prefer the mounted official GitHub MCP lane for that upstream issue-note
     mutation when the needed issue write surface is exposed there.
   - Keep this step explicit even if the generic publication algorithm already
     performed issue association mechanics on one surface.
   - This algorithm owns the stronger composed local rule that the upstream
     futon issue should visibly show the PR receipt and solved/handled status.
   - Do not silently treat PR creation alone as enough.
   - Do not close the upstream futon issue here unless some separate upstream
     closure policy explicitly says to do so.
11. Close the local mission and the local GitLab issue.
   - Close the mission through:
     - `algorithms/close-verified-mission-and-delegate-deferred-work.md`
   - Close the GitLab issue through the typed close surface.
   - This local closure happens when the work has been verified locally and
     handed off through PR-open publication.
12. Record the final local closure state.
   - The mission should show closed/complete local execution ownership.
   - The GitLab issue should show closed with the PR association visible.
   - The upstream futon issue should show the PR receipt plus a short
     solved/handled note.
   - Keep any later merged-PR continuation as a separate repo/publication
     concern rather than reopening the closed local issue by default.

## Decision Rule

Use this algorithm when all are true:

- the source work item is a futon-origin issue
- the intended local handling mode is:
  - local repair mission under `holes/missions/issues/`
  - local native GitLab issue
- the run should include discussion/triage before implementation
- the local tracking surfaces should be closed after PR-open handoff rather
  than waiting for upstream merge

Do not use this algorithm when:

- the issue should remain only a local futon-origin proxy
- the strongest local owner is a futon-scoped mission under
  `holes/missions/futon-issues/...`
- there is no local GitLab issue in scope for the run

## Outputs

- one local repair mission under `holes/missions/issues/`
- one linked native local GitLab issue
- triage-updated mission scope before implementation
- locally verified repair published to a PR
- PR association recorded on the GitLab issue
- upstream futon issue updated with the PR receipt and a short solved/handled
  note
- local mission closed
- local GitLab issue closed

## Case Anchors

Partial or sibling precedents already present in this installation:

- first native mission-plus-GitLab-issue pairing:
  `holes/missions/M-mfuton-familiar-typed-surface-world-and-memory/M-my-familiar-distributed-resource-plane-extensions/M-gitlab-internal-familiar-component/M-first-native-gitlab-issue-intake-for-superpod-handoff-recovery.md`
  - proves the native GitLab issue envelope, first backlink rules, and the
    mission + local GitLab issue pairing
- local mission lane that already uses `mfuton gitlab issue` continuity as a
  projected engineering shell:
  `holes/missions/M-futon-full-system-operation-and-onboarding/M-futon6-onboarding-and-familiarization/M-frontiermath-proof-stack-and-classic-code-familiarization/M-solo-frontiermath-forum-codex-compatible-enactment.md`
  - proves a mission-owned local execution lane that keeps GitLab issue
    continuity explicit without collapsing the whole inner loop into GitLab
- sibling futon-origin publication/closure path that keeps the local proxy as
  the closure carrier rather than a local GitLab issue:
  `holes/missions/M-futon-full-system-operation-and-onboarding/M-world-backed-local-issue-case-system/M-futon-origin-issue-github-publication-and-local-proxy-pr-open-finalization-codification.md`
  - proves the adjacent sibling boundary this algorithm intentionally does not
    replace

## References

- futon-origin proxy intake:
  `algorithms/create-futon-issue-before-triage.md`
- futon-origin triage/handoff:
  `algorithms/triage-futon-issue-and-decide-local-work-handoff.md`
- futon-origin direct-mission completion through GitHub publication:
  `algorithms/complete-futon-issue-mission-through-pr-open-and-local-proxy-finalization.md`
- local issue/case scheduling owner:
  `algorithms/process-world-backed-issue-through-triage-and-scheduled-repair.md`
- first native GitLab issue owner:
  `holes/missions/M-mfuton-familiar-typed-surface-world-and-memory/M-my-familiar-distributed-resource-plane-extensions/M-gitlab-internal-familiar-component/M-first-native-gitlab-issue-intake-for-superpod-handoff-recovery.md`
- local FrontierMath GitLab-shell precedent:
  `holes/missions/M-futon-full-system-operation-and-onboarding/M-futon6-onboarding-and-familiarization/M-frontiermath-proof-stack-and-classic-code-familiarization/M-solo-frontiermath-forum-codex-compatible-enactment.md`

## Notes

- This algorithm is intentionally a composed local processing routine.
- It does not make the upstream futon issue non-canonical.
- It does make the local mission plus local GitLab issue the active execution
  and tracking surfaces for this installation.
