# Algorithm: Triage Futon Issue And Decide Local Work Handoff

Date: 2026-03-11
Status: active

## Authority routing

- issue/case owner:
  `holes/missions/M-futon-full-system-operation-and-onboarding/M-world-backed-local-issue-case-system.md`
- futon-origin intake/write-up algorithm:
  `algorithms/create-futon-issue-before-triage.md`
- local `mfuton` intake/write-up algorithm:
  `algorithms/create-mfuton-issue-before-triage.md`
- mission-creation algorithm:
  `algorithms/create-mission.md`
- futon-origin mission completion through PR-open + proxy finalization:
  `algorithms/complete-futon-issue-mission-through-pr-open-and-local-proxy-finalization.md`
- record-shape note:
  `docs/M-world-backed-local-issue-case-system/canonical-record-shapes-and-first-layout.md`
- backing/projection note:
  `docs/M-world-backed-local-issue-case-system/world-backing-and-projection-boundary.md`

## Purpose

Use this when a futon-origin issue has already been written up locally under
`world/futon-issues/<futon>/...` and you want a repeatable triage flow for:

1. deciding whether only tiny local triage is needed or whether deeper triage
   is required
2. choosing the right local handoff:
   - continue as proxy-only
   - import into `mfuton` local issue execution
   - go directly to a futon-scoped mission
3. preserving the upstream futon issue as canonical for upstream issue identity
   and problem statement
4. recording the handoff honestly instead of silently drifting into work

## Companion Flexiarg Resolution (2026-03-14)

Governing companion flexiargs for this algorithm:
- `library/social/futon-self-description-defers-to-installation-authority.flexiarg`
  - governs deference to the upstream futon issue as canonical authority for
    issue identity and problem statement
- `library/system-architecture/open-world-ingest-mission-reconstruction-and-canonicalization-factorization.flexiarg`
  - governs the local proxy-to-owner factorization between capture, handoff,
    and later local execution ownership

Inspected but non-owning candidates:
- `library/system-architecture/deterministic-evidence-authority.flexiarg`
  - relevant to stronger verification/post-triage proof posture, but this
    algorithm stops at honest triage and handoff choice

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

## Inputs

- a futon-origin local proxy under `world/futon-issues/<futon>/...`
- upstream issue wording/evidence already captured there
- current local work context
- operator intent about whether local execution should begin now

## Preconditions

1. The futon-origin proxy should already exist.
2. If it does not, first run:
   - `algorithms/create-futon-issue-before-triage.md`
3. Keep authority layers explicit:
   - upstream futon issue remains canonical for upstream issue identity and
     problem statement
   - local futon proxy owns local trigger context, triage read, and
     continuation state
4. Do not silently convert the futon-origin issue into an `mfuton` local issue
   unless that is the explicit triage outcome.

## Procedure

1. Open the futon-origin proxy and re-read the trigger context.
   - Start from the proxy, not from memory.
   - Preserve why the issue entered the installation now.
2. Decide whether tiny triage is enough.
   - Tiny triage is allowed when the upstream work item already states a
     concrete requested change and the likely work owner is obvious.
   - Deeper triage is needed when local execution still depends on resolving:
     - unclear scope
     - ambiguous expected behavior
     - uncertain authority
     - missing evidence
3. Perform the minimal honest triage.
   - Record only what is needed to choose the handoff.
   - Do not over-triage just to simulate certainty.
4. Choose one of three handoff outcomes.
   - `proxy-only`
     - keep the issue only as a futon-origin local proxy for now
   - `import-to-mfuton`
     - move local execution into an `mfuton` local issue under `world/issues/`
   - `direct-futon-mission`
     - the issue is already specific enough to go directly to a futon-scoped
       mission under `holes/missions/futon-issues/<futon>/...`
5. If the outcome is `proxy-only`:
   - update the proxy with:
     - `current_status = triaged-open`
     - unresolved unknowns
     - exact continuation condition
   - stop after recording that continuation.
6. If the outcome is `import-to-mfuton`:
   - update the proxy to say local execution will move into the `mfuton` issue
     lane
   - then run:
     - `algorithms/create-mfuton-issue-before-triage.md`
   - keep the futon-origin proxy as the causal upstream-proxy record.
7. If the outcome is `direct-futon-mission`:
   - update the proxy to say local triage determined that the upstream work item
     is already specific enough for direct missionization
   - then run:
     - `algorithms/create-mission.md`
   - place the resulting mission under:
     - `holes/missions/futon-issues/<futon>/`
   - link the mission from the proxy
   - link the proxy from the mission as provenance
   - capture the intended completion owner for later:
     - when that mission reaches verified repair publication readiness, use:
       `algorithms/complete-futon-issue-mission-through-pr-open-and-local-proxy-finalization.md`
8. Append the triage event.
   - Record:
     - triage kind: tiny or deeper
     - chosen handoff outcome
     - why that handoff was chosen
     - next continuation

## Decision Rule: When Tiny Triage Is Enough

Tiny triage is enough when all are true:

- the upstream issue already states a concrete requested change
- the likely work owner is obvious
- there is no unresolved authority ambiguity about where the work belongs
- the next step is not “investigate what the issue even means”

Tiny triage does not mean no triage. It means the triage result is already
clear after a short local read.

## Outputs

- an updated futon-origin local proxy with explicit triage outcome
- one of:
  - continuation as proxy-only
  - handoff into `mfuton` local issue import
  - direct futon-scoped mission creation
- explicit local record of why that handoff was chosen

## References

- intake/write-up owner:
  `algorithms/create-futon-issue-before-triage.md`
- local execution import owner:
  `algorithms/create-mfuton-issue-before-triage.md`
- mission creation owner:
  `algorithms/create-mission.md`
- futon mission completion owner after direct-futon-mission handoff:
  `algorithms/complete-futon-issue-mission-through-pr-open-and-local-proxy-finalization.md`

## Notes

- This algorithm exists because futon-origin issues are not all the same:
  some should remain only as proxies, some should become `mfuton` local
  issues, and some are already specific enough to go directly to a futon-scoped
  mission.
- The direct futon-scoped mission path should not be described as an
  `mfuton` local issue import.
