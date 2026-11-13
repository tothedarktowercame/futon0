# Algorithm: Process World-Backed Issue Through Triage and Scheduled Repair

Date: 2026-03-08
Status: active

## Authority routing

- issue/case owner:
  `holes/missions/M-futon-full-system-operation-and-onboarding/M-world-backed-local-issue-case-system.md`
- intake/write-up algorithm:
  `algorithms/create-mfuton-issue-before-triage.md`
- record-shape note:
  `docs/M-world-backed-local-issue-case-system/canonical-record-shapes-and-first-layout.md`
- backing/projection note:
  `docs/M-world-backed-local-issue-case-system/world-backing-and-projection-boundary.md`
- operator-verification gate:
  `library/social/operator-verification-gate-after-agent-triage-or-fix.flexiarg`
- checkpoint discipline:
  `holes/missions/M-checkpoint-commit-discipline.md`
- commit-phase procedure:
  `algorithms/conduct-commit-phase-with-diff-and-total-dirty-closure.md`
- adjacent resource-allocation owner:
  `futon3c/holes/missions/M-mission-control.md`
- adjacent semi-automated allocation/inference owner:
  `futon3c/holes/missions/M-portfolio-inference.md`
- publication-aware closure:
  `algorithms/publish-verified-repair-and-close-world-backed-issue.md`
- post-mortem capture gate:
  `library/social/post-mortem-for-novel-learning-capture.flexiarg`
- automation extraction:
  `library/social/extract-classical-automation-from-novel-evolver-work.flexiarg`
- episode-to-owner factorization:
  `library/system-architecture/open-world-ingest-mission-reconstruction-and-canonicalization-factorization.flexiarg`
- EAL language-spec owner:
  `holes/missions/M-eal-language-spec.md`

## Purpose

Use this when a local issue/case enters the system and you want a repeatable
flow for:

1. starting from a written-up world-backed issue/case
2. performing triage
3. deciding whether substantive repair work is actually scheduled
4. creating a proposed repair mission under `holes/missions/issues/` only when
   that scheduling boundary has been crossed
5. checkpointing the scheduled issue + proposed mission in `gh` before active
   repair begins
6. preserving continuity cleanly when repair is not yet scheduled
7. closing the repair honestly only after verification, publication, and any
   needed post-mortem capture have been handled

## Companion Flexiarg Resolution (2026-03-14)

Governing companion flexiargs for this algorithm:
- `library/social/operator-verification-gate-after-agent-triage-or-fix.flexiarg`
  - governs the requirement that repair closure waits for operator-verification
    rather than agent plausibility alone
- `library/system-architecture/open-world-ingest-mission-reconstruction-and-canonicalization-factorization.flexiarg`
  - governs the factorization from case intake into triage, proposed repair
    mission, and later closure/publication stages
- `library/social/post-mortem-for-novel-learning-capture.flexiarg`
  - governs whether the run should enter explicit learning-capture mode once
    repair execution has completed

Inspected but non-owning candidates:
- `library/social/extract-classical-automation-from-novel-evolver-work.flexiarg`
  - relevant when stable replayable subflows are discovered during the run, but
    not the direct owner of the case-processing lifecycle itself
- `library/system-architecture/deterministic-evidence-authority.flexiarg`
  - relevant to evidence posture within triage, but not the primary owner of
    the end-to-end issue-to-repair lifecycle

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

Issue-lifecycle family note:
- this algorithm is the shared family owner for the local world-backed
  issue/case lifecycle
- downstream closure algorithms in this family should inherit the
  operator-verification and post-mortem companion posture from here and name
  only their additional local closure/publication delta explicitly

## Inputs

- trigger report or other issue ingress
- written-up local issue/case, or the ability to create one first through:
  `algorithms/create-mfuton-issue-before-triage.md`
- source surface, source entity, and source artifact
- recorded discovery/trigger context from the pre-triage write-up
- current evidence available for triage
- operator or lane decision about whether substantive repair work is scheduled
  now

## Preconditions

1. The issue/case should be recorded locally under `world/issues/` even if no
   repair mission exists yet.
2. If the case exists only as a raw report, first run:
   - `algorithms/create-mfuton-issue-before-triage.md`
3. If the issue currently exists only as a futon-origin local proxy under
   `world/futon-issues/<futon>/...`, first import it into the `mfuton` local
   issue lane through:
   - `algorithms/create-mfuton-issue-before-triage.md`
4. Triage begins after the write-up boundary has been crossed.
5. Do not create a repair mission only because the issue appears real.
6. Keep the ownership split explicit:
   - `world/issues/...` owns the causal case record, triage, and continuation
     state
   - `holes/missions/issues/...` owns the substantive repair program once that
     program has been proposed and later started

## Procedure

1. Open the written-up world-backed case.
   - If only a raw report exists, first run:
     - `algorithms/create-mfuton-issue-before-triage.md`
   - Start triage from the existing local write-up rather than from memory.
   - Re-read the recorded discovery/trigger context so triage preserves what
     work episode, mission, or verification step actually surfaced the issue.
2. Perform triage.
   - Reproduce, inspect, classify, or otherwise reduce uncertainty.
   - Keep the triage evidence and observations in the world-backed case
     artifacts.
   - Do not silently promote triage into repair-program ownership.
3. Write the triage read.
   - Update `issue.md` with the current triage understanding.
   - Append the triage event result and `continuation_state`.
   - If the case is unresolved after triage, use `current_status`:
     - `triaged-open`
4. Run the repair-scheduling gate.
   - Ask: is there now an explicit repair-program continuation for this case?
   - `scheduled` means the system has committed this case to substantive repair
     work, either:
     - immediately in the current session, or
     - later behind a recorded continuation boundary
   - `scheduled` does not mean only:
     - "this looks like a real bug"
     - "someone should probably fix this later"
     - "more triage may be useful"
5. If substantive repair is not yet scheduled, perform a call with continuation.
   - Preserve the pending step:
     - `create repair mission under holes/missions/issues/ for <issue-id> when scheduled`
   - Record the exact resume condition in the case record:
     - operator approval
     - access grant
     - planned later work slot
     - dependent upstream state change
     - or another concrete continuation event
   - Stop after recording that continuation.
6. If substantive repair is scheduled now, create the proposed repair-program
   mission.
   - Create `holes/missions/issues/M-<issue-repair-id>.md`.
   - Start it in:
     - `Status: Proposed`
   - Scope it to the actual repair program, not the whole causal history.
   - Link the mission from the world issue.
   - Link the world issue from the repair mission.
   - Update `issue.md`:
     - `current_status = triaged-repair-proposed`
     - `related_missions`
     - `next_continuation`
   - Record the next continuation explicitly:
     - checkpoint the issue + proposed mission in `gh`
     - then start the proposed mission and continue the repair
7. Checkpoint the issue + proposed mission in `gh` before active repair starts.
   - Reuse:
     - `holes/missions/M-checkpoint-commit-discipline.md`
     - `algorithms/conduct-commit-phase-with-diff-and-total-dirty-closure.md`
   - Check in the world-backed issue updates plus the newly linked proposed
     repair mission so the context and links are durable before code-bearing
     repair work continues elsewhere.
   - When that checkpoint becomes a routine local mutation step, let the
     commit-phase algorithm carry the bounded helper composition rather than
     re-describing raw `git add` / `git commit` inline here.
   - Unless the operator explicitly chooses otherwise, do not continue into
     active repair implementation before this checkpoint has been taken.
8. Start the proposed mission when active repair actually begins.
   - Change the repair mission from:
     - `Status: Proposed`
   - to:
     - `Status: In Progress`
   - This is the resource-allocation boundary:
     - today, the operator usually makes this start decision
     - later, a semi-automated allocator may recommend or stage it
   - Resume the preserved continuation under that mission owner.
9. Execute the repair under the mission owner.
   - The world-backed issue remains the causal record.
   - The repair mission owns repair steps, repair scope, and repair-specific
     acceptance.
10. If the repair reaches a plausible fix, use the operator-verification gate.
11. After operator verification, do not over-close code-bearing repairs.
   - Run:
     - `algorithms/publish-verified-repair-and-close-world-backed-issue.md`
   - Reuse the publication algorithm there rather than duplicating PR flow in
     this algorithm.
12. On closure, update both surfaces without collapsing them:
   - world issue = causal history, triage, verification state, publication
     state, and continuation record
   - repair mission = work program and repair outcome

## Decision Rule: When To Create the Proposed Repair Mission

Create the proposed repair mission when at least one is true:

- the continuation is now a substantive work program rather than a one-step
  note
- the repair needs multiple concrete steps, artifacts, or code changes
- the operator or active lane explicitly schedules the repair now or for a
  later bounded continuation
- the case needs a distinct owner for repair scope, acceptance, or verification

Starting the mission is a later boundary:

- first create the linked proposed mission
- then checkpoint the issue + mission in `gh`
- then let the active allocator choose when the mission actually starts

Current default allocator:

- the operator acts as the resource allocator/scheduler for proposed repair
  missions

Adjacent future owner:

- `futon3c/holes/missions/M-mission-control.md` already owns
  prioritization and resource-allocation as an observation/recommendation
  surface
- `futon3c/holes/missions/M-portfolio-inference.md` is the adjacent
  future owner for the more deliberative semi-automated policy-selection layer

Do not create the proposed repair mission yet when any of these are true:

- triage is still the main work
- the next step is only more observation, reproduction, or evidence gathering
- the case was resolved by explanation or a tiny local note with no distinct
  work program
- no one has actually scheduled the repair continuation yet

## Call with continuation note

For the issue-processing boundary, the constructive reading of `call with
continuation` is:

1. suspend at the operator, approval, or scheduling boundary
2. preserve the pending step instead of discarding it
3. record the exact continuation event that should reactivate the flow
4. later resume that same step rather than restart the episode from scratch

EAL cross-reference:

- this suggests a candidate control form stronger than bare `CALL`:
  - `CALL WITH CONTINUATION <target> FOR <pending-step>`
- operational meaning:
  - transfer control to a human/operator or other scheduling boundary
  - keep the suspended step and resume condition explicit in the artifact
  - resume the preserved step when the continuation event occurs

## Outputs

- a world-backed issue/case record with triage and continuation state
- optionally, a proposed repair-program mission under `holes/missions/issues/`
- a `gh` checkpoint after the issue + proposed mission have been linked
- an explicit continuation note when repair is not yet scheduled
- a bounded handoff from causal-case ownership to proposed mission ownership
  when scheduling occurs
- a later continuation from proposed mission to active repair execution
- a publication-aware closure path once operator verification has been reached

## References

- first exercised local case:
  `world/issues/issue-20260308-futon3c-local-dev-stack-regression/`
- first repair-program mission:
  `holes/missions/issues/M-futon3c-local-dev-stack-regression-repair.md`
- first exercised child submission:
  `holes/missions/M-futon-full-system-operation-and-onboarding/M-world-backed-local-issue-case-system/M-first-world-backed-local-issue-flow-exercise.md`

## Notes

- The creation of the proposed repair mission is the scheduling boundary, not
  the triage boundary by default.
- Immediate same-session repair still counts as scheduled, but the proposed
  mission should be checkpointed in `gh` before the active repair run
  continues.
- The `triaged-repair-proposed` boundary is therefore analogous to a prepared
  ticket/job waiting for allocation or pickup.
- Operator verification is not by itself publication completion for
  code-bearing repairs.
- This algorithm is intentionally local-first and text-backed; it does not
  require an external issue tracker or scheduler.
