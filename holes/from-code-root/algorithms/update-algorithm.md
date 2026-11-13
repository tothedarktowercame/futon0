# Algorithm: Update Algorithm

Date: 2026-03-14
Status: active

## Purpose

Use this when an algorithm already exists and the truthful next move is to
change that existing algorithm owner rather than pretending the work is brand
new.

This is the update-version sibling of:
- `algorithms/create-algorithm.md`

It owns only the existing-algorithm gate for algorithm authoring.

It does not own the shared inner algorithm-authoring/refactoring work after the
existing-versus-new gate is crossed. That shared core belongs to:
- `algorithms/author-algorithm-core-and-map-companion-flexiargs.md`

Companion flexiarg creation/update is delegated to:
- `algorithms/create-flexiarg-from-recurring-judgment-surface.md`

Discussion and bound-input note:
- if a question, challenge, or new placement proposal reveals that owner,
  class, or placement is still disputed, stay in discussion/alignment rather
  than crossing into shared-core writeback
- a question is not implicit execution handoff in this update lane
- when the live pressure is really exposing a stronger new owner or a move in
  placement, classify that honestly before mutating the existing owner

## Companion Flexiarg Resolution (2026-03-14)

Governing companion flexiargs for this algorithm:
- `library/social/create-versus-update-algorithm.flexiarg`
  - governs the lifecycle judgment this algorithm exists to operationalize on
    the existing-owner side
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
  - governs reuse-first refinement of the current strongest algorithm owner
    before creating a parallel sibling
- `library/social/prefer-repo-local-data-tmp-before-os-temp.flexiarg`
  - governs temp-placement routing when the updated algorithm owns
    temp-producing behavior

Inspected but non-owning candidates:
- `library/social/algorithms-own-procedural-spines-flexiargs-own-judgment-surfaces.flexiarg`
  - governs the shared carrier boundary, but the shared authoring core is the
    direct owner of that inner split

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

## Inputs

- an existing algorithm artifact
- the requested change or the recurring live pressure exposing that change
- the current explicit mission owner, or a bounded decision to create one first
- nearby algorithms, flexiargs, docs, and missions in:
  - `gh/mfuton`
  - `darktower/futon3c`

## Preconditions

1. A concrete existing algorithm owner has already been identified.
2. The local mission neighborhood has been inspected before broadening the
   change.
3. Nearby algorithm siblings/parents have been checked before rewriting the
   existing owner.
4. The operator/evolver already has concrete evidence for why the current
   algorithm is under change pressure:
   - missing step
   - bad stop boundary
   - repeated clarification pressure
   - helper extraction pressure
   - companion-flexiarg pressure
5. The current shared model of whether this is still an update to the existing
   owner, or instead a new owner / new placement discussion, is sufficient for
   writeback.

## Procedure

1. Start from the existing algorithm owner and the concrete change pressure.
   - Name the current algorithm.
   - Name the recurring failure, ambiguity, or missing phase that is forcing
     the update.
2. Inspect explicit reuse first.
   - Check `gh/mfuton` for nearby parent/sibling algorithms, flexiargs,
     missions, and docs.
   - Check `darktower/futon3c` for upstream or adjacent patterns that already
     cover part of the lane.
   - Reuse/refine an existing owner when that is cleaner than creating a
     near-duplicate.
3. Decide lifecycle shape first.
   - Reuse the decision gate:
     `library/social/create-versus-update-algorithm.flexiarg`
   - Decide whether this wave should honestly:
     - update the existing algorithm
     - create a new sibling/child algorithm instead
     - stop at flexiarg-only or discussion/mission/doc level
4. Recheck owner/class/placement alignment before writeback.
   - If a question, challenge, or alternative placement reveals that the lane
     is still deciding between updating the current owner and choosing a
     different owner, stop at discussion/alignment level rather than mutating
     the existing algorithm on momentum.
5. If the stronger move is to create a new algorithm owner, do not keep
   forcing the update path.
   - Call:
     `algorithms/create-algorithm.md`
   - Stop this algorithm after handing off.
6. Handle flexiarg-only or not-yet-algorithm-ready outcomes explicitly.
   - If the pressure is really reusable judgment rather than procedural change:
     - call:
       `algorithms/create-flexiarg-from-recurring-judgment-surface.md`
     - stop after the flexiarg path is complete
   - If the lane is still too novel or ambiguous:
     - stop cleanly at mission/doc/discussion level instead of churning the
       algorithm family
7. If updating the existing algorithm is the truthful result, call the shared
   authoring core.
   - Use:
     `algorithms/author-algorithm-core-and-map-companion-flexiargs.md`
   - Pass the lifecycle mode as:
     - `existing`
   - Let that shared core own:
     - procedural-spine reshaping
     - companion-flexiarg handling
     - discoverability wiring
     - reused-versus-new reporting
     - temp-placement policy routing when the algorithm governs temp-producing
       behavior
7. If a follow-on artifact-boundary cleanup is still wanted after the shared
   core runs, use:
   - `algorithms/conduct-knowledge-orthogonalization-pass.md`
   - or, for narrower mission/devmap topology pressure:
     `algorithms/decide-follow-on-orthogonality-and-devmap-wiring-after-mission-reconstruction.md`

## Decision Rule

Default factoring rule:
1. use this algorithm only to decide whether the truthful move is still
   updating the existing algorithm owner
2. if owner, class, or placement is still under live discussion, stay in
   discussion/alignment rather than crossing into writeback
3. once the answer is yes, compose the shared authoring core:
   `algorithms/author-algorithm-core-and-map-companion-flexiargs.md`
4. keep reusable non-linear judgment and invariant policy in flexiargs by
   composing:
   `algorithms/create-flexiarg-from-recurring-judgment-surface.md`

## Outputs

- one of:
  - an updated existing algorithm via the shared authoring core
  - a routed handoff into
    `algorithms/create-algorithm.md`
  - an explicit flexiarg-only routing into
    `algorithms/create-flexiarg-from-recurring-judgment-surface.md`
  - an explicit decision that the lane is not yet ready for durable algorithm
    change
- downstream shared-core output that carries:
  - owning mission and local discoverability links
  - governing companion-flexiarg mapping when required
  - explicit companion-flexiarg resolution text embedded in the resulting
    algorithm artifact, not only in chat

## Stop Conditions

Stop and keep the lane at mission/doc/flexiarg level if:
- the pressure on the existing algorithm is still too ambiguous
- the same artifact would need to absorb too many unrelated concerns
- a new sibling/child algorithm is the stronger owner instead
- owner, class, or placement is still under active discussion
- the work has not actually stabilized enough for durable algorithm change

## References

- `algorithms/create-algorithm.md`
- `algorithms/conduct-discussion-on-idea-before-writeup-or-promotion.md`
- `algorithms/author-algorithm-core-and-map-companion-flexiargs.md`
- `algorithms/create-flexiarg-from-recurring-judgment-surface.md`
- `algorithms/conduct-knowledge-orthogonalization-pass.md`
- `algorithms/decide-follow-on-orthogonality-and-devmap-wiring-after-mission-reconstruction.md`
- `holes/missions/M-mfuton-pragmatic-operational-problem-solving-codification/M-algorithm-creation-and-flexiarg-factoring.md`
- `holes/missions/M-mfuton-pragmatic-operational-problem-solving-codification/M-algorithm-creation-and-flexiarg-factoring/M-create-algorithm-discussion-gates-and-default-location-contract.md`
- `library/social/create-versus-update-algorithm.flexiarg`
- `library/social/algorithms-own-procedural-spines-flexiargs-own-judgment-surfaces.flexiarg`
- `library/social/prefer-repo-local-data-tmp-before-os-temp.flexiarg`
