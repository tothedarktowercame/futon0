# Algorithm: Update Document

Date: 2026-03-14
Status: active

## Purpose

Use this when a document already exists and the truthful next move is to
change that existing docs owner rather than pretending the writeback is brand
new.

This is the update-version sibling of:
- `algorithms/create-document.md`

It owns only the existing-document gate for document authoring.

It does not own the shared inner document-authoring/writeback work after the
existing-versus-new gate is crossed. That shared core belongs to:
- `algorithms/author-document-core-and-route-writeback.md`

## Companion Flexiarg Resolution (2026-03-14)

Governing companion flexiargs for this algorithm:
- `library/social/create-versus-update-document.flexiarg`
  - governs the lifecycle judgment this algorithm exists to operationalize on
    the existing-owner side
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
  - governs reuse-first refinement of the current strongest docs owner before
    creating a parallel sibling

Inspected but non-owning candidates:
- `library/social/futon-self-description-defers-to-installation-authority.flexiarg`
  - may govern narrower upstream-deference cases, but not the generic
    existing-document gate
- `library/social/kolmogorov-factoring-for-composable-knowledge.flexiarg`
  - governs the shared inner writeback/factoring core after lifecycle has
    already been decided

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

## Inputs

- an existing document artifact
- the requested change or the recurring live pressure exposing that change
- nearby missions, ideas, docs, algorithms, and flexiargs in:
  - `gh/mfuton`
  - `darktower/futon3c`

## Preconditions

1. A concrete existing docs owner has already been identified.
2. The local owner neighborhood has been inspected before broadening the
   change.
3. Nearby sibling/parent docs owners have been checked before rewriting the
   existing owner.
4. The operator or evolver already has concrete evidence for why the current
   docs owner is under change pressure:
   - missing clarification
   - missing stop boundary
   - status/routing drift
   - repeated discoverability pressure
   - helper extraction or family-split pressure
5. The create-document family note remains in force:
   - `algorithms/create-document.md`

## Procedure

1. Start from the existing document owner and the concrete change pressure.
   - Name the current document.
   - Name the recurring failure, ambiguity, or missing phase that is forcing
     the update.
2. Inspect explicit reuse first.
   - Check `gh/mfuton` for nearby sibling/parent docs owners, missions,
     flexiargs, and docs neighborhoods.
   - Check `darktower/futon3c` for upstream or adjacent patterns that already
     cover part of the lane.
   - Reuse/refine an existing owner when that is cleaner than creating a
     near-duplicate.
3. Decide lifecycle shape first.
   - Reuse the decision gate:
     `library/social/create-versus-update-document.flexiarg`
   - Decide whether this wave should honestly:
     - update the existing docs owner
     - create a new docs owner instead
     - route out of docs because another artifact class is stronger
     - stop at discussion only for now
4. If the stronger move is to create a new docs owner, do not keep forcing the
   update path.
   - Call:
     `algorithms/create-document.md`
   - Stop this algorithm after handing off.
5. If the lane is no longer honestly docs-owned, hand back to the generic
   document classifier.
   - Call:
     `algorithms/create-document.md`
   - Stop this algorithm after handing off.
6. If the lane is still too novel or ambiguous for durable docs change, stop
   cleanly at discussion/mission/idea level instead of churning the docs
   family.
7. If updating the existing docs owner is the truthful result, call the shared
   document authoring core.
   - Use:
     `algorithms/author-document-core-and-route-writeback.md`
   - Pass the lifecycle mode as:
     - `existing`
8. If a follow-on artifact-boundary cleanup is still wanted after the shared
   core runs, use:
   - `algorithms/conduct-knowledge-orthogonalization-pass.md`

## Outputs

- one of:
  - an updated existing docs owner via the shared document-authoring core
  - a routed handoff into `algorithms/create-document.md`
  - a routed handoff into a stronger non-doc artifact owner
  - an explicit decision that the lane is not yet ready for durable document
    change

## Stop Conditions

Stop and keep the lane at discussion/mission/idea level if:
- the pressure on the existing docs owner is still too ambiguous
- the same artifact would need to absorb too many unrelated concerns
- a new sibling/child docs owner is the stronger owner instead
- the work has not actually stabilized enough for durable docs change

## References

- `algorithms/create-document.md`
- `algorithms/author-document-core-and-route-writeback.md`
- `algorithms/conduct-knowledge-orthogonalization-pass.md`
- `library/social/create-versus-update-document.flexiarg`
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
