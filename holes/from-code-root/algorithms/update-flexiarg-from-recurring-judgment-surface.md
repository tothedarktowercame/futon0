# Algorithm: Update Flexiarg from Recurring Judgment Surface

Date: 2026-03-14
Status: active

## Purpose

Use this when a flexiarg already exists and the truthful next move is to
change that existing flexiarg owner rather than pretending the judgment lane
is brand new.

This is the update-version sibling of:
- `algorithms/create-flexiarg-from-recurring-judgment-surface.md`

It owns only the existing-flexiarg gate for flexiarg authoring.

It does not own the shared inner flexiarg-authoring/refactoring work after the
existing-versus-new gate is crossed. That shared core belongs to:
- `algorithms/author-flexiarg-core-and-wire-discoverability.md`

## Companion Flexiarg Resolution (2026-03-14)

Governing companion flexiargs for this algorithm:
- `library/social/create-versus-update-flexiarg.flexiarg`
  - governs the lifecycle judgment this algorithm exists to operationalize on
    the existing-owner side
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
  - governs reuse-first refinement of the current strongest flexiarg owner
    before creating a parallel sibling
- `library/social/prefer-repo-local-data-tmp-before-os-temp.flexiarg`
  - governs the placement rule when the recurring judgment surface is about
    temp files, runtime scratch state, helper bodies, or bounded caches

Inspected but non-owning candidates:
- `library/social/algorithms-own-procedural-spines-flexiargs-own-judgment-surfaces.flexiarg`
  - governs the broader carrier boundary, but the shared core is the direct
    owner of that inner split
- `library/social/knowledge-factoring-pass-naming-and-subtypes.flexiarg`
  - governs lane/naming discipline for the shared inner authoring core after
    lifecycle has already been decided
- `library/social/create-versus-update-algorithm.flexiarg`
  - governs algorithm-family lifecycle shape, not flexiarg-owner lifecycle
    shape

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

## Inputs

- an existing flexiarg artifact
- the requested change or the recurring live pressure exposing that change
- the current explicit mission owner, or a bounded decision to create one first
- nearby flexiargs, algorithms, docs, and missions in:
  - `gh/mfuton`
  - `darktower/futon3c`

## Preconditions

1. A concrete existing flexiarg owner has already been identified.
2. The local mission neighborhood has been inspected before broadening the
   change.
3. Nearby flexiarg siblings/parents have been checked before rewriting the
   existing owner.
4. The operator/evolver already has concrete evidence for why the current
   flexiarg is under change pressure:
   - missing guardrail
   - missing boundary clarification
   - naming drift
   - repeated discoverability pressure
   - family-split or helper-extraction pressure

## Procedure

1. Start from the existing flexiarg owner and the concrete change pressure.
   - Name the current flexiarg.
   - Name the recurring failure, ambiguity, or missing distinction that is
     forcing the update.
   - If the recurring judgment is about temp-producing behavior, identify the
     producing surface and the intended repo-local `data/tmp/...` owner path.
2. Inspect explicit reuse first.
   - Check `gh/mfuton` for nearby parent/sibling flexiargs, algorithms,
     missions, and docs.
   - Check `darktower/futon3c` for upstream or adjacent patterns that already
     cover part of the lane.
   - Reuse/refine an existing owner when that is cleaner than creating a
     near-duplicate.
3. Decide lifecycle shape first.
   - Reuse the decision gate:
     `library/social/create-versus-update-flexiarg.flexiarg`
   - Decide whether this wave should honestly:
     - update the existing flexiarg
     - create a new sibling/child flexiarg instead
     - route back to a procedural owner instead
     - stop because the lane is still too weakly bounded
4. If the stronger move is to create a new flexiarg owner, do not keep
   forcing the update path.
   - Call:
     `algorithms/create-flexiarg-from-recurring-judgment-surface.md`
   - Stop this algorithm after handing off.
5. If the surface is really replayable procedure rather than reusable
   judgment, route back to the algorithm lane.
   - Call:
     `algorithms/create-algorithm.md`
   - Stop this algorithm after handing off.
6. If the lane is still too novel or ambiguous for durable flexiarg change,
   stop cleanly at mission/doc/discussion level instead of churning the
   flexiarg family.
7. If updating the existing flexiarg is the truthful result, call the shared
   flexiarg authoring core.
   - Use:
     `algorithms/author-flexiarg-core-and-wire-discoverability.md`
   - Pass the lifecycle mode as:
     - `existing`
8. If a follow-on artifact-boundary cleanup is still wanted after the shared
   core runs, use:
   - `algorithms/conduct-knowledge-orthogonalization-pass.md`

## Decision Rule

Default factoring rule:
1. use this algorithm only to decide whether the truthful move is still
   updating the existing flexiarg owner
2. once the answer is yes, compose the shared flexiarg authoring core:
   `algorithms/author-flexiarg-core-and-wire-discoverability.md`
3. if the surviving recurrence is really procedure rather than judgment,
   route to the algorithm-authoring family instead of forcing a flexiarg
4. if the lane is still too novel or weakly evidenced, stop honestly without
   creating churn

## Outputs

- one of:
  - an updated existing flexiarg via the shared flexiarg authoring core
  - a routed handoff into
    `algorithms/create-flexiarg-from-recurring-judgment-surface.md`
  - a routed handoff into `algorithms/create-algorithm.md`
  - an explicit decision that the lane is not yet ready for durable flexiarg
    change
- downstream shared-core output that carries:
  - updated indexes/references for discoverability
  - explicit reused-versus-new reporting for the flexiarg wave

## Stop Conditions

Stop and keep the lane at mission/doc/flexiarg-adjacent discussion level if:
- the pressure on the existing flexiarg is still too ambiguous
- the same artifact would need to absorb too many unrelated concerns
- a new sibling/child flexiarg is the stronger owner instead
- the work has not actually stabilized enough for durable flexiarg change

## References

- `algorithms/create-flexiarg-from-recurring-judgment-surface.md`
- `algorithms/create-algorithm.md`
- `algorithms/author-flexiarg-core-and-wire-discoverability.md`
- `algorithms/conduct-knowledge-orthogonalization-pass.md`
- `holes/missions/M-mfuton-pragmatic-operational-problem-solving-codification/M-algorithm-creation-and-flexiarg-factoring.md`
- `library/social/create-versus-update-flexiarg.flexiarg`
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
- `library/social/prefer-repo-local-data-tmp-before-os-temp.flexiarg`
