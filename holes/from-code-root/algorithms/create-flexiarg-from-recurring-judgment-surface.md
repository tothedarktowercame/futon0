# Algorithm: Create Flexiarg from Recurring Judgment Surface

Date: 2026-03-14
Status: active

## Purpose

Use this when recurring live work exposes a reusable judgment surface that is
not itself a replayable ordered procedure, and the truthful next move may
still be:
- create a new flexiarg owner
- update an existing flexiarg owner instead
- route back to a procedural owner because the surface is not actually
  flexiarg-owned
- stop because recurrence or boundary truth is still too weak

This is the new-flexiarg gate sibling of:
- `algorithms/update-flexiarg-from-recurring-judgment-surface.md`

It owns only the new-flexiarg gate for flexiarg authoring.

It does not own the shared inner flexiarg-authoring/refactoring work after the
new-versus-existing gate is crossed. That shared core now belongs to:
- `algorithms/author-flexiarg-core-and-wire-discoverability.md`

Family note:
- when create/update pressure is real on both sides, expect the full family
  split by default:
  - create gate
  - update gate
  - shared inner authoring core
  unless a later pass shows that split would be fake

## Companion Flexiarg Resolution (2026-03-14)

Governing companion flexiargs for this algorithm:
- `library/social/create-versus-update-flexiarg.flexiarg`
  - governs the lifecycle judgment this algorithm exists to operationalize on
    the new-owner side
- `library/social/algorithms-own-procedural-spines-flexiargs-own-judgment-surfaces.flexiarg`
  - governs the carrier boundary that keeps this lane about reusable judgment
    surfaces rather than replayable procedure
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
  - governs reuse-first inspection of existing flexiarg owners before creating
    a new one
- `library/social/prefer-repo-local-data-tmp-before-os-temp.flexiarg`
  - governs the placement rule when the recurring judgment surface is about
    temp files, runtime scratch state, helper bodies, or bounded caches

Inspected but non-owning candidates:
- `library/social/knowledge-factoring-pass-naming-and-subtypes.flexiarg`
  - governs lane/naming discipline for the shared inner authoring core after
    lifecycle has already been decided
- `library/social/create-versus-update-algorithm.flexiarg`
  - governs algorithm-family lifecycle shape, not flexiarg-owner lifecycle
    shape
- `library/social/futon-self-description-defers-to-installation-authority.flexiarg`
  - relevant only in the narrower subset where the candidate flexiarg would be
    describing futon's own semantics

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

## Inputs

- a concrete recurring judgment surface from live work
- evidence for recurrence (transcript fragments, diffs, notes, receipts)
- the local mission owner for the lane, or a decision to author one first
- nearby flexiargs, algorithms, docs, and missions in:
  - `gh/mfuton`
  - `darktower/futon3c`

## Preconditions

1. The candidate surface is genuinely recurring, not a one-off observation.
2. The surface is judgment/policy/invariant oriented, not mainly linear
   control flow.
3. Existing nearby flexiargs have been checked before creating a new file.
4. Before creating a brand-new flexiarg owner, check whether the stronger move
   is to update an existing flexiarg owner first.
5. Mission ownership for the lane is known, or a bounded decision to defer
   mission creation is explicit.

## Procedure

1. Start from one concrete recurring judgment surface.
   - Name the repeated decision/invariant clearly.
   - Name at least one real case where it mattered.
   - If the recurring judgment is about temp-producing behavior, identify the
     producing surface and the intended repo-local `data/tmp/...` owner path.
2. Inspect reuse first.
   - Check `gh/mfuton` for nearby flexiargs, algorithms, missions, and docs.
   - Check `darktower/futon3c` for upstream or adjacent pattern owners.
   - Prefer updating an existing owner when that is cleaner than creating a
     near-duplicate.
3. Decide create-new versus update-existing.
   - Reuse the decision gate:
     `library/social/create-versus-update-flexiarg.flexiarg`
   - Decide whether this wave should honestly:
     - create a new flexiarg owner
     - update an existing flexiarg owner instead
     - route back to an algorithm/doc/mission owner instead
     - stop because the surface is not yet durably codifiable
4. If the stronger move is to update an existing flexiarg owner, do not keep
   forcing the create path.
   - Call:
     `algorithms/update-flexiarg-from-recurring-judgment-surface.md`
   - Stop this algorithm after handing off.
5. If the surface is really replayable procedure rather than reusable
   judgment, route back to the algorithm lane.
   - Call:
     `algorithms/create-algorithm.md`
   - Stop this algorithm after handing off.
6. If the surface is not yet honestly ready for durable flexiarg writeback,
   stop cleanly at mission/doc/discussion level instead of creating flexiarg
   churn.
7. If a new flexiarg owner is the truthful result, call the shared
   flexiarg-authoring core.
   - Use:
     `algorithms/author-flexiarg-core-and-wire-discoverability.md`
   - Pass the lifecycle mode as:
     - `new`
8. Run the follow-on orthogonality pass.
   - The pass is not optional.
   - Route to:
     `algorithms/conduct-knowledge-orthogonalization-pass.md`
   - If the pass concludes that no broader cleanup is needed, record that
     trivial result explicitly rather than silently skipping the pass.

## Decision Rule

Default rule:
1. use this algorithm only to decide whether the truthful move is still
   creation of a new flexiarg owner
2. once the answer is yes, compose the shared flexiarg authoring core:
   `algorithms/author-flexiarg-core-and-wire-discoverability.md`
3. if the surviving recurrence is really procedure rather than judgment,
   route to the algorithm-authoring family instead of forcing a flexiarg
4. if the lane is still too novel or weakly evidenced, stop honestly without
   creating churn

## Outputs

- one of:
  - a new flexiarg via the shared flexiarg authoring core
  - a routed handoff into
    `algorithms/update-flexiarg-from-recurring-judgment-surface.md`
  - a routed handoff into `algorithms/create-algorithm.md`
  - an explicit decision to defer because recurrence is not yet stable
- downstream shared-core output that carries:
  - updated indexes/references for discoverability
  - explicit reused-versus-new reporting for the flexiarg wave

## Stop Conditions

Stop and defer if:
- recurrence evidence is too weak
- boundary cannot yet be stated cleanly
- the surface is really procedural rather than judgment-oriented
- the candidate would duplicate a stronger existing owner with only wording
  drift

## Case Anchors

- `library/social/create-versus-update-flexiarg.flexiarg`
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
- `library/social/create-versus-update-algorithm.flexiarg`
- `library/social/algorithms-own-procedural-spines-flexiargs-own-judgment-surfaces.flexiarg`

## References

- `algorithms/create-algorithm.md`
- `algorithms/update-flexiarg-from-recurring-judgment-surface.md`
- `algorithms/author-flexiarg-core-and-wire-discoverability.md`
- `algorithms/conduct-knowledge-orthogonalization-pass.md`
- `holes/missions/M-mfuton-pragmatic-operational-problem-solving-codification/M-algorithm-creation-and-flexiarg-factoring.md`
- `library/social/create-versus-update-flexiarg.flexiarg`
- `library/social/algorithms-own-procedural-spines-flexiargs-own-judgment-surfaces.flexiarg`
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
- `library/social/knowledge-factoring-pass-naming-and-subtypes.flexiarg`
- `library/social/prefer-repo-local-data-tmp-before-os-temp.flexiarg`
