# Algorithm: Rename Artifact

Date: 2026-03-27
Status: active

## Purpose

Use this when an artifact already exists, the artifact should remain the same
durable artifact, and the proposed change is "just rename it."

In this installation, a durable artifact rename is treated with the same
gravity as a move because a name/path change still changes topology,
discoverability, and repo-local routing even when the artifact body does not
change.

This is intentionally a tiny wrapper around:

- `algorithms/merge-artifact.md`

It does not replace that stronger owner. Its narrower job is to classify
"this is a rename of the same durable artifact" early and force the wave to
be carried with move discipline rather than as:

- a cosmetic direct filesystem rename
- a path-only cleanup with stale references left behind
- or a disguised "small" writeback

## Companion Flexiarg Resolution (2026-03-27)

Governing companion flexiargs for this algorithm:
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
  - governs reuse of the stronger `merge-artifact` owner instead of inventing
    a parallel rename writeback engine
- `library/system-architecture/mission-neighborhoods-and-semantic-topology.flexiarg`
  - governs inspection of the old and new topology/discoverability surfaces
    before treating the rename as complete

Inspected but non-owning candidates:
- `library/social/create-versus-update-algorithm.flexiarg`
  - lifecycle gating for algorithm families is adjacent but not the owner of
    same-identity artifact rename discipline

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

## Inputs

- the existing artifact path
- the proposed new name/path
- the bounded owner neighborhood affected by the rename
- the concrete reason the rename is being proposed
- any operator-bound destination or placement decision already supplied

## Preconditions

1. The artifact already exists as a durable artifact.
2. The truthful result is still the same artifact identity after the rename,
   not "create a new sibling and leave the current one."
3. The rename affects a durable tracked artifact, not only ephemeral tmp or
   generated scratch output.
4. The old and new local neighborhoods can be inspected clearly enough to run
   move discipline honestly.

## Procedure

1. Start from the concrete artifact and the proposed new name.
   - Name the current path.
   - Name the proposed new path.
   - Name the concrete reason the rename is being requested.
2. Classify same-identity rename pressure explicitly.
   - Ask whether the truthful result is:
     - rename the same durable artifact
     - move/reparent the same durable artifact more broadly
     - create a new artifact and leave the current one
     - or stay in discussion because owner/placement is still disputed
   - If the result is not a same-identity rename/move of the existing
     artifact, stop and route to the stronger owner.
3. Treat the rename as large-mode merge pressure, not as a cosmetic path edit.
   - A durable artifact rename is a move-class topology change.
   - Do not stop after a raw filesystem rename.
4. Re-read the bounded old and new neighborhoods at the scope needed for the
   rename.
   - If the rename stays in the same directory, still inspect the immediate
     README/index/reference surfaces that may mention the old name.
   - If the rename also changes owner/neighborhood, inspect both sides the way
     `algorithms/move-artifact.md` expects.
5. Call:
   - `algorithms/merge-artifact.md`
   - mode:
     - `large`
   - Treat that as the required inner owner for the actual rename wave.
6. Record the rename honestly through the large merge wave.
   - State that the trigger was a same-identity rename.
   - Update repo-local references in the same wave.
   - Leave historical immutable receipts alone unless there is a stronger
     bounded reason to rewrite them.

## Decision Rule

Default rule:
1. a durable artifact rename is a move-class change
2. use this wrapper only when the artifact identity remains the same
3. do not treat "rename only" as lighter than `algorithms/merge-artifact.md`
   in `large` mode
4. if owner/placement is still disputed, stop and discuss before mutation

## Outputs

- an explicit handoff to `algorithms/merge-artifact.md` in `large` mode
- a bounded rename wave carried with move discipline
- repo-local references and discoverability surfaces updated through the large
  rename wave rather than left stale

## Stop Conditions

Stop and ask the operator if:
- the rename would actually split the artifact into old and new owners
- the destination owner is still under live dispute
- the work is expanding into a broader refactor that is no longer just a
  rename/move-class topology change

## Case Anchors

- shortening the mission-creation algorithm filename to
  `algorithms/create-mission.md` while preserving the same algorithm owner and
  updating repo-local references in the same wave

## References

- `algorithms/merge-artifact.md`
- `algorithms/conduct-knowledge-orthogonalization-pass.md`
- `algorithms/move-artifact.md`
- `algorithms/README.md`
- `holes/missions/M-mfuton-pragmatic-operational-problem-solving-codification/M-algorithm-creation-and-flexiarg-factoring.md`
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
- `library/system-architecture/mission-neighborhoods-and-semantic-topology.flexiarg`
