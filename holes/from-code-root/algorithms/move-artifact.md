# Algorithm: Move Artifact

Date: 2026-03-25
Status: active

## Purpose

Use this when an artifact already exists, the truthful next move is to keep
that artifact but relocate/reparent it under a stronger owner, and the work
must be treated as a bounded topology/ownership change rather than as a raw
filesystem move.

This is intentionally a small wrapper around the existing stronger owners:

- `algorithms/merge-artifact.md`
- `algorithms/conduct-knowledge-orthogonalization-pass.md`

It does not replace either of them. Its narrower job is to classify
"this is a relocation/reparenting wave" early and force the move to be
executed as a real `large` merge rather than as:

- a disguised `small` writeback
- an ad hoc direct move
- or a direct filesystem move with stale routing left behind

## Companion Flexiarg Resolution (2026-03-25)

Governing companion flexiargs for this algorithm:
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
  - governs reuse of the existing merge/orthogonalization owners instead of
    inventing a parallel move procedure family
- `library/system-architecture/mission-neighborhoods-and-semantic-topology.flexiarg`
  - governs explicit old-neighborhood and new-neighborhood inspection before
    deciding that relocation is the truthful topology change

Inspected but non-owning candidates:
- `library/social/create-versus-update-algorithm.flexiarg`
  - governs create-versus-update lifecycle pressure for algorithm families,
    but this wrapper is about relocation/reparenting of an already-existing
    artifact and may apply outside algorithms too

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

## Inputs

- the existing artifact path
- the proposed stronger owner / target neighborhood
- the current weaker or no-longer-strongest owner / source neighborhood
- the bounded local mission/docs/index neighborhood affected by the move
- any operator-bound placement decision already supplied

## Preconditions

1. The artifact already exists as a durable artifact.
2. There is real pressure that the artifact should move or be reparented.
3. The explicit current owner neighborhood and the candidate stronger owner
   neighborhood have both been inspected before mutation.
4. The work is honestly about relocation/reparenting of the same durable
   artifact identity, not merely:
   - updating the existing owner in place
   - creating a brand-new sibling artifact
   - or doing a raw path edit while pretending topology/discoverability did
     not change

## Hard Gated Constraints

1. All repo-local references to the moved artifact must be searched for and
   updated in the same wave.
   - A move is not complete if repo surfaces still point at the old path.
   - Historical receipts or immutable trace artifacts may remain unchanged,
     but live installation references must be updated.
2. If the moved artifact is executable, all executables that depend on it must
   be retested after the move.
   - This includes direct wrapper callers, launchers, smoke surfaces, and
     other governed entrypoints that invoke the moved executable.
   - A path-only rewrite is not sufficient evidence for executable moves.

## Procedure

1. Start from the existing artifact and the concrete stronger-owner pressure.
   - Name the current artifact.
   - Name the proposed stronger owner.
   - Name the concrete reason the current location is no longer the strongest
     honest owner.
2. Re-read the bounded old and new neighborhoods explicitly.
   - Inspect the current artifact owner neighborhood first.
   - Inspect the target stronger-owner neighborhood second.
   - Include immediate parent/child/index/README/catalog surfaces needed to
     make the move legible.
   - Do not begin from raw semantic search alone.
3. Classify the truth of the wave before mutation.
   - Ask whether the truthful move is really:
     - relocate/reparent the existing artifact
     - update the current owner in place instead
     - create a new sibling/child owner instead
     - or stay in discussion/alignment because owner/placement is still
       disputed
   - If relocation/reparenting is not clearly the truthful result, stop and
     route to the stronger owner:
     - `algorithms/rename-artifact.md`
     - `algorithms/update-algorithm.md`
     - `algorithms/create-algorithm.md`
     - `algorithms/create-document.md`
     - `algorithms/update-document.md`
     - or discussion/alignment, as appropriate
4. Bind this wave as a `large` merge.
   - Relocation/reparenting must be treated as a `large`
     `algorithms/merge-artifact.md` wave.
   - Do not run this wrapper as a disguised `small` merge.
5. Decide whether the relocation is:
   - physical plus topological
   - or topological-first with the current path intentionally retained
   - Physically move the artifact only when the filesystem tree should now
     encode the stronger owner directly.
6. Run `algorithms/merge-artifact.md` in `large` mode for the bounded wave.
   - Reuse the existing artifact under the stronger owner.
   - Move or re-home old references upward or sideways to that stronger owner.
   - Update immediate discoverability surfaces in both the old and new local
     neighborhoods.
   - Reduce shadow ownership rather than just duplicating the artifact at the
     new path.
7. Run `algorithms/conduct-knowledge-orthogonalization-pass.md`.
   - This follow-on pass is mandatory for this wrapper.
   - Use it to:
     - pointerize stale local restatements
     - add any needed far-but-close cross-references
     - record any explicit non-links
     - and reduce duplicate ownership pressure created by the move
8. Record the relocation honestly.
   - State:
     - what was physically moved versus only rerouted
     - what old topology remained and why
     - what discoverability surfaces were updated
     - what semantically close non-local neighbors were discovered
     - what was left deliberately unchanged

## Decision Rule

Default rule:
1. use this wrapper only when an existing artifact should move to a stronger
   owner
2. treat relocation/reparenting as a `large` merge, not a raw file move
3. reuse the existing merge/orthogonalization owners instead of inventing a
   competing move procedure
4. update old-owner and new-owner discoverability surfaces in the same wave
5. stop if artifact class, owner, or placement is still under live dispute

## Outputs

- a bounded relocation/reparenting wave executed through:
  - `algorithms/merge-artifact.md` in `large` mode
  - `algorithms/conduct-knowledge-orthogonalization-pass.md`
- explicit old-owner vs new-owner routing after the move
- updated discoverability/index/README surfaces for the touched neighborhood
- explicit accounting of what was moved, pointerized, rerouted, or left alone

## Stop Conditions

Stop and ask the operator if:
- the stronger owner is still disputed
- the work is turning into a broad re-architecture rather than a bounded move
- multiple plausible destination owners remain equally strong
- the truth is really "create a new artifact and leave the current one"
  instead of relocating the current one

## Case Anchors

- moving repeatedly useful helper or wrapper surfaces out of ad hoc or
  provisional locations into canonical `agent_skills/` ownership, while
  updating the old neighborhood and discoverability surfaces rather than
  treating the move as only a path rename

## References

- `algorithms/merge-artifact.md`
- `algorithms/conduct-knowledge-orthogonalization-pass.md`
- `algorithms/rename-artifact.md`
- `algorithms/decide-follow-on-orthogonality-and-devmap-wiring-after-mission-reconstruction.md`
- `algorithms/create-algorithm.md`
- `algorithms/update-algorithm.md`
- `algorithms/create-document.md`
- `algorithms/update-document.md`
- `algorithms/README.md`
- `holes/missions/M-mfuton-pragmatic-operational-problem-solving-codification/M-algorithm-creation-and-flexiarg-factoring.md`
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
- `library/system-architecture/mission-neighborhoods-and-semantic-topology.flexiarg`
