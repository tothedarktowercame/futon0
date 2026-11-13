# Algorithm: Conduct Knowledge Orthogonalization Pass

Date: 2026-03-11
Status: active

## Authority routing

- architecture-level factoring objective:
  `library/system-architecture/knowledge-factoring-objective.flexiarg`
- naming/taxonomy owner:
  `library/social/knowledge-factoring-pass-naming-and-subtypes.flexiarg`
- kolmogorov/factoring cadence:
  `library/social/kolmogorov-factoring-for-composable-knowledge.flexiarg`
- algorithm-family lifecycle gate:
  `library/social/create-versus-update-algorithm.flexiarg`
- mission-neighborhood / authority-boundary owner:
  `library/system-architecture/mission-neighborhoods-and-semantic-topology.flexiarg`
- narrower topology-wiring continuation:
  `algorithms/decide-follow-on-orthogonality-and-devmap-wiring-after-mission-reconstruction.md`

## Purpose

Use this when a bounded lane, change set, or artifact neighborhood needs an
explicit orthogonalization pass:

- reduce overlap
- sharpen canonical ownership
- separate mixed concerns
- preserve only the local delta that actually belongs in each artifact
- discover semantically close but topologically non-local installation
  neighbors that the local neighborhood alone would not reveal
- add the missing cross-references or explicit non-links those farther
  semantic neighbors require

This algorithm is narrower than the whole umbrella of `knowledge factoring
pass`, but broader than a purely mechanical normalization pass.

Important local sequencing rule:
- in creation/merge waves, this pass is commonly the second topology step, not
  the first
- in that common case, the caller should usually have already added the
  immediate connected-neighborhood links and discoverability edges that make
  the changed local topology visible
- this pass then reduces overlap, sharpens ownership, pointerizes neighbors,
  or records explicit non-links
- routine bounded waves that do not introduce meaningful new topology may
  honestly stop before invoking this algorithm
- but this algorithm is also a valid standalone maintenance/drift-reduction
  pass:
  - use it when a neighborhood has accumulated overlap, stale owner boundaries,
    or missed orthogonalization over time
  - in that standalone mode, begin by making sure the currently relevant local
    neighbor links are visible enough for an honest reduction pass, then
    continue with the bounded cleanup

Local terminology note:
- the naming policy still treats `knowledge factoring pass` as the umbrella
  term
- this algorithm owns the operator-facing procedure for the
  `orthogonality pass` subtype
- in software-development practice, the closely related child term is often
  `refactoring`
  - use that term when the same boundary-cleanup activity is happening inside
    code or executable structure rather than only docs/missions/flexiargs

## Companion Flexiarg Resolution (2026-03-14)

Governing companion flexiargs for this algorithm:
- `library/system-architecture/knowledge-factoring-objective.flexiarg`
  - governs the architecture-level objective of reducing duplication and
    improving canonical ownership/composability
- `library/social/knowledge-factoring-pass-naming-and-subtypes.flexiarg`
  - governs the local naming split between umbrella knowledge factoring and
    the narrower orthogonality subtype
- `library/system-architecture/mission-neighborhoods-and-semantic-topology.flexiarg`
  - governs the explicit-neighborhood-first rule and the bounded discovery of
    semantically close but topologically non-local neighbors

Inspected but non-owning candidates:
- `library/social/kolmogorov-factoring-for-composable-knowledge.flexiarg`
  - governs recurring cadence and objective pressure, but not the direct
    operator-facing pass procedure
- `library/social/create-versus-update-algorithm.flexiarg`
  - only governs the narrower algorithm-lifecycle branch that may result from
    some orthogonality passes

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

## Inputs

- the active bounded target:
  - lane
  - mission neighborhood
  - change set
  - docs cluster
  - code subsystem
- explicit mission neighborhood and current authority routing
- candidate overlapping or duplicate artifacts
- semantically close but currently non-local candidate artifacts in the wider
  installation
- any required verification surface if executable/code behavior may be touched:
  - tests
  - receipts
  - live checks

## Preconditions

1. The target is bounded enough that the pass can finish honestly.
2. There is real overlap, mixed boundaries, or duplicate ownership pressure.
3. The pass is not merely a cosmetic rename with no authority or boundary
   consequence.

## Procedure

1. State the bounded target and the subtype emphasis.
   - Name the concrete target of the pass.
   - Classify the current emphasis:
     - docs/knowledge orthogonality
     - code refactoring
     - mixed orthogonalization
   - If the pass is only format/path/metadata alignment, stop and route it as
     a normalization pass instead.
2. Start from the explicit mission neighborhood.
   - Re-read the owning mission, direct parent/child edges, named
     dependencies, lane references, and governing flexiargs.
   - Do not begin from semantic search results alone.
3. Run the bounded semantic-neighbor discovery ring.
   - After the explicit local neighborhood is re-read, inspect the wider
     installation for artifacts that are:
     - topologically non-local to the target neighborhood
     - but semantically close to the current overlap, truth claim, or
       abstraction pressure
   - Treat this as a second ring, not as the starting point.
   - Ask:
     - does the current local cleanup actually touch a stronger non-local
       semantic owner?
     - is there a proof/evidence/typed-surface/authority artifact elsewhere in
       the installation that should now be linked?
     - does the new local rule imply a missing cross-reference to a far-but-
       close artifact?
   - Record for each discovered non-local neighbor whether the truthful result
     is:
     - cross-reference in
     - pointerize toward it
     - lift local content toward it
     - or explicitly keep the boundary local and name why
4. Build the overlap map.
   - List the actual duplicated or mixed concerns.
   - For each one, name the current artifacts involved.
   - Include both:
     - local neighborhood overlaps
     - semantically close non-local neighbors discovered in the second ring
   - Ask which artifact should be the canonical owner.
5. Classify the intended edit for each overlap.
   - `keep`:
     - current placement/owner is already correct
   - `split`:
     - one artifact is carrying more than one concern
   - `lift`:
     - repeated local content should move to a stronger shared owner
   - `pointerize`:
     - repeated prose should become a reference to the canonical owner
   - `merge`:
     - two local artifacts are really one concern
   - `drop`:
     - a local duplicate no longer adds value
6. Preserve execution meaning while reducing duplication.
   - Do not compress away mission-local or operator-local instructions that are
     still needed to execute the lane.
   - Keep direct mission edges in mission files.
   - Keep README/docs practical and execution-focused.
7. If code or executable structure is in scope, treat this as refactoring.
   - Preserve behavior unless a semantic change is explicitly intended.
   - Keep invariants and test obligations explicit.
   - If algorithm files are in scope, use:
     `library/social/create-versus-update-algorithm.flexiarg`
     to decide whether the truthful result is:
     - local refactor of the existing algorithm owner
     - extraction of a new sibling/child algorithm
     - or both
   - Run the relevant verification surface before claiming the refactor is
     complete.
8. If topology/wiring pressure appears, call the narrower follow-on routine.
   - Use:
     `algorithms/decide-follow-on-orthogonality-and-devmap-wiring-after-mission-reconstruction.md`
   - Do not overload this algorithm with every devmap-wiring decision if that
     narrower owner is the truthful next step.
9. Update discoverability and routing.
   - Update mission/docs indices, lane catalogs, devmaps, or local READMEs as
     needed so the new boundaries are legible.
   - If the second semantic-neighbor ring found a far-but-close artifact, make
     the resulting cross-reference or explicit non-link legible.
10. Record the pass honestly.
   - Report:
     - what was reused vs new
     - what semantically close non-local neighbors were discovered
     - which of them became new cross-references versus explicit non-links
     - what was split/lift/pointerized/dropped
     - what verification was run, if any
   - Ask whether another follow-on orthogonality pass is still warranted.

## Decision Rule

1. Use this algorithm when the main problem is mixed boundaries or duplicate
   ownership.
2. Use `normalization` language for narrowly mechanical alignment work.
3. Use `refactoring` language when the same boundary-cleanup procedure is
   occurring inside code/executable structure.
4. Keep the stronger umbrella language (`knowledge factoring pass`) in mind,
   but use this algorithm when the narrower orthogonality/refactoring procedure
   is the real operative work.

## Outputs

- a completed bounded orthogonality/refactoring pass
- explicit canonical-owner routing for the touched concerns
- explicit accounting of any semantically close but topologically non-local
  neighbors discovered during the pass
- updated discoverability surfaces if boundaries moved
- reused-vs-new accounting
- explicit continuation if further orthogonalization is still needed

## Stop Conditions

Stop and ask the operator if any of these happen:

- the pass is not actually bounded and is turning into a broad re-architecture
- multiple canonical owners remain equally plausible
- code behavior may have changed but the verification surface is unavailable
- the work is really two separate passes:
  - for example, one normalization pass and one orthogonality/refactoring pass

## Notes

- This algorithm is intentionally about the inside of the cleanup arrow:
  - how to inspect overlap
  - how to discover semantically close but non-local installation neighbors
  - how to classify the edits
  - how to preserve execution meaning while reducing duplication
- It should be read together with the Kolmogorov/factoring flexiargs, not as a
  replacement for them.
- When the pass touches algorithm families specifically, the new-vs-refactor
  choice gate should be made explicit rather than left as tacit author habit.
