# Algorithm: Create New Mission With Operator Discussion and RMR Branch

Date: 2026-03-12
Status: active

## Purpose

Use this when a non-trivial lane appears to need a new mission and the
operator is present to discuss the lane directly.

This algorithm exists to prevent two common failures:
- creating a mission before the real owner, scope, or acceptance is clear
- using reverse morphogenesis reconstruction (`RMR`) when the work is actually
  forward-planned rather than already realized

Practical upstream usage note to preserve:
- Joe's common live prompt is:
  - "Let's make a futonic mission"
- the usual next step is to point the agent at:
  - `futon4/holes/mission-lifecycle.md`
- in `mfuton`, do not automatically use that upstream lifecycle document on
  every run
- instead, ask the operator whether they want to use the current
  mission-creation run as an example to inspect that upstream lifecycle
  document and potentially integrate what is learned back into this
  create-mission algorithm
- the shape is not required to be rigidly identical every time:
  - detail scales to meet the need
- a mission may still be worth creating at a very early stage before the run
  has really started
- the operator also reported Joe citing a `structural-law` mission in
  `futon3c` as an example of an early setup / pre-IDENTIFY mission; preserve
  that usage note even when the exact upstream local path is not currently
  available in this checkout

The required posture is:
- discuss the mission with the operator first
- classify the chronology honestly
- only use `RMR` when the mission is genuinely being reconstructed from
  already-realized work
- keep the mission-creation shape continuous across forward and retroactive
  cases rather than inventing a separate retroactive-only mission style

## Companion Flexiarg Resolution (2026-03-14)

Governing companion flexiargs for this algorithm:
- `library/social/reverse-morphogenesis-for-retroactive-mission-reconstruction.flexiarg`
  - governs the retroactive branch and honest chronology handling
- `library/social/stable-mission-files-with-sibling-artifact-trees.flexiarg`
  - governs stable mission topology and sibling-directory growth rules

Inspected but non-owning candidates:
- `library/system-architecture/devmap-wiring-decision-for-floating-and-retroactive-missions.flexiarg`
  - governs the narrower post-creation devmap-wiring decision rather than the
    whole mission-creation run
- `library/system-architecture/open-world-ingest-mission-reconstruction-and-canonicalization-factorization.flexiarg`
  - broader architectural factorization, but not the direct operational owner
    of day-to-day mission creation

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

## Inputs

- the operator's request, problem statement, or lane discussion
- the current explicit mission neighborhood in:
  - `gh/mfuton`
  - `darktower/futon3c`
- any realized support evidence if work already happened:
  - diffs
  - receipts
  - transcripts
  - notes
  - prior artifacts
- candidate parent missions and devmaps

## Preconditions

1. The lane is non-trivial enough that a mission may be warranted.
2. The operator is available for at least the initial mission-shaping
   discussion, unless the mission is being reconstructed post hoc from already
   realized work.
3. The explicit mission neighborhood will be inspected before creating a new
   mission file.

## Procedure

1. Start with operator discussion, not file creation, and capture reusable
   creation deltas from this run.
   - If this run surfaces a better practical mission-creation move, prompt
     form, staging pattern, or upstream usage note than this algorithm already
     records, update this algorithm before ending the mission-creation wave so
     the next run incorporates that learning directly.
   - Treat that update as part of the mission-creation work, not an optional
     afterthought.
   - In live operator collaboration, a good practical prompt form is:
     - "Let's make a futonic mission"
   - Ask whether the operator wants to use this current mission-creation run
     as an example for inspecting the upstream lifecycle reference and
     improving this create-mission algorithm:
     - `futon4/holes/mission-lifecycle.md`
   - Only if the operator says yes:
     - inspect it
     - compare it against the current create-mission routine
     - integrate any useful learned delta back into this algorithm before
       ending the mission-creation wave
   - Preserve the upstream practical read:
     - detail scales to meet the need
     - the first written mission may be lightweight or pre-IDENTIFY rather
       than already fully elaborated
   - First try to infer from the operator's text:
     - `trigger`
     - `issue`
     - `suggested solution`
     - strongest current owner or parent lane
     - first acceptance / receipt target
   - Prompt only for the fields that remain missing, ambiguous, or conflicting
     after that inference pass.
   - Ask what the lane is trying to accomplish.
   - Ask what should count as success or closure.
   - Ask whether the work is mostly future work or whether the important work
     already happened.
   - Ask what current owner or parent lane seems strongest.
   - When presenting candidate next-step choices or owner options, use an
     alphabetical list so the operator can reply with letters directly.
2. Inspect explicit reuse first.
   - Check `gh/mfuton` for nearby missions, flexiargs, algorithms, and docs.
   - Check `darktower/futon3c` for upstream or adjacent owner intent that may
     already cover the lane.
   - Prefer extension or refinement of an existing owner when that is cleaner
     than creating a near-duplicate mission.
3. Classify the chronology honestly.
   - `forward mission creation` when the mission is being created before the
     substantive work exists.
   - `retroactive mission reconstruction` when the work already happened and
     the mission is being written after the realized delta.
   - Do not use `RMR` as a stylistic flourish for forward-planned work.
4. If the lane is retroactive, run the `RMR` branch honestly.
   - Use:
     `library/social/reverse-morphogenesis-for-retroactive-mission-reconstruction.flexiarg`
   - Keep the factoring explicit:
     - `RMR` changes chronology handling, not the existence of the mission
       creation routine
     - after honest chronology classification, continue through the same
       mission-creation path used for forward missions
     - do not fork a second ad hoc "retroactive mission writer" that bypasses
       the practical futonic mission shape
   - If the retroactive branch reveals a missing mission-creation heuristic or
     a better practical prompt into this routine, feed that back into Step 1 so
     the create-mission algorithm itself improves for the next run.
   - Identify:
     - realized delta (`Delta`)
     - reconstructed mission owner (`M`)
     - reconstructed situation (`Sigma`)
   - Cite the concrete support evidence.
   - Keep the mission prose honest about chronology instead of writing as if
     the mission always existed first.
5. If the lane is forward, keep the future-oriented branch explicit.
   - Name the goal, scope-in, scope-out, and acceptance without laundering the
     work as already realized.
   - Preserve unresolved questions as scope notes or open decisions instead of
     pretending they are already closed.
   - Allow the first mission draft to stay lightweight when the lane is still
     being set up, provided the current stage and open unknowns are legible.
6. Choose the strongest current owner before writing.
   - Start from the explicit mission neighborhood:
     - parent mission
     - direct dependencies
     - lane catalog
     - governing flexiargs already named there
   - Decide whether the new mission belongs:
     - under an existing parent mission
     - as a new top-level lane owner
     - as a bounded child under a stronger active owner
   - If the strongest honest first owner is instead a bounded docs note,
     docs neighborhood, or operator-facing routing surface, stop the mission
     creation path and route into:
     `algorithms/merge-artifact.md`
     rather than forcing a weak new mission.
7. Write only the canonical mission file first.
   - Use:
     `docs/mission-creation-checklist.md`
   - Create:
     `holes/missions/M-<mission-id>.md`
   - Add the standard mission artifact authority block, policy routing block,
     and practical scope/acceptance sections.
   - Add a sibling directory only if the mission already has children or
     node-local artifacts.
8. Make the topology state explicit.
   - After the mission exists, decide whether it is:
     - `wire-now`
     - `floating-by-design`
     - `floating-pending-cleanup`
   - Use:
     `algorithms/decide-follow-on-orthogonality-and-devmap-wiring-after-mission-reconstruction.md`
     and the devmap-wiring flexiarg
   - If the mission is forward-created rather than retroactive, still record
     the wiring state explicitly.
9. Publish the local discoverability updates.
   - Update:
     - `holes/missions/README.md`
     - the relevant lane catalog if needed
     - any directly relevant mission/owner pointers
10. Offer explicit operator revision/finalization.
   - Summarize:
     - the chosen owner
     - forward vs retroactive classification
     - the drafted mission id/title
     - the first receipt / acceptance target
     - the current dirty set that belongs to this mission-creation wave
   - Ask whether the operator wants mission-text revision first or approves the
     current draft for finalization.
   - If approval is not yet given:
     - leave the mission draft uncommitted
     - keep the open questions and requested revisions explicit
   - If approval is given:
     - treat that approval as the creation-phase finalization receipt for the
       drafted mission text
11. Record reused vs new and run the follow-on orthogonality pass.
   - Name the reused authorities and stronger owners.
   - Name the new mission artifact and cross-references created.
   - If the new mission introduced a meaningful new local topology or aha
     connection, first add the obvious connected-neighborhood links around the
     mission before reducing overlap.
   - Call out any missing cross-references or still-weak owner boundaries.
   - The follow-on orthogonality pass is not optional.
   - It occurs after any requested mission-text revision and after operator
     approval of the drafted mission text.
   - Treat this as the second step after the link-enrichment step above.
   - Use:
     `algorithms/conduct-knowledge-orthogonalization-pass.md`
   - If the pass concludes that no broader cleanup is needed, record that
     trivial result explicitly rather than silently skipping the pass.
   - If the remaining question is specifically mission topology or devmap
     wiring, keep using:
     `algorithms/decide-follow-on-orthogonality-and-devmap-wiring-after-mission-reconstruction.md`
     as the narrower owner.
12. If approval remains in force after the orthogonality pass, finalize the
    mission-creation wave through the commit algorithm.
   - Route into:
     `algorithms/conduct-commit-phase-with-diff-and-total-dirty-closure.md`
   - Checkpoint the mission-creation wave in `gh`.

## Decision Rule

1. Discuss first with the operator when the mission is still being formed.
2. Use `RMR` only when the mission is honestly reconstructed from realized
   work.
3. Keep forward mission creation forward-looking.
4. Prefer the strongest existing owner that can contain the new mission
   cleanly.

## Outputs

- one of:
  - a newly created forward mission
  - a newly reconstructed retroactive mission using `RMR`
  - an explicit decision that a new mission should not yet be created
- updated owner/index wiring for discoverability
- an explicit devmap-wiring state
- an explicit operator finalization decision
- a completed follow-on orthogonality pass, including the explicit case where
  it ended trivially
- when approved, a checkpoint commit through the governed `gh` commit algorithm

## Stop Conditions

Stop and ask the operator if any of these happen:

- multiple parent owners remain equally plausible after explicit-neighborhood
  inspection
- the chronology is unclear enough that you cannot honestly tell whether the
  lane is forward-created or retroactive
- the lane is actually two missions and cannot be kept truthful in one file
- the work is still too exploratory and should remain an idea, note, or
  discussion instead of becoming a mission
- the strongest honest first owner is a docs surface rather than a new mission

## References

- `docs/mission-creation-checklist.md`
- `holes/missions/mission-template.md`
- `futon4/holes/mission-lifecycle.md`
- `library/social/reverse-morphogenesis-for-retroactive-mission-reconstruction.flexiarg`
- `library/social/stable-mission-files-with-sibling-artifact-trees.flexiarg`
- `library/system-architecture/open-world-ingest-mission-reconstruction-and-canonicalization-factorization.flexiarg`
- `library/system-architecture/devmap-wiring-decision-for-floating-and-retroactive-missions.flexiarg`
- `algorithms/conduct-knowledge-orthogonalization-pass.md`
- `algorithms/decide-follow-on-orthogonality-and-devmap-wiring-after-mission-reconstruction.md`
- `algorithms/merge-artifact.md`
- `algorithms/conduct-commit-phase-with-diff-and-total-dirty-closure.md`
- `holes/missions/M-mfuton-pragmatic-operational-problem-solving-codification/M-algorithm-creation-and-flexiarg-factoring.md`
