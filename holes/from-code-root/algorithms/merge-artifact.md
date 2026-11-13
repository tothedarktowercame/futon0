# Algorithm: Merge Artifact

Date: 2026-03-19
Status: active

## Purpose

Use this when a live operator/evolver session or bounded subflow has produced
one or more durable deltas that should be reconciled back into the
installation as artifacts, starting with bounded current owners such as:

- mission artifacts
- idea artifacts
- flexiargs
- algorithms
- docs-owned operator-facing or routing artifacts when they are the strongest
  current owners
- world-backed local issue/case artifacts when the bounded delta belongs there
- immediate discoverability surfaces for the chosen artifact neighborhood

This algorithm is intentionally about reconciliation, not mere storage.

Boundary note:
- when the honest task is renaming an already-existing artifact while
  preserving artifact identity, route first through:
  - `algorithms/rename-artifact.md`
- when the honest task is relocating or reparenting an already-existing
  artifact under a stronger owner, route first through:
  - `algorithms/move-artifact.md`
- `merge-artifact.md` remains the inner writeback/restructuring engine for
  those relocation/rename waves, especially in `large` mode, but it is not
  the narrow front-door classifier for either one

The live session may have produced:

- one clarified idea
- an idea neighborhood
- one new mission
- a mission tree
- one refined flexiarg or one newly justified flexiarg
- one refined algorithm or one newly justified algorithm
- one bounded docs neighborhood or operator-facing note
- one bounded world-backed issue/case update
- one updated index/catalog/README or routing surface
- a mixed artifact wave spanning several of the above
- or no durable writeback at all

The job here is to decide what durable delta honestly belongs in the
installation, merge that delta into the strongest current artifacts, and leave
the remaining detail in the session trace when appropriate.

This algorithm now has two explicit merge modes:

- `small`
  - optimize for the smallest honest durable delta
  - this is the default
- `large`
  - optimize for the best bounded restructuring of the chosen local artifact
    neighborhood
  - use when the operator explicitly wants a major merge/restructuring wave,
    or when the agent detects that a new stronger owner/component/topology
    change makes a minimal writeback dishonest

Preferred control-flow rule:
- begin in `small` unless the operator already requested `large`
- let this algorithm perform the early gate check
- if the gate detects `large` restructuring pressure, stop and ask the
  operator whether to escalate the merge
- do not silently upgrade a merge wave from `small` to `large`

Typical `large` merge pressure includes:

- introducing a new higher owner that should pull older references upward
- reparenting existing artifacts under a new component owner
- changing which artifacts are children, siblings, or reused authorities of a
  newly created owner
- reducing shadow ownership across missions, ideas, docs, devmaps, and indexes

Relocation note:
- if the `large` pressure is specifically "this existing artifact should be
  renamed while preserving artifact identity", use
  `algorithms/rename-artifact.md` as the wrapper that classifies and governs
  that rename wave
- if the `large` pressure is specifically "this existing artifact should move
  to a stronger owner", use `algorithms/move-artifact.md` as the wrapper that
  classifies and governs that relocation wave

This algorithm is not the primary owner of broader semantic-neighbor discovery
across the installation. When the writeback pressure reveals a need to inspect
semantically close but topologically non-local artifacts, route that deeper
boundary discovery through:

- `algorithms/conduct-knowledge-orthogonalization-pass.md`

## Companion Flexiarg Resolution (2026-03-19)

Governing companion flexiargs for this algorithm:
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
  - governs reuse-first writeback into the strongest current durable artifacts
    before inventing new artifacts
- `library/system-architecture/mission-neighborhoods-and-semantic-topology.flexiarg`
  - governs explicit neighborhood routing and the distinction between local
    artifact topology and wider semantic adjacency, even when the bounded
    merge is not mission-first

Inspected but non-owning candidates:
- `library/social/knowledge-factoring-pass-naming-and-subtypes.flexiarg`
  - relevant when the merge wave later expands into a broader factoring pass,
    but not the direct owner of bounded writeback itself
- `library/social/futon-self-description-defers-to-installation-authority.flexiarg`
  - relevant in narrower upstream-authority lanes, not the generic
    installation-artifact merge procedure

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

## Inputs

- the current live session or bounded child-frame result
- merge mode:
  - `small`
  - `large`
- any newly created or newly clarified artifact text from the session
- the explicit artifact neighborhood in:
  - `gh/mfuton`
  - `darktower/futon3c` when upstream ownership may matter
- current owner/index surfaces such as:
  - `holes/missions/README.md`
  - lane reference catalogs in `docs/`
  - mission-local or root-level docs neighborhoods in `docs/`
  - `ideas/README.md`
  - `algorithms/README.md`
  - `library/*/README.md`
  - the relevant `world/` issue/case neighborhood when the delta is world-backed

## Preconditions

1. A bounded session result exists and is legible enough to classify.
2. The question is no longer pure brainstorming only; some durable writeback
   may now be warranted.
3. Existing artifact owners will be inspected before creating or editing a new
   durable artifact.
4. If no merge mode was explicitly chosen:
   - default to `small`
   - and let the early gate check below decide whether that default remains
     honest

## Hard Gated Constraints

1. Mandatory orthogonalization rule:
   every invocation of this algorithm must run
   `algorithms/conduct-knowledge-orthogonalization-pass.md`
   as an explicit substep of the merge wave before the merge can be treated as
   complete.

2. No agent-discretion bypass rule:
   the orthogonalization step is not optional and may not be skipped merely
   because the local writeback appears routine, small, or already "good
   enough".

3. Merge closure rule:
   this algorithm may not report a finished merge wave until it has:
   - applied the chosen artifact updates
   - applied the immediate discoverability/cross-reference updates
   - run the orthogonalization pass explicitly
   - recorded what, if anything, remained outside the bounded merge

4. Large-mode restructuring rule:
   when the operator explicitly chose `large`, this algorithm must not be run
   as a disguised `small` merge. Large mode requires an actual bounded
   restructuring pass over the chosen local artifact neighborhood, including
   broader-neighborhood inspection and at least one of:
   - reparenting
   - rerouting
   - shadow-ownership reduction
   - stronger-owner confirmation with corresponding neighborhood rewiring

5. Large-mode insufficiency rule:
   adding only one new note, one new mission, or a few index links is not by
   itself enough to satisfy `large` mode when the surrounding neighborhood
   still carries the old topology unchanged.

6. Large-mode accounting rule:
   a completed `large` merge must explicitly state:
   - what broader neighborhood was inspected
   - what was reparented or rerouted
   - what shadow ownership was reduced
   - what old topology intentionally remained and why

## Procedure

1. Bind merge mode before editing files.
   - Decide whether the current wave is:
     - `small`
     - `large`
   - Default:
     - `small`
   - Use `large` when the honest task is not merely preserving one bounded
     result but restructuring a local artifact neighborhood around a stronger
     owner.
   - Optimization target by mode:
     - `small`:
       - smallest honest durable delta
     - `large`:
       - best bounded restructuring of the chosen local neighborhood
   - If the merge began as implicit/default `small`, keep that provisional
     until the next early gate step confirms it remains honest.

2. Run the early merge-mode gate.
   - Before doing substantive writeback, ask whether the wave still honestly
     fits `small`.
   - Signals that the wave likely wants `large` instead:
     - a new stronger owner has appeared and several older artifacts should now
       point upward to it
     - the truthful merge requires reparenting or rerouting multiple existing
       artifacts
     - ideas/docs/devmaps/indexes are currently shadow-owning the same
       question and should be cleaned up together
     - leaving the wave in `small` mode would preserve a topology known to be
       misleading rather than merely incomplete
   - If those signals do not appear:
     - continue in `small`
   - If those signals do appear and the operator already requested `large`:
     - continue in `large`
   - If those signals do appear and the current mode is still implicit/default
     `small`:
     - stop and ask the operator whether to escalate the merge into `large`
     - do not proceed as if that judgment can be made unilaterally by the
       agent

3. Classify the session result before editing files.
   - Decide whether the current result is mainly:
     - mission shaping or mission refinement
     - idea shaping or idea refinement
     - flexiarg shaping or flexiarg refinement
     - algorithm shaping or algorithm refinement
     - docs-neighborhood shaping or docs-neighborhood refinement
     - world-backed issue/case artifact refinement
     - discoverability/index refinement
     - mixed artifact updates
     - still too exploratory for durable writeback
   - A docs-first result is acceptable when the strongest honest durable delta
     is a bounded routing note, operator-facing explanation, runbook, or
     mission-local docs neighborhood rather than a new mission, idea,
     flexiarg, or algorithm.
   - A world-backed result is acceptable when the strongest honest durable
     delta is a bounded change to issue/case state, notes, links, or events
     rather than a new mission/idea/flexiarg/algorithm.
   - If the honest answer is `still too exploratory`, stop cleanly and leave
     the result in the session trace rather than forcing a weak installation
     update.

4. Inspect reuse and strongest current artifacts first.
   - Check `gh/mfuton` for the strongest current artifact owners.
   - Check `darktower/futon3c` when the lane may already have an upstream owner
     or adjacent authority.
   - Prefer refining an existing artifact when that is cleaner than creating a
     near-duplicate.
   - In `small` mode:
     - keep the inspection bounded to the strongest current artifacts needed
       for the merge wave
   - In `large` mode:
     - inspect the broader local owner neighborhood needed to reach a good
       local fixed point, such as:
       - parent mission/devmap/docs neighborhoods
       - direct child or sibling owners now affected by a stronger owner
       - nearby ideas/indexes that currently shadow-own the same question
     - this broader-neighborhood inspection is mandatory in `large` mode and
       may not be reduced to only the immediately edited file set

5. Decide the honest durable artifact delta for the chosen mode.
   - Ask:
     - in `small` mode:
       - what is the smallest durable writeback that preserves the session
         result truthfully?
     - in `large` mode:
       - what is the best bounded restructuring of this neighborhood that
         leaves the topology in a good local state?
     - what belongs in an existing artifact?
     - what, if anything, deserves a new artifact?
     - what should remain only in the session trace?
   - A common honest shape in `small` mode is a docs-first merge wave:
     - discuss and sharpen the result first
     - create one bounded docs note or docs neighborhood because it is the
       clearest current explanatory artifact
     - wire that docs delta into the owning mission, lane catalog, or adjacent
       indexes
     - run a bounded orthogonality pass afterward if the first merge created
       overlap or blurry boundaries
   - A common honest shape in `large` mode is a restructuring merge wave:
     - create or confirm a stronger current owner
     - move older references upward to that owner
     - reroute nearby artifacts into child/sibling/reused-authority roles
     - reduce shadow ownership in ideas/docs/devmaps/indexes
     - leave the neighborhood in a clearly better topological state than it
       had before the merge
   - In `large` mode, do not stop after the first local writeback that merely
     records the discovery. The wave must continue until the chosen local
     neighborhood has actually been restructured as far as the bounded pass can
     honestly take it.
   - Do not force a full session transcript into the installation.

6. Keep trace retention distinct from active installation merge.
   - The session trace may preserve the fuller exploratory reasoning.
   - The installation merge should usually preserve only:
     - the clarified claim or result
     - the chosen artifact(s)
     - the durable artifact delta
     - the important cross-references
   - A good merge does not pretend the session trace and the installation are
     the same object.

7. If the result is mission-facing, route through mission artifacts rather
   than inventing ad hoc mission prose.
   - Reuse:
     - `algorithms/create-mission.md`
     - `docs/mission-creation-checklist.md`
   - Decide whether the mission result is:
     - new mission
     - mission tree expansion
     - mission refinement
     - no mission writeback yet
   - If a new or revised mission needs topology/index updates, make them part
     of the merge rather than leaving them implicit.

8. If the result is idea-facing, merge into the idea neighborhood honestly.
   - Reuse:
     - `algorithms/conduct-discussion-on-idea-before-writeup-or-promotion.md`
     - `algorithms/conduct-brainstorming-discussion-on-idea-or-artifact.md`
   - Decide whether the durable idea result is:
     - one clarified idea
     - an idea neighborhood
     - a companion idea
     - a promotion trigger but not yet a durable idea artifact
   - Keep idea notes exploratory unless a stronger artifact owner has already
     been chosen.

9. If the result is flexiarg-facing, route through flexiarg artifacts
   honestly.
   - Reuse:
     - `algorithms/create-flexiarg-from-recurring-judgment-surface.md`
     - `algorithms/update-flexiarg-from-recurring-judgment-surface.md`
   - Decide whether the durable flexiarg result is:
     - one new flexiarg
     - one bounded refinement of an existing flexiarg
     - a pressure that still belongs in mission/idea/docs instead
   - Keep judgment-surface ownership explicit rather than hiding it in a
     mission note or docs artifact.

10. If the result is algorithm-facing, route through algorithm artifacts
   honestly.
   - Reuse:
     - `algorithms/create-algorithm.md`
     - `algorithms/update-algorithm.md`
   - Decide whether the durable algorithm result is:
     - one new algorithm
     - one bounded refinement of an existing algorithm
     - a pressure that still belongs in flexiarg/docs/mission instead
   - Keep procedural ownership explicit rather than burying it in adjacent
     artifacts.

11. If the result is docs-facing, merge into the docs neighborhood honestly.
   - Reuse the strongest current docs artifact when one exists:
     - a mission-local docs neighborhood
     - a root-level docs neighborhood
     - an existing operator-facing note or runbook
   - Decide whether the durable docs result is:
     - one routing note
     - one operator-facing explanation or runbook
     - one small docs neighborhood
     - a refinement of an existing docs neighborhood
     - or no docs writeback yet
   - Keep docs-owned material bounded:
     - docs own operator-facing explanation, retrieval/routing notes,
       runbooks, and mission-local docs neighborhoods
     - docs do not silently absorb mission scope/acceptance or leave an
       exploratory idea neighborhood disguised as settled documentation
   - If docs are created first, follow immediately with owner/discoverability
     wiring so the docs do not remain free-floating.

12. If the result is world-backed, merge into the local issue/case artifact
    neighborhood honestly.
   - Reuse the strongest current issue/case artifact when one exists:
     - `world/issues/...`
     - `world/futon-issues/...`
     - the bounded `issue.md`, `notes.md`, `links.edn`, or `events.ndjson`
       surface that actually owns the delta
   - Decide whether the durable world-backed result is:
     - one local issue/case clarification
     - one bounded note/event/link update
     - a promotion trigger into mission work but not yet that mission itself
     - or no world-backed writeback yet
   - Keep case-state ownership explicit rather than burying it in a free-
     floating discussion note.

13. Apply the core artifact updates.
   - In `small` mode:
     - edit only the strongest artifact owners required by the chosen minimal
       delta
   - In `large` mode:
     - edit the bounded restructuring set required to leave the neighborhood
       in a good local topological state
     - this means the edited set should normally extend beyond the brand-new
       artifact itself and include the surrounding owners/indexes/adjacent
       artifacts whose topology is being corrected
   - When several artifact kinds are involved, keep the split explicit:
     - mission owns scope/acceptance/derivation/dependencies
     - idea owns exploratory conceptual content unless promoted further
     - flexiarg owns reusable judgment surfaces
     - algorithm owns procedural spines
     - docs own operator-facing explanation, routing, runbooks, and docs
       neighborhoods
     - world-backed artifacts own local issue/case state and evidence-local
       continuity

14. Apply the necessary discoverability and cross-reference updates.
   - Update the immediate index or lane surfaces needed so the merge is
     discoverable.
   - Treat this as the first explicit topology step before orthogonality:
     add the immediate connected-neighborhood links needed so the resulting
     owner and its adjacent reused artifacts are honestly visible.
   - When the merge carries an aha/reframing or other topology-shaping
     insight, prefer deliberate excess local linkage first:
     - the stronger owner
     - immediate siblings/children/parents
     - nearby informing notes or glossary surfaces
     - semantically important upstream or non-local neighbors already known in
       the wave
     - only after that visibility exists should the pass reduce overlap
   - Typical examples:
     - `holes/missions/README.md`
     - lane reference catalogs in `docs/`
     - `docs/README.md`
     - mission-local docs neighborhood `README.md`
     - `ideas/README.md`
     - `algorithms/README.md`
     - `library/*/README.md`
     - direct parent/child or adjacent-artifact pointers
   - Do not scatter wide index edits unless the merge truly changes those
     broader surfaces.

15. Run orthogonality explicitly as a required merge subroutine.
   - Use:
     `algorithms/conduct-knowledge-orthogonalization-pass.md`
   - This is the required second topology step after the
     link/discoverability enrichment above in every merge wave, not a
     substitute for it.
   - In both `small` and `large` mode:
     - keep the merge wave bounded
     - treat the orthogonality pass as an explicit child step of the merge
       rather than re-performing its internal procedure inline
     - return to this algorithm afterward to finish the bounded installation
       writeback and trace/non-trace accounting
   - The orthogonality pass remains bounded by the chosen merge wave; it does
     not authorize an unbounded repository sweep.

16. Make explicit what was not merged.
   - Name what remains only in the session trace.
   - Name any open questions or still-unmerged neighboring artifacts.
   - If helpful, point to the next likely artifact owner or promotion trigger.

17. Offer adjacent improvements beyond the required orthogonality step rather
    than silently expanding the wave.
   - If you see nearby useful refinements beyond the core merge, name them
     explicitly.
   - Ask the operator whether to include those adjacent improvements now.
   - Typical adjacent improvements may include:
     - non-local semantic-neighbor discovery and authority routing
     - index tightening
     - artifact-boundary clarification
     - algorithm/checklist improvement triggered by the current run

18. If adjacent improvements are approved, merge them as a bounded extension of
    the same wave.
   - Keep the extension explicit.
   - Do not silently turn a small merge into an unbounded repository sweep.

19. Finalize the merge wave honestly.
   - Summarize:
     - merge mode:
       - `small`
       - `large`
     - reused vs new
     - chosen artifacts
     - core merged delta
     - what stayed only in trace
     - the required orthogonality subroutine that was invoked
     - any adjacent improvements included
     - the current dirty set
   - In `large` mode, also summarize:
     - what was reparented or rerouted
     - what broader neighborhood was inspected
     - what shadow ownership was reduced
     - what remains a sibling rather than a child
     - what still shadows the stronger owner and should likely be cleaned up
       later
   - If the operator approves checkpointing, route into:
     - `algorithms/conduct-commit-phase-with-diff-and-total-dirty-closure.md`

## Outputs

- one of:
  - a bounded merge into existing installation artifacts
  - a bounded merge that includes new installation artifacts plus
    discoverability updates
  - an explicit decision that no durable merge should happen yet
- a clear distinction between:
  - what was merged into the installation
  - what remains only in the session trace
- explicit composition with the orthogonality algorithm when boundary cleanup
  is part of the truthful merge wave
- an explicit adjacent-improvement decision
- an explicit merge-mode declaration so bounded small writeback and broader
  local restructuring are not silently conflated

## Stop Conditions

Stop and ask the operator if any of these happen:

- the strongest current artifact owner is still ambiguous after explicit-
  neighborhood inspection
- the session result is still too exploratory to merge honestly
- the result seems to be two different durable deltas that should not be merged
  in one wave
- the wave clearly wants `large` restructuring but the operator has not yet
  approved that broader topology rewrite
- docs-owned explanation and mission-owned scope remain entangled enough that
  they should first be split before writeback
- the needed merge would require a broader orthogonality/refactor sweep than is
  honest to smuggle into the current wave
- the session writeback depends on discovering semantically close but
  topologically non-local installation artifacts whose owner/boundary impact is
  not yet clear and the orthogonality subroutine cannot be bounded honestly

## References

- `algorithms/create-mission.md`
- `algorithms/conduct-discussion-on-idea-before-writeup-or-promotion.md`
- `algorithms/conduct-brainstorming-discussion-on-idea-or-artifact.md`
- `algorithms/create-flexiarg-from-recurring-judgment-surface.md`
- `algorithms/update-flexiarg-from-recurring-judgment-surface.md`
- `algorithms/create-algorithm.md`
- `algorithms/update-algorithm.md`
- `algorithms/conduct-knowledge-orthogonalization-pass.md`
- `algorithms/move-artifact.md`
- `algorithms/conduct-commit-phase-with-diff-and-total-dirty-closure.md`
- `ideas/retrieve-and-merge-as-runtime-dual-for-installation-reconciliation.md`
- `docs/mission-creation-checklist.md`
- `docs/M-my-familiar-software-development-operating-posture/artifact-role-reconciliation.md`
- `holes/missions/M-mfuton-pragmatic-operational-problem-solving-codification/M-algorithm-creation-and-flexiarg-factoring.md`
