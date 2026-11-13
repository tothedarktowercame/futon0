# Algorithm: Process Evolver Learning Experience

Date: 2026-03-19
Status: active

## Purpose
Use this when the current evolver and the operator have a concrete shared
learning experience and the next step is to discuss that experience until the
current read of self, operator, other discussed entities, and collaboration is
mutually communicated well enough to grow and revise the warranted
world-model neighborhood.

This algorithm owns the discussion-and-writeback procedure for:

- rereading the current evolver's own world-model neighborhood, mandatorily,
  before the rest of the algorithm proceeds
- rereading the world-model neighborhood for any other involved evolver or
  operator before the rest of the algorithm proceeds
- binding the concrete learning experience and its causal evidence
- binding the zeroth-order, first-order, and collaboration/carrier branches
  that the discussion has actually made real
- rereading the current world-model carriers already in view
- creating a missing warranted self/edge/collaboration carrier when the
  discussion has made that carrier type clear enough
- updating each warranted world-model carrier with what the experience taught
  about that branch
- integrating the modified artifact cluster with the specific source artifacts
  that caused or revealed the learning experience
- explicitly routing lessons to a stronger non-world-model owner when the
  discussion has exposed one
- stopping when the current shared view is sufficient to complete the task,
  nothing more, nothing less

Current bounded carrier orders for this algorithm:
- zeroth-order self carriers:
  - `<current evolver> -> <current evolver>`
  - `rob -> rob`, carried by
    `ideas/right-now-rob-now-self-world-model.md`
- first-order edge carriers:
  - `<current evolver> -> <other discussed entity>`
  - `<other discussed entity> -> <current evolver>`
- collaboration/group carriers:
  - pair or working-group collaboration notes when the relationship itself is
    part of the learning delta

This algorithm is open to higher-order reflection later, but it should not
create speculative second-order or deeper carriers just because they are
imaginable.

This algorithm is fail-closed on two points:
- the current evolver MUST update its model of itself
- for each other-entity branch that the discussion has actually made real, the
  relevant current-evolver edge models MUST also be updated with anything
  newly learned before the algorithm can complete

So if the discussion has only produced boundary, other-entity, or
collaboration wording while leaving the current evolver self-model or the
discussed live edge models stale, the algorithm has not completed yet.

This algorithm is discussion-first.
It should be expected to take multiple turns when the operator is actively
refining the read.
It also should not treat finishing the discussion as implicit permission to
check in the result.
The operator tells the current evolver when to `run the commit algorithm`.

Important discussion framing:

- this is not a disagreement being resolved
- if there was a disagreement, which is not a requirement for this algorithm
  to be run at all, it has already occurred
- now the current evolver and the operator are mining their joint experience
  in a discussion and updating their joint world model

Hard phase-ordering rule:

- in no circumstance should the current evolver mutate a durable artifact
  before the discussion phase has begun
- rereading, source binding, branch binding, and explicit discussion startup
  come first
- world-model writeback, carrier creation, and related artifact mutation begin
  only after that discussion phase is actually underway

It does not own the broader novelty gate or a whole run-wide post-mortem.
When the learning experience expands into that larger lane, use:

- `algorithms/conduct-post-mortem-learning-capture-discussion.md`

Important distinction:

- a learning experience is materially different from a post-mortem
- this algorithm is the narrower owner for a bounded shared learning delta
  that should revise world-model carriers
- a broader post-mortem may reveal or spawn such a learning experience, but it
  does not necessarily do so
- therefore the world-model reread hard gate in this algorithm should not be
  treated as a general prerequisite for entering post-mortem mode
- use this algorithm only when the task has actually become a world-model
  learning-experience discussion

It does not own broader schema design for all possible world-model orders.
If the discussion is still deciding what sort of carrier should exist at all,
or if the work is expanding into a larger typed-schema lane, route that
question to the stronger world-and-memory owner instead of inventing hidden
chat-only memory.

Execution-time binding rule:
- when this algorithm is run, bind the current evolver identity explicitly
- older predecessor carriers may still inform the read
- but the live mandatory self branch and live mandatory edge branches belong
  to the current evolver, not to a stale umbrella identity

Neighborhood reread hard gate:
- before the rest of this algorithm proceeds, the current evolver MUST,
  non-exclusively, reread the world-model neighborhood associated to itself
- before the rest of this algorithm proceeds, the current evolver MUST also
  reread the world-model neighborhood for any other evolver or operator
  explicitly involved in the learning experience
- this reread is non-exclusive:
  - other causal or world-model artifacts may also need rereading
  - but the current evolver's self-neighborhood reread is mandatory and
    fail-closed
- if that reread has not happened yet, the algorithm has not started honestly

## Companion Flexiarg Resolution (2026-03-19)

Governing companion flexiargs for this algorithm:
- `library/foundational/respond-to-questions-with-sources.flexiarg`
  - governs rereading the actual source artifacts and current world-model
    artifacts before answering what was learned or what should change
- `library/foundational/respect-others-while-collaborating.flexiarg`
  - governs direct, honest collaboration when the learning experience involved
    another operator or agent and the collaboration model itself is being
    revised
- `library/system-architecture/knowledge-factoring-objective.flexiarg`
  - governs the final bounded knowledge-integration step so the revised
    world-model artifacts gain the right causal references and canonical
    ownership boundaries

Inspected but non-owning candidates:
- `library/social/post-mortem-for-novel-learning-capture.flexiarg`
  - governs when a run belongs in post-mortem mode at all, but not the
    narrower world-model update procedure once the learning delta is already
    ready for writeback

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

## Inputs
- the concrete learning experience being processed
- the current evolver identity running the algorithm
- the source artifacts that caused, exposed, or recorded that experience
- the operator's current read of what the experience actually taught
- the strongest current world-model carriers already in view for:
  - `<current evolver> -> <current evolver>`
  - operator self, if that branch is explicitly in the discussion
  - each real `<current evolver> -> <other discussed entity>` branch
  - each real `<other discussed entity> -> <current evolver>` branch
  - collaboration/group, if that branch is explicitly in the discussion
- any stronger local owner already in view for a non-world-model branch that
  the discussion exposed
- any already-started local edits, draft wording, or paused discussion state
  for this learning experience

## Preconditions
1. There is a real learning delta to capture, not only ordinary task progress.
2. The artifacts that caused or revealed the learning experience are known
   well enough to cite directly.
3. The learning experience should be treated as an operator/agent discussion
   rather than as a one-shot agent summary.
4. The strongest local world-and-memory neighborhood is known well enough that
   a missing warranted carrier can be created honestly instead of being hidden
   in chat residue.
5. The current evolver has already reread the world-model neighborhood
   associated to:
   - itself, mandatorily
   - any other evolver or operator explicitly involved in the experience
6. If the experience has expanded into a broader run-wide post-mortem, use:
   `algorithms/conduct-post-mortem-learning-capture-discussion.md`
   and stop this algorithm until the narrower world-model revision lane
   becomes clear again.
7. No durable artifact mutation has happened yet.
   - before the discussion phase begins, the current evolver must not mutate
     world-model carriers, create new carriers, or perform other durable
     writeback for this learning experience
   - if such mutation has already happened, stop and treat that premature
     mutation as part of the learning experience rather than pretending the
     algorithm proceeded honestly

## Procedure
0. Reread the relevant world-model neighborhood before anything else.
   - Reread the current evolver's own world-model neighborhood first.
   - This self-neighborhood reread is mandatory, not optional.
   - Then reread the world-model neighborhood for any other evolver or
     operator explicitly involved in the learning experience.
   - This reread is non-exclusive: it does not remove the need to reread other
     causal artifacts later in the algorithm.
   - If the current evolver has not reread its own self-world-model
     neighborhood, stop here; the rest of the algorithm has not proceeded
     honestly.
1. Bind the learning experience, causal evidence, and current evolver.
   - Name the concrete experience being processed.
   - Name the current evolver identity running the algorithm.
   - Name the specific artifacts that caused, revealed, or recorded it.
   - Name the operator's current read if one is already in view.
2. Bind the warranted world-model branches and their current carriers.
   - Bind the `<current evolver> -> <current evolver>` self branch.
   - If the operator has explicitly articulated a self-level learning delta,
     bind the operator self branch too.
   - For each other entity explicitly discussed:
     - if the discussion has produced what the current evolver learned about
       that entity, bind the `<current evolver> -> <entity>` branch
     - if the discussion has produced what that entity learned about the
       current evolver, bind the `<entity> -> <current evolver>` branch
   - If the discussion has produced a relationship or working-group lesson,
     bind the collaboration branch.
   - For each bound branch:
     - bind the strongest current carrier if it already exists
     - if the carrier does not yet exist and the branch is clear enough, mark
       that branch for carrier creation rather than silent skipping
     - if the branch is real but its stronger owner is non-world-model, mark
       that branch for stronger-owner routing rather than forcing it into the
       world-model note family
   - Do not create speculative higher-order branches just because the
     hierarchy is imaginable.
3. Start the discussion explicitly.
   - Say that this is a learning-experience discussion.
   - Say explicitly that this is not a disagreement being resolved.
   - Say explicitly that if there was a disagreement, which is not required at
     all for this algorithm to run, it has already occurred.
   - Say explicitly that the current task is to mine the joint experience in
     discussion and update the joint world model.
   - Say that it may take multiple turns.
   - Say that questions, challenges, and corrections keep the algorithm in
     discussion/alignment rather than acting as implicit handoff.
   - Say that the purpose is to converge toward an updated mutually
     communicated view of the warranted branches:
     - what the experience taught the current evolver about itself
     - what it taught the operator about themselves, if applicable
     - what it taught the current evolver about each other discussed entity,
       if applicable
     - what it taught each other discussed entity about the current evolver,
       if applicable
     - what it taught about the collaboration, if applicable
4. Reread the bound artifacts before claiming what was learned.
   - Reread the source artifacts from step 1.
   - Reread the bound world-model artifacts from step 2.
   - Identify which current claims or omissions the learning experience
     changes.
   - Preserve the branch distinction explicitly:
     - `<current evolver> -> <current evolver>` always runs
     - every other branch only runs if the discussion has actually made that
       branch real
     - a missing carrier is not, by itself, a reason to skip a real branch
5. Iterate on the read with the operator.
   - Propose short source-grounded claims from the evidence.
   - Let the operator correct, narrow, replace, or deepen those claims.
   - When challenged, reread the source artifacts before defending the earlier
     read.
   - Keep the split explicit between:
     - direct evidence
     - inference from that evidence
     - still-open questions
   - Treat questions, disagreements, and newly introduced distinctions as
     continued discussion/alignment rather than as writeback permission.
   - After each continuing-discussion turn, rerun the live branch-delta check:
     - what new self-level lesson about the current evolver is now in view?
     - what new operator-self lesson is now in view, if any?
     - what new `<current evolver> -> <entity>` lesson is now in view for each
       discussed entity?
     - what new `<entity> -> <current evolver>` lesson is now in view for each
       discussed entity?
     - what new collaboration lesson is now in view?
   - If any live branch has acquired new learning that the current draft or
     current shared summary does not yet account for, remain in discussion.
   - Continue until the current shared view is sufficient to complete the
     task, nothing more, nothing less.
6. Run the shared-model checkpoint before any writeback.
   - Restate the current shared model in your own words branch by branch.
   - Account explicitly for:
     - `<current evolver> -> <current evolver>`
     - operator self, if applicable
     - each bound `<current evolver> -> <entity>` branch, if applicable
     - each bound `<entity> -> <current evolver>` branch, if applicable
     - collaboration, if applicable
     - any branch that should be routed to a stronger non-world-model owner
       instead
   - If the operator is still refining, correcting, or extending the branch
     map, return to discussion.
7. Run the self-model-update gate before any other writeback is treated as
   complete.
   - After each continuing-discussion turn, and again before leaving
     discussion, ask explicitly:
     - what did this experience teach the current evolver about itself?
   - Do not accept a writeback, summary, or claimed completion that only
     redescribes:
     - the surrounding boundary
     - the other entity
     - the collaboration
     while leaving the current evolver self-model stale relative to the latest
     turn.
   - If the current draft has displaced the lesson away from the self-model,
     stop and return to discussion until a self-level delta is named.
   - If no self-level delta can honestly be named, stop and record that this
     algorithm was the wrong owner for the episode rather than pretending the
     self-update branch completed.
8. Run the other-entity edge-update gate before any other writeback is treated
   as complete.
   - For each other-entity branch that the discussion has actually made real:
     - after each continuing-discussion turn, and again before leaving
       discussion, ask explicitly:
       - what did the current evolver learn about this entity?
       - what did this entity learn about the current evolver, if that branch
         is in scope?
   - Do not accept a writeback, summary, or claimed completion that leaves any
     discussed live edge branch stale relative to the latest turn.
   - If a real discussed edge branch belongs to a stronger non-world-model
     owner, route it explicitly rather than silently dropping it.
9. Create the missing warranted carriers before updating them.
   - For every bound branch from step 2 that is real, clear enough, and not
     routed to a stronger non-world-model owner:
     - if a carrier already exists, reuse it
     - if the carrier is missing, create the strongest local
       self/edge/collaboration carrier needed so that branch can be encoded
       durably
   - Treat missing carriers as work to be done, not as permission to silently
     drop the branch.
   - Keep the creation bounded to:
     - zeroth-order self carriers
     - first-order edge carriers
     - collaboration/group carriers
   - If the work has turned into deeper schema design rather than bounded
     carrier creation, stop and route that part to the stronger
     world-and-memory owner.
10. Update the current evolver self-world-model artifact.
    - Record what the experience taught the current evolver about itself.
    - Preserve the evidence / inference / open-question split.
    - Add direct references to the causal artifacts from step 1.
11. If the operator self branch is real, then update that self-world-model
    artifact.
    - Record what the experience taught the operator about themselves as
      directly articulated in the discussion.
    - Preserve the evidence / inference / open-question split.
    - Add direct references to the causal artifacts from step 1.
12. For each real `<current evolver> -> <entity>` branch, update that
    world-model artifact.
    - Record what the experience taught the current evolver about that entity.
    - Preserve the evidence / inference / open-question split.
    - Add direct references to the causal artifacts from step 1.
13. For each real `<entity> -> <current evolver>` branch, update that
    world-model artifact.
    - Record what the experience taught that entity about the current evolver
      as directly articulated in the discussion.
    - Preserve the evidence / inference / open-question split.
    - Add direct references to the causal artifacts from step 1.
14. If the collaboration branch is real, then update that collaboration
    artifact.
    - Record what the experience taught about:
      - the collaboration
      - mutual understanding or misunderstanding
      - shared failure modes or convergence improvements
    - Add direct references to the causal artifacts from step 1.
15. If the discussion exposed a stronger non-world-model owner for part of the
    lesson, route that branch explicitly.
    - Record which branch belongs to that stronger owner.
    - Link the stronger owner directly rather than burying the lesson only in
      chat.
16. Run the final knowledge-integration pass over the modified artifact
    cluster.
    - Use:
      `algorithms/conduct-knowledge-orthogonalization-pass.md`
    - Treat the bounded target as:
      - the revised current-evolver self-world-model artifact
      - any created or revised operator self artifact
      - any created or revised edge artifact
      - any created or revised collaboration artifact
      - the source artifacts that caused or revealed the learning experience
    - Add the missing cross-references or explicit non-links needed so the
      revised world-model artifacts point back to the specific artifacts that
      caused the learning experience.
17. Pause before check-in.

    - Do not assume the writeback should be committed immediately.
    - Wait for the operator to tell you to `run the commit algorithm`.
    - If the operator has not said that yet, leave the updated artifacts as
      local working state and report the current stopping point honestly.

18. Record the outcome honestly.

    - State which world-model artifacts were actually updated.
    - State which world-model carriers had to be created in order to encode
      the learning honestly.
    - State which branches were routed to a stronger non-world-model owner.
    - State which conditional branches were not taken because:
      - no clear learning delta for that branch was actually present
      - the work would have required speculative higher-order carrier
        invention
    - State whether the operator has or has not yet told the current evolver
      to `run the commit algorithm`.
    - If the experience revealed a reusable stable policy rather than only a
      one-time world-model delta, consider follow-on promotion into a
      flexiarg, mission update, or another stronger canonical owner.

## Decision Rule
0. Before the rest of the algorithm proceeds, reread the world-model
   neighborhood for:
   - the current evolver, mandatorily
   - any other evolver or operator explicitly involved in the experience
   - this reread is non-exclusive, but the current evolver self-neighborhood
     reread is fail-closed and mandatory
1. Use this algorithm when a concrete learning experience should grow or
   revise the warranted world-model neighborhood through operator discussion.
2. Treat the learning experience as a discussion by default, not as a one-turn
   summary.
3. Do not treat this algorithm as a disagreement-resolution routine.
   - if a disagreement existed, that is not by itself the point of this
     algorithm
   - the current point is to mine the joint experience in discussion and
     update the joint world model
4. Before the discussion phase begins, do not mutate durable artifacts for the
   learning experience.
3. If the lesson expands into a broader run-wide post-mortem or
   novelty-capture lane, route to:
   `algorithms/conduct-post-mortem-learning-capture-discussion.md`
5. Missing world-model carriers are not, by themselves, a reason to skip a
   real branch:
   - if the discussion has made a zeroth-order, first-order, or collaboration
     branch clear enough, create the missing carrier through the strongest
     local world-model owner and continue honestly
6. The current-evolver self-world-model branch is mandatory and fail-closed:
   - the current evolver MUST update its model of itself
   - the self-level delta must be reassessed after each continuing-discussion
     turn
   - if the lesson has been displaced into only boundary, other-entity, or
     collaboration wording, return to discussion rather than claiming
     completion
7. The live discussed other-entity edge branches are also mandatory and
   fail-closed once they become real:
   - each real `<current evolver> -> <entity>` and
     `<entity> -> <current evolver>` branch must be reassessed after each
     continuing-discussion turn
   - do not silently drop those branches only because their carriers were
     missing or because an earlier summary has gone stale
   - create the missing carrier or route the branch to a stronger owner
     explicitly
8. Do not create speculative higher-order carriers unless the discussion has
   actually made that order real and bounded.
9. After the discussion and writeback steps, do not commit unless the operator
   tells you to `run the commit algorithm`.
10. Finish by reusing:
   `algorithms/conduct-knowledge-orthogonalization-pass.md`
   rather than doing ad hoc link sprinkling.

## Outputs
- updated current-evolver self-world-model artifact
  - mandatory; this algorithm does not complete without it
- if the discussion warranted them, then:
  - created or updated operator self-world-model artifact
  - created or updated `<current evolver> -> <entity>` world-model artifacts
  - created or updated `<entity> -> <current evolver>` world-model artifacts
  - created or updated collaboration world-model artifact
- explicit references from the revised world-model artifacts to the source
  artifacts that caused or revealed the learning experience
- explicit created-carrier accounting when a warranted branch needed new
  carrier creation
- explicit stronger-owner routing accounting when part of the lesson belonged
  elsewhere
- explicit skipped-branch accounting when a conditional update branch did not
  apply because no clear learning delta existed
- explicit statement of whether the operator has or has not yet told the
  current evolver to `run the commit algorithm`
- if the discussion pauses before check-in, an honest current stopping note
  rather than fake closure

## Stop Conditions
Stop and return to discussion or stronger owner routing if:

- the current evolver has not reread its own self-world-model neighborhood yet
- another explicitly involved evolver or operator neighborhood has not yet been
  reread
- there is no real learning delta to capture
- the required current-evolver self-level lesson cannot honestly be named
- the self-level lesson has been displaced into only boundary, other-entity,
  or collaboration language and the self-model has not been updated yet
- a real discussed live edge branch has new learning that has not yet been
  accounted for in the current draft or summary
- the work is expanding into a broader run-wide post-mortem and should move to
  that owner
- the world-model revision is turning into a broad re-architecture or
  typed-schema design project rather than a bounded learning-experience
  discussion
- the only remaining candidate branches are speculative higher-order carriers
  that the current discussion has not actually made real
- the operator pauses the discussion and the current shared view is enough for
  this turn
- the operator has not yet told the current evolver to `run the commit
  algorithm`

## References

- `algorithms/conduct-post-mortem-learning-capture-discussion.md`
- `algorithms/conduct-knowledge-orthogonalization-pass.md`
- `ideas/right-now-alice-now-self-world-model.md`
- `ideas/right-now-alice-model-of-rob-world-model.md`
- `ideas/right-now-rob-model-of-alice-world-model.md`
- `ideas/right-now-rob-now-self-world-model.md`
- `ideas/right-now-collaboration-world-model.md`
- predecessor/background carriers:
  - `ideas/right-now-codex-artifact-in-the-loop-world-model.md`
  - `ideas/right-now-codex-now-self-world-model.md`
  - `ideas/right-now-codex-model-of-rob-world-model.md`
  - `ideas/right-now-rob-model-of-codex-world-model.md`
- `holes/missions/M-mfuton-pragmatic-operational-problem-solving-codification.md`
- `holes/missions/M-mfuton-familiar-typed-surface-world-and-memory.md`
- `holes/missions/M-mfuton-familiar-typed-surface-world-and-memory/M-world-model-neighborhood-growth-via-learning-experiences.md`
- `library/foundational/respond-to-questions-with-sources.flexiarg`
- `library/foundational/respect-others-while-collaborating.flexiarg`
- `library/system-architecture/knowledge-factoring-objective.flexiarg`
