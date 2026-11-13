# Algorithm: Conduct Brainstorming Discussion on Idea or Artifact

Date: 2026-03-11
Status: active

## Authority routing

- pragmatic operational codification owner:
  `holes/missions/M-mfuton-pragmatic-operational-problem-solving-codification.md`
- algorithm-creation lane:
  `holes/missions/M-mfuton-pragmatic-operational-problem-solving-codification/M-algorithm-creation-and-flexiarg-factoring.md`
- broader idea-discussion owner:
  `algorithms/conduct-discussion-on-idea-before-writeup-or-promotion.md`
- adjacent promotion / factoring gate:
  `algorithms/create-algorithm.md`
- adjacent flexiarg-creation gate:
  `algorithms/create-flexiarg-from-recurring-judgment-surface.md`
- adjacent orthogonalization owner:
  `algorithms/conduct-knowledge-orthogonalization-pass.md`
- adjacent post-mortem discussion owner:
  `algorithms/conduct-post-mortem-learning-capture-discussion.md`

## Purpose

Use this when the operator wants an explicit exploratory discussion mode for an
new idea, artifact, naming choice, boundary question, or partial design, and
the right next move is collaborative sharpening rather than immediate
implementation, post-mortem, or formal promotion.

This is the default mode for the broader idea-discussion algorithm unless a
stronger alternative discussion form is chosen.

This algorithm owns the bounded discussion mode where:

1. the operator presents a candidate idea or artifact read
2. the agent reflects what makes sense in it
3. the agent surfaces tensions, mismatches, or pressure points without
   treating disagreement as the default frame
4. the discussion is collaborative and generative, not adversarial and not a
   race to the first plausible solution
5. the discussion will most likely generate more than one refined
   distinction, idea, decomposition, candidate owner, or artifact direction
   before closure is honest
6. the result may remain exploratory rather than forcing promotion, redesign,
   or premature closure
7. the opening response is proportional to the amount of context actually
   present rather than pretending an early large synthesis is already useful

Boundary of ownership:
- this algorithm owns exploratory sharpening
- the broader idea-discussion algorithm owns the final decision about whether
  to leave the idea live-only, write it up, or promote it

## Companion Flexiarg Resolution (2026-03-14)

Governing companion flexiargs for this algorithm:
- `library/social/algorithms-own-procedural-spines-flexiargs-own-judgment-surfaces.flexiarg`
  - inherited from the broader idea-discussion family owner:
    `algorithms/conduct-discussion-on-idea-before-writeup-or-promotion.md`
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
  - inherited from the broader idea-discussion family owner:
    `algorithms/conduct-discussion-on-idea-before-writeup-or-promotion.md`

Inspected but non-owning candidates:
- `library/social/post-mortem-for-novel-learning-capture.flexiarg`
  - same non-owner read as the broader idea-discussion family owner

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

## Inputs

- the current idea, artifact, or boundary question
- any nearby artifacts already in play
- the operator's current read of what seems promising
- any specific naming, scope, or implementation concern already noticed

## Preconditions

1. The work is exploratory or partially formed enough that a rigid procedure or
   direct implementation step would be premature.
2. Brainstorming mode is not a substitute for ordinary implementation when the
   correct work is already clear.
3. Brainstorming mode is not a substitute for post-mortem when the real task is
   retrospective learning capture from an already-completed wave.
4. Brainstorming mode may end with:
   - clarified idea only
   - a decision not to promote
   - a decision to create or refine a mission, algorithm, flexiarg, runbook,
     idea note, or test surface
5. A small initial prompt does not by itself obligate a large initial reply.
   The first turn may honestly remain a short reaction, restatement, or one
   lightweight distinction until there is enough substance for more.

## Procedure

1. Name the topical neighborhood explicitly.
   - Say what is being brainstormed:
     - Examples
      - idea
      - artifact
      - naming question
      - boundary question
      - partially formed plan     
   - Treat `subject` here as the current topical neighborhood, not as a claim
     that the discussion already has:
     - one artifact
     - one owner
     - one mission
     - or one eventual durable output
2. Classify the input shape.
   - Distinguish:
     - topical neighborhood not yet decomposed
     - single-subject brainstorm
     - multi-point bundle brainstorm
   - Do not force `single-subject` just because the operator named one broad
     topic.
    - When artifact count, owner shape, or decomposition is still unclear, stay
      in:
      - topical neighborhood not yet decomposed
    - Do not compress a multi-point bundle into one main idea too early.
3. Size the opening response to the available context.
   - The first reply should be proportional to:
     - what the operator actually said in the current prompt
     - any already-live nearby context clearly referenced by that prompt
   - If the operator only gave a tiny description, do not force:
     - a long synthesis
     - candidate owners
     - candidate artifacts
     - candidate solutions
     - or multiple suggested mission shapes
   - In that thin-context case, an honest first reply may be only:
     - a brief reaction
     - a brief restatement
     - one small boundary distinction
     - or one small framing move for the next exchange
   - A larger opening reply is appropriate only when the operator has already
     provided enough detail, or has clearly invoked previous session concepts
     that give the discussion a stronger live footing.
4. State the discussion stance explicitly.
   - Treat the operator and agent as collaborators generating understanding
     together.
   - Reflect what already makes sense before pushing on weaknesses.
   - Surface tensions, mismatches, or pressure points honestly when needed,
     but do not frame the default mode as disagreement management.
   - If a weakness is found, try to improve the idea rather than only reject
     it.
   - Treat brainstorming as a generative mode:
     it will most likely produce new distinctions, decompositions, candidate
     owners, candidate directions, and sometimes candidate solutions.
   - Do not treat the first plausible redesign or solution sketch as the
     default stopping point.
   - Identifying a problem, inversion, or pressure point does not by itself
     license proposing a replacement before the operator has responded or
     explicitly invited convergence.
   - Reuse the broader algorithm-creation norm that live use may refine an
     algorithm's procedural usability, with the operator mainly confirming,
     correcting, or adding a few high-value distinctions when that is the
     honest collaborative shape.
5. Reuse nearby authorities before inventing a duplicate concept.
   - Inspect adjacent missions, algorithms, flexiargs, glossary notes, idea
     notes, or upstream futon artifacts.
   - Distinguish:
     - stronger existing owner
     - adjacent but non-owning neighbor
     - genuinely new local concept
6. If the input is a multi-point bundle, normalize it before synthesis.
   - Enumerate the distinct points explicitly.
   - Cluster nearby points into themes when that helps.
   - Mark which points appear to be:
     - already integrated
     - not yet integrated
     - tension, mismatch, or unresolved pressure point
     - proposed improvement or reframing
   - Respond pointwise before attempting a global synthesis.
7. Iterate through the core brainstorming loop.
   - restate the current idea in the strongest charitable form
   - say what seems right or high-signal about it
   - identify the nearest stronger formal neighbor
   - identify the nearest weaker, over-broad, or semantically occupied
     neighbor
   - identify the causal boundary that makes this a real concept rather than
     mere topic drift
   - identify the main boundary pressure or confusion
   - propose one or more improved readings, names, or decompositions
   - before speaking as if there is already one owner or one artifact, check
     whether the discussion is still at the neighborhood-discovery stage
   - if yes, speak in terms of:
     - candidate owners
     - candidate artifacts
     - possible decompositions
     - still-open topology
   - allow the discussion to yield:
     - one refined idea
     - an idea neighborhood
     - a candidate artifact neighborhood
   - treat generation as the expected mode:
     - new distinctions
     - candidate owners
     - candidate decompositions
     - candidate next moves
   - keep the unresolved choice visible if there is not yet one best answer
    - do not jump from diagnosis to redesign merely because a pressure point is
      now visible
    - if a candidate solution is spoken before the operator responds, keep it
      visibly provisional rather than treating it as the new default track
    - do not act as if the opening turn already owes the operator a complete
      neighborhood map when the current prompt is still thin
8. Prefer constructive refinement over binary verdicts.
   - Good outputs include:
     - tighter naming
     - sharper scope boundary
     - stronger distinction from nearby concepts
     - a better decomposition
     - a clearer promotion trigger
   - A valid result is also:
     - `interesting, but not yet worth durable promotion`
9. Distinguish idea work from promotion work.
   - If the discussion yields only a promising concept, keep it in ideas or
     ordinary chat-level discussion.
   - Do not collapse an idea neighborhood into a fake single-output summary
     just to make the result look tidier than it really is.
   - If the idea becomes stable enough to deserve a durable owner, route to the
     broader idea-discussion owner for the final writeup/promotion
     classification rather than pretending brainstorming itself finished the
     whole job.
10. Record the current best read.
   - Summarize:
     - what the idea is
     - what makes it useful
     - what remains unresolved
     - what causal structure or trigger makes the concept real
     - what stronger or weaker neighboring concepts it should not be confused
       with
11. When using this algorithm itself teaches a better brainstorming pattern,
   feed that back into the algorithm before ending the wave.
   - Brainstorming about brainstorming is allowed here.
   - If the improvement is stable, update this algorithm or factor a companion
     flexiarg.

## Outputs

- a sharpened idea or artifact read
- generated distinctions, decompositions, candidate owners, or candidate
  directions
- possibly only a small opening alignment turn when the initial prompt is
  still thin
- possibly:
  - an idea neighborhood
  - a candidate artifact neighborhood
- explicit unresolved questions, tensions, or still-waiting-on-operator
  response points
- a current best recommendation or candidate next move, if the discussion has
  honestly reached that point
- a recommendation back to the broader idea-discussion owner about whether the
  idea should remain exploratory, be captured in `ideas/`, or be routed toward
  durable promotion
- possible improvement notes for this brainstorming algorithm itself

## Notes

- This is intentionally lighter than a post-mortem and lighter than a
  mission-close or publication algorithm.
- It is a discussion-mode algorithm, not a claim that the idea is already
  mature.
- It also does not own the final writeup/promotion boundary; it is the default
  exploratory submode inside the broader idea-discussion owner.
- This is the alignment-plus-generation mode inside idea discussion.
- Its normal outcome is generated structure:
  distinctions, decompositions, candidate owners, candidate next moves, and
  sometimes candidate solutions.
- The main local failure mode is premature convergence:
  - diagnosis being mistaken for redesign
  - tension being mistaken for settled disagreement
  - the first plausible answer being treated as the answer before operator
    response
- Another local failure mode is premature bulk synthesis:
  - a thin prompt being answered as if it already justified a large structured
    recommendation
- Good brainstorming preserves gradient:
  - promising
  - partial
  - wrong but improvable
  - not worth promotion
- Early discussion-shape classification must not harden into a premature
  artifact-count or owner-count claim.
- A good brainstorming result often identifies the right neighboring concept
  just as much as it identifies the new concept itself.
- `Neighborhood` is intentional local language here:
  it names a local family of adjacent ideas or artifact candidates rather than
  a forced single output, and it fits the installation's broader topological
  and locality-oriented vocabulary.
- Reuse the broader algorithm-creation norm for how live use improves the
  procedure itself; this algorithm is only one local instance of that pattern.
