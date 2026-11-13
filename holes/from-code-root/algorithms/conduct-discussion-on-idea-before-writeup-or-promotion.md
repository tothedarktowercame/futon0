# Algorithm: Conduct Discussion on Idea Before Writeup or Promotion

Date: 2026-03-12
Status: active

## Authority routing

- pragmatic operational codification owner:
  `holes/missions/M-mfuton-pragmatic-operational-problem-solving-codification.md`
- algorithm-creation lane:
  `holes/missions/M-mfuton-pragmatic-operational-problem-solving-codification/M-algorithm-creation-and-flexiarg-factoring.md`
- default exploratory mode:
  `algorithms/conduct-brainstorming-discussion-on-idea-or-artifact.md`
- adjacent promotion / factoring gate:
  `algorithms/create-algorithm.md`
- adjacent flexiarg-creation gate:
  `algorithms/create-flexiarg-from-recurring-judgment-surface.md`
- adjacent installation-merge owner:
  `algorithms/merge-artifact.md`
- adjacent orthogonalization owner:
  `algorithms/conduct-knowledge-orthogonalization-pass.md`

## Purpose

Use this when the operator says they have an idea, term, concept, artifact
read, or partial design and the truthful next move is to discuss it with the
agent before jumping straight to writeup, codification, or promotion.

This algorithm owns the broader discussion boundary where:

1. the idea is first made explicit in session text
2. the operator and evolver work toward being on the same page about it
3. the discussion itself is treated as part of the work, not as disposable
   prelude
4. the discussion may yield not only one refined idea but a local idea or
   artifact neighborhood
5. only after that discussion do we decide whether to leave the resulting read
   exploratory, write it up, or promote it

Default local mode:
- use brainstorming as the normal discussion mode unless there is a stronger
  reason to use a different discussion form

## Companion Flexiarg Resolution (2026-03-14)

Governing companion flexiargs for this algorithm:
- `library/social/algorithms-own-procedural-spines-flexiargs-own-judgment-surfaces.flexiarg`
  - governs the boundary between this discussion procedure and any later
    flexiarg/algorithm/doc promotion choice
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
  - governs reuse-first promotion into existing idea, mission, docs, or
    flexiarg owners after the discussion converges

Inspected but non-owning candidates:
- `library/social/post-mortem-for-novel-learning-capture.flexiarg`
  - governs retrospective learning capture after realized work, not ordinary
    idea discussion before promotion

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

Discussion-family note:
- this broader discussion owner carries the shared companion posture for the
  ordinary idea-discussion family
- the default brainstorming submode should inherit this same governing pair
  unless it later acquires a real narrower delta

## Inputs

- the operator's current idea or partial idea
- any nearby artifacts or naming collisions already known
- the operator's read of what feels promising, wrong, or unfinished

## Preconditions

1. The operator is asking to work an idea collaboratively, not merely to have
   it transcribed.
2. Do not skip directly from `I have an idea` to durable writeup unless the
   operator explicitly asks for that and is comfortable bypassing discussion.
3. The discussion is valuable for two reasons:
   - it helps improve or sharpen the idea
   - it helps ensure the operator and agent actually share the same read
4. The relevant words should appear in the session text rather than being
   silently replaced by an agent-authored durable artifact.
5. The opening discussion turn should be proportional to the amount of context
   already present. A short initial prompt does not by itself require a long
   interpretive or suggestive response.

## Procedure

1. Name the idea and the current ask explicitly.
   - Restate what is being discussed.
   - Say that the next move is discussion first, not immediate writeup.
   - Keep that opening response proportional to the prompt and any clearly
     referenced live context; do not force a large synthesis before enough
     shared substance exists.
2. Choose the discussion mode.
   - Default:
     - `algorithms/conduct-brainstorming-discussion-on-idea-or-artifact.md`
   - Only choose a different mode if a stronger owner clearly fits better.
3. Treat mutual understanding as an explicit goal.
   - The discussion should not only test whether the idea is good.
   - It should also test whether the operator and agent mean the same thing by
     the current words.
   - The honest result may be:
     - one clarified idea
     - an idea neighborhood
     - a candidate artifact neighborhood
4. Keep the important words in the live session.
   - Do not replace the discussion with a silent summary or immediate durable
     writeup.
   - If terminology, distinctions, or caveats matter, they should be spoken in
     the session text.
   - But do not over-speak early merely to prove that discussion is happening;
     an honest small opening turn is often the right move when the operator has
     only sketched the topic.
5. After the discussion loop, classify the result honestly.
   - possibilities include:
     - still exploratory
     - clearer but not yet ready for durable capture
     - clearer as an idea neighborhood rather than a single idea
     - ready for an idea note
     - ready for a bounded docs note, runbook, routing note, or mission-local
       docs neighborhood
     - ready for mission / algorithm / flexiarg / runbook / glossary
       promotion
     - ready for direct implementation
6. Only then write or promote.
   - If durable capture is chosen, route through:
     `algorithms/merge-artifact.md`
     when owner selection, discoverability wiring, or docs-first capture are
     part of the honest result.
   - If the idea should remain live-only for now, say so explicitly.

## Outputs

- an explicit idea discussion in session text
- improved mutual understanding between operator and agent
- either:
  - one clarified idea, or
  - an aligned idea neighborhood, or
  - an aligned candidate artifact neighborhood
- a decision about whether that result remains exploratory or should be
  captured durably
- when appropriate, routing into brainstorming or into a later promotion /
  installation-merge step

## Notes

- This algorithm is broader than brainstorming.
- Brainstorming is the usual mode inside it, but not the only possible one.
- Discussion here is alignment-first; brainstorming is the usual
  alignment-plus-generation submode.
- The key boundary is:
  - discussion first for shared understanding
  - then writeup or promotion if justified
- Proportionality matters at the opening gate:
  - the first reply should match the amount of context actually available
  - larger synthesis belongs later, once the discussion has earned it
