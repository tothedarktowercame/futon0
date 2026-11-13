# Algorithm: Render Operator Algorithm In Chat

Date: 2026-03-24
Status: active

## Purpose

Use this when the operator wants an existing operator algorithm rendered back
into chat in a readable step-by-step form, with the written procedure preserved
faithfully enough that the operator can follow the chat rendering without
having to reconstruct the source markdown mentally.

This algorithm owns the rendering move where:

- the source operator algorithm is reread directly
- the numbered procedure steps are reproduced in chat one step at a time
- explanatory prose remains prose
- concrete commands remain fenced code blocks
- continue/stop conditions remain explicit after the relevant step instead of
  being flattened into a summary

It does not own:
- editing or correcting the underlying operator algorithm
- inventing missing packet values or omitted steps
- rendering arbitrary non-operator artifacts as though they were operator
  algorithms
- recovering an unknown source artifact from session history or rollout files
- broader discussion about whether the operator wants summary, promotion, or a
  different artifact form instead of a literal in-chat rendering

## Companion Flexiarg Resolution (2026-03-24)

Governing companion flexiargs for this algorithm:
- `library/foundational/respond-to-questions-with-sources.flexiarg`
  - governs rereading the source operator algorithm before rendering it in
    chat rather than relying on remembered paraphrase
- `library/foundational/respect-others-while-collaborating.flexiarg`
  - governs answering the operator's request directly and in the requested
    readable form rather than redirecting them back to the file unnecessarily
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
  - governs reusing the source algorithm's real step structure and wording
    where faithful reuse is stronger than ad hoc rephrasing

Inspected but non-owning candidates:
- `library/social/algorithms-own-procedural-spines-flexiargs-own-judgment-surfaces.flexiarg`
  - relevant to why the source artifact is an algorithm at all, but not the
    direct owner of this chat-rendering procedure
- `algorithms/retrieve-from-session.md`
  - adjacent when the source algorithm must first be recovered from session
    evidence, but not the owner once the source artifact is already bound
- `algorithms/conduct-discussion-on-idea-before-writeup-or-promotion.md`
  - broader owner for discussion about what artifact form is wanted, but not
    the owner of the literal step-by-step rendering move once that ask is
    already clear

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

## Inputs

- one existing operator algorithm artifact
- the operator's current rendering preference, if they have made it explicit
- any immediate local constraints on formatting, such as:
  - literal step order
  - command blocks preserved as code
  - explanation text left as prose

## Preconditions

1. A concrete operator algorithm source artifact is already known.
2. The operator wants the algorithm rendered in chat rather than merely linked.
3. The source artifact is stable enough that rendering it is more truthful than
   re-deriving the procedure from memory.

## Procedure

1. Bind the source operator algorithm artifact.
   - Name the exact operator algorithm being rendered.
   - If the source artifact is still ambiguous, stop and resolve that first.
2. Reread the source artifact directly.
   - Do not render from memory.
   - Reuse the actual written step order and command bodies from the source.
3. Bind the rendering fidelity from the operator's ask.
   - If the operator asked for the algorithm "literally" or "the md file" in
     chat form, preserve the written procedure rather than summarizing it.
   - If the operator asked for a different rendering style, follow that style
     honestly.
4. Render the global failure rule first when the source algorithm has one.
   - Keep it as prose before the numbered steps.
   - Do not hide it inside a later summary.
5. Render each numbered procedure step in order.
   - For each step:
     - render the step heading as chat prose
     - render explanatory text as prose
     - render actual commands in fenced code blocks
     - render continue/stop conditions after that step's prose/commands
   - Do not merge multiple steps into one chat bullet unless the source
     artifact itself does so.
6. Preserve bounded branches honestly.
   - If the source step says:
     - continue only if X
     - stop if Y
     - report Z back to the agent
     keep those branch rules explicit in the chat rendering.
   - Do not smooth them into generic advice.
7. End with the source handoff/stop boundary.
   - If the source algorithm says to stop and hand control back to the agent,
     preserve that boundary explicitly.
   - Do not append extra follow-on steps unless the operator asked for them.

## Outputs

- a chat rendering of the source operator algorithm that preserves:
  - the source step order
  - explanatory prose
  - command blocks
  - continue/stop conditions
  - the final handoff boundary

## Stop Conditions

Stop and ask for clarification or stronger source binding if:
- the source operator algorithm is not actually known
- the source artifact is still under active rewrite and the rendering would be
  misleading
- the operator's requested rendering style is still under discussion

## References

- `algorithms/create-algorithm.md`
- `algorithms/author-algorithm-core-and-map-companion-flexiargs.md`
- `algorithms/retrieve-from-session.md`
- `algorithms/conduct-discussion-on-idea-before-writeup-or-promotion.md`
- `algorithms/operator/README.md`
- `holes/missions/M-mfuton-pragmatic-operational-problem-solving-codification/M-algorithm-creation-and-flexiarg-factoring.md`
- `holes/missions/M-mfuton-pragmatic-operational-problem-solving-codification/M-algorithm-creation-and-flexiarg-factoring/M-render-operator-algorithm-in-chat.md`
- `library/foundational/respond-to-questions-with-sources.flexiarg`
- `library/foundational/respect-others-while-collaborating.flexiarg`
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
