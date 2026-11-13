# Algorithm: Research Issue Online

Date: 2026-03-25
Status: active

## Purpose

Use this when a live technical issue may already have been diagnosed or solved
somewhere in the public world and the next efficient move is to learn from that
existing evidence before continuing local trial-and-error.

The purpose of this algorithm is to learn a solution that someone has already
found. The point is not to recreate the wheel. In this lane, "online research"
includes:
- web search
- directly reading GitHub issues and discussions
- directly reading Stack Exchange / Stack Overflow posts
- directly reading official docs or tracker threads when they are part of the
  same failure family

This algorithm does not stop at search-result titles. It owns the follow-through
step:
- keep reading the promising hits, even when they are only approximately
  related
- then determine from the body of the issue/post whether the hit is actually
  associated with the evidence seen so far

## Companion Flexiarg Resolution (2026-03-25)

Governing companion flexiargs for this algorithm:
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
  - governs reuse-first learning from already-existing solutions rather than
    blindly re-deriving the same debugging work locally
- `library/foundational/respond-to-questions-with-sources.flexiarg`
  - governs reading the governing source materials before claiming that a hit
    is relevant or resolved
- `library/social/time-anchored-debugging-hypothesis-priors.flexiarg`
  - governs comparing the online hit to the actual observed failure context
    rather than treating every superficially similar report as the same bug

Inspected but non-owning candidates:
- `library/social/session-evidence-over-memory-claims.flexiarg`
  - governs recovery of local session history, not the broader online issue
    research procedure itself

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be applied rather than re-derived from
  scratch

## Inputs

- the current bounded issue or question
- the evidence seen so far:
  - exact error text when available
  - tool/product/library names
  - operating system / runtime mode / transport
  - current observed symptoms
  - versions, dates, or recency clues when available
- any current local hypothesis
- any preferred source class:
  - GitHub
  - Stack Exchange
  - official docs
  - broader web

## Preconditions

1. There is a real bounded issue/question, not just generic curiosity.
2. Enough local evidence exists to form a search target.
3. Online research is allowed and appropriate for the lane.
4. The agent is prepared to read hits directly instead of deciding from titles
   or snippets alone.

## Procedure

1. Bind the current evidence first.
   - Write down the actual observed issue surface before searching:
     - exact failure text when present
     - component/product names
     - transport/runtime/OS details
     - what is configured versus what is actually exposed
   - Separate:
     - observed evidence
     - current guesses
2. Start with targeted searches.
   - Prefer search terms built from the actual evidence:
     - exact error string
     - product/tool plus symptom
     - product/tool plus transport/runtime/OS
     - product/tool plus the missing capability or visibility symptom
   - Search broadly enough to include:
     - GitHub issues/discussions
     - Stack Exchange / Stack Overflow
     - official docs
     - other trackers or forum posts when they appear in the same failure
       family
3. Prefer hits that are likely to contain real diagnosis.
   - For product/runtime/tooling failures, GitHub issues and official trackers
     are usually the first strong class.
   - Stack Exchange can be very helpful, especially for configuration and
     failure-pattern translation, but treat accepted folklore as evidence to
     audit rather than automatic truth.
4. Do not stop at the search results page.
   - Open the promising hits directly.
   - Read the issue/post body itself.
   - If needed, read enough comments or maintainer replies to determine:
     - what the real failure was
     - whether a fix/workaround was confirmed
     - whether the report was closed as duplicate, fixed, unsupported, or
       user-error
5. Keep reading related hits even when they are not exact matches.
   - Similar hits can still be useful when they share:
     - the same product
     - the same transport/runtime layer
     - the same capability-not-exposed symptom
     - the same OS or environment class
   - Use these as hypothesis-shaping evidence, not as proof by title
     similarity.
6. For every promising hit, determine whether it is actually associated with
   the current evidence.
   - Compare the hit against the local evidence already bound in step 1.
   - Classify each hit as one of:
     - direct match
     - partial match
     - useful adjacent case
     - not associated
   - This classification must come from reading the hit, not merely from the
     search snippet.
7. Extract the learned solution or failure model.
   - Record, in compact form:
     - what the hit says the problem actually was
     - where the ownership appears to sit
     - any workaround/fix/config change already discovered
     - whether the remedy was confirmed or still speculative
8. Convert the research into the next local move.
   - Choose the smallest discriminating local check that tests the strongest
     learned hypothesis.
   - Prefer a check that can separate:
     - Codex-side exposure bug
     - MCP server/tool-advertisement problem
     - config/reload/session-mount issue
     - version/platform limitation
9. Report the research honestly.
   - Name which hits were actually read.
   - State which ones were truly associated with the evidence and which were
     only adjacent.
   - Say plainly when the online research is suggestive rather than conclusive.
10. If the research yields durable installation knowledge, write it back.
   - Update the owning mission/docs/algorithm neighborhood as appropriate.
   - Preserve why a source was accepted or rejected when that reasoning is
     likely to matter again.

## Decision Rule

Default rule:
1. use online issue research when the problem plausibly already exists in the
   public world
2. treat the goal as learning an already-found solution or failure model,
   rather than recreating the wheel locally
3. do not decide from titles/snippets alone
4. continue by reading promising hits, including related-but-not-exact ones
5. determine relevance only after comparing the read hit against the evidence
   seen so far
6. turn the research into one discriminating next local check instead of
   indefinite browsing

## Outputs

- a bounded set of actually-read online hits
- a classification for each hit:
  - direct match
  - partial match
  - useful adjacent case
  - not associated
- a compact statement of the learned likely cause, workaround, or fix
- one recommended next local check derived from the research
- durable writeback when the result becomes installation knowledge rather than
  one-off chat residue

## Stop Conditions

Stop when:
- a sufficiently strong direct/partial match yields a clear next local check
- the remaining hits are drifting away from the evidence
- the search is repeating the same failure pattern without new learning
- the issue is not actually online-research-shaped and should return to local
  source reading or runtime inspection instead

## Case Anchor

- current Codex/MCP investigation lane:
  - installed MCP server present in host config
  - server-status visibility present
  - but session-callable tool exposure absent or partial
  - online issue reading is the efficient next move because similar failures
    may already have been diagnosed in Codex issue trackers

## References

- `holes/missions/M-mfuton-pragmatic-operational-problem-solving-codification.md`
- `algorithms/create-algorithm.md`
- `algorithms/author-algorithm-core-and-map-companion-flexiargs.md`
- `algorithms/conduct-knowledge-orthogonalization-pass.md`
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
- `library/foundational/respond-to-questions-with-sources.flexiarg`
- `library/social/time-anchored-debugging-hypothesis-priors.flexiarg`
- `library/social/session-evidence-over-memory-claims.flexiarg`
