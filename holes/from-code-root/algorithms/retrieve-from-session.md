# Algorithm: Retrieve From Session

Date: 2026-03-14
Status: active

## Authority routing

- pragmatic operational codification owner:
  `holes/missions/M-mfuton-pragmatic-operational-problem-solving-codification.md`
- algorithm-creation lane:
  `holes/missions/M-mfuton-pragmatic-operational-problem-solving-codification/M-algorithm-creation-and-flexiarg-factoring.md`
- session-preservation parent:
  `holes/missions/M-eal-language-spec/M-eal-session-stack-runtime-model/M-codex-rollout-preservation-and-compression-aware-shadow-archive.md`
- session/file discovery and binding authority:
  `holes/missions/M-eal-language-spec/M-eal-session-stack-runtime-model/M-codex-rollout-preservation-and-compression-aware-shadow-archive/M-codex-session-authority-and-subscriber-pubsub.md`
- governed command surface:
  `agent_skills/session-tools/README.md`
- governing companion flexiargs:
  - `library/social/session-evidence-over-memory-claims.flexiarg`
  - `library/foundational/do-not-recreate-the-wheel.flexiarg`
  - `library/foundational/respond-to-questions-with-sources.flexiarg`

## Companion Flexiarg Resolution (2026-03-14)

The generic companion-flexiarg scan procedure is owned by:
- `algorithms/create-algorithm.md`
- `algorithms/create-flexiarg-from-recurring-judgment-surface.md`

This section records only the concrete owner map for this algorithm:
- governing companion flexiargs reused/created for this algorithm:
  - `library/social/session-evidence-over-memory-claims.flexiarg`
    - direct owner for the session-recovery judgment surface:
      when memory about prior prompts is uncertain, prefer governed
      session evidence bound to this session by session id
  - `library/foundational/do-not-recreate-the-wheel.flexiarg`
    - reuse-first owner for the decision to use the existing
      `agent_skills/session-tools/` lane instead of inventing a parallel
      recovery path
  - `library/foundational/respond-to-questions-with-sources.flexiarg`
    - direct foundational owner for the question-answering discipline:
      when the operator asks what they previously said, answer from governed
      session sources rather than from conversational memory alone
- inspected but not adopted as direct companion owners:
  - `library/system-architecture/deterministic-evidence-authority.flexiarg`
    - adjacent architecture authority for evidence ordering, but too broad to
      own this recovery-specific decision gate directly
  - `library/social/typed-command-surface-over-eval.flexiarg`
    - adjacent command-surface policy, but this lane is session-recovery and
      does not primarily govern typed-command-vs-eval selection
  - `library/social/futon-self-description-defers-to-installation-authority.flexiarg`
    - useful cross-installation deference discipline, but not the direct owner
      of this session-evidence-over-memory rule
  - `futon3c/library/social/evidence-landscape.flexiarg`
    - upstream-adjacent evidence ontology, but too broad and theoretical to
      serve as the direct governing companion for this operator recovery
      procedure

Application rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of the execution authority surface and should be read/applied, not treated as
  optional background references

## Purpose

Use this when the operator asks to recover prior user prompts from the current
Codex session rather than trusting unstated conversational memory.

This algorithm exists in part for a concrete failure mode: Codex may state what
the operator's last prompt was and be wrong. In that situation, the truthful
next move is to recover the answer from the governed session/rollout artifacts
under `.codex/sessions`, using the existing `agent_skills/session-tools/`
commands, bound to this session's rollout file by session id, not to restate a
remembered guess.

Invocation rule:
- interpret the remainder of the operator prompt after the words
  `retrieve from session` as the retrieval specification

Examples:
- `run the retrieve from session algorithm to recover my last prompt`
- `run the retrieve from session algorithm to recover my last five prompts`

## Inputs

- a retrieval specification in the invoking prompt, such as:
  - `recover my last prompt`
  - `recover my last five prompts`
- optional stronger binding hints already known in the current lane:
  - the current session id
  - explicit `--session` path
  - `session_meta.source`
  - `session_meta.originator`
  - `session_meta.cwd`
- the governed session-tools launchers under:
  `agent_skills/session-tools/`

## Preconditions

1. The question is about what the operator previously said in the Codex
   session, and ordinary local memory is not strong enough.
2. Prefer the governed session-tools launchers over raw ad hoc file scraping.
3. Do not treat "newest file under `~/.codex/sessions`" as strong enough when
   a stronger binding signal is available.
4. Keep the current retrieval request itself out of the returned history.
5. Distinguish:
   - completed paired-turn recovery
   - raw user-message fallback when interruption/crash left the relevant prompt
     unpaired

## Procedure

1. Parse the retrieval specification from the remainder of the invoking prompt.
   - extract the requested window:
     - `my last prompt` -> `1`
     - `my last five prompts` -> `5`
   - if the count is not explicit, default to the smallest honest window that
     answers the ask
2. Read/apply the governing companion flexiargs before executing the rest of
   the procedure.
   - `library/social/session-evidence-over-memory-claims.flexiarg`
     governs evidence-over-memory authority and session-id-first binding
   - `library/foundational/do-not-recreate-the-wheel.flexiarg`
     governs reuse of the existing session-tools lane instead of improvising a
     parallel recovery path
   - `library/foundational/respond-to-questions-with-sources.flexiarg`
     governs rereading the source-bearing session artifacts before answering
     the operator's question from memory
   - treat the `Companion Flexiarg Resolution` section above as the governing
     owner map for this algorithm rather than re-deriving companion owners from
     scratch on every invocation
3. Bind the session/file before reading history.
   - strongest order:
     1. this session's rollout file identified from the current session id
     2. explicit `--session` path if already known
     3. session-meta-aware binding using the governed payload helper:
        `agent_skills/session-tools/session_latest_turn_payload.bat`
        with available selectors such as:
        - `--session-id-filter`
        - `--source-filter`
        - `--originator-filter`
        - `--cwd-filter`
     4. newest-file fallback only if stronger signals are absent
   - normal in-session behavior should resolve this session id to this
     session's rollout file first, then use that exact file for later reads
   - when newest-file fallback is used, say so explicitly in the answer
4. Preview the bound session with the governed summary helper when needed.
   - use:
     `agent_skills/session-tools/session_summary.bat`
   - this is the fast check that the selected session looks like the intended
     one before quoting prompts from it
5. Recover the previous prompts by default from completed paired turns.
   - use:
     `agent_skills/session-tools/session_last_turns.bat`
   - request enough turns to cover the needed window
   - because this command reads completed paired turns, the current in-flight
     retrieval request is normally excluded automatically, so the newest shown
     `USER` block is the prompt before the retrieval request
6. If the relevant prior prompt may be unpaired, fall back to raw user-message
   inspection.
   - use:
     `agent_skills/session-tools/session_tail.bat --type response_item --role user`
   - this fallback is for interruption/crash or other cases where the desired
     prompt exists in the rollout file but does not yet have a final assistant
     answer, so paired-turn tooling would omit it
7. Return the recovered prompts explicitly.
   - quote the recovered user prompt text
   - name the session file used
   - state whether the answer came from:
     - paired-turn recovery, or
     - raw user-message fallback
   - if multiple prompts were requested, label them in a clear bounded order
8. State uncertainty honestly.
   - if session binding was ambiguous, say that
   - if only newest-file fallback was available, say that
   - if the desired prompt cannot be recovered from the current session file,
     stop rather than guessing

## Outputs

- the recovered prior user prompt or prompts
- the session file actually consulted
- the retrieval mode used:
  - paired-turn recovery, or
  - raw-user fallback
- an explicit note about any weaker binding or ambiguity still present

## Stop Conditions

Stop and report the limitation instead of guessing when:
- no session file can be bound honestly
- the bound session is clearly the wrong surface/session
- the prompt count or target is too ambiguous to answer faithfully
- the requested prompt is absent from both:
  - completed paired-turn recovery
  - raw user-message fallback

## Case Anchors

- concrete failure state:
  Codex stated what the operator's last prompt was, and that statement was
  wrong; the recovery surface needed to move from memory claim to governed
  session lookup
- supporting authority lane:
  `holes/missions/M-eal-language-spec/M-eal-session-stack-runtime-model/M-codex-rollout-preservation-and-compression-aware-shadow-archive.md`

## References

- `agent_skills/session-tools/README.md`
- `agent_skills/session-tools/session_summary.bat`
- `agent_skills/session-tools/session_last_turns.bat`
- `agent_skills/session-tools/session_tail.bat`
- `agent_skills/session-tools/session_latest_turn_payload.bat`
- `library/social/session-evidence-over-memory-claims.flexiarg`
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
- `library/foundational/respond-to-questions-with-sources.flexiarg`
- `algorithms/create-algorithm.md`
- `algorithms/merge-artifact.md`
- `holes/missions/M-eal-language-spec/M-eal-session-stack-runtime-model/M-codex-rollout-preservation-and-compression-aware-shadow-archive.md`
- `holes/missions/M-eal-language-spec/M-eal-session-stack-runtime-model/M-codex-rollout-preservation-and-compression-aware-shadow-archive/M-codex-session-authority-and-subscriber-pubsub.md`
