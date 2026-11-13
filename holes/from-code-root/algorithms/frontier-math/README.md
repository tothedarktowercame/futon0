# Frontier Math Human-Supervision Algorithms

Date: 2026-03-26
Status: active

## Purpose

This family owns the repeatable human prompt choreography that currently sits
on top of the local FrontierMath IRC room loop.

These algorithms are not about debugging the room implementation itself. They
are about what the human supervisor actually says and checks while:

- establishing current room state
- seeding or redirecting work
- handling long-running room tasks without panicking early
- advancing the loop with Bell
- closing a bounded cycle with summary and handoff

Current local landing points that this family must respect:
- direct `@codex ...` / `@math-curator ...` / `@math-mentor ...` mentions are materially stronger than
  advisory relay text when the human actually wants the room worker to act
- local artifact references should be rooted through `mfuton/...` when the
  intended files live in the local `mfuton` mirror or artifact lanes
- Bell may legitimately end in an idle-state acknowledgement:
  - `No new assignable obligations right now.`
  rather than always reposting the next bounded task

These procedures exist because the current local FrontierMath lane is not just
"press start and walk away." The room loop is partly automatic and partly
human-supervised.

## Authority Routing

- owning FrontierMath runtime mission:
  `holes/missions/M-futon-full-system-operation-and-onboarding/M-futon6-onboarding-and-familiarization/M-frontiermath-proof-stack-and-classic-code-familiarization/M-solo-frontiermath-forum-codex-compatible-enactment.md`
- adjacent reusable room-command surface:
  `agent_skills/futon/irc-api/futon-irc-command.bat`
- adjacent reusable room-visibility surface:
  `agent_skills/irc-lane/irc-lane.bat`
- adjacent transport-follow surface:
  `src/mfuton/futon/json_over_http_api/invoke_client.py`

Conceptual precedent inspected in futon repos:
- `futon6/holes/missions/M-distributed-frontiermath.md`
  - Bell phrase, role framing, mentor/prover/tickle split
- `futon3/fulab/meetings/2026-01-27-irc-coordination-review.md`
  - briefing-after-restart, explicit handoffs, short human decision prompts

## Companion Flexiarg Resolution (2026-03-26)

Governing companion flexiargs for this family:
- `library/social/typed-command-surface-over-eval.flexiarg`
  - governs using the existing typed IRC and futon room-command surfaces
    rather than freehand shell or remembered manual room traffic
- `library/system-architecture/typed-command-control-plane-authority.flexiarg`
  - governs treating those typed room-command surfaces as the real callable
    authority for human-supervised room work
- `library/social/extract-classical-automation-from-novel-evolver-work.flexiarg`
  - governs extracting this repeatable supervisory choreography into explicit
    algorithms instead of rediscovering it ad hoc in chat

Inspected but non-owning candidates:
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
  - relevant to reuse posture generally, but the stronger local family owner
    here is the typed-command and extraction posture above

Execution-time family rule:
- these algorithms should prefer:
  - `agent_skills/irc-lane/irc-lane.bat`
  - `agent_skills/futon/irc-api/futon-irc-command.bat`
  rather than ad hoc IRC probing or one-off room scripts
- when the room should actually perform work, reseed the task as a direct
  mention rather than assuming a quoted relay line already executed it
- when naming local FrontierMath files, prefer `mfuton/...` rooted paths over
  ambiguous bare `data/...` references

## Current Algorithms

- `ask-frontiermath-room-for-briefing-and-current-state.md`
  - establish the room state before taking supervisory action
- `seed-or-redirect-frontiermath-room-work.md`
  - start or redirect one bounded work obligation in `#math`
- `supervise-long-running-frontiermath-room-task.md`
  - decide whether a long-running room task is still healthy, needs narrowing,
    or needs intervention
- `run-frontiermath-bell-and-next-work-advance.md`
  - drive and verify the Bell → repost → next-work transition
- `close-frontiermath-room-cycle-with-summary-and-handoff.md`
  - close one bounded room cycle into a summary, git-facing outcome, and next
    handoff
