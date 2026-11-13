# Algorithm: Query IRC

Date: 2026-03-17
Status: active

## Purpose

Use this when the operator or an agent wants a bounded IRC query answer from
the current `mfuton` IRC typed surface without expanding into full IRC
listening, chat-history retrieval, or persistent session-handle work.

The current most common phrasing is:
- `run the query-irc algorithm to see if joe is online`

But the more general primitive is:
- take one stateless IRC room snapshot
- then optionally derive a presence answer for one target nick

This algorithm is the operator-facing procedural owner for that query slice.
It does not own the broader IRC typed-surface mission or the underlying skill
implementation.

## Companion Flexiarg Resolution (2026-03-17)

Governing companion flexiargs for this algorithm:
- `library/social/typed-command-surface-over-eval.flexiarg`
  - governs using the existing typed IRC command surface rather than ad hoc
    shell reconstruction or remembered manual IRC steps
- `library/system-architecture/typed-command-control-plane-authority.flexiarg`
  - governs treating the typed IRC commands as the authoritative callable
    surface for this bounded query lane
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
  - governs reuse of the existing `irc-lane` / typed-command surface instead
    of inventing a parallel one-off query path

Inspected but non-owning candidates:
- `library/social/extract-classical-automation-from-novel-evolver-work.flexiarg`
  - governs the broader promotion posture for the skill/typed surface, not the
    narrower operator-facing query procedure itself

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be applied rather than re-derived from
  scratch

## Inputs

- one IRC scope:
  - `lane`, or explicit `server` + `port`
  - one `room`
- one local query `nick`
- optional `target_nick`
- one timeout budget for the snapshot attempt
- optional server-auth boundary input:
  - prefer `IRC_PASSWORD` from operator secret env
  - later may allow `IRC_PASSWORD_FILE`
  - do not treat the password as normal command text or durable artifact data

Default current derived use:
- `target_nick = joe`

Current live command/skill surfaces:
- typed commands:
  - `m.irc.room.snapshot`
  - `m.irc.presence.query`
- wrapper/skill:
  - `agent_skills/irc-lane/irc-lane.bat`
  - `agent_skills/irc-lane/irc_lane.py`

## Current Auth Boundary (2026-03-17)

The first foreign-server `query-irc` investigation exposed a truthful next
necessity:

- the remote Joe/Linode IRC socket is reachable
- unauthenticated IRC handshake returns `ERROR :Access denied: Bad password?`
- therefore the current query slice needs a bounded auth extension before it
  can answer remote presence questions truthfully

Current auth-handling rule:

- reuse the existing `futon3c` secret convention `IRC_PASSWORD`
- for the current Windows-hosted governed wrapper path, the first
  installation-local source is a private copy of
  [secrets-config.json](/i:/gh/mfuton/config/secrets-config.json)
  placed at `%USERPROFILE%/.mfuton/private/secrets-config.json`
- the governed copy helper for that bootstrap is
  [copy-secrets-config.bat](/i:/gh/mfuton/config/copy-secrets-config.bat)
- keep secret loading at the boundary only
- keep the interior typed rather than passing raw password text around as
  normal command data
- do not write the secret into committed artifacts, receipts, or durable
  mission/algorithm notes
- if receipts mention auth, record only that auth was used, never the secret
  value itself
- if a selected world IRC profile requires `IRC_PASSWORD` and it is still
  missing at launch time, stop immediately with a clear missing-secret result
  instead of attempting a degraded unauthenticated query

## World-Backed IRC Profile Fallback (2026-03-17)

When explicit query data is incomplete, `query-irc` should now prefer
world-backed IRC profile knowledge under `world/irc/`.

Current intended first profile set:

- `world/irc/mfuton-local.json`
- `world/irc/mfuton-remote-on-joe.json`
- `world/irc/joe-irc.json`

Resolution rule for this bounded query algorithm:

- explicit command data still wins
- if the query scope is incomplete, prefer a matching `world/irc` profile
- only then fall back to generic env/hardcoded defaults
- password indirection belongs in the profile boundary, not as durable secret
  text in the profile body
- room expectations in the profile may resolve the first truthful room for a
  nick query such as `joe`

## Room Scope Clarification (2026-03-17)

Do not collapse these two room meanings:

- `#futon`
  - Joe's primary shared futon room
  - the default room to prefer when the operator asks whether Joe is online
- `#zabuton`
  - Rob's remote hosted agent room on Joe's IRC server
  - not the default room for "is Joe online?" queries unless the operator is
    explicitly asking about Rob's hosted room

So for foreign-server `query-irc` use:

- default Joe-presence scope: `#futon`
- explicit Rob hosted-room scope: `#zabuton`

## Preconditions

1. The query being asked is bounded to current IRC room visibility, not human
   availability.
2. The caller accepts that the answer is intentionally stateless and may be
   slightly stale by the time it is read.
3. The caller does not require chat history, notifications, or broad listening
   in the same step.
4. The relevant IRC scope is known or can be named explicitly for the query.

## Procedure

1. Decide whether the caller wants:
   - the primitive `room snapshot`, or
   - the derived `presence query`
2. Bind the IRC query scope.
   - Choose one of:
     - `lane`
     - explicit `server` + `port`
   - Bind exactly one `room`
   - Bind one local query `nick`
2a. If the selected IRC server requires server-password auth, bind auth at the
    boundary.
   - prefer inherited `IRC_PASSWORD`
   - later may allow `IRC_PASSWORD_FILE`
   - do not place the literal password into committed artifacts, receipts, or
     algorithm notes
3. If the caller wants room state directly, run the snapshot primitive.
   - Use:
     - `m.irc.room.snapshot`
     - or `agent_skills/irc-lane/irc-lane.bat snapshot ...`
   - The snapshot should:
     - connect statelessly
     - join the room
     - read the initial membership state
     - normalize it into a typed room-snapshot result
4. If the caller wants to know whether a target nick is online, run the
   derived presence query.
   - Use:
     - `m.irc.presence.query`
     - or `agent_skills/irc-lane/irc-lane.bat presence-query ... --target-nick <nick>`
   - Bind:
     - `target_nick`
5. Interpret the result honestly.
   - `yes` means:
     - the target nick is visible in the queried room snapshot
   - `no` means:
     - the target nick is not visible in the queried room snapshot
   - `unknown` means:
     - the snapshot could not be obtained truthfully enough to make a `yes` or
       `no` claim
6. Preserve the current boundary.
   - Do not widen this one step into:
     - persistent login/session handles
     - chat history / scrollback
     - notifications
     - broad IRC listening
     - guard-rail policy work
7. If a different question appears, route to the stronger adjacent owner.
   - Room/context confusion:
     - prefer the snapshot result itself
   - Recent activity:
     - leave to the backlog neighbor under the IRC mission
   - Wrong-room action safety:
     - route to the guard-rail owner instead of forcing it into `query-irc`

## Decision Rule

Default rule:
1. prefer the reusable primitive first:
   - `room snapshot`
2. treat `is <nick> online?` as the first derived use of that primitive
3. answer only current room visibility, not human availability
4. stop before adding history, notifications, or listening pressure to the
   same procedure

## Outputs

- one of:
  - a typed room snapshot result
  - a typed presence-query result with:
    - `yes`
    - `no`
    - `unknown`
- explicit room/network scope for the claim
- freshness boundary via capture time
- a truthful reason string when the result is `unknown`

## Stop Conditions

Stop and route elsewhere if:
- the caller is really asking for availability rather than IRC visibility
- the caller needs message history or recent-activity interpretation
- the caller needs broad listening or notifications
- the room/network scope is still too unclear to make a bounded query

## References

- `holes/missions/M-mfuton-familiar-typed-surface-world-and-memory/M-my-familiar-multicast-and-virtual-space-component/M-irc-command-typed-surface-and-skill-promotion.md`
- `holes/missions/M-mfuton-familiar-typed-surface-world-and-memory/M-my-familiar-multicast-and-virtual-space-component/M-irc-command-typed-surface-and-skill-promotion/presence-query-user-story-2026-03-17.md`
- `holes/missions/M-mfuton-familiar-typed-surface-world-and-memory/M-my-familiar-multicast-and-virtual-space-component/M-irc-command-typed-surface-and-skill-promotion/presence-query-pm-lead-dev-simulation-2026-03-17.md`
- `holes/missions/M-mfuton-familiar-typed-surface-world-and-memory/M-my-familiar-multicast-and-virtual-space-component/M-irc-command-typed-surface-and-skill-promotion/presence-query-lead-dev-dev-simulation-2026-03-17.md`
- `agent_skills/irc-lane/irc-lane.bat`
- `agent_skills/irc-lane/irc_lane.py`
- `src/mfuton/commands/irc.py`
- `src/mfuton/contracts/irc.commands.schema.json`
- `library/social/typed-command-surface-over-eval.flexiarg`
- `library/system-architecture/typed-command-control-plane-authority.flexiarg`
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
