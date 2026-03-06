# Technote: Jazz Night — Distributed Human-Agent Rhythm

Date: 2026-03-01

## The Idea

Jazz Night is a continuous-uptime operating model for human-agent swarms.
Collaborators in different timezones take ~4-hour shifts supervising a fleet
of autonomous agents. The agents work around the clock; humans follow the
sun, handing off oversight responsibility as the earth rotates.

The name comes from the structure: improvisation within a shared rhythm,
each player taking a solo while the rhythm section (the agent fleet) keeps
going.

## Proof of Concept

On 2026-02-28, a three-agent pipeline (Codex writing, Claude reviewing,
Tickle orchestrating) produced 42 batch PRs (~200 PlanetMath encyclopedia
entries) overnight before hitting OAuth token expiry. The pipeline ran
unsupervised for ~10 hours. Human intervention was needed for:

- Token refresh (Codex + GitHub OAuth expiry)
- Course correction (Codex pushing to wrong repo)
- Bridge restart (stale WS connections blocking relay)

These are exactly the kind of interventions a shift collaborator handles:
small, situational, requiring human judgment but not deep expertise.

## Shape of a Shift

A Jazz Night shift (~4 hours) consists of:

1. **Pickup**: read the handoff summary from the previous shift. Check
   agent fleet health, token status, open PRs, error log.
2. **Supervision**: monitor IRC, review PRs, merge good work, course-correct
   when agents go off-track. Intervene only when needed.
3. **Handoff**: write a brief summary (what landed, what's stuck, what needs
   attention). Refresh any expiring tokens. Leave the fleet in a clean state.

The shift collaborator does not need to understand the full codebase or the
research domain in depth. They need to be able to read a PR diff, spot
obvious errors, restart a bridge, and refresh a token. The agents do the
domain work; the human provides judgment and continuity.

## Capability Approach

Amartya Sen's capability approach asks not "what do people have?" but "what
are people able to do and become?" The standard worry about AI and labour
is framed as job loss — a question of *having* (employment, income). But
the deeper ethical question may be about *capability displacement*: not that
people lose jobs, but that certain skills and capacities concentrate in a
few centres while the wider population loses access to the conditions for
developing them.

Roberto Mangabeira Unger's critique of institutional fetishism is relevant
here: the tendency to treat current institutional arrangements as natural
rather than as contingent structures that could be otherwise. The
concentration of AI capability in a small number of firms and research labs
is not a law of nature — it is an institutional arrangement that could be
reorganised.

Jazz Night is an experiment in that reorganisation. It distributes the
*practice* of working with AI agents — not just consuming their outputs,
but supervising, correcting, and directing them — across a wider group of
people. The shift collaborator develops capabilities that would otherwise
accrue only to those inside AI labs or well-resourced engineering teams:

- **Judgment under uncertainty**: when to intervene, when to let the agent
  work, when to escalate.
- **Multi-agent coordination**: understanding how agents interact, where
  handoffs fail, how to restart a stalled pipeline.
- **Quality oversight at scale**: reviewing 40 PRs overnight isn't about
  reading every line — it's about developing an eye for what matters.
- **Institutional imagination**: seeing that the current arrangement (one
  person, one laptop, one overnight run) is not the only possible
  arrangement.

The philosophical critics who find Sen's capability approach "vague and
under-elaborated" are right that it needs institutional specificity. What
does it look like, concretely, to expand capabilities rather than just
redistribute income? Jazz Night is one answer: a structured practice that
gives people access to the *activity* of directing AI systems, not just
their outputs. The scaffold matters — raw LLMs don't develop human
capabilities because they don't require human judgment in a sustained,
structured way. A shift roster does.

## Scaling Dimensions

The overnight proof-of-concept used one MSC class (18 — Category Theory).
The architecture supports parallelism along several axes:

- **MSC classes**: one IRC channel and Tickle conductor per class. 63
  top-level classes × 5 entries per batch = systematic PlanetMath
  renovation.
- **Machines**: 3 Linodes + 1 laptop = ~12 concurrent agent sessions.
  Each machine runs its own futon3c instance with WS federation.
- **Collaborators**: one per timezone band (UTC-8, UTC, UTC+8 covers
  24 hours with comfortable overlap).
- **Domains**: nothing restricts Jazz Night to mathematics. Any domain
  with structured knowledge and a corpus to mine is eligible.

## Relation to Futon0

Jazz Night extends futon0's rhythm infrastructure from individual to
collective:

| Futon0 concept | Individual | Jazz Night |
|----------------|-----------|------------|
| Time envelope | One person's wake/sleep cycle | Union of all collaborators' work windows |
| Vitality gap | Hours since last activity | Hours since last supervised shift |
| Salients snapshot | Weekly personal review | Per-shift handoff summary |
| Cadence tracking | Daily/weekly/monthly | Shift/day/week |
| WIP cap | ≤3 active threads | ≤N active MSC classes per shift |

A future mission (successor to the current futon0 rhythm buildout) would
formalise the shift registry, handoff protocol, and coverage metrics. The
agent fleet management (token refresh, bridge health, conductor tuning) is
already operational; what's missing is the human coordination layer.

## Open Questions

- **Incentive structure**: why would collaborators take shifts? Intrinsic
  interest, co-authorship, skill development, or something else?
- **Quality floor**: what's the minimum competence for a shift collaborator?
  Can we lower it through better tooling (dashboard, one-click token refresh,
  automated PR review)?
- **Handoff fidelity**: how much context survives a shift change? The
  salients summary is lossy by design — is that enough?
- **Liability**: who is responsible for incorrect content that lands during
  a shift? The collaborator, the agent, or the system?

## Sources

- Sen, A. (1999). *Development as Freedom*. Oxford University Press.
- Unger, R. M. (2007). *Free Trade Reimagined*. Princeton University Press.
- Nussbaum, M. (2011). *Creating Capabilities*. Harvard University Press.
  (For the "vagueness" critique and the list-based elaboration of Sen.)
- futon0 manifest: `futon0_manifest.md`
- futon0 rhythm: `futon0_rhythm.md`
- futon0 protocol: `futon0_protocol.md`
- Overnight proof-of-concept: futon3c IRC logs, 2026-02-28
