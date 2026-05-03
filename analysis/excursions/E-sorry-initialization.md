# Excursion: Sorry Initialization

**Date:** 2026-04-02 → 2026-04-07
**Entry point:** Joe HUD (futon0) → Click 1 → Sonnet dissolution → sorry ontology

## What Excursions Are

An excursion is a walk through the stack that starts from wherever you are,
follows the edges that are live, does work where work is needed, and returns
having changed both the stack and your understanding of it. It has a direction
and a sensitivity to what it encounters, not a destination.

Excursions are scattering experiments relative to a fixed object (the stack
as-is). Missions are scattering experiments one dimension up — they deform the
stack through design-build-use, generating information about the deformation
process itself. Same class of thing, different timescale. An excursion is a
section of the sheaf at a fixed time; a mission is a path through the sheaf
across time, where each step changes what the next section sees.

The sorry ontology lives at the interface: visible to excursions (you can see
typed holes by probing), modified by missions (design-build-use fills, composts,
or abandons them). The excursion discovers which sorrys are live; the mission
acts on that discovery.

## Edges Walked

- Joe HUD (futon0/emacs/joe-hud.el, futon0/scripts/futon0/report/joe_hud.clj)
- Click 1 Bayesian model (futon0/analysis/)
- Zoom R4 recording index (storage/zoomr4/meta/)
- Affective event detector (futon0/scripts/futon0/rhythm/affect.clj)
- M-repl-wins-over-cli (futon3c) — evidence gap as data pipeline blocker
- M-self-representing-stack (futon4) — futon0 as cyborg facet
- Portfolio inference (futon3c/portfolio/) — the existing conductor
- Mission control (futon3c/peripheral/mission_control_backend.clj) — scanning
- futon2 AIF engine — mechanism at ant scale
- Candidate Queue (futon4) — semi-structural channel
- M-mission-peripheral, M-mission-control, M-cyder — lifecycle infrastructure
- Sonnet dissolution conversation (2026-04-06) — sorry as typed hole
- inhabitation-feeds-evolution.flexiarg — Baldwin loop: inhabitation generates evolution data
- surface-earns-inhabitation.flexiarg — surface must earn entry by being less friction than primitive path

## Work Done

- Built Joe HUD with 4 signals (schedule, evidence, breadth, creative)
- Built Click 1: Beta-binomial P(recording | commit-time bucket)
- Wired creative workflow to real Zoom R4 data
- Added diff histogram (current vs previous window)
- Fixed timezone, table alignment, hot reload integration
- Located affective event code (futon0/rhythm/affect.clj, futon3a/affect.clj)
- Surveyed portfolio inference, mission control, futon2, candidate queue
- Developed sorry ontology and lifecycle vocabulary
- Identified channels-as-sorrys pattern
- Connected sorry ontology to inhabitation patterns: property 5 (inhabitation rate)
- Recognised that excursions are the minimal inhabitation that makes sorrys legible

## The Claim

A sorry — in the Lean typed-hole sense — is the unit of attention in an active
inference architecture. The sorry is not a metaphor applied across domains; it
is a shared formal property: **a commitment to a shape that you can't currently
fill, where the surrounding structure compiles around the absence**.

Every sorry has:
1. A **type signature** — what would close it
2. A **maintenance cost** — what you pay to keep the structure around it
3. A **liveness status** — whether anything downstream still needs it
4. A **staleness signal** — how long since anything touched its neighbourhood
5. An **inhabitation rate** — whether anyone is inside the space where this
   sorry could generate data about itself

The fifth property is the one that makes this different from "wiring."
Properties 1-4 can be evaluated from outside — you can inspect the
hypergraph and compute connectivity, cost, liveness, staleness. But
property 5 can only be discovered by inhabiting the space. A sorry in
an uninhabited peripheral is epistemically opaque: you can't even read
its type signature, because the type signature only becomes legible
through use.

This is the insight from `peripherals/inhabitation-feeds-evolution`
and `peripherals/surface-earns-inhabitation`: the Baldwin loop that
evolves peripherals runs on inhabitation data. A sorry in a dead
peripheral generates no data about whether it's live or dead. The
Candidate Queue sorry is unreadable not because the queue is badly
designed but because nobody inhabits it long enough for the sorry to
reveal its shape. The REPL evidence gap isn't "we need to connect A
to B" — it's "we don't yet know what the connection looks like because
the space where we'd discover it isn't inhabited."

This is why "wiring" diminishes the issue. Wiring implies known
topology, known endpoints, known cable types. What we actually have
are spaces that would *tell us* what the topology is, if we inhabited
them. The sorrys that would specify the connections live in those
spaces. The excursion — a brief, directed inhabitation — is the
minimal act that generates enough data for the next move.

Excursions are therefore not just observational. They are the
inhabitation that makes sorrys legible. Each excursion walks through
a region of the stack, briefly inhabits each space it crosses, and
the inhabitation generates data about the sorrys in that space —
their type signatures, their liveness, their connections. Without
the excursion, the sorrys are dark.

## Where This Comes From

A conversation (2026-04-06) escalated through three registers:

- **Sonnet (hierarchical):** enumerated connections between proof sorrys,
  behavioural couplings (Click 1 Beta-binomial), and institutional sorrys
  (UKRN NPT model). Correct but cheap — "everything is a graph."

- **Dissolution pass:** broke domain boundaries. The interesting insight:
  the sorry in Click 1 (the unobserved interior of the commit-to-recording
  edge) is a typed hole in the same sense as a sorry in Lean. The model
  compiles around its absence. The prior is the type signature; the posterior
  is whether the hole closes.

- **Re-crystallisation (Opus):** the sorry ontology demands a specific thing
  from futon2 — not a phase-gate conductor but a **liveness evaluator over
  typed holes**. The conductor should ask "what is the expected free energy
  reduction from closing this sorry?" as a function of downstream
  connectivity, maintenance cost, closure probability, and staleness.

## Connection to the Garage

A sorry that will never close is mouse-infested. It occupies typed space,
the surrounding structure compiles around it, and you maintain it out of
vague obligation. Dead sorrys are not visible at the leaf level. You need
the hypergraph — the relational structure — to see which sorrys are upstream
of live goals and which are just taking up room.

The garage clearout is hypergraph reasoning: evaluating whether each
commitment is upstream of anything live. Disposal in the formal architecture
means marking a sorry as **abandoned** — not failed, not closed, but actively
decommitted. This is a state the conductor doesn't currently have.

## Sorry Lifecycle Vocabulary

The lifecycle isn't binary (alive/dead). A sorry can close in structurally
different ways, and the way it closes matters for the hypergraph:

**Live states** (sorry maintained):
- `:active` — under attention
- `:blocked` — waiting on another sorry to close
- `:parked` — intentionally deferred (味→未@0 boundary rule)

**Closed states** (sorry resolved):
- `:complete` — the typed hole was filled
- `:superseded-by <M-xxx>` — another sorry absorbed the type signature
- `:composted-into <M-xxx>` — material fed a new sorry with different shape
- `:abandoned` — the type signature itself is no longer wanted

The first two closed states create edges to successor sorrys in the
hypergraph. `:abandoned` is a terminal node. This vocabulary replaces
the current flat set {active, complete, blocked, ready, in-progress,
unknown} with one that preserves structural information about *how*
sorrys relate to each other across time.

## Channels Are Sorrys Too

The existing "views over the system" — the Candidate Queue (futon4),
the Mission Portfolio (M-mission-control), the Peripheral Inspector
(M-cyder), the Joe HUD (futon0) — are themselves sorrys on the
hypergraph. Each is a typed commitment to a navigable view that partially
exists but doesn't connect to the others. They are semi-structural:
the surrounding architecture compiles around them, but their interiors
aren't filled enough to be useful as conductor inputs.

"Upsampling" these channels means: giving each one enough internal
structure that the conductor can read them as observation signals, not
just as human-readable summaries. The Mission Portfolio produces data
but the Candidate Queue doesn't consume it. The HUD shows staleness
but the portfolio inference loop doesn't read it. Each channel is a
sorry whose type signature is "I provide a structured signal that the
conductor can act on" — and most of them don't yet close that promise.

The sorry audit itself is a channel with this same property. Building
it means filling a typed hole; building it *well* means filling it in
a way that's consumable by `portfolio/observe.clj`, not just readable
by Joe.

## Two Conductors, Not Yet Connected

**futon2** (ant scale, ~5500 LOC): A running AIF loop — observe →
perceive → affect → policy, with hunger-driven precision modulation,
mode FSM, default-mode reflex fallback, and pattern-aware EFE. This
is the *mechanism* of active inference, proven on a toy domain. The
adapter protocol exists (`futon2/aif/adapter.clj`, 11 LOC) but is
minimal. M-aif-head (complete) designed how this mechanism extends to
mission peripherals.

**futon3c/portfolio** (institutional scale, ~2000 LOC): A running AIF
loop for mission prioritisation — 15 observation channels, Bayesian
belief update, mode transitions with hysteresis, EFE-based action
selection with softmax + abstention, core.logic dependency queries.
This *is* the conductor. It already recommends actions ({:work-on,
:review, :consolidate, :upvote, :wait}) backed by free energy
minimisation.

Neither currently has:
1. The sorry lifecycle vocabulary (no `:abandoned`, `:superseded-by`,
   `:composted-into`)
2. A maintenance-cost observation channel
3. Cross-sorry-type evaluation (the dissolution move)

## What Exists Already

**Operational:**
- **Portfolio inference** (futon3c/portfolio/): the conductor. Observe →
  perceive → affect → policy. 15 channels, EFE action selection, core.logic
  dependency queries. `portfolio-step!()` returns recommended action.
- **Mission control** (futon3c/peripheral/mission_control_backend.clj):
  800+ LOC. Cross-repo mission scanning, dependency graph, coverage analysis,
  tension export. `build-inventory()` scans M-*.md across repos.
- **Self-representing stack** (futon4): tensions as navigable hyperedges in
  Arxana. Reflection API live. Narrative trails. VERIFY complete.
- **futon2 AIF engine**: full ant-scale AIF loop. Adapter protocol for
  domain-agnostic use. Pattern-aware EFE. M-aif-head complete.

**Prototypes:**
- **Click 1** (futon0/analysis): Beta-binomial sorry-closure prediction.
- **Joe HUD** (futon0): staleness and breadth signals.
- **Affective event detector** (futon0/scripts/futon0/rhythm/affect.clj):
  12 affect intents, 10-minute lookahead, novel-term accumulation.

**Semi-structural (channels that are themselves sorrys):**
- **Candidate Queue** (futon4): invariant prioritisation, not yet
  consumable by portfolio inference.
- **Mission Portfolio** (M-mission-control): produces reviews, but the
  Candidate Queue and HUD don't read them.
- **Peripheral Inspector** (M-cyder): peripheral introspection, not yet
  wired to liveness evaluation.

## Buildable Steps

1. **Sorry lifecycle vocabulary in mission schema:** add `:parked`,
   `:superseded-by`, `:composted-into`, `:abandoned` to
   `mission_control_shapes.clj` and the status parser.
2. **Maintenance-cost observation channel** in `portfolio/observe.clj`:
   for each active/blocked mission, count downstream references weighted
   by their staleness. A mission nothing live depends on is cheap; a
   mission many stale things reference is mouse-infested.
3. **Sorry audit as HUD signal:** scan missions, compute liveness from
   the dependency graph, surface zero-downstream and high-maintenance
   sorrys. Make this consumable by the portfolio conductor, not just
   human-readable.
4. **`:decommit` action in portfolio policy:** the conductor can now
   recommend abandoning a sorry, not just working on one.
5. **Generalise Click 1:** the Beta-binomial structure fits any binary
   outcome conditioned on observable state. Add clicks for different
   sorry-types as the evidence pipeline grows.
6. **Channel upsampling via inhabitation:** for each semi-structural
   channel (Candidate Queue, Peripheral Inspector, etc.), the type
   signature of the sorry *cannot be determined from outside*. It
   requires an excursion: inhabit the channel, let the inhabitation
   reveal what the sorry looks like, then define the structured signal
   it needs to emit. This is the Baldwin loop at the channel level —
   inhabitation generates the data that specifies the connection.

## Execution Notes after Vasilopita

`E-Vasilopita` removed the local friction (`cr` makes claude-repl as
cheap to inhabit as the CLI), so property 5 is measurable again: session
starts, turn counts, and agent registrations now flow into the Agency
evidence store. That makes the next moves in this file executable rather
than aspirational — the conductor can receive structured sorry data if
we thread the new inhabitation signal into the stack.

### 1. Upgrade the mission schema before touching policy

- **Files:** `futon3c/peripheral/mission_control_shapes.clj`,
  `mission_control_backend.clj`, `futon3c/peripheral/mission_parser.clj`.
- **Action:** extend the enum with `:parked`, `:superseded-by`,
  `:composted-into`, `:abandoned`. Add structured fields for
  `:superseded-by` and `:composted-into` so graph edges survive the
  transition. Update the parser/serializer so CLJ ↔︎ MD is lossless.
- **Reason:** without this vocabulary the rest of the pipeline has
  nowhere to write the result of any sorry audit or conductor action.

### 2. Add the maintenance-cost observation channel

- **Files:** `futon3c/portfolio/observe.clj`, `futon3c/portfolio/state.clj`.
- **Action:** walk the dependency graph built by mission control,
  count downstream references, weight by staleness, and emit a scalar
  per active or blocked mission. Feed it into the `observation->belief`
  map so the conductor can see “mouse-infested” sorrys as high-cost
  energy drains.
- **Reason:** this is the quantitative version of the garage metaphor;
  without the channel the conductor cannot trade off maintenance cost
  against expected free energy reduction.

### 3. Build the sorry audit as both HUD and conductor signal

- **Files:** `futon3c/peripheral/mission_control_backend.clj`,
  `futon0/scripts/futon0/report/joe_hud.clj`,
  `futon3c/portfolio/observe.clj`.
- **Action:** produce a ranked list of zero-downstream or high-cost
  sorrys, surface it on the HUD, and expose the same data structure as
  an observation channel. The audit should be consumable by
  `portfolio/observe.clj`, not only human-readable.
- **Reason:** excursions need to see the sorrys; the conductor needs to
  act on them. A single pipeline should feed both.

### 4. Teach the policy to `:decommit`

- **Files:** `futon3c/portfolio/policy.clj`,
  `futon3c/portfolio/efe.clj`.
- **Action:** add a `:decommit` action whose utility is driven by the
  maintenance-cost channel and the sorry audit. Update softmax routing
  so `:decommit` competes with `:work-on`, `:review`, etc., and ensure
  the resulting recommendation writes the new lifecycle state back
  through mission control.
- **Reason:** without a first-class `:decommit`, the conductor can only
  shift attention, never deliberately abandon a dead sorry.

### 5. Generalise Click 1 now that inhabitation data flows

- **Files:** `futon0/analysis/clicks/`, `futon0/analysis/excursions/`.
- **Action:** refactor the Beta-binomial machinery so any binary
  sorry-closure signal (REPL usage, mission completion, audit outcome)
  can plug in with new priors. Back the new channels with the `cr`
  evidence logs so Click 2 (recording→creativity) has the data it
  needs.
- **Reason:** the causal model depends on the evidence pipeline; tying
  Clicks to inhabitation ensures we do not regress into CLI opacity.

### 6. Upsample the other channels via excursions

- **Targets:** Candidate Queue (futon4), Peripheral Inspector (M-cyder),
  Mission Portfolio (M-mission-control), HUD.
- **Action:** schedule excursions that inhabit each surface long enough
  to read the sorrys inside it. Each inhabit → measure loop should end
  with a structured signal that portfolio/observe can merge (e.g.,
  queue tension counts, peripheral liveness, reviewer coverage).
- **Reason:** property 5 cannot be faked; only inhabitation reveals the
  type signature of these channel sorrys. The `cr` work proved the
  pattern once — replicate it across the remaining semi-structural
  surfaces.

## The Test

The sorry ontology is worth keeping if it produces **better conductor
decisions** than the phase-gate. Empirically testable: if the sorry
audit surfaces a mission that you then abandon or compost, and that
act frees attention that closes a live sorry, that's evidence. Click 1
is the prototype of this test at the behavioural scale.

## Residue

What this excursion now knows that future excursions can use:

- The portfolio conductor exists and is operational — don't build a new
  one, extend it.
- futon2's AIF engine is the mechanism; futon3c/portfolio is the
  institutional instance. They share the observe→perceive→affect→policy
  shape but aren't connected.
- The evidence gap from CLI usage blocks Click 2 (recording → creativity).
  M-repl-wins-over-cli is on the critical path for the causal model.
- Channels (Candidate Queue, Mission Portfolio, HUD, etc.) are themselves
  sorrys. Upsampling them is not a separate project — it's the same work
  as filling the conductor's observation channels.
- The sorry lifecycle vocabulary (`:parked`, `:superseded-by`,
  `:composted-into`, `:abandoned`) needs to land in mission_control_shapes.clj
  for any of the downstream work to have a place to record its effects.
- Excursions are the right container for this kind of work. Missions are
  one dimension up — they deform the stack; excursions probe it.
- Sorry property 5 (inhabitation rate) is the bridge between the sorry
  ontology and the inhabitation patterns. A sorry in an uninhabited space
  is epistemically dark — you can't even read its type signature.
- The reason "wiring" is the wrong word: we don't know the topology in
  advance. The topology is revealed by inhabitation. Excursions are how
  we inhabit spaces long enough to read the sorrys in them.
- Next excursions could walk the Candidate Queue, the Peripheral Inspector,
  or the portfolio inference loop itself — each one generating the data
  that specifies what connection that channel needs.
