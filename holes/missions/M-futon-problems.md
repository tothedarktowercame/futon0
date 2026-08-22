# M-futon-problems: what problems is this stack actually solving?

**Type:** Mission
**Status:** HEAD (drafted 2026-08-21, claude-13 + Joe, emacs-repl)
**Gate:** operator-acceptance — HEAD must be recognised as faithful before
IDENTIFY hardens it.
**Owner:** unassigned (drafted by claude-13)
**Home-repo:** futon0 (cross-repo coordination home; the mission spans
futon0/futon3/futon3c/futon1b corpora)

**Cross-references (read-only intake):**
- `futon3/library/war-room/*.flexiarg` — the root tier, WR-1..27 (27 patterns)
- `futon3/holes/war-bulletin-{1..15}.md`, `futon3/holes/war-room.md` — sources
- `futon0/holes/missions/M-capability-star-map.md` + `.graph.edn` — 37 stars
- `futon3c/docs/retrieval-whitepaper-v2.md` §4.2, §4.3, §4.5, §5.1, §7.1 — the
  measurement discipline this mission inherits
- `futon3/library/futon-theory/reverse-morphogenesis.flexiarg` — the method for
  the fourth corpus
- `futon0/analysis/business-models/` — a worked precedent for the
  prose→records→checker path

---

## HEAD

### The question

**What problems is this stack actually solving?** Not what capabilities it has
(the star map answers that, and answers it in a misleading way — see below),
and not what work was done (the mission corpus answers that). What *problems*.

The question resists the obvious sources. Each corpus that looks like a problem
ledger is a partial view with a structural blind spot, and the blind spots are
different. That is the mission's central premise and the reason for a **panel**
rather than a metric.

### Why now

Three things came available in August 2026 that were not available before:

1. **The root tier exists.** WR-1..27 were promoted from bulletin prose into
   `futon3/library/war-room/` on 2026-08-21 (commits `6ee8c7c`, `832809a7`,
   `66e3150`). Before that, 7 of 28 rulings were patterns; the rest were prose
   scattered across 15 bulletins, and WR-11..22 were widely believed missing.
   They were not missing — they were in bulletins 9 and 10, unpromoted.
2. **The evidence landscape is queryable.** futon1b on `:7073` holds 155,807
   evidence entries; 10,980 authored by `joe` across 344 sessions since
   **2026-02-18**. Backward paging works via `before` + newest-first sort.
3. **A measurement discipline exists and is written down** —
   `retrieval-whitepaper-v2.md`, which has already falsified two lexical
   mechanisms, inverted a spectral threshold, and recorded four denominator
   failures *on this codebase*.

### The four corpora, and why no one of them suffices

| corpus | catches | misses |
|---|---|---|
| **WR rulings** (27) | problems that caused **friction** | smooth ones |
| **held capability stars** (13) | problems that **stalled** | solved ones |
| **missions** (267 files) | problems someone **chose** | emergent ones |
| **VSATARCS ← retrieval** | problems solved **silently** | ones nobody looks up |

The fourth is not a problem ledger as written — it documents what exists. It
becomes one under **reverse morphogenesis**: 象 ← 香, *given this form and this
salience, what constraints made the form stable?* The flexiarg is explicit that
**both** inputs are required; without salience the operation degenerates into
just-so history. The salience source is `context-retrieval` evidence events,
which carry scored ranked results and are produced as exhaust by agents doing
work, not authored to make a point.

**The first three ledgers are all biased toward problems that were *noticed*.**
A problem solved smoothly produces a working artifact, a doc entry, and
silence. Those are precisely the capabilities most worth selling. Only the
fourth corpus can see them.

---

## The measures

Six measures over the corpora. **The output that matters is not the ranking —
it is the disagreement**, because each measure's blind spot is different.

| # | measure | over | status (2026-08-21) | denominator |
|---|---|---|---|---|
| M1 | mission-citation degree | 267 mission files | ✅ computed | 1,174 patterns |
| M2 | `@references` in-degree | library graph | ✅ computed | 1,271 authored edges |
| M3 | eigenvector centrality | library graph | ✅ computed, **degenerate alone** | 1,174 nodes |
| M4 | ruling-invocation | 27 WR patterns | ⛔ **blocked** — see below | 27 rulings |
| M5 | stall-association (star→mission→pattern) | 13 held stars | ◐ partial — `:minted-by` is populated mostly on *satisfied* stars | 13 |
| M6 | retrieval frequency | evidence store | ◐ partial — events exist; `tag=` is silently ignored on `/api/alpha/evidence`, so it needs a `before`-walk | ~thousands |

### Results to date

```
M1 mission-citation      7  futon-theory/baldwin-cycle
                         7  agent/evidence-over-assertion
                         7  peripherals/inhabitation-feeds-evolution
                         6  aif/expected-free-energy-scorecard
                         6  stack-coherence/evidence-ledger
M2 in-degree            39  stack-coherence/evidence-ledger
                        28  futon-theory/proof-path
                        16  futon-theory/stop-the-line
                        14  futon-theory/baldwin-cycle
M3 centrality         1.00  futon-theory/proof-path
                      0.80  futon-theory/baldwin-cycle
                      0.68  futon-theory/four-types
```

**876 of 1,174 patterns are cited by no mission at all** (75%). The ceiling is
7 of 267 missions — **no pattern is attested by more than 2.6% of the corpus**.
Attestation is real and thin. The working set is *64 patterns cited by 3+
missions*, chosen by evidence rather than taste.

**Exactly one pattern ranks top-5 on all three computed measures:
`futon-theory/baldwin-cycle`.** That is the defensible sense of "high-ranking
eigenvalue".

### Why M3 must never be used alone

`retrieval-whitepaper-v2.md` §4.5: λ₂ **does** detect real wiring structure
(~15 SD below a degree-preserving configuration null, disconfirming the
pre-registered expectation) — **but the threshold is inverted**, monotone
decreasing in richness across 59×:

| graph | hyperedges | λ₂ |
|---|---:|---:|
| deployed memories, patterns only | 1 | 1.0000 |
| deployed + subjects | 51 | 0.0689 |
| git history | 3,014 | 0.0360 |

**A degenerate graph scores best.** Confirmed independently here: eigenvector
centrality on the `@references` graph puts **10 of its top 12 in
`futon-theory/*`** — it is measuring cluster membership, not importance.
`stack-coherence/evidence-ledger`, the single most-referenced pattern in the
library (39 in-edges), does not appear in the eigen-top-12 because it sits
outside that cluster.

### Why M4 is blocked, and the unblock

The WR rulings invoke patterns **in prose, by title, not by id**. WR-4's
BECAUSE reads *"Surface earns inhabitation… Inhabitation feeds evolution…
the Baldwin loop starves"* — three pattern titles, zero machine-readable edges.
Token-matching therefore returns 3 patterns across all 27 rulings.

Two options: match `@title` strings against WR prose (a **lexical** mechanism,
and §4.2 records two such mechanisms already falsified on this corpus), or
**author `@references` on the 27 war-room patterns**. The second is 27 files
and is the first increment of the `@why` layer rather than a detour from it.
Take the second.

### The disagreement taxonomy — the actual output

| pattern of results | reading |
|---|---|
| high M6, zero M1 | **silent workhorse** — used constantly, never written about |
| high M1, zero M6 | discussed but not used; aspirational or superseded |
| high M4, zero elsewhere | a one-off ruling that never propagated |
| high M2, low M1 | library-internal authority with no field uptake (`stack-coherence/evidence-ledger` today) |
| high on all | the genuine core (today: `futon-theory/baldwin-cycle`, on 3 of 3 computable) |

---

## Discipline (inherited, non-negotiable)

*Renumbered `DP*` on 2026-08-22: bare `D*` collided with the DERIVE items in
§2 of this same file. See `futon0/holes/GLOSSARY-numbering.md`.*

**DP1 — No fabricated tensions.** A `+ HOWEVER:` must be quotable or closely
paraphrasable from its source. Where a source states a ruling with no tension,
mark `@verdict incomplete` and record what is missing. *A tension we invent is
a false problem.* Three of 27 WR patterns are so marked: WR-12, WR-17, WR-20 —
all architectural assertions rather than friction residues.

**DP2 — State the denominator.** The corpora span 27 to thousands. §7.1 of the
whitepaper: *"Four inherited denominators failed on re-counting."* Every count
in this mission carries its population or it does not count.

**DP3 — Rank by agreement, never by one statistic.** Four consistent data points
now say a single spectral measure rewards degeneracy on these graphs.

**DP4 — No clique expansion.** A file touched by *k* commits is one incidence
relation, not *k(k−1)/2* pairwise ones. The whitepaper records that this error
inverted a metric once already.

**DP5 — Populated ≠ correct.** Recurrent failure family across this workspace,
observed six times on 2026-08-21 alone: `state: done` over
`execution.executed: false`; star titles reading n=0 over `:position` n=1–2;
autoclock mission-linkage present on 65% of turns and substantially wrong;
`offset` and `tag` accepted and silently ignored; a busy-store 503 counted as
zero results. **Check the field means what it says before counting it.**

---

## Corrections carried (do not quietly drop these)

- **"WR-4 cites three of the top four patterns"** — wrong verb. WR-4 *uses
  their titles in prose*; there is no citation edge. The correspondence is real
  and unlinked.
- **"May and June produced zero WR rulings"** — an artifact of reading
  `war-room.md` alone. Dates across the promoted patterns show 7 rulings on
  2026-05-17 and 5 on 2026-05-31.
- **"65% of turns carry mission linkage, so chat→mission is a runnable join"** —
  measured population, not correctness. 47 of 60 recent joe turns are tagged
  `M-apm-demonstration` across at least three distinct workstreams.
- **The `@verdict incomplete` split is not yet a trustworthy classifier.** Batch
  A marked 2 of 7 unprompted; batch C marked 0 of 8; batch B's single finding
  (WR-20) **was named in its own dispatch packet by claude-13** and is therefore
  not independent. Needs one single-judge pass with no hints.

---

## Next moves (unordered; IDENTIFY should rank them)

1. **Author `@references` on the 27 war-room patterns.** Unblocks M4; first
   increment of the `@why` layer.
2. **Single-judge invertibility pass** over all 27 rulings. Which were forced by
   friction and which were chosen? Only the first kind should be `@why` targets
   — a `@why` edge to an architectural assertion is justification by fiat.
3. **Author `@why` on the 64 patterns cited by 3+ missions**, pointing at
   invertible roots. Derive `@how` as the reverse index — **do not hand-author
   both**; two hand-maintained inverse relations will disagree.
4. **Walk the retrieval events** (`before`-paging) to compute M6.
5. **Reverse-morphogenesis pass over VSATARCS**, with retrieval frequency as 香.
6. **Recover the mint dates** for the 37 capability stars from
   `git log -p M-capability-star-map.graph.edn`. The star map holds 37
   capabilities and **13 date strings**; the diachronic axis is recoverable
   from version control and has never been extracted.
7. **Ask what the 876 uncited patterns are.** A library that outran its use, or
   a citation convention missions do not follow? Do not assume the first.

---

## Seeded checkpoints

### Checkpoint 1 — the belief-update chain is instrumented end to end

**What lands:** the `retrieved → used → verdict` chain, running per turn.

`fig:loop` in `p4ng/contents.tex` has three boxes in one frame — Generative
Model (patterns as priors) → Action Selection (which pattern now?) → Belief
Update (did it work?). **The first two are live; the third is empty**, and the
frame has no external input, so the loop currently grades itself.

| link | state | source |
|---|---|---|
| **retrieved** | ✅ live | `context-retrieval` evidence events, per turn, ranked + scored (`:score 0.4116 :rank 1 :retrieval-method "embeddings"`) |
| **used** | ◐ checkable | pattern cited in a produced artifact — same measure as M1 (298 patterns cited across 267 mission files) |
| **verdict** | ⛔ unbuilt | must be **exogenous**: Lean kernel accept, test suite, `clj-kondo`, `check-parens`, or an external acceptance event |

**The confound to design around, and it is pre-registered against us.**
Retrieval ≠ use. A score of 0.4116 at rank 1 says a pattern is *lexically near*
the turn, not that anything applied it. Assign credit on retrieval alone and
you reward embedding proximity — and `retrieval-whitepaper-v2.md` §4.2 records
**two lexical mechanisms already falsified on this corpus**. The `used` link is
what stops that, and it is why the chain has three steps rather than two.

**Graph inference** then means credit assignment over the pattern graph (1,271
authored `@references` edges), attenuating from the used pattern to its
neighbours. That is a belief update over a structured hypothesis space —
`aif/candidate-pattern-action-space` already declares patterns as the action
space.

**Free result available before any of it is built:** patterns with high
retrieval and zero citation are embedding noise rather than silent workhorses.
That distinction was a hole in the panel's disagreement taxonomy (D6) and this
chain closes it.

**Acceptance:** one turn's credit computed end to end from a non-manufactured
verdict, with the retrieved-but-unused set reported separately.

**Diagram delta (overlay, not redraw):** five WR badges — WR-8 on the
generative model and the edges, WR-19 + WR-14 on action selection, WR-9 + WR-25
on belief update, WR-27 on the frame itself — plus **one new node outside the
frame: the operator**, with edges crossing the drawn boundary. The
boundary-crossing edge is what carries `retrieved → used → verdict`. The
per-turn story and the theoretical claim are the same wire.

### Checkpoint 2 — business models as a pattern subspace

**What lands:** the 30 encoded business-model cases described in pattern form,
as a library category, so patterns-per-business becomes a measurable subspace
alongside patterns-per-mission.

The schema's seven node types (`requirement`, `benchmark`, `bounty`,
`dependency`, `call`, `environment`, `market-exchange`) **are already a pattern
language**; the 30 records in `futon0/analysis/business-models/records/` are its
instances, each carrying chain, oracles, acceptance-event, budget-line, seat.
Rendering them as flexiargs makes them citable, linkable, and part of the same
graph as everything else — and lets `@why` edges run from business patterns to
WR roots.

**Why this is load-bearing rather than tidy:** it is the mechanism behind the
standard offer (below). If a client's model is built in the same pattern
language, **their model is comparable to 30 encoded cases with known outcomes.**
That comparability is the product; the pattern language is what makes it
possible.

**Acceptance:** a client-shaped model expressible in the same vocabulary as the
30 cases, and positionable against them.

### The standard offer (Rob, via Joe, 2026-08-21) — recorded, not yet a checkpoint

> Using the open-source concepts of FUTON, come into a business and — better
> than *"we will make a model of what you do"* — **"by using these tools, you
> will make a model of how your business works."**

This is the **Environment** row crossed with the *"evidence substrate [for X]"*
template at X = a company: the client brings their own goals and the apparatus
makes their work legible to them. Per batch C, that row's discriminator is a
direct per-unit payment line arriving *with* the population, not after it.

**The demonstration already exists and is not a screenshot:** 298 cited
patterns, 267 mission files, 27 WR rulings, a 37-star capability map, and an
evidence landscape of 10,980 operator turns since 2026-02-18. FUTON is a worked
instance of a system that made a model of how it works. That is the strongest
form of the dogfooding claim available — the artifact is six months of
accumulated self-model, not a demo.

---

## 1. IDENTIFY

**Status note (2026-08-22):** this section and §2 were reported as written on
2026-08-21 and were not. The file went from HEAD straight to the measures. The
omission is recorded rather than quietly repaired, because it is an instance of
the failure this mission is about: a report of completion not backed by the
artefact.

### 1.1 The problem

**How do we know what problems, if any, we are solving?**

Second-order, and that is what makes it a mission rather than a task. The
system produces capabilities, missions, patterns and rulings in quantity. What
it does not produce is a way of telling which problems those address, for whom,
or whether any of them are closing.

### 1.2 The gaps

Each is stated as *we cannot currently tell*, which is the level they belong
at. They are obstacles to answering the problem, not the problem.

| | we cannot tell | evidence |
|---|---|---|
| **G1** | the *current* state of the landscape | six strategic views assert continuous currency; ages 25–79 days, median 36; one not serving at all |
| **G2** | which problems *the world* cared about | `:warm-customer-pays` satisfied and unsurfaced; cold-ladder titles read n=0 over positions n=1–2; `:position` absent from the projection the machine steers by |
| **G3** | *whose* problem a capability answers | `serves` dangling on 24 of 30 cases; `:beneficiary :unknown` on 24 of 30 |
| **G4** | *once* from *reliably* | 22 of 23 satisfied stars carry `:scale :tbd :position :tbd :next-rung :tbd` |
| **G5** | whether the fields we would measure with can be trusted | six instances in one day, four independent subsystems, two introduced by the reviewer while auditing the other four |
| **G6** | *asserted* from *held* | 65 candidate invariants, no processing pipeline (WR-4, April); no checked/violated/unchecked split anywhere |

**G1, G4 and G5 are one thing at three altitudes** — an assertion carried
forward without a check — and G2 is that failure pointed at the outside world.

**Rejected candidate:** *the library outran its use* (876 of 1,174 patterns
cited by no mission). Not a problem: patterns are generative, and a garbage
collection may become warranted but has not. Recorded so the count does not
quietly change later — 7 candidates, 1 rejected, 6 standing.

## 2. DERIVE

### D1 — G5 is upstream; nothing else is measured until a reading can be trusted
Every other gap closes by measuring something, and G5 says the instruments are
unreliable. Building on that base produces confident wrong numbers, which are
worse than none. *Rejected:* fixing each field as found — that was 2026-08-21,
and it produced six fixes and a seventh error.

### D2 — The fix is a record type, not a per-field repair
In all six instances a *second* field carried the truth: `execution.executed`
beside `state`, `:position` beside `:title`, HTTP status beside body, the file
count beside the arithmetic. So: **a measured claim names the field read, the
population read over, and the check that the field means what it says.** Same
shape as `:discharge` on unverified claims and `:expected` on conjectures, both
of which caught real errors.

### D3 — Reuse the proven instrument
prose → EDN records → checker with `:expected`, non-zero exit on mismatch. Used
three times; it caught an arithmetic error, refuted an overstated universal, and
made findings re-runnable. *Rejected:* a bespoke problems format.

### D4 — G1 closes by adding freshness checks to the six existing views, not a seventh
WR-4 turned on ourselves. A view asserting currency should carry a check that
fails loudly when stale. *Rejected, explicitly:* building a new strategic
overview — the tempting move, and what the last six were.

### D5 — Order by cost of the reading, not importance of the gap
G5 → G2, G4 (a projection change and a field-authoring pass) → G1 (freshness
checks) → G3, G6 (genuinely new work).

### D6 — Every gap is tested against the 30-case corpus
A gap appearing only in our own operation is housekeeping, not product. G4 and
G2 validate on positive evidence — records that *state* the defect. G1 and G6
are unvalidated: the corpus is businesses-as-businesses, and those are gaps of
*organisations operating AI agents*, for which no reference set exists. The test
must distinguish *the corpus records this gap* from *the company had this gap*;
only the second validates.

### D7 — A gap is closed only by an artefact that re-derives
Rendered artefacts decay; derived ones are current or absent. WR-8 already
governs this. By its own criterion most of 2026-08-21's output closes nothing:
`check.bb`, `graph.bb` and the `empirics-futon/` generators re-derive; the prose
documents do not.

**Steering direction.** Cost per turn is computable today (~$0.30 flat, 10,980
operator entries since 2026-02-18). Benefit is an *allocation*, not a
measurement: rung events are dated and lumpy, and attributing them backwards to
turns needs G5 (trustworthy attribution) and G2 (visible rungs). Neither
requires new theory.

### D8 — Operator attention is one typed field short of computable (measured 2026-08-22)

Joe (2026-08-22): the interesting model is not one "largely derived a priori"
but one "pegged to what you (and your team) are actually doing" — e.g. *how
much operator attention went to M-futon-problems vs M-apm-demonstration, while
everything else seemingly languishes unattended*, with the rebuttal available:
"yes, but this is dealing with crucial capability stars and it is worth the
investment."

Measured against the live store rather than assumed:

| hop | status | where |
|---|---|---|
| operator presence + recency | **available** | `GET /api/alpha/evidence/sessions` — 1,025 sessions, 348 ever attended by joe, 9 active in 24h, 4 within 5 min |
| session → mission | **MISSING as data** | exists only in mission-file prose (`**Status:** ... claude-13 + Joe`) |
| mission → capability star | **available, typed** | `M-capability-star-map.graph.edn` (33,776 b), built via `:mission-source :futon2.aif.mission-registry/load-missions`; carries `:status`, `:position`, `:next-rung` |

So the chain is broken in exactly one place. A mission binding on a session
makes attention-by-mission divide, join to the stars each mission advances, and
turn the investment case into a computation. This is WR-8 failing in its own
house: the seat→mission binding is real and instantly suppliable by the
operator, but its source of truth is prose.

**Correction folded in:** `?author=joe` on the sessions endpoint is *silently
ignored* — the response echoes `:author-filter null` and the same 1,025-session
total for every value tried (`author`, `authors`, `author-filter`). The filter
is client-side, over the row's `:authors` array. An earlier draft of
`p4ng/sec-operator.tex` stated the server-side form; corrected 2026-08-22, and
the footnote now names the miss as an instance of **DP5 (populated ≠ correct)**.

**Cross-domain instrument.** `~/code/ukrn-services-simulation/` satisfies the
same R1–R12 contract (`docs/aif-completeness.md`) and supplies two transferable
constructions: (i) the **Latent / Multiplied / Mismatch / Absorbed** quadrants
on a delivery-viability × architectural-sustainability plane — *latent is not
mismatch*, so "looks unattended" does not license cutting; (ii) **lesion
panels** (evaluator role disabled, researcher-advisor role disabled) as the way
to argue a role's worth — project the counterfactual rather than defend the
current allocation. Figure candidate for the paper:
`ukrn_v2_lesion_comparison.png` (awaiting Joe's word; it is a UKRN
working-paper asset and its reuse is his call).

### D9 — The star presupposes its complement; the rows are not phases (2026-08-22)

**The claim.** "It is worth it because I will get a capability star" presupposes
that something becomes possible once the star is held. Joe's UKRNS reading is
the counterexample made visual: in `ukrn_v2_lesion_comparison.png` most
trajectories are **mostly horizontal most of the time** — training raises
knowledge (x = delivery viability) but does not by itself raise institutional
support (y = architectural sustainability). Only the bottom-right *Rosy
(matured + favourable conditions)* panel shows sustained diagonal ascent.

**Diagnostic that falls out:** *a capability star without its complement
produces horizontal motion* — travel along the axis you can affect, none along
the axis that decides whether it mattered. A rung flipping to `:satisfied` is
x-motion. Whether anything downstream can now happen is y-motion, and the star
map does not currently record it.

**The two failure modes** (Joe, 2026-08-22):
1. chase stars reachable with laptop + agents but not valuable to others → no money;
2. chase stars related to making money → solve no problem of interest to me.

**Where each is already handled — and how:**

- Mode 2 is **named and deliberately unresolved**:
  `M-futon-forward-model.backlog-cascade-merged-v0.edn` `:tensions` `:t3` —
  *"centrality vs interest (inherited from v0, unresolved by design)"*, priced
  as *"structure says m13/fulab-logic; heart says m19/ukrns"*. Note that this
  put **UKRNS on the heart pole in July 2026**; proposing it now as a portfolio
  piece is an attempt to make one item serve both poles, which is the correct
  move under WR-19 (below).
- Mode 1's guard is **not a pattern but an open hole**: `mh7` — *"T-inf
  evaluation rubric (a yardstick someone else holds) … xeno-evaluation cannot
  be self-supplied; R9 at the ladder's top."* The defence against building what
  no one values is an acknowledged missing artefact.
- The refusal to average them is **rule (4)**: *"terminals union, ψ composes
  conjunctively — no weighted sum, no in-band scalarization."* The tension is
  not a modelling defect; it is the model declining to assert a commensurability
  that does not exist.

**The design pattern Joe guessed at exists: WR-19** (*Tension Must GENERATE,
Not Only Rank*) — "a ranking signal cannot create an option that is absent from
the menu." If domains cannot be scalarized, tension can only act by
**proposing candidates in the intersection**. But WR-19's `IF` clause is
currently scoped to the geometric manifold ("the geometric stack detects
high-tension regions", ΔT as a sorry-only tie-breaker). Joe's case is the same
shape one level up — portfolio tension between incommensurable streams.
**Pattern-work item: widen WR-19's IF, or mint a sibling at portfolio
altitude.** This is concrete work for the "WR patterns need work" increment.

**Rows vs columns — the resemblance is real but not an identity.** Joe noticed
the merged cascade's rows A–F resembling Figure 2's columns
PERCEIVE/BELIEVE/EVALUATE/SELECT/ACT. They are not the same kind:

- The columns are **phases of one loop** and are sequential.
- The rows are **not** sequential — E *preempts all*, and the brief lists them
  out of order (A, B, D, C, E, F).

The defensible reading is a **grid, not a match**: A (certifying
instrumentation) and B (grammar & actuator) are the phase-like rails — the
PERCEIVE and ACT ends factored out as shared infrastructure. C (told outward),
D (operator loop), E (livelihood), F (mathematics) are four **domains** that
run on those rails, each carrying its own BELIEVE/EVALUATE/SELECT.

That is precisely why the two failure modes are structural rather than
accidental: **each domain has its own EVALUATE and there is no shared one.**
F evaluates by epistemic gain, E by money. With no common currency, maxing one
while starving the other is the expected behaviour of the architecture, not a
lapse of operator discipline.

### D10 — Pointer: the vertical spine proposal (2026-08-22)

Joe proposes futon-2026 organise on the **vertical A–F** axis, plop-2026 having
used the **horizontal** PERCEIVE→ACT one. Worked up in two files under `p4ng/`:

- `SPINE-vertical.md` — the two axes are different kinds (one turn of the
  horizontal happens *inside* a row); A–F mapped to their 23 missions; the
  half-match with `analysis/business-models/SPINE.md`'s seven layers; and the
  thesis it buys — **one hole, rediscovered three times under three names**
  (layer 6 Valuation / mh7 xeno-evaluation / the dead `wm_outer_loop.clj`
  calibration term).
- `NOTE-bridge-as-surprisal.md` — Joe's conjecture stated falsifiably: a Bridge
  is high surprisal requiring *structure learning* and lowering expected
  surprisal thereafter (noise is high surprisal that does not generalise);
  serendipity potential = the epistemic term of EFE; **WR-19 is the serendipity
  pattern** (a ranking signal cannot create an absent option, and the Bridge is
  definitionally absent).

Both await Joe's word before any restructuring.

### D11 — The mission acquires a hard external gate (Joe, 2026-08-22)

**The cliffedge did not disappear; it moved to Rob.** D10/§7 recorded Joe's
runway extending to *sinecure → sabbatical → ?* and concluded the external
forcing function was removed. That is **half wrong**: the joint venture runs on
Rob's clock, and Rob has said he needs *"a clear path to be making money
sometime in October."*

**Dated gates** (from 2026-08-22):

| by | gate |
|---|---|
| ~2026-09-11 (~20 days) | meet Rob; until then, **loosely coupled** work |
| ~09-11 → ~10-01 (~20 days) | work on *the* or *a* candidate project that would actually make money |
| **end of 2026-09** | **a very clear idea of who the client would be** |
| 2026-10 | Rob's bar: a clear path to making money |

So *what problems are we solving* is no longer an open-ended inquiry with a
sabbatical behind it — it is gated, and the gate is **client identity by end of
September**. Joe: *"asking 'what problems are we solving' is crucial to do
now."* Note this lands directly on the 30-case finding: the discriminator was
never distance-in-hops but **whether a discrete per-unit acceptance event moves
money**, and "who is the client" is that question with a date attached.

**Rob's standing:** he holds capability FUTON does not have, which would form
part of a shared offering; some of the framing already in use came from
conversations with him; he would be part of a delivery team. So his input is
wanted *into* the problem statement, not after it. That is also the first
concrete answer to `mh7` (*a yardstick someone else holds*) — Rob is a
plausible holder, though a partner's verdict is not the same as a buyer's and
should not be recorded as one.

### D12 — Acceptance criterion: multi-operator from the start (measured)

Joe's criterion: *"if we ingest Joe's turns now, great, but we should also be
able to ingest Rob's turns later."* Checked against the code rather than
assumed — **it fails today, in three distinct ways:**

1. **Operator identity is a hardcoded string literal**, not a parameter, in at
   least 8 files: `futon0/scripts/futon0/report/joe_hud.clj`,
   and in `futon2/holes/labs/M-zaif-harness/`: `z1_views.clj`,
   `l1_referent_resolution.clj`, `b1_gamma_mission_fold.clj`,
   `pz1_build_labeling_sheet.clj`, `pz1_lexicon_scan.clj`,
   `mark_vocab_probe.py`.
2. **The concept is singular, not a set.** `joe_hud.clj` partitions turns into
   `joe-turns` vs `agent-turns` — a binary with no room for a second operator.
   A second human would be classified as an agent.
3. **Worst: operator identity is inside a scoring predicate.**
   `b1_gamma_mission_fold.clj:43` — `(or (neg? perf) (= author "joe"))`. The
   fold logic treats one named human specially, so adding an operator changes
   scores rather than just widening a filter.

Corroborating shape, **restated after a correction from Joe (2026-08-22)**:
an earlier version of this entry said *"of 146 distinct authors, `joe` is the
only human"* on the strength of eyeballing the top 15 of a name-prefix
heuristic — the DP5 failure again. Joe: *"It may be that 'charlie' is in there
or some other such — I had workshop participants joining in a live IRC
workshop."* Enumerated properly this time, on **both** stores (:7070 JSON and
:7073 EDN — same content, 1025 sessions, ~122k entries): the full author list
contains no `charlie` and no workshop participants, and `?author=charlie`
returns **0 entries on both**. So the count survives, but the framing was
wrong, and the right framing is more useful:

**The stack has two unconnected vocabularies for humans.**

- **Authorship** — the evidence store's `:evidence/author`. Populated by one
  human (`joe`) and 145 agent/pipeline personas.
- **Transport** — *"exogenous writes into the genotype"*
  (`futon3c/holes/excursions/E-causal-coupling-top-down.md` §7), with a
  measured caveat: *ungated transport stays ordered; only transport gated on a
  real interface clears.* This is where the other humans actually live. Charlie
  appears in a **2026-07-29 standup with Joe and Charlie**; Rob appears
  throughout `futon7/holes/E-business-exotype-audit.md`.

**And the IRC bridge is the existing precedent for multi-human ingestion.**
`futon3c/README-irc.md` describes "Human clients" (plural) on ngircd 6667/6697,
and `!todo add <text>` is *"attributed to the sender"* — so per-sender identity
exists **at the surface** and simply does not reach `:evidence/author`.

So the gap is sharper than "no second human has authored": **Rob joining a
delivery team means crossing from transport to authorship, and nothing bridges
those two vocabularies today.** The substrate does not block it — `:authors` is
already a list — so the work is (i) route the bridge's per-sender identity into
the author field, and (ii) rename-and-widen the 8 hardcoded `"joe"` literals to
an operator set resolved from config. Neither is a schema change.

### D13 — The September gate already has a worked answer document

`futon7/holes/E-business-exotype-audit.md` predates this conversation and is
the gate material for D11. It contains, already written:

- **§4 — two failure modes stated in client language**: *"Boolean gates buy
  nothing"* (every stage-gate/sign-off/RAG status that reduces a rich outcome
  to a yes/no is not neutral overhead — on this measure it is *silent*) and
  *"Frozen reads are blind reads"* (deciding against last quarter's dashboard
  is not "less good than live", it is *no better than not looking*). That is
  **what problems are we solving**, in the client's own idiom.
- **§6 — what not to sell past**: no calibration anchors (the business version
  has no equivalent of the elementary-rule scale, so a first engagement yields
  *relative* ordering only); selection unproven (the honest claim is
  **diagnostic, not evolutionary**); and — the line that anticipates Joe's
  2026-08-22 thesis exactly — **"n=1 is instrument construction. The first
  engagement is *building the instrument on a real subject*, and should be
  scoped and priced as that, not as a validated diagnostic."**
- **§8 — deliverable shape** *"for a month run-up, or a mini-sabbatical"*: four
  steps (exotype inventory → gain measurement → blind control → xenotype map),
  with the acceptance bar *steps 1–3 on one real subject, blind control run and
  reported even where it embarrasses the instrument.*
- **§5, §9, §10** — why clients before Hyperreal; strategic position; open
  questions for Joe.

**Two things to check before leaning on it**, both dated 2026-07-30 and
possibly moved: it records invoice 202506 as **41.25 hours still owed across
six VSAT lines** against an engagement dated **2026-08-31** — nine days out
from today — and it treats August as having three partial claimants. The
document also warns, in §6's register, that *"overclaiming would be the fastest
way to lose a technically careful collaborator"* — worth carrying into the
September 11 meeting.



**Endpoint note, corrected.** `GET /api/alpha/evidence?author=X` **does**
honour the filter (verified for `joe` and `claude-13`); only
`/evidence/sessions?author=` ignores it. An earlier draft risked generalising
the miss to both — tested instead. The mark-vocab probe's 6,086-turn pull used
`/evidence` and was correctly filtered, so its clustering result stands.

## 3. The paper as projection

`p4ng/futon-2026.tex` is the working draft, in the role `plop-2026.tex` played
for the War Machine: **a draft paper that shapes the patterns and is revised as
they are.** The R-series patterns were written, implemented against, and
rewritten to describe what got built; plop-2026 was that instrument until it
shipped.

The division of labour:

- **plop-2026 holds the R patterns.** They are shipped and need no
  replication here.
- **futon-2026 holds the WR patterns**, which are the ones that need work. 28
  rulings, 3 non-invertible, 9 in the spine, and prose written for none of
  them.
- The paper carries only what a reader needs; the mission carries the
  measurements, the corrections and the gap ledger.

**What the paper is currently missing** is exactly the WR pattern work: the
spine section lists nine patterns in a comment block and writes up none. That
is the next increment, and it is the same increment for both artefacts — a WR
pattern revised until it describes what is actually there is simultaneously
mission progress and paper content.
