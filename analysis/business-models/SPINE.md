# SPINE — capability to valuation, and how FUTON eats its tail

**Date:** 2026-08-20 · **Status:** architecture, not yet built.
**Author:** claude-13, from a working session with Joe, 2026-08-20.
**Companion:** `SCHEMA.md` (the 30-case business-model dataset and its findings).

This organises the session's results into one structure. It is not a plan to
build a business; it is the arrangement that lets FUTON *find out* whether it
has one, using material it already produced for other reasons.

---

## The core relation

**A Mission is a capability delta.** It carries a completion criterion, and
completing it changes what is possible. So the ~200-mission corpus is already a
capability ledger — it has only ever been read as a record of work done.

**A serendipity Bridge connects a capability delta to an unattached need.**
Per Corneli, Jordanous, Guckelsberger, Pease & Colton (arXiv:1411.0440), a
bridge cannot be forced or predicted; only *serendipity potential* can be
raised architecturally. A bridge has a **pre** and a **post** and is invisible
from the pre side — so the target is never "predict the bridge", it is
**enumerate the banks and build the occasions.**

The organising question, in Joe's words: *what can you do now that you couldn't
do before* — and *what would that unlock for something shaped like X.*

---

## The seven layers

| # | Layer | Owner | Status |
|---|---|---|---|
| 1 | Capability ledger | FUTON | **mechanism proven, extraction hollow** |
| 2 | Warrant | FUTON | unbuilt, cheap |
| 3 | Need shapes | FUTON | **raw material exists** |
| 4 | Occasions | FUTON | 3 recorded; the real lever |
| 5 | Bridge candidates | FUTON | unbuilt, cheap |
| 6 | **Valuation** | **the world** | **the hole — instrument only, never build** |
| 7 | Acceptance event | criterion | decides business vs instrument |

Layers 1-5 are things FUTON can build. **Layer 6 is the one thing it must not
build** — the moment the system values its own bridges it reverts to
`:fire-pattern`: 168 emissions, no substrate, held at prior. Layer 7 is a test
applied to layer 6's output, not a component.

---

### 1. Capability ledger — per mission, the delta

*What can be done now that could not be done before.* Answerable from the
mission text alone: no buyer, no hindsight, no speculation.

**What exists.** More than raw material. `futon0/holes/missions/M-capability-star-map.md`
(CLOSED 2026-06-10) already models "the entire mission landscape as a graph —
capabilities as nodes, in dependency order (foundational -> composite)", with a
"capability inventory grounded across all 7 thesis-regions" and the EFE
mechanism demonstrated.

**Why it is empty anyway**, in its own words: *"extraction hollow =
`:prototyping-forward`"*, *"Integration DEFERRED until REAL data assembled"*.
The apparatus is proven and unfilled. **So layer 1 is not a build — it is a
fill**, and the mission records exactly why it stalled.

**Rule R1: only closed missions have deltas.** An open mission has unlocked
nothing yet.

### 2. Warrant — the level-shift discipline

Capability must be stated at a level where it can meet a need in another
domain. That level shift is also where fantasy enters: *solved these PhD
problems* -> *AI-supported reasoning over complex problem spaces* -> *we can
reason about anything*, each step shedding grounding until the claim is
unfalsifiable and therefore worthless.

**Rule R2: state the capability at the highest level the warrant supports, and
name the warrant.** The warrant is usually the oracle.

Worked example, `M-apm-demonstration`. The warrant is the Lean kernel, and the
30-case dataset says the kernel is the load-bearing part, not the reasoning
(see `SCHEMA.md`, defect 4: artifact oracle vs sale oracle). So:

> **Not:** "AI-supported reasoning over complex problem spaces."
> **But:** "Sustained multi-agent closure of hard formal problems against a
> mechanical verifier, at PhD difficulty, with an audit trail."

Narrower, and far more useful — it names **three separable capabilities**
(sustained multi-agent closure; mechanical verification; audit trail), each
bridging to a different need shape. "Complex problem spaces" bridges to nothing
because it is one undifferentiated claim.

**Rule R6: a capability claim with no named falsifying observation is not a
claim.**

### 3. Need shapes — strawmen, not targets

The 30 encoded cases are **shapes of need**, not companies to imitate. Each is
a probe: *would this capability unlock anything for something Andela-shaped?
Docker-shaped? Kaggle-shaped? PlanetMath-shaped?*

**What exists.** `cases/batch-{A..F}.md` (30 cases, outcomes known for 15,
predictions frozen for 10) and `futon6/data/business-landscape-embed.html`
(106 records: 43 org, 19 vacancy of which 14 confirmed vacant with a named
`:vacancy/checked` census, 9 funder, 6 capital, 7 yardstick-gap, 7
scope-finding, 12 refuted).

**Open question deferred, not answered** (Joe, 2026-08-20): every vacancy
carries `:capital/present? false`, which is ambiguous — *opportunity* (nobody
there yet) or *verdict* (nobody there because no acceptance event can fire).
The landscape checked for absent capability, not for present demand. Resolving
that is the vacancy pass; explicitly not started.

### 4. Occasions — the only reliable lever

A bridge needs somewhere to happen. Joe's case: reconnecting with Rob at their
undergraduate advisor's 30-year celebration was straightforward; that they
would collaborate 25 years after meeting was the bridge. The celebration was
the **occasion** that put two unattached banks in the same room.

**What exists.** 3 `convening` records in the business landscape.

**This is the lever.** You cannot increase bridges. You can increase banks
(layers 1 and 3) and occasions (layer 4). Everything else is hope.

### 5. Bridge candidates — generate freely, value never

The cross product of layers 1 and 3. Cheap to produce and worthless on its own.

**Rule R3: the system may generate bridges; it may never value them.**
**Rule R4: report precision, not recall.** An LLM will produce infinite
plausible bridges; a count of connections found is a coincidence machine.
The only number that means anything is proposed-to-accepted, and acceptance
comes from outside.

### 6. Valuation — the hole

`aif/no-self-certification`: a verdict moves only on evidence its maker did not
manufacture. In the six-phase model, phases 1-5 are all internal and
manufacturable; **phase 6 is the only one a system cannot perform on itself
without destroying the result.**

**This is the same hole found three separate ways today:**

- **In the missions:** `Gate:` has effectively one value. Eight declarations
  across the futon0/futon3/futon3c/futon4 corpora, all `operator-*`, one
  generic `human`, **zero naming an external party.** The object model cannot
  express an audience other than Joe.
- **In the War Machine:** `futon2/scripts/wm_outer_loop.clj` rewards
  *follow-through* — "a new `M-*.md` file appears in git". Credit for opening a
  mission is incremented by writing the file. It is a compliance meter, and it
  has been dead since 2026-07-14 (`ERROR nil`).
- **In the serendipity model:** the missing Valuation phase.

They are one missing field. **The missing business model and the missing
calibration term are the same hole.**

### 7. Acceptance event — the criterion

From the 30 cases (`SCHEMA.md`): the discriminator is not distance-in-hops but
**whether a discrete, per-unit acceptance event moves money.**

- Andela: fires per engineer, per placement -> **scaled**
- Minecraft: fires per copy, from before full release -> **scaled**
- Docker: adoption enormous, no acceptance event until the licence move -> **sideways**
- AppJet: fired on the *byproduct* (Etherpad), not the product -> **sideways**
- PlanetMath: never fires -> **zombie**

**Rule R5: per-unit acceptance, or it is a calibration instrument rather than a
business.** Both are worth having; say which one you are buying.

---

## The circuit

```
mission -> capability delta -> warranted claim -> bridge candidate
   ^                                                     |
   |                                                     v
mission selection <- exogenous alpha <- ACCEPTANCE <- valuation (external)
```

Accepted bridges are exogenous alpha. Exogenous alpha is the missing term in
the War Machine's outer loop. The outer loop selects the next mission. The
mission produces the next capability delta.

**Every segment of that circuit is a thing that exists**, except the return
path — which is precisely layer 6, and is exactly one field.

Note the estimator constraint carried over from `SCHEMA.md`: the outer loop is
*emission-driven* (beta increments per unanswered emission), which crushed
`:address-sorry` to 0.009 by measuring the lab's own emission rate. External
events arrive monthly at best. **An exogenous class must be event-driven** —
alpha increments on the rare external event; beta does not tick per unanswered
emission.

---

## Starting set — twelve missions, not two hundred

A ledger of 200 vague entries is worth less than one of 12 defensible ones, and
R2 (the level shift with a named warrant) is what takes the time. Chosen for
closure and for spread across capability kinds:

| Mission | Repo | Kind |
|---|---|---|
| M-apm-demonstration | futon3c | proving against a mechanical oracle |
| M-capability-star-map | futon0 | capability graphing (and the layer-1 apparatus itself) |
| M-typed-holes | futon3c | typed gap representation |
| M-alfworld-pattern-discovery | futon3c | pattern extraction from runs |
| M-interest-network-coupling | futon4 | landscape/interest graphing |
| M-war-machine-first-outing | futon3c | autonomous selection loop |
| M-codex-agent-behaviour | futon3c | cross-vendor agent characterisation |
| M-mission-control | futon3c | dispatch and orchestration |
| M-self-representing-stack | futon4 | self-representation |
| M-transport-adapters | futon3c | substrate portability |
| M-pilot-appearance | futon3c | operator-ratified lifecycle |
| M-agency-unified-routing | futon3 | routing / the exchange substrate |

---

## What to do next, in order

1. **Fill layer 1** for these twelve — delta + warrant + level, per R1/R2/R6.
   This is the fill `M-capability-star-map` deferred, at a scale that can be
   checked by hand.
2. **Layer 5 cross-product** against the 30 need shapes. Generate freely; value
   nothing.
3. **Layer 4** — ask what occasions exist or could be made. This is the only
   lever, and it is the cheapest.
4. **Layer 6** — do not build. Widen `Gate:` to admit an external party, and
   let a real occasion supply the value.

Deferred by decision, not oversight: the vacancy pass (layer 3), and
normalising the case records to schema v2 (blocked on the freeze — see
`SCHEMA.md`).

---

# CORRECTION (2026-08-20, same session) — layer 6 is not empty

Selecting the twelve missions by capability star, as Joe directed, meant
reading `futon0/holes/missions/M-capability-star-map.graph.edn` (37 stars: 23
`:satisfied`, 13 `:held`, 1 `:active`). That falsifies two claims made above.

## 1. FUTON *can* express an external audience

Above I wrote that the object model cannot name an audience other than Joe. The
**mission** corpus cannot (8 `Gate:` declarations, all `operator-*`). The **star
map** can, and does:

- `:minted-by` entries with an `external/` prefix — `external/rob-joe-codrafting`,
  `external/rob-proof-graph-kb`, `external/vsat-poc-customer`. Stars minted by
  parties who are not us.
- `:operator-disposition` on `:cold-eoi-sent` records
  `:gated-by :consent-gate`, `:mission "M-eoi-outbox-management"`, and the
  reason: *"Send-side ... witnessed by operator review+send (the consent gate /
  Rembrandt-Hancock), not a FUTON deliverable (**author != send**)."*

`author != send` is the separation-of-powers discipline applied to external
contact, designed and dated `:since "2026-06-09"`. Layer 6 has representation.

## 2. The exogenous acceptance ladder EXISTS, and it is NOT at zero

Four pre-registered frontier stars form exactly the ladder this session
reconstructed from scratch — and every one has moved:

| Rung | `:title` says | `:position` says |
|---|---|---|
| authored into outbox | — | **n=2** (andrew-hyatt, james-henderson), 2026-08-15 |
| cold EOI sent | "the crux, **n=0**" | **n=2**; hyatt sent 2026-07-05 22:02 BST, **message-id witnessed** |
| draws a response | "n_outreach remains **zero**" | **n=1** — henderson call held 2026-08-05, debrief same day |
| converts to paid | "n_conversion remains **zero**" | **n=1 verbal, £5,000 of March work, unsigned; operator-attested 2026-08-15** |

**Every title says zero. Every position says non-zero.** The exogenous alpha
this whole session went looking for already exists at n=1 through n=2 on all
four rungs, recorded with dates and a witnessed message-id, five days before
the session began.

Read against the 30-case dataset: a verbal, unsigned £5,000 engagement is a
**per-unit acceptance event** (rule R5) performed by an external party (layer
6) — the thing 30 companies were encoded to characterise. `:next-rung` for that
star is *"a countersigned MoU"*, which is the honest next observation.

### Why nobody could see it

The titles are frozen pre-registration text; `:position` is the live field.
That is a *coherent* design — pre-registered claims should not be edited (the
same freeze discipline this dataset applies to batches E and F). But:

**`futon6/scripts/starmap_to_capability_graph.bb` does not project
`:position` at all.** It emits `:title :status :claimed :minted_by :scope
:frontier`. So the capability-graph JSON that the WM and the EFE field steer by
carries the stale titles and **none of the live counts**. The machine cannot
see that these rungs moved.

Same failure shape as `state: done` over `execution.executed: false`: a stale
label covering live data, where every consumer reads the label.

**Smallest useful fix in this whole document:** add `:position` and
`:next-rung` to the projection. One line in a `.bb` script; it is the
difference between a machine that can see its own only external traction and
one that cannot.

## 3. What this does to the spine

Layers 6 and 7 are **not holes — they are unprojected**. The corrected reading:

- layer 1 — apparatus proven, extraction hollow (*fill*)
- layers 2, 5 — unbuilt, cheap
- layer 3 — raw material exists
- layer 4 — occasions, the lever, 3 recorded
- **layer 6 — represented, populated at n=1..2, not projected**
- **layer 7 — satisfied once, verbally, unsigned**

Joe's golem reading was right and more literal than intended: the apparatus is
built and inert, and the sigil is a field name in a projection script.

---

# The twelve, selected by capability star (Joe's criterion)

**Minters of satisfied stars — capability demonstrated:**

| Mission | Star(s) |
|---|---|
| M-agency-unified-routing | `:agency` — multi-agent dispatch, N:1 registry/routing |
| M-distributed-proofreaders | 3 stars (highest single-mission yield) |
| M-interest-network-coupling | interest-network coupling |
| M-war-machine-first-outing | WM first outing |
| M-self-representing-stack | self-representation |
| M-apm-solutions | APM solutions |
| M-superpod-mark2 | superpod |
| M-capability-star-map | the layer-1 apparatus itself |

**Attached to held / frontier stars — delta pending:**

| Mission | Star(s) |
|---|---|
| **M-eoi-outbox-management** | the entire cold ladder; owns the consent gate | 
| M-futonzero-prelim-practice | toward `:ai-passes-prelims` |
| M-webarxana | corpus/reading surface |
| M-essay-corpus-substrate | corpus substrate |

`M-eoi-outbox-management` (`futon5a/holes/missions/M-eoi-outbox-management.md`)
is the one to read first. It owns the only part of FUTON that has ever touched a
paying stranger.
