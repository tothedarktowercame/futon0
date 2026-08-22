# Business-model case dataset — schema v1

**Date:** 2026-08-20 · **Status:** schema under test; no cases encoded yet.
**Purpose:** validate an object model for "who pays for this kind of thing"
against real companies with **known outcomes**, before applying it to FUTON.

Derived from a claude-13 / Joe working session, 2026-08-20. The upstream
motivation is the exogenous-calibration gap in the War Machine outer loop
(`futon2/scripts/wm_outer_loop.clj`): its reward signal is follow-through on
lab-internal events, so it cannot be moved by evidence the lab did not
manufacture. See `futon0/README-costs.md` for the cost-side counterpart.

## Two tests, stated BEFORE the data exists

1. **Schema adequacy (achievable).** Can every case be encoded without a
   residual "and also X" that fits no slot? Needing a *new node type* is a
   result, not a failure. Needing several means the model is too weak.
2. **Predictive separation (underpowered at N=20).** Do `oracle-hardness` x
   `distance` x `terminal` sort the outcomes? At this N it is a sniff test,
   not evidence. Do not report it as evidence.

## Encoding discipline

- **`knowable-at-T` fields are filled BLIND to the outcome**, from what a
  scan could plausibly have seen at T = founding + ~1 year. Attach the
  outcome afterwards. Hindsight encoding teaches nothing.
- Every claim is either **cited** (public, linkable source) or explicitly
  marked **`[recollection — unverified]`**. Never silently assert.
- Inherits the status discipline of
  `futon5a/interest-scanning/company-candidates-from-network-v1.md`:
  concrete named orgs, hypotheses labelled as hypotheses.

## Node types

The outside world emits mechanically-assertable structure in (at least) six
shapes. Rows 1-5 share a direction: **the world hands you a specification and
you satisfy it.** Row 6 reverses it.

| # | Node type | Fixture (from outside) | Oracle | Distance to money | Business model |
|---|---|---|---|---|---|
| 1 | **Requirement** | tender, RFP, certification checklist | line-item satisfaction | medium | vendor / services |
| 2 | **Benchmark** | public scored task set + leaderboard | the score; you don't hold the scorer | far | capability licensing |
| 3 | **Bounty** | posted problem with a price on it | acceptance by the poster | **zero** | piecework |
| 4 | **Dependency** | someone else's lockfile / build / upstream | the dependency graph | medium | open-core, support |
| 5 | **Call** | funder eligibility + review criteria | peer review (*interpretive*) | short | grant-funded |
| 6 | **Environment** | users generating their own goals inside your apparatus | do they bring their own goals and return | direct, but needs a population | platform / subscription / licence |

**Row 6 (Environment)** was added at Joe's prompting, 2026-08-20: FUTON may be
more like Minecraft than like any of rows 1-5, because *Missions are inherently
valuable* — the asset is the apparatus that makes work legible and gradeable,
not the output of any one mission. Its precondition is real but narrower than first
stated: **it needs a population.** (An earlier draft of this section claimed it
also cannot be attempted without permission. Batch C refutes that — see the
correction below.)

## Record shape

```
case-id          slug
org              name
period           founding -> outcome date
node-types       primary + secondary, from the six above
chain            artifact -> sponsor -> payer -> terminal{obligation|interest|none}
oracle           what non-manufacturable check gated the sale
oracle-hardness  mechanical | interpretive | none
distance         hops from artifact to a payer's line item
fixture-access   could a newcomer attempt the test without permission? y/n
knowable-at-T    <<< filled BEFORE outcome is consulted >>>
outcome          scaled | acquired-sideways | acqui-hired | failed | zombie
                 | sustained-noncommercially | open
evidence-refs    public + citable, or marked [recollection - unverified]
```

## Why `terminal` matters

Follow the chain until it hits an **obligation** (someone is required to do
this, by regulation / audit / contract / certification) rather than an
**interest** ("they think it's neat"). Obligations recur, survive budget cuts,
have a named holder, and are publicly citable. Interests generate
conversations. A chain terminating in an interest is a **drop**.

Known bias: obligation-chasing over-selects mature, procurement-heavy markets.
Rows 2, 3, 4 and 6 exist partly to counteract that.

## Sample frame (20)

Chosen for spread across the six rows **and across outcomes, including
failures**. A winners-only set is explained perfectly by any schema and
predicts nothing.

- **A - Requirement / formal-methods (FUTON's nearest neighbours):** Andela,
  Galois, Trail of Bits, Runtime Verification, Kestrel Institute
- **B - Dependency / open-core:** Docker, Juxt (XTDB), Lightbend (Typesafe),
  Sourcegraph, Redis Labs
- **C - Environment / sandbox:** Mojang (Minecraft), Roblox, Linden Lab
  (Second Life), PlanetMath, GNU Emacs / FSF
- **D - Benchmark / Bounty / sideways:** Kaggle, HackerOne, Gitcoin,
  AppJet (-> Etherpad), Wolfram Research

**The two highest-information cases:**

- **Docker** — maximal success on the Dependency node, famously poor capture.
  If the schema cannot explain Docker, the schema is wrong.
- **PlanetMath** — an Environment-node case with a known outcome that Joe has
  primary sources for, and the same node type FUTON would be running.

`AppJet` and `Docker` together are two instances of *artifact widely used,
value realised elsewhere* — the failure mode the `distance` axis predicts.
`GNU Emacs` is the Environment-row extreme: maximal environment value, no
capture, sustained by a different terminal entirely.

---

## Findings from batch B — schema v2 deltas (2026-08-20)

**Do NOT apply these to in-flight batches.** Batches A, C, D are encoding
against v1; changing fields mid-flight would make the records inconsistent.
Normalise after all four land.

Batch B (`cases/batch-B-dependency.md`, commit `d90b9cc`) surfaced three
defects and one substantive result.

### Defect 1 — `outcome` is a single enum and cannot hold Docker

Docker is simultaneously `acquired-sideways` (the 2019 Mirantis sale of the
enterprise business, i.e. the death of the original capture vehicle) and
`scaled` (the company survived, recapitalised, and later built a subscription
business), while the artifact continued creating value outside the firm's
capture boundary entirely. **v2: split into `adoption-outcome` and
`capture-outcome`.** The whole point of including Docker was that adoption and
capture diverge; the schema must be able to say so in two fields.

### Defect 2 — `fixture-access` is not a boolean

Sourcegraph is **y** for the public test (a newcomer could search hundreds of
thousands of public repos without permission) and **n** for the *decisive*
test (the enterprise sale turns on a private corpus that requires customer
permission). **v2: `fixture-access` records the public/attemptable test and
the decisive test separately.** The gap between them is itself informative —
it is the distance between what you can practise on and what you are graded on.

### Defect 3 — `terminal` has no value for a self-manufactured obligation

See below; this is the substantive result, not just a missing enum value.

### Result — the open-core obligation is manufactured by the seller

Across batch B, **every terminal is `interest`**. Three of the five
nevertheless carry an obligation — and in all three it is **created by the
vendor's own licence change**, not imposed by any external party:

| Org | Obligation source |
|---|---|
| Docker | paid Desktop subscription required by Docker's own licence |
| Lightbend | production use requires a Lightbend licence under BSL |
| Redis | uses outside the source-available grants |

This cuts against the framing that seeded the schema ("follow the chain until
it terminates in an obligation someone else imposes"). The Dependency row has
a **third route**: hold a dependency position first, then manufacture the
obligation by relicensing. Docker's record shows the transition in time — its
2013-14 snapshot terminates in `interest`, and the licence obligation arrives
years after adoption.

Crucially this does **not** violate `aif/no-self-certification`, because the
step that makes it stick is not manufactured: **you cannot fake being in
someone else's lockfile.** The sequence is (1) earn a non-manufacturable
Dependency position with a hard oracle, then (2) manufacture the obligation.
Step 1 carries the evidential weight; step 2 is a pricing decision.

**v2: `terminal` takes `obligation-external` | `obligation-manufactured` |
`interest` | `none`**, and a manufactured obligation must record the date it
was created relative to the adoption it rests on.

### Correction to the seeding example

The XTDB -> JUXT -> fintech -> bitemporal-regulation chain was the worked
example this whole framework was built from. Batch B could not substantiate
it: **no citable named customer and no specific regulation, audit clause, or
contract** requiring anyone to buy XTDB. Its terminal is encoded `interest`.
The XTDB dev diary does record an origin in a Tier-1 bank project, which is
suggestive and is not an obligation. "SQL:2011 defines bitemporal
capabilities" is a technical standard, not a buyer obligation.

Treat the original example as a just-so story until someone produces the
named obligation.

## Findings from batch A — schema v2 deltas, continued (2026-08-20)

Batch A (`cases/batch-A-requirement.md`, commit `cece8e9`) — the formal-methods
cluster, FUTON's nearest neighbours. Encoding discipline was strict: it
repeatedly refuses to invent an obligation where sources do not support one
("this record does not invent one").

### Defect 4 — one `oracle` field is doing two different jobs

The artifact oracle and the sale oracle are **not the same check**, and the
records keep splitting them by hand:

- Galois: mathematical proof "may be mechanical inside the artifact, but award
  acceptance remains sponsor interpretation" -> `oracle-hardness: interpretive`
- Trail of Bits: "findings can have mechanical exploits or tests, while
  engagement completeness and risk acceptance remain judgments"
- JUXT/XTDB (batch B): "**Mechanical** for temporal database behavior;
  **interpretive** for the consulting purchase"

**v2: split into `artifact-oracle` (+hardness) and `sale-oracle` (+hardness).**

This matters more than the other three defects because it lands on FUTON
directly. The APM loop has a genuinely hard **artifact** oracle — the Lean
kernel, which agents cannot fake. It has **no sale oracle whatsoever**. Those
were being treated as one property ("a domain with a cheap non-manufacturable
checker"), and they are two. A hard artifact oracle is what makes the *machine*
work. It says nothing about whether anyone buys.

### Result — ten cases, zero external demand-side obligations

Across batches A and B, no case terminates in an external certification or
regulatory mandate. What was found instead falls in two kinds:

1. **Contractual delivery** (Galois: satisfy the funded work statement;
   Kestrel: deliver Phase I under NASA SBIR 80NSSC20C0497).
2. **Vendor-manufactured** (Docker, Lightbend, Redis — licence changes).

Neither is what the obligation heuristic promised. **A contractual-delivery
obligation is downstream of the sale, not upstream of it**: it explains why
money moves *after* someone decided to buy, not why they decided. The heuristic
was supposed to answer "why does demand reliably exist here", and a delivery
obligation cannot answer that — it is a consequence of the purchase, not a
cause.

Galois's record makes the distinction explicit and is worth quoting as the
correction to the original framing: the sources "do **not** establish that
Galois's general revenue terminates in a named product-certification mandate;
the named obligation here is contractual delivery, not DO-178C/Common
Criteria."

**Standing hypothesis, now under real pressure:** that one can find a business
by chasing external obligations. Ten cases, none found. Batch F (AI-adjacent)
is the next test — if its terminals are also all `interest`, the
external-obligation route should be treated as closed in this neighbourhood
until a counter-example turns up.

### Andela — candidate seventh node type

Encoded primary **Requirement**, secondary **Environment**, and flagged as the
weakest fit in the batch. Suggested refinement: a **Capability intermediary**
node for screened human capacity. Do not add the row on one case; hold it and
see whether anything in C-F needs it.

## Findings from batch D — the seventh row (2026-08-20)

Batch D (`cases/batch-D-benchmark-bounty.md`, commit `d446a48`).

### Row 7 — Market/Exchange

Three independent cases converged on the same missing node type, which is a
far stronger signal than Andela's single-case suggestion:

- **HackerOne** — "is not primarily a bounty hunter. It monetises the operation
  of many Bounty nodes."
- **Gitcoin** — needs it "for matching issue" sponsors and suppliers.
- **Kaggle** — fits Benchmark + Environment, but "similarly operates benchmarks
  rather than competing in them."

| # | Node type | Fixture | Oracle | Distance | Business model |
|---|---|---|---|---|---|
| 7 | **Market/Exchange** | outside principals posting priced acceptance conditions | the operator's matching/triage/settlement working | direct (fee per transaction) | marketplace take-rate |

Outside principals post priced acceptance conditions, outside suppliers submit,
and the operator earns for matching, workflow, trust, triage, and settlement.

### The pattern behind row 7 — operating a node beats travelling it

HackerOne does not win bounties; it runs the bounty market. Kaggle does not win
competitions; it runs the benchmark. This is the picks-and-shovels observation,
but reached structurally from the object model rather than as a slogan — and it
says the *same* node type supports two quite different businesses, one of which
is systematically better positioned than the other.

**Relation to row 6.** Environment and Market/Exchange are neighbours — both
supply a space that others act inside. They differ in what is monetised:
Environment charges for **access**, Market/Exchange takes a cut of
**transactions**.

### Result — fifteen cases, still zero external demand-side obligations

With batch D in, the tally across A + B + D is unchanged in kind:

- **`interest`** at the terminal: every case.
- **Contractual-delivery obligation** (downstream of the sale): Galois, Kestrel.
- **Vendor-manufactured obligation** (licence): Docker, Lightbend, Redis, and
  now **Wolfram** — "obligation to obtain/use the applicable licence class".

Four manufactured-obligation cases now. **The obligation heuristic is dead as a
search rule for external mandates**, on this evidence. It survives only in the
manufactured form, which is not a way to *find* a business — it is a pricing
move available once a position already exists.

### Encoding note — AppJet passed the hardest discipline test

AppJet's record encodes the original hosted-development business at T and marks
the Etherpad/Google chain explicitly "excluded from the T snapshot". That is
the case most exposed to hindsight contamination in the whole dataset, and the
separation held.

## Findings from batches E and F — the prospective set (2026-08-20)

`cases/batch-E-agent-native.md`, `cases/batch-F-ai-adjacent.md`. Both are
**pre-registrations**: `knowable-at-T` means T = 2026-08-20, and each record
carries a dated falsifier. All ten predictions carry an explicit falsifying
observation with a hard date (2028-12-31 / end 2028). **Frozen — resolve by
appending, never by editing.**

### THE HEADLINE — twenty cases, zero external demand-side obligations

Batch F's own cross-case note states it plainly:

> The terminal result is uniform: every commercial chain terminates in
> `interest`, not an externally imposed obligation. Compliance features
> shorten procurement and make products acceptable, but none of the cited
> material makes a named buyer legally or contractually obliged to buy one of
> these specific vendors.

Across all twenty cases in A, B, D, E, F, **not one terminates in an external
demand-side obligation.** The obligation heuristic — "follow the chain until it
hits an obligation, because obligations pay reliably" — is **refuted on this
evidence.** What exists instead:

- `interest` at the terminal, universally;
- **contractual-delivery** obligations, which are downstream of the sale;
- **vendor-manufactured** obligations (Docker, Lightbend, Redis, Wolfram,
  OpenHands Enterprise), which are a pricing move, not a way to find demand.

### Row 8 candidate — Assurance

Batch F: Braintrust and Humanloop are encoded Environment, but "their
distinctive output is **evidence about another artifact** rather than
completion of the user's goal itself." Proposed row — fixture: a system plus a
quality claim; oracle: repeatable evaluation/audit evidence; business model:
observability, certification, risk assurance.

**This is the row "proof-checking as a service" / "audit apparatus" would
occupy** — reached independently from the data rather than from the framing,
which makes the convergence meaningful. And it carries a warning: **the
Assurance cases terminate in `interest` like everything else.** The claim that
audit apparatus gets bought because someone is *obliged* to buy it fails on its
own nearest comparables.

Row 8 is a candidate, not adopted: two cases, and v1 is frozen for this round.

### VERSES — the manufacture-without-position case

Batch E's VERSES prediction is the schema making a real dated claim derived
from batch B's finding:

> A granted patent can manufacture an obligation only after a
> non-manufacturable adoption position exists; with no evidenced adoption at T,
> the schema predicts that this patent alone will not produce material capture
> by 2028.

"No reviewed source establishes material third-party adoption or
patent-licensing revenue as of T." VERSES holds the IP without the position —
the exact inverse of Docker/Redis, who held the position first and priced
second. Falsifier: a named arms-length licensee, material patent-linked
licensing revenue, or a final enforceable judgment tied to US 12,393,581, by
2028-12-31.

### Defect 3 confirmed independently

Both OpenHands and VERSES records say in their own words that **"v1 cannot
distinguish this manufactured licence obligation from an external regulatory or
contractual obligation."** The v2 `terminal` split
(`obligation-external` | `obligation-manufactured`) is confirmed necessary by
encoders who were not shown the v2 deltas.

## The systematic blind spot — every row is written from the supplier's seat

Batch E's bellback (commit `393caf8`) recorded two further defects, and
together with row 7 they turn out to be one defect seen three times.

### Defect 5 — one entity may need two linked records

OpenHands "needs linked OSS-project/company records to separate adoption from
capture." This is defect 1 (Docker's single `outcome`) escalated: the fix is
not only splitting a *field* but sometimes splitting the *record*, into an
artifact record and a capture-vehicle record with a link between them.
`AppJet -> Etherpad` and `LangChain -> LangSmith` are the same shape.

### Defect 6 — the Benchmark row cannot name the benchmark's own payer

SWE-bench "fits Benchmark but exposes that the row does not naturally represent
the benchmark producer's maintenance payer." The row describes what a
*competitor* on the benchmark faces. It has no slot for who keeps the fixture
alive.

### The unification

Rows 1-6 are all written from the seat of someone **travelling** the node —
satisfying the requirement, scoring on the benchmark, claiming the bounty,
being the dependency. **None of them describes operating the node.** That is
why the gap surfaced three separate times before being named:

| Symptom | Case | Missing seat |
|---|---|---|
| row 7 Market/Exchange | HackerOne, Gitcoin | operator of Bounty nodes |
| Kaggle "operates benchmarks rather than competing in them" | Kaggle | operator of Benchmark nodes |
| defect 6 | SWE-bench | operator of a Benchmark node |

**v2: `seat` becomes an explicit field — `travelling` | `operating` —
orthogonal to node type**, rather than bolting on one operator-flavoured row.
Row 7 was the right observation and probably the wrong fix: Market/Exchange is
what `operating` looks like on the Bounty row specifically, and Kaggle and
SWE-bench show it recurring on Benchmark.

This matters for FUTON because batch D's pattern — **operating a node beats
travelling it** — is the one durable positive finding in a dataset whose main
result is negative, and the schema was structurally incapable of expressing it.

## Batch C and the completed dataset — 30 cases (2026-08-20)

`cases/batch-C-environment.md` (re-run locally; the oxf-codex-1 copy committed
`3e8429f` on the Oxford filesystem and never existed here).

### Final tally — 30 of 30 terminate in `interest`

Batch C's five Environment cases terminate in `interest` like every other row.
**Across the entire dataset there is not one external demand-side obligation.**
That result is now as strong as this method can make it.

### The one place Test 2 shows separation — and it is within a row

The Environment row has clean outcome spread, and it sorts on a single field:

| Case | `distance` | outcome |
|---|---|---|
| Minecraft | **Direct / 1 hop** | scaled |
| Roblox | **Direct / 1 hop** | scaled |
| Second Life | **Direct / 1 hop** | open (~20 years) |
| GNU Emacs / FSF | 2 hops | sustained-noncommercially |
| PlanetMath | **Far / >=2 hops**, "no direct user-payment line" | **zombie** |

The three with a direct payment line survived commercially; the two without did
not. This is **within-row**, so node type is held constant — the first thing in
the dataset that looks like a real discriminator rather than a description.

**Caveats, and they are not small:** N=5, one row, outcome categories are
hand-assigned by the encoder, and the direction of causation is not established
(a product with paying users can afford to keep existing, which is not the same
as payment causing survival). Treat as a sniff test, per Test 2's stated
limits.

**Mechanism worth noting:** Minecraft's record says alpha sales let Persson
leave his job — the payment line arrived *with* the first population, not after
it. There was no unmonetised growth phase to convert later.

### The uncomfortable case

**PlanetMath is the closest analogue to FUTON in the dataset** — an Environment
in mathematics, contributors bringing their own goals, interpretive oracle,
`terminal{interest}`, "no regulation, accreditation, contract, or named
recurring grant obligation", far distance, "no direct user-payment line and no
permanent paid staff" — and it is the `zombie`.

GNU Emacs is the same shape with a non-commercial terminal, and survives on it.

If FUTON is Minecraft-shaped, the row says the discriminator is **not the
quality of the environment.** Emacs and PlanetMath are both superb
environments. It is whether a direct payment line exists, and whether it
arrives with the population rather than after it.

## Open conflict — normalisation versus the freeze

Checklist item (8) was "normalise A-F to schema v2". **This cannot be done
in place for E and F**, whose records are pre-registered and frozen; editing
them would destroy the only thing that makes a pre-registration worth having.

Resolution required before any normalisation pass:
- A-D (retrospective): may be normalised in place.
- E-F (prospective): v2 must be a **derived view or an appended block**, never
  an edit.

Not started. Flagged rather than done.

## Correction — population and permission are not the same requirement

Batch C (commit `e868745`) corrects a claim made in this document's row-6
description, which was mine and not an encoder's:

> Environment tests require a population, but **not necessarily permission** —
> Minecraft, public Roblox, PlanetMath, and GNU Emacs permitted self-serve
> participation.

Four of five Environment cases were self-serve. The original claim — that
Environment is the one row whose test cannot be attempted without permission,
"the opposite of the Playwright property" — conflated two separate things.

**This raises the row's standing rather than lowering it.** On the batch-C
evidence the Environment row has:

- an **attemptable fixture** (open it; see whether anyone comes) — the
  Playwright property after all, for four of five cases;
- a **hard oracle** — whether people bring their own goals and return is
  mechanically observable and cannot be manufactured;
- a **slow** clock, and a population that can be attracted but not fabricated.

So the constraint is *not manufacturable and slow*, which is a different and
much better problem than *permission-gated*. Combined with the distance
finding, the Environment row reads: cheap to attempt, honest oracle, and the
thing that decides the outcome is whether a direct payment line exists.

Two further notes from the same batch: Second Life's record separates
resident-economy value from unverified Linden Lab capture (the adoption/capture
split again, defect 1); GNU Emacs "terminates in nonprofit stewardship rather
than commercial payment", which is a terminal kind the v1 enum does not have.

## Andela / AppJet — the acceptance event (2026-08-20, Joe's pairing)

Joe's reading: Andela in its earliest phase is *sponsored development of
talent*; AppJet is *hosted development of apps*. Both invest ahead of demand in
a capability, then try to capture its value downstream. The encoded fields make
the contrast sharp, and it **inverts** the framing this document started with.

| | Andela | AppJet |
|---|---|---|
| `fixture-access` | **n** — cannot run the placement test without Andela and a client granting access | **y** — self-serve hosted platform |
| `distance` | **1 hop** to the client's staffing line item | aggregate / hoped-for |
| `oracle` | "the client company's decision to engage and retain the engineer" | "whether independent developers ... returned or paid for hosting" |
| `oracle-hardness` | interpretive | interpretive |
| `terminal` | interest | interest |
| `outcome` | **scaled** | **acquired-sideways** |

**Andela's fixture was permission-gated and it scaled. AppJet's was self-serve
and it did not.** That is the opposite of what the Playwright-property framing
predicts — cheap attemptable fixtures were supposed to be the advantage.

### What actually differs is the granularity of the acceptance event

Andela's oracle fires **per engineer, per placement**: one named client says
yes to one person, and money moves at that instant. AppJet's oracle only fires
**in aggregate** — "do developers return and pay for hosting" is a statistic,
not an event. There is no moment you can point at and say *someone accepted*.

So the Capability-intermediary shape works when capability is sold in **units
carrying an individual acceptance event**, and fails when it is sold as an
environment where adoption must accumulate before revenue appears.

### This unifies with the Environment-row finding

Minecraft's payment line also fires per unit at the moment of acceptance (buy
the game); PlanetMath's never fires at all. Across both rows the discriminator
is not distance-in-hops as such — it is **whether a discrete, per-unit
acceptance event moves money.** `distance` is a proxy for that; the acceptance
event is the thing.

**v2: add `acceptance-event` — per-unit | aggregate | none**, and treat it as
the primary predictive field rather than `distance`.

### And it closes the loop back to the calibration gap

The Andela shape, stated for FUTON, is: sell **accepted units of capability**,
each with a named external party who accepts it and pays at that moment. Not a
subscription to the machine, not an environment — one mission, one acceptance,
one payment.

That is exactly the Mission object with its `Gate:` field pointed outward. The
first finding of this whole investigation was that `Gate:` has effectively one
value (8 declarations across four mission corpora, all `operator-*`), which is
why the War Machine's outer loop can only reward operator follow-through.
**The missing business model and the missing calibration term are the same
missing field.**

### Sequencing — travel first, operate second

Batch D found that operating a node beats travelling it. Andela is not a
counter-example; it is the sequence. IFC describes its shift "from a
campus/apprenticeship model to a **global match-as-a-service** model" — it
travelled the node (train and place engineers) to build a position, then became
the operator of the matching market.

Same shape as the open-core finding: earn a non-manufacturable position by
travelling, then convert. Two independent routes to one sequencing rule.

## Reproduction run — what the checker confirmed, and what it broke (2026-08-21)

`check.bb` + `conjectures.edn` over `records/*.edn` (30 records, extracted from
the prose corpus without re-research). First machine-checked run of the
session's hand-derived findings.

**Arithmetic correction, mine: the corpus is 30 cases, not 25.** Six batches of
five. I carried a wrong total through `SCHEMA.md`, `SPINE.md` and
`RECORDS-SPEC.md`, and the checker reported it back as "expected corpus: 25 …
missing: 0" against 30 loaded. Corrected everywhere.

### Reproduced from the records alone

| conjecture | verdict |
|---|---|
| `all-terminals-interest` | **holds, n=30** |
| `no-external-obligation` | **holds, n=30** |
| `acceptance-predicts-survival` | holds, n=9 |
| `market-exchange-convergence` | holds, n=3 |
| `operating-seat-rare` | holds, n=4 |
| `every-unverified-has-discharge` (lint) | holds, **n=68** |
| `frozen-predictions-unedited` (lint) | holds, **n=10** |

The headline result survives mechanisation: **30 of 30 terminate in
`:interest`, zero external demand-side obligations**, now decided by
enumeration rather than by grep.

### Checker defect found and fixed in review

`holds?` was a boolean, which cannot distinguish **refuted by a
counterexample** from **undecidable because some records are `:unknown`**.
`sale-oracle-never-mechanical` came back `false` when the truth is 29
`:interpretive` and one `:unknown` (gitcoin) — nothing refutes it; it simply
cannot be decided. Conflating those manufactures false findings in both
directions.

Patched to a three-valued `:verdict` — `:holds | :refuted | :undecided`. Under
it the run separates cleanly: 6 hold, 2 undecided, 1 refuted.

### One hand-derived finding REFUTED

`no-acceptance-implies-zombie` — **counterexample: Redis**, `:acceptance-event
:none` with `:capture-outcome :scaled`.

Diagnosis: this is extraction reading `:acceptance-event` off the **artifact**
rather than the **capture vehicle**. All four open-core cases (Docker, JUXT,
Lightbend, Redis) came back `:none`, because the free OSS artifact has no
payment event — while the enterprise subscription plainly does.

**So defect 1 recurs for a third time.** `:outcome` needed the adoption/capture
split; `:oracle` needed the artifact/sale split; **`:acceptance-event` needs it
too.** Redis is the case that exposed it. The finding is not refuted in
substance; the field was measuring the wrong object.

### One hand-derived finding TOO STRONG

`manufactured-needs-position` — "every manufactured obligation rests on a prior
adoption position" — is **refuted by VERSES**, whose own `:rests-on` records
*"No evidenced adoption position at T."*

That is exactly what batch E predicted in its frozen 2028 falsifier. The
universal was overstated; the surviving claim is narrower and testable:
**manufactured obligations that PRODUCE CAPTURE rest on a prior
non-manufacturable position**, and VERSES is the dated experiment. Restated in
`conjectures.edn` with that note.
