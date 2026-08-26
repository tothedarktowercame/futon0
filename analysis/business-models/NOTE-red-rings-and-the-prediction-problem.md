# Do the red rings transfer to the prediction problem?

**Date:** 2026-08-26 · claude-13, from Joe: *"whether any of our 'red ring'
issues could help with the prediction problem (if they were adapted to other
observables, not just stack components). Maybe that's reaching too far though."*

Follow-on to `NOTE-red-cells-as-consulting-targets.md`. Short answer: **not
reaching too far for one ring, and the fit is specific rather than analogical.**

## R8 is the ring, and Docker is its business instance

`p4ng/empirics-futon/wr-overlay.edn`, R8 under WR-27:

> *a loop is born instrumented for its gain — the per-tick mismatch IS the gain
> reading, and the daily cadence that would read it stopped 2026-07-14.*

Docker priced something already adopted **three times**: Hub pull limits
(Nov 2020), Desktop subscription (Aug 2021), Free Team sunset (Mar 2023). The
corpus record: *"each time … the recorded responses were mitigation and
substitution rather than purchase."* Three runs of one loop, one result.

Where did the gain reading come from? **From outside, every time.** Apache Flink
published its position the day after Lightbend's announcement; Redis's named
providers announced a fork within nine days; Docker withdrew the 2023 offer
within ten. Those are environmental readings — the market told them — not
instrument readings taken by the party making the move.

That is R8 exactly, in another domain: **a loop that was not born instrumented
for its gain, so its gain arrived as an external shock.** The ring's discipline
transfers unchanged; only the observable swaps — from per-tick mismatch to
**time-to-substitution**, for which the corpus already supplies three calibration
points: **1 day, 9 days, 10 days.**

So the supplier-facing offer in the companion note ("foresight, not persuasion")
has a name in the stack's own vocabulary: *apply R8 to a pricing move* —
instrument it for its gain before making it, and name the substitution path as
the observable.

## R5 sits behind the other offer, less exactly

R5 under WR-25: *good news gets the same discipline as bad — `:warm-customer-pays`
satisfied, uncounted, unsurfaced.*

Linden Lab is an outcome that occurred and was not read: buyers paid, and *"no
criterion existed against which the observation could be scored"* — Stern's
1,200 visitors with *"no very precise expectations"*. R5's defect is the same
family (a result that happened and went uncounted) but not the same instance:
R5 is specifically about favourable outcomes going unsurfaced, where Linden Lab
had no scoring rule for any outcome. Related, and worth stating as a family
rather than a match.

## The sequencing hypothesis: supported at n=1, not n=3

Joe: *"maybe the costly choices were based on bad sequencing of parallel
streams."*

Docker supports it. Three moves in sequence, each made after the previous had
already produced substitution, with no visible incorporation of that reading.
That is a `seq` whose second and third steps should have been gated on the first
step's gain.

**But Lightbend and Redis were each a single move**, not a mis-sequenced series.
The n=3 pattern in the red set is *price-on-adoption → substitution*, which is
about the move itself. Sequencing is n=1. Worth keeping separate so the
hypothesis is not read as better supported than it is.

## "BV by hand" is exact — and BV is already built, 0-sorry

Joe, noticing himself sequencing several work streams mid-turn: *"I'm basically
doing BV by hand."*

BV — Guglielmi's calculus of structures — is the system whose non-commutative
`seq` sits beside commutative `par`: the algebra of *this before that* against
*these together*.

**Correction (Joe, 2026-08-26).** An earlier draft of this note said the stack
had "recorded its absence as a typed hole". That was an as-of finding about
**mathlib**, not about this stack: `M-typed-holes-mathlib-audit.edn` ran
`rg BV/…` under `Mathlib/Logic`, `Mathlib/CategoryTheory`, `Mathlib/Order`. The
stack then built it. `mathlib4/DarkTower/BV.lean` is **147 lines with zero
`sorry`**:

    inductive BV      -- atoms, self-dual unit, seq / copar / par
    inductive Cong
    inductive Step
    theorem seq_assoc_cong · par_comm_cong · atom_medial · seq_assoc_step

and its header reads `seq S T` as *"sequential fill or a path through typed
holes"*. So the operator's hand-sequencing has a machine-checked counterpart,
with the medial rule proven — not a gap.

## The F4 constraint, which surprised its own author

`M-diagramprover/capability-proof-futon.edn` F4 is `:constrained-by [:WR-27]`,
committed by Joe on 2026-08-14 and a surprise to him on 2026-08-26. It is not
incidental. F4 claims *"a prover whose logic ladder grows **by demand**"* —
a consumption-to-acquisition loop — and WR-27 says:

> *Ship every new consumption-to-acquisition loop with an instrument that
> measures how much returned demand changes what the loop does next.*

F4's certificate, *"five typed refusals from F3's claims demanded D2"*, is the
first reading of exactly that instrument: five demand signals, one acquisition.

Two things follow. First, WR-27 carries `@holds-open R8 R14`, so those rings are
red **by the ruling's design** rather than by neglect. Second, WR-27's own
example is the same shape as the business cases: *"a judge/formalizer
disagreement log that never alters the formalization pipeline is the
uninstrumented-loop smell."*

### And that makes the Docker transfer computable

WR-27 prescribes a specific metric: **the fraction of returned signals that
change subsequent acquisition or action.** Docker made three price-on-adoption
moves (2020, 2021, 2023). Substitution was returned after the first and after
the second. Both subsequent moves proceeded on the same shape.

    returned signals that changed the next action:  0 of 2

That is not an analogy to R8 — it is R8's prescribed instrument, computed on a
business case from the corpus's own record. The three cases supply the
calibration for the *observable* (1, 9, 10 days to substitution); Docker alone
supplies a reading of the *gain*.

## What this does not license

Nothing here predicts an outcome. R8 supplies a *discipline* (instrument before
moving) and one observable with three calibration points; it does not supply a
model. Part III's own preregistration calls predictive separation underpowered
at thirty-two cases, and three instances of one refusal shape is a pattern worth
naming, not a result.

## Related

- `NOTE-red-cells-as-consulting-targets.md` — the five red cells and the two offers.
- `futon3c/holes/excursions/README-red-ring-chain.md` — how a ring reddens and what derives from it.
- `p4ng/empirics-futon/wr-overlay.edn` — the rings themselves; R2, R5, R6, R8, R14 currently red.
