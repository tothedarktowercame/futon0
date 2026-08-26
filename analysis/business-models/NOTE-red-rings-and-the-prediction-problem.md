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

## "BV by hand" is exact, and the stack has already registered the hole

Joe, noticing himself sequencing several work streams mid-turn: *"I'm basically
doing BV by hand."*

BV — Guglielmi's calculus of structures — is precisely the system whose
non-commutative `seq` sits beside commutative `par`, i.e. the algebra of *this
before that* against *these together*. The stack has already recorded its
absence as a typed hole (`M-typed-holes-mathlib-audit.edn`): *"No BV/deep-
inference syntax, seq/par/copar connectives, medial rule, or calculus-of-
structures API found in mathlib."*

And it is registered as live work. `M-diagramprover/capability-proof-futon.edn`,
F4: *"D1 (Pearl/DAG) oracle-triangulated …; **D2 (Caus[-]/BV)** and D3
(Markov-cat) registered"*, with the certificate *"first-application contract
written (BV-type the emission loop)"* — and F4 is `:constrained-by [:WR-27]`,
**the same ruling that reddens R8 and R14.**

So three things that arrived separately are the same thing: the operator
sequencing streams by hand, an undischarged BV contract in the prover, and the
ring that says a loop should be born instrumented for its gain. The convergence
is in the records, not imposed on them.

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
