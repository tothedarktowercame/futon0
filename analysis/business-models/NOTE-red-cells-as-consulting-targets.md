# The red cells as consulting targets

**Date:** 2026-08-26 · claude-13, from Joe's reading: *"the 'red' boxes might be
the most interesting ones from a consulting standpoint. With 'green' we can ask:
could we help you get there faster. But with 'red' we can ask: can we help turn
those green?"*

Companion to `NOTE-consultancy-shape.md`, which is about the *unit* (mission vs
hour). This is about *targeting*.

## What red is, exactly

In `lights.bb`, red is the single state `:declined-on-merit` — an offer existed
and the demand side turned it down on the merits. Amber is two softer states
(`:interest-no-offer`, `:declined-on-capacity`); green is `:engaged`.

Of 31 demand records carrying a response class: **17 green, 5 red**, the rest
amber/muted/unrecorded.

| case | phase located | what the buyer actually did |
|---|---|---|
| `:docker-inc` | SELECT | Each time Docker priced something already in use (Hub pull limits 2020, Desktop 2021, Free Team 2023) the response was "mitigation and substitution rather than purchase"; the 2023 offer was **withdrawn within ten days**. |
| `:lightbend-typesafe-akka` | SELECT | BSL relicence at \$1,995–2,995 per core above \$25m revenue. Terms and price were seen exactly; a documented subset took an unpriced substitute. **Apache Flink published its position the day after** the announcement. |
| `:redis-labs-redis-inc` | *none located* | RSALv2/SSPL aimed at converting cloud providers. **Named providers announced a fork within nine days.** The record: "a decision made fast, not a decision that failed." |
| `:linden-lab-second-life` | EVALUATE | Buyers saw the emptiness and bought anyway. Donnelly toured competitors' islands, found "There was nobody else around", and "decided to put money into Second Life anyway". Stern on the NBA island: "I can't say we have very precise expectations. We just want to be there." **No criterion existed against which the observation could be scored.** |
| `:gnu-emacs-fsf` | ACT | The recipient's loop completes without the payment step: GPL redistribution means a user can perceive, believe, evaluate and select and never reach an act of payment. FSF program-service revenue **never exceeded \$15,039 across 2011–2023, and was \$0 in some years.** |

## The first thing the table says: red is better evidenced than green

The green pitch — *could we help you get there faster* — is a claim about
counterfactual speed. The corpus's own acceptance grid is exactly that question
("could this apparatus have reached that case's known outcome sooner, and would
anyone have paid"), and Part III states plainly that its **one hundred and sixty
cells are all unrun**. So the green pitch currently rests on a simulation nobody
has performed.

Every red cell, by contrast, carries a cited `:failing-phase/:why` describing
what the buyer did instead, with dates. That is a diagnosis available today
without running anything. **On present evidence the red pitch is the defensible
one and the green pitch is not** — which is a sharper version of Joe's hunch
than the hunch itself.

## But red is three different things, and only one is "turn it green"

### 1. Correct refusals — Docker, Lightbend, Redis (3 of 5)

These are one pattern seen three times: **a vendor attached a price to something
already adopted for free, and the demand side substituted.** Nothing failed in
the buyer's loop. It ran correctly and returned "no" — Redis's record says so in
terms: *a decision made fast, not a decision that failed.*

You cannot consult these buyers into buying. The substitute exists, is good, and
in every case arrived within days: **ten days, one day, nine days.**

**The consulting customer here is the supplier, not the buyer, and the offer is
foresight rather than persuasion.** Three independent firms ran the same
experiment and got the same answer inside a fortnight. That is the most
replicated finding in the red set, and predicting it before the relicence — with
the substitution path named — is a service that does not require turning anything
green.

### 2. An unscored purchase — Linden Lab (1 of 5)

This is the real "turn it green", and it is the only one.

The buyers **did** pay. What was missing was any criterion for whether it worked
— "we just want to be there", 1,200 visitors and no expectation to compare it
against. That is not a failure to perceive or to decide; it is a missing
measurement, which is what this stack builds.

Note where it sits: the grid's **BELIEVE column is empty across the whole
corpus**. Linden Lab is the case standing nearest that hole. Whatever fills it is
the same instrument that would have given Stern a number to hold his \$1,200-per-
visitor against.

### 3. Structurally unpayable — GNU Emacs (1 of 5)

The loop completes without payment by construction of the licence. No offer can
be assembled that the buyer must reach. This is red and will stay red; it belongs
in the table as a boundary marker, not a target.

## What follows

- **Two offers, not one.** *Foresight* to suppliers contemplating a
  price-on-adoption move (kind 1, n=3, days-to-substitution recorded).
  *Criteria* to buyers who are already paying without a scoring rule (kind 2,
  n=1, and adjacent to the corpus's one empty column).
- **The n's are small and the corpus says so.** Part III's own preregistration
  calls predictive separation underpowered at thirty-two. Three instances of one
  refusal shape is a pattern worth naming, not a result.
- **A cheap next step, if wanted:** the seventeen greens have not been read for
  which of them bought *speed* versus bought *a criterion*. If greens cluster on
  criteria too, kind 2 is the offer and kind 1 is the marketing.
