# The SELECT column, and what it does and does not say about R14

**Date:** 2026-08-26 · claude-13, at Joe's direction: *"since R14 is at SELECT
and there are two green and two red company events in SELECT, maybe it's worth
developing the company story here a bit too."*

Companion to `NOTE-red-rings-and-the-prediction-problem.md` (which develops R8)
and `NOTE-red-cells-as-consulting-targets.md` (which classifies the five reds).

## The four cells

Of 32 cases, 15 have a phase-located problem: PERCEIVE 3, **SELECT 4**,
EVALUATE 3, ACT 5, BELIEVE 0. The SELECT four:

| case | response | what the record says |
|---|---|---|
| Galois, Inc. | `:engaged` | *"the option is known and demonstrated, and it is not chosen"* |
| PlanetMath | `:engaged` | *"The step that never completed is the commitment, not the appraisal."* |
| Docker, Inc. | `:declined-on-merit` | *"no difficulty perceiving or evaluating the artifact … What is documented is the choice step"* |
| Typesafe/Lightbend/Akka | `:declined-on-merit` | *"Nothing failed at perception or evaluation; the choice went against the seller."* |

**Four independent records, written from four unrelated sources, state the same
thing in nearly the same words: the appraisal completed and the commitment did
not follow from it.** That is R14's node statement — commitment as a step
distinct from evaluation — arrived at from the demand side without reference to
the ring.

## What separates the greens from the reds here — not what the colour suggests

Reading the `:response-signal` detail rather than the class:

- **Galois** is `:engaged` on a record that splits the buyer in two:
  `:by-buyer` carries DARPA as `:engaged` (*"Paid repeatedly across HACMS/SMACCM"*)
  and the military services as `:declined-on-capacity` (*"gated on resources,
  not on merit"*). The party that evaluated and the party that paid are
  different parties.
- **PlanetMath** is `:engaged` on a Springer contract, three Google Summer of
  Code cohorts, and ten sponsoring societies — and the record adds: *"All of
  these were project-shaped and time-boxed; **none became a standing budget
  line**, and all had lapsed by the time the portal wound down in 2018."*

So **both greens are green on episodic sponsorship, not on a commitment that
followed an appraisal.** In all four SELECT cells the evaluating party never
committed; in two of them a sponsor absorbed the gap, and the grid renders that
absorption as green.

That is a finding about the grid as much as about R14: **in the SELECT column,
green does not mean the loop closed.** It means someone else paid for a while.
The two reds differ by having no substitute payer and an available substitute
good — Docker's population mitigated and substituted, Lightbend's forkers *"then
spent engineering hours on a migration, which is the opposite of having none"*,
i.e. they paid in labour rather than licence.

## The correction this forces to the R8 note

`NOTE-red-rings-and-the-prediction-problem.md` is titled *"R8 is the ring, and
Docker is its business instance"* and computes, from Docker's three price moves,

    returned signals that changed the next action:  0 of 2

That reading stands. Its ring assignment needs one refinement: **R8 sits in
PERCEIVE and R14 sits in SELECT, and Docker's own `:failing-phase` field says
SELECT.** The note reached R8 through the ruling — WR-27, which carries
`@holds-open R8 R14` and therefore does not discriminate between them. The
corpus's phase field does. So Docker and Lightbend are more precisely **R14's**
business instances, and the 0-of-2 is a reading of the *commitment* step.

Worth noting the shape, and only the shape. *"0 of 2 returned signals changed
the next action"* and the War Machine's own verified defect — *no value of τ
changes the selected action on the enacting path*
(`futon3c/holes/excursions/E-R14-red-ring-fill.md`) — are the same sentence
about two different systems: a quantity is computed, returned, and the choice
is invariant to it. That is a structural parallel between a company record and
our own selector, not evidence that one mechanism explains the other.

## What this does not license, stated because it is the obvious next step

**These four cases are not a salience instance for R14.** The ring's pattern
asks for a dated observation of *commitment temperature* — our dial — being
wrong, costly, or noticed, and records `?salience(required)` rather than argue
one from the mechanism. Four companies whose own commitment steps failed are a
**demand-side argument for instrumenting that step**; they are not evidence
that our uninstrumented dial cost anyone anything. Writing them into the
salience slot would be the false-salience move the pattern exists to refuse.

What they do support is narrower and still useful: of the corpus's 15
phase-located problems, **the commitment step is the second most common
location, and it is the one where the record most often says the preceding
stages worked.** A stack that instruments that step is aiming at a place the
demand side independently marks.

## Related

- `NOTE-red-rings-and-the-prediction-problem.md` — R8, and the Docker 0-of-2.
- `NOTE-red-cells-as-consulting-targets.md` — the five reds, and why three are correct refusals.
- `futon3c/holes/excursions/E-R14-red-ring-fill.md` — R14 as red on a disconnected dial.
- `records/batch-H-demand.edn` — the four records, `:failing-phase` and `:response-signal`.
- `lights.bb` — the grid, and the state vocabulary these colours come from.
