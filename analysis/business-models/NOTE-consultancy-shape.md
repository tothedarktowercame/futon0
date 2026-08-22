# The consultancy shape, and the unit that decides it

**Date:** 2026-08-22 · Short by intent (Joe: *"we don't need to rehash this
massively right now"*).

## The shape

*"Building an AI-assisted software consultancy business"* is a reasonable shape
for Hyperreal to take (Joe, 2026-08-22). It has some PlanetMath-like features —
a contributor apparatus producing legible units of work — and the difference
that matters is in the unit economics:

> **completed missions would be shippable to clients, whereas PlanetMath
> "articles" were just shippable to (non-paying) readers.**

That is the demand-side finding (`FINDING-volunteer-layer.md`) restated in the
first person. Same production apparatus, different terminal: a unit with a
party obliged to accept it, versus a unit with an audience.

## The caution, with numbers

**The mission is not yet the billed unit.** From `~/code/ledger/ledger.edn`:

- 35 line items; the unit is `:item/hours` × `:item/rate-gbp`
- 20 items carry `:item/evidence`
- **15 evidence references name missions**, across 7 distinct ones
  (`M-interim-director`, `M-authoring-3d`, `M-gaze-mirror`, `M-relay-authoring`,
  `M-anthology-auth`, `M-authoring-2d`, `M-futon-forward-model`)

So missions already appear in the invoice — as the **warrant** for a line item,
not as the **unit** of one. The only client to date was billed in hours.

That gap is the whole of the work. A mission becomes an economic unit when a
client accepts *the mission*, not the hours spent inside it — which is exactly
the per-unit acceptance event the 32-case corpus says decides capture. Until
then the mission is FUTON's internal ontology and the invoice's ontology is
time, and the two are joined only by a free-text evidence string.

## The failure mode to watch

If missions ship to readers, to a repository, or to nobody — rather than to a
client who accepts them — the apparatus reproduces PlanetMath **regardless of
the quality of the missions.** PlanetMath's articles were not bad. They had no
obliged recipient. An AI-assisted consultancy that completes excellent missions
nobody has agreed to accept is the same case with better tooling.

The check is cheap and worth running periodically: *for the last N completed
missions, how many had a party obliged in advance to accept the result?*

## Aside, not pursued

Joe's reading of the Math Forum's fit with Drexel: *"grown out of NSF-funded
research"* likely mattered because it **bypassed the not-invented-here
problem** — an externally-funded pedigree makes an institution's adoption a
recognition rather than a concession. Recorded because it generalises to any
sponsor approach; not developed here.
