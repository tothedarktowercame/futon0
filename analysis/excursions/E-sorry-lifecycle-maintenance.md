# Excursion: Sorry Lifecycle Maintenance Channel

**Date:** 2026-04-07  
**Entry point:** E-sorry-initialization residue → mission_control_shapes → portfolio observe

## What This Is

The sorry ontology demands lifecycle richness (`:parked`, `:superseded-by`,
`:composted-into`, `:abandoned`) and a maintenance-cost observation channel.
Without them, the conductor cannot recommend `:decommit` actions and the sorry
audit has nowhere to land its findings. This excursion sets up the schema,
parser, and observation scaffolding so later work can focus on policy changes.

## Moves

1. **Extend the mission schema**
   - Update `futon3c/peripheral/mission_control_shapes.clj` and friends so the
     status enum includes the four lifecycle states plus structured fields for
     successor references.
   - Ensure the Markdown parser/serializer preserves these additions so
     missions can be edited anywhere without lossy round-trips.

2. **Add the maintenance-cost observation channel**
   - Walk the mission dependency graph produced by mission control, compute
     downstream counts weighted by staleness, and emit a scalar per mission in
     `portfolio/observe.clj`.
   - Store the new observation in `state.clj` so EFE/policy can read it later
     even if we initially keep its weight small.

3. **Prepare the policy hook**
   - Add placeholder wiring in `portfolio/policy.clj` for a forthcoming
     `:decommit` action so the maintenance channel has an obvious consumer once
     it stabilises.

## Tests

- Mission control inventory includes the new lifecycle states and links without
  validation errors.
- Portfolio runs log the maintenance-cost channel with sensible ranges
  (0=isolated, high values = mouse-infested).
- Policy layer loads with the new action stub enabled (even if it is gated
  behind a feature flag), proving the pipeline is ready for conductor decisions.

## Residue

- Mission files annotated with lifecycle metadata that the sorry audit can
  mutate.
- A repeatable calculation for maintenance cost, sourced from the same data the
  human HUD consumes.
- Clear seam for the follow-up excursion that will teach the conductor how to
  choose `:decommit` when the maintenance channel spikes.
