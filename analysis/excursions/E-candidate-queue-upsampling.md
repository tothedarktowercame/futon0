# Excursion: Candidate Queue Upsampling

**Date:** 2026-04-07  
**Entry point:** E-sorry-initialization residue → Candidate Queue → Portfolio conductor

## What This Is

Candidate Queue, Peripheral Inspector, and Mission Portfolio were identified as
semi-structural channels: they promise structured signals to the conductor but
emit only human-readable summaries. This excursion inhabits the Candidate Queue
surface long enough to read its sorrys, specify their type signatures, and make
the resulting signals consumable by `portfolio/observe.clj`.

## Moves

1. **Walk the queue surface**
   - Sit inside the Futon4 Candidate Queue UI/API (textures, Arxana export,
     queue navigation scripts) and enumerate every artefact it exposes today.
   - For each artefact, note whether it already has a machine-legible schema or
     whether we only have prose (the latter are typed holes).

2. **Map queue entities to conductor needs**
   - For each sorry uncovered above, state the type of observation the
     portfolio loop needs (e.g., invariant tension count, reviewer coverage,
     time-since-last-touch, upstream mission ID).
   - Document these mappings inline so the queue can emit JSON/EDN that matches
     `portfolio/observe.clj` expectations.

3. **Define an export path**
   - Pick the lowest-friction interface (EDN file, API endpoint, or mission
     control hand-off) that can stream queue data to futon3c.
   - Ensure the export includes provenance (queue snapshot timestamp, query
     parameters) so the conductor can reason about recency.

## Tests

- At least one queue artefact produces structured output with a SKU that the
  conductor recognises (even if it is still a stub value).
- Portfolio observation layer can ingest the file/endpoint without schema
  errors and log the channel as present (even if weights are neutral).
- Running the queue excursion twice produces comparable data, proving the queue
  sorry is now partly legible.

## Residue

- A list of queue sorrys annotated with their type signatures and maintenance
  cost estimates.
- A concrete plan for the first portfolio observation field sourced from the
  queue (e.g., `candidate-queue/tension-count`).
- Direction for the follow-up excursion that will do the actual wiring once the
  type signatures stop shifting underfoot.
