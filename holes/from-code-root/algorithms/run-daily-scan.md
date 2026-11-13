# Algorithm: Run Daily Scan — Brief, Workup, Frame

Date: 2026-04-24
Status: active
Owner mission: M-daily-scan (futon7)
Cross-ref: M-war-machine (consumer — daily scan is a frame derivative)

## Subseries (axes)

As of 2026-04-24 the scan supports multiple subseries (axes). Each
subseries has its own probe set, data location, and frame sequence,
but shares the apparatus.

| Axis | Task | Probes dir | Daily dir | Briefs dir | Frame filename |
|---|---|---|---|---|---|
| **main** (consulting) | `bb daily-scan` | `data/probes/*.edn` + static | `data/daily/` | `data/briefs/` | `frame-daily-scan-NNN.edn` |
| **:lean** (ICARM/Lean/superpod-mining) | `bb daily-scan-lean` | `data/probes/lean/*.edn` | `data/daily/lean/` | `data/briefs/lean/` | `frame-daily-scan-lean-NNN.edn` |

`bb daily-scan --axis <name>` runs an arbitrary axis. Subseries
use only their own custom probes; the static consulting probe set
(`f7.probes/all-probes`) is included only for `main`.

All frames share the regex `^frame-daily-scan-.*\\.edn\\'` and appear
together in the Arxana Browser's "Scans" view, with a `Series`
column distinguishing `main` from subseries.

## Purpose

Run one day of the 20-day M-daily-scan cycle end-to-end: probe the
landscape, render a brief, produce a workup, then write a frame
event that promotes the day's findings into futon5a where they are
consumable by the War Machine and browsable via the Arxana Browser
"Scans" entry point.

Three artefacts per scan day, living in three places:

| Artefact | Location | Source |
|---|---|---|
| Raw scan data (EDN)   | `futon7/data/daily/YYYY-MM-DD.edn`   | `bb daily-scan` |
| Brief + workup (MD)   | `futon7/data/briefs/YYYY-MM-DD{,-workup}.md` | `bb daily-scan` + human workup |
| Frame event (EDN)     | `futon5a/data/frames/frame-daily-scan-NNN.edn` | this algorithm, hand-crafted from the workup |

**All three must exist for the scan to be "done" in the Arxana
Browser sense.** Missing the frame means the scan ran but never
fed back into the hypergraph.

## Prerequisites

1. `bb` available on PATH and working in `/home/joe/code/futon7`
2. `GITHUB_TOKEN` environment variable set (probe generation uses the
   GH search API)
3. Previous day's frame committed (so `frame/day` numbering is correct)

## Procedure

### Step 1: Run the probe pass (10-30 min)

```bash
cd /home/joe/code/futon7

# Main (consulting) axis:
bb daily-scan

# Or a subseries — e.g. :lean:
bb daily-scan-lean
# equivalent to:
bb daily-scan --axis lean
```

This writes (where `<axis>` is empty for main, or `/<name>` for subseries):

- `data/daily<axis>/YYYY-MM-DD.edn` — raw probe results
- `data/briefs<axis>/YYYY-MM-DD.md` — rendered brief

For subseries, the brief footer names the axis explicitly; the scan's
stored EDN carries an `:axis` key.

Skim the brief. If the Today's Focus section is empty or looks stale
(same top-3 as yesterday), treat that as a signal — it means the
probes need tightening before the workup, not after. See the Day 2
methodology correction (`frame-daily-scan-002.edn`) for precedent.

### Step 2: Workup (45-90 min, conversational with an agent)

Open the brief in the REPL surface.  The workup is a conversation,
not an edit — you talk through each focus item and the agent
structures it.

The workup produces:

- `data/briefs/YYYY-MM-DD-workup.md` — headline, capability map,
  probe-experiment notes, actions, cross-day adjacencies

Workup must answer:

1. **Headline.** What's the single most important thing this scan
   surfaced? One or two sentences.
2. **Fingerprint.** If a new org/person/product was found, enumerate
   repos/contacts/tooling/adjacencies in a table.
3. **Capability map.** For each of Joe's capability stories, what
   surface does it plug into for this target, and what's the
   specific conversation to have?
4. **Probe experiment.** What worked, what didn't, what was
   reconfirmed.  Track blindspots across scans (AIQA Day 3 + Orcro
   Day 4 reconfirmed the scoring-pre-commercial blindspot).
5. **Actions** with priority + next step.

### Step 3: Write the frame (15-30 min, agent-assisted)

Create `futon5a/data/frames/frame-daily-scan-NNN.edn` (main axis) or
`frame-daily-scan-<axis>-NNN.edn` (subseries) where `NNN` is
zero-padded sequence (`001`, `002`, ...) **per axis**.  Do **not**
reuse a date in the filename — use the sequence number; dates live
in `:frame/timestamp` and `:frame/window`.  Subseries frames omit
`:frame/day` and use `:frame/subseries <keyword>` +
`:frame/subseries-day N`.

Minimum keys every frame carries:

```clojure
{:frame/id "daily-scan-NNN"
 :frame/timestamp "YYYY-MM-DDTHH:MM:SSZ"
 :frame/window {:start "..." :end "..." :hours N}
 :frame/type :daily-scan
 :frame/day N               ; sequence number, not calendar day
 :frame/gap-days [...]      ; calendar days skipped since last scan
 :frame/scan {:probes N :repos-scanned N :cross-probe-hits N
              :source-files {:brief "..." :workup "..." :daily "..."}}
 :frame/headline {...}
 :frame/leads [...]
 :frame/actions [...]
 :frame/instruments {:options [...] :futures [...]
                     :shorts [...] :yields [...]}
 :frame/cardinal-direction {:hermit N :foraging N :cargo N :depositing N}
 :frame/constraints {:income-deadline {...}
                     :daily-scan-streak {...}}
 :frame/delta {...}}          ; change vs previous frame
```

**Shape-variable keys** (add as the scan's cargo requires):

- `:frame/methodology` — if the scan corrected or validated a
  methodology (Day 2, Day 3)
- `:frame/methodology-firsts` — first adoptions of a pattern in
  this mission (Day 4 seed-frame-before-probes,
  hinge-log-operational)
- `:frame/capability-map` — vector per capability-story when the
  scan's audience is concrete
- `:frame/probe-experiment` — when the scan tested probe typology
  (keyword vs audience; person vs org vs topic)
- `:frame/infra-finding` — scan-infrastructure issues surfaced by
  the workup (scoring blindspots, search-API quirks)
- `:frame/graph-edges` — new cross-day edges for downstream graph
  consumers
- `:frame/integration-sketches` — pre-commitment sketches with
  effort estimates and drop criteria (Day 4)

**Frames evolve.**  Day 1's `:frame/leads` was a flat list; by Day 4
leads are typed (`:warm-technical`, `:pitch-with-vouch`,
`:artefact-source`) with explicit relationship fields.  This is
expected — the frame schema is a hypothesis being refined.  Add
keys the workup justifies; don't backfill old frames.

### Step 4: Validate and inspect

```bash
cd /home/joe/code/futon5a
clojure -M -e "(clojure.edn/read-string (slurp \"data/frames/frame-daily-scan-NNN.edn\"))" \
  && echo OK
```

Open the Arxana Browser in Emacs and navigate to **Scans**. The new
frame should appear in the list with correct day + date + frame-id.

### Step 5: Update the mission ledger

In `futon7/holes/missions/M-daily-scan.md`:

- Update `daily-scan-streak` completed count
- Add to `gap-days` if the calendar day before was skipped
- Note any standing-task promotions from `:frame/infra-finding`
  entries (two data points crosses the threshold from "flag" to
  "standing task"; see Day 4 scoring blindspot).

## Outputs

- One new file in `futon5a/data/frames/`
- Updated mission ledger in `futon7/holes/missions/M-daily-scan.md`
- Arxana Browser > Scans shows the new entry on next refresh

## When not to run

Skip a scan day if:

- No 1-hour block available (the mission's test hypothesis is 1
  hour/day × 20 days; half-attempts dilute the signal)
- Previous day's workup hasn't been written (the brief sitting
  without a workup is a backlog item; don't stack)
- The probe landscape hasn't refreshed meaningfully (GitHub search
  is cached; running twice in a 12-hour window is wasted calls)

Gap days get recorded in `:frame/gap-days` of the next frame, not
hidden.  Day 3 skipped 04-15; Day 4 skipped 04-17.

## Adding a new subseries

1. Pick an axis keyword (e.g. `:lean`, `:infrastructure`,
   `:pedagogy`).
2. Create `futon7/data/probes/<axis>/` with audience-first probe
   EDNs (one per person/org target). Each probe carries
   `:axis "<axis>"` for traceability.
3. Add a bb task: `<axis>-scan {:task (daily/-main "--axis" "<axis>")}`
   in `futon7/bb.edn`, or rely on `bb daily-scan --axis <axis>`.
4. First scan writes the brief at `futon7/data/briefs/<axis>/`.
5. Workup produces `<date>-workup.md` in the same dir.
6. Frame goes to `futon5a/data/frames/frame-daily-scan-<axis>-NNN.edn`
   with `:frame/subseries :<axis>` + `:frame/subseries-day N`.
7. No Arxana Browser change needed — existing regex matches.
8. Record the new subseries in memory (project type).

## Related algorithms

- `apm-daily-batch.md` — the other daily cadence mission (APM prelim
  problems through two-agent pipeline). Different shape: that one is
  production work; this one is landscape scanning.
- (future) `workup-brief-to-frame.md` — could formalise Step 3 as
  its own algorithm once the frame schema stabilises.
- `futon3/library/scan-coherence/mission-anchored-scan.flexiarg` — the
  quality criterion for a *good* scan day: anchor the scan to a live
  mission's named hypothesis, split audience-first probing from
  concept-research, and read a confirming-negative as a result. Origin:
  frame-daily-scan-006 (the M-explore-aiqa outward-MAP scan).

## Schema drift policy

The frame schema is under active refinement. When a new key is added
in a scan:

1. Document its intent in the frame's own EDN comments.
2. If the key recurs across three scans, promote it to the
   "Minimum keys" list in this algorithm.
3. Never rename or remove a key from an existing frame — frames are
   append-only history.  Change the schema forward.
