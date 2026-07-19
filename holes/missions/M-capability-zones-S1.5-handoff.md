# M-capability-zones S1.5 handoff — the partition made visible before it is used

**From:** claude-3 (owner seat) → **To:** codex-10 (you built S1; this continues it)
**Mission:** `futon0/holes/missions/M-capability-zones.md` — re-read the amended
mechanism paragraph and S1.5 slice; the **dimensional amendment is normative**
(operative partition in a 3-D reduction; 2-D carpet renders its projection; raw
high-D nearest-seed is diagnostic only; R9-spirit invariant: the object Joe
accepts is the object G reads in S2, version-pinned).
**Context:** your S1 work landed as futon2 `56df8e5` after review (the job died at
the 30-min Agency cap; the embedding is now checkpointed/resumable — see
`scripts/capability_zones_prepare.py`). S1 numbers: 6810 items, 4892 assigned,
1918 resisted at margin<0.01 in raw BGE, 11/14 classes with evidence.
**Scope:** S1.5 ONLY. No G wiring, no posterior deposit, no writes to any live
store (:7070/:7071/:7073). The live-map layer must not alter existing layers'
behavior (saucers/rocket honest-absence rule untouched).

## Goal

Build the operative 3-D partition, re-assign the S1 harvest in it, and render the
zones as a layer on the existing 2-D carpet — so Joe can walk the partition and
accept or reject it BEFORE anything reaches G. Deposit stays locked behind his
acceptance.

## Deliverables

### D1 — the 3-D reduction, v1 = PCA-3 (deterministic, inspectable)
- Fit PCA with 3 components on the S1 corpus embeddings (all 6810 item vectors
  from `futon2/data/capability_zones/harvest-prepared.json` + the 14 seed
  vectors). Store as an explicit JSON artifact — mean vector + 3×1024 component
  matrix + per-component variance-explained + fit-corpus description + version id
  `"pca3-v1"` — at `futon2/data/capability_zones/reduction-pca3-v1.json`.
  NO pickled models: the transform must be reproducible from the artifact alone
  (that inspectability is the point of choosing PCA for v1).
- Rationale on record: PCA-3 is fully deterministic and matrix-auditable. If
  Joe's walk rejects the v1 partition as unwalkable, v2 tries UMAP-3 under a new
  version id with pinned random_state and a persisted reducer — do NOT build v2
  speculatively now.

### D2 — `zone-of-3d` (extend `futon2.aif.capability-zones`)
- `(transform-3d reduction embedding)` → [x y z] (pure matrix math, no Python).
- `(zone-of-3d reduction seeds-3d v)` → `{:class :distance :margin :runner-up
  :reduction-version "pca3-v1" :metric :interim-metric-3d}` — nearest seed by
  EUCLIDEAN distance in the 3-D space (a true Voronoi tessellation), margin =
  (runner-up distance − top distance), reported always, no hidden threshold.
- Operative seeds in 3-D: transform of the **centroid seed** where
  `centroid_evidence_count > 0`, text seed otherwise (open-mission, survey,
  pursue); record which generation each class used in the artifact.

### D3 — re-assignment + the disagreement set
- Re-assign all 6810 items via `zone-of-3d`; produce
  `futon2/data/capability_zones/harvest-2026-07-19-3d.edn` with the same record
  machinery as S1 (monthly `next-update-record` chunks — artifact only, NO
  `persist-record!`).
- Resistance in 3-D: ONE global threshold (never per-class). Propose the value
  yourself from the 3-D margin distribution and DOCUMENT the rationale + the
  distribution deciles in the artifact header; review judges defensibility.
- **Disagreement set:** every item whose 3-D class differs from its S1 high-D
  class, with both classes, both margins, repo/sha. Include per-class-pair
  counts in the artifact and a readable summary in D5. This is the
  boundary-distortion diagnostic made concrete — the high-D reading survives
  ONLY inside this diagnostic.

### D4 — the zones layer on the live map (2-D projection of the 3-D truth)
- Add a toggleable "capability zones" layer to the existing WebArxana live-map
  render (beside saucers/rocket), following that codebase's existing layer/data
  conventions (`futon3c/src/futon3c/live_efe_map.clj` and its front-end).
- The layer **colors existing carpet entities** (missions at their existing
  carpet positions) by the 3-D zone of their BGE embedding
  (`~/code/data/notions/bge_mission_embeddings.json` → transform-3d →
  zone-of-3d). Do NOT draw a fresh 2-D Voronoi tessellation over carpet
  coordinates — a 2-D-computed partition is exactly the adjacency-distortion
  failure the mission's dimensional note forbids. Zone identity comes from 3-D;
  the carpet only displays it.
- Legend with the 14 class colors + per-zone mission counts. Below-threshold
  (mixed) items get a visually distinct treatment (e.g. hatched/dimmed), and
  disagreement-set items a distinct marker. Distinctness must survive both
  light/dark map modes.
- Serving changes: follow the drawbridge discipline — :reload your own edits,
  NO JVM restarts, no side JVMs; if the layer can be served as a static data
  artifact consumed by the existing front-end, prefer that.

### D5 — acceptance material for Joe's walk
- `futon0/holes/missions/M-capability-zones-S1.5-acceptance.md`: per-zone
  census (3-D counts vs S1 high-D counts side by side), 3-D margin deciles vs
  the S1 high-D deciles, disagreement-set summary (size, top class-pairs,
  5 example items with repo/sha), which seed generation each class used,
  PCA variance-explained, and HOW TO WALK IT (URL, layer toggle, what the
  visual encodings mean). Fractions, not adjectives.

## Acceptance bar (demonstrate in your bell-back)

1. Determinism: transforming the same vector twice, and re-fitting from the
   artifact's recorded corpus description, yields identical 3-D coordinates
   (test with fixtures; no RNG anywhere in v1).
2. `zone-of-3d` tests: nearest+margin on fixture vectors; seeds-generation
   selection logic covered.
3. Re-assignment artifact replays via `intrinsic-values/rehydrate!` in-memory
   without error; report classes-with-evidence count in 3-D.
4. Disagreement set computed and non-trivially reported (size + pairs).
5. The layer renders: verify with Playwright against the live map — zone colors
   present, legend present, toggle works; attach the screenshot path.
6. ZERO live-store writes; :7073 `wm-hyperparameter-update` count stays 108.

## Gates

clj-kondo clean; `futon4/dev/check-parens.el` (invoke:
`emacs -Q --batch -l futon4/dev/check-parens.el --eval "(arxana-check-parens-cli)" -- --no-defaults FILES`);
tests for D2/D3; Playwright check for D4; no hand-tuned per-class constants;
nothing outside this spec (stop and ask instead).

## Delivery

- Commit to futon2 (reduction, zone-of-3d, re-assignment) and futon3c (layer)
  and futon0 (acceptance doc) with clear messages. NOTE: futon2 is currently on
  branch `M-propagators-ant-gate` — commit there too (do not switch branches);
  the branch question is flagged to Joe separately.
- Nothing here needs long compute (PCA fit is seconds); if anything unexpectedly
  approaches the job window, commit partials + state what's partial + bell back
  anyway — do not go silent.
- **Bell claude-3 back with: summary, commit shas, acceptance-bar evidence
  (determinism proof, replay counts, disagreement size, screenshot path,
  store count), and any spec gaps you hit.**
