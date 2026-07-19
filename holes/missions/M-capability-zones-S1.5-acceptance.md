# M-capability-zones S1.5 — PCA-3 walk acceptance

Date: 2026-07-19. Candidate partition: `pca3-v1`. Status: **candidate for
operator walk; not deposited into G or any live store**.

## What is being accepted

The operative object is the Euclidean nearest-seed Voronoi partition after the
explicit PCA-3 transform. The 2-D carpet only displays those already-computed
3-D identities at existing mission positions. Raw 1024-D cosine assignments
are retained solely as a disagreement diagnostic. S2 must consume the exact
version-pinned `pca3-v1` reduction and seed-generation manifest accepted here.

PCA-3 was fit deterministically in float64, with full SVD, on all 6,810 S1
evidence embeddings followed by the 14 operative seed vectors (6,824 vectors,
1,024 dimensions). The artifact contains the mean and complete 3×1,024 matrix;
there is no pickle or RNG. Its explained-variance ratios are 0.086523,
0.044902, and 0.028533 (0.159957 combined). PCA-3 is used because it is
deterministic and matrix-auditable. If this walk rejects it as unwalkable, a
separately versioned UMAP-3 with pinned random state may be proposed; none was
built in S1.5.

## Evidence census

Counts below exclude each partition's globally resisted/mixed observations;
fractions use the full 6,810-item corpus as denominator so the two columns are
directly comparable.

| Action class | S1 high-D count (fraction) | PCA-3 count (fraction) |
|---|---:|---:|
| `no-op` | 45 (0.66%) | 765 (11.23%) |
| `address-sorry` | 11 (0.16%) | 14 (0.21%) |
| `open-mission` | 0 (0.00%) | 77 (1.13%) |
| `advance-mission` | 1,609 (23.63%) | 1,265 (18.58%) |
| `close` | 82 (1.20%) | 636 (9.34%) |
| `close-mission` | 4 (0.06%) | 24 (0.35%) |
| `close-hole` | 6 (0.09%) | 137 (2.01%) |
| `survey` | 0 (0.00%) | 1,298 (19.06%) |
| `survey-mission` | 255 (3.74%) | 381 (5.59%) |
| `apply-cascade` | 2,345 (34.43%) | 279 (4.10%) |
| `fire-pattern` | 11 (0.16%) | 127 (1.86%) |
| `learn-action-class` | 484 (7.11%) | 879 (12.91%) |
| `pursue` | 0 (0.00%) | 173 (2.54%) |
| `decompose` | 40 (0.59%) | 75 (1.10%) |
| resisted / mixed | 1,918 (28.16%) | 680 (9.99%) |
| **accepted total** | **4,892 (71.84%)** | **6,130 (90.01%)** |

PCA-3 uses one corpus-derived threshold for every class: the empirical p10
margin, 0.0038972942965891377. The bottom 680/6,810 (9.99%) observations are
therefore shown as mixed. No per-class constants are used.

## Margin distribution

| Decile | S1 high-D cosine margin | PCA-3 Euclidean margin |
|---|---:|---:|
| p0 | 0.00000473 | 0.00000128 |
| p10 | 0.00343131 | 0.00389729 |
| p20 | 0.00704157 | 0.00806116 |
| p30 | 0.01077476 | 0.01232005 |
| p40 | 0.01496085 | 0.01805892 |
| p50 | 0.01962752 | 0.02346685 |
| p60 | 0.02540751 | 0.02774528 |
| p70 | 0.03241347 | 0.03349190 |
| p80 | 0.04206666 | 0.04528031 |
| p90 | 0.05636576 | 0.06540071 |
| p100 | 0.18433873 | 0.27906016 |

## Boundary-distortion diagnostic

The class changes for 5,401/6,810 evidence items (79.31%). This is deliberately
not suppressed: it says the amended 3-D operative geometry is materially
different from raw high-D cosine. The largest directed class-pair changes are:

| S1 high-D → PCA-3 | Count | Fraction of disagreements |
|---|---:|---:|
| `apply-cascade` → `survey` | 804 | 14.89% |
| `apply-cascade` → `advance-mission` | 584 | 10.81% |
| `advance-mission` → `survey` | 362 | 6.70% |
| `advance-mission` → `no-op` | 329 | 6.09% |
| `apply-cascade` → `no-op` | 328 | 6.07% |
| `advance-mission` → `learn-action-class` | 321 | 5.94% |
| `apply-cascade` → `learn-action-class` | 284 | 5.26% |
| `advance-mission` → `close` | 235 | 4.35% |

Five examples (the full set, margins, and subjects are in the harvest artifact):

| Repo / SHA | Subject | S1 → PCA-3 | S1 / PCA-3 margin | Mixed? |
|---|---|---|---:|---|
| futon0 / `42785bcf6b20` | Record cleared WM technical gate | `close-mission` → `advance-mission` | 0.006189 / 0.025935 | no |
| futon0 / `f655a721bcbd` | Disable WM schedule under stop-line ruling | `apply-cascade` → `decompose` | 0.010407 / 0.000583 | yes |
| futon0 / `0fb40e02c7de` | Register full-loop cron in authoritative substrate | `apply-cascade` → `decompose` | 0.028175 / 0.004101 | no |
| futon0 / `09e3bc8e586a` | Add usage-hacking mission proposal | `advance-mission` → `close` | 0.025092 / 0.000787 | yes |
| futon0 / `1e84d4f9896` | Extract replayable affect detector | `apply-cascade` → `learn-action-class` | 0.034035 / 0.020419 | no |

## Operative seed generations

Centroid seeds are used exactly when S1 harvested evidence exists; otherwise
the text seed remains operative.

| Generation | Classes |
|---|---|
| centroid seed | `no-op`, `address-sorry`, `advance-mission`, `close`, `close-mission`, `close-hole`, `survey-mission`, `apply-cascade`, `fire-pattern`, `learn-action-class`, `decompose` |
| text seed | `open-mission`, `survey`, `pursue` |

## How to walk it

Open
`file:///home/joe/code/futon6/data/mission-efe-field-embed.html` in a browser.
The **capability zones: on/off** button beside the live status toggles only this
layer; existing carpet, saucers, rocket, and honest-absence behavior are
unchanged.

- Each coloured circle is an existing mission at its existing carpet position;
  its colour is the class assigned in PCA-3, never a 2-D recomputation.
- The legend at upper right gives all 14 class colours and positioned-mission
  counts. The source has 311 embedding records for 277 unique mission ids; 34
  duplicate records are collapsed by taking their vector centroid. The
  projection contains 269/277 positioned missions (97.11%); eight missions
  have no embed-carpet position.
- Dashed, dim circles are below the one global PCA-3 margin threshold (30/269,
  11.15%). A white × marks a disagreement with the raw high-D diagnostic
  (217/269, 80.67%). These encodings use stroke and line pattern as well as
  colour, retaining contrast on light or dark backgrounds.
- Hover a circle to see mission id, PCA-3 class/margin, mixed status, and the
  high-D diagnostic class/margin.

Playwright evidence: `holes/missions/evidence/capability-zones-pca3-v1.png`.
The automated check observed 269 coloured entities, one 14-class legend, and
layer display transitions `inline → none → inline` under two toggle clicks.

## Deposit lock

The S1.5 harvest is an artifact-only sequence of 134 pure
`wm-hyperparameter-update` records. In-memory replay covers all 14 classes.
No `persist-record!` call occurs in any S1.5 builder. The authoritative live
store baseline supplied at handoff was 108. The after-count could not be read:
`:7073` accepted TCP connections but timed out without bytes on both `/health`
and the typed hyperedge query while its JVM was already CPU-saturated. It was
not restarted. Thus the no-write property is code- and operation-audited, but
the requested after-count remains an explicit acceptance gap until the
existing store responds. Deposit remains locked on Joe's explicit acceptance.
