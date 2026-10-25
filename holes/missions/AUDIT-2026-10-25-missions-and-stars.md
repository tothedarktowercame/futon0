# Mission / capability-star audit - 2026-10-25

Scope: `~/code/*/holes/missions/M-*.md` (422 docs across 16 repos). Read-only audit approved by Joe 2026-10-25. Script: `futon0/scripts/mission_star_audit.py` (`collect`, then `infer --emit-batches` / `infer --verify`, then `report`).

Status parsing mirrors `futon2.aif.mission-registry`: the first 20 lines are matched against its `status-line-pattern`, and the lead token is classified (draft / inactive / complete / active / open / partial / unknown). Docs with no Status line in the first 20 lines are counted `no-status`.

Wants are read as unchecked `- [ ]` task lines. **admitted** uses a documented text-level heuristic standing in for the registry's runtime locator classes (:C3 :C4 :C5 :C6 :C8 of `futon2.aif.cascade-problems`): the task text names a mechanically checkable artifact (a path, a script/test invocation, an exit code, a sha, a count, or an explicit check/test/emit word). **stated-no-locator** = unchecked tasks matching none of those. This is not a run of `futon2.aif.outcome-wants/wants` against substrate-2; it is the same question asked of the doc text alone.

Live = a commit within 30 days of 2026-10-25 and status not complete/inactive.

Inferred links were produced by馬-sonnet (batched, 10 missions per whistle, star titles + first 1500 characters each, at most 2 stars per mission, each with a verbatim quote of >= 8 words); quotes were string-checked against the mission text and picks whose quote failed were dropped. Every such link is marked `inferred` and is not a declaration.

## Counts

| Measure | Count |
|---|---:|
| status: complete | 122 |
| status: draft | 1 |
| status: inactive | 54 |
| status: no-status | 20 |
| status: open | 172 |
| status: unknown | 53 |
| declared star link | 29 |
| inferred star only | 87 |
| no star (declared or inferred) | 306 |
| want: admitted | 202 |
| want: stated-no-locator | 24 |
| want: none | 196 |
| no commit in 30 days | 133 |

## Per star

| Star | Status | Declared | Inferred | Live among them |
|---|---|---:|---:|---:|
| `ai-passes-prelims` | held | 0 | 11 | 2 |
| `apm-prelim-corpus-substrate` | satisfied | 3 | 4 | 5 |
| `arxana-hypergraph-store` | satisfied | 3 | 5 | 6 |
| `cold-eoi-authored-outbox` | held | 1 | 3 | 2 |
| `cold-eoi-sent` | held | 0 | 0 | 0 |
| `cold-response-conversion` | held | 0 | 0 | 0 |
| `cold-send-response` | held | 0 | 0 | 0 |
| `concept-authority` | satisfied | 2 | 0 | 1 |
| `daily-scan-lead-foraging` | satisfied | 0 | 2 | 1 |
| `distributed-proofreaders` | active | 2 | 3 | 1 |
| `efe-trustworthy-over-starmap` | held | 1 | 0 | 0 |
| `evidence-persistence` | satisfied | 1 | 6 | 1 |
| `full-arxiv-mining` | held | 0 | 2 | 0 |
| `hypergraph-operator` | held | 1 | 0 | 1 |
| `interest-network-apparatus` | satisfied | 1 | 6 | 5 |
| `kit-cadence` | held | 1 | 0 | 1 |
| `kit-certify` | satisfied | 0 | 0 | 0 |
| `kit-intake` | held | 1 | 0 | 1 |
| `kit-mint` | satisfied | 0 | 0 | 0 |
| `kit-observables` | satisfied | 0 | 0 | 0 |
| `kit-outbox` | held | 1 | 0 | 1 |
| `kit-registry` | satisfied | 0 | 0 | 0 |
| `kit-render` | satisfied | 0 | 0 | 0 |
| `latexml-fragment-parse` | satisfied | 2 | 0 | 1 |
| `live-geometric-stack` | satisfied | 1 | 5 | 0 |
| `math-ct-prior-substrate` | satisfied | 2 | 5 | 2 |
| `math-embedding-retrieval` | satisfied | 2 | 2 | 1 |
| `operator-disposition` | ? | 0 | 0 | 0 |
| `rob-joe-codrafting` | satisfied | 0 | 0 | 0 |
| `rob-proof-graph-kb` | satisfied | 0 | 0 | 0 |
| `self-representing-stack` | satisfied | 2 | 2 | 2 |
| `stack-stereolithography-substrate` | satisfied | 2 | 1 | 3 |
| `symbol-grounding` | held | 0 | 2 | 0 |
| `war-machine` | satisfied | 4 | 21 | 7 |
| `warm-customer-pays` | satisfied | 0 | 8 | 6 |
| `wm-overnight-unsupervised` | held | 0 | 2 | 1 |
| `wm-steps-forward-guardrailed` | satisfied | 0 | 6 | 5 |

Stars with **no live mission** (no declared or inferred mission with a commit in 30 days and not closed):
- `cold-eoi-sent` (held) - Cold EOI gets sent to a real cold lead — the crux, n=0
- `cold-response-conversion` (held) - Cold response converts to a paid engagement — n_conversion remains zero
- `cold-send-response` (held) - Cold send draws a response — n_outreach remains zero
- `efe-trustworthy-over-starmap` (held) - EFE does real AIF over the star-map — the WM's pick is an applicable single-cycle leaf (no
- `full-arxiv-mining` (held) - Full ArXiv mining with structure-based representation — held beyond the CT prior substrate
- `kit-certify` (satisfied) - Pudding-prover certify — held→satisfied transition plus anti-laundering guard
- `kit-mint` (satisfied) - Pudding-prover mint — static metric to priced conditional sorry
- `kit-observables` (satisfied) - Pudding-prover observable tracking — render + observe! verb over the :observables vector (
- `kit-registry` (satisfied) - Pudding-prover kit registry — two-kind sorry registry + schema
- `kit-render` (satisfied) - Pudding-prover render — headless text/org render and CLI
- `live-geometric-stack` (satisfied) - Substrate-2 — the live geometric stack / commit-factored hypergraph the WM reads
- `operator-disposition` (?) - 
- `rob-joe-codrafting` (satisfied) - Rob + Joe mathematical co-drafting — external/lived collaboration witness
- `rob-proof-graph-kb` (satisfied) - Rob proof-graph KB + similarity retrieval — external proof graph and embedding retrieval w
- `symbol-grounding` (held) - Symbol-grounding / CT-vision — categorical wiring vocabulary and prototypes; vision remain

Held stars of note:
- `wm-overnight-unsupervised` - status held, declared 0, inferred 2, live 1. The WM runs overnight without supervision — 'I trust the stack to code while I s
- `efe-trustworthy-over-starmap` - status held, declared 1, inferred 0, live 0. EFE does real AIF over the star-map — the WM's pick is an applicable single-cycl
- `ai-passes-prelims` - status held, declared 0, inferred 11, live 2. AI passes UT-Austin prelims — compute-gated held claim above the shipped corpus/

## The 20 live missions with no star

| Mission | Repo | Status | Last commit |
|---|---|---|---|
| `M-war-machine-wiring` | futon5a | open | 2026-10-24 |
| `M-self-improvement-loop` | futon5a | open | 2026-10-23 |
| `M-futonzero-mvp` | futon0 | open | 2026-10-15 |
| `M-categorical-code` | futon5 | open | 2026-10-15 |
| `M-landscape-positioning` | futon0 | open | 2026-10-11 |
| `M-象-cascade` | futon3c | unknown | 2026-10-09 |
| `M-diagramprover` | futon3c | open | 2026-10-02 |
| `M-pheromone-field` | futon6 | open | 2026-09-30 |
| `M-structure-seed-promotion` | futon6 | open | 2026-09-30 |
| `M-superpod-mark3` | futon6 | open | 2026-09-30 |
| `M-symbol-grounding-scaling-plan` | futon6 | open | 2026-09-30 |
| `M-symbol-grounding` | futon6 | open | 2026-09-30 |
| `M-distributed-frontiermath` | futon6 | open | 2026-09-30 |
| `M-efe-bge-followon-actions` | futon6 | open | 2026-09-30 |
| `M-hyperreal-dictionary-planning` | futon6 | open | 2026-09-30 |
| `M-metric-harness` | futon6 | open | 2026-09-30 |
| `M-paper-reverse-morphogenesis` | futon6 | open | 2026-09-30 |
| `M-artificial-stack-exchange` | futon6 | open | 2026-09-30 |
| `M-canon-fingerprint-store` | futon6 | open | 2026-09-30 |
| `M-differentiable-math` | futon6 | open | 2026-09-30 |


## Review corrections (claude-11, 2026-10-25)

- **"Live" is mostly the 2026-09-30 bulk edit.** 275 of the 422 docs have their last commit on 2026-09-30, the day of the bulk rewrite of holes/ (about 330 files), and 119 have no commit at all. Excluding commits that touch more than 25 mission files, **only 13 missions have a commit since 2026-10-01**:
  - futon5a: M-war-machine-wiring, M-self-improvement-loop, M-stack-geometry
  - futon0: M-futonzero-mvp, M-landscape-positioning, M-joe-told-me-about-futon
  - futon5: M-categorical-code
  - futon3c: M-象-2000, M-象-cascade, M-aif-over-the-operator, M-diagramprover, M-autonomous-pattern-lifecycle
  - futon7: M-daily-scan

  Of these 13, one has a declared star (M-stack-geometry → `stack-stereolithography-substrate`). The live-per-star column above should be read with this in mind.
- **The inferred links pass the quote check but are often not about the star.** The check confirms only that the quote exists in the mission; it does not confirm that the quote is about the star. Examples:
  - `ai-passes-prelims` ← M-IRC-stability, M-kangaroo, M-agency-forum;
  - M-aif-over-the-operator → `warm-customer-pays`.

  Treat the inferred links as unreviewed candidates, not as evidence of a link.
- **The "wants" column counts unchecked `- [ ]` task lines,** not stated outcomes. "Admitted" is therefore a statement about tasks.

Recomputed data: ~/notes/experiments/2026-10-25-mission-star-audit/missions-reallast.json
