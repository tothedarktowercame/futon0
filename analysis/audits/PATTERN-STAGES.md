# Pattern stages — 2026-09-21

**595 retrieved IDs labelled, covering all 2,147 accepted rank-one hits.** The top 50 account for **801 hits (37.31%)**. These are provisional pattern labels, pending Joe’s blind check.

| Stage | Practice | Subject (topical) | Mixed | Pattern IDs | Rank-one hits | Hit share |
|---|---:|---:|---:|---:|---:|---:|
| perceive | 24 | 29 | 6 | 59 | 224 | 10.43% |
| believe | 56 | 54 | 14 | 124 | 409 | 19.05% |
| evaluate | 11 | 22 | 9 | 42 | 128 | 5.96% |
| select | 29 | 30 | 12 | 71 | 234 | 10.90% |
| act | 20 | 38 | 17 | 75 | 307 | 14.30% |
| assurance | 74 | 43 | 9 | 126 | 491 | 22.87% |
| coordination | 35 | 22 | 10 | 67 | 289 | 13.46% |
| none | 0 | 30 | 1 | 31 | 65 | 3.03% |
| **Total** | **269** | **268** | **78** | **595** | **2,147** | **100.00%** |

This first count integral sums retrieval events by inherited stage. It is **not token cost or hours**, nor proof that Joe performed the retrieved pattern. Subject stages are topical. Practice contributes 1,022 hits; subject 905; mixed 220. The hits attach to **2,144 distinct operator turns**, **63.10% of 3,398 eligible recorded turns**; two turns have multiple retrievals with different stages. Unassigned turns are not silently classified.

Window: August 22 through the **September 21, 17:19:12 UTC extraction snapshot**. A same-session, unique text-prefix join links retrievals to preceding operator turns, with a six-hour bound. Truncated envelope matches remain heuristic. Of 17,637 unique retrieval records, 2,147 join, 1,644 are automatic/resume matches, 323 are ambiguous, and 13,523 are unmatched (mostly agent/harness traffic). The narrower window contains 2,205 exact resume-marker turns, not the source audit’s wider-window 3,024. All automatic resume/wake candidates are excluded from hit totals.

Files:

- [Per-pattern labels](pattern-stages-2026-09-21.edn): source ID/path/line/hash, hits, kind, stage, optional node, confidence, and quoted rationale.
- [Directory priors](pattern-stage-dir-priors-2026-09-21.edn): **121 directories**, including 111 containing `.flexiarg` files. Of **1,404 files**, 538 supply hit labels; **866 remaining files get directory priors only**. Priors are weak, representative-based, and excluded from hit totals.
- [Method, exclusions, exceptions, and replay commands](pattern-stage-method-2026-09-21.md), [manifest](pattern-stage-manifest-2026-09-21.json), and [evidence-ID join ledger](pattern-stage-joins-2026-09-21.jsonl).

Rubric exceptions: 53 IDs resolve to multiarg sources, one outside the library, two to retired IDs in git history, and one to an excluded template. Thirty-nine problem-directory entries describe content or gaps rather than performed work. Configuration records, broad theory/problem stubs, and some contemplative material do not fit one stage: **31 none labels, 65 hits**. Three sources lack IF/THEN/BECAUSE clauses and explicitly quote their conclusion instead. Math techniques are subject as requested. R8 is absent from the supplied node mapping and is flagged. The method document lists the specific cases. Confidence: 476 high, 114 medium, five low; this assesses the source-to-rubric reading, not retrieval attribution.

Joe’s blind check is in [the sibling Marimo notebook](../../../marimo-zone/notebooks/pattern-stages-20260921.py), avoiding codex-14’s concurrent business-ideas notebook edits. **Seed 20260921; ten patterns per kind; 30 total.** Cards hide the assigned labels and hit counts. Explicit submission saves `pattern-stage-joe-labels-2026-09-21.json`; partial labels persist. Confusion matrices and Cohen’s kappa remain **awaiting labels** until all cards are complete. No synthetic Joe labels are supplied.

Validated every source hash, quotation and header line, the hit/turn totals, directory coverage, and seeded sample. Five focused tests pass; `marimo check` passes and HTML export executes the initial awaiting-labels view. futon1b and the library remained read-only. No Clojure/Lisp was touched.
