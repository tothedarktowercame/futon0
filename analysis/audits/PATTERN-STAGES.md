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

## Coverage drop from 2026-09-13

**Correction: the thinning is a retrieval-download coverage failure, not evidence of a drop in Joe’s work.** The whole-window endpoint violates its newest-first pagination contract: its first 1,000-result page advances the cursor into September 12 while omitting valid newer retrieval records. The prefix join never saw those records. Earlier totals above and the Minard’s shares describe the incomplete downloaded cohort; they must not be used as a time-varying work-volume census.

Same fixed cutoff as before: `2026-09-21T17:19:12.718167Z`. Read-only day-bounded requests to `:7073/api/alpha/evidence`, `tags=context-retrieval`, `limit=1000`, following every cursor, return the counts below. These diagnostic queries are **not a certified replacement corpus**. “Claude sessions” means sessions appearing in the frozen eligible operator records with a `claude…` turn ID; those sessions also carry agent/harness traffic. Emissions use retrieval dates; published joins use operator-turn dates. Joe’s UUID-deduplicated transcript denominator is a different source and is not substituted here.

| UTC day | Retrievals returned by day query | In operator Claude sessions | In original whole-window download | Published joined hits |
|---|---:|---:|---:|---:|
| 09-10 | 2066 | 21 | 2066 | 24 |
| 09-11 | 528 | 0 | 528 | 57 |
| 09-12 | 359 | 173 | 359 | 53 |
| 09-13 | 301 | 37 | 125 | 4 |
| 09-14 | 226 | 145 | 24 | 7 |
| 09-15 | 456 | 208 | 39 | 8 |
| 09-16 | 608 | 319 | 70 | 9 |
| 09-17 | 291 | 267 | 41 | 19 |
| 09-18 | 231 | 222 | 45 | 20 |
| 09-19 | 392 | 259 | 98 | 25 |
| 09-20 | 525 | 385 | 102 | 30 |
| 09-21 | 324 | 186 | 141 | 31 |

**Reproduction and cause.** The original first page has 101 adjacent newest-first order violations and returns cursor `2026-09-12T18:41:31.672414246Z / e-9f058c82-3135-4596-a8ed-c41d0e32c12c`. A fresh identical whole-window request has 103 violations and cursor `2026-09-12T18:34:49.984301693Z / e-191fdb4e-f7b5-43b7-8044-38059f10b5bf`. For example, it returns `16:25:08.380610343Z` followed by the newer `17:18:15.930813301Z` on September 21. Continuing backwards from September 12 cannot recover the omitted September 13–21 records. The original 136 duplicate IDs were another pagination warning that the downloader wrongly tolerated; identical bodies made deduplication harmless to those IDs, but did not establish completeness.

The service journal independently records **359, 301, and 226 successful retrieval log lines** on September 12, 13, and 14 respectively, matching the day-query counts. There are zero `[context] retrieval error:` lines in that three-day slice (`journalctl --user -u futon3c-zone.service --since 2026-09-12 --until 2026-09-15 --grep='\[context\]'`). Thus retrieval actually ran during the apparent collapse.

**Three concrete missed joins.** Each retrieval below is absent from the original download, present in the day query, in the same session as its turn, and a unique ordinary prefix match under the unchanged six-hour rule. No envelope parsing change, session reassignment, or looser time bound is needed. Newlines are shown as `\n`.

| Turn date / text start | Retrieval query (entire field) |
|---|---|
| 2026-09-14 — `So, it's an interesting question about how the code would run b/c yes, that's a separate layer from the REPL, e.g., I think it wou` | `So, it's an interesting question about how the code would run b/c yes, that's a separate layer from ` |
| 2026-09-17 — `joe: system: [interrupted]\n\nsystem: [interrupted]\n\n> So, with the DAG, in Marimo, I see very little green, but what I'd like to kn` | `joe: system: [interrupted]\n\nsystem: [interrupted]\n\n> So, with the DAG, in Marimo, I see very little ` |
| 2026-09-20 — `Well why don't you communciate with claude-4 because we have until midnight AOE, which is in about 12 hours.  Not that I plan to s` | `Well why don't you communciate with claude-4 because we have until midnight AOE, which is in about 1` |

- 2026-09-14: turn `emacs-997d192e69d87aaac20a9d9b920857d3` at `2026-09-14T23:04:44.318801408Z` → retrieval `e-0155bff8-4f5b-444e-b1de-4f6a0b067922` at `2026-09-14T23:05:56.695063631Z`; shared session `50b99b03-3e13-4853-81d0-c5efab15ef13`.

- 2026-09-17: turn `emacs-aafd006dfe4524f1c66d49f33698e16e` at `2026-09-17T22:56:07.809771034Z` → retrieval `e-36f0332d-c82c-43fb-b9f2-9fa15c108b1d` at `2026-09-17T22:58:48.345685221Z`; shared session `af24caa1-54d3-4f19-9d73-c8183eb9cb65`.

- 2026-09-20: turn `emacs-86747086725f9f3b225ae8292629c200` at `2026-09-20T23:43:00.166503728Z` → retrieval `e-3d108931-acfa-4451-be47-c046b01f7acb` at `2026-09-20T23:44:13.596365796Z`; shared session `d158cebc-06aa-4763-8704-e216a5a39f5c`.

**Code and date findings.** `futon3c/dev/futon3c/dev.clj:1000` retrieves only after a completed turn; line 1011 requires nonempty search results, line 1015 emits the evidence, and line 968 clips its query to 100 characters. Claude cold and warm callers are at lines 3838 and 4041; Codex’s is at 4720. The core retrieval path dates to `a6d4e8725` (2026-04-12); the warm-turn call to `c0f47dbfc` (2026-06-11). No September 13 change to this file exists in the inspected history. No particular surface or turn shape is shown to have stopped triggering retrieval. September 13 is the first page’s artificial coverage boundary, **not an established deployment date**.

The failed contract belongs to `futon1b/futon1b_evidence.clj`: `page-query` declares descending time/ID ordering at lines 264–268; `bounded-window` takes the first LIMIT identities and advances from the last at lines 328–335; hydration promises to preserve projected order at lines 105–122. These keyset changes date to `4cd17bcb` / `be913d6` on 2026-08-23. The HTTP observation proves an ordering/completeness violation; it does **not** isolate whether the running projection query, hydration, or loaded implementation diverges from that source. No unsupported September 13 code-change attribution is made.

**Smallest repair proposal:** restore global `(evidence/at, id)` descending order **before LIMIT and cursor selection** in the served evidence reader. Add a real-store regression with more than 1,000 matching identities across days: concatenated pages must be strictly ordered, duplicate-free, and equal the bounded full identity set; test the currently failing whole-window query too. Sorting an already limited page or fetching smaller days does not repair that invariant. Then re-extract both operator and retrieval records, inspect newly hit patterns, regenerate labels and shares, and republish the figure.

The downloader now refuses non-monotone pages/cursors rather than producing another apparently complete snapshot. Its guard rejected the actual fresh 1,000-row HTTP response; six focused tests pass, including the observed bad adjacent pair. **No emitter/store code was changed; no stage counts were regenerated.** The figure was regenerated only to add a prominent coverage-failure warning and republished with its original, explicitly incomplete counts. A corrected quantitative figure is blocked on the server’s pagination invariant; substituting the diagnostic day queries would be a workaround, contrary to the workspace rule.
