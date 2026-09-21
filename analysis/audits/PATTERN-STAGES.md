# Pattern stages — 2026-09-21

**Repaired extraction: 689 retrieved IDs labelled, covering all 2,882 accepted rank-one hits.** This adds 94 manually read patterns and 735 hits. The top 50 account for **1,023 hits (35.50%)**. Existing kind/stage assignments are unchanged; all labels remain provisional pending Joe’s blind check.

| Stage | Practice | Subject (topical) | Mixed | Pattern IDs | Hits | Hit share |
|---|---:|---:|---:|---:|---:|---:|
| perceive | 28 | 32 | 7 | 67 | 301 | 10.44% |
| believe | 65 | 64 | 17 | 146 | 535 | 18.56% |
| evaluate | 11 | 23 | 9 | 43 | 164 | 5.69% |
| select | 40 | 34 | 14 | 88 | 334 | 11.59% |
| act | 23 | 45 | 21 | 89 | 391 | 13.57% |
| assurance | 91 | 46 | 9 | 146 | 689 | 23.91% |
| coordination | 39 | 25 | 12 | 76 | 391 | 13.57% |
| none | 0 | 33 | 1 | 34 | 77 | 2.67% |
| **Total** | **297** | **302** | **90** | **689** | **2,882** | **100.00%** |

This count integral sums retrieval events by inherited stage, **not token cost, hours, or proof that Joe performed the pattern**. Subject stages are topical. Practice contributes 1,384 hits; subject 1,211; mixed 287. The hits attach to **2,879 distinct operator turns, 84.73% of 3,398 eligible recorded turns**. Two turns have multiple retrievals with different stages; unassigned turns remain unclassified.

Window: `2026-08-22` inclusive to **`2026-09-21T17:19:12.718167Z` exclusive**, unchanged. The guarded whole-window extraction took **383.77 seconds (6m24s)**: 21 retrieval pages / 20,306 rows, six Joe pages / 5,786 rows. Both concatenated streams are strictly descending and duplicate-free. No day-window substitute was used. The same-session, unique text-prefix join and six-hour bound are unchanged: 2,353 ordinary prefix joins and 529 truncated operator-envelope joins. These are heuristic associations, not foreign keys.

Of 20,306 retrieval records, 2,882 join, 2,132 match automatic/excluded traffic, 356 are ambiguous, and 14,936 are unmatched. The operator census remains 5,605 user records, excluding 2,205 resume/continuation markers and two wake payloads, leaving 3,398. Neither missing joins nor retrieval relevance is inferred away.

Files:

- [Per-pattern labels](pattern-stages-2026-09-21.edn): ID/path/line/hash, hits, kind, stage, optional node, confidence and quoted rationale.
- [Directory priors](pattern-stage-dir-priors-2026-09-21.edn): 121 directories, including 111 directly containing `.flexiarg`. Of 1,404 files, 625 supply hit labels; **779 remaining files get directory priors only**. Priors do not contribute to hits.
- [Original method record](pattern-stage-method-2026-09-21.md), [current manifest](pattern-stage-manifest-2026-09-21.json), [evidence-ID joins](pattern-stage-joins-2026-09-21.jsonl), and [transcript coverage ledger](pattern-stage-transcript-coverage-2026-09-21.json).

Rubric exceptions: 60 IDs resolve to multiarg sources, one outside the library, two retired IDs and one excluded template. Fifty problem-directory entries describe content or gaps rather than performed work. Configuration records and broad theory/metaphors can span or fall outside the rubric: **34 none labels, 77 hits**. Three sources lack IF/THEN/BECAUSE and explicitly quote their conclusion. Math techniques remain subject. R8 and R17 are absent from the supplied node-to-stage mapping and explicit uses are flagged. Confidence: 538 high, 140 medium, 11 low; this concerns source interpretation, not retrieval attribution.

Joe’s [30-card blind notebook](../../../marimo-zone/notebooks/pattern-stages-20260921.py) and cards are unchanged. **Seed 20260921, ten per kind, sampled from the original 595-ID population.** That population is now frozen in the separate answer key so adding patterns cannot silently resample the exercise. No Joe labels were fabricated. The existing [Minard regeneration notebook](../../../marimo-zone/notebooks/minard-operator-work-20260921.py) rereads the current EDN and ledger without needing a notebook edit.

## Re-extraction after futon1b 5d9938c

Joe restarted the services with the pagination repair live. The guarded whole-window downloader now completes under the original cutoff. All original retrieved identities and accepted joins survive; 2,669 previously skipped retrievals add 735 accepted hits. Newly hit source texts were read and labelled with the same rubric, and every source hash, header line and quotation resolves.

| Stage | Old hits | Old share | Repaired hits | Repaired share | Change (pp) |
|---|---:|---:|---:|---:|---:|
| perceive | 224 | 10.43% | 301 | 10.44% | +0.01 |
| believe | 409 | 19.05% | 535 | 18.56% | -0.49 |
| evaluate | 128 | 5.96% | 164 | 5.69% | -0.27 |
| select | 234 | 10.90% | 334 | 11.59% | +0.69 |
| act | 307 | 14.30% | 391 | 13.57% | -0.73 |
| assurance | 491 | 22.87% | 689 | 23.91% | +1.04 |
| coordination | 289 | 13.46% | 391 | 13.57% | +0.11 |
| none | 65 | 3.03% | 77 | 2.67% | -0.36 |

**The post–September 13 collapse is removed.** For a like-for-like Claude comparison, the numerator below is a transcript UUID whose payload uniquely matches a stage-labelled store turn, in the same session and within five minutes (exact whitespace-normalized text, unique in both directions). This stricter cross-source confirmation does not alter the six-hour retrieval join. Total joined store turns include Codex and notebook traffic and are shown separately; they must not be divided by a Claude-only denominator.

| UTC date | Retrieval emissions | Old joined store turns | Repaired joined store turns | Covered Claude transcript turns | Claude transcript denominator | Coverage |
|---|---:|---:|---:|---:|---:|---:|
| 09-08 | 1244 | 80 | 80 | 79 | 85 | 92.9% |
| 09-09 | 1730 | 63 | 63 | 48 | 53 | 90.6% |
| 09-10 | 2066 | 24 | 24 | 6 | 6 | 100.0% |
| 09-11 | 528 | 57 | 57 | 0 | 0 | n/a |
| 09-12 | 359 | 53 | 53 | 36 | 39 | 92.3% |
| 09-13 | 301 | 4 | 21 | 16 | 17 | 94.1% |
| 09-14 | 226 | 7 | 84 | 80 | 92 | 87.0% |
| 09-15 | 456 | 8 | 109 | 70 | 86 | 81.4% |
| 09-16 | 608 | 9 | 102 | 67 | 73 | 91.8% |
| 09-17 | 291 | 19 | 140 | 129 | 136 | 94.9% |
| 09-18 | 231 | 20 | 113 | 111 | 121 | 91.7% |
| 09-19 | 392 | 25 | 101 | 90 | 99 | 90.9% |
| 09-20 | 525 | 30 | 157 | 139 | 149 | 93.3% |
| 09-21 | 324 | 31 | 61 | 51 | 54 | 94.4% |

The denominator follows [claude-5’s census](claude_operator_census.py), commit `870497b`: all current and pre-compact Claude logs, user rows containing `From: joe` and `Origin: operator`, excluding `resumed: parked` and `WAKE CHECKLIST`, deduplicated by UUID. [The coverage script](pattern_stage_coverage.py) additionally applies the **full timestamp cutoff** (the original census CLI compares dates only). Thus September 21 has 54 transcript turns before 17:19:12, rather than the growing whole-day count. Zero on September 11 is Claude-only; the store contains other operator surfaces that day.

Confirmed transcript coverage is **169/183 = 92.35% on September 8–12**, and **753/827 = 91.05% on September 13–21**. September 15 is lower at 81.4%, but the earlier persistent 10–20% collapse is absent. Remaining unmatched/ambiguous transcript records, missing retrievals and conservative joins remain visible. This is a lower bound under the stated exact-match rule, not proof of complete telemetry.

The [published Minard figure](https://zone.hyperreal.enterprises/wip/audits/minard-operator-work-2026-09-21.html) was regenerated and the obsolete pagination-failure warning removed (HTTP 200 verified). Its provisional-label, topical-subject, turn-count-not-cost and incomplete-coverage caveats remain, with coverage updated to 84.7%. Light and dark renderings were inspected; annotations and all seven direct labels are legible. The count scale and lower-panel forensic data are unchanged.

Replay the comparison from the private raw snapshot with:

```sh
../marimo-zone/.venv/bin/python analysis/audits/pattern_stage_coverage.py /tmp/codex16-pattern-stages-repaired --output /tmp/pattern-stage-coverage.json
../marimo-zone/.venv/bin/python analysis/audits/validate_pattern_stages.py
../marimo-zone/.venv/bin/python analysis/audits/minard_operator_work.py
```

Source stores and the pattern library remained read-only. Validation covers all 689 source citations, counts and frozen blind cards, six pattern tests and the real-data Minard relabelling test, plus browser hover, keyboard, tables, dark palette and mobile containment. No Clojure/Lisp was touched.

## Coverage drop from 2026-09-13

**Historical diagnosis, superseded by the repaired extraction above.** The following records the pre-repair failure and stop decision; its counts and statements that work was blocked apply to that earlier snapshot.


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
