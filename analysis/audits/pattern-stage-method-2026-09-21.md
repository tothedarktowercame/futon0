# Pattern-stage method and exceptions — 2026-09-21

**595 retrieved IDs labelled; 2,147 joined rank-one retrieval hits covered (100% of accepted joins).** The top 50 IDs cover **801 hits (37.31%)**. These hits attach to **2,144 distinct operator-turn evidence records**, or **63.10% of 3,398 eligible recorded operator turns**. The remaining turns are unassigned; this is not a census of all operator work. Labels are provisional source-text readings, pending Joe’s blind check.

## Kind × stage and the first count integral

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

The unit is a distinct retrieval evidence event, using only its rank-one result. For stage `s`, the reported mass is `M(s) = Σ_retrieval 1[label(rank1).stage = s]`. This is an event-count proxy, **not token cost, elapsed time, or proof that Joe performed the retrieved pattern’s function**. Practice accounts for 1,022 hits, subject for 905, mixed for 220. Subject stages describe content; mixed labels span content and working method. Keep `kind` and `stage-mode` when using these totals.

Two operator-turn records have multiple accepted retrievals (one has three and one has two), so 2,147 hits must not be called 2,147 distinct turns. Both have different retrieved stages. No arbitrary single stage or cost apportionment is imposed. A later cost integral needs an explicit allocation rule and real cost observations.

## Evidence window and attribution

- UTC window: **2026-08-22 inclusive to 2026-09-21T17:19:12.718167Z exclusive**. This is the September 21 extraction snapshot, not the still-unfinished full day.
- Read-only `GET http://localhost:7073/api/alpha/evidence` with `since`, `before`, `limit=1000`, and either `tags=context-retrieval` or `author=joe`. Followed every `next-cursor` via `cursor-at` and `cursor-id`, including short pages. No deep health query.
- Retrieval: 18 pages, 17,773 returned records; 136 duplicate evidence IDs had identical complete records and were collapsed, leaving **17,637**. Joe: six pages, 5,786 records, of which 5,605 are user chat turns/inbound Marimo records and 181 are other records.
- Eligible operator evidence uses the source audit’s author/transport/role conventions. Excludes automatic resume markers, continuation payloads, wake checklists, and agent-origin envelopes. There are **2,205 exact resume-marker turns** in this narrower window: 2,085 classified directly as resume markers and 120 classified earlier as continuation payloads. Two further wake payloads are excluded. The source audit’s 3,024 marker rows concern its wider window, not this one.
- A retrieval query contains at most the first 100 characters of a concatenation of user prompt and response preview; its retrieval counter is not the user turn ID. Join on the **same session**, a preceding turn within **six hours**, and a **unique normalized text-prefix match**. Whitespace is collapsed; complete CURRENT TURN headers are stripped from operator text. Short prompts can be followed by response text in the retrieval query.
- 1,720 hits use the plain prefix match. Another 427 have an explicit `From: joe` / `Origin: operator` envelope and a nonempty payload suffix matching exactly one candidate. The suffix can be very short. These are **heuristic associations, not foreign-key joins**; the six-hour bound and truncated text can miss or misassociate turns. Empty payloads and multiple eligible candidates are not guessed. No nearest-turn fallback.

| Retrieval disposition | Count |
|---|---:|
| Accepted unique operator-prefix join with exactly one rank-one result | 2,147 |
| Matched explicit resume marker | 687 |
| Matched continuation payload | 2 |
| Every possible prefix match is automatic/excluded | 955 |
| Ambiguous prefix with an eligible candidate | 323 |
| No matching operator turn in the bounded window | 13,523 |
| **Unique retrieval records** | **17,637** |

Unmatched queries are predominantly other traffic: 10,759 say `Origin: agent`, 2,330 say `harness`, 182 truncate the origin to `a`, 44 say `operator`, and 208 have no recoverable origin. Those 13,523 are **not** asserted to be missing operator retrievals. The 323 ambiguous cases are also not assigned a stage. Turn coverage does not imply every eligible turn had a retrieval.

The attribution procedure extends the prefix-matching approach in [PILOT-redirection-geometry](../../../futon2/holes/labs/wm-contract/PILOT-redirection-geometry-2026-09-20.md) and the exclusions in [SOURCES-work-records §3](SOURCES-work-records-2026-09-21.md). The executable join is [pattern_stage_evidence.py](pattern_stage_evidence.py). [The manifest](pattern-stage-manifest-2026-09-21.json) records page metadata, snapshot digests, and denominators; [the join ledger](pattern-stage-joins-2026-09-21.jsonl) records every retrieval’s disposition, candidate/matched evidence IDs, query digest, and rank-one IDs, without copying private prompt text.

## Labels, source resolution, and rubric limits

[Per-pattern EDN](pattern-stages-2026-09-21.edn) contains `id`, `path`, `hits`, `kind`, `stage`, optional `node`, `confidence`, and a rationale quoting the source. It additionally records source header line, whole-file SHA-256, quotation field/text, stage mode, and exceptions. Labels came from the source’s IF/THEN/BECAUSE text and, where these were generic or absent, its conclusion and embedded argument. They were not inferred from filenames. The source checkout at inspection was futon3 `7fc6a05004aa3e601e53eba633d62213da0af4b2`; individual file hashes are the exact content authority. This is present-source coding of historical retrievals, not a reconstruction of every pattern version at retrieval time.

- **53 IDs are in `.multiarg` files**, and `futon-stack/argument` is in `futon3/holes/futon-stack.flexiarg`. Header IDs, including `@arg`, determine identity. A filename-only join would lose real hits. For IDs with both standalone and multiarg copies, the standalone `.flexiarg` text was used; variants are not additional hits.
- **Two retired IDs** resolve through git history: `math-informal/structural-obstruction-as-theorem` at `a89618a2378d684a602b33b1ef036474170da72f` (subsequently moved into math-strategy), and `math-strategy/clarification-meta` at `92f68a72eeebaa40b63150c4a98895eb05852da4`. The latter explicitly describes a **meta-tag, not a pattern** and receives subject/none. Their `source-revision` makes the otherwise absent paths resolvable.
- `iiching/exotype-XYZ` is the excluded `TEMPLATE.flexiarg.txt`, yet received a hit. All 20 retrieved iiching records are configuration/lookup content and receive **subject/none**; a named encoding is not evidence of a performed control stage.
- **39 problem-directory entries** include bare problem statements and some concrete mechanisms. A problem about R5 is topical evaluate even when its THEN only supplies a rationale landing point. Six section-level problem stubs receive none because their text does not warrant one primary stage. Their presence is not evidence that the stated gap has been resolved.
- **Three sources lack usable IF/THEN/BECAUSE clauses**: `capability/capability-vocabulary-v0`, `futon-theory/futonic-logic`, and `social/ARGUMENT`. These explicitly use a CONCLUSION quotation and carry `missing-if-then-because`; the requested clause-quotation rule cannot literally fit them. The enrichment argument uses an embedded THEN instead.
- Math formalisation techniques are **subject**, as the rubric specifies, even when written imperatively. General proof-work planning or audit disciplines can be practice or mixed. Specific design decisions are generally subject rather than practice merely because they say “implement”.
- I Ching, taiji, and contemplative material requires interpretation across domains (33 flagged IDs); most are mixed with medium confidence. `iching/hexagram-61-zhongfu` and `liberation/noble/right-concentration` receive none. Futonic logic/composition also crosses the whole loop and has no defensible single stage.
- **R8 is omitted from the requested stage-to-node list.** `aif/free-energy-as-tick-scalar` is topical evaluate, medium confidence, R8, with the mismatch flagged. R17-related structure learning is labelled believe by its operation without inventing an unrequested node mapping. Optional nodes were supplied only where the source clearly names the function.

Confidence distribution: 476 high, 114 medium, five low. High means the source-to-rubric reading is clear; it does not certify retrieval relevance, operator intent, or empirical validity of the source’s claims. There are 31 `none` labels, accounting for 65 hits (3.03%).

## Remaining library: directory priors only

[Directory-prior EDN](pattern-stage-dir-priors-2026-09-21.edn) has **121 rows**, one per non-hidden directory below `futon3/library`, excluding the root and `.spider` metadata trees. **111** directly contain `.flexiarg` files; ten are containers, scratch directories, or multiarg-only directories. This explains why the packet’s “120 directories” is not the on-disk count under this definition.

The **1,404 `.flexiarg` files** are confirmed. **538 current files** supply hit labels; **866 remaining files** receive no individual label. Priors are low-confidence directory summaries grounded in quoted representatives (normally two). They are not assigned to all members, are not used for hit totals, and are not recommendations to apply a pattern. Directly empty/container/scratch directories receive none rather than borrowing a child’s stage.

## Joe’s blind check

Open [pattern-stages-20260921.py](../../../marimo-zone/notebooks/pattern-stages-20260921.py) in Marimo. This permitted sibling was chosen while codex-14 was actively modifying the business-ideas notebook; that notebook was not edited here.

**Seed 20260921**, Python `random.Random`, sorted-ID strata, ten sampled without replacement from each of practice/subject/mixed, then shuffled with the same generator. Sampling is not weighted by hits. [Blind items](pattern-stage-blind-items-2026-09-21.json) contain titles, conclusion/IF/THEN, and source pointers, but no assigned kind/stage/hit counts. [The separate answer key](pattern-stage-blind-key-2026-09-21.json) is read by the notebook only after all cards have both human labels.

The dropdown form starts unselected. Explicit submission atomically saves to `analysis/audits/pattern-stage-joe-labels-2026-09-21.json`; no synthetic Joe labels are shipped. Partial saves survive reload and still show **awaiting labels**. Once complete, the notebook shows kind and stage confusion matrices (machine rows, Joe columns), exact agreement, and unweighted Cohen’s kappa; a constant-marginal degenerate kappa is explicitly undefined. This measures agreement on the stratified sample, not population accuracy or hit-weighted agreement.

## Replay and validation

From `futon0`, with Python 3.10+ and `edn-format==0.7.5` (or `uv run --with edn-format`):

```sh
uv run analysis/audits/pattern_stage_evidence.py /tmp/pattern-stage-snapshot --fetch
uv run --with edn-format python analysis/audits/validate_pattern_stages.py
uv run --with edn-format python -m unittest discover -s analysis/audits -p test_pattern_stages.py
../marimo-zone/.venv/bin/marimo check ../marimo-zone/notebooks/pattern-stages-20260921.py
```

Replay defaults to the recorded cutoff; subsequent service backfills may change the snapshot digests. Raw snapshot/queries stay in the chosen temporary directory. The published ledger fixes the evidence IDs used for these totals. Stage assignments are manual judgments preserved in the EDN, not an automatic classifier to be silently rerun.

Validated: all 595 source hashes, exact quotations and header lines (including git-history sources); every hit and unique-turn denominator; all 121 directories; all 30 seeded blind selections; five focused tests covering real join exclusions/rank uniqueness/conflicting duplicates and human-save/agreement behavior. The notebook passes `marimo check` and was executed by HTML export in the initial, awaiting-labels state. No Lean, library, or futon1b data was modified. Markdown, JSON, EDN data and Python only: Clojure/Lisp gates are not applicable.
