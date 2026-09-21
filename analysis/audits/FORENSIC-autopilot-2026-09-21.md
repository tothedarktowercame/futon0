# Forensic discovery: operator gaps and autonomous builds, 2026-09-21

Read-only discovery by codex-15 for claude-5 / Joe, job `invoke-1790011315001-23008-4d9c0ff4`. Nothing was deleted, moved, reverted, run as a campaign, or written to a store. Only this report is committed. `/tmp/autopilot15/` contains scratch projections, not a new evidence authority.

**No candidate is identified as Joe's unwanted build without his confirmation.** The largest observed-usage gap is September 10's topology campaign; the second is the September 5–6 gap with an explicit preceding statement about air travel; the third is an August 27–28 topology scaffold campaign. Retained instructions show authorization for continuing programmes, but that does not establish that every artifact produced was requested, useful, or wanted. Nor does the absence of a recorded turn prove physical absence. These are candidates for Joe to recognize.

A useful counterexample to simply labelling the output unrequested: the August 27 window starts with Joe asking for continuous work through chained parks and Agency bells. Conversely, the September 10 worker prompt explicitly demands a nonempty replacement portfolio and continued foundation-building. That is a concrete source of momentum worth inspecting, rather than an inferred model motive.

## Sources and counting limits

Window requested: July 21 through September 21 inclusive, UTC. Observation end is 2026-09-21T17:23:36Z. September 21 is partial. Operator evidence was freshly paged by GET from `http://localhost:7073/api/alpha/evidence`, with `author=joe&since=2026-07-21&before=2026-09-22&limit=1000`, header `Accept: application/json`, following `next-cursor.at/id` even on short pages. No deep-health endpoint was called.

Qualified operator rows are `body.event=chat-turn, body.role=user`, plus Marimo `transport=marimo,direction=inbound`. Exclude the literal `--- resumed: parked dependencies complete` anywhere in text, and explicit `Origin: agent|harness` or bell/whistle/auto-bellback surfaces if present. This fresh read excluded 3030 resume rows (the earlier SOURCES snapshot had 3,024) and retained 6394 operator-shaped turn rows. Admin/session-start rows are not turns. All retained turn timestamps delimit maximal gaps of at least six hours; activity is assigned within each gap. Marimo rows matter: they split some gaps that an Emacs-only calculation would erroneously retain. The lexical exclusion is not a complete human-authorship detector; other automatic inputs or missing operator channels can still bias these gaps. Treat “no genuine turn” as “none found after these explicit exclusions,” not a certified physical absence.

Commit counting follows the audit's unique-SHA / `git log --all` / UTC **committer** date rule, filtering after walking history. Canonical repositories are counted once; linked worktrees are not additional repositories. The initial audit's 20-repository list omits **apm-lean**, where most of these builds live, so I extended it with top-level `/home/joe/code/*` directories possessing an actual `.git` directory. Mathlib remains restricted to `DarkTower/WarMachine/`, as in the cited counting rule. This is explicitly not a census of every arbitrary nested repository or other host. Unavailable `futonY` is missing, not zero. Commit counts include merges and supervisor bookkeeping; they are **all observed commits in the gap**, not a count proven to have been authored by Codex. Commit-only candidates below are retained but labelled attribution-unproved.

Repository scope: `futon0, futon1, futon1a, futon1b, futon1bi, futon2, futon2a, futon3, futon3a, futon3b, futon3c, futon4, futon5, futon5a, futon6, futon7, futon7a, futonY, mathlib4, p4ng, chipwits-forth, apm-lean, voxterm, expenses-jac, marimo-zone, mfuton-share, codex, ukrn-services-simulation, nnexus, expenses-hel, storage, 18_Category_theory_homological_algebra, FloWrTester, mathse-xtdb-benchmark, nlab-content, easyeffects, mmca-clj, powerbi-tui, xtdb, gflownet, mmca, orbook.github.io, 18_Category_theory_homological_algebra.upstream, chatgpt-tui, kissat, filings`.

Codex source: `/home/joe/.codex/sessions/**/*.jsonl`, `event_msg` / `payload.type=token_count`. Each event contributes **last_token_usage input + output**, never its cumulative total. Exact duplicate events and unchanged consecutive cumulative snapshots are skipped; a decreased cumulative counter starts another segment rather than subtracting previous work. No cumulative counters are summed. Cache input is a subset of input, and reasoning output is a subset of output; neither is added again. The scan saw 1917 unchanged snapshots, 10793 cumulative resets and 613 no-info token events across retained files. These diagnostics cover the scanned logs, not just the three windows.

There is also a concrete total-field anomaly: September 10's summed `last.total_tokens` exceeds summed input+output by 258,400. For example the codex-12 rollout at 00:30:38.308Z records zero input/output but `last.total_tokens=82861`. I therefore report component totals, not that inconsistent total field. This is observed token telemetry, **not invoiced dollars or unique text**: repeatedly supplied cached context contributes heavily. Last usage is assigned at its event timestamp; calls crossing a boundary cannot be apportioned internally. Identical copied histories across different session identities have not been independently authenticated against provider billing. Local rollouts begin August 4 in this source; “no retained usage” before then is not free work. No complete historical prices/model/tier/billing join is available, so no dollar estimate is invented.

Agent names come from retained rollout routing headers. A reused rollout may name more than one seat; both labels are preserved, rather than inventing a time-specific attribution. Frame solver/proctor/scribe seats are Codex activity too. Unknown identities remain counted as unmapped sessions. Window totals include **all** observed Codex work, not only the build highlighted in its deep dive. Claude/ZAI costs are not included.

## All observed activity-bearing gaps, sorted by Codex usage

`NR` = no retained qualifying token event; it does not mean no usage. Rows with only commits have unproved Codex attribution. Hours use full timestamp precision; displayed timestamps are rounded to seconds. The table is a candidate superset, not a claim that every listed commit was Codex's.

| Rank | Start UTC | End UTC | Hours | Codex input+output tokens | Commits by repository | Active rollout seats / limits |
|---:|---|---|---:|---:|---|---|
| 1 | 2026-09-10T00:26:23Z | 2026-09-10T11:16:58Z | 10.843 | 831,983,803 | apm-lean: 640 | codex-1, codex-10, codex-11, codex-12, codex-13, codex-14, codex-3, codex-6, codex-8, f211-proctor, f211-promotion-proctor, f211-scribe, f211-solver, f212-proctor, f212-solver, f213-proctor, f213-solver |
| 2 | 2026-09-05T12:03:45Z | 2026-09-06T13:56:21Z | 25.877 | 639,137,908 | apm-lean: 182; futon2: 174; futon3: 16; futon3c: 21; mathlib4: 19; p4ng: 45 | codex-17, codex-18, codex-9, f100-proctor, f100-promotion-proctor, f100-scribe, f100-solver, f101-proctor, f101-solver, f102-proctor, f102-solver, f103-proctor, f103-solver, f104-proctor, f104-promotion-proctor, f104-scribe, f104-solver, f105-proctor, f105-promotion-proctor, f105-scribe, f105-solver, f106-proctor, f106-solver, f107-proctor, f107-solver, f108-proctor, f108-promotion-proctor, f108-scribe, f108-solver, f109-proctor, f109-solver, f88-proctor, f88-promotion-proctor, f88-scribe, f88-solver, f89-proctor, f89-promotion-proctor, f89-scribe, f89-solver, f90-proctor, f90-promotion-proctor, f90-scribe, f90-solver, f91-proctor, f91-promotion-proctor, f91-scribe, f91-solver, f92-proctor, f92-promotion-proctor, f92-scribe, f92-solver, f93-proctor, f93-solver, f94-proctor, f94-promotion-proctor, f94-scribe, f94-solver, f95-proctor, f95-promotion-proctor, f95-scribe, f95-solver, f96-proctor, f96-promotion-proctor, f96-scribe, f96-solver, f97-proctor, f97-promotion-proctor, f97-scribe, f97-solver, f98-proctor, f98-promotion-proctor, f98-scribe, f98-solver, f99-proctor, f99-promotion-proctor, f99-scribe, f99-solver; 36 unmapped sessions |
| 3 | 2026-08-27T20:37:44Z | 2026-08-28T04:56:28Z | 8.312 | 620,559,113 | apm-lean: 102; futon3c: 14 | codex-1, codex-10, codex-12, codex-18, codex-2, codex-22, codex-3, codex-8, f49-proctor, f49-promotion-proctor, f49-solver |
| 4 | 2026-08-26T23:10:08Z | 2026-08-27T06:05:56Z | 6.930 | 574,011,934 | apm-lean: 61; futon3c: 1 | codex-10, codex-12, codex-18, codex-8, f46-proctor, f46-promotion-proctor, f46-solver |
| 5 | 2026-08-31T23:24:01Z | 2026-09-01T06:03:13Z | 6.653 | 530,311,602 | apm-lean: 41; futon2: 338; futon3c: 23; mathlib4: 32; p4ng: 58 | codex-1, codex-17, codex-18, codex-2, codex-22, codex-3, f70-proctor, f70-promotion-proctor, f70-scribe, f70-solver, f71-proctor, f71-promotion-proctor, f71-scribe, f71-solver, f72-proctor, f72-promotion-proctor, f72-scribe, f72-solver, f73-proctor, f73-promotion-proctor, f73-scribe, f73-solver, wm-evidence, wm-nouns, wm-organization, wm-verbs |
| 6 | 2026-09-10T18:31:57Z | 2026-09-11T00:35:41Z | 6.062 | 478,942,443 | apm-lean: 444; futon2: 3; futon3c: 33; voxterm: 1 | codex-1, codex-10, codex-11, codex-12, codex-13, codex-14, codex-15, codex-17, codex-18, codex-3, codex-6, f217-proctor, f217-promotion-proctor, f217-scribe, f217-solver, f218-proctor, f218-promotion-proctor, f218-scribe, f218-solver |
| 7 | 2026-09-13T01:44:04Z | 2026-09-13T16:01:24Z | 14.289 | 462,526,514 | futon2: 505; futon3c: 40; mathlib4: 36 | codex-22, codex-23, codex-24, codex-26 |
| 8 | 2026-08-28T21:17:30Z | 2026-08-29T17:53:16Z | 20.596 | 461,053,296 | apm-lean: 77; futon3c: 6 | codex-10, codex-12, codex-17, codex-18, codex-2, f53-promotion-proctor; 1 unmapped sessions |
| 9 | 2026-09-11T02:52:23Z | 2026-09-11T09:55:11Z | 7.047 | 276,371,998 | apm-lean: 62; futon2: 40; futon3c: 47 | codex-10, codex-12, codex-17, f218-promotion-proctor, f219-proctor, f219-promotion-proctor, f219-scribe, f219-solver, f220-proctor, f220-solver, f221-proctor, f221-promotion-proctor, f221-scribe, f221-solver |
| 10 | 2026-08-06T23:15:51Z | 2026-08-07T08:54:45Z | 9.648 | 243,805,430 | apm-lean: 87; codex: 34; futon6: 2; nlab-content: 2 | ; 1 unmapped sessions |
| 11 | 2026-09-06T21:56:29Z | 2026-09-07T11:43:59Z | 13.792 | 230,554,249 | apm-lean: 95; futon2: 102; mathlib4: 23; p4ng: 38 | codex-1, codex-17, codex-2, codex-3, f175-proctor, f175-promotion-proctor, f175-scribe, f175-solver, f176-proctor, f176-promotion-proctor, f176-scribe, f176-solver, f177-proctor, f177-promotion-proctor, f177-scribe, f177-solver, f178-proctor, f178-promotion-proctor, f178-scribe, f178-solver, f179-proctor, f179-promotion-proctor, f179-scribe, f179-solver, f180-proctor, f180-promotion-proctor, f180-scribe, f180-solver, f181-proctor, f181-promotion-proctor, f181-scribe, f181-solver, f182-proctor, f182-promotion-proctor, f182-scribe, f182-solver, f183-proctor, f183-promotion-proctor, f183-scribe, f183-solver, f184-proctor, f184-promotion-proctor, f184-scribe, f184-solver, f185-proctor, f185-promotion-proctor, f185-scribe, f185-solver; 2 unmapped sessions |
| 12 | 2026-08-23T23:16:43Z | 2026-08-24T07:43:28Z | 8.446 | 151,168,913 | futon3c: 2; mathse-xtdb-benchmark: 4 | codex-10, codex-12, codex-14, codex-15 |
| 13 | 2026-08-30T22:23:53Z | 2026-08-31T08:33:05Z | 10.153 | 118,759,241 | apm-lean: 6; futon2: 2; futon3: 3; futon3c: 6; futon4: 1 | codex-1, codex-10, codex-12, codex-17, codex-18, codex-20, codex-22, codex-3, codex-5, codex-8, f65-proctor, f65-promotion-proctor, f65-scribe |
| 14 | 2026-09-11T20:51:48Z | 2026-09-12T13:11:17Z | 16.324 | 109,595,830 | apm-lean: 33; futon0: 1; futon1b: 1; futon2: 1; futon3c: 6; voxterm: 1 | codex-16, codex-17, f226-proctor, f226-promotion-proctor, f226-scribe, f226-solver, f227-proctor, f227-promotion-proctor, f227-scribe, f227-solver |
| 15 | 2026-08-24T21:23:55Z | 2026-08-25T07:06:56Z | 9.717 | 79,296,027 | apm-lean: 13 | codex-10, codex-17, codex-18, f32-proctor, f32-promotion-proctor, f32-solver |
| 16 | 2026-09-04T12:44:52Z | 2026-09-05T03:49:36Z | 15.079 | 58,706,876 | apm-lean: 18; futon2: 53; futon3c: 2; mathlib4: 3; p4ng: 12 | codex-17, f85-proctor, f85-promotion-proctor, f85-scribe, f85-solver; 65 unmapped sessions |
| 17 | 2026-08-29T22:39:23Z | 2026-08-30T09:23:52Z | 10.741 | 39,501,950 | apm-lean: 14; futon3c: 4; futon4: 1 | codex-10, codex-17, codex-2, f58-proctor, f58-promotion-proctor, f58-scribe, f58-solver, f59-proctor, f59-promotion-proctor, f59-scribe, f59-solver |
| 18 | 2026-09-01T20:35:58Z | 2026-09-02T06:22:28Z | 9.775 | 32,982,234 | apm-lean: 2; futon2: 37; futon3: 19; futon3c: 3; mathlib4: 1; p4ng: 14 | codex-17, codex-20, f79-proctor, f79-promotion-proctor, f79-scribe, f79-solver; 41 unmapped sessions |
| 19 | 2026-08-22T16:21:26Z | 2026-08-23T09:57:04Z | 17.594 | 22,364,403 | apm-lean: 11; futon3c: 4; p4ng: 1 | codex-10, f9957156633803-proctor, f9957156633803-solver |
| 20 | 2026-08-21T15:56:51Z | 2026-08-22T06:41:10Z | 14.738 | 8,739,220 | futon3c: 4 | codex-10 |
| 21 | 2026-08-04T21:19:30Z | 2026-08-05T06:25:27Z | 9.099 | 8,192,588 | apm-lean: 1; codex: 19; futon3c: 1; nlab-content: 2 | ; 1 unmapped sessions |
| 22 | 2026-09-03T22:58:01Z | 2026-09-04T05:51:02Z | 6.884 | 7,766,383 | futon2: 78; futon3c: 1; mathlib4: 12; p4ng: 68 | ; 63 unmapped sessions |
| 23 | 2026-08-19T19:51:12Z | 2026-08-20T07:13:14Z | 11.367 | 3,907,601 | none | codex-8 |
| 24 | 2026-08-15T21:15:59Z | 2026-08-16T08:46:36Z | 11.510 | 3,359,351 | futon3c: 3 | codex-1, codex-3, codex-4 |
| 25 | 2026-08-09T20:13:37Z | 2026-08-10T04:58:55Z | 8.755 | 1,996,571 | apm-lean: 1; codex: 5; easyeffects: 2; nlab-content: 8 | ; 1 unmapped sessions |
| 26 | 2026-08-13T17:46:27Z | 2026-08-14T07:09:38Z | 13.386 | 1,526,684 | codex: 40; easyeffects: 2; futon3c: 1; gflownet: 8; nlab-content: 3 | codex-1, codex-3 |
| 27 | 2026-08-18T18:22:56Z | 2026-08-19T05:39:45Z | 11.280 | 1,251,749 | apm-lean: 4; xtdb: 3 | codex-7 |
| 28 | 2026-08-20T22:53:23Z | 2026-08-21T08:06:19Z | 9.216 | 624,737 | futon2: 3; futon3c: 1; futon5a: 1; futon6: 4 | codex-10 |
| 29 | 2026-09-19T01:28:37Z | 2026-09-19T13:48:22Z | 12.329 | 608,176 | futon2: 5; futon3c: 2; p4ng: 1 | codex-1, codex-23, codex-3 |
| 30 | 2026-09-02T20:24:57Z | 2026-09-03T05:41:07Z | 9.269 | 261,558 | none | codex-9 |
| 31 | 2026-07-23T13:08:47Z | 2026-07-23T21:02:19Z | 7.892 | NR | codex: 18; futon3c: 1; nlab-content: 26 | No rollout usage; commit author attribution unproved |
| 32 | 2026-07-24T01:56:38Z | 2026-07-24T19:05:09Z | 17.142 | NR | codex: 23; easyeffects: 2; futon3c: 1; gflownet: 7; nlab-content: 19; xtdb: 3 | No rollout usage; commit author attribution unproved |
| 33 | 2026-07-24T21:57:33Z | 2026-07-25T16:50:53Z | 18.889 | NR | codex: 17; futon1b: 1; futon3c: 1; gflownet: 1; nlab-content: 5 | No rollout usage; commit author attribution unproved |
| 34 | 2026-07-25T22:53:51Z | 2026-07-26T09:38:31Z | 10.744 | NR | apm-lean: 9; codex: 2; futon3c: 1; nlab-content: 5 | No rollout usage; commit author attribution unproved |
| 35 | 2026-07-26T11:45:16Z | 2026-07-26T19:12:38Z | 7.456 | NR | futon1b: 1; futon3c: 2; nlab-content: 3 | No rollout usage; commit author attribution unproved |
| 36 | 2026-07-26T21:15:38Z | 2026-07-27T05:48:35Z | 8.549 | NR | codex: 5; easyeffects: 1; futon1b: 1; futon2: 1; futon3c: 3; gflownet: 2; mmca-clj: 1; nlab-content: 2 | No rollout usage; commit author attribution unproved |
| 37 | 2026-07-29T13:22:31Z | 2026-07-30T00:51:12Z | 11.478 | NR | apm-lean: 2; codex: 34; easyeffects: 1; futon3c: 9; futon5: 1; gflownet: 2; nlab-content: 28; xtdb: 1 | No rollout usage; commit author attribution unproved |
| 38 | 2026-08-01T17:20:29Z | 2026-08-02T07:37:14Z | 14.279 | NR | codex: 4; futon2: 1; nlab-content: 10 | No rollout usage; commit author attribution unproved |
| 39 | 2026-08-03T21:39:23Z | 2026-08-04T08:03:06Z | 10.395 | NR | codex: 12; futon5: 2; gflownet: 3; nlab-content: 1 | No rollout usage; commit author attribution unproved |
| 40 | 2026-08-05T15:44:00Z | 2026-08-06T05:28:40Z | 13.744 | NR | codex: 37; easyeffects: 4; futon3c: 1; nlab-content: 11 | No rollout usage; commit author attribution unproved |
| 41 | 2026-08-10T19:17:56Z | 2026-08-11T06:46:32Z | 11.477 | NR | apm-lean: 10; codex: 23; futon3c: 1; nlab-content: 3 | No rollout usage; commit author attribution unproved |
| 42 | 2026-08-11T22:52:41Z | 2026-08-12T09:06:07Z | 10.224 | NR | codex: 11; futon3c: 2; nlab-content: 4 | No rollout usage; commit author attribution unproved |
| 43 | 2026-08-12T16:57:41Z | 2026-08-13T07:53:32Z | 14.931 | NR | codex: 37; futon3: 1; futon3c: 1; gflownet: 73; nlab-content: 9 | No rollout usage; commit author attribution unproved |
| 44 | 2026-08-14T18:57:14Z | 2026-08-15T07:10:33Z | 12.222 | NR | codex: 34; futon3c: 1; nlab-content: 3; xtdb: 1 | No rollout usage; commit author attribution unproved |
| 45 | 2026-08-17T19:24:35Z | 2026-08-18T03:40:12Z | 8.260 | NR | futon3c: 1 | No rollout usage; commit author attribution unproved |
| 46 | 2026-09-14T04:03:35Z | 2026-09-14T11:31:13Z | 7.461 | NR | p4ng: 1 | No rollout usage; commit author attribution unproved |
| 47 | 2026-09-17T00:16:12Z | 2026-09-17T12:32:53Z | 12.278 | NR | futon2: 2; futon3: 1; mathlib4: 2; p4ng: 5 | No rollout usage; commit author attribution unproved |
| 48 | 2026-09-18T05:00:53Z | 2026-09-18T12:30:07Z | 7.487 | NR | p4ng: 2 | No rollout usage; commit author attribution unproved |
| 49 | 2026-09-21T06:05:12Z | 2026-09-21T13:26:51Z | 7.361 | NR | p4ng: 1 | No rollout usage; commit author attribution unproved |
| 50 | 2026-07-21T15:30:02Z | 2026-07-22T09:40:09Z | 18.169 | NR | codex: 49; easyeffects: 4; nlab-content: 13; xtdb: 2 | No rollout usage; commit author attribution unproved |
| 51 | 2026-07-27T23:13:59Z | 2026-07-28T10:20:17Z | 11.105 | NR | codex: 18; nlab-content: 4; xtdb: 2 | No rollout usage; commit author attribution unproved |
| 52 | 2026-07-28T16:06:01Z | 2026-07-29T06:37:09Z | 14.519 | NR | codex: 36; easyeffects: 1; nlab-content: 19; xtdb: 1 | No rollout usage; commit author attribution unproved |
| 53 | 2026-07-31T20:00:48Z | 2026-08-01T08:54:02Z | 12.887 | NR | codex: 12; gflownet: 6; nlab-content: 8 | No rollout usage; commit author attribution unproved |
| 54 | 2026-08-02T17:11:34Z | 2026-08-03T04:28:19Z | 11.279 | NR | codex: 3; nlab-content: 3 | No rollout usage; commit author attribution unproved |

## Candidate 1 — September 10 topology expansion (highest observed usage)


2026-09-10T00:26:23Z → 2026-09-10T11:16:58Z, 10.843 hours; **831,983,803** logged input+output tokens. Input 830,320,353, including 814,514,688 cached; output 1,663,450. Nineteen active rollouts. Of the reported last-total allocation, 93,442,549 belongs to APM frame roles and is not automatically topology cost. The remaining named seats include topology authors, reviewers, controller notifications, and possibly other work; exact per-build billing is not established.

**What:** 640 reachable commits in `apm-lean`, touching 164 paths. The changed set contains 152 newly introduced `ConstructionTargets/` Lean/documentation files, two existing topology ledgers, eight problem files, the DAG and an experiment note. Today the 152 target files contain 12,482 lines; the two ledgers contain 124,836 lines, so calling all 143,199 current lines in the touched set “new code” would be false. Across commits, numstat is +52,024/−8,708; this is churn, not net unique output. The newest state also includes subsequent work. No compilation or mathematical re-validation was performed for this forensic task.

Examples include `GraphMetricComparison`, `LinearGraphDensity`, `LinearGraphMeasure`, manifold regular-fiber charts/atlases, transverse-incidence charts, and coefficient-relative/excision/boundary-normalization targets. This is mathematics infrastructure plus scheduling records, not a single standalone application. `GraphMetricComparison.md` explicitly says the result supplies neither a nonlinear area formula nor a frozen consumer closure. The complete changed-file inventory below preserves scope and first touching commit for each path.

Window commit endpoints (chronological, not asserted to delimit one linear branch): `b6fca559de68fd200bc8834484716f0df9bb212f` through `30360ff1c5cd2e763006c7822a5688572f880d52`.

**Immediate dispatch and reason:** rollout `/home/joe/.codex/sessions/2026/09/09/rollout-2026-09-09T11-42-33-01a085fa-1198-7fe3-8706-fe2c5a6cb807.jsonl`, line 6347, retains job `invoke-1789000037365-17429-a26398dd`, caller **topology-build-loop**, recipient **codex-12**, row `checkpoint-full-dag-refill-32-after-antipodal-and-manifold-fiber-charts`. The actual delivered instruction says:

> Append a fresh `:strategy-checkpoint` as the final vector item, depending on
> every elected work-row id, so the campaign refills again after this portfolio.
> Every inserted row must satisfy the full ledger schema and use a new id. An
> ordinary replan may not return an empty tail.

And:

> `FOUNDATION`, `SPEC-DEFECT` and
> `DEFERRED-REP` rows are STANDING AUTHORIZED: electing one needs no further
> owner approval.

The same packet requires reading `TOPOLOGY-DEPENDENCY-DAG.md`, `docs/TN-topology-closure-experiments.md`, dependency receipts, and the design's GO/REVISE/STOP definitions. It ranks downstream consumer count before effort, with a recently inserted near-closure exception. It even records the contrary evidence: 349 completed rows, growth from 257 to 588 modules, yet no topology problem placeholder-count change in its stated preceding comparison. That number is a **quoted controller diagnosis**, not an independently rerun census here.

The trace then records codex-12 electing another portfolio and codex-10 reviewing it in `invoke-1789000395829-17437-9754f4df`; author rows are separately sent to codex-12/codex-13 and others, supervisor commits their ledger transitions, and the next checkpoint repeats. Codex-8 receives milestone notifications; acknowledging those also costs tokens. The retained assistant at lines 6350/6385 explicitly says it is recounting placeholders and electing the next portfolio on that basis. This provides an observed reason and dispatch mechanism, not a speculative psychological account.

**Upstream instruction history:** `ea059cfc463b7a6928825cfe44ee88a53a5476ba` adds the supervisor (September 2); `8f741da03fddcef2c61e5963cd3646959ae3f438` makes checkpoints refill; `96a39230a1262a7ee61311cc1e8fa78d2d5e6d8b` withdraws the owner-authorization gate (September 6); `ad65f1c795216ef7f9fd1968d1132af7b60e72b7` adds the near-closed-consumer priority (September 9). These are accessible with `git show SHA -- holes/labs/topology-contract/work-prompt.md`. The delivered packet explicitly labels the September 6 rule as Joe's. The independently retained operator row `emacs-d6435dc360bb9d5fd55ac5fb5e67839e` (September 6 20:52:43Z) asks to follow DAG order and build needed library components; `emacs-c7552ddeb24cd3fc25b8a1fb9b453192` (September 9 15:09:20Z) asks whether the loop actually completes problems. These support the broad programme and his concern, **not verbatim ratification of every template clause**. The stale design document still contains the older owner gate; do not reconstruct historical authority from today's design paragraph alone.

A subsequent change, `998770091480581e31976b3bfb1bcad6cf6cf718` at September 10 12:11:59Z, narrows election to infrastructure an open problem needs. It postdates this gap and must not be projected backwards onto its dispatches.

**Deletion footprint:** review the 152 target paths in the inventory individually. Do not delete `ConstructionTargets/` wholesale or either shared worklist. There are current references from files outside the window's changed set, including:

- `apm-lean/problems/t96J05/lean/Main.lean:3-4` imports `ManifoldRegularFiberGlobalAtlas` and `TransverseIncidenceChosenAtlas`.
- `apm-lean/problems/t98A06/lean/Main.lean:2` imports `ManifoldTargetRegularFiberGlobalAtlas`.
- `apm-lean/ConstructionTargets/SphereCapL2Image.lean:7` opens `GraphMetricComparison`; `PlaneArcFiniteSmallTranslations.lean:1` imports `PlaneArcSmallTranslation`.
- `futon3c/holes/labs/M-apm-demonstration/analysis/pattern-construction-2026-09-10/topology_induction.py:30-31` names `CoefficientRelativeSmall.lean` and `CoefficientExcision.lean`.

The bounded current-reference search found 38 matching lines in 22 outside files. These establish actual dependency/referencing consequences, not usefulness of every module. Imports inside the candidate set are excluded from this “outside” count but would still matter for partial deletion. No-reference results cannot prove deletability: dynamic loading, untracked/ignored files, external hosts and indirect dependencies remain outside the textual test.

## Candidate 2 — September 5–6, explicit air-travel lead

2026-09-05T12:03:45Z → 2026-09-06T13:56:21Z, 25.877 hours; **639,137,908** logged tokens. Input 637,233,118, cached subset 623,196,800, output 1,904,790; 118 rollouts. APM frame roles account for 530,457,563 of the last-token totals. Codex-9 accounts for 42,126,216; codex-17 for 33,971,559. **Do not assign all 639 million tokens to library annotations**: their authors include non-Codex activity, which this usage scan does not price.

The strongest physical-travel evidence is Joe's `emacs-f81536eb967098bd5e1089215c2b722a`, September 5 10:49:17.127570048Z:

> I think in addition, as I will be on a flight, we should ideally find a way to get the "theory" part moving, possibly with Zai agents to balance my usage.

He continues:

> the programme here would continue the library loop, mining the evidence landscape and annotating patterns, so that we get a sense of what problems we have been solving historically.

This is air travel, not FUTON mission vocabulary. His first retained turn after the gap (`emacs-bdb82f48c62df1acd8364817f54619a1`) says the system has encountered a blockage over the last 24 hours. It does not identify the unwanted artifact. July 21's operator records also mention editing on a plane “yesterday”; that flight precedes the requested window and the retained Codex usage coverage. It cannot be excluded as the remembered incident from this data.

**What:** 457 commits across `apm-lean` (182), `futon2` (174), `futon3` (16), `futon3c` (21), `mathlib4` (19 WM-path commits), and `p4ng` (45). This is a mixed window:

- APM changes to 22 existing problem files (today 15,178 lines): algebra, analysis/PDE, distributions and variational problems, including long `m01J04` and `m01J06` proof attempts. Historical numstat +10,327/−574. These are not 22 newly invented problems.
- War Machine formalization/readback/convergence apparatus in `futon2/holes/labs/wm-contract/`, `src/futon2/aif/`, report scripts/tests, and 20 new `mathlib4/DarkTower/WarMachine/` files (3,270 current lines in that changed set). Examples: `MachineBeliefUpdate`, `MachinePolicyFreeEnergy`, precision, action, depth, temperature and Dirichlet accumulation modules and witnesses. `futon2`'s +190,003 lines are dominated by retained evidence/document output, not 190,003 lines of implementation.
- Library L6–L19 work in `futon2/holes/labs/library-loop/` and 754 touched `futon3/library/` files. Only 15 of those library files are new by first-touch-parent comparison; the rest are edits to existing material. L7–L10 subjects explicitly disclose zero matched corpus sources in several families, followed by provenance-pointer corrections and later section-document grounding. Examples: `5c0b7371` (92-pattern L7 annotation), `2a91028b` (97-pattern L8 annotation), `a43f0280` (256 exotype records), `50bd5309e8a64d0240eff6cf4d13aaf5a11a9611` (L18 v3). Those are candidate annotation changes to inspect, not grounds to delete the existing pattern library.
- Generated paper/readiness reports in `p4ng`, plus APM park-decision and HTTP transport changes in `futon3c`.

**Dispatch chain / builder's stated reason:** the highest-usage session is `/home/joe/.codex/sessions/2026/09/06/rollout-2026-09-06T01-49-14-01a07467-cd1d-7e93-941b-1f8c346c4254.jsonl`, 98,373,674 tokens during this window. Its first task is `From: countdown-control`, `To: f101-solver`, job `apm-role-edd8bb547342c4affc38566ea2f3c988fb8c65db9b77ba1c76bf95737301ba1c`, workspace `/home/joe/code/apm-frames/f101-m01J04-solver`, frozen base `9fa428f7a7bb0292e3699c69ccbdc68e65187c14`, and role card `futon3c/holes/labs/M-apm-demonstration/role-cards/codex-solver-v5.md`. The instruction includes:

> Opening siege. Own a substantial proof episode: search, test multiple routes, build missing infrastructure when needed, and continue through friction. Do not stop merely because one lemma compiled.

The builder responds that the target is the full distributional identity and checks repository/sibling infrastructure before choosing a route. Analogous frozen packets dispatch f103/m01J06 and f105/m02A03. This is controller-driven execution of supplied mathematical problems, not evidence that the solver invented the original goal. The inspected highest-cost sessions do not establish every upstream scheduler decision; current Agency retention does not cover that chain completely.

For the WM subset, codex-9's retained rollout `01a063cb-15ec-7a42-af7a-0aa13d598da5` contains `wm-edge-loop` job `invoke-1788615738834-7918-7d1b19aa` commissioning **one F8 slice: state the belief update in Lean**; a subsequent `wm-build-work` packet commissions F_pi using `mathlib4 d15325c004` and `futon2 69154b1d` as templates. It instructs reading the worklist and C517/C518 predecessor reports, and explicitly separates quantities. That is a traceable supplied instruction, though usefulness is a separate question.

**Deletion footprint:** no single disposable directory represents this window. The inventory identifies exact touched files. Shared APM problem solutions, existing pattern files, HTTP transport and WM modules require change-level review, not whole-file deletion. Readback and declaration modules have consumers outside the changed set; pattern references also cross repository boundaries. Concrete current references include `futon3c/src/futon3c/apm/cascade_dry_run.clj` naming `math-strategy/missing-dependency-protocol`, `futon3c/scripts/memory_latency_monitor.clj` naming `math-formalization/tactic-algebra-interference`, and `futon3c/src/futon3c/vsatarcs/feeder.clj:33` naming `futon2/scripts/futon2/report/war_machine.clj`. These show why deleting edited original files would be broader than removing unwanted annotations. Reference search over this broad set finds 1,563 outside files; many are textual reports/receipts, not executable dependency edges. The exact file set is listed below without claiming each mention requires runtime migration.

## Candidate 3 — August 27–28 continuous topology scaffolds

2026-08-27T20:37:44Z → 2026-08-28T04:56:28Z, 8.312 hours; **620,559,113** tokens. Input 619,369,326, cached subset 607,451,136, output 1,189,787. Ten rollouts; the four principal topology seats codex-8/10/12/18 total 572,292,655 tokens, but session totals can contain other work.

**Origin is directly retained in two vocabularies.** Operator evidence `emacs-baa0934c8d5c8944b86c9377dfd3c108` at the start of the gap, and codex-18's rollout `01a0345b-9cf0-7601-b80f-d207fd6844cb`, line 44664 (20:37:47.492Z), both carry Joe's instruction:

> OK... now my question is, can we run continuously on this programme with chained parks, bells to Agency agents, and see whether we make more progess that way than we did with the old t00J02 problem approach?

The spelling is retained. Codex-18 explicitly decides at line 44671 to use “repeated bounded elections” and choose the DAG's highest-ranked edge, production open-cover Mayer–Vietoris, followed by closed gluing through collars. It dispatches:

- `invoke-1787863104877-2532-11de7197` → codex-8: canonical two-cover structure avoiding the rejected `eqToIso` rewriting loop.
- `invoke-1787863104879-2533-a45f5bdb` → codex-10: closed-gluing/collar consumer survey.
- `invoke-1787863104899-2534-d006c77d` → codex-12: alternative architecture audit.

The September 2 Agency backup `/tmp/futon3c-invoke-jobs.edn.bak-20260902-1900` independently retains caller, target, session and creation timestamps for these jobs. Its inspected rows lack `request-commission`; the original prompt text is recovered from the recipients' rollouts rather than invented from an absent commission. The first result recommends a definition-first Boolean two-cover complex; the next round elects t00A01's production MV/H0 comparison and t96A03's explicit equatorial gluing. Codex-18 repeatedly banks reviewed constructions, revises the DAG, then dispatches prerequisites. This supplies both the chain and its stated technical motivation.

**What:** 102 `apm-lean` commits touch 94 paths; 88 are ConstructionTargets files (80 newly introduced, including Lean and companion prose), four are problem files, one is the DAG and one is `scratch2.lean` (not currently present at the canonical path). Today those 88 targets total 19,340 lines. Historical numstat for all 94 is +13,635/−372. Another 14 `futon3c` commits touch 22 existing apparatus/test/receipt paths, +793/−201, and must not be silently charged to the topology scaffold programme.

Representative commits: `44d294de46d11dea32c4dfc52576f9c544308e2f` (Boolean two-cover scaffold), `edef025af9c4406057084ee1456edfb16a9c5c3e` (cover-small union), `569f6988a8c22a9324bab3726525a0ff9c8c8b78` (singular path homotopy prism), `9e145f9a` (path homotopy invariance in H1), `5d9fa3d9da565d97aa3cdde48422ed8bc348492b` (concatenation triangle coordinates). Targets also include boundary-torus homology, circle universal cover, fundamental-group products and abelianization naturality. Read `ConstructionTargets/SingularPathChain.md`: it distinguishes proved boundary identities from a remaining conditional subdivision formula. A compiled internal construction is not automatically the requested end theorem.

**Deletion footprint:** the exact 94-path inventory follows. Do not delete all ConstructionTargets, the shared DAG, or entire problem files to remove these increments. Current outside references number 1,213 lines across 264 files. For example `apm-lean/problems/t92J05/lean/Main.lean:176` uses `ConstructionTargets.CircleUniversalCover.circleFundamentalGroupMulEquivInt`; multiple later target modules import the scaffold chain. Apparatus changes in `futon3c/src/futon3c/apm/` and `src/futon3c/transport/http.clj` are shared live-loop components, not isolated disposable experiment output. No compiled-cache or worktree deletion is authorized or proposed here.

## Evidence needed before identifying or deleting anything

The top three are **ranked windows, not three proven unwanted systems**. Joe must identify the remembered artifact or date. September 5 has direct flight evidence; August 27 has direct continuous-work authorization; September 10 has the largest retained token expenditure and the clearest nonempty-refill mechanism. The claim that a specific builder invented an unrequested goal is not established for any of these three.

Agency's 24-hour detail / seven-day tombstone policy and overlapping backups prevent a complete historic dispatch graph. The September 2 backup cannot recover September 5 or 10 commissions; current job survivors do not imply full coverage. `/tmp/futon3c-invoke-jobs.edn.commissions/` covers September 13–14 per SOURCES and cannot fill those dates. Codex first-user messages are often unrelated older tasks in reused sessions; the quotations above use **in-window dispatches**, not those misleading first messages. Local evidence channels and rollouts are not an all-device record of Joe's activity. No provider invoice, all-host usage reconciliation, or exact per-artifact cost attribution was found. Commit author names alone do not identify the model.

Line counts are current readable-file line counts and may include later changes. New-file status means absent in the parent of the earliest touching commit **within this window**, not guaranteed first creation anywhere in all history. Numstat sums repeated additions/deletions and skips binary counts; it is not net surviving LOC. `--all` includes reachable unmerged branch work. Current imports establish dependency presence, not scientific merit. These limits are important to deletion decisions.

## Reproduction and inventories

The operator query is documented above and in `SOURCES-work-records-2026-09-21.md §3`. The basic commit query was `git -C /home/joe/code/REPO log --all --format='%H%x09%ct%x09%an%x09%cn%x09%s'`, with the Mathlib path filter noted above. Read every reachable result, deduplicate `(repo,SHA)`, then compare `%ct` against the table's full-precision operator evidence boundaries. Do not substitute author dates or sum worktree logs. For each selected SHA, `git diff-tree --root --no-commit-id --numstat -r SHA` supplies the changed-file counts; merge commits with no ordinary diff remain in commit counts.

Current inbound references were searched with `rg -n -F -f PATTERNS` across the canonical roots listed below, constrained to `.lean`, `.md`, `.clj`, `.bb`, `.py`, `.edn`, `.tex`. For changed ConstructionTargets files the patterns are the full slash path and dotted module name; for changed flexiarg files, the library-relative pattern id; for changed Clojure/Python/bb files, the repo-relative source path. Remove matches whose file is itself in the changed set. This is an explicit bounded textual search: other extensions, ignored files, worktrees, other hosts, arbitrary symbol references and dynamic resolution are not exhaustively covered. In particular the broad window-2 reference census contains references to pre-existing patterns, not only to their new annotations.

The scope roots were:

`/home/joe/code/chipwits-forth, /home/joe/code/apm-lean, /home/joe/code/voxterm, /home/joe/code/expenses-jac, /home/joe/code/futon3c, /home/joe/code/marimo-zone, /home/joe/code/mfuton-share, /home/joe/code/ukrn-services-simulation, /home/joe/code/expenses-hel, /home/joe/code/18_Category_theory_homological_algebra, /home/joe/code/futon2a, /home/joe/code/FloWrTester, /home/joe/code/futon0, /home/joe/code/p4ng, /home/joe/code/mathse-xtdb-benchmark, /home/joe/code/futon1bi, /home/joe/code/easyeffects, /home/joe/code/futon6, /home/joe/code/mmca-clj, /home/joe/code/powerbi-tui, /home/joe/code/futon7, /home/joe/code/mathlib4, /home/joe/code/futon5, /home/joe/code/gflownet, /home/joe/code/mmca, /home/joe/code/futon3a, /home/joe/code/futon3b, /home/joe/code/futon3, /home/joe/code/orbook.github.io, /home/joe/code/futon1a, /home/joe/code/futon1, /home/joe/code/futon7a, /home/joe/code/chatgpt-tui, /home/joe/code/futon1b, /home/joe/code/futon4, /home/joe/code/filings, /home/joe/code/futon2, /home/joe/code/futon5a`.

The following inventories retain every changed path for each candidate, current line counts, earliest touching SHA, and whether absent at its parent. Parent-absent does **not** authorize deletion. The complete unique commit lists and outside-reference file lists make the inventory reproducible without requiring the scratch files.

### Candidate 1: changed paths

<details><summary>Expand full path inventory</summary>

| Repository / path | Current lines | Parent-absent | Earliest touching commit |
|---|---:|---|---|
| `apm-lean/ConstructionTargets/ChartTriangleEmbedding.lean` | 36 | yes | `3e20822e8672ac4e3dd9c2cf8fbcbd35bb782ed5` |
| `apm-lean/ConstructionTargets/ChartedSpaceModelDerivativeTransport.lean` | 182 | yes | `afa7b6005988aaee5651332d5765a77ee1734c71` |
| `apm-lean/ConstructionTargets/ChartedSpaceModelSmoothTransport.lean` | 82 | yes | `94c20be8f8066c470a0d1426f0886addcb2ea7a7` |
| `apm-lean/ConstructionTargets/ChartedSpaceModelTransport.lean` | 70 | yes | `ab99abe095db8d5d4d0a783b732bd8dc461408a7` |
| `apm-lean/ConstructionTargets/CoefficientAffineNaturality.lean` | 118 | yes | `ffb357f532bd183024c14cfe10bc04246beb9d33` |
| `apm-lean/ConstructionTargets/CoefficientAffineNaturality.md` | 37 | yes | `ffb357f532bd183024c14cfe10bc04246beb9d33` |
| `apm-lean/ConstructionTargets/CoefficientBallBoundaryLift.lean` | 109 | yes | `76299a8f91a9bc6a992a92e28c0678f40a884411` |
| `apm-lean/ConstructionTargets/CoefficientBallBoundaryLift.md` | 13 | yes | `76299a8f91a9bc6a992a92e28c0678f40a884411` |
| `apm-lean/ConstructionTargets/CoefficientBarycentricNaturality.lean` | 279 | yes | `3f41ed7d65c5ebf382f57492c65e5fe1460676e2` |
| `apm-lean/ConstructionTargets/CoefficientBarycentricNaturality.md` | 41 | yes | `3f41ed7d65c5ebf382f57492c65e5fe1460676e2` |
| `apm-lean/ConstructionTargets/CoefficientBoundaryReflection.lean` | 145 | yes | `6f66faffa3b127bf4f4b2bcb480d32a81b3c1579` |
| `apm-lean/ConstructionTargets/CoefficientBoundaryReflection.md` | 13 | yes | `6f66faffa3b127bf4f4b2bcb480d32a81b3c1579` |
| `apm-lean/ConstructionTargets/CoefficientChainSupport.lean` | 70 | yes | `1e40c51ca8c3f07db1f3ecdbbd2505f1801ac183` |
| `apm-lean/ConstructionTargets/CoefficientChainSupport.md` | 28 | yes | `1e40c51ca8c3f07db1f3ecdbbd2505f1801ac183` |
| `apm-lean/ConstructionTargets/CoefficientChartBallNormalization.lean` | 71 | yes | `1dea549a487aa6baefdc1787eaaa3fa85a7ef098` |
| `apm-lean/ConstructionTargets/CoefficientChartBallTransport.lean` | 94 | yes | `7d1d361d8ca63cf4d71b5f942abf4f919dfc5cd9` |
| `apm-lean/ConstructionTargets/CoefficientChartBoundaryNeighborhood.lean` | 78 | yes | `5c23afeb2297af242b868b7495dc3c16275f9149` |
| `apm-lean/ConstructionTargets/CoefficientChartBoundaryNeighborhood.md` | 13 | yes | `5c23afeb2297af242b868b7495dc3c16275f9149` |
| `apm-lean/ConstructionTargets/CoefficientChartLocal.lean` | 138 | yes | `c17b1ff4cd7551b2bf8af599968b4443be23c48c` |
| `apm-lean/ConstructionTargets/CoefficientChartLocal.md` | 13 | yes | `c17b1ff4cd7551b2bf8af599968b4443be23c48c` |
| `apm-lean/ConstructionTargets/CoefficientChartLocalAllDegrees.lean` | 57 | yes | `e002d6ed1469f41a889adc808e3896c42d987a1e` |
| `apm-lean/ConstructionTargets/CoefficientChartPair.lean` | 118 | yes | `6f7d4072f5919e08cd1ca6d68864f3f0d84cd7a5` |
| `apm-lean/ConstructionTargets/CoefficientChartPair.md` | 13 | yes | `6f7d4072f5919e08cd1ca6d68864f3f0d84cd7a5` |
| `apm-lean/ConstructionTargets/CoefficientChartRestriction.lean` | 97 | yes | `8a65c6799993ac37ded3159abd82d26a2f6c7ac0` |
| `apm-lean/ConstructionTargets/CoefficientChartRestriction.md` | 11 | yes | `8a65c6799993ac37ded3159abd82d26a2f6c7ac0` |
| `apm-lean/ConstructionTargets/CoefficientChartTransitionUnit.lean` | 142 | yes | `dd9752ee9e4151dfe7d5b3e1211b700eec73471a` |
| `apm-lean/ConstructionTargets/CoefficientChartTransitionUnit.md` | 11 | yes | `dd9752ee9e4151dfe7d5b3e1211b700eec73471a` |
| `apm-lean/ConstructionTargets/CoefficientCodiagonalKernel.lean` | 77 | yes | `089792c96eeca964059b880db51828277040970b` |
| `apm-lean/ConstructionTargets/CoefficientCodiagonalKernel.md` | 13 | yes | `089792c96eeca964059b880db51828277040970b` |
| `apm-lean/ConstructionTargets/CoefficientCokernelComparison.lean` | 125 | yes | `4e98d2ec29709dbe236bdf37d44241c09ced6741` |
| `apm-lean/ConstructionTargets/CoefficientCokernelComparison.md` | 30 | yes | `4e98d2ec29709dbe236bdf37d44241c09ced6741` |
| `apm-lean/ConstructionTargets/CoefficientCompactBoundaryMotion.lean` | 86 | yes | `234a5bbd4d1ad4b9e98cbbcd82db20adfcaf1d61` |
| `apm-lean/ConstructionTargets/CoefficientCompactBoundaryMotion.md` | 11 | yes | `234a5bbd4d1ad4b9e98cbbcd82db20adfcaf1d61` |
| `apm-lean/ConstructionTargets/CoefficientConnectedH0Map.lean` | 84 | yes | `58ea5c3159fa6d8042ba513b4ba948af9efb1c64` |
| `apm-lean/ConstructionTargets/CoefficientConnectedH0Map.md` | 13 | yes | `58ea5c3159fa6d8042ba513b4ba948af9efb1c64` |
| `apm-lean/ConstructionTargets/CoefficientContractiblePair.lean` | 211 | yes | `0a72159bc678d910de3b21550e9b772016dc507f` |
| `apm-lean/ConstructionTargets/CoefficientContractiblePair.md` | 15 | yes | `0a72159bc678d910de3b21550e9b772016dc507f` |
| `apm-lean/ConstructionTargets/CoefficientCoordinateReflections.lean` | 95 | yes | `62a0af3d4b4b7b2cbe034858bbe8a3d969b9ebce` |
| `apm-lean/ConstructionTargets/CoefficientCoordinateReflections.md` | 15 | yes | `62a0af3d4b4b7b2cbe034858bbe8a3d969b9ebce` |
| `apm-lean/ConstructionTargets/CoefficientCoveringTransfer.lean` | 138 | yes | `4b689d83309be91957b2ccc93d385e5cb9e0afd7` |
| `apm-lean/ConstructionTargets/CoefficientDiskReflection.lean` | 130 | yes | `d9f48550bd527f4fffbca519fb106a4165c4419f` |
| `apm-lean/ConstructionTargets/CoefficientDiskReflection.md` | 15 | yes | `d9f48550bd527f4fffbca519fb106a4165c4419f` |
| `apm-lean/ConstructionTargets/CoefficientDistinguishedChart.lean` | 59 | yes | `385d575847d78cce76a75d8f18732265e2b1ca9a` |
| `apm-lean/ConstructionTargets/CoefficientDistinguishedNormalization.lean` | 79 | yes | `3510365eedac863a7e69e71cefc9135cfd808f98` |
| `apm-lean/ConstructionTargets/CoefficientDistinguishedTargetNormalization.lean` | 45 | yes | `52e867b4c5fcac554b40e23580b96972d85be785` |
| `apm-lean/ConstructionTargets/CoefficientEventualSmallness.lean` | 215 | yes | `0f36d6c5f7c6fe1d99ded92e9888192dd4ac21ca` |
| `apm-lean/ConstructionTargets/CoefficientEventualSmallness.md` | 34 | yes | `0f36d6c5f7c6fe1d99ded92e9888192dd4ac21ca` |
| `apm-lean/ConstructionTargets/CoefficientExcision.lean` | 257 | yes | `5e52400d5d3b3bdc949cdfb044c9186123891664` |
| `apm-lean/ConstructionTargets/CoefficientExcision.md` | 17 | yes | `5e52400d5d3b3bdc949cdfb044c9186123891664` |
| `apm-lean/ConstructionTargets/CoefficientFiniteModelChart.lean` | 83 | yes | `9f48c402684d7499653a7c0ae8cfe96db3f955c9` |
| `apm-lean/ConstructionTargets/CoefficientFiniteOperators.lean` | 225 | yes | `15c18c05f76310b8424165ca8a056f447ece1591` |
| `apm-lean/ConstructionTargets/CoefficientFiniteOperators.md` | 33 | yes | `15c18c05f76310b8424165ca8a056f447ece1591` |
| `apm-lean/ConstructionTargets/CoefficientFiniteRealization.lean` | 121 | yes | `acbc19f4dbdbf16fa96171575974ee2966c09f97` |
| `apm-lean/ConstructionTargets/CoefficientFiniteRealization.md` | 31 | yes | `acbc19f4dbdbf16fa96171575974ee2966c09f97` |
| `apm-lean/ConstructionTargets/CoefficientFirstReflection.lean` | 107 | yes | `4e85600767edfe7f7de94ad2244abc5aadfe1a81` |
| `apm-lean/ConstructionTargets/CoefficientFirstReflection.md` | 13 | yes | `4e85600767edfe7f7de94ad2244abc5aadfe1a81` |
| `apm-lean/ConstructionTargets/CoefficientIteratedHomotopy.lean` | 61 | yes | `42e4bb083eec774f433dd1d223d89c4c3d4a5f37` |
| `apm-lean/ConstructionTargets/CoefficientIteratedHomotopy.md` | 27 | yes | `42e4bb083eec774f433dd1d223d89c4c3d4a5f37` |
| `apm-lean/ConstructionTargets/CoefficientManifoldLocalComparison.lean` | 132 | yes | `8cd96d529989af0cabe673ddcb8737a2680a674e` |
| `apm-lean/ConstructionTargets/CoefficientModelLocal.lean` | 114 | yes | `d5fc86bde4bd87f7ee934d9001fa754d9124f456` |
| `apm-lean/ConstructionTargets/CoefficientModelLocal.md` | 15 | yes | `d5fc86bde4bd87f7ee934d9001fa754d9124f456` |
| `apm-lean/ConstructionTargets/CoefficientMovingSphereCenter.lean` | 83 | yes | `871330d204d7123544de83b01a08f03ee4799a74` |
| `apm-lean/ConstructionTargets/CoefficientMovingSphereCenter.md` | 11 | yes | `871330d204d7123544de83b01a08f03ee4799a74` |
| `apm-lean/ConstructionTargets/CoefficientNeighborhoodLocal.lean` | 111 | yes | `a80d438beb226fc6049065706e4f51444e0a2ad4` |
| `apm-lean/ConstructionTargets/CoefficientNeighborhoodLocal.md` | 13 | yes | `a80d438beb226fc6049065706e4f51444e0a2ad4` |
| `apm-lean/ConstructionTargets/CoefficientOrthogonalRadial.lean` | 115 | yes | `cbd921f6177307b5341ec6e9a66aa7830d4ec154` |
| `apm-lean/ConstructionTargets/CoefficientOrthogonalRadial.md` | 15 | yes | `cbd921f6177307b5341ec6e9a66aa7830d4ec154` |
| `apm-lean/ConstructionTargets/CoefficientOrthogonalSphere.lean` | 249 | yes | `788855cf349b8be015b18eee50e7715b52a66667` |
| `apm-lean/ConstructionTargets/CoefficientOrthogonalSphere.md` | 15 | yes | `788855cf349b8be015b18eee50e7715b52a66667` |
| `apm-lean/ConstructionTargets/CoefficientOverlapNormalization.lean` | 152 | yes | `237dcad73487fe524354edb74123fe837d5cb962` |
| `apm-lean/ConstructionTargets/CoefficientOverlapNormalization.md` | 13 | yes | `237dcad73487fe524354edb74123fe837d5cb962` |
| `apm-lean/ConstructionTargets/CoefficientOverlapPair.lean` | 140 | yes | `c9ca1f19065f0c079760f8c6f9227c5d6c248945` |
| `apm-lean/ConstructionTargets/CoefficientOverlapPair.md` | 13 | yes | `c9ca1f19065f0c079760f8c6f9227c5d6c248945` |
| `apm-lean/ConstructionTargets/CoefficientOverlapRelative.lean` | 128 | yes | `94b0259c67f7223c4938d3021da18cbaa3175062` |
| `apm-lean/ConstructionTargets/CoefficientOverlapRelative.md` | 11 | yes | `94b0259c67f7223c4938d3021da18cbaa3175062` |
| `apm-lean/ConstructionTargets/CoefficientOverlapTransition.lean` | 230 | yes | `3efa5cad095e40711f8843f92e8b8345a8316d1f` |
| `apm-lean/ConstructionTargets/CoefficientOverlapTransition.md` | 13 | yes | `3efa5cad095e40711f8843f92e8b8345a8316d1f` |
| `apm-lean/ConstructionTargets/CoefficientPairNaturality.lean` | 115 | yes | `8a43bbdecfed605f6a144eb28e4e74b635a5b253` |
| `apm-lean/ConstructionTargets/CoefficientPairNaturality.md` | 13 | yes | `8a43bbdecfed605f6a144eb28e4e74b635a5b253` |
| `apm-lean/ConstructionTargets/CoefficientPairSequence.lean` | 81 | yes | `e66d6787b182b899404ae0e7da2ec57089749c22` |
| `apm-lean/ConstructionTargets/CoefficientPairSequence.md` | 15 | yes | `e66d6787b182b899404ae0e7da2ec57089749c22` |
| `apm-lean/ConstructionTargets/CoefficientPushoutRelative.lean` | 72 | yes | `16723870b8f8bac73a04e6a8b053641ec47d221b` |
| `apm-lean/ConstructionTargets/CoefficientPushoutRelative.md` | 13 | yes | `16723870b8f8bac73a04e6a8b053641ec47d221b` |
| `apm-lean/ConstructionTargets/CoefficientRadialPair.lean` | 94 | yes | `0b805b3d8a900604fb887e3b2b075ddcd95c57d7` |
| `apm-lean/ConstructionTargets/CoefficientRadialPair.md` | 13 | yes | `0b805b3d8a900604fb887e3b2b075ddcd95c57d7` |
| `apm-lean/ConstructionTargets/CoefficientRelativeSmall.lean` | 91 | yes | `568d4a99f0c68ff1783418b7c7fe89ab2bb5e8dc` |
| `apm-lean/ConstructionTargets/CoefficientRelativeSmall.md` | 34 | yes | `568d4a99f0c68ff1783418b7c7fe89ab2bb5e8dc` |
| `apm-lean/ConstructionTargets/CoefficientScaledBoundaryNormalization.lean` | 85 | yes | `e10459ce658ef4ef9658cccf1f6ec1cc3e8d3600` |
| `apm-lean/ConstructionTargets/CoefficientScaledBoundaryNormalization.md` | 11 | yes | `e10459ce658ef4ef9658cccf1f6ec1cc3e8d3600` |
| `apm-lean/ConstructionTargets/CoefficientSecondChartBoundary.lean` | 80 | yes | `bd4a32af71ac87b15ef685642519c89948d48d84` |
| `apm-lean/ConstructionTargets/CoefficientSecondChartBoundaryMotion.lean` | 60 | yes | `41e1e96d924c20182cf8fb888a7cf48ba13458e9` |
| `apm-lean/ConstructionTargets/CoefficientSingularPrism.lean` | 239 | yes | `eea08bb2c491498ef19b7024ccf5f86d9d6c40df` |
| `apm-lean/ConstructionTargets/CoefficientSingularPrism.md` | 32 | yes | `eea08bb2c491498ef19b7024ccf5f86d9d6c40df` |
| `apm-lean/ConstructionTargets/CoefficientSingularSubdivision.lean` | 224 | yes | `7ba91c22931e88abd31450ef81a1bc4adb945424` |
| `apm-lean/ConstructionTargets/CoefficientSingularSubdivision.md` | 32 | yes | `7ba91c22931e88abd31450ef81a1bc4adb945424` |
| `apm-lean/ConstructionTargets/CoefficientSmallComparison.lean` | 162 | yes | `ad9d15f31e8b970d705d3b46f4c0482f69d5aca5` |
| `apm-lean/ConstructionTargets/CoefficientSmallComparison.md` | 34 | yes | `ad9d15f31e8b970d705d3b46f4c0482f69d5aca5` |
| `apm-lean/ConstructionTargets/CoefficientSmallPreservation.lean` | 198 | yes | `4939bd7e057ced86a7c6ed89e736143261927f41` |
| `apm-lean/ConstructionTargets/CoefficientSmallPreservation.md` | 32 | yes | `4939bd7e057ced86a7c6ed89e736143261927f41` |
| `apm-lean/ConstructionTargets/CoefficientSphereAntipodal.lean` | 126 | yes | `9e8352926c5896dd66aaf93eae771391865b0fac` |
| `apm-lean/ConstructionTargets/CoefficientSphereAntipodal.md` | 13 | yes | `9e8352926c5896dd66aaf93eae771391865b0fac` |
| `apm-lean/ConstructionTargets/CoefficientSphereEndpoints.lean` | 193 | yes | `dfa06cecfa45962b8152a8035128b1c977c33b01` |
| `apm-lean/ConstructionTargets/CoefficientSphereEndpoints.md` | 15 | yes | `dfa06cecfa45962b8152a8035128b1c977c33b01` |
| `apm-lean/ConstructionTargets/CoefficientSphereShift.lean` | 165 | yes | `0a94275eecb3e7a059de4816cb4a8b06c8d7e339` |
| `apm-lean/ConstructionTargets/CoefficientSphereShift.md` | 15 | yes | `0a94275eecb3e7a059de4816cb4a8b06c8d7e339` |
| `apm-lean/ConstructionTargets/CoefficientTripleOverlap.lean` | 116 | yes | `a4977876b1ebb365bc8d06208da4fc7f18ca2228` |
| `apm-lean/ConstructionTargets/CoefficientVertexPrism.lean` | 194 | yes | `c87641d5b6608827ee76c19f50ae21801553575c` |
| `apm-lean/ConstructionTargets/CoefficientVertexPrism.md` | 33 | yes | `c87641d5b6608827ee76c19f50ae21801553575c` |
| `apm-lean/ConstructionTargets/CompactSurfaceTriangleCover.lean` | 59 | yes | `dce382db8ea5b6c8dee5c8b18f3b0fc6338108f7` |
| `apm-lean/ConstructionTargets/CompactTransverseArcIntersections.lean` | 87 | yes | `0f8248aa09e2cf9df2246f22b963ce10a101271b` |
| `apm-lean/ConstructionTargets/CoveringLiftRestriction.lean` | 77 | yes | `3830e6d3a77240a763f428b7d7d5412c19499a1f` |
| `apm-lean/ConstructionTargets/CoveringSimplexTransfer.lean` | 45 | yes | `753cb1e75f7e271eefb3c49c27a5d6ee018d8ad1` |
| `apm-lean/ConstructionTargets/CoveringTransferChain.lean` | 73 | yes | `164692ffb310606a149de4a27a2e2cbf0a17a324` |
| `apm-lean/ConstructionTargets/CoveringTransferDegree.lean` | 37 | yes | `cdd6e4106062e7ee220add31b154afb8f55892dd` |
| `apm-lean/ConstructionTargets/FiberPredicateTransfer.lean` | 48 | yes | `638aeee0b5d9bf53664d2db05a4a47efbd6a9e8a` |
| `apm-lean/ConstructionTargets/GraphMetricComparison.lean` | 217 | yes | `f882696c8da80267fff63443a43247be0f355c80` |
| `apm-lean/ConstructionTargets/GraphMetricComparison.md` | 21 | yes | `f882696c8da80267fff63443a43247be0f355c80` |
| `apm-lean/ConstructionTargets/IncidenceParameterProjection.lean` | 75 | yes | `cc3261525c486f5926995c6e582362658e70c44b` |
| `apm-lean/ConstructionTargets/LinearGraphDensity.lean` | 87 | yes | `f52931597246cf0a47e933920adfa02143319517` |
| `apm-lean/ConstructionTargets/LinearGraphDensity.md` | 11 | yes | `f52931597246cf0a47e933920adfa02143319517` |
| `apm-lean/ConstructionTargets/LinearGraphMeasure.lean` | 73 | yes | `773bf66a17f555bcfa561f3d6b65f18b65e8ea8d` |
| `apm-lean/ConstructionTargets/LinearGraphMeasure.md` | 11 | yes | `773bf66a17f555bcfa561f3d6b65f18b65e8ea8d` |
| `apm-lean/ConstructionTargets/LiteralCoveringPullback.lean` | 96 | yes | `f57572c751cfd302578d81f27e63de6498c52c88` |
| `apm-lean/ConstructionTargets/LocalTransverseDifference.lean` | 46 | yes | `61b2f309247354b812375a42f1696f65856cd3bf` |
| `apm-lean/ConstructionTargets/LocalTransverseExtensionChart.lean` | 39 | yes | `e69c078acad42ca017e665ff68e238ad8aac6b62` |
| `apm-lean/ConstructionTargets/ManifoldRegularFiberGlobalAtlas.lean` | 81 | yes | `62d6817a673d5d0fbce70691e0808b85e31bdd63` |
| `apm-lean/ConstructionTargets/ManifoldRegularFiberTangentImage.lean` | 102 | yes | `822296cada11aa7ea4fe97840543b092eb03f29b` |
| `apm-lean/ConstructionTargets/ManifoldTargetFiberChartExtension.lean` | 74 | yes | `87eec33a44aa52ae296a1a6575c6f3b5f6421624` |
| `apm-lean/ConstructionTargets/ManifoldTargetIncidenceChartExistence.lean` | 88 | yes | `45d19f67a564afeae1dc9dc648af535471261e56` |
| `apm-lean/ConstructionTargets/ManifoldTargetIncidenceCoordinateTransport.lean` | 49 | yes | `6c88882b137ae584373de69074f938159c3faefd` |
| `apm-lean/ConstructionTargets/ManifoldTargetIncidenceDifference.lean` | 61 | yes | `ec1c707a1cc5d8d86de9cf053c579d0578afd8ad` |
| `apm-lean/ConstructionTargets/ManifoldTargetIncidenceGlobalAtlas.lean` | 192 | yes | `7593f33c5eda754661e5169a50fd3dd72ef5589e` |
| `apm-lean/ConstructionTargets/ManifoldTargetIncidenceImmersion.lean` | 102 | yes | `c5d0be94674823aaf1b85f6c3152783d8430aef2` |
| `apm-lean/ConstructionTargets/ManifoldTargetIncidenceLocalization.lean` | 43 | yes | `f4c002fcf61a50d47acd217c9c5b7fe196d76667` |
| `apm-lean/ConstructionTargets/ManifoldTargetIncidenceSmoothCharts.lean` | 184 | yes | `ab87b24fd4909eb34a40fbb3d14d9ca19900cf96` |
| `apm-lean/ConstructionTargets/ManifoldTargetIncidenceTangent.lean` | 102 | yes | `b831e4012f68281dfa003ca1b59d14a766a3cab1` |
| `apm-lean/ConstructionTargets/ManifoldTargetRegularFiberCharts.lean` | 128 | yes | `c20972de0f36d273644ea424e28d49ff411cb029` |
| `apm-lean/ConstructionTargets/ManifoldTargetRegularFiberGlobalAtlas.lean` | 82 | yes | `7dd6810529b01300cc375dbd5a30706e47aacaa5` |
| `apm-lean/ConstructionTargets/ManifoldTargetRegularFiberTangentImage.lean` | 103 | yes | `73de6ddd70496c3c807fd5f4836fe82dab1aa634` |
| `apm-lean/ConstructionTargets/NullWindingPreimageTriviality.lean` | 40 | yes | `0f3d11b9bcc54dda5419df74b6d8e726c81136c8` |
| `apm-lean/ConstructionTargets/PlaneArcSmallTranslation.lean` | 117 | yes | `8b3aa9a1bed98a184d0d8ea2d43a8afcdb50d7d4` |
| `apm-lean/ConstructionTargets/ScaledTriangleNeighborhood.lean` | 48 | yes | `dfe31bd074dedf6fab48fa0e87badbc0d79fea42` |
| `apm-lean/ConstructionTargets/StandardTriangleEmbedding.lean` | 74 | yes | `45a52ad71f5b91454d457126a2320dac46d33800` |
| `apm-lean/ConstructionTargets/SurfaceNestedTriangleCover.lean` | 140 | yes | `cc19e0bb884a344db82c69ad4c434849a3e6f595` |
| `apm-lean/ConstructionTargets/TransverseDifferenceKernel.lean` | 39 | yes | `1d45605fb372cb48d047f52b37f4be24dd8472b1` |
| `apm-lean/ConstructionTargets/TransverseDifferenceRegularity.lean` | 54 | yes | `d775d4083df884f250a89e5ac7e5137748f12cde` |
| `apm-lean/ConstructionTargets/TransverseIncidenceChosenAtlas.lean` | 185 | yes | `7587067571c0e30ada8f76bc2199ccb2450a1081` |
| `apm-lean/ConstructionTargets/TransverseIncidenceGeometry.lean` | 163 | yes | `970a6b8be94bdc7b73f529283e1db607aea176ba` |
| `apm-lean/ConstructionTargets/TransverseIncidenceManifold.lean` | 82 | yes | `d56071ca6089d347fb5cb9ebe1cc5368005deae9` |
| `apm-lean/ConstructionTargets/TransverseIncidenceTangent.lean` | 101 | yes | `82d876986a2e972df752b10321b458f2ea54569a` |
| `apm-lean/ConstructionTargets/TwoOpenFreeProductMapping.lean` | 148 | yes | `5ff0a7d76d3546f2c62d9d7f1140eab4eee4066e` |
| `apm-lean/ConstructionTargets/TwoOpenPointSpanComparison.lean` | 81 | yes | `bf1d6bb906793851edd2327c910b3b582b814153` |
| `apm-lean/TOPOLOGY-DEPENDENCY-DAG.md` | 1357 | no | `f57ce4659db7de171ae3e99bdb187c2a742c298e` |
| `apm-lean/docs/TN-topology-closure-experiments.md` | 1132 | no | `f57ce4659db7de171ae3e99bdb187c2a742c298e` |
| `apm-lean/holes/labs/topology-contract/worklist.edn` | 76106 | no | `3ee99c3b38d8715e633ee82ce75ef0798077e6f6` |
| `apm-lean/holes/labs/topology-contract/worklist2.edn` | 48730 | no | `b6fca559de68fd200bc8834484716f0df9bb212f` |
| `apm-lean/problems/m02J04/lean/Main.lean` | 1858 | no | `5d062b269b9041535741aa9a6a14bda70bb70b76` |
| `apm-lean/problems/m02J05/lean/Main.lean` | 203 | no | `71d4b6c8962f6ec73d6ca212cd20bba241a36e23` |
| `apm-lean/problems/t00A01/lean/Main.lean` | 509 | no | `3612428cad65c85e6006c7488ea8eec4620a0181` |
| `apm-lean/problems/t00J01/lean/Main.lean` | 236 | no | `589b09f99ce484739c6a1637de5f703c183c17a1` |
| `apm-lean/problems/t94J08/lean/Main.lean` | 250 | no | `69eb3d52d58a9189da8413b0f26bc09ddd90f035` |
| `apm-lean/problems/t95J04/lean/Main.lean` | 198 | no | `a13852f35c671f0d40d4bbf58e832710563a4ef7` |
| `apm-lean/problems/t96J07/STATEMENT-REPAIR.md` | 31 | yes | `6f475b3c40e2a2528a0efd7e17c89f8f1af904a7` |
| `apm-lean/problems/t96J07/lean/Main.lean` | 107 | no | `6f475b3c40e2a2528a0efd7e17c89f8f1af904a7` |

</details>

<details><summary>Expand complete commit list</summary>

```text
apm-lean b6fca559de68fd200bc8834484716f0df9bb212f 2026-09-10T00:26:46Z topology-loop: record oriented-checkpoint18-t00a03-exact-pants-decomposition-1 transition invoke-1788999901696-17423-b688e835
apm-lean 3ee99c3b38d8715e633ee82ce75ef0798077e6f6 2026-09-10T00:27:12Z topology-loop: record t96j05-local-radial-counterexample-preservation-refill31 transition invoke-1788999939901-17425-4d9952eb
apm-lean 44b411f033a6d05c251d925327d5f1ce9a484acc 2026-09-10T00:29:33Z topology-loop: record oriented-checkpoint18-t00a03-exact-pants-decomposition-1 transition invoke-1789000009107-17427-85cee1c6
apm-lean 6b0bba2dc79a397c15ddf3fabb1f6c81aa776277 2026-09-10T00:32:20Z topology-loop: record oriented-checkpoint18-coefficient-affine-realization-naturality-1 transition invoke-1789000176008-17432-7655982d
apm-lean 334b5ce6b82ae888f7fdc36301600c5b46b0193f 2026-09-10T00:33:10Z topology-loop: record checkpoint-full-dag-refill-32-after-antipodal-and-manifold-fiber-charts transition invoke-1789000037365-17429-a26398dd
apm-lean ffb357f532bd183024c14cfe10bc04246beb9d33 2026-09-10T00:33:39Z Prove explicit-coefficient affine realization naturality
apm-lean 5d062b269b9041535741aa9a6a14bda70bb70b76 2026-09-10T00:33:56Z m02J04: establish catenary and derivative infrastructure
apm-lean f35037284c98fdf5b03334328deb7b629acbc8d3 2026-09-10T00:34:26Z topology-loop: record oriented-checkpoint18-coefficient-affine-realization-naturality-1 transition invoke-1789000342566-17435-83ceaca7
apm-lean 2239af262e0724498718b7f7e9a83a787d373069 2026-09-10T00:34:47Z topology-loop: record checkpoint-full-dag-refill-32-after-antipodal-and-manifold-fiber-charts transition invoke-1789000395829-17437-9754f4df
apm-lean 6d41ad7f0debdcf49241a36f98c2228d1741da43 2026-09-10T00:36:17Z topology-loop: record oriented-checkpoint18-coefficient-affine-realization-naturality-1 transition invoke-1789000469125-17439-a5a8c278
apm-lean 2d3a43bfed352989e8a8e7473ad6770ccd3fff7b 2026-09-10T00:37:05Z m02J04: add catenary calibration inequalities
apm-lean b4c1b9a8341918f6c1d00320ff1654c5d59c51e4 2026-09-10T00:38:23Z topology-loop: record oriented-checkpoint19-pants-and-affine-naturality-refill transition invoke-1789000579256-17444-01f978e1
apm-lean 75823166e8283b3983a5747a5999c765ef5adf37 2026-09-10T00:38:28Z topology-loop: record t95j04-frozen-sphere-image-nullity-closure-probe-refill32 transition invoke-1789000494151-17441-1530d110
apm-lean a13852f35c671f0d40d4bbf58e832710563a4ef7 2026-09-10T00:39:18Z Close t95J04 with literal sphere Hausdorff image nullity
apm-lean 2a019935219a61d696c2697f7016600741827981 2026-09-10T00:39:50Z topology-loop: record oriented-checkpoint19-pants-and-affine-naturality-refill transition invoke-1789000705972-17447-7729e02c
apm-lean be7d3a68365f8149f4d2860423409747a86014d6 2026-09-10T00:40:04Z topology-loop: record t95j04-frozen-sphere-image-nullity-closure-probe-refill32 transition invoke-1789000714269-17449-cf3b0cea
apm-lean 8ab01c0a59a676ec79f36f05816fbfa733014315 2026-09-10T00:40:25Z m02J04: evaluate catenary area and package integration by parts
apm-lean 16bf830834060fd030f94de1cbffc83a05475414 2026-09-10T00:41:24Z topology-loop: record t95j04-frozen-sphere-image-nullity-closure-probe-refill32 transition invoke-1789000811470-17453-38632ef7
apm-lean a1c9365d51b9e46196008124ee77d7159afde77f 2026-09-10T00:41:59Z topology-loop: record oriented-checkpoint19-t94j09-exact-polygon-normalization-1 transition invoke-1789000792290-17451-ff32feae
apm-lean e2fc34e7af4b5c186162a2fdcad4b9af4cf7a52b 2026-09-10T00:43:06Z topology-loop: record oriented-checkpoint19-t94j09-exact-polygon-normalization-1 transition invoke-1789000922001-17458-82a8ccc2
apm-lean d2354ef921e6ab28b19387a92b0b408c33633004 2026-09-10T00:44:20Z topology-loop: record manifold-regular-fiber-global-atlas-inclusion-probe-refill32 transition invoke-1789000890064-17456-ea1c4cb1
apm-lean 4da81cdb90fef8fe9baf4a8ab7ceeb2d1ee9c8cf 2026-09-10T00:44:23Z m02J04: add partial regularity and two-variable chain rule
apm-lean 62d6817a673d5d0fbce70691e0808b85e31bdd63 2026-09-10T00:45:21Z Bank manifold regular-fiber atlas and same-atlas smooth inclusion
apm-lean 586f6ab881c10c3518a5b845acdecaf7f2513046 2026-09-10T00:46:15Z topology-loop: record manifold-regular-fiber-global-atlas-inclusion-probe-refill32 transition invoke-1789001065263-17462-61bd767e
apm-lean d42956f8b94ee8fceae03035be27c1f77b9362cd 2026-09-10T00:47:02Z m02J04: prove smooth Beltrami identity
apm-lean d90de0216c7d3e2490366872f8eacdd48a06c592 2026-09-10T00:47:50Z topology-loop: record manifold-regular-fiber-global-atlas-inclusion-probe-refill32 transition invoke-1789001180573-17465-23c9c0aa
apm-lean eff34b0ef3dc910dc655af1742cf4978d70f2857 2026-09-10T00:48:54Z topology-loop: record oriented-checkpoint19-coefficient-barycentric-affine-change-1 transition invoke-1789000988368-17460-60d99a3d
apm-lean 3f41ed7d65c5ebf382f57492c65e5fe1460676e2 2026-09-10T00:50:18Z Construct coefficient-general barycentric affine-change compatibility
apm-lean 658ab8c2981ea43901ca7ad02754bf93d9bec41c 2026-09-10T00:50:55Z topology-loop: record checkpoint-full-dag-refill-33-after-sphere-nullity-and-manifold-fiber-atlas transition invoke-1789001278467-17467-505c2ec4
apm-lean b4a05513210101d3981e3c70a1dafdaccebb3613 2026-09-10T00:50:56Z m02J04: derive pointwise first-variation formula
apm-lean 4fcd2930c11641f3f5db99790e3d701f61c74dc0 2026-09-10T00:51:03Z topology-loop: record oriented-checkpoint19-coefficient-barycentric-affine-change-1 transition invoke-1789001337080-17470-ab4efc55
apm-lean ec40ae3ee980361ba87c8a0f88d9946c8de94e50 2026-09-10T00:52:34Z topology-loop: record oriented-checkpoint19-coefficient-barycentric-affine-change-1 transition invoke-1789001467985-17474-faea7287
apm-lean 76e1612d30fe1c21b79326b4813430cbfcc268e9 2026-09-10T00:52:36Z topology-loop: record checkpoint-full-dag-refill-33-after-sphere-nullity-and-manifold-fiber-atlas transition invoke-1789001461232-17472-9b510539
apm-lean 296a66f7160a2513b3c3b4a91af6d8ae5a66e9b2 2026-09-10T00:53:48Z m02J04: establish first-variation density regularity
apm-lean b70e2f1f2d3c9352e74967350a2f4e41db3e8c26 2026-09-10T00:54:55Z topology-loop: record t01j04-frozen-closed-neighborhood-preimage-closure-probe-refill33 transition invoke-1789001564411-17479-9220a744
apm-lean 9488304e4ca1e2774691ba84b7a9c656c95e71bf 2026-09-10T00:56:11Z topology-loop: record t01j04-frozen-closed-neighborhood-preimage-closure-probe-refill33 transition invoke-1789001701181-17482-a7bed4c8
apm-lean a6dc3ac0feaf18cbe346baeb6ae47fdeb3eca2e0 2026-09-10T00:56:42Z m02J04: bound first variation on compact parameter strips
apm-lean d6dc52ef4c058070b0922081c2bdfe27bbf76737 2026-09-10T00:56:47Z topology-loop: record oriented-checkpoint20-polygon-and-barycentric-refill transition invoke-1789001556566-17477-eda2ef48
apm-lean edff17ddc4a6dfe899637ea8f6bcb30ae790478b 2026-09-10T00:58:16Z topology-loop: record oriented-checkpoint20-polygon-and-barycentric-refill transition invoke-1789001810392-17486-588a490f
apm-lean b761b8aff251714b65db25dc083149d5aec63549 2026-09-10T01:00:29Z m02J04: extend first variation across parameter strip
apm-lean c2deab5499fa84e16aec1a8cc0376bc6d741ff8d 2026-09-10T01:02:12Z topology-loop: record manifold-regular-fiber-tangent-image-probe-refill33 transition invoke-1789001777208-17484-4a0a0674
apm-lean 228f855d00d7a9e82374075c82ec69abd80429dc 2026-09-10T01:02:46Z topology-loop: record oriented-checkpoint20-t96j07-parenthesized-axis-repair-1 transition invoke-1789001899180-17489-2afd69fd
apm-lean 822296cada11aa7ea4fe97840543b092eb03f29b 2026-09-10T01:03:24Z Bank chosen manifold-fiber inclusion injectivity and tangent kernel equality
apm-lean 092e5108ae24ff028b9a1f6178a2e9fb35467a05 2026-09-10T01:04:08Z topology-loop: record manifold-regular-fiber-tangent-image-probe-refill33 transition invoke-1789002138029-17492-c6dc94b9
apm-lean 6f475b3c40e2a2528a0efd7e17c89f8f1af904a7 2026-09-10T01:05:39Z Repair t96J07 implication precedence and prove corrected statement
apm-lean 0d71e5fcf0b86a52e5f48af8f7471a4bb8ea1eab 2026-09-10T01:05:44Z topology-loop: record manifold-regular-fiber-tangent-image-probe-refill33 transition invoke-1789002254080-17496-cc6a0d7a
apm-lean fcc0497069805c23c40d2644da14d7528cc672ec 2026-09-10T01:06:54Z topology-loop: record oriented-checkpoint20-t96j07-parenthesized-axis-repair-1 transition invoke-1789002169281-17494-9e948408
apm-lean f57ce4659db7de171ae3e99bdb187c2a742c298e 2026-09-10T01:07:28Z Record reviewed t95J04 closure and retain historical experiment evidence
apm-lean 8ff8b3f0b5a7cfbd2faaf41a72972b0e65c884d0 2026-09-10T01:08:00Z topology-loop: record t95j04-reviewed-closure-documentation-refill33 transition invoke-1789002349389-17499-cf60d3d8
apm-lean 42d7b09cbcd7c609bd4deaaf80ee38ebe44a2030 2026-09-10T01:08:05Z m02J04: derive weak first variation from stationarity
apm-lean 21712a63198cd4b3fcb7e667931d5e0a16db083e 2026-09-10T01:08:21Z topology-loop: record oriented-checkpoint20-t96j07-parenthesized-axis-repair-1 transition invoke-1789002417270-17501-59e6c3e7
apm-lean ccfc84a96217a046485e65322284b8c471978173 2026-09-10T01:11:03Z m02J04: reduce stationarity to weak Euler-Lagrange residual
apm-lean b89be40c8732e8e79fb2b0cbfc9b6e3980280881 2026-09-10T01:12:08Z topology-loop: record oriented-checkpoint20-coefficient-singular-subdivision-assembly-1 transition invoke-1789002503769-17505-6ba58697
apm-lean a9639d2efd3577244b5b86275703bce76c5f55b3 2026-09-10T01:12:58Z topology-loop: record t95j04-reviewed-closure-documentation-refill33 transition invoke-1789002485978-17503-4bf96279
apm-lean 7ba91c22931e88abd31450ef81a1bc4adb945424 2026-09-10T01:13:27Z Assemble coefficient-general singular subdivision chain map
apm-lean b06a52ad9dc9ffc2581743dc06771c1a7e7e4372 2026-09-10T01:14:37Z topology-loop: record oriented-checkpoint20-coefficient-singular-subdivision-assembly-1 transition invoke-1789002731103-17509-d432ae56
apm-lean b2fbf056de1e9cad700b36a86b069e7b63791455 2026-09-10T01:15:07Z m02J04: construct localized smooth endpoint test bumps
apm-lean c806a226e31237866aab54ecc78a1ed2be44d94d 2026-09-10T01:16:03Z topology-loop: record oriented-checkpoint20-coefficient-singular-subdivision-assembly-1 transition invoke-1789002879492-17513-00cdd4ce
apm-lean ce00018ae639c70ef4b5335183036497be85acf2 2026-09-10T01:16:16Z topology-loop: record checkpoint-full-dag-refill-34-after-neighborhood-preimage-and-tangent-image transition invoke-1789002783104-17511-d25720b3
apm-lean 089d1ef416b5b2990b446c748b4bc0d06d2a8a7a 2026-09-10T01:18:17Z topology-loop: record checkpoint-full-dag-refill-34-after-neighborhood-preimage-and-tangent-image transition invoke-1789002983822-17518-0a09fccc
apm-lean cdba4522ad1db8fe95bd05847f83d2869ec6d998 2026-09-10T01:19:15Z topology-loop: record oriented-checkpoint21-axis-repair-and-singular-subdivision-refill transition invoke-1789002969318-17516-f18b623f
apm-lean 0a86bf7260667cd14ef903a69148b77ec5dc3d49 2026-09-10T01:19:53Z topology-loop: record t94a06-frozen-normal-translation-closure-probe-refill34 transition invoke-1789003103492-17520-47a12897
apm-lean d564f2a5856094965d9bce191d11aa65898f196c 2026-09-10T01:20:19Z m02J04: prove Euler-Lagrange from stationarity
apm-lean 07f37f04c5af4614020489871367b84e55ef2490 2026-09-10T01:20:22Z topology-loop: record oriented-checkpoint21-axis-repair-and-singular-subdivision-refill transition invoke-1789003157409-17522-76b9a7c4
apm-lean d781b621470d307b4a43bb0d5c2814e5a45439be 2026-09-10T01:21:09Z topology-loop: record t94a06-frozen-normal-translation-closure-probe-refill34 transition invoke-1789003199649-17524-f12be4cf
apm-lean caab2c64088a0e775c353fe7b52a6cafb9fc7ca6 2026-09-10T01:23:49Z m02J04: consolidate variational principles and area regularity
apm-lean 2c8290203ffc258d1aac6a18eeccd3ad9074aed4 2026-09-10T01:23:50Z topology-loop: record regular-fiber-manifold-target-local-charts-probe-refill34 transition invoke-1789003279897-17529-525d94ae
apm-lean 6126845547a69a1cb918c0c2f00bc9b99b8ffd56 2026-09-10T01:24:32Z topology-loop: record oriented-checkpoint21-t96j05-nonconstant-radius-repair-1 transition invoke-1789003225273-17526-2c085f77
apm-lean 87eec33a44aa52ae296a1a6575c6f3b5f6421624 2026-09-10T01:24:40Z Bank target-chart extension and exact local fiber identification
apm-lean afdbe32f70953712a5d4ce588ea849e7d57eed5f 2026-09-10T01:25:25Z topology-loop: record manifold-target-fiber-chart-extension-implementation-refill34 transition invoke-1789003435534-17531-bc82a457
apm-lean 709cb6d1c4f2ca50365d94e65226658895b69fec 2026-09-10T01:26:00Z topology-loop: record oriented-checkpoint21-t96j05-nonconstant-radius-repair-1 transition invoke-1789003475270-17533-584e1f7d
apm-lean eb9b53ac8bd1189aea5e4e6f9dd6747340d44b6c 2026-09-10T01:27:01Z topology-loop: record manifold-target-fiber-chart-extension-implementation-refill34 transition invoke-1789003530847-17536-dd9a0b14
apm-lean 831bc857ee0acb841807c65a84f1e9984d77bdf5 2026-09-10T01:27:57Z m02J04: prove global endpoint-variation area bound
apm-lean 4354cac355e00e243ac19e0859400a1ccba0b1ec 2026-09-10T01:28:47Z topology-loop: record oriented-checkpoint21-coefficient-subdivision-homotopy-1 transition invoke-1789003562560-17538-901e69e2
apm-lean 36b08b65fa96c86dfaa7b9d108e16124014b3f99 2026-09-10T01:29:38Z topology-loop: record regular-fiber-manifold-target-local-charts-probe-refill34 transition invoke-1789003627951-17540-599be07e
apm-lean c87641d5b6608827ee76c19f50ae21801553575c 2026-09-10T01:29:56Z Construct coefficient-general ordered-vertex prism
apm-lean 638aeee0b5d9bf53664d2db05a4a47efbd6a9e8a 2026-09-10T01:30:36Z Bank topological predicate-fiber transfer and exact domain specifications
apm-lean 3f5b24fe204cd0add28e3ef209e78340de8c7752 2026-09-10T01:31:14Z topology-loop: record coefficient-general-ordered-vertex-prism-1 transition invoke-1789003729335-17542-aa14ea5f
apm-lean 4f9488d4f21cdebe0bfa645a99c0992951cabd8e 2026-09-10T01:31:16Z topology-loop: record fiber-predicate-transfer-implementation-refill34 transition invoke-1789003785198-17545-252db8d1
apm-lean 19c3c95a8a9d098d6d00e57bb45b559819a2b816 2026-09-10T01:32:12Z m02J04: formalize normalized calibration defect
apm-lean a91af22390dace81101f88caf37a3da940a132e8 2026-09-10T01:32:42Z topology-loop: record coefficient-general-ordered-vertex-prism-1 transition invoke-1789003876642-17547-3efb57ea
apm-lean 7358d06e24ff3b6dcef9f0dfeca629d5c5fab053 2026-09-10T01:32:58Z topology-loop: record fiber-predicate-transfer-implementation-refill34 transition invoke-1789003883788-17549-d1260d19
apm-lean 9e4d4685f8e8e0a2b6287d1d2b880510619cfe37 2026-09-10T01:36:58Z m02J04: integrate normalized calibration mixed term
apm-lean dbeeffd0173acf1888782fb1e3aa79324e424ce6 2026-09-10T01:38:58Z topology-loop: record regular-fiber-manifold-target-local-charts-probe-refill34 transition invoke-1789003985578-17553-eb02cb9c
apm-lean 2087ac276fcc8b14845742f6083dfcaf8c5cd000 2026-09-10T01:39:53Z topology-loop: record oriented-checkpoint21-coefficient-subdivision-homotopy-1 transition invoke-1789003964928-17551-8a975bca
apm-lean c20972de0f36d273644ea424e28d49ff411cb029 2026-09-10T01:39:58Z Bank manifold-target literal fiber charts and smooth overlaps
apm-lean f8ddf4218e3ca7b53c1845602191a88ac52c58f7 2026-09-10T01:40:28Z m02J04: globalize normalized calibration bound
apm-lean 45c5efd810206e5c792101b4f24642b07c1c6dc4 2026-09-10T01:40:57Z topology-loop: record regular-fiber-manifold-target-local-charts-probe-refill34 transition invoke-1789004344195-17557-b14f9b43
apm-lean eea08bb2c491498ef19b7024ccf5f86d9d6c40df 2026-09-10T01:41:08Z Construct coefficient-general singular subdivision homotopy
apm-lean 2563266a1f945e80565ab9526f39373f56e6699f 2026-09-10T01:42:24Z topology-loop: record oriented-checkpoint21-coefficient-subdivision-homotopy-1 transition invoke-1789004396185-17559-1033d469
apm-lean e2d16403cd97f8a05079969d9d933501368a56d8 2026-09-10T01:42:38Z topology-loop: record regular-fiber-manifold-target-local-charts-probe-refill34 transition invoke-1789004464076-17561-0878509f
apm-lean e5a876ecd8406b32ca7433ff2b6ade79e6b6df9d 2026-09-10T01:43:31Z m02J04: add outer-region closed calibration
apm-lean ad2ef10fa4d086369391618e792dabe5f4dea69f 2026-09-10T01:43:51Z topology-loop: record oriented-checkpoint21-coefficient-subdivision-homotopy-1 transition invoke-1789004546504-17564-df3bc0a6
apm-lean 7ba06a9ebac26ba95c30d53f10001ff175679f2b 2026-09-10T01:45:26Z Record reviewed t96J07 corrected closure and historical refutation
apm-lean 702d83b953c057a1b56230b0c87fc1a72fba6a5e 2026-09-10T01:46:19Z topology-loop: record oriented-checkpoint21-t96j07-corrected-closure-documentation-1 transition invoke-1789004633973-17568-8db8e273
apm-lean 39ac401decf535c0ee8d5a09cd79067a58e47e59 2026-09-10T01:46:39Z topology-loop: record checkpoint-full-dag-refill-35-after-normal-translation-and-target-fiber-charts transition invoke-1789004568086-17566-fe10b757
apm-lean 51f3ad0371ee114180e107351cac9e9cc4b29a00 2026-09-10T01:47:26Z topology-loop: record oriented-checkpoint21-t96j07-corrected-closure-documentation-1 transition invoke-1789004781629-17571-7b47e35b
apm-lean a74cf1b8907948f25d287f956abf3c87f3c8c003 2026-09-10T01:47:34Z m02J04: build outer calibration primitive
apm-lean 8d61f1be5f25e6e99634ab05c869cb86f2ba86f5 2026-09-10T01:48:19Z topology-loop: record checkpoint-full-dag-refill-35-after-normal-translation-and-target-fiber-charts transition invoke-1789004805921-17573-6e1a03ea
apm-lean 286b6f2343dc88996d2dedfa38d5a41d8c3dd521 2026-09-10T01:49:57Z topology-loop: record t02a08-frozen-integer-degree-closure-probe-refill35 transition invoke-1789004906849-17577-aaf6f106
apm-lean 704de5e12b7101b3e8d7240d9526746def3411e5 2026-09-10T01:50:15Z topology-loop: record oriented-checkpoint22-radial-repair-and-subdivision-homotopy-refill transition invoke-1789004849158-17575-13b72c1b
apm-lean 2970b56fb38ea4767c116a94aa5d6cdc1cff7ee5 2026-09-10T01:51:22Z topology-loop: record oriented-checkpoint22-radial-repair-and-subdivision-homotopy-refill transition invoke-1789005017633-17582-c5bb7574
apm-lean ca69318762a07e559c432202779c66e218217ab8 2026-09-10T01:51:42Z m02J04: evaluate outer calibration at catenary endpoint
apm-lean 0628659bfa7d6d23890a0d8895f0e77e3df392f1 2026-09-10T01:51:42Z topology-loop: record t02a08-frozen-integer-degree-closure-probe-refill35 transition invoke-1789005004579-17580-4d3bea20
apm-lean f18ab58d07393c4a15362b870ad65829a6cc1ce4 2026-09-10T01:53:24Z topology-loop: record manifold-target-fiber-global-atlas-inclusion-probe-refill35 transition invoke-1789005110579-17586-c025faee
apm-lean 71c0f8c8ec1bd053fe7c08a06eccc57303540855 2026-09-10T01:53:52Z m02J04: prove outer-class catenary minimality
apm-lean 7dd6810529b01300cc375dbd5a30706e47aacaa5 2026-09-10T01:54:34Z Bank manifold-target fiber atlas and same-atlas smooth inclusion
apm-lean b7504576796e53789d095c2f1e03a7795b4bc0e6 2026-09-10T01:54:34Z topology-loop: record oriented-checkpoint22-t01a05-connected-cover-repair-1 transition invoke-1789005088112-17584-ea4adfe9
apm-lean 11184eb4347d82aaa4baed6b3cab4e355046e0b5 2026-09-10T01:55:21Z topology-loop: record manifold-target-fiber-global-atlas-inclusion-probe-refill35 transition invoke-1789005211009-17589-39c5bbd4
apm-lean 5a35882528e2fc18346a16381d6b40786302c4bb 2026-09-10T01:56:21Z topology-loop: record oriented-checkpoint22-t01a05-connected-cover-repair-1 transition invoke-1789005276731-17591-aea9e05a
apm-lean c4ed1936034447e7dd82c162120774d5f3893c46 2026-09-10T01:56:41Z topology-loop: record manifold-target-fiber-global-atlas-inclusion-probe-refill35 transition invoke-1789005326995-17594-da9df8cf
apm-lean 9eedc7feaed6b71855c8835e0d72dca1a84a8da3 2026-09-10T01:56:57Z m02J04: bound radial variation on subintervals
apm-lean 9b6944bcb06fa7b8772c09d5cfb5e0ef792e6d69 2026-09-10T01:57:49Z Verify existing t96J07 corrected closure and retain intrinsic comparison boundary
apm-lean f18520841e5c991bd80ba1e534f95b08bb63a7ad 2026-09-10T01:58:39Z topology-loop: record t96j07-reviewed-corrected-closure-documentation-refill35 transition invoke-1789005406784-17598-d187e6dd
apm-lean a0eb5d342089d6d0736c98f8279aa9fa7023d33e 2026-09-10T01:59:28Z topology-loop: record oriented-checkpoint22-coefficient-small-chain-preservation-1 transition invoke-1789005383477-17596-7e15eed0
apm-lean bd3f2275df13886c767267b303c8a2fa122bc9be 2026-09-10T01:59:40Z m02J04: combine below-neck variation bounds
apm-lean b777a8df8a4dbb983875946ae09f43eeac36f0d4 2026-09-10T01:59:58Z topology-loop: record t96j07-reviewed-corrected-closure-documentation-refill35 transition invoke-1789005526122-17601-7a24f98f
apm-lean acbc19f4dbdbf16fa96171575974ee2966c09f97 2026-09-10T02:00:41Z Realize finite signed chains with explicit coefficients
apm-lean dd72b1129edbcc2eb07f3aae5738563c9af55014 2026-09-10T02:01:56Z topology-loop: record coefficient-general-finite-signed-realization-1 transition invoke-1789005570976-17603-a900ae78
apm-lean 976753f83c1523a776008b156baf6166d0e91ded 2026-09-10T02:02:16Z m02J04: retain horizontal cost in coarse bound
apm-lean 7ee7b7da44cf4cb8cec180954ca286c1bad946f6 2026-09-10T02:02:55Z topology-loop: record checkpoint-full-dag-refill-36-after-degree-and-target-atlas transition invoke-1789005603960-17605-23af22e3
apm-lean 164a012a231f0dd1c8faab1c096cde398ae49579 2026-09-10T02:03:24Z topology-loop: record coefficient-general-finite-signed-realization-1 transition invoke-1789005718545-17608-17c81fd6
apm-lean 31eaf2353e2ae73f139b1cc6f0f0d0ad2d030063 2026-09-10T02:04:52Z topology-loop: record checkpoint-full-dag-refill-36-after-degree-and-target-atlas transition invoke-1789005780937-17610-3b67ff63
apm-lean b51372b20a8706b03497f5fb2f29afcb916ac92a 2026-09-10T02:05:42Z m02J04: parameterize closed calibration by neck radius
apm-lean 7523795ea76b88239391141d40f3c39f7e1200c5 2026-09-10T02:06:09Z topology-loop: record t98a01-frozen-fivefold-cover-closure-probe-refill36 transition invoke-1789005898141-17615-36974fc1
apm-lean 8601ebf27f168edb8569d47811d605dff695b923 2026-09-10T02:07:16Z topology-loop: record oriented-checkpoint22-coefficient-small-chain-preservation-1 transition invoke-1789005806501-17613-9a48d911
apm-lean bf4c79390b0631dc10d4e8e71653f2f2c6728b41 2026-09-10T02:07:47Z topology-loop: record t98a01-frozen-fivefold-cover-closure-probe-refill36 transition invoke-1789005975143-17617-e2057336
apm-lean 48d169e3d43292dd9916c3f09f205b84e6226d29 2026-09-10T02:08:03Z m02J04: integrate minimum-radius calibration
apm-lean 15c18c05f76310b8424165ca8a056f447ece1591 2026-09-10T02:08:29Z Identify finite realizations with coefficient-general operators
apm-lean f66230ffd97e6e6cf78c0562fb14b6ec23b6d57e 2026-09-10T02:09:27Z topology-loop: record manifold-target-fiber-tangent-kernel-probe-refill36 transition invoke-1789006073909-17622-62e170ab
apm-lean 0c8860d9cd55ba5237ef631b188d01bb7bcd1b29 2026-09-10T02:09:46Z topology-loop: record coefficient-general-finite-operator-realization-1 transition invoke-1789006039088-17620-f504d78e
apm-lean 0c20b8c75326cef4a639e2f7758aa3b252e5319f 2026-09-10T02:10:30Z m02J04: close deep-neck minimality regime
apm-lean 73de6ddd70496c3c807fd5f4836fe82dab1aa634 2026-09-10T02:10:39Z Bank manifold-target fiber inclusion injectivity and tangent kernel equality
apm-lean e74549e3e78e12a932bc710b1edce591b545920b 2026-09-10T02:11:13Z topology-loop: record coefficient-general-finite-operator-realization-1 transition invoke-1789006188521-17627-018ff60e
apm-lean 144b4394f23ce19e99b1088cae74386e053f49db 2026-09-10T02:11:31Z topology-loop: record manifold-target-fiber-tangent-kernel-probe-refill36 transition invoke-1789006174982-17625-043de1c2
apm-lean 06f215df8fa1ff19604b32e737c7fdcb4357a4df 2026-09-10T02:13:10Z topology-loop: record manifold-target-fiber-tangent-kernel-probe-refill36 transition invoke-1789006299884-17632-de51f2eb
apm-lean 727d16e442c3f6e89899dfef6eb9028917e6a080 2026-09-10T02:15:47Z topology-loop: record checkpoint-full-dag-refill-37-after-fivefold-and-target-tangent transition invoke-1789006395643-17634-1908872b
apm-lean 4f6af84d8254a96b208d79b8a13aacc6e5223bb8 2026-09-10T02:16:09Z topology-loop: record oriented-checkpoint22-coefficient-small-chain-preservation-1 transition invoke-1789006276062-17629-8115412d
apm-lean 551f580eeedefbfb6bf56d213701cf32dea51374 2026-09-10T02:16:59Z m02J04: prove sharp split minimum calibration
apm-lean 649165a1823feb77b2e90b6658506e93dc381fb4 2026-09-10T02:17:23Z topology-loop: record checkpoint-full-dag-refill-37-after-fivefold-and-target-tangent transition invoke-1789006552577-17637-1533bfae
apm-lean 4939bd7e057ced86a7c6ed89e736143261927f41 2026-09-10T02:17:26Z Restrict coefficient-general subdivision and prism to small chains
apm-lean c50b55559bcc2d04462b92a867a77857c6590fbc 2026-09-10T02:18:38Z topology-loop: record oriented-checkpoint22-coefficient-small-chain-preservation-1 transition invoke-1789006571787-17639-4c1225e9
apm-lean 84513edd479d594197c88eabe389eca919e415a1 2026-09-10T02:19:23Z topology-loop: record t00j02-frozen-duality-producer-closure-probe-refill37 transition invoke-1789006649280-17641-8048850c
apm-lean 0e41ea81fbf3b9be2839bcf4261ca283c90aec92 2026-09-10T02:19:54Z m02J04: derive parameter primitive closed-form components
apm-lean b3fe65b7f774ca5ebb0120f79e42032400327abc 2026-09-10T02:20:05Z topology-loop: record oriented-checkpoint22-coefficient-small-chain-preservation-1 transition invoke-1789006720756-17644-7102535c
apm-lean 581d56f6318d28057c9ee8e4f4469e256df3390a 2026-09-10T02:20:59Z topology-loop: record t00j02-frozen-duality-producer-closure-probe-refill37 transition invoke-1789006768733-17646-7857c903
apm-lean 108fff80654ab8b559b2da784a288b13411a267b 2026-09-10T02:22:54Z topology-loop: record oriented-checkpoint23-connected-cover-and-small-preservation-refill transition invoke-1789006807716-17648-28f812cd
apm-lean dc0a83a8d942ece57085075d8c5ef5c729e44d49 2026-09-10T02:22:59Z m02J04: identify parameter primitive closed form
apm-lean 8ea051eef84049afe4acb8eafb6d0590f2f4197b 2026-09-10T02:24:21Z topology-loop: record oriented-checkpoint23-connected-cover-and-small-preservation-refill transition invoke-1789006976313-17653-7e526154
apm-lean 6f3377b9347928e0c16dd97b6fb0a5a3c28c09f6 2026-09-10T02:25:23Z topology-loop: record transverse-vector-fiber-product-geometry-probe-refill37 transition invoke-1789006864932-17651-9388dbb3
apm-lean 4d2b48933ccc6eca2f287a7eef858a8ce2cfa9dc 2026-09-10T02:25:44Z m02J04: differentiate primitive in neck parameter
apm-lean d775d4083df884f250a89e5ac7e5137748f12cde 2026-09-10T02:26:13Z Bank transverse difference-map smoothness derivative and regularity
apm-lean 876c45d3e44e5fa2b0b3c018dcfd8378b9fb93cd 2026-09-10T02:26:59Z topology-loop: record transverse-difference-regularity-implementation-refill37 transition invoke-1789007129066-17658-36f75170
apm-lean 7d04e2670540aafa01f08b61751eb448ef8c4f72 2026-09-10T02:28:01Z m02J04: derive scalar calibration derivative
apm-lean c2b2c4d437c6c9c54b89d0618cd7f795dd4ee2c4 2026-09-10T02:28:15Z topology-loop: record transverse-difference-regularity-implementation-refill37 transition invoke-1789007224423-17661-38db1531
apm-lean cc3bf4c8d4f7f8619d7dc798d0a4d179fed763bc 2026-09-10T02:30:28Z m02J04: reduce scalar derivative sign to cosh bound
apm-lean f2c387748ec322929489c9dae6d7430d74beebf5 2026-09-10T02:30:31Z topology-loop: record transverse-vector-fiber-product-geometry-probe-refill37 transition invoke-1789007300408-17663-d3c5a39b
apm-lean 1d45605fb372cb48d047f52b37f4be24dd8472b1 2026-09-10T02:31:31Z Bank actual transverse difference kernel and derived dimension
apm-lean e1445517483a3db8d03ea587d34f81827c593226 2026-09-10T02:31:56Z topology-loop: record oriented-checkpoint23-coefficient-eventual-smallness-1 transition invoke-1789007064178-17656-a8ca1bb1
apm-lean c5c491d4065145b36650f4bcb2292def9f5266a2 2026-09-10T02:32:09Z topology-loop: record transverse-difference-kernel-implementation-refill37 transition invoke-1789007436909-17666-2b71d7b5
apm-lean d5dc57563e4821d17af8770e0ba961fc707e06d4 2026-09-10T02:32:55Z m02J04: prove arcosh bound on intermediate range
apm-lean 1e40c51ca8c3f07db1f3ecdbbd2505f1801ac183 2026-09-10T02:33:07Z Construct finite coordinates for actual coefficient-general chains
apm-lean a6d1752d2e8628fbd1a1b3b75e62c9377e13a046 2026-09-10T02:33:45Z topology-loop: record transverse-difference-kernel-implementation-refill37 transition invoke-1789007534862-17671-a9f7ab23
apm-lean 4166c75c14aa7ac24b24bc8fb25fbb24d3f13a30 2026-09-10T02:34:05Z topology-loop: record coefficient-general-actual-chain-finite-support-1 transition invoke-1789007518344-17669-0ff5d5d9
apm-lean cc6f98c09e14a6b925e508e36ed59b9568206776 2026-09-10T02:36:41Z m02J04: close scalar calibration comparison
apm-lean e2816408718319ca5820065e01313696481126d0 2026-09-10T02:38:16Z topology-loop: record coefficient-general-actual-chain-finite-support-1 transition invoke-1789007651168-17676-fe37c755
apm-lean 020b933f14f05cc65ea038945bfa96bb74b6d37f 2026-09-10T02:39:28Z topology-loop: record transverse-vector-fiber-product-geometry-probe-refill37 transition invoke-1789007635166-17674-c5242c56
apm-lean 5e01f134527a2121217b673ffb08455c97832300 2026-09-10T02:41:05Z topology-loop: record transverse-vector-fiber-product-geometry-probe-refill37 transition invoke-1789007974334-17681-98a01185
apm-lean 6af8154bede460de7c5fb90ae43c090c4cc07386 2026-09-10T02:41:58Z Complete global catenary minimality proof
apm-lean 2cc05a156abdfedc46c1dee763f07aac25f5c41a 2026-09-10T02:43:05Z topology-loop: record oriented-checkpoint23-coefficient-eventual-smallness-1 transition invoke-1789007898702-17679-c1708a86
apm-lean 8b914a58dea9bc6dbcc8e20230d3dad9215b63fc 2026-09-10T02:44:03Z Land 1 certified solver proofs onto master
apm-lean 0f36d6c5f7c6fe1d99ded92e9888192dd4ac21ca 2026-09-10T02:44:19Z Prove eventual smallness for actual coefficient-general chains
apm-lean 30b4b0bbb52f2f66a739fa0dbfb5a9a23a010c44 2026-09-10T02:45:04Z topology-loop: record checkpoint-full-dag-refill-38-after-duality-and-transverse-product transition invoke-1789008071009-17683-32a77836
apm-lean b73192a162b195600d57de7cc1956df9ba93abc4 2026-09-10T02:45:40Z topology-loop: record oriented-checkpoint23-coefficient-eventual-smallness-1 transition invoke-1789008191159-17685-6f385496
apm-lean c942fd187dd98d35f1a3ffe9eb4481843fdeb5df 2026-09-10T02:46:44Z topology-loop: record checkpoint-full-dag-refill-38-after-duality-and-transverse-product transition invoke-1789008311597-17688-d36af4e2
apm-lean 4f249ecd43a9650dfdf260320710fb3f8b5de29a 2026-09-10T02:47:09Z topology-loop: record oriented-checkpoint23-coefficient-eventual-smallness-1 transition invoke-1789008342628-17690-980f35e7
apm-lean 92ff07964e15351afda1a2994a20fb586cd6e364 2026-09-10T02:49:06Z topology-loop: record t95j05-frozen-hausdorff-integral-closure-probe-refill38 transition invoke-1789008410991-17693-3485e7f2
apm-lean 0fc020e2fb7d1014464531bd6b363c7cab3f34af 2026-09-10T02:49:40Z topology-loop: record oriented-checkpoint24-eventual-smallness-and-consumer-refill transition invoke-1789008431788-17695-abbe2b57
apm-lean 053b511b16d09cf5bd7001d791110b34044f290e 2026-09-10T02:50:25Z topology-loop: record t95j05-frozen-hausdorff-integral-closure-probe-refill38 transition invoke-1789008552252-17697-5e6feba7
apm-lean 98c26c674925d13be99d8e098fff86194f847c5f 2026-09-10T02:51:10Z topology-loop: record oriented-checkpoint24-eventual-smallness-and-consumer-refill transition invoke-1789008583924-17700-6b2d69f5
apm-lean 3c46f30c2df6f2c00e49a3652b7b425cd243ad7d 2026-09-10T02:52:23Z topology-loop: record null-winding-preimage-constant-loop-audit-probe-refill38 transition invoke-1789008632006-17702-71767084
apm-lean 0f3d11b9bcc54dda5419df74b6d8e726c81136c8 2026-09-10T02:53:09Z Bank exact constant-loop triviality of null-winding preimage predicate
apm-lean c440aeebe1fea48a8a0152f83be8f218a6a05bae 2026-09-10T02:53:17Z topology-loop: record oriented-checkpoint24-coefficient-small-chain-homology-comparison-1 transition invoke-1789008672722-17704-57ec0da6
apm-lean 40ab0566f5b5c6ec964c5968c720444f64cf3227 2026-09-10T02:54:03Z topology-loop: record null-winding-preimage-constant-loop-audit-probe-refill38 transition invoke-1789008749514-17706-272632b8
apm-lean 42e4bb083eec774f433dd1d223d89c4c3d4a5f37 2026-09-10T02:54:27Z Construct iterated subdivision homotopies and homology identities
apm-lean c29bc7414b8ab10f0f740192e12752b8acc770a6 2026-09-10T02:55:26Z topology-loop: record coefficient-general-iterated-subdivision-homotopy-1 transition invoke-1789008800567-17708-c5311160
apm-lean b63cbd28d50cd1086c5bc984c26b3f9ec426bb16 2026-09-10T02:55:42Z topology-loop: record null-winding-preimage-constant-loop-audit-probe-refill38 transition invoke-1789008850671-17710-0133905c
apm-lean 864947195348a979bc7d24fb5d68a5e6accdc64d 2026-09-10T02:56:54Z topology-loop: record coefficient-general-iterated-subdivision-homotopy-1 transition invoke-1789008929184-17712-ee5ae71e
apm-lean 3609379acd9b8af1adc2098302b6350ad8fb6b7d 2026-09-10T02:59:08Z topology-loop: record product-self-model-recharting-probe-refill38 transition invoke-1789008953383-17714-1c71eabf
apm-lean ab99abe095db8d5d4d0a783b732bd8dc461408a7 2026-09-10T03:00:22Z Bank actual chart-carrier transport and product model dimension
apm-lean 762b8ba3adc5221b416b279c64ba8abfad486c4b 2026-09-10T03:01:27Z topology-loop: record charted-space-model-transport-implementation-refill38 transition invoke-1789009155099-17718-08983b84
apm-lean 97eca62286849343fca82de9ea33a2d5d4228d54 2026-09-10T03:02:23Z topology-loop: record oriented-checkpoint24-coefficient-small-chain-homology-comparison-1 transition invoke-1789009017085-17716-aff631b9
apm-lean ad9d15f31e8b970d705d3b46f4c0482f69d5aca5 2026-09-10T03:03:36Z Prove coefficient-general small-chain homology comparison
apm-lean cf785fd3b8fe395bc81406a9c5180f8eddc39307 2026-09-10T03:04:53Z topology-loop: record oriented-checkpoint24-coefficient-small-chain-homology-comparison-1 transition invoke-1789009346379-17722-2a2667cc
apm-lean 94d4381150f666cb6b88a7f031cce3b9a7d7d2f5 2026-09-10T03:06:20Z topology-loop: record oriented-checkpoint24-coefficient-small-chain-homology-comparison-1 transition invoke-1789009495871-17725-e087a06b
apm-lean 46801356ab996574b167058a7f3acbe2fe629885 2026-09-10T03:07:07Z topology-loop: record charted-space-model-transport-implementation-refill38 transition invoke-1789009295089-17720-ac66ef2d
apm-lean b42fb178d656e3c4bc4db0b9b06b70b1f0e83ed4 2026-09-10T03:07:54Z preserve f211 m02J04 student attempt 1
apm-lean e3a60ef234faf08670197a365197ff95df04b5b2 2026-09-10T03:08:49Z topology-loop: record oriented-checkpoint25-small-chain-comparison-and-consumer-refill transition invoke-1789009582908-17727-d2a2b504
apm-lean aa3854740fbeebfab21fb6d37198b49a68b54636 2026-09-10T03:10:16Z topology-loop: record oriented-checkpoint25-small-chain-comparison-and-consumer-refill transition invoke-1789009732035-17732-be0b4aa1
apm-lean 65a4b158c4fdbfd6196391f5def4541bdabfd57f 2026-09-10T03:13:04Z topology-loop: record oriented-checkpoint25-coefficient-relative-small-comparison-1 transition invoke-1789009819158-17734-e4df84ab
apm-lean 4e98d2ec29709dbe236bdf37d44241c09ced6741 2026-09-10T03:14:11Z Construct coefficient-general cokernel homology comparison
apm-lean d2757de04823c6a026f31d5150fe0d0b746c565b 2026-09-10T03:14:11Z topology-loop: record product-self-model-recharting-probe-refill38 transition invoke-1789009633446-17729-767fbb70
apm-lean 57882e7536192eb968309c8b5364c4159642d69d 2026-09-10T03:15:12Z topology-loop: record coefficient-general-cokernel-homology-comparison-1 transition invoke-1789009987223-17738-7a1b762a
apm-lean 94c20be8f8066c470a0d1426f0886addcb2ea7a7 2026-09-10T03:15:13Z Bank smooth compatibility and identity maps for transported chart model
apm-lean f679bfd071bdad455ba6459035ce9494d8b09cbe 2026-09-10T03:16:08Z topology-loop: record charted-space-model-smooth-transport-implementation-refill38 transition invoke-1789010056664-17740-762769e2
apm-lean 886e13fd557508f2bc461a608f769a330033a514 2026-09-10T03:16:41Z topology-loop: record coefficient-general-cokernel-homology-comparison-1 transition invoke-1789010115230-17742-ed1a544c
apm-lean 2c0312c6d719623b8c8c4005243f361e768748d7 2026-09-10T03:17:26Z topology-loop: record charted-space-model-smooth-transport-implementation-refill38 transition invoke-1789010174692-17744-17adc0cc
apm-lean a032a92aac0aa1111e555d8c2ec9aa89ca0a5fb9 2026-09-10T03:19:51Z topology-loop: record oriented-checkpoint25-coefficient-relative-small-comparison-1 transition invoke-1789010204512-17746-a019046d
apm-lean 568d4a99f0c68ff1783418b7c7fe89ab2bb5e8dc 2026-09-10T03:21:05Z Construct coefficient-general relative small-chain comparison
apm-lean d06c3feddebc9c5345549f330a0e764ad0d072a3 2026-09-10T03:22:21Z topology-loop: record oriented-checkpoint25-coefficient-relative-small-comparison-1 transition invoke-1789010394161-17750-4103df6d
apm-lean b7233d94a5e18bc94179179ebfeb585767ec05fa 2026-09-10T03:23:10Z topology-loop: record product-self-model-recharting-probe-refill38 transition invoke-1789010253597-17748-d4f4d9b9
apm-lean fa7cde09f171040201f9da44d4af6fc3999c7bf1 2026-09-10T03:23:51Z topology-loop: record oriented-checkpoint25-coefficient-relative-small-comparison-1 transition invoke-1789010544196-17752-38d86e88
apm-lean 83b802294ab07567098cd9bd10bc224c0d5384a3 2026-09-10T03:26:00Z topology-loop: record oriented-checkpoint26-relative-comparison-and-consumer-refill transition invoke-1789010634483-17756-074001e0
apm-lean 1fccbcfd7db5ae77b01d851f3442d5dfd2f3f5dc 2026-09-10T03:27:30Z topology-loop: record oriented-checkpoint26-relative-comparison-and-consumer-refill transition invoke-1789010763365-17758-7b026b00
apm-lean afa7b6005988aaee5651332d5765a77ee1734c71 2026-09-10T03:27:44Z Bank chosen-model derivative transport and product recharting
apm-lean b05b3b4aac2b9e45fe546b609678180eaed58a0c 2026-09-10T03:28:30Z topology-loop: record product-self-model-recharting-probe-refill38 transition invoke-1789010598675-17754-8e9fa057
apm-lean 20c82ca9eb339d147b521f8596308175679f8309 2026-09-10T03:29:59Z topology-loop: record oriented-checkpoint26-coefficient-simplicial-pushout-relative-1 transition invoke-1789010853619-17761-c7f23b50
apm-lean 18bedae49dbfa97a8da4f66beafadc60ac08f077 2026-09-10T03:30:07Z topology-loop: record product-self-model-recharting-probe-refill38 transition invoke-1789010916237-17763-021c47c7
apm-lean 16723870b8f8bac73a04e6a8b053641ec47d221b 2026-09-10T03:32:40Z Construct coefficient-general pushout relative chain isomorphism
apm-lean 3125fcf7f32e9d552cd9574d8b562b12af5d2c45 2026-09-10T03:33:47Z topology-loop: record oriented-checkpoint26-coefficient-simplicial-pushout-relative-1 transition invoke-1789011002534-17765-0e5e25e5
apm-lean a4aa11ea7985af5fbe77663323be183c097d2d36 2026-09-10T03:34:06Z topology-loop: record checkpoint-full-dag-refill-39-after-integral-recharting-and-null-winding-audit transition invoke-1789011014678-17767-44bf135c
apm-lean 9bbd5dc070186441b8571c0061b5cbab99bd65da 2026-09-10T03:35:15Z topology-loop: record oriented-checkpoint26-coefficient-simplicial-pushout-relative-1 transition invoke-1789011229999-17769-3e064538
apm-lean 989d88a8dda2c5c5b2c9815e4335922573ac887d 2026-09-10T03:35:44Z topology-loop: record checkpoint-full-dag-refill-39-after-integral-recharting-and-null-winding-audit transition invoke-1789011251802-17771-84f161ab
apm-lean 06070c699898054add6dceef6539355002554090 2026-09-10T03:40:43Z topology-loop: record oriented-checkpoint27-pushout-relative-and-consumer-refill transition invoke-1789011317945-17773-b3a8efb8
apm-lean 3b1f4c550a3ca7888913d630deba8655ada96b71 2026-09-10T03:42:01Z topology-loop: record t00a01-frozen-connected-sum-closure-probe-refill39 transition invoke-1789011350433-17775-c54caadb
apm-lean a7bc08ea16d2e40c8bdd6906cccc8e3dd36f55cd 2026-09-10T03:42:13Z topology-loop: record oriented-checkpoint27-pushout-relative-and-consumer-refill transition invoke-1789011645663-17779-b6c8b1ba
apm-lean bbb46bd3507ef8a0c1b3b89a27214b602d3b2bda 2026-09-10T03:45:25Z topology-loop: record t00a01-frozen-connected-sum-closure-probe-refill39 transition invoke-1789011729189-17781-021b0770
apm-lean f3dea6e4d43d3f958cb817d2c5b1e1ab808d1057 2026-09-10T03:48:45Z topology-loop: record oriented-checkpoint27-coefficient-actual-subspace-excision-1 transition invoke-1789011740373-17783-d2bc771b
apm-lean 5e52400d5d3b3bdc949cdfb044c9186123891664 2026-09-10T03:50:34Z Construct coefficient-general actual-subspace excision
apm-lean 307d41d6d23b940240c7120b85f24e6e4cb94ba8 2026-09-10T03:51:26Z topology-loop: record transverse-vector-incidence-atlas-tangent-probe-refill39 transition invoke-1789011932868-17785-6a290596
apm-lean b548bfdb54c8d186fdceeb88c96751b5068a9a48 2026-09-10T03:51:56Z topology-loop: record oriented-checkpoint27-coefficient-actual-subspace-excision-1 transition invoke-1789012128441-17787-578b44c6
apm-lean d56071ca6089d347fb5cb9ebe1cc5368005deae9 2026-09-10T03:52:35Z Bank literal transverse incidence manifold and smooth inclusion
apm-lean ad5416fc193709f8940ee52eb235eb41018693a2 2026-09-10T03:53:28Z topology-loop: record transverse-incidence-manifold-implementation-refill39 transition invoke-1789012294631-17789-ddc4359e
apm-lean e8a6f02b523490edd2cb5ab63b03181d76d04e6c 2026-09-10T03:53:47Z topology-loop: record oriented-checkpoint27-coefficient-actual-subspace-excision-1 transition invoke-1789012319312-17791-9d2bd335
apm-lean dcef8853950ede92b35d4677eb8fede093766997 2026-09-10T03:55:10Z topology-loop: record transverse-incidence-manifold-implementation-refill39 transition invoke-1789012418278-17793-70ad6890
apm-lean ed4360ad4de8c98dba9a942a50112f10a5ff1197 2026-09-10T03:56:22Z topology-loop: record oriented-checkpoint28-actual-excision-and-consumer-refill transition invoke-1789012431179-17795-83fdd437
apm-lean 07c08913537d77b0809d44c7fc6d339020059b8e 2026-09-10T03:58:10Z topology-loop: record oriented-checkpoint28-actual-excision-and-consumer-refill transition invoke-1789012584602-17800-e0e211fe
apm-lean 58ef149ce3225ef545b88055ea81903947fcf427 2026-09-10T04:00:38Z topology-loop: record transverse-vector-incidence-atlas-tangent-probe-refill39 transition invoke-1789012516912-17798-6c373317
apm-lean 8053e605cbbd082a568a94c6f2f10fd7f41366d7 2026-09-10T04:00:59Z topology-loop: record oriented-checkpoint28-coefficient-contractible-pair-sequence-1 transition invoke-1789012693172-17802-37eeb762
apm-lean 7587067571c0e30ada8f76bc2199ccb2450a1081 2026-09-10T04:01:42Z Bank chosen incidence atlas and inclusion derivative transport
apm-lean e66d6787b182b899404ae0e7da2ec57089749c22 2026-09-10T04:02:12Z Construct coefficient-general actual pair homology sequence
apm-lean 8ab85dbf892e3f922c27567e804366aca7ec322d 2026-09-10T04:02:57Z topology-loop: record transverse-incidence-chosen-atlas-implementation-refill39 transition invoke-1789012844647-17804-50a01bbd
apm-lean 26e843344ce67095353da96aa4ded9ff3fb64856 2026-09-10T04:03:29Z topology-loop: record coefficient-general-actual-pair-homology-sequence-1 transition invoke-1789012862229-17806-6b6d1c38
apm-lean de24d2c342b0e2ca13aeeff6f128be78fd10bb6a 2026-09-10T04:04:57Z topology-loop: record transverse-incidence-chosen-atlas-implementation-refill39 transition invoke-1789012983449-17808-f794442d
apm-lean d242cfa8c82090db139ac45ee01ec6490c29131b 2026-09-10T04:04:58Z topology-loop: record coefficient-general-actual-pair-homology-sequence-1 transition invoke-1789013011761-17810-2bf1e007
apm-lean 4a645918334ff4e07223e61ad4fa751c149c158c 2026-09-10T04:06:48Z preserve f211 m02J04 student attempt 3
apm-lean 48e73892108fdc618185e2819fb747b4c57df871 2026-09-10T04:09:50Z topology-loop: record oriented-checkpoint28-coefficient-contractible-pair-sequence-1 transition invoke-1789013103037-17813-cdb5c3fb
apm-lean 0a72159bc678d910de3b21550e9b772016dc507f 2026-09-10T04:11:24Z Construct coefficient-general contractible pair endpoints
apm-lean fd59ebf7418cd6c2272a5c14477255ca2c586750 2026-09-10T04:12:41Z topology-loop: record oriented-checkpoint28-coefficient-contractible-pair-sequence-1 transition invoke-1789013393475-17817-b4cadda0
apm-lean fd28ec99451a9a02b140d11eba57b6f7e39afedb 2026-09-10T04:12:57Z topology-loop: record transverse-vector-incidence-atlas-tangent-probe-refill39 transition invoke-1789013104332-17814-3eed5c8c
apm-lean 82d876986a2e972df752b10321b458f2ea54569a 2026-09-10T04:14:14Z Bank chosen incidence inclusion tangent equalizer
apm-lean 750c7e17a7baaf207e8fb4bc7bf5bd37fb189c2f 2026-09-10T04:14:30Z topology-loop: record oriented-checkpoint28-coefficient-contractible-pair-sequence-1 transition invoke-1789013564682-17820-9cca12d7
apm-lean 5e9d1220903f1c1b5202775afc6fc50f7f699bda 2026-09-10T04:15:05Z topology-loop: record transverse-incidence-tangent-implementation-refill39 transition invoke-1789013587423-17822-74aa9e05
apm-lean 8e04c6e4f7b179e7f9ecf2d8eeedaa45dbd42617 2026-09-10T04:16:44Z topology-loop: record oriented-checkpoint29-pair-sequence-and-consumer-refill transition invoke-1789013673549-17824-e6e553f7
apm-lean 97aae991c3935b233ea52b0efadc49811648acbf 2026-09-10T04:16:46Z topology-loop: record transverse-incidence-tangent-implementation-refill39 transition invoke-1789013712888-17826-d6497535
apm-lean 7178639e87ab9ceced5b76faf6ae590fb6d5230b 2026-09-10T04:18:34Z topology-loop: record oriented-checkpoint29-pair-sequence-and-consumer-refill transition invoke-1789013807702-17828-019a010e
apm-lean 469270953fe2906cc342344e34faa9af7f130455 2026-09-10T04:21:29Z topology-loop: record transverse-vector-incidence-atlas-tangent-probe-refill39 transition invoke-1789013815654-17830-a5f8ae4d
apm-lean 970a6b8be94bdc7b73f529283e1db607aea176ba 2026-09-10T04:22:46Z Bank normalized transverse incidence atlas and tangent geometry
apm-lean 1ee33a5da824806c0fdc55d70be853b5ea2d3f5c 2026-09-10T04:23:05Z topology-loop: record oriented-checkpoint29-coefficient-geometric-sphere-shift-1 transition invoke-1789013917241-17832-7612c1fd
apm-lean 6b63383fbcea5ab57b83dde2741bbf9e660b7e68 2026-09-10T04:23:49Z topology-loop: record transverse-vector-incidence-atlas-tangent-probe-refill39 transition invoke-1789014095670-17835-da6b39b9
apm-lean 0b805b3d8a900604fb887e3b2b075ddcd95c57d7 2026-09-10T04:24:15Z Construct coefficient-general radial relative comparison
apm-lean f67dbe65f95ad9825659150dad1e291a93b9a700 2026-09-10T04:25:35Z topology-loop: record coefficient-general-radial-relative-comparison-1 transition invoke-1789014188336-17837-47a62151
apm-lean a7c99308f0313f6f05cf2a2954f5c54ef4d94fc1 2026-09-10T04:25:47Z topology-loop: record transverse-vector-incidence-atlas-tangent-probe-refill39 transition invoke-1789014236309-17840-920a2707
apm-lean 3edee51749b85780e3f0a9cff59c66f7e14dde71 2026-09-10T04:27:03Z topology-loop: record coefficient-general-radial-relative-comparison-1 transition invoke-1789014337680-17842-cf6734f2
apm-lean 71d4b6c8962f6ec73d6ca212cd20bba241a36e23 2026-09-10T04:29:55Z m02J05 prove derivative of Young primitive
apm-lean 6bf816149d1683a9403b375a87a0def75870bf5a 2026-09-10T04:33:12Z topology-loop: record oriented-checkpoint29-coefficient-geometric-sphere-shift-1 transition invoke-1789014425953-17847-2f478851
apm-lean 0a94275eecb3e7a059de4816cb4a8b06c8d7e339 2026-09-10T04:34:37Z Construct coefficient-general geometric sphere shift
apm-lean adf561b7892d2b7f634695642b9105f8bc33abbc 2026-09-10T04:35:11Z topology-loop: record checkpoint-full-dag-refill-40-after-connected-sum-and-incidence-atlas transition invoke-1789014359660-17845-f42d3417
apm-lean c2a9123d46e6bf86673d4592f2ef6b6d43340a24 2026-09-10T04:35:59Z topology-loop: record oriented-checkpoint29-coefficient-geometric-sphere-shift-1 transition invoke-1789014794681-17850-d930aed3
apm-lean 9138dcff0820632fac8475ae775e83809dccf274 2026-09-10T04:36:01Z m02J05 establish convex Young primitive
apm-lean 8ffd4692cfd2ac6e05543de14bbf367e9e17dae9 2026-09-10T04:38:09Z topology-loop: record checkpoint-full-dag-refill-40-after-connected-sum-and-incidence-atlas transition invoke-1789014917549-17852-fe8c5dbf
apm-lean 4f025d0d63f6c0bf2e7504f911a38b8311231a96 2026-09-10T04:39:49Z topology-loop: record oriented-checkpoint29-coefficient-geometric-sphere-shift-1 transition invoke-1789014962618-17854-ab60e27b
apm-lean e562b512f1a85ad849d67ae303a6dbe7f244206c 2026-09-10T04:40:07Z topology-loop: record t94j09-frozen-polygon-homology-closure-probe-refill40 transition invoke-1789015095598-17857-b2438f04
apm-lean 910f763955ea9e7c1ddf663740401696ca13d881 2026-09-10T04:41:25Z m02J05 prove convexity of modular class
apm-lean 06d3c87c7d9f924ccb765d332cfd1ab3054a0afc 2026-09-10T04:41:26Z topology-loop: record t94j09-frozen-polygon-homology-closure-probe-refill40 transition invoke-1789015213597-17861-c743a66c
apm-lean 6d7826d54f54aa1e87599d07306b540250e9d5a7 2026-09-10T04:42:19Z topology-loop: record oriented-checkpoint30-geometric-sphere-shift-and-consumer-refill transition invoke-1789015191986-17859-529c2e64
apm-lean 0b3af42ee6030a9f3780257ce8d0eed4f17437ca 2026-09-10T04:42:45Z topology-loop: record compact-surface-polygon-presentation-probe-refill40 transition invoke-1789015293749-17863-b96cd79c
apm-lean e128c610c4d78943dd5e7771c79d74f692043b4b 2026-09-10T04:44:02Z topology-loop: record compact-surface-polygon-presentation-probe-refill40 transition invoke-1789015371566-17868-05ce044c
apm-lean 4e90e57b0772db47f0b59387f53726b04c16d6d3 2026-09-10T04:44:31Z m02J05 close convexity and submodule bridge
apm-lean cd126632e87b22a8677a41020a16c71f978be973 2026-09-10T04:44:50Z topology-loop: record oriented-checkpoint30-geometric-sphere-shift-and-consumer-refill transition invoke-1789015344487-17865-93cb9407
apm-lean 49c5f2a21f8b5b5acd333d0742258823bb42d87a 2026-09-10T04:46:21Z topology-loop: record manifold-target-incidence-local-charts-probe-refill40 transition invoke-1789015448599-17870-71b1ccd0
apm-lean 397f25fb2a12be0113edffbf285832b0a3823d99 2026-09-10T04:47:18Z topology-loop: record oriented-checkpoint30-coefficient-sphere-endpoints-1 transition invoke-1789015492935-17872-a135fa0b
apm-lean 97b8d6bd86fdd60b67e91e1de0ea344ea8d639b3 2026-09-10T04:47:26Z m02J05 derive complementary inverse structure
apm-lean f4c002fcf61a50d47acd217c9c5b7fe196d76667 2026-09-10T04:47:31Z Bank common target-chart incidence localization
apm-lean d39ebc396c4a95b3b722193ad1628f2cc23a4b2a 2026-09-10T04:48:18Z topology-loop: record manifold-target-incidence-localization-implementation-refill40 transition invoke-1789015587134-17875-122fbd50
apm-lean 089792c96eeca964059b880db51828277040970b 2026-09-10T04:48:27Z Normalize coefficient-general codiagonal kernel
apm-lean 2b1497bc8ec10c31c6c3dcd8229f7350f925e0f6 2026-09-10T04:49:31Z topology-loop: record coefficient-general-codiagonal-kernel-normalization-1 transition invoke-1789015641223-17877-13f447ed
apm-lean e06e92a3dfd4362c2774032ca8092f2291dd3e3e 2026-09-10T04:49:38Z topology-loop: record manifold-target-incidence-localization-implementation-refill40 transition invoke-1789015706221-17879-d14f235f
apm-lean 23f6d1e91ee0f5cd4476605441b14fd6f41b4224 2026-09-10T04:50:10Z m02J05 build Luxemburg infimum foundations
apm-lean 3b9b7e5e7ee664ce05f3d14285af6ce7196b94e7 2026-09-10T04:50:58Z topology-loop: record coefficient-general-codiagonal-kernel-normalization-1 transition invoke-1789015773700-17882-20e0b4cb
apm-lean 9e9972502b21e978de7350d40d8e6b39ed47caa0 2026-09-10T04:52:07Z topology-loop: record manifold-target-incidence-local-charts-probe-refill40 transition invoke-1789015787371-17884-af7b0587
apm-lean 6c88882b137ae584373de69074f938159c3faefd 2026-09-10T04:53:01Z Bank common target-chart derivative and transversality transport
apm-lean d623b6e417ba540c85f100d8c7e9666c80944b0f 2026-09-10T04:53:44Z topology-loop: record manifold-target-incidence-coordinate-transport-implementation-refill40 transition invoke-1789015934115-17889-94221946
apm-lean 6e2c6c2f2ac0d45b6712ea5676be3b68ee3ab615 2026-09-10T04:54:10Z topology-loop: record oriented-checkpoint30-coefficient-sphere-endpoints-1 transition invoke-1789015861321-17886-efa84464
apm-lean 4aaf856382a96a9da9106debd88cb79c39669244 2026-09-10T04:54:24Z m02J05 prove Luxemburg admissible scales exist
apm-lean d3104f9db626db36973fb78381178f8f2b6ffc8f 2026-09-10T04:55:05Z topology-loop: record manifold-target-incidence-coordinate-transport-implementation-refill40 transition invoke-1789016029555-17891-0ffb6ef5
apm-lean 58ea5c3159fa6d8042ba513b4ba948af9efb1c64 2026-09-10T04:55:24Z Normalize connected coefficient H0 maps and prove S2 H1 vanishing
apm-lean 25b13597c40f472cd4c6c3fd2b0a1f1b3ad1ce33 2026-09-10T04:56:24Z topology-loop: record coefficient-general-connected-h0-map-1 transition invoke-1789016053016-17893-f69c8b1e
apm-lean 99e3acc7c9f781c2b349176092a16d4792549e45 2026-09-10T04:57:23Z topology-loop: record manifold-target-incidence-local-charts-probe-refill40 transition invoke-1789016111681-17895-a039ad4f
apm-lean 61b2f309247354b812375a42f1696f65856cd3bf 2026-09-10T04:58:10Z Bank local transverse difference smoothness and surjectivity
apm-lean c62f318605188c9c552bdbd4b68f813447f43f00 2026-09-10T04:58:12Z topology-loop: record coefficient-general-connected-h0-map-1 transition invoke-1789016186975-17898-3f68774d
apm-lean 44de50e482389a47b9b2fffbf4efe1aac83a42b4 2026-09-10T04:58:58Z m02J05 exhibit frozen definiteness counterexample
apm-lean 3b8f19713157269309390395b1e6b9ed3dc4b037 2026-09-10T04:59:02Z topology-loop: record local-transverse-difference-implementation-refill40 transition invoke-1789016250076-17900-d579e955
apm-lean 0ccef88ea7cb1ad69f2c0c522298d228feb3cb58 2026-09-10T05:00:19Z topology-loop: record local-transverse-difference-implementation-refill40 transition invoke-1789016347773-17904-58f3d10e
apm-lean 03f338505623849220416c50e854e93ecb6cad65 2026-09-10T05:02:59Z topology-loop: record manifold-target-incidence-local-charts-probe-refill40 transition invoke-1789016426459-17907-09fbf521
apm-lean ec1c707a1cc5d8d86de9cf053c579d0578afd8ad 2026-09-10T05:04:03Z Bank actual chart-coordinate local difference packet
apm-lean 89795d4d670b046bae64185abed88ae87f007b75 2026-09-10T05:04:21Z topology-loop: record oriented-checkpoint30-coefficient-sphere-endpoints-1 transition invoke-1789016294638-17902-9888cf29
apm-lean 449472351c52834248c76ed690271c7cf751b560 2026-09-10T05:04:39Z topology-loop: record manifold-target-incidence-difference-implementation-refill40 transition invoke-1789016585374-17909-16295ca8
apm-lean 4fd454fca827b53dd129a28c7db98c29bf29a35a 2026-09-10T05:05:59Z topology-loop: record manifold-target-incidence-difference-implementation-refill40 transition invoke-1789016687412-17913-1d2ec9ff
apm-lean dfa06cecfa45962b8152a8035128b1c977c33b01 2026-09-10T05:06:09Z Construct coefficient-general sphere homology endpoints
apm-lean f4f3fa4d42645eca46c35f0192f3b233dbff6081 2026-09-10T05:08:20Z topology-loop: record manifold-target-incidence-local-charts-probe-refill40 transition invoke-1789016765592-17915-f09d6323
apm-lean e69c078acad42ca017e665ff68e238ad8aac6b62 2026-09-10T05:09:08Z Bank constructed local transverse extension fiber chart
apm-lean d6188f8673cbedc583630075c52d901812b852d4 2026-09-10T05:09:39Z topology-loop: record local-transverse-extension-chart-implementation-refill40 transition invoke-1789016907725-17923-ca015e03
apm-lean 802b55803480418b8bf5856e94273f6f97788cee 2026-09-10T05:10:45Z m02J05 statement repair: Luxemburg sublevel set needs IntegrableOn conjunct
apm-lean cb3a75974fffce7effe199dc08090a6fb6a381ca 2026-09-10T05:11:01Z topology-loop: record local-transverse-extension-chart-implementation-refill40 transition invoke-1789016988278-17929-8ff1c36a
apm-lean 9c1dd72f234cea3a8ed52ea09ea08d24664dd772 2026-09-10T05:12:23Z Remove the blank line at EOF that fails the whitespace gate
apm-lean c2f37312ff5cd54234c452f585a0b642ca2f27b2 2026-09-10T05:14:39Z topology-loop: record manifold-target-incidence-local-charts-probe-refill40 transition invoke-1789017067141-17935-563c7c77
apm-lean 654b65dadbca02cd4cfa126b68e06c5f6a93d68e 2026-09-10T05:14:51Z Record owner whitespace repair for sphere endpoint fixture
apm-lean 45d19f67a564afeae1dc9dc648af535471261e56 2026-09-10T05:15:46Z Bank literal manifold-target incidence chart existence
apm-lean 5bba0e819a3b53b5ed7204e97f612e7e662c0c19 2026-09-10T05:16:27Z topology-loop: record manifold-target-incidence-chart-existence-implementation-refill40 transition invoke-1789017295244-17943-d024c688
apm-lean 158426c202950d8a701bd47535ffe7746522a62f 2026-09-10T05:17:36Z Record the sphere-endpoints row's full commit provenance
apm-lean 1a0a8cdede20580074fc6c5dd543131749848ddc 2026-09-10T05:18:04Z topology-loop: record manifold-target-incidence-chart-existence-implementation-refill40 transition invoke-1789017392906-17946-80d7f5f2
apm-lean 886aa441d4e90cd0b61ff9921a222ef97faae4de 2026-09-10T05:20:44Z topology-loop: record oriented-checkpoint30-coefficient-sphere-endpoints-1 transition invoke-1789017499298-17950-580f5dd7
apm-lean a43ee9d2672cc6e8107e1bea909aa7c05c32cc30 2026-09-10T05:22:46Z topology-loop: record manifold-target-incidence-local-charts-probe-refill40 transition invoke-1789017489848-17948-6d23c4cb
apm-lean 475489cc67c1c1dc361c8aebcf5abde496dcf176 2026-09-10T05:23:31Z topology-loop: record oriented-checkpoint31-sphere-endpoints-and-consumer-refill transition invoke-1789017646811-17952-530c0734
apm-lean ab87b24fd4909eb34a40fbb3d14d9ca19900cf96 2026-09-10T05:23:34Z Bank manifold-target incidence charts and smooth overlaps
apm-lean fdee7db3ee1492439520adbf29f2c04bc4798d44 2026-09-10T05:24:23Z topology-loop: record manifold-target-incidence-local-charts-probe-refill40 transition invoke-1789017772041-17954-6887db45
apm-lean edc0f22aeef9b12b0dc259a04aa2a0c2ab7d7d4e 2026-09-10T05:25:00Z topology-loop: record oriented-checkpoint31-sphere-endpoints-and-consumer-refill transition invoke-1789017814258-17956-d9f478b3
apm-lean f4d0166d1fba13bc62df6db80d3dacbb833be02c 2026-09-10T05:26:01Z topology-loop: record manifold-target-incidence-local-charts-probe-refill40 transition invoke-1789017869817-17958-7e7eec9c
apm-lean 001aa2dbfb246356dea6aa5574517f7c434568f8 2026-09-10T05:27:28Z topology-loop: record oriented-checkpoint31-t00j01-sphere-identity-uptake-1 transition invoke-1789017903013-17960-4396e5a0
apm-lean 589b09f99ce484739c6a1637de5f703c183c17a1 2026-09-10T05:28:51Z Discharge t00J01 sphere identity branch with actual homology traces
apm-lean dc89e4402fb9cfd0845c91765ed49132882b36fb 2026-09-10T05:29:55Z topology-loop: record oriented-checkpoint31-t00j01-sphere-identity-uptake-1 transition invoke-1789018050479-17964-b6802d6c
apm-lean 239e9ae109fc908abdb378848a7dfa85010d8f7d 2026-09-10T05:30:40Z topology-loop: record checkpoint-full-dag-refill-41-after-polygon-and-target-incidence transition invoke-1789017967332-17962-7a286db0
apm-lean d4927bc30d40921e7b003d6762dfef3ea24b6be2 2026-09-10T05:31:45Z topology-loop: record oriented-checkpoint31-t00j01-sphere-identity-uptake-1 transition invoke-1789018197839-17966-c23934ce
apm-lean c43f6a7ab2845ec2444d19a2e86d3daff79601f8 2026-09-10T05:32:37Z topology-loop: record checkpoint-full-dag-refill-41-after-polygon-and-target-incidence transition invoke-1789018245956-17968-3992b67c
apm-lean 772367d7eea201152cc440c864a3a34c50d0ad96 2026-09-10T05:34:15Z topology-loop: record oriented-checkpoint32-sphere-identity-uptake-and-consumer-refill transition invoke-1789018307603-17970-f17d1b5c
apm-lean fd7a08c3516177afcc7c93198f08d6491fdeeb1f 2026-09-10T05:34:34Z topology-loop: record t03j04-frozen-torus-euler-closure-probe-refill41 transition invoke-1789018363277-17972-dfc20f50
apm-lean 79463f5c8e390de467bb56928661e1e7ae5ce777 2026-09-10T05:36:07Z topology-loop: record oriented-checkpoint32-sphere-identity-uptake-and-consumer-refill transition invoke-1789018462095-17974-2da51677
apm-lean 810b7f271c4c586048351a9d9de494c2609cf192 2026-09-10T05:36:11Z topology-loop: record t03j04-frozen-torus-euler-closure-probe-refill41 transition invoke-1789018480619-17976-17c65bfa
apm-lean 8126641712b539b7cffc7ac9aa7c0242958b6e9f 2026-09-10T05:41:31Z topology-loop: record compact-surface-finite-triangulation-probe-refill41 transition invoke-1789018580252-17980-f91012f7
apm-lean a05fe41ebbb21d9421e4e9513b1362cbf1954b18 2026-09-10T05:42:15Z topology-loop: record oriented-checkpoint32-coefficient-pair-connecting-naturality-1 transition invoke-1789018569698-17978-5dc02b92
apm-lean 8a43bbdecfed605f6a144eb28e4e74b635a5b253 2026-09-10T05:43:37Z Construct actual coefficient-general pair connecting naturality
apm-lean 8f2f70aa823b64fec23ba46f103a2e6e41bb0996 2026-09-10T05:44:43Z topology-loop: record oriented-checkpoint32-coefficient-pair-connecting-naturality-1 transition invoke-1789018938602-17984-d576df1b
apm-lean 1857293a5633f4d45afdc75cfc808c7d6926f299 2026-09-10T05:46:31Z topology-loop: record oriented-checkpoint32-coefficient-pair-connecting-naturality-1 transition invoke-1789019086413-17986-a3b4b783
apm-lean 0187f9b18df22b540ed5515fa8b49a057d140777 2026-09-10T05:46:49Z topology-loop: record compact-surface-finite-triangulation-probe-refill41 transition invoke-1789018896602-17982-76acd3c7
apm-lean 2b7a46e5f9eba92adc3fdab347ef353530c3bd7f 2026-09-10T05:50:03Z topology-loop: record oriented-checkpoint33-pair-naturality-and-consumer-refill transition invoke-1789019197820-17988-e1943dbb
apm-lean 6fea7995a6d789e5efb9ed67d54ffec66f0c777f 2026-09-10T05:51:05Z topology-loop: record literal-covering-pullback-probe-refill41 transition invoke-1789019214127-17990-e1f58962
apm-lean 569f8c1b5ee1ba777a0fa90ffff39323b7ec02f9 2026-09-10T05:51:32Z topology-loop: record oriented-checkpoint33-pair-naturality-and-consumer-refill transition invoke-1789019405877-17992-abc7766c
apm-lean 3d73be237f643eef0a7280faa9ec7d825bfd6255 2026-09-10T05:55:04Z topology-loop: record manifold-target-incidence-global-atlas-probe-refill41 transition invoke-1789019472388-17994-73988cff
apm-lean f57572c751cfd302578d81f27e63de6498c52c88 2026-09-10T05:55:55Z Bank literal covering pullback and fiber transport
apm-lean 6215650ec83fa7529568aa85089560723937922c 2026-09-10T05:56:22Z topology-loop: record oriented-checkpoint33-coefficient-disk-reflection-sign-1 transition invoke-1789019494918-17996-b27cd3d6
apm-lean 9ac71a5ed3dff8c95ca809fb0491394be9184c21 2026-09-10T05:56:40Z topology-loop: record literal-covering-pullback-probe-refill41 transition invoke-1789019709221-17998-61173182
apm-lean 33f36aa24191299c430871b1ab5e8468be2201cc 2026-09-10T05:58:37Z topology-loop: record literal-covering-pullback-probe-refill41 transition invoke-1789019806128-18002-428b6fd6
apm-lean 6f66faffa3b127bf4f4b2bcb480d32a81b3c1579 2026-09-10T05:59:22Z Construct coefficient-general boundary reflection coordinates
apm-lean 7593f33c5eda754661e5169a50fd3dd72ef5589e 2026-09-10T06:00:00Z Bank manifold-target global incidence atlas and smooth inclusion
apm-lean 0aee62d2fdc05b26211107c82a25795c432246e5 2026-09-10T06:00:50Z topology-loop: record coefficient-general-boundary-reflection-comparison-1 transition invoke-1789019784485-18000-7797622c
apm-lean 568d7d09d93ed0f9afc8402cf21ac160c3fbefba 2026-09-10T06:00:55Z topology-loop: record manifold-target-incidence-global-atlas-probe-refill41 transition invoke-1789019923837-18004-a189d8a1
apm-lean 8e38a91910d67f01c63f2083f2ebc0ab89518f76 2026-09-10T06:02:41Z topology-loop: record coefficient-general-boundary-reflection-comparison-1 transition invoke-1789020052717-18006-6c0280cc
apm-lean 601682e5f05198a7edf2d446e7f7db452fd74868 2026-09-10T06:02:55Z topology-loop: record manifold-target-incidence-global-atlas-probe-refill41 transition invoke-1789020063208-18008-99e2097f
apm-lean bce3268bf3f1d252046ecbb47eb561d8166bd19f 2026-09-10T06:04:52Z topology-loop: record manifold-target-incidence-global-atlas-probe-refill41 transition invoke-1789020181610-18012-99231bda
apm-lean a02047fb5a62fe711fdba08d5145449875bf97df 2026-09-10T06:06:29Z topology-loop: record oriented-checkpoint33-coefficient-disk-reflection-sign-1 transition invoke-1789020163768-18010-9cea91ad
apm-lean 04cd21730e91075fa7aeb619a116dfb201ec9468 2026-09-10T06:06:29Z topology-loop: record manifold-target-incidence-global-atlas-probe-refill41 transition invoke-1789020298315-18014-d3c2d4a7
apm-lean d9f48550bd527f4fffbca519fb106a4165c4419f 2026-09-10T06:08:00Z Prove coefficient-general actual disk-reflection sign
apm-lean 21b0a71dd151a4bbaf952e14a65a7215e990ce69 2026-09-10T06:09:20Z topology-loop: record oriented-checkpoint33-coefficient-disk-reflection-sign-1 transition invoke-1789020394642-18017-3ffe3f74
apm-lean 36ff15b8cf5aaddf23f18ccbd73baeb9bc011803 2026-09-10T06:11:07Z topology-loop: record oriented-checkpoint33-coefficient-disk-reflection-sign-1 transition invoke-1789020562597-18020-f848ebae
apm-lean 2470cc2f2a270a5ac22f86ef867ae27b480a78ca 2026-09-10T06:11:29Z topology-loop: record checkpoint-full-dag-refill-42-after-torus-euler-and-shared-geometry transition invoke-1789020395908-18018-695349fb
apm-lean 2fa37d4a562acdd82079b1af240798cebce8b94f 2026-09-10T06:13:26Z topology-loop: record checkpoint-full-dag-refill-42-after-torus-euler-and-shared-geometry transition invoke-1789020695281-18024-d3039467
apm-lean 339841ef5a95a541ab625392f45f2c52324080ce 2026-09-10T06:13:40Z topology-loop: record oriented-checkpoint34-disk-reflection-and-consumer-refill transition invoke-1789020670391-18022-d1851df0
apm-lean ae2e96fbf593bb70ced72e73bb8d225773fe97f0 2026-09-10T06:15:05Z topology-loop: record t02a08-frozen-integer-degree-closure-probe-refill42 transition invoke-1789020812525-18026-c172c807
apm-lean 16afc69a658fb6b3781b8ec8176d128d91802a8a 2026-09-10T06:15:11Z topology-loop: record oriented-checkpoint34-disk-reflection-and-consumer-refill transition invoke-1789020825898-18028-9af7849c
apm-lean ac18eb1680136568eb02ece634f584f7ba40e212 2026-09-10T06:16:23Z topology-loop: record t02a08-frozen-integer-degree-closure-probe-refill42 transition invoke-1789020910502-18030-2537de00
apm-lean ea585e455c070f830e4d8d07e80f435bf79b692c 2026-09-10T06:18:24Z topology-loop: record oriented-checkpoint34-coefficient-orthogonal-sphere-comparison-1 transition invoke-1789020915270-18032-0a2921ab
apm-lean fec38caa1602d0bbf85a8d568d8e7e82bdb42241 2026-09-10T06:19:02Z topology-loop: record compact-surface-embedded-triangle-cover-probe-refill42 transition invoke-1789020990731-18034-2aeeba92
apm-lean cbd921f6177307b5341ec6e9a66aa7830d4ec154 2026-09-10T06:19:49Z Prove orthogonal equivariance of the actual radial pair comparison
apm-lean 89d927803565b08836f691d67564a540d99f7ad8 2026-09-10T06:21:12Z topology-loop: record coefficient-general-orthogonal-radial-equivariance-1 transition invoke-1789021106807-18036-d5ae3cca
apm-lean d1aa58bb8923f308242ebd5c0c2c3fa42ff2d001 2026-09-10T06:22:43Z topology-loop: record finite-cover-singular-chain-transfer-probe-refill42 transition invoke-1789021149835-18038-6a64a519
apm-lean dab8a4fa31fd9954595b0fe39712f686a2cf8c64 2026-09-10T06:22:45Z topology-loop: record coefficient-general-orthogonal-radial-equivariance-1 transition invoke-1789021275220-18040-7b65d452
apm-lean f3ac9cfa3e61d644b38d8b0631e6c8940f77c0a8 2026-09-10T06:25:43Z topology-loop: record manifold-target-incidence-tangent-probe-refill42 transition invoke-1789021371707-18044-7991ac96
apm-lean 3830e6d3a77240a763f428b7d7d5412c19499a1f 2026-09-10T06:26:40Z Bank covering lift evaluation and simplex restriction bijections
apm-lean 3d80dab52d54974cb6178d5b0119d6e112f1e826 2026-09-10T06:27:24Z topology-loop: record covering-lift-restriction-implementation-refill42 transition invoke-1789021550519-18046-81c92bab
apm-lean dad28a49f1e0e8401c0714196b8703328e2a33ef 2026-09-10T06:29:04Z topology-loop: record covering-lift-restriction-implementation-refill42 transition invoke-1789021651883-18048-2ce7b12f
apm-lean 656514fef3f1593ae392ed20b9923475aab2fb35 2026-09-10T06:29:37Z topology-loop: record oriented-checkpoint34-coefficient-orthogonal-sphere-comparison-1 transition invoke-1789021369201-18043-440a87c7
apm-lean 788855cf349b8be015b18eee50e7715b52a66667 2026-09-10T06:31:18Z Prove height-preserving orthogonal sphere comparison equivariance
apm-lean f568fe2b083672bba94cd51de68a80ca489cf4e7 2026-09-10T06:32:03Z topology-loop: record finite-cover-singular-chain-transfer-probe-refill42 transition invoke-1789021750888-18050-ee104b06
apm-lean 339c0bb0e85ffe473f90c22db58c822dfd4dc3f5 2026-09-10T06:33:06Z topology-loop: record oriented-checkpoint34-coefficient-orthogonal-sphere-comparison-1 transition invoke-1789021780413-18052-d22cfd55
apm-lean 753cb1e75f7e271eefb3c49c27a5d6ee018d8ad1 2026-09-10T06:33:53Z Bank production covering simplex transfer and projection identity
apm-lean 691487aaf2503de737710fbcdfde136dc9835a9f 2026-09-10T06:34:41Z topology-loop: record covering-simplex-transfer-implementation-refill42 transition invoke-1789021929244-18054-4db1df62
apm-lean 205dbfa759f72baf2354106d20c31531f0fe1c55 2026-09-10T06:34:55Z topology-loop: record oriented-checkpoint34-coefficient-orthogonal-sphere-comparison-1 transition invoke-1789021988608-18056-f01bac7a
apm-lean 081db07251540dac86fd77d77087f58bdc9be9e0 2026-09-10T06:36:22Z topology-loop: record covering-simplex-transfer-implementation-refill42 transition invoke-1789022089256-18058-e7191d59
apm-lean 4839021d849dc662f97c46fecd6615cf2307978f 2026-09-10T06:38:07Z topology-loop: record oriented-checkpoint35-orthogonal-comparison-and-consumer-refill transition invoke-1789022101430-18060-8e599e5a
apm-lean d180204c127f974a182dece64572b934ce3005ba 2026-09-10T06:39:35Z topology-loop: record oriented-checkpoint35-orthogonal-comparison-and-consumer-refill transition invoke-1789022290272-18064-6dbb8ea0
apm-lean fbaee89494392bc241edfc46d22d4be568ab0e53 2026-09-10T06:39:41Z topology-loop: record finite-cover-singular-chain-transfer-probe-refill42 transition invoke-1789022189073-18062-536b57ca
apm-lean cdd6e4106062e7ee220add31b154afb8f55892dd 2026-09-10T06:41:03Z Bank production degreewise covering transfer
apm-lean d42253fc56d0c09ac4af49ae904bb3d99c91d72b 2026-09-10T06:41:44Z topology-loop: record covering-transfer-degree-implementation-refill42 transition invoke-1789022388638-18068-303415f9
apm-lean c885d502d8b4ed1af53330530174da1d10f0ed46 2026-09-10T06:43:00Z topology-loop: record covering-transfer-degree-implementation-refill42 transition invoke-1789022509666-18070-61151703
apm-lean 8c6050460d61a226deb8d2750ad82e1cedb8e510 2026-09-10T06:45:43Z topology-loop: record oriented-checkpoint35-coefficient-positive-sphere-antipodal-1 transition invoke-1789022378086-18066-7393aa78
apm-lean 62a0af3d4b4b7b2cbe034858bbe8a3d969b9ebce 2026-09-10T06:47:26Z Prove actual coordinate-reflection permutation conjugation
apm-lean d75b7c98d30572262fb5ab88437fea732f6570c5 2026-09-10T06:48:51Z topology-loop: record coefficient-general-coordinate-reflection-conjugation-1 transition invoke-1789022745977-18074-5226152b
apm-lean aa10a52f145bb6f0d3250febce2efbbc1845ef50 2026-09-10T06:49:17Z topology-loop: record finite-cover-singular-chain-transfer-probe-refill42 transition invoke-1789022586264-18072-8a30fcac
apm-lean c420b4ff59b0835b3cffcae813e6962dbb85eec4 2026-09-10T06:50:39Z topology-loop: record coefficient-general-coordinate-reflection-conjugation-1 transition invoke-1789022933978-18076-675a8caa
apm-lean 164692ffb310606a149de4a27a2e2cbf0a17a324 2026-09-10T06:53:29Z feat(topology): construct integral covering chain transfer
apm-lean f6e3c7407bfa8bb9dd6ed08883d338db3c8f3237 2026-09-10T06:54:19Z topology-loop: record finite-cover-singular-chain-transfer-probe-refill42 transition invoke-1789022964060-18078-e3bc54c9
apm-lean d288bac11318e69a6429604353be4cf5d84b27f6 2026-09-10T06:55:38Z topology-loop: record finite-cover-singular-chain-transfer-probe-refill42 transition invoke-1789023265680-18082-e0039ba6
apm-lean 21c853ad74421bac8e35d178bc635bbf2807b31d 2026-09-10T06:55:48Z topology-loop: record oriented-checkpoint35-coefficient-positive-sphere-antipodal-1 transition invoke-1789023041640-18080-297a6171
apm-lean c5d0be94674823aaf1b85f6c3152783d8430aef2 2026-09-10T06:57:02Z feat(topology): prove normalized incidence immersion
apm-lean 4e85600767edfe7f7de94ad2244abc5aadfe1a81 2026-09-10T06:57:33Z Prove positive top-homology coordinate-reflection signs
apm-lean edcd983b130febf4c5dd93d884076c4618e4d23f 2026-09-10T06:57:55Z topology-loop: record manifold-target-incidence-immersion-implementation-refill42 transition invoke-1789023343869-18084-7bb50bbb
apm-lean b3364b19c461114c71d60f7b589a292b63cad2b4 2026-09-10T06:59:01Z topology-loop: record coefficient-general-positive-top-reflection-induction-1 transition invoke-1789023355049-18086-163703db
apm-lean 9653998ecfbbfbc88c9402615cbf1a0c38c93292 2026-09-10T06:59:17Z topology-loop: record manifold-target-incidence-immersion-implementation-refill42 transition invoke-1789023481744-18088-5c48a3ed
apm-lean 6e3588b7f6d96c0b2b40663cf4bcde7e92775fb6 2026-09-10T07:03:35Z topology-loop: record coefficient-general-positive-top-reflection-induction-1 transition invoke-1789023544021-18090-94a5b7ee
apm-lean aa96fbaf7d0dea499159aeb411c23f13251aaec6 2026-09-10T07:05:58Z topology-loop: record manifold-target-incidence-tangent-probe-refill42 transition invoke-1789023565815-18092-8ffd3f92
apm-lean b831e4012f68281dfa003ca1b59d14a766a3cab1 2026-09-10T07:06:59Z feat(topology): prove normalized incidence tangent equalizer
apm-lean 31a512cd628e03b70e419224b99a86a788d5cc08 2026-09-10T07:07:37Z topology-loop: record manifold-target-incidence-tangent-probe-refill42 transition invoke-1789023965085-18096-a593e6db
apm-lean f668a7a575aa76869fdff934f612ce2c863312a4 2026-09-10T07:07:44Z topology-loop: record oriented-checkpoint35-coefficient-positive-sphere-antipodal-1 transition invoke-1789023817875-18094-d64cc231
apm-lean 43fe2faa8f2a6b1900a5f4105baa21f600c9852e 2026-09-10T07:09:14Z topology-loop: record manifold-target-incidence-tangent-probe-refill42 transition invoke-1789024063009-18098-5b886ff8
apm-lean 9e8352926c5896dd66aaf93eae771391865b0fac 2026-09-10T07:09:16Z Prove actual antipodal action on positive sphere top homology
apm-lean 45a52ad71f5b91454d457126a2320dac46d33800 2026-09-10T07:09:59Z feat(topology): construct standard triangle embedding
apm-lean fc1917b45bfdf58c5117e6679ea1e47e8875f743 2026-09-10T07:10:34Z topology-loop: record oriented-checkpoint35-coefficient-positive-sphere-antipodal-1 transition invoke-1789024067829-18100-a8e3cd0c
apm-lean d9aefc9bf45a1583d54159d4c9a7997a589adab3 2026-09-10T07:10:54Z topology-loop: record standard-triangle-embedding-implementation-refill42 transition invoke-1789024159823-18102-b1f64861
apm-lean d2695f9e84368165423aa9b5324c1d23cd93f4ab 2026-09-10T07:12:14Z topology-loop: record standard-triangle-embedding-implementation-refill42 transition invoke-1789024260479-18106-b01343a3
apm-lean d4bbe3097b2e9575b1d3effea46f6b2697abcb8b 2026-09-10T07:12:22Z topology-loop: record oriented-checkpoint35-coefficient-positive-sphere-antipodal-1 transition invoke-1789024237239-18104-a9e38c4c
apm-lean 2a0da4fe4be93aea4b6c2aeccb794e22024ce20d 2026-09-10T07:14:50Z topology-loop: record compact-surface-embedded-triangle-cover-probe-refill42 transition invoke-1789024340176-18108-4409f8ab
apm-lean e426e617dcf475faed81cdf99b9e74258cdd987c 2026-09-10T07:14:55Z topology-loop: record oriented-checkpoint36-antipodal-computation-and-consumer-refill transition invoke-1789024345359-18110-05f87632
apm-lean dfe31bd074dedf6fab48fa0e87badbc0d79fea42 2026-09-10T07:15:59Z feat(topology): fit scaled triangles inside open neighborhoods
apm-lean 35bc430bc6e53f1f5de77c168e1ceae74572069b 2026-09-10T07:16:31Z topology-loop: record scaled-triangle-neighborhood-implementation-refill42 transition invoke-1789024497923-18113-63c20ce9
apm-lean 284307a5cb7950753d3284d1bf7a1bf948dd659b 2026-09-10T07:16:44Z topology-loop: record oriented-checkpoint36-antipodal-computation-and-consumer-refill transition invoke-1789024499075-18114-7e39d772
apm-lean e8727a7adad1ed51145de5bf069fdfe67251ce12 2026-09-10T07:17:49Z topology-loop: record scaled-triangle-neighborhood-implementation-refill42 transition invoke-1789024599031-18116-20e605c0
apm-lean 4644d6f071fab0f645d75d606e219f530c421112 2026-09-10T07:18:56Z topology-loop: record oriented-checkpoint36-t94j08-positive-antipodal-closure-1 transition invoke-1789024609628-18118-62454f07
apm-lean 91c2b55f3850556d96770fe7cc99bb030573da1b 2026-09-10T07:20:08Z topology-loop: record compact-surface-embedded-triangle-cover-probe-refill42 transition invoke-1789024675886-18120-43542a45
apm-lean 69eb3d52d58a9189da8413b0f26bc09ddd90f035 2026-09-10T07:20:29Z Complete repaired positive-dimensional t94J08 antipodal proofs
apm-lean 3e20822e8672ac4e3dd9c2cf8fbcbd35bb782ed5 2026-09-10T07:21:23Z feat(topology): embed triangles through inverse charts
apm-lean a968d397008aea050297df3ff296e3e4ee0698ae 2026-09-10T07:21:27Z topology-loop: record oriented-checkpoint36-t94j08-positive-antipodal-closure-1 transition invoke-1789024739159-18122-874ffa7e
apm-lean c1e2f1c7801d9631f126866f57c3667a01840546 2026-09-10T07:22:07Z topology-loop: record chart-triangle-embedding-implementation-refill42 transition invoke-1789024834139-18124-1100e94f
apm-lean 800a5fa8d33b008770e21f7f74dd93b01f5ed966 2026-09-10T07:23:15Z topology-loop: record oriented-checkpoint36-t94j08-positive-antipodal-closure-1 transition invoke-1789024889748-18126-04ebb0b2
apm-lean 25f3a911cef82633bf6bf27f7592fc3f5ab7fe56 2026-09-10T07:23:27Z topology-loop: record chart-triangle-embedding-implementation-refill42 transition invoke-1789024933179-18128-efeca2f0
apm-lean 531a85839de8359da77312d1a6252138a0c38e03 2026-09-10T07:26:04Z topology-loop: record oriented-checkpoint37-t94j08-uptake-and-consumer-refill transition invoke-1789024997589-18130-e6721e59
apm-lean 291322242dd5df7fe0f87658aaa8d3e7bab5b9ec 2026-09-10T07:26:46Z topology-loop: record compact-surface-embedded-triangle-cover-probe-refill42 transition invoke-1789025013544-18132-963d3594
apm-lean dce382db8ea5b6c8dee5c8b18f3b0fc6338108f7 2026-09-10T07:27:36Z feat(topology): construct finite embedded triangle interior cover
apm-lean 12a4e339ea5a7adf2cff82ae81755246c710cc93 2026-09-10T07:27:52Z topology-loop: record oriented-checkpoint37-t94j08-uptake-and-consumer-refill transition invoke-1789025166877-18134-577bd929
apm-lean bfec0ce5b4fb9835790d711501b4aece049430f0 2026-09-10T07:28:24Z topology-loop: record compact-surface-embedded-triangle-cover-probe-refill42 transition invoke-1789025212144-18136-b1f46e94
apm-lean 0e16695e8869af92afdf382365c1f2c5e05fbf52 2026-09-10T07:29:41Z topology-loop: record compact-surface-embedded-triangle-cover-probe-refill42 transition invoke-1789025310202-18140-681b0c9e
apm-lean 6ac2e9106f544137a60be46512ad9ecc1959ac07 2026-09-10T07:31:20Z topology-loop: record oriented-checkpoint37-coefficient-model-local-negation-1 transition invoke-1789025275120-18138-45681546
apm-lean d5fc86bde4bd87f7ee934d9001fa754d9124f456 2026-09-10T07:32:46Z Construct coefficient-general Euclidean local homology and negation
apm-lean c6e0f15dcf84dfd89d048f283b8095b1ec46c1e4 2026-09-10T07:34:01Z topology-loop: record checkpoint-full-dag-refill-43-after-degree-triangles-and-tangent-transfer transition invoke-1789025387419-18142-b864e051
apm-lean 9ab3389c3b6a6fd419166acfa563181a9f77d713 2026-09-10T07:34:12Z topology-loop: record oriented-checkpoint37-coefficient-model-local-negation-1 transition invoke-1789025483140-18144-e3f00c1f
apm-lean 07b0e701ef49cd00b1b4561ccbc16e16f3f49b49 2026-09-10T07:36:04Z topology-loop: record checkpoint-full-dag-refill-43-after-degree-triangles-and-tangent-transfer transition invoke-1789025648871-18146-cf15549c
apm-lean d2f91ffa69c1d30eafbf60936169dece08665719 2026-09-10T07:36:05Z topology-loop: record oriented-checkpoint37-coefficient-model-local-negation-1 transition invoke-1789025660083-18148-acdc312f
apm-lean eaeeb5b7ddadaabd38a3b0c47bfa288da8e13fb5 2026-09-10T07:38:23Z topology-loop: record t01a08-frozen-regular-line-closure-probe-refill43 transition invoke-1789025770464-18152-0ef0f6fe
apm-lean ba3c7d4c9ea3fbb3695a8ae9f75400f95154f646 2026-09-10T07:38:35Z topology-loop: record oriented-checkpoint37-t94j08-reviewed-closure-documentation-1 transition invoke-1789025769316-18151-f99f7c1e
apm-lean fd8b2bc5f43286fa5c50547ddd50fa46f8f0302c 2026-09-10T07:39:40Z topology-loop: record t01a08-frozen-regular-line-closure-probe-refill43 transition invoke-1789025909241-18154-312349be
apm-lean e8822f1a9d97d76dec8967feed88b9215d71b679 2026-09-10T07:40:00Z Record reviewed repaired positive-dimensional t94J08 closure
apm-lean bbf579f5a94ed860736bad7a7e07e90af8275b9a 2026-09-10T07:41:08Z topology-loop: record oriented-checkpoint37-t94j08-reviewed-closure-documentation-1 transition invoke-1789025921550-18156-2467455e
apm-lean d618605d2b2d96e20c65a2e350bad0adddc9a2b7 2026-09-10T07:42:56Z topology-loop: record oriented-checkpoint37-t94j08-reviewed-closure-documentation-1 transition invoke-1789026070823-18160-2538c1e9
apm-lean f33a866e0c217400ff61a2a4a6fdfa7fb8f9acf2 2026-09-10T07:43:35Z topology-loop: record finite-cover-rational-transfer-probe-refill43 transition invoke-1789026002590-18158-7518b4de
apm-lean 8ab450344af08ec2c0348ab15ba0cd70ba5630c7 2026-09-10T07:45:06Z topology-loop: record oriented-checkpoint38-local-model-and-consumer-refill transition invoke-1789026179075-18162-642cf622
apm-lean b2475bda83db8dedd5d54547b6525762a935c27f 2026-09-10T07:46:35Z topology-loop: record oriented-checkpoint38-local-model-and-consumer-refill transition invoke-1789026309285-18166-d768d8db
apm-lean fd7069ecbd7d4e1db1c886562c5bfab73a5d2e72 2026-09-10T07:46:53Z topology-loop: record incidence-parameter-projection-regularity-probe-refill43 transition invoke-1789026220891-18164-92315431
apm-lean 2d1b389ca555a02b8eec9195348e6d488b5a7edc 2026-09-10T07:48:31Z topology-loop: record surface-triangle-compatible-refinement-probe-refill43 transition invoke-1789026419722-18170-06941ec7
apm-lean 0d32c55620daf49e5a4ed7cc79edf2ef9b1f2ec9 2026-09-10T07:49:49Z topology-loop: record surface-triangle-compatible-refinement-probe-refill43 transition invoke-1789026517814-18172-415ebc8e
apm-lean 06514df2c03645154bf5405e08fee4daf05d839c 2026-09-10T07:50:09Z topology-loop: record oriented-checkpoint38-coefficient-neighborhood-local-excision-1 transition invoke-1789026399936-18168-1d7d6262
apm-lean 4b689d83309be91957b2ccc93d385e5cb9e0afd7 2026-09-10T07:50:38Z feat(topology): construct coefficient covering transfer and homology identity
apm-lean f8a1ac6115ec78e8b424a4f3150a59f7b75c39a8 2026-09-10T07:51:29Z topology-loop: record finite-cover-rational-transfer-probe-refill43 transition invoke-1789026594930-18174-8d03d541
apm-lean a80d438beb226fc6049065706e4f51444e0a2ad4 2026-09-10T07:51:35Z Construct coefficient-general neighborhood local excision
apm-lean 21d6beb967f98d8b57a8dfe00666284c4d0b965f 2026-09-10T07:52:58Z topology-loop: record oriented-checkpoint38-coefficient-neighborhood-local-excision-1 transition invoke-1789026611533-18176-55be681e
apm-lean 155f8965399aaa2819c5b6c47c6dbb632d21a15e 2026-09-10T07:53:12Z topology-loop: record finite-cover-rational-transfer-probe-refill43 transition invoke-1789026699844-18178-10575c26
apm-lean cc3261525c486f5926995c6e582362658e70c44b 2026-09-10T07:54:17Z feat(topology): prove incidence parameter projection regularity
apm-lean dc050f3f61c7bf51a8b5d2bbbc6f79a80d45996e 2026-09-10T07:54:48Z topology-loop: record oriented-checkpoint38-coefficient-neighborhood-local-excision-1 transition invoke-1789026780758-18180-1ef8c3d8
apm-lean 670245a25ac6e76404c3325c0611d0552a07dc64 2026-09-10T07:55:09Z topology-loop: record incidence-parameter-projection-regularity-probe-refill43 transition invoke-1789026797864-18182-1425c2e3
apm-lean 1336224bcc770c7814961101c8629f1b868d2a83 2026-09-10T07:56:29Z topology-loop: record incidence-parameter-projection-regularity-probe-refill43 transition invoke-1789026916018-18186-b6e2e15e
apm-lean e6ed179b7c577aca3539f5fade882d13e6ea43d5 2026-09-10T07:57:03Z topology-loop: record oriented-checkpoint39-neighborhood-excision-and-consumer-refill transition invoke-1789026895417-18184-de46a0b7
apm-lean 3384a549d433715b9d8236c011c52babd63d1eb0 2026-09-10T07:58:32Z topology-loop: record oriented-checkpoint39-neighborhood-excision-and-consumer-refill transition invoke-1789027026156-18190-bf1e7eca
apm-lean 9e5e1da032b46d1b5d269999b3bdc07fc15e64cc 2026-09-10T08:00:08Z topology-loop: record checkpoint-full-dag-refill-44-after-regular-line-and-face-projection-transfer transition invoke-1789026995375-18188-08529970
apm-lean e45005bfc3e0d96100670217aa25fe6b64b9d45e 2026-09-10T08:02:08Z topology-loop: record checkpoint-full-dag-refill-44-after-regular-line-and-face-projection-transfer transition invoke-1789027214613-18194-71997d3e
apm-lean 32f89efe75be9c1640402de28ef6aaf048ee20f4 2026-09-10T08:04:44Z topology-loop: record oriented-checkpoint39-coefficient-actual-chart-local-homology-1 transition invoke-1789027115650-18192-8d925aad
apm-lean 6f7d4072f5919e08cd1ca6d68864f3f0d84cd7a5 2026-09-10T08:06:26Z Construct actual coefficient-general chart deleted-point comparison
apm-lean bbf1f449f943dd5de9cc535d6fef1ec444d7a3f6 2026-09-10T08:07:58Z topology-loop: record coefficient-general-actual-chart-pair-comparison-1 transition invoke-1789027486714-18198-a0df82aa
apm-lean a0834961b429c7ec3ff0f4b1a150004ff38e60ee 2026-09-10T08:08:06Z topology-loop: record t02a05-frozen-flux-closure-probe-refill44 transition invoke-1789027334424-18196-ba5753b4
apm-lean 2f5eaeefd5c0222f0654eb911a88eb70018de674 2026-09-10T08:09:47Z topology-loop: record coefficient-general-actual-chart-pair-comparison-1 transition invoke-1789027680609-18200-cd25c3a8
apm-lean df2916aa6cf26b9e9f711b319888d7506129a9b6 2026-09-10T08:13:07Z topology-loop: record t02a05-frozen-flux-closure-probe-refill44 transition invoke-1789027695025-18202-25b05d09
apm-lean 54d7155798142621dbd2356fc1c53d89905c2d64 2026-09-10T08:13:55Z topology-loop: record oriented-checkpoint39-coefficient-actual-chart-local-homology-1 transition invoke-1789027789939-18204-296e4bf2
apm-lean c17b1ff4cd7551b2bf8af599968b4443be23c48c 2026-09-10T08:15:22Z Construct actual translated-chart local homology normalization
apm-lean 18351ba22b3b402034a72fd2f5adfff70ca78256 2026-09-10T08:17:05Z topology-loop: record oriented-checkpoint39-coefficient-actual-chart-local-homology-1 transition invoke-1789028038709-18208-f500fd91
apm-lean ac2bcd7a536c83e1a6230a7ba8b9f9f0b1d823d6 2026-09-10T08:18:27Z topology-loop: record coefficient-manifold-local-chart-comparison-probe-refill44 transition invoke-1789027994230-18206-3205e290
apm-lean 44c9294744789f8f939a0574a99a3065ecce01d4 2026-09-10T08:18:34Z topology-loop: record oriented-checkpoint39-coefficient-actual-chart-local-homology-1 transition invoke-1789028227782-18210-db8df80a
apm-lean e002d6ed1469f41a889adc808e3896c42d987a1e 2026-09-10T08:19:51Z feat(topology): compare chart local homology in all degrees
apm-lean 7f98795f5398540d6942c47d8a1b9217d108a593 2026-09-10T08:20:47Z topology-loop: record coefficient-chart-local-all-degrees-implementation-refill44 transition invoke-1789028315483-18213-1b827aa7
apm-lean 1c41db2d4f9810bf84dc7a5ce318d8792a57ef52 2026-09-10T08:21:26Z topology-loop: record oriented-checkpoint40-chart-local-homology-and-consumer-refill transition invoke-1789028317910-18214-f69f6a77
apm-lean c36685480e97d18ef44e27cfe65eae56f02f0644 2026-09-10T08:22:45Z topology-loop: record coefficient-chart-local-all-degrees-implementation-refill44 transition invoke-1789028453363-18216-6af29383
apm-lean 7240bb3a0fec4a5a985b68a71843913d14f9d397 2026-09-10T08:22:55Z topology-loop: record oriented-checkpoint40-chart-local-homology-and-consumer-refill transition invoke-1789028489326-18218-c5b6c45e
apm-lean 238a0c42a5fdbcc0480493c674b30910d7ecb641 2026-09-10T08:27:05Z topology-loop: record oriented-checkpoint40-coefficient-actual-overlap-transition-1 transition invoke-1789028578398-18222-c6d5f431
apm-lean 366b92d00e291fa82809b46ceae4a17443f25803 2026-09-10T08:27:08Z topology-loop: record coefficient-manifold-local-chart-comparison-probe-refill44 transition invoke-1789028573146-18220-a5a5516f
apm-lean 9f48c402684d7499653a7c0ae8cfe96db3f955c9 2026-09-10T08:28:35Z feat(topology): transport coefficient local homology from finite models
apm-lean 39733ad8c6a57a18dc81ed19910a70af8bc00bdc 2026-09-10T08:29:30Z topology-loop: record coefficient-finite-model-chart-implementation-refill44 transition invoke-1789028837512-18226-e68312d1
apm-lean c9ca1f19065f0c079760f8c6f9227c5d6c248945 2026-09-10T08:30:14Z Construct actual coefficient-general translated overlap pairs
apm-lean 7bca354a96a469399fc311c333e1b464755befc6 2026-09-10T08:30:49Z topology-loop: record coefficient-finite-model-chart-implementation-refill44 transition invoke-1789028975907-18228-04205ae6
apm-lean 074bec5cba390bd24dbfcb2aefcc0b544caf5757 2026-09-10T08:31:56Z topology-loop: record coefficient-general-geometric-overlap-pair-1 transition invoke-1789028828173-18224-8f532013
apm-lean 9f728964c85fafb628b660f4e5beab3aea2deb7d 2026-09-10T08:33:45Z topology-loop: record coefficient-general-geometric-overlap-pair-1 transition invoke-1789029118581-18232-2b2bb146
apm-lean f0fc2ca73dc9bca443139e687e64e011dcde4819 2026-09-10T08:34:10Z topology-loop: record coefficient-manifold-local-chart-comparison-probe-refill44 transition invoke-1789029054468-18230-a32c2422
apm-lean 8cd96d529989af0cabe673ddcb8737a2680a674e 2026-09-10T08:35:28Z feat(topology): compare manifold local homology in every dimension
apm-lean dfa1eba6049950ec8bcf75ac1d4933f2b3347e0c 2026-09-10T08:36:27Z topology-loop: record coefficient-manifold-local-chart-comparison-probe-refill44 transition invoke-1789029256222-18236-233e1093
apm-lean 3793bd27bc783df74b7d6761db319fbb9d96f8c8 2026-09-10T08:37:36Z topology-loop: record oriented-checkpoint40-coefficient-actual-overlap-transition-1 transition invoke-1789029230895-18234-00d926dd
apm-lean e0f98c46010d9fc9c9a2c1c1c38e46676949a5e0 2026-09-10T08:38:05Z topology-loop: record coefficient-manifold-local-chart-comparison-probe-refill44 transition invoke-1789029393439-18238-513fe059
apm-lean 94b0259c67f7223c4938d3021da18cbaa3175062 2026-09-10T08:38:53Z Construct actual relative overlap inclusion square
apm-lean 55f5066b88b52f50a507d2dc0efd8ec6153240b5 2026-09-10T08:39:24Z topology-loop: record surface-embedded-arcs-finite-position-probe-refill44 transition invoke-1789029492339-18242-f1ed9792
apm-lean 71b386f1bfcaf1d29e11a8097ab9ca05236fa826 2026-09-10T08:40:27Z topology-loop: record coefficient-general-relative-overlap-inclusion-square-1 transition invoke-1789029459440-18240-3e130aa3
apm-lean 2a77a5927fc96a99ad89c860db985cf812302391 2026-09-10T08:40:42Z topology-loop: record surface-embedded-arcs-finite-position-probe-refill44 transition invoke-1789029570510-18244-ef604876
apm-lean 0b23c2e7b95caf0cf58ba944c9451365561e2061 2026-09-10T08:41:56Z topology-loop: record coefficient-general-relative-overlap-inclusion-square-1 transition invoke-1789029630136-18246-9db174a4
apm-lean 6af5755f6f509a1dfa7d41baa2559c2d1c066235 2026-09-10T08:44:21Z topology-loop: record checkpoint-full-dag-refill-45-after-flux-local-charts-and-arcs transition invoke-1789029649827-18248-3673a5e4
apm-lean 4f2aaf804272e8c10c3ecc5e5088c7aeda9aa69c 2026-09-10T08:46:01Z topology-loop: record checkpoint-full-dag-refill-45-after-flux-local-charts-and-arcs transition invoke-1789029867866-18252-c0ccab94
apm-lean 181d01a12d572ea775b71f94e8c9d4c067f880d5 2026-09-10T08:46:48Z topology-loop: record oriented-checkpoint40-coefficient-actual-overlap-transition-1 transition invoke-1789029719123-18250-99048f41
apm-lean cddd39bf73c9f70276385fe4c8a40ac85296f708 2026-09-10T08:48:04Z topology-loop: record t00a01-frozen-open-union-pi1-closure-probe-refill45 transition invoke-1789029968529-18254-bae458e7
apm-lean 237dcad73487fe524354edb74123fe837d5cb962 2026-09-10T08:48:18Z Construct common-neighborhood overlap normalization comparison
apm-lean 25efa075adba20c7105c799f4fb0c0f1b70ed2d2 2026-09-10T08:49:33Z topology-loop: record t00a01-frozen-open-union-pi1-closure-probe-refill45 transition invoke-1789030101387-18258-81c58962
apm-lean ea516415b5f67b8194ed7c956365a8151626b353 2026-09-10T08:49:41Z topology-loop: record coefficient-general-geometric-overlap-normalization-1 transition invoke-1789030011072-18256-85386d26
apm-lean 316f111fb8d3fddaf316d603ec23ae06150a99e3 2026-09-10T08:51:33Z topology-loop: record coefficient-general-geometric-overlap-normalization-1 transition invoke-1789030186673-18262-a8e674f5
apm-lean 875b86bd0dbfe9fbf46ffbc0d90351712779f2d2 2026-09-10T08:53:00Z topology-loop: record coefficient-overlap-ambient-normalization-probe-refill45 transition invoke-1789030185339-18261-2c899cfa
apm-lean 385d575847d78cce76a75d8f18732265e2b1ca9a 2026-09-10T08:54:06Z feat(topology): identify distinguished translated chart pair
apm-lean cf415de5d06bf577f450ab96538d12b8a861ec77 2026-09-10T08:54:59Z topology-loop: record coefficient-distinguished-chart-implementation-refill45 transition invoke-1789030386790-18266-97f332e5
apm-lean 9b20518681b42ac1e246ad732070e8a7d7782871 2026-09-10T08:56:19Z topology-loop: record coefficient-distinguished-chart-implementation-refill45 transition invoke-1789030506591-18268-d5246d4f
apm-lean a05c8e04a56c537a96b75d3bf05bf2aab2033b09 2026-09-10T08:57:02Z topology-loop: record oriented-checkpoint40-coefficient-actual-overlap-transition-1 transition invoke-1789030296131-18264-fbe341a1
apm-lean 8a65c6799993ac37ded3159abd82d26a2f6c7ac0 2026-09-10T08:58:19Z Prove chart normalization independence under neighborhood restriction
apm-lean e82392b6e07100f2c2be7e47bbfbbbc7c0462fb9 2026-09-10T08:59:31Z topology-loop: record coefficient-general-chart-normalization-restriction-1 transition invoke-1789030625513-18272-5a8eed5d
apm-lean 39816b7efe75840a89e22eca4a869910e27af12a 2026-09-10T09:02:38Z topology-loop: record coefficient-overlap-ambient-normalization-probe-refill45 transition invoke-1789030586376-18270-3557478e
apm-lean 52e867b4c5fcac554b40e23580b96972d85be785 2026-09-10T09:03:36Z feat(topology): prove distinguished target normalization square
apm-lean 21f95044bb319c080552d245e5aade0dcf0c8325 2026-09-10T09:04:22Z topology-loop: record coefficient-general-chart-normalization-restriction-1 transition invoke-1789030774279-18274-ea77eaab
apm-lean b7dad427ecfbdc4d689c0415924f981434f755bf 2026-09-10T09:04:36Z topology-loop: record coefficient-distinguished-target-normalization-implementation-refill45 transition invoke-1789030963997-18276-30fad797
apm-lean 467c58a27c574dec9a62eadbd987bbf46f7aa08f 2026-09-10T09:05:55Z topology-loop: record coefficient-distinguished-target-normalization-implementation-refill45 transition invoke-1789031083788-18280-8eff93c6
apm-lean 583fef5493dfc1691abe170ed13d8572792da117 2026-09-10T09:10:12Z topology-loop: record coefficient-overlap-ambient-normalization-probe-refill45 transition invoke-1789031161533-18282-415ca74e
apm-lean 69e116fe908d461394ed868f7da4ec0f2f4bf17b 2026-09-10T09:11:12Z topology-loop: record oriented-checkpoint40-coefficient-actual-overlap-transition-1 transition invoke-1789031065492-18278-a8ebd4d1
apm-lean 3510365eedac863a7e69e71cefc9135cfd808f98 2026-09-10T09:11:26Z feat(topology): identify full distinguished chart normalization
apm-lean 391c2b9e70ee54d30de8480851745f8bac5167e7 2026-09-10T09:12:10Z topology-loop: record coefficient-overlap-ambient-normalization-probe-refill45 transition invoke-1789031418456-18284-a061206b
apm-lean dd9752ee9e4151dfe7d5b3e1211b700eec73471a 2026-09-10T09:12:38Z Construct geometric chart transition units and comparison cocycles
apm-lean 9bb9d2de8f5e871c43bd0d5f1a3b71814dd9d25d 2026-09-10T09:13:49Z topology-loop: record coefficient-overlap-ambient-normalization-probe-refill45 transition invoke-1789031537156-18288-dfb60dd1
apm-lean daed7aa513436e301f892abcddb1eaa72f63c913 2026-09-10T09:14:03Z topology-loop: record coefficient-general-geometric-chart-transition-unit-1 transition invoke-1789031475558-18286-6c10ad42
apm-lean 6f173d44726bf9bdfc36ea193a26b2997973255d 2026-09-10T09:15:56Z topology-loop: record coefficient-general-geometric-chart-transition-unit-1 transition invoke-1789031650029-18292-5727644e
apm-lean 96a9f272ad2b078df8121bbef806cd77a85ca2ca 2026-09-10T09:18:09Z topology-loop: record surface-nested-triangle-cover-margin-probe-refill45 transition invoke-1789031636848-18290-df008a30
apm-lean cc19e0bb884a344db82c69ad4c434849a3e6f595 2026-09-10T09:19:01Z feat(topology): construct finite nested triangle cover
apm-lean 26eda706d5858e245e36cf62a4241a05e46ef5a4 2026-09-10T09:19:46Z topology-loop: record surface-nested-triangle-cover-margin-probe-refill45 transition invoke-1789031895236-18296-3a4ea028
apm-lean f699206d8890866401b060cfb71c9049d5ca6c43 2026-09-10T09:21:04Z topology-loop: record surface-nested-triangle-cover-margin-probe-refill45 transition invoke-1789031992412-18298-2bd0a985
apm-lean 4ae4edde581cb485ddb59a11cdd22abb7cddb7bb 2026-09-10T09:23:25Z topology-loop: record oriented-checkpoint40-coefficient-actual-overlap-transition-1 transition invoke-1789031759203-18294-5256d729
apm-lean 3efa5cad095e40711f8843f92e8b8345a8316d1f 2026-09-10T09:25:37Z Assemble actual overlap transitions and geometric unit cocycles
apm-lean edc518c0cfea660e35a4ee97e2d55f3fe7b7d598 2026-09-10T09:25:42Z topology-loop: record checkpoint-full-dag-refill-46-after-open-union-overlap-and-nested-triangles transition invoke-1789032071228-18300-912cce18
apm-lean 4dab685a45f912da697ec141e0e47fb4d9621965 2026-09-10T09:27:16Z topology-loop: record oriented-checkpoint40-coefficient-actual-overlap-transition-1 transition invoke-1789032208289-18302-dd2b4dda
apm-lean 54126df56ada22e351f5ded78f4f9e6c3ab0a4ff 2026-09-10T09:27:42Z topology-loop: record checkpoint-full-dag-refill-46-after-open-union-overlap-and-nested-triangles transition invoke-1789032349040-18304-51dbfe99
apm-lean a7a09d02a89232fc08617edf84157aa264f23fe9 2026-09-10T09:29:08Z topology-loop: record oriented-checkpoint40-coefficient-actual-overlap-transition-1 transition invoke-1789032441730-18306-d0918155
apm-lean ffd7d753b195ef10d85751112d9618e6d4c6ef53 2026-09-10T09:30:21Z topology-loop: record t03j04-frozen-rational-euler-closure-probe-refill46 transition invoke-1789032469119-18308-b1ca9cc6
apm-lean 54ab7a2a4aea73fae0a48ead9849c07e3d64b741 2026-09-10T09:31:39Z topology-loop: record t03j04-frozen-rational-euler-closure-probe-refill46 transition invoke-1789032627834-18312-60e8ee00
apm-lean 29a9ee29c2ba4d0edc2af3031a8176ead2314f66 2026-09-10T09:34:17Z topology-loop: record oriented-checkpoint41-overlap-transition-and-consumer-refill transition invoke-1789032551394-18310-7e33f26b
apm-lean 061890d7862107784d8c51c1bcf4670e392c7b8a 2026-09-10T09:35:19Z topology-loop: record coefficient-triple-overlap-geometric-composition-probe-refill46 transition invoke-1789032706325-18314-99a3bc22
apm-lean 3ebc79da7a71218dd83c950f9abaa151be5fca34 2026-09-10T09:36:07Z topology-loop: record oriented-checkpoint41-overlap-transition-and-consumer-refill transition invoke-1789032860337-18316-45d02838
apm-lean 19107829bd4944393ecd0db08bb2e6e8c69fa1af 2026-09-10T09:38:57Z topology-loop: record generic-two-open-vertex-coproduct-comparison-probe-refill46 transition invoke-1789032925736-18318-09f057a4
apm-lean 8719b07fd80c8d4d054d3562162ded9c0d67f6bb 2026-09-10T09:39:36Z topology-loop: record oriented-checkpoint41-coefficient-transition-local-constancy-1 transition invoke-1789032970084-18320-4b9fb663
apm-lean a4977876b1ebb365bc8d06208da4fc7f18ca2228 2026-09-10T09:40:07Z feat(topology): prove actual triple overlap composition
apm-lean 871330d204d7123544de83b01a08f03ee4799a74 2026-09-10T09:40:54Z Construct moving-center sphere homotopy and coefficient homology equations
apm-lean 726f3e0dbe1d80cdbd6f22a9d946b0d05a3f4906 2026-09-10T09:40:58Z topology-loop: record coefficient-triple-overlap-geometric-composition-probe-refill46 transition invoke-1789033144324-18322-f81fb7a0
apm-lean e27c15a0b87f7a3d8d72e0e329d4b7eac19be645 2026-09-10T09:42:05Z topology-loop: record coefficient-general-moving-sphere-center-homotopy-1 transition invoke-1789033179317-18324-0ac88154
apm-lean a63b84ce7e252138ab2e7813d596dd0e6c9e95ba 2026-09-10T09:42:38Z topology-loop: record coefficient-triple-overlap-geometric-composition-probe-refill46 transition invoke-1789033264623-18326-a78bddea
apm-lean bf1d6bb906793851edd2327c910b3b582b814153 2026-09-10T09:43:30Z feat(topology): compare actual overlap span with point span
apm-lean 3496d2d2153d004a5c66e85610ade6f4c1d43d6f 2026-09-10T09:43:56Z topology-loop: record coefficient-general-moving-sphere-center-homotopy-1 transition invoke-1789033328078-18328-02bf5955
apm-lean ff791e7708776f77a82691ec5e43b4b08afc86f9 2026-09-10T09:44:35Z topology-loop: record two-open-point-span-comparison-implementation-refill46 transition invoke-1789033363910-18330-ab2d2ced
apm-lean 27f3f6d9c22fdfa4a28bef63f0a3fe27c1690dfd 2026-09-10T09:45:52Z topology-loop: record two-open-point-span-comparison-implementation-refill46 transition invoke-1789033481148-18334-b45210f8
apm-lean 1a5ce753699587149186209f03319ec45aabdcc8 2026-09-10T09:47:26Z topology-loop: record oriented-checkpoint41-coefficient-transition-local-constancy-1 transition invoke-1789033439283-18332-b1910d61
apm-lean 234a5bbd4d1ad4b9e98cbbcd82db20adfcaf1d61 2026-09-10T09:48:49Z Construct compact boundary center-motion homotopy
apm-lean 158e83386d21c3e94ff52b948ad0477bd124617d 2026-09-10T09:49:54Z topology-loop: record coefficient-general-compact-boundary-center-motion-1 transition invoke-1789033648629-18338-d66ef79c
apm-lean 56fd9d34970098f7c9828019b029962f73868b88 2026-09-10T09:50:31Z topology-loop: record generic-two-open-vertex-coproduct-comparison-probe-refill46 transition invoke-1789033559110-18336-dcb07956
apm-lean 36b53110c524bd8c2d18eff2c99e6b1f7fc4f070 2026-09-10T09:51:24Z topology-loop: record coefficient-general-compact-boundary-center-motion-1 transition invoke-1789033797555-18340-c0861566
apm-lean 5ff0a7d76d3546f2c62d9d7f1140eab4eee4066e 2026-09-10T09:55:27Z feat(topology): promote generic two-open free-product mapping
apm-lean c32573ea2b2dbcb57139d0b6d020e95f27c4a6ec 2026-09-10T09:55:33Z topology-loop: record oriented-checkpoint41-coefficient-transition-local-constancy-1 transition invoke-1789033887278-18344-ab6772ca
apm-lean 33c9b5b14670f25d77ed288bea24badfcf55f3e9 2026-09-10T09:56:31Z topology-loop: record generic-two-open-vertex-coproduct-comparison-probe-refill46 transition invoke-1789033837703-18342-1fad846c
apm-lean 5c23afeb2297af242b868b7495dc3c16275f9149 2026-09-10T09:56:59Z Construct actual chart boundary and nearby-point homology neighborhood
apm-lean c0d4ec1515376bd81e2a25e35ab34e553ebcef5b 2026-09-10T09:58:03Z topology-loop: record coefficient-general-actual-chart-boundary-neighborhood-1 transition invoke-1789034136286-18346-a234782f
apm-lean c88dbfb6a2c9b1e1065a3724a43f3515a034193b 2026-09-10T09:58:30Z topology-loop: record generic-two-open-vertex-coproduct-comparison-probe-refill46 transition invoke-1789034198685-18348-c20aeb07
apm-lean e88747d0815353ea15a24fa16e842d25a504db90 2026-09-10T09:59:35Z topology-loop: record coefficient-general-actual-chart-boundary-neighborhood-1 transition invoke-1789034286282-18350-dd92067a
apm-lean 27357b4b1b554ed92455a5f58802f2a929314092 2026-09-10T10:03:25Z topology-loop: record oriented-checkpoint41-coefficient-transition-local-constancy-1 transition invoke-1789034378893-18354-53b32061
apm-lean e10459ce658ef4ef9658cccf1f6ec1cc3e8d3600 2026-09-10T10:04:47Z Construct scaled boundary normalization and actual radial inverse
apm-lean 7504cb5f79a4cf44979471fdfa73269579ab27bf 2026-09-10T10:04:49Z topology-loop: record compact-transverse-arc-intersection-finiteness-probe-refill46 transition invoke-1789034316733-18352-2bc01a2d
apm-lean 0f8248aa09e2cf9df2246f22b963ce10a101271b 2026-09-10T10:05:36Z feat(topology): promote compact transverse arc finiteness
apm-lean 1a402985b56988b54fe96a4cb49f1b7390ffac4f 2026-09-10T10:05:56Z topology-loop: record coefficient-general-scaled-boundary-radial-normalization-1 transition invoke-1789034609142-18356-01a1fc08
apm-lean ada06b72c1bb8d6379fa83bcc01bea7f1543467d 2026-09-10T10:06:29Z topology-loop: record compact-transverse-arc-intersection-finiteness-probe-refill46 transition invoke-1789034696310-18358-42477154
apm-lean e56d5c54220ff652d54861680826dc192289987d 2026-09-10T10:07:29Z topology-loop: record coefficient-general-scaled-boundary-radial-normalization-1 transition invoke-1789034758925-18360-a2ec9551
apm-lean a23c64509baaf333a173f818ba8efa99c9759f57 2026-09-10T10:07:50Z topology-loop: record compact-transverse-arc-intersection-finiteness-probe-refill46 transition invoke-1789034796350-18362-17f90b98
apm-lean 1709a78951cc1cd4730799eb4c4ad8ec5c394979 2026-09-10T10:11:27Z topology-loop: record checkpoint-full-dag-refill-47-after-euler-triple-arcs-and-vertex transition invoke-1789034876103-18366-195d06c9
apm-lean 31e2a7930a6bf34abcff36f9264828646166223c 2026-09-10T10:13:07Z topology-loop: record checkpoint-full-dag-refill-47-after-euler-triple-arcs-and-vertex transition invoke-1789035094305-18368-5476c076
apm-lean 2fd228c0aff9a5ccc226dc7bec92c62265751501 2026-09-10T10:15:20Z topology-loop: record oriented-checkpoint41-coefficient-transition-local-constancy-1 transition invoke-1789034852116-18364-e895b7ff
apm-lean 3029bdd1f6bc5f153e858b2804c3b69a63747092 2026-09-10T10:16:29Z topology-loop: record t00a01-frozen-cover-coproduct-closure-probe-refill47 transition invoke-1789035193537-18370-2e7feacb
apm-lean 76299a8f91a9bc6a992a92e28c0678f40a884411 2026-09-10T10:16:37Z Construct fixed ball-boundary relative lift and recentering normalization
apm-lean 7832cec67b8da7299f0a2a99a6e79e7bf08aba10 2026-09-10T10:17:49Z topology-loop: record coefficient-general-ball-boundary-relative-lift-1 transition invoke-1789035323051-18372-c448a910
apm-lean 3612428cad65c85e6006c7488ea8eec4620a0181 2026-09-10T10:17:51Z feat(t00A01): close frozen connected-sum cover theorem
apm-lean b354142f8ee7f4c279d00b6c4d379a61f0b9834d 2026-09-10T10:18:33Z topology-loop: record t00a01-frozen-cover-coproduct-closure-probe-refill47 transition invoke-1789035400894-18374-2cfe8803
apm-lean 4a01faef221c8076d45bdeaf0d7644d5df9b3180 2026-09-10T10:19:20Z topology-loop: record coefficient-general-ball-boundary-relative-lift-1 transition invoke-1789035472733-18376-a140b069
apm-lean db762b37ee8c3df0abe8513437b8d5877d2feaf9 2026-09-10T10:19:56Z topology-loop: record t00a01-frozen-cover-coproduct-closure-probe-refill47 transition invoke-1789035520521-18378-53d9f62c
apm-lean b033ab76428646f7b26466e3bbdc94ce422ebda5 2026-09-10T10:22:12Z topology-loop: record oriented-checkpoint41-coefficient-transition-local-constancy-1 transition invoke-1789035563088-18380-10485227
apm-lean 0a6f88e5bf8831ac0149b2f38e5bdc2e182577d2 2026-09-10T10:22:56Z topology-loop: record coefficient-moving-boundary-relative-normalization-probe-refill47 transition invoke-1789035603066-18382-71898e82
apm-lean cd585e443ff8fc39c8832f299cb5bc903afaeb99 2026-09-10T10:23:22Z topology-loop: record oriented-checkpoint41-coefficient-transition-local-constancy-1 transition invoke-1789035734921-18384-47ea9f5b
apm-lean 7d1d361d8ca63cf4d71b5f942abf4f919dfc5cd9 2026-09-10T10:24:00Z feat(topology): promote coefficient chart-ball transport
apm-lean b8e903eff7a6f18cd1fdf5135dea0902a7446e52 2026-09-10T10:24:57Z topology-loop: record coefficient-chart-ball-transport-implementation-refill47 transition invoke-1789035782494-18386-d53d44af
apm-lean b896ecaa022c2542588e930a47900bb85c2cc5cd 2026-09-10T10:27:14Z topology-loop: record oriented-checkpoint42-local-constancy-and-consumer-refill transition invoke-1789035805578-18388-2b8e7aad
apm-lean 3c86f6b96178bdb2d9b3274edb85d2757eedb51c 2026-09-10T10:28:43Z topology-loop: record oriented-checkpoint42-local-constancy-and-consumer-refill transition invoke-1789036036869-18392-e5fd6ef1
apm-lean 442d01b37c76cbc41406e86a6b92c605d7065d60 2026-09-10T10:30:18Z topology-loop: record coefficient-chart-ball-transport-implementation-refill47 transition invoke-1789035903404-18390-a907e443
apm-lean 8fc9bdf0d982e631823e0b643e5e8df8aab3559a 2026-09-10T10:30:52Z topology-loop: record oriented-checkpoint42-euclidean-graph-hausdorff-area-1 transition invoke-1789036125908-18394-269b1df3
apm-lean 1569ccdfe3f716644ee88f4d86131008274970bf 2026-09-10T10:32:21Z topology-loop: record oriented-checkpoint42-euclidean-graph-hausdorff-area-1 transition invoke-1789036255020-18398-d534a894
apm-lean 94c21e2e9bcae584b38e15ec78e5b2dd3f50f597 2026-09-10T10:33:57Z topology-loop: record coefficient-moving-boundary-relative-normalization-probe-refill47 transition invoke-1789036224578-18396-4a6d43d0
apm-lean 05d174fcb9410f571e96f0b2f423296080253f33 2026-09-10T10:34:34Z topology-loop: record oriented-checkpoint43-graph-area-and-consumer-refill transition invoke-1789036344226-18400-5fe07a60
apm-lean 1dea549a487aa6baefdc1787eaaa3fa85a7ef098 2026-09-10T10:35:10Z feat(topology): promote ambient chart-ball normalization
apm-lean 9295e7ba26c5bf0e4a1fa9541d9de58461869cb3 2026-09-10T10:36:00Z topology-loop: record coefficient-moving-boundary-relative-normalization-probe-refill47 transition invoke-1789036444191-18402-98196db6
apm-lean 5fdf2f31589c59d2ca48a93dfd20b49aa6e534e2 2026-09-10T10:36:03Z topology-loop: record oriented-checkpoint43-graph-area-and-consumer-refill transition invoke-1789036476706-18404-eb290e0c
apm-lean 3ded070eb709602f9dedc625902b853a0914d730 2026-09-10T10:37:40Z topology-loop: record coefficient-moving-boundary-relative-normalization-probe-refill47 transition invoke-1789036567603-18408-043cff86
apm-lean a38f10c36388899215ac52a68feeaede7915ffe5 2026-09-10T10:40:14Z topology-loop: record oriented-checkpoint43-linear-graph-hausdorff-distortion-1 transition invoke-1789036566211-18407-4284d202
apm-lean 773bf66a17f555bcfa561f3d6b65f18b65e8ea8d 2026-09-10T10:41:46Z Construct linear graph range Hausdorff determinant comparison
apm-lean 960fd4afcf441ae11702b7ffa73e999fec1477e5 2026-09-10T10:42:19Z topology-loop: record plane-arc-small-translation-finite-intersections-probe-refill47 transition invoke-1789036667221-18410-8c48a9df
apm-lean 1c26a4c350408d4f1421b66d1bb2016ef044141e 2026-09-10T10:43:04Z topology-loop: record linear-graph-range-hausdorff-determinant-1 transition invoke-1789036817702-18412-8cd3bbe1
apm-lean 8b3aa9a1bed98a184d0d8ea2d43a8afcdb50d7d4 2026-09-10T10:43:23Z feat(topology): promote small plane-arc translations
apm-lean ec1f0cbd24c3cf48f0bdc0cb089bcfd7ba9de741 2026-09-10T10:44:17Z topology-loop: record plane-arc-small-translation-finite-intersections-probe-refill47 transition invoke-1789036945151-18414-db31ce24
apm-lean 3034656643598758260e49bd8efadd7da6baf872 2026-09-10T10:44:36Z topology-loop: record linear-graph-range-hausdorff-determinant-1 transition invoke-1789036987549-18416-305658ea
apm-lean fb6f9b9af1854e46fa6efa1eb305b3f30be955be 2026-09-10T10:45:55Z topology-loop: record plane-arc-small-translation-finite-intersections-probe-refill47 transition invoke-1789037064020-18418-9b7d32d0
apm-lean e70ece3c16fc7b5d6288eb2d39c93edc51e4df15 2026-09-10T10:48:49Z topology-loop: record oriented-checkpoint43-linear-graph-hausdorff-distortion-1 transition invoke-1789037083074-18420-36e77dcb
apm-lean a031b36db7d7279d1b10fa8c327ce64755b2e192 2026-09-10T10:49:34Z topology-loop: record checkpoint-full-dag-refill-48-after-cover-closure-boundary-and-translations transition invoke-1789037162904-18422-e6f966ad
apm-lean f52931597246cf0a47e933920adfa02143319517 2026-09-10T10:50:34Z Prove exact linear graph Hausdorff density
apm-lean 63c08cc07226ba669818b653c6706f6ec87a9886 2026-09-10T10:51:13Z topology-loop: record checkpoint-full-dag-refill-48-after-cover-closure-boundary-and-translations transition invoke-1789037381567-18426-0ec00da5
apm-lean cae3bb9a05d47a47ed5640d7eb1c94750152248a 2026-09-10T10:51:40Z topology-loop: record oriented-checkpoint43-linear-graph-hausdorff-distortion-1 transition invoke-1789037332479-18424-b007aec0
apm-lean 00e931c745e1b83ff9cc20787d018c71a617b61b 2026-09-10T10:53:10Z topology-loop: record oriented-checkpoint43-linear-graph-hausdorff-distortion-1 transition invoke-1789037503962-18430-5b4d081f
apm-lean db73e34eb80699872f845e738c9d6e1191ac5cf8 2026-09-10T10:53:12Z topology-loop: record t03j02-frozen-null-circle-gluing-closure-probe-refill48 transition invoke-1789037479382-18428-56bd1e1e
apm-lean e446d1562a85e878808f69703f0ae45cb82aec3f 2026-09-10T10:54:53Z topology-loop: record t03j02-frozen-null-circle-gluing-closure-probe-refill48 transition invoke-1789037601806-18434-58f436fa
apm-lean d645020593530a7385e2904fb363f2551afe809f 2026-09-10T10:55:21Z topology-loop: record oriented-checkpoint44-linear-area-and-consumer-refill transition invoke-1789037593377-18432-bbf4a0e3
apm-lean 80eaa00e27f7f650311d3c851a9bc242edeb9b76 2026-09-10T10:56:50Z topology-loop: record oriented-checkpoint44-linear-area-and-consumer-refill transition invoke-1789037723667-18438-a0b6fb40
apm-lean e26850503770ecf839c8bbcfb07d27ff9ccb6bad 2026-09-10T10:57:54Z topology-loop: record coefficient-chart-transition-unit-local-constancy-probe-refill48 transition invoke-1789037700193-18436-fb35b883
apm-lean bd4a32af71ac87b15ef685642519c89948d48d84 2026-09-10T10:59:01Z feat(topology): promote second-chart boundary comparison
apm-lean 7280226c500375714d48e82c4b9a239b70af2642 2026-09-10T10:59:52Z topology-loop: record coefficient-second-chart-boundary-implementation-refill48 transition invoke-1789037880259-18442-16944c73
apm-lean baf02c995f553ae0d11d77f8edeb3e3a88f2f16e 2026-09-10T11:01:31Z topology-loop: record coefficient-second-chart-boundary-implementation-refill48 transition invoke-1789037999068-18444-447071d9
apm-lean 0cf89cbceecc13764747259007a88d258c16374e 2026-09-10T11:04:14Z topology-loop: record coefficient-chart-transition-unit-local-constancy-probe-refill48 transition invoke-1789038098549-18446-84ca58b6
apm-lean 41e1e96d924c20182cf8fb888a7cf48ba13458e9 2026-09-10T11:05:24Z feat(topology): promote second-chart boundary motion
apm-lean f54a36c825bea97d5fdddeba0a2f5211770e979e 2026-09-10T11:05:41Z topology-loop: record oriented-checkpoint44-uniform-graph-hausdorff-bounds-1 transition invoke-1789037813332-18440-826842ad
apm-lean 16f9d25eb8cd2f4dccda843559b5d8e338acf963 2026-09-10T11:06:16Z topology-loop: record coefficient-second-chart-boundary-motion-implementation-refill48 transition invoke-1789038262449-18448-b612fa81
apm-lean b7790b72e5630433d629dae4464b82b51d647b0c 2026-09-10T11:08:15Z topology-loop: record coefficient-second-chart-boundary-motion-implementation-refill48 transition invoke-1789038383499-18452-4a88445d
apm-lean f882696c8da80267fff63443a43247be0f355c80 2026-09-10T11:09:25Z Construct uniform L2 graph Hausdorff bounds
apm-lean c534ad9f2e3f7c380245c31bc218be657794676d 2026-09-10T11:10:52Z topology-loop: record oriented-checkpoint44-uniform-graph-hausdorff-bounds-1 transition invoke-1789038344789-18450-10c20142
apm-lean fe25dcd9a49c788e2f18031551c549d2c188762f 2026-09-10T11:12:26Z topology-loop: record oriented-checkpoint44-uniform-graph-hausdorff-bounds-1 transition invoke-1789038655657-18457-4f4f4a2d
apm-lean e7e9ab4aa6ca38f964e18013ca75604b4fb40a4a 2026-09-10T11:14:06Z m02J05 formalize repaired definiteness counterexample
apm-lean 30695e3fe8faad8e253dab1a16c575b8125c6a99 2026-09-10T11:16:26Z topology-loop: record coefficient-chart-transition-unit-local-constancy-probe-refill48 transition invoke-1789038503007-18454-9f8c4bc2
apm-lean 30360ff1c5cd2e763006c7822a5688572f880d52 2026-09-10T11:16:39Z topology-loop: record oriented-checkpoint45-uniform-area-bounds-and-consumer-refill transition invoke-1789038749776-18460-347a949c
```

</details>

<details><summary>Expand outside reference files (literal matches, not all runtime dependencies)</summary>

```text
apm-lean/ConstructionTargets/C1GraphLocalApproximation.lean
apm-lean/ConstructionTargets/C1GraphMeasureDecomposition.lean
apm-lean/ConstructionTargets/CoefficientAmbientSubspaceRangeCompatibility.lean
apm-lean/ConstructionTargets/CoefficientCompactComplementNestedExcisionChains.lean
apm-lean/ConstructionTargets/CoefficientCompactComplementNestedExcisionSource.lean
apm-lean/ConstructionTargets/CoefficientCompactComplementSmallComparison.lean
apm-lean/ConstructionTargets/CoefficientCompactSupportConnecting.lean
apm-lean/ConstructionTargets/CoefficientCompactSupportPiecesToIntersection.lean
apm-lean/ConstructionTargets/CoefficientCompactSupportRelativeArrowComplex.lean
apm-lean/ConstructionTargets/CoefficientCompactSupportRestrictionMaps.lean
apm-lean/ConstructionTargets/CoefficientCompactSupportRestrictionNaturality.lean
apm-lean/ConstructionTargets/CoefficientPairConnectingMayerVietorisComparison.lean
apm-lean/ConstructionTargets/CoefficientSubcomplexMayerVietoris.lean
apm-lean/ConstructionTargets/CoefficientSubspaceUnionRelativeCokernelSquare.lean
apm-lean/ConstructionTargets/CoefficientSubspaceUnionRestrictedSmallHomology.lean
apm-lean/ConstructionTargets/CoefficientTransitionLocalConstancy.lean
apm-lean/ConstructionTargets/PlaneArcFiniteSmallTranslations.lean
apm-lean/ConstructionTargets/RadialSquaredFiber.lean
apm-lean/ConstructionTargets/SphereCapL2Image.lean
apm-lean/problems/t96J05/lean/Main.lean
apm-lean/problems/t98A06/lean/Main.lean
futon3c/holes/labs/M-apm-demonstration/analysis/pattern-construction-2026-09-10/topology_induction.py
```

</details>

### Candidate 2: changed paths

<details><summary>Expand full path inventory</summary>

| Repository / path | Current lines | Parent-absent | Earliest touching commit |
|---|---:|---|---|
| `apm-lean/problems/b97J04/lean/Main.lean` | 312 | no | `301466ec13bca2cc92d837d96870476c06f2e167` |
| `apm-lean/problems/b98A01/lean/Main.lean` | 527 | no | `c1c6daeb77755685f7c38104b234350bb0da8305` |
| `apm-lean/problems/b98A02/lean/Main.lean` | 230 | no | `40214c33b7633f064da074735b469dfd564b41d9` |
| `apm-lean/problems/b98A04/lean/Main.lean` | 849 | no | `72064774f851af7167143ed9319c1bd096e0dff4` |
| `apm-lean/problems/b98J01/lean/Main.lean` | 495 | no | `b0fded058ab220f0bf5b8d4d3729a4d8375568f5` |
| `apm-lean/problems/b98J04/lean/Main.lean` | 770 | no | `64139bff8356e7e454a1cf469866e40502fff1cd` |
| `apm-lean/problems/b99A02/lean/Main.lean` | 394 | no | `8706d052c575dd267b5e4f901ecce318250bea11` |
| `apm-lean/problems/bpm-1-8-1/lean/Main.lean` | 450 | no | `52ec53e0bfe682bdde6f96a286c7c3cc0f7ad807` |
| `apm-lean/problems/m00A02/lean/Main.lean` | 468 | no | `e2adb88859f461268d764f5c1a7aae0ff3cf0feb` |
| `apm-lean/problems/m00A03/lean/Main.lean` | 206 | no | `fe0c41ec5eacdfdb5918474afe538f4baba18a85` |
| `apm-lean/problems/m00A04/lean/Main.lean` | 500 | no | `a5cc91f3845107130e31a127fb98635968402624` |
| `apm-lean/problems/m00A05/lean/Main.lean` | 624 | no | `cd591fd234bc7ac874412d3d649e91a169b5a442` |
| `apm-lean/problems/m00A06/lean/Main.lean` | 569 | no | `ad9825a0c60c7bd14ab79d5c10f301fa7dd8bbbb` |
| `apm-lean/problems/m01J04/lean/Main.lean` | 2275 | no | `85866ac46693cec2c4bd234f4e6bbdbc573388cb` |
| `apm-lean/problems/m01J05/lean/Main.lean` | 71 | no | `e29168d5bfa247ec4bfe8484bc17f1fd89163b78` |
| `apm-lean/problems/m01J06/lean/Main.lean` | 280 | no | `7023f5f111dd75a4b8963fb7938490c05329307a` |
| `apm-lean/problems/m02A02/lean/Main.lean` | 813 | no | `300c046f12768c55342295dde6b06dc2159b58f8` |
| `apm-lean/problems/m02A03/lean/Main.lean` | 895 | no | `7b931f5a0af81e6d62a90e0f5b16a580636fd32c` |
| `apm-lean/problems/m02A05/lean/Main.lean` | 1031 | no | `b0fa43cfc3e2b622a3d67654fef9ba44d76a5c1f` |
| `apm-lean/problems/m02A06/lean/Main.lean` | 2267 | no | `40747e41c9943a2b5bfd10695607215cf74ca854` |
| `apm-lean/problems/m02J01/lean/Main.lean` | 948 | no | `a5df9344621b2c7230f131fe0def72c1c5cc9e11` |
| `apm-lean/problems/m02J02/lean/Main.lean` | 204 | no | `8cbf54488d786886ea38a53ceab50937475c4ce0` |
| `futon2/holes/labs/library-loop/runs/L10-no-source-check.edn` | 140 | yes | `1f605f500b2f85aa7e3c3f06ce4400d7bb46e9a4` |
| `futon2/holes/labs/library-loop/runs/L11-iching-iiching-triage.md` | 89 | yes | `63b38f8ba598a32efde08943e61b8d46141e16af` |
| `futon2/holes/labs/library-loop/runs/L12-census-graph.edn` | 8058 | yes | `237742696b0175dc13251d1ccc90cb8bd876dfbe` |
| `futon2/holes/labs/library-loop/runs/L12-census-receipt.edn` | 4399 | yes | `237742696b0175dc13251d1ccc90cb8bd876dfbe` |
| `futon2/holes/labs/library-loop/runs/L12-delta.md` | 53 | yes | `237742696b0175dc13251d1ccc90cb8bd876dfbe` |
| `futon2/holes/labs/library-loop/runs/L14-census-graph.edn` | 8132 | yes | `a92877e39afd4438d274e0451f853823942c3c4d` |
| `futon2/holes/labs/library-loop/runs/L14-census-receipt.edn` | 4424 | yes | `a92877e39afd4438d274e0451f853823942c3c4d` |
| `futon2/holes/labs/library-loop/runs/L14-delta.md` | 29 | yes | `a92877e39afd4438d274e0451f853823942c3c4d` |
| `futon2/holes/labs/library-loop/runs/L14-edge-resolution.edn` | 479 | yes | `a92877e39afd4438d274e0451f853823942c3c4d` |
| `futon2/holes/labs/library-loop/runs/L16-census-receipt.edn` | 4426 | yes | `57296b21d1ba73d7685060f122d17ebdc6f64ceb` |
| `futon2/holes/labs/library-loop/runs/L16-duplicate-check.edn` | 41 | yes | `57296b21d1ba73d7685060f122d17ebdc6f64ceb` |
| `futon2/holes/labs/library-loop/runs/L16-edge-match.edn` | 49 | yes | `57296b21d1ba73d7685060f122d17ebdc6f64ceb` |
| `futon2/holes/labs/library-loop/runs/L17-advisory-gate-report.md` | 83 | yes | `1054d51cb393765c9919cd011ace1605b34da58a` |
| `futon2/holes/labs/library-loop/runs/L17-census-receipt.edn` | 4426 | yes | `1054d51cb393765c9919cd011ace1605b34da58a` |
| `futon2/holes/labs/library-loop/runs/L17-graph.edn` | 8397 | yes | `1054d51cb393765c9919cd011ace1605b34da58a` |
| `futon2/holes/labs/library-loop/runs/L18-advisory-gate-report.md` | 83 | yes | `266d14f32bd0dda747ded8d7f97524aa3f192204` |
| `futon2/holes/labs/library-loop/runs/L18-graph.edn` | 8601 | yes | `266d14f32bd0dda747ded8d7f97524aa3f192204` |
| `futon2/holes/labs/library-loop/runs/L18-remainder.edn` | 225 | yes | `266d14f32bd0dda747ded8d7f97524aa3f192204` |
| `futon2/holes/labs/library-loop/runs/L19-how-side.md` | 59 | yes | `755ec930754e08f67675495f8b812bcdd62b539b` |
| `futon2/holes/labs/library-loop/runs/L6-no-source-check.edn` | 111 | no | `2554119f6ed7d29c41aa4f6dee7a9e2bcda699e9` |
| `futon2/holes/labs/library-loop/runs/L7-no-source-check.edn` | 135 | yes | `30f3183f8b33d11ee6c747a7211b563c9ef832e9` |
| `futon2/holes/labs/library-loop/runs/L8-no-source-check.edn` | 175 | yes | `baa74c5ed7e28fd6739a58d45e8310e5b5961ed3` |
| `futon2/holes/labs/library-loop/runs/L9-no-source-check.edn` | 117 | yes | `d0c82cb05fab5197384f023ea9f69bc5f8f5faea` |
| `futon2/holes/labs/library-loop/runs/l13-negative-control.edn` | 5 | yes | `e94e0bcdf73f7b3e746ab8a0766cbb39ac6b4688` |
| `futon2/holes/labs/library-loop/runs/l13-reading-control-wr.edn` | 8 | yes | `f1f36b928c39b336cd741cf7222d9b71ea8c881c` |
| `futon2/holes/labs/library-loop/runs/l13_graph_gate.clj` | 76 | yes | `e94e0bcdf73f7b3e746ab8a0766cbb39ac6b4688` |
| `futon2/holes/labs/library-loop/runs/l14_resolve.py` | 112 | yes | `a92877e39afd4438d274e0451f853823942c3c4d` |
| `futon2/holes/labs/library-loop/runs/l15_exotype_why.py` | 43 | yes | `f8ee74473a8e06c94a88a03e730d89866109993b` |
| `futon2/holes/labs/library-loop/runs/l17_advisory_report.py` | 107 | yes | `1054d51cb393765c9919cd011ace1605b34da58a` |
| `futon2/holes/labs/library-loop/runs/l18_complete_check.py` | 180 | yes | `eb2ac61fa873e6d6c22906fab94f1cf80f2a270a` |
| `futon2/holes/labs/library-loop/runs/l18_ground.py` | 89 | yes | `266d14f32bd0dda747ded8d7f97524aa3f192204` |
| `futon2/holes/labs/library-loop/runs/l18c_mint.py` | 99 | yes | `9d35fd455bae5a30cfd4ce3255ea344147c209a8` |
| `futon2/holes/labs/library-loop/runs/l19_how_side.py` | 202 | yes | `755ec930754e08f67675495f8b812bcdd62b539b` |
| `futon2/holes/labs/library-loop/runs/l2_parse_gate.py` | 113 | no | `6cc33fee282b7dc7c40343e5c8f994a9e7223c5f` |
| `futon2/holes/labs/library-loop/runs/l6_backfill.py` | 121 | no | `2554119f6ed7d29c41aa4f6dee7a9e2bcda699e9` |
| `futon2/holes/labs/library-loop/runs/l6_no_source_check.py` | 90 | no | `2554119f6ed7d29c41aa4f6dee7a9e2bcda699e9` |
| `futon2/holes/labs/library-loop/worklist.edn` | 256 | no | `d465a975bf1643baae7c82b17e841ec206b9eff5` |
| `futon2/holes/labs/wm-contract/C514-i4-registry-drafts.md` | 130 | yes | `0b0bb8881699dbf67f076a3461649bd18259362c` |
| `futon2/holes/labs/wm-contract/C515-F7-cascade-policy-carrier.md` | 192 | no | `1f389b41e6852b708e20caae0fa840c7e37132d0` |
| `futon2/holes/labs/wm-contract/C516-F1-machine-grain-q.md` | 156 | yes | `43a1b3dc51a3e3cf041d6e6df427396b2d4ec021` |
| `futon2/holes/labs/wm-contract/C517-F8-lean-state-join-refresh.md` | 141 | yes | `993d0aa9147fad0f756ace84c66fd60d2332ffd1` |
| `futon2/holes/labs/wm-contract/C518-F8-precision-lean.md` | 179 | yes | `13d1a7cf045b77f68999b1b93a2d788f811b46cf` |
| `futon2/holes/labs/wm-contract/C519-F8-prediction-error-lean.md` | 250 | yes | `f471ac10ac87c8fb5d87e7417b321a242521d869` |
| `futon2/holes/labs/wm-contract/C520-F8-belief-update-lean.md` | 257 | yes | `04dcaa876ebb855f9a93cff5592f23e8caaf80d4` |
| `futon2/holes/labs/wm-contract/C521-F8-policy-free-energy-lean.md` | 264 | yes | `a776e2c9537b340228720b360086005736c1732c` |
| `futon2/holes/labs/wm-contract/C522-F8-code-pointer-sweep.md` | 289 | yes | `0b7cf967e3bbca8d5536d13b3a57f925c196930a` |
| `futon2/holes/labs/wm-contract/C523-F8-observe-lean.md` | 274 | yes | `d7a274f747587c46087ed3bc78fab4dddd336a91` |
| `futon2/holes/labs/wm-contract/C524-F8-belief-state-lean.md` | 271 | yes | `f66fdefb83827b6ab01729e385fc0c814cc1a21a` |
| `futon2/holes/labs/wm-contract/C525-F8-depth-lean.md` | 257 | yes | `af438f19ded56e0d16afa65d255f3ac26d437262` |
| `futon2/holes/labs/wm-contract/C526-F8-temperature-lean.md` | 266 | yes | `80a1a7a8544e151ccab55655a78009881e123d6f` |
| `futon2/holes/labs/wm-contract/C527-F8-action-lean.md` | 292 | yes | `13dbb58cdccc5f320cd1e2fb9773351fb6bedfc2` |
| `futon2/holes/labs/wm-contract/C528-F8-r17-class-b-lean.md` | 270 | yes | `561b18c5b4adcd89a4059ae2dd152e3858478a33` |
| `futon2/holes/labs/wm-contract/C529-F8-symbol-concordance.md` | 92 | yes | `c49f07eda382787cc66c2d1f282d94ff88772964` |
| `futon2/holes/labs/wm-contract/C530-F8-symbol-concordance-checker.md` | 115 | yes | `bd206447f0b1ea9c8879b610ec7636c72c851f71` |
| `futon2/holes/labs/wm-contract/C531-F8-convergence-ledger.md` | 78 | yes | `b40c81cd28554b3f0442c599f796d5e6a9da3c44` |
| `futon2/holes/labs/wm-contract/C532-F8-convergence-checker.md` | 124 | yes | `3526b79e134922d31831fd178316eba88d299d66` |
| `futon2/holes/labs/wm-contract/C533-F9-cascade-decision-wiring.md` | 237 | yes | `adbf53fa04a1590ea4c3d1a1f33ebaf68a96a91d` |
| `futon2/holes/labs/wm-contract/C534-re6-fold-vocabulary-split.md` | 159 | yes | `4c62c776e0a9ed8474b907e9f41c951a2029ccd3` |
| `futon2/holes/labs/wm-contract/C535-U64-rollout-parameter-provenance.md` | 200 | yes | `09b92d7ee8bb9f1b31d17963dae5bf5755cec5a9` |
| `futon2/holes/labs/wm-contract/C536-U65-registered-seat-callers-and-job-grain-cancel.md` | 175 | yes | `59d8a21b156e41b710b5890333984cedaeced189` |
| `futon2/holes/labs/wm-contract/CONVERGENCE.edn` | 146 | yes | `b40c81cd28554b3f0442c599f796d5e6a9da3c44` |
| `futon2/holes/labs/wm-contract/EPIC-run-era.md` | 1130 | no | `15125c9317b9350d781f3b465d65b40dd55afcfe` |
| `futon2/holes/labs/wm-contract/Q-interface-completeness.edn` | 445 | no | `43a1b3dc51a3e3cf041d6e6df427396b2d4ec021` |
| `futon2/holes/labs/wm-contract/aif-equations.edn` | 1293 | no | `ef08d0fd427038fb75a7dbfe90c1b611dc669158` |
| `futon2/holes/labs/wm-contract/bulletins/BULLETIN-2026-09-05.md` | 538 | no | `bb4a5c03b3297c3357ddfcdcc9ae48b8901c0efd` |
| `futon2/holes/labs/wm-contract/convergence_check.bb` | 234 | yes | `3526b79e134922d31831fd178316eba88d299d66` |
| `futon2/holes/labs/wm-contract/f1_machine_grain_q.clj` | 374 | yes | `43a1b3dc51a3e3cf041d6e6df427396b2d4ec021` |
| `futon2/holes/labs/wm-contract/f8_action_readback.clj` | 145 | yes | `4f1cc81ee48cb0878b7bf0c8f3e5b5e46c34f45f` |
| `futon2/holes/labs/wm-contract/f8_belief_state_readback.clj` | 89 | yes | `5a553275e6611ec264b25fd2516fcace500677be` |
| `futon2/holes/labs/wm-contract/f8_belief_update_readback.clj` | 119 | yes | `69154b1d53007eb88519cf34efaec4b39cc0281f` |
| `futon2/holes/labs/wm-contract/f8_depth_readback.clj` | 115 | yes | `af438f19ded56e0d16afa65d255f3ac26d437262` |
| `futon2/holes/labs/wm-contract/f8_dirichlet_accumulation_readback.clj` | 119 | yes | `b26b5f4df75145bfdeb9caa3c8c08911153e870b` |
| `futon2/holes/labs/wm-contract/f8_observation_readback.clj` | 182 | yes | `402b8cfdfe5ed75ee1e658a8e2f88da7d5bc5063` |
| `futon2/holes/labs/wm-contract/f8_policy_free_energy_readback.clj` | 130 | yes | `928b898c90075c5fad534a516c855e1049174700` |
| `futon2/holes/labs/wm-contract/f8_precision_readback.clj` | 65 | yes | `13d1a7cf045b77f68999b1b93a2d788f811b46cf` |
| `futon2/holes/labs/wm-contract/f8_prediction_error_readback.clj` | 159 | yes | `f471ac10ac87c8fb5d87e7417b321a242521d869` |
| `futon2/holes/labs/wm-contract/f8_temperature_readback.clj` | 161 | yes | `80a1a7a8544e151ccab55655a78009881e123d6f` |
| `futon2/holes/labs/wm-contract/f9_cascade_target_check.bb` | 172 | yes | `adbf53fa04a1590ea4c3d1a1f33ebaf68a96a91d` |
| `futon2/holes/labs/wm-contract/run-era-ledger.edn` | 570 | no | `4c62c776e0a9ed8474b907e9f41c951a2029ccd3` |
| `futon2/holes/labs/wm-contract/run_era_ledger.bb` | 1261 | no | `4c62c776e0a9ed8474b907e9f41c951a2029ccd3` |
| `futon2/holes/labs/wm-contract/runs/2026-09-01-s1b/CONFORMANCE.md` | 71 | no | `ef08d0fd427038fb75a7dbfe90c1b611dc669158` |
| `futon2/holes/labs/wm-contract/runs/F1-machine-q/03-machine-grain-q.edn` | 325 | yes | `43a1b3dc51a3e3cf041d6e6df427396b2d4ec021` |
| `futon2/holes/labs/wm-contract/runs/F8-action/clojure-readback.txt` | 22 | yes | `4f1cc81ee48cb0878b7bf0c8f3e5b5e46c34f45f` |
| `futon2/holes/labs/wm-contract/runs/F8-action/lean-state-join-after.edn` | 2203 | yes | `13dbb58cdccc5f320cd1e2fb9773351fb6bedfc2` |
| `futon2/holes/labs/wm-contract/runs/F8-action/lean-state-join-before.edn` | 2168 | yes | `13dbb58cdccc5f320cd1e2fb9773351fb6bedfc2` |
| `futon2/holes/labs/wm-contract/runs/F8-action/lean-state-probe.log` | 95 | yes | `13dbb58cdccc5f320cd1e2fb9773351fb6bedfc2` |
| `futon2/holes/labs/wm-contract/runs/F8-action/review-independent-probe.clj` | 125 | yes | `13dbb58cdccc5f320cd1e2fb9773351fb6bedfc2` |
| `futon2/holes/labs/wm-contract/runs/F8-action/review-independent-probe.txt` | 31 | yes | `13dbb58cdccc5f320cd1e2fb9773351fb6bedfc2` |
| `futon2/holes/labs/wm-contract/runs/F8-belief-state/clojure-readback.txt` | 16 | yes | `5a553275e6611ec264b25fd2516fcace500677be` |
| `futon2/holes/labs/wm-contract/runs/F8-belief-state/lean-state-join-after.edn` | 2015 | yes | `f66fdefb83827b6ab01729e385fc0c814cc1a21a` |
| `futon2/holes/labs/wm-contract/runs/F8-belief-state/lean-state-join-before.edn` | 2011 | yes | `f66fdefb83827b6ab01729e385fc0c814cc1a21a` |
| `futon2/holes/labs/wm-contract/runs/F8-belief-state/review-independent-probe.clj` | 39 | yes | `f66fdefb83827b6ab01729e385fc0c814cc1a21a` |
| `futon2/holes/labs/wm-contract/runs/F8-belief-state/review-independent-probe.txt` | 17 | yes | `f66fdefb83827b6ab01729e385fc0c814cc1a21a` |
| `futon2/holes/labs/wm-contract/runs/F8-belief-update/clojure-readback.txt` | 24 | yes | `69154b1d53007eb88519cf34efaec4b39cc0281f` |
| `futon2/holes/labs/wm-contract/runs/F8-belief-update/lean-state-join-check.edn` | 1923 | yes | `69154b1d53007eb88519cf34efaec4b39cc0281f` |
| `futon2/holes/labs/wm-contract/runs/F8-convergence/leg3-slice1-independent-probe.bb` | 160 | yes | `848fa3833c780f4f327c9f623ae9b7e23faba79b` |
| `futon2/holes/labs/wm-contract/runs/F8-convergence/leg3-slice1-independent-probe.txt` | 154 | yes | `848fa3833c780f4f327c9f623ae9b7e23faba79b` |
| `futon2/holes/labs/wm-contract/runs/F8-convergence/leg3-slice1-review-check.bb` | 140 | yes | `8f6a39b1d46d67d8e74128974877ffcf48deca49` |
| `futon2/holes/labs/wm-contract/runs/F8-convergence/leg3-slice1-review-check.txt` | 77 | yes | `8f6a39b1d46d67d8e74128974877ffcf48deca49` |
| `futon2/holes/labs/wm-contract/runs/F8-convergence/leg3-slice2-adversarial-plants.bb` | 167 | yes | `19accec9757cc0b0f9d6864273c6809dea807ee5` |
| `futon2/holes/labs/wm-contract/runs/F8-convergence/leg3-slice2-adversarial-plants.txt` | 52 | yes | `96b84d7d62d752052254512bd9d36ca3376a4032` |
| `futon2/holes/labs/wm-contract/runs/F8-convergence/leg3-slice2-independent-probe.bb` | 127 | yes | `a401ecd225e6d0d572f4fda21be329c4b3651ad5` |
| `futon2/holes/labs/wm-contract/runs/F8-convergence/leg3-slice2-independent-probe.txt` | 40 | yes | `a401ecd225e6d0d572f4fda21be329c4b3651ad5` |
| `futon2/holes/labs/wm-contract/runs/F8-depth/clojure-readback.txt` | 17 | yes | `af438f19ded56e0d16afa65d255f3ac26d437262` |
| `futon2/holes/labs/wm-contract/runs/F8-depth/lean-state-join-after.edn` | 2133 | yes | `af438f19ded56e0d16afa65d255f3ac26d437262` |
| `futon2/holes/labs/wm-contract/runs/F8-depth/lean-state-join-before.edn` | 2015 | yes | `af438f19ded56e0d16afa65d255f3ac26d437262` |
| `futon2/holes/labs/wm-contract/runs/F8-depth/review-independent-probe.clj` | 52 | yes | `af438f19ded56e0d16afa65d255f3ac26d437262` |
| `futon2/holes/labs/wm-contract/runs/F8-depth/review-independent-probe.txt` | 15 | yes | `af438f19ded56e0d16afa65d255f3ac26d437262` |
| `futon2/holes/labs/wm-contract/runs/F8-dirichlet-accumulation/clojure-readback.txt` | 21 | yes | `b26b5f4df75145bfdeb9caa3c8c08911153e870b` |
| `futon2/holes/labs/wm-contract/runs/F8-dirichlet-accumulation/lean-state-join-after.edn` | 2234 | yes | `561b18c5b4adcd89a4059ae2dd152e3858478a33` |
| `futon2/holes/labs/wm-contract/runs/F8-dirichlet-accumulation/lean-state-join-before.edn` | 2203 | yes | `561b18c5b4adcd89a4059ae2dd152e3858478a33` |
| `futon2/holes/labs/wm-contract/runs/F8-dirichlet-accumulation/lean-state-probe-after.log` | 97 | yes | `561b18c5b4adcd89a4059ae2dd152e3858478a33` |
| `futon2/holes/labs/wm-contract/runs/F8-dirichlet-accumulation/lean-state-probe-before.log` | 95 | yes | `561b18c5b4adcd89a4059ae2dd152e3858478a33` |
| `futon2/holes/labs/wm-contract/runs/F8-dirichlet-accumulation/review-independent-probe-2.clj` | 22 | yes | `561b18c5b4adcd89a4059ae2dd152e3858478a33` |
| `futon2/holes/labs/wm-contract/runs/F8-dirichlet-accumulation/review-independent-probe-2.txt` | 9 | yes | `561b18c5b4adcd89a4059ae2dd152e3858478a33` |
| `futon2/holes/labs/wm-contract/runs/F8-dirichlet-accumulation/review-independent-probe.clj` | 69 | yes | `579bdecaddb27ba5d4ad7f23c6a5a73eac2f7f13` |
| `futon2/holes/labs/wm-contract/runs/F8-dirichlet-accumulation/review-independent-probe.txt` | 29 | yes | `579bdecaddb27ba5d4ad7f23c6a5a73eac2f7f13` |
| `futon2/holes/labs/wm-contract/runs/F8-observe/clojure-readback.txt` | 18 | yes | `402b8cfdfe5ed75ee1e658a8e2f88da7d5bc5063` |
| `futon2/holes/labs/wm-contract/runs/F8-observe/codex-handoff-summary.md` | 38 | yes | `402b8cfdfe5ed75ee1e658a8e2f88da7d5bc5063` |
| `futon2/holes/labs/wm-contract/runs/F8-observe/lean-state-join-check.edn` | 1983 | yes | `d7a274f747587c46087ed3bc78fab4dddd336a91` |
| `futon2/holes/labs/wm-contract/runs/F8-policy-free-energy/clojure-readback.txt` | 17 | yes | `928b898c90075c5fad534a516c855e1049174700` |
| `futon2/holes/labs/wm-contract/runs/F8-policy-free-energy/codex-handoff-summary.md` | 32 | yes | `928b898c90075c5fad534a516c855e1049174700` |
| `futon2/holes/labs/wm-contract/runs/F8-policy-free-energy/lean-state-join-check.edn` | 1950 | yes | `a776e2c9537b340228720b360086005736c1732c` |
| `futon2/holes/labs/wm-contract/runs/F8-policy-free-energy/lean-state-probe.edn` | 1950 | yes | `928b898c90075c5fad534a516c855e1049174700` |
| `futon2/holes/labs/wm-contract/runs/F8-precision/clojure-readback.txt` | 19 | yes | `13d1a7cf045b77f68999b1b93a2d788f811b46cf` |
| `futon2/holes/labs/wm-contract/runs/F8-precision/lean-state-join-check.edn` | 1859 | yes | `13d1a7cf045b77f68999b1b93a2d788f811b46cf` |
| `futon2/holes/labs/wm-contract/runs/F8-prediction-error/clojure-readback.txt` | 39 | yes | `f471ac10ac87c8fb5d87e7417b321a242521d869` |
| `futon2/holes/labs/wm-contract/runs/F8-prediction-error/lean-state-join-check.edn` | 1891 | yes | `f471ac10ac87c8fb5d87e7417b321a242521d869` |
| `futon2/holes/labs/wm-contract/runs/F8-symbol-concordance/independent-probe.bb` | 90 | yes | `a538b9a7e005ba1a27c16e572d6e771b26408e97` |
| `futon2/holes/labs/wm-contract/runs/F8-symbol-concordance/independent-probe.txt` | 77 | yes | `a538b9a7e005ba1a27c16e572d6e771b26408e97` |
| `futon2/holes/labs/wm-contract/runs/F8-symbol-concordance/review-column-check.bb` | 173 | yes | `eeedf2de2deef996ac7ce5ae41457c1a5bff51ff` |
| `futon2/holes/labs/wm-contract/runs/F8-symbol-concordance/review-column-check.txt` | 121 | yes | `eeedf2de2deef996ac7ce5ae41457c1a5bff51ff` |
| `futon2/holes/labs/wm-contract/runs/F8-symbol-concordance/slice2-adversarial-plants.bb` | 137 | yes | `f48a57c85704b0bd4c3b086ecdb0c247558c5953` |
| `futon2/holes/labs/wm-contract/runs/F8-symbol-concordance/slice2-adversarial-plants.txt` | 25 | yes | `3dcba597a21cda670ba8a153f2c840e6d410d6ca` |
| `futon2/holes/labs/wm-contract/runs/F8-symbol-concordance/slice2-independent-probe.bb` | 137 | yes | `f4cf278ebcbdda1e5411bc1a58f20559b64f416a` |
| `futon2/holes/labs/wm-contract/runs/F8-symbol-concordance/slice2-independent-probe.txt` | 49 | yes | `f4cf278ebcbdda1e5411bc1a58f20559b64f416a` |
| `futon2/holes/labs/wm-contract/runs/F8-symbol-concordance/slice2-tex-pointer-blast-radius.bb` | 36 | yes | `f48a57c85704b0bd4c3b086ecdb0c247558c5953` |
| `futon2/holes/labs/wm-contract/runs/F8-symbol-concordance/slice2-tex-pointer-blast-radius.txt` | 277 | yes | `f48a57c85704b0bd4c3b086ecdb0c247558c5953` |
| `futon2/holes/labs/wm-contract/runs/F8-temperature/clojure-readback.txt` | 34 | yes | `80a1a7a8544e151ccab55655a78009881e123d6f` |
| `futon2/holes/labs/wm-contract/runs/F8-temperature/lean-state-join-after.edn` | 2168 | yes | `80a1a7a8544e151ccab55655a78009881e123d6f` |
| `futon2/holes/labs/wm-contract/runs/F8-temperature/lean-state-join-before.edn` | 2133 | yes | `80a1a7a8544e151ccab55655a78009881e123d6f` |
| `futon2/holes/labs/wm-contract/runs/F8-temperature/lean-state-probe.log` | 93 | yes | `80a1a7a8544e151ccab55655a78009881e123d6f` |
| `futon2/holes/labs/wm-contract/runs/F8-temperature/review-independent-probe.clj` | 126 | yes | `80a1a7a8544e151ccab55655a78009881e123d6f` |
| `futon2/holes/labs/wm-contract/runs/F8-temperature/review-independent-probe.txt` | 53 | yes | `80a1a7a8544e151ccab55655a78009881e123d6f` |
| `futon2/holes/labs/wm-contract/runs/F9-cascade-decision/00-corpus-target-gap.edn` | 402 | yes | `adbf53fa04a1590ea4c3d1a1f33ebaf68a96a91d` |
| `futon2/holes/labs/wm-contract/runs/F9-cascade-decision/01-step-016-before.edn` | 18 | yes | `adbf53fa04a1590ea4c3d1a1f33ebaf68a96a91d` |
| `futon2/holes/labs/wm-contract/runs/F9-cascade-decision/016-f9-base-flagoff-delta.edn` | 27623 | yes | `adbf53fa04a1590ea4c3d1a1f33ebaf68a96a91d` |
| `futon2/holes/labs/wm-contract/runs/F9-cascade-decision/016-f9-base-flagoff-step.edn` | 28 | yes | `adbf53fa04a1590ea4c3d1a1f33ebaf68a96a91d` |
| `futon2/holes/labs/wm-contract/runs/F9-cascade-decision/017-f9-wired-flagon-delta.edn` | 33899 | yes | `adbf53fa04a1590ea4c3d1a1f33ebaf68a96a91d` |
| `futon2/holes/labs/wm-contract/runs/F9-cascade-decision/017-f9-wired-flagon-step.edn` | 28 | yes | `adbf53fa04a1590ea4c3d1a1f33ebaf68a96a91d` |
| `futon2/holes/labs/wm-contract/runs/F9-cascade-decision/018-f9-newcode-flagoff-delta.edn` | 27623 | yes | `adbf53fa04a1590ea4c3d1a1f33ebaf68a96a91d` |
| `futon2/holes/labs/wm-contract/runs/F9-cascade-decision/018-f9-newcode-flagoff-step.edn` | 28 | yes | `adbf53fa04a1590ea4c3d1a1f33ebaf68a96a91d` |
| `futon2/holes/labs/wm-contract/runs/F9-cascade-decision/02-step-017-after.edn` | 22 | yes | `adbf53fa04a1590ea4c3d1a1f33ebaf68a96a91d` |
| `futon2/holes/labs/wm-contract/runs/F9-cascade-decision/03-flag-off-control-compare.edn` | 19 | yes | `adbf53fa04a1590ea4c3d1a1f33ebaf68a96a91d` |
| `futon2/holes/labs/wm-contract/runs/F9-cascade-decision/04-flag-off-control-world-drift.edn` | 21 | yes | `adbf53fa04a1590ea4c3d1a1f33ebaf68a96a91d` |
| `futon2/holes/labs/wm-contract/runs/F9-cascade-decision/05-step-017-rank1-mode.edn` | 22 | yes | `adbf53fa04a1590ea4c3d1a1f33ebaf68a96a91d` |
| `futon2/holes/labs/wm-contract/runs/RE2-run-era-ledger/RE2-RUN-ERA-LEDGER.txt` | 32 | no | `4c62c776e0a9ed8474b907e9f41c951a2029ccd3` |
| `futon2/holes/labs/wm-contract/runs/RE2-run-era-ledger/RE2-SELF-TEST.txt` | 36 | no | `4c62c776e0a9ed8474b907e9f41c951a2029ccd3` |
| `futon2/holes/labs/wm-contract/runs/RE2-run-era-ledger/run-era-report.edn` | 211 | no | `4c62c776e0a9ed8474b907e9f41c951a2029ccd3` |
| `futon2/holes/labs/wm-contract/runs/RE2-run-era-ledger/run-era-self-test.edn` | 246 | no | `4c62c776e0a9ed8474b907e9f41c951a2029ccd3` |
| `futon2/holes/labs/wm-contract/runs/U35-lean-state/lean-state-report.edn` | 3969 | no | `993d0aa9147fad0f756ace84c66fd60d2332ffd1` |
| `futon2/holes/labs/wm-contract/symbol-concordance.edn` | 163 | yes | `c49f07eda382787cc66c2d1f282d94ff88772964` |
| `futon2/holes/labs/wm-contract/symbol_concordance_check.bb` | 203 | yes | `bd206447f0b1ea9c8879b610ec7636c72c851f71` |
| `futon2/holes/labs/wm-contract/wm-build-loop.sh` | 149 | no | `59d8a21b156e41b710b5890333984cedaeced189` |
| `futon2/holes/labs/wm-contract/wm-inbox-drain.sh` | 39 | yes | `59d8a21b156e41b710b5890333984cedaeced189` |
| `futon2/holes/labs/wm-contract/workflow-report.edn` | 235 | no | `bd4b4bf64597d0b0daf8726178aa0533f4d5e337` |
| `futon2/holes/labs/wm-contract/worklist-prompt.md` | 43 | no | `59d8a21b156e41b710b5890333984cedaeced189` |
| `futon2/holes/labs/wm-contract/worklist.edn` | 1724 | no | `1f389b41e6852b708e20caae0fa840c7e37132d0` |
| `futon2/scripts/futon2/report/cascade_lane.clj` | 588 | no | `adbf53fa04a1590ea4c3d1a1f33ebaf68a96a91d` |
| `futon2/scripts/futon2/report/war_machine.clj` | 7378 | no | `adbf53fa04a1590ea4c3d1a1f33ebaf68a96a91d` |
| `futon2/scripts/futon2/run_tick_once.clj` | 353 | no | `adbf53fa04a1590ea4c3d1a1f33ebaf68a96a91d` |
| `futon2/scripts/generate_variable_situation_accounting.bb` | 1218 | no | `54a42aef1a337a077f0ce701637b559182544311` |
| `futon2/src/futon2/aif/machine_q.clj` | 471 | no | `43a1b3dc51a3e3cf041d6e6df427396b2d4ec021` |
| `futon2/test/futon2/aif/machine_q_test.clj` | 255 | no | `43a1b3dc51a3e3cf041d6e6df427396b2d4ec021` |
| `futon2/test/futon2/report/cascade_lane_decision_target_test.clj` | 103 | yes | `adbf53fa04a1590ea4c3d1a1f33ebaf68a96a91d` |
| `futon3/library/agency/bounded-lifecycle.flexiarg` | 53 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/agency/delivery-receipt.flexiarg` | 54 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/agency/identifier-separation.flexiarg` | 47 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/agency/invariants.flexiarg` | 86 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/agency/loud-failure.flexiarg` | 56 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/agency/self-attribution.flexiarg` | 46 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/agency/single-routing-authority.flexiarg` | 53 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/agency/state-atomicity.flexiarg` | 52 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/agent/budget-bounds-exploration.flexiarg` | 32 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/agent/commitment-varies-with-confidence.flexiarg` | 32 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/agent/coordination-has-cost.flexiarg` | 30 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/agent/environment-over-optimization.flexiarg` | 33 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/agent/escalation-cost-vs-risk.flexiarg` | 30 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/agent/evidence-over-assertion.flexiarg` | 32 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/agent/handoff-preserves-context.flexiarg` | 32 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/agent/hypothetical-proof-architecture.flexiarg` | 72 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/agent/intent-handshake-is-binding.flexiarg` | 32 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/agent/pause-is-not-failure.flexiarg` | 30 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/agent/provisional-claims-ledger.flexiarg` | 30 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/agent/reduction-to-kernel.flexiarg` | 78 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/agent/scope-before-action.flexiarg` | 32 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/agent/sense-deliberate-act.flexiarg` | 32 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/agent/state-is-hypothesis.flexiarg` | 32 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/agent/student-dispatch.flexiarg` | 87 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/agent/trail-enables-return.flexiarg` | 32 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/aif/belief-state-operational-hypotheses.flexiarg` | 35 | no | `c0b001cb8308804721dad51c01cc2f7a6e665cc3` |
| `futon3/library/aif/candidate-pattern-action-space.flexiarg` | 36 | no | `c0b001cb8308804721dad51c01cc2f7a6e665cc3` |
| `futon3/library/aif/evidence-precision-registry.flexiarg` | 34 | no | `c0b001cb8308804721dad51c01cc2f7a6e665cc3` |
| `futon3/library/aif/expected-free-energy-scorecard.flexiarg` | 40 | no | `c0b001cb8308804721dad51c01cc2f7a6e665cc3` |
| `futon3/library/aif/free-energy-as-tick-scalar.flexiarg` | 37 | no | `c0b001cb8308804721dad51c01cc2f7a6e665cc3` |
| `futon3/library/aif/grounded-actuation-not-reobservation.flexiarg` | 37 | no | `c0b001cb8308804721dad51c01cc2f7a6e665cc3` |
| `futon3/library/aif/hierarchical-and-temporal-depth.flexiarg` | 53 | no | `c0b001cb8308804721dad51c01cc2f7a6e665cc3` |
| `futon3/library/aif/hierarchical-budget-aware-action-selection.flexiarg` | 41 | no | `c0b001cb8308804721dad51c01cc2f7a6e665cc3` |
| `futon3/library/aif/interoceptive-tripwires.flexiarg` | 62 | no | `c0b001cb8308804721dad51c01cc2f7a6e665cc3` |
| `futon3/library/aif/no-self-certification.flexiarg` | 61 | no | `c0b001cb8308804721dad51c01cc2f7a6e665cc3` |
| `futon3/library/aif/policy-precision-commitment-temperature.flexiarg` | 37 | no | `c0b001cb8308804721dad51c01cc2f7a6e665cc3` |
| `futon3/library/aif/predictive-coding-belief-update.flexiarg` | 39 | no | `c0b001cb8308804721dad51c01cc2f7a6e665cc3` |
| `futon3/library/aif/scheduled-observer-entrypoint.flexiarg` | 35 | no | `c0b001cb8308804721dad51c01cc2f7a6e665cc3` |
| `futon3/library/aif/shared-kernel-predictive-forward-model.flexiarg` | 41 | no | `c0b001cb8308804721dad51c01cc2f7a6e665cc3` |
| `futon3/library/aif/structure-learning-by-model-reduction.flexiarg` | 38 | no | `c0b001cb8308804721dad51c01cc2f7a6e665cc3` |
| `futon3/library/aif/structured-observation-vector.flexiarg` | 35 | no | `c0b001cb8308804721dad51c01cc2f7a6e665cc3` |
| `futon3/library/aif/temporal-depth-beyond-greedy.flexiarg` | 35 | no | `c0b001cb8308804721dad51c01cc2f7a6e665cc3` |
| `futon3/library/aif/two-layer-calibration.flexiarg` | 48 | no | `c0b001cb8308804721dad51c01cc2f7a6e665cc3` |
| `futon3/library/baldwin/ARGUMENT.flexiarg` | 125 | no | `647bccf426117a5ffb8a1efef7a64d83b4e234ea` |
| `futon3/library/baldwin/ablation-axes-must-not-disable-the-instrument.flexiarg` | 68 | no | `647bccf426117a5ffb8a1efef7a64d83b4e234ea` |
| `futon3/library/baldwin/assimilable-traits-need-heritable-shadows.flexiarg` | 68 | no | `647bccf426117a5ffb8a1efef7a64d83b4e234ea` |
| `futon3/library/baldwin/assimilation-requires-a-resolved-path.flexiarg` | 55 | no | `647bccf426117a5ffb8a1efef7a64d83b4e234ea` |
| `futon3/library/baldwin/charge-for-realized-work-not-for-capacity.flexiarg` | 75 | no | `647bccf426117a5ffb8a1efef7a64d83b4e234ea` |
| `futon3/library/baldwin/choose-the-heritable-unit-where-invariance-lives.flexiarg` | 70 | no | `647bccf426117a5ffb8a1efef7a64d83b4e234ea` |
| `futon3/library/baldwin/metaca-negative-evidence-localizes-the-bottleneck.flexiarg` | 95 | no | `647bccf426117a5ffb8a1efef7a64d83b4e234ea` |
| `futon3/library/baldwin/plasticity-builds-a-selective-neighbourhood.flexiarg` | 65 | no | `647bccf426117a5ffb8a1efef7a64d83b4e234ea` |
| `futon3/library/baldwin/stationarity-decides-what-can-be-fixed.flexiarg` | 80 | no | `647bccf426117a5ffb8a1efef7a64d83b4e234ea` |
| `futon3/library/baldwin/test-evolution-of-learnability-before-static-assimilation.flexiarg` | 79 | no | `647bccf426117a5ffb8a1efef7a64d83b4e234ea` |
| `futon3/library/baldwin/two-claims-not-one.flexiarg` | 55 | no | `647bccf426117a5ffb8a1efef7a64d83b4e234ea` |
| `futon3/library/campaign-coherence/campaign-as-temporary-institution.flexiarg` | 33 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/campaign-coherence/cross-mission-escrow.flexiarg` | 33 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/campaign-coherence/shared-standard-has-no-single-owner.flexiarg` | 33 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/career-coherence/free-solo-vs-rope.flexiarg` | 76 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/cascades/declared-skeleton.flexiarg` | 44 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/cascades/edges-earn-permanence.flexiarg` | 45 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/cascades/on-the-fly-cascade.flexiarg` | 44 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/cascades/repointable-declarations.flexiarg` | 41 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/cascades/the-slice-is-the-unit-of-use.flexiarg` | 41 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/code-coherence/dead-code-hygiene.flexiarg` | 38 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/code-coherence/subsumption-claim-discipline.flexiarg` | 54 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/collaboration-coherence/determined-fork-proto-psr.flexiarg` | 44 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/collaboration-coherence/isolation.flexiarg` | 39 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/collaboration-coherence/magical-thinking.flexiarg` | 39 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/collaboration-coherence/messy-with-lurkers.flexiarg` | 39 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/collaboration-coherence/misunderstanding-power-laws.flexiarg` | 40 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/collaboration-coherence/navel-gazing.flexiarg` | 39 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/collaboration-coherence/stasis.flexiarg` | 39 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/collaboration-coherence/weak-tie-conflation.flexiarg` | 39 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/coordination/pattern-search-protocol.flexiarg` | 25 | no | `50bd5309e8a64d0240eff6cf4d13aaf5a11a9611` |
| `futon3/library/cycle-machine/runtime-restoration.flexiarg` | 47 | no | `50bd5309e8a64d0240eff6cf4d13aaf5a11a9611` |
| `futon3/library/cycle-machine/step-machine.flexiarg` | 40 | no | `50bd5309e8a64d0240eff6cf4d13aaf5a11a9611` |
| `futon3/library/data-mining/gates-as-code.flexiarg` | 39 | no | `50bd5309e8a64d0240eff6cf4d13aaf5a11a9611` |
| `futon3/library/data-mining/saturate-the-accelerator-with-concurrency.flexiarg` | 39 | no | `50bd5309e8a64d0240eff6cf4d13aaf5a11a9611` |
| `futon3/library/devmap-coherence/baseline-freeze.flexiarg` | 32 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/devmap-coherence/devmap-scope-discipline.flexiarg` | 42 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/devmap-coherence/ifr-f0-sati.flexiarg` | 30 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/devmap-coherence/ifr-f1-dhammavicaya.flexiarg` | 32 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/devmap-coherence/ifr-f2-viriya.flexiarg` | 32 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/devmap-coherence/ifr-f3-piti.flexiarg` | 32 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/devmap-coherence/ifr-f3a-piti-audit.flexiarg` | 32 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/devmap-coherence/ifr-f4-passaddhi.flexiarg` | 32 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/devmap-coherence/ifr-f5-samadhi.flexiarg` | 32 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/devmap-coherence/ifr-f6-upekkha.flexiarg` | 32 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/devmap-coherence/ifr-f7-upa-upekkha.flexiarg` | 32 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/devmap-coherence/ifr-state-convergence.flexiarg` | 40 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/devmap-coherence/next-steps-to-done.flexiarg` | 31 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/devmap-coherence/prototype-alignment-bridge.flexiarg` | 32 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/devmap-coherence/prototype-alignment-embedding.flexiarg` | 32 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/devmap-coherence/prototype-alignment-role.flexiarg` | 32 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/devmap-coherence/prototype-alignment-tension.flexiarg` | 32 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/devmap-coherence/prototype-maturity-lifecycle.flexiarg` | 49 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/devmap-coherence/prototype-structure-checklist.flexiarg` | 67 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/devmap-coherence/status-keywords.flexiarg` | 55 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/eight-gates/an-push.flexiarg` | 30 | no | `647bccf426117a5ffb8a1efef7a64d83b4e234ea` |
| `futon3/library/eight-gates/cai-pluck.flexiarg` | 30 | no | `647bccf426117a5ffb8a1efef7a64d83b4e234ea` |
| `futon3/library/eight-gates/elbow-immediate.flexiarg` | 45 | no | `647bccf426117a5ffb8a1efef7a64d83b4e234ea` |
| `futon3/library/eight-gates/ji-press.flexiarg` | 30 | no | `647bccf426117a5ffb8a1efef7a64d83b4e234ea` |
| `futon3/library/eight-gates/kao-lean.flexiarg` | 30 | no | `647bccf426117a5ffb8a1efef7a64d83b4e234ea` |
| `futon3/library/eight-gates/lean-commit.flexiarg` | 55 | no | `647bccf426117a5ffb8a1efef7a64d83b4e234ea` |
| `futon3/library/eight-gates/lie-split.flexiarg` | 30 | no | `647bccf426117a5ffb8a1efef7a64d83b4e234ea` |
| `futon3/library/eight-gates/lu-roll-back.flexiarg` | 30 | no | `647bccf426117a5ffb8a1efef7a64d83b4e234ea` |
| `futon3/library/eight-gates/peng-ward-off.flexiarg` | 30 | no | `647bccf426117a5ffb8a1efef7a64d83b4e234ea` |
| `futon3/library/eight-gates/pluck-extract.flexiarg` | 39 | no | `647bccf426117a5ffb8a1efef7a64d83b4e234ea` |
| `futon3/library/eight-gates/press-mechanism.flexiarg` | 45 | no | `647bccf426117a5ffb8a1efef7a64d83b4e234ea` |
| `futon3/library/eight-gates/push-warrant.flexiarg` | 56 | no | `647bccf426117a5ffb8a1efef7a64d83b4e234ea` |
| `futon3/library/eight-gates/roll-back-hold.flexiarg` | 40 | no | `647bccf426117a5ffb8a1efef7a64d83b4e234ea` |
| `futon3/library/eight-gates/split-isolate.flexiarg` | 39 | no | `647bccf426117a5ffb8a1efef7a64d83b4e234ea` |
| `futon3/library/eight-gates/ward-off-boundary.flexiarg` | 40 | no | `647bccf426117a5ffb8a1efef7a64d83b4e234ea` |
| `futon3/library/eight-gates/zhou-elbow.flexiarg` | 30 | no | `647bccf426117a5ffb8a1efef7a64d83b4e234ea` |
| `futon3/library/futon-theory/agent-contract.flexiarg` | 76 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/futon-theory/all-or-nothing.flexiarg` | 57 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/futon-theory/baldwin-cycle.flexiarg` | 68 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/futon-theory/coordination-protocol.flexiarg` | 90 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/futon-theory/counter-ratchet.flexiarg` | 65 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/futon-theory/crime-relocates-to-a-scarcer-witness.flexiarg` | 43 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/futon-theory/curry-howard-operational.flexiarg` | 59 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/futon-theory/derive-exits-on-a-minted-sorry.flexiarg` | 56 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/futon-theory/durability-first.flexiarg` | 52 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/futon-theory/error-hierarchy.flexiarg` | 65 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/futon-theory/event-protocol.flexiarg` | 79 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/futon-theory/four-types.flexiarg` | 73 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/futon-theory/futonic-logic.flexiarg` | 627 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/futon-theory/honest-map-over-flattering-counter.flexiarg` | 38 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/futon-theory/interface-loop.flexiarg` | 86 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/futon-theory/local-gain-persistence.flexiarg` | 67 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/futon-theory/minimum-viable-events.flexiarg` | 63 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/futon-theory/mission-dependency.flexiarg` | 85 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/futon-theory/mission-interface-signature.flexiarg` | 234 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/futon-theory/mission-lifecycle.flexiarg` | 72 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/futon-theory/mission-scoping.flexiarg` | 78 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/futon-theory/progress-signal.flexiarg` | 94 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/futon-theory/proof-path.flexiarg` | 61 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/futon-theory/rapid-debugging.flexiarg` | 64 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/futon-theory/retroactive-canonicalization.flexiarg` | 152 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/futon-theory/retrospective-stability.flexiarg` | 49 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/futon-theory/reverse-morphogenesis.flexiarg` | 269 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/futon-theory/single-source-of-truth.flexiarg` | 53 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/futon-theory/stop-the-line.flexiarg` | 67 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/futon-theory/structural-tension-as-observation.flexiarg` | 199 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/futon-theory/symbolic-geodesic.flexiarg` | 50 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/futon-theory/symbolic-individuation.flexiarg` | 153 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/futon-theory/task-as-arrow.flexiarg` | 124 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/futon-theory/the-woven-form.flexiarg` | 34 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/futon-theory/theory-as-exotype.flexiarg` | 99 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/futon-theory/wyrd.flexiarg` | 197 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/futon-theory/xenotype-portability.flexiarg` | 74 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/iiching/README.md` | 24 | no | `9a6e3d5a295db230931f49784356dcc9041a9168` |
| `futon3/library/iiching/TEMPLATE.flexiarg` | not present/readable | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/TEMPLATE.flexiarg.txt` | 72 | yes | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-000.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-001.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-002.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-003.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-004.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-005.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-006.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-007.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-008.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-009.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-010.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-011.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-012.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-013.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-014.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-015.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-016.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-017.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-018.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-019.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-020.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-021.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-022.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-023.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-024.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-025.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-026.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-027.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-028.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-029.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-030.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-031.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-032.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-033.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-034.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-035.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-036.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-037.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-038.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-039.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-040.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-041.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-042.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-043.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-044.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-045.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-046.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-047.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-048.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-049.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-050.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-051.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-052.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-053.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-054.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-055.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-056.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-057.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-058.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-059.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-060.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-061.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-062.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-063.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-064.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-065.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-066.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-067.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-068.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-069.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-070.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-071.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-072.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-073.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-074.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-075.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-076.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-077.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-078.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-079.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-080.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-081.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-082.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-083.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-084.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-085.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-086.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-087.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-088.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-089.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-090.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-091.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-092.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-093.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-094.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-095.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-096.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-097.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-098.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-099.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-100.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-101.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-102.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-103.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-104.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-105.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-106.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-107.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-108.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-109.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-110.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-111.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-112.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-113.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-114.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-115.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-116.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-117.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-118.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-119.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-120.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-121.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-122.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-123.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-124.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-125.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-126.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-127.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-128.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-129.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-130.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-131.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-132.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-133.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-134.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-135.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-136.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-137.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-138.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-139.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-140.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-141.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-142.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-143.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-144.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-145.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-146.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-147.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-148.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-149.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-150.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-151.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-152.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-153.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-154.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-155.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-156.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-157.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-158.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-159.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-160.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-161.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-162.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-163.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-164.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-165.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-166.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-167.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-168.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-169.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-170.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-171.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-172.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-173.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-174.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-175.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-176.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-177.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-178.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-179.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-180.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-181.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-182.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-183.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-184.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-185.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-186.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-187.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-188.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-189.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-190.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-191.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-192.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-193.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-194.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-195.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-196.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-197.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-198.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-199.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-200.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-201.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-202.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-203.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-204.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-205.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-206.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-207.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-208.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-209.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-210.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-211.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-212.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-213.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-214.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-215.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-216.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-217.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-218.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-219.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-220.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-221.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-222.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-223.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-224.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-225.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-226.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-227.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-228.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-229.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-230.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-231.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-232.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-233.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-234.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-235.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-236.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-237.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-238.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-239.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-240.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-241.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-242.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-243.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-244.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-245.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-246.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-247.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-248.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-249.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-250.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-251.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-252.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-253.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-254.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/iiching/exotype-255.flexiarg` | 68 | no | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/math-formalization-CA/ae-integral-zero.flexiarg` | 26 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization-CA/complex-arg-of-cpow-root.flexiarg` | 33 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization-CA/continuous-linear-map-composition.flexiarg` | 26 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization-CA/divisor-sum-to-root-count-without-monic.flexiarg` | 52 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization-CA/fourier-inversion-for-real-oscillatory-integrals.flexiarg` | 68 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization-CA/hilbert-projection-properties.flexiarg` | 26 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization-CA/laurent-clearing-to-algebraic-polynomial.flexiarg` | 40 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization-CA/layer-cake-crossover-split.flexiarg` | 38 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization-CA/lp-norm-comparison.flexiarg` | 26 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization-CA/measure-integration-api.flexiarg` | 97 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization-CA/measure-restrict-simplify.flexiarg` | 26 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization-CA/metric-cauchy-convergence.flexiarg` | 26 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization-CA/ode-gronwall-api.flexiarg` | 55 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization-CA/pointwise-logderiv-bridge-interval-to-contour.flexiarg` | 43 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization-CA/radialize-via-gauge-rescale.flexiarg` | 31 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization-CA/riemann-darboux-api.flexiarg` | 66 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization-CA/rpow-exponent-limit.flexiarg` | 26 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization-CA/schwarz-disk-automorphism-formula.flexiarg` | 39 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization-CA/separation-function-from-distance.flexiarg` | 26 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization-CA/series-evaluation-api.flexiarg` | 76 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization-CA/surj-via-oriented-root-preimage.flexiarg` | 35 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization-CA/transfer-derivatives-via-eventuallyeq.flexiarg` | 30 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization-CA/uniform-continuity-boundedness.flexiarg` | 51 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization/additive-principal-parts-from-order-germs.flexiarg` | 50 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization/assemble-a-basis-from-an-independent-supremum.flexiarg` | 34 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization/bank-cheap-obligations-before-the-hard-bridge.flexiarg` | 44 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization/binder-expression-mismatch.flexiarg` | 55 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization/bound-sylow-order-from-faithful-prime-action.flexiarg` | 40 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization/cast-normalization.flexiarg` | 50 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization/classify-square-zero-actions-by-adapted-complements.flexiarg` | 24 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization/close-bijectivity-by-counting-not-inverting.flexiarg` | 43 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization/coercion-bridge.flexiarg` | 71 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization/collapse-self-normalizing-sylow-via-faithful-prime-action.flexiarg` | 46 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization/compact-thickening-upgrades-pointwise-analyticity.flexiarg` | 45 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization/construction-cost-asymmetry.flexiarg` | 78 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization/field-simp-reciprocal.flexiarg` | 26 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization/fixed-field-equality-via-degree-and-automorphism-count.flexiarg` | 42 | yes | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization/lift-prove-upstairs-reflect-by-injectivity.flexiarg` | 49 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization/memlp-power-law-scalar-thresholds.flexiarg` | 57 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization/minmax-normalization.flexiarg` | 51 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization/normalize-meromorphic-point-values-before-continuation.flexiarg` | 46 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization/notation-semantics-traps.flexiarg` | 64 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization/pointwise-hassum-to-taylor-coefficients-via-fps-uniqueness.flexiarg` | 52 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization/probe-constant-name-and-signature.flexiarg` | 55 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization/recover-radical-generators-from-a-reciprocal-polynomial-root.flexiarg` | 36 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization/replace-orbit-combinatorics-by-a-cyclic-module-quotient.flexiarg` | 47 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization/rewrite-orientation.flexiarg` | 50 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization/separate-proof-transfer-from-artifact-replay.flexiarg` | 43 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization/stamp-siblings-from-one-compiled-branch.flexiarg` | 30 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization/statement-ladder-before-proof-text.flexiarg` | 41 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization/tactic-algebra-interference.flexiarg` | 73 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization/transport-across-an-instance-diamond.flexiarg` | 55 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-formalization/weld-range-lemmas-at-representation-seams.flexiarg` | 29 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-informal/argue-by-contradiction.flexiarg` | 37 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-informal/check-the-extreme-cases.flexiarg` | 36 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-informal/construct-an-explicit-witness.flexiarg` | 37 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-informal/construct-auxiliary-object.flexiarg` | 41 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-informal/convert-growth-counts-to-summability-by-geometric-shells.flexiarg` | 45 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-informal/dualise-the-problem.flexiarg` | 37 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-informal/exploit-symmetry.flexiarg` | 36 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-informal/failure-mode-characterization.flexiarg` | 59 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-informal/find-the-right-abstraction.flexiarg` | 39 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-informal/induction-and-well-ordering.flexiarg` | 38 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-informal/local-to-global.flexiarg` | 38 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-informal/parametric-tension-dissolution.flexiarg` | 68 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-informal/quotient-by-irrelevance.flexiarg` | 37 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-informal/reduce-to-known-result.flexiarg` | 37 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-informal/separate-into-independent-pieces.flexiarg` | 45 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-informal/split-into-cases.flexiarg` | 39 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-informal/structural-characterization.flexiarg` | 57 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-informal/structural-equivalence.flexiarg` | 60 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-informal/structural-inclusion.flexiarg` | 55 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-informal/the-diagonal-argument.flexiarg` | 39 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-informal/transport-across-isomorphism.flexiarg` | 39 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-informal/try-a-simpler-case.flexiarg` | 36 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-informal/unfold-the-definition.flexiarg` | 37 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-informal/work-examples-first.flexiarg` | 36 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-strategy/characterization-result.flexiarg` | 68 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-strategy/compose-independent-lemmas.flexiarg` | 51 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-strategy/constraint-tension-resolution.flexiarg` | 128 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-strategy/construction-before-estimates.flexiarg` | 62 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-strategy/convention-bridge.flexiarg` | 50 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-strategy/corpus-trust-protocol.flexiarg` | 88 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-strategy/exhaustion-as-theorem.flexiarg` | 65 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-strategy/existence-result.flexiarg` | 65 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-strategy/hypothesis-category-check.flexiarg` | 53 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-strategy/isolate-computational-kernel-before-transport.flexiarg` | 33 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-strategy/missing-dependency-protocol.flexiarg` | 95 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-strategy/non-circularity-check.flexiarg` | 54 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-strategy/plan-first-attempt.flexiarg` | 31 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-strategy/preemptive-objection-clearance.flexiarg` | 132 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-strategy/proof-architecture.flexiarg` | 68 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-strategy/property-of-object-result.flexiarg` | 68 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-strategy/route-exploration-and-pivot.flexiarg` | 110 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-strategy/structural-obstruction-as-theorem.flexiarg` | 65 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-strategy/structural-relation-result.flexiarg` | 66 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/math-strategy/technique-landscape-map.flexiarg` | 65 | no | `2a91028b995fc9d8217313846054c94fbaca06ee` |
| `futon3/library/musn/aif-live-scores.flexiarg` | 28 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/musn/declare-scope.flexiarg` | 28 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/musn/expensive-move-consent.flexiarg` | 29 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/musn/fulab-report-block.flexiarg` | 28 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/musn/intent-restatement.flexiarg` | 28 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/musn/off-trail-brief-log.flexiarg` | 29 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/musn/off-trail-budget.flexiarg` | 29 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/musn/pattern-action-helper-command.flexiarg` | 28 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/musn/pattern-action-justification.flexiarg` | 28 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/musn/pattern-action-rpc.flexiarg` | 28 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/musn/pause-backtrace.flexiarg` | 29 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/musn/plan-before-tool.flexiarg` | 28 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/musn/psr-pur-real-actions.flexiarg` | 28 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/musn/read-aif-plus-one.flexiarg` | 28 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/musn/selection-before-write.flexiarg` | 28 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/musn/tool-name-hygiene.flexiarg` | 28 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/musn/use-requires-evidence.flexiarg` | 28 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/or3/ask-first-then-bring-the-expert.flexiarg` | 31 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/or3/authorship-for-first-time-contributors.flexiarg` | 30 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/or3/borrow-a-training-network.flexiarg` | 30 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/or3/co-design-is-not-consultation.flexiarg` | 30 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/or3/co-design-the-call.flexiarg` | 31 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/or3/consent-bounded-analysis.flexiarg` | 30 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/or3/coordinator-in-your-school.flexiarg` | 31 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/or3/count-every-card-back.flexiarg` | 30 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/or3/design-around-the-red-line.flexiarg` | 30 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/or3/openness-without-exposure.flexiarg` | 30 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/or3/plural-value-accounting.flexiarg` | 30 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/or3/reproducibility-as-a-teaching-habit.flexiarg` | 31 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/or3/scenarios-from-lived-experience.flexiarg` | 30 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/or3/skills-that-travel-onward.flexiarg` | 30 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/or3/student-project-as-collaboration.flexiarg` | 30 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/or3/sustainability-without-enclosure.flexiarg` | 30 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/or3/teach-the-process-not-the-facility.flexiarg` | 30 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/peeragogy/calling-in-not-out.flexiarg` | 39 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/peeragogy/carrying-capacity.flexiarg` | 38 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/peeragogy/check-in-rhythm.flexiarg` | 39 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/peeragogy/creating-a-guide.flexiarg` | 37 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/peeragogy/curating-not-experting.flexiarg` | 39 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/peeragogy/discerning-a-pattern.flexiarg` | 37 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/peeragogy/heartbeat.flexiarg` | 37 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/peeragogy/moderation.flexiarg` | 38 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/peeragogy/newcomer.flexiarg` | 38 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/peeragogy/par.flexiarg` | 37 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/peeragogy/pattern-language.flexiarg` | 37 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/peeragogy/polling-for-ideas.flexiarg` | 37 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/peeragogy/reciprocal-participation.flexiarg` | 41 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/peeragogy/roadmap.flexiarg` | 38 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/peeragogy/roles.flexiarg` | 38 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/peeragogy/stewardship-succession.flexiarg` | 41 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/peeragogy/use-or-make.flexiarg` | 38 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/peeragogy/wrapper.flexiarg` | 37 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/plos-npt-with-small-n/absence-as-evidence.flexiarg` | 42 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/plos-npt-with-small-n/abstract-n-method-frame.flexiarg` | 43 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/plos-npt-with-small-n/barrier-enabler-strategy-display.flexiarg` | 43 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/plos-npt-with-small-n/coding-reliability-stated.flexiarg` | 39 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/plos-npt-with-small-n/coherence-first-when-using-npt.flexiarg` | 43 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/plos-npt-with-small-n/coreq-checklist-adherence.flexiarg` | 39 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/plos-npt-with-small-n/data-availability-and-prereg.flexiarg` | 39 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/plos-npt-with-small-n/disconfirmation-not-just-triangulation.flexiarg` | 42 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/plos-npt-with-small-n/framework-spine-with-empirical-ribs.flexiarg` | 43 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/plos-npt-with-small-n/inductive-before-deductive-warrant.flexiarg` | 42 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/plos-npt-with-small-n/multi-method-triangulation-ladder.flexiarg` | 43 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/plos-npt-with-small-n/open-science-phase-register.flexiarg` | 43 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/plos-npt-with-small-n/paradigm-matched-rigour-frame.flexiarg` | 42 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/plos-npt-with-small-n/quote-table-not-quote-flood.flexiarg` | 43 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/plos-npt-with-small-n/research-questions-stated-early.flexiarg` | 42 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/plos-npt-with-small-n/scope-shield-via-companion-paper.flexiarg` | 43 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/plos-npt-with-small-n/site-case-data-display.flexiarg` | 43 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/plos-npt-with-small-n/small-n-is-a-design-feature.flexiarg` | 43 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/plos-npt-with-small-n/theory-as-heuristic-not-law.flexiarg` | 42 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/plos-npt-with-small-n/training-role-feature-register.flexiarg` | 43 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/plos-npt-with-small-n/transferability-not-generalisability.flexiarg` | 43 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/plos-npt-with-small-n/venue-register-boundary.flexiarg` | 44 | no | `5704359975a41ff125ffa94f7ce227dcf5d53217` |
| `futon3/library/problems/baldwin-causal-claims-vs-engineering-metaphor.flexiarg` | 26 | yes | `647bccf426117a5ffb8a1efef7a64d83b4e234ea` |
| `futon3/library/problems/catalogue-r5-efe-label-auditability.flexiarg` | 27 | yes | `835278bc4bbd9fd2396b112c3f614a7d43246293` |
| `futon3/library/problems/coordination-patterns-derivation.flexiarg` | 26 | yes | `50bd5309e8a64d0240eff6cf4d13aaf5a11a9611` |
| `futon3/library/problems/cycle-model-boundary-gaps.flexiarg` | 26 | yes | `50bd5309e8a64d0240eff6cf4d13aaf5a11a9611` |
| `futon3/library/problems/devmap-self-consistency-standards.flexiarg` | 26 | yes | `647bccf426117a5ffb8a1efef7a64d83b4e234ea` |
| `futon3/library/problems/exotype-encoding-programme.flexiarg` | 25 | yes | `a43f0280d046d2c9e38296d2753455f2d435e1d8` |
| `futon3/library/problems/llm-over-corpus-mining-lessons.flexiarg` | 26 | yes | `50bd5309e8a64d0240eff6cf4d13aaf5a11a9611` |
| `futon3/library/problems/pattern-coding-and-novel-force-mining.flexiarg` | 26 | yes | `50bd5309e8a64d0240eff6cf4d13aaf5a11a9611` |
| `futon3/library/problems/pattern-genesis-from-evidence-bearing-holes.flexiarg` | 26 | yes | `835278bc4bbd9fd2396b112c3f614a7d43246293` |
| `futon3/library/problems/snatch-play-theory-gaps.flexiarg` | 26 | yes | `89e686d36c23de0f4bd1366da7f753d53c076b17` |
| `futon3/library/problems/tensions-are-navigated-not-eliminated.flexiarg` | 26 | yes | `647bccf426117a5ffb8a1efef7a64d83b4e234ea` |
| `futon3/library/problems/theory-general-enough-to-drive-the-stack.flexiarg` | 26 | yes | `50bd5309e8a64d0240eff6cf4d13aaf5a11a9611` |
| `futon3/library/problems/transferable-open-research-practice.flexiarg` | 26 | yes | `647bccf426117a5ffb8a1efef7a64d83b4e234ea` |
| `futon3/library/relationship-coherence/absence-as-spaciousness.flexiarg` | 31 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/relationship-coherence/affirmation-reframe.flexiarg` | 30 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/relationship-coherence/base-camp.flexiarg` | 30 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/relationship-coherence/body-attunement.flexiarg` | 30 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/relationship-coherence/capacity-honoring.flexiarg` | 31 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/relationship-coherence/co-created-project.flexiarg` | 31 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/relationship-coherence/co-regulation.flexiarg` | 30 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/relationship-coherence/cook-as-care.flexiarg` | 30 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/relationship-coherence/held-uncertainty.flexiarg` | 31 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/relationship-coherence/lexicon-minting.flexiarg` | 30 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/relationship-coherence/low-pressure-bid.flexiarg` | 30 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/relationship-coherence/meeting-the-people.flexiarg` | 30 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/relationship-coherence/mutual-coaching.flexiarg` | 30 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/relationship-coherence/narrative-suspension.flexiarg` | 31 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/relationship-coherence/parallel-lives-mirroring.flexiarg` | 30 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/relationship-coherence/quote-as-oblique-address.flexiarg` | 31 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/relationship-coherence/resonance-weaving.flexiarg` | 31 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/relationship-coherence/running-gag.flexiarg` | 30 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/relationship-coherence/rupture-repair.flexiarg` | 30 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/snatch/a-free-mark-is-always-worth-assigning.flexiarg` | 29 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/snatch/accept-an-offer-that-beats-holding.flexiarg` | 31 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/snatch/an-unmodelled-response-stops-the-line.flexiarg` | 38 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/snatch/ask-for-surplus-not-surrender.flexiarg` | 29 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/snatch/consult-the-remedy-before-exiting.flexiarg` | 34 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/snatch/escalate-only-as-far-as-you-can-lose.flexiarg` | 31 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/snatch/exchange-when-both-sides-gain.flexiarg` | 31 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/snatch/forced-play-needs-a-loss-floor.flexiarg` | 29 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/snatch/grim-cuts-the-cascade-and-never-widens-it.flexiarg` | 54 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/snatch/have-a-temperament.flexiarg` | 63 | no | `d26ae2a0c1281045572dd71de55bda14798e811b` |
| `futon3/library/snatch/institutions-vary-by-position-and-force.flexiarg` | 42 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/snatch/lead-with-the-exchange-rule.flexiarg` | 44 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/snatch/mark-without-force.flexiarg` | 40 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/snatch/non-binding-talk-still-moves-play.flexiarg` | 37 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/snatch/play-the-authored-order-first.flexiarg` | 42 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/snatch/preserve-the-right-to-abstain.flexiarg` | 38 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/snatch/price-the-final-round-as-final.flexiarg` | 29 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/snatch/probe-before-committing.flexiarg` | 34 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/snatch/promote-the-remedy-before-the-exit.flexiarg` | 45 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/snatch/protect-the-unprotected-move.flexiarg` | 34 | no | `d26ae2a0c1281045572dd71de55bda14798e811b` |
| `futon3/library/snatch/re-enter-after-observed-repair.flexiarg` | 29 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/snatch/revert-then-invert.flexiarg` | 41 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/snatch/use-talk-to-make-a-testable-offer.flexiarg` | 28 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/snatch/widen-the-cascade-only-on-evidence.flexiarg` | 53 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/storage/all-or-nothing-startup.flexiarg` | 32 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/storage/canonical-interface.flexiarg` | 32 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/storage/determinism-vs-expansion.flexiarg` | 32 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/storage/deterministic-ingest-pipeline.flexiarg` | 32 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/storage/deterministic-substrate.flexiarg` | 32 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/storage/durability-first.flexiarg` | 35 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/storage/durability-throughput-gate.flexiarg` | 32 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/storage/error-layer-hierarchy.flexiarg` | 32 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/storage/graph-memory-contract.flexiarg` | 32 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/storage/guardrails-vs-tooling.flexiarg` | 32 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/storage/identity-flex-uniqueness.flexiarg` | 32 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/storage/identity-uniqueness.flexiarg` | 32 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/storage/invariants-vs-repair.flexiarg` | 32 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/storage/open-world-continuity.flexiarg` | 32 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/storage/open-world-velocity-validation.flexiarg` | 32 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/storage/persistence-speed-mirroring.flexiarg` | 32 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/storage/postcommit-materialization-gate.flexiarg` | 34 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/storage/rapid-debugging.flexiarg` | 32 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/storage/reproducible-mirroring.flexiarg` | 32 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/storage/schema-evolution-stability.flexiarg` | 32 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/storage/startup-integrity-gate.flexiarg` | 32 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/ukrns/anchor-case-maintenance.flexiarg` | 54 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/ukrns/assumptions-commons.flexiarg` | 60 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/ukrns/calibrate-impact-promises-to-current-indicator-capacity.flexiarg` | 24 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/ukrns/coding-provenance.flexiarg` | 66 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/ukrns/computation-as-exploration.flexiarg` | 64 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/ukrns/cycle-position-publication.flexiarg` | 62 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/ukrns/design-as-function-of-evidence.flexiarg` | 64 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/ukrns/design-state-lineage.flexiarg` | 64 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/ukrns/diagnose-before-prescribing.flexiarg` | 64 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/ukrns/distinguish-delivery-from-practice-change.flexiarg` | 24 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/ukrns/distributed-coverage.flexiarg` | 77 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/ukrns/exercise-response-capture.flexiarg` | 62 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/ukrns/join-up-training-indicators-and-sector-positioning.flexiarg` | 25 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/ukrns/measure-where-least-sure.flexiarg` | 64 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/ukrns/model-recompute-schedule.flexiarg` | 63 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/ukrns/open-the-triangle.flexiarg` | 64 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/ukrns/provision-at-network-scale.flexiarg` | 64 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/ukrns/publication-cadence.flexiarg` | 63 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/ukrns/reader-as-participant.flexiarg` | 70 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/ukrns/reader-run-path.flexiarg` | 64 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/ukrns/scale-register.flexiarg` | 63 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/ukrns/self-application.flexiarg` | 68 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/ukrns/separate-direct-indirect-and-out-of-scope-levers.flexiarg` | 24 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/ukrns/separate-evidence-gaps-from-implementation-decisions.flexiarg` | 25 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/ukrns/treat-funder-cycles-as-operational-rhythm.flexiarg` | 25 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/ukrns/worked-contrast.flexiarg` | 68 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/vsatlas/accountable-poc.flexiarg` | 33 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/vsatlas/askew-layer.flexiarg` | 32 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/vsatlas/audience-shift.flexiarg` | 30 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/vsatlas/current-strength-gap.flexiarg` | 34 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/vsatlas/economic-resonance-layer.flexiarg` | 31 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/vsatlas/festival-model.flexiarg` | 30 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/vsatlas/isolarion-drift.flexiarg` | 31 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/vsatlas/minimal-linking-pilot.flexiarg` | 35 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/vsatlas/non-destructive-relational-layers.flexiarg` | 31 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/vsatlas/offer-ladder.flexiarg` | 31 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/vsatlas/open-ecosystem-mandate.flexiarg` | 30 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/vsatlas/peeragogical-infrastructures.flexiarg` | 30 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/vsatlas/pilot-offers.flexiarg` | 32 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/vsatlas/pluriversal-infrastructure.flexiarg` | 23 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/vsatlas/proof-of-concept-scope.flexiarg` | 33 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/vsatlas/proof-through-pilots.flexiarg` | 30 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/vsatlas/reasons-to-causes.flexiarg` | 30 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/vsatlas/shared-stewardship-compact.flexiarg` | 30 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/vsatlas/stewardship-layer.flexiarg` | 30 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/vsatlas/story-emporium.flexiarg` | 31 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/vsatlas/sustainable-trajectory.flexiarg` | 30 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/vsatlas/upful-learning-trajectory.flexiarg` | 30 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/vsatlas/value-flow-constellation.flexiarg` | 30 | no | `5c0b7371e819d61143cbc322d35a33b5288a8b3e` |
| `futon3/library/war-room/wr-1-three-futon-split.flexiarg` | 26 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/war-room/wr-10-next-move-surface-is-recursive-closure.flexiarg` | 27 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/war-room/wr-11-external-applications-carry-predecessor-exemplar-relationships.flexiarg` | 26 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/war-room/wr-12-essay-health-is-the-aif-observation-channel.flexiarg` | 27 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/war-room/wr-13-futonic-debt-is-paid-in-order.flexiarg` | 26 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/war-room/wr-14-predecessor-completion-pull-is-a-fourth-next-move-criterion.flexiarg` | 27 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/war-room/wr-15-head-as-escrow-is-a-sanctioned-pattern.flexiarg` | 27 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/war-room/wr-16-operationalised-exploit-loops-are-first-class-observation-channels.flexiarg` | 36 | no | `c0b001cb8308804721dad51c01cc2f7a6e665cc3` |
| `futon3/library/war-room/wr-17-futon0-as-cyborg-futon7-as-markov-blanket.flexiarg` | 27 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/war-room/wr-18-war-machine-is-demonstrated-not-hypothesised.flexiarg` | 27 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/war-room/wr-19-tension-must-generate-not-only-rank.flexiarg` | 36 | no | `c0b001cb8308804721dad51c01cc2f7a6e665cc3` |
| `futon3/library/war-room/wr-2-pattern-library-stays-canonical.flexiarg` | 24 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/war-room/wr-20-action-class-inventory-becomes-data.flexiarg` | 27 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/war-room/wr-21-geometrization-is-a-shared-cross-mission-dependency.flexiarg` | 28 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/war-room/wr-22-logic-model-before-code-is-a-sanctioned-verify-method.flexiarg` | 27 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/war-room/wr-23-upstream-trackers-are-stack-surfaces.flexiarg` | 27 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/war-room/wr-24-a-removed-constraint-does-not-remove-the-discipline-it-supplied.flexiarg` | 37 | no | `c0b001cb8308804721dad51c01cc2f7a6e665cc3` |
| `futon3/library/war-room/wr-25-good-news-gets-the-same-evidence-discipline-as-bad.flexiarg` | 42 | no | `c0b001cb8308804721dad51c01cc2f7a6e665cc3` |
| `futon3/library/war-room/wr-26-a-capability-switched-off-carries-its-re-arm-condition-in-writing-at-the-switch.flexiarg` | 34 | no | `c0b001cb8308804721dad51c01cc2f7a6e665cc3` |
| `futon3/library/war-room/wr-27-a-loop-is-born-instrumented-for-its-gain.flexiarg` | 35 | no | `c0b001cb8308804721dad51c01cc2f7a6e665cc3` |
| `futon3/library/war-room/wr-3-social-exotype-before-implementation.flexiarg` | 26 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/war-room/wr-5-war-machine-is-not-a-mission.flexiarg` | 26 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/war-room/wr-6-daily-scan-is-depositing-heartbeat.flexiarg` | 27 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/war-room/wr-7-frames-interoperate-or-they-do-not-exist.flexiarg` | 26 | no | `e01cbbcddb89edbcfc2045ebc85d760b13a263e3` |
| `futon3/library/writing-coherence/balanced-pair-padding.flexiarg` | 36 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/writing-coherence/citation-density-load-bearing-claim.flexiarg` | 47 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/writing-coherence/cross-section-claim-drift.flexiarg` | 38 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/writing-coherence/first-mention-undischarged.flexiarg` | 37 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/writing-coherence/floating-formalism.flexiarg` | 39 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/writing-coherence/hedged-lift.flexiarg` | 36 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/writing-coherence/meet-the-reader-where-they-are.flexiarg` | 38 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/writing-coherence/meta-lede.flexiarg` | 36 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/writing-coherence/meta-title.flexiarg` | 43 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/writing-coherence/missing-mechanism.flexiarg` | 36 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/writing-coherence/name-what-you-drop.flexiarg` | 38 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/writing-coherence/one-paragraph-per-typed-claim.flexiarg` | 47 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/writing-coherence/paraphrase-drift.flexiarg` | 36 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/writing-coherence/plain-language-thesis.flexiarg` | 39 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/writing-coherence/recursion-cheque.flexiarg` | 36 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/writing-coherence/scope-mush.flexiarg` | 36 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/writing-coherence/section-bridge-missing.flexiarg` | 36 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/writing-coherence/stale-reference-after-restructure.flexiarg` | 43 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/writing-coherence/structural-style-inconsistency.flexiarg` | 38 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/writing-coherence/subheading-without-paragraph.flexiarg` | 35 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/writing-coherence/throat-clearing-close.flexiarg` | 36 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/writing-coherence/triad-inflation.flexiarg` | 36 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3/library/writing-coherence/unearned-essentially.flexiarg` | 36 | no | `fccd14e9f706e85f4ea33a1186060adeac64d659` |
| `futon3c/holes/labs/M-apm-demonstration/frame-park-decisions.edn` | 4400 | no | `5bc67c7668b72b60b12e2aaf7f5f7d06b9458a49` |
| `futon3c/src/futon3c/transport/http.clj` | 9669 | no | `80f4ebc97ed770879c642ade895c5b739cc7aaab` |
| `futon3c/test/futon3c/transport/job_timeout_test.clj` | 475 | no | `80f4ebc97ed770879c642ade895c5b739cc7aaab` |
| `mathlib4/DarkTower/WarMachine/MachineAction.lean` | 253 | yes | `c2ab8bb42b27fdb90032a8068929b06f529e7de7` |
| `mathlib4/DarkTower/WarMachine/MachineActionWitness.lean` | 47 | yes | `c2ab8bb42b27fdb90032a8068929b06f529e7de7` |
| `mathlib4/DarkTower/WarMachine/MachineBeliefState.lean` | 102 | yes | `290ce8ae9f57dfc616d5e9b1d4b573885bdba735` |
| `mathlib4/DarkTower/WarMachine/MachineBeliefStateWitness.lean` | 138 | yes | `290ce8ae9f57dfc616d5e9b1d4b573885bdba735` |
| `mathlib4/DarkTower/WarMachine/MachineBeliefUpdate.lean` | 200 | yes | `d15325c004314547f2186f998ea33464f198549b` |
| `mathlib4/DarkTower/WarMachine/MachineBeliefUpdateWitness.lean` | 99 | yes | `d15325c004314547f2186f998ea33464f198549b` |
| `mathlib4/DarkTower/WarMachine/MachineDepth.lean` | 276 | yes | `955bffc561f59fb89d9d05ae73f469dbb73c660c` |
| `mathlib4/DarkTower/WarMachine/MachineDepthWitness.lean` | 81 | yes | `955bffc561f59fb89d9d05ae73f469dbb73c660c` |
| `mathlib4/DarkTower/WarMachine/MachineDirichletAccumulation.lean` | 264 | yes | `6f5df367eca529f707461065b09e48b1237843b4` |
| `mathlib4/DarkTower/WarMachine/MachineDirichletAccumulationWitness.lean` | 66 | yes | `6f5df367eca529f707461065b09e48b1237843b4` |
| `mathlib4/DarkTower/WarMachine/MachineObservation.lean` | 155 | yes | `169662b19653d20c76274bdce4845ab414da10cf` |
| `mathlib4/DarkTower/WarMachine/MachineObservationWitness.lean` | 132 | yes | `169662b19653d20c76274bdce4845ab414da10cf` |
| `mathlib4/DarkTower/WarMachine/MachinePolicyFreeEnergy.lean` | 96 | yes | `3783d50968e21801198e8394135a770bf75f22f3` |
| `mathlib4/DarkTower/WarMachine/MachinePolicyFreeEnergyWitness.lean` | 74 | yes | `3783d50968e21801198e8394135a770bf75f22f3` |
| `mathlib4/DarkTower/WarMachine/MachinePrecision.lean` | 272 | yes | `e2e8ee9649908a5564da6237b7d8def80a4bbaf5` |
| `mathlib4/DarkTower/WarMachine/MachinePrecisionWitness.lean` | 132 | yes | `e2e8ee9649908a5564da6237b7d8def80a4bbaf5` |
| `mathlib4/DarkTower/WarMachine/MachinePredictionError.lean` | 338 | yes | `1282b75e3223d3f94d536ebabbb5b4125989722d` |
| `mathlib4/DarkTower/WarMachine/MachinePredictionErrorWitness.lean` | 163 | yes | `1282b75e3223d3f94d536ebabbb5b4125989722d` |
| `mathlib4/DarkTower/WarMachine/MachineTemperature.lean` | 313 | yes | `d7c43bca25babdfe4b9e84b83a9bfec17ca801c5` |
| `mathlib4/DarkTower/WarMachine/MachineTemperatureWitness.lean` | 69 | yes | `d7c43bca25babdfe4b9e84b83a9bfec17ca801c5` |
| `p4ng/aif-control-map-live.svg` | 3 | no | `35c34aad8d6a8631968dd08eb70277167c74590a` |
| `p4ng/aif-equation-dag.svg` | 65 | no | `0a95a1135ae2f8c0bd76f6355e53eef3ef3e49bf` |
| `p4ng/empirics-futon/aif-conformance.edn` | 1 | no | `0a95a1135ae2f8c0bd76f6355e53eef3ef3e49bf` |
| `p4ng/empirics-futon/negative_controls.sh` | 2359 | no | `4edf014847637087fac55255ff15bb40576f76ed` |
| `p4ng/empirics-futon/pointer_check.bb` | 403 | no | `2de1763bcd67025f90b6f109fc80493c62204193` |
| `p4ng/sec-aif-conformance-generated.tex` | 17 | no | `0a95a1135ae2f8c0bd76f6355e53eef3ef3e49bf` |
| `p4ng/sec-fundamentals-generated.tex` | 36 | no | `0a95a1135ae2f8c0bd76f6355e53eef3ef3e49bf` |
| `p4ng/sec-lane-campaign-generated.tex` | 20 | no | `0a95a1135ae2f8c0bd76f6355e53eef3ef3e49bf` |
| `p4ng/sec-lean-holes-generated.tex` | 13 | no | `d968d10c0c4236d6b767f92fb791ba3bbc5fa0b1` |
| `p4ng/sec-lean-state-generated.tex` | 54 | no | `c128cbdef333dfc3bdc6c0ebe1f28e29b9c7a803` |
| `p4ng/sec-q-interface-generated.tex` | 24 | no | `50ddbe245e75b8a052ce517aa5a04c3736ae3a3d` |
| `p4ng/sec-rnode-dossiers-generated.tex` | 266 | no | `0a95a1135ae2f8c0bd76f6355e53eef3ef3e49bf` |
| `p4ng/sec-workflow-generated.tex` | 20 | no | `0a95a1135ae2f8c0bd76f6355e53eef3ef3e49bf` |
| `p4ng/war-room-tetrahedron.svg` | 19 | no | `0a95a1135ae2f8c0bd76f6355e53eef3ef3e49bf` |

</details>

<details><summary>Expand complete commit list</summary>

```text
apm-lean 301466ec13bca2cc92d837d96870476c06f2e167 2026-09-05T12:41:18Z prove generalized eigenspace and charpoly clauses
apm-lean ce15d9df24196394a948841f16f25545aa982940 2026-09-05T12:46:04Z complete multiplicative Jordan decomposition
apm-lean c1c6daeb77755685f7c38104b234350bb0da8305 2026-09-05T13:43:42Z b98A01 construct order-35 subgroup from normal order-7
apm-lean 8939490450b1cbfcd4f5d6dc5fa000aded08da30 2026-09-05T13:54:42Z b98A01 prove order-35 normal-seven equivalence
apm-lean 5be929f560fdc06f749e36e4b71e1e77fffb8993 2026-09-05T14:01:40Z b98A01 complete subgroup normalizer proof
apm-lean 40214c33b7633f064da074735b469dfd564b41d9 2026-09-05T15:01:01Z prove b98A02 via commutator fixed field
apm-lean 72064774f851af7167143ed9319c1bd096e0dff4 2026-09-05T15:37:25Z b98A04 package supplied chain as composition series
apm-lean e72fc75c4ac6c7d97dddb8be2a4b179bd3bb4b18 2026-09-05T15:39:40Z b98A04 transport finite length to simple quotient
apm-lean 3d91cb7faacd463a9ac909acc4e7ec51dd439423 2026-09-05T15:43:33Z b98A04 prove simple subquotient factor occurrence
apm-lean 62f23e2b3734187a121446e4babb8e15e1b6c99f 2026-09-05T15:45:38Z b98A04 add kernel-range simple quotient dichotomy
apm-lean 87494e99cf3bdf3115a19ebb2e288e3f7c67024a 2026-09-05T15:47:22Z b98A04 complete simple factor occurrence proof
apm-lean b0fded058ab220f0bf5b8d4d3729a4d8375568f5 2026-09-05T16:26:49Z b98J01: constrain abelian images of A4
apm-lean 53f8c2f37420c9472e6505ed4ceb760855737498 2026-09-05T16:35:00Z b98J01: prove order-three kernel central
apm-lean ab8236ccda1d4d2001965ae8522b8b1e5691ed66 2026-09-05T16:41:04Z b98J01: close pullback and count Sylow threes
apm-lean 1ee0748c9eaf2ce79855fa6a1f5e860718be0a76 2026-09-05T16:46:47Z b98J01: complete Sylow action quotient proof
apm-lean 64139bff8356e7e454a1cf469866e40502fff1cd 2026-09-05T17:27:55Z b98J04: factor finite CRT through coordinate separators
apm-lean 9cfa36b25f3b745b66da810a1a6218c4efe7347d 2026-09-05T17:31:38Z b98J04: prove finite noncommutative CRT
apm-lean 61ec22bf0e0531c674e2b97d2fd9ceb9d55bf564 2026-09-05T17:36:08Z b98J04: lift finite simple decomposition into ambient ring
apm-lean 49339578e9b4f7438a6baa0fcf34cd4dac47a9e8 2026-09-05T17:38:41Z b98J04: prove annihilator intersection is zero
apm-lean 7424bbd401912cad0f533b361dfc1ae44f5432ad 2026-09-05T17:41:12Z b98J04: prove faithful right ideal has full left span
apm-lean 5bd437af133c1aa879d3da9ce1663851f817f61e 2026-09-05T17:44:41Z b98J04: separate annihilators using minimal faithfulness
apm-lean 7e3df1504ab0f89059995cfe81329851c1b3842f 2026-09-05T17:48:04Z b98J04: isolate maximal two-sided quotient seam
apm-lean ea3df9ab41879fd0f53ab876f07ec4b2d6361c76 2026-09-05T17:52:44Z b98J04: derive matrix quotient from maximal two-sided ideal
apm-lean f6d75ae199362a42f2681080070794ddf7b88136 2026-09-05T17:55:49Z b98J04: close algebraic core and isolate universe defect
apm-lean f2dfdf39d51fa860efd6dec811c319fbf3e20c76 2026-09-05T17:58:10Z b98J04: refresh quotient nontriviality API
apm-lean b287fb1de930396f3d3a8868c588c0868746cc42 2026-09-05T18:00:31Z b98J04: prove universe-polymorphic repaired theorem
apm-lean 77d59f12d7deec3be17d4f8ef027acea6647dec7 2026-09-05T18:03:02Z b98J04: prove frozen conclusion under smallness
apm-lean 8445a122641e8be5fbcd56907e1b165462393fbb 2026-09-05T18:04:53Z b98J04: expose necessary smallness of frozen conclusion
apm-lean 57d67c369cf3d30759f2f420438bf486e8486c64 2026-09-05T18:09:29Z b98J04: specialize universe defect to division rings
apm-lean a1d1c96567f66a8831fc349d17ae3d420392cee8 2026-09-05T18:11:06Z b98J04: derive global smallness from frozen theorem
apm-lean fa4324aaeb5fae009454b0774bbc7781c3318101 2026-09-05T18:13:31Z b98J04: formalize counterexample to frozen universe
apm-lean cca0d3fbbec0cd21a81416f92c2a8e49bd28ebc2 2026-09-05T18:15:02Z b98J04: refute frozen conclusion without circularity
apm-lean 3fa78a5aa70bb80b5590849c82d6fac7ca920b42 2026-09-05T18:16:12Z b98J04: record axiom-safe counterexample checks
apm-lean 8706d052c575dd267b5e4f901ecce318250bea11 2026-09-05T18:33:12Z Solve b99A02 Sylow cascade
apm-lean 52ec53e0bfe682bdde6f96a286c7c3cc0f7ad807 2026-09-05T19:24:26Z bpm-1-8-1: select geometric continuity scales
apm-lean 64a6ad536d9383ee2c0e514be0cc2bcebdb078da 2026-09-05T19:27:28Z bpm-1-8-1: build decreasing scales and cap summability
apm-lean d79f442d6e572fa8af071ce845a6e39c9f0be158 2026-09-05T19:29:06Z bpm-1-8-1: construct continuous concave cap series
apm-lean 5b7d26f8613d60fbcf87d0dcafc723fd003cc9cc 2026-09-05T19:33:22Z bpm-1-8-1: close concave majorant construction
apm-lean e2adb88859f461268d764f5c1a7aae0ff3cf0feb 2026-09-05T20:20:16Z m00A02: add remainder derivative infrastructure
apm-lean c2f4b187ec641f531de685ca249cc6cec632a948 2026-09-05T20:24:09Z m00A02: force leading derivative coefficients to vanish
apm-lean 1d045c6e4311bb1eb4b6c8b9647bcd12e14cb34d 2026-09-05T20:27:56Z m00A02: normalize scaled asymptotic remainders
apm-lean 725803f5b77520e4cc6d693bd51e24c78967270d 2026-09-05T20:31:42Z m00A02: reduce rigidity to derivative limit transport
apm-lean 7349623422d7adc39181322c88a16d6066250be4 2026-09-05T20:37:09Z m00A02: prove derivative expansion coefficient rigidity
apm-lean fe0c41ec5eacdfdb5918474afe538f4baba18a85 2026-09-05T21:55:00Z prove compact operator spectral finiteness
apm-lean a5cc91f3845107130e31a127fb98635968402624 2026-09-05T22:30:55Z m00A04: bundle Sobolev energy pairing interface
apm-lean 1302e97e539ac5a260123e28db397045a8849d13 2026-09-05T22:34:20Z m00A04: prove admissible energy pairs closed under negation
apm-lean d549b1b290980cf69c54930a26eebd7fd69c2f9c 2026-09-05T22:37:35Z m00A04: prove admissible energy pairs closed under scaling
apm-lean c5bfaa5d2f2080f411d4a1d1904ef0bd56d80efb 2026-09-05T22:42:50Z m00A04: close positive-dimensional weak solution
apm-lean cd591fd234bc7ac874412d3d649e91a169b5a442 2026-09-05T23:20:04Z m00A05 prove endpoint-safe solution uniqueness
apm-lean aa83f30eef5cd4e196cede4f3a5ed3d78515fa70 2026-09-05T23:22:36Z m00A05 add restart and equilibrium rigidity lemmas
apm-lean 5a20b3972317f3ec7476581a9c0186075cb8b580 2026-09-05T23:25:35Z m00A05 prove equilibrium trapping and monotonicity
apm-lean 2b3d631119adc049de7dbfb0ad3086df180ce9e8 2026-09-05T23:27:45Z m00A05 construct finite trajectory limits
apm-lean 02aaf2582087d0fdf41b9d1e66a289f060c16e09 2026-09-05T23:31:48Z m00A05 complete global attraction proof
apm-lean d6b5113529f4c0b3538d336fac8d02f4e0e6c1c1 2026-09-05T23:34:50Z m00A05 prove uniform non-Zeno Picard step
apm-lean d3da39a99deb6127a17400ca7a5c8b5757d030e5 2026-09-05T23:37:21Z m00A05 build recursive Picard continuation data
apm-lean 1e3d02898bfe1cb8bfff5c34b15a7206b957f3cd 2026-09-05T23:40:56Z m00A05 define global mesh selector and interior ODE law
apm-lean b6a17e5b250d363e2bce43849cc40d6d721d1d6e 2026-09-05T23:45:12Z m00A05 complete global Picard continuation proof
apm-lean ad9825a0c60c7bd14ab79d5c10f301fa7dd8bbbb 2026-09-06T00:22:20Z m00A06: recombine finite-part split integral
apm-lean 5a69b62b3a736e974c409570a59d1357e3a441dc 2026-09-06T00:25:15Z m00A06: add seminorm bounds and close main reduction
apm-lean f2c0d37f218bec4157047b50993bdc2dd2c30c17 2026-09-06T00:27:07Z m00A06: package test functions as a submodule
apm-lean f0bdc554b2fe7d8a499b29ff037136e767e93d01 2026-09-06T00:28:59Z m00A06: prove inner finite-part integrability
apm-lean 742cb20fd816f0b5b8f957ded6fdfdaf268677d8 2026-09-06T00:31:28Z m00A06: prove outer finite-part integrability
apm-lean 033301a7b209fa73ea450ba928554317171f55ca 2026-09-06T00:34:10Z m00A06: construct finite-part linear map on tests
apm-lean dea9a339e9ab14c67d7ea8a576b057f1a8733782 2026-09-06T00:35:43Z m00A06: extend finite-part map to all functions
apm-lean ecfa47db456a63988056ba197b028a45c63e0b14 2026-09-06T00:37:52Z m00A06: bound regularized kernel by derivative seminorm
apm-lean 8ba55450066264eb9cda522d92d65c65843e2a5f 2026-09-06T00:39:34Z m00A06: integrate inner seminorm bound
apm-lean a319aa8d39072fc749eaeebb4813755eed978f43 2026-09-06T00:41:15Z m00A06: localize and bound outer finite-part term
apm-lean cc36aafa989dc8eb0ba8cdb000e3055bf107c2c1 2026-09-06T00:44:03Z m00A06: complete finite-part distribution proof
apm-lean 85866ac46693cec2c4bd234f4e6bbdbc573388cb 2026-09-06T01:51:00Z m01J04 bound derivatives of test functions
apm-lean b0c096d8acb3f395ee33bd0c3e28a7b9d586496f 2026-09-06T01:54:25Z m01J04 establish multiplier field integrability
apm-lean 771754af801139a47a2e706e2afb615ab47b4f6e 2026-09-06T01:56:38Z m01J04 formalize test-function product rule
apm-lean d5b5bcfb477a11f611790ace0ad196cd27e3a42f 2026-09-06T02:00:00Z m01J04 expose derivative coordinate Lp bounds
apm-lean 1e22e58331ca1a6eaa1f93f8b28e920629aa74c3 2026-09-06T02:03:09Z m01J04 construct weak Sobolev product record
apm-lean 9d782394b29d0e3a5c5d120c617f6d2aaa42d408 2026-09-06T02:06:06Z m01J04 prove product eLpNorm estimates
apm-lean 608ce65e83033f71534db7af13f6312cf8a72e0f 2026-09-06T02:08:30Z m01J04 close multiplier norm bound
apm-lean d80030a235d3fe0f39f218c04126a1137c8b9271 2026-09-06T02:11:10Z m01J04 globalize weak Leibniz test identity
apm-lean e4c82723b4870a6fa3fb127326bddf5555bdcd6b 2026-09-06T02:14:01Z m01J04 add weak Sobolev subtraction transport
apm-lean c13546ff98b585510996936d9e76a7a456f2f6b5 2026-09-06T02:16:50Z m01J04 prove product distance transport
apm-lean 87d284e3dca2d3e3d5125cc97f09abd85b8f4a5b 2026-09-06T02:19:26Z m01J04 transport zero-boundary approximations
apm-lean 049779491d93f0b67a3973b1bdfa2890a0090084 2026-09-06T02:21:22Z m01J04 isolate localization density interface
apm-lean 7a95705265a82c086893699b96e7fc185d3530e2 2026-09-06T02:24:27Z m01J04 add raw L2 pairing bounds
apm-lean 21e79a073776a751288bb4d6dbe35d60736c9daf 2026-09-06T02:27:22Z m01J04 preserve strong L2 under bounded multipliers
apm-lean e711952c2020b5bda4033a9eee33291666bfcb18 2026-09-06T02:29:13Z m01J04 prepare Rellich pairing bounds
apm-lean 415f63213bad7c0e6ff09461465856dc3529dbb5 2026-09-06T02:31:46Z m01J04 close Rellich strong error pairing
apm-lean 55059a39b96aaaabc1d81b3bdd067cf4f9156465 2026-09-06T02:34:23Z m01J04 prove the Rellich nonlinear pairing
apm-lean 772e4ed82fce9e241a1373a1f91a2ffd7ebfaa3f 2026-09-06T02:37:59Z m01J04 add H1 normalization infrastructure
apm-lean f4ca712bee403373596ea3e66d1bac88fab7de57 2026-09-06T02:42:14Z m01J04 bound weakly convergent H1 sequences
apm-lean bd4d504a02ebcc15d6f357fbd486a868af8e64a4 2026-09-06T02:44:02Z m01J04 extend divergence convergence to bounded tests
apm-lean 37317e619659ac5b2f26342683be1d6b9ffda76c 2026-09-06T02:46:08Z m01J04 package bounded bridge sequences
apm-lean 14a4bd3c2a7d4c85565f5dd35a76fe3794aea266 2026-09-06T02:48:09Z m01J04 close bridge divergence pairing
apm-lean 7d10cd505f4dd1202c9d534547c9e28fa7af7063 2026-09-06T02:51:19Z m01J04 make weak L2 membership explicit
apm-lean 84243845d76faa314076079c56a45a7450feec95 2026-09-06T02:53:15Z m01J04 add subsequence convergence infrastructure
apm-lean 8deb4c121ed8cb0b108db3560daa67a4bf8757ea 2026-09-06T02:56:37Z m01J04 identify Rellich strong value limits
apm-lean 27442d03712deb982f10d322f40aa77cf8238d20 2026-09-06T02:59:14Z m01J04 refine subsequences to the weak limit
apm-lean f47a97bb3f201127ecfad3cf14714722b1973a00 2026-09-06T03:01:47Z m01J04 bound weak L2 difference fields
apm-lean 8c18d4a7974d1878b563d3e0a4e6ece1b8429533 2026-09-06T03:03:16Z m01J04 close strong-value weak-field error
apm-lean a1a906e9cdce27f6d852d4c0518a9382939884cf 2026-09-06T03:05:34Z m01J04 close variable lower order limit
apm-lean eb8fb46f6efcff272dd52467e2a0c22ae8a862b2 2026-09-06T03:07:18Z m01J04 close localized lower order term
apm-lean 1ea48ae542f3961c50c1ca7357e05e1ca53f13c3 2026-09-06T03:10:07Z m01J04 isolate principal divergence term
apm-lean 2a2e88c414db835b622b316c25097908eac9871b 2026-09-06T03:13:15Z m01J04 close compensated pairing clause
apm-lean 17ac0d6b4965a870401c62029f7fa33f0a44a453 2026-09-06T03:15:30Z m01J04 separate localization from smooth density
apm-lean 95bd15f768b4f927d232d4f86e1482705ae0d64e 2026-09-06T03:17:20Z m01J04 expose componentwise smooth density interface
apm-lean 3b6075558afb3909514f9ff1fe1fad2ff520f9b4 2026-09-06T03:19:15Z m01J04 reduce localization to epsilon smooth density
apm-lean b0846b43b846f3f63af09402494dfd8aa313e84b 2026-09-06T03:22:03Z m01J04 add localized Lp globalization lemma
apm-lean 2f7a68736152e2b46f9b454f5652a1f47c0722fd 2026-09-06T03:23:56Z m01J04 globalize localized value field
apm-lean 901c6c8959b1931cfd6e6fdf54c56b5fe9d407e8 2026-09-06T03:26:12Z m01J04 support a.e. gradient globalization
apm-lean 082d9ac91c64d486f75b007e7814bd6bb0f0f951 2026-09-06T03:28:01Z m01J04 globalize Leibniz gradient from level-set vanishing
apm-lean a1fdc5e23d0ac0582950b339c367654fbcf4a15b 2026-09-06T03:30:11Z m01J04 name level-set derivative interface
apm-lean 790f095756e54aa2bfe3523a1ddcf782e85ea7b5 2026-09-06T03:34:25Z m01J04 prove level-set derivative vanishing
apm-lean 10f487e3c404f4f7ff3816d8df64bb56395c0706 2026-09-06T03:36:27Z m01J04 package global mollifier inputs
apm-lean 093255b1b2a06fa7389b5575763ef2eb2c5e81f4 2026-09-06T03:40:47Z Package localized product as global H1 record
apm-lean 995284210fca7e1244e78d0bbdfade2821c1b83d 2026-09-06T03:43:00Z Add global H1 restriction interface
apm-lean b6b4e79941543d85d883880c5987db07717dcbef 2026-09-06T03:46:37Z Activate global H1 restriction infrastructure
apm-lean 6e36a050d773f5ebf94df6fa0a4e132edf323057 2026-09-06T03:49:12Z Reduce localization to global support-preserving density
apm-lean 5a97bbad8b6109901a5444b03b8dfdfca95e8cac 2026-09-06T03:52:06Z Establish compact support cores for mollification
apm-lean 05f3c1edc36a51003a373a35ffa542d77a6a0d87 2026-09-06T03:54:33Z Control convolution support by compact core margin
apm-lean 35683065c0a185ce273e955d843df9af57e52c2b 2026-09-06T03:55:46Z Package support-safe mollifier radius
apm-lean e29168d5bfa247ec4bfe8484bc17f1fd89163b78 2026-09-06T04:00:59Z m01J05: certify frozen statement is false
apm-lean 4b04568097f6d0023d5809a3910ef230aaa0c911 2026-09-06T04:04:02Z m01J05: prove minimally repaired contraction clause
apm-lean 7023f5f111dd75a4b8963fb7938490c05329307a 2026-09-06T04:49:03Z m01J06: prove integration by parts and zero stationarity
apm-lean 4a29e9034f774829a93ed619305737248ccaa0a0 2026-09-06T04:52:26Z m01J06: add dominated variation calculus lemmas
apm-lean 17afe0fc29b1b568f0cc1007bf3a0dab815574c8 2026-09-06T04:56:40Z m01J06: prove energy first variation bridge
apm-lean 8d3aef26ef28827fa8d9a77f191daa084aa1f6ed 2026-09-06T04:59:56Z m01J06: add interval fundamental lemma
apm-lean cd5333214701493bc8abb000518ba413be5c7ad7 2026-09-06T05:03:47Z m01J06: derive stationary test residual identity
apm-lean d703bf8d0602bc1406784523e110bdb976a09258 2026-09-06T05:08:50Z m01J06: prove stationary Euler-Lagrange conditions
apm-lean e2878b34740e666ddb92997ab180bd08669954a7 2026-09-06T05:12:34Z m01J06: expose linear mode as ODE trajectory
apm-lean 6d7fb862018b89428b08ec8c0da275203a78b8b7 2026-09-06T05:15:19Z m01J06: build harmonic comparison trajectory
apm-lean 6bf2610a333149e4edc4bb52e11515667c798546 2026-09-06T05:17:51Z m01J06: prove harmonic normal form for linear modes
apm-lean 600639103883264e95710c54344a1391f32b8a91 2026-09-06T05:21:56Z m01J06: force linear-mode frequency cosine zero
apm-lean cf8212832060ce7b2419f04348ad8bb64cd7399d 2026-09-06T05:25:46Z prove linear spectrum converse
apm-lean 56354ba1bc27cb3a95b1e77cf01a4bf1d3d0a66f 2026-09-06T05:28:53Z add nonlinear pendulum local flow infrastructure
apm-lean 74120155aa6ceef01583c53abca93724fcba564f 2026-09-06T05:32:25Z develop pendulum quarter-period kernel
apm-lean eeab274aa5846df9e305721b0eff0dca8c8c7162 2026-09-06T05:34:32Z prove quarter-period sine lower bounds
apm-lean 05c54024aface20212044d8913f620b6c63d3256 2026-09-06T05:37:09Z bound pendulum quarter-period denominator
apm-lean 161f5f46c9c3876102ba9a968995c2fabe8f4d57 2026-09-06T05:42:27Z prove quarter-period kernel integrable
apm-lean 3d37eaf694213e2326a0dd7d1e1776111ec99906 2026-09-06T05:47:26Z prove quarter-kernel pointwise limit
apm-lean 3a24ab24dd12acec762161ab718ca52c313e8d57 2026-09-06T05:50:13Z prove quarter-period dominated convergence
apm-lean 6db54da3d47c9df0903295ebc7ea58351d511ae2 2026-09-06T05:53:05Z evaluate small-amplitude quarter-period limit
apm-lean 3242795637a20861640a611464b9379b2719daa0 2026-09-06T05:56:13Z define cumulative pendulum quarter time
apm-lean 9db02bbe7b2e206b2f042c8ab49d916643de42ac 2026-09-06T05:58:15Z prove quarter-time strict monotonicity
apm-lean 300c046f12768c55342295dde6b06dc2159b58f8 2026-09-06T06:04:04Z m02A02: close mean-zero Fourier subspace
apm-lean 5f4e44f0de34556a160f368faad29c2ad2150fe8 2026-09-06T06:07:29Z m02A02: establish finite Fourier smoothness infrastructure
apm-lean 4eed558d1b85395f5e5a90c33a6a4707922cdbed 2026-09-06T06:13:59Z m02A02: prove density by symmetric Fourier truncation
apm-lean 9e765a5a406d0ab13b4c2a40d3301b4a9c148e17 2026-09-06T06:18:45Z m02A02: prove Fourier Poincare inequality
apm-lean b8e20562137d2720e53bb23dad8cf0688a9cc168 2026-09-06T06:24:40Z m02A02: construct bounded Poisson multiplier
apm-lean 649e9ddc33923aa9cf51e39781a86d3183d064ab 2026-09-06T06:27:46Z m02A02: add affine Poisson solution infrastructure
apm-lean fff389910eba89d23fcaae3f507c4c3224e478dd 2026-09-06T06:30:59Z m02A02: assemble solution around weak converse seam
apm-lean ec5547fe5e8f7b0bdff63eca7b9f9c6924efcc07 2026-09-06T06:35:36Z m02A02: prove weak equation by paired Fourier probes
apm-lean 7b931f5a0af81e6d62a90e0f5b16a580636fd32c 2026-09-06T07:09:31Z m02A03: prove local integrability of logarithmic kernel
apm-lean 0b6d9ca2c4697c686ab368829be2e57ffc475324 2026-09-06T07:11:56Z m02A03: add test-function integration interfaces
apm-lean 8ea611fe226575a548e5e684e24e993d674b0e6d 2026-09-06T07:15:19Z m02A03: expose coordinate derivatives as Frechet derivatives
apm-lean bcfb3b77c2e880619397e19d318cbc4f98be9f45 2026-09-06T07:19:25Z m02A03: identify encoded and standard Laplacians
apm-lean 1ba6c48a9051e51a835bcdd4c202ca19e8d75ab4 2026-09-06T07:23:03Z m02A03: prove Laplacian support and integrand integrability
apm-lean ad1167c110f8aa1e0782977f6ed40b37e5c6fb5f 2026-09-06T07:27:15Z m02A03: add smooth logarithmic regularization
apm-lean eab65dba593f8a50481083dc10ae6d15c2956662 2026-09-06T07:29:26Z m02A03: reduce regularized kernel to scalar coordinate profile
apm-lean 75986ae53d68214f6f76b1dfa12d936766aa19f0 2026-09-06T07:33:40Z m02A03: calculate regularized kernel Laplacian
apm-lean 1c8166a67a004e616021542b86574686473db5ba 2026-09-06T07:39:04Z m02A03: prove compact-support Laplacian integration by parts
apm-lean 046376ea6918eff7b356b9bfe4a929a0614f27ce 2026-09-06T07:43:59Z m02A03: translate regularized Green identity
apm-lean e888ce6c522352af08a8aadc5daf6939858f7d14 2026-09-06T07:48:39Z m02A03: normalize regularized peak profile
apm-lean 9786e7a6e45980b9cebf690449ab2b53a1ff6021 2026-09-06T07:51:53Z m02A03: prove regularized density converges to evaluation
apm-lean d266ca026571d60fdd500fb39ddfffea0484be6b 2026-09-06T07:57:08Z m02A03: complete logarithmic Green identity
apm-lean b0fa43cfc3e2b622a3d67654fef9ba44d76a5c1f 2026-09-06T08:51:04Z m02A05: formalize off-interval bump witness
apm-lean 1e741e0a33bf10c9279afe790d103ae193aacb8e 2026-09-06T08:53:27Z m02A05: prove frozen first conjunct is false
apm-lean 38b7925a392af50cae2e30b28ff09b5fb2f6ef6a 2026-09-06T08:54:34Z m02A05: refute exact frozen proposition
apm-lean 40747e41c9943a2b5bfd10695607215cf74ca854 2026-09-06T09:42:07Z m02A06: close Green transform scalar estimates
apm-lean befdaa6bf7b2fbde385f67ef276b33259a809eaf 2026-09-06T09:45:51Z m02A06: formalize frozen uniqueness defect
apm-lean 9247fae16aeca274f2941486ec239846223199d7 2026-09-06T09:47:25Z m02A06: refute exact frozen proposition
apm-lean a5df9344621b2c7230f131fe0def72c1c5cc9e11 2026-09-06T10:37:18Z m02J01 prove test-function primitive equivalence
apm-lean 904be7cb702c88f379ba295d6818844ad78dcf26 2026-09-06T10:42:12Z m02J01 prove uniqueness of distributional primitives
apm-lean f6524a6283406218ee61e306df243c113cfd8700 2026-09-06T10:48:36Z m02J01 add LF-continuous zero-mass projection
apm-lean 7bdcfb23d84a20f039574d649b22f682f17712c4 2026-09-06T10:52:46Z m02J01 bundle canonical normalized primitive
apm-lean 57d1fa0b2e0ea340dfaac2e15068ef55306820bb 2026-09-06T10:56:32Z m02J01 factor primitive through fixed support stages
apm-lean 5d7caf64664b5aa95730e459945a3fa5ccad478e 2026-09-06T11:01:24Z m02J01 prove zeroth seminorm primitive bound
apm-lean 132cfb5e040fc2bbfcce2dd0e46cd7da36835c90 2026-09-06T11:05:49Z m02J01 prove all primitive seminorm estimates
apm-lean 6923ba12727466f08af103f67ebea97c6ea1fb9d 2026-09-06T11:10:45Z m02J01 complete distributional primitive theorem
apm-lean 8cbf54488d786886ea38a53ceab50937475c4ce0 2026-09-06T12:15:05Z m02J02 add H10 zero infrastructure and close aggregator
apm-lean 19f7a2c3b951f57e4499bba4719fed604610d676 2026-09-06T12:20:38Z m02J02 prove Hermite pair linear independence
apm-lean bf6e927bea6d60717c3dc61ea2e9d38ba8be4cf2 2026-09-06T12:23:58Z m02J02 expose Hermite derivative Gaussian normal form
apm-lean a21f1883c0445a0e144c0e746a5d02a1a45cb7d1 2026-09-06T12:28:56Z m02J02 prove Hermite Gaussian L2 bounds
apm-lean 8df87f0ba9b3a7111256ef2371b18f4fd0488e5b 2026-09-06T12:33:36Z m02J02 prove weak derivative and sine L2 layers
apm-lean 432a517668d0b015c74a6f75959d3da1d4f49c3b 2026-09-06T12:37:24Z m02J02 establish sine mode normalization
apm-lean 88c1eeeca6ca417d84edaf1b637727dd8fc4e8cb 2026-09-06T12:41:13Z m02J02 prove sine pair linear independence
futon2 1f389b41e6852b708e20caae0fa840c7e37132d0 2026-09-05T12:04:26Z :F7 -- the reversal branch closed mid-row by Joe's :policy-grain ruling
futon2 15125c9317b9350d781f3b465d65b40dd55afcfe 2026-09-05T12:05:04Z U59 ruled met, RE6 ruled split, I4 reviewer decision taken; pause checklist recorded
futon2 d465a975bf1643baae7c82b17e841ec206b9eff5 2026-09-05T12:05:36Z library-loop: reopen L6 on incomplete no-source check
futon2 2554119f6ed7d29c41aa4f6dee7a9e2bcda699e9 2026-09-05T12:08:02Z L6 done-unreviewed v2: complete-corpus no-source check; 2 real @why edges, 62 no-source notes
futon2 0b0bb8881699dbf67f076a3461649bd18259362c 2026-09-05T12:08:16Z wm-contract: draft I4 registry corrections
futon2 ef08d0fd427038fb75a7dbfe90c1b611dc669158 2026-09-05T12:09:10Z File I4's two registry writes (reviewer decision, C514 drafts reviewed at source)
futon2 8ef4ca9d50d4622ffa7744e23e78a51ef9a50414 2026-09-05T12:10:20Z library-loop: review L6 complete-corpus backfill
futon2 54a42aef1a337a077f0ce701637b559182544311 2026-09-05T12:10:38Z :F6 review readiness witness evidence gate
futon2 bd4b4bf64597d0b0daf8726178aa0533f4d5e337 2026-09-05T12:11:32Z wm-contract: workflow-report snapshot (wm-build-loop)
futon2 30f3183f8b33d11ee6c747a7211b563c9ef832e9 2026-09-05T12:11:38Z L7 done-unreviewed: ukrns+snatch+vsatlas+storage backfill, 92 annotated, 0 corpus sources found
futon2 b8d724f02833b4083451c74540f716a1779fdc48 2026-09-05T12:14:11Z library-loop: reopen L7 on invalid source pointers
futon2 73b6616aee5f61f0b194528d2806ee1500493404 2026-09-05T12:15:23Z :F7 -- second-reader review
futon2 6cc33fee282b7dc7c40343e5c8f994a9e7223c5f 2026-09-05T12:15:51Z L7 done-unreviewed v2: annotations repointed to L7 receipt; gate enforces source pointers on backfill sections
futon2 05a625a31a0b2790677f7c58d0d9023e5707e00a 2026-09-05T12:16:13Z wm-contract: workflow-report snapshot (wm-build-loop)
futon2 c576018328be669f28b4432e3bb200110d0dc065 2026-09-05T12:17:17Z wm-contract: workflow-report snapshot (wm-build-loop)
futon2 40bd3ad463a70ea6af2d375073eb8329a23b61da 2026-09-05T12:17:59Z Review L7 rationale backfill corrections
futon2 bb4a5c03b3297c3357ddfcdcc9ae48b8901c0efd 2026-09-05T12:18:18Z bulletin: the day's digest (build loop end-of-session, :B1)
futon2 fd64f8f1c4ab4f55ac934303f4fa684e907ea681 2026-09-05T12:19:19Z bulletin: the day's digest (build loop end-of-session, :B1)
futon2 baa74c5ed7e28fd6739a58d45e8310e5b5961ed3 2026-09-05T12:20:33Z L8 done-unreviewed: math-sections backfill, grounding corpus problems+futon-theory, 0 matches found; untracked file disclosure
futon2 544d15a9526e527f02bdebb23b010858ee2ac9a5 2026-09-05T12:20:39Z Pause checklist executed + mint F8/F9/U64
futon2 667f87b11491ce216779366aace0be78298cca5b 2026-09-05T12:20:56Z U64 class :M -> :V (this board's vocabulary; :M is the zaif/library boards')
futon2 a544fbf1973946af13c7d849e9ce7e5698f584fe 2026-09-05T12:23:10Z library-loop: reopen L8 on broken receipt pointers and swept file
futon2 a2830e5f928b385df2d25972b487c0debdfa3c57 2026-09-05T12:24:45Z L8 done-unreviewed v2: LL8 pointer bug fixed + receipt-resolving gate (negative control red); swept file body sourced
futon2 a6d6d074d1ee321f53b26c17c73e563e974fc69c 2026-09-05T12:24:59Z L8: receipt regenerated at 54d58f6 (md5 8c2d9ed7), evidence corrected
futon2 f88234df57cb1b66df1b219f73a73dd8d95a6af5 2026-09-05T12:29:42Z Review L8 math rationale backfill
futon2 d0c82cb05fab5197384f023ea9f69bc5f8f5faea 2026-09-05T12:30:46Z L9 done-unreviewed: coherence-family backfill, 76 patterns, 0 corpus sources found
futon2 64daa4e4f45af5f29f76d9b1c819713265802358 2026-09-05T12:32:59Z library-loop: review L8 rationale backfill
futon2 1f605f500b2f85aa7e3c3f06ce4400d7bb46e9a4 2026-09-05T12:33:58Z L10 done-unreviewed: peeragogy+or3+musn+plos-npt+agency+agent backfill, 99 patterns, 0 corpus sources found
futon2 7769ddcae2c9bcec4398c6412298c60a7775c5fa 2026-09-05T12:36:36Z library-loop: review L9 coherence backfill
futon2 63b38f8ba598a32efde08943e61b8d46141e16af 2026-09-05T12:38:26Z L11 done-unreviewed: iiching/iching triage -- section-grain for exotypes, defer hexagrams, reversal stated
futon2 43a1b3dc51a3e3cf041d6e6df427396b2d4ec021 2026-09-05T12:40:14Z :F1 slice 2 -- the machine-grain Q(o|pi) over F7's two cascades
futon2 4c3804a086b7a15b55e446812c1695d1e4146217 2026-09-05T12:41:02Z library-loop: review L10 rationale backfill
futon2 a791b1bb542c6f41f8ec4b5d3f7b703846ec6485 2026-09-05T12:42:06Z :F1 ledger -- slice 2 landed, row to :done-unreviewed
futon2 237742696b0175dc13251d1ccc90cb8bd876dfbe 2026-09-05T12:42:13Z L12 done-unreviewed: re-census + reachability delta vs L1 baseline; graph regenerated
futon2 91e16a145ddb1d4c7e4f0a8d88c39b6c97496eca 2026-09-05T12:44:06Z library-loop: review L11 triage
futon2 e94e0bcdf73f7b3e746ab8a0766cbb39ac6b4688 2026-09-05T12:45:57Z L13 done-unreviewed: graph-certificate gate v0 (offered, not wired) with red negative control
futon2 fc22bd941db7f14b32f9c977422c783f34726b58 2026-09-05T12:46:52Z :F1 second-reader review
futon2 002b38bf920845d228f226723e06c4b2e0928797 2026-09-05T12:47:42Z wm-contract: workflow-report snapshot (wm-build-loop)
futon2 62c9bb052d4ee1182f09655bfeb271974cf2e574 2026-09-05T12:48:12Z library-loop: review L12 reachability delta
futon2 8a17542c42438c83315d1ffbfe32dd943a36ed16 2026-09-05T12:50:44Z library-loop: reopen L13 graph gate readings
futon2 f1f36b928c39b336cd741cf7222d9b71ea8c881c 2026-09-05T12:51:54Z L13 done-unreviewed v2: +wr roots added to both up and down readings; reading-distinguishing control committed
futon2 41293ea9839d804f7164100d23dcb1dda2e08dda 2026-09-05T12:53:57Z library-loop: review L13 graph gate
futon2 aacc76d807fef877343daf507563453eef15645b 2026-09-05T12:55:25Z bulletin: the day's digest (build loop end-of-session, :B1)
futon2 06c131e937c923ecd6b1f7b33796c262648445d9 2026-09-05T12:56:37Z Library board: mint L14-L17 -- edge resolution, iiching section-grain rationale, catalogue-IF/HOWEVER problem mining, advisory gate report
futon2 993d0aa9147fad0f756ace84c66fd60d2332ffd1 2026-09-05T12:57:49Z F8 leg 1 slice 0: refresh the U35 Lean-state receipt (STALE-PIN -> CURRENT)
futon2 a92877e39afd4438d274e0451f853823942c3c4d 2026-09-05T12:58:36Z L14 done-unreviewed: edge-resolution pass -- 35 resolvable edges, reachability delta 34->67 down-problems
futon2 b26819d0cae31be054f497cf80989c5744725a03 2026-09-05T12:58:56Z F8 ledger: leg 1 slice 0 (U35 lean-state join refreshed), row stays open
futon2 0898013d08e94283d900e64cfbb0b4af96341cc3 2026-09-05T13:00:01Z wm-contract: workflow-report snapshot (wm-build-loop)
futon2 bf176ba73fd15eb89991e39bcb9b72c1e4de2c45 2026-09-05T13:00:43Z library-loop: reopen L14 edge resolution
futon2 8f8478d0976308e3a35120edf6450a05a31162d0 2026-09-05T13:02:58Z L14 done-unreviewed v2: all holds tokens parsed (37 edges); transformation receipt with bases; census at 64436d7
futon2 1f765d264d14100faf6518988d6c8a2cc27355b9 2026-09-05T13:06:10Z library-loop: review L14 edge resolution
futon2 f8ee74473a8e06c94a88a03e730d89866109993b 2026-09-05T13:07:41Z L15 done-unreviewed: exotype-encoding-programme node + 256 shared edges; TEMPLATE excluded; iiching reachable 0->256
futon2 61531c40f059a8c249eea968a912f7e462a10549 2026-09-05T13:11:15Z library-loop: review L15 section-grain rationale
futon2 13d1a7cf045b77f68999b1b93a2d788f811b46cf 2026-09-05T13:16:06Z :F8 leg 1 slice 1 -- the precision map Pi, stated in Lean
futon2 57296b21d1ba73d7685060f122d17ebdc6f64ceb 2026-09-05T13:17:06Z L16 done-unreviewed: 2 catalogue problem nodes minted (6/8 covered by duplicate check), 1 holds edge, war-room/aif delta stated
futon2 e648d6cadbea9212f63d6b73ee78afd7d0f9051a 2026-09-05T13:17:13Z wm-contract: workflow-report snapshot (wm-build-loop)
futon2 cda7a72148e0f342f69cf5001297cea3dd8eb929 2026-09-05T13:19:57Z library-loop: review L16 catalogue problem expansion
futon2 1054d51cb393765c9919cd011ace1605b34da58a 2026-09-05T13:22:06Z L17 done-unreviewed: advisory gate report -- 7 cascades x 4 readings, delta vs L4 baseline
futon2 680d4acf5157939b1e210259e1a564e0684e03fc 2026-09-05T13:24:34Z library-loop: reopen L17 advisory report
futon2 088ce06a697f4745df1d626f2f1f30a013c8076a 2026-09-05T13:27:42Z library-loop: reopen L17 advisory report
futon2 4ab183a0a134f3cbf09b6deec09b7ba72c6a3282 2026-09-05T13:28:50Z L17 done-unreviewed v2: refused patterns named for every cascade x reading (28 lists)
futon2 04f580461d22adf6849de1c92e7a13914e6551be 2026-09-05T13:30:21Z library-loop: review L17 advisory report
futon2 d58e33c046dfe20c10c44d17273381c74366afaa 2026-09-05T13:31:47Z bulletin: the day's digest (build loop end-of-session, :B1)
futon2 98e5ea04c5ff4096ca57c492c244de54511ffc4c 2026-09-05T13:32:35Z Library board: mint L18 (ground served-refused sets, targeted) + L19 (@how-side reachability, measured)
futon2 266d14f32bd0dda747ded8d7f97524aa3f192204 2026-09-05T13:35:54Z L18 done-unreviewed: 4 section-doc problem nodes + 64 shared edges; served-refused union 71->64
futon2 f471ac10ac87c8fb5d87e7417b321a242521d869 2026-09-05T13:36:52Z :F8 leg 1 slice 2 -- the prediction error eps, stated in Lean
futon2 178632c60af353525e41b57e9e38af20abf38963 2026-09-05T13:38:59Z library-loop: reopen L18 incomplete no-source search
futon2 eb2ac61fa873e6d6c22906fab94f1cf80f2a270a 2026-09-05T13:41:49Z L18 done-unreviewed v2: complete per-pattern no-source search; snatch grounded from its README (19 members); snatch+alfworld cascades now PASS
futon2 9b07a7c1b9a85d8f2cfbbd20257ec38b92c3f821 2026-09-05T13:43:37Z library-loop: reopen L18 after review
futon2 755ec930754e08f67675495f8b812bcdd62b539b 2026-09-05T13:46:05Z L19 done-unreviewed: the @how side measured -- 32 resolvable edges, thin editorial layer; combined-reading table for 7 cascades
futon2 f8f664eb791f995e43a970d520fab8e50a31d03c 2026-09-05T13:48:13Z library-loop: reopen L18 incomplete rationale-source search
futon2 9d35fd455bae5a30cfd4ce3255ea344147c209a8 2026-09-05T13:51:53Z L18 done-unreviewed v3: fixed 71-member worklist, complete 4-arm source search serialized, 5 more section-doc nodes, 14 grounded
futon2 69154b1d53007eb88519cf34efaec4b39cc0281f 2026-09-05T13:52:30Z Add F8 belief update production readback
futon2 650624b87762f644fe221d08a71ed091c20405a6 2026-09-05T13:54:14Z :F8 gate repair -- slice 2's evidence prose reddened pointer_check at HEAD
futon2 c607eeaf6539360a1ff18638956c5ccc3a6de50f 2026-09-05T13:54:37Z :F8 ledger: cite the gate-repair sha 650624b8
futon2 6db9911dff78a5be931a71140d362a1bc6b09d32 2026-09-05T13:55:13Z library-loop: reopen L18 unpinned source receipt
futon2 b6d4c7c628b662d5925b3ad7e44db9202338fb08 2026-09-05T13:56:19Z wm-contract: workflow-report snapshot (wm-build-loop)
futon2 47b3710425e8fc5b0cf5624c0ecb42387c4b5591 2026-09-05T13:56:40Z L18 done-unreviewed v4: concrete per-member searched paths; pinned source authorities; worktree-reproducible receipt
futon2 17af965e58aeed3ca72a2c7b4c952cbe78bf548f 2026-09-05T13:58:24Z library-loop: reopen L18 incomplete pinned provenance search
futon2 af2397966fdb25f8ad57a0d28cb92edaee64f9aa 2026-09-05T13:59:47Z L18 done-unreviewed v5: pinned, labeled provenance authorities (library root included, hard-fail on missing); receipt 2a1f6309 reproducible
futon2 72ce72716664f4f89ae43fc76646c5fff9fa6f94 2026-09-05T14:02:46Z library-loop: review L18 served-set grounding
futon2 029d2ed2a9f7a43413ad1e16f471014958a25de1 2026-09-05T14:04:46Z library-loop: reopen L19 value distribution
futon2 7767b510dd03554775b2a54f7f1fe70bb2e3fc9f 2026-09-05T14:05:47Z L19 done-unreviewed v2: @how value distribution with values/edges/patterns distinguished (21 resolving + 534 prose values)
futon2 3111f9067339e35350df035c3876f1f4a4d1ae40 2026-09-05T14:08:02Z library-loop: review L19 how-side measurement
futon2 04dcaa876ebb855f9a93cff5592f23e8caaf80d4 2026-09-05T14:09:08Z :F8 leg 1 slice 3 -- the belief update mu-next, declared and reviewed
futon2 ce99c5b90f93e65cf813adfc383253f7e901fde7 2026-09-05T14:09:30Z bulletin: the day's digest (build loop end-of-session, :B1)
futon2 d1ca662823ed7530d4f2f5ef7164c34f7c0a76b0 2026-09-05T14:10:14Z wm-contract: workflow-report snapshot (wm-build-loop)
futon2 0cbb6f6b022f6264eed88ce6b8416a44ed36037c 2026-09-05T14:10:18Z Theory-track coda: three rounds complete, mechanical frontier honest, authoring round offered to Joe
futon2 e02b3195323ffab0ea9414610360713f079707ae 2026-09-05T14:10:31Z bulletin: the day's digest (build loop end-of-session, :B1)
futon2 928b898c90075c5fad534a516c855e1049174700 2026-09-05T14:20:29Z Add policy free energy production readback
futon2 a776e2c9537b340228720b360086005736c1732c 2026-09-05T14:32:15Z :F8 leg 1 slice 4: declare machinePolicyFreeEnergy as the F_pi carrier
futon2 52ba60629e7d2de6c82e911ab5c03fa2554a259f 2026-09-05T14:33:20Z wm-contract: workflow-report snapshot (wm-build-loop)
futon2 0b7cf967e3bbca8d5536d13b3a57f925c196930a 2026-09-05T14:48:22Z :F8 leg 1: sweep all 18 rows' :code pointers against HEAD
futon2 e875314ec55edba4b9383a572a98036df55101b6 2026-09-05T14:51:28Z :F8 ledger: record the :code pointer sweep slice
futon2 f2083a97ea9ab2445853fe3b714580126ae1df2f 2026-09-05T14:52:27Z wm-contract: workflow-report snapshot (wm-build-loop)
futon2 402b8cfdfe5ed75ee1e658a8e2f88da7d5bc5063 2026-09-05T15:01:20Z Add structured observation production readback
futon2 d7a274f747587c46087ed3bc78fab4dddd336a91 2026-09-05T15:12:02Z :F8 leg 1 slice 5: declare machineObservation as the o carrier
futon2 b686d1afbb2714e4c246662f59a064dc0cc6caf8 2026-09-05T15:12:18Z :F8 ledger: resolve slice 5's C523 sha to d7a274f7
futon2 a35818ef3d7ffadd3decf72578c35aad21f659d2 2026-09-05T15:14:29Z wm-contract: workflow-report snapshot (wm-build-loop)
futon2 5a553275e6611ec264b25fd2516fcace500677be 2026-09-05T15:24:07Z :F8 leg 1 slice 6: read back stored belief
futon2 f66fdefb83827b6ab01729e385fc0c814cc1a21a 2026-09-05T15:31:33Z :F8 leg 1 slice 6: declare machineBeliefState on the :belief-state row
futon2 c4224cd3b2a71a098d8726b8da47658b1b794e34 2026-09-05T15:32:50Z :F8 ledger: leg 1 slice 6 (:belief-state) recorded, row stays :open
futon2 39c372720518182e4da1c03c460024361b23183f 2026-09-05T15:34:05Z wm-contract: workflow-report snapshot (wm-build-loop)
futon2 4deb8138251937ba003e514f619e6d16dd5e6494 2026-09-05T15:46:10Z F8 slice-7 coordination incident recorded; U65 pending mint (registered seat callers, job-grain cancel)
futon2 af438f19ded56e0d16afa65d255f3ac26d437262 2026-09-05T15:57:29Z :F8 leg 1 slice 7: declare machineDepth as the T carrier
futon2 6e2ddde56cfb570bd64f05c55a723ae1659a8888 2026-09-05T15:58:36Z :F8 ledger: leg 1 slice 7 (:depth) recorded, row stays :open
futon2 289a3f827f2fae3504e827df9430fbcb724ea930 2026-09-05T15:59:39Z wm-contract: workflow-report snapshot (wm-build-loop)
futon2 80a1a7a8544e151ccab55655a78009881e123d6f 2026-09-05T16:19:27Z :F8 leg 1 slice 8: declare machineTemperature as the tau carrier
futon2 e9c3c00d1100799404306056832c371892398217 2026-09-05T16:21:32Z :F8 ledger: leg 1 slice 8 (:temperature) recorded, row stays :open
futon2 5eaa36d56aa97829f7c4c267093e481929de9439 2026-09-05T16:22:45Z wm-contract: workflow-report snapshot (wm-build-loop)
futon2 4f1cc81ee48cb0878b7bf0c8f3e5b5e46c34f45f 2026-09-05T16:29:21Z :F8 leg 1 slice 9: read back selected action
futon2 13dbb58cdccc5f320cd1e2fb9773351fb6bedfc2 2026-09-05T16:42:22Z :F8 leg 1 slice 9: declare machineAction as the u carrier
futon2 21de2c53c605ae87a51c06d6435af291e7a1d87a 2026-09-05T16:44:07Z :F8 ledger: leg 1 slice 9 (:action) recorded, row stays :open
futon2 6393bb93f0f899df16dd7eb37551b83200d6fa24 2026-09-05T16:45:08Z wm-contract: workflow-report snapshot (wm-build-loop)
futon2 579bdecaddb27ba5d4ad7f23c6a5a73eac2f7f13 2026-09-05T16:51:46Z :F8 leg 1 slice 10: independent probe of the A4a accumulation, before dispatch
futon2 b26b5f4df75145bfdeb9caa3c8c08911153e870b 2026-09-05T16:55:59Z :F8 leg 1 slice 10: read back Dirichlet recount
futon2 561b18c5b4adcd89a4059ae2dd152e3858478a33 2026-09-05T17:12:46Z :F8 leg 1 slice 10 review: annotate the R17 row, mint :C27, and rewrite the readback
futon2 43b850ed3f2d1764c7967ba10e08a3d0c9b09c5c 2026-09-05T17:14:27Z wm-contract: workflow-report snapshot (wm-build-loop)
futon2 a538b9a7e005ba1a27c16e572d6e771b26408e97 2026-09-05T17:19:31Z F8 leg 2 slice 1: independent symbol-occurrence probe, run before the dispatch
futon2 9cbb1f626e2219f534189033d3d8f1ead92a83e2 2026-09-05T17:22:55Z wm-contract: workflow-report snapshot (wm-build-loop)
futon2 c49f07eda382787cc66c2d1f282d94ff88772964 2026-09-05T17:29:01Z F8 leg 2 slice 1: census symbol concordance
futon2 eeedf2de2deef996ac7ce5ae41457c1a5bff51ff 2026-09-05T17:33:27Z :F8 leg 2 slice 1 review: resolve every concordance pointer, repair three
futon2 88813560d802ada6f1481a6b36f6cc29b5c9b335 2026-09-05T17:35:49Z :F8 ledger: leg 2 slice 1 (symbol concordance census) recorded, row stays :open
futon2 f370ceccdf67aa361bb03240aa0156ea527b5152 2026-09-05T17:36:48Z wm-contract: workflow-report snapshot (wm-build-loop)
futon2 f4cf278ebcbdda1e5411bc1a58f20559b64f416a 2026-09-05T17:40:42Z F8 leg 2 slice 2: reviewing seat's independent probe, before the dispatch
futon2 f48a57c85704b0bd4c3b086ecdb0c247558c5953 2026-09-05T17:46:10Z F8 leg 2 slice 2: the reviewing seat's plant suite and the widening measurement, before the delivery
futon2 bd206447f0b1ea9c8879b610ec7636c72c851f71 2026-09-05T17:51:01Z F8 leg 2 slice 2: add refusing symbol checker
futon2 41804a78b7dbb61337477011b880f6db353d6a26 2026-09-05T18:02:03Z wm-contract: workflow-report snapshot (wm-build-loop)
futon2 3dcba597a21cda670ba8a153f2c840e6d410d6ca 2026-09-05T18:13:46Z F8 leg 2 slice 2 review: two refusals the checker was missing, and the counts it mislabelled
futon2 2f67f7c11248ab7e8b99bf0ab0d6cd6fb5213dae 2026-09-05T18:15:54Z :F8 ledger: leg 2 slice 2 (the refusing symbol checker) recorded, row stays :open
futon2 d81303012188e40ad60796648f23270b7253227b 2026-09-05T18:18:20Z wm-contract: workflow-report snapshot (wm-build-loop)
futon2 848fa3833c780f4f327c9f623ae9b7e23faba79b 2026-09-05T18:22:38Z F8 leg 3 slice 1: independent probe of CONVERGENCE-draft.edn, before the dispatch
futon2 b40c81cd28554b3f0442c599f796d5e6a9da3c44 2026-09-05T18:29:49Z F8 leg 3 slice 1: adopt convergence ledger
futon2 8f6a39b1d46d67d8e74128974877ffcf48deca49 2026-09-05T18:37:50Z F8 leg 3 slice 1 review: the one rung above formula-transcribed rested on a rule that rejects it
futon2 e534c191bfc099bf08b0a6f335efdf00edf57022 2026-09-05T18:38:57Z :F8 ledger: leg 3 slice 1 (adopt the convergence ledger) recorded, row stays :open
futon2 46c113400a909994fca3908fe980383c7623751e 2026-09-05T18:39:20Z :F8 ledger: drop a literal planted pointer from the leg 3 slice 1 evidence
futon2 c3cc9e8ac4ff1e5e21b894b4cf1d7f572d435c2a 2026-09-05T18:41:50Z wm-contract: workflow-report snapshot (wm-build-loop)
futon2 a401ecd225e6d0d572f4fda21be329c4b3651ad5 2026-09-05T18:45:00Z F8 leg 3 slice 2: reviewing seat's independent probe, committed before dispatch
futon2 19accec9757cc0b0f9d6864273c6809dea807ee5 2026-09-05T18:47:14Z F8 leg 3 slice 2: adversarial plant suite, committed before the delivery was read
futon2 3526b79e134922d31831fd178316eba88d299d66 2026-09-05T18:55:09Z F8 leg 3 slice 2: add refusing convergence checker
futon2 efc43bb838bf9bf2570388103b9814241cdd0c1d 2026-09-05T19:08:29Z F8 leg 3 slice 2 review: pin the ledger's frame, tie the nested-identity policy to its obligation, count the counted numbers
futon2 96b84d7d62d752052254512bd9d36ca3376a4032 2026-09-05T19:09:16Z F8 leg 3 slice 2: plant-suite output after the review repairs
futon2 386d3baa3d19288f8bbdf28ad1b1732c58fd5072 2026-09-05T19:10:10Z F8 leg 3 slice 2: ledger row -- checker slice done, all three legs complete, row stays open
futon2 45ca092943ccd1bb3e458d06723322eef48b8978 2026-09-05T19:18:02Z wm-contract: workflow-report snapshot (wm-build-loop)
futon2 338e0ee31ef4d23f04bcfcda5d9d11a8fc8a0902 2026-09-05T19:27:32Z F8: blocked on two reviewer decisions -- three legs complete, zero quantities converged
futon2 2e0f2cc41a96a7477030f634af318918670d029d 2026-09-05T19:31:31Z wm-contract: workflow-report snapshot (wm-build-loop)
futon2 adbf53fa04a1590ea4c3d1a1f33ebaf68a96a91d 2026-09-05T20:20:59Z :F9: wire the cascade lane to the committed decision, advisory lane on
futon2 ba2ef01c54b0c4392c05c4c243e5215123379621 2026-09-05T20:21:53Z :F9: ledger row -- done-unreviewed, cascade lane wired to the decision
futon2 9ab2d89bc37228504b812cf34dbe4bb22411bc0b 2026-09-05T20:29:15Z :F9: review cascade decision wiring
futon2 484dbeb162d962c614ee3eab6250e61a17a8acef 2026-09-05T20:33:17Z wm-contract: workflow-report snapshot (wm-build-loop)
futon2 8084475e050d61ab51c7c05570ac8e8ea4a9ef4e 2026-09-05T20:40:08Z :C27: second read -- sign the three keys C8 covered, at the slice-10 state
futon2 1b8652c91d7ad00fe1c628ca3e8d54483910882f 2026-09-05T20:44:21Z wm-contract: workflow-report snapshot (wm-build-loop)
futon2 0be3fa0c1ce59a858c6423b5a1854ff003988d20 2026-09-05T20:54:22Z :C26: second read -- sign the :precision entry as it now reads
futon2 67732b03f488444ff9ae00396af5035ce298646e 2026-09-05T20:58:24Z wm-contract: workflow-report snapshot (wm-build-loop)
futon2 4c62c776e0a9ed8474b907e9f41c951a2029ccd3 2026-09-05T21:15:39Z RE6: split the fold vocabulary, and label the fourteen standing absences
futon2 535ca06e4e0f5636f7132a725bb0c5b33462e3bc 2026-09-05T21:16:36Z RE6: ledger row -> :done-unreviewed with the fold-split evidence
futon2 a841d897ae143f3f9995d1a1a89db5949abfb482 2026-09-05T21:22:55Z RE6: review fold vocabulary split
futon2 fef0d2674c7397cdbb4b43d9cf12afbcc90c053a 2026-09-05T21:26:51Z wm-contract: workflow-report snapshot (wm-build-loop)
futon2 09b92d7ee8bb9f1b31d17963dae5bf5755cec5a9 2026-09-05T21:40:09Z U64: the rollout parameters' provenance -- three carried from a witness, one commissioned
futon2 6e663696d425ae5094cc2ad7173b81c1c71e3e2d 2026-09-05T21:41:04Z worklist: U64 -> :done-unreviewed (rollout parameter provenance recovered; three carried from a witness, one commissioned)
futon2 5c57a82d12f23062eb315fd664e5c66b6d6db196 2026-09-05T21:48:07Z U64: review rollout parameter provenance
futon2 1ab52e70e72bbacbb1dbaebd0bcf7ef73ff7b5c2 2026-09-05T21:52:03Z wm-contract: workflow-report snapshot (wm-build-loop)
futon2 b6821bfce53a9726a175fa0db613fe8e6f73a0a7 2026-09-05T21:56:25Z wm-contract: workflow-report snapshot (wm-build-loop)
futon2 3265705b7eaef87677983754099671e077137d8e 2026-09-05T21:57:26Z bulletin: the day's digest (build loop end-of-session, :B1)
futon2 d94cf594a0cdfbaa602866085e797da1d34ec61b 2026-09-05T22:00:33Z F8 signed done (owner review, both decisions taken); I4 closed via F9; mint RUN13 + U65
futon2 59d8a21b156e41b710b5890333984cedaeced189 2026-09-05T22:23:42Z U65(a): the loop's seats get their own registered Agency identity
futon2 fa3b9238fc845e4b5f9d82b750c45b47578867dc 2026-09-05T22:25:45Z U65 done-unreviewed: registered seat callers + job-grain cancel
futon2 20c7b2af0355347d88cdf57d7459d6a43fc59e2d 2026-09-05T22:34:45Z U65 review: verify job-grain cancellation
futon2 563270b5d4130a7b86d5730e40ac1541432e08d1 2026-09-05T22:38:41Z wm-contract: workflow-report snapshot (wm-build-loop)
futon2 e0a49d01977ed6f6335f3a6e41c841d424e8f8f1 2026-09-05T22:42:46Z wm-contract: workflow-report snapshot (wm-build-loop)
futon2 b2e9469f4e460eac60ebd64631b62a88cb3dc026 2026-09-05T22:43:47Z bulletin: the day's digest (build loop end-of-session, :B1)
futon2 a22b7527d30b0eba54d351418f729e8fad84fb62 2026-09-05T22:44:39Z RUN13 blocker reworded: run-gated wording without needs-joe channel false-positive
futon2 49c4c4666223aa5b16686f58199d672641974ebe 2026-09-05T22:45:40Z bulletin: the day's digest (build loop end-of-session, :B1)
futon2 a0dcf797d6ea79e9e4ee0ec805792ab008be01ef 2026-09-05T22:45:54Z Day-end state: boards drained to the run boundary; live run held for morning on the census's own R20/T7 grounds
futon3 e01cbbcddb89edbcfc2045ebc85d760b13a263e3 2026-09-05T12:07:45Z library: L6 backfill v2 -- complete-corpus match (futonic-logic, reverse-morphogenesis gain @why edges; 62 no-source notes)
futon3 5c0b7371e819d61143cbc322d35a33b5288a8b3e 2026-09-05T12:11:23Z library: L7 rationale backfill -- ukrns + snatch + vsatlas + storage (92 patterns annotated, complete-corpus match, 0 sources found)
futon3 d26ae2a0c1281045572dd71de55bda14798e811b 2026-09-05T12:15:34Z library: L7 v2 -- no-source notes repointed to the L7 receipt; two pre-L7 curated edges gain provenance source pointers
futon3 2a91028b995fc9d8217313846054c94fbaca06ee 2026-09-05T12:19:54Z library: L8 rationale backfill -- math-formalization + -CA + math-informal + math-strategy (97 patterns, grounding corpus problems+futon-theory, 0 matches found)
futon3 54d58f6731dd86d957f5770820d4a63e7d25c3c3 2026-09-05T12:24:36Z library: L8 v2 -- 83 annotations repointed to runs/L8 receipt (LL8 bug fixed at generator); swept file body sourced via @provenance
futon3 fccd14e9f706e85f4ea33a1186060adeac64d659 2026-09-05T12:30:36Z library: L9 rationale backfill -- coherence family (76 patterns, 0 corpus sources found)
futon3 5704359975a41ff125ffa94f7ce227dcf5d53217 2026-09-05T12:33:49Z library: L10 rationale backfill -- peeragogy + or3 + musn + plos-npt-with-small-n + agency + agent (99 patterns, 0 corpus sources found)
futon3 c0b001cb8308804721dad51c01cc2f7a6e665cc3 2026-09-05T12:57:37Z library: L14 edge-resolution pass -- 35 resolvable @why edges added (holds/named bases only, no invention)
futon3 64436d7efb815994b69d6c7d2b2b3ac28f406d68 2026-09-05T13:02:09Z library: L14 v2 -- 37 resolvable edges (all @holds-at tokens parsed, incl. wr-24 R13 R15 and wr-25 R9 R12)
futon3 a43f0280d046d2c9e38296d2753455f2d435e1d8 2026-09-05T13:07:30Z library: L15 -- exotype-encoding-programme problem node + shared @why edge on 256 exotype records; TEMPLATE excluded from census (renamed .txt)
futon3 9a6e3d5a295db230931f49784356dcc9041a9168 2026-09-05T13:09:48Z library: keep iiching template instructions consistent with L15 exclusion
futon3 835278bc4bbd9fd2396b112c3f614a7d43246293 2026-09-05T13:16:53Z library: L16 -- 2 catalogue problem nodes (r5 EFE-label auditability, r17mint pattern genesis) + 1 holds edge; duplicate check: 6 of 8 covered
futon3 647bccf426117a5ffb8a1efef7a64d83b4e234ea 2026-09-05T13:35:22Z library: L18 -- 4 section-doc problem nodes (devmap, eight-gates, or3, baldwin) + 64 shared edges + own-mechanism @how
futon3 89e686d36c23de0f4bd1366da7f753d53c076b17 2026-09-05T13:40:41Z library: L18 v2 -- snatch-play-theory-gaps node from README gap assessment; refused snatch members edged
futon3 e58576cec0f14c3da4667ed452d522c561487ee8 2026-09-05T13:41:12Z library: L18 v2b -- 13 more snatch members edged (README names them explicitly)
futon3 50bd5309e8a64d0240eff6cf4d13aaf5a11a9611 2026-09-05T13:50:50Z library: L18 v3 -- 5 more section-doc problem nodes; worklist members in data-mining, cycle-machine, coordination, futon-theory, plos grounded
futon3c 5bc67c7668b72b60b12e2aaf7f5f7d06b9458a49 2026-09-05T12:34:13Z f87 park decision: partial apparatus failure
futon3c 96c0d7359ca9a7a67d62bdd6d525a5f5a80cb8f7 2026-09-05T13:40:13Z f88 park decision: partial apparatus failure
futon3c aec83f90098a3e73bb98ddf6ae1d2b03551729b9 2026-09-05T14:54:39Z f89 park decision: partial apparatus failure
futon3c cc94a92bcbb13bd0edca5d13a0f24ee50fcc964c 2026-09-05T15:32:27Z f90 park decision: partial apparatus failure
futon3c cae9015514a91b22c80749ec49f9a2c46996fed9 2026-09-05T16:23:40Z f91 park decision: partial apparatus failure
futon3c 14ba30f004d9fcde3e662a2d3ea83e2800337952 2026-09-05T17:24:59Z f92 park decision: partial apparatus failure
futon3c b2011fcab2f692021e409c88f377aba44ae8d355 2026-09-05T18:19:44Z f93 park decision: partial statement and apparatus failure
futon3c c57788a5d61afd3141805dfd2bd7526ff9ddd03d 2026-09-05T19:21:04Z f94 park decision: partial apparatus failure
futon3c 80f4ebc97ed770879c642ade895c5b739cc7aaab 2026-09-05T22:17:19Z U65(b): cancel a job, not the agent — job-grain interrupt + no post-cancel resurrection
futon3c bfe75f5a38d55fae60e29b01b5b32ee2854da41f 2026-09-05T22:27:47Z f97 park decision: partial apparatus failure
futon3c 9621cef0eea25d98a51433936d0ea0ea5be1c2ce 2026-09-05T23:16:20Z f98 park decision: partial apparatus failure
futon3c 14454233330c1cd24f5b4fcb01995e7bfa74410b 2026-09-06T00:20:37Z f99 park decision: partial apparatus failure
futon3c d8c71503ad5a5816e97fc004d4aaf6a65f462456 2026-09-06T01:48:23Z f100 park decision: partial apparatus failure
futon3c 274c3a6ecfb036d5fc0ae2d2764afd142b523cf6 2026-09-06T03:58:57Z f101 park decision: partial
futon3c 46822a5a29595014984eee536ab78020c7938084 2026-09-06T04:45:34Z f102 park decision: partial specification conflict
futon3c 83e6b80cb11644b6226e4a68b1632720369798d7 2026-09-06T06:00:46Z f103 park decision: partial apparatus failure
futon3c 1a7ba6fcdfea05eb0df39b57a4266f349a22dc1e 2026-09-06T07:04:40Z f104 park decision: partial apparatus failure
futon3c c9f61cef91cac3ce362c2eb610e3d2a027dc70f7 2026-09-06T08:47:59Z f105 park decision: partial retirement repair
futon3c 55e7a990e1b9cbe737465e7a40a9350052a5b6e6 2026-09-06T09:40:05Z f106 park decision: partial specification conflict
futon3c 5d21f4d595d14e5a78761c557cf084aab257def9 2026-09-06T10:33:11Z f107 park decision: partial specification conflict
futon3c e496e99aefb68e1d8df642e841197fc79beba5d9 2026-09-06T12:13:03Z f108 park decision: partial retirement repair
mathlib4 e2e8ee9649908a5564da6237b7d8def80a4bbaf5 2026-09-05T13:10:09Z F8 leg 1 slice 1: state the machine's precision map Pi in Lean
mathlib4 1282b75e3223d3f94d536ebabbb5b4125989722d 2026-09-05T13:34:16Z F8 leg 1 slice 2: state the machine's prediction error eps in Lean
mathlib4 d15325c004314547f2186f998ea33464f198549b 2026-09-05T13:52:15Z Specify machine belief update and witnesses
mathlib4 0e89cc1cb5622b252b43ced54dbfa65959043a5b 2026-09-05T14:02:01Z F8 leg 1 slice 3: name the belief-update carrier machineBeliefUpdate
mathlib4 3783d50968e21801198e8394135a770bf75f22f3 2026-09-05T14:19:50Z Specify machine policy free energy
mathlib4 d7a45a358acbc7680b44267032607216bf2a4b12 2026-09-05T14:25:42Z F8 leg 1 slice 4 review: drop the vacuous finiteness theorem, witness the carrier
mathlib4 169662b19653d20c76274bdce4845ab414da10cf 2026-09-05T15:00:54Z Specify structured machine observation
mathlib4 a4c2276d515730a32db27f97f1fd955a9bae1433 2026-09-05T15:07:06Z F8 leg 1 slice 5 review: state the two findings the first draft only defined
mathlib4 290ce8ae9f57dfc616d5e9b1d4b573885bdba735 2026-09-05T15:23:55Z :F8 leg 1 slice 6: specify stored belief
mathlib4 3a8e26f61e2ef3f4e96c92c96d39743442133db9 2026-09-05T15:27:55Z :F8 leg 1 slice 6 review: state the collision as one proposition, discharge Normalised
mathlib4 955bffc561f59fb89d9d05ae73f469dbb73c660c 2026-09-05T15:45:03Z :F8 leg 1 slice 7: state temporal depth
mathlib4 a4f5f77e9603c49e2af0bfc9d20e389a5527285a 2026-09-05T15:50:36Z :F8 leg 1 slice 7 review: name the machine's depths, state what the mixing does not do
mathlib4 9eef38b6a1117924fcf6476671ec21c3d4dd8a6b 2026-09-05T15:51:19Z :F8 leg 1 slice 7: name machineDepth, the carrier the registry row declares
mathlib4 d7c43bca25babdfe4b9e84b83a9bfec17ca801c5 2026-09-05T16:06:49Z :F8 leg 1 slice 8: state machine temperature
mathlib4 b31e5db2e494a0f11c4c35077c25d7399b1d19c9 2026-09-05T16:11:43Z :F8 leg 1 slice 8 review: state that the three laws are three, and gamma against the registry's own equation
mathlib4 c2ab8bb42b27fdb90032a8068929b06f529e7de7 2026-09-05T16:29:14Z :F8 leg 1 slice 9: state selected action
mathlib4 461720489008ecadc5eef01f4cc1c0c323e7b86f 2026-09-05T16:36:13Z :F8 leg 1 slice 9 review: state the argmax preservation, the production defaults, and the habit prior alone
mathlib4 6f5df367eca529f707461065b09e48b1237843b4 2026-09-05T16:55:53Z :F8 leg 1 slice 10: state Dirichlet accumulation divergence
mathlib4 4d89779d08ba625b8a9e1bc0a52cb42f19c567dc 2026-09-05T17:01:03Z :F8 leg 1 slice 10 review: state the unit-cell increment, the one-hot special case, and a repair predicate that refuses a recount
p4ng 0a95a1135ae2f8c0bd76f6355e53eef3ef3e49bf 2026-09-05T12:11:32Z futon-2026: regenerate (wm-build-loop, ledger clear)
p4ng 35c34aad8d6a8631968dd08eb70277167c74590a 2026-09-05T12:16:13Z futon-2026: regenerate (wm-build-loop, ledger clear)
p4ng 37aafa95bb80c1c1f277a568701ed786e81db15b 2026-09-05T12:17:17Z futon-2026: regenerate (wm-build-loop, ledger clear)
p4ng 4edf014847637087fac55255ff15bb40576f76ed 2026-09-05T12:46:16Z :F1 review: re-pin ancestry population control
p4ng 50ddbe245e75b8a052ce517aa5a04c3736ae3a3d 2026-09-05T12:47:42Z futon-2026: regenerate (wm-build-loop, ledger clear)
p4ng c128cbdef333dfc3bdc6c0ebe1f28e29b9c7a803 2026-09-05T12:57:40Z F8 leg 1 slice 0: re-render the U35 Lean-state section at the current corpus
p4ng 515bc51e92d02f2aa8b80c81c5d19e952c564dc1 2026-09-05T13:00:01Z futon-2026: regenerate (wm-build-loop, ledger clear)
p4ng 50fa2999c12cc6acfb16eb3259c6b59059475829 2026-09-05T13:15:56Z F8 leg 1 slice 1: re-pin negative_controls 10j after :C26 is minted
p4ng d968d10c0c4236d6b767f92fb791ba3bbc5fa0b1 2026-09-05T13:17:13Z futon-2026: regenerate (wm-build-loop, ledger clear)
p4ng e5fa8112247731aaa11dd76911208f6ca3855758 2026-09-05T13:56:19Z futon-2026: regenerate (wm-build-loop, ledger clear)
p4ng b47de7d3f189c3d8f62a6b34e82f733d3cb9097a 2026-09-05T14:10:14Z futon-2026: regenerate (wm-build-loop, ledger clear)
p4ng 2f51a51d522f5bd1d3b60fdec89d4ba072356ebb 2026-09-05T14:33:20Z futon-2026: regenerate (wm-build-loop, ledger clear)
p4ng a7b88e1fbf66d66b3797b1d79afa0349021d0017 2026-09-05T14:48:28Z :F8 :code sweep: re-anchor pointer controls 4e/4e2 off a repaired defect
p4ng f146debec546eba654e39d12ef334d2538edc342 2026-09-05T14:52:27Z futon-2026: regenerate (wm-build-loop, ledger clear)
p4ng 99f53eee25bb3a0b054a17188d5b159403e66388 2026-09-05T15:14:29Z futon-2026: regenerate (wm-build-loop, ledger clear)
p4ng cb493ec5e11bc61b6585d8caf1c2b653e263cd25 2026-09-05T15:34:05Z futon-2026: regenerate (wm-build-loop, ledger clear)
p4ng dea3004a38fbc89fcda911bdbfd9cc01c94fd260 2026-09-05T15:59:39Z futon-2026: regenerate (wm-build-loop, ledger clear)
p4ng 68f7cba30c9df464d29805aaa09a4ee9568be685 2026-09-05T16:22:45Z futon-2026: regenerate (wm-build-loop, ledger clear)
p4ng c8e8563bc43985768db8c86877cf09f5e25729fe 2026-09-05T16:45:08Z futon-2026: regenerate (wm-build-loop, ledger clear)
p4ng 0be176c7ecec0b96fe853c267fe839ac5890632c 2026-09-05T17:09:42Z :F8 leg 1 slice 10: re-pin the ancestry control for the C27 mint (4 -> 5 under jurisdiction)
p4ng a38fb166b165f02fb8814277694fc5063e31a0cb 2026-09-05T17:14:27Z futon-2026: regenerate (wm-build-loop, ledger clear)
p4ng 9c0f512dd98a6f6731a6cd2ab56364a58d23cfac 2026-09-05T17:22:55Z futon-2026: regenerate (wm-build-loop, ledger clear)
p4ng 41245fa40d08d1aea6e8e71460893739f2ed2c33 2026-09-05T17:36:48Z futon-2026: regenerate (wm-build-loop, ledger clear)
p4ng 9997cab692434d16c5c57377d94385dcbb42082e 2026-09-05T17:51:07Z F8 leg 2 slice 2: control symbol checker refusals
p4ng 0b5e2a6441fdd67a2531491a3063fa09c2826ccc 2026-09-05T18:02:03Z futon-2026: regenerate (wm-build-loop, ledger clear)
p4ng 2de1763bcd67025f90b6f109fc80493c62204193 2026-09-05T18:13:24Z F8 leg 2 slice 2 review: widen the pointer gate to the concordance, and make the extras reachable from a control
p4ng 7a2074a6106dd6b4bf2488996956c4ae44da0855 2026-09-05T18:18:20Z futon-2026: regenerate (wm-build-loop, ledger clear)
p4ng df767be6cace240173f17d41dac831a5377dc9a3 2026-09-05T18:37:39Z F8 leg 3 slice 1 review: put the convergence ledger inside the pointer gate
p4ng fa165bf5fdd20667eb731afda2da2bb23c799aa8 2026-09-05T18:41:50Z futon-2026: regenerate (wm-build-loop, ledger clear)
p4ng 978ae88b1f89267cd9dedc446c567b4aaa5acf98 2026-09-05T18:55:14Z F8 leg 3 slice 2: control convergence refusals
p4ng 68c0674de4b8ffaba3e7d6e17284ebf6ef9bc5cb 2026-09-05T19:08:35Z F8 leg 3 slice 2 review: four controls for the convergence ledger's frame
p4ng 29637312c304768a5f27a08eb4653a3849c6ff17 2026-09-05T19:18:02Z futon-2026: regenerate (wm-build-loop, ledger clear)
p4ng 155aeb50caa002944cde4cc71936fc2899553e0d 2026-09-05T19:31:31Z futon-2026: regenerate (wm-build-loop, ledger clear)
p4ng 97fb123bce376f08517ffc57cc6b6d017a3d00ca 2026-09-05T20:20:41Z :F9: negative_controls section 15 -- cascade target equality (125/51 -> 133/53)
p4ng 8caddb8de282320c8ec1aff3f9d57e6932cf23c6 2026-09-05T20:33:17Z futon-2026: regenerate (wm-build-loop, ledger clear)
p4ng b6434e555dfa5d27060cf3db502e4a3169f7315a 2026-09-05T20:44:21Z futon-2026: regenerate (wm-build-loop, ledger clear)
p4ng a95a83135cb77d9d006b207651ab997e15283967 2026-09-05T20:58:24Z futon-2026: regenerate (wm-build-loop, ledger clear)
p4ng 5f481ff7c8d9eff598b851c1ffac4a8bcf0751bc 2026-09-05T21:26:51Z futon-2026: regenerate (wm-build-loop, ledger clear)
p4ng 35519077641e1418668a5857fb1a167e0d03486a 2026-09-05T21:47:30Z U64: teach pointer check the rollout witness root
p4ng 8d7d8c414d25cea91b532796950aadce3b61425f 2026-09-05T21:52:03Z futon-2026: regenerate (wm-build-loop, ledger clear)
p4ng ea0c0225051b0ad6f4cd22f97eb8a7677096d7c0 2026-09-05T21:56:25Z futon-2026: regenerate (wm-build-loop, ledger clear)
p4ng 593928a9cd95609754e662fe9b4900b4ad40bd95 2026-09-05T22:23:29Z U65: re-pin the ancestry control for the RUN13/U65 mint (5 -> 7 under jurisdiction)
p4ng 8c9d59819506f0a86c7a0d8de2020d6a08896176 2026-09-05T22:34:03Z U65 review: resolve Agency implementation pointers
p4ng b0b80b922778b1be6bccbe9086aa469fb5e57605 2026-09-05T22:38:41Z futon-2026: regenerate (wm-build-loop, ledger clear)
p4ng 3f28d9065fd21a543aec63fc8b91964f204f9ad9 2026-09-05T22:42:46Z futon-2026: regenerate (wm-build-loop, ledger clear)
```

</details>

<details><summary>Expand outside reference files (literal matches, not all runtime dependencies)</summary>

```text
apm-lean/FutonPhase3Trial3.lean
futon0/CLAUDE.md
futon0/JOE.md
futon0/analysis/audits/SOURCES-work-records-2026-09-21.md
futon0/analysis/business-models/SCHEMA.md
futon0/analysis/business-models/SPINE.md
futon0/analysis/problems/STATEMENTS-v1.md
futon0/docs/excursion-planning.md
futon0/docs/stack-annotations-schema.md
futon0/holes/S-pattern-cascade.md
futon0/holes/missions/M-capability-star-map.md
futon0/holes/missions/M-futon-problems.md
futon0/holes/missions/M-futonzero-capability.md
futon0/holes/missions/M-futonzero-mvp.md
futon0/holes/missions/M-patterns-done-right.md
futon0/scripts/futon0/futonzero/observe.clj
futon0/scripts/futon0/futonzero/profile.clj
futon0/scripts/futon0/futonzero/trajectory.clj
futon1a/README-conventions.md
futon1a/docs/invariants.md
futon1a/docs/module-map.md
futon1a/docs/traceability.md
futon1a/src/futon1a/api/errors.clj
futon1a/src/futon1a/api/routes.clj
futon1a/src/futon1a/auth/penholder.clj
futon1a/src/futon1a/core/entity.clj
futon1a/src/futon1a/core/invariants.clj
futon1a/src/futon1a/core/mirror.clj
futon1a/src/futon1a/core/pipeline.clj
futon1a/src/futon1a/diag/health.clj
futon1a/src/futon1a/ingest/open_world.clj
futon1a/src/futon1a/model/descriptor_store.clj
futon1a/src/futon1a/model/registry.clj
futon1a/src/futon1a/model/type_registry.clj
futon1a/src/futon1a/model/validation.clj
futon1a/src/futon1a/scripts/repair.clj
futon1b/TN-futon1b-boot-incident-2026-08-13.md
futon1b/TN-pattern-duplication-findings.md
futon1b/docs/sigil-rejoin-2026-08-23.edn
futon1b/docs/sigil-rejoin-adjudication-2026-08-23.md
futon1b/docs/sigil-rejoin-resolution-2026-08-23.edn
futon1b/holes/DEFECT-bitemporal-as-of-two-routes.md
futon1b/holes/M-evidence-landscape-index.md
futon1b/hx-backfill-per-type.bb
futon1b/migration/export.clj
futon1b/seed/evidence-slice.edn
futon1b/seed/substrate-slice.edn
futon1b/textprobe/history-versions-full.edn
futon1b/textprobe/history-versions.edn
futon1b/textprobe/updates.edn
futon2/INSTALL.md
futon2/README-gamma.md
futon2/checks/absence-coercion-dispositions.edn
futon2/checks/contract_authority_current.clj
futon2/checks/dirichlet_accumulation_import_absence.clj
futon2/checks/policy_posterior_fpi_flagged_witness.clj
futon2/checks/r17_generator_disposer_check.clj
futon2/checks/r8_f_contract.clj
futon2/checks/witness-fragments/wmRunsOnce.edn
futon2/checks/witness-registry.edn
futon2/checks/wm_workspace_gate.clj
futon2/data/capability_zones/harvest-2026-07-19-3d.edn
futon2/data/capability_zones/harvest-2026-07-19.edn
futon2/docs/futon-aif-completeness.md
futon2/holes/E-aif-docs-live.md
futon2/holes/E-cascade-sampler-four-2026-08-26.md
futon2/holes/E-have-want-pairs.md
futon2/holes/E-live-loop-2.md
futon2/holes/E-live-loop-3.md
futon2/holes/E-operator-as-attached-agent.md
futon2/holes/E-precision-over-policies.md
futon2/holes/M-G-over-cascades.md
futon2/holes/M-aif-head.md
futon2/holes/M-aif2.md
futon2/holes/M-custom-harness.md
futon2/holes/M-futon1b-port.md
futon2/holes/M-goals-and-holes.md
futon2/holes/M-points-de-fuite.md
futon2/holes/M-strategic-mission-value.md
futon2/holes/M-wm-policies.md
futon2/holes/M-wm-substrate-1a-to-1b-port.md
futon2/holes/M-wm-three-factor-mission-value.md
futon2/holes/NOTE-slice4-slice5-understood.md
futon2/holes/TN-astra-wmreview.md
futon2/holes/TN-wm-failure-to-launch.md
futon2/holes/labs/A-next-a-sorry-enterprise/a-sorry-enterprise-sorry-EMPIRICAL.edn
futon2/holes/labs/A-next-agency-rebuild/agency-rebuild-sorry-EMPIRICAL.edn
futon2/holes/labs/A-next-autoclock-in/autoclock-in-sorry-CASCADE.edn
futon2/holes/labs/A-next-codex-agent-behaviour/codex-agent-behaviour-sorry-EMPIRICAL.edn
futon2/holes/labs/A-next-invariant-queue-unstuck/invariant-queue-unstuck-sorry-EMPIRICAL.edn
futon2/holes/labs/A-next-patterns-done-right/patterns-done-right-sorry-EMPIRICAL.edn
futon2/holes/labs/A-next-sorry-hyperedge-schema.md
futon2/holes/labs/A-next-stepper-calibration/stepper-calibration-sorry-EMPIRICAL.edn
futon2/holes/labs/A-next-typed-bells/typed-bells-sorry-EMPIRICAL.edn
futon2/holes/labs/E-operator-as-attached-agent/01-retrievals.edn
futon2/holes/labs/E-operator-as-attached-agent/02-census.edn
futon2/holes/labs/E-operator-as-attached-agent/FINDINGS-embedding-pipeline.md
futon2/holes/labs/E-operator-as-attached-agent/activations.edn
futon2/holes/labs/E-operator-as-attached-agent/phase-cross-2026-06-01.edn
futon2/holes/labs/E-operator-as-attached-agent/phase_window_cross.bb
futon2/holes/labs/M-aif-full-loop-68/TREE-ACTION-2026-09-21.md
futon2/holes/labs/M-evaluate-policies/defect-dG-nil-for-cascades.md
futon2/holes/labs/M-evaluate-policies/exhibit/fold-turn.edn
futon2/holes/labs/M-evaluate-policies/exhibit/m-evaluate-policies.clean.edn
futon2/holes/labs/M-legacy-sorry-cleanup/legacy-sorries-snapshot.edn
futon2/holes/labs/fold-gfn/reward.py
futon2/holes/labs/library-loop/DECISION-problem-authoring.md
futon2/holes/labs/library-loop/runs/A1-round1-receipt.md
futon2/holes/labs/library-loop/runs/A2-round2-receipt.md
futon2/holes/labs/library-loop/runs/A3-round3-receipt.md
futon2/holes/labs/library-loop/runs/A4-authoring-receipt.md
futon2/holes/labs/library-loop/runs/A5-authoring-receipt.md
futon2/holes/labs/library-loop/runs/L1-census-graph.edn
futon2/holes/labs/library-loop/runs/L1-census-receipt.edn
futon2/holes/labs/library-loop/runs/L1-census-report.md
futon2/holes/labs/library-loop/runs/L3-no-dossier-check.edn
futon2/holes/labs/library-loop/runs/W1-witness-survey.md
futon2/holes/labs/library-loop/runs/mining-exemplar/cascade.edn
futon2/holes/labs/library-loop/runs/mining-exemplar/receipt.md
futon2/holes/labs/library-loop/runs/mining-l2-policy-grain/cascade.edn
futon2/holes/labs/library-loop/runs/mining-p3-transport-boundary/cascade.edn
futon2/holes/labs/wm-contract/ALIGN-rnode-process-census.md
futon2/holes/labs/wm-contract/AUD-D1-findings.md
futon2/holes/labs/wm-contract/AUD-D2-findings.md
futon2/holes/labs/wm-contract/AUD-D5-findings.md
futon2/holes/labs/wm-contract/AUDIT-built-vs-pending-2026-09-19.md
futon2/holes/labs/wm-contract/C113-avoidance-unknown-safety-design.md
futon2/holes/labs/wm-contract/C140-reader-portability-lint.md
futon2/holes/labs/wm-contract/C163-diagnostic-tick-failure.edn
futon2/holes/labs/wm-contract/C182-immediate-absence-option-measurement.md
futon2/holes/labs/wm-contract/C186-diagnostic-evidence-growth.md
futon2/holes/labs/wm-contract/C206-cohort-cancellation-boundary.md
futon2/holes/labs/wm-contract/C215-evidence-occurrence-time.md
futon2/holes/labs/wm-contract/C223-construction-timeout.md
futon2/holes/labs/wm-contract/C253-click-envelope-blocker.md
futon2/holes/labs/wm-contract/C301-agency-snapshot-revision-design.md
futon2/holes/labs/wm-contract/C399-serving-topology-code-closure.md
futon2/holes/labs/wm-contract/C451-unexplained-drawn-edges.md
futon2/holes/labs/wm-contract/C452-undrawn-theory-edges.md
futon2/holes/labs/wm-contract/C454-free-choices.md
futon2/holes/labs/wm-contract/C461-beta-gamma-discovery.md
futon2/holes/labs/wm-contract/C462-f-pi-discovery.md
futon2/holes/labs/wm-contract/C465-shadow-run-discovery.md
futon2/holes/labs/wm-contract/C466-run-lock-negative-control.md
futon2/holes/labs/wm-contract/C467-trace-run-identity.md
futon2/holes/labs/wm-contract/C471-f-scalar-readers.md
futon2/holes/labs/wm-contract/C472-f-scalar-disposition.md
futon2/holes/labs/wm-contract/C474-cascade-order-discovery.md
futon2/holes/labs/wm-contract/C477-prediction-triple-migration.md
futon2/holes/labs/wm-contract/C478-belief-aggregation-typed-absence.md
futon2/holes/labs/wm-contract/C479-strategic-mode-typed-absence.md
futon2/holes/labs/wm-contract/C480-policy-sorry-pressure-typed-absence.md
futon2/holes/labs/wm-contract/C481-rollout-move-score-typed-absence.md
futon2/holes/labs/wm-contract/C482-unscored-move-refuse-floor.md
futon2/holes/labs/wm-contract/C483-fulab-temperature-typed-absence.md
futon2/holes/labs/wm-contract/C484-refusal-harvester.md
futon2/holes/labs/wm-contract/C487-u4-ambiguity-discrimination.md
futon2/holes/labs/wm-contract/C491-c-mis-v1.md
futon2/holes/labs/wm-contract/C494-u28-eoi-gauge-census.md
futon2/holes/labs/wm-contract/C495-U29-glossary-pointer-resolution.md
futon2/holes/labs/wm-contract/C496-r16-schema-code-resync.md
futon2/holes/labs/wm-contract/C501-h3-h4-falsifier-correction.md
futon2/holes/labs/wm-contract/C504-r3a-placement.md
futon2/holes/labs/wm-contract/C505-re6-typed-absence-blocker.md
futon2/holes/labs/wm-contract/C509-inter-tick-state-boundary.md
futon2/holes/labs/wm-contract/C509-inter-tick-state-census.edn
futon2/holes/labs/wm-contract/C510-stepper.md
futon2/holes/labs/wm-contract/C511-ladder-on-step.md
futon2/holes/labs/wm-contract/C511-repair-or-elaborate.md
futon2/holes/labs/wm-contract/C533-belly-node-history.md
futon2/holes/labs/wm-contract/C538-F10-outcome-domain-decision-sheet.md
futon2/holes/labs/wm-contract/C541-F12-o4-reachability.md
futon2/holes/labs/wm-contract/C548-F12-support-grain-arm.md
futon2/holes/labs/wm-contract/C554-F12-d2-denominator.md
futon2/holes/labs/wm-contract/C555-F12-d2-fifth-arm.md
futon2/holes/labs/wm-contract/C559-F11-f2-falsifier-reconciliation.md
futon2/holes/labs/wm-contract/C566-F11-remainder.md
futon2/holes/labs/wm-contract/C572-F12-o4-edges-constructibility.md
futon2/holes/labs/wm-contract/C576-F10-lean-carrier.md
futon2/holes/labs/wm-contract/C577-F10-runtime-fold.md
futon2/holes/labs/wm-contract/CASCADE-RUBRIC-SCORING-2026-09-12.md
futon2/holes/labs/wm-contract/CLEANUP-QUEUE.md
futon2/holes/labs/wm-contract/DECLARATION-WM13-C-composition-2026-09-19.md
futon2/holes/labs/wm-contract/DECLARATION-machine-aim-2026-09-20.md
futon2/holes/labs/wm-contract/DERIVE-ARGUE-C-realization-2026-09-09.md
futon2/holes/labs/wm-contract/DESIGN-tensions-as-patterns.md
futon2/holes/labs/wm-contract/EDGES-D1-census.md
futon2/holes/labs/wm-contract/FUNDAMENTALS.edn
futon2/holes/labs/wm-contract/NOTE-glossary-only-triage.md
futon2/holes/labs/wm-contract/NOUNS-D1-visibility.md
futon2/holes/labs/wm-contract/PHASE-COVERAGE-SURVEY-2026-09-20.md
futon2/holes/labs/wm-contract/PROBLEMS-assurance-band-batch6.md
futon2/holes/labs/wm-contract/PROBLEMS-r16-r13-r14-batch4.md
futon2/holes/labs/wm-contract/PreferenceStackWitness.edn
futon2/holes/labs/wm-contract/R19-preference-stack.edn
futon2/holes/labs/wm-contract/R6-glossary-formalisation.md
futon2/holes/labs/wm-contract/R8-D2-findings.md
futon2/holes/labs/wm-contract/R8-D2-report.edn
futon2/holes/labs/wm-contract/R8-D3-findings.md
futon2/holes/labs/wm-contract/R8-D3-report.edn
futon2/holes/labs/wm-contract/RECEIPT-typed-nil-selection-2026-09-19.md
futon2/holes/labs/wm-contract/REVIEW-typed-nil-selection-2026-09-19.md
futon2/holes/labs/wm-contract/RUNBOOK.md
futon2/holes/labs/wm-contract/SESSION-model-choices-2026-09-09.md
futon2/holes/labs/wm-contract/SPEC-fundamentals-build-2026-09-12.md
futon2/holes/labs/wm-contract/SPEC-row22-r6-scoring-correspondence-2026-09-13.md
futon2/holes/labs/wm-contract/TN-F13-disposition-bridge-discovery-2026-09-15.md
futon2/holes/labs/wm-contract/TN-F13-runtime-seam-discovery-2026-09-15.md
futon2/holes/labs/wm-contract/TN-box3-machine-contract-join-discovery-2026-09-12.md
futon2/holes/labs/wm-contract/TN-fundamentals-four-link-trace-2026-09-15.md
futon2/holes/labs/wm-contract/TN-in-loop-interpretation-design-2026-09-15.md
futon2/holes/labs/wm-contract/TN-paper05-09-11-r20-closure-path-2026-09-15.md
futon2/holes/labs/wm-contract/TN-paper13-paper07-closure-path-2026-09-15.md
futon2/holes/labs/wm-contract/TN-row13-live-accumulation-2026-09-12.md
futon2/holes/labs/wm-contract/TN-row14-discovery-2026-09-12.md
futon2/holes/labs/wm-contract/TN-row14-live-wiring-blocker-2026-09-14.md
futon2/holes/labs/wm-contract/TN-row15-selector-spec-2026-09-12.md
futon2/holes/labs/wm-contract/TN-row16-capture-family-2026-09-12.md
futon2/holes/labs/wm-contract/TN-row17-discovery-2026-09-12.md
futon2/holes/labs/wm-contract/TN-row19-scoping-2026-09-12.md
futon2/holes/labs/wm-contract/TN-row19-serving-retention-deployment-2026-09-13.md
futon2/holes/labs/wm-contract/TN-ticket-selection-prediction-blocker-2026-09-15.md
futon2/holes/labs/wm-contract/TN-wm-lean-attestation-audit-2026-09-12.md
futon2/holes/labs/wm-contract/U78-citation-revet-2026-09-08.md
futon2/holes/labs/wm-contract/U79-eoi-producer-measurement.md
futon2/holes/labs/wm-contract/VERIFY-r-nodes.edn
futon2/holes/labs/wm-contract/audit-2026-09-19/d1-absent-value.edn
futon2/holes/labs/wm-contract/audit-2026-09-19/d2-key-list-drift.edn
futon2/holes/labs/wm-contract/audit-2026-09-19/d3-nil-as-identity.edn
futon2/holes/labs/wm-contract/audit-2026-09-19/d4-silent-miss.edn
futon2/holes/labs/wm-contract/audit-2026-09-19/d5-unbounded-write.edn
futon2/holes/labs/wm-contract/audit-2026-09-19/d6-stale-controls.edn
futon2/holes/labs/wm-contract/b2_strawman_experiments.clj
futon2/holes/labs/wm-contract/clojure-census/D1-g-cluster.edn
futon2/holes/labs/wm-contract/clojure-census/D2-selection.edn
futon2/holes/labs/wm-contract/clojure-census/D3-model-quartet.edn
futon2/holes/labs/wm-contract/clojure-census/D4-preference-carriers.edn
futon2/holes/labs/wm-contract/clojure-census/D5-f-and-q.edn
futon2/holes/labs/wm-contract/clojure-census/D6-precisions.edn
futon2/holes/labs/wm-contract/clojure-census/D7-remainder.edn
futon2/holes/labs/wm-contract/clojure-census/T3-claude4.edn
futon2/holes/labs/wm-contract/clojure-census/T4-claude4.edn
futon2/holes/labs/wm-contract/clojure-census/T6-claude4.edn
futon2/holes/labs/wm-contract/clojure-census/T7-claude4.edn
futon2/holes/labs/wm-contract/deposit-templates/T-addressed/subject-after.edn
futon2/holes/labs/wm-contract/deposit-templates/T-addressed/subject-before.edn
futon2/holes/labs/wm-contract/deposit-templates/T-addressed/supporting-resolved-before.edn
futon2/holes/labs/wm-contract/dirichlet-accumulation-import-absence.edn
futon2/holes/labs/wm-contract/f12_d2_denominator_check.clj
futon2/holes/labs/wm-contract/f12_d3_encoding_check.clj
futon2/holes/labs/wm-contract/f12_decision_sheet.bb
futon2/holes/labs/wm-contract/f12_mining_exemplar_check.bb
futon2/holes/labs/wm-contract/f12_o4_edges_constructibility.bb
futon2/holes/labs/wm-contract/f12_o4_reachability.clj
futon2/holes/labs/wm-contract/f12_o4_selected_rule_carriers.bb
futon2/holes/labs/wm-contract/f12_snatch_exemplar_check.bb
futon2/holes/labs/wm-contract/f3_node_sim.clj
futon2/holes/labs/wm-contract/f7_cascade_policy_decision.clj
futon2/holes/labs/wm-contract/facts-R14.md
futon2/holes/labs/wm-contract/facts-R2.md
futon2/holes/labs/wm-contract/facts-R5.md
futon2/holes/labs/wm-contract/facts-R6.md
futon2/holes/labs/wm-contract/flip_readiness_check.bb
futon2/holes/labs/wm-contract/harvest_refusals.bb
futon2/holes/labs/wm-contract/noun-census.edn
futon2/holes/labs/wm-contract/pair/R14-r5r14-round1.md
futon2/holes/labs/wm-contract/pair/R14-r5r14-round2.md
futon2/holes/labs/wm-contract/pair/R5-R14-delivery.edn
futon2/holes/labs/wm-contract/pair/R5-r5r14-round1.md
futon2/holes/labs/wm-contract/pair/R5-r5r14-round2.md
futon2/holes/labs/wm-contract/pair/R7-r2r7-round1.md
futon2/holes/labs/wm-contract/policy-posterior-fpi-flagged-witness.edn
futon2/holes/labs/wm-contract/proposals/STRAWMAN-M-aif-policy-conditioned-eig.md
futon2/holes/labs/wm-contract/proposals/STRAWMAN-M-wm-aif-policy-grain-compliance.md
futon2/holes/labs/wm-contract/re7_selection_discrimination.bb
futon2/holes/labs/wm-contract/review-prompt.md
futon2/holes/labs/wm-contract/runs/2026-09-01-s1b/wm-trace-s1b.edn
futon2/holes/labs/wm-contract/runs/2026-09-01-s2/wm-trace-s2.edn
futon2/holes/labs/wm-contract/runs/2026-09-01-s4/wm-trace-s4.edn
futon2/holes/labs/wm-contract/runs/2026-09-01-s5/wm-trace-s5.edn
futon2/holes/labs/wm-contract/runs/2026-09-01/wm-trace-2026-09-01.edn
futon2/holes/labs/wm-contract/runs/2026-09-04-010-accepted/README.md
futon2/holes/labs/wm-contract/runs/2026-09-04-010-accepted/wm-trace-2026-09-04.edn
futon2/holes/labs/wm-contract/runs/2026-09-04-010-accepted/world-after.edn
futon2/holes/labs/wm-contract/runs/2026-09-04-010-accepted/world-before.edn
futon2/holes/labs/wm-contract/runs/2026-09-04-re5/README.md
futon2/holes/labs/wm-contract/runs/2026-09-04-re5/wm-trace-re5.edn
futon2/holes/labs/wm-contract/runs/2026-09-05-u59-a/README.md
futon2/holes/labs/wm-contract/runs/2026-09-05-u59-a/delta.edn
futon2/holes/labs/wm-contract/runs/2026-09-05-u59-a/normalized.edn
futon2/holes/labs/wm-contract/runs/2026-09-05-u59-a/rationale/rationale-2026-09-05-feec6327-e0b0-41fc-9697-2fc46bff2830.edn
futon2/holes/labs/wm-contract/runs/2026-09-05-u59-a/wm-trace-2026-09-05.edn
futon2/holes/labs/wm-contract/runs/2026-09-05-u59-a/world-after.edn
futon2/holes/labs/wm-contract/runs/2026-09-05-u59-a/world-before.edn
futon2/holes/labs/wm-contract/runs/2026-09-05-u59-b/README.md
futon2/holes/labs/wm-contract/runs/2026-09-05-u59-b/delta.edn
futon2/holes/labs/wm-contract/runs/2026-09-05-u59-b/normalized.edn
futon2/holes/labs/wm-contract/runs/2026-09-05-u59-b/rationale/rationale-2026-09-05-62f229b5-14f2-442b-9358-d936e1dc05a5.edn
futon2/holes/labs/wm-contract/runs/2026-09-05-u59-b/wm-trace-2026-09-05.edn
futon2/holes/labs/wm-contract/runs/2026-09-05-u59-b/world-after.edn
futon2/holes/labs/wm-contract/runs/2026-09-05-u59-b/world-before.edn
futon2/holes/labs/wm-contract/runs/2026-09-05-u60/README.md
futon2/holes/labs/wm-contract/runs/2026-09-05-u60/delta.edn
futon2/holes/labs/wm-contract/runs/2026-09-05-u60/ladder/00-inputs.edn
futon2/holes/labs/wm-contract/runs/2026-09-05-u60/normalized.edn
futon2/holes/labs/wm-contract/runs/2026-09-05-u60/rationale/rationale-2026-09-05-3416e82b-771d-454d-8d4e-ae3d279cd23c.edn
futon2/holes/labs/wm-contract/runs/2026-09-05-u60/wm-trace-2026-09-05.edn
futon2/holes/labs/wm-contract/runs/2026-09-05-u60/world-after.edn
futon2/holes/labs/wm-contract/runs/2026-09-05-u60/world-before.edn
futon2/holes/labs/wm-contract/runs/B2-accounting-machine-union-2026-09-12/README.md
futon2/holes/labs/wm-contract/runs/B2-strawman/02-read-and-replay-probes.edn
futon2/holes/labs/wm-contract/runs/B2-strawman/RECEIPT.md
futon2/holes/labs/wm-contract/runs/E-live-strategic-discovery-2026-09-09.md
futon2/holes/labs/wm-contract/runs/F1-machine-q/01-runtime-seam.edn
futon2/holes/labs/wm-contract/runs/F1-machine-q/02-policy-family-census.edn
futon2/holes/labs/wm-contract/runs/F11-find/01-find-snatch-live.edn
futon2/holes/labs/wm-contract/runs/F11-find/14-dispatch.edn
futon2/holes/labs/wm-contract/runs/F11-find/15-dispatch.edn
futon2/holes/labs/wm-contract/runs/F12-organise/01-zaif-transcription.edn
futon2/holes/labs/wm-contract/runs/F12-organise/02-o4-reachability.edn
futon2/holes/labs/wm-contract/runs/F12-organise/06-attribution-arm.edn
futon2/holes/labs/wm-contract/runs/F12-organise/10-d3-encoding.edn
futon2/holes/labs/wm-contract/runs/F12-organise/11-d2-denominator.edn
futon2/holes/labs/wm-contract/runs/F12-organise/12-repoint.edn
futon2/holes/labs/wm-contract/runs/F12-organise/14-arms-dry.edn
futon2/holes/labs/wm-contract/runs/F12-organise/19-mining-exemplar.edn
futon2/holes/labs/wm-contract/runs/F12-organise/21-o4-edges-constructibility.edn
futon2/holes/labs/wm-contract/runs/F12-organise/22-o4-selected-rule-carriers.edn
futon2/holes/labs/wm-contract/runs/F12-organise/23-snatch-joint.edn
futon2/holes/labs/wm-contract/runs/F12-organise/26-second-joint.edn
futon2/holes/labs/wm-contract/runs/F12-organise/27-third-joint.edn
futon2/holes/labs/wm-contract/runs/F12-organise/28-fourth-joint.edn
futon2/holes/labs/wm-contract/runs/F12-organise/29-fifth-joint.edn
futon2/holes/labs/wm-contract/runs/F12-organise/30-sixth-joint.edn
futon2/holes/labs/wm-contract/runs/F12-organise/31-snatch-reproducibility.edn
futon2/holes/labs/wm-contract/runs/F12-organise/32-naturalistic-construction.edn
futon2/holes/labs/wm-contract/runs/F13-model-manifest-2026-09-15/README.md
futon2/holes/labs/wm-contract/runs/F13-model-manifest-2026-09-15/admission-refusal.edn
futon2/holes/labs/wm-contract/runs/F13-model-manifest-2026-09-15/partial-manifest.edn
futon2/holes/labs/wm-contract/runs/F13-model-manifest-2026-09-15/redo/assemble.py
futon2/holes/labs/wm-contract/runs/F13-model-manifest-2026-09-15/redo/interpreted-pattern-set.edn
futon2/holes/labs/wm-contract/runs/F13-model-manifest-2026-09-15/redo/target-selection.edn
futon2/holes/labs/wm-contract/runs/F2-run4-readiness/A4-recording-adoption-2026-09-09.md
futon2/holes/labs/wm-contract/runs/F2-run4-readiness/RUN4-execution-runbook-2026-09-09.md
futon2/holes/labs/wm-contract/runs/F3-node-sim/00-r5-pilot.edn
futon2/holes/labs/wm-contract/runs/F7-cascade-policy/f7-cascade-policy-decision.edn
futon2/holes/labs/wm-contract/runs/F7-cascade-policy/f7-single-cascade-decision.edn
futon2/holes/labs/wm-contract/runs/FLIP-READINESS.md
futon2/holes/labs/wm-contract/runs/ISSUE-BOARD-design-2026-09-09.md
futon2/holes/labs/wm-contract/runs/LEARNING-theory-history-runtime-2026-09-09.md
futon2/holes/labs/wm-contract/runs/R12-two-layer-calibration-2026-09-17/00-reconciliation.md
futon2/holes/labs/wm-contract/runs/R12-two-layer-calibration-2026-09-17/01-producer-consumer-wiring.md
futon2/holes/labs/wm-contract/runs/RE1-hole-count-reconciliation/README.md
futon2/holes/labs/wm-contract/runs/RE4-rationale-logging/README.md
futon2/holes/labs/wm-contract/runs/RE6-check-deposits/enumeration-completeness-2026-09-01-s5.edn
futon2/holes/labs/wm-contract/runs/RE6-check-deposits/enumeration-completeness-2026-09-04-010-accepted.edn
futon2/holes/labs/wm-contract/runs/RE6-check-deposits/enumeration-completeness-2026-09-04-re5.edn
futon2/holes/labs/wm-contract/runs/RE6-check-deposits/enumeration-completeness-2026-09-05-u59-a.edn
futon2/holes/labs/wm-contract/runs/RE6-check-deposits/enumeration-completeness-2026-09-05-u59-b.edn
futon2/holes/labs/wm-contract/runs/RE6-check-deposits/enumeration-completeness-2026-09-05-u60.edn
futon2/holes/labs/wm-contract/runs/RUN4-C-validation-2026-09-10/README.md
futon2/holes/labs/wm-contract/runs/RUN4-preparation-2026-09-10/EXECUTION-PATH.md
futon2/holes/labs/wm-contract/runs/RUN4-preparation-2026-09-10/G1-effective-consumer-review.md
futon2/holes/labs/wm-contract/runs/RUN4-preparation-2026-09-10/PREPARATION.md
futon2/holes/labs/wm-contract/runs/RUNTIME-VALIDATION-CATALOG.edn
futon2/holes/labs/wm-contract/runs/U12-c-mis-falsifier/node-fixtures/0a18c4f7-R5.edn
futon2/holes/labs/wm-contract/runs/U12-c-mis-falsifier/node-fixtures/0a18c4f7-R6.edn
futon2/holes/labs/wm-contract/runs/U12-c-mis-falsifier/node-fixtures/0a18c4f7-R9.edn
futon2/holes/labs/wm-contract/runs/U12-c-mis-falsifier/node-fixtures/4abad68c-R5.edn
futon2/holes/labs/wm-contract/runs/U12-c-mis-falsifier/node-fixtures/4abad68c-R6.edn
futon2/holes/labs/wm-contract/runs/U12-c-mis-falsifier/node-fixtures/4abad68c-R9.edn
futon2/holes/labs/wm-contract/runs/U12-c-mis-falsifier/node-fixtures/801976e7-R5.edn
futon2/holes/labs/wm-contract/runs/U12-c-mis-falsifier/node-fixtures/801976e7-R6.edn
futon2/holes/labs/wm-contract/runs/U12-c-mis-falsifier/node-fixtures/801976e7-R9.edn
futon2/holes/labs/wm-contract/runs/U16-outcome-semantics/measurements.edn
futon2/holes/labs/wm-contract/runs/U21-selection-focus/measurements.edn
futon2/holes/labs/wm-contract/runs/U23-cascade-catalog/carrier-population.edn
futon2/holes/labs/wm-contract/runs/U27-hole-closability/RUN4-input-refresh-2026-09-09.md
futon2/holes/labs/wm-contract/runs/U32-flip-readiness/flip-readiness.edn
futon2/holes/labs/wm-contract/runs/U37-enumeration-completeness/REPLAY-2026-09-02-RECORDS.md
futon2/holes/labs/wm-contract/runs/U37-enumeration-completeness/replay-2026-09-03.edn
futon2/holes/labs/wm-contract/runs/U40-first-retrospective/README.md
futon2/holes/labs/wm-contract/runs/U40-first-retrospective/u40-measurements.edn
futon2/holes/labs/wm-contract/runs/U41-tension-ledger/tension-report.edn
futon2/holes/labs/wm-contract/runs/U41-tension-ledger/u23-recheck/carrier-population.edn
futon2/holes/labs/wm-contract/runs/U42-producers/measurements.edn
futon2/holes/labs/wm-contract/runs/U43-focus-reconcile/cases.edn
futon2/holes/labs/wm-contract/runs/U52-ladder/00-inputs.edn
futon2/holes/labs/wm-contract/runs/U57-flip-readiness-capture/world-before.edn
futon2/holes/labs/wm-contract/runs/U58-runtime-validation-basis/world-before.edn
futon2/holes/labs/wm-contract/runs/U59-outcome-vocabulary/RECEIPT.md
futon2/holes/labs/wm-contract/runs/U60-structured-attribution/tension-ledger-planted.edn
futon2/holes/labs/wm-contract/runs/U84-trace-reason-census-2026-09-10/README.md
futon2/holes/labs/wm-contract/runs/V7-R1-node-sim/00-r1.edn
futon2/holes/labs/wm-contract/runs/V7-R13-node-sim/00-r13.edn
futon2/holes/labs/wm-contract/runs/V7-R14-node-sim/00-r14.edn
futon2/holes/labs/wm-contract/runs/V7-R2-node-sim/00-r2.edn
futon2/holes/labs/wm-contract/runs/V7-R20-node-sim/00-r20.edn
futon2/holes/labs/wm-contract/runs/V7-R3-node-sim/00-r3.edn
futon2/holes/labs/wm-contract/runs/V7-R3a-node-sim/00-r3a.edn
futon2/holes/labs/wm-contract/runs/V7-R4-node-sim/00-r4.edn
futon2/holes/labs/wm-contract/runs/V7-R6-node-sim/00-r6.edn
futon2/holes/labs/wm-contract/runs/V7-R7-node-sim/00-r7.edn
futon2/holes/labs/wm-contract/runs/V7-R8-node-sim/00-r8.edn
futon2/holes/labs/wm-contract/runs/WM-BACKLOG-2026-09-11/worklist.edn
futon2/holes/labs/wm-contract/runs/a-labels-exercise2-2026-09-14/observer-view/002-selection.blinded.edn
futon2/holes/labs/wm-contract/runs/a-labels-exercise2-2026-09-14/observer-view/003-construction.blinded.edn
futon2/holes/labs/wm-contract/runs/a-labels-exercise2-2026-09-14/observer-view/005-build.blinded.edn
futon2/holes/labs/wm-contract/runs/a-labels-exercise2-2026-09-14/observer-view/006-adjudication.blinded.edn
futon2/holes/labs/wm-contract/runs/a-labels-exercise2-2026-09-14/observer-view/close-conditioning.edn
futon2/holes/labs/wm-contract/runs/a-labels-exercise4-2026-09-14/observer-view/002-selection.blinded.edn
futon2/holes/labs/wm-contract/runs/a-labels-exercise4-2026-09-14/observer-view/003-construction.blinded.edn
futon2/holes/labs/wm-contract/runs/a-labels-exercise4-2026-09-14/observer-view/close-conditioning.edn
futon2/holes/labs/wm-contract/runs/a-labels-exercise5-2026-09-14/observer-view/002-selection.blinded.edn
futon2/holes/labs/wm-contract/runs/a-labels-exercise5-2026-09-14/observer-view/003-construction.blinded.edn
futon2/holes/labs/wm-contract/runs/a-labels-exercise5-2026-09-14/observer-view/005-build.blinded.edn
futon2/holes/labs/wm-contract/runs/a-labels-exercise5-2026-09-14/observer-view/006-adjudication.blinded.edn
futon2/holes/labs/wm-contract/runs/a-labels-exercise5-2026-09-14/observer-view/close-conditioning.edn
futon2/holes/labs/wm-contract/runs/a-labels-exercise6-2026-09-15/observer-view/003-construction.blinded.edn
futon2/holes/labs/wm-contract/runs/a-labels-exercise6-2026-09-15/observer-view/005-build.blinded.edn
futon2/holes/labs/wm-contract/runs/a-labels-exercise7-2026-09-15/observer-view/002-selection.blinded.edn
futon2/holes/labs/wm-contract/runs/a-labels-exercise7-2026-09-15/observer-view/003-construction.blinded.edn
futon2/holes/labs/wm-contract/runs/a-labels-exercise7-2026-09-15/observer-view/close-conditioning.edn
futon2/holes/labs/wm-contract/runs/a-labels-one-close-2026-09-14/observer-view/002-selection.blinded.edn
futon2/holes/labs/wm-contract/runs/a-labels-one-close-2026-09-14/observer-view/003-construction.blinded.edn
futon2/holes/labs/wm-contract/runs/a-labels-one-close-2026-09-14/observer-view/005-build.blinded.edn
futon2/holes/labs/wm-contract/runs/a-labels-one-close-2026-09-14/observer-view/006-adjudication.blinded.edn
futon2/holes/labs/wm-contract/runs/a-labels-one-close-2026-09-14/observer-view/close-conditioning.edn
futon2/holes/labs/wm-contract/runs/a-small-model-route-2026-09-19/41bdd7aa-b9ba-43ee-9dd3-313fe4a170ff.closure.edn
futon2/holes/labs/wm-contract/runs/a-small-model-route-2026-09-19/receipt.edn
futon2/holes/labs/wm-contract/runs/a-small-model-route-main-2026-09-19/9f1c9ff2-df71-488e-8dc2-08b869b0867b.closure.edn
futon2/holes/labs/wm-contract/runs/c-family-2026-09-20/ce8b6f70-cecb-4deb-9471-874ae01b8815.closure.edn
futon2/holes/labs/wm-contract/runs/c-family-2026-09-20/registry.edn
futon2/holes/labs/wm-contract/runs/cascade-fold-repair-2026-09-20/post-commit/52d2dca2-3887-46ca-9a93-8c1695dec135.closure.edn
futon2/holes/labs/wm-contract/runs/cascade-fold-repair-2026-09-20/review-followup-post/02ca4839-5879-44ea-8f59-2839c540814a.closure.edn
futon2/holes/labs/wm-contract/runs/cascade-realizer-2026-09-20/post-decision/86a5ecb9-a93e-404b-b3e1-e579f850c997.closure.edn
futon2/holes/labs/wm-contract/runs/cascade-realizer-2026-09-20/post-realizer/12a5969f-fa94-4799-9c30-17ba31f4ecf9.closure.edn
futon2/holes/labs/wm-contract/runs/cascade-realizer-2026-09-20/post-repair/8b9bfca2-dcb6-498e-9b2f-c36410d30482.closure.edn
futon2/holes/labs/wm-contract/runs/cascade-realizer-2026-09-20/post-runner/85213a15-8e21-4f6c-ba9a-3a7a4223ed8d.closure.edn
futon2/holes/labs/wm-contract/runs/cascade-realizer-2026-09-20/registration-decision.edn
futon2/holes/labs/wm-contract/runs/cascade-realizer-2026-09-20/registration-realizer.edn
futon2/holes/labs/wm-contract/runs/cascade-realizer-2026-09-20/registration-repair.edn
futon2/holes/labs/wm-contract/runs/cascade-realizer-2026-09-20/registration-runner.edn
futon2/holes/labs/wm-contract/runs/cascade-realizer-2026-09-20/registry-decision.edn
futon2/holes/labs/wm-contract/runs/cascade-realizer-2026-09-20/registry-realizer.edn
futon2/holes/labs/wm-contract/runs/cascade-realizer-2026-09-20/registry-repair.edn
futon2/holes/labs/wm-contract/runs/cascade-realizer-2026-09-20/registry-runner.edn
futon2/holes/labs/wm-contract/runs/cascade-realizer-closure-2026-09-20/post/eaeb4e03-7fa2-4e9d-859d-8cd31921104f.closure.edn
futon2/holes/labs/wm-contract/runs/cascade-realizer-closure-2026-09-20/registration.edn
futon2/holes/labs/wm-contract/runs/cascade-realizer-closure-2026-09-20/registry.edn
futon2/holes/labs/wm-contract/runs/certificate-v1-emitter-controls-2026-09-12/scratch/002-selection.edn
futon2/holes/labs/wm-contract/runs/certificate-v1-emitter-controls-2026-09-12/scratch/003-construction-digest-mismatch.edn
futon2/holes/labs/wm-contract/runs/certificate-v1-emitter-controls-2026-09-12/scratch/003-construction-divergence.edn
futon2/holes/labs/wm-contract/runs/certificate-v1-emitter-controls-2026-09-12/scratch/003-construction-typed-divergence.edn
futon2/holes/labs/wm-contract/runs/certificate-v1-emitter-controls-2026-09-12/scratch/003-construction.edn
futon2/holes/labs/wm-contract/runs/certificate-v1-emitter-controls-2026-09-12/scratch/aif-equations.edn
futon2/holes/labs/wm-contract/runs/certificate-v1-emitter-controls-2026-09-12/scratch/registry-missing.edn
futon2/holes/labs/wm-contract/runs/cohort-write-serialization-2026-09-20/7944fc27-2bdd-42c5-83d6-7f99eaf03576.closure.edn
futon2/holes/labs/wm-contract/runs/d-enactment-2c-discovery-2026-09-20/DISCOVERY.md
futon2/holes/labs/wm-contract/runs/d-task-authority-2c-2026-09-20/64e8dd6d-59f2-40f9-9961-1856ea5ccdc3.closure.edn
futon2/holes/labs/wm-contract/runs/d-task-authority-2c-2026-09-20/6cb84ebe-60a6-46b6-bd7d-8f53728247e8.closure.edn
futon2/holes/labs/wm-contract/runs/d-task-authority-2c-2026-09-20/78323e5d-a2f8-4585-832d-801d5855f79e.closure.edn
futon2/holes/labs/wm-contract/runs/d-task-authority-2c-2026-09-20/dc30b8d0-7458-42ce-856e-7459fd7d9313.closure.edn
futon2/holes/labs/wm-contract/runs/d-token-carry-2a-2026-09-20/41d1a2e4-35d5-459b-a817-382a5e1f64bb.closure.edn
futon2/holes/labs/wm-contract/runs/d-token-carry-2a-2026-09-20/67fbc466-18ee-44d0-9ae8-9465448bf316.closure.edn
futon2/holes/labs/wm-contract/runs/d-token-carry-2a-2026-09-20/b0818d3e-409a-491a-8af2-8d11567e81f2.closure.edn
futon2/holes/labs/wm-contract/runs/d-token-carry-2a-2026-09-20/registry-cascade-model-manifest.edn
futon2/holes/labs/wm-contract/runs/d-token-carry-2a-2026-09-20/registry-exact-belief-adapter.edn
futon2/holes/labs/wm-contract/runs/d-token-carry-2a-2026-09-20/registry-scoring-input-receipts.edn
futon2/holes/labs/wm-contract/runs/d-token-carry-2a-2026-09-20/registry-token-belief-carry.edn
futon2/holes/labs/wm-contract/runs/d-token-carry-2b-2026-09-20/4bfa3861-7f32-4b15-988d-ef2d1b5ce579.closure.edn
futon2/holes/labs/wm-contract/runs/d-token-carry-2b-2026-09-20/736f9597-3889-4a98-9335-0316f996b2fc.closure.edn
futon2/holes/labs/wm-contract/runs/d-token-carry-2b-2026-09-20/8efdd5fe-2a8f-4cc4-b830-0eb027a84c0d.closure.edn
futon2/holes/labs/wm-contract/runs/d-token-carry-2b-2026-09-20/registry-scoring-input-receipts.edn
futon2/holes/labs/wm-contract/runs/d-token-carry-2b-2026-09-20/registry-token-belief-carry.edn
futon2/holes/labs/wm-contract/runs/d-token-carry-2b-2026-09-20/registry-token-belief-predecessor.edn
futon2/holes/labs/wm-contract/runs/decision-guard-locators-2026-09-21/25054031-4bdd-45f1-9cd5-3b25c52da3ce.closure.edn
futon2/holes/labs/wm-contract/runs/decision-guard-locators-2026-09-21/561a7514-dbc2-407c-836d-cd00b0b50dbc.closure.edn
futon2/holes/labs/wm-contract/runs/decision-guard-locators-2026-09-21/75124456-67eb-4839-aa30-86d879a17d51.closure.edn
futon2/holes/labs/wm-contract/runs/decision-guard-locators-2026-09-21/EXECUTION.md
futon2/holes/labs/wm-contract/runs/decision-guard-locators-2026-09-21/ff6490b7-722f-4bbc-96c5-d56a2252fcc6.closure.edn
futon2/holes/labs/wm-contract/runs/declaration-reads-2026-09-19/18394b73-4f5d-4781-a7d4-87b61496667e.closure.edn
futon2/holes/labs/wm-contract/runs/declaration-reads-2026-09-19/registry.edn
futon2/holes/labs/wm-contract/runs/declared-scales-2026-09-20/4d55804d-9e59-4d2e-bd16-58399b74f2dc.closure.edn
futon2/holes/labs/wm-contract/runs/declared-scales-2026-09-20/registry.edn
futon2/holes/labs/wm-contract/runs/dismiss-superseded-attempt-2026-09-19/c8529f2e-9f48-46cc-a8b8-7ab98bd7951e.closure.edn
futon2/holes/labs/wm-contract/runs/eoi-declaration-2026-09-20/2c1d1762-6a6b-4e4b-876a-9ca26483373f.closure.edn
futon2/holes/labs/wm-contract/runs/fix-16-2026-09-21/11bc4b70-f797-4053-826f-659e6b005883.closure.edn
futon2/holes/labs/wm-contract/runs/fix-16-2026-09-21/README.md
futon2/holes/labs/wm-contract/runs/fix-16-2026-09-21/d08c48fc-35ae-4a7f-a70c-7778d460d8a2.closure.edn
futon2/holes/labs/wm-contract/runs/fix-16-2026-09-21/registry-admission-check-result.edn
futon2/holes/labs/wm-contract/runs/fix-16-2026-09-21/registry-admission-check.edn
futon2/holes/labs/wm-contract/runs/fix-16-2026-09-21/registry-admission-result.edn
futon2/holes/labs/wm-contract/runs/fix-16-2026-09-21/registry-admission.edn
futon2/holes/labs/wm-contract/runs/fix-16-2026-09-21/registry-decision-check-result.edn
futon2/holes/labs/wm-contract/runs/fix-16-2026-09-21/registry-decision-check.edn
futon2/holes/labs/wm-contract/runs/fix-16-2026-09-21/registry-decision-result.edn
futon2/holes/labs/wm-contract/runs/fix-16-2026-09-21/registry-decision.edn
futon2/holes/labs/wm-contract/runs/fix-18-2026-09-21/24e08269-4a18-4377-98d8-f185e9a11a3c.closure.edn
futon2/holes/labs/wm-contract/runs/fix-18-2026-09-21/c592f502-4544-47bf-92cf-416cbd253ad1.closure.edn
futon2/holes/labs/wm-contract/runs/fix-18-2026-09-21/registry-carry-result.edn
futon2/holes/labs/wm-contract/runs/fix-18-2026-09-21/registry-carry.edn
futon2/holes/labs/wm-contract/runs/fix-18-2026-09-21/registry-uniform-result.edn
futon2/holes/labs/wm-contract/runs/fix-18-2026-09-21/registry-uniform.edn
futon2/holes/labs/wm-contract/runs/fix-19-2026-09-21/owner-reload.clj
futon2/holes/labs/wm-contract/runs/fix-19-2026-09-21/probe.edn
futon2/holes/labs/wm-contract/runs/fix-19-2026-09-21/since-jvm-start/probe.edn
futon2/holes/labs/wm-contract/runs/fix-20-2026-09-21/a0321cfe-afad-4571-b93f-43bc9cdd218a.closure.edn
futon2/holes/labs/wm-contract/runs/fix-20-2026-09-21/registry-runner-result.edn
futon2/holes/labs/wm-contract/runs/fix-21-2026-09-21/README.md
futon2/holes/labs/wm-contract/runs/fix-7-2026-09-21/14b7083d-8af1-4c52-bdef-97361dc2c751.closure.edn
futon2/holes/labs/wm-contract/runs/fix-7-2026-09-21/registry-check-result.edn
futon2/holes/labs/wm-contract/runs/fix-7-2026-09-21/registry-result.edn
futon2/holes/labs/wm-contract/runs/fixlist-2026-09-21/fix-10-DISCOVERY.md
futon2/holes/labs/wm-contract/runs/fixlist-2026-09-21/fix-10e-VALIDATION.md
futon2/holes/labs/wm-contract/runs/fixlist-2026-09-21/fix-5-DISCOVERY.md
futon2/holes/labs/wm-contract/runs/fixlist-2026-09-21/fix-5b-BLOCKED.md
futon2/holes/labs/wm-contract/runs/fixlist-2026-09-21/fix-5c-SPEC.md
futon2/holes/labs/wm-contract/runs/fixlist-2026-09-21/fix-5c-evidence/manifest.edn
futon2/holes/labs/wm-contract/runs/fixlist-2026-09-21/fix-6-results.edn
futon2/holes/labs/wm-contract/runs/fixlist-2026-09-21/fix-9-DISCOVERY.md
futon2/holes/labs/wm-contract/runs/fixlist-2026-09-21/fix-9-evidence/checkpoints.edn
futon2/holes/labs/wm-contract/runs/fixlist-2026-09-21/fix-9-evidence/registered/accdd1d0-2b22-4550-b3ef-49c3db3b6934.closure.edn
futon2/holes/labs/wm-contract/runs/fixlist-2026-09-21/fix-9-evidence/registration.edn
futon2/holes/labs/wm-contract/runs/fixlist-2026-09-21/improve-1-results.edn
futon2/holes/labs/wm-contract/runs/fundamentals-checklist-2026-09-15/futon2__holes__M-G-over-cascades.md
futon2/holes/labs/wm-contract/runs/fundamentals-checklist-2026-09-15/futon2__holes__labs__wm-contract__FUNDAMENTALS.edn
futon2/holes/labs/wm-contract/runs/fundamentals-checklist-2026-09-15/futon2__holes__labs__wm-contract__TN-fundamentals-four-link-trace-2026-09-15.md
futon2/holes/labs/wm-contract/runs/fundamentals-checklist-2026-09-15/futon2__holes__labs__wm-contract__TN-paper13-paper07-closure-path-2026-09-15.md
futon2/holes/labs/wm-contract/runs/fundamentals-checklist-2026-09-15/mathlib4__DarkTower__WarMachine__Holes.lean
futon2/holes/labs/wm-contract/runs/fundamentals-checklist-2026-09-15/p4ng__empirics-futon__control-stages.edn
futon2/holes/labs/wm-contract/runs/fundamentals-checklist-2026-09-15/priority-1-owner-proposal.md
futon2/holes/labs/wm-contract/runs/g-term-decomposition-2026-09-19/78a7d898-364c-4be7-a345-020967df097c.closure.edn
futon2/holes/labs/wm-contract/runs/g-term-decomposition-2026-09-19/check-result.edn
futon2/holes/labs/wm-contract/runs/g-term-decomposition-2026-09-19/receipt.edn
futon2/holes/labs/wm-contract/runs/g-term-decomposition-2026-09-19/warrant.edn
futon2/holes/labs/wm-contract/runs/h3-precision-discovery-2026-09-21/DISCOVERY-PROPOSAL.md
futon2/holes/labs/wm-contract/runs/h3-precision-learning-2026-09-21/13aae401-51b9-48ad-a4f4-e735cd42b88b.closure.edn
futon2/holes/labs/wm-contract/runs/h3-precision-learning-2026-09-21/8e7420e4-f4f7-4fc5-bf4b-a2bc7c9e38f4.closure.edn
futon2/holes/labs/wm-contract/runs/h3-precision-learning-2026-09-21/9959a7af-9ea6-4897-8f59-5ffbaf5f5d98.closure.edn
futon2/holes/labs/wm-contract/runs/h3-precision-learning-2026-09-21/ec6f6d6b-7598-4f5a-bfa3-942078194e3b.closure.edn
futon2/holes/labs/wm-contract/runs/h3-precision-learning-2026-09-21/registry-cascade-decision.edn
futon2/holes/labs/wm-contract/runs/h3-precision-learning-2026-09-21/registry-policy-precision-carry.edn
futon2/holes/labs/wm-contract/runs/h4-staged-prefix-2026-09-21/13df14a2-bc10-4425-9ff7-91861c088e0d.closure.edn
futon2/holes/labs/wm-contract/runs/h4-staged-prefix-2026-09-21/28db391f-ce9c-4cce-a783-75c03b056671.closure.edn
futon2/holes/labs/wm-contract/runs/h4-staged-prefix-2026-09-21/2be7822c-46f1-463d-8506-553c282475e3.closure.edn
futon2/holes/labs/wm-contract/runs/h4-staged-prefix-2026-09-21/63381f1c-6b75-4a35-aa90-2ee3309b4723.closure.edn
futon2/holes/labs/wm-contract/runs/h4-staged-prefix-2026-09-21/72d50637-4670-4b73-b389-ab66ee31dd81.closure.edn
futon2/holes/labs/wm-contract/runs/h4-staged-prefix-2026-09-21/8ad66820-fb0d-46eb-86f2-2e4020eb228e.closure.edn
futon2/holes/labs/wm-contract/runs/h4-staged-prefix-2026-09-21/914a0914-3563-4685-8fb2-cd4f475dd5e3.closure.edn
futon2/holes/labs/wm-contract/runs/h4-staged-prefix-2026-09-21/combined-arithmetic-registry.edn
futon2/holes/labs/wm-contract/runs/h4-staged-prefix-2026-09-21/combined-production-registry.edn
futon2/holes/labs/wm-contract/runs/h4-staged-prefix-2026-09-21/dc38dcd9-1176-4367-a3d1-16e9c19c5a0a.closure.edn
futon2/holes/labs/wm-contract/runs/h4-staged-prefix-2026-09-21/e1434cd2-cb96-4f1e-b5d4-03eaeeabb7c0.closure.edn
futon2/holes/labs/wm-contract/runs/h4-staged-prefix-2026-09-21/f3b87bb3-b78c-4b1d-94ba-b02859880cbf.closure.edn
futon2/holes/labs/wm-contract/runs/h4-staged-prefix-2026-09-21/production-registry.edn
futon2/holes/labs/wm-contract/runs/h4-staged-prefix-2026-09-21/registry.edn
futon2/holes/labs/wm-contract/runs/habit-accumulate-rewarrant-2026-09-19/registry.edn
futon2/holes/labs/wm-contract/runs/habit-accumulation-2026-09-19/author.edn
futon2/holes/labs/wm-contract/runs/habit-accumulation-2026-09-19/fe049e9b-b068-4415-abb5-352763c9fd1c.closure.edn
futon2/holes/labs/wm-contract/runs/habit-accumulation-2026-09-19/prior-author.edn
futon2/holes/labs/wm-contract/runs/hermetic-fixtures-2026-09-19/execution/00230918-ab05-40f5-bbc6-860a2bf0bea5.closure.edn
futon2/holes/labs/wm-contract/runs/hermetic-fixtures-2026-09-19/repair-ea1-a7a5fc7c81ad45251922d33718170eba32a100cda770df0af9bcd759e28df913--attempt-001-artifact-binding-mismatch.edn.refusal.edn
futon2/holes/labs/wm-contract/runs/hermetic-fixtures-2026-09-19/repair-ea1-b28b40fe3c109454107faa2309cfcf0e51abf79e39c436fda76a2d7d665f1913--attempt-001-artifact-binding-mismatch.edn.refusal.edn
futon2/holes/labs/wm-contract/runs/history-admission-closing-2026-09-21/CLOSURE.md
futon2/holes/labs/wm-contract/runs/history-admission-closing-2026-09-21/pinned-post-history/8decba32-5014-4e07-9bf2-b7c23ef17c18.closure.edn
futon2/holes/labs/wm-contract/runs/history-admission-closing-2026-09-21/pinned-post-runner/7a5938a7-a750-4da2-bef4-d1a0615c0541.closure.edn
futon2/holes/labs/wm-contract/runs/history-admission-closing-2026-09-21/post-history/ea48e48d-8211-42c4-b6fe-b647acda386d.closure.edn
futon2/holes/labs/wm-contract/runs/history-admission-closing-2026-09-21/post-runner/298d553f-9668-4a03-a9fc-29b0e909b752.closure.edn
futon2/holes/labs/wm-contract/runs/history-admission-closing-2026-09-21/registration-history.edn
futon2/holes/labs/wm-contract/runs/history-admission-closing-2026-09-21/registration-pinned-history.edn
futon2/holes/labs/wm-contract/runs/history-admission-closing-2026-09-21/registration-pinned-runner.edn
futon2/holes/labs/wm-contract/runs/history-admission-closing-2026-09-21/registration-runner.edn
futon2/holes/labs/wm-contract/runs/history-admission-split-2026-09-21/post-history/98d61944-5d9b-4f90-808d-a66b0c5140e8.closure.edn
futon2/holes/labs/wm-contract/runs/history-admission-split-2026-09-21/post-runner/95d24380-1e8e-47df-be51-b7b44bf0fb67.closure.edn
futon2/holes/labs/wm-contract/runs/history-admission-split-2026-09-21/registration-history.edn
futon2/holes/labs/wm-contract/runs/history-admission-split-2026-09-21/registration-runner.edn
futon2/holes/labs/wm-contract/runs/narrative-11-2026-09-21/REVIEW.md
futon2/holes/labs/wm-contract/runs/narrative-11-2026-09-21/registered/428bd098-5822-401f-a2d3-c6c51da51f8d.closure.edn
futon2/holes/labs/wm-contract/runs/narrative-11-2026-09-21/registration.edn
futon2/holes/labs/wm-contract/runs/narrative-11-2026-09-21/registry.edn
futon2/holes/labs/wm-contract/runs/narrative-13-2026-09-21/REVIEW.md
futon2/holes/labs/wm-contract/runs/narrative-13-2026-09-21/registered/da243b22-56f5-4749-b9f7-c25d3ba43740.closure.edn
futon2/holes/labs/wm-contract/runs/narrative-13-2026-09-21/registration.edn
futon2/holes/labs/wm-contract/runs/narrative-13-2026-09-21/registry.edn
futon2/holes/labs/wm-contract/runs/narrative-3-2026-09-21/registered-dispatch/e510cd3f-48a6-43be-ab5e-8c476a80a6a7.closure.edn
futon2/holes/labs/wm-contract/runs/narrative-3-2026-09-21/registration-dispatch.edn
futon2/holes/labs/wm-contract/runs/narrative-4-2026-09-21/registered-ranking/7c70218e-2368-4bd4-b677-4d4a11fe2b83.closure.edn
futon2/holes/labs/wm-contract/runs/narrative-4-2026-09-21/registration-ranking.edn
futon2/holes/labs/wm-contract/runs/narrative-8-2026-09-21/REVIEW.md
futon2/holes/labs/wm-contract/runs/narrative-8-2026-09-21/registered/ad786f9a-5a3d-42aa-94d9-776552bae689.closure.edn
futon2/holes/labs/wm-contract/runs/narrative-8-2026-09-21/registration.edn
futon2/holes/labs/wm-contract/runs/narrative-8-2026-09-21/registry.edn
futon2/holes/labs/wm-contract/runs/narrative-9a-2026-09-21/registered/a622651f-db11-434e-95b4-ceed499d9872.closure.edn
futon2/holes/labs/wm-contract/runs/narrative-9a-2026-09-21/registration.edn
futon2/holes/labs/wm-contract/runs/node-evaluation-trace-2026-09-19/434811e8-b732-48db-975d-b8d9995f4e0f.closure.edn
futon2/holes/labs/wm-contract/runs/node-evaluation-trace-2026-09-19/test-run-record.edn
futon2/holes/labs/wm-contract/runs/nonempty-cascades-2026-09-21/canonical-reregistration/post-assembly/d2cebe11-8ab0-4e18-ab48-8a1653514cbb.closure.edn
futon2/holes/labs/wm-contract/runs/nonempty-cascades-2026-09-21/canonical-reregistration/post-construction/b77bd58d-1797-4fe7-8c18-268d23de6ba5.closure.edn
futon2/holes/labs/wm-contract/runs/nonempty-cascades-2026-09-21/canonical-reregistration/post-decision/4c9aacfe-017f-49f7-af48-c381151fbb42.closure.edn
futon2/holes/labs/wm-contract/runs/nonempty-cascades-2026-09-21/canonical-reregistration/post-runner/78d189d6-6418-425f-9a16-e6c8381b987d.closure.edn
futon2/holes/labs/wm-contract/runs/nonempty-cascades-2026-09-21/canonical-reregistration/post-selection/54ebc82a-c1fc-4f2a-a47a-fe932258f8cc.closure.edn
futon2/holes/labs/wm-contract/runs/nonempty-cascades-2026-09-21/canonical-reregistration/registration-assembly.edn
futon2/holes/labs/wm-contract/runs/nonempty-cascades-2026-09-21/canonical-reregistration/registration-construction.edn
futon2/holes/labs/wm-contract/runs/nonempty-cascades-2026-09-21/canonical-reregistration/registration-decision.edn
futon2/holes/labs/wm-contract/runs/nonempty-cascades-2026-09-21/canonical-reregistration/registration-runner.edn
futon2/holes/labs/wm-contract/runs/nonempty-cascades-2026-09-21/canonical-reregistration/registration-selection.edn
futon2/holes/labs/wm-contract/runs/nonempty-cascades-2026-09-21/canonical-reregistration/registry-assembly.edn
futon2/holes/labs/wm-contract/runs/nonempty-cascades-2026-09-21/canonical-reregistration/registry-construction.edn
futon2/holes/labs/wm-contract/runs/nonempty-cascades-2026-09-21/canonical-reregistration/registry-decision.edn
futon2/holes/labs/wm-contract/runs/nonempty-cascades-2026-09-21/canonical-reregistration/registry-runner.edn
futon2/holes/labs/wm-contract/runs/nonempty-cascades-2026-09-21/canonical-reregistration/registry-selection.edn
futon2/holes/labs/wm-contract/runs/nonempty-cascades-2026-09-21/pinned-post-assembly/42509306-1307-4bf6-93d9-5167b350b2f0.closure.edn
futon2/holes/labs/wm-contract/runs/nonempty-cascades-2026-09-21/pinned-post-construction/eea6040d-b421-4c5c-98cd-c9ec5ac8fb5a.closure.edn
futon2/holes/labs/wm-contract/runs/nonempty-cascades-2026-09-21/pinned-post-decision/489c232f-8527-4d2d-a094-3fec84482e8d.closure.edn
futon2/holes/labs/wm-contract/runs/nonempty-cascades-2026-09-21/pinned-post-runner/ee606426-8e78-46eb-9e04-f0a7883c0782.closure.edn
futon2/holes/labs/wm-contract/runs/nonempty-cascades-2026-09-21/pinned-post-selection/c37b7480-310d-4352-b9a2-04a17c9b81d8.closure.edn
futon2/holes/labs/wm-contract/runs/nonempty-cascades-2026-09-21/post-assembly/402206f7-6679-4e3a-babb-8fa2cd6050b1.closure.edn
futon2/holes/labs/wm-contract/runs/nonempty-cascades-2026-09-21/registration-assembly-dirty-canonical.edn
futon2/holes/labs/wm-contract/runs/nonempty-cascades-2026-09-21/registration-assembly.edn
futon2/holes/labs/wm-contract/runs/nonempty-cascades-2026-09-21/registration-construction.edn
futon2/holes/labs/wm-contract/runs/nonempty-cascades-2026-09-21/registration-decision.edn
futon2/holes/labs/wm-contract/runs/nonempty-cascades-2026-09-21/registration-runner.edn
futon2/holes/labs/wm-contract/runs/nonempty-cascades-2026-09-21/registration-selection.edn
futon2/holes/labs/wm-contract/runs/nonempty-cascades-2026-09-21/registry-assembly.edn
futon2/holes/labs/wm-contract/runs/nonempty-cascades-2026-09-21/registry-construction.edn
futon2/holes/labs/wm-contract/runs/nonempty-cascades-2026-09-21/registry-decision.edn
futon2/holes/labs/wm-contract/runs/nonempty-cascades-2026-09-21/registry-pinned-assembly.edn
futon2/holes/labs/wm-contract/runs/nonempty-cascades-2026-09-21/registry-pinned-construction.edn
futon2/holes/labs/wm-contract/runs/nonempty-cascades-2026-09-21/registry-pinned-decision.edn
futon2/holes/labs/wm-contract/runs/nonempty-cascades-2026-09-21/registry-pinned-runner.edn
futon2/holes/labs/wm-contract/runs/nonempty-cascades-2026-09-21/registry-pinned-selection.edn
futon2/holes/labs/wm-contract/runs/nonempty-cascades-2026-09-21/registry-runner.edn
futon2/holes/labs/wm-contract/runs/nonempty-cascades-2026-09-21/registry-selection.edn
futon2/holes/labs/wm-contract/runs/outstanding-dag-2026-09-14/sources/blocker.md
futon2/holes/labs/wm-contract/runs/outstanding-dag-2026-09-14/sources/paper-sec-glossary.tex
futon2/holes/labs/wm-contract/runs/outstanding-dag-2026-09-14/sources/r6-authority.md
futon2/holes/labs/wm-contract/runs/outstanding-dag-2026-09-14/sources/row14-discovery.md
futon2/holes/labs/wm-contract/runs/pattern-source-hash-2026-09-19/65cc4bb9-78fb-4091-8f44-eb1af632594c.closure.edn
futon2/holes/labs/wm-contract/runs/pattern-source-hash-2026-09-19/bd67a74b-28c6-4ba6-9312-e52dc1584522.closure.edn
futon2/holes/labs/wm-contract/runs/pattern-source-hash-2026-09-19/check-result.edn
futon2/holes/labs/wm-contract/runs/pattern-source-hash-2026-09-19/remint-warrant.edn
futon2/holes/labs/wm-contract/runs/pattern-source-hash-2026-09-19/warrant.edn
futon2/holes/labs/wm-contract/runs/proposal-supply-b1-2026-09-21/post-decision/cfc58c74-b7d2-43be-a93b-d26c232e2855.closure.edn
futon2/holes/labs/wm-contract/runs/proposal-supply-b1-2026-09-21/post-proposals/ca191473-8201-493b-bca9-a94379a5879f.closure.edn
futon2/holes/labs/wm-contract/runs/proposal-supply-b1-2026-09-21/post-request/da8c754a-4159-4a48-b0ff-dbfe2f6b610b.closure.edn
futon2/holes/labs/wm-contract/runs/proposal-supply-b1-2026-09-21/registry-decision.edn
futon2/holes/labs/wm-contract/runs/proposal-supply-b1-2026-09-21/registry-proposals.edn
futon2/holes/labs/wm-contract/runs/proposal-supply-b1-2026-09-21/registry-request.edn
futon2/holes/labs/wm-contract/runs/proposal-supply-b1-2026-09-21/registry-wants.edn
futon2/holes/labs/wm-contract/runs/proposal-supply-b1-2026-09-21/retrieval-demonstration.edn
futon2/holes/labs/wm-contract/runs/q-conditioned-evaluator-2026-09-20/6de2a632-a1b0-4287-89b4-4afc51d05483.closure.edn
futon2/holes/labs/wm-contract/runs/q-conditioned-evaluator-2026-09-20/a3eabf51-7c8f-470d-b7c1-80a2a1f7d612.closure.edn
futon2/holes/labs/wm-contract/runs/registry-orthogonality-2026-09-20/c3c8b4ce-d048-423c-9685-6a5d4d2db0b5.closure.edn
futon2/holes/labs/wm-contract/runs/repair-proposal-supply-2026-09-21/post-decision/9fa20f32-710d-49a6-acf1-d6b180557c3d.closure.edn
futon2/holes/labs/wm-contract/runs/repair-proposal-supply-2026-09-21/post-proposals/31156b2a-e93f-4278-abf8-b1a75ffcfcc6.closure.edn
futon2/holes/labs/wm-contract/runs/repair-proposal-supply-2026-09-21/post-repairs/b4cf9e6f-97a0-4860-945f-5a03c042db5d.closure.edn
futon2/holes/labs/wm-contract/runs/repair-proposal-supply-2026-09-21/registry-decision.edn
futon2/holes/labs/wm-contract/runs/repair-proposal-supply-2026-09-21/registry-proposals.edn
futon2/holes/labs/wm-contract/runs/repair-proposal-supply-2026-09-21/registry-repairs.edn
futon2/holes/labs/wm-contract/runs/row-13-live-accumulation-2026-09-12/NOTES.md
futon2/holes/labs/wm-contract/runs/row-13-live-accumulation-2026-09-12/check-parens-execution-receipt.edn
futon2/holes/labs/wm-contract/runs/row-13-live-accumulation-2026-09-12/clj-kondo-execution-receipt.edn
futon2/holes/labs/wm-contract/runs/row-13-live-accumulation-2026-09-12/gate-receipts.edn
futon2/holes/labs/wm-contract/runs/row-15-branches-capture-2026-09-12/capture_branches.clj
futon2/holes/labs/wm-contract/runs/row-15-branches-capture-2026-09-12/controller-head.edn
futon2/holes/labs/wm-contract/runs/row-15-branches-capture-2026-09-12/first-max-tie-control.edn
futon2/holes/labs/wm-contract/runs/row-15-branches-capture-2026-09-12/full-score-first-max.edn
futon2/holes/labs/wm-contract/runs/row-15-branches-capture-2026-09-12/habit-last-max.edn
futon2/holes/labs/wm-contract/runs/row-15-branches-capture-2026-09-12/machinery-capture.edn
futon2/holes/labs/wm-contract/runs/row-15-branches-capture-2026-09-12/no-op-abstain.edn
futon2/holes/labs/wm-contract/runs/row-15-branches-capture-2026-09-12/requested-posterior-f-pi-absent.edn
futon2/holes/labs/wm-contract/runs/row-15-depth-capture-2026-09-12/capture.edn
futon2/holes/labs/wm-contract/runs/row-15-depth-capture-2026-09-12/capture_tick.clj
futon2/holes/labs/wm-contract/runs/row-15-depth-capture-2026-09-12/check-parens-receipt.edn
futon2/holes/labs/wm-contract/runs/row-15-depth-capture-2026-09-12/clj-kondo-receipt.edn
futon2/holes/labs/wm-contract/runs/row-15-selector-trace-2026-09-12/execution-receipts.edn
futon2/holes/labs/wm-contract/runs/row-16-r6-posterior-2026-09-12/fixture.edn
futon2/holes/labs/wm-contract/runs/row-16-r8-policy-f-2026-09-12/fixture.edn
futon2/holes/labs/wm-contract/runs/row-16-r8-policy-f-2026-09-12/generate.clj
futon2/holes/labs/wm-contract/runs/row-16-r8-policy-f-2026-09-12/readback.edn
futon2/holes/labs/wm-contract/runs/row-17-r9-trace-noncredit-2026-09-12/non-credit-record.edn
futon2/holes/labs/wm-contract/runs/row-18-field-applicability-2026-09-13/checker-receipt.edn
futon2/holes/labs/wm-contract/runs/row-19-commission-retention-2026-09-13/execution-receipts.edn
futon2/holes/labs/wm-contract/runs/row-19-commission-retention-2026-09-13/implementation-note.md
futon2/holes/labs/wm-contract/runs/row-19-commission-retention-2026-09-13/review-fixes/execution-receipts.edn
futon2/holes/labs/wm-contract/runs/row-19-commission-retention-2026-09-13/review-fixes/r1-separate-store/execution-receipts.edn
futon2/holes/labs/wm-contract/runs/row-19-commission-retention-2026-09-13/review-fixes/r1-separate-store/retry-hardening/execution-receipts.edn
futon2/holes/labs/wm-contract/runs/row-19-selective-loader-2026-09-13/execution-receipts.edn
futon2/holes/labs/wm-contract/runs/row-22-e4-causal-evidence-2026-09-13/SPEC.md
futon2/holes/labs/wm-contract/runs/row-22-e5-e6-semantic-integration-2026-09-13/source-pins.edn
futon2/holes/labs/wm-contract/runs/row-22-e6b-store-protocol-2026-09-13/source-pins.edn
futon2/holes/labs/wm-contract/runs/row-22-r6-scoring-correspondence-2026-09-13/source-pins.edn
futon2/holes/labs/wm-contract/runs/row-26-on-demand-entrypoint-2026-09-13/execution-receipts.edn
futon2/holes/labs/wm-contract/runs/row-26-on-demand-entrypoint-2026-09-13/source-pins.edn
futon2/holes/labs/wm-contract/runs/run-participants-2026-09-19/34ef1c53-deac-444d-8a14-90387885c256.closure.edn
futon2/holes/labs/wm-contract/runs/run-participants-2026-09-19/c634083e-34f1-434c-9a67-8551c157277b.closure.edn
futon2/holes/labs/wm-contract/runs/runner-discharge-stage-2026-09-21/post-runner/2c10237b-45f0-4b38-9d70-e0cf630d9f88.closure.edn
futon2/holes/labs/wm-contract/runs/runner-discharge-stage-2026-09-21/post-runner/3e6d44d1-0f23-402e-9686-df5c5d76e074.closure.edn
futon2/holes/labs/wm-contract/runs/runner-discharge-stage-2026-09-21/post-runner/53c8d2f5-709a-47c8-844d-7f6a99d94ea7.closure.edn
futon2/holes/labs/wm-contract/runs/runner-discharge-stage-2026-09-21/post-store/27df3a57-a9b2-465d-92bb-446dfe578cfb.closure.edn
futon2/holes/labs/wm-contract/runs/runner-discharge-stage-2026-09-21/post-store/96419170-1d18-4cde-a125-2553a46e8312.closure.edn
futon2/holes/labs/wm-contract/runs/runner-waits-2026-09-21/d-predecessor-task-authority-isolated-1/c19a83cf-4361-4d73-8c2b-4c99d607f857.closure.edn
futon2/holes/labs/wm-contract/runs/runner-waits-2026-09-21/d-predecessor-task-authority-isolated-2/61e7fbad-2910-4df4-bee8-dbf5f080297a.closure.edn
futon2/holes/labs/wm-contract/runs/runner-waits-2026-09-21/d-predecessor-task-authority-registered-1/7cd28086-2293-4fb0-b42d-455cb16e0ec7.closure.edn
futon2/holes/labs/wm-contract/runs/runner-waits-2026-09-21/d-predecessor-task-authority-registered-2/d494083c-9fdf-4f21-82f3-e4447f06f2f9.closure.edn
futon2/holes/labs/wm-contract/runs/runner-waits-2026-09-21/full-loop-runner-isolated-1/3c39595c-ad56-4896-b7c2-50790b95bd57.closure.edn
futon2/holes/labs/wm-contract/runs/runner-waits-2026-09-21/full-loop-runner-isolated-2/87cb772b-2631-4993-a3b4-dd8d10e1dc73.closure.edn
futon2/holes/labs/wm-contract/runs/runner-waits-2026-09-21/full-loop-runner-registered-1/e1d61a96-7e94-4bde-8d1b-9c61708a3990.closure.edn
futon2/holes/labs/wm-contract/runs/runner-waits-2026-09-21/tripwire-isolated-1/aa6c4a1b-05e2-4601-8c80-1938eeebe812.closure.edn
futon2/holes/labs/wm-contract/runs/runner-waits-2026-09-21/tripwire-isolated-2/3bdfb53b-d5d8-433e-8e65-7430ced2141b.closure.edn
futon2/holes/labs/wm-contract/runs/runner-waits-2026-09-21/tripwire-registered-1/9224ae87-e22e-4410-96f8-38cf874e80d1.closure.edn
futon2/holes/labs/wm-contract/runs/runner-waits-2026-09-21/tripwire-registered-2/228e729f-583c-49f5-9e52-d2887e43c022.closure.edn
futon2/holes/labs/wm-contract/runs/scoring-input-receipts-2026-09-19/6606e71d-74ee-441d-ab36-df0feb34d01c.closure.edn
futon2/holes/labs/wm-contract/runs/scoring-input-receipts-2026-09-19/registry.edn
futon2/holes/labs/wm-contract/runs/scoring-input-receipts-2026-09-19/test-run-record.edn
futon2/holes/labs/wm-contract/runs/selection-always-2026-09-19/execution/81e4eac5-31ca-448d-a1fc-87048a15b8b9.closure.edn
futon2/holes/labs/wm-contract/runs/selection-always-2026-09-19/reconciled/execution/77ec3714-0e3c-4037-947e-f7d0e1e73cf9.closure.edn
futon2/holes/labs/wm-contract/runs/selection-fixture-migration-2026-09-20/6fa79dfb-f19d-4b5c-8077-944069c055eb.closure.edn
futon2/holes/labs/wm-contract/runs/selection-fixture-migration-2026-09-20/d603f4eb-9ba4-4777-89fe-c02984c956c9.closure.edn
futon2/holes/labs/wm-contract/runs/standing-cascade-g-refresh-2026-09-20/register.edn
futon2/holes/labs/wm-contract/runs/t8-dismissal-integration-2026-09-19/execution/c7a3e173-d891-4917-9e44-4656ae3104ea.closure.edn
futon2/holes/labs/wm-contract/runs/tldr-pass1-2026-09-20/sections/S06.md
futon2/holes/labs/wm-contract/runs/tldr-pass1-2026-09-20/sections/S12.md
futon2/holes/labs/wm-contract/runs/tldr-pass1-2026-09-20/sections/S16.md
futon2/holes/labs/wm-contract/runs/tldr-pass1-2026-09-20/sections/S35.md
futon2/holes/labs/wm-contract/runs/tldr-pass1-2026-09-20/sections/S37.md
futon2/holes/labs/wm-contract/runs/typed-nil-selection-2026-09-19/execution/bf10d0b8-40f2-44c2-a4a2-d952ed8898a6.closure.edn
futon2/holes/labs/wm-contract/runs/typed-nil-selection-2026-09-19/register.edn
futon2/holes/labs/wm-contract/runs/typed-terminal-retention-2026-09-19/execution/c2f6cc25-b10b-41d0-89ed-1e2f59c12653.closure.edn
futon2/holes/labs/wm-contract/runs/uniform-run-record-2026-09-19/aedf4dc8-f4ad-4c20-a49e-8d6a64e9994f.closure.edn
futon2/holes/labs/wm-contract/runs/uniform-run-record-2026-09-19/af0a16a2-1b96-4c23-b0d5-f3e783445a4a.closure.edn
futon2/holes/labs/wm-contract/runs/uniform-run-record-2026-09-19/check-result.edn
futon2/holes/labs/wm-contract/runs/uniform-run-record-2026-09-19/g-warrant.edn
futon2/holes/labs/wm-contract/runs/uniform-run-record-2026-09-19/offline-replay-run-record.edn
futon2/holes/labs/wm-contract/runs/uniform-run-record-2026-09-19/receipt.edn
futon2/holes/labs/wm-contract/runs/uniform-run-record-2026-09-19/warrant.edn
futon2/holes/labs/wm-contract/runs/wire-live-c-2026-09-19/registry-spec.edn
futon2/holes/labs/wm-contract/runs/witness-reference-2026-09-20/14464f8c-5ad8-483d-98d1-ecb9b5684bb8.closure.edn
futon2/holes/labs/wm-contract/runs/wm-08-external-f2-2026-09-16/EXPECTATIONS-v2.edn
futon2/holes/labs/wm-contract/runs/wm-08-external-f2-2026-09-16/EXPECTATIONS.edn
futon2/holes/labs/wm-contract/runs/wm-08-external-f2-2026-09-16/FROZEN-CONTEXT.edn
futon2/holes/labs/wm-contract/runs/wm-08-external-f2-2026-09-16/REEXPRESSED-RECORD.edn
futon2/holes/labs/wm-contract/runs/wm-08-external-f2-2026-09-16/ROUTE-A-REHEARSAL.md
futon2/holes/labs/wm-contract/runs/wm-build-loop-2026-09-15/join-6/RECORD.md
futon2/holes/labs/wm-contract/runs/wm-build-loop-2026-09-15/p1-reproducer/readback.edn
futon2/holes/labs/wm-contract/runs/wm-build-loop-2026-09-15/p1-reproducer/reproduce.clj
futon2/holes/labs/wm-contract/runs/wm-build-loop-2026-09-15/wm-08-09-review-1/probe-results-attempt1.edn
futon2/holes/labs/wm-contract/runs/wm-build-loop-2026-09-15/wm-08-09-review-1/probe-results.edn
futon2/holes/labs/wm-contract/runtime_validation_check.bb
futon2/holes/labs/wm-contract/scalar_typing_check.bb
futon2/holes/labs/wm-contract/sim/R1-carriers.edn
futon2/holes/labs/wm-contract/sim/R13-carriers.edn
futon2/holes/labs/wm-contract/sim/R2-carriers.edn
futon2/holes/labs/wm-contract/sim/R3-carriers.edn
futon2/holes/labs/wm-contract/sim/R3-claim.edn
futon2/holes/labs/wm-contract/sim/R3a-carriers.edn
futon2/holes/labs/wm-contract/sim/R4-carriers.edn
futon2/holes/labs/wm-contract/sim/R5-carriers.edn
futon2/holes/labs/wm-contract/sim/R6-carriers.edn
futon2/holes/labs/wm-contract/sim/R7-carriers.edn
futon2/holes/labs/wm-contract/sim/R8-carriers.edn
futon2/holes/labs/wm-contract/tension-ledger.edn
futon2/holes/labs/wm-contract/u16_outcome_semantics.clj
futon2/holes/labs/wm-contract/u21_selection_focus.clj
futon2/holes/labs/wm-contract/u24_survey_mission.clj
futon2/holes/labs/wm-contract/u37_enumeration_replay.clj
futon2/holes/labs/wm-contract/u39_selection_retrospective.bb
futon2/holes/labs/wm-contract/u40_first_retrospective.bb
futon2/holes/labs/wm-contract/u41_tension_ledger.bb
futon2/holes/labs/wm-contract/u42_gauge_producers.clj
futon2/holes/labs/wm-contract/u43_focus_reconcile.clj
futon2/holes/labs/wm-contract/u44_doability_liveness.clj
futon2/holes/labs/wm-contract/u49_route_transcribe.bb
futon2/holes/labs/wm-contract/u52_ladder.clj
futon2/holes/labs/wm-contract/v7_r13_node_sim.clj
futon2/holes/labs/wm-contract/v7_r14_node_sim.clj
futon2/holes/labs/wm-contract/v7_r1_node_sim.clj
futon2/holes/labs/wm-contract/v7_r20_node_sim.clj
futon2/holes/labs/wm-contract/v7_r2_node_sim.clj
futon2/holes/labs/wm-contract/v7_r3_node_sim.clj
futon2/holes/labs/wm-contract/v7_r6_node_sim.clj
futon2/holes/labs/wm-contract/v7_r7_node_sim.clj
futon2/holes/labs/wm-contract/v7_r8_node_sim.clj
futon2/holes/labs/wm-contract/wm04-pilot/adjudications/zai-21.edn
futon2/holes/labs/wm-contract/wm_step_records.bb
futon2/holes/labs/zaif-harness/NOTE-trace-surfacing-triage.md
futon2/holes/labs/zaif-harness/census-ledger.edn
futon2/holes/labs/zaif-harness/runs/PA11z-exemplar/annotator-report-AS-STORED-trimmed.md
futon2/holes/labs/zaif-harness/runs/PA11z-exemplar/annotator-report.md
futon2/holes/labs/zaif-harness/runs/PA11z-exemplar/commission-prompt.md
futon2/holes/labs/zaif-harness/runs/PA11z-exemplar/lifecycle-census.edn
futon2/holes/labs/zaif-harness/runs/PA15z-cell-linkage/census-after-mechanism-a.edn
futon2/holes/labs/zaif-harness/runs/PA15z-cell-linkage/census-clean-2026-09-08.edn
futon2/holes/labs/zaif-harness/runs/PA1z-census-harness/negative-control-2026-09-06.edn
futon2/holes/labs/zaif-harness/runs/PA2z-slice-a/census-slice-a.edn
futon2/holes/labs/zaif-harness/runs/PA3z-slice-b/census-slice-b.edn
futon2/holes/labs/zaif-harness/runs/PA3z-v2-control/control-v1.edn
futon2/holes/labs/zaif-harness/runs/PA3z-v2-control/control-v2.edn
futon2/holes/labs/zaif-harness/runs/PA4z-r17/census-r17.edn
futon2/holes/labs/zaif-harness/worklist.edn
futon2/holes/missions/M-aif-a-matrix-faithfulness.md
futon2/holes/missions/M-aif-policy-conditioned-eig.md
futon2/holes/missions/M-wm-aif-policy-grain-compliance.md
futon2/holes/missions/M-zaif-harness-v1.md
futon2/holes/overnight-flights-2026-07-06.md
futon2/holes/problems/BUILD-PLAN-0831.md
futon2/holes/problems/BUILD-packets/AUD-D3.md
futon2/holes/problems/BUILD-packets/EDGES-D1.md
futon2/holes/problems/BUILD-packets/NOUNS-D1.md
futon2/holes/problems/BUILD-packets/R8-D2.md
futon2/holes/problems/BUILD-packets/WM-RUN1.md
futon2/holes/problems/BUILD-packets/WM-RUN2.md
futon2/holes/problems/P-R8.md
futon2/holes/problems/P-dispatch-workflow.md
futon2/holes/problems/P-organise-the-library.md
futon2/holes/problems/P-snatch-microcosm.md
futon2/holes/problems/P-validated-R5-snatch-reexamination.md
futon2/holes/problems/PREREG-war-machine.md
futon2/holes/problems/facts-find-snatch-D1.md
futon2/holes/reflow/9aaeeeb0__M-points-de-fuite/orbit.edn
futon2/holes/supervised-flight-2026-07-06.md
futon2/holes/thread-orbits.edn
futon2/holes/wm-baseline.md
futon2/resources/sorrys.edn
futon2/resources/wm/cascade-sources/M-expressions-of-interest.edn
futon2/scripts/futon2/aif/fold_llm_demo.clj
futon2/scripts/futon2/aif/l2_verify.clj
futon2/scripts/reference_regression.clj
futon2/src/futon2/aif/bulletin.clj
futon2/src/futon2/aif/efe.clj
futon2/src/futon2/aif/enumeration_completeness.clj
futon2/src/futon2/aif/full_loop_runner.clj
futon2/src/futon2/aif/load_identity.clj
futon2/src/futon2/aif/mission_epistemic_value.clj
futon2/src/futon2/aif/ruled_outcome_c.clj
futon2/src/futon2/aif/selection_rationale.clj
futon2/test/fixtures/cascade-fold-repair/expressions-of-interest.edn
futon2/test/fixtures/d-token-carry/baseline-outcomes.edn
futon2/test/fixtures/learning-trial/1789964661.edn
futon2/test/fixtures/narrative-discrimination/1789952479.edn
futon2/test/fixtures/narrative-discrimination/1789964661.edn
futon2/test/fixtures/observation-model/tick-001.edn
futon2/test/fixtures/tick-b-enactment.edn
futon2/test/fold_realized_zero_coverage_test.clj
futon2/test/futon2/aif/active_horizon_g_test.clj
futon2/test/futon2/aif/cascade_beta_update_test.clj
futon2/test/futon2/aif/cascade_free_energy_test.clj
futon2/test/futon2/aif/cascade_problems_test.clj
futon2/test/futon2/aif/check_candidates_test.clj
futon2/test/futon2/aif/close_loop_test.clj
futon2/test/futon2/aif/find_designation_test.clj
futon2/test/futon2/aif/fold_cascade_test.clj
futon2/test/futon2/aif/fold_classical_test.clj
futon2/test/futon2/aif/full_loop_runner_test.clj
futon2/test/futon2/aif/g_term_decomposition_test.clj
futon2/test/futon2/aif/mission_control_graph_test.clj
futon2/test/futon2/aif/observation_warrant_refusal_test.clj
futon2/test/futon2/aif/pattern_registry_test.clj
futon2/test/futon2/aif/selection_rationale_test.clj
futon2/test/futon2/aif/strategic_habit_test.clj
futon2/test/futon2/aif/trace_test.clj
futon2/test/futon2/aif/wm08_route_a_test.clj
futon2/test/futon2/report/cascade_decision_test.clj
futon2/test/futon2/report/war_machine_test.clj
futon2/test/futon2/report/wm01_bindings_test.clj
futon2/vm-test/futon2/vm/tick_001_s03_r6_test.clj
futon2/vm-test/futon2/vm/tick_001_s04_r13_test.clj
futon2/vm-test/futon2/vm/tick_001_s05_r4_test.clj
futon2/vm-test/futon2/vm/tick_001_s06_r5_test.clj
futon2/vm-test/futon2/vm/tick_001_s07_r14_test.clj
futon2/vm-test/futon2/vm/tick_001_s08_r16_test.clj
futon2/vm-test/futon2/vm/tick_001_s09_r9_test.clj
futon2/vm-test/futon2/vm/tick_001_s11_tick_test.clj
futon3/CLAUDE.md
futon3/README-flexiarg.md
futon3/README-pattern-mining.md
futon3/README-sokoban.md
futon3/checks/F11-find-comparison-manifest.edn
futon3/checks/F12-why-cycle-diagnosis.md
futon3/checks/README.md
futon3/checks/alfworld-cascade.edn
futon3/checks/ants-cascade.edn
futon3/checks/construct-cascade.edn
futon3/checks/construct_cascade.clj
futon3/checks/construct_retrodiction_cascade.clj
futon3/checks/construct_zaif_cascade.clj
futon3/checks/edge-proposals.edn
futon3/checks/edge-weights.edn
futon3/checks/find-organise.edn
futon3/checks/find-snatch-choice-evidence.edn
futon3/checks/find-snatch-evidence.edn
futon3/checks/find-snatch.edn
futon3/checks/find_snatch_choices.clj
futon3/checks/how_witness_declare_conditioning.clj
futon3/checks/how_witness_delivery_vs_practice.clj
futon3/checks/how_witness_heartbeat.clj
futon3/checks/how_witness_no_self_certification.clj
futon3/checks/how_witness_scheduled_observer.clj
futon3/checks/how_witness_snatch.clj
futon3/checks/how_witness_split_transport.clj
futon3/checks/how_witness_status_gated_belief.clj
futon3/checks/how_witness_two_layer_calibration.clj
futon3/checks/learn-edge-weights.edn
futon3/checks/open-cascade-short-cue.edn
futon3/checks/open-cascade.edn
futon3/checks/open-plausibility.edn
futon3/checks/open-tensions-short-cue.edn
futon3/checks/open-tensions.edn
futon3/checks/playout_snatch.clj
futon3/checks/retrodiction-cascade-per-clause.edn
futon3/checks/retrodiction-cascade.edn
futon3/checks/retrodiction-comparison.edn
futon3/checks/zaif-cascade.edn
futon3/dev/lab_stream_codex.clj
futon3/dev/musn_stream.clj
futon3/docs/TN-mission-pattern-correspondence.md
futon3/docs/TN-tensegrity.md
futon3/docs/aif-exploratory-mode.md
futon3/docs/aif-pattern-engine.md
futon3/docs/draft-provenance-standard.md
futon3/docs/fulab-hud-design.md
futon3/docs/fulab-plan.md
futon3/docs/fulab/fulab-experiments.md
futon3/docs/guides/README-patterns.md
futon3/docs/guides/README-proofwork.md
futon3/docs/p4ng-evidence-tickets.md
futon3/docs/protocol/golden-transcripts.md
futon3/docs/sigil-collisions.md
futon3/flexiarg-directives.edn
futon3/holes/excursions/E-clause-vocabulary-survey.md
futon3/holes/excursions/E-pattern-peripheral.md
futon3/holes/excursions/E-ukrn-paper-v2.4-comments.md
futon3/holes/labs/M-essays-edit-cycle/psr/2026-05-13__stable-registry__xtdb-projected-catalog.md
futon3/holes/labs/M-essays-edit-cycle/psr/2026-05-14__annotation-lifecycle__persistent-retraction-visibility.md
futon3/holes/labs/M-essays-edit-cycle/pur/2026-05-13__stable-registry__xtdb-projected-catalog.md
futon3/holes/labs/M-essays-edit-cycle/pur/2026-05-14__annotation-lifecycle__persistent-retraction-visibility.md
futon3/holes/labs/M-live-geometric-stack/psr/2026-04-27__phase-1__edge-taxonomy-lift.md
futon3/holes/labs/M-live-geometric-stack/psr/2026-04-27__phase-2__geometric-layer.md
futon3/holes/labs/M-live-geometric-stack/psr/2026-04-27__phase-3__commits-as-vertices.md
futon3/holes/labs/M-live-geometric-stack/psr/2026-04-27__phase-4__live-watcher.md
futon3/holes/labs/M-live-geometric-stack/psr/2026-04-27__phase-5__futonic-zapper.md
futon3/holes/labs/M-live-geometric-stack/pur/2026-04-27__B-1__vocab-whitespace-fix.md
futon3/holes/labs/M-live-geometric-stack/pur/2026-04-27__B-2__per-repo-qname-prefix.md
futon3/holes/labs/M-live-geometric-stack/pur/2026-04-27__codex-review__elisp-projector.md
futon3/holes/labs/M-live-geometric-stack/pur/2026-04-27__codex-review__python-projector.md
futon3/holes/labs/M-live-geometric-stack/pur/2026-04-27__phase-1-2__pyramidal-expansion.md
futon3/holes/labs/M-live-geometric-stack/pur/2026-04-27__phase-1__edge-taxonomy-lift.md
futon3/holes/labs/M-live-geometric-stack/pur/2026-04-27__phase-2__geometric-layer.md
futon3/holes/labs/M-live-geometric-stack/pur/2026-04-27__phase-3__commits-as-vertices.md
futon3/holes/labs/M-live-geometric-stack/pur/2026-04-27__phase-4.5__per-file-multi-watcher.md
futon3/holes/labs/M-live-geometric-stack/pur/2026-04-27__phase-4__live-watcher.md
futon3/holes/labs/M-live-geometric-stack/pur/2026-04-27__phase-5__futonic-zapper-v0.md
futon3/holes/labs/M-live-geometric-stack/pur/2026-04-27__pyramidal-expansion__round-2.md
futon3/holes/labs/M-weird-modernism/anchor-stewardship.md
futon3/holes/labs/futon1a/psr/2026-02-07__core-xtdb__storage-durability-first.md
futon3/holes/labs/futon1a/psr/2026-02-07__invariants__counter-ratchet.md
futon3/holes/labs/futon1a/psr/2026-02-07__layer0-xtdb__durability-gate.md
futon3/holes/labs/futon1a/psr/2026-02-07__layer1-identity__uuid-uniqueness.md
futon3/holes/labs/futon1a/psr/2026-02-07__layer2-integrity__rehydrate-entity.md
futon3/holes/labs/futon1a/psr/2026-02-07__layer3-4__auth-validation.md
futon3/holes/labs/futon1a/psr/2026-02-07__pipeline-api__write-surface.md
futon3/holes/labs/futon1a/pur/2026-02-07__core-xtdb__storage-durability-first.md
futon3/holes/labs/futon1a/pur/2026-02-07__invariants__counter-ratchet.md
futon3/holes/labs/futon1a/pur/2026-02-07__layer0-xtdb__durability-gate.md
futon3/holes/labs/futon1a/pur/2026-02-07__layer1-identity__uuid-uniqueness.md
futon3/holes/labs/futon1a/pur/2026-02-07__layer2-integrity__rehydrate-entity.md
futon3/holes/labs/futon1a/pur/2026-02-07__layer3-4__auth-validation.md
futon3/holes/labs/futon1a/pur/2026-02-07__pipeline-api__write-surface.md
futon3/holes/labs/library-contract/LA1c-restatement.md
futon3/holes/labs/library-contract/LA2-A2-closure-evidence-2026-09-09.md
futon3/holes/labs/library-contract/decisions.edn
futon3/holes/labs/library-contract/worklist.edn
futon3/holes/missions/M-agency-rebuild.md
futon3/holes/missions/M-agency-unified-routing.md
futon3/holes/missions/M-coordination-rewrite.md
futon3/holes/missions/M-drawbridge-multi-agent.md
futon3/holes/missions/M-futon1a-rebuild-scoping-review.md
futon3/holes/missions/M-futon1a-rebuild.md
futon3/holes/missions/M-futon1a-workplan.md
futon3/holes/missions/M-futon3x-e2e.md
futon3/holes/missions/M-live-geometric-stack.md
futon3/holes/missions/M-mission-coherence-patterns.md
futon3/holes/missions/M-mission-control-scoping.md
futon3/holes/missions/M-pattern-application-diagnostic.md
futon3/holes/missions/M-pattern-inference-engine-scoping-review.md
futon3/holes/missions/M-pattern-ingest.md
futon3/holes/missions/M-pattern-mining.md
futon3/holes/missions/M-pattern-retrieval-calibration.md
futon3/holes/missions/M-weird-modernism.md
futon3/holes/war-bulletin-8.md
futon3/library/aif/attestations.edn
futon3/library/aif/posthoc-readings.edn
futon3/library/baldwin/INDEX.md
futon3/library/cycle-machine/INDEX.md
futon3/library/futon-theory/INDEX.md
futon3/library/math-strategy/PAPER-SHAPES-INDEX.md
futon3/library/musn/attestations.edn
futon3/library/problems/attestations.edn
futon3/library/snatch/attestations.edn
futon3/library/storage/README.md
futon3/library/ukrns/SHADOW-PASS-2026-05-08.md
futon3/library/ukrns/attestations.edn
futon3/library/vsatlas/proposals.edn
futon3/library/war-room/attestations.edn
futon3/library/war-room/posthoc-readings.edn
futon3/library/writing-coherence/attestations.edn
futon3/plugins/futon-peripherals/README.md
futon3/plugins/futon-peripherals/agents/reflect.md
futon3/plugins/futon-peripherals/commands/patterns.md
futon3/plugins/futon-peripherals/commands/psr.md
futon3/plugins/futon-peripherals/commands/pur.md
futon3/plugins/futon/README.md
futon3/plugins/futon/commands/psr.md
futon3/plugins/futon/commands/pur.md
futon3/resources/hints-log.edn
futon3/resources/sigils/bridge-assignments.edn
futon3/resources/sigils/rationale-examples.edn
futon3/resources/tatami-context.edn
futon3/scripts/musn_http_preflight.clj
futon3/scripts/musn_preflight.clj
futon3/src/futon3/chops.clj
futon3/src/futon3/fulab/hud.clj
futon3/src/futon3/musn/router.clj
futon3/src/futon3/portal.clj
futon3/test/fixtures/find-receipt-consumers.edn
futon3/test/fixtures/library-graph/evidence-records.edn
futon3/test/futon3/agency/invariants/a0_delivery_test.clj
futon3/test/futon3/agency/invariants/a1_identity_test.clj
futon3/test/futon3/agency/invariants/a2_atomicity_test.clj
futon3/test/futon3/agency/invariants/a3_loud_failure_test.clj
futon3/test/futon3/agency/invariants/a5_bounded_test.clj
futon3/test/futon3/agency/invariants/soak_test.clj
futon3/test/futon3/find_organise_test.clj
futon3/test/futon3/find_snatch_evidence_test.clj
futon3/test/futon3/similarity_test.clj
futon3/test/scripts/pattern_pull_test.clj
futon3/test/spider_runner_test.clj
futon3/test/transport_test.clj
futon3a/docs/aif-technote.md
futon3a/docs/compass-exploratory-missions.md
futon3a/docs/compass-mission-1-results.md
futon3a/docs/mission-8-self-description-results.md
futon3a/holes/labs/M-memes-arrows/E-fold-engine-wiring-ALEXANDER.edn
futon3a/holes/labs/M-memes-arrows/E-fold-engine-wiring-ALEXANDRIAN-AIF.edn
futon3a/holes/labs/M-memes-arrows/E-fold-engine-wiring-GENERATED.edn
futon3a/holes/labs/M-memes-arrows/E-fold-engine-wiring.edn
futon3a/holes/labs/M-memes-arrows/alexandrian_aif.py
futon3a/holes/labs/M-memes-arrows/cascade_semilattice_test.py
futon3a/holes/labs/M-memes-arrows/discharge_experiment.py
futon3a/holes/labs/M-memes-arrows/fold_engine.clj
futon3a/holes/labs/M-memes-arrows/pattern_posteriors_ab.self_graded.md
futon3a/holes/labs/M-memes-arrows/pattern_posteriors_test.py
futon3a/holes/labs/M-memes-arrows/reward_v1_test.py
futon3a/holes/labs/M-memes-arrows/similarity_join_spike.py
futon3a/holes/labs/M-memes-arrows/wiring_corpus_test.py
futon3a/holes/labs/M-memes-arrows/worked-examples/h4-similarity-join.py
futon3a/holes/missions/E-fold-engine.md
futon3a/holes/missions/M-memes-arrows-patterns-diagrams.md
futon3a/src/futon/notions.clj
futon3a/src/meme/fold.clj
futon3a/test/futon/flexiarg/projection_test.clj
futon3b/AGENTS.md
futon3b/holes/missions/M-coordination-rewrite.md
futon3b/library/coordination/INDEX.md
futon3b/scripts/live_gate_run.clj
futon3b/src/futon3/gate/canon.clj
futon3b/src/futon3/gate/level1.clj
futon3b/src/futon3/gate/observe.clj
futon3b/src/futon3/gate/pattern.clj
futon3c/README-drawbridge.md
futon3c/README-evidence.md
futon3c/README-walkie-talkie.md
futon3c/README.md
futon3c/data/chipwitz-warrant-map.edn
futon3c/data/pattern-staging/case-1/TOMBSTONE-search-the-namespace.md
futon3c/data/pattern-staging/slice-1/mining-report.md
futon3c/data/pattern-staging/slice-3/mining-report.md
futon3c/docs/boundary-pattern.md
futon3c/docs/evidence-facets.md
futon3c/docs/invariants.md
futon3c/docs/pattern-retrieval-architecture.md
futon3c/docs/repl-parity-claims.edn
futon3c/docs/research-plan-v1.md
futon3c/docs/retrieval-evidence-ledger.md
futon3c/docs/retrieval-whitepaper-v3.md
futon3c/docs/system-now-next.md
futon3c/docs/technote-codex-code-invariants.md
futon3c/docs/technote-portfolio-inference-debt.md
futon3c/docs/technote-smart-cursor-external-e2e-handoff.md
futon3c/docs/wiring-claims.edn
futon3c/docs/wiring-contract.md
futon3c/holes/C251-invoke-ledger-durability-discovery.md
futon3c/holes/C254-atomic-invoke-ledger-snapshot.md
futon3c/holes/C263-invoke-ledger-schema-and-post-rename.md
futon3c/holes/CODEX-HANDOFF-live-wm-memory-selection-verify.md
futon3c/holes/E-memory-latency.md
futon3c/holes/E-possible-world-regulator.md
futon3c/holes/NOTE-agency-accounting-gaps-2026-09-21.md
futon3c/holes/PILOT-STOCK-TAKE-002.md
futon3c/holes/PILOTS-LOG.md
futon3c/holes/PLAN-H5-populate-the-graph.md
futon3c/holes/PLAN-apm-cascade-demo-instance.md
futon3c/holes/T-typed-submission-wrapper-cancellation-evidence.md
futon3c/holes/campaigns/C-cascade-real.md
futon3c/holes/campaigns/C-substrate-completion.md
futon3c/holes/evidence/run-participants-2026-09-19/b0dac719-2345-439a-8c4d-0da74cd7b26e.closure.edn
futon3c/holes/evidence/run-participants-2026-09-19/d9f384c6-fdf6-4382-b499-7c2ad59645b2.closure.edn
futon3c/holes/evidence/run-participants-2026-09-19/registry.edn
futon3c/holes/excursions/E-APM-f10-defects.md
futon3c/holes/excursions/E-APM-f11-defects.md
futon3c/holes/excursions/E-APM-f12-defects.md
futon3c/holes/excursions/E-R14-red-ring-fill.md
futon3c/holes/excursions/E-R5-red-ring-fill.md
futon3c/holes/excursions/E-R6-red-ring-fill.md
futon3c/holes/excursions/E-R8-red-ring-fill.md
futon3c/holes/excursions/E-apm-A3-ingest-efficiency.md
futon3c/holes/excursions/E-apm-bundle-sorry-drift.md
futon3c/holes/excursions/E-apm-halftime-pre-go-live-A3.md
futon3c/holes/excursions/E-apm-halftime-pre-go-live-B.md
futon3c/holes/excursions/E-bell-clink-adapter.p1-report.md
futon3c/holes/excursions/E-bell-clink-adapter.p2-report.md
futon3c/holes/excursions/E-cascade-assembly.md
futon3c/holes/excursions/E-fetch-entity-miss-path.md
futon3c/holes/excursions/E-futon-memories.md
futon3c/holes/excursions/E-futon1b-latency-inventory.md
futon3c/holes/excursions/E-memory-whitepaper-v2-plan.md
futon3c/holes/excursions/E-operator-turn-modelling-2026-08-25.md
futon3c/holes/excursions/E-pattern-census-and-orphans.md
futon3c/holes/excursions/E-pipeline-pipecleaner.md
futon3c/holes/excursions/E-promotion-deadlock-discovery.md
futon3c/holes/excursions/E-wm-operator-lane.md
futon3c/holes/excursions/o4-land-payloads.edn
futon3c/holes/excursions/o4-upward-clusters.dryrun.edn
futon3c/holes/excursions/pipeline-pattern-map.edn
futon3c/holes/excursions/pipeline-semilattice-clusters.edn
futon3c/holes/excursions/pipeline-semilattice-clusters.md
futon3c/holes/f42a-H4-judgement-2026-08-26.md
futon3c/holes/f42a-cascade-example.edn
futon3c/holes/f42a-cascade-run-cap100.edn
futon3c/holes/f42a-cascade-run-cap1000.edn
futon3c/holes/f42b-cascade-run-cap1000.edn
futon3c/holes/f42c-cascade-run-cap1000.edn
futon3c/holes/flights/first-flights-cascade.edn
futon3c/holes/labs/E-futon-memories/s1-results-note.md
futon3c/holes/labs/E-futon-memories/s1_topology.py
futon3c/holes/labs/M-apm-demonstration/analysis/apm-v4-offline-2026-09-11/WM-ALIGNMENT-2026-09-11.md
futon3c/holes/labs/M-apm-demonstration/analysis/f13-guide-working-notes.md
futon3c/holes/labs/M-apm-demonstration/analysis/h5/NOTE-H5-before-after-2026-08-26.md
futon3c/holes/labs/M-apm-demonstration/analysis/h5/memory-assert-after-h5a-2026-08-26.edn
futon3c/holes/labs/M-apm-demonstration/analysis/h5/memory-assert-after-h5b-2026-08-26.edn
futon3c/holes/labs/M-apm-demonstration/analysis/h5/memory-assert-asof-2026-08-26T1640Z.edn
futon3c/holes/labs/M-apm-demonstration/analysis/h5/why-relations-after-h5a-2026-08-26.edn
futon3c/holes/labs/M-apm-demonstration/analysis/h5/why-relations-before-h5a-2026-08-26.edn
futon3c/holes/labs/M-apm-demonstration/analysis/h5/why_graph_metrics.py
futon3c/holes/labs/M-apm-demonstration/analysis/historical-pattern-reconstruction-2026-09-10/candidate-patterns.md
futon3c/holes/labs/M-apm-demonstration/analysis/historical-pattern-reconstruction-2026-09-10/comparison-table.md
futon3c/holes/labs/M-apm-demonstration/analysis/historical-pattern-reconstruction-2026-09-10/proof-reconstructions.md
futon3c/holes/labs/M-apm-demonstration/analysis/pattern-construction-2026-09-10/freeze_prelim.py
futon3c/holes/labs/M-apm-demonstration/analysis/pattern-construction-2026-09-10/prelim-development.md
futon3c/holes/labs/M-apm-demonstration/analysis/pattern-construction-2026-09-10/topology-patterns.md
futon3c/holes/labs/M-apm-demonstration/analysis/pattern-first-development-2026-09-10/census.md
futon3c/holes/labs/M-apm-demonstration/analysis/pattern-first-development-2026-09-10/check_catalog.bb
futon3c/holes/labs/M-apm-demonstration/analysis/pattern-first-development-2026-09-10/probe.py
futon3c/holes/labs/M-apm-demonstration/analysis/series.edn
futon3c/holes/labs/M-apm-demonstration/analysis/ta-cascade-2026-09-10/STUDENT.md
futon3c/holes/labs/M-apm-demonstration/analysis/ta-transfer-pilot-2026-09-10/student/c1-initial.md
futon3c/holes/labs/M-apm-demonstration/analysis/ta-transfer-pilot-2026-09-10/student/c1-revised.md
futon3c/holes/labs/M-apm-demonstration/analysis/ta-transfer-pilot-2026-09-10/student/c2.md
futon3c/holes/labs/M-apm-demonstration/analysis/ta-transfer-pilot-2026-09-10/student/c3-initial.md
futon3c/holes/labs/M-apm-demonstration/f8-retro-mining-receipts.edn
futon3c/holes/labs/M-apm-demonstration/frame-11-registration.edn
futon3c/holes/labs/M-apm-demonstration/frame-12-registration.edn
futon3c/holes/labs/M-apm-demonstration/frame-13-registration.edn
futon3c/holes/labs/M-apm-demonstration/frame-14-registration.edn
futon3c/holes/labs/M-apm-demonstration/frame-15-frame/guide-log.md
futon3c/holes/labs/M-apm-demonstration/frame-15-registration.edn
futon3c/holes/labs/M-apm-demonstration/frame-9-registration.edn
futon3c/holes/labs/M-apm-demonstration/pattern-library-codex-scribe-f35-a95J04.md
futon3c/holes/labs/M-apm-demonstration/pattern-library-zai-scribe-f46-a96J08.md
futon3c/holes/labs/M-apm-demonstration/pattern-library-zai-scribe-f74-b94J01.md
futon3c/holes/labs/M-apm-demonstration/pattern-library-zai-scribe-f75-b94J03.md
futon3c/holes/labs/M-apm-demonstration/prereg-capability-transfer-v1.edn
futon3c/holes/labs/M-apm-demonstration/retro-promotion-manifest.edn
futon3c/holes/labs/M-apm-demonstration/retro-promotion-receipts.edn
futon3c/holes/labs/M-apm-demonstration/revalidation-f33-f35-pattern-accounting-20260830.edn
futon3c/holes/labs/M-archaeology-control/psr/2026-04-29__derive__subsumption-witness-siblings.md
futon3c/holes/labs/M-archaeology-control/pur/2026-04-29__instantiate__subsumption-witness-siblings.md
futon3c/holes/labs/M-chipwitz-corps/pxr-log.edn
futon3c/holes/labs/M-codex-sorry-loop/harvest-dryrun-019f8b63.edn
futon3c/holes/labs/M-codex-sorry-loop/promote_scribe_pass_1.bb
futon3c/holes/labs/M-codex-sorry-loop/promote_scribe_pass_13.bb
futon3c/holes/labs/M-codex-sorry-loop/promote_scribe_pass_19.bb
futon3c/holes/labs/M-codex-sorry-loop/promote_scribe_pass_23.bb
futon3c/holes/labs/M-codex-sorry-loop/promote_scribe_pass_25.bb
futon3c/holes/labs/M-codex-sorry-loop/promote_scribe_pass_2_3.bb
futon3c/holes/labs/M-codex-sorry-loop/promote_scribe_pass_32.bb
futon3c/holes/labs/M-codex-sorry-loop/promote_scribe_pass_33.bb
futon3c/holes/labs/M-codex-sorry-loop/promote_scribe_pass_4_5.bb
futon3c/holes/labs/M-codex-sorry-loop/promote_scribe_pass_6_7.bb
futon3c/holes/labs/M-codex-sorry-loop/promote_scribe_pass_8_9.bb
futon3c/holes/labs/M-codex-sorry-loop/promotion-pass-13-report.edn
futon3c/holes/labs/M-codex-sorry-loop/s4-pass-19-note.md
futon3c/holes/labs/M-codex-sorry-loop/s4-pass-32-note.md
futon3c/holes/labs/M-codex-sorry-loop/s4-pass-33-note.md
futon3c/holes/labs/M-codex-sorry-loop/s4-pass-34-note.md
futon3c/holes/labs/M-codex-sorry-loop/s4-pass-35-note.md
futon3c/holes/labs/M-codex-sorry-loop/scribe-pass-32-drafts.edn
futon3c/holes/labs/M-codex-sorry-loop/scribe-pass-33-drafts.edn
futon3c/holes/labs/M-codex-sorry-loop/scribe-pass-35-drafts.edn
futon3c/holes/labs/M-diagramprover/apm-driver/corpus-export/corpus.edn
futon3c/holes/labs/M-diagramprover/wm-wiring.edn
futon3c/holes/labs/M-invariant-queue-unstuck/psr/2026-04-29__derive__single-routing-authority.md
futon3c/holes/labs/M-invariant-queue-unstuck/pur/2026-04-29__instantiate__single-routing-authority.md
futon3c/holes/labs/M-loud-failure/hot-swap-drift-detector.md
futon3c/holes/labs/M-memory-retrieval/E9-pull-probe-prereg.md
futon3c/holes/labs/M-memory-retrieval/arm-attribution-backfill-20260801.edn
futon3c/holes/labs/M-memory-retrieval/attachment-export/JOIN.md
futon3c/holes/labs/M-memory-retrieval/capability-proof-store.md
futon3c/holes/labs/M-memory-retrieval/damage-state-scale-fixture-20260801.edn
futon3c/holes/labs/M-memory-retrieval/damage-state-scale-results-20260801.edn
futon3c/holes/labs/M-memory-retrieval/psi-v2-replay-results-20260728.edn
futon3c/holes/labs/M-memory-retrieval/receipts-export-20260728.edn
futon3c/holes/labs/M-single-locus/psr/2026-04-29__derive__single-locus-mission-home.md
futon3c/holes/labs/M-single-locus/pur/2026-04-29__instantiate__single-locus-mission-home.md
futon3c/holes/labs/M-typed-memories/connectivity-meter-20260727.edn
futon3c/holes/labs/M-typed-memories/latency-monitor-20260831.edn
futon3c/holes/labs/M-typed-memories/live-graph-export-20260727.edn
futon3c/holes/labs/M-typed-memories/phase3-trial-results.edn
futon3c/holes/labs/RUN4-serving-trust-gap-2026-09-10.md
futon3c/holes/labs/test-registry-read-endpoints-2026-09-19/drift-execution/2877171f-933e-4bef-b798-879784f2e491.closure.edn
futon3c/holes/labs/test-registry-read-endpoints-2026-09-19/execution/cedb8e78-cc8e-4c99-a116-728d5efa13cd.closure.edn
futon3c/holes/labs/test-registry-read-endpoints-2026-09-19/final-execution/f9524ab2-f1e2-4972-aec7-4df92d1cf2d9.closure.edn
futon3c/holes/labs/test-registry-read-endpoints-2026-09-19/register-drift.edn
futon3c/holes/labs/test-registry-read-endpoints-2026-09-19/register-final.edn
futon3c/holes/labs/test-registry-read-endpoints-2026-09-19/register-shape.edn
futon3c/holes/labs/test-registry-read-endpoints-2026-09-19/register.edn
futon3c/holes/labs/test-registry-read-endpoints-2026-09-19/shape-execution/271a106f-7d1d-4183-8896-f7d27ad1eefd.closure.edn
futon3c/holes/labs/wm-apparatus-tranche-two-2026-09-19/WM-08-failure-classification.payload.edn
futon3c/holes/labs/wm-apparatus-tranche-two-2026-09-19/WM-08-failure-classification/ffd791a2-1ecf-4c88-a4aa-adb2b4fcb6de.closure.edn
futon3c/holes/labs/wm-apparatus-tranche-two-2026-09-19/WM-09-predecessor-history.payload.edn
futon3c/holes/labs/wm-apparatus-tranche-two-2026-09-19/WM-09-predecessor-history/f07e9b5d-1519-4c61-80b9-082430f9109b.closure.edn
futon3c/holes/labs/wm-contract/runs/RUN4-F11-production-successor-2026-09-12/production-fold-commissioning-v5/run4-f11-production-successor-20260912-v1/attempt-001/002-selection.edn
futon3c/holes/labs/wm-contract/runs/RUN4-F11-production-successor-2026-09-12/production-fold-commissioning-v5/run4-f11-production-successor-20260912-v1/attempt-001/003-construction.edn
futon3c/holes/labs/wm-contract/runs/chip-board-cascade-verifier/run-2026-09-12.edn
futon3c/holes/labs/wm-contract/runs/r10-wiring-2026-09-14/receipt.edn
futon3c/holes/labs/wm-contract/runs/row-19-http-creator-integration-2026-09-13/execution-owner/execution-receipts.edn
futon3c/holes/labs/wm-contract/runs/row-19-http-creator-integration-2026-09-13/execution-owner/source-pins.edn
futon3c/holes/labs/wm-contract/runs/row-19-http-creator-integration-2026-09-13/execution-receipts.edn
futon3c/holes/labs/wm-contract/runs/row-19-http-creator-integration-2026-09-13/lifecycle/execution-receipts.edn
futon3c/holes/labs/wm-contract/runs/row-19-http-creator-integration-2026-09-13/lifecycle/source-pins.edn
futon3c/holes/labs/wm-contract/runs/row-19-http-creator-integration-2026-09-13/source-pins.edn
futon3c/holes/labs/wm-contract/runs/row-19-http-creator-integration-2026-09-13/worker-lifetime/execution-receipts.edn
futon3c/holes/labs/wm-contract/runs/row-19-http-creator-integration-2026-09-13/worker-lifetime/final/execution-receipts.edn
futon3c/holes/labs/wm-contract/runs/row-19-http-creator-integration-2026-09-13/worker-lifetime/final/source-pins.edn
futon3c/holes/labs/wm-contract/runs/row-19-http-creator-integration-2026-09-13/worker-lifetime/source-pins.edn
futon3c/holes/labs/wm-three-apparatus-registrations-2026-09-19/WM-08-failure-classification.payload.edn
futon3c/holes/labs/wm-three-apparatus-registrations-2026-09-19/WM-08-failure-classification/f1817fee-088e-4a31-aa29-918228817ecd.closure.edn
futon3c/holes/missions/E-g-incorporates-deltaT.md
futon3c/holes/missions/E-pattern-mining.md
futon3c/holes/missions/E-pilot-hop-trigger-wiring.md
futon3c/holes/missions/E-street-sweeper.md
futon3c/holes/missions/E-substrate-2-sorry-typing.md
futon3c/holes/missions/E-wm-live-recommendation.md
futon3c/holes/missions/E-wm-metric-redesign.md
futon3c/holes/missions/E-wm-staleness-meta-stop.md
futon3c/holes/missions/M-action-cost-modelling.md
futon3c/holes/missions/M-apm-demonstration.md
futon3c/holes/missions/M-bounded-in-flight-state.md
futon3c/holes/missions/M-codex-agent-behaviour.md
futon3c/holes/missions/M-codex-irc-execution.md
futon3c/holes/missions/M-cyder.md
futon3c/holes/missions/M-diagramprover.md
futon3c/holes/missions/M-dispatch-peripheral-bridge.md
futon3c/holes/missions/M-federated-agency-hardening.md
futon3c/holes/missions/M-first-flights.md
futon3c/holes/missions/M-futon3c-codex.md
futon3c/holes/missions/M-improve-irc.md
futon3c/holes/missions/M-invariant-queue-unstuck.md
futon3c/holes/missions/M-mission-control.md
futon3c/holes/missions/M-mission-peripheral.md
futon3c/holes/missions/M-mission-wiring.md
futon3c/holes/missions/M-peripheral-behavior.md
futon3c/holes/missions/M-peripheral-phenomenology.md
futon3c/holes/missions/M-pilot-appearance.md
futon3c/holes/missions/M-repl-wins-over-cli.md
futon3c/holes/missions/M-shared-memory-control-build-test.md
futon3c/holes/missions/M-single-locus.md
futon3c/holes/missions/M-structural-law.md
futon3c/holes/missions/M-substrate-metric.R1-report.md
futon3c/holes/missions/M-the-perfect-crime.md
futon3c/holes/missions/M-transport-adapters.md
futon3c/holes/missions/M-typed-holes-ARGUE.md
futon3c/holes/missions/M-typed-memories.md
futon3c/holes/missions/M-war-machine-first-outing.md
futon3c/holes/missions/M-war-machine-pilot.md
futon3c/holes/missions/M-war-machine-tuning.md
futon3c/holes/missions/alleycat-scorecard.md
futon3c/holes/missions/par-alfworld-inhabitation-2026-02-13.edn
futon3c/holes/missions/pilot-handoff/pilot-appearance-argument.aif.edn
futon3c/holes/notes/retrieval-strategy-pattern-conditioned-recall.md
futon3c/holes/ops/claude-6.md
futon3c/holes/qa/discipline-live-gate-2026-02-15T23-27-28-874513530Z.edn
futon3c/holes/qa/implementation-inventory.md
futon3c/holes/qa/issue-11-stepper-calibration.md
futon3c/holes/specs/repl.spec.edn
futon3c/holes/specs/traces/trace-witness-cg-5b03db29.edn
futon3c/holes/technotes/TN-APM-cascades-exist-unused.md
futon3c/holes/technotes/TN-APM-pattern-first-development-2026-09-10.md
futon3c/holes/technotes/TN-http-test-stable-failures-2026-09-01.md
futon3c/holes/technotes/TN-opus-f47-observation.md
futon3c/holes/technotes/TN-sonnet-F29-finding.md
futon3c/holes/technotes/TN-sonnet-f28-finding.md
futon3c/holes/technotes/TN-spec-delta-cannot-judge-and-trace-checker.md
futon3c/holes/technotes/ordinary-click-budget-2026-09-19/registry.edn
futon3c/holes/tickets/T-codex-auto-bellback.md
futon3c/holes/tickets/T-evidence-pinned-to-mutable-prose-26082026.md
futon3c/holes/tickets/T-strategic-cascade-emits-disconnected-patterns.md
futon3c/holes/tickets/T-typed-bell-arse-write-async.md
futon3c/holes/verification/V-typed-memory-dynamic-queries-20260724.md
futon3c/holes/zaif-cascade-coverage.edn
futon3c/holes/zaif-cascade-gate-holdout.edn
futon3c/holes/zaif-cascade-gate.edn
futon3c/resources/fixtures/pattern_memory_phase3_trials.edn
futon3c/scripts/adapters/cascade-report.md
futon3c/scripts/agency_send.py
futon3c/scripts/memory_latency_monitor.clj
futon3c/scripts/pattern_store_census.py
futon3c/scripts/zaif_cascade_gate.clj
futon3c/src/futon3c/aif/chipwitz.clj
futon3c/src/futon3c/aif/loop_learning.clj
futon3c/src/futon3c/aif/mission_head.clj
futon3c/src/futon3c/apm/cascade_dry_run.clj
futon3c/src/futon3c/enrichment/query.clj
futon3c/src/futon3c/evidence/boundary.clj
futon3c/src/futon3c/peripheral/mission_control_backend.clj
futon3c/src/futon3c/peripheral/mission_shapes.clj
futon3c/src/futon3c/peripheral/war_machine_pilot_backend.clj
futon3c/src/futon3c/scripts/mission_scope_ingest.clj
futon3c/src/futon3c/vsatarcs/feeder.clj
futon3c/test/chipwitz_test.clj
futon3c/test/fixtures/apm/f28-solver-promotion-string-enums.edn
futon3c/test/fixtures/apm/f29-proof-text-memory.edn
futon3c/test/fixtures/futon9a/essays/synthetic-slate/annotations.edn
futon3c/test/futon3c/agency/invariant_test.clj
futon3c/test/futon3c/agency/selective_form_loader_test.clj
futon3c/test/futon3c/apm/cascade_dry_run_test.clj
futon3c/test/futon3c/apm/conductor_test.clj
futon3c/test/futon3c/apm/cycle_harness_test.clj
futon3c/test/futon3c/apm/live_learning_phases_test.clj
futon3c/test/futon3c/apm/promotion_pipeline_test.clj
futon3c/test/futon3c/apm/promotion_review_store_test.clj
futon3c/test/futon3c/apm/role_memory_search_test.clj
futon3c/test/futon3c/enrichment/query_test.clj
futon3c/test/futon3c/logic/cascade_real_live_test.clj
futon3c/test/futon3c/peripheral/memory_write_test.clj
futon3c/test/futon3c/peripheral/pattern_memory_phase3_test.clj
futon3c/test/futon3c/peripheral/problem_test.clj
futon3c/test/futon3c/peripheral/war_machine_pilot_test.clj
futon3c/test/futon3c/watcher/multi_test.clj
futon3c/test/futon3c/watcher/projections/essay_test.clj
futon3c/test/futon3c/watcher/projections/flexiarg_test.clj
futon3c/test/resources/apm-regressions/f193-semantic-stall/queue-state.edn
futon3c/test/resources/apm-regressions/guide-submissions-2026-08-30.edn
futon4/README-rewriting.md
futon4/docs/vsatarcs-alignment-completeness.aif.edn
futon4/holes/campaign-lifecycle.md
futon4/holes/labs/M-peeragogy-rewrite/handoff-packages.md
futon4/holes/labs/M-peeragogy-rewrite/peeragogy-2016-prior.md
futon4/holes/labs/M-peeragogy-rewrite/peeragogy-handbook-reviewer-report-2026-04-29.md
futon4/holes/labs/M-peeragogy-rewrite/podcasts/posterior-notes/01-0N5cdNjsEfA.md
futon4/holes/labs/M-peeragogy-rewrite/podcasts/posterior-notes/02-kg91z9tUtt4.md
futon4/holes/labs/M-peeragogy-rewrite/podcasts/posterior-notes/03-Z9yeUmRoKOA.md
futon4/holes/labs/M-peeragogy-rewrite/podcasts/posterior-notes/08-HT92XcTm-0I.md
futon4/holes/mission-lifecycle-wm-alignment.md
futon4/holes/mission-lifecycle.md
futon4/holes/missions/M-futon-enrichment.md
futon4/holes/missions/M-interest-network-coupling.md
futon4/holes/missions/M-or-training-as-learning-system.md
futon4/holes/missions/M-peeragogy-rewrite.md
futon4/holes/missions/M-self-representing-stack.md
futon4/holes/missions/M-simulating-or-training-as-learning-system.md
futon4/holes/missions/M-three-column-stack.md
futon4/holes/missions/M-vsatarcs-invariants-integration.md
futon4/holes/missions/M-vsatarcs-writer.md
futon4/holes/missions/M-writing-ethics.md
futon5/README-mission-specification.md
futon5/holes/baldwin-notes/TN-baldwin-experiment-guidance.md
futon5/holes/missions/M-categorical-code.md
futon5/holes/missions/M-differentiable-code.md
futon5/holes/tech-notes/TN-baldwin-experiment-guidance.md
futon5/holes/tech-notes/paper/codex-coherence-audit.edn
futon5/holes/tech-notes/paper/coherence-annotations.edn
futon5/resources/exotype-xenotype-lift.edn
futon5/src/futon5/ct/mission.clj
futon5a/docs/joe-terminal-vocabulary.md
futon5a/essays/anthropic-fellows-2026/annotations-v1.edn
futon5a/essays/anthropic-fellows-2026/annotations-v5.edn
futon5a/essays/anthropic-fellows-2026/coherence-annotations-v5.edn
futon5a/essays/interest-reduce/interest_reduce_import.py
futon5a/essays/ukrn-open-research-training-plos-one/annotations.edn
futon5a/essays/ukrn-open-research-training-plos-one/revision-plan.md
futon5a/holes/excursions/E-interest-mining.md
futon5a/holes/excursions/E-wm-operator-observations.md
futon5a/holes/holistic-argument-semilattice.edn
futon5a/holes/labs/M-a-sorry-enterprise/affinity_score_v0.py
futon5a/holes/labs/M-learning-loop/capability-graph-contract.edn
futon5a/holes/labs/M-learning-loop/handoff-bridges.edn
futon5a/holes/labs/M-learning-loop/test_capability_contract.bb
futon5a/holes/labs/M-trip-journal/protections-as-specs.md
futon5a/holes/missions/M-expressions-of-interest.md
futon5a/holes/missions/M-recommendation-bindings.md
futon5a/holes/missions/M-stack-stereolithography.md
futon5a/holes/stories/futon-pilot-contra-claim.aif.edn
futon5a/holes/tech-notes/TN-halliday-clines.md
futon5a/holes/tech-notes/TN-mission-mention-lattice.md
futon6/README-mentor.md
futon6/holes/E-ground-G.md
futon6/holes/anatomy-of-a-futonic-mission.tex
futon6/holes/clean/agency-rebuild.clean.edn
futon6/holes/clean/aif-grounded-loop.clean.edn
futon6/holes/clean/autoclock-in.clean.edn
futon6/holes/clean/f6-ingest.clean.edn
futon6/holes/clean/invariant-queue-unstuck.clean.edn
futon6/holes/clean/pattern-ingest.clean.edn
futon6/holes/clean/patterns-done-right.clean.edn
futon6/holes/clean/single-entry-point.clean.edn
futon6/holes/clean/stepper-calibration.clean.edn
futon6/holes/closure-folds.edn
futon6/holes/early-closures.md
futon6/holes/excursions/E-informal-proof-checking.md
futon6/holes/excursions/cas0-worked-a96J01.md
futon6/holes/fold-turn-adjudications.edn
futon6/holes/handoffs/question-asking-as-reverse-morphogenesis.md
futon6/holes/handoffs/question-asking-pattern-mining-from-mo-rm-2026-03-06.md
futon6/holes/missions/E-mission-head.aif.edn
futon6/holes/missions/E-mission-head.md
futon6/holes/missions/E-mission-head.tex
futon6/holes/missions/M-P3-rational-reconstruction.md
futon6/holes/missions/M-P7-rational-reconstruction.md
futon6/holes/missions/M-P8-rational-reconstruction.md
futon6/holes/missions/M-artificial-stack-exchange.md
futon6/holes/missions/M-live-efe-map.md
futon6/holes/missions/M-metric-harness.md
futon6/holes/missions/M-superpod-mark3.md
futon6/scripts/aif_plus_method_audit.py
futon6/scripts/fold_embed/check_fold_embed_gates.py
futon6/src/futon6/arxiv_pattern_prompt.py
futon6/technote-arxiv-mining.md
futon6/tests/test_arxiv_pattern_prompt.py
futon6/tests/test_mission_scope_detect.py
futon6/tests/test_superpod_job_smoke.py
futon7/holes/M-autonomous-doc-maintenance.md
futon7/holes/M-demonstration-foundry.md
futon7/holes/M-interim-director-proxy-metric-inventory.md
futon7/holes/M-self-documenting-stack.md
futon7/holes/M-war-machine-aif-completion.md
futon7/holes/M-war-machine-frontend-upgrade1.md
futon7/holes/Q-CL1-decision-note.md
futon7/holes/missions/M-value-creation-loop.md
futon7/holes/pudding-prover-registry.edn
futon7a/essays/futonic-logic/futonic-logic.md
futon7a/essays/operator-foreword/annotations-v1.edn
futon7a/essays/operator-foreword/annotations.edn
futon7a/essays/reverse-morphogenesis/reverse-morphogenesis.md
futon7a/essays/the-woven-form/the-woven-form.md
futon7a/lab/pattern-cascade/cascade.edn
mathlib4/DarkTower/HandoffCascade.lean
mathlib4/DarkTower/R8Cascade.lean
mathlib4/DarkTower/WarMachine/CascadeOrder.lean
mathlib4/DarkTower/WarMachine/F11AppliedConformance.lean
mathlib4/DarkTower/WarMachine/F11NonSelfCertifying.lean
mathlib4/DarkTower/WarMachine/Holes.lean
p4ng/CHECKLIST-fundamentals.md
p4ng/app-wr-catalog.tex
p4ng/contents.tex
p4ng/detect_drift.py
p4ng/empirics-futon/NOTE-a-spider-for-the-edge-layer.md
p4ng/empirics-futon/NOTE-catalogue-on-the-same-grid.md
p4ng/empirics-futon/NOTE-the-one-edge-price.md
p4ng/empirics-futon/NOTE-wr-rulings-as-a-reduced-space.md
p4ng/empirics-futon/control-stages.edn
p4ng/empirics-futon/control_vacuity_probe.py
p4ng/empirics-futon/defect-repair-tally.edn
p4ng/empirics-futon/edge-fragments/_control-map_R2-R3a-R7-precision.edn
p4ng/empirics-futon/edge-fragments/_control-map_R5-R6-R14-score-field.edn
p4ng/empirics-futon/edge-fragments/_wm_run-once-receipt-chain.edn
p4ng/empirics-futon/factoring-table.tex
p4ng/empirics-futon/gen_control_stages.py
p4ng/empirics-futon/hyper-edge-schema.edn
p4ng/empirics-futon/issue-board.edn
p4ng/empirics-futon/r-wr-factoring.edn
p4ng/main.tex
p4ng/referents.edn
p4ng/sec-glossary.tex
p4ng/vetting/C229-BELIEF-REFERENT-REVET-2026-08-31.md
p4ng/vetting/C234-CASCADE-LANE-REFERENT-REVET-2026-08-31.md
p4ng/vetting/CLEANUP-QUEUE.md
p4ng/vetting/O20-DRIFT-BASELINE-2026-08-31.md
p4ng/wm-walkthroughs/01-prediction.md
p4ng/wm-walkthroughs/build-loop/closure/ARGUE-realness-requirements-2026-09-19.md
p4ng/wm-walkthroughs/build-loop/closure/AUDIT-flat-action-grain-2026-09-17.md
p4ng/wm-walkthroughs/build-loop/closure/AUDIT-flat-path-removal-2026-09-17.md
p4ng/wm-walkthroughs/build-loop/closure/DATA-cascade-outcomes-2026-09-17.edn
p4ng/wm-walkthroughs/build-loop/closure/E08-B-SELF-ACCOUNT.md
p4ng/wm-walkthroughs/build-loop/closure/PROPOSAL-pattern-interpretation.md
p4ng/wm-walkthroughs/build-loop/closure/WM-10-LIVE-SHADOW-G.md
p4ng/wm-walkthroughs/build-loop/closure/accept-receipts-r177.py
p4ng/wm-walkthroughs/build-loop/closure/enact-realness-r97.py
p4ng/wm-walkthroughs/build-loop/closure/record-e02-audit-r154.py
p4ng/wm-walkthroughs/build-loop/closure/record-judge-reload-r178.py
p4ng/wm-walkthroughs/build-loop/closure/record-lean-reverified-r109.py
p4ng/wm-walkthroughs/build-loop/vm/VM-PROTOCOL.md
p4ng/wm-walkthroughs/build-loop/vm/tick-001/03-R6.edn
p4ng/wm-walkthroughs/build-loop/vm/tick-001/04-R13.edn
p4ng/wm-walkthroughs/build-loop/vm/tick-001/05-R4.edn
p4ng/wm-walkthroughs/build-loop/vm/tick-001/06-R5.edn
p4ng/wm-walkthroughs/build-loop/vm/tick-001/07-R14.edn
p4ng/wm-walkthroughs/build-loop/vm/tick-001/08-R16.edn
p4ng/wm-walkthroughs/build-loop/vm/tick-001/09-R9.edn
p4ng/wm-walkthroughs/build-loop/vm/tick-001/11-R10-wiring.edn
p4ng/wm-walkthroughs/cascades/NEXT-DESIGN.md
p4ng/wm-walkthroughs/cascades/r1/README.md
p4ng/wm-walkthroughs/cascades/r10/README.md
p4ng/wm-walkthroughs/cascades/r11/README.md
p4ng/wm-walkthroughs/cascades/r12/README.md
p4ng/wm-walkthroughs/cascades/r13/README.md
p4ng/wm-walkthroughs/cascades/r14/README.md
p4ng/wm-walkthroughs/cascades/r15/README.md
p4ng/wm-walkthroughs/cascades/r16/README.md
p4ng/wm-walkthroughs/cascades/r17-variants/README.md
p4ng/wm-walkthroughs/cascades/r17/README.md
p4ng/wm-walkthroughs/cascades/r20/README.md
p4ng/wm-walkthroughs/cascades/r3/README.md
p4ng/wm-walkthroughs/cascades/r6/README.md
p4ng/wm-walkthroughs/cascades/r8/README.md
p4ng/wm-walkthroughs/cascades/r9/README.md
p4ng/wm-walkthroughs/cascades/remaining-wm/author.py
p4ng/wm-walkthroughs/cascades/wm-01/README.md
p4ng/wm-walkthroughs/cascades/wm-01/render.py
p4ng/wm-walkthroughs/cascades/wm-02/README.md
p4ng/wm-walkthroughs/cascades/wm-02/render.py
p4ng/wm-walkthroughs/cascades/wm-03/README.md
p4ng/wm-walkthroughs/cascades/wm-03/render.py
p4ng/wm-walkthroughs/cascades/wm-04/README.md
p4ng/wm-walkthroughs/cascades/wm-04/render.py
p4ng/wm-walkthroughs/cascades/wm-08/README.md
p4ng/wm-walkthroughs/collections/README.md
p4ng/wm-walkthroughs/collections/connected-learning/README.md
p4ng/wm-walkthroughs/collections/pattern-design/README.md
p4ng/wm-walkthroughs/collections/prediction/README.md
p4ng/wm-walkthroughs/collections/purpose-and-account/README.md
p4ng/wm-walkthroughs/collections/qualifying-run/README.md
p4ng/wm-walkthroughs/item-owners/closure-plans/LF-observe/VERDICT-2026-09-18.md
p4ng/wm-walkthroughs/item-owners/closure-plans/e09/CLAIM-ACCOUNT-1.md
p4ng/wm-walkthroughs/item-owners/closure-plans/r12/VERDICT-r12-two-layer-calibration.md
p4ng/wm-walkthroughs/item-owners/closure-plans/wm-02/PINNED-DISPATCH-2026-09-18.md
p4ng/wm-walkthroughs/item-owners/closure-plans/wm-02/RUN-SPEC-D-OCCURRENCE-2026-09-18.md
p4ng/wm-walkthroughs/item-owners/closure-plans/wm-05/PLAN.md
p4ng/wm-walkthroughs/item-owners/closure-plans/wm-05/owner-audit.md
p4ng/wm-walkthroughs/item-owners/closure-plans/wm-06/PINNED-DISPATCH-2026-09-18.md
p4ng/wm-walkthroughs/item-owners/evidence/OPS-serving-activation-2026-09-18.md
voxterm/fixtures/apm-cascade-strip/ledger.edn
```

</details>

### Candidate 3: changed paths

<details><summary>Expand full path inventory</summary>

| Repository / path | Current lines | Parent-absent | Earliest touching commit |
|---|---:|---|---|
| `apm-lean/ConstructionTargets/BooleanTwoCoverScaffold.lean` | 127 | yes | `44d294de46d11dea32c4dfc52576f9c544308e2f` |
| `apm-lean/ConstructionTargets/BooleanTwoCoverScaffold.md` | 60 | yes | `44d294de46d11dea32c4dfc52576f9c544308e2f` |
| `apm-lean/ConstructionTargets/BoundaryTorusH0Cokernel.lean` | 98 | yes | `fbf7ab2a144f0c7f704e0ea4ad801be8de3a8330` |
| `apm-lean/ConstructionTargets/BoundaryTorusH0Cokernel.md` | 18 | yes | `fbf7ab2a144f0c7f704e0ea4ad801be8de3a8330` |
| `apm-lean/ConstructionTargets/BoundaryTorusH1ProjectiveSplitting.lean` | 185 | yes | `442d2041cae5a0aba51240f78f22165df44a4174` |
| `apm-lean/ConstructionTargets/BoundaryTorusH1ProjectiveSplitting.md` | 19 | yes | `442d2041cae5a0aba51240f78f22165df44a4174` |
| `apm-lean/ConstructionTargets/BoundaryTorusH2ConnectingIso.lean` | 77 | yes | `40277a00f1a7c2e9f0f02cc1f216c9ba53844eb3` |
| `apm-lean/ConstructionTargets/BoundaryTorusH2ConnectingIso.md` | 16 | yes | `40277a00f1a7c2e9f0f02cc1f216c9ba53844eb3` |
| `apm-lean/ConstructionTargets/BoundaryTorusIntegralHomology.lean` | 250 | yes | `a5f296cbda49fc3100719148bb1d3f256886c5f8` |
| `apm-lean/ConstructionTargets/BoundaryTorusIntegralHomology.md` | 26 | yes | `a5f296cbda49fc3100719148bb1d3f256886c5f8` |
| `apm-lean/ConstructionTargets/BoundaryTorusIntegralHomologyComplete.lean` | 90 | yes | `e26485d1348f88fbef22397ee4975b0e0d75f0da` |
| `apm-lean/ConstructionTargets/BoundaryTorusIntegralHomologyComplete.md` | 19 | yes | `e26485d1348f88fbef22397ee4975b0e0d75f0da` |
| `apm-lean/ConstructionTargets/BoundaryTorusMayerVietoris.lean` | 287 | yes | `b56aae2e5437d208a92b1e2b659b3190158d31ce` |
| `apm-lean/ConstructionTargets/BoundaryTorusMayerVietoris.md` | 43 | yes | `b56aae2e5437d208a92b1e2b659b3190158d31ce` |
| `apm-lean/ConstructionTargets/BoundaryTorusMayerVietorisAdjacentExactness.lean` | 285 | yes | `4d046f0c2f4605e9509de54537c52c06a8c405c2` |
| `apm-lean/ConstructionTargets/BoundaryTorusMayerVietorisAdjacentExactness.md` | 21 | yes | `4d046f0c2f4605e9509de54537c52c06a8c405c2` |
| `apm-lean/ConstructionTargets/BoundaryTorusMayerVietorisAlgebra.lean` | 155 | yes | `15073ebb5e7a1768d7e04f5d9b954eb15eb3045e` |
| `apm-lean/ConstructionTargets/BoundaryTorusMayerVietorisAlgebra.md` | 29 | yes | `15073ebb5e7a1768d7e04f5d9b954eb15eb3045e` |
| `apm-lean/ConstructionTargets/BoundaryTorusMayerVietorisExactness.lean` | 106 | yes | `7a5a5649cbee57a248475bc977a4592aa17d9a83` |
| `apm-lean/ConstructionTargets/BoundaryTorusMayerVietorisExactness.md` | 20 | yes | `7a5a5649cbee57a248475bc977a4592aa17d9a83` |
| `apm-lean/ConstructionTargets/BoundaryTorusOpenCover.lean` | 305 | yes | `f8b7553e5de023524244aece4e1a574166d2226d` |
| `apm-lean/ConstructionTargets/BoundaryTorusOpenCover.md` | 28 | yes | `f8b7553e5de023524244aece4e1a574166d2226d` |
| `apm-lean/ConstructionTargets/CircleUniversalCover.lean` | 349 | yes | `22ef3d01c84a80e52092a42d2fd07d2640696a07` |
| `apm-lean/ConstructionTargets/CircleUniversalCover.md` | 23 | yes | `22ef3d01c84a80e52092a42d2fd07d2640696a07` |
| `apm-lean/ConstructionTargets/ConnectingMapNegationNaturality.lean` | 54 | yes | `de8e2c3297fff8efe4c8fd8effa1dc20661e4985` |
| `apm-lean/ConstructionTargets/ConnectingMapNegationNaturality.md` | 18 | yes | `de8e2c3297fff8efe4c8fd8effa1dc20661e4985` |
| `apm-lean/ConstructionTargets/CoverSmallRelativeProjectionComparison.lean` | 143 | yes | `314663d481115cc054a54dec7ab006c20a2bbced` |
| `apm-lean/ConstructionTargets/CoverSmallRelativeProjectionComparison.md` | 20 | yes | `314663d481115cc054a54dec7ab006c20a2bbced` |
| `apm-lean/ConstructionTargets/EmptyAndSumSingularHomology.lean` | 50 | yes | `3014401cae2901d89837eda73327d2fc6919cc5e` |
| `apm-lean/ConstructionTargets/EmptyAndSumSingularHomology.md` | 22 | yes | `3014401cae2901d89837eda73327d2fc6919cc5e` |
| `apm-lean/ConstructionTargets/FundamentalGroupAbelianizationNaturality.lean` | 73 | yes | `8c96558aebd2bdbc9d9dcdb28c7bd25ae8852e11` |
| `apm-lean/ConstructionTargets/FundamentalGroupAbelianizationNaturality.md` | 24 | yes | `8c96558aebd2bdbc9d9dcdb28c7bd25ae8852e11` |
| `apm-lean/ConstructionTargets/FundamentalGroupProduct.lean` | 51 | yes | `f3e675ada3624e221282522e5316fb44fe355dbd` |
| `apm-lean/ConstructionTargets/FundamentalGroupProduct.md` | 17 | yes | `f3e675ada3624e221282522e5316fb44fe355dbd` |
| `apm-lean/ConstructionTargets/HomologyBiprodIso.lean` | 54 | yes | `b1727b179b77a57893e9cd7281b976107cfb54d1` |
| `apm-lean/ConstructionTargets/HomologyBiprodIso.md` | 15 | yes | `b1727b179b77a57893e9cd7281b976107cfb54d1` |
| `apm-lean/ConstructionTargets/HomologyBiprodNormalization.lean` | 291 | yes | `e2a004db4f9550d58510bfc1c5d4a8b0efc20b31` |
| `apm-lean/ConstructionTargets/HomologyBiprodNormalization.md` | 54 | yes | `e2a004db4f9550d58510bfc1c5d4a8b0efc20b31` |
| `apm-lean/ConstructionTargets/IntegralSingularHomologyComparison.lean` | 362 | no | `2e65f0f73bfa43e4cc14290faf5224c8636b837a` |
| `apm-lean/ConstructionTargets/IntegralSingularHomologyComparison.md` | 27 | yes | `2e65f0f73bfa43e4cc14290faf5224c8636b837a` |
| `apm-lean/ConstructionTargets/MayerVietorisConnectingSwap.lean` | 137 | yes | `d1894e454df029f001fa74e9160e9f7342c8f41b` |
| `apm-lean/ConstructionTargets/MayerVietorisConnectingSwap.md` | 14 | yes | `d1894e454df029f001fa74e9160e9f7342c8f41b` |
| `apm-lean/ConstructionTargets/NestedSubspaceIntersectionComparison.lean` | 145 | yes | `7846a89f1ae9598a5a1b491cbbacf5ba072a7feb` |
| `apm-lean/ConstructionTargets/NestedSubspaceIntersectionComparison.md` | 19 | yes | `7846a89f1ae9598a5a1b491cbbacf5ba072a7feb` |
| `apm-lean/ConstructionTargets/PairConnectingMayerVietorisComparison.lean` | 124 | yes | `b80492e4d3387d1c33775be5203b1b970cfca341` |
| `apm-lean/ConstructionTargets/PairConnectingMayerVietorisComparison.md` | 25 | yes | `b80492e4d3387d1c33775be5203b1b970cfca341` |
| `apm-lean/ConstructionTargets/ProductionOpenCoverMayerVietoris.lean` | 835 | no | `8beb7ab1f8cb62089dd5a01a2c41c9cce9699e51` |
| `apm-lean/ConstructionTargets/ProductionOpenCoverMayerVietoris.md` | 255 | no | `8beb7ab1f8cb62089dd5a01a2c41c9cce9699e51` |
| `apm-lean/ConstructionTargets/ProductionPairConnectingMayerVietorisComparison.lean` | 70 | yes | `348546ebf2c0e4d10dd347ef0056ad45093b4729` |
| `apm-lean/ConstructionTargets/ProductionPairConnectingMayerVietorisComparison.md` | 22 | yes | `348546ebf2c0e4d10dd347ef0056ad45093b4729` |
| `apm-lean/ConstructionTargets/ProjectiveQuotientExactSplitting.lean` | 77 | yes | `c08f946d77048b2604889f60a2fd66cb49873edd` |
| `apm-lean/ConstructionTargets/ProjectiveQuotientExactSplitting.md` | 24 | yes | `c08f946d77048b2604889f60a2fd66cb49873edd` |
| `apm-lean/ConstructionTargets/SignedDiagonalCokernel.lean` | 181 | yes | `915ee169646ea938dfdf31819c21a451e55f78e0` |
| `apm-lean/ConstructionTargets/SignedDiagonalCokernel.md` | 26 | yes | `915ee169646ea938dfdf31819c21a451e55f78e0` |
| `apm-lean/ConstructionTargets/SingularExcision.lean` | 2015 | no | `134d35ce4a599fab7a8526e4b62f8cc2f909fad0` |
| `apm-lean/ConstructionTargets/SingularExcision.md` | 76 | no | `134d35ce4a599fab7a8526e4b62f8cc2f909fad0` |
| `apm-lean/ConstructionTargets/SingularH0Augmentation.lean` | 306 | yes | `781eed21cfb046a06c96e449fde5ea5fb9eedbb8` |
| `apm-lean/ConstructionTargets/SingularH0Augmentation.md` | 29 | yes | `781eed21cfb046a06c96e449fde5ea5fb9eedbb8` |
| `apm-lean/ConstructionTargets/SingularPathChain.lean` | 691 | yes | `3c04cd165205062c164da8edd95044407b221f6e` |
| `apm-lean/ConstructionTargets/SingularPathChain.md` | 51 | yes | `3c04cd165205062c164da8edd95044407b221f6e` |
| `apm-lean/ConstructionTargets/SingularSubdivision.lean` | 3967 | no | `b66a9ee34566a51f039d814c33caf05c96f88160` |
| `apm-lean/ConstructionTargets/SingularSubdivision.md` | 282 | no | `b66a9ee34566a51f039d814c33caf05c96f88160` |
| `apm-lean/ConstructionTargets/SolidTorusGluingOpenCover.lean` | 1374 | yes | `dbfa574220a31c15715a45db7f9a50d943ac6704` |
| `apm-lean/ConstructionTargets/SolidTorusGluingOpenCover.md` | 43 | yes | `dbfa574220a31c15715a45db7f9a50d943ac6704` |
| `apm-lean/ConstructionTargets/SolidTorusHomotopyAndHomology.lean` | 154 | yes | `193df1afce1e7f138dc5176bb3e7d31348f7a945` |
| `apm-lean/ConstructionTargets/SolidTorusHomotopyAndHomology.md` | 19 | yes | `193df1afce1e7f138dc5176bb3e7d31348f7a945` |
| `apm-lean/ConstructionTargets/SphereAntipodalHomology.lean` | 514 | yes | `f1e0280ba08dcd62fb3cf5c811051ee62141a2cc` |
| `apm-lean/ConstructionTargets/SphereAntipodalHomology.md` | 73 | yes | `f1e0280ba08dcd62fb3cf5c811051ee62141a2cc` |
| `apm-lean/ConstructionTargets/SphereEquatorGluingOpenCover.lean` | 1920 | yes | `bb7a7c676adee3c6424da0ba8d889f35e9d33149` |
| `apm-lean/ConstructionTargets/SphereEquatorGluingOpenCover.md` | 122 | yes | `bb7a7c676adee3c6424da0ba8d889f35e9d33149` |
| `apm-lean/ConstructionTargets/T00A01MayerVietorisAdapters.lean` | 233 | yes | `c59a4dd9c2c90e3f9f046c5b43e471dc8879fce7` |
| `apm-lean/ConstructionTargets/T00A01MayerVietorisAdapters.md` | 37 | yes | `c59a4dd9c2c90e3f9f046c5b43e471dc8879fce7` |
| `apm-lean/ConstructionTargets/T01J03FrozenQuotientRepresentation.lean` | 159 | yes | `61676ffe59ade6b1f6d117be68a16b6e458089f9` |
| `apm-lean/ConstructionTargets/T01J03FrozenQuotientRepresentation.md` | 22 | yes | `61676ffe59ade6b1f6d117be68a16b6e458089f9` |
| `apm-lean/ConstructionTargets/T02A06HemisphereCoordinates.lean` | 264 | no | `c6b6a239076977bb09b0a9e2ae888832185f8f90` |
| `apm-lean/ConstructionTargets/T02A06HemisphereCoordinates.md` | 6 | yes | `c6b6a239076977bb09b0a9e2ae888832185f8f90` |
| `apm-lean/ConstructionTargets/T96A03DegreeTwoMayerVietoris.lean` | 184 | yes | `efa4805eb5a21f3e414f9440570a5f03a7390678` |
| `apm-lean/ConstructionTargets/T96A03DegreeTwoMayerVietoris.md` | 21 | yes | `efa4805eb5a21f3e414f9440570a5f03a7390678` |
| `apm-lean/ConstructionTargets/T96A03DegreeTwoNormalization.lean` | 41 | yes | `21301fae445bcd75160044529327591a2c3888e2` |
| `apm-lean/ConstructionTargets/T96A03DegreeTwoNormalization.md` | 13 | yes | `21301fae445bcd75160044529327591a2c3888e2` |
| `apm-lean/ConstructionTargets/T96A03FrozenQuotientRepresentation.lean` | 130 | yes | `b3b3f1538555efb44a34fdbb1a745233ab508f80` |
| `apm-lean/ConstructionTargets/T96A03FrozenQuotientRepresentation.md` | 23 | yes | `b3b3f1538555efb44a34fdbb1a745233ab508f80` |
| `apm-lean/ConstructionTargets/T96A03NonDegreeTwoHomology.lean` | 185 | yes | `5aecf0c31f69a76a32838a62aac15f3002bd3f8c` |
| `apm-lean/ConstructionTargets/T96A03NonDegreeTwoHomology.md` | 18 | yes | `5aecf0c31f69a76a32838a62aac15f3002bd3f8c` |
| `apm-lean/ConstructionTargets/TopologicalSumSingularHomology.lean` | 315 | yes | `a91884647f3a92f24ce1901381fd3546edb86243` |
| `apm-lean/ConstructionTargets/TopologicalSumSingularHomology.md` | 24 | yes | `a91884647f3a92f24ce1901381fd3546edb86243` |
| `apm-lean/ConstructionTargets/UnionToRelativeProductionComparison.lean` | 138 | yes | `37e3a5d2ba3ae7dc365803496d286fce557455fe` |
| `apm-lean/ConstructionTargets/UnionToRelativeProductionComparison.md` | 26 | yes | `37e3a5d2ba3ae7dc365803496d286fce557455fe` |
| `apm-lean/TOPOLOGY-DEPENDENCY-DAG.md` | 1357 | no | `ed83a6648161b64d5e422d66971d8354f5170d60` |
| `apm-lean/problems/a98A04/lean/Main.lean` | 465 | no | `f5351ea02603316232577411fb6f27182aff86fb` |
| `apm-lean/problems/t00A01/lean/Main.lean` | 509 | no | `f5b0e191d380345d4d4469f07eb0d6659de786e0` |
| `apm-lean/problems/t01J03/lean/Main.lean` | 482 | no | `d4d472710b5c7b73256269d080435a0e6ef5b36b` |
| `apm-lean/problems/t96A03/lean/Main.lean` | 338 | no | `f6cfdaf87137860ee10617bdc9572cddc8acec11` |
| `apm-lean/scratch2.lean` | not present/readable | yes | `c2a1177c2b9f480811cc495269e82930902fd69f` |
| `futon3c/data/apm-campaigns/ftriangle-live-smoke-v1/config.edn` | 20 | no | `e9e180e6c0466223bbc1dbb7f169e8d55db12f2d` |
| `futon3c/holes/labs/M-apm-demonstration/ftriangle-live-smoke-manifest-v1.edn` | 30 | no | `e9e180e6c0466223bbc1dbb7f169e8d55db12f2d` |
| `futon3c/scripts/apm-frame-pulse.py` | 328 | no | `6727bacc7d98b8d8a558ee63df30c9c6a5bea233` |
| `futon3c/scripts/apm-watch.sh` | 199 | no | `27bbbd9f4b2fb6160310cd9ad96b17d8add06b0b` |
| `futon3c/src/futon3c/apm/countdown_control.clj` | 2625 | no | `e4f63a69bd17e137737bb0f0a270c2dafeb3d197` |
| `futon3c/src/futon3c/apm/durable_coordinator.clj` | 1077 | no | `e4f63a69bd17e137737bb0f0a270c2dafeb3d197` |
| `futon3c/src/futon3c/apm/ftriangle_live_smoke.clj` | 543 | no | `e9e180e6c0466223bbc1dbb7f169e8d55db12f2d` |
| `futon3c/src/futon3c/apm/jit_queue_coordinator.clj` | 221 | no | `d2c8013b5805006db02314f3695b1d6023611a24` |
| `futon3c/src/futon3c/apm/live_preflight_runtime.clj` | 248 | no | `e94ed8b850a6dd732bc3990023e3240c2987998a` |
| `futon3c/src/futon3c/apm/live_regulator.clj` | 364 | no | `e4f63a69bd17e137737bb0f0a270c2dafeb3d197` |
| `futon3c/src/futon3c/apm/semantic_progress_watchdog.clj` | 256 | no | `ef34262b59a2a973c49cb1d2a88bf96445a5dfa9` |
| `futon3c/src/futon3c/transport/http.clj` | 9669 | no | `ec230708679f16c22065c35ee747d28fabdf50d2` |
| `futon3c/test/futon3c/apm/disruption_soak_test.clj` | 137 | no | `e4f63a69bd17e137737bb0f0a270c2dafeb3d197` |
| `futon3c/test/futon3c/apm/durable_coordinator_test.clj` | 1136 | no | `e4f63a69bd17e137737bb0f0a270c2dafeb3d197` |
| `futon3c/test/futon3c/apm/ftriangle_live_smoke_test.clj` | 276 | no | `e9e180e6c0466223bbc1dbb7f169e8d55db12f2d` |
| `futon3c/test/futon3c/apm/jit_queue_coordinator_test.clj` | 347 | no | `e4f63a69bd17e137737bb0f0a270c2dafeb3d197` |
| `futon3c/test/futon3c/apm/library_lane_coordinator_test.clj` | 255 | no | `e4f63a69bd17e137737bb0f0a270c2dafeb3d197` |
| `futon3c/test/futon3c/apm/live_preflight_runtime_test.clj` | 116 | no | `e94ed8b850a6dd732bc3990023e3240c2987998a` |
| `futon3c/test/futon3c/apm/live_regulator_test.clj` | 329 | no | `e4f63a69bd17e137737bb0f0a270c2dafeb3d197` |
| `futon3c/test/futon3c/apm/semantic_progress_watchdog_test.clj` | 564 | no | `ef34262b59a2a973c49cb1d2a88bf96445a5dfa9` |
| `futon3c/test/futon3c/transport/auto_bellback_test.clj` | 693 | no | `ec230708679f16c22065c35ee747d28fabdf50d2` |
| `futon3c/test/futon3c/transport/http_test.clj` | 3411 | no | `e94ed8b850a6dd732bc3990023e3240c2987998a` |

</details>

<details><summary>Expand complete commit list</summary>

```text
apm-lean 44d294de46d11dea32c4dfc52576f9c544308e2f 2026-08-27T20:49:24Z Add Boolean two-cover scaffold
apm-lean 781eed21cfb046a06c96e449fde5ea5fb9eedbb8 2026-08-27T20:59:28Z add natural integral H0 augmentation
apm-lean bb7a7c676adee3c6424da0ba8d889f35e9d33149 2026-08-27T21:02:30Z Add sphere equator gluing open cover geometry
apm-lean 915ee169646ea938dfdf31819c21a451e55f78e0 2026-08-27T21:05:11Z identify the signed diagonal cokernel
apm-lean ed83a6648161b64d5e422d66971d8354f5170d60 2026-08-27T21:09:15Z reconcile topology dependency DAG
apm-lean 134d35ce4a599fab7a8526e4b62f8cc2f909fad0 2026-08-27T21:10:55Z Add intersection range coherence
apm-lean 8beb7ab1f8cb62089dd5a01a2c41c9cce9699e51 2026-08-27T21:16:33Z Add production Mayer-Vietoris objects
apm-lean ad1cff988dd2dfabdd7bac812e0dcb4d781657bb 2026-08-27T21:20:32Z Normalize production homology isomorphisms
apm-lean e2a004db4f9550d58510bfc1c5d4a8b0efc20b31 2026-08-27T21:23:47Z Normalize homology maps across biproducts
apm-lean b1727b179b77a57893e9cd7281b976107cfb54d1 2026-08-27T21:28:26Z Extract canonical homology biproduct isomorphism
apm-lean 88b8635b722cbd588bec0002248600d11fbbc037 2026-08-27T21:36:32Z Normalize signed homology lifts after biproduct maps
apm-lean 11b20af1c3fd15d293c20f15cea58d327f2c4bc7 2026-08-27T21:42:24Z Normalize whiskered signed homology lifts
apm-lean f242c6ebffe63dee5b10f12152b8ec3842de64d6 2026-08-27T21:46:39Z Add fully associated homology normalization
apm-lean 621699fba1cc051760b8b1490007805896f34ecf 2026-08-27T21:51:50Z Identify the production MV intersection arrow
apm-lean edef025af9c4406057084ee1456edfb16a9c5c3e 2026-08-27T21:58:39Z Identify the Boolean cover-small union
apm-lean 12b6a091ac316499953e0945ed5da00544bab9c4 2026-08-27T22:02:48Z Identify Boolean cover member inclusions
apm-lean b98a03bc9aa31cc1dc22c7152042a8144c658951 2026-08-27T22:07:01Z Normalize mapped homology reassembly components
apm-lean f5351ea02603316232577411fb6f27182aff86fb 2026-08-27T22:09:27Z prove spike superlevel interval
apm-lean dbd2d8cb539effc51e4ed30b4c409be05dc725a9 2026-08-27T22:13:44Z prove normalized spike integral
apm-lean fb09df81e1407b50baaa0a1208c37c0516ab55d3 2026-08-27T22:15:24Z Prove production Mayer-Vietoris reassembly equality
apm-lean cccf459d5c212fc475fe8fbd1e95a52b85934b91 2026-08-27T22:17:42Z build dense weighted spike family
apm-lean d03946a0aa8def6ddef328ac29aa2f00f8354967 2026-08-27T22:24:34Z Package production open-cover Mayer-Vietoris exactness
apm-lean 02044e21801f383b135d2aa290e7e33bafb711ae 2026-08-27T22:24:40Z prove weighted spikes converge almost everywhere
apm-lean 400f537641b40e0cb41007dfb8c7f8c168ce4f4d 2026-08-27T22:30:01Z complete dense integrable spike construction
apm-lean a44c13968e0733f4d697e2dc72b9ed73ea56cf9d 2026-08-27T22:34:24Z Derive nonemptiness from integral zeroth homology
apm-lean 1a926373a6d2b551603a0e2451177c97161c8802 2026-08-27T22:40:53Z Generalize signed diagonal cokernel coordinates
apm-lean b66a9ee34566a51f039d814c33caf05c96f88160 2026-08-27T22:49:50Z Identify singular postcomposition representations
apm-lean c75a86a38911a73e6ff2c7e97c509a258ba76799 2026-08-27T22:56:39Z Normalize topological subspace inclusion maps
apm-lean c59a4dd9c2c90e3f9f046c5b43e471dc8879fce7 2026-08-27T23:02:46Z Add t00A01 Mayer-Vietoris adapters
apm-lean 43a492daf1efa00cec9ef17ca989d616de2fc655 2026-08-27T23:05:57Z Expose t00A01 overlap H0 monicity
apm-lean f5b0e191d380345d4d4469f07eb0d6659de786e0 2026-08-27T23:16:29Z Close t00A01 connected-sum homology bridge
apm-lean 47f88869152383c55de54e14f679ab30e42b473a 2026-08-27T23:18:58Z Reconcile topology DAG after t00A01 homology closure
apm-lean f181ffa2b60e83371382ea53887b2fe528b65a00 2026-08-27T23:37:37Z Add sum decompositions for gluing cover sources
apm-lean 1c2ca5131a9db32fd848bfcfcf32b47163f53ddf 2026-08-27T23:42:14Z Add source homotopies for sphere gluing cover
apm-lean e2636b3ed5fba48efe76463f26912043b52c7ac3 2026-08-27T23:47:01Z Prove left gluing homotopy coherence
apm-lean c1d0782dd58262ef32831f560e034e4adcd358a0 2026-08-27T23:56:11Z Prove gluing source homotopy coherence
apm-lean c6c724dd55360e62d960103db0fb8ccf42150bad 2026-08-28T00:00:14Z Descend left gluing source homotopy
apm-lean eb7a21fc962b444a9332c91e07baca2d4e3cf6ca 2026-08-28T00:04:43Z Descend sphere gluing piece homotopies
apm-lean 1c079228925eb3039a835aa0aa0554922eaacae2 2026-08-28T00:11:50Z Normalize sphere gluing source homotopy endpoints
apm-lean e04f43ec0ea90e1182294eeb8a99cbe2f405e306 2026-08-28T00:21:49Z Descend sphere gluing source retraction
apm-lean 2c616654f107ad0d7d61e88d070a8b6f516799dd 2026-08-28T00:29:25Z Package left sphere gluing homotopy equivalence
apm-lean 4ca79e09e16da4204dc55ba364f0b4bce6b74a41 2026-08-28T00:34:04Z Package right sphere gluing homotopy equivalence
apm-lean f7399011b085d3a84e440a3589e0ec888c193af7 2026-08-28T00:40:12Z Package sphere gluing intersection equivalence
apm-lean 21301fae445bcd75160044529327591a2c3888e2 2026-08-28T00:41:55Z Add t96A03 degree-two module normalization
apm-lean c08f946d77048b2604889f60a2fd66cb49873edd 2026-08-28T00:46:13Z Add projective quotient exact splitting
apm-lean c6b6a239076977bb09b0a9e2ae888832185f8f90 2026-08-28T00:48:22Z Add positive-dimensional equator nonemptiness
apm-lean b3b3f1538555efb44a34fdbb1a745233ab508f80 2026-08-28T00:49:01Z Add t96A03 quotient representation adapter
apm-lean f6cfdaf87137860ee10617bdc9572cddc8acec11 2026-08-28T00:51:44Z Connect t96A03 frozen and reusable quotients
apm-lean 5aecf0c31f69a76a32838a62aac15f3002bd3f8c 2026-08-28T00:55:15Z Compute equator gluing homology outside degree two
apm-lean efa4805eb5a21f3e414f9440570a5f03a7390678 2026-08-28T00:57:29Z Compute degree-two homology of equator gluing
apm-lean adcd54a2e49f448449467410cfa93afbfbc980da 2026-08-28T00:58:44Z Complete t96A03 homology calculation
apm-lean 13c5968926d383a3e860026870047e5b02fd9b39 2026-08-28T01:01:57Z Record t96A03 topology scaffold closure
apm-lean dbfa574220a31c15715a45db7f9a50d943ac6704 2026-08-28T01:04:19Z Add solid torus gluing open cover
apm-lean 6b0cc8113cc80f6ccb599df2fe52f3a20f2b2a9a 2026-08-28T01:10:44Z Add solid torus annular deformation
apm-lean 81e2d81d4c0329d61d63a6bd6ffb32e4206f51d2 2026-08-28T01:17:42Z Add quotient-piece solid torus homotopies
apm-lean b094da36a6e083e678a3c37a792b8202a967a5ce 2026-08-28T01:23:34Z Complete solid torus quotient-piece equivalences
apm-lean 61676ffe59ade6b1f6d117be68a16b6e458089f9 2026-08-28T01:27:05Z Add t01J03 quotient representation adapter
apm-lean 2e65f0f73bfa43e4cc14290faf5224c8636b837a 2026-08-28T01:28:34Z prove integral homology comparison naturality
apm-lean d4d472710b5c7b73256269d080435a0e6ef5b36b 2026-08-28T01:30:29Z Connect t01J03 quotient representation
apm-lean f1e0280ba08dcd62fb3cf5c811051ee62141a2cc 2026-08-28T01:30:38Z add zero-sphere antipodal homology calculation
apm-lean 193df1afce1e7f138dc5176bb3e7d31348f7a945 2026-08-28T01:34:45Z Compute solid torus homology by contraction
apm-lean 4426b0683e28e97c8fc4df4529130f0bb785e2f4 2026-08-28T01:36:32Z compare antipodal punctured sphere projections
apm-lean b90d79fd401aa56096a8ddf89e43d78fed463c35 2026-08-28T01:50:22Z prove functoriality of relative singular maps
apm-lean f8b7553e5de023524244aece4e1a574166d2226d 2026-08-28T01:52:27Z Add boundary torus open cover geometry
apm-lean 965c59c5fbe2ee0308814a29b2006a2545de1062 2026-08-28T01:57:03Z Prove antipodal hemisphere excision naturality
apm-lean b56aae2e5437d208a92b1e2b659b3190158d31ce 2026-08-28T01:57:40Z Add boundary torus homology coordinates
apm-lean 5c995b4a440be3787a1f9203f3bdd8694fdc41da 2026-08-28T02:00:47Z Normalize boundary torus overlap inclusions
apm-lean 85e3d8d9b8698ca4d0e7cac71999845ff4685132 2026-08-28T02:05:57Z Prove literal antipodal disk coordinate squares
apm-lean 15073ebb5e7a1768d7e04f5d9b954eb15eb3045e 2026-08-28T02:08:24Z Add signed fold Mayer Vietoris algebra
apm-lean 3014401cae2901d89837eda73327d2fc6919cc5e 2026-08-28T02:08:55Z Prove singular homology vanishes on empty space
apm-lean a91884647f3a92f24ce1901381fd3546edb86243 2026-08-28T02:14:13Z Add topological sum singular homology isomorphism
apm-lean de8e2c3297fff8efe4c8fd8effa1dc20661e4985 2026-08-28T02:15:13Z Prove connecting map negation naturality
apm-lean d1894e454df029f001fa74e9160e9f7342c8f41b 2026-08-28T02:22:33Z Prove Mayer-Vietoris connecting map swap sign
apm-lean c3dcbffcfa670d3be31548510ff4a3c1beb7f2d3 2026-08-28T02:28:53Z Normalize topological sum homology components
apm-lean b80492e4d3387d1c33775be5203b1b970cfca341 2026-08-28T02:30:28Z compare pair and Mayer-Vietoris connecting maps
apm-lean 348546ebf2c0e4d10dd347ef0056ad45093b4729 2026-08-28T02:34:31Z transport relative connecting maps to pair chains
apm-lean 7846a89f1ae9598a5a1b491cbbacf5ba072a7feb 2026-08-28T02:39:18Z compare nested intersection pair chains
apm-lean 85a63571d8ae94125846ad5235d2ae63cd0764f0 2026-08-28T02:40:54Z Add boundary torus homology fold coordinates
apm-lean 37e3a5d2ba3ae7dc365803496d286fce557455fe 2026-08-28T02:44:43Z transport cover-small union to relative chains
apm-lean a5f296cbda49fc3100719148bb1d3f256886c5f8 2026-08-28T02:47:00Z Add boundary torus homology integration coordinates
apm-lean 314663d481115cc054a54dec7ab006c20a2bbced 2026-08-28T02:53:00Z Compare cover-small and relative projections
apm-lean 0364befc4dc96053e6cf71350db4dfe9ba80d857 2026-08-28T02:57:09Z Normalize boundary torus intersection maps
apm-lean 7a5a5649cbee57a248475bc977a4592aa17d9a83 2026-08-28T03:03:21Z Transport boundary torus Mayer-Vietoris exactness
apm-lean 4d046f0c2f4605e9509de54537c52c06a8c405c2 2026-08-28T03:12:03Z Transport adjacent boundary torus exactness
apm-lean 40277a00f1a7c2e9f0f02cc1f216c9ba53844eb3 2026-08-28T03:18:24Z Identify boundary torus second homology
apm-lean 442d2041cae5a0aba51240f78f22165df44a4174 2026-08-28T03:26:06Z Split boundary torus H1 exact sequence
apm-lean e26485d1348f88fbef22397ee4975b0e0d75f0da 2026-08-28T03:32:48Z Complete positive-degree boundary torus homology
apm-lean fbf7ab2a144f0c7f704e0ea4ad801be8de3a8330 2026-08-28T03:39:09Z Compute boundary torus H0 by Mayer Vietoris
apm-lean aa8915ec1ecdb64160b37900ac5fa5086501920d 2026-08-28T03:43:31Z Expose boundary torus homology in t01J03
apm-lean f3e675ada3624e221282522e5316fb44fe355dbd 2026-08-28T03:46:54Z Construct fundamental group product equivalence
apm-lean 22ef3d01c84a80e52092a42d2fd07d2640696a07 2026-08-28T03:52:18Z Add concrete circle universal cover geometry
apm-lean 69bd1a7d5cabd0903d17e499fe7fe33996a4179f 2026-08-28T03:55:25Z add homotopy-invariant circle lift endpoint
apm-lean af239588dd186f475dd434800b9dd9d265cd41f3 2026-08-28T04:01:48Z Add circle lift realization and concatenation laws
apm-lean d904b32121566e51e39f18950512b730305cbf4e 2026-08-28T04:07:20Z classify circle loops by lifted endpoint
apm-lean 228ce52b0cbaf3134f1cb4d260e86ed0979d99e7 2026-08-28T04:10:21Z Close t01J03 boundary torus invariants
apm-lean 8c96558aebd2bdbc9d9dcdb28c7bd25ae8852e11 2026-08-28T04:15:48Z add fundamental group abelianization naturality
apm-lean 3c04cd165205062c164da8edd95044407b221f6e 2026-08-28T04:20:27Z Add singular path chain boundary and naturality
apm-lean 569f6988a8c22a9324bab3726525a0ff9c8c8b78 2026-08-28T04:25:12Z add raw singular path homotopy prism
apm-lean 9e145f9abf20f0565f9fec405688a17adce449b3 2026-08-28T04:32:30Z Prove path homotopy invariance in singular H1
apm-lean a5eaf0a7302c67b82140f4b5627b8d3844d2054f 2026-08-28T04:37:40Z reduce path concatenation to subdivision formula
apm-lean c2a1177c2b9f480811cc495269e82930902fd69f 2026-08-28T04:49:16Z f49 a98A04 student-attempt-1 scratch, rescued from /tmp
apm-lean 5d9fa3d9da565d97aa3cdde48422ed8bc348492b 2026-08-28T04:53:04Z compute concatenation triangle face coordinates
futon3c e9e180e6c0466223bbc1dbb7f169e8d55db12f2d 2026-08-27T20:38:06Z isolate Ftriangle fixture and checkpoint traversal evidence
futon3c 4cc383cd17b8299c3481ebfd02960d556ac844ef 2026-08-27T20:38:29Z resume only the existing Ftriangle coordinator
futon3c e3513a1ae0fd3619cf58d00cbbdc8c5fa755caea 2026-08-27T20:43:30Z distinguish Ftriangle harness failures and repair dispatch input
futon3c e8b799a019ce3e3d4b59da31ab3fd3ddaead3614 2026-08-27T20:51:58Z mint distinct Ftriangle repair successor identity
futon3c ec230708679f16c22065c35ee747d28fabdf50d2 2026-08-27T20:56:39Z finalize non-seat invoke delivery dispositions
futon3c e94ed8b850a6dd732bc3990023e3240c2987998a 2026-08-27T21:00:41Z preserve delivery observation through terminal projections
futon3c 6ad6d55b34656494e939af1ea054a6b754108677 2026-08-27T21:05:46Z hide half-complete job finalization from readers
futon3c 7f88b93e7c4df7360d5e0e2e0947a0f3844b7707 2026-08-27T21:10:50Z type Ftriangle effect port boundaries
futon3c e4f63a69bd17e137737bb0f0a270c2dafeb3d197 2026-08-27T21:14:55Z fix(apm): drain coordinators before witnessed stop
futon3c 27bbbd9f4b2fb6160310cd9ad96b17d8add06b0b 2026-08-27T21:57:06Z Do not change subject mid-comparison
futon3c d2c8013b5805006db02314f3695b1d6023611a24 2026-08-27T22:01:44Z Give JIT tick intents scheduler deadlines
futon3c ef34262b59a2a973c49cb1d2a88bf96445a5dfa9 2026-08-27T22:45:45Z Distinguish waiting tick claims from stale claims
futon3c 6727bacc7d98b8d8a558ee63df30c9c6a5bea233 2026-08-27T22:45:59Z Count reassign as the accept it is
futon3c 0a0f16cce7bfc4a86787e5ededb67eb6989418d5 2026-08-28T04:52:03Z Budget JIT tick intents by dispatched work
```

</details>

<details><summary>Expand outside reference files (literal matches, not all runtime dependencies)</summary>

```text
apm-lean/ConstructionTargets.lean
apm-lean/ConstructionTargets/BoundaryTorusHurewiczIsomorphism.lean
apm-lean/ConstructionTargets/ChartPairRelativeHomologyTransport.lean
apm-lean/ConstructionTargets/CircleActualLocalization.lean
apm-lean/ConstructionTargets/CircleAngleTransition.lean
apm-lean/ConstructionTargets/CircleHurewiczIsomorphism.lean
apm-lean/ConstructionTargets/CircleMetricFundamentalGroup.lean
apm-lean/ConstructionTargets/ClassicalLocalPathConnectivity.lean
apm-lean/ConstructionTargets/CoefficientAmbientSubspaceRangeComparison.lean
apm-lean/ConstructionTargets/CoefficientAmbientSubspaceRangeCompatibility.lean
apm-lean/ConstructionTargets/CoefficientAmbientSubspaceRangeNaturality.lean
apm-lean/ConstructionTargets/CoefficientBarycentricNaturality.lean
apm-lean/ConstructionTargets/CoefficientCompactComplementCoverSequence.lean
apm-lean/ConstructionTargets/CoefficientCompactComplementNestedExcisionChains.lean
apm-lean/ConstructionTargets/CoefficientCompactComplementNestedExcisionSource.lean
apm-lean/ConstructionTargets/CoefficientCompactComplementPiecesComparison.lean
apm-lean/ConstructionTargets/CoefficientCompactComplementTransportedExactness.lean
apm-lean/ConstructionTargets/CoefficientCompactComplementUnionComparison.lean
apm-lean/ConstructionTargets/CoefficientCompactComplementUnionRelativeFormulas.lean
apm-lean/ConstructionTargets/CoefficientCompactComplementUnionToRelative.lean
apm-lean/ConstructionTargets/CoefficientEventualSmallness.lean
apm-lean/ConstructionTargets/CoefficientH0.lean
apm-lean/ConstructionTargets/CoefficientIntersectionRangeComparison.lean
apm-lean/ConstructionTargets/CoefficientIntersectionRangeNaturality.lean
apm-lean/ConstructionTargets/CoefficientNestedSubspaceIntersectionComparison.lean
apm-lean/ConstructionTargets/CoefficientPairConnectingMayerVietorisComparison.lean
apm-lean/ConstructionTargets/CoefficientPieceRangeBiproductComparison.lean
apm-lean/ConstructionTargets/CoefficientRangeIntersectionToRightComparison.lean
apm-lean/ConstructionTargets/CoefficientRangeRightRelativeCokernelComparison.lean
apm-lean/ConstructionTargets/CoefficientRangeRightToProductionPairComparison.lean
apm-lean/ConstructionTargets/CoefficientRelativeSmall.lean
apm-lean/ConstructionTargets/CoefficientSubcomplexRelativeDegreeRangeOrder.lean
apm-lean/ConstructionTargets/CoefficientSubcomplexRelativeMayerVietorisArrows.lean
apm-lean/ConstructionTargets/CoefficientSubspaceUnionCoverSmallComparison.lean
apm-lean/ConstructionTargets/CoefficientSubspaceUnionRangeFactorization.lean
apm-lean/ConstructionTargets/CoefficientSubspaceUnionRangeHomologyEquivalence.lean
apm-lean/ConstructionTargets/CoefficientSubspaceUnionRangeInclusion.lean
apm-lean/ConstructionTargets/CoefficientSubspaceUnionRelativeCokernelSquare.lean
apm-lean/ConstructionTargets/CoefficientSubspaceUnionRestrictedAmbientRange.lean
apm-lean/ConstructionTargets/CoefficientSubspaceUnionRestrictedCoverImage.lean
apm-lean/ConstructionTargets/CoefficientSubspaceUnionRestrictedRangeIso.lean
apm-lean/ConstructionTargets/CoefficientUnionToProductionRelativeComparison.lean
apm-lean/ConstructionTargets/CoveringSimplexTransfer.lean
apm-lean/ConstructionTargets/DegreeOneHurewiczDirectClosing.lean
apm-lean/ConstructionTargets/DegreeOneHurewiczMap.lean
apm-lean/ConstructionTargets/DegreeOneHurewiczSurjective.lean
apm-lean/ConstructionTargets/DegreeOneHurewiczSurjectivity.lean
apm-lean/ConstructionTargets/DegreeOneHurewiczSurjectivityFinal.lean
apm-lean/ConstructionTargets/FiniteFreeHurewiczNaturality.lean
apm-lean/ConstructionTargets/LocalHomologyNeighborhoodExcision.lean
apm-lean/ConstructionTargets/ModelPairLocalHomologyOne.lean
apm-lean/ConstructionTargets/NestedOrientedChartCompatibility.lean
apm-lean/ConstructionTargets/OneDimensionalChartLocalGenerator.lean
apm-lean/ConstructionTargets/OneDiskIncreasingOrientation.lean
apm-lean/ConstructionTargets/OneDiskLocalOrientation.lean
apm-lean/ConstructionTargets/OrientedOverlapPuncturedCircle.lean
apm-lean/ConstructionTargets/ProductSimplexAffineRealization.lean
apm-lean/ConstructionTargets/RelativeSingularConnectingNaturality.lean
apm-lean/ConstructionTargets/SingularCapLowDegree.lean
apm-lean/ConstructionTargets/SphereHomology.lean
apm-lean/ConstructionTargets/SphereOneH1Concrete.lean
apm-lean/ConstructionTargets/SphereThreeProductMayerVietoris.lean
apm-lean/ConstructionTargets/T02A06DiskPairShift.lean
apm-lean/ConstructionTargets/T02A06DiskRadialComparison.lean
apm-lean/ConstructionTargets/T02A06HemisphereExcision.lean
apm-lean/ConstructionTargets/T02A06HomologyScaffold.lean
apm-lean/ConstructionTargets/T02A06SphereH0.lean
apm-lean/ConstructionTargets/T02A06SphereOne.lean
apm-lean/ConstructionTargets/T02A06SphereShift.lean
apm-lean/ConstructionTargets/T02A06SphereShiftStandalone.lean
apm-lean/ConstructionTargets/T95J04FinalProductComparison.lean
apm-lean/ConstructionTargets/TorusCircleWedgeConnectorComparison.lean
apm-lean/ConstructionTargets/TorusCircleWedgeMayerVietoris.lean
apm-lean/ConstructionTargets/TwiceSpiralAlgebraicTargetNaturality.lean
apm-lean/ConstructionTargets/TwiceSpiralBasedCoordinateInterface.lean
apm-lean/ConstructionTargets/TwiceSpiralBasedPseudopushoutExtraction.lean
apm-lean/ConstructionTargets/TwiceSpiralConnectorExistence.lean
apm-lean/ConstructionTargets/TwiceSpiralConnectorLegCompatibility.lean
apm-lean/ConstructionTargets/TwiceSpiralConnectorReconstructionComparison.lean
apm-lean/ConstructionTargets/TwiceSpiralConnectorReconstructionNaturality.lean
apm-lean/ConstructionTargets/TwiceSpiralConnectorVertexData.lean
apm-lean/ConstructionTargets/TwiceSpiralConnectorVertexEquivalence.lean
apm-lean/ConstructionTargets/TwiceSpiralConnectorVertexExtraction.lean
apm-lean/ConstructionTargets/TwiceSpiralConnectorVertexMorphisms.lean
apm-lean/ConstructionTargets/TwiceSpiralConnectorVertexRoundTrip.lean
apm-lean/ConstructionTargets/TwiceSpiralCoordinateVertexEquivalence.lean
apm-lean/ConstructionTargets/TwiceSpiralCoordinateVertexRepresentations.lean
apm-lean/ConstructionTargets/TwiceSpiralFinalTargetNaturality.lean
apm-lean/ConstructionTargets/TwiceSpiralFundamentalGroupExtraction.lean
apm-lean/ConstructionTargets/TwiceSpiralFundamentalGroupoidCoordinates.lean
apm-lean/ConstructionTargets/TwiceSpiralFundamentalGroupoidOpenCover.lean
apm-lean/ConstructionTargets/TwiceSpiralGluingHomology.lean
apm-lean/ConstructionTargets/TwiceSpiralGluingHomologyConsequences.lean
apm-lean/ConstructionTargets/TwiceSpiralGluingHomologyTable.lean
apm-lean/ConstructionTargets/TwiceSpiralH1Coordinates.lean
apm-lean/ConstructionTargets/TwiceSpiralPseudococoneTransport.lean
apm-lean/ConstructionTargets/TwiceSpiralPushoutRepresentationRoundTrip.lean
apm-lean/ConstructionTargets/TwiceSpiralPushoutSingleObjEquivalence.lean
apm-lean/ConstructionTargets/TwiceSpiralRepresentabilityCandidates.lean
apm-lean/ConstructionTargets/TwiceSpiralSourceEquivalence.lean
apm-lean/ConstructionTargets/TwiceSpiralVertexTargetNaturality.lean
apm-lean/LEMMA-INDEX.md
apm-lean/Reports/t02A06-declaration-reachability.md
apm-lean/Reports/t02A06DeclarationReachability.lean
apm-lean/Reports/t03J02-defect.md
apm-lean/Reports/t03J02DeclarationReachability.lean
apm-lean/Reports/t98J03-checkpoint-10.md
apm-lean/Reports/t98J03-declaration-reachability.md
apm-lean/Reports/t98J03-library-increment.md
apm-lean/Reports/t98J03DeclarationReachability.lean
apm-lean/docs/TN-codex-t00J02-strategy.md
apm-lean/holes/labs/topology-contract/evidence/t94j08-zero-sphere/T94J08ZeroSphereRefutation.lean
apm-lean/holes/labs/topology-contract/worklist.edn
apm-lean/holes/labs/topology-contract/worklist2.edn
apm-lean/problems/t02A03/lean/Main.lean
apm-lean/problems/t91J02/lean/Main.lean
apm-lean/problems/t92J05/lean/Main.lean
apm-lean/problems/t97A02/lean/Main.lean
futon0/analysis/audits/SOURCES-work-records-2026-09-21.md
futon1b/seed/substrate-slice.edn
futon1b/textprobe/history-versions-full.edn
futon1b/textprobe/history-versions.edn
futon2/INSTALL.md
futon2/data/capability_zones/harvest-2026-07-19-3d.edn
futon2/data/capability_zones/harvest-2026-07-19.edn
futon2/holes/E-aif-docs-live.md
futon2/holes/labs/A-next-a-sorry-enterprise/a-sorry-enterprise-sorry-EMPIRICAL.edn
futon2/holes/labs/A-next-codex-agent-behaviour/codex-agent-behaviour-sorry-EMPIRICAL.edn
futon2/holes/labs/A-next-typed-bells/typed-bells-sorry-EMPIRICAL.edn
futon2/holes/labs/M-legacy-sorry-cleanup/legacy-sorries-snapshot.edn
futon2/holes/labs/library-loop/runs/mining-p3-transport-boundary/cascade.edn
futon2/holes/labs/wm-contract/C301-agency-snapshot-revision-design.md
futon2/holes/labs/wm-contract/C536-U65-registered-seat-callers-and-job-grain-cancel.md
futon2/holes/labs/wm-contract/TN-F13-runtime-seam-discovery-2026-09-15.md
futon2/holes/labs/wm-contract/TN-paper13-paper07-closure-path-2026-09-15.md
futon2/holes/labs/wm-contract/TN-row17-discovery-2026-09-12.md
futon2/holes/labs/wm-contract/TN-row19-scoping-2026-09-12.md
futon2/holes/labs/wm-contract/TN-row19-serving-retention-deployment-2026-09-13.md
futon2/holes/labs/wm-contract/facts-R2.md
futon2/holes/labs/wm-contract/runs/RUN4-preparation-2026-09-10/EXECUTION-PATH.md
futon2/holes/labs/wm-contract/runs/RUNTIME-VALIDATION-CATALOG.edn
futon2/holes/labs/wm-contract/runs/fundamentals-checklist-2026-09-15/futon2__holes__labs__wm-contract__TN-paper13-paper07-closure-path-2026-09-15.md
futon2/holes/labs/wm-contract/runs/row-17-r9-trace-noncredit-2026-09-12/non-credit-record.edn
futon2/holes/labs/wm-contract/runs/row-19-commission-retention-2026-09-13/execution-receipts.edn
futon2/holes/labs/wm-contract/runs/row-19-commission-retention-2026-09-13/implementation-note.md
futon2/holes/labs/wm-contract/runs/row-19-commission-retention-2026-09-13/review-fixes/execution-receipts.edn
futon2/holes/labs/wm-contract/runs/row-19-commission-retention-2026-09-13/review-fixes/r1-separate-store/execution-receipts.edn
futon2/holes/labs/wm-contract/runs/row-19-commission-retention-2026-09-13/review-fixes/r1-separate-store/retry-hardening/execution-receipts.edn
futon2/holes/labs/wm-contract/runs/row-19-selective-loader-2026-09-13/execution-receipts.edn
futon2/holes/labs/wm-contract/runs/row-22-e6b-store-protocol-2026-09-13/source-pins.edn
futon2/holes/labs/wm-contract/runs/row-26-on-demand-entrypoint-2026-09-13/execution-receipts.edn
futon2/holes/labs/wm-contract/runs/row-26-on-demand-entrypoint-2026-09-13/source-pins.edn
futon2/holes/labs/wm-contract/runs/tldr-pass1-2026-09-20/sections/S12.md
futon2/holes/labs/wm-contract/runs/tldr-pass1-2026-09-20/sections/S16.md
futon2/holes/labs/wm-contract/runs/tldr-pass1-2026-09-20/sections/S35.md
futon2/holes/labs/wm-contract/runs/wm-build-loop-2026-09-15/join-6/RECORD.md
futon2/holes/labs/wm-contract/variable-situation-accounting.edn
futon2/holes/labs/wm-contract/worklist.edn
futon2/holes/labs/zaif-harness/runs/PA11z-exemplar/lifecycle-census.edn
futon2/holes/labs/zaif-harness/runs/U14d-consumer-census.md
futon2/holes/labs/zaif-harness/worklist.edn
futon2/resources/sorrys.edn
futon2/scripts/generate_variable_situation_accounting.bb
futon3/checks/how_witness_heartbeat.clj
futon3/checks/how_witness_split_transport.clj
futon3/holes/excursions/E-pattern-peripheral.md
futon3/holes/labs/library-contract/worklist.edn
futon3/holes/missions/M-futon3x-e2e.md
futon3/holes/war-bulletin-8.md
futon3c/README-drawbridge.md
futon3c/README-walkie-talkie.md
futon3c/docs/TN-validated-system-HOWTO.md
futon3c/docs/invariants.md
futon3c/docs/repl-parity-claims.edn
futon3c/docs/system-now-next.md
futon3c/docs/technote-codex-code-invariants.md
futon3c/docs/technote-portfolio-inference-debt.md
futon3c/docs/technote-smart-cursor-external-e2e-handoff.md
futon3c/docs/wiring-claims.edn
futon3c/docs/wiring-contract.md
futon3c/holes/C251-invoke-ledger-durability-discovery.md
futon3c/holes/C254-atomic-invoke-ledger-snapshot.md
futon3c/holes/C263-invoke-ledger-schema-and-post-rename.md
futon3c/holes/NOTE-agency-accounting-gaps-2026-09-21.md
futon3c/holes/T-apm-recurring-failure-end-to-end.md
futon3c/holes/T-typed-submission-wrapper-cancellation-evidence.md
futon3c/holes/evidence/run-participants-2026-09-19/b0dac719-2345-439a-8c4d-0da74cd7b26e.closure.edn
futon3c/holes/evidence/run-participants-2026-09-19/d9f384c6-fdf6-4382-b499-7c2ad59645b2.closure.edn
futon3c/holes/evidence/run-participants-2026-09-19/registry.edn
futon3c/holes/excursions/E-bell-clink-adapter.p1-report.md
futon3c/holes/excursions/E-bell-clink-adapter.p2-report.md
futon3c/holes/excursions/E-futon-memories.md
futon3c/holes/excursions/E-promotion-deadlock-discovery.md
futon3c/holes/labs/E-futon-memories/s1-results-note.md
futon3c/holes/labs/E-futon-memories/s1_topology.py
futon3c/holes/labs/M-apm-demonstration/analysis/pattern-first-development-2026-09-10/check_catalog.bb
futon3c/holes/labs/M-apm-demonstration/analysis/pattern-first-development-2026-09-10/probe.py
futon3c/holes/labs/M-codex-sorry-loop/harvest-dryrun-019f8b63.edn
futon3c/holes/labs/RUN4-serving-trust-gap-2026-09-10.md
futon3c/holes/labs/test-registry-read-endpoints-2026-09-19/drift-execution/2877171f-933e-4bef-b798-879784f2e491.closure.edn
futon3c/holes/labs/test-registry-read-endpoints-2026-09-19/execution/cedb8e78-cc8e-4c99-a116-728d5efa13cd.closure.edn
futon3c/holes/labs/test-registry-read-endpoints-2026-09-19/final-execution/f9524ab2-f1e2-4972-aec7-4df92d1cf2d9.closure.edn
futon3c/holes/labs/test-registry-read-endpoints-2026-09-19/register-drift.edn
futon3c/holes/labs/test-registry-read-endpoints-2026-09-19/register-final.edn
futon3c/holes/labs/test-registry-read-endpoints-2026-09-19/register-shape.edn
futon3c/holes/labs/test-registry-read-endpoints-2026-09-19/register.edn
futon3c/holes/labs/test-registry-read-endpoints-2026-09-19/shape-execution/271a106f-7d1d-4183-8896-f7d27ad1eefd.closure.edn
futon3c/holes/labs/test-registry-validation-cli-2026-09-19/execution/24f8de92-bec0-4074-a67a-c05f1f074dfd.closure.edn
futon3c/holes/labs/wm-contract/runs/r10-wiring-2026-09-14/receipt.edn
futon3c/holes/labs/wm-contract/runs/row-19-http-creator-integration-2026-09-13/execution-owner/execution-receipts.edn
futon3c/holes/labs/wm-contract/runs/row-19-http-creator-integration-2026-09-13/execution-owner/source-pins.edn
futon3c/holes/labs/wm-contract/runs/row-19-http-creator-integration-2026-09-13/execution-receipts.edn
futon3c/holes/labs/wm-contract/runs/row-19-http-creator-integration-2026-09-13/lifecycle/execution-receipts.edn
futon3c/holes/labs/wm-contract/runs/row-19-http-creator-integration-2026-09-13/lifecycle/source-pins.edn
futon3c/holes/labs/wm-contract/runs/row-19-http-creator-integration-2026-09-13/source-pins.edn
futon3c/holes/labs/wm-contract/runs/row-19-http-creator-integration-2026-09-13/worker-lifetime/execution-receipts.edn
futon3c/holes/labs/wm-contract/runs/row-19-http-creator-integration-2026-09-13/worker-lifetime/final/execution-receipts.edn
futon3c/holes/labs/wm-contract/runs/row-19-http-creator-integration-2026-09-13/worker-lifetime/final/source-pins.edn
futon3c/holes/labs/wm-contract/runs/row-19-http-creator-integration-2026-09-13/worker-lifetime/source-pins.edn
futon3c/holes/missions/E-pattern-mining.md
futon3c/holes/missions/E-pilot-hop-trigger-wiring.md
futon3c/holes/missions/E-street-sweeper.md
futon3c/holes/missions/E-wm-live-recommendation.md
futon3c/holes/missions/E-wm-staleness-meta-stop.md
futon3c/holes/missions/M-codex-agent-behaviour.md
futon3c/holes/missions/M-codex-irc-execution.md
futon3c/holes/missions/M-cyder.md
futon3c/holes/missions/M-federated-agency-hardening.md
futon3c/holes/missions/M-futon3c-codex.md
futon3c/holes/missions/M-improve-irc.md
futon3c/holes/missions/M-invariant-queue-unstuck.md
futon3c/holes/missions/M-mission-control.md
futon3c/holes/missions/M-mission-wiring.md
futon3c/holes/missions/M-repl-wins-over-cli.md
futon3c/holes/missions/M-transport-adapters.md
futon3c/holes/missions/M-war-machine-tuning.md
futon3c/holes/qa/implementation-inventory.md
futon3c/holes/technotes/TN-apm-acceptance-ordering-decision-points-2026-09-12.md
futon3c/holes/technotes/TN-apm-watcher.md
futon3c/holes/technotes/TN-bank-audit.p3-report.md
futon3c/holes/technotes/TN-fable-F30-findings.md
futon3c/holes/technotes/TN-http-test-stable-failures-2026-09-01.md
futon3c/holes/technotes/TN-memory-caption-jobA-consumer-trace-2026-09-10.md
futon3c/holes/technotes/TN-reason-code-vocabulary.md
futon3c/holes/technotes/TN-solver-blocked-to-target.md
futon3c/holes/technotes/ordinary-click-budget-2026-09-19/registry.edn
futon3c/holes/tickets/T-codex-auto-bellback.md
futon3c/holes/tickets/T-typed-bell-arse-write-async.md
futon3c/scripts/agency_send.py
futon3c/src/futon3c/enrichment/query.clj
futon3c/src/futon3c/evidence/boundary.clj
futon3c/test/futon3c/agency/selective_form_loader_test.clj
futon3c/test/futon3c/enrichment/query_test.clj
futon4/holes/missions/M-futon-enrichment.md
futon4/holes/missions/M-vsatarcs-invariants-integration.md
futon5a/holes/holistic-argument-semilattice.edn
futon5a/holes/missions/M-recommendation-bindings.md
futon5a/holes/missions/M-stack-stereolithography.md
futon5a/holes/tech-notes/TN-mission-mention-lattice.md
futon7/holes/M-autonomous-doc-maintenance.md
futon7/holes/M-self-documenting-stack.md
futon7/holes/M-war-machine-frontend-upgrade1.md
p4ng/empirics-futon/NOTE-the-one-edge-price.md
p4ng/wm-walkthroughs/item-owners/closure-plans/wm-02/RUN-SPEC-D-OCCURRENCE-2026-09-18.md
```

</details>

## Boundary evidence for the three candidates

Candidate 1, last operator turn: `emacs-a91ed2004097d99e2e890f272f004ec3`, `2026-09-10T00:26:23.841569620Z`.

> apm loop supervisor gone... i don't know why this recurs

Candidate 1, next operator turn: `emacs-1cbaac08d56874a409e4e50771fe329b`, `2026-09-10T11:16:58.853915366Z`.

> So it occurs to me that the memory system that we're using in the APM loop, Could be an interesting way to start. Implementing some of our learning-related goals in the War Machine. But that presupposes the memory system that we've designed. I've been testing out over the last. Couple dozen. Frames in the APM loop. It's actually now working the way it's hoped for. And to explore that would require a bit of an audit. Of that system. Looking at previous tech notes that have been written, and probably writing a new one, catching up on. Whether it's actually working as desired now. At the same time, I would also like to get back to the... Cascade Live. Investigation of How we're going to implement those. Hai level. Requirements. And whether we're going to think about them as I'll see you next time. Institutions or Patterns. Or what? So let's have that in the back of our mind while we think about this memory system.

Candidate 2, last operator turn: `emacs-83e6d8456820d0bacf82b1dcd60269f3`, `2026-09-05T12:03:45.173815169Z`.

> i agree with your recommendations on tge other 2

Candidate 2, next operator turn: `emacs-bdb82f48c62df1acd8364817f54619a1`, `2026-09-06T13:56:21.237392764Z`.

> Well, we've certainly run into some kind of blockage over the last... 24 hours. Can you look into why the system has stopped?

Candidate 3, last operator turn: `emacs-baa0934c8d5c8944b86c9377dfd3c108`, `2026-08-27T20:37:44.504450554Z`.

> OK... now my question is, can we run continuously on this programme with chained parks, bells to Agency agents, and see whether we make more progess that way than we did with the old t00J02 problem approach?

Candidate 3, next operator turn: `emacs-b9ad5e8ec0234f5763e46acc6aeff194`, `2026-08-28T04:56:28.803906975Z`.

> can i have an overview of work done overnight?

