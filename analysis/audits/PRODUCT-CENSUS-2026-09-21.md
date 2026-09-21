# Weekly product census, 2026-08-01 through 2026-09-21

Discovery by codex-14 for Joe / claude-5. Snapshot started **2026-09-21T20:55:39.125155+00:00**. September 21 is partial. This measures observable artifacts and retention, **not desirability, scientific value, successful deployment, or the value of a paper submission**. Different units are never summed. A reference or import is a use proxy, not an endorsement.

[Chart and downloads](https://zone.hyperreal.enterprises/wip/audits/product-census-2026-09-21.html). Files: `product-census-2026-09-21.csv`, `product-census-2026-09-21.svg`, `product-census-2026-09-21-manifest.json`, `product-census-2026-09-21-ledger.jsonl.gz`. The ledger has producing SHA, timestamp, path, week, presence proxy, counts and citation/import witnesses. The manifest pins all 45 HEADs and ref tips, source hash, checks and limitations. No classifier, test suite, Lake build, or service call was run.

## Three August–September contrasts

1. **More code per day, less expansion in named idea documents.** August added 78,832 src lines over 31 calendar days; September added 83,880 over 21 date bins (last partial): 2,543 versus at least 3,994 lines/day, **57% higher**. Named idea docs moved from 284 (9.16/day) to 166 (at least 7.90/day); this cannot measure chat-only ideas or unversioned memories. Code surviving today is 60,266 known August lines (2,014 added lines have unknown survival) and 74,831 September lines. Newer work has had less time to be revised or removed.
2. **More retained tests and Lean files, but retention is not validation.** New test files: August 288, 264 retained; September 436, 429 retained. Lean: 1,007 / 640 retained versus 990 / 952 retained. Only 119 August and 181 September new Lean files are statically reachable through the explicitly configured library targets under the bounded import scan. None of these new files is reachable through the two repositories' bare default targets. Explicit per-module builds and worktree build receipts are outside this test; absence here is not failed proof.
3. **More paper-area commits did not mean more surviving manuscript text.** August: 421 paper-area commits, 46 still contributing a line to selected current drafts; September: 1,286, 63 contributing. Text additions were 37,710 / 32,080; retained draft lines 3,379 / 1,901. Paper tooling, notes, generated text and revision churn inflate these activity counts. This says nothing about acceptance, submission or the intellectual value of the result.

The isolated E6b apparatus illustrates the limit: its seven source files account for **2,212 added lines, 2,063 retained lines**, with internal imports but **zero detected external src importers**. A line-retention metric alone would reward this unwanted work. The forensic report identifies the mismatch with the requested deliverable; this census does not infer it from Git.

## Weekly products

P = produced; S = survived; U = use proxy, with definitions below. A dash is unavailable / not meaningful. † means a **known minimum**, not a completed survival estimate; CSV `survived`/`used` is blank when incomplete, with separate known and unknown-production fields. W31 includes only August 1–2; W39 only September 21 to snapshot. All timestamps are UTC committer times; ISO week starts Monday.

| ISO week (Monday) | Ideas docs P/S/U | Code lines P/S/U | Test files P/S/U | Lean files P/S/U | Paper commits P/S |
|---|---:|---:|---:|---:|---:|
| 2026-W31 (2026-07-27) | 21 / 21 / 17 | 6,425 / 4,706 / 3,004 | 28 / 26 / 26 | 22 / 22 / 22 | 2 / 0 |
| 2026-W32 (2026-08-03) | 52 / 51 / 32 | 5,739 / 5,055† / 3,870† | 37 / 35 / 35 | 309 / 309 / 5† | 7 / 2 |
| 2026-W33 (2026-08-10) | 47 / 46 / 28 | 9,698 / 6,353† / 5,910† | 33 / 17 / 17 | 5 / 5 / 4 | 11 / 2 |
| 2026-W34 (2026-08-17) | 36 / 36 / 29 | 31,267 / 23,228† / 19,500† | 106 / 102 / 102 | 42 / 40 / 33 | 40 / 12 |
| 2026-W35 (2026-08-24) | 123 / 122 / 105 | 23,740 / 19,263 / 15,793 | 66 / 66 / 66 | 577 / 215 / 9† | 96 / 15 |
| 2026-W36 (2026-08-31) | 30 / 29 / 26 | 15,365 / 13,659 / 9,030 | 56 / 56 / 56 | 144 / 133 / 85† | 850 / 33 |
| 2026-W37 (2026-09-07) | 97 / 97 / 81 | 29,931 / 25,824 / 20,654 | 157 / 156 / 156 | 770 / 740 / 79† | 194 / 28 |
| 2026-W38 (2026-09-14) | 39 / 39 / 26 | 33,780 / 30,575 / 23,823 | 181 / 175 / 175 | 126 / 126 / 61† | 495 / 12 |
| 2026-W39 (2026-09-21) | 5 / 5 / 4 | 6,767 / 6,434 / 5,034 | 60 / 60 / 60 | 2 / 2 / 2 | 12 / 5 |

## Other units and discard signals

These are separate subtypes, never added into an “idea”, “paper” or “discard” total. Missing without observed deletion includes work only on other branches, renames, removals outside the window, and other unknown histories; it is not automatically discarded work.

| Week | CLAUDE sections P/S/U | Paper lines P/S | Revert commits | New files later deleted | E6b isolated files | Missing without observed deletion |
|---|---:|---:|---:|---:|---:|---:|
| 2026-W31 | 0 / 0 / 0 | 288 / 0 | 1 | 24 | 0 | 0 |
| 2026-W32 | 0 / 0 / 0 | 19,935 / 2,097 | 2 | 199 | 0 | 19 |
| 2026-W33 | 1 / 1 / 0 | 100 / 9 | 3 | 185 | 0 | 377 |
| 2026-W34 | 0 / 0 / 0 | 2,359 / 644 | 5 | 7 | 0 | 16 |
| 2026-W35 | 0 / 0 / 0 | 6,245 / 533 | 3 | 164 | 0 | 711 |
| 2026-W36 | 0 / 0 / 0 | 12,356 / 331 | 1 | 14 | 0 | 8 |
| 2026-W37 | 3 / 3 / 0 | 3,922 / 1,149 | 2 | 41 | 7 | 23 |
| 2026-W38 | 20 / 18 / 1 | 24,176 / 445 | 4 | 49 | 0 | 0 |
| 2026-W39 | 0 / 0 / 0 | 409 / 72 | 2 | 1 | 0 | 0 |

## Operator-present / absent attribution

The forensic report's complete table contains 54 activity-bearing gaps of at least six hours. A commit strictly inside one of those intervals is `operator-absent-proxy`; outside is `operator-present-proxy`. After the report's 2026-09-21 17:23:36Z observation cutoff it is **unknown**, even if outside every listed gap. Neither category establishes physical presence, attention, authorization, which agent worked, or whether work began before the gap. Attribution is by producing commit time, not by duration or last editor. Boundary instants are assigned outside the gap. The CSV contains produced / survived / used for every week × subtype × presence group, plus aggregate rows.

| Week | Idea docs present / absent / unknown | Code lines present / absent / unknown | Tests present / absent / unknown | Lean present / absent / unknown | Paper commits present / absent / unknown |
|---|---:|---:|---:|---:|---:|
| 2026-W31 | 21 / 0 / 0 | 6,425 / 0 / 0 | 28 / 0 / 0 | 22 / 0 / 0 | 2 / 0 / 0 |
| 2026-W32 | 52 / 0 / 0 | 5,687 / 52 / 0 | 31 / 6 / 0 | 309 / 0 / 0 | 7 / 0 / 0 |
| 2026-W33 | 46 / 1 / 0 | 9,583 / 115 / 0 | 22 / 11 / 0 | 5 / 0 / 0 | 11 / 0 / 0 |
| 2026-W34 | 36 / 0 / 0 | 30,288 / 979 / 0 | 100 / 6 / 0 | 41 / 1 / 0 | 39 / 1 / 0 |
| 2026-W35 | 123 / 0 / 0 | 22,309 / 1,431 / 0 | 64 / 2 / 0 | 323 / 254 / 0 | 96 / 0 / 0 |
| 2026-W36 | 29 / 1 / 0 | 14,000 / 1,365 / 0 | 43 / 13 / 0 | 85 / 59 / 0 | 647 / 203 / 0 |
| 2026-W37 | 86 / 11 / 0 | 18,741 / 11,190 / 0 | 111 / 46 / 0 | 572 / 198 / 0 | 162 / 32 / 0 |
| 2026-W38 | 39 / 0 / 0 | 33,437 / 343 / 0 | 179 / 2 / 0 | 126 / 0 / 0 | 486 / 9 / 0 |
| 2026-W39 | 3 / 0 / 2 | 5,500 / 0 / 1,267 | 45 / 0 / 15 | 2 / 0 / 0 | 11 / 1 / 0 |

## Exact rules and undercounts

**Scope and commits.** Scan immediate children of `/home/joe/code` with a real `.git` directory, once each: 45 repositories, 21,407 distinct nonmerge SHAs within their own repository and the window. Ref tips and HEAD were frozen before scanning. No worktree replicas (`.git` files), `/tmp` worktrees, nested corpora, or other hosts. `futonY` is absent. All reachable frozen refs, not only current master; identical SHAs in different repositories remain distinct. No author filter: third-party work in canonical clones can count; this is a repository census, not proof Joe/agents authored it. This census includes all mathlib4 paths, unlike the earlier WarMachine-only commit plot. Nonmerge diffs, root additions, no rename detection; merge-only resolutions are omitted. A file first added in this window counts once per repo/path even if re-added on another branch; reintroductions and renamed/copied files can look new. Uncommitted products are excluded. Source addition counts include blanks, comments, generated code and repeated rewrites; they are not unique ideas or net growth.

**Ideas.** New `holes/**/[MCE]-*.md`, and `.md/.org/.txt/.rst` documents whose basename starts `NOTE-`, `TN-`, `ANSWERS-`, or `DECISIONS-`. Survived = same path at pinned HEAD. Used = exact basename or extensionless basename (at least eight characters, token boundaries) appears in a strictly later committer-timestamp commit body or newly added line of another document in another commit, across the frozen repo set. Only `.md/.org/.txt/.rst/.tex/.bib` doc additions are searched; a self-doc mention cannot satisfy it. Same-timestamp references, shortened IDs, synonyms, non-text links, chat citations, removed lines and citations after the snapshot are missed. Existing text is not recounted merely because its doc changes. Copied indexes or critical/rejecting mentions can be false-positive “use.” One witness per used item is in the ledger.

`CLAUDE.md` sections are separately counted as unique (repo, path, added Markdown heading text) in diff additions. Survived = exact heading still in that file; used = a later exact heading-text citation under the same temporal rule (not a generic CLAUDE.md mention). This is a heading-addition proxy: renames/reinsertions may count as new sections, and prose added beneath an existing heading is missed. There were 24 such headings, 22 retained, one later citation. This lexical census cannot count implicit ideas.

The memory directory `~/.claude/projects/-home-joe-code/memory` contains **58 current files** (recursive regular-file inventory). `git -C /home/joe/.claude/projects/-home-joe-code/memory rev-parse --show-toplevel` returned “not a git repository”; no versioned creation/change history was available in this census. Mtime is not creation time; **weekly production/survival/use is unknown**, not zero. Every week's `idea_memory_files` CSV row is blank. No memory text is published.

**Code.** Added lines in paths with a directory component `src` and extensions `.clj/.lean/.py/.el`. All 1,255 eligible current code/paper paths were scheduled for exact, unsampled `git blame --incremental` at pinned HEAD; 1,202 completed, 53 XTDB paths were marked unavailable. Blame retains original commit/path attribution; count only attributions to matching in-window source additions. No `-w`, copy detection, or text-similarity estimate. Renamed *current* paths not in the changed-path census can be missed. Survived = those attributed current lines. Used = survived lines in files with a detected import/require by a different current src file across these repos. Clojure namespace names/libspec vectors, Python absolute first import module, Emacs Lisp provide/require symbols and Lean import module names are scanned lexically. Relative imports, aliases, multi-import statements, namespace metadata variants, dynamic loads and non-src callers can be missed; lexical strings/vectors may give false positives. Internal cluster imports count per file; they do not establish external use or runtime execution. The ledger lists inbound callers.

**Read-only exception encountered.** XTDB is a partial clone. An ordinary blame invocation automatically launched promisor fetches before this was recognized; the census/fetch subprocesses were stopped. Git may have cached objects, so an entirely zero-write repository audit cannot be claimed. No tracked file, ref, service or source store was intentionally edited, and no fetched data are used to fill its survival gap. Subsequent XTDB blame was excluded. Its **2,014 added lines** have unknown survival/use, and no zero is substituted. Other code survival is exact for the stated paths. No sampling was used.

**Validated proxies.** New test files have test/tests directory components or test naming (`test_*.py`, `*_test.*`, `*-test.*`, plurals), restricted to `.clj/.cljs/.cljc/.py/.el`. S/U = same file retained at HEAD, the requested “test still exists” proxy; no pass result, namespace body, execution, coverage, or semantic correctness is asserted. Lean P = all new `.lean` paths, including fixtures and experiment copies; S = same path retained. U = retained module in a **statically enumerated named library target**: configured roots (default root is library name), explicitly declared `. +` submodule globs, and recursively parsed local imports. Lake semantics were read from installed `Lake/Config/LeanLibConfig.lean` (Lean 4.31.0-rc1, lines 30–49). Libraries are `ApmCanaries`, `ConstructionTargets`, `YoungL2`, `DarkTower` in apm-lean; `Mathlib`, `Cache`, `MathlibTest`, `Archive`, `Counterexamples`, `DarkTower`, `docs` in mathlib4. Root configs/closure counts are in the manifest. Other repos with no top-level Lake config have unknown U. Per-module commands, nested projects, generated/dynamic imports, executable targets, package dependencies and build receipts are not evaluated; no Lake command was run. Static membership is not proof that a build passes. `declared_library_prefix` and `default_build_reachable` are additional ledger fields, not replacements for U.

**Paper.** P commits = any nonmerge commit in p4ng, or touching a directory component exactly named paper/papers/manuscript/manuscripts/publication/publications. Discovered changed paper areas outside p4ng: `futon5/holes/tech-notes/paper/`. This includes infrastructure/image-only changes; the separate line kind counts additions to textual `.tex/.bib/.md/.org/.rst/.txt` files in those areas, excluding node_modules and .lake. S lines = blame attribution in current tracked files reachable from selected current draft roots: p4ng `futon-2026.tex`, `plop-2026.tex`, `science-2026.tex` (documented driver family), and futon5 `holes/tech-notes/paper/draft9.tex` (default in `html-build/build-site-oxide.sh`). Literal input/include/bibliography/addbibresource closure; explicit abstract-futon/abstract-shared macro choices; comments stripped for include discovery. Blame still counts comments/blank lines in those files as text. Other macro expansion, external/untracked generated inputs, old draft variants, non-TeX manuscripts elsewhere and semantic survival after rewriting are missed. S commits = at least one such line remains. U is unknown: neither publication nor readership was queried. The manifest lists all selected draft paths. No old draft was called current merely because it exists.

**Discard signals.** Revert commits = commit body starts `Revert` (case-insensitive). Revert count is assigned to revert week, not original work week; reverted reverts may also count. Deleted new files = in-window added paths absent at HEAD and with a later in-window deletion record; assigned to original production week. A later deletion on another branch is still a candidate; ancestry, renames and intentional supersession are not resolved. Missing paths with no such deletion are reported separately. E6b = seven report-named store/carrier/provenance/capture/projection/completeness source files; current static src import scan found zero inbound callers from outside that cluster. This is the report's known parked cluster, not a universal assertion that isolated modules are useless. Scripts, CLI entry points, dynamic loading and deployed copies remain gaps. No whole-stack clustering or automatic deletion recommendation is made. The discard subtype S/U fields are not meaningful and remain blank.

## Month totals (different observation lengths)

| Kind / unit | August P/S/U | September to snapshot P/S/U |
|---|---:|---:|
| Ideas · new named documents (files) | 284 / 281 / 216 | 166 / 165 / 132 |
| Ideas · CLAUDE heading additions (sections) | 1 / 1 / 0 | 23 / 21 / 1 |
| Code · added src lines (lines) | 78,832 / 60,266 / 49,287 | 83,880 / 74,831 / 57,331 |
| Validated proxy · new test files (files) | 288 / 264 / 264 | 436 / 429 / 429 |
| Validated proxy · new Lean files (files) | 1,007 / 640 / 119 | 990 / 952 / 181 |
| Paper · commits in paper areas (commits) | 421 / 46 / — | 1,286 / 63 / — |
| Paper · textual additions / retained draft lines (lines) | 37,710 / 3,379 / — | 32,080 / 1,901 / — |
| Discarded signal · revert commits (commits) | 14 / — / — | 9 / — / — |
| Discarded signal · new files later deleted (files) | 582 / — / — | 102 / — / — |
| Discarded signal · isolated E6b files (files) | 0 / — / — | 7 / — / — |
| Unmerged / missing · no observed deletion (files) | 1,125 / — / — | 29 / — / — |

## Repository coverage

| Repository | Nonmerge commits in window | Idea docs | Code added lines | New tests | New Lean | Paper commits |
|---|---:|---:|---:|---:|---:|---:|
| 18_Category_theory_homological_algebra | 0 | 0 | 0 | 0 | 0 | 0 |
| 18_Category_theory_homological_algebra.upstream | 0 | 0 | 0 | 0 | 0 | 0 |
| FloWrTester | 0 | 0 | 0 | 0 | 0 | 0 |
| apm-lean | 7779 | 4 | 0 | 0 | 1636 | 0 |
| chatgpt-tui | 2 | 0 | 0 | 0 | 0 | 0 |
| chipwits-forth | 0 | 0 | 0 | 0 | 0 | 0 |
| codex | 568 | 0 | 32 | 6 | 0 | 0 |
| easyeffects | 18 | 0 | 0 | 0 | 0 | 0 |
| expenses-hel | 2 | 0 | 0 | 0 | 0 | 0 |
| expenses-jac | 2 | 0 | 0 | 0 | 0 | 0 |
| filings | 4 | 0 | 0 | 0 | 0 | 0 |
| futon0 | 146 | 11 | 0 | 7 | 0 | 0 |
| futon1 | 0 | 0 | 0 | 0 | 0 | 0 |
| futon1a | 0 | 0 | 0 | 0 | 0 | 0 |
| futon1b | 112 | 14 | 0 | 0 | 0 | 0 |
| futon1bi | 0 | 0 | 0 | 0 | 0 | 0 |
| futon2 | 5771 | 125 | 49851 | 282 | 95 | 0 |
| futon2a | 1 | 0 | 8902 | 29 | 0 | 0 |
| futon3 | 386 | 2 | 2740 | 22 | 0 | 0 |
| futon3a | 10 | 0 | 225 | 0 | 0 | 0 |
| futon3b | 4 | 0 | 95 | 1 | 0 | 0 |
| futon3c | 3091 | 182 | 86027 | 279 | 2 | 0 |
| futon4 | 78 | 2 | 0 | 0 | 0 | 0 |
| futon5 | 124 | 48 | 2754 | 11 | 0 | 31 |
| futon5a | 9 | 1 | 0 | 0 | 0 | 0 |
| futon6 | 175 | 8 | 15 | 10 | 0 | 0 |
| futon7 | 7 | 3 | 0 | 0 | 0 | 0 |
| futon7a | 4 | 0 | 0 | 0 | 0 | 0 |
| gflownet | 198 | 0 | 0 | 16 | 0 | 0 |
| kissat | 0 | 0 | 0 | 0 | 0 | 0 |
| marimo-zone | 16 | 0 | 0 | 5 | 0 | 0 |
| mathlib4 | 646 | 0 | 0 | 0 | 264 | 0 |
| mathse-xtdb-benchmark | 75 | 1 | 7120 | 28 | 0 | 0 |
| mfuton-share | 0 | 0 | 0 | 0 | 0 | 0 |
| mmca | 0 | 0 | 0 | 0 | 0 | 0 |
| mmca-clj | 49 | 0 | 2937 | 8 | 0 | 0 |
| nlab-content | 215 | 0 | 0 | 0 | 0 | 0 |
| nnexus | 0 | 0 | 0 | 0 | 0 | 0 |
| orbook.github.io | 0 | 0 | 0 | 0 | 0 | 0 |
| p4ng | 1676 | 49 | 0 | 3 | 0 | 1676 |
| powerbi-tui | 1 | 0 | 0 | 0 | 0 | 0 |
| storage | 0 | 0 | 0 | 0 | 0 | 0 |
| ukrn-services-simulation | 0 | 0 | 0 | 0 | 0 | 0 |
| voxterm | 72 | 0 | 0 | 15 | 0 | 0 |
| xtdb | 166 | 0 | 2014 | 2 | 0 | 0 |

## Queries and verification

Read-only query families actually run (Python subprocess argv, no shell interpolation of paths):

```text
# Per top-level repository with an actual .git directory:
git -C REPO rev-parse HEAD
git -C REPO for-each-ref --format=%(objectname)
git -c core.quotepath=false -C REPO log <frozen ref tips and HEAD> --no-merges --since-as-filter=2026-08-01T00:00:00Z --until=2026-09-21T23:59:59Z --format=%x1e%H%x1f%ct%x1f%ae%x1f%B%x1d --raw --numstat --no-abbrev --no-renames --root
git -C REPO ls-tree -r --name-only PINNED_HEAD
git -C REPO cat-file --batch   # stdin PINNED_HEAD:path, current source/config/draft files
# Added doc text: log -p with the same frozen window; after expensive nlab history walk,
# remaining repos use exactly the already enumerated window commit SHAs on stdin:
git -c core.quotepath=false -C REPO diff-tree --stdin --root --format=@@COMMIT\ %H -p --no-renames -- '*.md' '*.org' '*.txt' '*.rst' '*.tex' '*.bib'
git -C REPO blame --incremental PINNED_HEAD -- PATH
# Memory inventory (no mtime attribution):
sum(p.is_file() for p in Path('/home/joe/.claude/projects/-home-joe-code/memory').rglob('*'))
```

The frozen-ref log parser deduplicates within repo; each file has raw status and numstat additions. Product kinds use predicates above. Added doc lines are matched after removing the diff `+`; later means strictly greater committer timestamp. Weekly aggregation is `datetime.fromtimestamp(t, timezone.utc).strftime('%G-W%V')`. The 54 gap rows are parsed from the forensic report's first table, whose SHA256 is pinned in the manifest. Aggregate/presence reconciliation and survival bounds were checked; zero rows were found with known survival greater than production.

Independent checks:

- futon2 on **2026-09-13**, raw `git log <frozen refs> --no-merges --since-as-filter=2026-09-13T00:00:00Z --until=2026-09-13T23:59:59Z --format= --numstat --no-renames --root`, filtered to the src extensions above: **6,443 added lines**; ledger filtered to repo/day/code: **6,443**.
- `git -C futon2 blame --line-porcelain c6ae03fc8d61a74a22fa37e82ffbc7a48a83032f -- src/futon2/aif/ruled_outcome_c.clj`: **68** current lines attributed to producing commit `000580f9676fba074d7a3f00ab445e219664ff50`; ledger survival cell: **68** (84 originally added).
- Exact current E6b namespace import scan: seven files, no external src importer. Cross-check with the forensic report §5; no runtime claim.

## Relation to the Minard figure and the September 13 question

The supplied Minard HTML actually has a **daily** UTC axis (August 22–September 21), not a weekly one. This chart bins that same calendar into ISO Mondays and extends left to the requested August 1 boundary; it marks August 30 and September 13 without shifting the dates. Each operational subtype has its own panel and y-scale, so sections, lines, files and commits are not added or made visually commensurate. Bars split production by the gap attribution; lines show retention/use proxies.

The week beginning September 14 has 181 new tests (175 retained), 126 new Lean files (126 retained; 61 in the named-library build closure), and 33,780 added src lines (30,575 retained). That is evidence for more retained validation artifacts, **not evidence that all post-September-13 code was validated**. A paper submission could be more valuable than any of these counts; that value is absent from this dataset. A next step would need actual validation/submission receipts and Joe's judgment, rather than summing these quantities.
