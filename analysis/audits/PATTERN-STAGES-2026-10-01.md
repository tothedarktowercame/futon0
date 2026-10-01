# Pattern stages — 2026-10-01 (full window)

**Full window 2026-08-22 .. 2026-10-01T21:12:00Z: 786 labelled pattern ids cover all 4,062 accepted rank-one hits.** This extends the 2026-09-21 analysis (689 ids / 2,882 hits) with the 2026-09-21→10-01 fetch (joins in `pattern-stage-joins-2026-10-01.jsonl`) and 97 provisional additions (`pattern-stage-additions-2026-10-01.jsonl`, read by Kimi packs; a 5-row sample was 2 agree / 3 arguable). Every accepted rank-one id in both join ledgers is labelled (asserted in test-time check; rank-one ids appearing only in non-accepted retrievals remain unlabelled, as before). Method change vs 09-21, both applied here: (1) the operator census additionally excludes turns `futon3c/scripts/xiang2000_p6o3.py classify()` flags; (2) the figure's lower panel draws only where the forensic token report has data (it ends 2026-09-21) and marks the rest "no token data".

| Stage | Practice | Subject (topical) | Mixed | Pattern IDs | Hits | Hit share | Provisional hits |
|---|---:|---:|---:|---:|---:|---:|---:|
| perceive | 138 | 270 | 16 | 78 | 424 | 10.44% | 19 |
| believe | 294 | 374 | 78 | 161 | 746 | 18.37% | 18 |
| evaluate | 88 | 114 | 20 | 54 | 222 | 5.47% | 17 |
| select | 223 | 204 | 67 | 100 | 494 | 12.16% | 17 |
| act | 231 | 206 | 112 | 108 | 549 | 13.52% | 20 |
| assurance | 707 | 264 | 31 | 161 | 1,002 | 24.67% | 44 |
| coordination | 315 | 171 | 31 | 87 | 517 | 12.73% | 14 |
| none | 0 | 106 | 2 | 37 | 108 | 2.66% | 4 |
| **Total** | **1,996** | **1,709** | **357** | **786** | **4,062** | **100.00%** | **153** |

(Joe reclassified `test-registry/bind-the-subject` believe→assurance on 2026-10-01, after the first build: 13 hits moved, believe 759→746, assurance 989→1,002; kind stays practice, so the totals row is unchanged.)

The "Provisional hits" column separates hits whose label comes from the 97 Kimi-pack additions (**153 hits, 3.77%**) from the 3,909 hits labelled by the original 689 (whose stage×kind distribution is the difference of the columns). Provisional labels are lower-confidence: treat their stage attribution as arguable pending Joe's blind check. Counts are count integrals of retrieval events by inherited stage — not token cost, hours, or proof that Joe performed the pattern. The hits attach to **4,059 distinct operator turns, 82.87% of 4,898 eligible recorded turns**.

## Operator census, each subtraction stated

| Census step | 09-21 window | 10-01 window | Full window |
|---|---:|---:|---:|
| joe-authored records fetched | 5,786 | 3,071 | 8,857 |
| not user turns (dropped) | 181 | 42 | 223 |
| user turns | 5,605 | 3,029 | 8,634 |
| − park-resume markers | 2,085 | 1,421 | 3,506 |
| − continuation payloads | 120 | 0 | 120 |
| − wake payloads | 2 | 16 | 18 |
| − xiang p6o-3 classify() flags (new) | 0 | 92 | 92 |
| **eligible turns** | **3,398** | **1,500** | **4,898** |

The xiang subtraction is exact for the 10-01 window (census turn texts on disk in the raw snapshot): 42 kimi-notice, 38 inbox-zero, 12 late park-wake variants; it drops **75 matched hits** (35 inbox-zero, 29 kimi-notice, 11 park-wake) from the figure and tables. For the 09-21 window the census-level subtraction is **0**: the raw snapshot's turn texts were never retained and futon1b was out of scope for this job, so classify() was run transcript-level over the park-wake-pilot archive instead — it flags 9 user turns the Minard exclusion keeps (5 late park-wake variants, 4 inbox-zero), none traceable to census store IDs via the transcript-coverage ledger, and none carrying a matched hit.

## Join dispositions (both ledgers combined, 28,135 retrievals)

| Disposition | Count |
|---|---:|
| joined (matched, rank-1) | 4,137 |
| − matched rows on xiang-flagged turns (removed) | 75 |
| joined to automatically-excluded turns | 2,446 |
| automatic-excluded (all prefix candidates excluded) | 1,055 |
| ambiguous text-prefix | 409 |
| unmatched | 20,163 |

Join methods on the surviving 4,062: 3,460 session-text-prefix, 602 operator-envelope-unique-payload-prefix. Same-session unique text-prefix join with a six-hour bound, unchanged; these are heuristic associations, not foreign keys.

## Files

- [Full-window labels](pattern-stages-2026-10-01.edn): 689 original + 97 additions carrying `:label-source :additions-2026-10-01`.
- [Full-window joins](pattern-stage-joins-2026-10-01-fullwindow.jsonl) and [manifest](pattern-stage-manifest-2026-10-01-fullwindow.json) (census exclusions itemised per window).
- Component snapshots: [09-21 manifest](pattern-stage-manifest-2026-09-21.json), [10-01 manifest](pattern-stage-manifest-2026-10-01.json); raw 10-01 snapshot uncommitted at `storage/futon0/pattern-stage-2026-10-01/`.
- Figure: [minard-operator-work-2026-10-01.html](minard-operator-work-2026-10-01.html), published at `https://zone.hyperreal.enterprises/wip/audits/minard-operator-work-2026-10-01.html`. `minard_operator_work.py` now takes `--start/--end/--joins/--manifest/--labels/--report/--template` with the 09-21 values as defaults; a default run reproduces the committed 09-21 page byte for byte (verified with `cmp`). The lower panel's token data ends at the forensic report's 2026-09-21 cutoff; the hatched region after that is marked "no token data", not zero.
- All `*-2026-09-21*` files are byte-identical to their committed versions.

## Families

One panel per stage in [minard-families-2026-10-01.html](minard-families-2026-10-01.html) (published at https://zone.hyperreal.enterprises/wip/audits/minard-families-2026-10-01.html): each stage band opened into its top families (up to 8 + grey other), same y-scale in every panel, marker at 2026-09-21T17:19Z where the +97 provisional labels begin. Families above 5% of their stage's hits in either window (script-generated; "before → after" hits with within-window shares):

**perceive** (424 hits): features 42 (14%) → 22 (18%); problems 42 (14%) → 11 (9%); futon-theory 25 (8%) → 12 (10%); war-room 32 (11%) → 3 (2%); stack-coherence 30 (10%) → 3 (2%); cascade-construction 5 (2%) → 8 (7%); (other families) 96 (32%) → 56 (46%)

**believe** (746 hits): problems 61 (11%) → 24 (11%); war-room 47 (9%) → 12 (6%); enrichment 28 (5%) → 5 (2%); or2 12 (2%) → 14 (7%); cascade-construction 12 (2%) → 12 (6%); (other families) 305 (57%) → 122 (58%)

**evaluate** (222 hits): memory 39 (24%) → 10 (17%); problems 26 (16%) → 16 (28%); aif 19 (12%) → 4 (7%); transition 10 (6%) → 2 (3%); snatch 9 (5%) → 2 (3%); math-strategy 4 (2%) → 6 (10%); (other families) 47 (29%) → 15 (26%)

**select** (494 hits): war-room 53 (16%) → 13 (8%); problems 39 (12%) → 25 (16%); musn 40 (12%) → 19 (12%); futon-theory 23 (7%) → 14 (9%); iching 21 (6%) → 10 (6%); eight-gates 18 (5%) → 2 (1%); p4ng 10 (3%) → 10 (6%); (other families) 116 (35%) → 62 (39%)

**act** (549 hits): iching 68 (17%) → 30 (19%); peripherals 42 (11%) → 17 (11%); writing-coherence 39 (10%) → 4 (3%); cycle-machine 30 (8%) → 10 (6%); inbox-zero 23 (6%) → 17 (11%); math-formalization 24 (6%) → 11 (7%); data-mining 24 (6%) → 8 (5%); vsatlatarium 16 (4%) → 12 (8%); (other families) 125 (32%) → 49 (31%)

**assurance** (1002 hits): devmap-coherence 104 (15%) → 31 (10%); memory 89 (13%) → 19 (6%); problems 55 (8%) → 22 (7%); test-registry 21 (3%) → 43 (14%); stack-coherence 40 (6%) → 10 (3%); storage 36 (5%) → 13 (4%); (other families) 286 (42%) → 154 (49%)

**coordination** (517 hits): orchestration 81 (21%) → 16 (13%); futon-theory 51 (13%) → 22 (17%); social 28 (7%) → 21 (17%); pacspine 25 (6%) → 5 (4%); cycle-machine 20 (5%) → 2 (2%); (other families) 142 (36%) → 46 (37%)

**none** (108 hits): problems 39 (51%) → 25 (81%); iiching 22 (29%) → 4 (13%); math-strategy 8 (10%) → 0 (0%); futon-theory 4 (5%) → 0 (0%); iching 2 (3%) → 2 (6%)
