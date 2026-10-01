# PBASE stage comparison: Minard vs 象 (2026-08-22 .. 2026-09-21T17:19:12Z)

All numbers below are produced by `pbase_compare.py` from the files named in each line.

## Denominators

- Minard join rows total: 20306; matched: 2882; matched with turn-at in window: 2882
- distinct Minard-labelled turns (session + turn-at) in window: 2879
- batch turn files scanned: 3420; in window with a 象 reading: 3235 (window turns 3235, window turns without a reading 0; 0 in-window readings shared a (session, created_at) key with another and were merged)
- joined rows (batch turn with >=1 filtered-row match on session+at): 3235
- of those, with a single-stage Minard label: 2684
- batch turns matching zero filtered rows: 0; matching several: 0
- fragments total: 9804; mapped to a core intent: 7532 (76.8%); unmapped: 2272 (23.2%)
- reconciliation: joins window turns 2882 -> distinct turns 2879 (repeat retrievals on one turn collapse); joined 3235 = window batch turns 3235 - zero-filter-matches 0; cross-table rows 2684 = 3235 - no-Minard-label 551 - multi-stage-Minard 2

## Cross-table: Minard stage (rows) x 象 modal class (columns)

| Minard \ 象 | PERCEIVE | BELIEVE | EVALUATE | SELECT | ACT | ANNOTATOR | unmapped | total |
|---|---|---|---|---|---|---|---|---|
| PERCEIVE | 100 | 37 | 23 | 27 | 26 | 0 | 66 | 279 |
| BELIEVE | 171 | 58 | 33 | 64 | 54 | 0 | 118 | 498 |
| EVALUATE | 56 | 23 | 13 | 17 | 13 | 0 | 29 | 151 |
| SELECT | 112 | 53 | 24 | 35 | 31 | 0 | 52 | 307 |
| ACT | 135 | 51 | 15 | 32 | 55 | 0 | 72 | 360 |
| ASSURANCE | 219 | 85 | 29 | 74 | 98 | 0 | 124 | 629 |
| COORDINATION | 109 | 45 | 24 | 52 | 49 | 0 | 79 | 358 |
| NONE | 33 | 2 | 3 | 7 | 10 | 0 | 13 | 68 |
| total | 935 | 354 | 164 | 308 | 336 | 0 | 553 | 2684 |

## Agreement (PBASE-only turns: Minard stage and 象 modal both among the five)

- overall: 20.7% agreement on n=1258 turns; Cohen's kappa 0.010
- Minard kind = practice only: 21.0% agreement on n=481 turns; Cohen's kappa 0.026
- one-sided check (denominator = Minard-PBASE turns, 象 non-PBASE counted as miss): 16.2%; with roles swapped: 12.4% (the two differ, so the roles cannot be silently exchanged)

## Multi-retrieval / multi-stage Minard turns (listed, not picked)

- turns with >1 matched retrieval in window: 2
  - session 01a032d8-d669-7c63-9be9-b37f94ece3ae at 2026-08-24T09:16:56.873012236Z: 3 retrievals
  - session 01a032d8-d669-7c63-9be9-b37f94ece3ae at 2026-08-24T09:19:36.658713500Z: 2 retrievals
- turns whose rank-1 patterns span more than one stage: 2
  - session 01a032d8-d669-7c63-9be9-b37f94ece3ae at 2026-08-24T09:16:56.873012236Z: stages ['BELIEVE', 'COORDINATION', 'SELECT'] patterns ['enrichment/ARGUMENT', 'musn/plan-before-tool', 'orchestration/state-in-substrate-deltas-in-messages']

  - session 01a032d8-d669-7c63-9be9-b37f94ece3ae at 2026-08-24T09:19:36.658713500Z: stages ['COORDINATION', 'SELECT'] patterns ['eight-gates/lean-commit', 'orchestration/recorded-handoff']

## Top disagreement cells (Minard stage -> 象 modal class), 2 example turn ids each

| count | Minard | 象 | examples |
|---|---|---|---|
| 219 | ASSURANCE | PERCEIVE | emacs-1adba203e2eeee03329bdbe92eacf1bd, emacs-b9ceded7c297b3770f37d045fc8a8566 |
| 171 | BELIEVE | PERCEIVE | emacs-605a76d00914f64da00954e83cb697a9, emacs-844dd84c1b8a7d86f154ac9a40955c10 |
| 135 | ACT | PERCEIVE | emacs-e9642b95b5ddfbc70b9a1a4ca19d27c5, emacs-a2db9c746e0890920a3d14d4748e69f6 |
| 124 | ASSURANCE | unmapped | emacs-5215e81c22405fe31d327f0e1fcea6ce, emacs-dabb46ff6c403474648696b28b7b5691 |
| 118 | BELIEVE | unmapped | emacs-9dfa7c5452613b643ae02a62256880da, emacs-46ce6e3612b9c14f09b060d5da5e2ab3 |
| 112 | SELECT | PERCEIVE | emacs-2c3d8536d682bf2a2a6d7c55ab07a5ce, emacs-b1d7a8694a3721a81405e13029f8cd22 |
| 109 | COORDINATION | PERCEIVE | emacs-40117c2ebc208b34332b5df5107e12a1, emacs-a653776709d96e7399b25a936985372b |
| 98 | ASSURANCE | ACT | emacs-f38c993fad9330b9835b3e0fcd6be959, emacs-30289677db9f4ea93e5a19c437d22d49 |
| 85 | ASSURANCE | BELIEVE | emacs-c5a1215d6cfa1a6140abe2663f9f45cb, emacs-cfa4bf87a76b892b668d75766672970a |
| 79 | COORDINATION | unmapped | emacs-1bb5a51300683908033cb191d147c164, emacs-700590537ec390a949abe4b2f5783e2d |
| 74 | ASSURANCE | SELECT | emacs-cd29606c554f1cb531b1f181f5995d2e, emacs-76e7d69386cc5f397bf38679a0697297 |
| 72 | ACT | unmapped | emacs-2bebdbdc33351830b7814e2f430168df, emacs-ce5f18da8d507473a8dbf7805be3ca17 |
| 66 | PERCEIVE | unmapped | emacs-b17b66b059d917e13f5a7a647b95f018, emacs-b100cd120d1bd807dd377f9cabb6c0d1 |
| 64 | BELIEVE | SELECT | emacs-a3e8facb19a802408c61ed1b8a4a9331, emacs-06024d6cb22da61e0c6cb07018136294 |
| 56 | EVALUATE | PERCEIVE | emacs-d135794fc6e64c44b3d2e6a828f2d4b2, emacs-f0dd2571497f10ea7efc5de3c4c4dde1 |
| 54 | BELIEVE | ACT | emacs-0d9ee819bff1c0030ee62db90cc0e039, emacs-411c2d3d508d603f3d7e20802db41f26 |
| 53 | SELECT | BELIEVE | emacs-fd6956dc1c6fd6ded5ff5e1453c7a41f, emacs-4ccb233cb8fb2963deb3d23479a17207 |
| 52 | COORDINATION | SELECT | emacs-f898488f965c6ac3c13c22699669e6da, emacs-000eb1bb1cc9d45d59567f93557e42ab |
| 52 | SELECT | unmapped | emacs-8482a0cc4f3988f9679748426f563b46, emacs-c0944b9636b16cb9c4bc34c01609274c |
| 51 | ACT | BELIEVE | emacs-69a465f27a388319c31b0c8ef3953ccc, emacs-65fa9bbf23c14a996780346454b1f7fc |
