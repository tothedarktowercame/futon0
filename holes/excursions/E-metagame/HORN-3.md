# HORN-3 — needed sets from recorded derivations

Date: 2026-11-12.  Engine: `scripts/metagame_horn.py`.  Measurement records:
`~/code/storage/metagame/horn3/`.

## Design

Closure consequences are unchanged.  A position now additionally records:

- the first `Derivation(rule, premises)` for each relation, `neq`, and
  rule-produced equality;
- a contradiction derivation;
- congruence equalities whose premises are the corresponding argument
  equalities; and
- the move which constructed each operation term.

Derivation keys are stable ground atoms.  A secondary normalized index is
rebuilt after a union, so a later representative change does not erase the
first reason.  `provenance_moves` walks a goal to its recorded premises and
to the moves constructing every term and argument in that support.

`needed` replays the full run once (cached across the goals of one setter
run), takes the support moves in original order, verifies that they entail
the goal, and runs the old one-at-a-time deletion minimizer only on that
candidate.  Missing or incomplete provenance safely falls back to all moves.
The former implementation remains public as `needed_by_replay`.

## Semantic gates

The unmodified HORN-1 suite passed:

```text
7 passed in 266.21 s
equational positions compared: 12000
line facts checked against Q: 94509
```

Focused provenance tests cover the first of two independent derivations,
congruence reasons, fresh entailment and removal minimality.  The setter's
seven existing orbit/replay tests also pass.  On the completed 300-run group
campaign, **633/633 new facts** replayed from their provenance candidate and
were removal-minimal; the needed-size delta against `needed_by_replay` was
`0` for all 633.

The requested 300-run line campaign did not complete.  Four 75-run workers
stopped on assertions in the campaign harness because it incorrectly grouped
canonical-renaming or symmetry-orbit members and required them to share an
identical concrete move proof.  This was not an engine entailment failure.
The harness was corrected to check each fact independently, but, following
the requisition's “report; do not relaunch” rule, those workers were not
restarted.  The focused line test independently checks 20 new facts; every
provenance result entailed and was removal-minimal.  Therefore the requested
line distribution is **unmeasured**, not inferred.

## Ten-run classification timing

Each completed row used identical seeds and ten `classify_run` calls inside
`futon-testing.slice` (`MemoryMax=8G`, `TasksMax=256`).

| givens | K | old replay median | provenance median | speedup |
|---:|---:|---:|---:|---:|
| 3 | 4 | 5.280 s | 2.884 s | 1.83x |
| 3 | 6 | 36.765 s | 20.357 s | 1.81x |
| 3 | 8 | censored | 35.992 s | — |
| 4 | 4 | 10.911 s | 5.011 s | 2.18x |
| 4 | 6 | not run | 18.338 s | — |
| 4 | 8 | not run | 74.341 s | — |

The old 3/K=8 worker was stopped by us after more than 23 minutes without
finishing the requested ten runs; its scope then became inactive and it was
not relaunched.  Because that made the remaining replay-only high-K timings
impractical, 4/K=6 and 4/K=8 old rows were not started.  All six provenance
rows completed ten runs.

## HORN-2b cell rerun

The six new cells used the same 120-second alarm and seeds as HORN-2b.  A
record is kept when it is neither direct, reflexive, nor `neq`.

| givens | K | HORN-2b runs | HORN-3 runs | HORN-2b kept 2/3/4+ | HORN-3 kept 2/3/4+ |
|---:|---:|---:|---:|---:|---:|
| 3 | 4 | 13 | 10 | 25/19/0 | 21/10/0 |
| 3 | 6 | 3 | 6 | 5/2/2 | 6/6/0 |
| 3 | 8 | 1 | 1 | 2/0/0 | 4/7/1 |
| 4 | 4 | 11 | 10 | 13/0/0 | 20/9/0 |
| 4 | 6 | 2 | 5 | 2/0/0 | 8/2/2 |
| 4 | 8 | 0 | 2 | 0/0/0 | 5/0/1 |

Across cells HORN-3 found **98 distinct kept theorems**: 60 needed-2, 34
needed-3, and 4 needed-4+.  Most importantly, 4/K=8 moved from zero complete
runs to two.  The lower K=4 run counts are roughly flat/slightly lower because
recording first derivations has a cost and the four-worker scopes contend;
the benefit grows with the number of candidate moves deletion replay would
otherwise reconsider.

## Limits and next step

The result is deliberately still removal-minimal rather than claiming a
globally shortest proof: one recorded derivation chooses one support when
several coexist, then deletion minimizes within it.  The independent case
pins that the first rule is the one recorded.  A later engine could retain a
small antichain of supports, but that is not required for the throughput gain
shown here.

## Review (claude-9, 2026-11-12)

- Nested-term case built by hand: x = ext(a,b,a,c), y = ext(b,x,a,b), plus an
  unrelated ext(c,a,b,c) played between them. For E(x,y,a,b) and B(b,x,y) the
  provenance `needed` returns both x and y (x only occurs inside y's term) and
  drops the unrelated move; the result entails on a fresh replay and matches
  `needed_by_replay` in size.
- The line campaign the packet asked for, run by claude-9 with the corrected
  harness (each fact checked on its own), 12 seeded runs (first = 0..10, 2 per
  shard, own scope `claude9-horn3-line2`, 6 processes): **1,382 line facts,
  every provenance result entailed on fresh replay and was removal-minimal,
  size difference from `needed_by_replay` 0 for all 1,382.** A first attempt
  with 10 runs per shard produced nothing within 480 s per shard (the harness
  writes only at the end; old `needed_by_replay` on line facts is the slow
  part), so the shards were cut to 2 runs. Results:
  `~/code/storage/metagame/horn3-claude9/line-*.json`.
