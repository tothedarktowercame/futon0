# Win-in-one profile: Euclid I.2 stage-1 prefix

Date: 2026-11-11.  Commit profiled: `98d2b69` plus the uncommitted Probe 11
wrapper already present in `scripts/euclid_open_spiel_alpha_zero.py`; no engine,
play, or Spiel source was changed for this discovery.

## Reproduction

The position is the seven-move prefix in
`rc11/stage-1/prefix.json`, loaded as proposition I.2 with dense actions and
perturbation disabled.  It has 545 legal actions and one winning action
(`58156`).  The process used the OpenSpiel virtualenv and
`PYTHONHASHSEED=0`.  One complete pass warmed the engine's cross-state linear
arithmetic memo; then a fresh state and fresh `WinInOneEvaluator` were
profiled once:

```text
PYTHONHASHSEED=0 PYTHONPATH=scripts \
  ~/.venvs/openspiel/bin/python <cProfile driver>
```

`cProfile` itself expands the warm check from roughly 2.5--3.2 seconds on
this host to 6.87 seconds (10,985,837 calls), so its cumulative times are for
attribution, not an alternative wall benchmark.  The top 25 by cumulative
time were:

```text
 ncalls  cumtime  function
   3282    5.061  euclid_engine.close
    545    4.042  EuclidState._apply_action
    545    3.957  euclid_engine.apply_step
    545    3.273  euclid_engine._apply_step_unchecked
   2737    3.079  euclid_engine.contradictory
   1096    2.669  EuclidState.goal_reached
   1095    2.668  EuclidState.is_terminal
   2192    2.406  euclid_engine.contradictory_outer_state
   4388    1.943  euclid_engine._fm_sat
   4939    1.459  euclid_engine.r_classify
   4388    1.381  euclid_engine._elim_equalities
   1096    1.318  euclid_play._goal_witness
    548    1.130  EuclidState.current_player
  45659    1.105  LinearArithmetic.entails
  14910    0.703  euclid_engine.la_entails_eq
 301231    0.567  euclid_engine.norm
  92628    0.450  euclid_engine._subst_one
  14709    0.348  euclid_engine.la_entails_lt
1414838    0.340  dict.get
  21366    0.316  LinearArithmetic._canonical_rows
  50112    0.265  euclid_engine._all_points
   6394    0.237  euclid_engine.r_pos_neq
  31430    0.223  LinearArithmetic._equality_representatives
   6394    0.215  euclid_engine.r_dpl
  12396    0.214  euclid_engine.la_entails_gt
```

## Where the time goes

A separate unprofiled, warm, stage-timed run (fresh state per repetition)
gave 3.19 seconds at its fastest stable repetition: legal enumeration 0.050
s, observation/key construction 0.0005 s, 545 outer state clones 0.0085 s,
public move application 2.400 s, explicit post-application `is_terminal`
0.727 s, and the winning child's `returns` 0.0022 s.  Normalising those
fractions to Probe 11's 2.49-second median gives approximately:

| work | share of a 2.49 s check | important detail |
|---|---:|---|
| legal enumeration + cache key | 0.04 s (2%) | often already materialised by MCTS |
| the evaluator's 545 outer clones | 0.007 s (<1%) | `State.copy` is cheap; cloning is not the problem |
| public move application | 1.87 s (75%) | dominated by `apply_step`, closure, contradiction, and linear arithmetic |
| explicit child terminal/goal check | 0.57 s (23%) | witness search plus another contradiction check |
| winning return | <0.01 s | only one child reaches it |

The categories are API-level and additive.  Internally, “move application”
also contains a goal check: OpenSpiel's public `apply_action` asks
`current_player`, which calls `is_terminal`, before `_apply_action`.  Hence
the profile records 1,096 `goal_reached` calls for 545 children: about one
pre-move check and one post-move check per action, plus the winning return.
Those goal calls themselves split between
`contradictory_outer_state`/linear arithmetic and
`euclid_play._goal_witness`.  Conversely, `close`, `contradictory`, and
Fourier--Motzkin times overlap in the cumulative profile; they must not be
added together.

The dominant engine work is therefore not copying.  Each candidate is
applied transactionally (`apply_step` makes another internal copy), its
new facts are closed, and the complete child is checked for contradiction.
Closure invokes rule classification and linear-arithmetic entailment over
the mostly identical parent store.  The goal is then checked twice through
the public OpenSpiel path.  Replacing that path experimentally with one
`_apply_action` and one `goal_reached` retained the same single winner and
reduced a paired warm run from 4.63 to 3.72 seconds (about 20%; absolute
times varied with memo warmth).

## Work already done by legal-move enumeration

`euclid_spiel.enumerate_actions` does more than list rule/argument tuples.
For every grounded candidate it:

1. joins stated relational premises against the parent's indexed facts
   (`_premise_environments`);
2. grounds typed free arguments and computes collision-free fresh bindings
   (`_candidate_actions`, `_fresh_bindings`);
3. runs every declared arithmetic entailment premise through
   `_entailments_hold`, with a per-enumeration query cache; and
4. retains the complete `(rule, args, bindings)` action in
   `EuclidState._actions`/`_action_by_id`.

The win loop then calls `apply_step`, which checks the stated premises and
the rule's arithmetic entailments again before making facts, closing the
child, and checking consistency.  The linear-arithmetic layer's global memo
makes many repeated queries cheap after warm-up, which is why an experiment
that merely suppressed the already-certified rule entailment premises saved
only about 0.02 seconds in a warm 1.77-second direct loop.  Enumeration does
*not* compute the successor's closure, contradiction result, fact delta, or
goal result; those are the expensive repetitions left to the win check.

## Domain-general reductions

1. **Use a certified successor operation and check the goal once.**  Let
   enumeration return its grounded action plus a certificate for the
   parent's stated/arithmetic premises.  A successor operation consumes
   that certificate, performs the transactional mutation/consistency check,
   and returns the child without the public API's pre-move terminal query.
   This drops the duplicate goal check and redundant premise dispatch but
   keeps every rule and consistency guard.  The direct-path experiment
   measured about **20%**, or roughly **0.5 s of the 2.49 s median**; premise
   reuse alone is only a few hundredths once arithmetic caches are warm.

2. **Make closure/consistency incremental across sibling successors.**  All
   545 children share the same already-closed parent.  Cache the parent's
   classification and canonical linear-arithmetic state, then seed closure
   only with the action's added facts/rows; contradiction checking likewise
   needs to process the delta against the certified-consistent parent.  The
   profile attributes about three quarters of the API wall interval to
   application, with `close` its largest cumulative frame.  Conservatively
   removing half of that repeated work would save **0.7--1.0 s (30--40%)**;
   a fully effective delta closure could approach **1.3 s (50%)**.  This is
   engine-wide memoisation, not a geometry heuristic.

3. **Gate witness search with the closed successor delta, and cache its
   result.**  A monotone goal can become newly true only if the successor's
   *closed* delta adds a relational fact that can unify with a goal clause,
   or adds a linear-arithmetic row/atom on which a grounded metric goal
   depends.  If neither happens, the parent's false goal result remains
   false, so witness search is soundly skipped.  “Direct facts made by the
   rule” are not sufficient for this test because closure can derive the
   touching fact; the full closure delta is required.  Cache the resulting
   boolean/witness by logical state digest so `is_terminal`, `returns`, and
   transpositions share it.  Eliminating most of the 1,096 goal calls has an
   observed ceiling of about **0.5--0.8 s (20--30%)** here.  The existing
   evaluator cache only shares the final set of winning actions between
   `evaluate` and `prior`; it does not prevent the per-child duplicate goal
   calls inside the first scan.

The three changes compose: enumeration certifies the parent-side work,
incremental closure produces an exact child delta, and that delta decides
whether a goal witness search is necessary.  None drops a construction rule
or relies on Euclid-I.2-specific geometry.
