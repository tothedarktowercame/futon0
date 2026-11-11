# Soundness 1 — contradictory legal Euclid states

Date: 2026-11-11.  This is a discovery report; it makes no engine change.

## Result

The reported five-move I.2 trace is unsound, but the immediate cause is not a
freshness inequality in `intersection_circle_line_between_points`.  That rule
adds exactly the conclusion of its LeanEuclid axiom.  The first unsupported
diagram fact is instead

```
between g_4 c e_9
```

from the engine-only `between_order [SMT-bridge]` closure rule.  Its arithmetic
premise is accepted because Fourier--Motzkin incorrectly reports
`|g_4-c| < |g_4-e_9|`.  The same store entails
`|b-c| = |b-e_9|`; its two between equations therefore imply the two queried
lengths are equal, not strictly ordered.

Adding the bad between fact makes the store infeasible immediately.  Closure
then obtains `neq c e_9` through positive segment length.  Disabling only
`between_order [SMT-bridge]` makes the final five-move state consistent and
unsolved; neither `neq c e_9` nor `peq c e_9` is present.

One detail in the original report is not an infeasibility test:
`st.la.entails({}, 1, "<=")` asks whether `0 <= 1`, so it is true in every
state.  `euclid_engine.contradictory(st)` or `_fm_sat(st.la.rows)` is the
relevant check.  On the reported final state these return `store-unsat` and
false respectively.

## Exact derivation

The fifth move is implemented at
`scripts/euclid_engine.py:1615-1623`.  It checks that `b` is inside circle
`C_1`, `b` and `g_4` are on `BC`, and `g_4` is outside `C_1`; it then adds

```
on_circle e_9 C_1
on_line e_9 BC
between b e_9 g_4
```

This is a direct transcription of
`/home/joe/code/LeanEuclid/SystemE/Theory/Constructions/Intersections.lean:9-11`:

```
∃ a : Point, a.onCircle α ∧ a.onLine L ∧ between b a c
```

There is no added freshness or inequality fact.  `apply_step`, at
`euclid_engine.py:2749` onward, gives an unbound witness a new label, declares
the label, and adds only its rule's `make` facts.  A new label is not itself
inserted as `neq` from existing labels.

The first bad inference is `r_between_order` at
`euclid_engine.py:1054-1068`.  It has the intended implication

```
between a x z ∧ between a y z ∧ |a-x| < |a-y|  →  between a x y.
```

For `a=g_4`, `x=c`, `y=e_9`, `z=b`, its two diagram premises are present,
but `la_entails_lt` at lines 637-640 wrongly accepts the strict metric premise.
The general entailment route is `LinearArithmetic.entails` and
`_entails_run`, lines 216-298.  With `between_order` excluded the 69-row store
is feasible, yet it answers:

```
|g_4-c| = |g_4-e_9|    False
|g_4-c| < |g_4-e_9|    True
```

The equal-radius and between rows algebraically force equality.  Both the
current optimized query path and the frozen pre-optimization
`euclid_engine_reference.py` path give the same wrong answers, so this defect
predates the Probe 7 speed change.

Once `between g_4 c e_9` is inserted, its segment-sum row conflicts with the
equal-radius rows.  `positive_length_neq` subsequently records `neq c e_9`,
at `euclid_engine.py:828-846`, after the poisoned arithmetic store says the
segment has positive length.  That inequality is downstream garbage, not an
assumption made by the intersection construction.  `close` already stops after detecting the
infeasible arithmetic store (`euclid_engine.py:1378-1403`), but goal queries
later call arithmetic entailment on it and therefore accept arbitrary metric
facts ex falso.

## Construction audit

I compared the `make` clauses of the primitive construction families against
their cited LeanEuclid declarations:

- `Points.lean`: `arbitrary_point` adds no fact; `distinct_points` adds the
  stated inequality.
- `LinesAndCircles.lean`: line and circle constructors add only their stated
  incidence/centre facts.  They add no inequality against old objects.
- `Intersections.lean:3-26`: all intersection rules add the incidences,
  between/side relation, and only the inequality explicitly present in
  `intersections_circle_line` at lines 6-7.
- The Book proposition constructors add the conjunctions in their cited
  existential conclusions.  None has a generic "fresh witness differs from
  every existing point" conclusion.

Thus no other construction rule makes the alleged blanket freshness
assumption.  The engine does contain additional rules explicitly labelled
`[SMT-bridge]`; they represent consequences Lean's coordinate SMT is expected
to prove rather than named LeanEuclid construction statements.
`between_order` is the one exercised here, and its guard is only as sound as
the arithmetic decider.  The audit also found UI-only `name_point` and
`assert_*` rules, but those state an identification and validate it rather
than asserting freshness.

## Incidence among logged wins

### Probe 7

Both original random wins are false positives:

| logged trace | moves | goal reported | consistency |
|---|---:|---:|---|
| worker 0 | 10 | yes | infeasible, 392 LA rows |
| worker 1, game 3967 | 9 | yes | infeasible, 662 LA rows |

The committed deletion-minima are also infeasible (7 moves / 319 rows and
5 moves / 209 rows).  Consequently Probe 7's random rate is a rate of
contradictory terminal states, not genuine I.2 solutions, and its ZERO arm was
trained from an invalid start-state curriculum.

### Probe 3

Probe 3 records only aggregate counters in `spiel/az/i2-seed2/i2.jsonl`.
`euclid_alpha_zero.py` did not persist winning trajectories, so its seven I.2
wins cannot be replayed individually.  They must now be treated as
unvalidated, not as seven valid constructions.  Their source run used the
same unguarded goal predicate.

### Packaged AlphaZero I.1

I replayed 20 winning games sampled from the four final-checkpoint actor logs
under `spiel/az4/i1` (five final wins per actor).  All 20 final states were
consistent.  They use the genuine Euclid I.1 construction: circles centred at
the two given endpoints, followed by `intersection_circles`; redundant circle
constructions account for the longer variants.  The I.1 result reported to
Avigad is therefore genuine for this sample.

### Browser path

Calling `euclid_play.play(2, moves)` in-process with the five-move trace accepts
all five moves and returns both `goal_reached: true` and
`contradictory: true`.  Therefore the browser page can reach and display this
false solution.  No request was sent to the live server and port 8127 was not
touched.

The goal fast path is `euclid_play.py:784-785`: if every named goal passes
`_fact_holds`, it returns `{}`.  `_play` computes `goal_reached` before
`contradictory` and returns both independently (`euclid_play.py:3240-3304`).

## Fix choices and cost

### A. Correct the source inference

The source defect to repair is the arithmetic decision that certifies the
strict premise, not the LeanEuclid intersection statement.  Add a reduced
regression containing the equal-radius and between equations and require:

```
entails(equal lengths)       == True
entails(either strict order) == False
```

Then repair the equality-elimination/Fourier--Motzkin path.  Temporarily
removing `between_order` would prevent this trace but would also remove a
valid coordinate consequence used elsewhere; it is a diagnostic containment,
not the structural fix.

### B. Refuse or quarantine infeasible ordinary play

Every ordinary construction should be transactional: apply and close it on a
copy, run `contradictory`, and commit only a consistent result.  Contradiction
is legitimate inside `by_contra` and `by_cases`, so the guard must know that
context; it cannot simply reject every infeasible `State` globally.  At the
minimum, both OpenSpiel reward and browser `_goal_reached` must return false
when the outer state is contradictory.  That prevents ex-falso reward even if
another inference bug survives.

### C. Recommended

Do both.  Repairing the decider preserves valid inference; the transactional
outer-state guard enforces the game invariant and turns future soundness bugs
into refused moves rather than rewards.  Goal-only guarding is cheaper but
leaves a poisoned state in which later moves and advice are meaningless.

The existing 339 engine/trace tests and 99 play tests should retain their
current expected answers under the arithmetic repair plus context-aware
guard: their ordinary reference end states are intended to be consistent,
while contradictions inside proof blocks remain permitted.  The counts would
increase with new regressions for (1) the equal-distance strict-order query,
(2) refusal of this exact fifth move in ordinary play, (3) no reward from a
manually infeasible outer state, and (4) preservation of contradiction inside
`by_contra`.  If `between_order` were simply deleted instead, tests or traces
that rely on that valid ordering consequence could fail and would need proof
changes; that option should not be accepted merely because the current suites
do not name the rule directly.
