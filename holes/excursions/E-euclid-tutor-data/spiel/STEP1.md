# DESIGN-1 step 1: one name per object

Date: 2026-11-11.  Base: futon0 `62ed51a` (including LEGAL-1
`b5c93b8`).  Measurements used `PYTHONHASHSEED=0` and the OpenSpiel Python
environment at `~/.venvs/openspiel`.

## Ruling

The engine remains unchanged and continues to retain `peq` facts.  The game
view now takes the transitive closure of `peq` and represents each equivalence
class by its earliest-created name (givens, then history bindings, then other
declared names).  No other implemented object-equality predicate exists in
`euclid_engine.py`; `peq_line` in `euclid_spiel._objects` is a stale reader for
a fact the engine never produces.

Canonical names are used by move generation, dense-codec object slots,
sampled actions, action application, goal witness object lists, and the
observation.  Facts are canonicalised and deduplicated in the observation;
tautological `peq` and substitution bookkeeping made tautological by the
quotient are omitted.  An applied construction whose only additions are
`peq` facts naming fresh bindings is not a game move.  Every remaining listed
move is still checked by the LEGAL-1 certified application guard.

## Before and after

The fixed starts use the rc13 stage-2 clean prefix, two perturbations at rate
0.5, seed 2026111102.  Random positions use seeds from `Random(20261111)` and
up to four guarded random moves.  Enumeration time is one pass per position,
so the cold clean-prefix number is descriptive rather than a microbenchmark.

| population | aliases before | legal before | alias moves before | legal after | alias moves after | median enumerate before / after |
|---|---:|---:|---:|---:|---:|---:|
| rc13 stage-2 prefix | 0 | 532 | 0 | 527 | 0 | 2123.8 / 2190.8 ms |
| 10 fixed perturbed starts | 0 | 6,007 | 0 | 5,955 | 0 | 1087.7 / 1487.8 ms |
| 30 random I.2 positions | 7 | 4,554 | 937 | 3,571 | 0 | 188.5 / 117.1 ms |

The ten fixed legal counts changed from
`[804 532 532 532 532 532 947 532 532 532]` to
`[798 527 527 527 527 527 941 527 527 527]`.  The small reductions even
without an existing alias are the now-absent moves whose sole result would
have been a fresh alias.  The engine still contains aliases in the seven
random cases; the after-view reports zero exposed alias objects by design.

## Replays and falsifiers

The LEGAL-1 reproduction cannot recur through the game: after an engine-level
`name_point(a) -> alias_a`, the engine retains `peq(a, alias_a)`, while the
object slots and legal moves expose only `a`; an alias-only `name_point` is not
listed.  Goal witness search continues to range over every distinct canonical
point.

The recorded rc11, rc12 and rc13 stage-1 receipt files contain the same 20
acceptance wins (clean plus perturbed) in three runs, 60 rows total.  All 60
replay to the goal.  Replay maps any historical alias argument through the
current equivalence class before applying the unchanged engine step.  The
existing eight-move I.2 witness test also remains green.

## Warrant

`PYTHONHASHSEED=0 ~/.venvs/openspiel/bin/python -m pytest -q
scripts/euclid_spiel_test.py -k 'not actor_weight_snapshot'`:
25 passed, 1 deselected in 413.34 s.  The deselected pre-existing neural-model
snapshot test aborts in this environment inside JAX backend initialisation;
it does not exercise `euclid_spiel` game semantics.  The new quotient and
60-receipt replay tests are included in the 25.  `git diff --check` passes.
`PYTHONHASHSEED=0 ~/.venvs/openspiel/bin/python -m pytest -q
scripts/euclid_engine_test.py`: 209 passed in 53.30 s.
