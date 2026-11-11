# SMT-1 — System E entailment from notation-game positions

## Method

The checker is `scripts/euclid_smt_check.py`. It replays accepted notation-game moves, but sends cvc5 only
the proposition hypotheses and the explicit facts returned in each construction report's `added` field—not the
engine's forward closure. It extracts LeanEuclid's 112 literal SMT axioms verbatim from
`SystemE/Meta/Smt/EuclidTheory.lean`: the sorts and relation/function signature are at lines 10–30 and the
assertions begin at lines 31–38. Fact names follow LeanEuclid's translator: metric terms at
`Translator.lean:60–84`, logic and arithmetic at lines 99–135, and geometric relations at lines 137–146.

cvc5 was 1.2.1, with a fixed 5 s timeout for the table (the acceptance test uses 2 s for the known undecided
negative). Z3 4.13.2 was also tried on the seven-move I.2 query and returned no answer in 20 s. The goal witness
is enumerated over the position's constructed, non-given points, exactly the finite domain used by the game;
adding an SMT existential made cvc5 give `unknown` as well. `follows` means cvc5 found the negated goal unsat.

## I.2 prefixes

| position | engine | cvc5 | seconds |
|---|---:|---|---:|
| 0 moves | no | unknown (timeout) | 5.007 |
| 1 move | no | unknown (timeout) | 5.004 |
| 2 moves | no | unknown (timeout) | 5.005 |
| 3 moves | no | unknown (timeout) | 5.005 |
| 4 moves | no | unknown (timeout) | 5.007 |
| 5 moves | no | unknown (timeout) | 5.007 |
| 6 moves | no | unknown (timeout) | 5.008 |
| 7 moves | no | unknown (timeout) | 5.007 |
| 8 moves | yes | follows | 0.118 |

Thus cvc5 certifies the solved position quickly, but does not decide that any earlier position is a
non-solution. A `sat` answer would only establish non-entailment relative to this first-order axiom set; none was
obtained.

## Final Luna positions

| position | moves | engine | cvc5 | seconds |
|---|---:|---:|---|---:|
| luna-I1-1 | 3 | yes | follows | 0.038 |
| luna-I2-1 | 6 | no | unknown | 5.008 |
| luna-I2-2 | 7 | no | unknown | 5.004 |
| luna-I2-3 | 6 | no | unknown | 5.004 |
| luna-I2-4 | 6 | no | unknown | 5.008 |
| luna-I2-5 | 3 | no | unknown | 5.006 |
| luna-I2-6 | 6 | no | unknown | 5.009 |
| luna-I3-1 | 3 | no | unknown | 5.005 |
| luna-I3-2 | 2 | no | unknown | 5.005 |
| luna-I3-3 | 2 | no | unknown | 5.005 |
| luna-I3-4 | 2 | no | unknown | 5.006 |
| luna-I4-1 | 1 | no | follows | 4.349 |
| luna-I4-2 | 1 | no | follows | 4.289 |
| luna-I4-3 | 1 | no | follows | 4.093 |
| luna-I4-4 | 1 | no | follows | 4.185 |
| luna-I5-1 | 0 | no | unknown | 5.004 |
| luna-I5-2 | 1 | no | unknown | 5.004 |
| luna-I5-3 | 1 | no | unknown | 5.004 |

The I.4 disagreement is genuine and repeatable across the four logs. The raw hypotheses are the SAS data, and
LeanEuclid's full SMT theory proves the three I.4 goal equalities after the superposition move. The game engine
does not implement all consequences of that SMT theory in its hand-written closure, so `goal_reached` remains
false. This is not leakage from engine closure: the query contains only the givens and the superposition rule's
explicit eleven output facts. The other disagreements are `unknown`, not contrary verdicts.

## Conclusion

cvc5 is useful as a proof checker for positive positions: it certified I.1, the completed I.2 construction, and
four I.4 states. It is not a decision procedure here: quantified System E axioms prevented a satisfiable or
unknown-negative position from being classified within five seconds (and the I.2 seven-move case defeated Z3's
20 s cross-check too). It also exposes a real coverage difference: LeanEuclid's SMT axiom set proves I.4 where
the current game engine closure does not yet recognize the goal.
