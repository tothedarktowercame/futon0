# Cross-tab: pattern stage x 象 intent stage (2026-08-22 .. 2026-10-01)

All numbers are produced by `crosstab_stage_intent.py`.

## Join

- join rows: 28135, matched: 4062, distinct pattern-labelled turns: 4059
- analysis records: 1189 (with a reading 1122; with evidence_id 462)
- primary key (join turn-id == evidence_id): 331 turns
- exact fallback (session + parsed instant): 0 turns; text matching used: none
- joined: 331; could not join: 3728 (pattern turn with no analysis record under either key); multi-stage pattern turns set aside: 0
- fragments with an intent outside legend-rows: 4 (top: reframe x1, direct x1, define x1, agree x1)

## 1. Stage agreement matrix (per (turn, intent) pair)

| pattern \ intent | ACT | BELIEVE | EVALUATE | PERCEIVE | SELECT | total | cue-only |
|---|---|---|---|---|---|---|---|
| PERCEIVE | 3 | 24 | 14 | 19 | 24 | 84 | 22 |
| BELIEVE | 20 | 37 | 20 | 49 | 47 | 173 | 41 |
| EVALUATE | 5 | 13 | 5 | 11 | 18 | 52 | 8 |
| SELECT | 10 | 40 | 22 | 43 | 37 | 152 | 28 |
| ACT | 20 | 31 | 28 | 46 | 40 | 165 | 26 |
| ASSURANCE | 32 | 55 | 28 | 53 | 58 | 226 | 55 |
| COORDINATION | 6 | 14 | 15 | 16 | 22 | 73 | 17 |
| NONE | 0 | 4 | 0 | 1 | 0 | 5 | 2 |

Cue-only intents (secondary column, base-record cues[].label mapped through legend-rows): 199 pairs, agreement 16.6%.


Per-turn (first staged intent) overall agreement: 15.5% on n=317 turns; per-pair agreement 12.7% on n=930 pairs.

Per-pattern-stage agreement (pairs):
| pattern stage | agreeing | total | rate |
|---|---|---|---|
| PERCEIVE | 19 | 84 | 22.6% |
| BELIEVE | 37 | 173 | 21.4% |
| EVALUATE | 5 | 52 | 9.6% |
| SELECT | 37 | 152 | 24.3% |
| ACT | 20 | 165 | 12.1% |
| ASSURANCE | 0 | 226 | 0.0% |
| COORDINATION | 0 | 73 | 0.0% |
| NONE | 0 | 5 | 0.0% |

## 2. R-nodes of agreeing pairs, per stage

| stage | intent R-node counts | pattern :node counts (where present) |
|---|---|---|
| PERCEIVE | {'R2': 13, 'R8': 6} | {} |
| BELIEVE | {'R3': 25, 'R7': 10, 'R1': 2} | {'R17': 1} |
| EVALUATE | {'R5': 4, 'R4': 1} | {'R5': 2, 'R20': 1} |
| SELECT | {'R6': 29, 'R13': 3, 'R15': 2, 'R11': 2, 'R14': 1} | {} |
| ACT | {'R16': 14, 'R9': 6} | {'R16': 1} |

## 3. Pattern :node vs intent R-node equality (both present)

- both present: 5; equal: 2 (40.0%)

## 4. Examples for the 4 largest disagreement cells

### ASSURANCE -> SELECT (58 pairs)
- `cascade-construction/run-it-on-a-real-case` / intent `propose`: "OK, so PROOF-2a is build complete, apparently, or very nearly, and we now need to move to the runtime evindence gatherin"
- `contracts/holder-states-the-claim` / intent `propose`: "Well Rob has sent other things to me last week, maybe he forgot to upload it, but this is a bit of mystery.  If we look "
- `agency/invariants` / intent `propose`: "Well I think the "who" is left purposefully vague.  Let me interview you.  You too have an AI workflow, subagents, Agenc"
### ASSURANCE -> BELIEVE (55 pairs)
- `agency/invariants` / intent `clarify`: "Well I think the "who" is left purposefully vague.  Let me interview you.  You too have an AI workflow, subagents, Agenc"
- `agency/invariants` / intent `collect`: "Well I think the "who" is left purposefully vague.  Let me interview you.  You too have an AI workflow, subagents, Agenc"
- `contracts/holder-states-the-claim` / intent `approve`: "Very appropriate pattern selection BTW; on the topic of the re-arm condition, I'd say the explicit condition I personall"
### ASSURANCE -> PERCEIVE (53 pairs)
- `memory/verify-in-the-serving-process` / intent `report`: "Sorry, we had a futon1b problem, please continue"
- `cascade-construction/run-it-on-a-real-case` / intent `report`: "OK, so PROOF-2a is build complete, apparently, or very nearly, and we now need to move to the runtime evindence gatherin"
- `contracts/holder-states-the-claim` / intent `report-problem`: "Well Rob has sent other things to me last week, maybe he forgot to upload it, but this is a bit of mystery.  If we look "
### BELIEVE -> PERCEIVE (49 pairs)
- `features/emit-per-tick-mismatch` / intent `report`: "Hm... the old APM loop allowed up to 50 turns!"
- `features/emit-per-tick-mismatch` / intent `report-problem`: "Hm... the old APM loop allowed up to 50 turns!"
- `agency/identifier-separation` / intent `report`: "~/.linodetoken should work for you to do the IP swap"
