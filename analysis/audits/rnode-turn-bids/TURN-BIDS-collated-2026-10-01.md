# R-node bids on 300 blinded operator turns, collated (2026-10-01)

373 bids kept from 23 nodes (3 dropped: quote not in the turn). 167 of 300 turns have at least one bid.

**Stage consistency:** 70 of 373 bids (18.8%) come from a node in the pattern's stage label; chance from the marginals 15.2%.

**Graph agreement** (pattern's top turn-bid node is among the R-nodes within 2 graph steps; chance = a random node of the same count):

- graph18: 26 of 61 patterns (43%); chance expectation 25.7 (42%)
- graph52: 40 of 63 patterns (63%); chance expectation 35.8 (57%)

## Bids per node

R8 30 · R2 28 · GRAIN-GATE 26 · R1 25 · R3a 24 · R20 22 · R3 22 · R16 21 · R6 19 · R11 18 · R12 18 · R5 17 · R13 16 · R17 14 · R15 13 · R19 13 · R4 13 · R14 11 · R10 10 · R7 5 · R9 4 · CTAU-TOKEN 3 · CTAU-CLASS 1

## R-nodes by pattern stage label

- **ACT**: R16 8, GRAIN-GATE 7, R8 7, R2 6, R20 6, R3a 4, R4 4, R10 3, R12 3, R13 3, R3 3, R6 3, R14 2, R5 2, R11 1, R15 1, R17 1, R19 1, R7 1
- **ASSURANCE**: R11 10, R2 8, R20 7, GRAIN-GATE 6, R1 6, R13 6, R3 6, R5 6, R8 6, R3a 5, R12 4, R16 4, R19 4, R6 4, R14 3, R9 3, R15 2, R4 2, R7 2, CTAU-TOKEN 1, R10 1, R17 1
- **BELIEVE**: R1 7, R12 7, R17 7, R6 5, R15 4, R8 4, R16 3, R2 3, R3 3, R5 3, R19 2, R3a 2, R4 2, R7 2, GRAIN-GATE 1, R9 1
- **COORDINATION**: GRAIN-GATE 5, R8 5, R10 4, R14 3, R2 3, R3 3, R3a 3, R6 3, R11 2, R13 2, R16 2, R19 2, R4 2, CTAU-TOKEN 1, R1 1, R12 1, R15 1, R17 1
- **EVALUATE**: R3a 3, R1 2, R20 2, GRAIN-GATE 1, R16 1, R2 1, R8 1
- **NONE**: R16 1, R19 1, R8 1
- **PERCEIVE**: R1 6, R2 6, R3a 5, R3 4, R8 4, R17 3, R5 3, R11 2, R12 2, R14 2, R20 2, R10 1, R15 1, R16 1, R6 1
- **SELECT**: GRAIN-GATE 6, R13 5, R20 5, R15 4, R1 3, R11 3, R19 3, R3 3, R4 3, R5 3, R6 3, R3a 2, R8 2, CTAU-CLASS 1, CTAU-TOKEN 1, R10 1, R12 1, R14 1, R16 1, R17 1, R2 1

## Per pattern (3 turns each)

| pattern | label | turn bids | graph18 | graph52 |
|---|---|---|---|---|
| `agency/delivery-receipt` | assurance | R1 1, R8 1 | R1, R10, R11, R14, R15, R20, R6, R7, R8, R9 | R1, R10, R11, R12, R14, R15, R17, R2, R20, R5, R6, R7, R8, R9 |
| `agency/identifier-separation` | believe | R1 1, R15 1, R19 1, R2 1, R5 1, R6 1 | R15, R17, R8 | R14, R15, R17, R5, R8, R9, TRACE |
| `agency/invariants` | assurance | R11 1, R2 1, R6 1 |  |  |
| `aif/interoceptive-tripwires` | evaluate | R1 2, R20 2, R3a 2, GRAIN-GATE 1, R2 1, R8 1 | R10, R15, R16, R20, R7, R9 | R10, R14, R15, R16, R17, R2, R20, R7, R9 |
| `ants/baseline-cyber-ant` | believe | R15 1 |  |  |
| `ants/cargo-return-discipline` | act | GRAIN-GATE 1, R13 1, R14 1, R16 1, R2 1, R20 1, R8 1 |  |  |
| `apparatus/the-system-stops-on-schedule` | select | R1 1, R20 1 | R15, R20, R7 | R15, R17, R2, R20, R5, R7, R9 |
| `cascade-construction/lift-when-three-align` | believe | R17 2, R1 1, R15 1, R16 1 | R10, R13, R15, R20, R4, R5, R6, R8 | R10, R12, R13, R14, R15, R17, R2, R20, R4, R5, R6, R8, R9, TRACE |
| `cascade-construction/read-what-exists-first` | perceive | R2 2, R1 1, R20 1, R3a 1 | R10, R14, R15, R17, R20, R4, R5, R6, R7, R8, R9 | R10, R12, R14, R15, R17, R2, R20, R4, R5, R6, R7, R8, R9, TRACE |
| `cascade-construction/run-it-on-a-real-case` | assurance |  | R10, R14, R15, R17, R20, R4, R5, R6, R7, R8, R9 | R10, R12, R14, R15, R16, R17, R2, R20, R4, R5, R6, R7, R8, R9, TRACE |
| `cascades/on-the-fly-cascade` | select | CTAU-TOKEN 1, R19 1, R6 1 | R10, R17, R2, R20, R6, R8 | R10, R13, R17, R2, R20, R5, R6, R8, R9, TRACE |
| `code-coherence/dead-code-hygiene` | act | GRAIN-GATE 1, R3a 1 | R15, R2, R20, R7, R9 | R12, R15, R17, R2, R20, R5, R7, R9 |
| `contracts/holder-states-the-claim` | assurance | GRAIN-GATE 2, CTAU-TOKEN 1, R11 1, R12 1, R13 1, R15 1, R19 1, R5 1, R9 1 | R9 | R12, R5, R9 |
| `contributing/devmap-contribution-protocol` | believe | R17 2 |  |  |
| `contributing/stack-scan-logging-protocol` | assurance |  | R15 | R15 |
| `coordination/ARGUMENT` | coordination | R4 1, R6 1 | R10 | R10, R2 |
| `cycle-machine/job-port` | coordination | GRAIN-GATE 1, R1 1, R11 1, R12 1, R13 1, R15 1, R3 1, R3a 1, R8 1 | R2, R20, R3, R6, R8 | R2, R20, R3, R6, R8, R9 |
| `cycle-machine/runtime-restoration` | act | R19 1, R6 1, R8 1 | R1, R10, R15, R6, R7, R8 | R1, R10, R11, R14, R15, R16, R17, R2, R5, R6, R7, R8, R9, TRACE |
| `cycle-machine/step-machine` | act | GRAIN-GATE 1, R10 1, R12 1 | R10, R4 | R10, R17, R2, R4, R5, R8, TRACE |
| `cycle-machine/toolchain-port` | assurance | R14 1, R20 1, R8 1 | R15, R20 | R15, R20, R5, R9 |
| `data-mining/checkpoint-the-long-run` | act | R20 2, R12 1, R16 1, R17 1, R2 1, R3 1, R4 1 | R10, R15, R4, R9 | R10, R12, R15, R20, R4, R5, R9, TRACE |
| `devmap-coherence/baseline-freeze` | assurance | GRAIN-GATE 2, R11 2, R13 1, R14 1, R15 1 | R2 | R2 |
| `devmap-coherence/ifr-f1-dhammavicaya` | assurance | R11 2 | R2 | R2 |
| `devmap-coherence/ifr-f2-viriya` | assurance | R2 1, R3 1 | R2 | R2 |
| `devmap-coherence/ifr-f3-piti` | assurance | R20 3, R8 2, R2 1, R3 1 | R2 | R2 |
| `devmap-coherence/ifr-f7-upa-upekkha` | assurance | R1 1, R16 1, R19 1, R2 1, R6 1 | R2 | R2 |
| `devmap-coherence/prototype-alignment-bridge` | assurance | R11 1, R14 1 | R15, R17, R2, R7, R8 | R15, R17, R2, R4, R5, R7, R8, TRACE |
| `dsc/evidence-situated-log` | assurance | R13 1, R17 1, R20 1 |  |  |
| `eight-gates/lean-commit` | select | GRAIN-GATE 1, R13 1, R17 1, R3 1, R4 1, R5 1, R6 1 |  |  |
| `enrichment/ARGUMENT` | believe | R6 2, R16 1, R4 1 |  |  |
| `features/emit-per-tick-mismatch` | believe | R1 1, R15 1, R2 1, R3 1 | R4, R5 | R14, R2, R4, R5, R8 |
| `features/operator-turns-enter-the-observation-vector` | perceive | R12 2, R14 1, R17 1, R5 1 | R15 | R14, R15, R16, R2, TRACE |
| `fulab/clock-out` | assurance | GRAIN-GATE 1, R12 1, R3a 1 |  |  |
| `fulab/session-resume` | coordination | R10 3 |  |  |
| `futon-theory/coordination-protocol` | coordination |  | R10, R20, R4, R9 | R10, R2, R20, R4, R5, R9 |
| `futon-theory/mission-interface-signature` | coordination | GRAIN-GATE 1, R16 1 | R15, R17 | R14, R15, R16, R17, R2, R20, R4, R5, TRACE |
| `futon-theory/mission-lifecycle` | believe |  | R10, R15, R17, R8, R9 | R10, R12, R14, R15, R17, R2, R20, R5, R8, R9, TRACE |
| `futon-theory/progress-signal` | perceive | R11 1, R14 1, R5 1 | R10, R15, R4, R5, R6, R8, R9 | R10, R14, R15, R16, R17, R2, R4, R5, R6, R8, R9, TRACE |
| `futon-theory/proof-path` | assurance | R11 1, R3 1, R5 1 |  |  |
| `futon-theory/stop-the-line` | select | GRAIN-GATE 3, R1 1, R13 1, R15 1, R20 1 | R10, R15, R17, R2, R20, R7, R8, R9 | R10, R12, R14, R15, R17, R2, R20, R5, R7, R8, R9, TRACE |
| `futon-theory/structural-tension-as-observation` | perceive | R1 2, R17 1, R2 1, R3 1, R3a 1, R6 1 | R5 | R2, R5 |
| `iching/hexagram-18-gu` | act | GRAIN-GATE 1, R12 1, R16 1, R3 1, R3a 1, R4 1, R7 1 |  |  |
| `iching/hexagram-35-jin` | act | R10 1, R14 1, R16 1, R2 1, R5 1, R8 1 |  |  |
| `iching/hexagram-50-ding` | act | R3a 1 |  |  |
| `iching/hexagram-62-xiaoguo` | act | R13 1, R15 1, R4 1, R6 1 |  |  |
| `inbox-zero/ignore-by-kind` | act | GRAIN-GATE 1, R2 1, R20 1, R4 1, R5 1, R6 1 | R10, R15, R20 | R10, R14, R15, R16, R2, R20, TRACE |
| `inbox-zero/promote-at-turn-end` | act | R3 1, R8 1 | R10, R15, R16, R17, R20, R6, R7, R8, R9 | R10, R13, R14, R15, R16, R17, R2, R20, R4, R5, R6, R7, R8, R9, TRACE |
| `inbox-zero/push-the-declared` | act | GRAIN-GATE 1, R16 1, R20 1 | R10, R15, R20, R4, R6, R7, R8, R9 | R10, R14, R15, R16, R17, R2, R20, R4, R5, R6, R7, R8, R9, TRACE |
| `library-coherence/library-evidence-ledger` | assurance | R12 1, R3a 1 |  |  |
| `math-formalization/statement-ladder-before-proof-text` | act | GRAIN-GATE 1, R2 1, R20 1 |  |  |
| `memory/licence-to-fail` | assurance | R16 1, R3a 1 | R1, R10, R15, R20, R7, R9 | R1, R10, R11, R12, R15, R17, R2, R20, R4, R5, R7, R8, R9, TRACE |
| `memory/name-a-reusable-artifact` | believe | R12 2, R1 1, R17 1, R3 1, R3a 1, R8 1 | R20 | R2, R20 |
| `memory/no-tightening-while-held` | evaluate | R16 1 |  | R5 |
| `memory/verify-in-the-serving-process` | assurance | R1 1, R2 1 | R1, R10, R15, R16, R17, R20, R7, R8, R9 | R1, R10, R11, R12, R13, R14, R15, R16, R17, R2, R20, R4, R5, R7, R8, R9, TRACE |
| `musn/aif-live-scores` | perceive | R3a 2, R1 1, R2 1 |  |  |
| `musn/pause-backtrace` | assurance | R9 1 | R10, R9 | R10, R17, R2, R4, R5, R9 |
| `musn/plan-before-tool` | select | R15 1, R16 1, R6 1, R8 1 | R15, R17, R20, R4, R5, R7, R9 | R14, R15, R17, R2, R20, R4, R5, R7, R8, R9, TRACE |
| `or2/topology-dungeon-crawl` | believe |  |  |  |
| `or2/xiangqi-decorators` | believe | R6 2, R5 1 |  |  |
| `orchestration/recorded-handoff` | coordination | GRAIN-GATE 1, R11 1, R14 1, R16 1 | R1, R10, R14, R15, R17, R2, R20, R4, R5, R6, R7, R8, R9 | R1, R10, R11, R12, R13, R14, R15, R16, R17, R2, R20, R4, R5, R6, R7, R8, R9, TRACE |
| `orchestration/state-in-substrate-deltas-in-messages` | coordination | GRAIN-GATE 1, R14 1, R4 1, R8 1 | R10, R15, R20, R6, R7, R9 | R10, R15, R17, R2, R20, R4, R5, R6, R7, R9, TRACE |
| `p4ng/call-a-synthesizer` | coordination |  |  |  |
| `pacspine/frictionless-next` | coordination | GRAIN-GATE 1, R10 1, R13 1, R14 1, R2 1, R6 1 |  |  |
| `pacspine/simple-close` | act |  |  |  |
| `peripherals/hot-reload-as-default-fix-path` | act | R16 2, R10 1, R11 1, R13 1 | R1, R10, R15, R2, R20, R4, R7, R8, R9 | R1, R10, R11, R12, R14, R15, R17, R2, R20, R4, R5, R7, R8, R9, TRACE |
| `problems/coordination-patterns-derivation` | coordination | R17 1, R19 1, R2 1, R6 1 |  | R9 |
| `problems/cycle-model-boundary-gaps` | none |  | R15, R20, R7, R9 | R15, R2, R20, R7, R9 |
| `problems/deliberate-refusals-allude-to-unwritten-patterns` | believe | R12 2, R17 2, GRAIN-GATE 1, R19 1, R5 1, R7 1, R9 1 | R1, R14, R15, R17, R20, R4, R5, R7, R9 | R1, R11, R14, R15, R16, R17, R2, R20, R4, R5, R7, R8, R9, TRACE |
| `problems/fundamentals-drowned-by-backlog` | select | R11 1, R13 1, R15 1, R2 1, R20 1, R3 1, R4 1, R5 1 | R1, R10, R15, R8, R9 | R1, R10, R11, R12, R14, R15, R16, R2, R5, R8, R9, TRACE |
| `problems/g-over-cascade-is-undefined` | evaluate | R3a 1 | R10, R14, R15, R17, R5, R7, R8 | R10, R14, R15, R17, R2, R4, R5, R6, R7, R8, R9, TRACE |
| `problems/operator-turns-become-inference-observations` | perceive | R10 1, R15 1, R3 1, R5 1 | R10, R15, R2, R20, R3, R4, R7, R9 | R10, R13, R15, R17, R2, R20, R3, R4, R5, R7, R9 |
| `problems/process-conduct-is-unassured` | assurance | R1 2, R11 1, R2 1, R3 1, R3a 1, R5 1 | R15 | R15, R2, R5, R9 |
| `problems/r10-scheduled-entrypoint` | perceive | R1 1, R11 1, R2 1, R8 1 | R10, R16, R20 | R10, R16, R20 |
| `problems/refusal-prediction-error-v1--source-field-missing` | believe | R12 3, R1 1, R4 1, R7 1, R8 1 |  |  |
| `problems/theory-general-enough-to-drive-the-stack` | none | R16 1, R19 1, R8 1 | R10 | R10, R2 |
| `process-coherence/status-refresh-before-work` | believe | R8 2, R1 1, R2 1, R3a 1 | R10, R12, R15, R16, R17, R2, R20, R6, R7, R9 | R10, R12, R13, R14, R15, R16, R17, R2, R20, R4, R5, R6, R7, R8, R9 |
| `process/fake-done-via-binary-closure` | assurance | R3a 1, R8 1 | R1, R10, R14, R15, R16, R9 | R1, R10, R11, R12, R14, R15, R16, R2, R20, R5, R8, R9, TRACE |
| `relationship-coherence/quote-as-oblique-address` | coordination | R2 1, R3a 1, R8 1 | R10, R15, R6, R9 | R10, R15, R5, R6, R9 |
| `social/ARGUMENT` | coordination | CTAU-TOKEN 1, R19 1, R3 1, R3a 1, R8 1 | R10, R8, R9 | R10, R5, R8, R9, TRACE |
| `stack-coherence/commit-intent-alignment` | believe |  | R1, R10, R12, R15, R17, R2, R6, R7, R8 | R1, R10, R11, R12, R15, R17, R2, R4, R5, R6, R7, R8, R9, TRACE |
| `stack-coherence/futon-bridge-health` | assurance | GRAIN-GATE 1, R16 1 | R20 | R20 |
| `stack-coherence/futon1-determinism` | assurance | R10 1, R11 1, R13 1, R16 1, R2 1, R20 1, R3 1, R4 1, R5 1, R6 1, R7 1 |  |  |
| `stack-coherence/stack-blocker-detection` | perceive | R16 1, R3 1, R8 1 | R10, R20 | R10, R13, R17, R2, R20, R8, R9 |
| `storage/canonical-interface` | act |  | R15 | R15, R5, R9 |
| `storage/durability-first` | assurance | R1 1, R2 1, R20 1, R3 1 | R12, R6 | R12, R17, R20, R6 |
| `test-registry/bind-the-subject` | assurance | R13 1, R19 1, R5 1, R9 1 |  |  |
| `test-registry/judge-adequacy` | assurance | R12 1, R19 1, R6 1, R7 1 | R20, R9 | R2, R20, R9 |
| `test-registry/register-the-run` | assurance |  | R14, R15, R17, R2, R20, R4, R6, R7, R8 | R14, R15, R17, R2, R20, R4, R5, R6, R7, R8, R9, TRACE |
| `test-registry/rerun-when-the-warrant-fails` | assurance | R13 1, R4 1, R5 1 | R10, R15, R20, R6, R7, R9 | R10, R13, R14, R15, R17, R2, R20, R5, R6, R7, R9, TRACE |
| `transition/f0-f4-boundary` | evaluate |  |  |  |
| `vsatlatarium/cached-layout-computation` | act | R16 1, R2 1, R3a 1, R8 1 | R15, R20, R7, R9 | R15, R17, R2, R20, R7, R9 |
| `war-machine/ideal-actual-gap` | perceive | R17 1, R3 1, R8 1 | R1, R10, R14, R15, R16, R17, R20, R7, R8, R9 | R1, R10, R11, R12, R14, R15, R16, R17, R2, R20, R5, R7, R8, R9, TRACE |
| `war-room/wr-16-operationalised-exploit-loops-are-first-class-observation-channels` | perceive | R1 1, R2 1, R20 1, R3a 1, R8 1 | R1, R10, R15, R2, R20, R4, R5, R6, R7 | R1, R10, R11, R13, R14, R15, R17, R2, R20, R4, R5, R6, R7, R8, R9 |
| `war-room/wr-17-futon0-as-cyborg-futon7-as-markov-blanket` | select | R15 1 |  |  |
| `war-room/wr-18-war-machine-is-demonstrated-not-hypothesised` | believe | R1 1, R16 1, R3 1 | R10, R12, R13, R15, R17, R20, R4, R5, R6, R7, R8, R9 | R10, R12, R13, R14, R15, R16, R17, R2, R20, R4, R5, R6, R7, R8, R9, TRACE |
| `war-room/wr-23-upstream-trackers-are-stack-surfaces` | coordination | R3 1, R8 1 | R20, R6, R9 | R17, R2, R20, R6, R9 |
| `war-room/wr-26-a-capability-switched-off-carries-its-re-arm-condition-in-writing-at-the-switch` | select | R13 2, R20 2, R3a 2, GRAIN-GATE 1, R1 1, R10 1, R12 1, R19 1, R3 1, R4 1, R8 1 | R10, R14, R20 | R10, R14, R17, R20, R5, R9 |
| `war-room/wr-3-social-exotype-before-implementation` | assurance | R8 1 |  |  |
| `workday/single-focus-amidst-agenda` | select | R11 2, CTAU-CLASS 1, GRAIN-GATE 1, R14 1, R19 1, R5 1 |  |  |
| `writing-coherence/structural-style-inconsistency` | act | R8 2 | R15, R9 | R14, R15, R16, R17, R2, R4, R8, R9, TRACE |
