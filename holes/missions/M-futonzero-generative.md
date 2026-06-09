# Mission: FutonZero Generative — Self-Bootstrapping WM Learner

**Date:** 2026-06-09
**Status:** HEAD / IDENTIFY charter
**Owner:** Joe + agents
**Cross-ref:**
  - `M-futonzero-capability.md` §22 (source mapping; completed monitor)
  - `M-capability-star-map.md` §5 fan-out follow-on
  - `E-warranted-play` (play-outs over the capability landscape)
  - `M-pudding-peradams` §13-16 (Dokusan / peradam reward rigor)
  - `M-mission-scopes-into-substrate-2` (field / simulator substrate)
  - War Machine EFE ranking (`futon2.aif.efe`)

## 0. HEAD

FutonZero currently names a **monitor**: it reads evidence and measures capability
trajectories. This mission charters the generative sibling: a learner that turns
one-off warranted functionings into repeatable capabilities.

The motivating analogy is AlphaZero, but only at the structural level:

> no pre-labelled training corpus; start shallow; play against the field; learn
> from the outcomes of the system's own attested play.

This is **not** a claim that the stack has a chess-like simulator or a clean
win/loss reward today. It does not. Those are the two hard gates of this
mission.

## 1. IDENTIFY

### 1.1 Problem

The War Machine can already rank and execute bounded next moves under guardrails.
The capability star map can already name held/satisfied capability state and
pre-witness shapes. FutonZero can already observe capability trajectory after
the fact.

What is missing is the generative loop:

1. propose a bounded play-out over the capability landscape;
2. predict its likely field effect;
3. execute only if guardrail-admissible;
4. score the outcome with anti-laundered witness logic;
5. update future policy/value estimates from the play-out outcome;
6. distinguish one-off functioning from repeatable capability.

Without that loop, the stack has strategic selection and post-hoc monitoring,
but not a self-bootstrapping learner.

### 1.2 AlphaZero to WM mapping

| AlphaZero term | WM / FutonZero-generative term | Current status |
|---|---|---|
| self-play | warranted WM play-outs over the capability star map | shallow, propose-only and one-step variants exist |
| game state | capability graph + mission/proof state + substrate-2 field estimate | partial |
| policy network | pattern library as reusable policy proposals | partial |
| value network | EFE / forward model over play-out consequences | partial and one-step-biased |
| simulator | learned field simulator over mission/capability dynamics | **hard gate: not adequate yet** |
| reward | Dokusan / peradam / witness outcome | **hard gate: must remain anti-laundered** |
| training data | the stack's own play-out -> outcome evidence | exists only as raw substrate, not enough for rollout learning |

### 1.3 Sen objective

The objective is exactly the Sen distinction already used by FutonZero:

- **functioning:** one play-out lands a fruit or clears a level;
- **capability:** the system learns a policy that can clear that class
  repeatably under the right conditions.

The learner's job is not to maximize activity. It is to convert attested
functionings into durable freedoms-to-achieve.

## 2. Hard Gates

### G-SIM: field-simulator adequacy

No generative FutonZero runner may be built until the field simulator can make
bounded, checkable predictions over play-out consequences.

Minimum admissible simulator:

- reads the same capability/mission substrate as WM;
- predicts at least one-step and two-step consequences as explicit hypotheses;
- records confidence and uncertainty, not just a rank;
- is calibrated against prior play-out -> outcome evidence;
- refuses fabricated dynamics when the field has no data.

Open home: substrate-2 / full-C / field-model work. Until this gate clears,
only logic models, fixtures, and read-only evaluation are allowed.

### G-REWARD: anti-laundered reward rigor

No reward signal may train the learner unless it is anti-laundered.

Minimum admissible reward:

- distinguishes build discharge from witness discharge;
- uses Pudding Prover / peradam-style witness discipline where applicable;
- keeps G1 arrow-witness false until bound to live affect evidence;
- records rejected/routed outcomes, not only wins;
- treats operator approval as a gate, not as retroactive proof of fruit.

Open home: `M-pudding-peradams` G1 and the Pudding Prover reward channel. Until
this gate clears, "reward" remains a labelled evaluation artifact, not training
fuel.

## 3. Scope

### Scope in for this charter

- Name the generative FutonZero sibling of the completed monitor.
- Freeze the AlphaZero -> WM mapping.
- Name the two hard gates.
- Define the first safe work products that can happen before realization.

### Scope out until gates clear

- No autonomous self-play runner.
- No reward-trained policy update.
- No hidden scheduler.
- No claim that a bounded play-out proving one functioning is already a
  capability.
- No bypass of WM guardrails or Pudding Prover anti-laundering.

## 4. Safe First Work Products

These can proceed before G-SIM and G-REWARD clear:

1. **Static rollout ledger:** schema for recording proposed play-outs,
   predicted consequences, actual outcomes, and witness class.
2. **Toy field fixture:** a deliberately small graph with known transitions,
   used only to test update math and failure modes.
3. **Policy/value vocabulary:** data shape for pattern-as-policy proposals and
   value estimates, with no live actuator.
4. **Calibration audit:** compare past WM choices against observed outcomes
   without changing future WM behavior.
5. **Reward red-team fixture:** cases where a fake win would be tempting, and
   the certifier must reject or route it.

## 5. Exit Criteria for IDENTIFY

- The rollout ledger schema is specified.
- The two hard gates have owners / home missions and testable clearance
  criteria.
- At least one toy field fixture exists and demonstrates both a successful
  update and a refusal to learn from laundered reward.
- A future INSTANTIATE slice can be written without weakening WM, Pudding
  Prover, or capability-star-map invariants.

## 6. Current Disposition

This mission is chartered, not realized. The correct next move is not to build
an autonomous learner; it is to prepare the ledger, fixture, and gate tests that
would make such a learner honest.

When G-SIM and G-REWARD both clear, the mission can advance from charter to a
small, local, read-only rollout-learning prototype.
