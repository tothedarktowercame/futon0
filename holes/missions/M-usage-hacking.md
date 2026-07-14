# Mission: Usage Hacking — Spend Agent Quotas Deliberately

**Date:** 2026-07-14
**Status:** IDENTIFY (mission proposal)
**Owner:** Joe
**Blocked by:** Accurate, durable usage telemetry for Claude, Codex, and Zai
**Cross-ref:**
- `futon0/contrib/usage-report.el` — live provider usage in the Stack HUD
- `futon0/contrib/stack-hud.el` — operational display and APM formal-proof burndown
- `futon3c/scripts/apm_formal_zai_cron.py` — current conservative Zai quota gate
- `futon3c/.state/apm-formal-zai/formal-progress.jsonl` — formal-proof outcome history

## 1. Motivation

Subscription usage is a perishable resource: unused allowance disappears at a
reset, but spending too aggressively can starve interactive work or another
agent workload before that reset. We currently manage this with local rules of
thumb. For example, the unattended APM formal-proof job runs only when every
reported Zai token window has strictly more than 50% available. That is a good
conservative policy while other Zai work is in progress, but it is not yet a
general allocation strategy.

The missing capability is not another threshold. It is an evidence-backed view
of quota consumption over time, joined to workload and outcome evidence, from
which scheduling policies can be derived and evaluated. Only on 2026-07-13 did
Claude usage become accurate enough to contemplate this comparison. Zai has
live five-hour and weekly figures; Codex has its own windows and evidence
surfaces. They are not yet represented as one coherent time series.

This mission uses "usage hacking" in the constructive sense: understanding the
shape and reset semantics of paid agent allowances well enough to spend them
deliberately, preserve capacity for valuable foreground work, and avoid losing
useful capacity at the end of a window.

## 2. Mission Result

Deliver a provider-neutral usage ledger, burndown/forecast view, and policy
evaluation harness for Claude, Codex, and Zai. It must make four questions
answerable from evidence:

1. How much usage remains in each active window, and when does it reset?
2. At the present burn rate, how much allowance is likely to remain at reset?
3. Which foreground and unattended workloads consumed it?
4. What useful outcomes did that consumption produce?

The result should support—but must not silently enable—adaptive policies such
as relaxing an unattended-work threshold near the final overnight period before
a reset.

## 3. Worked Example: Zai, 13–18 July 2026

### 3.1 Initial policy

The APM formal Lean cron was configured to start at most one new problem every
15 minutes when:

- every Zai token window had **strictly more than 50% available**; and
- no more than one other Zai agent was invoking.

This treats the five-hour and weekly quotas as a conjunction. It intentionally
protects capacity for other Zai-related work.

### 3.2 Observed overnight behaviour

On the evening of 2026-07-13, the cron dispatched formal-proof attempts while
both windows remained above the threshold. By 22:30 UTC both windows reported
49% available, so dispatch stopped. The five-hour window subsequently reset:

| Observation | Five-hour free | Weekly free | Decision |
|---|---:|---:|---|
| Before closure | 58% | 51% | dispatch permitted |
| 22:30 UTC | 49% | 49% | gate closed |
| 04:15 UTC, after short-window reset | 99% | 45% | gate remained closed |
| Morning of 14 July | 91% | 42% | gate remained closed |

The weekly window reported its next reset for approximately 08:48 UTC on
2026-07-18. The job was therefore correct not to resume at 04:15 under the
current rule: the short window had recovered, but the protected weekly reserve
had not.

### 3.3 Outcome evidence

During the initial run, the formal-proof counters changed as follows:

| Measure | Baseline | Later observation | Delta |
|---|---:|---:|---:|
| Lean attempts | 96 | 104 | +8 |
| Clean proofs | 60 | 60 | 0 |
| Partial proofs | 36 | 44 | +8 |
| Open `sorry` occurrences | 111 | 133 | +22 |
| Problems remaining without a clean proof | 431 | 431 | 0 |

This is why quota data alone is insufficient. The run consumed allowance and
produced eight checked partial attempts, but no net clean-proof burndown. A
future policy must be able to distinguish useful boundary-finding from clean
completion without pretending that either is free.

### 3.4 Candidate end-of-window policy

On the night of 17 July and early morning of 18 July, unused weekly allowance
would be close to expiry. A candidate policy might progressively relax the
weekly reserve while retaining a foreground reserve and the concurrency gate.
For example, it could permit more unattended work when forecast unused quota at
reset exceeds a configured reserve.

This is an example to evaluate, not an authorised production policy. The
mission must first establish accurate cross-provider history, reset semantics,
forecast error, and operator-visible decisions.

## 4. Scope

### 4.1 Scope in

- A versioned, append-only usage observation schema shared across providers.
- Claude, Codex, and Zai adapters that preserve each provider's native windows,
  reset timestamps, units, and provenance.
- A usage burndown chart showing used/free allowance over time and reset
  boundaries.
- Forecasts for consumption rate and projected allowance remaining at reset,
  with uncertainty or forecast error made visible.
- Attribution to foreground and unattended workloads where durable invocation
  evidence exists.
- Outcome joins for workloads with explicit result measures, beginning with APM
  clean/partial proof progress.
- A replay harness that evaluates candidate scheduling policies against
  historical observations without dispatching agents.
- Operator-visible recommendations and explanations before any automatic
  policy actuation.

### 4.2 Scope out

- Bypassing provider limits, terms, or enforcement mechanisms.
- Treating estimated or scraped values as exact without provenance.
- A universal conversion between Claude, Codex, and Zai tokens or percentages.
- Automatic end-of-window quota burning before replay and shadow-mode evidence
  demonstrate that foreground reserves are preserved.
- Optimising only for raw token consumption or number of invocations.
- Replacing provider-specific safety, concurrency, or workload gates.

## 5. Evidence Model

Each observation should preserve at least:

- provider and account/plan identity without storing credentials;
- observation timestamp and source;
- window identity and duration, if known;
- percentage or absolute amount used and available;
- reported next-reset timestamp;
- whether the value is exact, provider-reported, inferred, or unavailable;
- active invocations and workload class at observation time;
- policy decision and the facts that caused it;
- links to durable outcome evidence when available.

Provider-native facts must remain recoverable. Normalised percentages are a
view, not a replacement for the original measurement.

## 6. Policy Invariants

1. **Fail closed on missing usage evidence.** Adaptive scheduling cannot turn
   an unavailable quota source into permission to spend.
2. **Protect foreground reserves explicitly.** "Use it or lose it" never means
   consuming capacity already allocated to interactive or higher-priority work.
3. **Respect every active window.** A short-window reset does not erase pressure
   in a longer window, as the 04:15 Zai example demonstrates.
4. **No silent policy changes.** Thresholds, forecasts, and relaxations are
   versioned and rendered with their decision rationale.
5. **Outcomes remain distinct.** Attempts, checked partials, clean completions,
   and failures are separate measures.
6. **Forecasts do not become facts.** Predicted remaining quota and reported
   remaining quota are stored and displayed separately.
7. **Replay before actuation.** A candidate adaptive policy must be evaluated on
   historical windows and then run in recommendation-only shadow mode.

## 7. Derivation Path

1. **IDENTIFY**
   - audit the accuracy, update cadence, and reset semantics of every provider;
   - inventory invocation and outcome evidence that can support attribution.
2. **MAP**
   - define the provider-neutral observation schema without erasing native
     differences;
   - map usage observations to workloads, agents, and outcomes.
3. **DERIVE**
   - derive burn rates, reset-aware forecasts, protected reserves, and candidate
     adaptive policies;
   - define what counts as productive use for each workload class.
4. **ARGUE**
   - compare conservative fixed gates with adaptive policies using historical
     replay;
   - make starvation risk, expired allowance, and outcome yield explicit.
5. **VERIFY**
   - test provider parsing, time-window boundaries, reset handling, missing-data
     failure, attribution, and deterministic policy replay.
6. **INSTANTIATE**
   - add the shared ledger and all-agent usage burndown to Futon0;
   - run candidate policies in shadow mode before separately authorising any
     scheduling integration.

## 8. IDENTIFY Exit Criteria

- [ ] Claude, Codex, and Zai usage sources have documented accuracy and reset
      semantics.
- [ ] At least one complete reset window per provider is captured durably.
- [ ] The observation schema represents all native windows without lossy
      special cases.
- [ ] The all-agent burndown renders recorded history and reset boundaries.
- [ ] Foreground versus unattended usage attribution limits are documented.
- [ ] The Zai 13–18 July example can be replayed from durable observations.
- [ ] At least two candidate policies—including the current fixed 50% control—
      produce explainable decisions in replay.
- [ ] Joe explicitly authorises any transition from shadow recommendations to
      automatic adaptive scheduling.

## 9. Open Questions

- What reserve should be protected for interactive work, and should it differ
  by provider, day, or active mission?
- Are provider reset times fixed, rolling, or revised after consumption?
- How should usage be attributed when several local agents share one provider
  account and invocations overlap?
- What outcome measures are meaningful outside formal proofs?
- Should policy optimise expected useful outcomes, avoided expiry, option value,
  or a multi-objective combination?
- How much forecast error is acceptable before the system falls back to the
  conservative fixed gate?

