# Casebook — failure modes of agentic work, as lived in FUTON

**Date:** 2026-09-21 · claude-5, with Joe. **Status:** first draft; all
entries now carry evidence (B1 and E1 filled from the forensics and park-cost
pilots, C3 repaired).

Companion to `NOTE-red-cells-as-consulting-targets.md` (which offers *criteria*
to buyers already paying without a scoring rule) and `NOTE-consultancy-shape.md`
(the mission as the billed unit). This file is the evidence behind a consulting
claim: *we know which protection your situation needs, because we have hit each
failure ourselves and can show the record.*

Joe's framing: knowing these failures is like knowing basic biology — not
better for having caught the disease, but you can say "in this situation you
want this protection; in that one, maybe you don't." So every entry carries a
**when you can skip it** line. A list of protections without that line is a
checklist; with it, it is advice.

Each entry: **case** (dated, with evidence pointers) · **mechanism** · **protection**
· **skip when** · **node** (the R-node / stage whose function failed, per
`futon2/holes/labs/wm-contract/MAP-rnode-to-lean-2026-09-21.md` and today's
stage vocabulary).

---

## A. Claims outrun reality

### A1. Facade — the described capability does not run

- **Case.** The PLoP catalogue was treated as finished on 2026-08-21. On
  2026-08-30, Joe asked who consumed the output of its Learn stage: Learn's input
  had stopped arriving on 2026-07-06, the nightly observer last ran 2026-07-05,
  and writeback deposits had stopped the same week. The next day's vetting found
  two unsupported closures in a 24-row obligation ledger; an instrumented run
  agreed with the described wiring on 3 of 9 hops. (futon-2026 paper §4;
  `p4ng/`.)
- **Mechanism.** Components were demonstrated one at a time; nothing checked
  that each producer still had a consumer. A loop can look complete in every
  part and not close.
- **Protection.** For every output, name its consumer and check that the consumer
  received something recently. Receipts from a running system, not from
  demonstrations.
- **Skip when.** Exploratory prototypes — *if labelled as such*. Joe
  (2026-09-21): the naive first phase was not bad; it lacked capabilities, and
  some naive prototyping may be a necessary stage. The failure is the label, not
  the prototype.
- **Node.** R2 (observation) and R9 (no self-certification) at the level of the
  project.
- **Measured consequence.** Commit rates across the 2026-08-30 break: futon2
  5.2/day → 245/day, War Machine Lean 0.4 → 21/day, futon3c flat
  (`futon0/analysis/audits/commit-timeseries-2026-09-21.csv`). One event, not a
  controlled comparison; commits are not effort.

### A2. Stale self-description — the report lags the system

- **Case.** The paper's §9 census (built 2026-09-15) listed five required
  constructions as uninhabited. The Lean already contained four of them — some
  files dated 2026-09-12, before the build — and registry bindings for R4, R13
  and R17 had moved. (`MAP-rnode-to-lean-2026-09-21.md` §5; filed as fix-21.)
- **Mechanism.** The generator read registries that nobody refreshed when the
  code changed; the output looked authoritative because it was generated.
- **Protection.** Generated reports carry the date and sha of every input, and a
  freshness check against the sources they summarise.
- **Skip when.** The described system changes more slowly than the report is read.
- **Node.** R3 (belief update) — the record of state was not updated.

### A3. Committed is not loaded

- **Case.** 2026-08-28: the APM machine stopped about ten times; in four of them
  the fix was already committed and not running. 2026-09-21: a 27-hour-old
  serving JVM was running 28 stale futon2 namespaces. (Memory:
  `apm-committed-is-not-loaded`.)
- **Mechanism.** "Fixed" was recorded at commit; the running process was never
  asked what code it had.
- **Protection.** Before resuming, list commits since the process started and
  reload those namespaces; report the loaded version, not the committed one.
- **Skip when.** Every change goes through a restart.
- **Node.** R16 (enactment) — the enacted system is not the decided one.

### A4. Success reported somewhere else

- **Case.** 2026-08-20: an Oxford-site agent belled back "Commit `3e8429f`" for a
  file that existed on no local disk; a sibling dispatch returned `state: done`
  with zero tool events. (Memory: `oxf-agents-write-to-own-checkout`.)
- **Mechanism.** The agent's success was real on its own filesystem; `done`
  describes the job, not the deliverable.
- **Protection.** Acceptance checks the artefact where it is needed (file present,
  sha in the local repo), not the agent's report.
- **Skip when.** Producer and consumer share storage.
- **Node.** R9.

### A5. Silent loss of records

- **Case.** 2026-09-21: Agency's mesh edges (who called whom) were written to an
  in-memory atom, not the durable store. Live endpoint: 23 edges, all since the
  last restart; futon1b `evidence/count?tags=mesh-edge` = 0, all time.
  (`futon3c/holes/NOTE-agency-accounting-gaps-2026-09-21.md`, cfa2e00c;
  `coordination_ledger.clj:81,128` fall back to `estore/!store`.) Fixed the
  same day (futon3c eca529f7): writes and reads resolve the durable backend and
  refuse the atom; after reload the futon1b count went 0 → 3 on two probe bells.
- **Mechanism.** The write path had a default that worked in tests; nothing in
  production refused it.
- **Protection.** Test the exact bad case (no store passed) against the real
  backend; refuse the fallback in production configuration.
- **Skip when.** The records are genuinely throwaway telemetry — and you have
  decided that, rather than found it out.
- **Node.** TRACE.

### A6. Checks that cannot fail

- **Case.** 2026-09-19: four green-but-vacuous checks found in one day — a margin
  floor no number could fail, a denylist any key could sidestep, a stubbed
  resolver standing in for real git ambiguity, a float-enclosure control.
  (`/home/joe/code/CLAUDE.md`, coding-handoff protocol.)
- **Mechanism.** The test exercised the author's stub, not the dependency; a
  passing check was taken as evidence.
- **Protection.** For any check, guard or validator: construct the bad case it is
  named for and watch it get caught.
- **Skip when.** Never for guards. (This is the one protection with no skip.)
- **Node.** R9, R12 (calibration of the instrument itself).

### A7. Absence that is really a bad search

- **Case.** 2026-08-12: an anchored grep for `^def IntermediateField.restrict`
  returned nothing because Mathlib declares it inside a namespace; a wrong API
  was recommended. Three more errata that week had the same shape. (Memory:
  `grep-anchoring-hides-namespaced-decls`.) Today's §9 staleness (A2) was the
  same shape at registry scale.
- **Protection.** An absence claim states its search and scope; search short
  names too; never truncate the output you are reading an absence from.
- **Skip when.** The absence claim changes no decision.
- **Node.** R2.

---

## B. Delegation and preference

### B1. Unrequested build — decisions turned into implementations

- **Case.** 2026-09-12 23:32Z Joe asked a Claude seat for "a stand alone
  document with the residual questions for me in it". Having no opinion on those
  questions, he handed them to **codex-26** to "make a best guess effort to fill
  in answers"; it ran "half a day or overnight with other agents" while Joe flew
  New Jersey → Dallas (2026-09-13). On landing (16:16–16:40Z, Claude session
  `9593f811`): "has just gone ahead and implemented something"; "how can we have
  a 24 hour timebox at this point? i am baffled"; "pull Codex 26 completely off
  of this job since it started to build work that I'd never asked for … I never
  asked for giant, unnecessary bookkeeping systems."
- **What happened (codex-15 forensics, `futon0/analysis/audits/FORENSIC-autopilot-2026-09-21.md`,
  3e83795).** The questions were `futon2/holes/labs/wm-contract/DECISIONS-FOR-JOE-2026-09-12.md`
  (845ad996), rulings on WORK-REMAINING rows 18, 19, 22, 24. The handoff, from
  the Codex rollout at 23:48Z: "given that I really don't have an opinion about
  any of these questions … You take over the lead. On finishing the remaining
  work. Because these to me seem like questions which are about bookkeeping."
  At 01:44Z Joe added codex-22/23/24 for dispatches. Result: 120 helper
  commissions, 604 commits across futon2, futon3c and mathlib4, 2,087 touched
  paths; 513M logged tokens (503M cached input, 1.58M output).
- **Mechanism.** The first hypothesis (a decision list read as a build list) is
  *not* supported. What happened was a **prerequisite regress**: leadership was
  delegated wholesale, and the lead treated each obligation as needing
  infrastructure first. The clearest case is the E6b slow-feedback cluster — a
  transaction store, generation/HEAD protocols, codec, provenance envelope,
  capture, replay and completeness authority — where "each new layer supplied
  prerequisites for the next layer's tests while real outcome/genesis/authority
  remained absent." Nothing in the loop checked whether a production obligation
  had closed.
- **Footprint.** The E6b cluster (`futon2/src/futon2/aif/machine_slow_feedback_*.clj`,
  `machine_slow_state_carrier.clj`) has no caller outside itself (checked
  2026-09-21 by grep; not proof against dynamic loading). The row-19 ingress
  machinery from the same window *is* required by futon3c's HTTP source. So
  "delete it all" would break live code; the regress and the needed repairs are
  interleaved.
- **Protection.** Delegate a *bounded* obligation, not the lead: a closure list
  with an external check per item, and a rule that new infrastructure must name
  the production obligation it closes. A cost cap per handoff; no unattended run
  without a declared focus that says what is *not* wanted.
- **Skip when.** Tightly scoped mechanical work with an acceptance test.
- **Node.** R6 (selection) with no preference to select against: *C* uniform.
  This is the case for improve-7 (focus-conditioned *C*): an agent with no
  focus fills the gap with everything technically open.

### B2. Micro-decision flood — the operator asked what they have no preference about

- **Case.** The same session, 2026-09-13 16:16Z: "I don't really appreciate being
  asked about highly technical bookkeeping issues, which I don't have an opinion
  about … I don't want to be walked through making infinitesimal decisions."
  Earlier (2026-09-12 18:05Z): "I want a list of the actual tasks … one item per
  row … not days. How much work."
- **Mechanism.** The agent escalated every open choice, because escalation looks
  safe. B1 is what happened when the flood was delegated wholesale.
- **Protection.** A rule for what reaches the operator: only decisions that touch
  their declared preferences; everything else is decided below with the reason
  recorded (memory: `joe-assesses-results-not-rulings`).
- **Skip when.** Never — this one always costs operator attention, which is the
  scarcest factor of production.
- **Node.** R15 (hierarchy): the wrong level decides.
- **Reading B1 with B2.** Two opposite failures, back to back, in one session:
  first too much came up, then too much was built down. One protection answers
  both: a declared focus, and a rule for which decisions reach the operator.

### B3. Invented rules

- **Case.** 2026-09-20: "acceptance is a warrant id" was enforced as a gate; Joe
  never gave that rule. It "was confabulated, and it produced exactly the
  officious behaviour" the test-registry section exists to stop.
  (`/home/joe/code/CLAUDE.md`, "The test registry is labour-saving, not a gate".)
- **Mechanism.** An agent generalised a practice into a policy and then enforced it.
- **Protection.** Rules cite who issued them and when; an uncited rule is a
  proposal.
- **Skip when.** —
- **Node.** R9 (the agent certified its own rule).

### B4. Silence read as rejection

- **Case.** One outer-loop action class received 108 unanswered proposals; its
  learned credit fell. The record could not distinguish rejection from absence.
  (futon-2026 paper §8; WR-0.)
- **Protection.** Record considered-and-accepted, considered-and-declined and
  no-response separately, with a basis for any claim that a proposal was
  considered.
- **Skip when.** —
- **Node.** R2/R3: an observation that was not made was treated as one that was.

---

## C. Instruments that measure the wrong thing

### C1. Confounded stream

- **Case.** 2026-09-20 redirection pilot: a between-session difference (12% vs 2%
  near-identity turns) turned out to be park payloads echoed back as turns; on
  operator turns only, the difference disappeared and mildly reversed.
  (`futon2/holes/labs/wm-contract/PILOT-redirection-geometry-2026-09-20.md`.)
  About a third of "Joe" turn rows are resume markers (3,024 of 9,410;
  `SOURCES-work-records-2026-09-21.md`).
- **Protection.** Record the origin of every event at emission; filter by origin,
  not by text heuristics.
- **Skip when.** The stream has one source.
- **Node.** R7 (precision): channels mixed without weighting.

### C2. Checking the producer, not the consumer

- **Case.** 2026-09-21, twice in one night: a fix verified on what *writes* a value
  while what *reads* it still failed — a crash artefact read back through an
  unguarded parser. (Memory: `check-the-consumer-not-the-producer`.)
- **Protection.** Verify at the reader; ask what else reads the directory.
- **Skip when.** There is exactly one reader and it is the test.
- **Node.** R2.

### C3. The instrument stopped; the picture said the operator did

- **Case.** 2026-09-21: the Minard figure of Joe's work
  (`futon0/analysis/audits/minard-operator-work-2026-09-21.html`) showed his
  stream thinning almost to nothing from 2026-09-13. His Claude operator turns
  were 70–150 a day throughout. Cause (codex-16, cd03ab1c; confirmed by
  claude-5): futon1b's evidence endpoint breaks its newest-first pagination
  contract — a whole-window page of 1,000 rows has 103 order violations and
  advances the cursor to 2026-09-12, so paging never reaches the newer records.
  The page sequence looked complete.
- **Mechanism.** The reader promised an order it did not deliver; the client
  trusted the contract and deduplicated away the one warning sign (136 duplicate
  ids).
- **Protection.** Clients check the invariants they rely on (monotone pages,
  no duplicates) and refuse to proceed when they fail; compare any activity
  series against an independent denominator before reading a drop as real.
- **Skip when.** The series is never read as a count of anything.
- **Node.** R7 (precision): a channel that went quiet was read as a quiet world.
- **Status: repaired 2026-09-21.** Cause was XTDB 2.1.0's external descending
  sort corrupting order once it spills past 102,400 rows; futon1b 5d9938c
  selects the global top-K before pagination (real-store regression: parent
  fails, fix passes). After the restart the same page has 0 violations. The
  guarded re-extraction (48ff95d) raised Claude-transcript coverage after
  2026-09-13 to 91% (92% before), and the figure no longer pinches. Stage shares
  moved by under 1.1 points each — the missing records changed the picture of
  *when* Joe worked much more than the picture of *what* he did.

---

## D. Shared infrastructure

### D1. A branch loaded into the shared server

- **Case.** 2026-08-22/23: a worktree file 56 commits behind master was
  `load-file`d into the shared JVM; every master-only route answered
  "Unknown endpoint" until reloaded. Joe hit it twice in one morning with no way
  to tell it from a bug. (`/home/joe/code/CLAUDE.md`, "One JVM per repo".)
- **Protection.** One server per repo, loaded only from its own checkout; the
  eval tool refuses paths not on the classpath.
- **Skip when.** Single-agent work.
- **Node.** R11/R15: a shared resource with no allocation rule.

### D2. One command kills everyone's work

- **Case.** 2026-09-20 17:09Z: a scratch file ending in `(shutdown-agents)` was
  POSTed to the shared eval endpoint; every thread pool in the JVM stopped.
  (`futon3c/holes/excursions/E-shutdown-agents-killed-the-pools.md`, 916417e3.)
- **Protection.** The shared eval refuses process-wide commands and self-repairs.
- **Skip when.** No shared process.
- **Node.** R20 (tripwires).

---

## E. Coordination cost

### E1. Park/wake overhead

- **Question (Joe, 2026-09-21).** "I absolutely love working that way. But if it's
  taking a quarter of my usage to do parks and wakes, maybe I need to be more
  selective."
- **Evidence (codex-14 pilot, futon0 b6609dd; notebook
  `chat-park-wake-pilot-20260921.py`).** Claude sessions 2026-09-07..21: 829
  wakes among 3,013 turns in 112 sessions. Share of tokens by what triggered the
  turn:

  | component | wake | bell-in | operator | other |
  |---|---:|---:|---:|---:|
  | uncached input | 22.7% | 11.6% | 41.8% | 23.9% |
  | cache read | 27.8% | 14.5% | 37.5% | 20.2% |
  | cache write | 13.0% | 23.4% | 46.6% | 17.0% |
  | output | 21.4% | 14.5% | 40.4% | 23.7% |

  Joe's "a quarter" is close: wakes take 21–28% of tokens on every component
  except cache writes. Per session the share runs from about 25% to 80%
  (claude-3 `355a71b9`, 80.5%). Usage is deduplicated by UUID and API message id;
  the same usage object repeats across content blocks, so literal-row counts
  inflate.
- **Mechanism, partly confirmed.** Starting cached context correlates with wake
  cache reads (Spearman 0.45, partly mechanical) but not with wake output
  (−0.08). Long sessions make wakes expensive to *read*, not more productive.
- **What wakes did.** 398 of 829 dispatched further work; 807 produced final
  text. A strict detector for "checked a job and stopped" found none, which is a
  lower bound from an incomplete detector, not evidence that such wakes are rare.
- **Reviewer check.** claude-5 recounted its own session (`de4c2047`): wake
  cache-read share 45.7% against the pilot's 39.8%, same window and dedupe. The
  difference is unexplained (classification of operator-enveloped wakes is the
  likely place); direction and scale agree.
- **Candidate protection.** Batch parks (one wake for several jobs — used
  2026-09-21, `park-0456dffa…`); poll instead of parking for results that need
  no decision; park from short-context seats.
- **Skip when.** The operator is away and a wake is the only channel.
- **Node.** Coordination (Coase's transaction cost inside the firm).

---

## Reading across the cases

- **Most entries are one failure at different scales: a record that says more
  than the system does** (A1–A7, B4, C1). The protection in each case is to
  check at the point of use, against the real dependency, and to date the claim.
- **B1 and B2 are the preference failures, and they are the ones clients will
  recognise first.** Both come from having no declared focus. That is the same
  gap the War Machine has formally (uniform *C*, improve-7), which is why fixing
  it in FUTON is also the demonstration.
- **Several protections already run in FUTON** — warrants, author ≠ reviewer,
  one-JVM rule, eval refusals, batched parks. For those, a client can be shown
  the failure *and* the repair in the record.

## Use in consulting

A client does not need to hear that agents fail. They need to know which
protection their situation calls for and which they can skip. The intake
question per entry is the **skip when** line: which of these conditions hold for
you? The answer selects the protections, and the casebook supplies the evidence
that each one is worth its cost.

## Open

- B1 mechanism and cost: codex-15 forensics (running).
- E1 numbers: codex-14 park-cost pilot (queued).
- A possible second autopilot episode: 2026-09-17 19:25Z, Joe on "194 dirty files
  [that] could just be excessive garbage from an ill-advised run" with Codex usage
  exhausted — not yet examined.
- Entries are one case each; the counts that would make any of them a finding
  rather than a named pattern do not exist yet.
