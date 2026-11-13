# The zai learning loop — how to reproduce it (inner + outer)

*From M-zai-learning-loop, built and proven live 2026-07-25→27. This note is
the portable recipe; the concrete artifacts live in
`futon3c/holes/missions/M-zai-learning-loop.md` (charter, incl. §Second
derivative), `futon3c/holes/labs/M-zai-learning-loop/` (preregs, results,
drafts, ops log), and `futon3c/scripts/dispatch_with_recall.clj` +
`wire_math_memory_patterns.clj`.*

## What it is

Two nested loops that make a runner-agent measurably better at a domain by
accumulating **typed, reviewed, pattern-wired memories** mined from its own
sessions — and then make the *learning process itself* improve from its own
records.

**Inner loop:** work → mine → review → store → recall → receipts → (better work)
**Outer loop:** the loop's own operation → typed self-records → meters + proposals → (better loop)

Verified end-to-end: the first witnessed chain was a memory mined from failed
sessions (Young's inequality absent from Mathlib), recalled into a revisit of
the same problem, cited by the runner as redirecting its effort, producing a
proven lemma and a sorry-count drop (cohort-1 revisit arm, receipts
`e-dfea2de9…`/`e-0436fa62…`, commit `f614856`).

## Preconditions (what you need before step 1)

1. **A mechanical witness for the domain.** Here: Lean/Mathlib via
   `lake env lean` (exit code, sorry count, error count). The loop never
   trusts self-report; every row is graded by the witness. *Without this,
   stop — see "Porting" below.*
2. **A typed evidence store** with: memory records (name/body/subjects),
   pattern entities, attachment edges (memory→pattern), review status on
   edges, and full-text search over evidence bodies. Here: futon1b store
   on :7073 (EDN API + `x-penholder` header for writes).
3. **Separated seats** (author ≠ reviewer ≠ runner):
   - *runner* (zai seat): does the domain work; fresh session per task; may
     read patterns but writes no memories.
   - *scribe* (codex seat): mines sessions into memory drafts; sole holder
     of `memory_record`; domain-stamped (see `resources/memory-seats.edn`).
   - *ground control* (Claude): dispatches, verifies, wires, reports.
     Fixes review findings directly; never rubber-stamps.
   - *operator* (human): review/promotion policy, casting, scope.
4. **Session evidence**: runner self-talk lands as turn-round evidence in
   the store automatically (the zaif profile), so the scribe mines the
   store, not screen-scrapes.

## Inner loop, step by step

### 0. Baseline (once per domain)
Preregister a batch of problems BEFORE running: fixed order, no retries,
every outcome in the denominator, typed outcome taxonomy (solved / partial
with sorry-count / failed-by-mechanism / substrate-confounded). Run it
against the empty store. This is your capability floor and your
memorization-contrast anchor. *Footgun: distinguish failure mechanisms from
the job ledger (tool-round exhaustion ≠ self-termination ≠ quota kill) —
they want different memories.*

Optional but strongly recommended: a **held-out problem set** (we used
Berkeley Problems) that is NEVER mined into memories — evaluation stays
uncontaminated. Meta-learning from held-out *failure shapes* is allowed;
mathematical content is not.

### 1. Work
Dispatch one problem per runner session via `dispatch_with_recall`
(full packet: repo, files to read, validation command, pacing note —
"edit early, commit an honest compiling partial by ~20 min", summary
contract incl. a **Memory usage** section). Park on every dispatch.

### 2. Ground truth
At session end, ground control re-runs the witness itself, checks the
commit exists, checks statement integrity (diff the theorem statement; the
runner proving a weakened statement is the one silent corruption), reverts
non-compiling leftovers. Never trust the final summary; job state `done`
with no commit happens.

### 3. Mine (scribe pass per session)
Bell the scribe with session id + commit + named targets. Four lanes:
- **solve-lane**: final summaries → lemma-location + proof-shape (strategy)
  memories. Near-mechanical, highest precision.
- **arc-lane**: error→fix spans → scoped tactic rewrite rules
  (scope / before / after / level / confidence / evidence-ids).
- **trajectory-lane**: expensive or failed stretches → process + cost
  memories ("this API region is absent; stop searching, promote the
  dependency to its own construction target"). Highest novelty — mines
  what error→fix schemes can't see.
- **challenge-lane**: runner corrections of corpus/prior claims → challenge
  records with machine witness (we caught a wrong informal proof this way).
Drafts are EDN with evidence traceability (turn-round ids, commits,
problems); instance accumulation updates confidence in place (n=1→n=2),
and the scribe may *refuse* false merges (absent-theorem ≠
prerequisites-unmet — keep the taxonomy sharp).

### 4. Review → promote
Two policies, both proven:
- *Operator review first* (bootstrap): human verdicts on drafts; amendments
  recorded in the draft (`:review` map) and carried into the store.
- *Proof-in-pudding* (steady state): promote with warrant
  `operator-delegated: in-use evaluation` — receipts and challenges judge
  quality post-hoc. Use once receipts are flowing.
Promotion = scribe records each draft (its authorship), then the wiring
script attaches memories to **trigger-class patterns** and marks edges
reviewed. *Recall requires this*: pattern-mediated recall only surfaces
memories reachable through a reviewed edge to a pattern endpoint in the
domain. Content without wiring is invisible (we proved this the hard way).

### 5. Recall at dispatch
`dispatch_with_recall --problem X --to runner < packet`:
- query terms from the problem's own files (not packet boilerplate), with a
  stopword list tuned by observed misses;
- pattern descriptions as append-only FTS bridge rows (lexical *proposal*,
  never independent warrant — the reviewed edge remains the warrant);
- bounded timeout, graceful degradation: recall failure NEVER blocks
  dispatch; `recall-empty` is a typed row condition;
- surfaced memories prepended as "POTENTIALLY RELEVANT MEMORIES … use your
  judgment" with ids; packet asks the runner to name used/ignored + why.

### 6. Receipts (the loop's sensory nerve)
- **Offered half** written at dispatch (surfaced-ids, query, term sources,
  recall-system version). *Footguns we hit, all three in one function:*
  EDN entry shape (namespaced keys) not JSON; `x-penholder` header; target
  the SUBSTRATE base, not the agency base.
- **Outcome half** written at verification (used-ids from the runner's own
  citation, outcome, witness refs). Honest non-use with reasons is a valid
  and valuable outcome.

### 7. Measure (four observables, reported per cohort)
1. *Leaf*: witnessed use events (memory cited + behavior changed + outcome).
2. *Pattern*: right-neighborhood surfacing (level match matters: strategy
   memories surfacing in series terrain ≠ the lemma you needed).
3. *Field*: solve/partial/fail rates vs baseline; honest-slope reporting.
4. *Structure-formation*: n≥2 merges, taxonomy splits, challenge records.
Record per-row: recall-system version (v0-lexical / v1-enriched /
v1.1-receipt-ranked / …) so infrastructure changes mid-cohort stay
comparable — amend the prereg BEFORE outcomes when the design changes.

## Outer loop (the second derivative)

The meta-level insight: the loop's own operation must become a corpus with
the same machinery. Its mechanical witness is **the system state itself**
(store, job ledger, git, sorry counts) — what's missing is only a claim
language and a stereotyped register. Four mechanisms:

1. **Close Ψ — receipts feed retrieval (S6).** Per-memory use/offer
   statistics as a bounded multiplicative ranking boost
   `(1 + α·used/offered)`, α≈0.5, cold-start neutral, never-blocking,
   audited in the receipt. Every use event now bends future recall.
   Reasoned non-use mildly demotes — that is correct, not a bug.
2. **Typed self-talk register (points-de-fuite markers).** Ground control's
   loop narration is constrained to marker-fronted statements
   (`⊸fix` defect→fix · `⊸miss` surfacing/coverage miss · `⊸win` mechanism
   worked · `⊸prop` proposed loop change · `⊸meter` measurement), kept in
   an ops log per cohort. Declare-at-creation makes the meta-corpus
   deterministically parseable — a grammar, not a model — so the scribe
   meta-lane costs ~nothing. The register is a *peripheral*: the open
   experiment is whether the constraint is Lean-like (enabling) or a
   capability tax; count violations honestly either way.
3. **CLean / Basic System Virtual.** Type the meta-claims so scripts check
   them against system state ("receipt with job-id X carries surfaced-ids
   Y", "sorry count fell", "pattern attached"). Every wake-protocol prose
   check is an informal instance begging to be typed. Vibes survive only
   in quality judgments — and pudding evaluation covers those.
4. **Curriculum from coverage + memory-proposed work.** The coverage map
   (which terrains have zero memories) plus memory next-actions (e.g.
   "promote the missing dependency to its own construction target")
   select the next cohort's problems — preregistered as a labeled design
   change. The loop assigning itself work is niche construction; the
   payoff row (blocked problem + newly constructed lemma) is its test.

**Meta-meters** (cohort-over-cohort trend = is learning-to-learn working):
extraction yield/session; surfacing precision (used/offered) and post-hoc
misses; time-to-first-reuse; coverage growth; register-violation count;
challenge/demotion rate under pudding policy.

## Operational discipline that made it work (do not skip)

- Park on every dispatch; wake payloads are checklists, not reminders.
- Ground-truth-first at every wake; "announced is not sent."
- Never kill running jobs on a stale diagnosis; re-check before kill.
- Typed row outcomes, no retries within a cohort; misdispatches (ground
  control's own errors) are typed as such and re-dispatched — that is not
  a retry.
- Fix review findings yourself; re-bell only substantial new work.
- Every store write verified by independent read-back.
- All of it preregistered, amendments dated before outcomes.

## Porting beyond mathematics

The loop never depended on the content being mathematics — it depends on
(a) a compile-shaped ground truth and (b) workflows theorisable as pattern
cascades. That is the demarcation criterion for War-Machine-ability: where
the cascade can't be written, the residue is a missing pattern, which is
exactly what human pattern-supply HITL provides (per-class, amortized —
measurable with the same receipts). Port order: WM operational corpus
first (attempt records, repair arcs, typed failures already exist), then
programming domains with test-suite witnesses.
