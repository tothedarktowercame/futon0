# Deep dive B: Mastra, against the War Machine and the FUTON stack

Mission: `M-landscape-positioning.md` §6 (row B). Written 2026-10-11 by claude-13.
Read-only work: no install, build or run of Mastra; WM files read, none changed.
The batch-A entry (`batch-A.md`, "Mastra") already covers the problem statement, licence
and verb list, and none of it is repeated here.

Clone: `~/code/external/mastra` @ `ebe143c4` (2026-10-11), `@mastra/core` 1.76.0-alpha.8,
`@mastra/memory` 1.37.0-alpha.2. Mastra paths below are relative to that clone. FUTON
paths are absolute or repo-relative (`futon2/…`, `futon3c/…`).

**R-numbering caveat (read first).** The brief's labels (R10 dispatch, R16 park, R19 C)
are the informal glosses from §3.1 of the mission. The canonical names are in
`p4ng/sec-catalog.tex` (pattern table, ~l.91–140): R9 No Self-Certification, R10
Scheduled Observer Entrypoint, R11 Hierarchical Budget-Aware Action Selection, R12
Two-Layer Calibration, R16 Grounded Actuation (not re-observation), R20 Interoceptive
Tripwires. R19 is not a catalogue number. The paper says C "is the model's C vector rather
than an R-numbered pattern", and `sec-issue-board-generated.tex:158` records "R18 and R19
have no canonical assignment". Separately, the `r9-*`/`r10-*`/`r11-*` box prefixes in
`futon3c/holes/labs/M-wm-wiring/wm-adjacency.edn` are **wiring-inventory row numbers**
(`futon3c/holes/missions/M-wm-wiring.md:46-57`; row 9 = selection law, row 10 = publish,
row 11 = warrants), not catalogue R-numbers. This document uses catalogue names.

---

## 1. What Mastra is, mechanically

A TypeScript monorepo (Apache-2.0 apart from `ee/` directories) that gives a Node
developer seven things:
- an **agent loop** that calls a model, runs tools and repeats until a stop condition holds;
- a **workflow builder** that compiles chained method calls into a step list;
- three **execution engines** that run that list (in-process, pub/sub-evented, Inngest),
  plus a Temporal adapter;
- **storage domains** for threads, messages, snapshots, scores, spans and more, with
  adapters for ~30 databases;
- a **harness layer**, a set of storage-backed controls around long runs (durable runs,
  signals, goals, schedules, background tasks);
- **eval scorers**;
- **span tracing** with ~13 exporters.

The agent loop is itself a workflow (`packages/core/src/loop/loop-builder.ts`):

    agentic-loop = dowhile(executionWorkflow, continuationPredicate)
    executionWorkflow = llmExecution → map tool calls → foreach(toolCall) → llmMapping
                        → backgroundTaskCheck → signalDrain → isTaskComplete → goal

So everything (approvals, signals, goals, completion checks) is a step in that one loop,
and every suspension is a workflow snapshot.

## 2. The problems it solves, in plain verbs

The subject is the developer-operator. Mastra is the instrument. Each item names what is
stored, retrieved or injected.

1. **delegate · a multi-step task to a model.** Mastra sends the prompt and the tool
   schemas to a provider, executes the tool calls that come back, appends the results,
   and repeats. It stops after `maxSteps` (default 5) or at a `stopWhen` condition.
2. **constrain · the order of steps.** Mastra compiles `.then/.branch/.parallel/.dountil/
   .foreach/.map/.sleep` into a `StepFlowEntry[]` and runs it with schema-checked inputs.
3. **approve · a tool call before it runs.** Mastra suspends the run, writes a snapshot
   row, emits a `tool-call-approval` chunk, and waits. `resume({approved})` reads the
   snapshot back and continues. Approval is accepted only through the resume call, not
   from anything the model writes.
4. **redirect · a run in progress.** Mastra writes a `role:'signal'` message into the
   thread. At the next loop boundary it inserts that message into the request. If the
   current request has produced only reasoning so far, it cancels that request first.
5. **defer / continue · work across restarts.** Mastra writes run snapshots and
   per-run ownership claims. After a crash it re-drives runs left in `running` from the
   last snapshot.
6. **collect · conversation state for later turns.** This is the feature Mastra calls
   "memory". In mechanical terms:
   - it stores messages and injects the last N;
   - it embeds the messages, stores the vectors, and injects the top-k matches;
   - it keeps one text or JSON blob per user or thread, which the model rewrites through
     a tool call and which is injected as a system message;
   - optionally, a second model rewrites old messages into dated bullet "observations"
     that replace the raw history in the prompt.
7. **verify · an output.** Mastra runs scorer pipelines (a function, or an LLM-judge
   prompt) and stores the score, the reason and each intermediate step. It can gate CI on
   the result, and can gate the loop itself (`isTaskComplete`, goals).
8. **explore · what runs did.** Mastra records a span tree per run (model, tool, workflow
   step, processor, memory op) and exports it to OTel, Langfuse, Braintrust and others.
9. **constrain · spend.** Mastra caps steps, retries, timeouts and judge runs, and offers
   one processor (`TokenCostControl`) that sums recorded cost over a time window and
   aborts the call when the sum passes a cap.

Vendor words to translate when reading the docs (T8 layer 3): "humanlike long-term
memory" (`docs/…/memory/observational-memory.mdx`), "Observer"/"Reflector" (two
summarising model calls), "subconscious" agents (`curate`, `remind`), "learning" (the EE
trace-clustering service, §3 row 18), "self-evaluate" (a separate grader call,
`docs/…/subagents.mdx` §Rubric scorer).

## 3. Concept map

Key to the "relation" column. **same** means the same problem with a comparable
mechanism. **different** means the same problem with a different mechanism (§4
explains). **none** means no counterpart. The FUTON column says whether the
counterpart is *code* (and whether it is live), *Lean* or *prose*.

| # | Mastra primitive (path) | WM / FUTON counterpart (path; status) | relation |
|---|---|---|---|
| 1 | Workflow graph: `createWorkflow`, `StepFlowEntry[]` (`packages/core/src/workflows/workflow.ts`, `types.ts:703-804`). The graph *is* the program. | WM wiring diagram: `futon3c/holes/labs/M-wm-wiring/wm-flight-wiring.edn` (code sites with read/write fields), derived `wm-adjacency.edn` (149 boxes, 197 wires, 6 isolated boxes); checked against the equation DAG `futon2/holes/labs/wm-contract/aif-equations.edn` via `wm-vs-equation-dag.edn`. Code-generated record; the diagram *describes* code, it does not run it. | different |
| 2 | Static check of stored/LLM-built workflows: `validateDynamicWorkflow` returns coded issues (`missing-reference`, `incompatible-schema`, …) with repair actions (`workflows/dynamic/validate/`). Code-built graphs are checked only by TS types. | `spike/wm_coverage.bb`, `wm-wire-coverage.edn`; `:holes` in `aif-equations.edn` (e.g. `[:R4 :R7] :likelihood-precision :implemented-awaiting-live-witness`). Code. | different (Mastra checks types flow; WM checks theory-forced edges exist) |
| 3 | Suspend/resume: `suspend(payload)`, `resumeSchema`, atomic `#claimResume` (`WORKFLOW_RESUME_ALREADY_CLAIMED`), snapshot `WorkflowRunState` (`types.ts:405`) in `mastra_workflow_snapshot`. | Agency park/wake: `futon3c/src/futon3c/agency/parked_on.clj` (`park!` :423, `sweep-deadlines!` :559; EDN on disk), `POST /api/alpha/park` (`transport/http.clj` :5994); R16 "parked" cell in `futon2/…/full_loop_runner.clj:141-182`. Code, live. | same problem; Mastra typed, Agency has deadlines |
| 4 | Signals: `sendMessage` / `queueMessage` / `sendSignal` / `sendStateSignal` (`packages/core/src/signals/`, `docs/…/harness/signals.mdx`) | Bells (async, 202 + job id) and whistles (blocking): `futon3c/scripts/agency_send.py`, `agency/bell_router.clj`. Code, live. | same |
| 5 | Signal providers (GitHub PR, webhook, task: `signals/github`) | None as a general mechanism; ad hoc pollers (zone-health, Matrix bridge). | none |
| 6 | Durable agents: snapshot policy, run claims renewed every 10 s, lapse at 30 s, store-level **write fencing** (pg, libsql, in-memory), `recoverAllDurableAgents()` (`docs/…/harness/durable-agents.mdx` §Run ownership) | Agency restart is manual and Joe's (memory: "Agency restart is Joe's"); WM debugger attach state lost on restart (memory: "WM restart debugger"); park without session id strands the wake. Partial code. | Mastra ahead |
| 7 | Supervisor / network routing: a routing *agent* gets a prose list of primitives and returns `{primitiveId, primitiveType, prompt, selectionReason}` as structured output (`loop/network/index.ts:169-302`); `maxIterations` default 1. | Policy selection: `futon2/src/futon2/aif/cascade_selection.clj` posterior ∝ habit·exp(−F − G/β); G = KL risk against C + ambiguity (`cascade_model_manifest.clj` :600, :612, :632); called from `policy.clj` (`select-action-cascades`). Seat dispatch box: `full_loop_runner.clj` `dispatch!`. Code; selection runs on recorded ticks, R10 live loop disabled since 2026-07-06 (`futon2/holes/aif-r1-r16-pattern-map.md`). | different |
| 8 | Completion / goal judge: `isTaskComplete`, network `completion.scorers`, `goal` with `judge` model and `maxRuns` (`loop/shared/steps/goal-core.ts`, `docs/…/harness/goals.mdx`). The judge may be the same model; read-only `goal.tools` optional. | R9 No Self-Certification: `futon2/src/futon2/aif/r9_checker.clj` ("not integrated at any admission boundary"), `futon3c/src/futon3c/agency/r9_authority.clj`; Lean `mathlib4/DarkTower/WarMachine/F11NonSelfCertifying.lean`. Code without production caller + Lean. | different |
| 9 | Scorers and evals: `createScorer` pipeline preprocess → analyze → generateScore → generateReason (`packages/core/src/evals/base.ts`); ~24 built-ins (`packages/evals/src/scorers/`); `runEvals` gates/thresholds → `passed / scored / failed`. | R12 Two-Layer Calibration: `futon2/…/calibration_cycle.clj` wraps `scan-r12-apparatus` (per-class Beta posteriors from XTDB); Layer 2 "NOT AVAILABLE"; `held_out_calibration.clj` has no caller. Warrants: `futon2` `warrant_suite.py`. R12 has no Lean module. Partial code. | different |
| 10 | Datasets / experiments: versioned items, `groundTruthSchema`, `compareExperiments` (`packages/core/src/datasets/`) | wm-contract walkthroughs replay pinned `tick-run-record-*.edn` through code at a pinned sha (`futon2/holes/labs/wm-contract/walkthroughs/01..06`); futon6 run contract. Code + prose. | similar |
| 11 | Tracing: span tree, ~35 span types, table `mastra_ai_spans` with entity/root-entity, thread, run, session ids (`observability/types/tracing.ts:38-127`, `storage/domains/observability/tracing.ts:114`) | R8 per-tick trace (tick-run-records); futon1b evidence entries (`:evidence/type`, `:claim-type`, `:author`, `:in-reply-to`, `:fork-of`, `:conjecture?`), `/chain`, `valid-as-of=` (`futon1b/API-CONTRACT.md` §3, `futon1b_evidence.clj`). Code, live. | different (span tree vs claim graph) |
| 12 | Processors + `TripWire`: any input/output hook may `abort(reason, {retry})`; guard processors (PII, moderation, prompt injection) (`processors/index.ts:625`, `agent/trip-wire.ts`) | R20 Interoceptive Tripwires: `futon2/src/futon2/aif/tripwire.clj` T1–T13 (turn conservation, ledger closure, livelock, phase wall-clock budget…), default action `:record`; Lean `R20Interoception.lean` (viable ranges as preferences), `R20Certificate.lean`. Code (record mode) + Lean. | different |
| 13 | Budgets: `maxSteps`, `maxRetries`, timeouts, `goal.maxRuns`, `TokenCostControl` (scope run/thread/user/org, window, block or warn; **fail-open** when the scope key, cost query or cap function fails: `processors/processors/token-cost-control.ts:22,83,389,467`) | R11 `futon2/src/futon2/aif/hierarchical_budget.clj` (Pareto frontiers composed upward; exact on the supplied proposals); only caller `policy/select-budgeted-actions`, which has no production caller; T9 phase wall-clock budget is live in record mode. Code without production caller. Lean `R11JointAction.lean` states R11 as joint action, not budget. | different |
| 14 | Message history, semantic recall, working memory (`packages/core/src/processors/memory/*`, `packages/memory/src/index.ts`) | Claude memory files (`MEMORY.md` + topic files) injected at session start; futon1b evidence queries. Code / convention. | same problem, Mastra more complete |
| 15 | Observational Memory: Observer/Reflector, generation rows (`mastra_observational_memory`, `originType`), supersession decided by the reflector prompt in prose (`packages/memory/src/processors/observational-memory/`) | minus40: `:memory/version` hyperedges with `:hx/valid-time`, `export-as-of` byte-identical to disk, `:memory/sorry` conflict edges that stay visible as-of before discharge (`~/code/minus40/README.md`, `docs/spec-2026-10-09.txt`). Code (prototype, scratch futon1b :7093). | different |
| 16 | Experimental Knowledge store: `KnowledgeRecord` with `capturedAt` + `when`, soft delete, proposals with `expectedVersion`, activity log (`packages/core/src/knowledge/`, `storage/domains/knowledge/base.ts:179`) | futon1b `system-as-of` / `valid-as-of`; minus40 alias proposals (17 proposed, 0 applied). Code. | same, closest cousin |
| 17 | Goal objective (one string, judged 1/0, `GOAL_SCORE_WAITING = 0.5`) | Preferences C: `preferences.clj`, `c_vector.clj`, `mission_c.clj`, `live_c.clj` (futon2/aif); Lean `Proof2/MissionPreference.lean` (C read at click time). Code, used in cascade ranking. | different |
| 18 | **EE** trace intelligence: each trace tagged on `goal/sentiment/behavior/outcome`, clustered into themes, tracked over time (`packages/playground-ui/src/ee/signals/`, served from `output.signals.mastra.ai/api/learning/*` per `packages/cli/src/commands/api/learning-route-metadata.ts`) | 象 intent marks per turn (`futon3c/emacs/xiaoxiang-preview.el`), `session-turn-analysis.el`, minus40 象 turn follower. Code. | same intent; theirs is paid and closed-server |
| 19 | **EE** RBAC / FGA / SSO / licence client (`packages/_internals/auth/src/ee`) | none (§3 triples row 8) | none |
| 20 | Schedules (cron runs or prompts into a thread) | R10 Scheduled Observer Entrypoint: `scripts/wm_scheduled_run.clj`; Lean `R10ScheduledEntrypoint.lean` (`firing_carries_no_liveness_information`, `no_changed_state_has_no_live_tick`). Lean proved; code cron disabled. | different |
| 21 | Background tasks (`docs/…/harness/background-tasks.mdx`) | `futon3c/scripts/bg.py` + `bg_warn.py` hook (§5 backlog). Code, live. | same |
| 22 | `agent-sdks/` (wrap Claude Agent SDK, OpenAI Agents, Cursor, ACP behind `generate()`) | Agency pouches running `claude --print` / codex (`agent_pouch.clj`). Code, live. | same |
| 23 | AgentController sessions, modes, approvals; Studio UI | Emacs REPL buffers, ground control (`futon3c/holes/GROUND-CONTROL.md`: consent, mana gate, follow mode), Matrix/Element. Code + prose. | different, Mastra more polished |

## 4. Same problems, different way

For each shared problem: what Mastra does, what the WM does, and whether the WM's way is
derived from the model, built in code, or only written down.

**Dispatch / routing (rows 7, 22).** Mastra: an LLM reads the agents' `description`
strings and a prose instruction ("If you already called a primitive, you shouldn't need to
call it again, unless you strongly believe…", `network/index.ts:233`). It returns a choice
and a reason. Nothing it has observed about past outcomes enters the choice, except as
conversation text. WM: candidate policies are scored by G = D_KL[Q(o|π) ‖ C] + ambiguity,
and one is chosen from posterior ∝ habit·exp(−F − G/β). Risk measures how far the
predicted outcomes are from C. Ambiguity favours candidates whose outcomes are uncertain,
so exploring is built into the score rather than added as an extra rule. **Derived from
the model and in code**, with two limits: the walkthrough `02-selection-law` found
`:f nil, :f-status :not-supplied` on every recorded candidate (it ran as
habit·exp(−G/β)), and the live loop (R10) is off. The two selectors also choose different
things. Mastra picks a worker for a task. The WM picks a cascade/mission for the machine.
Agency's seat dispatch (who gets a bell) is still chosen by people and agents, not by G.

**Budget (row 13).** Mastra has many local caps, each with its own default (steps 5,
network iterations 1, goal runs 50, retries 0), and one cost cap that **fails open** when
its inputs are missing. Nothing composes the caps: a goal's judge calls, a network's
routing calls, Observational-Memory summarisation calls, and the LLM calls replayed after
crash recovery (which the docs warn "can reissue LLM calls (with real cost)") each count,
if at all, against whatever cap reads the same metric. WM: R11 composes per-child Pareto
frontiers upward, so a parent can pick a jointly feasible set without a greedy child
hiding a cheaper option. That is **engineering in code with no production caller**. The
*theoretical* claim is in R20's Lean (`R20Interoception.lean`): an internal reading kept
inside a viable range is just another preference, so overspend shows up as risk in G
rather than needing a special rule. That claim is stated, but no code reads spend into G.

**Suspend–resume / park (rows 3, 6).** This is the same problem, and here **Mastra is
better engineered**. It has typed `resumeSchema`, atomic claim-on-resume, snapshot
policy, ownership leases, write fencing, and documented replay semantics, including
the warning that tools must be idempotent. Agency park has deadlines and bellbacks.
Those are things Mastra does not offer directly: a Mastra suspension waits indefinitely
unless the developer schedules something. But Agency park depends on a session id
the payload does not enforce, and restart recovery is manual. Neither the WM nor Agency
has a model-derived account of suspension. R16 is about actuation, not waiting. **No
theoretical advantage claimed here.**

**Evals vs calibration (rows 8, 9, 10).** Mastra answers "how good was this output?"
with a number in [0,1] from a function or an LLM judge, compared against a threshold the
developer picks. Nothing in the code tests whether the judge's verdicts agree with
outcomes over time. A grep for Brier, ECE or reliability found nothing, and "calibration"
means only running a judge over ground-truth items
(`datasets/experiment/executor.ts:65`). R12 asks a different question: are the
precisions the machine *uses* (Π = 1/var) consistent with how often its predictions came
true? It does this with per-class Beta posteriors on two layers. R9 adds a structural
rule: the process that produced the work may not certify it. Mastra's goal judge is a
separate call but may be the same model with the same context, and that is the
self-certification R9 rules out. Status: **R9 checker is code with no admission
boundary; R12 Layer 2 is unavailable; held-out calibration has no caller.** The theory
is sharper, and Mastra's scorer infrastructure is what actually runs today.

**Tracing vs evidence (row 11).** Mastra records *what ran*: a span tree with typed
attributes, joined by trace, run, thread and entity. futon1b records *what was claimed,
by whom, in reply to what*, with conjecture flags and valid-time / system-time queries.
WM tick-run-records add the per-tick free-energy scalar (R8). A Mastra trace can't say
"this claim superseded that one" or "as of last Tuesday we believed X". futon1b
can't render a model/tool span waterfall. The two are complementary, and neither is
derived from the other. The FUTON choice of claims over spans *is* motivated by R9:
certification is a relation between claims and authors.

**Memory (rows 14–16).** This is the same problem, and Mastra's coverage is broader.
The difference is in how time and supersession are handled. Mastra's Observer is
*prompted* to write two times per observation (when it was said, and when it refers
to), and the Reflector *prompt* drops superseded items. Both are text in the context
window, decided by a model, and lost once the next generation is written. minus40
stores each version as a hyperedge with `:hx/valid-time`, can reproduce any past state
exactly, and keeps a conflict open as a `:memory/sorry` edge until it is discharged, so
"what did we believe then" stays queryable. Mastra's experimental Knowledge store
(`capturedAt` + `when`, soft delete, proposals) is converging on the same design from
the engineering side. **minus40 is code, a prototype, and not model-derived.** It is
motivated by the evidence-store discipline, not by AIF.

**Liveness of scheduled work (row 20).** Mastra counts a cron firing that completes as a
successful run. R10's Lean (`R10ScheduledEntrypoint.lean`) proves that a firing carries
no liveness information, and that a tick with no changed state is not live. **This one is
derived and proved**, and it is cheap to apply to anyone's scheduler (D1 below).

**Tripwires (row 12).** Mastra's `TripWire` fires on *what crosses the boundary*
(content, PII, injection, cost) and aborts or retries. WM R20 fires on *the machine's
own condition* (turn conservation, ledger closure, livelock, phase wall-clock), and in
theory needs no abort rule (see Budget). The code (`tripwire.clj`) is a separate
rule-based registry in record mode. Its Lean header says it was written "without
importing the discarded tripwire implementation". So the theory and the running code
disagree about whether R20 is a special mechanism. That is worth recording as a
**tension**, not glossing over.

## 5. Learn from them (borrowables)

Every item below is outside `ee/`, so Apache-2.0. Mastra is TypeScript and FUTON is
Clojure/Python/Elisp (T3), so in practice "borrow" means *take the format or the
protocol*, not the code.

| # | What | Where it would land | Licence |
|---|---|---|---|
| B1 | **Run claims + write fencing**: a claim renewed every 10 s that lapses at 30 s; each write checked against the claim generation; `force` takeover; "refuse a run the same process is executing" (`docs/…/harness/durable-agents.mdx` §Run ownership; `storage/domains/workflows/base.ts`) | Agency park/wake and job state; WM flight runner restart (memory: "WM restart debugger", "APM restart freeze") | clean |
| B2 | **Signal interruption rule**: send-now vs queue; cancel a request that has produced only reasoning, otherwise wait for the loop boundary; interrupted requests don't count toward the step budget; `cancelQueuedMessages` by signal id (`docs/…/harness/signals.mdx`) | bell semantics for working pouches; voxterm dictation into a running agent | clean |
| B3 | **Typed suspend/resume payloads** + atomic claim-on-resume (`WORKFLOW_RESUME_ALREADY_CLAIMED`) | park schema: a missing `session` field fails at park time instead of stranding the wake (memory: "Park needs session id") | clean |
| B4 | **Coded validation issues with repair actions** (`workflows/dynamic/validate/`, `repair-actions.ts`) | `wm_coverage.bb` output; flexiarg checks (memory: "flexiarg directive misuse") | clean (format) |
| B5 | **Scorer pipeline that stores every step** (preprocess/analyze results and prompts in `mastra_scorers`) | warrants and R12 evidence: store the judge prompt and intermediate output as futon1b evidence, so a later audit can re-score | clean |
| B6 | **Verdict trichotomy** `passed / scored / failed`, with the verdict omitted when every check is `notScorable` (`docs/…/evals/gates-and-verdicts.mdx`) | `warrant_suite.py` output. This matches `R20Certificate.lean`'s "absence is a hole, never a pass" | clean |
| B7 | **Span context fields** (entity / parent / root entity, run, thread, session, request ids) and the OTel gen-ai conventions (`storage/domains/shared.ts`, `observability/otel-exporter`) | futon1b evidence subject refs; an exporter from tick-run-records to any OTel viewer | clean |
| B8 | **Two-timestamp rule** from the Observer prompt ("time said / time referred to", `observer-agent.ts:80-96`) | minus40 ingest: fill `:hx/valid-time` separately from transaction time when extracting facts from transcripts | clean |
| B9 | **LongMemEval harness** (`explorations/longmemeval/`) | benchmark minus40 against Mastra OM on the same questions (D3). Check the dataset's own licence separately. | clean (harness) |
| B10 | **GitHub signal provider** (`signals/github`): PR commits, review comments and merge/close events become thread input | a bell source for agents waiting on a PR | clean |
| B11 | `TokenCostControl`'s **scope × window** vocabulary (run/thread/user/org × 1h…365d) | the leaf inputs to R11. Adopt the vocabulary and make it **fail-closed** (futon6 rule: "missing contract input refuses, never falls back") | clean |
| B12 | `agent-sdks/` adapter shape (a foreign agent runtime behind one `generate/stream` interface) | Agency pouch interface, when Codex/Claude/Zai differ | clean |

Not borrowable as code: everything in §3 row 18 and row 19 (`ee/`: production use
needs a signed agreement and a licence key, modifications belong to Kepler,
redistribution is barred, `ee/LICENSE` §3). **What they charge for tells you what they
value.** It is (a) org access control (RBAC/FGA/SSO), (b) admin policy over the agent
builder, and (c) *trace intelligence*, which clusters runs by goal / behaviour / outcome
over time. The engine itself is free. Of the entries surveyed so far, (c) is the
closest to 象-2000 (tagging turns or runs by intent and tracking the tags over time), and
it is the part they charge for. That matches T2's reading: the
harness is not where the value is, and the record of what the work *did* is.

## 6. Demonstrations

Each one is small, can be done read-only or with a few hand-written cases, and says what
counts as failure for FUTON.

**D1. A Mastra harness config, redrawn as a WM wiring diagram, shows missing budget and
liveness edges.**
- *Input:* one realistic Mastra config, written out on paper (not run), taken from the
  docs' own examples: a supervisor with two subagents, `goal {judge, maxRuns: 50}`,
  Observational Memory on, `TokenCostControl {scope:'user', window:'7d'}`,
  `recovery.durableAgents:'auto'`, a nightly schedule.
- *Procedure:* draw one box for every site that spends model tokens, as
  `wm-flight-wiring.edn` does (site, reads, writes): worker steps, routing agent,
  completion check, goal judge, Observer, Reflector, recovery replay, schedule firing.
  Then check two things:
  - that each spending box has a wire to the cost box, and whether that wire survives
    the fail-open paths at `token-cost-control.ts:22,83,389,467`;
  - for the schedule, whether anything distinguishes a firing from a state change (the
    R10 lemma).
- *Prediction:*
  - at least one spending site sits outside the cap, either because it is unscoped (no
    `userId` in the request context of a background or recovery path) or because it is
    not attributed to the scope;
  - a nightly run that changes nothing is reported as a success.
- *Failure (for us):* every spending site is reached by the cap in the default config,
  and the schedule exposes "did anything change". In that case the wiring discipline
  found nothing that reading the docs would not have found.

**D2. Calibration finds what a fixed-threshold judge misses.**
- *Input:* 40–60 historical cases with known outcomes. Candidates are futon2 warrants
  with their later pass/fail, or WM tick records with their adjudication (`05-close-and-
  adjudication`).
- *Procedure:*
  - Score every case twice, once with a Mastra-style LLM judge (rubric, threshold 0.7)
    and once with an R12-style per-class Beta posterior updated from earlier outcomes.
  - For both, report accuracy, a reliability curve (stated confidence vs observed
    frequency) and the number of gate decisions that differ.
  - Include the R9 variant: the judge is the same model and context as the author,
    versus a different one.
- *Prediction:*
  - the same-context judge passes more failing cases than the separate one (R9);
  - the posterior's confidence tracks observed frequency better than the judge's score,
    and changes at least one gate decision.
- *Failure:* no gate decision changes, or the judge is as well calibrated as the
  posterior. Then R12 is theory without consequence at this scale. Note that this demo
  needs a small new script; `held_out_calibration.clj` exists but has no caller.

**D3. Memory as of a past date: minus40 vs Observational Memory.**
- *Input:* Joe's memory files, as already reconstructed by minus40 M2 (515 versions,
  from 2026-09-01).
- *Procedure:*
  - Write 20 questions about past state ("on 09-20, what was the rule for X?",
    "which note did 10-02's fix supersede?"). Take known answers from `export-as-of`.
  - Answer them three ways: (a) minus40 as-of queries; (b) a summary written in
    Mastra's Observer/Reflector format (a two-timestamp bullet log) from the same
    transcripts, with a model answering from that log; (c) plain latest-state retrieval.
  - Building (b) with a hand-run prompt avoids installing Mastra. If it is ever
    installed, use B9's harness instead.
- *Prediction:* (a) is exact on "what did we believe then" and "what conflicts are
  still open". (b) does well on recent facts and loses superseded states once the
  Reflector condenses them.
- *Failure:* (b) answers past-state questions about as well as (a). That would mean a
  prompt rule does the job of valid-time storage at this size.

**D4. Restart in the middle of a long run (Mastra is expected to win).**
- *Input:* a long run that is suspended or waiting, then a server restart. For Mastra
  this is read from the documented behaviour and the store code. For FUTON it is a
  parked Agency job and a WM flight with a debugger attached.
- *Procedure:* list what survives the restart, what is re-driven automatically, and
  what can run twice.
- *Prediction:* Mastra recovers within about 30 s with fenced writes on pg/libsql, and
  documents the double-execution risk. Agency needs a manual restart, which is Joe's
  to do, and loses the WM debugger attach state.
- *Failure (for Mastra):* their recovery fails to cover a case Agency covers. Park
  deadlines are the candidate: a Mastra suspension has no deadline, so a forgotten
  approval waits for ever. If Mastra wins as expected, B1 + B3 go to the alternative
  backlog.

A fifth, in prose only because it needs the live loop: **EFE dispatch vs LLM routing.**
On a roster whose `description`s are stale, so that one agent's real success rate has
dropped, an LLM router keeps choosing by description. A G-based selector that observes
outcomes moves away from that agent, and its ambiguity term sends a few tasks to agents
it has not observed. Mastra should win on a cold start with novel task types, where G
has no observations. This cannot be run until R10 is live and Agency dispatch reads G.
Today neither is true, and WM is Codex-only, so it is recorded, not scheduled.

## 7. Honest negatives

- **It ships and is used.** 28.7k stars (batch-A). There are ~30 storage adapters,
  ~13 trace exporters, chat channels, ~10 sandbox providers, browser automation, MCP
  and A2A. It runs on Inngest and Temporal. A developer gets a durable, approvable,
  traced agent in an afternoon. No FUTON component is near that for someone other than
  Joe.
- **Its documentation is a spec.** The harness pages state edge cases precisely:
  reasoning-only cancellation, fencing per adapter, what replay can repeat, which
  engines ignore which options. Most WM statements of comparable precision are in Lean
  or in walkthroughs about recorded runs, not about how to operate the system.
- **The ideas FUTON has not built are already running there.** Goals with budgets,
  completion scorers that feed back into the loop, gates in CI, and crash recovery are
  all running in `@mastra/core` today. On the WM side, the corresponding nodes are code
  with no production caller (R9, R11, R12 held-out, tripwire calibration), Lean only
  (R10, R20), or switched off (the R10 cron since 2026-07-06). The theory is ahead on
  *what to check*. Mastra is ahead on *checking something every run*.
- **Breadth of memory.** Semantic recall, working memory, observational compression,
  token limiting and tool-call filtering all exist and are configurable per agent.
  FUTON's injected memory is a directory of Markdown files plus futon1b queries.
- **Where FUTON is not behind:** the selection law (G against C, with an exploration
  term), valid-time history (futon1b, minus40), claims linked to their authors as
  evidence (R9's object), and a proved liveness criterion (R10). None of these is a
  Mastra feature. The trace-intelligence paywall suggests Mastra's customers will pay
  for a record of what runs did and why, which is nearer to futon1b and 象 than to
  the engine they give away.
- **T4 (samey vs starred).** Mastra is "samey" in its *problem list*: it is group A
  (§3.2), tech for tech people. It earns the star because it is an open, detailed
  statement of the operations a software factory needs. That makes it a good
  yardstick for showing which WM R-nodes add a check the engineering approach
  leaves out (D1, D2, D3), and which only restate something Mastra already does (D4).

## 8. Sources

Mastra (clone `~/code/external/mastra` @ ebe143c4):
- `LICENSE.md`, `ee/LICENSE` (v2.0, 2026-09-22), `docs/src/content/en/docs/license.mdx`
- Workflows: `packages/core/src/workflows/{create.ts,workflow.ts,types.ts,default.ts,step.ts}`, `workflows/handlers/`, `workflows/dynamic/validate/`, `workflows/evented/`, `workflows/inngest/`, `workflows/temporal/`
- Agent loop: `packages/core/src/loop/loop-builder.ts`, `loop/shared/continuation-core.ts`, `loop/workflows/agentic-execution/` (`tool-approval-verdict.ts`, `llm-execution-step.ts`), `agent/agent.ts`, `agent/trip-wire.ts`, `llm/model/model.loop.ts`
- Networks / goals: `packages/core/src/loop/network/{index.ts,validation.ts}`, `loop/shared/steps/{is-task-complete-core.ts,goal-core.ts}`, `agent/goal/`
- Processors / budget: `packages/core/src/processors/index.ts`, `processors/processors/{token-cost-control.ts,token-limiter.ts,tool-call-filter.ts,structured-output.ts}`, `processors/retry-budget.ts`
- Memory: `packages/core/src/memory/{types.ts,memory.ts}`, `packages/core/src/processors/memory/`, `packages/memory/src/index.ts`, `packages/memory/src/processors/observational-memory/`, `packages/core/src/knowledge/`
- Evals: `packages/core/src/evals/{base.ts,types.ts,run/,scoreTraces/}`, `packages/evals/src/scorers/`, `packages/core/src/datasets/`
- Observability: `packages/core/src/observability/types/tracing.ts`, `packages/core/src/storage/domains/observability/`, `observability/*`
- Storage: `packages/core/src/storage/{base.ts,constants.ts}`, `packages/core/src/storage/domains/*`, `stores/*`
- Signals: `packages/core/src/signals/`, `signals/github/README.md`; EE: `packages/playground-ui/src/ee/signals/`, `packages/cli/src/commands/api/learning-route-metadata.ts`, `packages/deployer/src/server/index.ts:320`
- EE: `packages/_internals/auth/src/ee/`, `packages/core/src/agent-builder/ee/`, `packages/editor/src/ee/`
- Docs: `docs/src/content/en/docs/harness/{overview,signals,goals,durable-agents,background-tasks,agent-controller}.mdx`, `evals/gates-and-verdicts.mdx`, `memory/observational-memory.mdx`, `agents/guardrails.mdx`, `subagents.mdx`; online https://mastra.ai/llms.txt , https://mastra.ai/docs/harness/overview.md , https://mastra.ai/docs/harness/signals.md
- `explorations/` (longmemeval, agent-network-vs-ralph-wiggum.md, durable-agent.md)

FUTON:
- `futon3c/holes/labs/M-wm-wiring/` (`wm-adjacency.edn`, `wm-flight-wiring.edn`, `wm-vs-equation-dag.edn`, `wm-wire-coverage.edn`, `spike/*.bb`); `futon3c/holes/missions/M-wm-wiring.md:46-57`
- `futon2/holes/labs/wm-contract/aif-equations.edn`, `…/walkthroughs/01..06`; `futon2/holes/aif-r1-r16-pattern-map.md`
- `futon2/src/futon2/aif/`: `policy.clj`, `cascade_selection.clj`, `cascade_model_manifest.clj`, `full_loop_runner.clj`, `hierarchical_budget.clj`, `calibration_cycle.clj`, `held_out_calibration.clj`, `tripwire.clj`, `tripwire_calibration.clj`, `r9_checker.clj`, `preferences.clj`, `c_vector.clj`, `mission_c.clj`, `live_c.clj`, `enact.clj`
- Lean: `~/code/mathlib4/DarkTower/WarMachine/` (`Holes.lean`, `R10ScheduledEntrypoint.lean`, `R11JointAction.lean`, `R20Interoception.lean`, `R20Certificate.lean`, `F11NonSelfCertifying.lean`, `Proof2/MissionPreference.lean`)
- `p4ng/sec-catalog.tex`, `sec-rnode-dossiers-generated.tex`, `sec-issue-board-generated.tex` (R-number names and status)
- Agency: `futon3c/src/futon3c/agency/{parked_on.clj,bell_router.clj,r9_authority.clj}`, `futon3c/src/futon3c/transport/http.clj`, `futon3c/scripts/{agency_send.py,bg.py}`, `futon3c/holes/GROUND-CONTROL.md`; `GET :7070/health` returned ok (124 agents) on 2026-10-11
- futon1b: `~/code/futon1b/API-CONTRACT.md` §3, `futon1b_evidence.clj`
- minus40: `~/code/minus40/README.md`, `docs/spec-2026-10-09.txt`
- Mission context: `futon0/holes/missions/M-landscape-positioning.md` §3, §3.1, §3.2, §5, §6, T2–T4, T8; `landscape/batch-A.md`
