# Mission: FutonZero MVP — Capability Monitor + Tutor for First Proof

**Date:** 2026-02-27
**Status:** IDENTIFY (mission proposal)
**Blocked by:** None
**Cross-ref:** `futon3c/docs/futonic-missions.md`, `futon3c/holes/missions/M-mission-control.md`, `futon3c/holes/missions/tickle-spec.md`, `futon6` first-proof workstream

## 1. Motivation

Futon0 is the cyborg utility layer. It already aggregates operational telemetry
(vitality, rhythm, stack HUD), but it does not yet track capability growth loops
for agents or humans in a way that is mission-aware.

The active need is concrete: in First Proof work (`futon6` mathematical tasks),
we need to know whether agents are actually adapting futonic discipline while
their task capability improves. We do not want a separate benchmark framework.
We want a local-first, evidence-backed capability loop that composes with
existing mission/peripheral infrastructure.

This mission creates that loop as FutonZero MVP.

## 2. Mission Result

Deliver a Clojure-first, local FutonZero kernel in `futon0/scripts/futon0/futonzero/`
that produces two visible products for First Proof:

1. **Capability Monitor (agency-facing):**
   - tracks per-agent capability delta on stable `futon6` math task sets
   - tracks discipline adaptation delta (PSR/PUR/Gate behavior)
   - emits machine-readable summaries and markdown reports

2. **Tutor/Coach (agent-facing):**
   - recommends next drills and discipline corrections
   - routes coaching over visible surfaces only (bell/IRC/chat style outputs),
     not hidden back channels

### 2.1 MVP Success Criteria

- Stable, versioned taskset loaded from `futon6` first-proof corpus.
- Practice/attempt logs captured append-only with deterministic IDs.
- Scheduling loop runs (due/review/edge/probe mix).
- 7-day report demonstrates at least one measurable task capability delta.
- 7-day report demonstrates at least one measurable discipline adaptation delta.
- Coach output changes next-task selection for at least one agent.

## 3. Scope

### 3.1 Scope In

- Clojure implementation only (`.clj`/`.bb`), no Python implementation path.
- Local append-only data model for tasks, attempts, schedule updates, and reports.
- First Proof adapter for `futon6` mathematical tasks.
- Discipline metrics derived from existing futonic artifacts:
  - PSR-before-execute presence
  - PUR-after-validate presence
  - gate progression quality (G5/G4/GF/G3/G2/GD/G1/G0)
  - explicit failed-approach records
  - session continuity vs silent resets
- CLI workflow for domains/import/practice/report/export.
- Tests proving determinism, scheduler behavior, and report correctness.

### 3.2 Scope Out

- Cloud deployment / SaaS / multi-tenant service.
- LLM dependency for core evaluation.
- Full UI (CLI and artifact files are sufficient for MVP).
- New mission-control or transport substrate in futon3c.
- Full multi-domain plugin ecosystem (MVP includes first-proof adapter + toy fallback).

## 4. Gate Contract (G5 -> G0)

- [ ] **G5 Task Specification:** task schema, rubric versioning, and discipline metric definitions are explicit.
- [ ] **G4 Agent Authorization:** monitored agents and task sources are explicitly listed.
- [ ] **GF Fidelity Contract:** baseline capabilities and preserve/adapt/drop decisions are recorded below.
- [ ] **G3 Pattern Reference:** PSR pattern choices for architecture and scheduling are recorded before implementation.
- [ ] **G2 Execution:** code + tests implemented in futon0.
- [ ] **GD Document:** mission updates + docs + hypergraph plan/defer note captured.
- [ ] **G1 Validation:** acceptance tests and capability delta checks pass.
- [ ] **G0 Evidence Durability:** report artifacts and PAR notes are persisted.

## Fidelity Contract (GF)

### Baseline Capability Inventory

- Capability: Mission/peripheral evidence as structured source material.
  - Source: `futon3c` mission/peripheral cycle and evidence conventions.
  - Consumer(s): FutonZero monitor, coach recommendations.
  - Semantic/query obligations: preserve mission IDs, session IDs, claim-type semantics.

- Capability: First Proof task corpus and repeatable drills.
  - Source: `futon6` first-proof mission artifacts and problem sets.
  - Consumer(s): practice scheduler, capability reports.
  - Semantic/query obligations: deterministic task IDs, versioned rubrics.

- Capability: Visible coordination surface for interventions.
  - Source: bell/IRC/chat pathways in current stack practice.
  - Consumer(s): agents receiving coaching nudges.
  - Semantic/query obligations: no hidden control channel for coaching decisions.

### Capability Preservation Matrix (CPM)

| Capability | Decision (`preserve`/`adapt`/`drop`) | Owner | Evidence/Test | Notes |
|---|---|---|---|---|
| Mission/evidence identity semantics | preserve | mission owner | schema tests + fixture replay | No remapping of IDs/claim-types |
| First-proof task semantics | preserve | mission owner | task adapter tests | Task intent must remain stable |
| Existing discipline vocabulary (PSR/PUR/PAR/Gates) | preserve | mission owner | report fixture checks | Reuse existing terms for comparability |
| Scheduling policy weights (60/30/10) | adapt | mission owner | scheduler known-case tests | Tuneable later, deterministic now |
| Rich interactive UI | drop (MVP) | mission owner | CLI acceptance tests | Deferred to later mission |

### Tripwire Matrix

- Preserve tripwires:
  - schema incompatibility in task/attempt logs must fail tests
  - mismatch between rubric version and evaluator must fail tests
  - missing identity keys for evidence-derived records must fail tests
- Adapt tripwires:
  - scheduling output must match known deterministic fixtures for seed inputs
  - policy-weight changes must update expected fixture snapshots intentionally

### Latent Dependency/Omission Probe

- Static probe:
  - enumerate required inputs from `futon6` and `futon3c` artifacts before coding
  - list required output consumers (monitor report, coach output, export)
- Runtime probe:
  - run a dry practice session from imported tasks
  - verify monitor and coach can complete without network calls
- Omission handling:
  - unresolved input assumptions must be captured as explicit preserve/adapt/drop entries or DRs

### Drop/Defer Decision Records (DR)

- DR-1: LLM-generated hints in MVP
  - Reason: keeps evaluator deterministic and local-first.
  - Risk: weaker explanatory UX early.
  - Re-entry condition: post-MVP adapter phase with strict separation from evaluation.
  - Linked mission: future `M-futonzero-llm-hints`.

- DR-2: Full mission-control integration hooks in MVP
  - Reason: keep MVP focused on capability loop correctness.
  - Risk: manual handoff from reports to portfolio actions.
  - Re-entry condition: once monitor outputs stabilize across 2+ weekly windows.
  - Linked mission: future `M-futonzero-mc-bridge`.

## 5. Derivation Path

1. **IDENTIFY**
   - audit current first-proof task artifacts and discipline evidence surfaces
   - define minimal compatible schemas and success metrics
2. **MAP**
   - map futonic mission/peripheral concepts to FutonZero model objects
   - map first-proof tasks to domain adapter contract
3. **DERIVE**
   - fix scheduling, curriculum, and capability-delta computations
   - derive discipline-adaptation metric formulas
4. **ARGUE**
   - justify why this is an increment on mission/peripheral infrastructure,
     not a parallel framework
5. **VERIFY**
   - deterministic tests for store, scheduler, adapter, and report logic
   - end-to-end CLI test over fixture data
6. **INSTANTIATE**
   - implement namespaces, CLI commands, tests, and example run artifacts

## 6. Source Material

- `futon3c/docs/futonic-missions.md`
- `futon3c/holes/missions/M-mission-control.md`
- `futon3c/holes/missions/tickle-spec.md`
- `futon3c/src/futon3c/peripheral/mission.clj`
- `futon3c/src/futon3c/peripheral/mission_backend.clj`
- `futon3c/src/futon3c/peripheral/mission_control_backend.clj`
- `futon0/scripts/futon0/rhythm/*.clj` (style + local artifact conventions)
- `futon6` first-proof mission/problem artifacts

## 7. IDENTIFY Inventory — 2026-02-27

### 7.1 First Proof Artifact Audit (`futon6/data/first-proof`)

| Class | Count | File pattern(s) | Notes |
|---|---:|---|---|
| Prompt traces | 10 | `problem[1-10]-codex-prompts.jsonl` | Node-level prompt payloads (`node_id`, `node_type`, `prompt`) |
| Result traces (core) | 9 | `problem{1,2,3,5,6,7,8,9,10}-codex-results.jsonl` | Node-level verification outcomes (`claim_verified`, `confidence`, notes) |
| Result traces (P4 variants) | 2 | `problem4-lt4-codex-results.jsonl`, `problem4-stam-codex-results.jsonl` | `problem4-codex-prompts.jsonl` has no single matching `problem4-codex-results.jsonl`; adapter must support variant runs |
| Wiring graphs | 11 | `problem[1-10]-wiring.json`, `problem4-wiring-enriched.json` | Stable task graph substrate for deterministic task IDs |
| Verification reports | 2 | `problem4-verification.json`, `problem4-verification-enriched.json` | Includes edge-level pass/fail details and aggregate scores |
| Trajectory metadata | 2 | `selfplay-proof-episodes.jsonl`, `project-flow-dag.json` | Longitudinal path/episode and commit-lane context |

### 7.2 Discipline/Evidence Audit (`futon3c`)

- Discipline operation mapping (source: `src/futon3c/peripheral/discipline.clj`):
  - `:psr-search`, `:psr-select` -> evidence type `:pattern-selection`
  - `:pur-update` -> evidence type `:pattern-outcome`
  - `:par-punctuate` -> evidence type `:reflection`
  - `:pur-mark-pivot` -> evidence type `:correction`
- Required evidence fields for scoring contract (source: `README-evidence.md` + `peripheral/evidence.clj`):
  - `:evidence/id`, `:evidence/type`, `:evidence/claim-type`, `:evidence/author`
  - `:evidence/at`, `:evidence/session-id`, `:evidence/body`
  - optional but useful: `:evidence/pattern-id`, `:evidence/in-reply-to`, `:evidence/tags`
- Retrieval surfaces:
  - `GET /api/alpha/evidence` supports filtered evidence retrieval.
  - Mission backend tool `:evidence-query` is currently a stub returning empty entries, so FutonZero must not depend on it for MVP.
- Visible intervention surfaces for coaching:
  - IRC/forum-post channel flow and bell pathway are explicit in current stack (`tickle-spec.md`, `README-evidence.md`).
  - No hidden/back-channel coaching transport is allowed for MVP.

### 7.3 Futon0 Integration Audit (Increment, Not Parallel Stack)

- Existing local-first artifact discipline in `futon0` is JSON/JSONL snapshots via CLI modules:
  - `scripts/futon0/rhythm/*.clj` (envelope/salients/experiments/quarterly/affect)
  - `scripts/futon0/vitality/scanner.clj` and `scanner.bb`
- Existing cadence style to preserve:
  - deterministic file outputs
  - append-only event lines where appropriate
  - CLI entrypoints with `--write` and explicit output roots
- FutonZero MVP should follow same pattern:
  - namespace root `scripts/futon0/futonzero/`
  - local output root under `~/code/storage/futon0/`
  - generated report artifacts compatible with mission/peripheral review flow

### 7.4 IDENTIFY Decisions Locked for MAP

- D-I1: Treat wiring graphs as canonical task graph substrate; treat prompt/result traces as attempt/evaluation overlays.
- D-I2: Represent P4 as multi-run task evidence (variant runs), not as a missing-data error.
- D-I3: Compute discipline adaptation from evidence entries (`pattern-selection`, `pattern-outcome`, `reflection`, gate-aligned claim-types), not from ad hoc logs.
- D-I4: Route tutor nudges only through visible surfaces (IRC/bell/chat transcript path), preserving operational transparency.

## 8. Checkpoint 1 — 2026-02-27

**What was done:**
- Defined `M-futonzero-mvp` scope and gate contract.
- Anchored MVP deliverables to First Proof capability + discipline adaptation.
- Added full GF block with CPM, tripwires, omission probe, and DRs.
- Completed IDENTIFY archaeology with concrete file-count inventory, schema surfaces,
  and integration constraints across `futon6`, `futon3c`, and `futon0`.

**Test state:**
- Not run (documentation checkpoint only).

**Next:**
- MAP phase for MVP1: define exact `futonzero` model mapping (`Task`, `Attempt`,
  `Result`, `ScheduleState`, `DisciplineSnapshot`) and first-proof adapter boundaries.
- Re-IDENTIFY addendum for MVP2 pedagogy lane before any MVP2 implementation.

## 9. Pedagogical Utility Extension (MVP2 lane, in-mission)

### 9.1 Gate Conditions (MVP2 Spec vs Implement)

MVP2 **specification** work (IDENTIFY/MAP/DERIVE/ARGUE) may proceed now.

MVP2 **implementation/deploy** work remains blocked until MVP1 exit evidence exists:

- MVP1 CLI commands run end-to-end (`domains`, `import-tasks`, `practice`, `report`, `export`).
- Deterministic tests for store/scheduler/adapter/report are passing.
- At least one 7-day report shows measurable task capability delta and discipline delta.
- At least one coach intervention was delivered through visible surface only (IRC/bell/chat transcript path).

### 9.2 Why this extension

Current wording emphasizes capability growth on math tasks. We also want to test
whether generated knowledge becomes pedagogically useful: can a learner follow a
pathway and actually acquire reusable understanding, not just consume outputs.

This extends the existing monitor+tutor loop, rather than creating a parallel
framework.

### 9.3 Added mission objective (MVP2)

For a fixed subset of first-proof content (`problem1`-`problem3`), FutonZero should measure:

- explanation quality under constrained learner conditions
- pathway effectiveness (guided path vs baseline/random path)
- retention and transfer (can the learner solve adjacent problems after delay)

### 9.4 Learning-simulation "pachinko" loop (MVP2)

Add a stochastic traversal mode over concept prerequisites:

1. Sample an entry point and learner state seed.
2. Roll through a weighted path graph (due/review/edge/probe + prerequisite hops).
3. At each node, present teaching artifact + micro-check.
4. Record pass/fail, hints used, latency, confidence, and error type.
5. Continue until mastery threshold or budget exhaustion.
6. Compare pathway policy variants over repeated rollouts.

This yields policy-level evidence for whether established pathways improve
comprehension versus unguided traversal.

### 9.5 Operational metrics (MVP2 defaults)

- `learning_gain_delta`: pre-check to post-check improvement per pathway.
- `hint_efficiency`: gain per hint token/budget unit.
- `retention_delta`: same concept family after delay window (default: 24h).
- `transfer_delta`: performance on structurally related but unseen tasks
  (default set: unseen nodes sharing at least one tag family with training nodes).
- `pathway_win_rate`: fraction of rollouts where guided policy beats baseline.

### 9.6 Model role split (keeps DR-1 intact)

- Deterministic core evaluator remains authoritative for mission scoring.
- Optional learner simulation is a probe lane for pedagogy stress tests, not
  the source of truth.
- In MVP2 this simulation is disabled by default and must be seedable/deterministic
  when enabled.
- No LLM-generated hinting is required for MVP core correctness.

### 9.7 Minimal artifact additions (MVP2 MAP input)

- `TeachingUnit` (concept id, explanation, examples, common pitfalls, checks)
- `LearningEpisode` (seed, pathway, node outcomes, final mastery state)
- `PathPolicyReport` (policy comparison summary + confidence bounds)

All remain local append-only JSON/JSONL artifacts under FutonZero output roots.

### 9.8 Tickle/coach implication

Tutor recommendations should be allowed to shift from "next proof task" to
"next pedagogical intervention" when pathway evidence shows comprehension drag.
This keeps the ball moving from raw task completion to durable understanding.

## 10. Re-IDENTIFY Addendum for MVP2 (required before MVP2 implementation)

Before MVP2 implementation starts, run a focused re-IDENTIFY pass over:

- concept/prerequisite extractability from selected wiring subset (`problem1`-`problem3`)
- availability of explanation artifacts to populate `TeachingUnit`
- baseline policy definition (random/unguided) and budget normalization rules
- retention/transfer fixture tasksets and seed strategy
- expected sample sizes for pathway comparisons

Output artifact requirement:

- append a new checkpoint with concrete file counts, schema notes, and unresolved
  assumptions (same evidence discipline as Section 7)

## 11. Checkpoint 2 — 2026-02-27

**What was done:**
- Reframed pedagogical extension as explicit `MVP2` lane with a hard MVP1 exit gate.
- Fixed initial MVP2 subset to `problem1`-`problem3` for bounded derivation.
- Added re-IDENTIFY requirement before MVP2 MAP/DERIVE.

**Test state:**
- Not run (documentation checkpoint only).

**Next:**
- Continue MVP1 MAP/DERIVE first.
- MVP2 spec track is allowed now; MVP2 implementation remains gated on MVP1 exit evidence.

## 12. MVP1 MAP Commitments — 2026-02-27

### 12.1 Namespace Map (`scripts/futon0/futonzero/`)

MVP1 implementation namespaces:

- `futon0.futonzero.cli`
  - command parsing + dispatch for `domains`, `import-tasks`, `practice`, `report`, `export`
- `futon0.futonzero.store`
  - append-only JSONL writes + deterministic id helpers + local index reads
- `futon0.futonzero.models`
  - map shape constructors/validators for `Task`, `Attempt`, `Result`, `ScheduleState`, `SessionSummary`
- `futon0.futonzero.schedule`
  - deterministic SM-2-like update + due-date computation
- `futon0.futonzero.curriculum`
  - selection mix (`due` 60 / `edge` 30 / `probe` 10)
- `futon0.futonzero.metrics`
  - capability delta and discipline-adaptation rollups
- `futon0.futonzero.domains.first-proof`
  - adapter over `futon6/data/first-proof` artifacts
- `futon0.futonzero.coach`
  - visible-surface recommendation payloads (no hidden back channel)

### 12.2 Model Mapping (MVP1)

| FutonZero object | Canonical fields | Primary source(s) | Notes |
|---|---|---|---|
| `Task` | `task_id`, `domain`, `version`, `prompt`, `payload`, `difficulty`, `tags`, `created_at` | `problem*-wiring.json`, `problem*-codex-prompts.jsonl` | `task_id` deterministic from `(domain,node_id,task_version)` |
| `Attempt` | `attempt_id`, `task_id`, `timestamp`, `user_answer`, `latency_ms`, `hints_used`, `result`, `rubric_version`, `agent_id`, `session_id` | live practice loop + optional imported result traces | Imported traces marked `origin=historical` |
| `Result` | `pass?`, `score`, `feedback`, `evaluator_id` | domain evaluator + imported verification fields | `claim_verified` mapped to pass/fail bucket |
| `ScheduleState` | `task_id`, `ease`, `interval`, `reps`, `next_due`, `last_attempt_id` | derived from attempt stream | single source of truth is append-only updates |
| `DisciplineSnapshot` | `agent_id`, `window`, `psr_count`, `pur_count`, `par_count`, `gate_signal`, `continuity_flags` | evidence entries (`pattern-selection`, `pattern-outcome`, `reflection`, gate-aligned claims) | no dependency on mission backend `:evidence-query` stub |
| `SessionSummary` | `session_id`, `agent_id`, `n_attempted`, `n_passed`, `avg_latency_ms`, `next_due_count`, `coach_events` | practice run + schedule + coach output | emitted at end of `practice` |

### 12.3 Artifact/Path Map (local-first)

Root: `~/code/storage/futon0/futonzero/`

- `tasks/<domain>.jsonl` — canonical task materialization
- `attempts/<domain>.jsonl` — append-only attempt log
- `schedule/<domain>.jsonl` — append-only schedule update ledger
- `sessions/<domain>.jsonl` — session summaries
- `reports/<domain>/YYYY-MM-DD.{md,json}` — capability reports
- `coach/<domain>.jsonl` — visible-surface coaching recommendations
- `exports/<domain>/...` — backup/export payloads

This follows existing futon0 rhythm/vitality practice: local JSON/JSONL artifacts
with deterministic, inspectable writes.

### 12.4 CLI Contract (MVP1)

- `futonzero domains`
  - lists installed adapters (`first-proof` now; toy fallback optional)
- `futonzero import-tasks <domain>`
  - materializes task JSONL and optional historical attempts
- `futonzero practice <domain> --n <N>`
  - runs curriculum selection + evaluation + schedule update + session summary
- `futonzero report <domain> --days <D>`
  - emits markdown + JSON capability/discipline deltas
- `futonzero export <domain>`
  - writes portable snapshot of tasks/history/reports

### 12.5 Mission/Peripheral-Aware Mapping

- Every `Attempt` and `SessionSummary` carries `agent_id` and `session_id`.
- Report layer includes discipline section keyed by evidence semantics
  (`pattern-selection`/`pattern-outcome`/`reflection`) and gate progression hints.
- Coach output is explicitly delivery-agnostic but visible-surface only:
  recommendation payloads are emitted as artifacts intended for IRC/bell/chat relay.
- No new futon3c transport substrate is introduced in MVP1.

### 12.6 MAP Constraints for DERIVE

- C1: deterministic ids and deterministic scheduling updates for identical seeds/input history.
- C2: strict append-only logs for attempts/schedule/sessions (no in-place mutation of history).
- C3: first-proof adapter must accept P4 multi-result variant reality without schema break.
- C4: discipline metrics must degrade gracefully when some evidence classes are absent.

## 13. Checkpoint 3 — 2026-02-27

**What was done:**
- Completed MVP1 MAP commitments for namespaces, model mapping, artifact paths,
  CLI contract, and mission/peripheral integration boundaries.
- Locked DERIVE constraints (`C1`-`C4`) to keep implementation deterministic and local-first.

**Test state:**
- Not run (documentation checkpoint only).

**Next:**
- DERIVE: finalize scheduler formulas, curriculum edge/probe thresholds, and
  discipline metric formulas as implementation-ready rules.

## 14. MVP1 DERIVE Commitments — 2026-02-27

### 14.1 D-1 Scheduler Update Rule (SM-2-like, deterministic)

**IF**
- `ScheduleState` is tracked per `task_id` with `ease`, `interval`, `reps`, `next_due`.
- Attempt outcomes include `pass?`, `latency_ms`, `hints_used`.

**HOWEVER**
- Raw pass/fail is too coarse for pacing; latency and hint dependence must affect spacing.

**THEN**
- Convert each attempt to quality `q` in `[0..5]`:
  - `q=5`: pass, `latency_ms <= latency_target_ms`, `hints_used=0`
  - `q=4`: pass, and (`latency_ms <= 2*latency_target_ms` or `hints_used=1`)
  - `q=3`: pass, otherwise
  - `q=2`: fail
- Ease update:
  - `ease' = max(1.3, ease + (0.1 - (5-q)*(0.08 + (5-q)*0.02)))`
- Interval/reps update:
  - if `q < 3`: `reps'=0`, `interval'=1`
  - else if `reps=0`: `reps'=1`, `interval'=1`
  - else if `reps=1`: `reps'=2`, `interval'=6`
  - else: `reps'=reps+1`, `interval'=round(interval * ease')`
- `next_due = attempt_timestamp + interval' days`.

**BECAUSE**
- This preserves SM-2 shape while incorporating futon-specific quality signals.
- Deterministic arithmetic/rounding gives reproducible schedules.

### 14.2 D-2 Curriculum Selection Rule (60/30/10 with bounded fallback)

**IF**
- Selection mix is `due 60%`, `edge 30%`, `probe 10%`.

**HOWEVER**
- Real datasets are sparse; some buckets can be empty in short histories.

**THEN**
- For request size `N`:
  - `due_n = floor(0.6*N)`
  - `edge_n = floor(0.3*N)`
  - `probe_n = N - due_n - edge_n`
- Bucket definitions:
  - `due`: `next_due <= now`
  - `edge`: attempts `>=3` and fail-rate in `[0.30, 0.60]`
  - `probe`: unseen tasks or held-out variant tasks
- Deterministic bucket ordering:
  - primary key: `next_due` ascending (due), fail-rate closeness to `0.45` (edge), `task_id` lexical tie-break.
- Fallback fill order when bucket underflows:
  - `probe -> edge -> due` deficits are filled from remaining pools in order `due`, then `edge`, then `probe`.

**BECAUSE**
- Policy intent (spaced review + boundary pushing + generalization) is preserved while always returning exactly `N`.

### 14.3 D-3 Capability Delta Rule (7-day report core)

**IF**
- Mission success is defined as capability change over time, not absolute score.

**HOWEVER**
- A single pass is noisy; we need stable improvement signals.

**THEN**
- `newly_mastered` task criterion:
  - at least one fail in baseline window, then at least two consecutive passes in report window.
- `latency_delta`:
  - compare median latency on stable tasks (`>=3` total attempts across windows).
- `hint_delta`:
  - compare mean hints-used on stable tasks.
- `tag_growth`:
  - aggregate `newly_mastered`, latency and hint improvements by task tag.
- Emit both markdown and JSON from same computed summary map.

**BECAUSE**
- These rules measure durable improvement and reduce single-attempt noise.

### 14.4 D-4 Discipline Adaptation Rule (evidence-backed, degradable)

**IF**
- Discipline evidence types are available as `pattern-selection`, `pattern-outcome`,
  `reflection`, plus claim-type continuity signals.

**HOWEVER**
- Some sessions may lack one evidence class due to tooling gaps or legacy flow.

**THEN**
- Per agent, per report window:
  - `psr_count`, `pur_count`, `par_count`
  - `continuity_ratio = chains_with_goal_step_conclusion / total_chains`
  - `discipline_score` weighted over available components only:
    - weights target: `psr 0.30`, `pur 0.30`, `par 0.20`, `continuity 0.20`
    - if a component is unavailable, renormalize remaining weights.
- Report must include explicit availability flags so missing data is visible, not silently zeroed.

**BECAUSE**
- This preserves comparability when data is complete while staying robust to partial evidence landscapes.

### 14.5 D-5 First-Proof Adapter Rule (P4 variant compatibility)

**IF**
- `problem4` has prompt traces plus variant result traces (`lt4`, `stam`) instead of one canonical result file.

**HOWEVER**
- Forcing one-to-one prompt/result pairing would either drop data or invent fake joins.

**THEN**
- Import produces:
  - one canonical `Task` set from wiring/prompts
  - multiple historical `Attempt` streams keyed by `run_variant` for P4 (`lt4`, `stam`, future variants)
- Evaluator treats imported variant attempts as historical evidence, not authoritative future evaluator logic.

**BECAUSE**
- This keeps source fidelity and avoids schema distortion.

### 14.6 DERIVE Test Vectors (must exist before G1)

- Scheduler known cases:
  - fail (`q=2`) resets reps/interval to `0/1`.
  - consecutive strong passes (`q=5`) produce `1 -> 6 -> round(6*ease)` growth.
- Curriculum composition:
  - returns exactly `N` with deterministic ordering and documented fallback behavior.
- Capability delta fixtures:
  - at least one fixture demonstrating `newly_mastered` transition.
- Discipline delta fixtures:
  - complete evidence chain case and missing-component case (weight renormalization).
- First-proof adapter fixture:
  - `problem4` import includes both `lt4` and `stam` streams without collision.

## 15. Checkpoint 4 — 2026-02-27

**What was done:**
- Completed DERIVE commitments as implementation-ready rules for scheduler,
  curriculum, capability deltas, discipline deltas, and first-proof adapter behavior.
- Locked deterministic formulas and required test-vector coverage for G1 validation.

**Test state:**
- Not run (documentation checkpoint only).

**Next:**
- ARGUE: compose the derivations into a concise architecture argument,
  then instantiate code in `scripts/futon0/futonzero/`.

## 16. MVP1 ARGUE — Why This Architecture Follows

### 16.1 Gap This Closes

First Proof currently has rich task/evidence artifacts but no local, deterministic
capability-growth harness that jointly answers:

- task capability delta (what changed in problem-solving ability)
- discipline adaptation delta (whether futonic method is actually being adopted)
- next action selection (what to practice next, and why)

This architecture closes that gap without introducing a new transport substrate.

### 16.2 Grounding Commitments

The architecture is constrained by five commitments established above:

- local-first, append-only durability (`C2`)
- deterministic behavior for identical inputs (`C1`)
- source fidelity to first-proof artifacts, including P4 variant reality (`C3`, `D-5`)
- discipline metrics computed from existing evidence semantics, with explicit missing-data handling (`C4`, `D-4`)
- visible intervention surfaces only (no hidden control channel)

### 16.3 Why Each Core Choice Follows

- **Store + model split** (`12.1`, `12.2`, `12.3`):
  follows from local-first and deterministic constraints; append-only ledgers
  are the stable substrate for replayable capability measurement.
- **SM-2-like scheduler with latency/hints quality** (`D-1`):
  follows from need to capture not only correctness but fluency and dependence.
- **60/30/10 curriculum with deterministic fallback** (`D-2`):
  follows from mission objective to balance review, edge-pushing, and probes
  while guaranteeing exactly `N` tasks per practice call.
- **Delta-first reporting** (`D-3`):
  follows from success metric being capability change over windows, not absolute score.
- **Evidence-semantic discipline scoring** (`D-4`):
  follows from futonic discipline contract (PSR/PUR/PAR/gate continuity) and
  preserves comparability with existing stack semantics.
- **Delivery-agnostic but visible coach artifacts** (`12.5`):
  follows from the requirement to compose with current IRC/bell/chat pathways
  and avoid hidden back channels.

### 16.4 Failure Modes This Prevents

- Prevents benchmark theater: reporting now centers on within-agent deltas.
- Prevents silent schema drift: deterministic IDs + append-only logs + fixture vectors.
- Prevents P4 data loss: variant-result handling is first-class, not a patch.
- Prevents overclaiming discipline adaptation: missing evidence classes are surfaced and weights renormalized explicitly.
- Prevents transport coupling: coach recommendations remain artifacts consumable by existing visible surfaces.

### 16.5 Why This Should Exist in Futon0

Futon0 already hosts rhythm/vitality local artifact pipelines. FutonZero extends
that same discipline from operational rhythm to capability rhythm, using the same
local JSON/JSONL cadence and review checkpoints. This keeps capability growth as
an incremental stack-native function, not a parallel framework.

### 16.6 Pattern Cross-Reference (`futon3/library`)

| Architecture claim | Pattern(s) | Why this supports the claim |
|---|---|---|
| Append-only durable local logs are mandatory | `futon-theory/durability-first`, `coordination/session-durability-check`, `agent/trail-enables-return` | Durability + reconstructability + queryable trail directly justify append-only session/attempt/schedule artifacts. |
| Task must be bounded and typed before execution | `coordination/task-shape-validation`, `futon-theory/mission-scoping` | G5 shape and bounded mission scope justify strict model schema and explicit scope-in/scope-out for MVP1. |
| Discipline adaptation must be measured through PSR/PUR/PAR loop | `coordination/mandatory-psr`, `coordination/mandatory-pur`, `coordination/par-as-obligation` | These patterns define the minimal closed evidence loop that discipline scoring depends on. |
| Reports must be evidence-grounded, not assertion-driven | `agent/evidence-over-assertion`, `coordination/artifact-registration` | Claim validity requires attached artifacts/evidence; report fields must resolve to recorded traces. |
| Capability gains must persist as durable structure | `futon-theory/local-gain-persistence` | Any discovered gain must persist or be explicitly dropped, matching delta reports + append-only stores. |
| Coach interventions must stay on visible surfaces | `corps/working-where-others-can-see` | Collaboration/coordination requires shared visibility; no hidden coaching channel. |

Pattern source roots:
- `futon3/library/futon-theory/*.flexiarg`
- `futon3/library/coordination/*.flexiarg`
- `futon3/library/agent/*.flexiarg`
- `futon3/library/corps/*.flexiarg`

### 16.7 Plain-Text Argument Summary (logic check)

1. First-proof already has task and evidence traces, but lacks a deterministic capability-growth loop.
2. Futonic design patterns require durability, typed task shape, PSR/PUR/PAR closure, and evidence-grounded claims.
3. Therefore FutonZero MVP1 must be local-first, append-only, and schema-strict.
4. Because success is "capability delta", scheduler/curriculum/reporting must optimize and measure change over windows, not one-shot scores.
5. Because discipline adoption is part of success, discipline metrics must derive from evidence semantics (pattern-selection/pattern-outcome/reflection + continuity), not ad hoc logs.
6. Because first-proof data is irregular at P4, adapter design must treat variant runs as first-class historical streams rather than forcing a fake 1:1 prompt/result pairing.
7. Because coaching must remain accountable in multi-agent operations, interventions are emitted as artifacts for visible IRC/bell/chat pathways only.
8. Therefore this architecture is an increment on existing futon0/futon3/futon6 substrate, not a parallel framework.

## 17. Checkpoint 5 — 2026-02-27

**What was done:**
- Added MVP1 ARGUE section tying architecture choices directly to MAP and DERIVE commitments.
- Documented prevented failure modes and futon0-fit rationale.
- Added explicit futon3 pattern cross-references and a plain-text argument summary for auditability.

**Test state:**
- Not run (documentation checkpoint only).

**Next:**
- INSTANTIATE MVP1 in `scripts/futon0/futonzero/` with tests for store,
  scheduler, first-proof adapter, and report generation.

## 18. MVP2 IDENTIFY Inventory — 2026-02-27

### 18.1 Pedagogy Subset Audit (`futon6/data/first-proof`, problems 1-3)

| Class | Count | Evidence |
|---|---:|---|
| Wiring graphs | 3 | `problem1-wiring.json`, `problem2-wiring.json`, `problem3-wiring.json` |
| Prompt traces | 3 files / 28 rows | `problem1-3-codex-prompts.jsonl` (9 + 10 + 9) |
| Result traces | 3 files / 28 rows | `problem1-3-codex-results.jsonl` (9 + 10 + 9) |
| Solution expositions | 3 files / 760 lines | `problem1-3-solution.md` (201 + 358 + 201) |
| Writeup expositions | 3 files / 237 lines | `problem1-3-writeup.md` (81 + 87 + 69) |
| Diagram variants | 6 files | `problem1-3-v1.mmd`, `problem1-3-v2.mmd` |

Notes:
- Problems 1-3 are sufficiently rich for MVP2 pedagogy without P4/P6 complexity.
- Expository artifacts already exist; MVP2 can derive `TeachingUnit` objects from
  solution/writeup + wiring node context.

### 18.2 Pedagogy-Relevant Pattern Audit (`futon3/library`)

- `gauntlet/teaching-inversion`: agent discoveries must transfer as learnable insight.
- `agent/environment-over-optimization`: optimize learning environment and policies, not single score.
- `agent/budget-bounds-exploration`: stochastic traversal must have explicit budget and escalation.
- `control/agential-efficiency-measurement`: track latency/cost/tries per policy arm.
- `f6/pattern-as-strategy`: reasoning patterns as operational moves in pathways.
- `f6/proof-as-social-process`: proof moves are structured process categories.
- `f6/learning-event-detection`: detect learning events over trajectories.
- `agent/evidence-over-assertion`: pedagogy claims must be trace-backed.
- `futon-theory/local-gain-persistence`: learned gains must persist as auditable artifacts.

### 18.3 MVP2 IDENTIFY Decisions Locked

- D2-I1: MVP2 training subset is fixed to `problem1`-`problem3` for first implementation.
- D2-I2: `TeachingUnit` derivation uses existing markdown + wiring context; no new authoring tool required for MVP2.
- D2-I3: Policy comparisons must include at least one unguided baseline arm.
- D2-I4: All pedagogy conclusions must be reproducible from local artifacts and seeded rollouts.

### 18.4 Strategic Data Advantage Addendum (storage GPU corpora)

Verified local corpora with major leverage:

| Corpus | Path | Size | Key signal |
|---|---|---:|---|
| MO GPU merged run | `~/code/storage/mo-processed-gpu` | ~9.1G | 95,321 QA pairs, 10,442 tags, stages through hypergraphs/thread wiring |
| Math.SE GPU merged run | `~/code/storage/math-processed-gpu` | ~61G | 805,200 QA pairs, 14,503 tags, stages through hypergraphs + GNN + FAISS |

Verified contrast with repo baseline:
- `futon6/mo-processed` currently reports early stages only (`parse`, `ner_scopes`) with 1,560 tags.
- storage MO GPU run includes later stages (`embeddings`, `llm_pattern_tags`, `thread_wiring`, `expression_surfaces`, `hypergraphs`) and much denser scope coverage (~0.82 vs ~0.43 in repo manifest lineage).

MVP2 integration decisions:
- D2-I5: Treat both GPU corpora as **read-only external inputs**; do not import raw blobs into git.
- D2-I6: Add configuration-based corpus roots for optional enrichment:
  - `FUTONZERO_MO_GPU_ROOT=~/code/storage/mo-processed-gpu`
  - `FUTONZERO_MATH_GPU_ROOT=~/code/storage/math-processed-gpu`
- D2-I7: MVP2 core remains runnable without these corpora; GPU corpora are enrichment lanes for:
  - richer transfer probe selection
  - pathway prior mining from thread-wiring/hypergraphs
  - candidate concept expansion
- D2-I8: Any in-repo artifact from GPU corpora must be sampled/summarized only (manifest snapshots, aggregates), not full data copies.

## 19. Checkpoint 6 — 2026-02-27

**What was done:**
- Completed MVP2 IDENTIFY inventory and pattern audit at concrete file/row granularity.
- Locked four MVP2 identify decisions for MAP/DERIVE.
- Added strategic GPU-corpus addendum with explicit external-read and no-repo-bloat decisions.

**Test state:**
- Not run (documentation checkpoint only).

**Next:**
- MVP2 MAP commitments (objects, namespaces, artifact paths, CLI extensions).

## 20. MVP2 MAP Commitments — 2026-02-27

### 20.1 Namespace Map (`scripts/futon0/futonzero/mvp2/`)

- `futon0.futonzero.mvp2.teaching`
  - derive `TeachingUnit` objects from first-proof artifacts
- `futon0.futonzero.mvp2.pathway`
  - prerequisite/path graph assembly and seeded traversal
- `futon0.futonzero.mvp2.policy`
  - guided vs baseline policy selection
- `futon0.futonzero.mvp2.episode`
  - `LearningEpisode` lifecycle + append-only event logging
- `futon0.futonzero.mvp2.retention`
  - delayed re-check scheduling and scoring
- `futon0.futonzero.mvp2.transfer`
  - unseen-but-related probe taskset selection
- `futon0.futonzero.mvp2.report`
  - `PathPolicyReport` generation (md + json)

### 20.2 Model Mapping (MVP2)

| MVP2 object | Canonical fields | Source(s) | Notes |
|---|---|---|---|
| `TeachingUnit` | `unit_id`, `problem_id`, `concept_id`, `prereqs`, `explanation`, `examples`, `pitfalls`, `checks`, `tags`, `version` | `problem*-solution.md`, `problem*-writeup.md`, wiring nodes/edges | deterministic `unit_id` from `(problem_id,concept_id,version)` |
| `PolicyArm` | `policy_id`, `kind` (`guided`/`baseline`), `params`, `seed` | mission config | baseline required for every comparison |
| `LearningEpisode` | `episode_id`, `agent_id`, `seed`, `policy_id`, `entry_unit`, `events`, `budget`, `end_reason`, `mastery_state`, `started_at`, `ended_at` | runtime loop | append-only event stream |
| `RetentionProbe` | `probe_id`, `episode_id`, `delay_hours`, `unit_set`, `results` | retention scheduler + checks | default delay 24h |
| `TransferProbe` | `probe_id`, `episode_id`, `relation_rule`, `task_ids`, `results` | tag/structure relation selector | uses unseen tasks only |
| `PathPolicyReport` | `window`, `arms`, `learning_gain_delta`, `retention_delta`, `transfer_delta`, `hint_efficiency`, `pathway_win_rate`, `cost_profile`, `confidence` | episode/probe logs | exported in md+json |

### 20.3 Artifact/Path Map (MVP2)

Root: `~/code/storage/futon0/futonzero/pedagogy/`

- `units/first-proof-p1p3.jsonl`
- `episodes/<domain>.jsonl`
- `events/<domain>.jsonl`
- `probes/retention/<domain>.jsonl`
- `probes/transfer/<domain>.jsonl`
- `reports/<domain>/path-policy-YYYY-MM-DD.{md,json}`

### 20.4 CLI Extensions (MVP2 spec)

- `futonzero pedagogy-build <domain> --subset p1,p2,p3`
- `futonzero pedagogy-run <domain> --policy guided --n <N> --seed <S>`
- `futonzero pedagogy-compare <domain> --days <D>`
- `futonzero pedagogy-export <domain>`

### 20.5 MVP2 MAP Constraints for DERIVE

- P2-C1: all rollouts must be seed-reproducible.
- P2-C2: guided-vs-baseline comparisons must use matched budget.
- P2-C3: retention and transfer probes must be explicit artifacts, not implied metrics.
- P2-C4: no LLM dependence for authoritative scoring.

## 21. Checkpoint 7 — 2026-02-27

**What was done:**
- Completed MVP2 MAP commitments with namespace/object/path/CLI contracts.

**Test state:**
- Not run (documentation checkpoint only).

**Next:**
- MVP2 DERIVE formulas for policy evaluation and probe scoring.

## 22. MVP2 DERIVE Commitments — 2026-02-27

### 22.1 D2-1 Episode Budget/Termination Rule

**IF**
- Rollouts are stochastic and may drift.

**HOWEVER**
- Unbounded traversal gives non-comparable outcomes.

**THEN**
- Each episode has hard budget tuple: `(max_steps, max_hints, max_latency_ms)`.
- Termination reasons are enum: `:mastered`, `:budget-exhausted`, `:no-next-unit`, `:manual-stop`.
- Guided and baseline arms must share identical budget tuple in paired comparisons.

**BECAUSE**
- Budget-bounded exploration is required for fair, repeatable policy evaluation.

### 22.2 D2-2 Learning Gain Rule

**IF**
- We need pathway-level learning effect, not raw completion count.

**HOWEVER**
- Different episodes touch different unit sets.

**THEN**
- Per episode:
  - `pre_score = mean(pre_check(unit_i))`
  - `post_score = mean(post_check(unit_i))`
  - `learning_gain = post_score - pre_score`
- Policy-level `learning_gain_delta` is paired mean difference:
  - `mean(guided.learning_gain - baseline.learning_gain)` over matched seeds.

**BECAUSE**
- Paired deltas control for seed/task variation and isolate policy effect.

### 22.3 D2-3 Retention/Transfer Rules

**IF**
- Immediate gain may not persist or generalize.

**HOWEVER**
- Persistence and transfer require separate measurement channels.

**THEN**
- `retention_delta`:
  - re-check same concept family at delay `24h` (default)
  - `retention_delta = delayed_score - post_score`
- `transfer_delta`:
  - evaluate unseen tasks sharing at least one structural tag family
  - `transfer_delta = transfer_score - baseline_transfer_score` (paired seeds)

**BECAUSE**
- This separates durable retention from immediate rehearsal effects.

### 22.4 D2-4 Hint Efficiency and Cost Profile

**IF**
- A policy can inflate gains by overspending hints/time.

**HOWEVER**
- Uncosted gains are misleading.

**THEN**
- `hint_efficiency = learning_gain / max(1, hints_used)`
- `time_efficiency = learning_gain / max(1, episode_latency_ms)`
- `cost_profile` reports hints/time/retries per arm with percentile bands.

**BECAUSE**
- Efficiency metrics are needed to compare pedagogy policies under resource constraints.

### 22.5 D2-5 Confidence/Decision Rule

**IF**
- We need deterministic decision criteria for policy wins.

**HOWEVER**
- Small sample differences can be noise.

**THEN**
- `pathway_win_rate = wins / comparisons`, where win requires:
  - `learning_gain_delta > 0`
  - and no degradation on both `retention_delta` and `transfer_delta`
- Report confidence tiers by sample count:
  - `low` `< 20`, `medium` `20-49`, `high` `>= 50` paired episodes.

**BECAUSE**
- Simple thresholding keeps MVP2 deterministic and auditable without heavy stats stack.

### 22.6 MVP2 DERIVE Test Vectors

- Paired-seed fixture where guided arm strictly beats baseline under equal budget.
- Counterexample fixture where guided gains more immediately but fails retention guard.
- Fixture proving deterministic replay from same seed/config produces identical episode traces.
- Fixture confirming transfer uses unseen task IDs only.

## 23. Checkpoint 8 — 2026-02-27

**What was done:**
- Completed MVP2 DERIVE commitments with concrete formulas and deterministic decision rules.

**Test state:**
- Not run (documentation checkpoint only).

**Next:**
- MVP2 ARGUE with pattern cross-references and plain-text logic summary.

## 24. MVP2 ARGUE — Why This Pedagogy Architecture Follows

### 24.1 Gap This Closes

MVP1 can measure task and discipline improvement, but does not yet test whether
produced knowledge is pedagogically transmissible and durable under guided pathways.

### 24.2 Grounding Commitments

- Pedagogy conclusions must be evidence-backed and replayable.
- Policy comparisons must be budget-matched and seed-paired.
- Retention and transfer must be measured explicitly, not inferred.
- Scoring authority remains deterministic evaluator path.

### 24.3 Pattern Cross-Reference (`futon3/library`)

| MVP2 claim | Pattern(s) | Why this supports the claim |
|---|---|---|
| Agent discoveries should teach humans/agents, not just produce outputs | `gauntlet/teaching-inversion` | Pedagogy lane is justified as insight-transfer infrastructure. |
| Optimize environment/policy selection, not one fixed tutor script | `agent/environment-over-optimization` | Supports policy-arm comparison architecture. |
| Stochastic exploration must be bounded | `agent/budget-bounds-exploration` | Justifies hard episode budgets and termination enums. |
| Learning effects should be treated as events over trajectories | `f6/learning-event-detection` | Supports episode/probe logging and delta measurements. |
| Proof skill acquisition is structured social process | `f6/proof-as-social-process`, `f6/pattern-as-strategy` | Supports pathway graph + unit-based pedagogy decomposition. |
| Claims about pedagogy wins must be traceable | `agent/evidence-over-assertion` | Enforces report-to-trace anchoring. |
| Gains must persist durably | `futon-theory/local-gain-persistence` | Justifies retention probes + durable artifacts. |
| Efficiency claims require explicit cost metrics | `control/agential-efficiency-measurement` | Justifies hint/time efficiency and cost profile fields. |

### 24.4 Plain-Text Argument Summary (logic check)

1. MVP1 shows whether capability changes; MVP2 tests whether that capability can be taught and retained.
2. Teaching effectiveness requires explicit pathways, not ad hoc next-task nudges.
3. Therefore MVP2 introduces `TeachingUnit`, `LearningEpisode`, and policy-arm comparisons.
4. Because stochastic traversal can hide inefficiency, comparisons are seed-paired and budget-matched.
5. Because immediate gains can be shallow, retention and transfer probes are mandatory outputs.
6. Because pedagogy claims can drift into narrative, reports must be derived from append-only traces and deterministic checks.
7. Therefore MVP2 remains stack-native, local-first, and auditable while extending FutonZero from capability monitoring to pedagogical utility.

### 24.5 MVP2 Spec/Implement Boundary

- Spec complete means MVP2 IDENTIFY/MAP/DERIVE/ARGUE sections and checkpoints exist (this document).
- Implementation start still requires MVP1 exit evidence (Section 9.1).

## 25. Checkpoint 9 — 2026-02-27

**What was done:**
- Brought MVP2 to MVP1-equivalent spec maturity (IDENTIFY/MAP/DERIVE/ARGUE).
- Added MVP2 pattern cross-references and plain-text argument summary.
- Clarified boundary: MVP2 spec now, MVP2 implementation gated by MVP1 exit evidence.

**Test state:**
- Not run (documentation checkpoint only).

**Next:**
- Proceed with MVP1 implementation.
- Run MVP2 implementation only after MVP1 exit gate is satisfied.
