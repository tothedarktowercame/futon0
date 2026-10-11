# Deep dive A: Alinery (Delegance) beside FUTON's pattern cascades

**Status:** written 2026-10-11 for `M-landscape-positioning` §6 (row A).
Reading only: Alinery was not built or run. The graph facts in §6 were
computed today by a throwaway script that parses the 14 bundled
`playbook.md` files using the edge rule Alinery's own scheduler uses. They
are reproducible from the procedure given there.

**Pins.** Alinery clone `~/code/external/alinery` at `cfc0d1e` (2026-10-10,
v0.22.2; shallow, one commit). FUTON at HEAD of `futon3`, `futon3c`,
`futon2`, `mathlib4/DarkTower/WarMachine` on 2026-10-11.

**Relation to other documents.** `landscape/batch-C.md` §Delegance/Alinery
already covers the company, licence, install and verb list, and none of that
is repeated here. Two of its counts do not match this commit: there are
**14** bundled playbooks with **98** steps (not 16/103), and
`mcp/src/main.rs` defines **26** MCP tools, plus 3 tools provided by the
harness extension (`alinery_ask_approval`, `alinery_phase_complete`,
`alinery_set_session_name`), not 35. batch-C missed one thing that matters
for this comparison: Alinery has a **formal model**, Dannenhauer's *Finite
Agent Machines* (`whitepaper/finite-agent-machines.tex`), so this is not a
case of theory on our side and none on theirs.

This document is also a partial down payment on the parked sibling packet in
`~/code/TN-deep-research-landscape-position-2026-07.md` (*where does
auditable pattern-cascade learning sit in the agent/dev-tooling landscape?*).
The TN's demarcation criterion is that *a workflow is WM-able iff it can be
theorised as a pattern cascade*. Alinery's playbooks are the nearest outside
corpus of workflows written down as dependency graphs, and §6 tests the
criterion on them. The packet's competitor set (agent memory, process
mining, buyers) is not covered here.

---

## 1. What Alinery is, mechanically

**Data model.** A playbook is one Markdown file: TOML front matter
(`version = 2`, `key`, `title`, defaults, `[[step]]` array), then one prompt
section per step, introduced by `<!-- alinery:step KEY -->`. A step has
`key, title, short, is_coding_step, auto_advance_default, inputs, outputs,
model, harness`, and a prompt (`alinery-core/src/playbook.rs`,
`NormalizedStep`). An input is `{path, mode}`, where `mode` is `single`
(one exact artifact), `each` (one execution per member of a wildcard
collection) or `complete` (the whole closed collection, once). An output is
`{path}`. A path is a relative `.md` path containing at most one filename
wildcard.

**Execution model: a dataflow graph, not a list.** The scheduler
(`alinery-core/src/playbook_scheduler.rs`, 3,011 lines) derives an edge
producer → consumer whenever an output selector overlaps an input selector
(`Graph::new`). It computes strongly connected components, treats each
cyclic component as a loop, and on every reconciliation tick binds steps to
concrete **artifact occurrences**: one accepted publication by one
execution, with its producer and its ancestors. A loop pass starts only when
a *fresh* occurrence of an entry role is accepted (`ContextCause::Loop`). A
step consuming `each` runs once per member (`ContextCause::Member`). A
`complete` input waits until the collection's membership closes, i.e. until
every expected producer has completed and shut down. If companion inputs
can be paired in more than one way, the engine refuses with `unsupported
ambiguous join` rather than guess from timestamps. Document order is
explicitly *not* a cursor (`docs/playbook-design-guidance.md`: "Destinations
come from the retained artifact-dependency graph, not definition order").

**Checks before a playbook is saved** (`parse_playbook_md`): unknown fields
are rejected (`deny_unknown_fields`); types; version 2 only; one prompt
section per step; unknown `{{TOKEN}}`s; lowercase slug keys; every step has
at least one output; `single` takes an exact path and `each`/`complete` a
wildcard; at most one collection input per step; coding steps take only
exact `single` inputs; `attachments/` and `subtasks/` are reserved; no two
steps' outputs may overlap unless both are the same exact path. There is
**no** check that every input has a producer, that every output has a
consumer, or that a loop's evidence stays fresh. Those checks appear only
as instructions to an LLM, in the `verify-save` prompt of the meta-playbook
(`playbooks/build-playbook/playbook.md`: "Check that every non-seed input
has a reachable producer…"; "parser success does not establish matching
loop occurrences").

**Gates.** `auto_advance_default = false` puts an execution into
`CompletionPermission::Locked`: the session stays interactive, and a
premature completion call gets a non-fatal "authorization required". The
human grants permission for *that one execution* ("Move to <step>" or
"Finish step"). Permission is not acceptance. Acceptance validates the
assigned output files (non-empty regular files at assigned paths, content
not inspected) and writes an immutable receipt. Shutdown confirmation is a
third, separate transition, and only after it do successors launch and
coding exclusivity get released (`docs/playbook-design-guidance.md`
§§"Human completion gate", "Completion and process shutdown are separate
transitions"). The MCP tool `alinery_save_playbook` is labelled "an
agent-followed trust boundary: the backend … does not verify an approval
receipt".

**State.** `.alinery/` in the repository holds `tasks/<slug>/artifacts/`
(numbered files `<depth>-<role>-<n>.md`, where depth = 1 + the maximum
depth of the producing parents; "presentation, not scheduling"),
`tasks/<slug>/sessions/`, `worktrees/`, `playbooks/` (repo scope),
`config.toml`, and the lock and socket files (`alinery-core/src/paths.rs`).
A per-repository daemon `alineryd` owns execution state and processes; the
app and the MCP server are clients. Sessions therefore survive the app
quitting. Each task keeps the validated playbook it was created with,
identified by a hash of its bytes (`execution.rs`,
`definition_identity`). A subtask is a child *task* with its own playbook.
It is finalised as `artifacts_only`, `integrated_code` or
`archive_without_code`, and its artifacts are snapshotted into the parent's
reserved `subtasks/` directory (`alinery-core/src/subtask.rs`,
`types.rs: FinalizeMode`).

**Their formal model** (whitepaper §2). A FAM is a transition system
(𝒜, 𝒮, s₀, →). A state is a finite set of artifact filenames, and an
invocation of schema α with inputs B ⊆ s publishes P ≠ ∅, so s′ = s ∪ P. The
paper compares this with coloured Petri nets with read arcs, workflow
patterns and statecharts, nondeterministic planning, PROV, and three
systems (Claude Code Dynamic Workflows, Smithers, LangGraph), using one
literature-review process written in all four. It is candid about limits:
"The FAM definition supplies a transition relation over artifact states,
without a goal predicate, an optimization objective, or a method for
synthesizing a policy."

## 2. Their problem in plain verbs

Subject is the operator throughout; verbs from the 象 intents; tools in the
*via* column only (M-landscape §3 rules).

| verb · object | via (actions) | fidelity check |
|---|---|---|
| delegate · a multi-session piece of work, under a fixed process | pick a process definition; the daemon starts one agent session per eligible step, binding exact input files | the execution record names, for each session, which files it read and published |
| constrain · when each part of the work may start | declare required input files per step; the engine refuses to start a step until accepted files exist | no step ever ran on a file that was not yet accepted (receipt order) |
| approve · a step's result before later work uses it | lock completion; the human grants permission per execution | each gated execution's grant is recorded against that execution and session |
| continue / defer · another round of investigation | a decision step publishes a fresh request file, or a stopping report | exactly one new pass per fresh request; none from a stopping report |
| verify · screening decisions, by two independent passes | two steps over the same items write to separate directories; a third reconciles | lane B's inputs exclude lane A's outputs (structural only; the prompt asks it not to look) |
| extend · the library of processes | run a three-step process that drafts, checks and saves a new definition, with a human gate at each step | the saved file parses, and a human approved the exact source |

The operator's problem, stated without vendor words: *I want to hand long,
branching work to several agent sessions, decide in advance which kinds of
step exist and what each must have in hand before it starts, see afterwards
exactly what each one used, and be asked before anything I care about moves
on.*

## 3. Concept map

Relation: **same** = same structure under another name; **diff** = related
but differs in a way that changes what can be checked or done; **none** = no
counterpart found.

| Alinery | FUTON | rel. | notes |
|---|---|---|---|
| Playbook (`playbooks/*/playbook.md`) | cascade as policy skeleton (`futon3/library/meta/meta-outer-policy-cascade.edn`; CASCADE-SPEC-v2 "policy construction") | diff | A playbook is authored once and chosen by a human. A cascade is meant to be *constructed* per problem from a library snapshot and scored against alternatives (§4) |
| Step: inputs, outputs, prompt (`playbook.rs: NormalizedStep`) | unit: `needs`, `produces` (`mathlib4/DarkTower/WarMachine/ConstructionReceipt.lean: UnitSemantics`) | same | at the dependency level |
| Step prompt (opaque Markdown) | flexiarg pattern IF/HOWEVER/THEN/BECAUSE, read as a blend square (`futon2/holes/E-aif-cascade.md` "one pattern is already a diamond"; `ThreeHalvesBlend.lean`) | diff | Alinery gives a step no internal structure |
| Artifact logical role (path selector) | token | same | |
| Edge derived from selector overlap (`playbook_scheduler.rs: Graph::new`) | `edgeValid`: tokens = produces ∩ needs, non-empty (`ConstructionReceipt.lean`) | same / diff | Same rule. FUTON edges are also *declared* and checked, so a forged edge is rejected (`forgedTokenReceipt_invalid`); Alinery edges exist only by derivation |
| "Document order is not a cursor" (design guidance) | "the linear extension is a parse, not the formation" (`E-aif-cascade.md`; CASCADE-SPEC-v2 §3) | same | arrived at independently |
| Loop = SCC re-entered by a fresh occurrence (`ContextCause::Loop`) | cut at the tick boundary, cross-tick edges recorded with a reason (E-aif-cascade R1; `r-cascade.edn`) | diff | Alinery executes the cycle. FUTON cuts it to get one click's acyclic formation |
| `each` / `complete` collections | none in the cascade representation | none | FUTON fans out through Agency dispatch, outside the cascade |
| Artifact occurrence + provenance (`execution.rs: ArtifactOccurrence`) | evidence entries (futon1b), construction receipt fields (CASCADE-SPEC-v2 "Construction receipt") | diff | Alinery records which publication fed which execution, by construction. FUTON records it per subsystem |
| Immutable completion receipt | test-registry warrant; `hygiene/receipt-then-gate.flexiarg` | same | |
| `auto_advance_default=false`, `CompletionPermission` | consent and mana gates (`futon3c/holes/GROUND-CONTROL.md` §0); 🈸 ask-action answered "🈸:yes" (`xiaoxiang-preview.el`) | same in intent | Alinery's per-execution grant is more precise (§5) |
| Execution lifecycle Queued…Finishing…Completed (`execution.rs`) | "Identical claims name their verified stage" (GROUND-CONTROL §0) | same | same discipline: do not call five states "done" |
| `is_coding_step` exclusivity | one JVM per repo; worktree rules (`~/code/CLAUDE.md`) | diff | |
| Session owned by `alineryd` | Agency seat; `bg.py` re-parenting to the JVM | same | |
| Subtask (child task + playbook, finalise modes) | mission → excursion; Agency bell `--mode work` | same | runtime nesting on both sides; neither composes definitions |
| `build-playbook` meta-playbook | `futon3/library/cascade-construction/` ("a policy for creating policies") | same move | Alinery's runs; FUTON's README says "None of this is implemented" |
| `verify-save` checklist ("every non-seed input has a reachable producer") | `order-by-what-each-step-needs` `@done` ("every need is produced, observed or a check candidate") | same | both currently carried out by an LLM or by hand |
| Parser/validator (`parse_playbook_md`) | `meta_outer_policy/contract-errors` (`futon2/src/futon2/aif/meta_outer_policy.clj`); `ConstructionReceipt.valid` | diff | Alinery's is strict on syntax; FUTON's Lean check is about structure but runs on hand-encoded receipts |
| Lane A / Lane B (`systematic-evidence-review`) | author ≠ reviewer (`~/code/CLAUDE.md`; M-landscape §3 row 6) | same intent | Alinery puts it in the graph (the lanes share no edges) |
| No objective ("without a goal predicate…") | G, preferences C, epistemic value (`cascade_shape_g.clj`, `meta_outer_policy.clj: evaluate`, `EpistemicValue.lean`) | none | §4 |
| — | meets / semilattice (`CascadeOrder.lean: IsMeet, hasMeets`) | none | §4, §6 D1 |
| — | pasting, gluing, pushouts (`ThreeHalvesBlend.lean`) | none | §4 |
| — | support / meet / precedence as three relations (CASCADE-SPEC-v2) | none | Alinery has one relation |
| Arms/XOR deliberately absent ("Do not add a general conditional-expression language") | `:arms` with typed guards (`meta-outer-policy-cascade.edn`) | diff | §6 D3 |
| Library scopes bundled/global/repo; sharing API (`docs/playbook-sharing-contract.md`) | `futon3/library` (1,441 flexiargs), sigils, `cascades/declared-skeleton` | diff | |
| MCP server, 26 tools | Agency HTTP `:7070/api/alpha`, `agency_send.py` | same role | |
| Depth-numbered artifact filenames | none | none | |
| FAM whitepaper (s′ = s ∪ P) | CASCADE-SPEC-v2/v3, Lean modules | both formal | §4 |

## 4. Where "a different terminological paint job" holds, and where it breaks

### It holds at the dependency layer

The step-level structures are the same object. Alinery's `Graph::new` and
FUTON's `ConstructionReceipt.edgeValid` both say there is an edge from one
unit to another exactly when something one produces is something the other
needs. Both projects reached, independently, the claim that the written
sequence is not the process: Alinery's "not a cursor", FUTON's "the cascade
is constructed, not chosen; a linear extension is a parse". Both insist on
receipts and on naming intermediate states rather than calling everything
"done". Both have a meta-level process for building processes. For this
layer Joe's reading is right: the same ideas under different names.

### Where it breaks

**1. A playbook is not a policy in the active-inference sense, and Alinery
says so.** In FUTON a cascade is a candidate policy. Several are
constructed, each gets an expected free energy G from preferences over
outcomes and predicted transitions, and one is selected
(`meta_outer_policy.clj: evaluate`, run over 103 cones in
`E-aif-cascade.md` B4). Alinery's playbook is a transition relation: it
says which steps *may* run, never which *should*. Choosing between enabled
continuations is left to the agent's prompt and the human, and choosing
between playbooks is left to the human. The whitepaper disclaims the policy
reading in so many words (§"Planning Policies"). So "graphs that act as
policies regulating workflows" covers both only in the weak sense of
*constraint*. In the sense of *selection under preferences*, Alinery has
nothing, and that difference is the substance of Joe's theory, not a
difference of names.

The FUTON side of this is thinner in code than in prose. In
`E-aif-cascade.md` B4 the real `evaluate` ranked 103 cones with
"EIG honest 0; ambiguity constant … so risk alone orders", and with both
readings removed it returned a 103-way tie as `:selected` by string order.
A 2026-09-16 memory note records cascade G as live-shadow only for enacted
War Machine selection (recorded, never selecting; `futon2/holes/M-G-over-cascades.md`).
The claim "a cascade is a policy" is therefore implemented as a scorer, but
there is not yet a record of it changing a decision.

**2. Authored versus constructed.** CASCADE-SPEC-v2: "A policy is
constructed for a pinned problem from component patterns in a pinned
library snapshot", with a receipt listing excluded patterns and their
reasons. An Alinery playbook is written by a person (or by the
`build-playbook` process with a person) and reused unchanged. Per-problem
construction is FUTON's distinctive move, and it is mostly aspiration: the
construction-level policy in `cascade-construction/README.md` is explicitly
not implemented, and the pattern-interpretation proposal D1–D8 is
unapproved (`translation/README.md`).

**3. One relation against three.** CASCADE-SPEC-v2 separates *support*
(child helps complete parent), *meet* (two requirements overlap in a named
shared unit) and *precedence* (a before b, for this problem), and warns that
"cascade = semilattice = build order is not the specification". Alinery has
one relation, artifact dependency, which acts as both support and
precedence. It has no way to say that two steps overlap in a shared unit.
The consequence for meaning: in Alinery a missing edge *means* the steps may
run concurrently, while in v2 "absence of a precedence edge is not an
assumption of simultaneous success".

**4. The semilattice.** Is there anything in Alinery that plays the role of
a semilattice join? The FAM state space is one: finite sets of filenames
under union, with s′ = s ∪ P. But that is a lattice of *states*, and every
monotone dataflow system has one. Joe's semilattice is on the *units*: the
descent order, in which every pair of patterns has a meet, i.e. a nearest
common origin (`CascadeOrder.lean`, with the direction ruled in
E-aif-cascade R3), and overlaps are named units (`declared-skeleton`;
`snatch/README.md`'s G4 diamond, computed by `futon3/checks/playout_snatch.clj`).
Alinery has no such condition, and §6 D1 shows that checking for it on
their own library finds something.

**5. Inside the step.** Alinery's step body is an opaque prompt. FUTON
reads a pattern as a blend square (IF = ground, HOWEVER = inputs, THEN =
blend, BECAUSE = why it commutes), and `ThreeHalvesBlend.lean` (2,656
lines; per E-aif-cascade, 60 theorems and no `sorry`) proves on a finite
carrier that pushouts are exactly coverage, that pasted pushouts remain
pushouts when the second square commutes (with a counterexample when it
does not), and that a set of occurrences is a `cascade` rather than a `bag`
iff they are glued into one component. All of this is proved on Goguen's
houseboat/boathouse examples and the 19 R-units. A5 found agreement between
pattern text and receipt tokens for 3 of 14 matched units, and R10 (which
order to use for library patterns) is deferred. It is real mathematics that
has not yet been applied to a library cascade at scale.

**6. Composition, refinement, equivalence.**
- *Composition:* Alinery has none at definition level: a playbook cannot
  include another, and a subtask is runtime nesting. FUTON has pasting and
  gluing in Lean (above), on toy carriers.
- *Refinement:* Alinery has none; an edit produces a new byte identity, and
  running tasks keep the old one. FUTON describes refinement as "a new
  square pasted in, or a higher blend over the same inputs"
  (E-aif-cascade IDENTIFY). There is one instance: the extended diamond,
  valid as a `ConstructionReceipt` (mathlib4 `3d7727b48d`).
- *Equivalence:* Alinery's notion is byte equality (`definition_identity`).
  FUTON has cone isomorphism (`isoCones`, Lean, toy), the requirement in
  CASCADE-SPEC-v2 that an abstraction preserving guards, transitions,
  observations, preferences and costs leaves G-rankings invariant (prose,
  with one Lean witness in `PeripheralRecognition.lean`), and the
  `translation/` patterns (prose, three worked instances).

**7. A check that outputs feed inputs.** Neither project runs one at save
time on real definitions. Alinery's version is a sentence in an LLM prompt
(`verify-save`). FUTON's is a decidable Lean predicate
(`ConstructionReceipt.valid`) applied to hand-encoded receipts, plus the
ruled default R8 that an unconsumed token earns no credit. R8 was measured
(the "house of alcoves" scored *better* than the diamond under the current
rule), but its G is typed `:unavailable` because the scorer has no
consumption provenance (`E-aif-cascade.md` C3). Alinery's runtime does have
that provenance (`ArtifactOccurrence.producer_execution_id`, ancestors), so
the data FUTON's scorer lacks exists in their engine.

**8. Learning.** FUTON intends receipts to become counts for a habit prior
over construction moves (`cascade-construction/README.md`, again "None of
this is implemented") and has Dirichlet learning in Lean
(`DirichletLearning.lean`). Where a cascade has been built and placed in
front of a learner, uptake has been close to zero: the APM memory-cascade
arm records `:used-via-cascade []` on every attempt in f58–f69, and about
one uptake per ~300 offers over the later window
(`futon3c/holes/technotes/TN-fable-F66-F70-review.md`,
`TN-fable-F81-F168-review.md`). Alinery does not claim to learn: playbooks
are improved by people.

### Implemented versus prose, FUTON side

| exists as code or Lean | exists as prose or proposal only |
|---|---|
| `ConstructionReceipt.lean` (370 lines): token edges, meet witnesses, forged-edge rejection, diamond and extended diamond | construction-level policy and habit prior over moves (`cascade-construction/README.md`) |
| `CascadeOrder.lean` (145): acyclicity, `IsMeet`, `hasMeets`, a refusal witness from a recorded run | pattern interpretation D1–D8 (`translation/README.md`) |
| `ThreeHalvesBlend.lean` (2,656): pushouts, pasting, gluing verdict | library-level blend order (R10, deferred) |
| `RCascadeReceipt.lean`, `EpistemicValue.lean` | Layer-4 process, "no runtime gate … is claimed" (`workshop/LAYER4-PROCESS.md`) |
| `meta_outer_policy.clj` (315): `contract-errors`, `evaluate` | IAD adapter, "not implemented or commissioned" (`workshop/IAD-ADAPTER.md`) |
| `cascade_shape_g.clj` (443): G for an arranged cascade | lumpability and ranking invariance under abstraction (CASCADE-SPEC-v2), beyond one witness |
| `playout_snatch.clj`: up-closure, tree versus semilattice | cascade G selecting enacted WM output (shadow only, per the 09-16 note) |
| APM `expand-memory-cascade` on the student path (H3 arm) | consumption-provenance scoring (R8 diff, unapplied) |

## 5. What to borrow

Each item names where it would go in FUTON. None is a commitment.

1. **A seed-only input, and the honest use of one.** Alinery authors read
   the original ticket through the `{{TICKET_FILE}}` prompt token so that a
   root step does not re-run on every fresh ticket ("This root-only step …
   later discovery tickets must not restart it"). The underlying need is an
   input that binds the initial occurrence only. FUTON's
   `ConstructionReceipt` already distinguishes the click's initial *have*
   from produced tokens (E-aif-cascade R2/R4). *Lands:* nothing to borrow
   here; it confirms the have/produced split. The suggestion goes the other
   way (§6 D1).
2. **Permission ≠ acceptance ≠ shutdown ≠ successor launch,** with a grant
   valid for one execution, no self-approval path through MCP, and a "Retry
   handoff" that resends without granting again. *Lands:* Agency job states
   and park/wake. FUTON's agency-send brief-mode trap (a bell that runs
   nothing and still reports done) is a case of confusing acceptance with
   completion that this vocabulary would have named.
3. **Occurrence-bound loops with a typed ambiguity refusal.** Rather than
   cutting a cycle, Alinery executes it and refuses joins it cannot pair
   (`unsupported ambiguous join`). *Lands:* CASCADE-SPEC-v3, beside the
   R1 cut. The cut gives one click's formation, and Alinery's contexts say
   how successive clicks bind to each other's outputs. The two fit
   together.
4. **A runnable meta-process.** `build-playbook` is three gated steps
   (define → draft and refine → verify and save), and the `verify-save`
   prompt is a detailed checklist (responsibilities, collections at 0/1/2
   members, loop second pass, every input produced, branch outcomes,
   syntax). *Lands:* `cascade-construction/`. Its moves already have `@done`
   lines, and Alinery shows a shell in which they could run with a human
   gate at save.
5. **Strict parsing.** `deny_unknown_fields`, unknown prompt tokens
   rejected, a reserved namespace. *Lands:* `futon.flexiarg.projection/parse-file`.
   The open problem that half the pattern graph dangles on prose inside
   `@why`/`@how` (memory: flexiarg-directive-misuse) is the kind of thing a
   strict parser reports at save time.
6. **Matched examples as the comparison method.** The whitepaper writes one
   literature-review process in Claude Code Dynamic Workflows, Smithers,
   LangGraph and Alinery, with shared fixtures (`whitepaper/examples/`).
   *Lands:* §6 here and the TN sibling packet. A FUTON cascade as a fifth
   column over the same fixtures would be the cheapest public
   demonstration available.
7. **Dual-lane screening as structure.** Two steps over the same `each`
   collection, separate aggregates, one reconciling join. After the loop
   cut, these 12 lane pairs are the only incomparable step pairs in the
   whole bundled library (§6 D2). *Lands:* author ≠ reviewer in the test
   registry and in reviews. "Reviewer's inputs contain no author output" is
   checkable from an ancestry record. Alinery still enforces only the graph
   half: lanes share a worktree, and the prompt says "Do not inspect Lane
   A's output".
8. **One authority, clients as projections.** `alineryd` owns state, while
   the UI and MCP are "repairable projections of durable execution records".
   *Lands:* the futon3c JVM and Emacs REPL buffers. The REPL-buffer-wipe
   recovery memory is an incident where the UI was the only copy.
9. **Step vocabulary for DERIVE.** The 98 step titles are a ready verb
   list. *Continue or Pause* is 🈰 continue / 🈝 defer; *Prove the Root
   Cause* is ㊬ verify; *Decide* (the SuperDevelop gate) is ㊝ prioritize
   plus ㊣ approve; *Deduplicate and Dispatch* is ㊮ collect plus ㊯
   delegate. *Lands:* M-landscape §3 verb table. There is no new verb gap:
   *Reconcile* and *Aggregate* map to ㊮ collect, and *compare* is already
   listed as a gap.
10. **An MCP face on Agency.** Alinery's 26 tools let any harness drive
    tasks, sessions and artifacts. *Lands:* alternative backlog entry 1
    (bg.py), method 3 ("an MCP tool or a native tool"). Alinery also labels
    trust boundaries in the tool description, which is worth copying.
11. **Explicit dispositions instead of inferred pruning.** A reader that
    finds a source unusable publishes a trigger saying so, rather than
    publishing nothing ("The engine does not infer branch pruning from
    missing files"). *Lands:* FUTON already does this with typed absences.
    This confirms an existing habit.

## 6. Demonstrations

Each has an input, a procedure, and what would count as failure. D1 and D2
were run today (read-only parse; numbers below). D3 and D4 are designed,
not run.

### D1. A meet check on Alinery's own library finds dependencies the graph does not record

- **Input.** The 14 bundled playbooks at `cfc0d1e`.
- **Procedure.** Build Alinery's own edge relation (selector overlap, as in
  `Graph::new`). Add one unit `SEED` producing `ticket.md`. Cut the edges
  that carry the loop-entry role (`ticket.md`, or `design-request.md` in
  `solution-exploration`) as cross-tick state (E-aif-cascade R1). Then test
  `CascadeOrder.hasMeets`: does every pair of units have a common origin?
- **Result.** After the cut every playbook is acyclic. 12 of 14 have a meet
  for every pair. `academic-survey` (1 pair) and `spec` (4 pairs) do not,
  because each has a second source, `frame-review` and `research`
  respectively, with `inputs = []`. Both prompts read the original ticket
  through `{{TICKET_FILE}}` ("Read the original task ticket at
  `{{TICKET_FILE}}` … This root-only step …"). The dependency is real, and
  it lives outside the graph so that the step does not re-fire on later
  tickets. Declaring it as a seed-only input (Alinery has no such mode) or,
  in FUTON terms, as an initial *have* restores the meets.
- **What it shows.** The semilattice condition, applied mechanically, found
  the one place where the bundled library routes a dependency around its
  own engine. It also suggests a small engine feature (`mode = "seed"`).
- **Failure.** The check would be noise if the no-meet pairs matched no
  undeclared read, i.e. if Alinery had legitimate independent roots. It
  does not here. In fairness: once the reason is known, "every step has a
  declared input or is the seed" is a one-line lint Alinery could add with
  no category theory. The theory supplied the question, not a capability
  their representation lacks.

### D2. Ordering ambiguity on Alinery's library: where the theory adds nothing

- **Input.** As D1, after the cut.
- **Procedure.** Count linear extensions (CASCADE-SPEC-v2: ordering
  ambiguity = log₂ #extensions, an EFE term) and incomparable step pairs.
  Transitive reduction as in E-aif-cascade A4.
- **Result.** Every acyclic bundled playbook has exactly **one** linear
  extension, and so does every loop playbook after the cut except
  `systematic-evidence-review`, whose **12** incomparable pairs are exactly
  its A/B lanes. The ordering term is 0 for 13 of 14. Declared edges are
  heavily redundant: SuperDevelop 21 → 6 after reduction,
  primed-feature-development 15 → 5, bug-hunting 9 → 4.
- **What it shows.** Alinery's processes are chains with context passed
  forward, and their branching happens at runtime (`each` fan-out), not in
  the static graph. The v2 ambiguity term, the linear-extension count and
  the meet structure have nothing to work on in 13 of 14 cases. The
  transitive reduction would also be *wrong* here: a "redundant" input in
  Alinery delivers a file to the agent's context, so an edge is a context
  channel as well as an order. FUTON's `edgeValid` agrees (an edge is a real
  need, and reduction is only for drawing), but the scoring theory has
  nothing to say about context delivery.
- **Failure (for the claim "the theory adds something to Alinery-style
  workflows").** Shown: it adds nothing at the order level for chains. The
  test is whether that changes once fan-out is written into the static
  graph rather than left to runtime.

### D3. Exclusive outcomes: arms with guards against prompt-enforced XOR

- **Input.** `solution-exploration`: `check` declares four outputs,
  including `design-request.md` (another pass) and `synthesis-request.md`
  (stop and compare). The engine will accept both, and both continuations
  will then run. Exclusivity is in the prompt only, by design ("any
  exclusivity belongs in the prompt, not engine XOR validation").
- **Procedure.** Translate `check` into two arms with guards, in the shape
  of `meta-outer-policy-cascade.edn :arms`, and the rest of the playbook
  into units with needs/produces. (a) Static: check that the arms' guards
  are disjoint. (b) Selection: put preferences over outcomes (budget
  fraction, comparison delivered, abstention) and score the two arms with
  `meta_outer_policy/evaluate` on a recorded state. Compare G's choice with
  the agent's choice over N real Alinery task records, which would have to
  be collected.
- **Expected.** (a) makes "both published" unrepresentable, which Alinery
  cannot state. However, I did not find a guard-disjointness check in
  `contract-errors`, so this needs a small addition on FUTON's side too.
  (b) is where Alinery is expected to tie or win: FUTON's own B4 run found
  epistemic value 0 and constant ambiguity, so the ranking reduces to a
  preference-weighted risk term that a sentence in a prompt also encodes.
- **Failure.** If, over N tasks, G's choice agrees with the prompt-driven
  agent no better than the prompt's own rule, AIF selection buys nothing
  here until there are learned transition counts. If (a) cannot be checked
  without a hand-written token vocabulary for `check`'s outcomes, the
  translation costs more than the check is worth.

### D4. Composition by gluing: two playbooks into one

- **Input.** `bug-hunting` (ends in `pr-note.md`) and `review` (starts from
  `ticket.md`: Confirm Review Scope).
- **Procedure.** In Alinery: no operation exists; the options are a subtask
  or a hand-written third playbook. In FUTON: encode both as receipts, glue
  on the shared token (`pr-note` read as the review's ticket) with
  `ThreeHalvesBlend.Gluing`, and ask for the verdict (`cascade` or `bag`)
  and for consistency of the pasting (Prop. 8 needs the second square to
  commute).
- **Expected.** A `cascade` verdict whose derived order is the concatenated
  chain. The machinery works, but on two chains it says nothing a person
  would not see. The first non-trivial input is `systematic-evidence-review`
  glued to `academic-survey`, the only pair with parallel structure and a
  shared role (`review-brief.md`).
- **Failure.** If the glue needs a token that neither playbook produces
  (here `pr-note` → `ticket` is a renaming), the composite is really a
  translation and should be handled by `translation/` (declare what is
  preserved and lost), not by pasting. That itself would be a finding about
  where FUTON's composition stops.

## 7. Where Alinery is simply ahead, or the theory adds nothing

- **It runs.** A per-repo daemon, durable execution records, recovery that
  requires explicit ownership transfer, fan-out and fan-in with collection
  closure, loop passes bound to fresh occurrences, coding exclusivity,
  capacity limits, and human gates with precise semantics, all tested
  (`alineryd/tests/playbook_v2_runtime.rs`, `semantic_events.rs`,
  `daemon_lifecycle.rs`). FUTON's cascade machinery is a scorer, Lean
  receipts over hand-encoded examples, and one APM arm with near-zero
  uptake.
- **Provenance by construction.** Every execution's exact inputs are
  recorded because the engine bound them. FUTON's R8 (no credit for
  unconsumed tokens) is typed `:unavailable` for lack of exactly this
  record.
- **Their formal model fits their implementation.** FAMs are modest:
  monotone filename sets, schemas, rollouts. The paper says what the engine
  enforces and what it does not ("Structural acceptance establishes neither
  the quality of a source assessment nor compliance…"). FUTON's theory is
  much richer and much less connected to running code; R10 and the v3 §7
  adequacy question (blend diagram → transition kernel) are open.
- **Restraint where FUTON has struggled.** "Do not add a general
  conditional-expression language, autonomous recovery system or
  proof-of-termination machinery merely to eliminate every pause." That is
  a ruling FUTON keeps having to make against itself (the
  single-action-ranking memory: a ruling that lives only in prose gets
  fixed at one level and returns at the next).
- **Human factors.** Distraction-free mode, notification choices,
  per-execution permission, written up as design principles (`VISION.md`,
  adopting the Resonant Computing Manifesto). FUTON's operator surface is
  Emacs plus 象, which is more expressive (speech-act marks) and less
  documented for anyone but Joe.
- **For workflows that are chains, the order theory is idle** (D2). The
  bundled corpus suggests most practitioner workflows are chains with a
  loop. The places FUTON's theory could pay are (i) choosing *between*
  processes (G), (ii) building one *per problem* from a library, and
  (iii) learning from receipts. None of the three has yet changed a real
  decision on the FUTON side.

## 8. Sources

Alinery (`~/code/external/alinery`, `cfc0d1e`):
- `alinery-app/src-tauri/alinery-core/src/playbook.rs` (parser/validator, selectors)
- `alinery-app/src-tauri/alinery-core/src/playbook_scheduler.rs` (graph, loops, collections, joins)
- `alinery-app/src-tauri/alinery-core/src/execution.rs` (lifecycle, permission, occurrences, `definition_identity`)
- `alinery-app/src-tauri/alinery-core/src/{paths,subtask,types,shared}.rs` (`.alinery/` layout, subtasks, `{{TICKET_FILE}}`)
- `alinery-app/src-tauri/mcp/src/main.rs` (26 MCP tools)
- `alinery-app/src-tauri/playbooks/*/playbook.md` (14 playbooks, 98 steps; esp. `systematic-evidence-review`, `academic-survey`, `spec`, `solution-exploration`, `build-playbook`)
- `docs/playbook-design-guidance.md`, `docs/playbook-sharing-contract.md`, `VISION.md`, `DESIGN.md`
- `whitepaper/finite-agent-machines.tex` (Dannenhauer, *Finite Agent Machines*), `whitepaper/examples/`
- https://github.com/DeleganceAI/alinery ; https://alinery.ai/ ; https://delegance.ai/

FUTON:
- `~/code/futon0/holes/missions/M-landscape-positioning.md` (§3 rules, §5, §6); `landscape/batch-C.md`
- `~/code/TN-deep-research-landscape-position-2026-07.md` (demarcation criterion; parked sibling packet)
- `~/code/futon3/library/cascade-construction/README.md`, `order-by-what-each-step-needs.flexiarg`
- `~/code/futon3/library/translation/README.md`, `hygiene/README.md`, `cascades/declared-skeleton.flexiarg`, `snatch/README.md`
- `~/code/futon3/library/workshop/LAYER4-PROCESS.md`, `workshop/IAD-ADAPTER.md`
- `~/code/futon3/library/meta/meta-outer-policy-cascade.edn`
- `~/code/futon3/checks/playout_snatch.clj`
- `~/code/futon2/holes/E-aif-cascade.md` (MAP table, rulings R1–R10)
- `~/code/futon2/holes/labs/wm-contract/CASCADE-SPEC-v2-2026-10-01.md`, `CASCADE-SPEC-v3-draft-2026-10-05.md`
- `~/code/futon2/src/futon2/aif/meta_outer_policy.clj`, `cascade_shape_g.clj`
- `~/code/mathlib4/DarkTower/WarMachine/{CascadeOrder,ConstructionReceipt,ThreeHalvesBlend,RCascadeReceipt,EpistemicValue}.lean`
- `~/code/futon3c/holes/PLAN-H3-wire-the-cascade.md`, `PLAN-apm-cascade-demo-instance.md`, `GROUND-CONTROL.md`
- `~/code/futon3c/holes/technotes/TN-APM-cascades-exist-unused.md`, `TN-fable-F66-F70-review.md`, `TN-fable-F81-F168-review.md`
- `~/code/futon3c/emacs/xiaoxiang-preview.el` (`xiaoxiang-mark-keys`, PBASE stages)
