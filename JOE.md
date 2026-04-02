# Joe's Active Context

## Current Missions

**M-structural-law** (futon3c) — Universal invariants as self-representing stack layer.
Three phases, ten Codex handoffs. All coding via GitHub issues.

**M-repl-wins-over-cli** (futon3c) — Make the Emacs REPL surfaces strictly
better than the legacy CLI, closing every UX gap so CLI usage drops to zero.
Phase 1 (basics) largely landed 2026-03-29; Phase 2 (cursor-sensor, scroll,
multi-base, frame inspector) in progress. See `futon3c/holes/missions/M-repl-wins-over-cli.md`.

## Active Patterns (PSR 2026-03-10)

Sigil string: `代内示功令马`

### Per-Handoff Patterns

| Sigil | Pattern                   | Guidance                                                                                                      |
|-------|---------------------------|---------------------------------------------------------------------------------------------------------------|
| 代    | handoff-preserves-context | Each GitHub issue carries full context: :in/:out files, shapes, test expectations. No "you know what I mean." |
| 内    | scope-before-action       | Sharp boundaries on every handoff. Codex works inside the box, not around it.                                 |
| 示    | evidence-over-assertion   | PRs come back with test counts and violation outputs, not "I think this works."                               |

### Coordination Patterns

| Sigil | Pattern               | Guidance                                                                                  |
|-------|-----------------------|-------------------------------------------------------------------------------------------|
| 功    | coordination-has-cost | Claude calls are expensive this week. Strategy only, no code review that tests can cover. |
| 令    | student-dispatch      | Codex handoffs ARE student-dispatch: context, dead ends, directions, report format.       |
| 马    | mission-lifecycle     | Running M-structural-law through its phases — carry the pattern being instantiated.       |

## Operational State

- Claude token budget: 93% used, resets in ~3 days
- Mode: Codex-primary, Claude for strategy only
- All 14 repos clean and pushed (futon-sync status: 2026-03-10)
- Server running refactored code (futon3c, port 6768 Drawbridge)
- Tickle + FM conductor default OFF at boot (use --start-tickle / --start-fm)

## UI Discipline Notes (2026-03-29)

- Preferred live surface: Emacs-native attached lanes, not ad hoc CLI Codex/Claude.
- For Codex, prefer attaching to `codex-1` from Emacs over starting standalone CLI sessions.
- Mirror mode is for debugging or renderer work, not the canonical working surface.
- `Frame 0` / session anchor should carry non-transcript context: working directory, transport, rollout file, session id, evidence base, and similar runtime facts.
- Turn frames should carry any later cwd changes or other session-context deltas; `Frame 0` stays immutable once opened.

### Practical Rule

- Treat `cd futon3c && make codex-repl` as a legacy launch route.
- Treat raw CLI Codex/Claude use as incurring **interaction evidence debt** unless the turn stream is mirrored into a surface that logs evidence canonically.
- The target invariant is: **all substantive agent turns are logged as evidence**.

### What To Do

- Start from the Emacs surface when possible, especially when the work matters enough to keep.
- If `codex-1` is already up, attach to it rather than opening a fresh CLI session.
- Use mirror mode only to inspect or improve the Emacs UI around an already-running Codex session.
- If CLI use is unavoidable, record at least:
  - reason for bypassing the Emacs/evidence surface
  - rollout path or session id
  - working directory
  - whether the debt is repayable by backfill or merely should be counted

### Near-Term Goal

- Add instrumentation that tracks evidence debt caused by off-surface CLI turns.
- Keep that debt visible enough that CLI convenience feels like a measured trade, not free ambient leakage.

## Claude REPL Improvements — Testing & Extension Plan (2026-03-29)

The codex-repl structural improvements (defvar-local, open-instance pattern,
attach-agent with completing-read) have been ported to claude-repl.el.
The file went from 712 → 773 lines. Nothing was removed; everything is additive.

### What To Test

1. **Basic smoke test** — `M-x claude-repl` should still work exactly as before.
   Verify: buffer appears, agent registers, send a message, get a streamed reply.

2. **Multi-buffer isolation** — Open two Claude REPL instances (e.g. from two
   workspace daemons, or use `claude-repl-attach-agent` to open a second buffer
   on a different agent). Verify evidence state vars don't leak between buffers.
   The `defvar` → `defvar-local` change is the fix; if evidence IDs or session
   IDs from buffer A appear in buffer B's evidence posts, the change didn't take.

3. **`M-x claude-repl-attach-agent`** — This is the new command (replaces the
   old `claude-repl-connect` string prompt). It should:
   - Hit the Agency API to fetch registered claude agents
   - Present them via `completing-read` (tab-completion in the minibuffer)
   - Open a `*claude-repl:<agent-id>*` buffer bound to that agent
   - Rebind the socket so blackboard calls route here
   - If the Agency API is down, fall back to a raw string prompt

4. **`claude-repl-connect` backward compat** — Still works, now delegates to
   `claude-repl-attach-agent`. If you have keybindings or scripts calling it,
   they should keep working.

5. **Session file directory creation** — If `claude-repl-session-file` points
   to a path whose parent directory doesn't exist yet, the REPL should create
   it automatically (via `make-directory ... t`). Previously this would error
   silently and lose the session ID.

### What To Extend (if needed)

- **Multi-base API discovery**: codex-repl has `codex-repl--resolved-api-base`
  that probes multiple candidate URLs with health-check fallback. Claude-repl
  still uses a single `claude-repl-api-url`. If you hit situations where the
  Agency is reachable on a non-default port (e.g. laptop with FUTON3C_PORT=47070),
  this would be the next thing to port. The pattern is already in codex-repl —
  see `codex-repl--resolved-api-base`, `codex-repl--base-reachable-p`.

- **Drawbridge eval helper**: codex-repl extracted `codex-repl--drawbridge-eval`
  as a general Clojure-eval-via-Drawbridge function. Claude-repl still has inline
  Drawbridge code in `claude-repl--reset-via-drawbridge`. If you add more
  Drawbridge interactions (registry queries, runtime inspection), factor this out
  the way codex-repl did.

- **Profile system**: codex-repl now has `codex-repl-open-profile` and
  `codex-repl--reference-profile-defaults` for named test profiles with isolated
  session files and working directories. If you want similar isolation for
  Claude (e.g. a reference-trial Claude instance hitting a different API stack),
  the pattern is ready to port.

- **Runtime state tracking**: codex-repl tracks `runtime.process` events from
  the CYDER lane system. Claude doesn't have this, but if futon3c ever exposes
  process-level state for Claude agents, the dashboard/progress patterns are
  already proven in codex-repl.

### Files Changed

- `emacs/claude-repl.el` — all changes here, no agent-chat.el modifications needed

## What Landed This Session (2026-03-10)

- `proof_logic.clj` — core.logic invariant layer for Proof Peripheral (13 tests, 29 assertions)
- `agency/logic.clj` — core.logic invariant layer for Agency (10 tests, 20 assertions)
- `M-structural-law.md` — mission file for universal invariants
- `M-invariant-violations.md` — violation ledger (V-1..V-5 peripheral asymmetries)
- `README-done.md` — agent completion signalling documentation
- Bootstrap defaults changed: tickle/FM conductor off unless --start-tickle/--start-fm
- FM-001b proof state moved to futon6 (canonical location)
- All repos cleaned, committed, pushed

## Detailed working copies of the patterns in Joe's backpack:

@flexiarg agent/coordination-has-cost
@title Coordination Has Cost
@sigils [🎎/功]
@audience agent architects, workflow designers, futon developers
@tone foundational
@style pattern

! conclusion: Involving other agents or humans has overhead; factor it into decisions.
  + context: An agent can work alone or involve others (other agents, human supervisors, external services).
  + IF:
    The agent wants to leverage external capabilities or get authoritative answers.
  + HOWEVER:
    Coordination is not free: it takes time, consumes attention, introduces latency, and may require context transfer. Treating it as free leads to over-reliance or spamming.
  + THEN:
    Model coordination as an action with explicit cost (latency, tokens, human attention units). Include this cost in decision-making: sometimes working with partial local knowledge is better than waiting for external input, unless the risk of error is higher than the coordination cost.
  + BECAUSE:
    Cost-aware coordination prevents both under-use (never asking for help when stuck) and over-use (constant interruption). It respects the resources of collaborators.
  + NEXT-STEPS:
    - Inventory coordination actions your agent can take (ask human, call service, delegate to agent).
    - Assign cost estimates to each and include them in action selection.
    - Track actual coordination costs and update estimates based on experience.
    - Add coordination cost fields to fulab session events (latency, token cost) and use them in action selection.
@flexiarg agent/evidence-over-assertion
@title Evidence Over Assertion
@sigils [📁/示]
@audience agent architects, workflow designers, futon developers
@tone foundational
@style pattern

! conclusion: Claims require grounding; an agent that asserts without evidence cannot be audited or trusted.
  + context: An agent makes claims about what it did, what it found, or what is true.
  + IF:
    The agent's outputs need to be verifiable by other agents, humans, or future selves.
  + HOWEVER:
    Assertions without anchors are indistinguishable from confabulation. Observers cannot tell if the agent is reporting reality or generating plausible-sounding noise.
  + THEN:
    Require every substantive claim to point to evidence: a file path, a trace event, a test result, a quotation. If evidence is unavailable, emit the claim as provisional with an expiry and a verification path. Make "unsupported claim" a detectable violation, not just a style preference.
  + BECAUSE:
    Evidence transforms agent output from "trust me" to "check this." It enables verification, correction, and cumulative trust-building.
    + evidence: Fulab PUR/PSR templates require anchors/certificates (`src/futon3/fulab/pattern_competence.clj`), and claim verification is enforced via `fulab-pattern-claim` + `fulab-pattern-check`.
  + NEXT-STEPS:
    - Identify claim types in your agent's output (facts, completions, diagnoses).
    - For each type, define what counts as adequate evidence.
    - Add validation that flags claims missing their evidence anchors and records provisional claims separately.
    - Auto-attach tool/file anchors from `lab/tool-uses` to PUR/PSR records and surface missing evidence in `fulab-session-report`.
@flexiarg agent/handoff-preserves-context
@title Handoff Preserves Context
@sigils [📤/代]
@audience agent architects, workflow designers, futon developers
@tone foundational
@style pattern

! conclusion: When passing work to another agent or future self, transfer enough state that the receiver doesn't restart from scratch.
  + context: An agent's work will be continued by another agent, a human, or the same agent in a future session.
  + IF:
    Continuity matters: the work is incremental, context is expensive to rebuild, or progress would be lost.
  + HOWEVER:
    Without explicit handoff, successors face cold starts: they repeat discoveries, miss constraints, or contradict prior decisions.
  + THEN:
    Define a handoff record that captures: current state hypothesis, decisions made and why, open questions, scope boundaries, next actions, evidence/trail anchors, and any provisional-claim ledger. Emit it at session end or when transferring to another agent.
  + BECAUSE:
    Handoffs convert ephemeral session state into persistent project state. They enable distributed and asynchronous work without context collapse.
    + evidence: Fulab exports session context to `lab/raw`, `lab/stubs`, and `lab/trace` (`dev/lab-export-claude.clj`), plus summaries via `fulab-summary` and `fulab-session-report`.
  + NEXT-STEPS:
    - Identify handoff points in your agent's lifecycle (session end, escalation, delegation).
    - Define what context is essential vs. nice-to-have for each handoff type.
    - Test handoffs by having a fresh agent resume and measuring ramp-up cost.
    - Extend lab stubs/summaries to capture open questions, scope boundaries, and provisional claims for resumption.
@flexiarg agent/scope-before-action
@title Scope Before Action
@sigils [📐/内]
@audience agent architects, workflow designers, futon developers
@tone foundational
@style pattern

! conclusion: Declare the territory before entering it; unbounded scope makes progress unmeasurable.
  + context: An agent is about to begin work on a task that could touch many things.
  + IF:
    The agent needs to make progress that can be verified and bounded.
  + HOWEVER:
    Without declared scope, "done" is undefined, side effects are unpredictable, and observers cannot tell if the agent is on track or wandering.
  + THEN:
    Before substantive action, state the scope: what files, concepts, or regions are in-bounds; what is explicitly out-of-bounds; what the exit condition is. Treat scope violations as requiring explicit expansion, not silent drift. Scope defines the territory; budgets and pauses govern what happens inside it.
  + BECAUSE:
    Declared scope makes progress measurable, side effects auditable, and completion definable. It also protects the broader system from unintended changes.
    + evidence: Fulab HUD prompts require a pre-action plan (`src/futon3/fulab/hud.clj`), and scope boundaries are spelled out in `CLAUDE.md`.
  + NEXT-STEPS:
    - Add scope declaration to your agent's initialization or planning phase.
    - Log scope boundaries and check actions against them.
    - Design a scope-expansion protocol for when legitimate work requires broadening.
    - Record explicit in/out-of-scope fields in fulab session metadata and log scope-expansion events.
@flexiarg agent/student-dispatch
@title Student Dispatch
@sigils [🎓/令]
@keywords dispatch, explore, student, grad, investigate, try, report, handoff, negative-knowledge, dead-end, direction, structured, findings
@audience proof engineers, research coordinators, multi-agent orchestrators
@tone collegial
@factor Right effort (sammā vāyāma)
@references [agent/handoff-preserves-context agent/scope-before-action math-informal/structural-obstruction-as-theorem]

! conclusion:
  Dispatch an agent as a student-explorer, not a junior-developer-verifier. Give them the
  problem context, what's been tried and why it failed, candidate directions to explore,
  and a structured report format. Let them discover rather than confirm.

  + context: You have a research problem with multiple unexplored directions. You could
    hand each direction to an agent (Codex, another Claude instance, a human collaborator)
    for parallel exploration. The temptation is to specify exactly what to compute and what
    answer to expect — but this discards the agent's ability to discover surprises.

  + IF:
      The remaining work involves genuine exploration (multiple approaches, unknown outcomes)
      rather than rote verification (check that X equals Y). The dispatched agent has the
      capability to reason about the problem, not just execute a script.

  + HOWEVER:
      Unrestricted exploration wastes time. "Go think about this problem" is too vague.
      The student needs: (1) what's known, (2) what's been tried and failed with explicit
      reasons, (3) candidate directions with rationale, (4) what NOT to try, and (5) a
      report format. The negative knowledge (what not to try) is as important as the
      positive — it prevents the student from repeating dead ends that took the advisor
      hours to discover.

  + THEN:
      Structure the dispatch as:

      1. **Context**: The problem statement and what's been proved so far. Enough for the
         student to understand the landscape without reading every prior document.

      2. **Dead ends with reasons**: Approaches that were tried and WHY they failed.
         Not just "SOS didn't work" but "SOS is structurally blocked because the interior
         zero forces all multipliers to vanish." This is where structural-obstruction-as-
         theorem and exhaustion-as-theorem pay off — they compress dead ends into
         transmissible theorems.

      3. **Directions to explore**: 2-4 candidate approaches, each with a sentence on why
         it might work. The student may pivot based on findings — that's fine.

      4. **Report format**: What to come back with. Not pass/fail but structured findings:
         what was tried, what happened, what surprised, what new dead ends were discovered,
         what partial results were obtained.

      5. **Success criteria**: What would constitute progress. "Find a universal c_0 >= 0.01"
         is better than "prove the theorem." Partial success is success.

  + BECAUSE:
      A verification dispatch ("confirm X") caps the return at 1 bit: yes or no. An
      exploration dispatch ("investigate X") can return discoveries, new dead ends,
      partial results, unexpected connections, and reformulations. The student may find
      something the advisor didn't think to look for. The negative-knowledge transfer
      prevents wasted work while the open-ended framing permits serendipity.

  + FAILURE-MODES:
      - Too little context: student wastes time rediscovering known results.
      - Too much prescription: student confirms rather than explores.
      - No report format: student returns unstructured prose that's hard to integrate.
      - Missing dead ends: student repeats the advisor's failed approaches.
      - Overloaded dispatch: giving 10 directions when 2-3 is the right scope per agent.

  + EVIDENCE:
      P4 n=4 Stam inequality (futon6, Feb 2026): Codex was dispatched in verification
      mode ("confirm these eigenvalues, verify this identity"). Returned pass/fail.
      Separately, Codex was dispatched in exploration mode for PHCpack — "find all critical
      points of this system" — and returned 12 in-domain CPs with full case classification,
      discovering the mixed_volume=0 issue independently. The exploration dispatch produced
      richer results per unit of effort.

      P6 epsilon-light subsets: the Claude→Codex handoff note contained dead ends
      (6 technique failures with reasons) and candidate directions (BSS greedy, expander
      decomposition, structural analysis). This is the right format for student dispatch;
      the missing piece was framing it as "go explore these" rather than "verify my claims."

  + NEXT-STEPS:
    next[Write the dispatch note: context, dead ends, directions, report format.]
    next[Send one direction per agent if parallelizing.]
    next[On return: integrate findings, update the dead-end list, re-dispatch if needed.]
@flexiarg futon-theory/mission-lifecycle
@title Mission Lifecycle (State Machine)
@sigils [🔃/马]
@tokipona sike pali
@truth 马

+ CONTEXT:
    Missions progress through defined states. Without a formal lifecycle,
    missions drift indefinitely or are abandoned without evidence. The
    proof-path pattern governs individual changes; this pattern governs
    the mission as a whole.

+ IF:
    You are defining, tracking, or transitioning a mission.

+ HOWEVER:
    Missions may be blocked by dependencies, split into sub-missions,
    or merged when scope overlaps. The lifecycle must accommodate these.

+ THEN:
    Apply the mission state machine:

    ```
    :greenfield → :scoped → :active → :blocked → :review → :done
                     ↑          ↓         ↓
                     └──────────┴─────────┘ (can regress)
    ```

    State definitions:
    - :greenfield - Idea captured, no work plan
    - :scoped - Success criteria, boundaries, dependencies defined
    - :active - Work in progress, owner assigned
    - :blocked - Waiting on dependency (cite blocker)
    - :review - Work complete, awaiting verification
    - :done - Success criteria met, evidence recorded

    Transition requirements:
    - greenfield → scoped: Mission doc has success criteria + scope
    - scoped → active: Owner assigned, dependencies clear
    - active → blocked: Blocker cited with link to blocking mission
    - active → review: All success criteria addressed
    - review → done: Evidence verified by reviewer
    - Any → :abandoned: Explicit decision with rationale

+ BECAUSE:
    - Explicit states prevent invisible drift
    - Transition requirements create evidence trail
    - Blocked states surface dependency graphs
    - Review gates ensure verification before closure

+ GOVERNANCE:
    Mission state transitions should be recorded:
    - In mission doc header (Status field)
    - Optionally in MUSN activity stream
    - With timestamp and agent attribution

+ NEXT-STEPS:
    - Check current mission state
    - Verify transition requirements are met
    - Update mission doc with new state
    - If blocked, link to blocker mission

@related futon-theory/proof-path, futon-theory/mission-scoping, futon-theory/mission-dependency, futon-theory/mission-interface-signature
