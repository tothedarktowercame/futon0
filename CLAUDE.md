# Futonic Development Practices

These instructions apply to all projects under ~/code/. Project-specific
CLAUDE.md files add to (not replace) these practices.

## 1. Evidence-Backed Design

Do not design in a vacuum. Before implementing a system:

1. **Extract evidence** from the predecessor system or problem domain. Record
   bug patterns, fix patterns, implicit invariants, and anti-patterns with
   concrete references (commit hashes, issue numbers, incident reports).
2. **Select patterns** from the pattern library (`futon3/library/`) that
   address the observed tensions. Write a PSR for each selection.
3. **Trace every module** to the evidence and pattern that motivated it.

The goal is that any future agent can answer "why was this built this way?"
by following the chain: evidence -> pattern -> PSR -> module -> test -> doc.

## 2. Traceability Chain

Every project that reaches "prototype" stage should have a traceability
document mapping:

```
Evidence (commits/incidents) -> Pattern -> PSR/PUR -> Module -> Test -> Doc -> Error shape
```

Each module's namespace docstring should reference:
- Which invariant or tension it serves
- Which pattern it implements
- Which theory it draws from

Example:
```clojure
(ns my.project.auth
  "Layer 3 authorization gate.

   Invariant: I3 (errors surface at the layer that caused them).
   Pattern:   storage/guardrails-vs-tooling
   Theory:    futon-theory/error-hierarchy")
```

## 3. Pattern-Theoretic Specification

Every project must have a specification grounded in patterns and theory before
implementation begins. The specification defines:

- Which invariants the system must uphold
- Which tensions exist and how each is resolved
- Which patterns from the library apply and why

Code that falls outside the specification is a **stop-the-line** problem.
Do not implement features, modules, or behaviors that are not covered by
the spec. If new work is needed that the spec does not cover, stop and
extend the specification first — add the pattern, write the PSR, update
the traceability chain — then implement.

## 4. PSR/PUR Discipline

Every non-trivial design choice gets:

- A **PSR** (Pattern Selection Record) before implementation — which pattern,
  why, what alternatives were considered.
- A **PUR** (Pattern Use Record) after implementation — what happened,
  prediction errors, notes.

PSR/PUR records live in the mission/lab directory structure (typically
`futon3/holes/labs/<project>/psr/` and `pur/`). See `futon3/CLAUDE.md` for
the full PSR/PUR format.

## 5. Mission Documents

Long-running work should be tracked in a mission document with:

- **Scope in / scope out** — bounded, explicit
- **Time box** — prevents unbounded expansion
- **Exit conditions** — what "done" looks like, concretely
- **Checkpoints** — appended after each significant milestone:
  ```
  ### Checkpoint N — YYYY-MM-DD
  **What was done:** [bullet list]
  **Test state:** N tests, M assertions, 0 failures
  **Next:** [what comes next]
  ```

If a mission grows beyond its scope, split it rather than expanding.

## 6. Test Discipline

Tests are evidence artifacts in the traceability chain, not just regression
checks. Every module must have tests that prove its invariants — a module
without an invariant proof is not complete.

Tests do not need to pass on every commit during active development. But
every mission checkpoint must record the test state (count, assertions,
failures), and **all tests must pass at checkpoint boundaries**. A checkpoint
with failing tests is not a checkpoint.

## 7. Review Checklist

Apply these checks to every batch of generated code. Adapt the specifics to
the project, but the categories are universal:

1. **Error contract correctness** — Do errors follow the project's standard
   shape? Are they attributed to the right subsystem/layer?
2. **Execution ordering** — If the system has a pipeline or gate sequence,
   is the order correct? Does an early failure prevent later stages from running?
3. **Test sufficiency** — Does every module have at least one test proving its
   core invariant? Are edge cases covered? Is test isolation correct (fixtures,
   setup/teardown for shared state)?
4. **Architectural alignment** — Do new modules match the documented module
   map and pattern assignments? Are protocol/interface contracts honored?
5. **Dependency hygiene** — Are all dependencies real and resolvable? No
   phantom packages, no version conflicts?

## 8. Checkpointing Companion Repos

If work spans multiple repos (e.g., a project repo + a pattern/mission repo),
commit and push both at each checkpoint. It is easy to forget the companion
repo — make it a habit.

## 9. Hard Lines (No Workarounds)

The following are never acceptable, regardless of context or justification:

- **Never weaken an invariant for convenience.** If the spec says writes must
  be durable before success, you do not add a "fast path" that skips
  durability. If identity must be unique, you do not add a "soft duplicate"
  mode. An invariant that can be bypassed is not an invariant.

- **Never propose a "tradeoff" that violates the specification.** A tradeoff
  is a choice between two valid approaches within the spec. Relaxing a
  constraint the spec requires is not a tradeoff — it is a defect. If you
  believe the spec is wrong, stop and make the case to change the spec.
  Do not route around it.

- **Never add --no-verify, --force, or equivalent flags to bypass safety
  checks.** If a check is failing, fix the cause. Skipping the check
  creates the exact class of silent failure the system was designed to
  prevent.

- **Never silently swallow errors.** Every error must surface at the layer
  that caused it with enough context to diagnose. Catching and ignoring
  exceptions, returning nil where an error should throw, or logging without
  propagating are all forms of silent failure.

- **Never implement outside the specification.** If the work isn't covered
  by the pattern-theoretic spec, stop. Extend the spec first. Unspecified
  code is unreviewed design — it will drift.

## 10. Multi-Agent Coordination (Optional)

When two or more agents work on the same repo (e.g., Claude interactive +
Codex async), the following structure helps:

### Roles

- **Reviewer agent**: Reviews all code before merge, owns critical fixes,
  owns traceability and documentation artifacts.
- **Implementer agent**: Scaffolds modules, generates bulk code, addresses
  significant/minor review items.

**Rule: the implementer never self-approves.** Every batch of generated code
gets a structured review before it is considered done.

### Handoff Protocol

After the reviewer finishes a review round:

1. Fix critical items directly (they block correctness).
2. Write remaining items into `AGENTS.md` at the repo root with:
   - Task ID and severity (S = significant, M = minor)
   - File path(s) affected
   - What the fix should accomplish
   - Which files the reviewer has already modified (hands off)
3. The implementer picks up `AGENTS.md` and works the list.
4. The reviewer pulls and re-reviews. If items remain, fix them directly
   rather than starting another round-trip.
