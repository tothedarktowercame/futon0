**Status:** OPEN (drafted 2026-05-04; sibling to M-the-futon-stack).
**Target completion:** open-ended — paced by the four levels' evidence, not a calendar.
**Home-repo:** futon0 (cross-stack — pattern library lives in futon3, parser in futon3a, peripheral surface in futon3c, categorical infrastructure in futon5, entity-graph in futon1a, code-receipts touch the whole codebase; futon0 is the cross-repo coordination home per `single-locus/mission-home`).
**Cross-references:**
- `futon0/holes/missions/M-the-futon-stack.md` — patterns are the substrate the homeostatic agent maintains itself with; this mission is the deep-dive on the *pattern* aspect of self-maintenance.
- `futon0/holes/missions/M-the-futon-stack.md` §"Cross-cutting threads / CCT-1 — Canonical parser per data-object class" — the canonical-parser invariant that grounds the whole mission.
- `futon3/holes/missions/M-pattern-retrieval-calibration.md` — A→B retrieval calibration; ships the canonical parser P-1 + clause-vocabulary survey + reshape rulebook that this mission inherits.
- `futon3/holes/missions/M-pattern-application-diagnostic.md` — typed pattern shape `{:context :tension :move :witness-shape :domain}`; the geometric `(T, ∇, Δ)` commitment; "demonstrable value" as line integral. This mission's Level-4 (code receipts) is the operational form of that commitment.
- `futon3/holes/excursions/E-pattern-peripheral.md` — original Codex-handoff excursion for the peripheral; reframed under this mission as the Level-1 Sokoban floor.
- `futon3/holes/excursions/E-clause-vocabulary-survey.md` — frequency-tallied clause vocabulary across 919 patterns; the empirical ground for canonicalisation.
- `futon3/holes/excursions/E-clause-vocabulary-reshape.sexp` — the rulebook with verb taxonomy (replace / substructure / metadata / facet / reject) and the conclusion-aliases set.
- `futon3a/src/futon/peripheral/pattern_author.clj` — the working Level-1 Sokoban mockup. `submit-draft!` is the single bound action. This mission specifies what it should grow into.
- `algorithms/author-flexiarg-core-and-wire-discoverability.md` — the agent-prep algorithm for Level-2 (algorithm-guided authoring).
- `futon5/src/futon5/ct/dsl.clj` — the Functor / Diagram / Category / NaturalTransformation DSL with a `design-pattern-category`. Substrate for Level-3 (functorial admission).
- `futon5/scripts/pattern_to_wiring.clj` — pattern-registry-entry → wiring-diagram generator; output in `resources/xenotype-wirings/`. Engine for Level-3.

# M-patterns-done-right: Patterns threaded through every layer

## 1. IDENTIFY

### Thesis

**Patterns are first-class objects of the homeostatic agent's self-maintenance, threaded from authoring (peripheral) through library structure (categorical wiring) to code-level receipts.** The agent doesn't *describe* its own patterns and then *separately* write code; patterns and code form one substrate, and a pattern is honestly "applied" only when there's a verifiable receipt linking the code that was built to the pattern's claim.

The mission is the deep-dive on the *pattern* aspect of M-the-futon-stack's homeostatic-agent thesis. M-the-futon-stack names ten cognitive-faculty shapes the agent needs to maintain itself; M-patterns-done-right asks: **what does each pattern have to be, structurally, so those faculties can compose into a working homeostat rather than a pile of well-intentioned text?**

### What this mission is NOT

- Not a rewrite of the pattern library. The 985 patterns currently in XTDB stay where they are; this mission upgrades the *discipline* under which new patterns are authored and existing ones are referenced.
- Not a category-theory research paper. The categorical framing (Level 3) is a working substrate built on `futon5/src/futon5/ct/dsl.clj`, not an exposition of CT for its own sake.
- Not a replacement for M-pattern-retrieval-calibration. That mission ships the canonical parser + reshape rulebook (the engine); this mission specifies how the engine threads through authoring → library → code.
- Not a mandate to retrofit code receipts (Level 4) onto historical commits. The receipts discipline starts from a named cutover; what came before stays as it was.

### The four levels

Each level is a graduation step on top of the previous. The peripheral mockup (Level 1) ships today; the rest are the named long-arc work.

#### Level 1 — Sokoban over text shape (operational floor — landed 2026-05-04)

Pattern peripheral as a *capability envelope*, not a downstream validator. An agent inside the surface can submit a draft via `submit-draft!`; either it lands as a canonical .flexiarg or it returns structured violations and writes nothing. No leakage — non-canonical patterns cannot be emitted from inside the peripheral.

**What's working today** (`futon3a/src/futon/peripheral/pattern_author.clj`, 21 tests / 86 assertions):
- Validates parser-status, conclusion presence (any of the four aliases — conclusion / claim / summary / instantiated-by per the rulebook), top-level clauses against the canonical seven.
- Refuses with structured `{:kind :detail :where}` violations on missing-header, missing-conclusion, non-canonical-top-level-clause, empty-draft.
- Admits canonical drafts (including those using `! summary:` or `! instantiated-by:` as conclusion-aliases) and writes the file.
- Co-located with the canonical parser (P-1) so the whole Sokoban predicate is one require away.

**What's missing for Level-1 completion**:
- Wiring into futon3c's session-mode dispatch so an agent *enters* the peripheral and has `Edit`/`Write` constrained for `library/**.flexiarg` paths.
- Surface contract on entry: "you are in pattern-author mode; your only legal write is via `submit-draft!`."
- Refusal-feedback pointers at the rulebook + the algorithm + the canonical parser so an agent can self-correct.

#### Level 2 — Algorithm-guided authoring

Reuse `~/code/algorithms/author-flexiarg-core-and-wire-discoverability.md` as the agent's *preparation step* before they invoke `submit-draft!`. The algorithm names lifecycle modes (new vs existing), companion flexiargs (`algorithms-own-procedural-spines / do-not-recreate-the-wheel / edge-types`), the carrier boundary, the orthogonalisation pass, and discoverability wiring. Soft setup, not enforcement; the peripheral remains the hard gate.

**What's missing for Level-2 completion**:
- Surface-contract sentence on peripheral entry: "before invoking `submit-draft!`, follow `algorithms/author-flexiarg-core-and-wire-discoverability.md`; the algorithm names companion flexiargs you should consult."
- Optional: peripheral checks the proposal's typed-edge headers (`@isa / @hasa / @childof / @references`) for resolvability before admission.

#### Level 3 — Functorial admission

The peripheral's predicate isn't "does the draft parse into seven components?" but **"do the draft's required morphisms compose cleanly with the existing futon5 pattern-category?"** A new pattern is admitted iff it lands as a new object whose morphisms compose with sibling objects without violating categorical laws (associativity, identity, naturality of the relevant transformations). On success, the pattern lands AND a wiring diagram is generated under `resources/xenotype-wirings/`.

**Why this gives "more than the sum of its parts"**: composition is what makes a pattern library a *language*. Two patterns that share a context-morphism, an if-morphism, or a then-morphism compose into a third pattern with shared structure. The library stops being a flat catalog and becomes a graph of morphisms whose paths *are* design moves.

**What's missing for Level-3 completion**:
- Decide which of the canonical seven are *required* morphisms vs *optional* ones, per pattern-category laws.
- Decide which categorical laws must hold (associativity is straightforward; naturality of `:tension` ↔ `:move` is the harder claim).
- Wire the futon5 ct/dsl as the peripheral's admission predicate — the reshape rulebook becomes the *projection* into the category, the categorical laws become the *admission test*.
- Decide what failure feedback looks like: "your `:tension` morphism's source object doesn't compose with `agent/intent-handshake-is-binding`'s `:tension` morphism — predicate scopes are disjoint."

This is research, not just engineering.

#### Level 4 — Pattern → code receipts

A pattern is honestly applied when there's a verifiable receipt linking the code that was built using it back to the pattern's claim. Today the link is via PSR/PUR notes (`futon3/CLAUDE.md`) plus per-turn evidence-mining (M-pattern-mining, M-pattern-retrieval-calibration); these are *advisory*. Level 4 makes the link *structural*.

**Receipt discipline (sketch)**:
- Every commit that touches code in a region with an active pattern-application carries a `Pattern: <ns>/<name>` footer (zero or more; multiple patterns are common).
- The footer joins the canonical parser + the M-pattern-application-diagnostic geometric substrate: the receipt names the pattern's `:witness-shape`, the commit's region of effect, and the implicit T-before / T-after pair.
- Emit `pattern-receipt` evidence per commit; the verify invariant `invariant-pattern-receipts-resolve` checks every receipt's pattern-ref resolves and every claimed witness can be re-derived from current code state.

**Why this is the long-arc payoff**: patterns become *measurable* — the IFR's "demonstrable value" property (M-pattern-application-diagnostic) graduates from claim to data. Mining + Haiku give per-turn evidence; receipts give per-commit evidence; together they triangulate which patterns the codebase *actually* embodies vs which are well-intentioned text.

**What's missing for Level-4 completion**:
- Footer parser (the pattern Joe already has in `M-bounded-in-flight-state` for `Block:` is the prior art — same shape: `^Pattern: <ns>/<name>$`, last occurrence wins).
- Commit-time advisory: pre-commit hook warns when committing in a region without a Pattern footer.
- Verify invariant + evidence emission.
- Geometric receipt: `T-before / T-after` derivation at commit time; line-integral check against the pattern's claim.
- Cutover discipline: receipts from this date forward; historical commits stay as they are.

### What this mission must verify

#### Q1 — Does the Sokoban actually constrain agents in practice?

The mockup refuses non-canonical drafts; an agent invoking `submit-draft!` directly will get refusals. But in normal authoring flow, agents reach for `Edit` or `Write` first — the peripheral isn't on the agent's tool surface yet. **Verification**: wire the peripheral into futon3c's session-mode dispatch; observe whether agents in pattern-author mode *actually* route through `submit-draft!` or whether they bypass with direct file writes. If they bypass, that's a tooling gap, not a design gap; tighten the dispatch.

#### Q2 — Do the four conclusion-aliases match how authors actually write?

The aliases set is `#{:conclusion :claim :summary :instantiated-by}`. The survey showed `:instantiated-by` has 64 occurrences across 9 files (devmap-prototype headers); the others are common. **Verification**: as new patterns land via the peripheral, tally which alias each author chose. If `:summary` is chosen 80% of the time, that's signal that "summary" is the natural authorial form and the rulebook should lead with it; if `:instantiated-by` is chosen exclusively for devmap entries, that's signal the alias has a typed meaning and should be promoted.

#### Q3 — Are the canonical seven the right components?

The clause-vocabulary survey shows the seven cover 82.6% of all clause occurrences in 919 patterns. But "covers usage" ≠ "is honest design vocabulary." **Verification**: the categorical-admission research (Level 3) tests whether the seven correspond to required morphisms in the design-pattern-category. If three of the seven turn out to be projections of one underlying morphism, the canonical set tightens; if a hidden morphism is required for composition, the set extends.

#### Q4 — Does Level 4 produce useful receipts or noise?

Per-commit `Pattern: <id>` footers are easy to require but trivial to game (claim a pattern; commit unrelated code). The real test: do the geometric receipts (T-before / T-after) actually correlate with claimed pattern application? **Verification**: hand-label a sample of commits with patterns they *should* claim, compare against what they *do* claim under Level-4 discipline, see whether the geometric witness can distinguish honest claims from drift. If the witness is weak, Level 4 collapses to advisory.

#### Q5 — What's the failure mode when a draft can't be canonicalised?

The Sokoban refuses; the agent re-drafts. But what if the agent's content genuinely doesn't fit the canonical seven? Three options: (a) the pattern is wrong-shaped (reject — the agent should write something else); (b) the canonical seven is incomplete (extend the rulebook); (c) a new namespace-canonical facet is needed (per the rulebook's existing facet machinery). **Verification**: catalogue refusals over the first month; for each, decide which option fits.

### Scope in / scope out

**In scope:**
- All four levels as named work, with Level 1 already shipped.
- The peripheral's session-mode wiring in futon3c.
- The categorical-admission research.
- The receipts discipline including footer parser, verify invariant, geometric witness check.
- Authoring 4–6 new flexiargs that capture the insights from this mission's drafting itself (test of the Sokoban as we go).

**Out of scope:**
- Library-wide retrofit. The 985 ingested patterns stay; new authoring goes through the peripheral.
- Cross-repo refactoring of the existing pattern infrastructure. M-pattern-retrieval-calibration owns retrieval; M-pattern-application-diagnostic owns the typed-slot lift; M-pattern-mining owns mining. This mission is the *integrative* arc, not a re-implementation.
- Replacing PSR/PUR. The receipts discipline (Level 4) supplements those, not replaces — PSR/PUR are reflexive; receipts are structural.

### Completion criteria

The mission completes when the operator can truthfully say:

1. **Authoring is constrained**: every new pattern in `library/` since the cutover landed via the peripheral, with a recoverable trail of refusals + admissions in evidence.
2. **The library is wired**: the futon5 design-pattern-category contains all 985+ patterns as objects; their morphisms compose without violation; admission of new patterns runs the categorical predicate.
3. **Code carries receipts**: every commit since the cutover that touches a pattern-claimed region carries a `Pattern:` footer; verify invariant `invariant-pattern-receipts-resolve` passes.
4. **The four levels are crystallised as patterns**: the operational insights from this mission (Sokoban, conclusion-aliases, categorical objects, code-receipts) are themselves flexiargs in `library/pattern-discipline/`, authored *through* the peripheral, validating the substrate.

### Risk register

- **Sokoban bypass via `Edit`/`Write`**: agents reach for the wrong tool first. Mitigation: futon3c session-mode dispatch tightens to refuse `Edit`/`Write` for `library/**.flexiarg` when in pattern-author mode.
- **Categorical research overruns**: Level 3 is genuinely open-ended. Mitigation: time-box the research arc; if no working admission predicate after three excursions, defer Level 3 and ship Levels 1+2+4.
- **Receipt spam / gaming**: agents add `Pattern:` footers without honest application. Mitigation: geometric witness check is the structural counter — claims without measurable T-before/T-after delta are flagged.
- **Cross-mission scope drift**: this mission can't own retrieval or mining or application-diagnostic. Mitigation: hard cross-refs to those missions; this one is the *integrator*.

### Owner and dependencies

- **Owner**: Joe — sole operator; defines the cutover for receipts discipline; judges completion criteria.
- **Co-author**: the agent itself (claude-N, codex-N — multi-session; this mission becomes self-referential as the four levels are themselves authored through the peripheral they specify).
- **Dependencies**:
  - M-pattern-retrieval-calibration §D-9..D-11 — canonical parser, rulebook, conclusion-aliases.
  - M-pattern-application-diagnostic — typed-slot schema, geometric commitment (T, ∇, Δ).
  - M-the-futon-stack §CCT-1 — canonical-parser invariant.
  - futon5's ct/dsl.clj and pattern_to_wiring.clj — Level-3 substrate.
  - `algorithms/author-flexiarg-core-and-wire-discoverability.md` — Level-2 prep step.
  - `futon3a/src/futon/peripheral/pattern_author.clj` — Level-1 mockup, working today.

## 2. MAP / DERIVE / ARGUE / VERIFY / INSTANTIATE / DOCUMENT

Like M-the-futon-stack, this mission's lifecycle phases cycle continuously per level. At any moment, Level 1 may be in VERIFY, Level 3 in MAP, Level 4 in IDENTIFY. The mission's overall "phase" is the integral over its levels' phases.

## Test-as-you-go: insights as flexiargs

Per Joe's framing 2026-05-04: each insight in this mission gets captured as a flexiarg in `library/pattern-discipline/`, authored *through the peripheral*. Refusals + admissions become evidence about whether the canonical-seven framing actually fits the insights we're encoding. Initial set:

- `pattern-discipline/peripheral-as-sokoban` — the Level-1 framing.
- `pattern-discipline/conclusion-required-others-recommended` — the conclusion-only-required discipline.
- `pattern-discipline/patterns-as-categorical-objects` — Level-3 framing.
- `pattern-discipline/pattern-to-code-receipts` — Level-4 framing.

These are the first round; more land as the mission progresses.

## Generalisation: peripheral as genus, per-artifact-class peripherals as species

The Sokoban architecture isn't pattern-specific. The structural genus is:

> **A pre-write capability envelope for a typed-artifact class, with a
> single bound `submit-draft!` action, a parser+rulebook engine that
> either canonicalises the draft or refuses with structured violations,
> and no other write-path exposed inside the surface.**

Per-artifact-class peripherals are species of this genus. The pattern
peripheral (this mission's Level 1) is the first.

### Callback to M-bounded-in-flight-state — FS→Git peripheral as a sibling species

`futon3c/holes/missions/M-bounded-in-flight-state.md` ships a *Block*
discipline: every commit lives inside one revolution of the futonic
loop (Identification / Specification / Elaboration / Passion /
Closure), with a `Block: <kind>-<YYYY-MM-DD>-<slug>$` footer in the
commit message naming the block. The block-id parser, the
`.futon-disposition.edn` shape, and the per-block phase predicates
are all already in place in futon3c.

That mission has been handled *classically* — pre-commit hooks,
commit-message parsers, evidence emission on commit. The architecture
is structurally the same Sokoban shape as the pattern peripheral:

| Pattern peripheral | FS→Git peripheral (Block discipline) |
|---|---|
| Input: `{author, target-path, draft-body}` | Input: `{author, intended-block-id, staged-paths, message-draft}` |
| Engine: canonical parser + rulebook + admitted-set | Engine: `Block:` footer parser + phase predicates + staged-change checks |
| Refusal: `:missing-conclusion`, `:non-canonical-clause`, ... | Refusal: `:missing-block-id`, `:phase-mismatch`, `:closure-without-passion`, ... |
| Output: canonical `.flexiarg` written via classical `spit` | Output: canonical `git commit` lands with `Block:` footer |
| Witness (Level 4): pattern→code receipts | Witness: block-as-mana-award per the Block-as-futonic-revolution pattern |

The interesting overlap with classical: the peripheral's *predicate*
is mostly classical (regex, file-presence, FS-state checks);
LLM-involvement only enters for cases like "did this commit's diff
actually embody the named pattern?" where geometric-witness reasoning
kicks in. That's the same Level-4 territory this mission names. **A
FS→Git peripheral and the pattern peripheral could share the same
Level-1/Level-4 architecture**, with different parsers and different
witness shapes — the "pre-write capability envelope with structured
refusal" is the genus, and per-artifact-class peripherals are species.

### Possible follow-on excursion

When this mission's Level 1 wiring lands in futon3c session-mode
dispatch (the peripheral becomes a real session surface, not just a
function call), the same dispatch substrate could host an FS→Git
peripheral for Block discipline. Excursion shape:
`E-fs-git-peripheral.md` (futon3c), companion to
`E-pattern-peripheral.md` (futon3) — both species under one
peripheral-as-genus framing.

Other candidate species worth listing here so future authors notice:

- **Mission-doc peripheral** — author a `holes/missions/M-*.md` only
  via `submit-mission-draft!`; engine validates required sections,
  cross-refs, and status-line shape.
- **Disposition peripheral** — author `.futon-disposition.edn` only
  via the disposition util namespace's `submit-disposition!`; engine
  validates against the `bounded-disposition` faculty's vocabulary.
- **Evidence peripheral** — append to the futon1a evidence stream
  only via `submit-evidence!`; engine validates `event` field against
  the registered event taxonomy.

Each species shares the genus's no-leakage / structured-refusal /
single-bound-action discipline; the parsers and predicates differ.
The genus may eventually crystallise as its own pattern under
`library/system-coherence/peripheral-as-pre-write-envelope.flexiarg`
or similar.

## Checkpoints

### 2026-05-04 — mission drafted; Level-1 substrate working

**What's now true that wasn't before:**
- Pattern peripheral Level-1 Sokoban mockup landed at `futon3a/src/futon/peripheral/pattern_author.clj` (committed `e3f68e8`).
- Canonical parser P-1 landed (committed `5664147`); 985 patterns in XTDB with five-facet edge structure.
- Canonical-parser invariant (CCT-1) opened in M-the-futon-stack; flexiarg row marked "Single by construction."
- Mission text drafted; four levels named; cross-refs woven; this is the integrative arc.

**What's not yet true:**
- Sokoban not wired into futon3c session-mode dispatch — agents can still bypass via `Edit`/`Write`.
- No flexiargs yet authored *through* the peripheral as test-of-substrate.
- Level 3 research not started.
- Level 4 receipts discipline not specified concretely.

**Risk register state:** all four risks above are live; none escalated.

**Cold-start accessibility evidence (added 2026-05-04 same day):** while this mission was being drafted, an unrelated Claude session — with no context about this mission, the Sokoban, or the rulebook — authored `library/writing-coherence/meet-the-reader-where-they-are.flexiarg`. Validating it against the Sokoban after the fact: `:status :ok`, parses cleanly, with `[conclusion, context, if, however, failure-modes, then, compositions, check]` clause shape — full canonical structure including rulebook-recognised substructure (failure-modes under however, compositions + check under then). **The substrate's ambient signal alone (the 985 already-canonical patterns in the corpus, the futon3 CLAUDE.md, the reshape rulebook on disk) was enough for an unprimed agent to author a Sokoban-admissible pattern.** That's strong cold-start evidence for Q1: the canonical shape is honest enough to be derivable from the corpus without needing to be told.

**Next-move:** author the four insight flexiargs in `library/pattern-discipline/` *through* the Sokoban. Each refusal is evidence about whether the canonical seven actually fits; each admission validates the substrate. After ~4 authoring rounds, decide what's worth promoting to MAP work.
