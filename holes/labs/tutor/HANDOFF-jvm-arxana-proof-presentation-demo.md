# Handoff to Zai — JVM Arxana-backed explanatory proof demo

**Date:** 2026-11-07  
**Requested by:** Joe  
**Consumer:** `M-futonzero-prelim-practice`  
**Architecture:** `M-jvm-arxana` as implemented in `futon3c`  
**First fixture:** APM `t98J02`  
**Status:** bounded demonstration request; the method is deliberately not
settled in advance

## Operator intent

Build the first demonstration in which JVM Arxana helps a reader move from a
Lean proof toward a more explanatory informal proof. The demonstration should
not assume that rendered Lean, elaborated proof terms or an automatically mined
alignment are readable. Its job is to test whether revision-pinned proof
manifestations plus explicit proof moves can support a futonzero-style tutor.

The prior side-by-side renderer is useful pipeline evidence but not the target
experience. Its right column was generated through
`Main.olean → PythonEmit → leantotex-python → TeX → LaTeXML-Oxide`, and was too
difficult to read as an argument. Preserve that output as optional provenance;
do not make it the primary explanation.

## Why `t98J02`

`t98J02` is a strong first fixture because it has all of the following:

- a closed, sorry-free Lean proof in
  `apm-lean/problems/t98J02/lean/Main.lean`;
- an authored informal proof in
  `apm-lean/problems/t98J02/informal-solution.md`;
- a worked tutor lesson in
  `futon0/holes/labs/tutor/lessons/t98J02.md`;
- four explicit moves in `futon0/holes/labs/tutor/moves.edn`;
- starmap evidence showing what dependency tags captured and missed;
- a meaningful route divergence rather than a cosmetic wording difference.

The authored informal proof proves part (a) through mod-2 homology. The Lean
proof instead uses a finite fibre count that is locally constant away from the
curve. The tutor lesson explains why that move avoids a much larger formal
development. Therefore the right result is not a forced sentence-by-sentence
alignment. It is a representation of two valid routes, plus a new explanation
of the route the Lean proof actually takes.

The absence of `apm-lean/problems/t98J02/proof-outline.md` is useful: the generated
informalization cannot be passed off as a pre-existing outline.

## Read before acting

JVM Arxana contract and implementation:

1. `/home/joe/code/futon4/holes/labs/M-jvm-arxana/D4-correspondence-model.md`
2. `/home/joe/code/futon4/holes/labs/M-jvm-arxana/D7-architecture-synthesis.md`
3. `/home/joe/code/futon4/holes/labs/M-jvm-arxana/V2-proof-presentation-contract-report.md`
4. `/home/joe/code/futon4/holes/labs/M-jvm-arxana/V4-client-risk-spikes-report.md`
5. `/home/joe/code/futon4/holes/labs/M-jvm-arxana/D1-emacs-jvm-demo-report.md`
6. `/home/joe/code/futon3c/src/futon3c/arxana/correspond.clj`
7. `/home/joe/code/futon3c/test/futon3c/arxana/correspond_test.clj`

Proof/tutor evidence:

1. `/home/joe/code/apm-lean/problems/t98J02/problem.md`
2. `/home/joe/code/apm-lean/problems/t98J02/informal-solution.md`
3. `/home/joe/code/apm-lean/problems/t98J02/lean/Main.lean`
4. `/home/joe/code/futon0/holes/labs/tutor/lessons/t98J02.md`
5. `/home/joe/code/futon0/holes/labs/tutor/moves.edn`
6. `/home/joe/code/futon0/holes/labs/tutor/moves-vs-tags.md`
7. `/home/joe/code/apm-lean/holes/labs/star-map/README.md`

Rendering evidence:

1. `/home/joe/code/apm-lean/docs/render-book.md`
2. `https://zone.hyperreal.enterprises/wip/apm-pythonemit-side-by-side/`
3. `https://zone.hyperreal.enterprises/wip/apm-highlight-demo/`

## Existing architectural facts — do not redesign them

- `proof/move` is a domain entity, not a new Arxana core primitive.
- One move may have zero or many revision-pinned manifestations.
- Authored prose, Lean source, generated formal output and generated
  informalization are distinct manifestations with distinct provenance.
- A correspondence may be partial, ambiguous, directional, rejected or
  explicitly absent. Total alignment is not required.
- Mark8, shared names and starmap tags are candidate evidence, not authority.
- A correspondence assessment is immutable and does not mutate the assertion.
- The client owns pedagogical order and presentation. JVM Arxana returns
  identities, revisions, assertions, assessments and bounded query results.
- Transport, rendering integrity and correspondence correctness are JVM
  conformance layers. Reader usefulness is separate consumer evidence.
- The corpus-scale query-shape spike has already passed at 2,454 moves and 636
  manifestations. Do not repeat that spike.

The current implementation already provides manifestation registration,
correspondence assertions, attributable assessments, `all-manifestations`, and
absent/ambiguous queries in `futon3c.arxana.correspond`. Reuse it. Do not create
a parallel alignment store.

## Demonstration question

Can a reader understand **why the Lean proof uses a locally constant fibre
count**, where each hypothesis acts, and how that route differs from the
homology proof—without reading the elaborated proof term?

The demo passes only if the answer becomes inspectable. Merely producing HTML,
a graph or four columns is not a pass.

## Seed move cascade

Use these domain moves in this order. The order belongs to the tutor fixture,
not to Arxana:

| Ordinal | Move ID | Explanatory question | Initial Lean evidence |
|---:|---|---|---|
| 1 | `move/replace-global-invariant-by-count` | Why replace homology by a fibre count, and why is that count locally constant? | `T98Sheets`, `t98J02_exists_sheets`, `t98J02_count`, and the comment plus opening of `t98J02_nonseparating_lift_connectedCover` (`Main.lean:329-431`) |
| 2 | `move/assign-each-hypothesis-a-job` | Where are finiteness, nonseparation and connectedness actually used? | the finite-fibre/sheet construction and the corresponding stages of `t98J02_nonseparating_lift_connectedCover`; do not claim an exact span until reviewed |
| 3 | `move/check-wild-curve` | Which local fact replaces an unstated smooth collar assumption? | `t98J02_no_inj_plane`, `t98J02_interior_range_eq_empty`, and their uses (`Main.lean:229-328` plus reviewed call sites) |
| 4 | `move/choose-counterexample-with-function` | Why is the Klein-bottle model cheaper, and how does a function certify separation? | `surface`, `height`, `continuous_height`, and `t98J02_connected_doubleCover_counterexample` (`Main.lean:802-935`) |

These are seeds for human/model review, not automatically accepted
correspondences. If a cited range contains several logically distinct steps,
split the manifestation or mark the assertion partial. If no honest range
supports a move, record explicit absence rather than manufacturing one.

## Required manifestations

For each move, attempt to register four roles:

1. **Authored informal** — an exact range in `informal-solution.md`, if the
   authored homology route actually expresses that move. For move 1 this may
   properly be absent or a route-level contrast rather than an alignment.
2. **Readable Lean source** — a reviewed declaration or source range in
   `Main.lean`. This is the main formal manifestation a human should see.
3. **Generated formal rendering** — PythonEmit/LeanToTex/Oxide output when it
   can be tied to the same declaration and exact input/output revisions. Keep
   it collapsed by default.
4. **Generated informalization** — a newly generated explanation of the Lean
   route, with model/tool/version, source revisions, and the move/assertion IDs
   it relies on. It must not reuse the authored-informal manifestation ID.

The generated informalization should explain:

- the mathematical claim of this move;
- why it is chosen here;
- which hypotheses make it work;
- the next obligation it creates;
- how it differs from the authored route;
- what remains uncertain or only partially aligned.

Do not translate Lean syntax line by line. Do not summarize an elaborated term
merely because it is available. Prefer the readable Lean declaration, its
source comment, dependency evidence and the tutor move rationale.

## Minimum Arxana records

Use the implemented D4 vocabulary. Names below are illustrative; use the
canonical constructors in `futon3c.arxana.correspond` rather than hand-minting
IDs.

```clojure
{:type :proof/move
 :entity/id "proof/move:replace-global-invariant-by-count"
 :proof/problem "t98J02"
 :proof/ordinal 1}

{:manifestation/authority "apm-proof-demo-v1"
 :artifact/family-id "apm/t98J02"
 :manifestation/role-key "move-1/lean-source"
 :manifestation/label "Lean source for locally constant fibre count"
 :manifestation/media-role :text/x-lean
 :manifestation/role :authored
 :manifestation/subject "proof/move:replace-global-invariant-by-count"}

{:manifestation/authority "apm-proof-demo-v1"
 :artifact/family-id "apm/t98J02"
 :manifestation/role-key "move-1/generated-informal"
 :manifestation/label "Generated explanation of the fibre-count move"
 :manifestation/media-role :text/markdown
 :manifestation/role :generated
 :manifestation/subject "proof/move:replace-global-invariant-by-count"
 :manifestation/origin
 {:origin/tool "<generator>"
  :origin/version "<version>"
  :origin/source-revisions ["arxana:revision:sha256:..."]}}
```

Every assertion endpoint must name an exact artifact revision and a locator.
Every assessment must name actor, policy/revision, reason and evidence. A
missing row is not evidence of absence; use the implemented bounded explicit-
absence assessment.

## Work sequence

### 1. Freeze and ingest exact inputs

- Record the git commit and SHA-256 content revision of `Main.lean`,
  `informal-solution.md`, the tutor lesson and the relevant move entries.
- Compile `Main.lean` and retain the resulting `.olean` as a derived input if
  PythonEmit is used. Do not assume an existing `.olean`; none was present in
  the current `.lake/build` census.
- Record renderer/generator versions and commands.

### 2. Build the move fixture

- Register the four move entities with client-owned ordinals.
- Register every available manifestation through the JVM Arxana
  correspondence repository.
- Add candidate assertions with honest direction, cardinality and standing.
- Add at least one accepted assessment, one rejected or ambiguous candidate,
  and one explicit-absence assessment if the evidence warrants it.
- Prove `all-manifestations` returns distinct IDs without identity merger.

### 3. Generate the explanatory account

- Generate one move card at a time from the frozen evidence.
- Keep claims traceable to source revision plus assertion/assessment IDs.
- Clearly label model-generated prose.
- Permit the reviewer to reject or revise an explanation without rewriting the
  underlying source or assertion.

### 4. Present a JVM-backed view

Build a small client whose alignment rows come from JVM Arxana query responses,
not from a duplicated static alignment table. A static HTML artifact is fine as
the visual output if it embeds the captured response digest and provenance.

Preferred initial presentation:

- one vertical card per move rather than two independently scrolling pages;
- generated explanatory prose visible first;
- authored-route comparison alongside it where one exists;
- readable Lean source highlighted and expandable;
- generated formal/PythonEmit rendering collapsed under “compiler-level
  provenance”;
- visible badges for `verified`, `partial`, `ambiguous`, `rejected` and
  `absent`;
- “why this move?”, “hypothesis jobs”, and one masked exercise per card.

Publish the demo under a new
`https://zone.hyperreal.enterprises/wip/` path. Do not overwrite the old
side-by-side demo; it is useful negative/comparison evidence.

### 5. Collect usefulness evidence

Ask Joe to inspect the demo without first reading the Lean source. Capture
answers to these questions as consumer evaluation records, not correspondence
truth:

1. Can he state why a fibre count replaces homology here?
2. Can he identify where finiteness, nonseparation and connectedness act?
3. Can he explain why the wild-curve check is needed?
4. Can he distinguish the authored homology route from the Lean count route?
5. After one masked card, can he predict the next proof obligation?

The demo may be mechanically correct and pedagogically unsuccessful. Record
that outcome rather than redefining correspondence to make the demo pass.

## Acceptance criteria

The bounded demo is complete when all of the following are evidenced:

- the visual rows are reconstructible from JVM Arxana records and queries;
- one move returns at least three distinct manifestations, including readable
  Lean and generated informalization;
- authored and generated informal manifestations have distinct IDs and
  provenance;
- at least one route divergence is represented without forced total alignment;
- ambiguous/rejected/absent evidence remains visible and attributable;
- the generated formal rendering is present only as optional provenance;
- changing a usefulness rating changes no artifact revision, assertion,
  assessment or JVM conformance result;
- a human-readable report states what the demo taught us about making Lean
  proofs explanatory.

## Stop-the-line conditions

Stop and report rather than patching around any of these:

- the client must invent IDs or alignment state that JVM Arxana cannot return;
- generated prose aliases or overwrites authored prose;
- a score, shared identifier or model judgment is silently promoted to truth;
- missing alignment is inferred solely from absence of a database row;
- the UI requires a total one-to-one alignment to render;
- PythonEmit output is presented as the readable proof merely because it is
  formally derived;
- tutor usefulness is allowed to change correspondence semantics;
- work requires redesigning D1–D7 instead of exercising the existing extension
  points.

## Requested outputs

Return:

1. a fixture manifest naming every artifact revision, move, manifestation,
   assertion and assessment;
2. focused JVM tests for the fixture and query results;
3. the generated informalization with provenance;
4. the published move-card demo and its captured JVM response digest;
5. a short evaluation form or record shape for the five usefulness questions;
6. a report classifying every result as transport, rendering integrity,
   correspondence correctness or usefulness;
7. exact reproduction commands and commits.

Keep changes separated by home repository: JVM correspondence/service work in
`futon3c`, APM source/generated consumer artifacts in `apm-lean`, and mission
receipts or architectural contradictions in `futon4`. Preserve unrelated dirty
work in `apm-lean`; do not fold the existing render-book exporter changes into
the demonstration commit unless they are directly required and separately
reviewed.

## First decision requested from Zai

Before coding, return a one-page fixture plan answering:

1. Which exact `t98J02` source ranges instantiate each move?
2. Which authored-route correspondences are genuine, which are contrasts, and
   which should be explicitly absent?
3. What generator will produce the first informalized move card, and what
   provenance will it record?
4. Which existing JVM Arxana operation supplies each client row?
5. What is the smallest public view that can test Joe's comprehension rather
   than merely demonstrate rendering?

That plan is the review gate. Do not begin by generating all four explanations
or building a general proof browser.
