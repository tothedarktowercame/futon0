# Algorithm: Formalize Type From Validator Source Through Example Roundtrip and Derived Surface Parity

Date: 2026-03-14
Status: active

## Purpose

Use this when a type or object shape is currently implicit across prose,
validator code, live examples, and partial local implementations, and the goal
is to converge it into:
- a bounded runtime model
- serializer/deserializer coverage over real examples
- aligned derived surfaces such as JSON Schema and Lean/CT

This algorithm captures the local process used to formalize futon and
`mfuton` mission/object types without overfitting one example or flattening
formatting noise into the semantic model.

Treat the work as type discovery over the graph, not isolated class authoring:
- start from all highest types in futon that are in scope for the active
  bounded lane
- discover the reachable graph defined by `isa` and `hasa` relations
- treat newly discovered child types as part of the same formalization problem
  rather than as optional later cleanup
- maintain the invariant that only whole-document top-level types in the lane
  point upward by `isa` to the lane's current top type
- do not collapse discovered inner document parts, payloads, carriers,
  projections, or enum/value types into `isa` children of the lane's top type
  just because they are discovered in the same wave
- allow discovered inner types to form their own local `isa` hierarchies when
  that is the honest structure

The fidelity obligation is recursive:
- once an outer type is bounded, apply the same authority/evidence discipline
  to the inner types, referenced carriers, and projection-layer types it owns
  or depends on
- keep a nested surface generic only when the current evidence does not yet
  justify a stronger primary type there

The closure obligation is logical-set-bounded:
- treat one outer type plus the recursively justified inner/reference/
  projection types discovered in that same wave as one logical set
  - a type in that logical set is not closed while one of its discovered
    relevant `isa` subtypes or discovered `hasa` child types remains open
- do not investigate or discover a deeper inner layer while currently
  discovered types in the open logical set still lag across the derived/check
  surfaces at the same fidelity boundary
- graph-only mention in Lean/CT is not enough once a discovered type has been
  promoted into a first-class Python runtime structure for the lane
- this is a mandatory gate, not an optional cleanup preference:
  - once a newly discovered layer exists, close that currently discovered
    layer first across Python, JSON/registry, and Lean/CT before descending
    further into ever more inner types
- do not move to a new logical set until the current one has either:
  - propagated its stronger recursive knowledge into the relevant derived/check
    surfaces and satisfied the active owner constraints, or
  - been reported honestly as still open

It supports two honest convergence modes:
- single-validator-centered
  - one executable validator/parser is the clear structural center
- authority-stack-centered
  - no single file owns the whole shape, so the model must be bound from a
    layered stack of executable and live authorities

It is especially appropriate when:
- there is a stronger executable authority such as a validator/parser source
  file
- there are multiple live examples whose real semantic payload must be covered
- the Python model is expected to become the current bounded source of truth
  for the wave
- Lean/CT must stay independently authored from futon-side source evidence so
  that `Lean -> Python` verification remains meaningful

## Companion Flexiarg Resolution (2026-03-14)

Governing companion flexiargs for this algorithm:
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
  - governs reuse-first inspection of existing validators, schemas, models,
    libraries, and serializers before inventing local structure
- `library/foundational/how-to-abstract.flexiarg`
  - governs abstraction discipline across multiple examples so the result does
    not overfit one vivid artifact or jump too early to a stronger formalism

Inspected but non-owning candidates:
- `library/social/deterministic-proof-gates-over-llm-assertions.flexiarg`
  - proof/evidence gate policy matters to downstream verification, but this
    algorithm owns the broader modeling/convergence path rather than proof-gate
    policy itself

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

## Inputs

- the target type or object family to formalize
- all highest types in futon that are in scope for the active bounded lane,
  when the work is being run as graph discovery rather than a single isolated
  type edit
- relevant repos and neighborhoods where the shape may already be described
- candidate authority surfaces or authority-stack elements:
  - prose contracts
  - validator/parser source
  - normalizer/compiler/indexer source
  - tests
  - live example artifacts
- the candidate implementation carriers for the wave:
  - Python model
  - serializer/deserializer
  - JSON Schema
  - Lean/CT surface
- nested or referenced surfaces that may also need refinement:
  - inner payload/value objects
  - projection-layer types
  - companion-schema types
  - edge/reference value types

## Preconditions

1. The type/object pressure is real enough that prose-only description is no
   longer sufficient.
2. The explicit local mission neighborhood has been inspected before starting
   new core/formalization artifacts.
3. Relevant upstream and local repos have been searched for:
   - shape docs
   - validator code
   - live examples
   - existing serializers/deserializers
4. At least one live example exists.
5. If one stronger executable authority exists, it has been identified before
   the local model is widened.
6. If no single validator owns the shape, the current authority stack has been
   made explicit before the local model is widened.
7. Current source evidence has been interpreted broadly enough to include both:
   - executable authority
   - document evidence from live artifacts when executable authority does not
     fully determine the inner structure by itself

## Procedure

1. Search the shape neighborhood first.
   - Search for files related to the target shape across the strongest local
     and upstream neighborhoods.
   - Separate:
     - prose descriptions
     - executable authority such as validator/parser source
     - live example artifacts
     - existing local model/schema/proof surfaces
2. Discover the current local type graph before narrowing the model.
  - Identify all highest whole-document types in futon that are in scope for
    the active bounded lane.
   - From each in-scope highest type, discover the reachable child graph
     through:
     - `isa` relations
     - `hasa` relations
   - Treat newly discovered child types, owned payload types, referenced value
     types, and projection-layer types as candidate members of the same
     logical set, not as automatically separate future work.
  - Maintain the working invariant that the lane's highest whole-document
    types satisfy `isa` to the lane's current top type.
  - Do not force discovered inner structure to satisfy that same `isa`
    relation unless the sources actually justify it as a whole-document type.
3. Bind authority strength honestly.
   - If one executable authority exists, such as a `.clj` validator, treat it
     as stronger than prose on exact structural obligations.
   - If no single file owns the shape, bind an authority stack instead, for
     example:
     - validator
     - parser/normalizer
     - compiler/indexer
     - tests
     - live corpus
   - Keep live examples as evidence for real payload usage, not as the only
     authority on abstract shape.
   - Treat document evidence from live artifacts as real source evidence for
     inner structure when the executable authority stack leaves part of that
     structure underdetermined.
   - Record which convergence mode is currently active:
     - single-validator-centered
     - authority-stack-centered
4. Start from the maximum-entropy model.
   - Build the simplest runtime model that can satisfy the current authority
     surfaces without overfitting one example.
   - Prefer a generic outer carrier plus derived inner projections when that
     avoids prematurely freezing a narrow class lattice.
   - Treat narrower consumer-specific views as projections unless they have
     clear independent reuse pressure as primary storage.
5. Choose a corpus-growth path before widening the model.
   - Default progression:
     - one representative live example
     - a bounded diverse set
     - the full currently available corpus
   - Make the bounded diverse set explicit enough to pressure the model across
     carrier variation, not just one vivid happy path.
   - If a schema-discovery helper such as `GenSON` is used to look for shared
     abstractions across related top types or sibling families, do not run the
     inference family-by-family and then treat those isolated results as the
     default abstraction signal.
   - For cross-type abstraction discovery, merge all relevant related
     exemplars into one inference run so repeated structure can surface across
     those types.
   - Family-by-family inference is still acceptable when the local goal is
     describing one family, but it is the wrong default when the local goal is
     tying related types together into a simpler shared abstraction.
6. Implement the first bounded serializer/deserializer slice.
   - Deserialize one representative live example into the runtime model.
   - Serialize it back out.
   - Compare semantically first, not cosmetically.
7. Classify differences before widening the model.
   - At each wave, classify differences into:
     - formatting-only drift
     - serializer lexeme bug
     - missing semantic field
     - evidence that the current model is too specific
     - consumer-specific projection that should not yet become primary storage
8. Fix clear serializer bugs before widening the model.
   - If the model already contains a semantic field and the serializer emits
     the wrong characters for it, fix that directly as a serializer bug.
   - Ignore whitespace-only drift.
9. Increase example coverage iteratively.
   - Move through growing waves such as:
     - one live example
     - a small bounded set
     - all available examples in the target family
   - Treat this as a local subroutine of the broader formalization arrow, not
     as a separate algorithm owner, unless the same growing example-set audit
     starts recurring independently of validator-source type formalization.
10. Widen only when repeated structure or authority demands it.
   - Add new modeled structure only when:
     - the validator authority requires it
     - the active authority stack requires it
     - repeated examples carry the same semantic payload
   - If schema inference is being used as a helper during this step, interpret
     it as compression evidence over the right exemplar set:
     - use combined related exemplars for shared abstraction discovery
     - use per-family exemplars only for per-family description
   - Avoid adding narrow one-off fields just because one file happens to spell
     something verbosely.
   - Do not split a separate inner payload or projection sibling merely because
     one consumer can interpret the generic carrier more narrowly.
   - Split that narrower owner only when it starts recurring independently and
     improves reuse/clarity as a primary view.
11. Recurse into inner and referenced types with the same fidelity standard.
   - After the outer type stabilizes enough to reveal its owned inner types,
     referenced value types, and projection-layer types, run the same
     authority/evidence check on those nested surfaces.
   - Treat this as continued graph discovery:
     - each newly justified child type becomes another node in the bounded
       `hasa` / `isa` graph for the current wave
   - Recurse one discovered layer at a time.
   - After a newly discovered layer has been identified strongly enough to be
     promoted into first-class Python runtime structure, stop deeper descent
     and close that currently discovered layer across Python, JSON/registry,
     and Lean/CT before investigating still deeper inner types.
   - For each nested surface ask:
     - is there stronger authority-backed type knowledge available now?
     - is there repeated document evidence that justifies a stronger nested
       type?
     - should this stay generic as honest primary storage, or has a stronger
       nested type now been earned?
   - Continue recursively until each nested surface is either:
     - refined to the strongest currently justified type boundary, or
     - explicitly left generic because stronger current source evidence does
       not yet exist.
12. Keep the source-of-truth boundaries explicit once the runtime model
    stabilizes.
   - If Python is the current bounded runtime source of truth for the wave,
     state that explicitly.
   - Do not leave Python, JSON Schema, and Lean/CT as if they co-own the same
     authority by default once one of them is actually driving the others.
   - Treat Lean/CT differently from generated sibling surfaces:
     - Lean/CT must be updated from futon-side source evidence and authority
       surfaces, not from the Python implementation being tested.
   - Updating Lean/CT from the Python implementation under test is wrong:
     - it is like fitting a model on the test data
     - it destroys the independence that the Lean-to-Python verification lane
       is supposed to prove
13. Update sibling surfaces with the correct directionality.
   - Update JSON Schema.
   - Update the current registry instance or equivalent data projection.
   - Update Lean/CT independently from futon-side authority surfaces and
     document evidence, ignoring the Python classes that will later be tested
     for equivalence.
14. Verify derived-surface parity and formal independence.
   - Add focused tests that compare the bounded runtime model against the JSON
     and Lean/CT surfaces.
   - Keep the parity checks narrow and machine-checkable.
   - When Lean/CT exports class shape, prove coherence in the direction:
     - futon-side authorities -> Lean/CT
     - Lean/CT -> regenerated Python classes
     - regenerated Python classes -> runtime equivalence against the native
       Python classes
15. If class shape is exported into Lean/CT, test generation of the classes
    themselves.
   - Regenerate the Python class definition from the Lean/CT class-shape
     export.
   - Verify equivalence against the native Python class definition using
     runtime checking in Python.
   - Structural/AST comparison may still be used as a helper, but it is not a
     substitute for keeping Lean/CT independently authored from the futon-side
     sources.
16. Run a naming and boundary cleanup pass once the model stabilizes.
   - Trim names so files and helper surfaces describe what they actually own.
   - Remove accidental mission-shaped or example-shaped naming drift from more
     general carriers.
17. Close the current logical set before starting a new one.
   - Treat the current logical set as:
     - the outer type or in-scope highest types being formalized in this wave
     - plus the recursively discovered inner/reference/projection types that
       were strong enough to refine during the same wave
   - Do not treat a parent type as closed while one of its discovered child
     types remains open.
   - Do not treat a type as closed while one of its discovered `isa` subtypes
     or discovered `hasa` child types remains open.
   - Before moving to a new logical set, ask:
     - have the newly refined recursive types been propagated into the
       relevant derived/check surfaces?
     - have the currently discovered types been kept first-class coherent at
       the same fidelity boundary across Python, JSON/registry, and Lean/CT?
     - are JSON Schema / Lean / registry / serializer / class-shape or other
       parity surfaces still lagging the current Python source of truth?
     - has any discovered type been promoted to a first-class Python type
       while still appearing in Lean/CT only as graph metadata?
     - do the focused tests now cover the stronger recursive boundary rather
       than only the older outer boundary?
     - do the discovered child types now themselves satisfy the active owner
       constraints, rather than only the outer/root type?
   - If any answer is “not yet,” the current logical set is still open and the
     run must continue here rather than advancing to a new outer type family.
   - Do not reinterpret this gate loosely:
     - if the lane has enough evidence to name or implement a newly discovered
       inner type, the run must close that newly discovered layer before
       descending further
     - do not defer that closure as optional follow-on cleanup while deeper
       discovery continues
18. Evaluate the active owner constraints before treating the logical set as
    complete.
   - Compose:
     `algorithms/how-to-evaluate-constraints.md`
   - Evaluate the relevant owner constraints or acceptance criteria that this
     logical set is supposed to satisfy.
   - If a constraint is still aligned with the owner's goals/scope but is not
     actually satisfied yet, loop back into implementation/evidence work
     rather than weakening the constraint or declaring the slice complete.
   - If a constraint appears misaligned with the owner's goals/scope, do not
     change it here; only offer the operator that option after the constraint
     evaluation algorithm has established that misalignment.
19. Wire discoverability and report ownership.
   - Update the owning mission(s).
   - Default to continuing in the current owning mission unless the new slice
     has become independently owned enough to justify a child mission.
   - Update `algorithms/README.md` when this algorithm lives under
     `algorithms/`.
   - Report what was reused versus newly created.

## Decision Rule

Default convergence rule:
1. search for the strongest existing authority first
2. decide whether the lane is single-validator-centered or
   authority-stack-centered
3. choose the weakest truthful runtime model that covers that authority or
   authority stack and the repeated examples
4. keep the primary storage maximum-entropy and treat narrower consumer views
   as projections unless they earn separate ownership
5. recurse through inner/referenced/projection types and apply the same
   evidence/fidelity rule until the remaining generic boundaries are
   explicitly justified
6. keep treating the work as discovery of a bounded `hasa` / `isa` type
   graph beneath all highest types in futon that are in scope for the active
   bounded lane, not as isolated outer class authoring
7. do not consider a parent type complete while its discovered child types are
   still open
8. maintain the invariant that all discovered types satisfy `isa` to the
   lane's top type
9. only after the runtime model stabilizes, declare the current source of
   truth and derive JSON/Lean/CT from it
10. only promote additional inner structure when repeated example pressure or
   authority pressure genuinely requires it
11. before moving to a new logical set, propagate the current recursive
    refinements into the relevant derived/check surfaces and verify that those
    surfaces are no longer stale
12. before moving deeper into ever more inner discovered types, ensure the
    already discovered layer is first-class coherent at the same fidelity
    boundary across Python, JSON/registry, and Lean/CT, not merely present as
    graph metadata
13. treat that depth-closure gate as mandatory whenever a newly discovered
    layer has been promoted into first-class Python runtime structure
14. before treating the logical set as complete, evaluate the owner constraints
    and loop back if they are still aligned but unsatisfied
15. prefer staying in the current owning mission unless a stronger independent
    owner has clearly emerged

## Outputs

- a bounded runtime model for the target type/object family
- an explicit authority mode:
  - single-validator-centered
  - authority-stack-centered
- serializer/deserializer coverage over a growing real example set
- explicit source-of-truth declaration for the current wave
- aligned JSON Schema and Lean/CT surfaces
- focused parity tests
- when applicable, a Lean/CT-driven Python class-shape regeneration test
- an explicit logical-set closure judgment:
  - closed and ready for the next logical set
  - or still open because derived/check surfaces or owner constraints lag the
    current recursive source of truth
- an explicit graph-closure judgment:
  - whether the discovered child-type graph under all highest types in futon
    that are in scope for this wave is closed for this wave
  - and whether all discovered child types now satisfy the active constraints
- an explicit depth-closure judgment:
  - whether deeper discovery is blocked because the already discovered layer
    still lacks first-class coherence across Python, JSON/registry, and
    Lean/CT at the same fidelity boundary
- an explicit statement of which inner nested surfaces were:
  - refined further
  - left generic intentionally because the current evidence does not justify a
    stronger type

## Stop Conditions

Stop and keep the lane at discussion/mission/design-note level if:
- no stronger authority surface can be identified yet
- no honest authority stack can be stated yet when no single validator exists
- there is only one thin example and no stable repeated structure
- the current model would need to encode formatting noise as semantics to
  proceed
- the only recurring pressure is a consumer-specific projection and there is
  still no evidence that it should become primary storage
- the source-of-truth carrier is still ambiguous
- the type boundary is still too unsettled to survive focused tests
- the current logical set still has recursive refinements that have not yet
  been propagated into the relevant derived/check surfaces
- the current logical set still fails aligned owner constraints

## Case Anchors

- `futon-object` mission formalization:
  - searched mission-shape surfaces in futon
  - found the `mission.clj` validator
  - iterated deserializer/serializer coverage across increasingly large live
    mission sets
  - updated JSON and Lean/CT once the Python model became the bounded current
    source of truth
- `futon-object` flexiarg formalization:
  - searched flexiarg-shape surfaces in futon
  - bound the strongest available executable/normalizer/indexing sources
    rather than pretending a single validator file existed
  - iterated deserializer/serializer coverage across representative live
    flexiarg families and then the full available flexiarg corpus
  - kept the model maximum-entropy by preserving ordered entries and
    open-world body blocks instead of freezing one narrow clause lattice
  - updated JSON and Lean/CT once the Python model became the bounded current
    source of truth
- shared Python class-shape regeneration:
  - exported class shape into Lean/CT
  - regenerated Python class definitions
  - verified AST-level equivalence against the native Python classes
- active-owner constraint evaluation:
  - after a bounded type slice looks done, evaluate the current owner's
    relevant constraints before claiming completion
  - if the constraint is aligned and still unsatisfied, return to the slice
    instead of weakening the owner contract
- logical-set closure:
  - if recursive refinements strengthen the Python source of truth, do not
    begin the next outer type set until those refinements have also been
    carried through the relevant derived/check surfaces and the owner
    constraints have been re-evaluated against that stronger boundary

## Orthogonality Note (2026-03-14)

The growing example-set roundtrip audit remains inside this algorithm for now.

Reason:
- no stronger existing owner surfaced in `mfuton` or `futon3c`
- the audit step is currently only justified as one bounded subroutine of the
  broader arrow:
  - search authority surfaces
  - bind validator strength
  - converge the runtime model through examples
  - align the derived JSON and Lean/CT surfaces
- extracting a separate helper now would create a thin sibling that has only
  one active case family and no independent reuse pressure yet

Current related no-split rule:
- if the current generic carrier is still the honest primary storage and a
  narrower view is only a consumer-specific projection, keep that narrower
  view inside the current owner rather than splitting it into a sibling lane
  prematurely

## References

- `holes/missions/M-mfuton-familiar-typed-surface-world-and-memory/M-futon-object-core-and-first-mission-type-formalization.md`
- `holes/missions/M-mfuton-familiar-typed-surface-world-and-memory/M-mfuton-object-core-type-and-subtype-formalization.md`
- `docs/M-my-familiar-software-development-operating-posture/artifact-role-reconciliation.md`
- `src/mfuton/core/futon-object/futon_object_serializer.py`
- `src/mfuton/lean/python_class_shape_serializer.py`
- `test/mfuton/core/futon_object_serializer_test.py`
- `test/mfuton/lean/python_class_shape_serializer_test.py`
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
- `library/foundational/how-to-abstract.flexiarg`
