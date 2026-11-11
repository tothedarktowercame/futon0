# EuclidZero design 2: score grounded moves

Date: 2026-11-11.  This is DESIGN-1 step 2.  It changes the learner and
search representation, not System E: the engine remains the authority for
whether a construction is playable and whether the goal follows.

## Representation

A policy item is a grounded construction, written as it is shown to a player:
`proposition_4(a,b,c,d,e,f)`.  Internally it retains the complete engine tuple
`(rule, args, fresh bindings)`, including line and circle arguments recovered
from the rule's premises.  It has no permanent integer from the 371,361-id
menu.  The candidate's index is local to one position and lasts only for that
position's search and replay record.

The state encoder builds one vector per canonical object (DESIGN-1 step 1):

* kind, given/fresh status and clipped creation-order embedding;
* provenance: the rule that created it, its result role, and a pooled vector
  of the parent objects used by that construction;
* facts mentioning it, represented as predicate embeddings with argument-role
  embeddings and pooled messages from the other arguments; numeric constants
  use small type/sign/bucket features rather than predicate-specific code;
* goal participation: whether its kind can fill a variable or constant role
  in a goal atom, and the corresponding goal-predicate and argument-role
  messages.  This marks possible witnesses without claiming that they work.

Thus the vocabulary comes from construction and fact syntax, not coordinates,
length formulae, or Book I proposition numbers.  Facts and provenance form a
labelled hypergraph.  Two rounds of shared message passing let an object's
representation depend on what created it and what is known with it.  Global
state pooling also receives the move count, object counts by kind, and pooled
goal atoms.

Each rule has a learned embedding.  Its static syntax contributes pooled
embeddings of argument kinds/roles and of premise and conclusion predicate
shapes.  A candidate vector concatenates the rule vector, the global state,
and one projection of each argument object tagged by its argument position;
repeated arguments remain visible.  A shared MLP emits one logit.  Fresh
binding names are not features: their result kinds and roles are in the rule
syntax.  This same representation handles zero-argument rules.

## Network and expected cost

The initial CPU model uses width 64: 32-dimensional kind, predicate, rule and
role embeddings projected to 64; two hypergraph message-passing blocks; a
two-layer 128/64 candidate MLP; and a two-layer value head over the global
state.  The policy head is shared across candidates, so adding objects or
moves does not add parameters.  The estimated total is 180k--260k parameters
(under 1.1 MB in float32), to be replaced by an exact count in the first
implementation receipt.

Object and fact encoding is once per position.  Candidate scoring is batched
as a padded `[candidate, role, feature]` tensor with masks.  For 500--1,200
candidates the estimate is 5--20 ms on one capped CPU process, measured after
JAX warm-up.  Premise-certified generation is expected to dominate: before
LEGAL-1's per-candidate guard it took 36 ms at the rc13 stage-2 prefix; STEP1
measured 117 ms median on random I.2 positions.  The first benchmark must
separate candidate generation, feature construction, JAX inference, guard
checks and engine application.  The target is below 2 s per stage-2 move,
versus P13's 22.9 s and P14's 8.6 s; this is an estimate, not acceptance by
fiat.

## Candidate generation and search

Search asks `enumerate_actions(validate=False)` for premise-certified
candidates over canonical objects.  This is intentionally a superset: cheap
relational premises and arithmetic entailments have passed, but the
transactional contradiction guard has not.  The default remains
`validate=True` for the play page, CLI and packaged OpenSpiel search.  Before
a candidate becomes a child, a copy is passed through
`apply_certified_step`; a refusal is cached for that exact logical-state key
and skipped.  The guard, not the network, is authoritative.  Root win-in-one
already applies candidates, so its scan produces the root's validated child
set without another pass.

All candidates at the current node are cheap enough to score together.  Rule-
by-rule paging is therefore deferred: STEP1/P14 positions have roughly
500--1,200 candidates, while scoring that batch is estimated below the
36--117 ms generation cost.  Paging would be reconsidered if generation or
feature materialisation exceeds 20% of move time at larger states.  If used,
rules would first be scored from the state/rule vectors and generated in that
order; it would not alter legality.

Our PUCT stores local candidate keys, logits, visits, value sums and optional
validated successors.  It uses the AlphaZero PUCT formula, Dirichlet noise on
validated root candidates during training, temperature over root visits, and
the existing root win-in-one rule.  Start with 32 simulations for comparison.
Progressive widening remains available as a branching control
`ceil(c*N^alpha)`, but no longer searches a mostly-illegal id space.  The
first comparison uses both `all candidates` and `c=1, alpha=0.5`; retain
widening only if it improves wall time without reducing stage-1 success.

## Training and process boundary

Each replay item contains the state feature record, the ordered variable-
length candidate feature records, visit counts aligned with them, and the
eventual value.  Batches pad objects, facts, roles and candidates and carry
masks; policy cross-entropy is computed only over real candidates.  Candidate
ordering is deterministic (`rule, args, bindings`) so receipts replay exactly.
The learner samples the existing 256-position buffer initially, trains the
shared policy/value network, and publishes checkpoints.

Actors never restore Orbax.  As in RESUME-1, the learner restores once and
writes an atomic plain-array snapshot; actors memory-map or copy that snapshot
and verify its generation number before swapping parameters.  THREADS-1's
XLA, BLAS and OpenMP limits remain mandatory for actors, learner and
evaluators.  Checkpoints also store optimizer state, feature-vocabulary
version and replay-schema version.

OpenSpiel remains the rules/game boundary: `EuclidState`, terminal rewards,
start files and perturbations, CLI notation, watch pages, and fresh-state/play-
referee verification are reused.  New code owns candidate feature extraction,
the variable-policy NNX model, PUCT, replay collation, actors and learner.  It
does not monkey-patch packaged AlphaZero; the old runner remains a baseline.

## Tests and step-2 acceptance

* **Candidate API:** `validate=True` exactly matches LEGAL-1; `False` is a
  deterministic superset.  The degenerate-superposition reproduction appears
  only in the superset and is refused before child admission.
* **Features:** permutation tests preserve outputs under a consistent rename
  of non-given objects; canonical aliases have identical records; provenance,
  fact-role, goal-role and padding masks have exact fixtures.
* **Model:** shapes and masks for zero, one and 1,200 candidates; padding does
  not change logits/value; save/load is array-for-array identical; gradients
  reach rule, role, object and value parameters.
* **Search:** root win-in-one wins the untrained clean stage-1 position 10/10;
  refused candidates are never children; fixed-seed PUCT and Dirichlet tests;
  visit targets align with the recorded candidate keys; dead ends return 0.
* **Training/processes:** a synthetic variable-length batch reduces loss; a
  12-actor smoke run loads one learner snapshot without Orbax reads or task-
  limit failures; every counted win passes fresh Spiel and play-page replay.
* **Learning acceptance:** train on stage 1 without demonstration targets.
  On held-out perturbed starts, the genuine winning move ranks first before
  search (state a threshold in the run packet; proposed: at least 8/10).
  Report warm per-position inference and full stage-2 move time against P13
  22.9 s and P14 8.6 s.  Acceptance is full moves under 2 s median with no
  legality or replay disagreement; otherwise profile before any curriculum.

## Implementation packets

Each packet changes one principal file, one behaviour and its tests:

1. **CANDIDATES:** `euclid_spiel.py` -- add guarded/unvalidated enumeration;
   exact/superset and LEGAL-1 tests.
2. **FEATURES:** `euclid_candidate_features.py` -- deterministic object,
   fact, rule and grounded-move tensors; rename/mask fixtures.
3. **MODEL:** `euclid_candidate_model.py` -- NNX encoder, scorer and value
   head; shape, gradient, parameter-count and CPU benchmark tests.
4. **SEARCH:** `euclid_candidate_puct.py` -- guarded candidate PUCT, root
   win-in-one, noise and optional widening; deterministic search tests.
5. **REPLAY:** `euclid_candidate_replay.py` -- variable-length records,
   collation and masked losses; round-trip and learning tests.
6. **LOOP:** `euclid_candidate_alpha_zero.py` -- actor/learner orchestration,
   RESUME-1 snapshots and THREADS-1 caps; 12-actor smoke test.
7. **STEP2-EVAL:** no semantic code -- stage-1 ranking experiment and the
   stage-2 phase/cost receipt.  No longer curriculum run is authorized by
   this packet.

## Review note (claude-9, 2026-11-11; Joe approved the packet sequence)

The "under 2 s per stage-2 move" target cannot hold while the root
win-in-one scan applies every candidate through the engine (2.2-3.7 s in
P13/P14). Until DESIGN-1 step 3, report move time with and without the root
scan, and apply the 2 s target to the time without it. Packets run one at a
time, with claude-9's review between them; no curriculum run before packet 7
passes.
