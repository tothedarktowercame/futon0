# Excursion: E-euclid-tutor

**Status:** OPEN — EuclidZero (the learned-prover line, Checkpoint 3) PAUSED 2026-11-13 pending a change of approach and outside review; proof game, Book I.1–I.25 replay and play page built (Checkpoints 1–3).
**Date:** 2026-11-07
**Owner:** claude-9 with Joe
**Relates to:** `holes/missions/M-futonzero-prelim-practice.md` (the prelim tutor; its lessons live in
`holes/labs/tutor/`, moved from apm-lean 2026-11-07), `futon2/docs/futonzero-alphazero.md` (FutonZero v1 and Fable's
review), `futon6/holes/missions/M-futonzero-grounding.md` (why self-graded loops self-confirm),
`holes/excursions/E-metagame/` (the domain-general sibling: equational and line-geometry games, a learner that
transfers across theories; see Checkpoint 3), `holes/missions/M-futonzero-capability.md`,
`M-futonzero-generative.md`, `M-futonzero-mvp.md`.
**Why futon0 and not apm-lean:** apm-lean is public and is about the proofs. Tutor work lives here
(Joe, 2026-11-07).

## Operator voice (Joe, 2026-11-07)

> "I'm interested in thinking about how to use an alpha-zero style game engine as a proof engine
> or as part of the backend for my futonzero tutor."

> "we could lean on the existing proofs but help people 'discover' (explicitly not in Moore's
> sense) the proof techniques. The idea that we're chasing AlphaGeometry is not a bad thing
> either."

> "an excellent 'course' in mathematics would go from Euclid to the 17-gon, but we can start
> with Euclid"

> "the 'moves' that we are talking about in the 'tutoring' game are not necessarily the same as
> moves in the proof game. The Tutor needs to build a theory of mind for the student as well as
> the proofs"

> "Regarding 'artificial students' what could be done is to upgrade each human student with an
> envelope of 'digital clones' (not twins). I like this framing a lot."

## Facts on the ground (verified 2026-11-07)

- **LeanEuclid** (Murphy et al., ICML 2024, arXiv 2405.17216) implements Avigad–Dean–Mumma's
  System E in Lean 4 and formalises all 48 propositions of Elements Book I. Cloned and built at
  `~/code/LeanEuclid` on Zone: Lean v4.19.0 via elan, Mathlib from cache, z3 4.13.2 and cvc5
  1.2.1 release binaries in `~/.local/bin`, `smt-portfolio` in the repo's `venv/`. Build of
  `SystemE` + `Book`: 9 min wall clock, 1.8 GB peak memory, no errors. Environment needed:
  `PATH` with `venv/bin` and `~/.local/bin`; `LD_LIBRARY_PATH="$(lean --print-prefix)/lib/lean"`.
- **Proof shape.** Each proof is a sequence of `euclid_apply <axiom-or-earlier-proposition> args`
  steps ending in `euclid_finish`, which discharges the diagrammatic reasoning by SMT. 48 files,
  557 `euclid_apply` steps. Most-used: `line_from_points` 85, `proposition_3` 34, `extend_point`
  34, `intersection_lines` 31, `proposition_29` 26, `proposition_4` 23, `proposition_34` 23,
  `proposition_5` 21, `proposition_31` 21.
- **Check times** (`lake env lean Book/PropNN.lean`, single run, import baseline 0.3 s):
  I.1 0.9 s, I.5 4.0 s, I.10 1.7 s, I.15 1.1 s, I.22 3.6 s, I.29 3.1 s, I.35 20.0 s, I.41 1.5 s,
  I.47 92.5 s, I.48 17.1 s. Most propositions check in seconds; a few take 20–90 s, almost all of
  it SMT.
- **Lineage.** Shang-Ching Chou's 1985 UT Austin thesis (Wu's method; Boyer group, J Moore
  co-supervisor) is the student Bob Boyer mentioned. Chou–Gao–Zhang's later deductive-database
  work is, as far as I recall, what AlphaGeometry's symbolic engine builds on (not yet checked
  against the AlphaGeometry paper).

## Three games, not one

### 1. The proof game

State: a System E goal with the diagram so far. Moves: `euclid_apply` of a construction axiom or an
earlier proposition, with arguments. Referee: `euclid_finish` / Lean.

At 1–90 s per check, SMT cannot sit inside a self-play search (roughly 15–300 nodes/s on 32 cores).
Follow AlphaGeometry: search in a fast Euclid-specific engine (points, lines, circles, betweenness,
same-side, congruence; earlier propositions as rules), and certify only the finished proof in Lean.
The hard choice is the arguments and the auxiliary constructions, not which proposition to cite.

Test (AlphaGeometry-style): hold out proposition n, train the prior on proofs of 1..n−1, and see
whether search finds a proof of n. Where the engine needs a long search is a first guess at where
students need a hint.

This game answers "what is true and how far is this state from a proof". It is a component of the
tutor, not the tutor.

### 2. The student model: an envelope of digital clones

A **twin** claims to be the student. An **envelope** is the set of student models still consistent
with what the student has done; its width says how much the tutor does not know.

- A clone is the proof engine with some techniques removed or weakened, some misconceptions added
  (trusting the diagram, assuming the converse), and limits on search depth and attention. Clones
  share the engine; they differ in a small set of settings.
- Each real interaction reweights or prunes the clones. This keeps the envelope tied to the actual
  student.
- A new student starts with a wide envelope drawn from earlier students.
- **Test of the theory of mind:** before the student acts, the clones predict the next step; score
  them on how often they are right.

Candidate hidden variables come from Koedinger & Anderson's diagram-configuration model (1990):
experts parse a diagram into perceptual chunks that cue schemas and let them skip steps. Those
chunks sit between single proof moves and teaching content, and are the first candidates for the
technique layer. They are also what the 557 steps should be mined for (recurring step sequences,
e.g. I.5's "extend, cut off an equal, join, SAS twice").

### 3. The tutoring game

A POMDP: the tutor does not see the student's state.

- **Observations:** proposed steps (marked valid/invalid and progress/no progress by the proof
  game), hesitations, answers to questions.
- **Tutor moves:** stay quiet; ask a question ("what do you know about these two triangles?");
  name a technique; point to an earlier proposition; give the specific step; show a worked
  sub-case; give a counter-diagram; ask for a justification; step back to an easier proposition.
  The hint ladder (technique → tool → specific step) is one family of these.
- **Planning:** roll each candidate move out against every clone; prefer moves that help across the
  envelope; when clones disagree about the next step, ask the question that separates them
  (epistemic value). Expected free energy is the natural objective: it trades diagnosing against
  teaching.
- **Reward:** later unaided success and transfer to an unseen proposition. Not finishing the
  current proof: giving the answer finishes the proof and teaches nothing.
- **"Discover", not Moore's method:** the reference proofs always back the tutor. The student finds
  the step; the tutor knows it exists.

## Prior work to read before building

- Anderson, Boyle & Yost, "The Geometry Tutor", IJCAI 1985 — model tracing: follow which rules the
  student instantiates, correct below a threshold. Anderson, Boyle & Reiser, "Intelligent Tutoring
  Systems", *Science* 228 (1985) 456–462.
- Koedinger & Anderson, "Abstract planning and perceptual chunks: elements of expertise in
  geometry", *Cognitive Science* 14(4) (1990) 511–550 — diagram configurations.
- Corbett & Anderson, Bayesian Knowledge Tracing (1995) — the standard student model.
- Rafferty, Brunskill, Griffiths & Shafto, "Faster teaching via POMDP planning", *Cognitive
  Science* 40(6) (2016) 1290–1332; partial replication by Brückner & Nioche, ReScience C (2020),
  which found the different student models performed very similarly.
- Recent LLM-simulated students: StudentSim (arXiv 2609.01591) and Scarlatos, Lee, Woodhead & Lan,
  "Simulated Students in Tutoring Dialogues: Substance or Illusion?". Both report that prompted
  LLM students follow guidance too readily and know too much. That is an argument for clones built
  from a weakened proof engine and fitted to the real student, rather than role-played students.

## Grounding (carried from M-futonzero-grounding)

A tutor trained only against simulated students learns to teach simulated students. The envelope
is grounded only if (a) clones are fitted to real student behaviour and (b) the tutor's reward is
measured on real students' later unaided work. Clone prediction accuracy on held-out real steps is
the first measurement that can fail.

## Compute

Everything above fits on Zone (32 cores, 251 GB, no GPU). If training ever needs more, rent CPU
for proof checking, not GPU. A 2026-11-07 price survey (DigitalOcean, SF Compute, RunPod, Vast.ai)
is in the claude-9 session of that date; GPU rental is only needed for a large policy net, which
this design does not require.

## Open

- What should the first small build be: the fast proof engine for Book I, or a step-sequence mine of
  the 557 steps for technique candidates? The mine is cheaper and feeds both the clones and the hint
  ladder.
- ~~Does the existing prelim tutor move to futon0?~~ Yes (Joe, 2026-11-07): now
  `holes/labs/tutor/`.
- Euclid → the 17-gon: the course's later part needs field-theory moves (Gauss periods, quadratic
  towers). Out of scope until Book I works.

## Checkpoints

### Checkpoint 1 — 2026-11-07 (overnight; claude-9 reviewing, zai-2/zai-9/zai-11 writing)
**What was done:**
- `scripts/euclid_step_motifs.py`: def-use step-motif mine of the 557 Book steps (c396606, review fixes 7734881).
- `scripts/euclid_engine.py`: fast System E engine (diagram facts, exact linear arithmetic by Fourier–Motzkin,
  forward closure, constructions); replays I.1–I.5 without SMT, I.5 in 1.1 s (0b63539, 26c1759; review fixes
  c610627, 2745a36). Rules marked `[SMT-bridge]` stand in for what Lean gets from SMT; two were unsound
  (x = y with missing premises) and were found only by counterexample. Inventory: `system-e-inventory.md`.
- `scripts/euclid_search.py`: uninformed BFS. Finds I.1 (10 nodes) and I.3 (484 nodes, 14 s) with the Book's
  constructions; I.2 not found in 600 s (282e6d2).
- `scripts/euclid_certify.py`: emits LeanEuclid source from a step sequence and checks it with Lean; any error,
  nonzero exit or `sorry` warning is a failure. Book I.1–I.3 and found I.1, I.3 certify in about 1 s each (c13b7c9).
- Viewer: `E-euclid-tutor-data/viewer/` (JSXGraph), hover links drawn objects and Lean lines both ways (84a7e85).
  An isolated code-server with Joyride for driving it runs on loopback :8099 (no public route yet).
**Test state:** at 2745a36, engine/search tests 19 passed; certify 11 passed (zai-2's run); 0 failures.
**In flight:** contradiction detection plus I.6/I.7 (zai-11); prior-guided search on I.2 (zai-9);
engine review of student attempts (zai-2).
**Next:** I.8–I.10; clones as weakened engines; held-out prior test once more propositions replay.

### Checkpoint 2 — 2026-11-07 (overnight, continued; claude-9 reviewing, zai-2/zai-9/zai-11 writing)
**What was done:**
- Engine: contradiction detection and `by_contra`; replays of I.6–I.9 (3106383, b879b83, b6407e7). I.6 uses an I.18
  bridge rule, so it does not mirror Lean's argument; I.9 states four inputs Lean derives; I.10 is blocked on
  `proposition_9'`. Speed work (e8d5f9a): I.1–I.9 replays 6.2 s → 0.7 s, search about 1.7× more nodes per second.
- **Soundness bug, found late (ceea2b1):** rows written as dict literals `{X: 1, Y: -1}` collapse when X and Y are the
  same canonical atom. Book I.5 step 6 (`proposition_4 f b c g c b`) asserted |bc| = |cb| and the store received
  |bc| = 0, so the I.5 replay proved its goal on an unsatisfiable store. zai-2 found the unsat store while adding I.5 to
  search; the cause was traced in review. Fixed with an accumulating `lin()`; every replay now asserts a satisfiable final
  store (I.7 excepted: its hypotheses are contradictory by design). Lesson: an entailment test must also check the store
  is consistent; I.5 had passed review twice without that check.
- Search: I.4/I.5 entries (1b32dd6); uninformed search is hopeless on I.5 (17 nodes in 120 s; proposition_4 candidates
  explode). Priors (ea6927c, 27872f9): the motif prior makes I.3 worse; motif + "cite the most recent proposition" finds
  I.3 at 35× the uninformed cost; nothing finds I.2. So far no prior beats breadth-first search on Book I.
- Lean certification of I.1–I.3 (c13b7c9) and I.5 (44b9096, review pending).
- Student model: engine review of an attempt, each step valid/invalid and progress/no progress by BFS distance
  (abeb26c; engine failures are never shown as an invalid step: 7674773, 9ca1c0e, 385c867). Clones as weakened engines and
  an envelope with likelihood scoring and a posterior (3be9fd8, 5b72952, a2d5275); a clone that trusts the diagram
  (9bcba36, 6b73ad3, 2833202). On the Book I.1 the full engine gets posterior 0.82, depth-1 0.15, trusting 0.023.
- Viewer (JSXGraph): I.1–I.5 with angle sectors and the I.4 superposition, board fitted to the figure; each step's facts
  are checked against the drawn placement (94fe35f, 4209a90, 7dea3a5, 7dedfc4). It opens inside the :8099 code-server via
  Joyride (`~/notes/euclid-cs/open-viewer.cljs`); a public route is Joe's call.
**Test state:** at ceea2b1: engine, search, review, clones, diagram clone and trace tests 90 passed; prior search 13 passed;
step motifs 6 passed; 0 failures. Certify tests need Lean and were run by zai-2 (not rerun here).
**Open:** I.10 (`proposition_9'`); I.6 by Lean's area argument rather than I.18; I.9's four stated inputs; clones fitted to
real student steps (none collected yet); a prior that beats breadth-first search.
**Next:** review mode in the viewer (in flight, zai-9); I.10 replay; first real-student trial of the envelope.

### Checkpoint 3 — 2026-11-13 (EuclidZero: what a learned prover can and cannot do here; claude-9 with Joe)

Written to be read on its own by an outside reviewer (Joe plans to ask GPT Astra or Fable for a change of
approach). Every claim points to a file in `E-euclid-tutor-data/spiel/` (abbreviated `spiel/`) or another
repository path; the trial-by-trial record is `spiel/LEDGER.md`.

**Terms.**
- *The game*: System E as implemented by our engine (`scripts/euclid_engine.py`; inventory
  `E-euclid-tutor-data/system-e-inventory.md`). A position is a set of diagram and metric facts; a move applies a
  construction rule or cites an earlier proposition, with point-named arguments ("I.4 abc def", not opaque ids);
  closure derives consequences; the goal is the proposition's statement. The engine is the authority for legality and
  for whether the goal holds. OpenSpiel wrapper: `scripts/euclid_spiel.py`.
- *ZERO* vs *DEMO* (the distinction to report to Avigad): ZERO = learned from the rules alone (self-play or
  self-generated theorems); DEMO = anything from Euclid's or LeanEuclid's proofs (prefixes, curricula, traces), even
  when the search itself is ZERO. Every ledger row carries the label.
- *Arms*: trained scorer, untrained network of the same shape ("generation 0", which turns out to act as a uniform
  prior), random move order with fixed seeds.

**What was done, 2026-11-08 to 11-13.**
1. *Engine and play.* Book I.1–I.25 replay in the engine following LeanEuclid's own steps, with each SMT-closed
   premise written out as a cited System E axiom; several soundness bugs were found and fixed on the way (list:
   `~/notes/euclid-morning-report.md`, "Soundness findings"; `spiel/SOUNDNESS-1.md`). A play page (`viewer/play.html`,
   served on :8100) lets a person play I.1–I.25 with refereed case splits. A strong LLM playtester (codex-1,
   GPT-6 "Astra") solved I.1–I.25 in the game unaided mathematically; weaker models stopped at I.4–I.6, at the
   first auxiliary construction (`~/notes/euclid-overnight-ledger.md`, slices 1–15). This is DEMO-in-the-weights: the
   model has read Euclid.
2. *AlphaZero on point-named moves (Probes 1–14, DESIGN-1, DESIGN-2; `spiel/PROBE-*.md`, `DESIGN-1.md`,
   `DESIGN-2.md`).* ZERO self-play learned I.1 (PROBE-4: greedy 0/10 → 20/20). I.2 failed from scratch. A DEMO
   curriculum (Book prefixes, staged backwards) passed stage 1 but not stage 2: search never visited the good child,
   because the prior ranked it about 30th and all values tied near −1 (run 9, `LEDGER` D2-9, `CHECKS-8C.md`). No
   transfer to I.1/I.3.
3. *Self-generated theorems (DESIGN-3, claude-6's proposal; `spiel/DESIGN-3.md`, `D3-E1*.md`…`D3-E6A.md`).* Random
   legal runs from a proposition's givens; facts derived on the way become hindsight goals; needed moves by provenance
   or deletion replay; a goal-conditioned scorer is trained on "is this candidate needed"; Book propositions are held
   out (records stating any Book goal are excluded, up to renaming and equivalent orderings). Data: E1c (single goals
   from I.1–I.3 givens), E4a (two-fact goals), E6a (I.4–I.25 givens). Prover: best-first over pages of 10 candidates,
   cost = summed −log softmax(logits / T), T chosen by calibration on held-out self-made theorems, 400 opened nodes.

**Results (ZERO unless stated).**

| Problem (from its givens) | Trained | Untrained | Random (3 seeds) | Where |
|---|---|---|---|---|
| I.1 | proved, 11 nodes | 26 | 33 / 187 / 106 | `D3-E4B.md` E4b-2c |
| I.2 | **proved, 4 moves** (both trained arms) | not proved | none | E4b-2c |
| I.3 | not proved | not proved | none | E4b-2c |
| I.2 stage 2 (DEMO prefix) | proved | proved | proved | E4b-2c |
| I.4, I.9–I.13, I.15 | none | none | none | `D3-E5.md` |

- The I.2 proof: `line_from_points(a,b); proposition_1(b,a,L_1); off_line_strict_triangle(d_2,a,b,L_1);
  superposition(b,c,b,a,b,d_2,…)`. It uses System E's superposition rule, which Euclid avoids in I.2 (his proof uses
  circles). In System E, I.2 has a shorter proof than Euclid's.
- One run per trained arm; the I.1 margin is a single sample.

**Assessment (`spiel/D3-E5-ASSESS.md`, with the diagnostic).** A DEMO probe measured how each scorer ranks the moves
of Euclid's own proof, step by step (`scripts/euclid_book_path_probe.py`). Path costs (sum of −log p): I.1 4.3,
I.4 15.7, I.10 23.7, I.15 35.4, I.12 61.2, I.11 91.0, I.9 97.6; a 400-node search reaches about 10–15. Findings:
- the untrained network is a uniform prior (its costs equal Σ log #candidates);
- citations of earlier propositions rank very low (I.13 inside I.15: about 600th of 1,339), and the self-made data
  never contains citations of I.8 or I.13 (`D3-E6A.md`: random runs do not set up their premises);
- branching reaches thousands of candidates by step 5–10 (9,561 in I.9), mostly lines through pairs of points;
- a third to two thirds of each Book path is near-free steps (a line through two existing points, restating a diagram
  fact).
Conclusion: training on self-made theorems helps near its data (I.1; I.2 by a shortcut) and does not reach Book-length
proofs; more data of the same kind (E6b) is not worth running. The one lever identified is the game, not the scorer:
make lines through existing points and restated diagram facts part of closure, as System E's diagram inferences are
("make the co-exact facts free", `E-metagame` notes). Not yet measured.

**Related lines that bear on a change of approach.**
- `E-metagame/` (claude-6, then claude-9): the same self-generated-theorem method on small equational theories, where a
  learner with symbol-free features *does* transfer to an unseen theory (`LEARN-0.md`: 65–81% vs 14–34% for the best
  hand-written player on goals no theory trained on); lemma reuse (`EQ-6.md`); a guarded Horn engine for line geometry
  and its theorem yield (`HORN-1..3.md`).
- The Astra playtests (item 1 above): an LLM that plans in words, translates the plan into moves, and repairs from the
  engine's refusals solved I.1–I.25. Which part of that is the loop and which the prior is unmeasured.
- The tutor direction Joe chose on 2026-11-12 (prelims, self-study guides, ArSE question-and-answer with a Skeptic
  descending and a Prover ascending; Dutilh Novaes): `~/notes/claude-9/expository/` (TEACHING-CRITERIA.md,
  PLAN-arse-prelims.md, ARSE-SCHEMA.md). It does not depend on a learned Euclid prover.

**Questions for an outside review.**
1. Is a learned move scorer the right object at all, given branching of 10³–10⁴ and Book proofs of 6–12 steps, or
   should effort go into the game (free closure for co-exact facts, macro moves for proved propositions) and a
   symbolic search, as AlphaGeometry pairs a language model with a deductive database?
2. How should citations of fact-concluding propositions (I.8, I.13) enter training, if random play never sets them up?
3. Would an LLM proposer in Astra's loop, with the renaming control from the metagame, be the better ZERO-vs-DEMO
   experiment than a small network?
4. What would make a convincing report to Avigad: the I.2 superposition shortcut, the uniform-prior finding, the path-cost
   diagnostic?

**Test state:** prover tests 4 passed (77764be, codex-26); book-goal matcher 8 passed (0d6153c); hindsight generator 16
passed (6572a2e). Long suites (engine/trace/play, 400+ tests) were not rerun for this checkpoint.
**Paused:** E6b (retrain on I.4–I.25 data); the free-closure engine check awaits Joe's decision.
**Next:** outside review of the questions above; then either the free-closure probe or a change of approach.
