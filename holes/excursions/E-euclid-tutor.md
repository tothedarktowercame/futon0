# Excursion: E-euclid-tutor

**Status:** OPEN — proof-game prototype and first student-model pieces built overnight 2026-11-07 (Checkpoints 1–2).
**Date:** 2026-11-07
**Owner:** claude-9 with Joe
**Relates to:** `holes/missions/M-futonzero-prelim-practice.md` (the prelim tutor; its lessons live in
`holes/labs/tutor/`, moved from apm-lean 2026-11-07), `futon2/docs/futonzero-alphazero.md` (FutonZero v1 and Fable's
review), `futon6/holes/missions/M-futonzero-grounding.md` (why self-graded loops self-confirm).
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
