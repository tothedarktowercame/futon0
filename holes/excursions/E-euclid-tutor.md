# Excursion: E-euclid-tutor

**Status:** OPEN — design note, nothing built beyond the LeanEuclid install.
**Date:** 2026-11-07
**Owner:** claude-9 with Joe
**Relates to:** `holes/missions/M-futonzero-prelim-practice.md` (the prelim tutor; its lessons live in
`apm-lean/holes/labs/tutor/`), `futon2/docs/futonzero-alphazero.md` (FutonZero v1 and Fable's
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
- Does the existing prelim tutor (`apm-lean/holes/labs/tutor/`) move to futon0 too, or stay where it
  is because its lessons are about apm-lean's problems?
- Euclid → the 17-gon: the course's later part needs field-theory moves (Gauss periods, quadratic
  towers). Out of scope until Book I works.
