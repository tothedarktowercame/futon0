# FINDINGS — AI-for-mathematics landscape probe

**Run:** 2026-07-27. Six lanes in parallel (L1 capital · L2 durability+digestion · L3 informal
proof-checking · L3b learning loops · L5/L6 funders+collaborators · V verification).
**Spec:** `TN-deep-research-landscape-position-2026-07.md`.
**Held-out control:** depintel.com, supplied by Joe before the run, withheld from every lane
agent. Result in §0.
**Budget note:** the session's WebSearch allowance (200) was exhausted; late verification ran on
direct WebFetch only. Three V-lane items and several L5 leads are marked accordingly.

---

## 0. HELD-OUT CONTROL: the probe did not surface depintel

**Result: MISS.** Depintel appears nowhere in any lane — not in L1's 45+ org roster, not in
L2, L3, L3b, L5 or V.

**Is that a recall failure? Mostly no — it is a SCOPE failure, and that is the more useful
finding.** Depintel (fetched directly, post-run) is an explainable-AI and formal-methods
company: *"artificial intelligence and machine learning should not only help people solve
problems effectively and efficiently, but also provide solutions that are interpretable and
trustworthy"*; products **Silas** (explainable structured-data analytics), **DeepDebugger**
(deep-learning visualisation/explanation), **ReaLLM**, **PAT — Process Analysis Toolkit**
(formal reasoning, model checking), plus sports analytics. Its own site does **not** mention
mathematics, theorem proving, Lean, or AI-for-math. (PAT is almost certainly the NUS Process
Analysis Toolkit lineage — *inference, unverified*, search budget exhausted.)

So no lane *should* have returned it under the briefs as written, which scoped everything to
**AI-for-mathematics**.

**But that is exactly the calibration this control was for.** Our thesis is not "AI for
mathematics" — it is **auditable, attributable instrumentation of reasoning**. Depintel is
doing that for ML models rather than for proofs: interpretability-as-the-product, sitting on a
model-checking substrate. Under the *thesis* framing it is adjacent; under the *field* framing
it is invisible.

**Consequence — a real gap in the spec, now recorded:** the probe's competitor set was drawn
by field, not by thesis. Drawn by thesis it must also include the explainable-AI vendors and the
formal-methods/model-checking companies, neither of which any lane touched. **If the pitch is
"we instrument what others assert," the room contains more people than the AI-for-math roster
shows.** A follow-up sweep of XAI + model-checking commercial players is now the highest-value
unrun lane.

**Recall verdict within declared scope: strong.** The lanes returned numerous entities absent
from the seed survey and from my own prior model — Mathlib Initiative, Mistral's Leanstral,
MerLean-Prover, Theorem Labs, Axiomatic AI, ARIA Safeguarded AI, QED-Nano/lm-provers,
FrenzyMath, Pythagoras-LM, Nous/Hillclimb — plus the single most decision-relevant fact in the
whole run (§3). The instrument works; it was pointed at the wrong-shaped target.

---

## 1. FALSIFIER HITS — read these first

### F-1. "Nobody with $200M is working on digestion" — **REFUTED on the non-profit side; upheld commercially.**

- **Mathlib Initiative**: $5M from Alex Gerko (XTX) via Renaissance Philanthropy, Sept 2025.
  8 employees + 8 contractors. Roadmap item **D3 Documentation and Educational Material**,
  Diátaxis classification, deliverable *"Tactics documentation completed" by Q3 2026*. Self-
  diagnosis names the problem verbatim: *"new contributors struggle with documentation gaps."*
  First time Mathlib maintainers have been paid (9 of 28 now are; 19 are not).
- **Lean FRO**: mandate includes *"usability, documentation"*; Y3 roadmap workstream #8 is
  **"Literate Programming & Next-Generation UX"** — *"mixed formal/informal document authoring
  tooling."* Convergent Research names the capability target outright: **"Computable
  Mathematical Papers."** Shipped artifact = **Verso** (597 commits, Lean FRO staff), which
  builds the Lean Language Reference and Tao's *Analysis I: a Lean Companion*.
  Funders incl. Simons Foundation International $5M and **$300K from Harmonic**.
- **AI for Math Fund**: $22M, 29 grants; **"Improved prover–mathematician interaction"** is one
  of five named funding categories.
- **Mathlib** enforces legibility as a merge gate: *"Every definition and major theorem is
  required to have a doc string"*, linted by `docBlame`.
- **DeepMind** ships a prose layer at **17% coverage** (71 Lean proof files, 12 prose files;
  ErdősProblems 9/9, StacksProject 0/11), hand-written, described in-paper as *"deformalized"*.

**Commercially it is genuinely vacant, and this was checked properly** — direct job-board
fetches across Harmonic (6 roles: 4 eng, 1 growth), Axiom, Math Inc., Theorem, Certora,
Axiomatic AI, and DeepMind (scraped live for `theorem prover`/`Lean`/`formal mathematics`:
**zero** hits; all 119 "formal verification" roles are ASIC/RTL hardware). **Not one technical
writer, documentation, DevRel, DX, education, or UX-for-proof role in the entire commercial
census.**

**The load-bearing legibility tools are bus-factor-1 and unpaid:** doc-gen4 (Böving, 380/500
commits) · Loogle (Breitner, 326/340) · **leanblueprint (Massot — last push 2025-12-23, and it
underpins FLT, Tao's PFR, and EquationalTheories)** · LeanExplore (an unfunded independent
researcher) · Paperproof (independent, and the highest-starred tool in the census at 537★).
The standalone Mathlib Manual is **explicitly deprecated**.

→ **L2 answer is (c) quietly occupied — but only by non-profits, and under-resourced.** The
sharpest formulation the probe produced: *formal artifacts are now produced at industrial rate
by well-funded orchestration infrastructure, while rendering them into something a human can
read is done by unpaid volunteers, a handful of grant-funded staff, and mathematicians writing
prose by hand afterwards.*

### F-2. Orchestration durability is **crowded and well-funded** — the opposite of the seed survey's read.

Harmonic's flagship engineering post is autoscaling and proof-tree state distribution
(~500,000 CPUs at 95–100% utilisation during IMO), not proving. Axiom's shipped artifact is
**AXLE**, a multi-tenant Lean cloud with 14 metaprogramming tools, an MCP server, and >500M
requests served. Numina pivoted from a prover to `numina-lean-agent` (**built on Claude Code**).
Mathlib Initiative built Queueboard. **If the demo bets on orchestration being empty, it isn't —
orchestration is the de facto product of every well-capitalised player, including the ones whose
press says "prover strength."**

### F-3. **The funder I was going to recommend has already funded a version of the methodology thesis.** ← most important finding for the letter

The AI for Math Fund's existing portfolio contains at least six neighbours, and one is severe:

1. **Databases of Structured Motivated Proofs — Timothy Gowers** (+ Tadipatri, Gandhi, Loader,
   Gerbscheid, Bayer). Verbatim: *"a comprehensive database of motivated proofs, where each
   idea's origin is clearly documented"*; *"a transparent and structured framework for
   understanding how mathematicians reason."* Gowers' group is described as *"focused on
   understanding the processes that human mathematicians use to find proofs."* Gandhi's stated
   research: *"the roles of failure and generalization in mathematical research."*
2. **Mathbench** (Bhat, Biderman) — proofs *"annotated with summaries, erroneous arguments, and
   logical structure metadata."* Direct hit on unjustified-step mapping.
3. **Polymath Plus** (Michelucci, Gowers again) — how mathematical collaboration works, instrumented.
4. **Sketchpad** (Li, Mai, **Paulson**, Xin) — proofs *"decomposed into individual statements."*
5. **An AI-Focused Tactic** (**Robert Y. Lewis**, mathlib maintainer) — *"analyzing the
   structural differences between AI-generated and human proofs."*
6. **BRIDGE** (Bauer, Berčič) — grounded in *"an analysis of how mathematicians create,
   structure, and interact with data in practice."*

**The daylight that survives, and it is real:** every one of those operates on **formal
artifacts** (Lean/Isabelle libraries, curated datasets) and is **tool-building** — Gowers
*constructs* motivated proofs; Mathbench *annotates a curated set*; Polymath Plus *builds a
platform*. **None maps an existing informal arXiv corpus at scale, observationally.**
Differentiate on: (a) informal literature, not formal libraries; (b) corpus-wide coverage, not
per-proof curation; (c) observation of work that already happened, not construction of new
artifacts. **Name Gowers and Mathbench in the letter's first page and concede the overlap** —
being caught by a reviewer who knows the portfolio is far worse.

### F-4. SAIR "distillation" is **not** human legibility — that lead is dead.

The Mathematics Distillation Challenge means compressing the 22M-implication Equational
Theories corpus into a **≤10KB cheat-sheet prompt** that raises a *cheap LLM's* accuracy.
Machine-facing. Stage 1 paid nothing; Stage 2 offers "modest prizes", amount undetermined.

### F-5. Five claims in the seed survey are wrong or unsupported. Do not let any reach a letter.

Ranked by damage:
1. **"Google DeepMind announced AlphaProof Nexus"** — the *name* is real (arXiv 2605.22763,
   21 May 2026) but there was **no DeepMind announcement**; it is a joint arXiv preprint with
   academic co-authors (Tsoukalas, Chaudhuri) and **no DeepMind blog post exists**. Say
   *"a DeepMind-affiliated team reported in a May 2026 preprint."* The correct product name
   makes the wrong attribution more dangerous — it looks checked.
2. **"Gauss finished strong PNT after eighteen months of stalled human effort"** — Math Inc.'s
   own page says Tao and Kontorovich *"announced **intermediate progress** in July 2025…
   **obstructed** by core difficulties"*; the word "stalled" never appears. Worse, the omitted
   caveat: *"with **targeted human scaffolding and review** of key lemmas and strategies."*
   Any sentence implying autonomy is refuted by the authors' own text.
3. **"Harmonic pitching Aristotle at blockchain/stablecoin developers"** — **zero** support in
   any primary Harmonic channel (homepage, full 9-post newsroom, Series C, Aristotle page).
   Delete it.
4. **"Seventeen researchers from Meta FAIR / GenAI / Google Brain–DeepMind"** — unconfirmed and
   in tension with Forbes's contemporaneous **"10 full-time employees"** at seed.
5. **"Axiom $64M seed at ~$300M"** — the $300M is a Forbes figure, never company-disclosed.

Also: Harmonic total "~$295M across three rounds" rests on a secondary-sourced $75M Series A;
Axiom's Series A is **"$1.6 billion+"**, not a flat $1.6B, and was raised for **"Verified AI"**
(code *and other machine-generated outputs*), not code verification alone; Zyphra's own phrase
is *"intelligence efficiency & silicon performance"*, **not** "intelligence density per
parameter" — that phrase appears nowhere in their materials. LeanMarathon is **confirmed real**
(arXiv 2606.05400) and says what the survey said it says — but it credits the
generation/verification/digestion trichotomy **to Tao (2026)**, so cite it as Tao's framing.

---

## 2. WHAT SURVIVED — the defensible claims, narrowed

### The comprehension gate is **genuinely unoccupied**. Confirmed, not assumed.

L3 tested four criteria: (i) grades informal proof for soundness, (ii) without formalization,
(iii) typed verdicts at >1 grain, (iv) principled abstention.

| Criterion | Verdict |
|---|---|
| (i) | **CROWDED** — PRMs, ProofGrader, RefGrader, MathArena, Open Proof Corpus. No novelty. |
| (ii) | **CROWDED** — but the *rationale* is now externally corroborated: "Beyond Compilation" measures a **29-point gap** between compiling (89.5%) and faithful (60.5%); BEq+ needs a ground-truth formal reference and so cannot judge novel work. |
| (iii) | **CONTESTED, NOT FREE.** RefGrader has a 7-way *fallacy* taxonomy at 2 grains — but it types **defects found**, not **conformance attained**, has **no N/A vs fail distinction**, and collapses to a scalar 0–7. Multi-axis verdict vectors exist only formalize-first. **Unoccupied specifically:** a typed conformance vector over *informal* proofs, with a containment lattice of grains, and N/A ≠ FAIL. Claim it narrowly. |
| (iv) | **GENUINELY UNOCCUPIED — the defensible claim.** Zero systems, informal or formal, emit a comprehension-failure verdict distinct from a soundness-failure verdict. PRM800K collapses *neutral into positive*. RefGrader/ProofGrader always score. **LeanTutor states the conflation outright and files it as future work**: *"a compiler error can indicate either an incorrect student proof step, or an autoformalization error. This is a limitation of the system."* The only abstention anywhere is Open Proof Corpus's **human** judges. |

Nearest neighbour: **RefGrader** (arXiv 2510.09021, Oct 2025) at 3/4 — and its failure mode is
**inverted**: low score = "bad proof", which is precisely the reading our gate refuses.

Also unoccupied: **no residual-gap map over a mathematical corpus exists.** Re²Math catches only
*citation-deferred* steps (860 gaps / 631 arXiv papers); the hedging-phrase class ("it is easy to
see", "left to the reader", "standard argument") is **entirely unmined**. The nearest complete
analogue — GAPMAP+TABI, explicit/implicit gap typing with Toulmin warrants at two grains — is
**biomedical**. The mathematics equivalent does not exist.

### THE REFRAME: the certificate is an **integrity** artifact, not a presentation artifact.

**The single most useful datum in the entire probe.** On the Lean Zulip LeanAgent thread (129
messages, read directly from the public archive): the PFR repo used `0 = 1` as a placeholder
statement, and **LeanAgent proved false statements and then used them to prove others.**
Tao: *"this was an unanticipated weakness of using 0=1 as a placeholder."* Carneiro on the
headline metric: *"a very convoluted and misleading way to report theorems proved."* Beffara
showed the flagship Wedderburn proof was `apply Field.toIsField` — findable by `exact?`. The
claim moved 162 → 155 and was reworded.

**The kernel was satisfied throughout.** This is a *durability* failure whose root cause is a
*conformance* failure — nothing checked that the formal statement meant what it claimed. That
makes a typed per-grain conformance certificate an **integrity** artifact: the missing per-grain
check is *what makes long runs corruptible*. Digestion and durability are not two problems.
**This reframing is worth more than the vacancy finding, and it comes with a public incident
involving Tao.**

### Durability: claimed but **not consolidated**; the measurement layer is unowned.

All four named harnesses are **not** owners of the problem — LeanDojo v1 is **deprecated**
(successor has 18 commits; Kaiyu Yang has moved to Meta FAIR), Pantograph (TACAS 2025) and Lean
Copilot are single-theorem interaction layers. Two independent claims now exist —
**LeanMarathon** (8 weeks old, 19 GitHub stars, five academics) and **MerLean-Prover** (arXiv
2605.26959; names *"context pressure"* and *"constraint pressure"*, externalises state into a
proof plan). Commercial players publish durability **numbers**, never durability **mechanisms**:
Axiom 869 min ≈ 14.5h on IMO 2026 Q3; Seed-Prover 1.5 IMO P3/P4 "3 days each"; AlphaProof "up
to three days"; Gauss "works autonomously for hours" sitting next to an admitted dependency on
"high-level expert guidance."

**Unowned sub-slot: there is no benchmark for durability itself.** FormalQualBench encodes it
only as a 4-hour timeout — a budget, not a property. No test exists for *"does this harness
resist drift and contain bad decisions?"* Two papers assert they do; neither can be falsified.

### Learning loop: the conjunction is empty; three mechanisms unoccupied outright.

**Occupied — do not claim:** per-item helpful/harmful counters (**ACE**, arXiv 2510.04618) ·
per-item causal attribution by counterfactual ablation (**AttriMem**, **MemAudit**) · Lean
compile as witness (the whole prover field) · contamination-stratified maths evaluation (GSM1k,
Putnam-AXIOM, MATH(), LiveAoPSBench) · memory invalidation on contradiction (**Zep**, shipped).

**Unoccupied:**
1. **Author ≠ actor by construction.** Nobody has it. ACE explicitly states it uses *the same
   LLM* for Generator/Reflector/Curator; Reflexion self-reflects; ExpeL self-extracts. Voyager's
   separate GPT-4 critic *verifies*, it does not *author*. This is the property answering "does
   the agent grade its own homework" — a question the field has not asked.
2. **Per-item use-receipts coupled to a mechanical verifier.** Attribution exists only where the
   witness is an LLM judge; mechanical witnesses exist only where reporting is aggregate. In
   Lean, **DreamProver**'s 58%-of-lemmas-reused / 71%-of-theorems is state of the art and is a
   corpus rate, not a receipt.
3. **Supersession driven by observed over-generality in use.** Zep invalidates on *contradiction*;
   DreamProver LRU-prunes on *frequency*; ACE de-duplicates on *similarity*. Retiring an item
   because its receipts show it was surfaced, used, and failed to help is not in the literature.
4. **Held out from MEMORY FORMATION, not just tuning** — un-named in the field. Cheap to claim.

**Sobering prior:** DreamProver cites **Berlot-Attwell et al. 2025** finding **no clear evidence
of effective reuse or performance gains under controlled compute budgets** for LEGO-Prover. The
field's previous library-learning result was a null. Our capability meter must beat that bar.

**"Self-improving loop" is saturated — do not say it.** ICLR 2026 Workshop on Recursive
Self-Improvement: **110 accepted papers**. Awesome-Self-Improving-Agents: **331 entries**.
A July 2026 survey co-authored by **Schmidhuber**. Letta sells *"a self-improving AI agent"* as
its homepage tagline. Lead instead with the discriminators — *a capability change traced to a
named stored memory with a receipt, authored by an agent that never used it, witnessed by a Lean
build someone else re-ran* — every clause of which empties the 331-entry list.

**Contamination design — harden before claiming.** Differential-slope-across-strata is nearly
unoccupied *in the predicted direction* (closest: Wu et al. 2507.10532, where the clean stratum
is a null check, never a stratum where gain is predicted to be *larger*). But four attacks land
first: (1) published-book-vs-unpublished fails the field's difficulty-matching bar — GSM1k
matched three covariates, Putnam-AXIOM pairs variants of the *same* problem; (2) slopes aren't
comparable across strata with different baselines (ceiling/floor confound); (3) "unpublished" ≠
uncontaminated — MathArena found 8/30 AIME-2025 problems online, and *soft contamination* shows
leakage crosses strata; (4) "contamination bias" already means something else in the DiD
literature. **Fix: replace the binary proxy with a continuous per-item contamination score
(n-gram, Min-K%, LLM decontaminator) and regress on it.**

---

## 3. DECISIONS

### D3 (letter) — **the target changed.**

**Apply to the AI for Math Fund SEED grant.** Rolling, decision ≤4 weeks, **up to $100K**,
≤12 months, and eligibility verbatim: *"Individuals, teams, academic institutions, nonprofits,
research institutes, and industry."* Open to unaffiliated individuals. Requires open-source
licence + arXiv preprint. The main 2026 round **closed 30 March**; decisions land August.

- **Harmonic's programme is not the target it looked like**: real (~$1M, PI ≈ $100K, announced
  22 Jan 2026) but **no published deadline and no grantee list at all** — there is no revealed
  scoring function to aim at. Rolling Typeform; contact directly.
- **AIMO**: nothing open. AIMO3 concluded June 2026; Proof Pilot concluded July 2026; no AIMO4.
- **NSF MFAI** is maths-foundations-*of*-AI, wrong direction, and requires a US institutional PI.
  **DARPA expMath** performers already selected (~$5M/team, 13 university teams). **Simons has
  no AI-for-math programme** and requires tenure-track status.
- **The letter must name Gowers and Mathbench and concede the overlap in the first page** (F-3).

### D3a — positioning relative to Gowers (Joe, 2026-07-27)

Joe's framing: *"I have sort of been drafting behind Gowers for a while — similar ethos, but he
is a Cambridge prof, Fields medalist and KBE, and I'm not. I can't compete in those
territories."* Correct, and the letter should not try. Three things follow from the probe data.

**1. It is not drafting; the lines are parallel and same-vintage.** Corneli et al., *Modelling
the way mathematics is actually done* (2017) and Pease et al., *Lakatos-style collaborative
mathematics…* (2017) are contemporaneous with Ganesalingam & Gowers, *A fully automatic theorem
prover with human-style output* (2017, J. Automated Reasoning — from memory, verify before
citing). Two independent lines starting the same year on the same question. Say "parallel", not
"following" — and cite him early and generously, which is both true and the best defence against
looking derivative when his funded project publishes.

**2. The roads diverge on a real axis: CONSTRUCTION vs OBSERVATION.** Gowers *builds* motivated
proofs — a normative artifact, hand-made, small-n, over formal material: what proofs *should*
look like. The mark7 apparatus *observes* an existing informal corpus at scale: what proofs
*actually* look like, hedges included. L3 confirmed the hedging-phrase class ("it is easy to
see", "left to the reader", "standard argument") is **entirely unmined**, and that no residual-gap
map over a mathematical corpus exists at all. These are complements, not competitors, and
complementarity is a *portfolio-completion* argument to a funder who has already bought the
construction side — a much better letter than a competition argument.

**3. The credential gap inverts on this specific task — this is the non-obvious point.**
A corpus-wide map of unjustified steps across 4,616 math.CT papers is socially expensive for a
Fields Medalist and KBE to publish: it reads as a senior figure grading colleagues' rigour.
It is *cheap* for an outsider whose instrument **refuses to say "this proof is weak"** — the
comprehension gate (low score ⇒ weak-EXTRACTION, never weak-proof) is precisely what makes the
observational study publishable without it landing as an attack, and CAS-CERT's refusal of the
word "verified" does the same work. **The gate is not only an epistemic commitment; it is the
social licence for the study.** Nobody with more standing to lose can run this comfortably.
That is a genuine advantage of position, not a consolation.

**Letter consequence:** lead with the complement and the licence, name Gowers and Mathbench in
the first page, and claim the observational half explicitly. Do not claim the methodology
question as territory — it is shared, and saying so is stronger than pretending otherwise.

### D1 (September demo) — **lead with the integrity reframe.**

Not "we can read proofs" but *"here is a public incident where a system proved false statements
from a `0 = 1` placeholder and used them downstream, with the kernel satisfied throughout — this
is the check that catches it, and it reports per grain, and it declines to say 'verified'."*
That story carries the comprehension gate (unoccupied, confirmed) and the grain lattice
(narrowly unoccupied) without ever making a prover-strength claim.

### D2 (sprint) — **unchanged in direction, sharpened in target.**

The corpus open-problem map remains the strongest deliverable, and L3 confirms **no residual-gap
map over a mathematical corpus exists** — with the hedging-phrase class entirely unmined and the
only complete analogue biomedical. Mark7 over 4,616 math.CT papers produces a field-specific
artifact with no competitor. Note the loop's capability meter must clear the LEGO-Prover null.

### L6 — collaborators, ranked by "already logs, no behaviour change needed"

**mathlib / Mathlib Initiative** (Queueboard; roadmap commits to quarterly review-queue reports
and **monthly HuggingFace datasets of goal states and tactic traces**) > **Equational Theories
Project** (live dashboard, raw JSON, Tao's personal log wiki, self-analysis paper) > **UW Math
AI Lab** — the best combined target: 178 undergrads / 19 grad / 68 projects, **all 229 human
prompts and 213 git commits archived**, plus a published failure analysis (*"Sorries Are Not the
Hard Part"*, arXiv 2606.13925) > **FLT/Imperial** (blueprint + Xena blog as running
retrospective) > **Bonn/van Doorn**.

**True gap shape (targets + students, no process methodology): CMU Hoskinson, EPFL, Math Inc.**
**Talia Ringer is a peer, not a gap** — her group already instrumented Coq to log every change 8
proof engineers made over a month (*"QED in Context"*, PACMPL).

**math.CT entry point: Emily Riehl.** Leads the ∞-cosmos project formalizing ∞-category theory
in Lean, presented at Lean Together 2025, ICM 2026 invited sectional speaker — simultaneously an
n-Category Café host *and* a formalizer, and her stated motivation (provers force rigorous
handling of weak equivalences and adjunctions) *is* "find the hand-waved steps." Also **David
Corfield** (philosopher of mathematical practice, n-Café host) and **Jules Hedges** — already an
AI for Math Fund grantee, so a warm dual-purpose contact. Guest posts on the n-Category Café are
accepted (precedent: Alexeev on Erdős-problem formalization, Dec 2025).

---

## 4. WHAT THIS PROBE DID NOT COVER

- **The held-out control's real lesson (§0): XAI vendors and model-checking/formal-methods
  companies were never in scope.** Highest-value unrun lane.
- Two timing signals worth watching: **Lean FRO's Y3 roadmap expires this month with no Y4
  published**, and its 5-year mission dissolves into a foundation in 2028.
- Unverified, flagged by lane agents: Axiomatic AI's product names (site unreachable), Morph
  Labs (recommend delisting — site is now agent-cloud infra, Trinity blog 410 / repo 404),
  Ginev's current KWARC role, TAC (site refused connection).
- The red-team pass (lane R) has **not** been run. Per the spec, findings are not accepted until
  it has been written and answered point by point.

---

## Addendum (2026-07-27, from live discussion): the Gowers probe — signal-competition read

Captured from the operator discussion following the run (tmux pane 6 + claude-4
session); feeds M-digital-nomad-patterns (futon2/holes/).

**History (Joe):** "I have sort of been drafting behind Gowers for a while — he
and I share a sort of similar ethos." The probe's §finding makes the draft
literal: his AI-for-Math-Fund project (Databases of Structured Motivated
Proofs) is a funded version of the methodology thesis, with six portfolio
neighbours.

**The signal grid (Joe):** Gowers holds {Cambridge professorship, Fields
Medal, KBE}. "I can't compete with him on any of those items or signals." This
is the Choose-The-Grid pattern's negative space stated exactly: that grid is
not playable, and optimising any score on it is wasted motion.

**The requirement this sets:** the Nomad patterns have to demonstrate their
capability as a war machine that can ENGAGE appropriately with Sir Tim and
related figures — engagement, not competition. Two distinct engagement
archetypes to design against:

1. **Engage-the-peak (the Gowers case):** approach on a complementary axis
   where the artifacts are self-certifying and the overlap is conceded on
   page one (per the probe's own advice). The daylight the probe measured is
   the axis: his portfolio builds tools over formal artifacts and documents
   how mathematicians reason; ours instruments an existing informal corpus at
   scale, observationally, with machine-witnessed receipts. Similar ethos +
   disjoint artifact class = a collaboration/citation surface, not a contest.
   Drafting-behind becomes drafting-ALONGSIDE exactly when the artifact
   carries its own validity (Self-Certifying Artifact is the entry ticket;
   Owner Of The Problem is the routing — Gowers the individual holds the
   discretion, not a posting).
2. **Climb-the-adjacent-ridge (the Carina Hong case):** no KBE-class signal,
   but a sufficient accolade stack plus deliberate upward network
   composition — proof that the grid can be ASSEMBLED from playable
   components rather than inherited. A different pattern instance: Choose
   The Grid run iteratively on winnable sub-grids, with the network as the
   compounding asset.

**Candidate deeper dive (flagged, not run):** a focused probe on the Gowers
portfolio — exact scope of the Motivated Proofs database, Mathbench's
annotation schema, where the instrumentation-thesis daylight is widest, and
which of the six neighbours is closest to a corpus-scale observational map —
plus the engagement-surface question (what would a page-one concession +
complementary-artifact letter to that portfolio look like). Letter From The
Future addressee candidate #3 (the thesis-framed adjacent room) may in fact
be this portfolio.

**Refinement (Joe, same discussion):** the Gowers contact is warm-but-faded,
not cold — a couple of in-person chats; possibly (unremembered) a support
letter for a past grant proposal. Direct approach ("showing up at his office
unannounced") is the named failure mode. The engagement route is
venue-mediated: another Big Proof-type symposium at the Isaac Newton
Institute, arrived at by invitation. This adds a guard pattern to the Nomad
set — **Licensed Ground**: engage where the convening licenses the
conversation; get there by contributing what the convening needs; the letter
justifies the invitation (addressee = organizers, not the peak).
WORK ITEMS: (1) verify what INI programmes/workshops in the Big Proof
lineage are scheduled (check the "Big Specification" programme and any
follow-ups) and who convenes; (2) check the archive for the possible Gowers
support letter (grounds the warmth claim); (3) shape the contribution that
makes the invitation self-justifying (proofcheck-readiness / learning-loop
receipts are the obvious candidates).
