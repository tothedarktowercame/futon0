# TN — Deep-research probe SPEC: where does our work sit in the AI-for-mathematics landscape?

**Status: DESIGNED, NOT RUN.** This is the probe, not its findings. Nothing
below is a claim about the world; every assertion about the landscape is a
*hypothesis the probe is built to break*.

**Date designed:** 2026-07-27
**Precedent format:** `TN-deep-research-langton-lambda-for-metaca.md` (that one is
the post-run report shape; this is the pre-run spec shape)
**Out of scope by design — and there are now TWO distinct War Machine
exclusions, which must not be conflated:**

- **(i) WM as capability-per-unit-compute** (Zyphra axis, non-frontier
  hardware). Joe's original ruling: two different letters, two different rooms.
- **(ii) WM as the *target* of the learning loop's method port** (Joe,
  2026-07-27) — the "mathematisization of programming": the same scribe
  pipeline pointed at the WM's own corpus (cohort attempt records, repair arcs,
  typed failure kinds), with the demarcation criterion *a workflow is WM-able
  iff it can be theorised as a pattern cascade*, and the HITL pattern economy
  (per-instance HITs vs amortized pattern-supply) riding on it.
  `M-zai-learning-loop.md` §"Beyond mathematics" parks this until the
  mathematics side works; the mission's own scope discipline says
  *mathematics domain first*.

**(ii) gets its own deep-research packet.** It is a *different question*
(where does auditable pattern-cascade learning sit in the agent/dev-tooling
landscape?), a different competitor set (agent memory, dev-workflow tooling,
process mining), and a different buyer. Folding it in here would produce a
document that fits neither. **The shared spine both packets inherit** — worth
stating once so the sibling packet does not re-derive it — is
*witnessed / attributable / author ≠ actor / honest negatives*, plus the
demarcation criterion as the thing that decides what is portable at all.
Sequencing: this packet first; the WM packet is unblocked by the same evidence
(a capability meter read) that unblocks D1 here.

---

## 0. The decision this probe feeds

The probe is instrumental. It exists to resolve three decisions, and it should
be judged on whether it moves them, not on how much landscape it covers.

| # | Decision | Owner | When |
|---|---|---|---|
| D1 | Does the September demo lead with **orchestration+digestion** or with **corpus**? | Joe | before the letter is written |
| D2 | Is the **monthlong sprint** = prover × mined-arXiv, or does one leg get dropped? | Joe | sprint kickoff |
| D3 | Is the first approach **Harmonic PI grants + AI-for-Math Fund**, or a university lab, or neither? | Joe | this week (letter), Sept (probe) |

**Design consequence:** every lane below terminates in a sentence that starts
"therefore D1/D2/D3 should…". A lane that cannot terminate that way is cut.

---

## 1. The thesis under test

Stated as a conjunction so it can fail in parts. The survey's strategic read
reduces to:

> **T1 (vacancy).** Capital in AI-for-math is concentrating on
> verification-as-infrastructure for code and protocols, vacating the
> "middle" — orchestration durability and digestion.
>
> **T2 (fit).** The middle is where our twenty-year methodological thesis
> (pattern-language analysis of how mathematical work actually goes; making
> formal artifacts legible at varying granularity) already lives.
>
> **T3 (durability).** A demo built on orchestration+digestion survives the
> next prover release; a demo built on "our prover is good" does not.

**T3 strengthened (2026-07-27, after A13–A18).** The learning loop gives T3 its
strongest available form: a loop that turns a runner's work into typed,
attributable memory **does not merely survive a better prover — it consumes
one.** Swap zai for Aristotle and the loop gets better, because the runner is a
parameter of the loop, not its content. That is a structurally different
relationship to the frontier than "we compete with it" or "we route around it,"
and it is the sentence to test in L3b.

**The structural risk in this thesis, and the reason the probe must be
adversarial:** T1∧T2 has the shape *"everyone is leaving exactly the room I am
standing in."* That is the most seductive and least reliable conclusion a
landscape survey can reach. A probe that confirms it has probably been built to
confirm it. So each lane carries an explicit **falsifier** — the finding that
would kill the leg — and the probe reports falsifier-hits *first*.

Three distinct failure modes for T1, which the probe must distinguish, because
they imply different actions:

- **(a) Vacant** — nobody is working on it. → Move fast, first-mover.
- **(b) Crowded but unfunded** — academia is all over it (proof engineering,
  human factors, Lean usability), just without $200M. → The differentiator is
  *not* the topic; it must be the apparatus or the corpus. Different letter.
- **(c) Occupied quietly** — the funded players do have digestion workstreams,
  just not in their press releases. → The pitch becomes complementary, not
  contrarian; approach shifts from "grant" to "collaboration/acquisition of
  method".

(b) and (c) are the likely answers. The probe is worthless if it cannot tell
them apart, so **lane L2 is the highest-value lane, not L1.**

**Note on T2, added after the §2 correction.** T2 is stronger than a
twenty-year *claim*: the grain lattice in CAS-CERT
(symbol ⊂ concept ⊂ technique ⊂ proof) **is** "legible at varying granularity"
in reviewed, running code with a rendered artifact. So the probe is not asking
*could we work on digestion* — it is asking **whether the thing we already
built has a competitor.** That is a sharper and much more answerable question,
and it is why L3 was re-aimed.

---

## 2. Asset ledger — what "what we are doing" actually denotes

The probe cannot assess relevance of an abstraction. It needs the concrete
objects. Verified present in the workspace as of 2026-07-27:

> ⚠ **VERSION HAZARD — read this before anything else.** The first draft of
> this spec was written off `futon6/technote-arxiv-mining.md` and
> `M-superpod-mark2/mark3` and consequently described the system as a
> *corpus-mining pipeline* (BGE embeddings, R-GCN, reverse morphogenesis,
> SE-thread hypergraphs). **Those are the old prototypes.** The current system
> is **mark7 / the proof-check spine**, and it is a different product
> category. Anyone re-running this probe: start from
> `futon6/holes/proofcheck-readiness.html` (audited 2026-07-04), not from the
> technote. Every lane below is aimed at the mark7 reading.

**The spine** (from `proofcheck-readiness.html`): deterministic where possible,
LLM only for the residue, incremental over the corpus —

> **rung −1** structure-first concepts (*are the terms even defined?*) →
> **`:structure` lift** (parse the defining formula) → **symbol grounding**
> (bind symbols to per-paper domains) → **rung 0–2** proof-checking (*is the
> reasoning over them sound?*) → **rung 3** technique-grounding (the residual).
> Cascade reframe: each proof = a *sorry*; checks = a menu; `select` matches
> the menu to *this* proof's topology.

| Asset | State | Evidence |
|---|---|---|
| **A1. First Proof Sprint 1** | Ran, published. 55h, 341 commits, 3 agents, **4/10 correct, 4/10 right direction, 2/10 wrong**; both wrong on NO-answer problems (YES bias). Human-in-loop. | arXiv:2602.13587 |
| **A2. First Proof 2** | **Skipped** — required full automation. | `.../M-first-proof-2-readiness-mission.md` |
| **A3. Automation** | mark7 is **turnkey**: gated stepper S1–S12 with phase-completeness ledger, smoke test, fresh-host deps, in-flight loss snapshots. One 20h window. | `linode_stepper.py`; `mark7-rob-handoff.md` |
| **A4. Corpus / run** | **4,616 primary math.CT papers**, citation-ranked manifest, substrate committed (15MB). Shipped to Rob 2026-07-04; **no mark7 run since** — this is the "Rob has yet to run it" gap, and it is now the *only* thing between us and a full-subclass result. | `holes/math-ct-full.ids.txt` |
| **A5. Informal proof checker (rungs 0–3)** | **Built + independently reviewed**, author≠reviewer, gated. rung-0 argcheck · rung-1 substance gate (anti-degeneracy) · R2a anchor-faithfulness (flags 27→10 after de-noise, genuine miss preserved) · R2b closure · R2c warrant-resolution (report-only, *reasoned*) · R2d concept-coverage · `iatc_semcheck` aggregator + gate. On loop-run-70b: 4/9 finals FAIL on orphan nodes. | `proofcheck-readiness.html` §C |
| **A6. `:structure` lift + symbol grounding** | SFC2a/v2 deterministic **LaTeXML→Clojure** transducer (∀ ∃ set-builder ⇒ ≅); symbols emit `:grounding :hole`. SFC2b fills holes with a **defeasible evidence-check** — a binding cites verbatim evidence or is `:unsupported`; symbols never introduced are honestly `:undefined-in-context`. | `sfc_def_structure.bb`; `sfc_symbol_grounding.py` |
| **A7. CAS-CERT conformance certificate** | Deterministic per-paper aggregator. Ports **by grain: symbol ⊂ concept ⊂ technique ⊂ proof**; conformance = a vector by grain; **residual-sorry map = empty ports = ArSE seeds**; gate FAILs *only* on mis-wire; N/A ≠ FAIL. Says *"well-formed wiring — every port filled or flagged"*, explicitly **NOT "verified"**. | `cas_cert.py` |
| **A8. Comprehension gate** | Composes noun-grounding (R2d) ⊕ strategy-grounding (rung-3 ⊕ prose) → corpus-relative per-proof score. **The gate: low comprehension ⇒ "weak-EXTRACTION / study more / richer corpus", NEVER "weak-proof."** Finding: noun axis ~1.0, **strategy axis is the binding constraint**. | `clean_comprehension.py` |
| **A9. Lean↔NL bridge** | Herald 44,553 NL/Lean pairs; vocab reconciled to **12 discursive + 11 hidden-layer (silent bookkeeping) tactics**; mathlib-CT replication on Herald's 3,952-proof CT subset; **0.71 recall on CT discursive strategies**. | `herald_ct_endtoend.py` |
| **A10. Cascade select** | Q1–Q3 **resolved empirically**: the menu **is** the 39 `.flexiarg` patterns; the **matched-pattern set IS the topology** — no separate taxonomy. Registry dispatch built+tested. | `cas_checks.py`; `cas-sel-1-spec.md` |
| **A11. Shipped demo** | Per-paper anatomy → proof-check HTML, 4 papers spanning the range (clean / orphans / anchor-flag / self-loop). **Exists today.** | `proofcheck-demo/index.html` |
| **A12. Methodology** | Lakatos-style dialectical modelling; pattern languages. Twenty years. | Pease et al. 2017; Corneli et al. 2017 |
| **A13. zai learning loop** | **Chartered 2026-07-25, cohorts 1–2 run.** Runner does maths (zai) → scribe distills typed memories (codex) → recall injects at dispatch → receipts/challenges evaluate. **Author ≠ actor by construction.** Mechanical witness = `lake env lean` exit code + sorry count + commit sha, **independently re-run by ground control**, not self-reported. Store at cohort-2 pause: ~35 memories / 13+ patterns. | `M-zai-learning-loop.md`; `cohort-2-ops-log.md` |
| **A14. Outer loop closed — twice, in one day** | ⊸miss → typed diagnosis → scribe meta-draft → **applied fix**, live: TeX-normalization + thin-packet gate (`6e54cf2`), then extraction v1.3 stopwords/TeX-stripping (`0642c53`) **within two sessions of the miss that motivated it**, verified clean in the next live receipt. The system repaired its own retrieval while running. | `cohort-2-ops-log.md` ⊸win lines 19, 25, 28 |
| **A15. Belief revision under use pressure** | Supersession chain, 3 generations: `e-0b423578` (gap) → `e-ba5a8bee` (n=2, **over-general**) → `e-30e87097` (direction-scoped) + `e-0e4e32fe` (separate gap). The over-general merge was scope-challenged **because use revealed it**. Lakatos, running, with provenance. | ops log ⊸meter line 26 |
| **A16. Honest negatives, root-caused** | S4 "self-help test NEGATIVE, doubly instructive": (i) memory not surfaced — **pollution, not absence**, diagnosed from the receipt's own query terms; (ii) the memory **would not have applied anyway** — `IsBoundedUnder` vs `IsCoboundedUnder`, opposite direction, circular for unbounded-above. Typed failure kinds incl. `rule-misleading`, `held-out-contamination`. | ops log ⊸meter line 20 |
| **A17. Memorization-contrast design** | BPM (published Fa77–Sp98, **probably pretrained**) vs APM (**probably not**); the claim is the **differential slope**, not absolute performance. Held-out means held out of *memory formation*, not just tuning. | `M-zai-learning-loop.md` §memorization contrast |
| **A18. Typed self-talk register** | Ground control's loop narration constrained to marker-fronted typed statements (`⊸fix ⊸miss ⊸win ⊸prop ⊸meter`), deterministically parseable by the scribe meta-lane — **no model pass for meta-extraction**. **0 register violations through S3.** Falsifiable on three preregistered meters. | ops log; `M-zai-learning-loop.md` §typed self-talk peripheral |

**Design consequences, all three of which the first draft got wrong:**

1. **The product is not a corpus.** It is an **informal proof checker that
   grades natural-language mathematics without formalizing it**, emits a typed
   conformance certificate, and converts its own failures into questions. The
   corpus is *input*. Lanes aimed at corpus differentiation are aimed at the
   wrong artifact.
2. **LaTeXML is already in the stack** (`sfc_def_structure.bb` shells to
   `latexmlmath`; the fresh-host deps script now installs it and fails loudly
   if absent). The KWARC toolchain is a *dependency in use*, not a competitor
   and not a declined upgrade.
3. **"Digestion" is shipped, not aspirational.** A7's grain lattice
   (symbol ⊂ concept ⊂ technique ⊂ proof) **is** the "varying granularity"
   axis of the twenty-year thesis, in reviewed code, with a rendered artifact.
   T2 is therefore much better supported than the first draft credited — and
   the probe should be told to test it *as an existing system*, not as a claim.

**Still true and still load-bearing:** A1's honest number is 4/10 with a
documented YES bias. A probe fed the CV line ("competed against OpenAI and
DeepMind") returns flattering garbage.

---

## 3. Lanes

Each lane: **Q** (question) · **F** (falsifier — what kills the leg) · **S**
(source surface) · **PA** (named prior art that MUST be hit; a lane that
returns without addressing these has failed) · **→D** (which decision it feeds).

### L1 — Capital direction (cheapest lane; run first, weight last)

**Q.** Where is funded activity actually pointed — prover strength, code/protocol
verification, corpora, or orchestration/digestion? Read *job postings and
engineering blogs*, not press releases; funding announcements describe the exit
story told to investors, not the workstreams.

**F.** Any funded player's careers page or eng blog advertising proof
*explanation*, *presentation*, *documentation*, or *human-legibility* roles →
T1 is (c), not (a).

**S.** Careers pages, eng blogs, conference talks, GitHub org activity of the
funded players; Lean FRO's roadmap and funders.

**PA.** Harmonic (Aristotle); Axiom; Math Inc. (Gauss); DeepMind AlphaProof line;
**Lean FRO** — a *funded* org whose explicit mandate includes usability and
tooling, i.e. the most likely single refutation of "nobody funded works on
digestion".

**→D1, D3.**

### L2 — Is the middle vacant, crowded-unfunded, or quietly occupied? (**highest value**)

**Q.** Who is *actually* working on (i) long-horizon agent durability in formal
maths, and (ii) making formal artifacts legible to humans at varying
granularity? Return a roster with names, venues, and dates — not a vibe.

**F.** A live, maintained line of work that already does structured
human-legibility of formal proof at multiple granularities → T2's
differentiator is not the topic and must be re-sited (candidate re-sites: the
*informal* corpus side A4, or the dialectical/argumentation modelling A6, or
the wiring-diagram *typing* A5).

**S.** ITP / CPP / AITP / CICM proceedings 2024–2026; Lean Zulip; arXiv cs.LO,
cs.AI, math.HO.

**PA — must address by name:**
- **Talia Ringer** (Illinois) — proof engineering + human factors. Closest
  existing academic match to the pattern-language angle; if anyone has
  pre-empted A12-applied-to-formal-maths it is this group.
- **Patrick Massot** — Lean natural-language proof presentation / `verbose`
  mode / leanblueprint. Direct prior art for "digestion" in the narrow sense.
- **Jeremy Avigad** (Hoskinson Center) — formal maths + philosophy of
  mathematical practice; the other natural claimant to A6's territory.
- **LeanDojo / ReProver, LeanAgent, Pantograph, Lean Copilot** — the harness
  layer. Does "durable multi-hour harness" already have an owner?
- **METR**-style long-horizon task-length reliability work — is the durability
  question being eaten by *generic* agent infra, making a maths-specific claim
  indefensible?
- **The LeanMarathon paper itself** — read it directly, don't cite it from the
  survey. Confirm it says what the survey says it says (generation /
  verification / digestion; harness coherence as the bottleneck), and check its
  related-work section — it will have done half this lane already.

**→D1.**

### L3 — Informal proof-checking: does the rung ladder have a competitor?

**Q.** Who else **grades natural-language mathematics for soundness without
first formalizing it**, and emits a *typed, per-grain* verdict rather than a
scalar? This is the lane that asks whether A5+A7+A8 is a category or a
re-implementation.

**Two corrections carried from the first draft, both worth stating because they
change where the probe points:**
- The corpus is **input, not product**. "Is there already a processed arXiv?"
  is answered *yes* and is *irrelevant* — arXMLiv/ar5iv/llamapun own ingest,
  LaTeXML is **already in our stack** (`sfc_def_structure.bb` → `latexmlmath`),
  and Joe co-authored with Ginev and Kohlhase (NNexus Reloaded 2014; Planetary
  2011; eMath 3.0 2010). That is a **warm collaborator tie → L6**, not a
  competitive threat, and it should never have been the falsifier.
- The nearest competitor is not a corpus builder. It is the **process reward
  model / step-level verifier** line — the people scoring reasoning steps
  without Lean.

**F (correctly aimed).** The checker leg dies if there is published work that
does *all four* of: (i) grades informal proof steps for soundness, (ii) without
requiring formalization, (iii) emitting **typed** verdicts at more than one
grain, (iv) with **principled abstention** — a low score that means "I didn't
understand this" rather than "this is wrong". Partial matches on (i)+(ii) are
expected and do **not** kill it; PRMs do that already.

**The three claims to test separately, ranked by how exposed they are:**

| Claim | The thing | Exposure |
|---|---|---|
| **K1 deterministic-first ladder** | rung 0–2 are *deterministic* checks (well-formedness, anti-degeneracy, anchor-faithfulness, closure); LLM only on the residue — measured at **~73% deterministic / 27.3% residue** | Medium — the field defaults to learned scorers; a deterministic floor is unusual but not unheard of |
| **K2 grain lattice** | conformance as a **vector** over symbol ⊂ concept ⊂ technique ⊂ proof, with **N/A ≠ FAIL** and the certificate refusing the word *"verified"* | **Low — I know of no counterpart.** This is the "digestion at varying granularity" thesis in executable form |
| **K3 comprehension gate** | *"you may only judge a proof you understand"* — low comprehension ⇒ **weak-EXTRACTION, never weak-proof** | **Lowest. This is the crown jewel and the probe should try hardest to break it.** Structurally, a company selling verification cannot ship this gate; its incentive runs the other way |

**S.** NeurIPS/ICML/ICLR 2024–26 on process supervision and verifiers; ACL/EMNLP
on NL proof/argument evaluation; ITP/CPP/AITP/CICM; arXiv cs.LO, cs.CL, math.HO.

**PA — must address by name:**
- *The real neighbours:* **process reward models** — PRM800K / *Let's Verify
  Step by Step*, **Math-Shepherd**, step-level verifier and self-consistency
  work. Ask precisely: do any emit **typed** or **multi-grain** output, and do
  any **abstain** rather than score? My expectation is no on both, which is
  exactly why it must be checked rather than assumed.
- *The formalize-then-check route:* autoformalization pipelines,
  **Draft-Sketch-Prove**, **ProofNet / miniF2F**, document-level
  autoformalization (EPFL, per survey — verify). These answer a *different*
  question; the probe must not let them be scored as competitors.
- *NL proof critique:* natural-language proof grading, Putnam/olympiad
  auto-grading, **MathArena**-style evaluation, LLM-as-judge for proofs.
- *Gap/conjecture mining:* whoever extracts **open problems and author-declared
  gaps** from a corpus — the direct competitor to A7's residual-sorry map and
  **PASS3-HARVEST**, which the readiness doc calls *"the Rob value-demo"*.
- *The inverse-direction check for A9:* tactic **prediction** from proof state
  is owned (LeanDojo/ReProver). **Recognizing tactic-classes in prose** — the
  12-discursive / 11-hidden split at 0.71 CT recall — is the unusual direction.
  Check informalization (Lean→NL) work for overlap.

**→D1, D2.**

### L3b — The learning loop: does "our prover is good is DOA" still hold? (added 2026-07-27)

**Q.** A13–A18 look like they might escape the DOA verdict on prover-strength
demos. Do they? And who else is doing witnessed, attributable self-improvement
in mathematics?

**The answer the probe should start from, to be tested not assumed: the DOA
verdict is not broken — it is *sidestepped*, and that is the stronger
position.** The DOA claim was about *prover strength as a pitch*. A13 makes no
prover-strength claim; on raw proving power cohort 2's rows (2 sorries,
1 sorry, zero-progress, failed-cap, one 429) lose to Aristotle and Gauss
badly, and everyone in the room will know it. Lead with "look how much maths it
did" and it dies exactly as predicted. The actual claim is different in kind:

> **Nobody can tell you which weight update made AlphaProof better at Erdős
> problem #N. We can point at `e-ba5a8bee`, show it was surfaced, show it was
> used, show the outcome, and show it superseded when use revealed it was
> over-general.**

That is the *same* thesis as the checker — instrument what others assert —
one level up. Which makes the September story two-layered and coherent:
**the checker says what a proof's gaps are without claiming "verified"; the
loop says what the system learned without claiming a capability curve.**

**F — four falsifiers, in descending order of how likely they are to bite:**

- **F₁ (the sharpest, and it is fair).** The loop's most impressive events —
  A14, A15 — are the loop improving **its own retrieval**, not improving
  **mathematics**. A skeptic says: you built a system that gets better at
  surfacing its own notes and you measured that; the mathematical capability
  signal is absent. **The mission already concedes the structure of this**
  ("a cohort can demonstrate the loop runs while capability is flat, and must
  say so"; neither claim may lean on the other). **Status as of 2026-07-27:
  claim 1 (the loop runs) is demonstrated; claim 2 (self-improvement pays rent
  on held-out problems) is NOT** — S6, the first Ψ-weighted surfacing test,
  and BPM batch 1 have not run. The probe must not let enthusiasm launder this.
  **But read the calendar before reading this as a weakness: chartered
  2026-07-25, two cohorts in two days.** The meter is unread because the
  instrument is new, not because it was avoided — and the held-out design
  (A17), the contamination discipline, and the typed failure kinds were all
  preregistered *before* the first run. The correct presentation is therefore
  **"designed experiment, meter reads pending"**, which is a fundable state;
  what is not available is a capability *claim*. By the time this probe runs
  the meter may have reads — step 2 of §6 says to check first.
- **F₂ (positioning).** "Self-improving loop" is one of the most crowded
  phrases in AI. Say it and the reader files you with skill-libraries,
  reflection loops and agent-memory startups, then stops listening. Check the
  category density — and if it's as crowded as I expect, the pitch phrase must
  be **witnessed / attributable / author≠actor**, never "self-improving."
- **F₃ (n).** Cohort 2 ran 6 zai sessions, bounded by a 5h quota window, with
  the ops log flagging the window as a design constraint. A capability slope
  over tens of problems will not move anyone who reads scaling-law papers.
  The demo must lead with mechanism and show the series as honest-and-small.
- **F₄ (prior art).** Someone has already published attributable
  memory-driven capability gain in formal maths with receipts. Check hard.

**PA — must address by name:** memory-augmented agents and skill libraries
(Voyager-style), **Reflexion** and self-critique loops, retrieval-augmented
agents with experience replay, LeanAgent and continual-learning-for-Lean work,
and the agent-memory product category. For each ask the two questions that
actually discriminate: **(a) is improvement attributable to a specific stored
item with a receipt, or only to an aggregate curve? (b) is the memory author
distinct from the acting agent, or does the agent grade its own homework?**
Separately check **benchmark contamination methodology** — A17's
differential-slope design is a methods contribution *independent of whether the
loop works*, aimed at a live controversy, and may be the more fundable half.

**The one path that would genuinely break the DOA paradigm** — and it is short,
preregistered, and not yet walked: if the improvement slope on probably-novel
APM problems **exceeds** the slope on possibly-memorized BPM, that is evidence
for a claim the funded players cannot easily make — *improvement without
training, attributable to structure*. Then the pitch is not "our prover is
good" but "our prover learns in a way you can audit," and that is not DOA.
**It requires BPM batch 1 + cohort 3. Until those run, this is a designed
experiment, not a result, and must be presented as one.**

**→D1 (this may outrank the checker as the demo), D2 (cohort 3 competes with
the mark7 run for the same month).**

### L4 — Does prover × corpus actually compound? (the sprint's core bet)

**Q.** What is the published evidence that retrieval over an *informal*
research corpus improves *research-level* proof attempts? Distinguish sharply
from premise selection over mathlib (a solved-ish, different problem).

**F₁.** Strong published gains → the bet is sound but **the ground is
occupied**; the sprint must target the delta, not the capability.
**F₂.** No evidence either way → the bet is *novel and unhedged*. That is
fundable-as-research and dangerous-as-a-sprint-deliverable; D2 should then
demand a falsifiable in-sprint metric, decided at kickoff, not at demo time.

**Re-scoped for mark7.** "Prover × corpus" was the first draft's framing and it
mis-states the bet. The actual sprint bet is **checker × corpus**: does running
the rung ladder over a *full subclass* (4,616 math.CT papers) produce something
qualitatively different from running it over 9 papers? Two things it could
produce that 9 papers cannot:

- **the corpus open-problem map** (PASS3-HARVEST) — author-declared conjectures
  and residual sorries aggregated across a whole field. Named in the readiness
  doc as *"the Rob value-demo."* At subclass scale this is a **publishable
  artifact about math.CT**, not a demo of our tooling. That is the strongest
  single output the sprint can produce.
- **cross-paper hole recurrence** — WARRANT-NORM's finding is that raw
  `:wanted` text is *paper-local*, **0 cross-paper recurrence at 9-paper
  scale**; recurrence needs the concept key *and* more papers. 4,616 is the
  test of whether recurring gaps exist at all. A null here is itself a result.

**Two known binding constraints the sprint must respect, both already
measured — do not let the probe rediscover them:**
1. **Strategy is the binding axis, not nouns.** COMPREHENSION finds noun-side
   ~1.0 and strategy-side weak. So corpus scale helps *STRAT-REC* (562 vocab
   misses on the mark7 CT corpus) more than it helps R2d.
2. **The deterministic path is all-thin on arXiv** (CAS-SEL seam-6). Real
   topology matches need the Tier-1 LLM verify. Sprint plans that assume the
   deterministic path suffices at scale will produce empty selects.

**Sharpen with First Proof data:** Sprint 1's failures were (i) YES bias,
(ii) domain-depth gap, (iii) keyword-biased literature mining that *missed the
hyperbolic Hessian family on P4* and *searched only the YES direction on P7*.
(iii) remains a concrete, already-instrumented test case, and (i) is
interesting for a different reason — **a YES bias is exactly the failure the
comprehension gate is designed to prevent**, so First Proof 2 under A8 is a
real test of A8, not just of the prover.

**PA.** Retrieval-augmented premise selection (LeanDojo/ReProver) as the
*formal-side* analogue; open-problem / conjecture extraction from corpora;
any work aggregating proof gaps across a field.

**→D2.**

### L5 — Clients and yardsticks

**Q.** Which funders/programmes are live *right now*, with what deadlines,
eligibility, sizes, and — critically — what they have already funded (the
revealed scoring function beats the stated one)?

**F.** If a programme's existing grantees already include a
digestion/methodology project, the letter must differentiate against that
specific grantee, by name, in its first paragraph.

**S.** Programme pages, grantee lists, past awardee announcements.

**PA.** Harmonic $1M research grants (PI Awards, Rising Mathematician Awards);
**Renaissance Philanthropy AI for Math Fund** + its published grantee list;
SAIR Mathematics Distillation Challenge (distillation ≈ digestion — check
whether this is literally our deliverable); XTX/AIMO; Simons; Erdős-problems
database as a live collaboration surface; the AlphaProof/Erdős results as
*benchmarks other people will hold us to*.

**Deliverable shape:** a table — programme · deadline · size · eligibility ·
2 exemplar funded projects · our nearest-fit asset (A1–A7) · differentiator
sentence. That table is directly draftable into the letter.

**→D3.**

### L6 — Collaborators

**Q.** Which groups have (formalisation targets + students) but no methodology
for studying how the work goes — i.e. the complementary shape to ours?

**F.** A group that already runs process/methodology analysis on its own
formalisation work is a competitor, not a collaborator; route to L2's roster.

**PA.** UW Math AI Lab (per survey — verify); Imperial/Buzzard; CMU Hoskinson;
Illinois/Ringer; the mathlib maintainer community; EPFL autoformalization.
**KWARC (Ginev, Kohlhase)** — pre-existing co-authorship and LaTeXML already in
our stack; the *warmest* tie on the list and the cheapest first approach.
**The math.CT community specifically** — mark7 targets 4,616 primary math.CT
papers, so the first corpus open-problem map will be *about their field*. A
field-specific artifact is a far easier first conversation than a
methodology pitch.
Include for each: do they publish *process* artifacts (retrospectives, logs,
failure analyses)? Groups that already log their process are the ones where our
apparatus can be applied without asking them to change behaviour — that is the
cheap first collaboration.

**→D3.**

### V — Verification lane (runs on the survey itself, not the world)

The seed survey's numbers come partly from weak sources (Mindplex,
siliconvalleyinvestclub, unattributed "washington"). Load-bearing figures must
be re-sourced to primary before any of them enters a letter, because a wrong
valuation in paragraph one costs the whole letter its credibility.

Verify to primary source, or mark **UNCONFIRMED**:
Harmonic ~$295M/3 rounds, $120M Series C Nov-2025 @ $1.45B · Axiom $64M seed
@ ~$300M → $200M Series A @ $1.6B in five months · 17 researchers from Meta
FAIR/GenAI/Google Brain–DeepMind · **AlphaProof Nexus, nine Erdős problems,
May** · **Math Inc. Gauss, strong PNT, three weeks after eighteen stalled
months** · Harmonic's blockchain/stablecoin API positioning · Axiom's
$200M-for-code-verification framing · the LeanMarathon
generation/verification/digestion framing.

**Standing rule from the Langton probe:** a future-dated or single-source
citation that is load-bearing gets pulled up and read, not trusted on a vote.
Apply here to the AlphaProof Nexus and Gauss claims — they are the two that a
reader at Harmonic will know cold.

### R — Red team (run last, on the probe's own output)

One pass whose sole job is to write **the strongest case that this work has no
landscape position**. Required arguments, each to be answered or conceded:
- the middle is crowded-unfunded, not vacant;
- the rung ladder is a process reward model with hand-written features, and
  hand-written features lose;
- **the comprehension gate is unfalsifiable in practice** — if every negative
  verdict can be re-read as "we didn't understand it," the checker never says
  anything is wrong, and a checker that cannot fail a proof has no customers.
  *(This is the sharpest available attack on A8 and the probe must not
  soften it.)*
- 9 papers with 4/9 FAIL is a pilot, not evidence; the subclass run hasn't
  happened;
- **the learning loop has only been shown to improve its own retrieval**; every
  headline event (supersession, outer-loop closure) is the system tidying its
  notes, and the held-out capability meter has not been read once;
- **"self-improving loop" is a crowded phrase with nothing in it that a
  reviewer hasn't seen pitched five times this year**;
- 4/10 with a YES bias is not a credential;
- twenty years of priority converts to zero capital position (it did in 2002 —
  state why 2026 differs, or concede).

**The probe's output is not accepted until this pass has been written and
answered point by point.** House standard: this is the 3-vote adversarial
refutation discipline from the Langton probe, applied to a strategy document
instead of a theorem.

---

## 4. Synthesis and decision rule

Fixed in advance, so the answer is not chosen after seeing the data:

- **L2 returns (a) vacant** → D1 leads with the checker; the September demo is
  `proofcheck-demo/index.html` scaled to a subclass, and CAS-CERT is the
  artifact.
- **L2 returns (b) crowded-unfunded** → D1 leads with **K2+K3** (grain lattice
  + comprehension gate) rather than the topic. Academia can match the topic; it
  has not shipped an abstention-gated typed certificate.
- **L2 returns (c) quietly occupied** → D3 flips from grant-application to
  collaboration; the letter becomes "here is a method you are missing", and L5
  drops in priority behind L6.
- **L3: PRMs match K1 only** (expected) → D1 leads with K2/K3 and treats the
  deterministic ladder as implementation detail, not headline.
- **L3: K3 pre-empted** (an abstention-gated informal proof checker exists) →
  the strongest differentiator is gone; fall back to the **corpus
  open-problem map for math.CT** as a field-specific deliverable, which stands
  on its own regardless of who else can check proofs.
- **L4: cross-paper hole recurrence is null at 4,616** → the recurrence claim
  dies, but the open-problem map survives; the sprint deliverable narrows to
  PASS3-HARVEST + STRAT-REC vocab growth, both still real.

**Highest-value single outcome:** a defensible answer to *"what do we have that
$200M cannot buy in six months?"* Candidates, in descending order of durability
and now stated against the real system:

0. **Attributable improvement** (A13–A18). Improvement traceable to a named
   stored item, with a receipt, an author distinct from the actor, a mechanical
   witness, and a recorded supersession when use proves the item wrong. Money
   buys a better curve; it does not buy attribution — that is an architectural
   choice made before the first run, and the funded players made the other one.
   **Caveat that must travel with this claim: demonstrated for the loop's own
   retrieval, not yet for mathematical capability** (L3b/F₁).
1. **The comprehension gate as an epistemic commitment.** A company selling
   verification has an incentive gradient pointing away from "we may only judge
   what we understand." We can ship it because we are not selling verification;
   CAS-CERT explicitly refuses the word *verified*. This is a **structural**
   advantage, not a head-start — money does not close it.
2. **The grain lattice** (symbol ⊂ concept ⊂ technique ⊂ proof) as a reusable
   *measurement instrument* others can point at their own runs.
3. **Twenty years of process data** on how mathematical collaboration actually
   fails — a corpus of practice, not of papers.
4. FUTON as platform.

Note that 1–3 are all **instrumentation** claims, which is precisely what the
funded players cannot produce quickly, because instrumentation requires having
run the work.

---

## 5. Budget, stop conditions, out-of-scope

- **Shape:** L1 cheap/fast; L2 and L3 deep (they carry the two killable legs);
  V bounded to the enumerated claims; R single pass, written not voted.
- **Stop condition per lane:** stop when the lane can write its `→D` sentence
  *or* when it has hit its named prior art and found nothing — a clean null on
  L3 is a valuable result, not a failed lane.
- **Anti-goal:** a comprehensive map of AI-for-mathematics. Landscape breadth
  is not the deliverable; three decisions are.
- **Out of scope:** War Machine / Zyphra / non-frontier hardware (§0). Also out:
  anything requiring us to name a prover we would compete with — T3 says that
  framing is dead on arrival, so the probe must not quietly reintroduce it by
  benchmarking A1 against Aristotle.

---

## 6. To run this later

1. **Re-read `futon6/holes/proofcheck-readiness.html` first**, then correct §2
   to whatever is true on the run date — especially **A4: has Rob run mark7
   yet?** As of 2026-07-27 he has not (handoff 2026-07-04, no run since), which
   means every number in §2 is 9-paper-scale. A stale or wrong-version ledger
   poisons every lane; that is exactly how the first draft of this spec went
   wrong.
2. Run L1 → L2 → L3 → **L3b** in that order; L2, L3 and L3b can go wide in
   parallel. **Check first whether S6 / BPM batch 1 have run since 2026-07-27** —
   if they have, L3b's F₁ changes status and the lane's weight goes up sharply.
3. Run V concurrently — it has no dependency on the other lanes.
4. Run L4, then L5/L6 (their framing depends on L2's answer (a)/(b)/(c)).
5. Run R on the assembled output. Answer it point by point.
6. Emit findings as `TN-deep-research-landscape-position-FINDINGS-<date>.md`
   in the Langton report format: falsifier-hits first, then verdict by tier,
   then the L5 programme table, then the three `→D` sentences.

**Note on the timeline pressure in the seed survey** ("window measured in
months", "write the letter this week rather than after Rob replies"): D3's
letter does **not** need this probe. It needs L5's programme table and V's
verified numbers — a small fraction of the spec. If the letter is going out
this week, run **V + L5 only**, ship the letter, and run the rest against D1/D2
before the sprint. Blocking a one-week letter on a full landscape probe is the
failure mode this note is meant to prevent.
