# Algorithm: eoi-next  (backs `M-x arxana-eoi-next`)

*The codified procedure an agent runs to take an **assembled draft** (eoi-new output) + a **proforma** →
a **submission-ready version in the operator's own voice**, by conducting a **second, adversarial
interview** whose answers AND rewrite-rules are **sourced from the operator**, while the agent maintains a
**live AIF² argument-graph** that challenges. Emits the **projection annotations** (the warrant edges) +
a **side-by-side** + the operator **backlog**.*

*The *model/why* is `futon4/holes/eoi-lifecycle.md`; the *design* is
`futon5a/holes/excursions/E-interest-mining.md`; the **argument-theoretic grounding** is
`futon5/README-aif+.md` (AIF+ / AIF² — §2 file shape, §11 contest, §12 the AIF² reading).*

**Status:** v1.0 — the **second-interview / AIF² contest** model. **Supersedes v0.x (transform/generation),
which was the load-bearing mistake** (Joe 2026-06-04: *"you're thinking about this as text generation …
the whole approach we have been taking is flawed."*). Validation run: the live Anthropic Fellows interview.

---

## The reframe (load-bearing — read this before running)

eoi-next is **NOT text-generation, NOT a transform, NOT copy-paste.** It is a **second interview.**

- The proforma's questions become a **new set of interview questions**.
- The operator's **eoi-new answers are the seed / hidden layer** — priors that ground (but do not dictate)
  the new answers.
- **The operator answers the questions himself.** The agent **guides** and holds an **argument model** in
  the background so its prompts are not glib ("you are so wise, Socrates") but substantive
  (**"Sorry Dave, I can't do that for you — a reviewer reads this and says X"**).
- **Both the new text AND the rewrite-rule are sourced FROM THE OPERATOR** (`:origin :operator`). The agent
  elicits and records; **it never authors the operator's prose and never invents the rule.**

The first validated datapoint (Joe, 2026-06-04), which fixes the shape:

| Donor (eoi-new) | Operator's answer (new text, his voice) | Operator's rule (the warrant) |
|---|---|---|
| "always be learning" (§1) | "As a fellow I'd be looking at how Baldwin style loops can be established to empower downstream users of tools like Claude to develop new capabilities" | *general interest → specific deliverable* |

That triple — donor span, operator's recipient text, operator's rule — **is one AIF² edge** (§ data
structure below). Sourcing the rule from the operator is what makes the hypergraph anchor real instead of
a satisficed label.

## eoi-new vs eoi-next — the two interview modes

Same concept (a seeded interview), set up for opposite difficulty:

| | **eoi-new** | **eoi-next** |
|---|---|---|
| Register | authorial voice | editorial imprimatur |
| Questions | mostly-static (the 8fold path subset) | derived from the proforma / recipient |
| Pressure | **moderate** — identify & resolve tensions among the operator's *own* claims | **hard mode** — adversarial: the agent generates the *reviewer's* attack and gates on it |
| Graph op | **builds the pro-side** of one lifeform (thesis + supports) | **runs the contest** — operator's answer-lifeform vs reviewer's-objection-lifeform |
| Exit | answers captured | every slot-thesis is **in the grounded extension** (all objections defeated) |

## The argument model = a live AIF² contest (grounded in README-aif+)

This is **not new infrastructure.** It is the **AIF² / aif2 machinery (M-aif2)** — the `.aif.edn` shape
(README-aif+ §2), the contest pattern (§11), the Walton/Reed reading (§12) — **pointed at the interview.**
Per recipient slot the runner stages a contest:

- the **operator's answer** is a `:claim :role :thesis :origin :operator`;
- the **reviewer's objections** are `:conflict` nodes the agent generates (the challenge ammunition);
- the slot resolves when the thesis is **accepted** — Dung grounded-extension over the attack graph.

The §5 Fellows push already given ("the resource framing reads as *wanting compute*, not *doing work*") is
itself a `:conflict :bite :logical` node biting the thesis. The interview is **already** the contest; this
doc gives it the data structure + the acceptability gate so it is principled, not ad hoc.

## The data structure (per-slot AIF² graph — the `.aif.edn` shape, README-aif+ §2)

```edn
{:slot "why-interested"
 :question "Why are you interested in participating in the Fellows program?"
 :nodes
 [{:id :thesis   :type :claim :role :thesis  :origin :operator :status :in-progress
   :content "<operator's answer — his voice, verbatim>"}
  {:id :seed1    :type :claim :role :support :origin :eoi-new/§1-right-view
   :content "always be learning"}
  {:id :warrant1 :type :frame :origin :operator          ; ← the operator's rewrite rule, as the warrant
   :content "general interest → specific deliverable"}
  {:id :obj1     :type :conflict :status :active :bite :logical :defeated-by nil
   :content "<reviewer objection the agent raises>"}]
 :edges
 {:supports [{:from :seed1 :to :thesis :via :warrant1
              :donor-span "always be learning"            ; exact substring in the eoi-new source
              :recipient-span "Baldwin style loops…"}]    ; exact substring in the slot answer
  :bites    [{:from :obj1  :to :thesis :note "…"}]}}
```

Two consequences fall straight out of README-aif+ §7:

- **The operator's rule is the `:frame` (warrant)** on the support edge. "Every `:claim` has ≥1 incoming
  `:supports` OR `:bites`; every spine node carries an `:origin`" ⇒ **a thesis with no warrant-frame is an
  orphan claim** — the typed form of the old "a bind with no rule is a smell." `:origin :operator` enforces
  that text *and* rule are the operator's.
- **The hypergraph anchor you wanted IS this `:supports` edge** — `donor-span → recipient-span`, labeled by
  the operator's `:via` warrant. The side-by-side overlays render from these edges (`hx/type`
  `arxana/eoi-projection-bind`, donor on the source endpoint, recipient on the annotated endpoint, the rule
  in `:note`).

## The classical argumentation code that supports the challenge

Three pieces, all already implied by README-aif+ — the "useful data structures / classical code" that keeps
the challenge principled instead of vibes:

1. **Dung grounded-extension acceptability = the slot-complete gate.** A slot closes only when its
   `:thesis` is *in the grounded extension*: every `:conflict` biting it is itself defeated (the operator
   answered the objection, which adds a defeating node). Degenerate single-thesis case:
   `thesis ∈ IN ⇔ ∀ obj biting thesis, obj is OUT`. **This is the hard-mode engine** — you cannot leave a
   slot while an undefeated reviewer-objection stands. Mechanical, not the agent's opinion.
2. **WR-9 bite-tagging (`:logical` | `:empirical`).** Every objection is typed. *Logical* = "this doesn't
   answer the question asked / doesn't follow." *Empirical* = "a reviewer won't believe this / where's the
   evidence." The defeat differs: a logical bite is defeated by reframing; an empirical bite by a fact or
   citation the operator supplies.
3. **The §7 / I1–I6 argument-shape checks, run as interview gates** — no orphan answer (every slot-thesis
   argued-for), warrant present, `:origin` tagged. The 12-row I1–I6 / G5–G0 audit becomes the EOI's
   *quality* readout (Layer 2).

## The Emacs command contract (target)

```
M-x arxana-eoi-next
  → prompt: draft essay (default: current Arxana essay = the eoi-new output)
  → prompt: proforma  (a file/entity, OR "none" → derive a VIRTUAL proforma)
  → spawns/uses a backing AGENT that conducts the interview below
  → per slot: builds the AIF² graph, runs the challenge loop, gates on acceptability
  → brings up the side-by-side (eoi-new draft ↔ proforma-fitted answers), panes linked by the warrant edges
  → returns the backing AGENT-ID so the operator can resume the interview / revise live in a repl
```

## Procedure (the runner — the interview loop)

1. **PROFORMA INGEST → typed slots → interview questions.**
   - *Explicit* (Fellows): parse the form; each field becomes a slot typed
     **{prose (w/ length budget) | discrete-choice | required-gap}**, and each prose slot becomes an
     **interview question**.
   - *Virtual / none* (Aleks, COGITO): **derive** the implicit slot-set from the target (JD criteria; or an
     outreach email's `{hook · the ask · shared-interest bridge · credibility · next-step}`). Save the
     virtual proforma as a stand-alone artifact (cf. `glasgow-RA-proforma.md`) so it can offset-annotate.

2. **SEED — load the hidden layer.** Import the eoi-new answers as `:claim :role :support
   :origin :eoi-new/§N` nodes. These are priors shown to the operator as *seeds* ("your §1 said …") — they
   ground the new answer; they are **not** pasted in as the answer.

3. **AUDIENCE MODEL → the objection generator's prior.** Identify the recipient + register: which
   figures/themes **land** vs are **tension**. Load-bearing — the COGITO↔Fellows inversion proves it (same
   figures, opposite calls: the agentic-coding/VSATARCS register **flattened for Prof Kelp**, **deployed for
   Anthropic**; dark-tower/CT figures **deployed for Aleks**). The audience model **feeds the `:conflict`
   generator** — an audience-tension figure is a `:conflict :bite :logical` ("this won't land with this
   reviewer → DEPLOY / FLATTEN / SUBSTITUTE / REDACT-and-relocate").

4. **INTERVIEW LOOP — per slot (the hard-mode engine):**
   a. **Pose** the slot's question, seeded by the relevant eoi-new claims (name the seeds; do not recite
      them as the answer).
   b. **Operator answers** → record verbatim as `:thesis :origin :operator` (his voice).
   c. **Operator names the rule** → record as `:frame` warrant `:origin :operator`. (Ask for it: "what's
      the transform from your seed to this answer?" — e.g. *general interest → specific deliverable*.)
   d. **Anchor** → create the `:supports` edge: `donor-span → recipient-span :via warrant` (exact
      substrings, for the overlay).
   e. **CHALLENGE loop (the contest):** the agent generates typed `:conflict` nodes — the objections a
      skeptical reviewer raises (sourced from the audience model + the question's actual demand). Present
      the **strongest undefeated attacker** as a real push. The operator defeats it (a reframed answer / a
      fact / a dropped-or-substituted figure = more graph). **Repeat until `:thesis ∈ grounded extension`**
      (no undefeated bites). Only then is the slot closed.

5. **COMPUTE N (= the cokernel).** Required slots with no operator answer. Split:
   - **N_auto** — mechanical, voice-preserving → autofill (metadata; references *proposed* from the warm
     network / CV buyer-trail; resolvable choices). **Propose, never fabricate** operator-only facts
     (emails, consent, probabilities) — mark *confirm*.
   - **N_hand** — substantive/decision → the **operator backlog** (the gating %s; real reference confirms;
     audience decisions; logistics).

6. **AUDIT (the lifecycle gate + the AIF² shape checks).**
   - *No orphaned content* (every eoi-new theme lands in a slot OR is deliberately redacted-with-tension-edge
     — not silently dropped) **AND** *no orphaned required slot*.
   - *No orphan thesis*: every slot-thesis has its warrant-frame and survives its bites (in the grounded
     extension). An open undefeated bite **blocks "send-ready"** — surfaced, not smoothed.

7. **EMIT.** (a) the fitted **vN** — rendered *from the graph* (the operator's `:thesis` texts), never
   hand-drifted; (b) **register vN as an Arxana essay** (a loader `.el`, the `fellows-v3-eoi-next.el`
   pattern: manifest with absolute `:source-file` + H2 `:sections`; run it) AND wire the **projection
   annotations** = one `arxana/eoi-projection-bind` hyperedge per `:supports` warrant edge (endpoints:
   `annotated`=vN slot with the **recipient span** as `:passage`, `source`=eoi-new section with the
   **donor span** as `:passage`; the **rule in `:note`**) — the commutative diagram; (c) **bring up the
   LIVE side-by-side** by calling `(arxana-essay-side-by-side <draft-id> <vN-id>)` (`arxana-essay-pair.el`)
   — left = eoi-new draft, right = fitted vN, panes linked by the projection-bind annotations. **This is
   the run's EXIT CRITERION: a markdown side-by-side file is NOT sufficient — the run is not complete until
   the Arxana pair view is up and the backing AGENT-ID is returned** (so the operator can chat/revise in a
   repl); (d) the **N_hand backlog**. **Each EOI is also emitted as a first-class `.aif.edn` lifeform** → it
   inherits the three-layer audit + the prose render (`render_aif2_prose.clj`-style) + cross-EOI contest.

8. **SEND (operator HIT).** On send, emit the typed **SEND-witness event** (`:send-witness`,
   `:plants-thesis`, `:lead-class`, `:projection {:from eoi-new :to vN …}`;
   `futon4/holes/eoi-lifecycle.md` §SEND-witness). *The send is always the operator's; the algorithm
   produces send-ready, it does not send.*

## What the agent must NOT do (anti-generation invariants)

- **Never author the operator's prose.** A `:thesis` is valid only with `:origin :operator`. If you catch
  yourself writing the answer, stop and ask the question instead.
- **Never invent the rule.** The `:frame` warrant is `:origin :operator`. A rule you made up is a satisficed
  label — the exact failure mode of v0.x.
- **Challenge from a real attack node**, never glib. Every push is a typed `:conflict` (logical/empirical),
  not "throw him some edges."
- **A thesis with no warrant-frame is an orphan claim** — flag it (the donor probably doesn't actually
  ground the answer), do not paper over it.

## Anti-laundering invariants (carried from the lifecycle)

- A figure may be **redacted but never orphaned** — its tension-edge stays in the (private) fabric even when
  absent from the (public) output. **Private fabric ≠ shipped output**; SEND is the private→public crossing.
- N_auto **proposes**; never fabricates operator-only facts (probabilities, referee consent/emails).
- Audit failures (an orphan either side, or an undefeated bite) block "send-ready" — surfaced, not smoothed.

## The payoff — each EOI becomes a lifeform

Because every EOI emits as a first-class `.aif.edn` (README-aif+ §10), it inherits:

- the **three-layer audit** (mechanised + narrative P4/P6 table + open-gap ledger);
- the **prose render** — the submission text generated *from* the operator's graph, structurally locked so
  it cannot drift (WR-8 graph-is-truth);
- the **cross-EOI contest** (§11 + the `:coalesces-from` quotient, §3): the "scattering experiment" — each
  EOI map-reduced toward **one overall EOI**, with EOIs that coalesce vs EOIs in tension *mechanically
  inspectable*. Any interest that finds **no** FUTON analogue (no coalescing support in the corpus) is
  flagged: legitimately-new, or legitimately-bogus — the E-half-mil-audit hook.

## Worked instances (the gold / validation)

- **COGITO** (virtual proforma; Prof Kelp; **shipped**): `futon5a/essays/glasgow-cogito-neurotech-RA-formal-application/`
  — draft `…-v1.md`; editorial layer `…-cover-letter-v3.md`; shipped `…-cover-letter-final.md`; virtual
  proforma `glasgow-RA-proforma.md`. Wired in Arxana (`arxana/eoi-projection` edge + offset-annotations).
  Audience call: **flatten** the agentic register. *(Pre-dates the interview model — recast as the first
  contest exemplar.)*
- **Anthropic Fellows** (explicit proforma; **the live validation run**): `futon5a/essays/anthropic-fellows-2026/`
  — eoi-new draft `…-v1.md`; the live second-interview answers land in the proforma-fitted essay. First
  captured datapoint: the §1 "always be learning" → Baldwin-loops triple above. Audience call: **deploy**
  the agentic register; §6 free-solo **redact-and-relocate**. In-progress slot: "why interested" §5 resource
  angle, against the standing `obj1` ("reads as wanting compute") attacker.
- **NEXT — Aleks outreach** (no proforma → virtual; dark-tower-fluent recipient): tests the §1
  virtual-proforma branch + a deploy-the-tower-figures audience call.
