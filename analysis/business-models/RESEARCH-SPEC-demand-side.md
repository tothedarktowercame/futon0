# RESEARCH-SPEC — the demand side, and 32 more

**Date:** 2026-08-25 · **Status:** task specification, not yet run.
**Companions:** `SCHEMA.md` (findings + v2 deltas), `RECORDS-SPEC.md` (v2 record
shape + checker), `grid.bb` (the 32 x 5 acceptance grid).

## Why

The corpus has a blind spot it recorded about itself and did not close.
SCHEMA.md's "The systematic blind spot" says every row is written from the seat
of someone **travelling** the node. The field evidence agrees, and the numbers
are worse than the prose suggests:

- all 32 `:entity-type` values are supplier-side — 19 `:company`,
  3 `:research-system`, 2 `:oss-project`, 2 `:self-case`, 1 `:benchmark`,
  5 `:unknown`. **Zero customer-side rows.**
- `:seat` is `:unknown` on 28 of 32; `:beneficiary` is `:unknown` on 24 of 32.
- `:response-signal` — what the recipient actually **did** — exists on 2 of 32,
  and both are the first-hand batch-G cases.
- batches E and F carry 10 frozen predictions with 2028 horizons, every one
  about whether an AI **toolmaker** captures value. None is about a firm using
  the tools.

That last line is the whole problem. The population of interest — firms that
adopted agentic AI and made a mess of it — is the one population the corpus
has no rows for, in the section where it is otherwise fully pre-registered.

This spec does not ask for opinions about that population. It asks for records
in the shape the checker already reads.

## Two deliverables

**D1 — a demand-side record for each of the existing 32.** Who was on the other
side of the trade, what shape were they, and what stopped them buying. Where
the answer is not publicly available, the field is `:unknown` and that is the
finding. Do not infer a buyer from the seller's marketing.

**D2 — 32 new cases, customer-side.** Firms that adopted agentic AI (or an
earlier automation wave) and whose adoption went badly, well, or ambiguously.
The sample must not be all disasters: a corpus of only failures cannot separate
"agentic adoption is hard" from "these firms were badly run". Target roughly
2:1 mess to non-mess, and record the ratio actually achieved.

## Demand-side record shape (v3, EDN)

```clojure
{:case-id        :example-corp-agentic-rollout
 :entity-type    :customer               ; NEW. the seat this record is written from
 :for-case       :cognition-devin        ; supply-side case this pairs with, or nil
 :org            "Example Corp"
 :sector         "insurance claims processing"
 :period         {:from "2024" :to "2026" :note ""}

 ;; Rob's question, made a field. Layers between an individual contributor and
 ;; the person who can sign. The hypothesis worth testing is that depth
 ;; predicts WHICH phase fails, not whether one does.
 :management-depth {:layers 5 :signer "VP Claims Ops" :provenance :cited}

 ;; Which phase of the CUSTOMER'S own loop is failing. Vocabulary is
 ;; p4ng/empirics-futon/control-stages.edn -- the same five the grid columns
 ;; come from, read off the drawn control map.
 :failing-phase  {:phase "PERCEIVE" :why "" :provenance :cited}

 ;; What KIND of problem, from the closed list below. A record may carry more
 ;; than one; order them, most binding first.
 :problem-class  [:knowledge-concentration :capacity]

 ;; The distinction that motivated this spec. Tonic's head of research retiring
 ;; is a CAPACITY problem wearing the clothes of a strategy problem; UKRN's
 ;; decline was capacity too ("~2.5 person-days per week", :admits
 ;; :immediate-revenue-only). Neither is a business-model defect, and a remedy
 ;; aimed at the business model would miss both.
 :capacity       {:scale "" :admits :immediate-revenue-only :provenance :unverified
                  :discharge "what would settle this"}

 ;; What was adopted, and what went wrong. Empty is a legitimate answer.
 :adoption       {:what "" :when "" :scale ""}
 :mess           {:symptom "" :cost "" :attributed-to ""}

 ;; Which of the eight repository-transition slots are present and which are
 ;; empty: identity-space, invariants, diagnostics, strawman, counterstrawman,
 ;; counterstrawman-indicators, transformation-engine, job-argument.
 ;; (futon3/library/repository-transition/, derived from a real transition.)
 ;; An empty slot is the actionable finding -- Rob's COBOL company had a
 ;; strawman and no counterstrawman.
 :transition-slots {:present [:strawman] :absent [:counterstrawman :invariants]
                    :provenance :inference}

 ;; What the recipient DID when something was offered, if anything was.
 ;; :engaged | :declined-on-capacity | :declined-on-merit | :silent | :unknown
 :response-signal {:class :unknown :note ""}

 :claims         [{:text "" :provenance :cited :ref "R1"}]
 :evidence-refs  [{:ref "R1" :url "" :retrieved ""}]}
```

## Controlled vocabulary — `:problem-class`

Closed list. If a case needs a class not here, add it to this file with a named
case that forced it, the way SCHEMA.md's row 7 and row 8 were added.

| class | means | seed case |
|---|---|---|
| `:capacity` | the people who could act have no hours | Tonic (head of research retired); UKRN |
| `:knowledge-concentration` | what the firm knows sits in few heads, near the exit | COBOL shops; "top-heavy" senior staff |
| `:integration` | two working systems that must become one | Salesforce + Tableau |
| `:modernization` | a mature system must move without losing its invariants | the COBOL rewrite |
| `:code-quality` | what was built is not maintainable at the rate it grew | AppJet, "don't write spaghetti code" |
| `:adoption-mess` | agentic tooling adopted, output exceeds review capacity | D2's target population |
| `:business-model` | the chain terminates in an interest and no money moves | the existing 32's finding |

`:capacity` and `:business-model` are deliberately separate. Conflating them is
the error this spec exists to prevent.

## Disciplines (inherited, non-negotiable)

1. **Blind to outcome.** Fields describing what was knowable at the time are
   filled before the outcome is consulted. For D2 this means: describe the
   adoption from what was visible when it started, then record how it went.
2. **Cite or mark.** Every claim is either cited to a public source with a
   retrieval date, or marked `:provenance :unverified` **and carrying a
   `:discharge` condition** — what would settle it. The checker enforces the
   discharge; over 75 such conditions already exist.
3. **`:unknown` is a finding, not a gap to fill.** A demand-side record that is
   mostly `:unknown` tells you the buyer is not publicly legible, which is
   itself a fact about why the sale is hard. Do not guess to reduce nulls.
4. **No outcome-flavoured self-assessment.** "We would have been good at this"
   is not a field. The grid asks whether a simulation could be run and what it
   would have to beat; it does not accept a judgement in place of one.

## Acceptance gates

- `bb check.bb` runs green: 0 expectation mismatches, 0 load/identity errors.
  New records must not break `all-terminals-interest` or
  `no-external-obligation` **silently** — if a demand-side row refutes one,
  that is a result and the expectation is updated with the counterexample
  named, the way `no-acceptance-implies-zombie` already records Redis.
- `bb grid.bb` renders without error and reports the new case count.
- Every `:provenance :unverified` claim has a non-empty `:discharge`
  (`every-unverified-has-discharge`, already enforced).
- New conjectures to add with the batch, expectations stated before running:
  - `every-case-has-demand-side`: forall cases, a paired demand-side record
    exists. Expected to FAIL initially, with the count of missing ones.
  - `capacity-not-business-model`: forall demand-side records with
    `:problem-class` containing `:capacity`, the paired supply-side record's
    `:chain :terminal` is `:interest`. States the Tonic hypothesis as something
    that can be wrong.

## What this does NOT ask for

- No prose case studies. `cases/batch-*.md` exists; this batch is records.
- No scoring, ranking, or recommendation. The grid decides what is green, and
  only after a simulation runs.
- No filling the 160 grid cells. Cells move off `:unknown` when a simulation is
  run, not when a company is researched.
