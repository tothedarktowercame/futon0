# RESEARCH-SPEC — the demand side, and 32 more

**Date:** 2026-08-25 · **Status:** task specification. Apparatus ready, D1 not
yet run.
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
;; D1 rows are named <supply-case-id>-demand, so the pairing is visible in the
;; id and case-ids stay unique across the directory (the checker's
;; duplicate-case-ids runs over every record, not per file). D2 rows are named
;; for the customer, since they pair with nothing.
{:case-id        :example-corp-agentic-rollout
 :entity-type    :customer               ; NEW. the seat this record is written from
 :for-case       :cognition-devin        ; supply-side case this pairs with, or :unknown
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
 ;; :inferred is the honest default here. No source writes "their PERCEIVE
 ;; stage failed" -- the phase is read off what the source describes. Use
 ;; :cited only when a source names the step that broke in its own words.
 :failing-phase  {:phase "PERCEIVE" :why "" :provenance :inferred}

 ;; What KIND of problem, from the closed list below. A record may carry more
 ;; than one; order them, most binding first.
 :problem-class  [:knowledge-concentration :capacity]

 ;; The distinction that motivated this spec. Tonic's head of research retiring
 ;; is a CAPACITY problem wearing the clothes of a strategy problem; UKRN's
 ;; decline was capacity too ("~2.5 person-days per week", :admits
 ;; :immediate-revenue-only). Neither is a business-model defect, and a remedy
 ;; aimed at the business model would miss both.
 ;; :admits is a closed list -- what kind of work the party can still take on:
 ;;   :immediate-revenue-only | :funded-work-only | :maintenance-only
 ;;   :discretionary-capacity | :none | :unknown
 ;; Add a value here with the case that forced it, as with :problem-class.
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
                    :provenance :inferred}

 ;; What the recipient DID when something was offered, if anything was.
 ;; :engaged | :declined-on-capacity | :declined-on-merit
 ;; :forked-after-engagement | :declined-ground-unknown | :silent | :unknown
 ;;
 ;; The last two were added 2026-08-25 with the cases that forced them, the way
 ;; :problem-class grows. :forked-after-engagement is Lucid Inc. against GNU
 ;; Emacs: it depended on Emacs for Energize, met "unexpected technical
 ;; resistance in getting their changes merged", and forked rather than
 ;; declining or paying. Engaged, refused on schedule, exited by building the
 ;; alternative -- which is neither a decline nor a purchase, and predicts a
 ;; different remedy from either. :declined-ground-unknown is Andela: the
 ;; buyers demonstrably stopped taking junior placements, and no buyer states
 ;; why anywhere. Recording that as :declined-on-capacity would put an
 ;; unattested ground in the one field built to distinguish grounds.
 ;;
 ;; :by-buyer is the cardinality fix, and it is optional. Galois forced it: the
 ;; SPONSOR engaged and paid (DARPA, repeatedly) while the TRANSITION buyer did
 ;; not adopt, per DARPA I2O director Kathleen Fisher in 2025. One field cannot
 ;; hold two buyers answering differently, and collapsing them loses which
 ;; remedy could work on which party. :class stays the dominant signal.
 :response-signal {:class :unknown :note ""
                   :by-buyer [{:buyer "" :class :unknown :note ""}]}

 ;; :cited | :inferred | :unverified, as in RECORDS-SPEC v2. An :unverified
 ;; claim carries :discharge. So does an :unverified FIELD above -- the field
 ;; lint reaches nested :provenance maps, which the :claims lint does not.
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
| `:evidence-gap` | the party is acting on a quantity nobody has measured | xtdb#5637's unmeasured ~1.5-2x divergence estimate |

`:evidence-gap` was added 2026-08-25, by the rule above, with the case that
forced it. The xtdb maintainers' own public design record carried an estimate
offered with no data, alongside a named stress case they had not measured. That
is not capacity, concentration, integration, modernization, code quality,
adoption mess, or business model: nothing downstream of the number was wrong,
and there was no number. Joe, 2026-08-25, on what the remedy actually was:
*"what I have produced for them is a detailed benchmarking that increases
observability."* The class names the buyer's problem; the remedy shape is
measurement that makes the quantity visible.

`:capacity` and `:business-model` are deliberately separate. Conflating them is
the error this spec exists to prevent.

## Disciplines (inherited, non-negotiable)

1. **Blind to outcome, in the fields — not in the sample.** Fields describing
   what was knowable at the time are filled before the outcome is consulted.
   For D2 this means: describe the adoption from what was visible when it
   started, then record how it went. **The D2 sample itself cannot be blind:
   a case is findable precisely because its outcome was reported.** Say so in
   the batch note rather than letting field-fill order stand in for selection
   blindness. The corpus can carry an outcome-selected sample; what it cannot
   carry is one that claims not to be.
2. **Cite or mark.** Every claim is either cited to a public source with a
   retrieval date, or marked `:provenance :unverified` **and carrying a
   `:discharge` condition** — what would settle it. The checker enforces the
   discharge; over 75 such conditions already exist.
3. **Sources that count for D2.** Vendor case studies are the seller's
   marketing about the buyer, and D1 already refuses to infer a buyer from
   them; the same refusal applies to a non-mess case sourced only that way.
   What counts: regulator and audit-office findings, court filings, 10-K risk
   disclosures and earnings-call transcripts, published incident post-mortems,
   procurement records, and journalism with named sources. A vendor case study
   is usable for `:adoption` (what was bought, when) and not for `:mess` or
   `:response-signal`.

4. **`:unknown` is a finding, not a gap to fill.** A demand-side record that is
   mostly `:unknown` tells you the buyer is not publicly legible, which is
   itself a fact about why the sale is hard. Do not guess to reduce nulls.
5. **No outcome-flavoured self-assessment.** "We would have been good at this"
   is not a field. The grid asks whether a simulation could be run and what it
   would have to beat; it does not accept a judgement in place of one.

## Apparatus changes made before running (2026-08-25)

The gates below could not be met by the checker as it stood, and the reason was
not a finding about the world. `check.bb` loads every `.edn` in `records/` and
`:over :cases` meant all of them, so one customer row — which has no `:chain`
and no `:acceptance-event` — arrived as three `:indeterminate`s and turned
`all-terminals-interest`, `no-external-obligation`, and
`acceptance-predicts-survival` from `holds` to `undecided`. That reports
*absence of scope* as *absence of a verdict*. Verified by adding a single
record built from the shape above: 3 expectation mismatches, none of them about
any company. `grid.bb` had the same reach — the same one record made the grid
33 x 5 = 165 cells.

What changed, with the corpus still green at 0 mismatches afterwards:

- `:cases` is now the supply side explicitly; `:demand-cases` is the new domain.
  `grid.bb` takes rows from the supply side only and prints pairing progress
  beneath the grid instead of growing it.
- Three predicates added, because the two new conjectures were not expressible.
  `:some=` tests membership in a **sequential** value — `[:in ...]` compiles to
  `(contains? coll v)`, and on a vector `contains?` tests indices, so
  `(contains? [:capacity] :capacity)` is `false` and `:problem-class` is a
  vector. `:exists-record` and `:joined` read across records, which any claim
  about a *pairing* needs.
- `:unverified-fields` is a new lint domain. The discharge lint walked `:claims`
  only, and v3 puts `:provenance` inside `:capacity`, `:management-depth`,
  `:failing-phase`, and `:transition-slots`. No v1/v2 record has a nested
  `:provenance` anywhere, so a v3 record could have been unverified in every
  field with no discharge and passed clean.
- An explicit `nil` now reads as absent, alongside a missing key and `:unknown`.

## Acceptance gates

- `bb check.bb` runs green: 0 expectation mismatches, 0 load/identity errors.
  New records must not break `all-terminals-interest` or
  `no-external-obligation` **silently** — if a demand-side row refutes one,
  that is a result and the expectation is updated with the counterexample
  named, the way `no-acceptance-implies-zombie` already records Redis.
- `bb grid.bb` renders 32 x 5 = 160 cells and reports how many cases are paired.
- Every `:provenance :unverified` claim has a non-empty `:discharge`
  (`every-unverified-has-discharge`, n=75 today), and so does every unverified
  **field** (`every-unverified-field-has-discharge`, n=0 until this batch).
- Every `:for-case` names a real case (`for-case-resolves`).
- Conjectures added and expectations frozen 2026-08-25, before the batch ran:
  - `every-case-has-demand-side`: forall supply cases, some record's
    `:for-case` names it. Recorded today as **refuted at n=32, all 32 as
    counterexamples** — the blind spot re-derived by the checker rather than
    asserted in prose. Counterexamples are left out of `:expected` so the list
    shrinks visibly as D1 lands instead of tripping a mismatch on each partial
    batch.
  - `capacity-not-business-model`: forall demand-side records whose
    `:problem-class` contains `:capacity`, it does not also contain
    `:business-model`. **Not** the form first written here — "the paired
    supply-side record's `:chain :terminal` is `:interest`" cannot be wrong,
    because `all-terminals-interest` holds 32/32 and the join returns
    `:interest` by construction. This form a record can refute, and if one
    genuinely needs both classes then the separation this spec exists to
    enforce is what was wrong.

## What this does NOT ask for

- No prose case studies. `cases/batch-*.md` exists; this batch is records.
- No scoring, ranking, or recommendation. The grid decides what is green, and
  only after a simulation runs.
- No filling the 160 grid cells. Cells move off `:unknown` when a simulation is
  run, not when a company is researched.

## What to expect back, recorded before running

Three limits on the yield, written down now so a thin result is read as data
about the world rather than as the research having gone badly.

- **`:management-depth` is rarely public.** Layer counts and a named signer
  turn up in audit reports, litigation, and procurement records, and almost
  nowhere else. If it comes back `:unknown` on most of 32, Rob's hypothesis —
  that depth predicts *which* phase fails — is not testable from this batch,
  and that is the finding. Prefer cases where depth is legible when the choice
  is otherwise even.
- **The 2:1 mess-to-non-mess ratio binds on the non-mess third.** Failures get
  written up; ordinary working adoptions get written up by the vendor, which
  discipline 3 rules out as a source for the parts that matter. Record the
  ratio actually achieved, and if it cannot be hit under discipline 3, report
  the shortfall rather than filling it from marketing.
- **D1 will be mostly `:unknown`, and that is its result.** The question it
  answers is how legible buyers are at all. Run D1 first and read it before
  committing to D2's sampling.
