# RECORDS-SPEC v2 — making the business-model corpus machine-checkable

**Date:** 2026-08-20 · **Status:** spec, for extraction + checker.
**Companions:** `SCHEMA.md` (findings + v2 deltas), `SPINE.md` (the architecture),
`cases/batch-{A..F}.md` (the prose corpus being formalised).

## Why

Every finding in `SCHEMA.md` was derived by hand — grep plus reading. Nothing
re-checks when a case is added or a field corrected. The corpus is small and
closed, so **universally quantified claims over it are decidable by
enumeration**; the only thing missing is machine-readable records.

The corpus is already shaped like a proof state: **68 `[recollection -
unverified]` markers** (sorries), **83 `[inference from …]` markers**
(derivation steps), **116 citations** (discharged holes). This spec makes that
explicit.

**What this is not.** There is no kernel here. Ground truth for any field is
empirical and arrives from outside — this is Lean *with sorries only the world
can discharge*. The checker is a lint plus a finite-model checker, not a prover.

## Record shape (EDN)

```clojure
{:case-id        :docker
 :org            "Docker, Inc."
 :entity-type    :company        ; :company :oss-project :research-system :benchmark :standard
 :period         {:from "2013" :to "2019" :note ""}

 :node-types     {:primary :dependency :secondary [:environment]}
 ;; :requirement :benchmark :bounty :dependency :call :environment :market-exchange
 :seat           :travelling      ; :travelling | :operating      (v2, defect 6)

 :chain          {:artifact "" :sponsor "" :payer ""
                  :terminal :interest}
 ;; :obligation-external | :obligation-manufactured | :interest | :none   (v2, defect 3)
 :terminal-note  {:manufactured-when "2021" :rests-on "adoption position"}

 :artifact-oracle {:what "" :hardness :mechanical}    ; v2, defect 4
 :sale-oracle     {:what "" :hardness :interpretive}  ; v2, defect 4

 :distance        {:hops 2 :note ""}
 :acceptance-event :aggregate     ; :per-unit | :aggregate | :none   (v2, PRIMARY predictor)
 :fixture-access  {:public :y :decisive :n}           ; v2, defect 2

 :beneficiary     :unknown
 ;; :self | :known-party | :known-party-clients | :unknown        (v2, NEW — see below)

 :knowable-at-T   "..."           ; verbatim, filled blind to outcome
 :adoption-outcome :scaled        ; v2, defect 1
 :capture-outcome  :acquired-sideways
 ;; :scaled :acquired-sideways :acqui-hired :failed :zombie
 ;; :sustained-noncommercially :open
 :lineage         {:successors [] :predecessors []}   ; v2, the golem/zombie distinction

 :prediction      {:claim "" :falsifier "" :horizon "2028-12-31" :frozen? true}

 :claims [{:text ""
           :provenance :cited        ; :cited | :inferred | :unverified
           :ref "https://…"          ; when :cited
           :from [:D1 :D4]           ; when :inferred
           :discharge ""}]           ; when :unverified — WHAT OBSERVATION WOULD CLOSE IT
 }
```

### `:beneficiary` — the new field (Joe, 2026-08-20)

The organising question going forward is **"what problem does this solve, and
for whom?"** with a gradient:

| value | meaning | Joe's examples |
|---|---|---|
| `:self` | solves a problem the builder had | Claude REPL in Emacs; Agency (cross-vendor agents talking) |
| `:known-party` | solves a problem a named other has | — |
| `:known-party-clients` | solves a problem that party's clients have | "Math.StackExchange as a validation test for full-text indexing" → JUXT |
| `:unknown` | solves a problem for parties not yet identified | "…or for unknown clients who would rather be my clients than JUXT's" |

This is layer 6's audience slot, stated at capability level rather than mission
level. It is the field the mission corpus lacks (`Gate:` = `operator-*`, 8/8).

**Every `:unverified` claim MUST carry `:discharge`** — the observation that
would close it. A sorry with no discharge condition is not a typed hole, it is
an omission.

## Conjecture shape

```clojure
{:id          :all-terminals-interest
 :statement   "Every case terminates in :interest."
 :quantifier  :forall
 :over        :cases
 :pred        [:= [:get-in [:chain :terminal]] :interest]
 :established "2026-08-20 session, by hand"
 :expected    {:holds? true :n 30 :counterexamples []}}
```

The checker runs each conjecture, reports `holds?` + counterexamples, and
**diffs against `:expected`.** A conjecture whose result differs from
`:expected` is either a regression in the data or a real change — both need
looking at.

## Conjecture set v1 — the reproduction test

These are the session's findings. **If the checker cannot re-derive them from
the data alone, the encoding lost something.** That is the acceptance bar.

| id | statement | expected |
|---|---|---|
| `:all-terminals-interest` | every case `:terminal` is `:interest` | holds, n=30 |
| `:no-external-obligation` | no case has `:terminal :obligation-external` | holds, n=30 |
| `:manufactured-needs-position` | every `:obligation-manufactured` rests on a prior adoption position | holds; Docker, Lightbend, Redis, Wolfram, OpenHands |
| `:sale-oracle-never-mechanical` | no case has `:sale-oracle {:hardness :mechanical}` | holds, n=30 |
| `:acceptance-predicts-survival` | `:acceptance-event :per-unit` ⇒ `:capture-outcome` ∈ {scaled, open} | check; Environment row was 3/3 |
| `:no-acceptance-implies-zombie` | `:acceptance-event :none` ⇒ not `:scaled` | check; PlanetMath, Emacs |
| `:operating-seat-rare` | count of `:seat :operating` | expect ≥3 (HackerOne, Gitcoin, Kaggle) |
| `:market-exchange-convergence` | ≥3 cases assigned `:market-exchange` | holds |
| `:every-unverified-has-discharge` | every `:unverified` claim has non-empty `:discharge` | **lint — must hold** |
| `:frozen-predictions-unedited` | every E/F `:prediction` has `:frozen? true` and a falsifier + horizon | **lint — must hold** |

## Extraction rules

1. **Do not re-research.** Extract only what the prose records already say.
   Every value must be traceable to the markdown.
2. Where v1 conflated two things (single `oracle`, single `outcome`,
   boolean `fixture-access`), the prose usually splits them by hand in the
   surrounding sentence — use that. If it genuinely does not split, emit
   `:unknown` rather than guessing.
3. `:knowable-at-T` is copied **verbatim**. Never paraphrased.
4. E and F records are **frozen**: `:prediction` copied verbatim, `:frozen? true`.
5. `:beneficiary` is new and mostly absent from the prose. Emit `:unknown`
   unless the record states who the payer is; do not infer a gradient position
   that is not written down.
