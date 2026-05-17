# Stack-annotations schema

*Canonical schema for the unified AIF+ hypergraph at futon-stack scope.
Modelled on `~/npt/working-paper/annotations-v12.edn` (which annotates a
single essay); generalised to annotate the **stack-as-essay**: leaves,
arms, fitness criteria, sorrys, and their cross-cutting relations.*

**Status:** v0 schema design. No artefact written yet. This document is
the spec the canonical `stack-annotations.edn` will be authored
against, and the ingest pass that lifts the 16 existing
`.aif.edn` files into the unified form will be checked against.

**Authored:** 2026-05-17 by claude-13, as **session task S8 / Phase 2.d**
of `~/code/futon7/holes/M-interim-director-proxy-metric-inventory.md`
§4.5.

## 0. Why this exists

The npt UKRN working-paper apparatus produced two surfaces that
project from one annotation hypergraph: the prose draft
(`UKRN_WP_draft_v12.md`) and the typed-rewrite review surface
(`*Arxana Rewrite A/Rule/B*` buffers in
`dev/arxana-browser-rewrites.el`). Both read from
`annotations-v12.edn` — the single source of truth. Rewrites edit
the hypergraph; both surfaces update.

The futon stack has two analogous surfaces — the **War Machine**
strategic scans (system-side reading) and **VSATARCS** anthology
(human-side reading) — but they do not yet project from a unified
hypergraph. Each surface has its own ad-hoc representation of the
stack's structure; the 16 existing `.aif.edn` files in
`futon5a/holes/stories/` are a partial first attempt at typing
some of that structure, but they:

- carry the AIF+ vocabulary (claim / status / role / spine) without
  the npt hypergraph schema (Markov-blanket cut, hyperedge types,
  closure blocks, invariant-audit);
- are per-leaf isolated (each one a closed island);
- are not authored under any compositionality discipline (per Joe's
  diagnosis 2026-05-17).

The unification move: a single `stack-annotations.edn` that
**both surfaces read from**. Once it exists, typed rewrites apply
to the hypergraph, propagating to both projections. Joe's framing:
*"rewriting could then switch into a morphogenetic evolution of the
system."*

## 1. Compositionality discipline (named upfront)

This is the load-bearing concern Joe surfaced. The npt rewriting
framework (`~/code/futon4/README-rewriting.md`) treats each
transformation as a **section of a sheaf** over the essay's typed
open cover of entities — local edits, compatibility on overlaps,
gluing to a global section. Six invariants (**I1-I6**) constrain
the gluing:

- I1 Boundary Integrity • I2 Observation-Action Asymmetry •
- I3 Timescale Separation • I4 Preference Exogeneity •
- I5 Model Adequacy • **I6 Compositional Closure** —
  *removing one role / capability / exchange degrades but doesn't crash.*

At single-essay scope (npt), the sheaf-gluing operates over the
essay's section structure (§1, §2, §3, …) with one author and one
intent. At **stack scope** (this schema), the sheaf glues over a
**richer base space** — leaves, arms, fitness criteria — authored
piecewise over time by different agents (Joe + claude-N + codex-N)
with potentially-inconsistent local readings of overlapping spans.

The ingest pass that lifts the 16 `.aif.edn` files must therefore
include a **compositionality check at lift time** — not just a
mechanical map operation but an explicit gluing-condition pass
that surfaces:

| Class of compositional failure | Detection | Resolution path |
|---|---|---|
| **Region disagreement** | two `.aif.edn` files implicitly type the same shared span as different `:aif-region`s | flag for operator decision; default = the more recent or more specific reading |
| **Status contradiction** | a claim is `:operational` in one leaf, `:settled` in another, `:in-progress` in a third | reconcile against the actual repo state at ingest time; the substrate-2 query is authoritative |
| **Reference dangling** | a `:ref` field points at a sorry / mission / pattern that has since been retired | flag for cleanup; do not auto-prune (might mask information) |
| **Cross-leaf hyperedge implicit only** | two leaves both reference the same prototype but the implicit edge isn't typed | promote to first-class `:hx-type :iatc/edge` entry in the lifted file |
| **I6 violation** | removing any one annotation entity crashes the global section | the lifted graph fails I6; flag for restructuring; *do not* auto-suppress |

A clean lift produces a `stack-annotations.edn` that satisfies the
sheaf-gluing condition (local readings agree on overlaps) AND I1-I6
(global section is well-formed). A dirty lift produces a flagged
`stack-annotations.edn` whose `:lift-anomalies` field enumerates the
unresolved conflicts and whose `:invariant-audit` records the
current state of I1-I6 against the unified graph.

**Implication for the ingest pass (β).** It cannot be a one-shot
mechanical lift. The pass is iterative: lift → compositionality
check → operator triage → re-lift with resolutions → re-check.
The schema must support the iteration — every lift result is a
valid (if flagged) `stack-annotations.edn` that the surfaces can
read, so the operator can see the unified graph even when it has
known compositional defects.

## 2. The schema

### 2.1 Top-level shape

```edn
{:schema-version 1
 :stack          "futon"
 :stack-id       "arxana/stack/futon-v1"
 :generated      "2026-05-17"
 :generator      "claude-13 (manual draft); successor: babashka ingest pass"
 :sources        [;; provenance for each ingested artefact
                  {:kind :aif-edn-leaf
                   :path "futon5a/holes/stories/leaf-0.aif.edn"
                   :ingested-at "..."}
                  {:kind :aif-edn-devmap
                   :path "futon5a/holes/stories/devmap-futon0.aif.edn"
                   :ingested-at "..."}
                  ;; ...
                  {:kind :inventory
                   :path "futon7/holes/M-interim-director-proxy-metric-inventory.md"
                   :ingested-at "..."}
                  {:kind :fitness-contract
                   :path "futon0/docs/stack-fitness-completeness.md"
                   :ingested-at "..."}]

 :system         { ... see §2.2 ... }
 :sections       [ ... see §2.3 ... ]
 :annotations    [ ... see §2.4 ... ]
 :invariant-audit [ ... see §2.5 ... ]
 :lift-anomalies [ ... see §2.6 ... ]}
```

### 2.2 `:system` — Markov-blanket cut at stack level

The npt analogue is the `:system` block in `annotations-v12.edn`
naming the reader, the decision they face, and what they do with
the argument. For the stack, the reader is broader (Joe + future
agents + potential external readers) and the "decision" is
likewise broader (engagement / build / purchase / cite).

```edn
:system
{:outside
 {:reader            "Joe (operator); future agents (claude-N, codex-N) reading the stack as substrate; potential external reader of the descriptive-essay-of-the-stack and Director EoIs"
  :decision-they-face "Whether to engage / build on / purchase / cite the futon stack's work"
  :what-they-do-with-the-argument "Inform — not compel — engagement decisions; the stack offers evidence + apparatus, not a sovereign claim"}

 :active-surface
 ;; The arms (per inventory §2). These are what the stack acts via.
 {:arms #{:arm/arxiv :arm/apm-lean :arm/eoi-corpus :arm/vsat
          :arm/external :arm/stack-fitness :arm/superpod}
  :surfaces #{:surface/war-machine :surface/vsatarcs :surface/stack-hud
              :surface/director-eoi-side-a :surface/director-eoi-side-b}}

 :boundary
 ;; Where stack-internal meets stack-external. Falsification surface.
 {:proxy-metric-3-vector #{:coord/external-uptake :coord/internal-mastery :coord/pipeline-health}
  :fitness-criteria      #{:F1 :F2 :F3 :F4 :F5 :F6 :F7 :F8 :F9 :F10}
  :falsification-surface "Stack-fitness contract F1-F10 in ~/code/futon0/docs/stack-fitness-completeness.md is the operationally-checkable boundary"}

 :preferences
 {:succeed-as           "homeostatic software agent maintaining its own self-description"
  :explicit-preferences ["sorrys close not via decree but via evidence"
                         "operator inhabits substrate (F6)"
                         "marketable arms accumulate posterior evidence (D, A, E)"
                         "internal-mastery arms compound (B, F)"
                         "pipeline-health sustained (G; mark2 flowing; daily-batch live)"]}}
```

### 2.3 `:sections` — typed entities at stack scope

The npt `:sections` are essay sections (§1, §2.1, …). At stack
scope, sections are **richer**: leaves (from VSATARCS), arms (from
inventory), fitness criteria (from the contract), plus
top-level repo / mission / pattern aggregates. Each carries an
`:aif-region` per the Markov-blanket cut.

```edn
:sections
[;; --- VSATARCS leaves (ingested from .aif.edn files) ---
 {:id          "arxana/stack/futon-v1/leaf/0"
  :kind        :leaf
  :name        "Leaf 0 — Family root"
  :ref         "futon5a/holes/stories/leaf-0.md"
  :aif-region  :internal
  :from-aif-edn "futon5a/holes/stories/leaf-0.aif.edn"
  :nodes-lifted [:n0 :n1 :n2]  ; preserve provenance of which .aif.edn nodes
  }

 {:id          "arxana/stack/futon-v1/leaf/2"
  :kind        :leaf
  :name        "Leaf 2 — Inhabitable Surfaces"
  :ref         "futon5a/holes/stories/leaf-2.md"
  :aif-region  :active-surface  ; leaf-2 is about user-facing inhabitation
  :from-aif-edn "futon5a/holes/stories/leaf-2.aif.edn"}

 ;; --- Arms (ingested from inventory §2) ---
 {:id          "arxana/stack/futon-v1/arm/D-vsat"
  :kind        :arm
  :name        "Arm D — VSAT consulting"
  :ref         "futon7/holes/M-interim-director-proxy-metric-inventory.md#arm-d"
  :aif-region  :active-surface  ; D is the most direct revenue-act
  :status      :data-flowing
  :data-source "~/code/invoices/log.edn"}

 ;; --- Fitness criteria (ingested from F1-F10) ---
 {:id          "arxana/stack/futon-v1/criterion/F1"
  :kind        :fitness-criterion
  :name        "F1 — Explicit fitness state"
  :ref         "futon0/docs/stack-fitness-completeness.md#f1"
  :aif-region  :internal  ; F1 is system-internal observability
  :status      :satisfied}

 ;; ... etc.
]
```

### 2.4 `:annotations` — typed hyperedges over sections

This is where the npt schema generalises cleanly. Each annotation
is a hyperedge with `:hx-type` and `:endpoints`. Vocabulary lifts
from the npt set; the new entries are the cross-cutting types this
schema needs.

Vocabulary (npt-inherited):

- `:aif/role` — assigns a role to an annotated entity
- `:aif/timescale` — assigns a timescale (fast / medium / slow / glacial)
- `:iatc/edge` — typed edge (intervention-action-evidence-causal)
- `:annotation/grounds` — grounding relation (this entity is the grounds for that one)
- `:aif/invariant-witness` — this entity witnesses an invariant
- `:annotation/comment` — open prose commentary

Vocabulary (new at stack scope):

- `:stack/arm-feeds-section` — links an arm to the sections it covers (e.g. Arm D feeds the VSAT engagement sections)
- `:stack/criterion-witnesses-arm` — links a fitness criterion to the arm it constrains (e.g. F4 self-balance constrains Arm G + cross-cuts all arms)
- `:stack/sorry-attaches-to-section` — links a strategic-sorry entry (from `futon5a/data/alignment.edn`) to the section that holds it
- `:stack/cross-leaf` — explicit edge between two leaves that share content / dependency / supersession
- `:stack/surface-projects` — links a surface (War Machine / VSATARCS) to the sections it renders

Each annotation has the npt shape plus an optional `:closure`
block recording any typed rewrite that produced it.

```edn
:annotations
[{:id "stack-anno-0001"
  :hx-type :aif/role
  :endpoints [{:role :annotated     :section "arxana/stack/futon-v1/leaf/0"}
              {:role :role-source   :pattern "library/agent/sense-deliberate-act"}]
  :note "Leaf 0 carries the sense-deliberate-act pattern as its organising shape"
  :provenance {:lifted-from "futon5a/holes/stories/leaf-0.aif.edn :node :n0 :role"}}

 {:id "stack-anno-0017"
  :hx-type :stack/arm-feeds-section
  :endpoints [{:role :arm     :section "arxana/stack/futon-v1/arm/D-vsat"}
              {:role :feeds   :section "arxana/stack/futon-v1/leaf/2"}]
  :note "Arm D's VSAT engagement evidence anchors leaf-2's inhabitation claims"}

 {:id "stack-anno-0033"
  :hx-type :stack/criterion-witnesses-arm
  :endpoints [{:role :criterion :section "arxana/stack/futon-v1/criterion/F4"}
              {:role :witnessed-by :section "arxana/stack/futon-v1/arm/G-stack-fitness"}]
  :note "F4 self-balance constrains Arm G specifically; metabolic-balance shape (10th-shape) is the operational witness"}]
```

### 2.5 `:invariant-audit` — F1-F10 status + I1-I6 status

Two parallel audits at stack scope. F1-F10 is the
stack-fitness contract (futon0/docs/stack-fitness-completeness.md);
I1-I6 is the AIF² hypergraph integrity audit (the gluing
conditions). Both must be reported and tracked.

```edn
:invariant-audit
{:F-fitness  ;; from stack-fitness-completeness.md
 [{:criterion :F1 :status :satisfied :as-of "2026-05-17"
   :note "M-LGS substrate-2 operational; ~360K hyperedges"}
  {:criterion :F2 :status :satisfied-with-caveat
   :note "6/8 satisficing signatures; G.3 pending operator review"}
  {:criterion :F6 :status :violated :as-of "2026-05-17"
   :note "2026-05-04 → 2026-05-17 zero-commit gap = documented F6 violation"}
  ;; ...
  ]
 :I-hypergraph  ;; from README-rewriting.md I1-I6
 [{:invariant :I1 :status :satisfied :note "section boundaries explicit per `:aif-region`"}
  {:invariant :I6 :status :tentative
   :note "Compositional Closure not yet proven for the lifted graph; needs gluing-check pass"}
  ;; ...
  ]}
```

### 2.6 `:lift-anomalies` — compositionality residue

Honesty register: what the ingest pass *couldn't* resolve cleanly.
Each anomaly is a typed record naming the conflict, the involved
entities, and the resolution path (or its absence).

```edn
:lift-anomalies
[{:anomaly-id "anom-001"
  :class :region-disagreement
  :involves [{:section "arxana/stack/futon-v1/leaf/2"
              :source "futon5a/holes/stories/leaf-2.aif.edn"
              :reading {:aif-region :active-surface}}
             {:section "arxana/stack/futon-v1/leaf/2"
              :source "futon5a/holes/stories/devmap-futon4.aif.edn"
              :reading {:aif-region :internal}}]
  :resolution :operator-decided
  :resolution-note "Joe chose :active-surface (the more specific reading; devmap was generic)"
  :resolved-at "2026-05-17"}

 {:anomaly-id "anom-002"
  :class :I6-violation-pending
  :involves [{:section "arxana/stack/futon-v1/leaf/start-here"}]
  :note "Removing the start-here leaf crashes 35 cross-references (every leaf links back); strict reading of I6 says this is a violation"
  :resolution :design-decision-needed
  :proposed-resolutions
  [{:option :exempt-start-here
    :rationale "landing pages are structurally singular; I6 should not apply"}
   {:option :strengthen-cross-refs
    :rationale "make each leaf landing-page-independent (large refactor)"}
   {:option :weaken-I6
    :rationale "redefine I6 to allow named structural exceptions"}]}]
```

## 3. Cross-references

- `~/npt/working-paper/annotations-v12.edn` — the template; 252KB; single-essay scope
- `~/code/futon4/README-rewriting.md` — the typed-rewrite discipline (f:(A,B)→C); I1-I6 invariants; sheaf-section framing
- `~/code/futon4/dev/arxana-browser-rewrites.el` — the 3-up RewriteReview surface; corpus-registry refactor in flight (Phase 2.a) will make it target multiple corpora including this one
- `~/code/futon5a/holes/stories/*.aif.edn` — the 16 per-leaf precursors; sources for the ingest pass
- `~/code/futon0/docs/stack-fitness-completeness.md` — F1-F10 audit criteria (this schema's `:invariant-audit` source)
- `~/code/futon7/holes/M-interim-director-proxy-metric-inventory.md` — §2 arms (this schema's `:active-surface` source); §4.5 session plan tracking
- `~/code/futon5a/data/alignment.edn` — strategic sorry topology (this schema's `:stack/sorry-attaches-to-section` source once that vocabulary is exercised)

## 4. Open questions

- **Q-SA1.** Where does `stack-annotations.edn` physically live? Candidates: `futon0/data/`, `futon5a/holes/`, `futon3/holes/`. Recommend `futon5a/holes/stack-annotations.edn` — sibling to the stories/ corpus it lifts from, futon5a's substrate-2 mission home.
- **Q-SA2.** Does the ingest pass also lift mission docs (`holes/missions/M-*.md`) as sections, or are they out of scope at first cut? Probably out of scope for v1; missions are *acted on*, not *annotated about*.
- **Q-SA3.** When two arms touch the same leaf (e.g. Arm A and Arm F both touch the arxiv-related leaves), how are the `:stack/arm-feeds-section` hyperedges typed? Recommend allowing multi-endpoint hyperedges (a single annotation with N `:role :arm` endpoints rather than N pairwise annotations).
- **Q-SA4.** Strategic SORRY topology: should it live as first-class sections (`:kind :sorry`) or as annotations (`:hx-type :stack/sorry-attaches-to-section`)? Sorrys are entities with lifecycle, not relations — recommend first-class sections.
- **Q-SA5.** The npt schema has `:section/aif-region` deterministic per section. At stack scope, some sections (arms, leaves) might *span* multiple regions. Allow `:aif-region` to be a vector?

## 5. Versioning + next steps

This is **v0** of the schema. v1 will land once:

1. Q-SA1 through Q-SA5 are settled (operator decisions).
2. A prototype ingest pass (β) lifts ≥1 leaf cleanly under this schema.
3. The lifted artefact is consumed by at least one surface (War Machine `scan-*` or VSATARCS overlay).

Per the inventory's revised phase numbering:

- **Phase 2.d.0 — this schema doc (✓ landed today)**
- Phase 2.d.1 — settle Q-SA1..Q-SA5 (operator decisions, ~30 min)
- Phase 2.d.2 — prototype ingest pass on 1-2 leaves (~2-3 hours)
- Phase 2.d.3 — extend ingest to all 16 `.aif.edn` files; surface compositionality anomalies (~half-day)
- Phase 2.d.4 — operator triage of anomalies, re-lift (iterative)
- Phase 2.d.5 — adapt VSATARCS + War Machine to read from `stack-annotations.edn` as their source of structural overlay (Phase 3 begins here)
