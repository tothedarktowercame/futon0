# Algorithm: eoi-finalisation — produce structured-data sidecars for EOI essays

Date: 2026-05-26 (v0.1); 2026-05-27 (v0.2: inline-at-assembly-time policy)
Status: v0.2 (algorithm specified; engine-emits-at-assembly-time as of 2026-05-27; eoi-new launcher's Essays-landing-rule extended to include this sidecar; retrospective bootstrap completed for 4 essays under the v0.1 post-hoc protocol)
Owner: claude-3 (algorithm authorship); promotion to peripheral after N≥3 engine-emitted instances confirm the at-assembly-time pattern

## Purpose

The `eoi-engine` algorithm produces EOI essays (markdown). The `atthangika-buckets.json` `eoi_instances[]` entries capture engine-level metadata (composite-arrow, tensions, institution-object refs). But the **per-essay structured-data layer** — interest-territory tagging at passage granularity, cross-essay links at sub-essay granularity, operator-doubts as first-class entities — was missing for every EOI essay except the Operator's Foreword (which has it via the Arxana annotations.edn v2 schema).

This algorithm produces that missing layer as an `annotations.edn` sidecar following the **Arxana v2 schema** already validated by `~/code/futon7a/essays/operator-foreword/annotations.edn`. The output is what makes **map-reduce over EOIs** possible: aggregating across structured per-essay outputs to surface Joe's core interests, cross-essay engagement patterns, and gaps.

**v0.2 policy (2026-05-27):** the `annotations.edn` is produced **at engine-assembly time** by the eoi-engine itself, NOT as a separate post-hoc finalisation pass. The eoi-new launcher's Essays-landing-rule was extended on 2026-05-27 to include a Structured-finalisation landing rule that instructs the engine to emit annotations.edn alongside annotations.el. The retrospective post-hoc protocol (this algorithm's original v0.1 shape) remains the bootstrap protocol for essays produced before 2026-05-27.

Two invariants:

1. **No duplication with `eoi_instances[].`** Where the buckets entry already carries data (composite-arrow, tensions, institution-object refs), the annotations.edn references it rather than restating it. The annotations.edn ADDS the passage-granularity layer that buckets entries don't carry.
2. **Operator-asserted, not extracted by inference.** Like `eoi-engine`'s I7 (inhabited mode) — annotations are authored against passages the operator endorses, not pattern-matched. The engine extracts passages from the verbatim-already-typed Q1-Q5 + tension answers (since those WERE operator-asserted at interview time); the engine does NOT invent new passages or reword. Claude-in-the-loop proposes operator-interpretation `:note` fields; operator extends `:note` and adds annotations after the run.

## Companion artefacts

- `eoi-engine.md` (this directory) — upstream engine that produces the essay markdown AND (as of v0.2 2026-05-27) the annotations.edn sidecar at assembly time.
- `~/code/futon0/scripts/eoi-new` — launcher; the Essays-landing-rule and Structured-finalisation-landing-rule live inside this script's inhabitation prompt.
- `~/code/futon7a/essays/operator-foreword/annotations.edn` — schema prior art (v2, two-endpoint shape).
- `~/code/futon7a/essays/operator-foreword/editorial-log-v1.edn` — resolution log prior art.
- `~/code/atthangika-buckets.json` `eoi_instances[]` — engine-level metadata that finalisation references (no duplication).
- `~/code/futon4/holes/missions/M-interest-network-coupling.md` §2.1 (interest-event-vocabulary) — the cross-mission event vocabulary that map-reduce REDUCE-side events emit against.
- `~/code/atthangika-interest-surface-v1.edn` + `.md` — REDUCE artefact (bipartite-graph + markdown summary).

## Inputs

- An EOI essay's markdown file (e.g. `<repo>/holes/missions/M-expressions-of-interest.strawmen/<slug>.md`, `<repo>/applications/<slug>/<slug>-v1.md`, etc.).
- The corresponding `eoi_instances[]` entry, looked up by `source_file` field in the JSON (or `schema_ref` in essay YAML frontmatter if present).

## Outputs

Per essay:

- `<repo>/essays/<slug>/annotations.edn` — sections list + EOI-specific annotations.
- `<repo>/essays/<slug>/editorial-log-v1.edn` — initially header-only; appended to as operator resolves annotations.

The `<repo>` is the repo holding the essay's markdown (per-repo Arxana convention; preserves privacy boundaries for npt/* applications vs futon7a/* public surfaces vs futon5a/* strawmen). The essay markdown is NOT moved or symlinked in v0.1 — `annotations.edn` `:paper` field carries an absolute path to the source. Moving the markdown into `essays/<slug>/` is a v0.2 question (deferred until consolidated).

## Schema (v0.1, extends Arxana v2)

`annotations.edn`:

```edn
{:version 1
 :paper "<absolute path to essay markdown>"
 :essay-id "arxana/essay/<slug>-v1"
 :eoi-instance-ref "<eoi_instances[].id, e.g. anthropic-institute-analyst-2026-05-11>"
 :institution-object-ref "<:node/... from buckets entry>"
 :generated "<ISO date>"
 :pass-status :v1-finalisation-in-progress
 :pass-purpose "Structured-data finalisation: interest-territory tagging + cross-essay links + operator-doubts + Path-arrow witnesses"

 :sections
 [{:id "arxana/essay/<slug>-v1/section/<idx>-<short-slug>"
   :name "<verbatim heading>"
   :index [<num>]
   :heading-level <2|3>
   :aif-region :internal | :outside-facing | :active-surface
   :path-arrow :right-view | :right-intention | ... | nil}]

 :annotations
 [{:id "hx:<slug>:<short-id>"
   :hx-type :eoi/<one of six below>
   :endpoints [{:role :annotated :section-id "<arxana-id>" :passage "<verbatim quote from markdown>"}
               {:role :target :section-id "<arxana-id>" [:passage-pattern "..."]}]
   :note "<operator interpretation: why this passage matters>"
   :severity :high | :medium | :low | :informational
   :status :open | :addressed | :addressed-differently
   :discharge-procedure "<plain-language: what addressing this would look like>"
   ;; :eoi/synthetic-question only:
   :question "<the question>"
   :expected-answer-area "<where the answer should be findable>"}]}
```

**Six `:hx-type` values** (the v0.1 EOI vocabulary):

| `:hx-type` | What it asserts | Endpoint shape |
|---|---|---|
| `:eoi/interest-territory` | Passage reveals an interest-territory (a recurring interest Joe holds across work). Target identifies the territory. | annotated = passage; target = `arxana/interest-territory/<slug>` |
| `:eoi/cross-essay-link` | Passage in this essay engages a section/passage in another essay. | annotated = this essay; target = other essay |
| `:eoi/operator-doubt` | Passage surfaces a Joe-side uncertainty, doubt, or open question. | annotated only |
| `:eoi/path-arrow-witness` | Passage exercises a specific Path-arrow as primary or secondary. | annotated = passage; target = `arxana/path-arrow/<arrow-slug>` |
| `:eoi/institution-object-ref` | Passage anchors an institution-object reference (named institution Joe is approaching or self-authoring). | annotated = passage; target = the institution-object |
| `:eoi/synthetic-question` | A needle-in-haystack question derivable from the passage, answerable from elsewhere in the corpus. | annotated only; `:question` + `:expected-answer-area` fields |

`editorial-log-v1.edn` (initially header-only):

```edn
{:version 1
 :essay-id "arxana/essay/<slug>-v1"
 :editorial-log []}
```

Appended to as operator resolves annotations (`:resolution` enum: `:addressed`, `:addressed-differently`, `:retag-not-applicable`, etc., mirroring operator-foreword's editorial-log).

## The MAP step (per-essay finalisation)

**v0.2 default path (engine-emitted at assembly time):** the eoi-engine runs steps 1-5 below as the final assembly step, immediately after writing annotations.el. The Structured-finalisation landing rule in `~/code/futon0/scripts/eoi-new` instructs the engine. No separate post-hoc pass needed.

**v0.1 fallback path (retrospective):** for essays produced before 2026-05-27 OR for any case where the engine didn't emit annotations.edn (e.g. simulated runs, hand-authored essays), claude-in-the-loop produces the file post-hoc against the markdown + buckets entry. Same steps below; the only difference is the operator-vs-engine authoring locus.

For each EOI essay needing finalisation:

1. **Locate.** Find markdown path + `eoi_instances[]` entry. If buckets entry missing, halt and flag — this algorithm doesn't create buckets entries (that's `eoi-engine`'s domain).
2. **Create `<essay-dir>/` directory** if absent (engine-emit case: directory was created when annotations.el was written, so this is a no-op).
3. **Generate `:sections[]`.** Parse `##` and `###` headings from the markdown. For each:
   - `:id` = `arxana/essay/<slug>-v1/section/<index>-<short-slug>`
   - `:path-arrow` = heuristic by heading text (e.g. "Right View" → `:right-view`); `nil` if section is structural (e.g. "Diagnostic notes", "Engine notes").
   - `:aif-region` = `:internal` for self-facing prose, `:outside-facing` for stakeholder-facing prose, `:active-surface` for action-bearing prose. Default `:internal` when ambiguous.
4. **Extract `:annotations[]` per the six `:hx-type` values.** Coverage targets (v0.2 minimum-at-engine-emit-time):
   - `:eoi/path-arrow-witness` — **MUST** have one per arrow named in `eoi_instances[].arrow_components`. Passage = the `answer_anchor_quote` from buckets, located in the markdown. This is the minimum-viable annotations.edn at engine-emit-time.
   - `:eoi/institution-object-ref` — **MUST** have one for the buckets entry's `institution_object_ref`; additional if other institutions named in the essay body.
   - `:eoi/operator-doubt` — **MUST** have one per `tensions_tagged[]` entry whose content surfaces a Joe-side uncertainty (not every address-now tension; only the doubt-flavoured ones). Plus one per `tech_debt_carried_forward[]` item that's a doubt rather than a deferral.
   - `:eoi/interest-territory` — aim for 3–8 per essay; cluster passages, not exhaustive. The engine extracts these from arrow-answers + tension-answers (Joe-asserted passages).
   - `:eoi/cross-essay-link` — every explicit named-mission or named-essay reference becomes a link annotation. Avoid speculative "this might link to..." links.
   - `:eoi/synthetic-question` — 1–3 per essay, derivable from the essay's content and answerable from elsewhere in the corpus.
5. **Polarity tagging.** For `:eoi/path-arrow-witness`, `:eoi/institution-object-ref`, and `:eoi/cross-essay-link`: include `:polarity :fit` (default — interest territory is a fit), `:friction` (friction-bearing for this essay), or `:foreclosed-load-bearing` (load-bearing absence). Polarity-signed weights are used at REDUCE: `:fit` = +1, `:friction` = -1, `:foreclosed-load-bearing` = -2. Default `:fit` if omitted.
6. **Write `<essay-dir>/annotations.edn` + (optional) `<essay-dir>/editorial-log-v1.edn`.** The editorial-log is initially header-only; engine emit-time may skip it (operator can add later if needed).
7. **Validate** (lightweight):
   - `annotations.edn` parses as EDN.
   - Every `:hx-type` is one of the six.
   - Every `:endpoints` entry has `:role` ∈ `{:annotated, :target}` and `:section-id` referencing an extant section OR an `arxana/<class>/<slug>` ref.
   - Every `:eoi/path-arrow-witness` covers an arrow named in the buckets entry's `arrow_components`.
   - Every `:eoi/institution-object-ref` cites the buckets entry's `institution_object_ref` (additional refs OK).

**Filename collision rule:** if `<essay-dir>/annotations.edn` already exists (e.g. the Operator's Foreword has its own non-EOI annotations.edn), use `<essay-dir>/eoi-annotations.edn` instead. The launcher's Structured-finalisation landing rule names this convention.

## The REDUCE step (map-reduce across finalised essays)

Across all `<repo>/essays/<slug>/annotations.edn` files in the corpus:

- **GROUP BY `:eoi/interest-territory`** — produces a frequency map of interest-territory IDs with supporting essays + passages. Surfaces what Joe's core interests are, by recurrence.
- **GROUP BY `:path-arrow`** — coverage map per arrow; identifies under-exercised arrows (gaps in the prior).
- **GROUP BY `:institution-object-ref`** — which institutions appear in multiple essays; which are isolated.
- **AGGREGATE `:eoi/cross-essay-link`** — the sub-essay-granularity link graph; complements the essay-level cross-references already in buckets.
- **AGGREGATE `:eoi/operator-doubt`** — open questions across the corpus; clusters into themes (e.g. foreclosure-cost, rate-stratification, audience-engineering).
- **AGGREGATE `:eoi/synthetic-question` :status :open`** — answerable-but-unanswered questions, candidates for follow-up work.

Output: an **interest-surface document** at `~/code/atthangika-interest-surface-v1.md` (sibling to `atthangika-buckets.json`), markdown summary + optional EDN reprojection. Generation v0.1 is Claude-in-the-loop (reading the annotations.edn files and writing the surface); if patterns stabilise, can be coded as a babashka script.

## Bridge to M-INC event vocabulary

When `M-interest-network-coupling` step (c) lands (projection wired into XTDB), finalisation events can emit against the M-INC vocabulary:

- New annotation → `:state/spawned` event targeting the annotation's `:id` at `:annotation` granularity.
- Annotation re-tagged or refined → `:state/refined`.
- Operator resolves an annotation → `:state/addressed`.
- Cross-essay link asserted → `:link/asserted` with `:link/src` = annotated, `:link/dst` = target.

Finalisation thus becomes a **first concrete event-emitting source** (alongside `M-interim-director` checkpoints), unblocking M-INC verification cases VC-1 through VC-5 with real data.

## Verification

After producing annotations.edn for an essay:

1. **Schema validation** (per step 6 above).
2. **Buckets cross-check** — every `arrow_components[]` arrow has ≥1 `:eoi/path-arrow-witness`; every `tension_tagged[]` with `:address-now`-but-no-content has a corresponding `:eoi/operator-doubt`; every `institution_object_ref` has ≥1 `:eoi/institution-object-ref`.
3. **Operator review** — does the annotated essay surface the interest-territories the operator would name unprompted? If a major operator-named territory is missing, that's a v0.1 algorithm gap to capture.
4. **Cross-essay coverage** — does this essay's links land on real targets in other essays' sections? Stale targets surface as v0.1 algorithm gaps.

## Open questions (v0.2 candidates)

- Whether interest-territories are open-vocabulary (per-essay invention) or pinned to a fixed list (curated). v0.1 default: open; consolidate into a canonical list at v0.2 after N≥3 essays processed.
- Whether the markdown source moves into `<essay-dir>/<slug>.md` (Arxana convention) or stays in place with `:paper` pointing to it. v0.1 default: stays in place.
- Whether map-reduce output is generated by code or by Claude-in-the-loop. v0.1: Claude-in-the-loop.
- Whether `:eoi/synthetic-question` annotations are answerable BY THE PROJECTION or only by an operator pass. v0.1: operator pass; v0.2 candidate: projection-side.
- Whether the algorithm's annotations should be cross-corpus aware at MAP time (each essay annotated in isolation) or whole-corpus at MAP time (each essay annotated with knowledge of other essays' content). v0.1 default: per-essay isolation at MAP; cross-corpus surfaces at REDUCE.

## Bootstrap

The v0.1 retrospective bootstrap is complete as of 2026-05-27. Per-essay annotations.edn files were produced for the following EOI essays under claude-in-the-loop:

1. ✅ **anthropic-institute-analyst** (`futon5a/essays/anthropic-institute-analyst/annotations.edn`) — 17 annotations across 6 hx-types
2. ✅ **glasgow-cogito-neurotech-RA** round-1 (`futon5a/essays/glasgow-cogito-neurotech-RA/annotations.edn`) — round-1 outreach finalised
3. ✅ **glasgow-cogito-neurotech-RA-formal-application** round-2 (`futon5a/essays/glasgow-cogito-neurotech-RA-formal-application/annotations.edn`) — 28 annotations including the cross-session El Capitan metaphor echo
4. ✅ **anthropic-fellows-2026** (`futon5a/essays/anthropic-fellows-2026/annotations.edn`) — first-contact fellowship essay; records the A-B-C basket cross-session echo
5. ✅ **vsat-poc-scenarios-2026-q2-q3** (`futon5a/essays/vsat-poc-scenarios-2026-q2-q3/annotations.edn`) — trio container; per-scenario path-arrow witnesses for A/B/C
6. ✅ **hyperreal-director-side-a-v1** (`npt/applications/hyperreal-director-side-a/eoi-annotations.edn`) — private; `eoi-` prefix to avoid collision
7. ✅ **hyperreal-director-side-b-v1** (`npt/applications/hyperreal-director-side-b/eoi-annotations.edn`) — private; `eoi-` prefix
8. ✅ **operator-foreword** (`futon7a/essays/operator-foreword/eoi-annotations.edn`) — `eoi-` prefix (foreword has its own non-EOI annotations.edn)

The v0.2 promotion at 2026-05-27 reflects the patterns observed across these eight bootstrap files:

- The minimum-coverage rule (path-arrow witnesses + institution-object refs + operator-doubts) is what makes the annotations.edn useful for REDUCE; everything else is bonus.
- The `:polarity` tag (`:fit` / `:friction` / `:foreclosed-load-bearing`) on path-arrow + institution-object + cross-essay-link annotations is the load-bearing signal at REDUCE — it was added retrospectively across the bootstrap files.
- The `eoi-` filename prefix is the chosen collision-handling for directories with existing annotations.edn (Operator's Foreword, hyperreal-director-side-{a,b} dirs in npt/).

Future EOI essays will be engine-emitted at assembly time per the Structured-finalisation landing rule in `~/code/futon0/scripts/eoi-new` (effective 2026-05-27).

---

*Algorithm drafted 2026-05-26 by claude-3 in `emacs-claude-repl` session, sibling to `eoi-engine.md` (this directory). Schema lifted from `~/code/futon7a/essays/operator-foreword/annotations.edn` (Arxana v2). v0.1 retrospective bootstrap completed 2026-05-26 to 2026-05-27 across 8 EOI essays. v0.2 inline-at-assembly-time policy landed 2026-05-27 via update to `~/code/futon0/scripts/eoi-new` Structured-finalisation landing rule.*
