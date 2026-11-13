# Algorithm: eoi-engine — flash-profile interactive fiction engine for EoI / mission HEAD authoring

Date: 2026-05-11
Status: spec (simulate by hand-running the interview in conversation until built as code)
Owner: M-expressions-of-interest (DERIVE phase, Checkpoint 2026-05-11)

## Purpose

Operationalises the EoI protocol derived in `M-expressions-of-interest.md` §3 DERIVE. Joe runs `eoi new` (or `eoi new <flash-type>`); the engine loads a *flash profile*, runs a branching state-accumulating interview, and assembles the resulting artefact (an outward EoI, an inward mission's HEAD section, a self-authored institution-object spec, etc.) from Joe's own words.

Two invariants are load-bearing:

1. **No-AI-writing of the artefact body.** AI orders and links Joe's answers; AI does not paraphrase, summarise, smooth, or substitute. If the artefact contains a sentence Joe did not write, the engine has failed.
2. **One-shot, closure-detected.** Unlike `cr new`'s long-running REPL, `eoi new` ends when the flash profile's closure condition is met. The output is the artefact; the dialogue is not preserved as a separate working surface.

## Companion artefacts

- `M-expressions-of-interest.md` (futon5a) — the parent mission; §3 DERIVE Checkpoint 2026-05-11 records the derivation
- `atthangika-buckets.json` (~/code/) — schema substrate (`path_arrows[]`, `factor_objects[]`, `institution_objects[]`, `eoi_instances[]`)
- `futon3/library/pattern-discipline/patterns-as-categorical-objects.flexiarg` — the BHK categorical framing the question-set sits over
- `futon3/library/structure/whose-question-is-this.flexiarg` — the inhabited-mode constraint enforced at the artefact level
- `futon3/library/writing-coherence/name-what-you-drop.flexiarg` — names the deferred-tension slot
- `futon3/library/structure/unresolved-tensions-at-closure.flexiarg` — round-aware closure discipline

## Inputs

- `flash-type` (string, optional) — names a flash profile by id. If omitted, the engine *sniffs from the cwd*:
  - cwd inside `holes/missions/` of some futon repo → defaults to `mission-creation` flash
  - cwd inside `M-expressions-of-interest.strawmen/` → defaults to `eoi-outward` flash
  - otherwise → engine prompts Joe to name the flash type explicitly
- `context-handle` (optional) — pointer to job description, mission proposal text, institution-object draft, etc. — material the flash profile parses to seed predicted-tensions

## Flash profile schema

A flash profile is a single EDN file with the following keys. Profiles live under `futon3/library/peripherals/head-flash/` (proposed location; refine when first profile lands).

```edn
{:flash/id            "eoi-outward"           ; unique id
 :flash/applies-to    "outward first-contact EoI letters; one-shot collapses rounds"
 :flash/predicts      {:composite-arrow  [:right-view :right-intention :right-speech :right-mindfulness :right-livelihood]
                       :round-default    "first-contact"
                       :round-collapse?  false}

 :flash/question-set  [{:q/id   :view
                        :q/path-arrow :right-view
                        :q/round-tag  :essential-round-1
                        :q/prompt     "What does <one-line invariance claim drawn from context> actually *buy* you when applied to <context substrate>?"
                        :q/style      :behavioural-proxy
                        :q/branch-on  nil}
                       ;; ... one entry per Path-arrow in :composite-arrow, plus one per
                       ;; tension surfaced from the context-handle (see :flash/tension-surface)
                       ]

 :flash/tension-surface {:source    "context-handle: job-description / mission-proposal / institution-spec"
                         :extract   "rules for extracting candidate tensions from the context (per flash type)"
                         :subtype-enum  #{:dialogue-debt :decision-debt :rhythm-debt :speech-act-discipline}
                         :speech-act-discipline-is-always-round-1?  true}

 :flash/closure  {:rule  "all :essential-round-1 questions answered AND tension-surface subtype :speech-act-discipline answered AND Joe explicitly closes OR engine detects closure"
                  :soft-floor  4   ; minimum non-empty answers before engine considers closing
                  :hard-cap   12}  ; engine forces closure prompt after this many turns

 :flash/assembly  {:order      "by composite-arrow then by tension-subtype"
                   :rule       "ordering and connective tissue AI-written; substantive sentences are Joe's literal answers verbatim"
                   :paraphrase-forbidden?  true
                   :template-pointer  "library/peripherals/head-flash/assembly/eoi-outward.template"}

 :flash/sniff-paths  ["**/M-expressions-of-interest.strawmen/**" "..."]
}
```

Flash profiles known at spec-time (each to be authored when its first case runs):

- `eoi-outward` — outward first-contact letters (Glasgow shape, Anthropic-draft shape)
- `eoi-outward-one-shot` — applications where rounds collapse (Anthropic-send shape)
- `eoi-inward-mission` — new mission creation, HEAD authored by Joe (mission-coherence-patterns mission is the seed case)
- `eoi-inward-institution-self-authored` — institution-object authored by Joe (Hyperreal-Director Side A)
- `eoi-outward-into-self-authored-institution` — EoI whose codomain is a Joe-authored institution-object (Hyperreal Side B). Notes: composes Side A ; Side B; meta-layer visibly active.

## Behaviour (the interview loop)

1. **Load.** Resolve `flash-type` (from arg or cwd sniff). Load profile EDN.
2. **Seed.** If `context-handle` provided, parse it through the profile's `:flash/tension-surface :extract` rules to populate predicted tensions. Tag each predicted tension with its subtype.
3. **Question pacing.** Issue one question at a time. Joe answers in own register, short-form fine, bullets fine. The engine never answers its own questions and never offers candidate phrasings before Joe has spoken. **No batching at any step**, including tension surfacing (see step 6) — the one-question-per-engine-turn invariant `I3` is not relaxed under round-collapse. Confirmed as load-bearing 2026-05-11 after the Anthropic VERIFY run surfaced the wall-of-text-then-social-closure failure mode.
4. **Branching.** Each question's `:q/branch-on` may shape the next question. If Joe's answer to `:view` says "no developed invariance claim available", the engine may downgrade the rest of the Right-View thread to `:behavioural-proxy` mode for the remainder of the session.
5. **State accumulation.** The engine maintains a running model of what is now known about the artefact. Subsequent questions skip what's been covered. Joe can revisit ("scratch Q2, my real answer is..." re-opens the slot).
6. **Tension surfacing.** After the Path-arrow questions, the engine asks **one question per predicted tension, one turn each, never batched**. Tag options offered to Joe depend on the flash's `:round-collapse?` flag:
   - For `:round-collapse? false` (multi-round / inward / first-contact): `address-now` / `dialogue-debt-defer` / `decision-debt-defer` / `rhythm-debt-defer` / `not-applicable`. Deferred tensions land in `eoi_instances[i].tech_debt_carried_forward[]`.
   - For `:round-collapse? true` (one-shot): **only `address-now` or `not-applicable`** — `*-defer` options are forbidden because there is no later round to defer to. The engine must not offer them; offering them is a spec violation (the Anthropic 2026-05-11 run did this and the VERIFY pass flagged it).
   - For both modes: `address-now` requires Joe to supply verbatim content for that tension in the next turn. If no content is supplied, see step 6.5.
6.5 **`address-now` content-supply rule.** When Joe tags a tension `address-now`, the engine immediately asks a follow-up question collecting the verbatim content. **If Joe declines to supply content, the engine offers two options** (no third): (a) **retag** `not-applicable` (drop the tension from the artefact), or (b) **block closure** (the engine cannot proceed to assembly until content is supplied or the tag is changed). The engine MUST NOT insert AI-written placeholder blocks inside the artefact body — placeholders within the artefact violate `I1` (assembly verbatim) and `I7` (inhabited mode). If a placeholder is unavoidable for operator-flow reasons (e.g. Joe wants to draft offline and return), the placeholder must be marked and **moved to a separate `:flash/assembly :side-channel`** section that is NOT part of the artefact body.
7. **Closure detection.** When `:flash/closure :rule` is met, the engine offers closure: *"I have enough to assemble. Anything else you want to say first?"*. Joe can add free-form answers or close. For `:round-collapse? true` flashes, closure cannot be granted while any `address-now` tension has no verbatim content (see 6.5).
8. **Assembly.** The engine assembles the artefact under `:flash/assembly :rule`: Joe's answers verbatim, ordered per the template, AI-written connective tissue only. The output is presented for Joe's review, not auto-saved. Joe edits in place or accepts.
9. **Schema write-back.** On acceptance, the engine writes the new `eoi_instances[i]` entry to `atthangika-buckets.json` and the artefact file to the appropriate location (strawman directory, new mission stub, institution-object draft, etc.). **Referential integrity is enforced**: any `eoi_instances[i].institution_object_ref` must resolve to an existing `institution_objects[].id`. If the institution-object doesn't exist yet, the engine creates it as part of the same write-back. **Institution-object split**: `subtype` ∈ `{approached, self-authored}` is independent of `xenotype_stance`. For new approached objects, stance defaults to `unknown` (awaiting evidence). For new self-authored objects, stance is `constituted` (set by the act of authoring), with the prediction-branches enumerating what the constituted-stance commits the work to. Failing referential integrity is a spec violation surfaced by the Anthropic 2026-05-11 VERIFY run. The subtype/stance split was clarified 2026-05-13 after the Hyperreal Side A VERIFY run flagged the spec confusing `self-authored` (subtype) with the stance value.

## Closure rules (per round-awareness)

- For `:round-default "first-contact"` flashes: closure requires only round-1-essentials covered + speech-act-discipline answered. Deferred tensions named, not addressed.
- For `:round-collapse? true` flashes: closure requires *all* tension-subtypes answered. One-shot applications collapse rounds; technical debt against the artefact is no longer appropriate.
- For inward (`:eoi-inward-*`) flashes: the diagnostic-probe slot is replaced by a clarity-probe ("what about this isn't yet clear to you?"). The institution-object has `subtype: self-authored` and `xenotype_stance: constituted`.
- **Inward emergent-tension capture rule (added 2026-05-13 after Hyperreal Side A VERIFY).** When the engine offers closure and Joe's response or the closure-offer itself names concrete open tensions (e.g. *"the income shape is unclear"*, *"the 8-essay project is not yet started"*), those tensions MUST be tagged into `eoi_instances[i].tech_debt_carried_forward[]` or an explicit drop list before assembly proceeds. The Side A run named more open issues at closure than it recorded; the spec gap is real. Inward cases lack the pre-strawman that pre-computes outward tensions, so emergent capture at closure is the structurally equivalent move. The engine should treat any specific tension named during closure-offer phrasing as if Joe had tagged it `address-now` or `dialogue-debt-defer` (operator chooses) and prompt for the tag before assembly.
- **Clarity-probe-as-missing-arrow-detector (added 2026-05-13 after N=2 inward runs).** The inward clarity-probe has now surfaced a Path-arrow the 5-essential question-set didn't reach in BOTH inward HEAD runs (mission-coherence-patterns: internal/external EoI articulation; Hyperreal Side A: Right Effort). This is feature-not-bug behaviour: the clarity-probe does for inward cases what the diagnostic-probe does for outward cases, on a *which Path-arrow is missing* axis. When the clarity-probe answer points at a specific Path-arrow that the question-set didn't reach, the schema entry should record this in optional fields `clarity_probe_surfaces_missing_arrow` (the named arrow, e.g. `"Right Effort"`) and `arrows_witnessed_but_not_in_composite[]` (a list including the missing arrow with passage-level citation).

## Assembly invariants

- **Verbatim with voice-preserving copy-edit.** Joe's words appear in the output. **Voice-preserving copy-edits** are permitted: article correction (*"a entree"* → *"an entree"*), typo correction (*"Coodinator"* → *"Coordinator"*), punctuation normalisation. **Paraphrase, summarisation, substantive rewording, and tone-changing edits are forbidden.** The boundary is operator-judgement-friendly: if a competent copy-editor would make the change without consulting the author, it preserves voice; if the change requires checking with the author, it's a rewording and is forbidden. Refined 2026-05-11 after the Anthropic VERIFY pass: Codex initially flagged *"a entree"* → *"an entree"* as a verbatim violation; Joe corrected the spec to distinguish copy-edit from paraphrase. Subsequent rounds may run a variant of `~/code/algorithms/de-ai-ify.md` to improve first-draft text into submittable text while preserving voice.
- **Connective tissue marking.** Sentences between Joe's answers that are AI-written must be marked in source EDN so they can be audited later. They must remain minimal and structural — section labels, orienting italics, transition phrases — not interpretive prose.
- **Voice:** the final artefact must read as Joe's. Any sentence that crosses the copy-edit boundary into rewording must be flagged for review before acceptance.
- **No inline placeholders inside the artefact body.** Per step 6.5: if a tension is tagged `address-now` but has no verbatim content, the engine moves the placeholder to a `:flash/assembly :side-channel` section, not inside the body. Placeholders inline are an `I7` (inhabited mode) violation surfaced by the Anthropic 2026-05-11 VERIFY run.
- **Reassembly check:** the engine's last step before write-back is the **ARGUE-style coverage check** from `M-expressions-of-interest.md` §4 — composite-arrow inventory against the assembled draft, round-appropriate-coverage check. Misses surface here, not after sending.

## Bootstrap

The first runs are simulated by hand (Claude asks the questions in conversation, Joe answers, Claude assembles the result while preserving Joe's words). These simulated runs serve double duty:

- They produce the first real artefacts (Glasgow EoI is retrospectively logged; mission-coherence-patterns HEAD is the first prospective use).
- They generate the first flash profiles. Each simulated run captures the questions-that-worked into a candidate EDN file. After ~3-5 simulated runs across different flash types, the proto-flashes are stable enough to encode in the library.

The simulated-run discipline is the same as the final tool's: no Claude writing the artefact body. Claude operates as the engine, not as a co-author.

## Example simulated invocation

```
$ eoi new                            # cwd inferred as mission-coherence proto-flash
[engine] flash: eoi-inward-mission (mission-coherence-patterns seed run)
[engine] Q1 (View, essential-round-1):
         What invariance claim do you want this mission's patterns
         to bear? (One line, your words.)
[joe]    Mission docs are first-class language objects whose
         coherence is a structural property the same way code
         coherence is.
[engine] Q2 (Intention, essential-round-1):
         Which existing pattern-coherence move from
         futon3/library/ should this new mission's set rhyme with?
[joe]    pattern-discipline/patterns-as-categorical-objects --
         missions are objects with morphisms too.
...
[engine] (closure detected after 8 turns; 4 round-1 essentials
          answered, 2 tensions addressed, 3 deferred)
         I have enough to assemble the HEAD. Anything else?
[joe]    no, go ahead.
[engine] [assembles HEAD using Joe's verbatim answers, ordered
          per :flash/assembly :order, AI-written connective tissue
          only, presents for review]
[joe]    [edits in place or accepts]
[engine] [writes HEAD to new mission stub at
          futon3c/holes/missions/M-mission-coherence-patterns.md;
          writes eoi_instances entry to atthangika-buckets.json]
```

## Failure modes the engine prevents

- **Strawman-skipped one-off mode** (Glasgow N=1 failure): the engine cannot proceed past tension-surfacing without Joe naming each tension's disposition. The discipline is structural, not remembered.
- **AI-prose drift** (the constraint this whole spec exists for): the engine refuses to paraphrase Joe's answers during assembly. The `:paraphrase-forbidden?` flag is enforced at assembly-time.
- **False-negative verdict** (the round-aware correction): the closure rule encodes the round-default so a first-contact artefact is not scored against one-shot-application diagnostic surface.
- **Long-running REPL drift**: the engine is one-shot. It cannot become a working surface for unrelated work. Closure is mandatory.

## Out of scope

- Authoring the body of Joe's outward letters or applications (Claude's role ends at engine-style facilitation).
- Sending letters or applications (separate decision, outside the engine).
- Cross-mission analysis (the engine is per-artefact; cross-EoI argument lives in §4 ARGUE of `M-expressions-of-interest.md` and in `atthangika-buckets.json`).
- Mining the existing 176-mission corpus for patterns (separate practice; possible flash profile `eoi-inward-mission-mining` for later).

## Open questions (defer until first simulated runs surface them)

- The exact format of `:flash/assembly :template-pointer` — what does an assembly template look like?
- Whether `eoi-outward-into-self-authored-institution` (Hyperreal Side B) needs its own flash or composes from `eoi-outward` + `eoi-inward-institution-self-authored`.
- Whether the engine should refuse a `:essential-round-1` question's blank answer or accept the blank as evidence-of-a-real-gap (which the strawman discipline can name).
- Where the `:flash/sniff-paths` registry lives — a single file in the library? per-profile?
- **Pushback step (`:adversarial-pass`).** Should the engine include an optional step where it pushes back on one or two specific answers, asking Joe to defend or revise? For outward EoIs, the pre-written strawman supplies pushback (Glasgow's 6 diagnostic notes were the pushback substrate). For inward missions there is no pre-strawman; the pushback substrate is the corpus the mission proposes to mine, which only activates when mining starts. Surfaced 2026-05-11 after the M-mission-coherence-patterns HEAD run produced no in-interview pushback. May be unnecessary for inward missions where the work itself supplies pushback; collect evidence from running M-mission-coherence-patterns in parallel with M-expressions-of-interest.

- **Arxana-Essays-driven tension surfacing (INSTANTIATE-phase implementation candidate).** Surfaced 2026-05-11 by Joe after the Anthropic VERIFY pass. The current spec pre-computes predicted tensions from the `:flash/tension-surface` rules over the context-handle (strawman / job posting); the alternative is to *let tensions emerge from the actual first-draft prose* using the recently-created Arxana Essays system. Inverted flow: (1) Joe answers Path-arrow questions; (2) engine assembles first draft; (3) Arxana Essays processes the draft to surface emergent tensions diagnostically; (4) engine presents emergent tensions to Joe for tag + content; (5) engine reassembles. This is closer to the spec's Right Speech anchor (dafiing / prose-as-debug-trace — tensions diagnosed from the prose itself rather than from a separate substrate) and may obsolete most of the `:flash/tension-surface :extract` rules. Implementation-detail only; does not block VERIFY. Slots into INSTANTIATE when the live engine is built.
