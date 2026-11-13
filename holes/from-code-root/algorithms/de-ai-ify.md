# Algorithm: De-AI-ify Prose

Date: 2026-05-07
Status: active

## Purpose

Use this when a real prose artifact already exists and the operator wants a
bounded rewrite pass that reduces recognizably LLM-shaped, managerial,
consulting, UX, startup, nonprofit, or other high-latent low-grounding prose
habits.

This algorithm is not a generic copyedit pass and not a ban-list enforcer. It
owns a narrower task:
- find places where the prose sounds explanatory without paying for
  explanation
- replace vague rhetorical smoothness with grounded claims, named agents,
  concrete operations, or cleaner omission
- preserve legitimate technical vocabulary and living style when they are
  actually doing work

The goal is not "sound less like a model" as a cosmetic exercise. The goal is
to raise information density and argumentative honesty by cutting surfaces that
are high-frequency, substitutable, rhythmically complete, and weakly grounded.

This algorithm should usually be run after the main argument roughly exists.
If the draft still does not know what it is trying to say, de-ai-ifying too
early will only flatten the symptoms while leaving the emptiness intact.

It does not own:
- argument repair when the real issue is missing evidence, missing mechanism,
  or missing decision
- ordinary proofing or grammar cleanup
- flexiarg induction from a recurring judgment surface, though it may expose
  that need
- voice addition. The algorithm subtracts recognizably model-shaped surfaces
  but cannot add the operator's voice. Voice is partly defined by what it
  does not sound like, and the algorithm operates only on prose it can read.
  After subtraction is exhausted, the next move is an operator-led voice
  pass, not another algorithm pass.

Discussion note:
- if the operator is still deciding whether the real need is argument repair,
  writing-coherence review, or a prose-only de-ai-ification pass, stay in
  discussion long enough to classify that honestly before mutating the draft

## Companion Flexiarg Resolution (2026-05-07)

Governing companion flexiargs for this algorithm:
- `library/writing-coherence/floating-formalism.flexiarg`
  - governs formal-looking language that is substituting for named instances
- `library/writing-coherence/missing-mechanism.flexiarg`
  - governs apparent explanation that never names the operation
- `library/writing-coherence/triad-inflation.flexiarg`
  - governs rule-of-three padding and third-item freeloading
- `library/writing-coherence/balanced-pair-padding.flexiarg`
  - governs synthetic contrast structures and fake middle-ground framing
- `library/writing-coherence/meta-lede.flexiarg`
  - governs meta-analytic sentence openers that delay the object-level point
- `library/writing-coherence/meta-title.flexiarg`
  - governs topic/function headings that do not state the section's claim
- `library/writing-coherence/first-mention-undischarged.flexiarg`
  - governs unresolved acronyms, cast members, and named entities
- `library/writing-coherence/plain-language-thesis.flexiarg`
  - governs thesis-level jargon and compressed abstraction
- `library/writing-coherence/meet-the-reader-where-they-are.flexiarg`
  - governs insider register that outruns the likely reader
- `library/writing-coherence/hedged-lift.flexiarg`
  - governs fake precision and vague risk-management hedging
- `library/writing-coherence/throat-clearing-close.flexiarg`
  - governs vague uplift endings and obviousness restatements

Inspected but non-owning candidates:
- `library/writing-coherence/unearned-essentially.flexiarg`
  - often co-occurs with prestige fillers, but does not own the broader
    de-ai-ification pass
- `library/writing-coherence/section-bridge-missing.flexiarg`
  - governs argument joints, not generic smoothness suspicion

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be applied rather than re-derived from
  scratch

## Inputs

- one existing prose artifact or a tightly bounded prose slice
- the target audience and genre
- any local annoyance lexicon, taboo phrasing, or register constraints the
  operator explicitly names
- optional:
  - a list of terms that must remain because they are true domain terms
  - an adjacent authoritative artifact whose register the draft should match

## Preconditions

1. The prose already exists.
2. The operator actually wants durable rewrite, not only diagnosis.
3. The main argumentative direction is at least stable enough that rewriting
   wording will not immediately be invalidated by wholesale conceptual change.
4. The target audience and genre are known strongly enough to tell legitimate
   technical compression from empty prestige wording.
5. The pass is bounded to an artifact, section, or paragraph set rather than
   "make all writing better" in the abstract.

## Suspicion Lexicon Rule

Treat words and phrases like these as suspicion triggers, not automatic bans:

- abstract systems nouns:
  - `connective tissue`, `ecosystem`, `landscape`, `framework`,
    `infrastructure`, `architecture`, `pipeline`, `surface area`, `modality`,
    `layer`, `paradigm`, `workflow`, `schema`, `stack`, `vector`, `space`,
    `domain`, `signal`, `artifact`
- soft causality verbs:
  - `supports`, `enables`, `facilitates`, `drives`, `shapes`, `grounds`,
    `anchors`, `bridges`, `orchestrates`, `unlocks`, `scaffolds`,
    `maps onto`, `aligns with`, `sits at the intersection of`, `emerges from`
- meta framing:
  - `one way to think about this`, `what's interesting here`, `the deeper
    issue`, `this points to`, `at a high level`, `stepping back`, `there's a
    tension between`, `the key distinction`, `in practice`, `conceptually`
- fake precision and hedging:
  - `arguably`, `plausibly`, `in many cases`, `often`, `tends to`, `can be
    understood as`, `roughly speaking`, `to some extent`, `not necessarily`,
    `less about X and more about Y`
- synthetic contrast:
  - `not X but Y`, `both X and Y`, `X rather than Y`, `on the one hand`, `on
    the other hand`, `while it's true that`, `this isn't merely X; it's also
    Y`
- prestige-academic fillers:
  - `nuanced`, `robust`, `meaningful`, `rich`, `complex`, `important`,
    `critical`, `subtle`, `multifaceted`, `holistic`
- over-smoothed transitions:
  - `that said`, `at the same time`, `with that in mind`, `in that sense`,
    `relatedly`, `more broadly`, `taken together`
- current-model nouns with high local overuse risk:
  - `epistemic`, `coordination`, `alignment`, `reasoning`, `capability`,
    `agent`, `governance`, `orchestration`, `abstraction`, `generalization`,
    `reflection`, `adaptation`, `synthesis`

Local self-suspicion terms currently named by the operator:
- `load-bearing`
- `delve`, `dive into`
- `tapestry`
- `navigate` used for abstract tasks
- `robust`, `nuanced`
- `crucial`, `critical`
- `leverage` as a verb
- `realm`, `landscape`, `ecosystem`
- `underscore`, `testament to`
- `in the realm of`, `at the intersection of`
- `multifaceted`, `meaningful`, `rich`
- `tackle`, `foster`, `showcase`
- `align`, `alignment`
- `holistic`

Sentence-level suspicion triggers:
- `it's not just X, it's Y`
- `X is more than just Y`
- `while X, it's also worth noting Y`
- `that said`
- `ultimately`
- `at its core`
- `X is a double-edged sword`
- tricolons whose third item is only a slight escalation
- flattering openers like `great question`
- vague uplift closers that merely restate the obvious

Punctuation-level suspicion triggers:
- em-dash performing thoughtfulness. If the structure (list, table,
  parenthesis, period plus follow-up sentence) already establishes the
  relation the dash is signalling, the dash is doing rhythm rather than
  work. Especially suspicious when stacked: a dashed clause inside a
  bulleted item that is itself inside a dashed list
- rule-of-three lists where two items would do
- typographic emphasis (*italic*, **bold**) used as rhythm rather than
  emphasis. When italic-bold sentences land at paragraph-end position more
  than twice across nearby surfaces, the typography is announcing "this is
  the takeaway" — which is the writer telling the reader where to read for.
  The point should land on its own merit; if it needs a font change to land,
  rewrite the prose, not the typography

## Procedure

1. Bind the actual rewrite target.
   - Name the file or section.
   - Name the intended audience and genre.
   - Bind any local annoyance lexicon the operator supplied.
   - Bind any protected vocabulary that must remain.
2. Read for density, not only for vocabulary hits.
   - Do not start by mechanically searching taboo words.
   - First ask:
     - what is the paragraph claiming?
     - what evidence or mechanism is it using?
     - which sentences are carrying the real load?
   - Then inspect suspicious surfaces.
3. Mark high-risk surfaces.
   - Flag places where many suspicious words cluster.
   - Flag places where several nouns could be swapped without changing the
     meaning materially.
   - Flag places where rhythm seems more complete than the thought.
   - Flag places where the sentence sounds explanatory but no agent,
     operation, or instance is named.
4. Classify each flagged surface before rewriting.
   - Choose one:
     - legitimate domain term
     - grounded metaphor that is earning its keep
     - empty or weakly grounded abstraction
     - soft causality without mechanism
     - rhetorical padding
     - register collision
     - semantic overcompletion
   - Do not rewrite a term just because it appears on the suspicion list.
5. Prefer cut over synonym swap.
   - If the suspicious phrase adds nothing, delete it rather than replacing it
     with a fresher equivalent that performs the same emptiness.
   - Banned-surface substitution alone is failure: `ecosystem` changed to
     `environment` is usually not improvement.
6. Replace abstract systems nouns with named reality.
   - For each noun like `framework`, `landscape`, `architecture`, or
     `connective tissue`, ask:
     - which named people, institutions, artifacts, or operations does this
       actually refer to?
   - Rewrite toward those names.
   - Example:
     - `connective tissue` -> the specific handoff, dependency, or evidence
       path the phrase was hiding
7. Replace soft causality verbs with actual operations.
   - For each verb like `supports`, `enables`, `facilitates`, `drives`,
     `shapes`, or `grounds`, ask:
     - who does what to whom, through what step?
   - Rewrite with the operation if it can be named.
   - If no operation can be named, weaken or cut the claim.
8. Strip meta-analytic framing unless it earns a real turn.
   - Cut openers like `at a high level`, `conceptually`, `stepping back`,
     `what's interesting here is`, unless they name a genuine change in level
     the paragraph could not otherwise signal.
   - Lead with the object-level claim instead.
9. Collapse fake precision and calibrated hedging.
   - Delete hedges that only perform social caution.
   - Keep only hedges that type a real uncertainty:
     - source incompleteness
     - estimate range
     - counterfactual dependence
     - explicit scope limit
10. Break synthetic contrast structures.
    - For `not X but Y`, `both X and Y`, `X rather than Y`, ask:
      - would a competent reader really have proposed X?
      - does Y stand up without the contrast?
    - If yes, lead with Y and only reintroduce X if a live misreading needs
      blocking.
11. Make metaphors earn their keep.
    - For `tapestry`, `bridge`, `scaffold`, `terrain`, `navigate`, `signal`,
      `friction`, `layers`, ask:
      - does the metaphor compress something real and hard to say otherwise?
      - or is it only creating an explanatory mood?
    - Keep only the first kind.
12. Cash out prestige fillers.
    - Replace `robust`, `nuanced`, `meaningful`, `rich`, `critical`,
      `holistic`, `multifaceted`, and similar terms with the property they are
      claiming.
    - Examples:
      - `robust` -> survives which perturbations
      - `meaningful` -> large enough to matter for which decision
      - `nuanced` -> what distinction is being preserved
13. Repair register instability.
    - If a sentence mixes theory jargon with customer-success reassurance,
      choose one register or split the sentence.
    - Keep technical claims technical.
    - Keep practical reassurance plain.
    - Do not let one sentence perform both if the mix makes neither honest.
14. Stop when the point lands.
    - After a sentence makes its claim, do not keep completing the rhetorical
      shape just because the cadence wants a third category or a summarizing
      flourish.
    - Cut semantic afterglow.
15. Check for cadence-repetition across surfaces.
    - The previous steps inspect single sentences and paragraphs. This one
      inspects the artifact.
    - When the same rhetorical structure appears in three or more adjacent
      surfaces, the pattern itself becomes the perceptual unit and the
      content becomes secondary.
    - High-frequency offenders:
      - parallel landing sentences ("X is not Y, it shows Z") repeated
        across bullets, panels, or sub-sections
      - identical synthetic-contrast structures ("not X but Y") in adjacent
        paragraphs
      - semicolon triads at the end of three or more nearby paragraphs
      - identical sentence-opening shape across consecutive bullets
    - Fix by varying the form, breaking the pattern, or removing the entries
      that are only there to complete the rhythm.
    - A single instance of any of these forms is fine. Three in a row is the
      thing the reader hears.
16. Check for typographic emphasis used as rhythm.
    - The previous step inspects sentence shape. This one inspects font
      shape.
    - When *italic* or **bold** sentences appear at paragraph-end position
      more than twice across nearby surfaces, the typography is announcing
      "this is the takeaway" — which is the writer telling the reader where
      to read for, instead of the prose making the point unmissable on its
      own merit.
    - Fix by removing the emphasis and rewriting the sentence so it lands in
      plain text. If it cannot land in plain text, the prose is the problem,
      not the typography.
    - Keep emphasis where it marks a true contrast or a single high-stakes
      claim that genuinely needs the reader to slow down.
17. Run the rewrite tests on each revised paragraph.
    - Named-instance test:
      - can the paragraph name the real thing it is about?
    - Mechanism test:
      - can the paragraph say how the claimed effect happens?
    - Substitutability test:
      - if two nouns or adjectives are swapped, does the meaning stay almost
        identical? if yes, the paragraph is still too latent
    - Asymmetry test:
      - does the paragraph still need all of its contrasts and list items?
    - Stoppage test:
      - does the paragraph stop once the point is made?
    - Spoken-language test:
      - would anyone actually say this in conversation? phrases like
        `calibrated commitment`, `binding lever`, `addressable population`,
        `reachable upper bound` can pass substitutability and meaning while
        existing only in writing of a particular register. if the phrase
        only lives on the page, name what it is trying to refer to in the
        words a person would use out loud
18. Re-read for overcorrection.
    - Do not flatten live style into dead style.
    - Do not remove real technical terms just because they are fashionable.
    - Do not punish a metaphor that is carrying true explanatory load.
    - Keep abruptness when abruptness is the truthful shape of the argument.
19. If the pass exposes a deeper recurring judgment surface, route that
    explicitly.
    - If the same rhetorical failure keeps recurring across artifacts and the
      current library does not own it cleanly, call:
      `algorithms/create-flexiarg-from-recurring-judgment-surface.md`

## Decision Rule

Default rewrite rule:
1. treat suspicious words and phrasings as probes, not bans
2. prefer deletion over thesaurus substitution
3. prefer named agents, named artifacts, and named operations over abstract
   systems nouns
4. prefer explicit mechanism over soft causality
5. prefer one strong claim over balanced framing or triadic cadence
6. keep legitimate technical or metaphorical compression only when it is
   grounded and doing work
7. stop the pass if the real problem is argument absence rather than
   AI-shaped wording

## Outputs

- an edited prose artifact with lower high-latent low-grounding density
- optionally:
  - a short list of recurring suspicion surfaces found in the artifact
  - a list of protected terms that were inspected and kept intentionally
  - one or more candidate recurring judgment surfaces to promote into new or
    updated flexiargs

## Stop Conditions

Stop and report rather than continuing to rewrite if:
- the draft still does not know what it is trying to claim
- most suspicious language is actually standing in for missing evidence or
  missing mechanism rather than poor style
- the operator's taboo list conflicts with legitimate domain terminology the
  artifact truly needs
- the pass would have to rewrite the argument itself rather than its prose
  realization
- the only available move is cosmetic synonym churn
- the subtractive pass is exhausted and the remaining gap is voice. The
  algorithm cannot add the operator's voice; it can only subtract surfaces
  that do not sound like it. After two passes have removed the model-shaped
  surfaces, further passes flatten the prose without adding what is missing.
  The next move is an operator-led voice pass — read the artifact aloud,
  mark sentences the operator would have written differently, and let the
  operator rewrite those without consulting the algorithm or any other
  model. Hand off rather than continuing.

## References

- `algorithms/create-flexiarg-from-recurring-judgment-surface.md`
- `library/writing-coherence/floating-formalism.flexiarg`
- `library/writing-coherence/missing-mechanism.flexiarg`
- `library/writing-coherence/triad-inflation.flexiarg`
- `library/writing-coherence/balanced-pair-padding.flexiarg`
- `library/writing-coherence/meta-lede.flexiarg`
- `library/writing-coherence/meta-title.flexiarg`
- `library/writing-coherence/first-mention-undischarged.flexiarg`
- `library/writing-coherence/plain-language-thesis.flexiarg`
- `library/writing-coherence/meet-the-reader-where-they-are.flexiarg`
- `library/writing-coherence/hedged-lift.flexiarg`
- `library/writing-coherence/throat-clearing-close.flexiarg`
