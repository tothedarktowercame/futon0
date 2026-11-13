# Codex handoff: pre-registered VERIFY pass for the simulated `eoi new` run on Hyperreal Director (Side A, inward self-authored institution)

Date: 2026-05-13
Status: pre-registered before the interview runs (so the rubric is not adjusted post-hoc to fit the artefact)
Caller: Joe via Claude-3
Owner: M-expressions-of-interest INSTANTIATE phase
Companion handoff: `~/code/algorithms/eoi-engine-verification-handoff.md` (the first verification pass, on Anthropic outward one-shot, 2026-05-11)

## Purpose

This is the **second experiment** in the protocol-verification series. Replicate the methodology Codex-6 ran on Anthropic-2026-05-11, applied to the structurally different Hyperreal Director Side A case: simulated `eoi new` run, flash `:eoi-inward-institution-self-authored` (first ever instance of this flash; profile being authored by the run itself). Read between line-anchored markers in the `*claude-repl:claude-3*` buffer, score against the rubric, produce a ~400-word report.

**Pre-registration discipline.** This handoff is being written *before* the simulated interview runs, so the rubric is not adjusted post-hoc to fit what gets produced. The rubric mirrors the first verification's structure with explicit additions for the structurally different inward-self-authored case. Any rubric divergence Codex notices between this handoff and the actual run is *information about the spec* (or about the run), not an indication that the rubric should retroactively change.

## Marker convention

The simulated run is delimited in the `*claude-repl:claude-3*` buffer by these markers, each appearing **only at the start of a line** (regex `^‡‡‡‡‡‡` anchors), distinguishing this run from the first verification's `^††††††` markers:

```
^‡‡‡‡‡‡ EOI-VERIFY:HYPERREAL-SIDE-A:BEGIN ‡‡‡‡‡‡
... (the simulated `eoi new` interview + assembly + schema write-back) ...
^‡‡‡‡‡‡ EOI-VERIFY:HYPERREAL-SIDE-A:END ‡‡‡‡‡‡
```

Read **only between the line-anchored markers**. JSONL transcript at `~/.claude/projects/-home-joe-code/<session-id>.jsonl` is the authoritative source if the buffer is paginated.

## Inputs to inspect

1. **The interview turns** between the markers — engine prompts + Joe's verbatim answers + state-accumulation observations.
2. **The assembled artefact** — Hyperreal Director Side A HEAD (an institution-object self-description / role-definition; Joe is constituting the institution by authoring this).
3. **Schema entries** in `~/code/atthangika-buckets.json`:
   - New `institution_objects[]` entry for Hyperreal Director (subtype `self-authored`, stance `constituted`).
   - New `eoi_instances[]` entry for the Side A run.
4. **Spec under test** — `~/code/algorithms/eoi-engine.md`, particularly: the inward-mission / inward-institution-self-authored branches; the clarity-probe replacing diagnostic-probe; the morphogenetic-prediction-branches-inversion for self-authored institutions; the new step 6.5 (`address-now` content-supply rule) and step 9 (referential integrity) added 2026-05-11.
5. **Prior cases for comparison (N=4 after this run):**
   - Glasgow Cogito Neurotech RA (outward multi-round, sent 2026-05-11). Schema: `eoi_instances[2]`. Strawman + draft at `~/code/futon5a/holes/missions/M-expressions-of-interest.strawmen/glasgow-cogito-neurotech-RA-198593.md` + clean essay at `~/npt/applications/glasgow-cogito-neurotech/glasgow-eoi-inquiry-v1.md`.
   - Anthropic Institute Analyst (outward one-shot draft, 2026-05-11, revised 2026-05-13). Schema: `eoi_instances[0]`. Draft at `~/npt/applications/anthropic-institute-analyst/anthropic-eoi-draft-v1.md`. First verification pass already done (`partial / needs-spec-revision-first` per Codex-6, revisions landed).
   - Mission-coherence-patterns HEAD (inward mission-creation, 2026-05-11). Schema: `eoi_instances[1]`. HEAD at `~/code/futon3/holes/missions/M-mission-coherence-patterns.md`.

## Rubric — invariants (Pass / Partial / Fail with cited line ranges)

Same 8 invariants as the first verification (with adjustments for the inward-self-authored case):

- **I1. No-AI-writing with voice-preserving copy-edit.** Assembly preserves Joe's answers verbatim; voice-preserving copy-edits (article correction, typo correction, punctuation normalisation) permitted; paraphrase / summarisation / substantive rewording / tone-changing edits forbidden. (Refined 2026-05-11 after Anthropic verification.)
- **I2. One-shot, closure-detected.** Interview ends when the closure rule is met. For inward HEAD (not round-collapse), closure requires Path-arrow essentials + clarity-probe answered.
- **I3. One question per engine turn.** No batching. Spec-violation surfaced on the Anthropic run — codified as load-bearing in the revised spec. Score here on whether the inward case re-introduces batching.
- **I4. Path-arrow coverage.** All five Path-arrows asked at round-1-essential level for inward case: View, Intention, Speech, Mindfulness, Livelihood. Inward case may foreground Right Intention more (morphogenesis at institution-layer).
- **I5. Round-awareness.** Inward case is single-round HEAD; `dialogue-debt-defer` is available; tensions tagged as `address-now` / `*-defer` / `not-applicable`.
- **I6. Composite arrow correctly recorded.** `arrow_components` cite passage-level evidence (per Anthropic-derived schema revision). For inward-self-authored, expect Right Intention at the institution-layer to be prominent.
- **I7. Inhabited mode.** Joe's answers in first-person, his voice. No consultant-voice drift. No inline placeholders in the artefact body (per Anthropic-derived spec revision — placeholders move to `:flash/assembly :side-channel`, not body).
- **I8. Negative-scope discipline.** Deferred tensions named, not silently omitted.

## Rubric — inward-self-authored specific checks (NEW)

Three additional invariants surfaced by this case's structural shape:

- **I9. Stance inversion.** The institution-object's `xenotype_stance` should be `constituted` (not `unknown`). The institution is being authored, not approached. The engine should treat the stance as set-by-this-very-act rather than awaiting external evidence.
- **I10. Sixth-slot is clarity-probe, not diagnostic-probe.** The spec says inward replaces the diagnostic-probe with a clarity-probe ("what about this isn't yet clear to you?"). Confirm the engine asks a clarity-probe, not a diagnostic-question-for-an-external-recipient.
- **I11. Morphogenetic-prediction-branches inversion.** For self-authored institutions, the prediction-branches structure inverts: Joe IS the stance, so the branches enumerate *what the stance commits the work to* (e.g. blog publication, free-solo continuity, etc.), not what it might be (the outward `tolerates / tacitly-endorses / explicitly-values / unaligned` enum). Confirm the schema entry the engine writes reflects this inversion.

## Rubric — phases (descriptive verification)

Same 9 phases as the first verification (load → seed → question pacing → branching → state accumulation → tension surfacing → closure detection → assembly → schema write-back). For inward-self-authored:

- **Load:** flash resolved as `:eoi-inward-institution-self-authored` (first run; no prior profile EDN exists). Joe is named as the institution-author.
- **Seed:** no pre-strawman (inward cases lack one). Engine may pre-compute candidate tensions from prior conversation context, or leave tension-surfacing to emerge from Joe's answers per the Arxana-Essays-driven approach the spec's open-question 5 anticipates.
- **Schema write-back:** institution-object **created** as part of the same write-back per the spec's referential-integrity revision (engine creates the institution-object if missing).

## Rubric — verdict

Same four tiers as the first verification: `spec-compliant` / `spec-compliant-with-minor-notes` / `partial` / `non-compliant`. Apply same calibration: "partial" doesn't mean "broken"; it means "schema needs refining at the inward-self-authored shape".

## Rubric — cross-case comparison (N=4)

Compare this run against the three prior cases on each dimension the spec names as varying:

- **Shape of variation.** Outward-multi-round (Glasgow) / outward-one-shot (Anthropic) / inward-mission-creation (Mission-coherence) / inward-institution-self-authored (THIS). Does the fourth case behave as the spec predicts? What's the same, what's different?
- **Hard prompts.** View and Intention required behavioural-proxy phrasing in three prior cases. Still hard here? Inward-self-authored may make Intention *easier* (morphogenesis-at-institution-layer is the artefact-of-the-run, not an abstract claim).
- **Closure rule.** Soft-floor 4 / hard-cap 12. Inward HEAD without round-collapse should land 5–8 turns based on Mission-coherence-patterns precedent.
- **Tension density.** Anthropic (one-shot): 0 deferred. Glasgow (multi-round): 5 deferred. Mission-coherence (inward HEAD): 5 deferred. Predicted for Side A: similar 3–6 deferred (decision-debt for self-authored ambiguities, e.g. what Hyperreal IS in operational terms).
- **Pushback substrate.** Outward = pre-strawman. Inward-mission = the mission corpus the mission proposes to mine. Inward-self-authored = ? *Open question that this run may help answer.* Could be the broader corpus of Joe's published work, or could genuinely have none (Joe is the only constituting party).

## Expected output format

Write a single response (~400 words) covering:

1. **Headline verdict** (one of the four tiers) with one-sentence reasoning.
2. **Per-invariant scoring** (8 standard + 3 inward-self-authored = 11 items, one line each).
3. **Phase observations** (9 items, terse).
4. **Cross-case comparison findings** — N=4 evidence for or against the spec's predictions, including the pushback-substrate question.
5. **Schema-revision notes** — concrete proposed mods to `eoi-engine.md` or `atthangika-buckets.json` (use file:line refs). Especially: what does the `:eoi-inward-institution-self-authored` flash profile EDN need to encode that this first run surfaced?
6. **Recommendation:** spec stable enough to encode the flash profile / needs further revision / live-build of `eoi new` would now have addressed-X-but-not-Y.

Do **not** write the verification report back into `eoi-engine.md` or any mission file — that's Claude-3's job after reviewing your report.

## Out of scope

- Do not draft or re-draft the Hyperreal Director self-description artefact.
- Do not modify the simulated transcript.
- Do not modify the spec directly; propose changes in the report.
- Do not run the engine yourself; just verify the simulated run.
- Do not extend to other prior cases except for comparison.

## Trigger

Joe will whistle this handoff to Codex (codex-6 by default, or whichever continuity-id is available) after the simulated Hyperreal-Side-A run is captured between the markers. Until then, this file is the spec for that future pre-registered verification pass.
