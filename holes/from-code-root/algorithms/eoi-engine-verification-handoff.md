# Codex handoff: VERIFY pass for eoi-engine via simulated Anthropic run

Date: 2026-05-11
Status: ready to execute (once the simulated Anthropic run is captured between markers)
Caller: Joe via Claude-3
Owner: M-expressions-of-interest VERIFY phase
Companion: `~/code/algorithms/eoi-engine.md` (the spec being verified)

## Purpose

Verify the `eoi-engine` design (per `~/code/algorithms/eoi-engine.md`) by reading a simulated run end-to-end and applying a qualitative rubric. The run is the third case after Glasgow (outward multi-round, 2026-05-11) and Mission-coherence-patterns (inward single-round HEAD, 2026-05-11). Anthropic adds the **outward one-shot round-collapse** shape — the third corner of the variation space the spec named.

This is the VERIFY pass for M-expressions-of-interest, not a fourth pilot. The engine is still simulated (not yet built as code); verification scores the simulation against the spec invariants and surfaces any schema mods needed before live build.

## Marker convention

The simulated Anthropic run is delimited in the `*claude-repl:claude-3*` buffer (Joe's Emacs surface) by these exact ASCII markers, each appearing **only at the start of a line** (regex `^††††††` anchors), so inline mentions of the marker text in prose (such as this paragraph) do not match:

```
^†††††† EOI-VERIFY:ANTHROPIC:BEGIN ††††††
... (the simulated `eoi new` interview + assembly + schema write-back) ...
^†††††† EOI-VERIFY:ANTHROPIC:END ††††††
```

Read **only between the line-anchored markers**. Use the line numbers visible in the buffer for citations. The session transcript JSONL at `~/.claude/projects/-home-joe-code/<session-id>.jsonl` is the authoritative source if the buffer is paginated; cite either, mark which.

## Inputs to inspect

The simulated run produces these artefacts; verify against the spec:

1. **The interview turns** — engine prompts + Joe's verbatim answers + state-accumulation observations. Inside the marker range.
2. **The assembled artefact** — the Anthropic outreach/application draft. Posted in the buffer at session-end.
3. **Schema entries** — `eoi_instances[]` and `institution_objects[]` additions to `~/code/atthangika-buckets.json` (cite path; check for the new `anthropic-*` id).
4. **Spec under test** — `~/code/algorithms/eoi-engine.md` (current state; the version of the spec the simulation was scored against).
5. **Prior cases for comparison:**
   - `~/code/futon5a/holes/missions/M-expressions-of-interest.strawmen/glasgow-cogito-neurotech-RA-198593.md` (outward multi-round)
   - `~/code/futon3/holes/missions/M-mission-coherence-patterns.md` (inward single-round HEAD)
   - `~/code/atthangika-buckets.json` — `eoi_instances[]` for both prior cases

## Rubric — invariants (Pass / Partial / Fail with cited line ranges)

Score each:

- **I1. No-AI-writing of artefact body.** Assembly preserves Joe's answers verbatim. No paraphrase, summarisation, or smoothing. Connective tissue is minimal and marked. *Check the assembled artefact line-by-line against the interview answers.*
- **I2. One-shot, closure-detected.** Interview ends when the closure rule is met, not when the conversation gets bored or runs out of steam. For Anthropic (one-shot round-collapse), closure requires **all** tension-subtypes answered, not just round-1-essentials. Cite the closure moment.
- **I3. One question per engine turn.** The engine does not preview multiple questions, does not write candidate answers for Joe, does not speculate before Joe has spoken. Cite any violations.
- **I4. Path-arrow coverage.** All five Path-arrows (View, Intention, Speech, Mindfulness, Livelihood) asked at round-1-essential tagging. Speech-act-discipline is always round-1.
- **I5. Round-awareness.** Tensions tagged with round-appropriateness. For Anthropic specifically: confirm that the round-collapse case treats every tension as round-1-essential (no `dialogue-debt-defer` allowed).
- **I6. Composite arrow correctly recorded.** The arrow_components in the schema entry should cite passage-level evidence from Joe's verbatim answers (per the schema-revision surfaced by the Glasgow case).
- **I7. Inhabited-mode artefact.** Per `library/structure/whose-question-is-this.flexiarg`: Joe's answers in first-person, his voice, no consultant-voice drift. Cite any voice slippage.
- **I8. Negative-scope discipline.** Per `library/writing-coherence/name-what-you-drop`: the deferred tensions / drop-list is co-located with the carried-forward list. No silent inheritance.

## Rubric — phases (descriptive verification)

For each, note observed / not observed / unclear:

- **Load** — was the flash type resolved correctly (`:eoi-outward-one-shot` for Anthropic)? If the engine defaulted from cwd-sniff, did the sniff make sense?
- **Seed** — were tensions seeded from the Anthropic job description / context-handle? Cite which tensions seeded.
- **Question pacing** — one-at-a-time honoured?
- **Branching** — did Joe's answer to one question reshape the next?
- **State accumulation** — did the engine skip what was covered, or re-ask?
- **Tension surfacing** — were predicted tensions raised explicitly for Joe's tagging? For Anthropic these include residency/visa, alignment-as-frame vs alignment-as-engineering-surface, employment-vs-contract speech-act ambiguity, scale-of-impact framing. Verify all four (plus any others) were surfaced.
- **Closure detection** — appropriate moment? Was the closure rule applied per `:flash/closure :rule`?
- **Assembly** — verbatim invariant honoured? Connective tissue minimal? Joe's voice preserved?
- **Schema write-back** — did the new `eoi_instances` entry and `institution_objects` entry get written? Are the fields populated correctly?

## Rubric — verdict

Aggregate one of:

- `spec-compliant` — all invariants Pass; phases observed.
- `spec-compliant-with-minor-notes` — all invariants Pass; phase notes accumulate refinements but not blockers.
- `partial` — one or more invariants Partial; specific schema/protocol mods needed before live build.
- `non-compliant` — invariant Fail observed; spec needs revision before another run.

Cite line ranges. The Glasgow forward-pass returned `partial → round-1-complete` after round-awareness was added; the precedent is that "partial" doesn't mean "broken" but means "schema needs refining". Apply the same calibration here.

## Rubric — cross-case comparison (N=3)

After per-case scoring, compare Anthropic against Glasgow and Mission-coherence on each dimension the spec names as varying:

- **Shape of variation:** outward-multi-round / inward-single-round / outward-one-shot — does the third case behave as the spec predicts? What's the same, what's different?
- **Hard prompts:** are View and Intention still the structural arrows that need behavioural-proxy phrasing in the round-collapse case? Or does one-shot change the phrasing requirement?
- **Closure rule:** does soft-floor 4 / hard-cap 12 hold for one-shot? Or does round-collapse push the floor up?
- **Tension density:** Glasgow and Mission-coherence both produced ~5 tech-debt items. Anthropic should produce **0** (one-shot doesn't allow deferral). Confirm.
- **Pushback substrate:** Anthropic has no pre-strawman (the job posting is the strawman analog but is *external*, not Joe-written). How does that affect the run? Evidence either way feeds the `:adversarial-pass` open question on `eoi-engine.md`.

## Expected output format

Write a single response to me (~400 words) containing:

1. **Headline verdict** (one of the four above) with one-sentence reasoning.
2. **Per-invariant scoring** (8 items, one line each).
3. **Phase observations** (9 items, terse).
4. **Cross-case comparison findings** — N=3 evidence for or against the spec's predictions.
5. **Schema-revision notes** — concrete proposed mods to `eoi-engine.md` or `atthangika-buckets.json` (use file:line refs).
6. **Recommendation:** ready to build live (`eoi-engine.md` stable enough to code), needs-one-more-pilot, or needs-spec-revision-first.

Do **not** write the verification report back into `eoi-engine.md` or the mission file — that's a separate write step Claude-3 will do after reviewing your report. Just produce the report as your response.

## Out of scope

- Do not draft or re-draft the Anthropic application itself.
- Do not modify the simulated transcript.
- Do not modify the spec (`eoi-engine.md`) directly; propose changes in the report.
- Do not run the engine yourself; just verify the simulated run.

## Trigger

Joe will whistle this handoff to Codex-2 after the simulated Anthropic run is captured between the markers in the buffer. Until then, this file is the spec for that future verification pass.
