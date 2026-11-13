# Codex handoff: retain retracted annotations with strikethrough in the Essays notes pane

Date: 2026-05-13
Status: ready to execute
Caller: Joe via Claude-3
Owner: futon4 / Arxana Essays subsystem
Mission attribution: extends `~/code/futon4/holes/missions/M-essays-edit-cycle.md` § *Annotation lifecycle visibility — Strikethrough-retained annotations*.

## Purpose

When an annotation transitions to `:retracted t` in the manifest — either automatically (overlay evaporated during a save because the anchored substrate was edited out) or manually — the notes pane currently *filters it out of view*. The diagnostic-history disappears silently. The fix: keep the entry visible in the notes pane with a strikethrough face, plus a small marker indicating retraction kind/time when available.

Argumentation-theoretic context (per the mission doc § Annotation lifecycle visibility): editing-out-the-substrate that an annotation anchored to is the same shape as Lakatos's **Monster Barring**. The move is legitimate but must be *visible* — silent disappearance conflates monster-barring with refutation, oversight, or silent retreat. The mission's framing of this work cites the Lakatos *Proofs and Refutations* dialectic explicitly.

## The defect (concrete with line refs)

`arxana-browser-essays.el`:

- **`arxana-browser-essays--annotations-for-section`** (around line 394-405; signature `(manifest section-id &optional include-retracted)`). The function filters retracted annotations from the result by default (`(seq-remove #'arxana-browser-essays--annotation-retracted-p all)`) unless `include-retracted` is non-nil.
- The notes-pane render calls this without `include-retracted`. So retracted entries vanish from view.
- The save function at line 1781 marks annotations as retracted when their overlay collapses below threshold (the `dead-ids` / `retracted` count machinery). The transition itself is correct; only the *render*  filters it out.

## The fix

Two small changes inside `arxana-browser-essays.el`:

1. **At the notes-pane render call site(s)** for `--annotations-for-section`, pass `t` for `include-retracted`. (There may be more than one call site; check the render path for the notes buffer and for the per-section item rendering at lines 670+.)

2. **In the notes-pane row-render function** (search for where annotation rows are emitted into the notes buffer; likely in the same file around line 2780 or the `arxana-browser-essays-row` / `arxana-browser-essays-items` family), branch on `(arxana-browser-essays--annotation-retracted-p ann)`. For retracted entries:
   - Apply the `:strike-through t` face attribute to the annotation's text. (Combine with the existing face if any.)
   - Add a small marker prefix or suffix indicating retraction (e.g. `[retracted]` or a unicode dingbat). The marker should be visually distinct from the strikethrough so screen readers can announce it.
   - If the manifest entry carries timestamp / kind metadata for the retraction, surface it in the row's description or as a property on the annotation entry (visible on inspect / `arxana-browser-essays-show-annotation` or equivalent).

3. **Tests**: add coverage in `test/arxana-browser-essays-test.el` for the case where a retracted annotation remains in the rendered notes pane with `:strike-through t` face on its row text. Use a small synthetic manifest with one live + one retracted annotation in a single section.

## Out of scope

- Do not change the retraction *transition* logic. The save function correctly transitions overlays-that-evaporated into `:retracted t`; that path is healthy.
- Do not implement the diachronic argumentation-graph view yet — that's the follow-on described in the mission doc and is a separate (larger) handoff.
- Do not modify Joe's manifest files or the annotations themselves.
- Do not touch the count formatting at line 699 (`%d(%d+%d)` for live/retracted/pending) — the counter is correct as-is.
- Do not kill or restart the futon1a JVM / XTDB process. The change is Emacs-side only.

## Test plan

After the patch lands, verify against the live Anthropic EoI essay:

- Open the essay at `arxana://view/essays-home → Anthropic Institute Analyst → §1` (or any section with both live and retracted annotations).
- Confirm: live annotations render with their normal face; retracted annotations render with strikethrough, visibly distinct, and remain selectable / inspectable (clicking through reveals the `:note` text as before, with whatever retraction-kind/time marker is available).
- Confirm: the count format at the section-level (`%d(%d+%d)`) still reads correctly — strikethrough is purely render-side.
- Confirm: edit-mode save behaviour is unchanged. If an edit causes another annotation to retract, that annotation appears struck-through in the next render rather than disappearing.

## Expected deliverable

A diff to `arxana-browser-essays.el`, new test coverage in `arxana-browser-essays-test.el`, and a checkpoint added to `~/code/futon4/holes/missions/M-essays-edit-cycle.md` recording: what was patched, test counts, and a one-line note on the Anthropic EoI live-test result. PSR/PUR pair per `~/code/futon0/CLAUDE.md` discipline.

## Open questions

1. **Retraction kind metadata.** The current manifest's `:retracted t` is a boolean; there's no kind/time information. To make the strikethrough informative (was this auto-evaporated, or manually retracted, and when?), we'd need to extend the manifest schema with `:retraction-kind` and `:retracted-at` fields, and have the save-time auto-retraction populate them. Scope question: include this in this handoff, or defer to a separate handoff? Minimal-scope answer: this handoff just retains strikethrough; the metadata extension is a separate small handoff. Larger-scope answer: include because it's the same atomic improvement. Joe's judgement.

2. **Where to render the kind/time marker.** Inline next to the strikethrough text, or as a property surfaced only on inspect? Tradeoff: visual clutter vs accessibility.

3. **Strikethrough policy under re-anchor.** If a previously-retracted annotation gets re-anchored (because the operator restores the substrate text), should the strikethrough lift automatically? Probably yes; verify the transition path can flip `:retracted t` back to nil.
