# Handoff: Arxana Essays "compiled view" — whole-essay edit-mode that round-trips into per-section sources

Date: 2026-05-20
Status: design sketch, awaiting Joe's prioritisation
Caller: Joe via claude-8 (eoi-engine first-run + Arxana Rewrite survey)
Owner: futon4 / Arxana Essays subsystem
Mission attribution: extends `~/code/futon4/holes/missions/M-essays-edit-cycle.md` (the multi-section editing axis the mission has not yet addressed)

## Purpose

The Arxana Essays browser currently exposes essays *per section*: clicking into a section opens a buffer whose source is one slice of the underlying `.md` file (the H2-delimited region per `arxana-browser-essays--extract-section-text`).  This makes section-bounded edits safe and well-typed.

What it does NOT support: editing across section boundaries in a single buffer.  An operator who wants to revise a foreword's overall throughline, normalise terminology across all sections, or address a cross-section diagnostic (e.g. `hx:of:v1:section-bridge-missing` in `/home/joe/npt/operator-foreword/annotations.edn`) has to either:

- open the raw `.md` file outside the Arxana Essays surface (losing annotation anchors and the typed edit-save round-trip), or
- make N section-by-section edits trying to keep them coherent (no atomic transaction; annotation anchors at risk of evaporating across saves).

Joe's words from the 2026-05-20 operator-foreword session:

> *"the contents are loaded but at this stage they are just a concatenation — I think if we were going to do rewriting, it would likely cross the 'subsection' boundaries considerably.  That could be a bit hard to do in the current essay 'view'.  Maybe we could extend Arxana Essays with a 'compiled' view that nevertheless retains the key 'edit and save and the contents go back to their sources' feature.  I recognise that's a potentially complex ask, given annotation anchors and targets need to be preserved as well."*

This handoff names what "compiled view" would mean concretely and sketches the implementation surface so it can be planned vs the existing per-section flow.

## What "compiled view" means

A single editable buffer that presents the entire essay's prose as one document (i.e. as the underlying `.md` would look opened directly), but:

1. **Save-back is section-aware.** On `C-c C-c`, the compiled buffer is parsed back into its H2-delimited sections; each section's contents are written to the `.md` source at its current location (replacing the old section text via the existing `arxana-browser-essays--replace-section-in-file` mechanism, applied per section).
2. **Annotation anchors survive edits whenever possible.** Overlays for each annotation are placed at the corresponding `:passage` text within the compiled view at open-time.  On save, anchors are recomputed against the new prose; if `:passage` still matches verbatim → anchor preserved; if `:passage` matches fuzzily → operator confirms; if anchor no longer locatable → annotation transitions to `:retracted t` (per the existing strikethrough-retained behaviour from `arxana-essays-strikethrough-handoff.md`).
3. **Section moves are first-class.** If the operator reorders sections (or moves prose across H2 boundaries), the save logic detects the new section partition.  Sections whose H2 heading-text is unchanged keep their `:id`; new H2s mint new section entities; vanished H2s retract their annotations or move them to whichever section now contains the matching passage (heuristic: longest-substring match wins).

## The annotation-anchor preservation problem (the hard part)

The current per-section view sidesteps anchor breakage because each edit is bounded to its own section: the section's annotations are scoped to that section by construction.  Compiled view breaks that boundary.  The risks:

- **Annotations that name multi-section passages** (e.g. `hx:of:v1:scope-mush-naming` cites three sections at once): the compiled-view edit might affect any or all of them.  Behaviour: each `:endpoint` re-resolves independently against the new prose.  If some endpoints anchor and others don't, the annotation transitions to a degraded state (`:partially-retracted` or similar — new schema field).
- **Section-bridge edits intentionally rewriting across H2s**: by design these change the section boundaries.  Annotations anchored at H2 lines themselves (none in the current operator-foreword set, but possible in general) need policy: do they retract, follow the H2 to its new position, or split?
- **Glue-adjustment chains within the rewrite protocol** (per `~/code/futon4/README-rewriting.md`): a closure block declares `:operations` and `:glue-adjustments` for one entity-decomposition.  A cross-section closure would need an entity-decomposition that spans multiple sections, which is currently allowed in the schema (operations target entities, not sections) but has no operational support in `arxana-browser-rewrites.el`.

## Suggested implementation phases

### Phase 1: read-only compiled view

Cheapest move that unblocks the survey.  Add `M-x arxana-browser-essays-open-compiled` that opens a non-editable buffer showing the full `.md` with annotation overlays placed at their anchored passages (same overlays as per-section view, but all visible at once).  No save logic; operator reads, navigates between annotations with `n`/`p`, and uses per-section edit-mode for any actual writes.

This alone would address Joe's immediate ask for the operator-foreword: the survey annotations become walkable as a whole, even if rewrites still go through the per-section surface (or through manual `.md` edits with subsequent annotation-anchor reconciliation).

Estimated scope: ~150 lines elisp; reuses existing `--extract-section-text`, the overlay machinery, the annotation reader.  No schema changes.

### Phase 2: editable compiled view with conservative save-back

Add edit-mode to the compiled buffer.  Save-back parses the buffer into H2 sections matched against existing section entities by `heading-text`; for matched sections, write contents to the source `.md` via the existing per-section rewrite path; for unmatched sections (added or removed), prompt the operator.  Annotation anchors are rechecked against new prose; mismatches transition to `:retracted t` with the strikethrough-retained behaviour already specified.

Estimated scope: ~400 lines elisp + tests; reuses `--replace-section-in-file` per section; new code for the buffer-parse + the multi-section save transaction.

### Phase 3: cross-section rewrite closures in the Rewrite protocol

Extend `arxana-browser-rewrites.el` (the 3-up surface) to accept `:closure` blocks whose entity-decomposition crosses section boundaries.  Render the cross-section before/after in the A and B panels with section-id labels per entity.  No new schema (the existing `:operations` already targets entities, not sections); the work is rendering + validation that the closure's section-coverage is well-defined.

Estimated scope: ~250 lines elisp + tests; depends on Phase 2 for the underlying multi-section edit primitive.

### Phase 4 (deferred): atomic transaction semantics

The above phases use the existing per-section save primitive iterated across multiple sections.  Failures partway leave the corpus in a half-saved state.  A proper transactional layer (write all sections to a temp file, validate, atomic rename) is worth doing but adds another ~200 lines and isn't needed for the immediate cross-section rewrite use case.

## Concrete entry points

- `~/code/futon4/dev/arxana-browser-essays.el`:
  - `--extract-section-text` (line 888) — current H2-bounded extraction
  - `--replace-section-in-file` (line 1656) — current per-section save primitive (call N times in Phase 2)
  - `--annotations-for-section` (line ~395) — currently scoped to one section; Phase 1 needs a no-scope variant returning all annotations for the essay
  - `arxana-browser-essays-edit-mode` (line ~1367) — current per-section edit-mode; Phase 2 needs a sibling compiled-mode
- `~/code/futon4/dev/arxana-browser-rewrites.el`:
  - `arxana-browser-rewrites-open` — Phase 3 entry point; section-id labels per entity in render
- `~/npt/operator-foreword/annotations.edn`:
  - `hx:of:v1:section-bridge-missing` — flagged as `:primary-cross-section-test-case? true` for any Phase-3 verification
  - `hx:of:v1:scope-mush-naming` — three-section endpoint, the canonical multi-anchor test
  - `hx:of:v1:meta-italic-lede` / tension-section-titles-meta / clarity-probe-section-as-engine-residue / coda-throat-clearing — tied annotations (`:tied-to` field) whose closures would naturally co-occur; compiled view would make this visible at open time

## Out of scope

- Modifying the per-section edit-mode (Phase 2 should add a sibling mode, not replace).
- Changing the .md as canonical-prose source-of-truth.
- Real-time multi-user editing (no concurrency assumption).
- WebArxana side; this is Emacs-surface only.

## Open questions

1. **Anchor-recomputation policy.** When `:passage` no longer matches verbatim after a save, should the system: (a) retract silently (current per-section default), (b) prompt operator per-annotation, (c) attempt fuzzy match and prompt only on ambiguity?  Phase 2 has to pick.  Recommendation: (c) for compiled view, with `:retracted-at` and `:retraction-kind` populated per the strikethrough handoff's open question 1.

2. **Section-reorder semantics.** If the operator moves §5 above §3 in compiled view, does (i) the .md's section order change to match, (ii) the order is rejected on save with a prompt, (iii) the order is preserved in the .md and the section indices update?  Phase 2 has to pick.  Recommendation: (i) for editorial flexibility; the manifest's `(index . N)` props are derived from the saved order rather than canonical.

3. **Interaction with the Rewrite protocol's :closure blocks.** A closure block in `annotations.edn` declares a `:before` and `:after` for a specific span.  If the compiled-view edit produces the same end-state as a hypothetical closure block, should the system: (a) accept the manual edit and mint a synthetic closure-block recording it (auto-promotion), (b) refuse manual edits where an open annotation overlaps the edited span (force the operator through the closure flow), (c) ignore the relationship entirely (manual edits and closures coexist without cross-tracking).  Phase 3 has to pick.  Recommendation: (c) for initial implementation; auto-promotion in (a) is a follow-on once enough manual-edit-then-recorded-closure cycles have been observed to know what the auto-promoted closure should look like.

## Mission attribution

Either:
- Extend `~/code/futon4/holes/missions/M-essays-edit-cycle.md` with a new section "Compiled view for cross-section rewrites" carrying Phase 1–3 above.
- Or create a sibling `M-essays-compiled-view.md` if the scope warrants a separate mission.

Joe's call.

## Expected deliverables

Phase 1 (minimum to unblock Joe's operator-foreword survey walkthrough):
- `arxana-browser-essays-open-compiled` interactive command.
- All annotations rendered as overlays in a non-editable single buffer.
- Navigation keys `n` / `p` between annotations.
- Test coverage in `~/code/futon4/test/arxana-browser-essays-test.el`.

Phases 2 / 3 / 4 as scoped above; estimate revisable once Phase 1 is in place and the cross-section closure shape stabilises.
