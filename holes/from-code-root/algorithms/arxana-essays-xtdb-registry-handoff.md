# Codex handoff: replace `arxana-browser-essays-catalogs` defcustom with an XTDB-projected catalog

Date: 2026-05-11
Status: ready to execute
Caller: Joe via Claude-3
Owner: futon4 / Arxana Essays subsystem
Mission attribution: extend `~/code/futon4/holes/missions/M-essays-edit-cycle.md` (this is structurally the same class of problem — what the source of truth for the annotated edition's identity is — and the edit-cycle mission already names re-anchoring under continued authorship as its core), OR create a sibling `M-essays-stable-registry.md` if you prefer to keep the scopes separate.

## Purpose

Make the Arxana Essays catalog (the registry consulted at `arxana://view/essays-home` and by every section open) **persistent across Emacs and JVM restarts**, by reading it from XTDB entity state rather than from a defcustom. The current architecture stores the catalog as `arxana-browser-essays-catalogs` (a defcustom in `~/code/futon4/dev/arxana-browser-essays.el` at line 305), with the essay's `:source-file` carried only in the manifest plist and in the runtime catalog entry. The defcustom is volatile under Emacs reload / eval-buffer / customize-save behaviour, and the XTDB entity's props do not carry the source-file. Result: every Emacs reload or save-then-refresh requires hand-re-registering the catalog. Joe's words: *"What seems more reasonable is that the Essays needs a stable registry of content, probably stored in XTDB, so that it's persisted across restarts of JVM and Emacs, not via an ad hoc s-exp in my init file."*

This is the right architectural shape. The defcustom is the wrong storage layer.

## Companion artefacts

- `~/code/futon4/dev/arxana-browser-essays.el` — the file to modify
- `~/code/futon4/holes/missions/M-essays-edit-cycle.md` — sibling/parent mission
- `~/code/futon0/CLAUDE.md` — futonic development practices (PSR/PUR discipline applies)
- `~/code/futon4/CLAUDE.md` — *never kill/restart the running JVM*; use Drawbridge nREPL for any Clojure-side change
- `~/code/futon4/dev/arxana-store.el` — XTDB data-access layer; query primitives live here

## The defect (concrete, with line refs)

`arxana-browser-essays.el`:

- **Line 203-211 (`arxana-browser-essays--ensure-essay`):** upserts the essay entity to XTDB but **drops the manifest's `:source-file` on the floor**. Only `:id`, `:name`, `:type`, `:props` are written. The XTDB entity has no record of where its source-of-truth markdown lives.
- **Line 305-318 (`arxana-browser-essays-catalogs` defcustom):** the registry that `arxana://view/essays-home` consults. Static. Each entry carries `:id` (catalog-entry id), `:label`, `:description`, `:essay-id`, `:manifest-symbol`, `:source-file`. Source-of-truth for the catalog lives here, not in XTDB.
- **Line 596-606 (`arxana-browser-essays--catalog-source-file`):** looks up `:source-file` from the catalog first, then falls back to the manifest's `:essay :source-file` resolved against `~/npt/working-paper/`. The fallback is hardcoded to that one directory, which means any essay outside `~/npt/working-paper/` (such as the new Anthropic EoI under `~/npt/applications/`) cannot be resolved without an explicit catalog entry.
- **Line 375-385 (`arxana-browser-essays--manifest-for`):** looks up the manifest plist by `:manifest-symbol` from the catalog spec — meaning if the catalog spec is lost, the manifest cannot be found by `essay-id`, even if the .el manifest file is present in `arxana-browser-essays-manifest-files`.

The symptom Joe observed: after Emacs reload / save-then-refresh / customize-save events, `arxana-browser-essays-catalogs` reverts to its defcustom default (which contains only the UKRN WP entry). The runtime `add-to-list` for new essays is wiped, the source-file lookup falls through to the hardcoded `~/npt/working-paper/` path, and rendering injects `(Could not extract section text from <no source-file in catalog>. See heading: <no heading-text in section props>)` into the buffer. The save function (line 1738) writes the .md correctly, but its post-save re-render (line 1824) re-hits the same broken lookup.

## The fix (two stages)

### Stage 1: write `:source-file` (and friends) into the XTDB essay entity at import time

Modify `arxana-browser-essays--ensure-essay` (line 203) to **merge the manifest's `:source-file` (resolved to absolute path against the manifest file's directory)** into the essay entity's `:props` before upserting. Also merge `:description`, `:label`, and `:manifest-file` (the path of the .el manifest, not the symbol — see Open Question 1 below) into props.

After Stage 1, the XTDB entity carries enough information to **reconstruct a catalog entry from scratch**: id, name, source-file, label, description, manifest-file. No defcustom state required.

Stage 1 must be backwards-compatible: a manifest with no `:source-file` (or no `:description`) should still import; missing props default to nil, and the catalog projection handles nil by falling back to current defcustom behaviour for those essays.

### Stage 2: replace the `arxana-browser-essays-catalogs` defcustom with an XTDB projection

Replace the defcustom with a function — call it `arxana-browser-essays--xtdb-catalog` — that queries XTDB for all entities with `:type "arxana/essay"` and materialises a catalog list with the same shape every existing reader expects (`:id` / `:label` / `:description` / `:essay-id` / `:source-file` / plus a manifest-file pointer to replace `:manifest-symbol`). Every existing call site that reads `arxana-browser-essays-catalogs` should now call this function (or read from a cached value). The cache invalidates on every `arxana-browser-essays-refresh` and on every import-completion.

Keep the defcustom as a small **fallback augment** layer for two reasons: (a) some essays may exist only at the manifest-file level without having been imported to XTDB yet (transitional period); (b) operator override semantics (Joe wants to forcibly include or exclude an entry from the home view) need a knob. So the final catalog returned by callers is `(append (xtdb-projected-catalog) (defcustom-augmentations))` with deduplication by `:essay-id`.

Stage 2 also retires the `:manifest-symbol` indirection: callers that need the manifest defconst should resolve via `:manifest-file` path → `load` → look up the defconst by convention (or — better — eliminate the symbol-binding requirement entirely by reading the manifest plist directly from the file via `read` rather than via a defconst-bound symbol). See Open Question 1.

### Migration

After Stage 1 lands, existing essay entities in XTDB (UKRN WP, Anthropic EoI v1, and any others) will lack `:source-file` in their props. Two paths to migrate:

- **Re-import**: run `arxana-browser-essays-import` against all configured manifest files. The new `--ensure-essay` upserts with `:source-file` populated. Idempotent.
- **Backfill query**: write a one-shot script that iterates XTDB essay entities, finds each one's manifest file by `:essay :id` matching, computes the absolute `:source-file`, and patches the props. Useful only if re-import is somehow unsafe.

Recommended: re-import. Document the migration step in the mission's checkpoint.

## Test plan

Verify against three real essays (UKRN WP, Anthropic EoI v1, the Peeragogy Handbook demo if loadable). For each:

- **Cold-start round-trip.** Restart Emacs (without restarting the JVM — Drawbridge stays up), run `arxana-browser-essays-import` once for the configured manifest files, open `arxana://view/essays-home`. All three essays should appear. Click into each, navigate to a section. Section text should render (not the fallback message). Buffer-local `--source-file` and `--heading-text` should be populated.
- **Save-and-refresh.** Enter `arxana-browser-essays-edit-mode` in a section, type a single character, `C-c C-c`. The .md should reflect the edit; the post-save re-render should show the edited content, **not the fallback message**. Repeat 3 times in succession to confirm no degradation.
- **JVM-restart round-trip** (carefully — coordinate with Joe; never kill the JVM unannounced per `~/code/futon4/CLAUDE.md`). After JVM restart with a fresh process re-attaching to the same XTDB store, the catalog should re-project from the persisted entities. Emacs catalog cache invalidates and re-queries.

## Mission attribution and reporting

Either:

- Extend `~/code/futon4/holes/missions/M-essays-edit-cycle.md` with a new section "Source-of-truth registry (catalog as XTDB projection)" describing this work. The existing mission already names the three-layer coherence problem (essay prose, flexiarg sources, annotation manifest); adding the catalog as a fourth layer (registry-of-essays) is a natural extension. **Status line should also be updated:** the current claim *"editing pathways are not [handled]"* is out of date (line 22-ish of that mission doc); the `arxana-browser-essays-edit-mode` minor mode is operational at lines 1367-1395 of `arxana-browser-essays.el`. Fix this stale claim while you're in the mission doc.
- Or, if scope warrants, create a sibling mission `M-essays-stable-registry.md` and cross-reference.

PSR/PUR discipline applies (per `~/code/futon0/CLAUDE.md`):

- **PSR before implementation**: which pattern from `futon3/library/` does this select? Likely candidates: `storage/guardrails-vs-tooling`, `pattern-discipline/patterns-as-categorical-objects`, or a new pattern about source-of-truth-in-storage-not-config.
- **PUR after**: prediction errors, what surprised you, anything that didn't fit the spec.

## Out of scope

- Do **not** modify Joe's manifest files (`~/npt/working-paper/annotations.el`, `~/npt/applications/anthropic-institute-analyst/annotations.el`). They are operator artefacts.
- Do **not** modify the .md essay sources.
- Do **not** modify Joe's `~/code/futon0/contrib/futon-config.el` or his init.el. The whole point of this handoff is to remove the need for ad-hoc init-level registration.
- Do **not** touch the annotation hyperedges or the pattern-resolution path. Those are healthy.
- Do **not** kill or restart the futon1a JVM / XTDB process. Use Drawbridge nREPL for any Clojure-side change.
- Do **not** implement an `advice-add` workaround. The structural fix supersedes it.

## Open questions

1. **`:manifest-symbol` vs `:manifest-file`.** The current catalog spec uses `:manifest-symbol` (an Elisp symbol whose value is the manifest defconst). For XTDB durability, the right replacement is `:manifest-file` (a filesystem path). However, several call sites currently call `(symbol-value (plist-get cat :manifest-symbol))`. The cleanest end-state is to retire the symbol-binding requirement entirely: the manifest can be read from the file via `read` directly. But this is a larger refactor than just XTDB-projection. Two options to consider: (a) do the minimal change (store manifest-file path in XTDB; reader still loads the .el and resolves the symbol by convention); (b) do the larger refactor (read manifest plist directly from file content). Joe's judgement on scope.
2. **Cache invalidation policy.** XTDB-projection on every catalog read is presumably too chatty. A simple cache that invalidates on `arxana-browser-essays-refresh` and on import-completion may be sufficient. Verify against the actual call frequency.
3. **Pre-existing UKRN annotation breakage.** Separate from this handoff, the UKRN WP manifest references `ukrns/ARGUMENT.flexiarg` which no longer exists (the pattern was renamed or removed; only 20 other flexiargs remain in `~/code/futon3/library/ukrns/`). Not blocking this handoff, but if you encounter it during testing, flag it; do not silently fix it.

## Concrete entry points

- Defect line refs above. Start with `--ensure-essay` (203) and `arxana-browser-essays-catalogs` (305).
- The `arxana-store-ensure-entity` interface (in `arxana-store.el`) is the upstream XTDB-write surface; check the query surface there for the projection (Stage 2).
- Three real essays for validation: UKRN WP (`~/npt/working-paper/annotations.el`), Anthropic EoI v1 (`~/npt/applications/anthropic-institute-analyst/annotations.el`), Peeragogy Handbook (if loadable from `~/code/futon4/data/essays/wikibooks/peeragogy-handbook`).

## Expected deliverable

A working `arxana-browser-essays.el` (and any necessary `arxana-store.el` query helpers) such that:

- After `arxana-browser-essays-import`, the XTDB essay entities carry `:source-file` / `:description` / `:label` / `:manifest-file` in their props.
- `arxana://view/essays-home` populates from XTDB.
- `arxana-browser-essays--catalog-source-file` reads from XTDB props (with defcustom-augmentation fallback).
- Save → refresh shows the edited content, not the fallback message, in three consecutive iterations.
- The mission doc reflects the work (checkpoint at end; stale "editing not operational" claim corrected).
- A PSR records the pattern selected; a PUR records the prediction errors and surprises.

Report back with the diff, the checkpoint, the PSR/PUR pair, and a brief note on which Open Question you resolved which way.
