# Handoff: R12 narrow-take-up — VSATARCS-side port

**Authored:** 2026-05-21 by claude-9 (WM-side INSTANTIATE author; session emacs-claude-repl).
**For:** claude-4 (VSATARCS reader-side; R1-R11 SATISFIED).
**Operator:** Joe.
**Predecessor:** `~/code/futon0/holes/handoffs/r12-to-stack-Q6-2026-05-21.md` (claude-4 → fresh agent, this morning).
**Bilateral closure target:** the VSATARCS R12 row in `vsatarcs-alignment-completeness.aif.edn:1584` was flipped to `:satisfied-via-stack-level-Q6-narrow-take-up-apparatus` *by deferral to the WM implementation* — the actual symmetric VSATARCS apparatus is what this handoff asks claude-4 to build.

## 1. What landed (WM side, done this session)

The narrow R12-unblocking subset of M-the-futon-stack Q6 shipped on the WM side as the reference implementation Joe asked for. Concretely, in `~/code/futon2/`:

| Artifact | Purpose |
|---|---|
| `src/futon2/aif/intrinsic_values.clj` | Atom + per-class Beta(α,β) posterior + bootstrap-replay from XTDB + `(credit-for class)` public API |
| `scripts/wm_outer_loop.clj` | Outer-loop scheduler. Reads trace window, extracts emissions per class (§2.J reframe: LAC + decision union), derives Stream B follow-through from git, computes Beta updates, persists hyperedges, runs sliding-window-from-scratch |
| `deps.edn` `:wm-outer-loop` alias | Cron entry-point |
| `src/futon2/aif/action_proposer.clj:39` | Static `:intrinsic-value 0.1` replaced by `(iv/credit-for target-class)` |
| `src/futon2/aif/sorry_registry.clj` | Per-target `:intrinsic-value` derived from `:kind` field (`:meta 0.4 / :technical-debt 0.25 / :decision-debt 0.25 / :external-dependency 0.15 / :prototyping-forward 0.1`). Breaks the G=-4.208 tie your Q2 chrome surfaced. |

Plus cron install (both `:wm-scheduled` hourly and `:wm-outer-loop` daily-at-0400-UTC, registry at `~/code/futon0/data/cron-jobs.edn` mirrored as hyperedge `arxana/index/cron-jobs`).

258 tests, 744 assertions, 0 failures across `futon2`. Three hyperedges of type `code/v05/wm-hyperparameter-update` already in futon1a XTDB from initial smoke + cron.

**Design choices documented at** `~/code/futon0/holes/missions/M-the-futon-stack-Q6-r12-design-choices.md` — 17 sections including all the forks I traversed. Read this before the port; it'll save you the same forks.

**Key reframe** (per design-choices §15, Joe-approved 2026-05-21): emissions count both `:learn-action-class :target-class C` **and** `:decision :action :type C` per trace record (deduplicated). Without this, currently-addressable classes have permanently-vacuous R12 inference. Follow-through capped at emissions; uncapped count preserved in `:n-followthrough-observed`.

## 2. What the VSATARCS port needs

The audit row's `:vsatarcs-port-blocker` names it precisely:

> `arxana-vsatarcs-efe.el` does not currently consume `:intrinsic-value` anywhere; consumption site needs to be added before symmetric apparatus can land elisp-side.

So the port has two halves:

### 2.a. Consumption site

In `~/code/futon4/dev/arxana-vsatarcs-efe.el` (or its successor), pick the point where writer-action candidates get their G computed and subtract a `:intrinsic-value` field from G-pragmatic (mirroring `futon2/src/futon2/aif/efe.clj:173`). This is the same shape as the WM's R5a — additive credit term that breaks G-ties and surfaces operator priority.

The candidates that need credit are the writer-action-classes from the v0.5.22 safety-property family:
- `:mission-doc-sync`
- `:aif-edn-revision-entry`
- `:story-update`
- `:essay-revise`
- (and `:stack-annotations-upsert` if claude-2 has shipped it by your read)

### 2.b. Outer-loop apparatus

Elisp ns roughly mirroring `futon2.aif.intrinsic-values`. Could live at:
- `~/code/futon4/dev/arxana-vsatarcs-intrinsic-values.el`

Public API:
- `arxana-vsatarcs-intrinsic-values-credit-for` (class → posterior intrinsic-value, defaults to 0.5)
- `arxana-vsatarcs-intrinsic-values-rehydrate!` (read XTDB hyperedges, seed in-memory state)
- `arxana-vsatarcs-intrinsic-values-apply-update!` (fold one record into state)

The outer-loop scheduler is harder to mirror in elisp — the WM-side runs as a one-shot Clojure JVM under cron. Two options:
- (A) Ship a separate Clojure script for the VSATARCS-side outer loop (probably under `~/code/futon4/scripts/`); cron-installable, mirrors the WM pattern exactly. Cron registry has space for it.
- (B) Run the outer loop from within the Emacs session via a timer (`run-with-timer`). Lower latency but ties inference cadence to Emacs uptime.

I'd suggest (A) for symmetry with the WM side and so cron stays the scheduling primitive. (B) is OK if you want the convenience.

### 2.c. Input substrate

This is where the WM-side reference doesn't directly transfer. The WM uses Stream B (operator follow-through derived from git: sorry-closures + mission-doc-additions). VSATARCS writer-action-classes have a *different* natural input substrate:

**Stream C** (consent decisions): `arxana-vsatarcs-consent.el` dispatches `:confirm / :reject / :ignore / :abstain-for-now` per writer-action proposal. Each consent decision IS an operator follow-through event, much more direct than the git-derived Stream B. Counts:
- `:confirm` → α += 1
- `:reject` / `:ignore` → β += 1
- `:abstain-for-now` → neither (operator deferred)

The cleanest substrate: a new XTDB stream-type `code/v05/operator-consent-decision` (as the original handoff §4.1 named); add a bridge that pushes each consent-gate dispatch into the stream; outer loop reads from XTDB to derive per-class Beta updates.

The bridge is ~50 lines + a stream-type registration. The original handoff §4 named this as the right shape; we just didn't ship it WM-side because Stream B from git was the honest WM substrate. For VSATARCS, Stream C from consent is the honest substrate.

### 2.d. Bilateral substrate question (design-choices §11 question #2)

Open question: should the two outer loops (WM + VSATARCS) share an atom namespace, or stay independent?

I lean **independent** — the writer-action-class set is disjoint from the WM-action-class set, so they index different things. Bilateral discipline already encourages parallel-but-independent surfaces over shared mutable state. But there's a case for shared: easier cross-side cross-class comparison ("operator prefers writer-actions over reader-actions"). Your call.

## 3. Files to read first (priority order)

1. `~/code/futon0/holes/missions/M-the-futon-stack-Q6-r12-design-choices.md` — 17 sections, all the forks. The §15/§16/§17 sections at the end cover the §2.J reframe + per-class window + `:fire-pattern` stay-honest decisions that landed mid-session.
2. `~/code/futon0/holes/missions/M-the-futon-stack.md` §"2026-05-21 — Q6 R12 narrow-take-up apparatus landed" (one checkpoint section).
3. `~/code/futon2/src/futon2/aif/intrinsic_values.clj` — the WM reference. Mirror this shape elisp-side.
4. `~/code/futon2/scripts/wm_outer_loop.clj` — the WM outer-loop. Note especially `extract-emissions` (§2.J reframe), `records-within-window` (per-class window), and the no-substrate-unavailable record shape.
5. `~/code/futon4/dev/arxana-vsatarcs-consent.el` — the existing consent-gate; this is your Stream C source.
6. `~/code/futon7/holes/M-war-machine-aif-last-mile.md` §2.I (continued same session 2026-05-21) — the current ledger of what's done and what's open.

## 4. Cross-pointers (the bilateral closure surface)

- VSATARCS R12 audit row currently flipped (by deferral to WM impl): `~/code/futon4/docs/vsatarcs-alignment-completeness.aif.edn:1584`. When the symmetric apparatus lands, the row's `:vsatarcs-port-status` flips from `:deferred-to-later-session` to `:landed` with a new `:revisions` entry naming the port.
- WM R12 row: `~/code/futon2/docs/futon-aif-completeness.md` §R12. ✓ as of 2026-05-21 (apparatus). Won't change again when you ship.
- Bilateral-evidence entry: this VSATARCS-side port qualifies as a new `:bilateral-evidence` entry of kind `:symmetric-apparatus-port` (or whatever you decide); record there too.
- `bb scripts/generate_vsatarcs_md.bb` from `~/code/futon4` after `.aif.edn` edits.

## 5. Out of scope (Joe directive 2026-05-21)

- Pragmatic-weights / channel-health-signs / preference ranges (the deeper R12 sub-directions on the WM side). Not your problem; each is its own future session.
- Temporal decay on β (design-choices §15 future note). Joe deferred; revisit if posterior-overshoot becomes a real problem.
- `:fire-pattern` substrate refinement (§11.d closed as stay-honest). VSATARCS-side writer-action-classes don't have an analogous substrate-unavailable case if Stream C covers them all.

## 6. Coordination notes

- The §2.J reframe (emissions = LAC ∪ decisions) is **important** for VSATARCS too if the writer-side has classes that are both "gap-eligible" AND "concretely-proposable" in different states. Mirror the deduplication logic. The consent-gate substrate (Stream C) is cleaner than the WM's Stream B because each consent decision is unambiguously about a specific proposed action — the cap shouldn't bite as often.
- `defprotocol` reload-safety: the WM side has `action_proposer.clj` defining `ActionProposer`, which is unsafe to Drawbridge-reload. If you change a protocol-defining elisp module (less of an issue in elisp), be careful.
- Penholder constraint: `FUTON1A_ALLOWED_PENHOLDERS=api` on this machine. Use `"api"` as penholder; record real authorship in `:props :provenance/author`. See design-choices §12.
- Cron registry: when you ship a VSATARCS outer-loop cron entry, add it to `~/code/futon0/data/cron-jobs.edn` and re-run `bb /home/joe/code/futon0/scripts/upsert-cron-jobs-index.bb` to refresh the Arxana mirror. Convention now established.

## 7. Suggested first move

DERIVE the consent-decision stream-type + the elisp atom shape on paper before coding. The WM side took 4 substantive forks during DERIVE (vocab-file path, no-external-EDN policy, β-over-α/γ choice, per-class follow-through substrate); the VSATARCS port should have *fewer* forks because the substrate is already named (Stream C), the inference shape is identical, and the architectural decisions (atom + bootstrap-replay, XTDB persistence, sliding-window-from-scratch) carry over.

Then INSTANTIATE in the order: stream-type registration → consent-gate-to-stream bridge → intrinsic-values elisp ns → consumption site in `arxana-vsatarcs-efe.el` → outer-loop script → cron install + registry update. Each step verifiable independently.

## 8. Why this handoff exists

You authored the original handoff that named the deferral target as M-the-futon-stack Q6 and the take-up shape as "per-action-class intrinsic-value learning." This session built the WM-side reference per that handoff. The VSATARCS-side port is the symmetric closure your handoff anticipated — both contracts deferred to the same place; both now need symmetric apparatus to make the bilateral closure operational on both sides. You own VSATARCS; the bell brings you back to it with the WM reference now in place to mirror.
