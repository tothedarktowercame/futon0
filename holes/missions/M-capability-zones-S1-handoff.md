# M-capability-zones S1 handoff — seeds, membership, retro harvest

**From:** claude-3 (owner seat this session) → **To:** codex-10
**Mission:** `futon0/holes/missions/M-capability-zones.md` — read HEAD + S1 before coding.
**Scope:** S1 ONLY. Do not touch S1.5 (rendering) or S2 (G wiring). Do not touch
`selection-discrimination` or anything in `full_loop_runner.clj`.

## Goal

Give the capability-zone partition its seeds and feed the per-action-class Beta
posteriors from retro-mined git history, so they leave Beta(1,1) on real
observation counts — WITHOUT writing to any live store (deposit is a separate,
reviewed step that the owner runs after review).

## Corpus (operator-pinned, 2026-07-19)

The FUTON stack repos, same landscape as M-capability-star-map:
`futon0 futon1 futon1a futon1b futon1bi futon2 futon3 futon3a futon3b futon3c
futon4 futon5 futon5a futon6 futon7 futon7a futonY` under `/home/joe/code/`.
Exclude worktree clones (e.g. `futon3c-index-check`). Default-branch history is
sufficient; state that in the coverage note.

## Deliverables

### D1 — action-class seed embeddings
- Vocabulary = `futon2/src/futon2/aif/forward_model.clj:25` `action-types` —
  ALL 14 classes (note: gap candidates are the 12 excluding `:no-op` and
  `:learn-action-class`; embed all 14 anyway, per mission S1).
- Write one seed text per class (a paragraph describing what *exercising* that
  class looks like in this stack — draw on the `forward_model.clj` docstrings,
  `predict-effects` arms, and `action_proposer.clj`). Check the seed texts in
  as data (suggested: `futon2/resources/capability_zones/action_class_seeds.edn`).
- Embed with the SAME recipe as the mission map:
  `futon6/.venv/bin/python futon3a/scripts/embed_text.py --json --model BAAI/bge-large-en-v1.5`
  (see `futon6/scripts/daily_reembed.sh` for the exact convention, incl.
  `embed_model`/`embed_dim` metadata). Output:
  `/home/joe/code/data/notions/bge_action_class_seeds.json`.

### D2 — `zone-of` membership fn
- New ns `futon2.aif.capability-zones`.
- `(zone-of embedding-vector)` → `{:class <kw> :cosine <sim> :margin <top1 - top2>
  :runner-up <kw>}`. Membership computed in the high-dim space (nearest seed by
  cosine). Margin reported always; NO thresholding hidden inside the fn.
- Distance is plain BGE cosine, tagged `:interim-metric` in the ns docstring and
  the return metadata (ground metric is `:held` on M-substrate-metric — consume,
  don't fork).

### D3 — the retro harvest (artifact only, NO live-store writes)
- Mine commits → action-classes across the corpus, three grains best-first
  (grain used per item recorded):
  1. attempt items + feature cards where they exist and are reachable;
  2. mission docs (`holes/missions/*.md`) + pattern references;
  3. raw commit paths + messages.
- Classification = `zone-of` over the BGE embedding of the grain text. Items
  whose margin is too low to call go to a "resisted description" bucket —
  counted, not dropped (no silent caps).
- Produce `wm-hyperparameter-update` records via
  `futon2.aif.intrinsic-values/next-update-record` (pure — do NOT call
  `persist-record!`). Chunk by calendar month per class so the posterior
  trajectory is replayable over time; `:evidence-refs` points into the harvest
  artifact (full sha lists live there).
- Emission/followthrough semantics: propose them yourself (e.g. emission =
  commit assigned to class; followthrough = not reverted), and DOCUMENT the
  choice in the artifact header. Review will judge defensibility.
- Artifact: `futon2/data/capability_zones/harvest-2026-07-19.edn`
  (records + per-item assignments + semantics header).

### D4 — seed drift
- Recompute each class seed as the centroid of its harvested evidence
  embeddings; log old→new cosine distance per class in the artifact. Keep both
  seed generations (text-seed and centroid-seed) in the seeds file, labelled.

### D5 — honest-coverage note
- `futon0/holes/missions/M-capability-zones-S1-coverage.md`: per grain and per
  repo, what fraction of history was described, what resisted and why, what was
  unreachable. No silent caps anywhere.

## Acceptance bar (all must hold; demonstrate in your bell-back)

1. In-memory replay of the artifact via `intrinsic-values/rehydrate!` yields
   non-uniform counts across ≥ 7 of the 14 classes, and `credit-for` returns
   non-default values for those classes.
2. `zone-of` tests pass: deterministic nearest+margin on fixture vectors.
3. Record-shape test: harvest records validate against `next-update-record`
   output shape (replay must not throw).
4. ZERO writes to live stores (:7070/:7071/:7073) — the store's
   `code/v05/wm-hyperparameter-update` count is unchanged. State the before/after
   count in your summary.
5. Coverage note exists and states fractions, not adjectives.

## Gates

- clj-kondo clean on all new/changed Clojure.
- `futon4/dev/check-parens.el` on all Lisp/Clojure touched.
- Tests for D2 and D3 shape; run the futon2 aif test surface you touched.
- No JVM restarts; no hand-tuned per-class constants (hard line — mission
  principle 2); nothing outside this spec (stop and ask instead).

## Delivery

- Commit to futon2 (and futon0 for the coverage note) with clear messages.
- If the full harvest run cannot finish inside the job window, commit the miner
  + a PARTIAL run + a coverage note saying exactly what's partial, and bell back
  anyway — do not go silent.
- **Bell claude-3 back with: summary, commit shas, the acceptance-bar evidence
  (replay counts, store count before/after), and any spec gaps you hit.**
