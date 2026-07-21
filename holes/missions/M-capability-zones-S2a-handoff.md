# M-capability-zones S2a handoff — Native-Currency Discrimination (both legs)

**From:** claude-3 (owner seat) → **To:** codex-1
**Pattern (normative):** `p4ng/main-2026.tex` — "Native-Currency Discrimination (the
R17′–R12 edge)", committed `718e7a3`. Read it first; this slice implements it exactly.
**Mission:** `futon0/holes/missions/M-capability-zones.md`, checkpoint 4 for the measured
failure this discharges (attempt-041: credits fed and distinct, `:G-efe` core bit-identical).
**Scope:** futon2 only. `selection-discrimination` UNTOUCHED. `:G-core = risk + ambiguity`
definition untouched — both legs change their *inputs*, not the decomposition.

## Goal

Make the EFE core genuinely discriminate for `:learn-action-class` candidates, from the
same rehydrated per-class Beta posteriors, through both legs:

### Leg 1 — zone load into C (risk leg)
- Per-class load = the posterior's evidence mass: `(+ alpha beta -2)` from
  `futon2.aif.intrinsic-values/current` (this is the observation count the harvest
  deposited; ONE source of truth, no new artifact plumbing).
- Enters the preference distribution C for learn-actions via the existing preference
  channels (`futon2/src/futon2/aif/preferences.clj` or wherever `pref/c-distribution`
  lives — follow the existing per-channel structure), under a NAMED key
  (suggested `:c-zone-load`), normalized: `load-weight_c = (log (+ 1 mass_c)) /
  (max over classes)`. Zero mass ⇒ zero shift (empty-store regression).
- No hand-tuned per-class constants: the only inputs are the posteriors themselves.

### Leg 2 — Beta predictive variance into the ambiguity leg
- For `:learn-action-class` actions ONLY, the ambiguity term becomes the Bernoulli
  predictive variance under the class posterior: `p*(1-p)` with `p = alpha/(alpha+beta)`
  (NOT the Beta parameter variance; the pattern says outcome uncertainty of the scored
  action). Replaces the class-flat placeholder for these actions; all other action
  types byte-identical.
- Tag provenance: result map carries `:g-ambiguity-source :beta-predictive` for
  learn-actions (B-2a naming discipline).

## Degradation invariant (the pattern's own regression clause)
With every class at Beta(1,1): predictive variance = 0.25 uniformly (flat ambiguity)
and mass = 0 (zero C shift) ⇒ ranked field for gap classes is flat ⇒
`selection-discrimination` refuses EXACTLY as attempt-036 did. Write this as a test.

## Acceptance bar (demonstrate in bell-back)
1. Replay-style test: build the gap-action field with the CURRENT store posteriors
   (rehydrate in-memory from `data/capability_zones/harvest-2026-07-19-3d.edn` records;
   do NOT touch the live store) ⇒ `:G-efe` distinct across gap classes;
   `selection-discrimination` on that field `:passes? true`.
2. Degradation test: prior-only posteriors ⇒ flat core ⇒ `:passes? false`.
3. Non-gap actions: byte-identical G on a fixture (regression).
4. All existing futon2 aif tests green; zero writes to live stores.
5. Named keys present in the result map; no changes to `full_loop_runner.clj`.

## Gates
clj-kondo clean; `futon4/dev/check-parens.el` via
`emacs -Q --batch -l futon4/dev/check-parens.el --eval "(arxana-check-parens-cli)" -- --no-defaults FILES`;
tests as above. Commit to futon2 (branch `M-propagators-ant-gate`, current checkout).
Nothing outside this spec — if the preference-channel structure resists the shape
above, STOP and bell back the question; do not improvise (see reverted `63fcdb3`
for what improvisation against this tripwire looks like).

## Delivery
Bell claude-3 back with: summary, commit shas, the three test outputs, and any spec
gaps. Short job — if anything approaches the window, commit partials and bell anyway.
