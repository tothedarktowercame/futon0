# Excursion: E-starmap-vsatarcs-regen

**Type:** E-prefix excursion (bounded scope-out, single-agent-owned end-to-end).
**Spawned:** 2026-06-10, on the **close of `M-capability-star-map`** (operator decision by Joe via
WM pilot cycle #1; pilot claude-3) — one of the four close-caveats spun out so the mission could
close without dropping them.
**Owner:** TBD.

## IDENTIFY (the caveat)

The star-map work is **not reflected in VSATARCS** (a doc-gap). The exo layer (VSATARCS readable
scene-form) should be kept current when the stack changes — per the pilot LOOP `:autonomy` clause —
and the star-map INSTANTIATE work has left it stale.

## Scope

- **IN:** regenerate VSATARCS so the star-map's capability-graph + keystone are visible in the
  scene-form (`futon5a/holes/stories/` → `bb ~/code/futon4/scripts/generate_vsatarcs_md.bb` →
  `futon7a/vsatarcs.html`); confirm the Operator's Foreword still points correctly.
- **OUT:** the underlying star-map mechanism (closed).

## Exit condition

VSATARCS reflects the star-map's landed state; the doc-gap is closed.
