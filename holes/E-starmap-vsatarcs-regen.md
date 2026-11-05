# Excursion: E-starmap-vsatarcs-regen

**VERDICT (2026-10-09, provisional):** DONE — Exit condition states the doc-gap is closed and git records an operator close of the mission. _(WM status classification by zai-2, high confidence; not yet confirmed by the author.)_

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

## Completion criteria

- VSATARCS output contains a scene reflecting the star-map's capability-graph and keystone landed state — check: `grep -ci 'star-map\|capability graph' /srv/hel-staging/vsatarcs.html` returns >= 1 (today 0) **Not met.**
- The generating story file(s) under `futon5a/holes/stories/` name the star-map content VSATARCS renders — check: `grep -ril 'star-map' /home/joe/code/futon5a/holes/stories/` non-empty (today empty) **Not met.**
- The Operator's Foreword in the regenerated VSATARCS still points correctly — check: manual read of the Foreword section of `/srv/hel-staging/vsatarcs.html` after regeneration **Not started.**
