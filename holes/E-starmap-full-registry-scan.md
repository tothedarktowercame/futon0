# Excursion: E-starmap-full-registry-scan

**VERDICT (2026-10-09, provisional):** OPEN — Exit condition (full-registry grounding with divergence characterized) is nowhere recorded as met; the caveat was deliberately kept open at mission close. _(WM status classification by zai-1, medium confidence; not yet confirmed by the author.)_

**Type:** E-prefix excursion (bounded scope-out, single-agent-owned end-to-end).
**Spawned:** 2026-06-10, on the **close of `M-capability-star-map`** (operator decision by Joe via
WM pilot cycle #1; pilot claude-3) — one of the four close-caveats spun out so the mission could
close without dropping them.
**Owner:** TBD.

## IDENTIFY (the caveat)

The star-map's capability inventory + EFE ranking were demonstrated over a **curated candidate set**,
not a **full registry scan**. The keystone (`efe-trustworthy-over-starmap`) is proven on the curated
set; generalising to the whole mission registry is unverified.

## Scope

- **IN:** run the star-map pipeline over the *full* mission registry (not the curated subset);
  confirm EFE still top-ranks an applicable single-cycle leaf with no cherry-pick at full scale;
  record where the curated-vs-full results diverge.
- **OUT:** EFE algorithm changes (that is [[E-starmap-efe-hardening]]).

## Exit condition

The star-map is grounded on the full registry, with the curated-vs-full divergence characterised and
the keystone re-confirmed (or its limits at full scale recorded).
