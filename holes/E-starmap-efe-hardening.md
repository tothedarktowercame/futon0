# Excursion: E-starmap-efe-hardening

**Type:** E-prefix excursion (bounded scope-out, single-agent-owned end-to-end).
**Spawned:** 2026-06-10, on the **close of `M-capability-star-map`** (operator decision by Joe via
WM pilot cycle #1; pilot claude-3) — one of the four close-caveats spun out so the mission could
close without dropping them.
**Owner:** TBD.

## IDENTIFY (the caveat)

The star-map's EFE scorer needs **body-term + decompose hardening**. The mechanism (Unit B
EFE-over-graph + INV-G) is verified, but the EFE body-term and the decomposition step are not yet
robust across the full range of inputs.

## Scope

- **IN:** harden the EFE body-term and the decompose step (edge cases, degenerate graphs, scale);
  add adversarial tests à la the logic-model-before-code discipline; keep INV-G intact.
- **OUT:** the full-registry data question ([[E-starmap-full-registry-scan]]).

## Exit condition

The EFE body-term + decompose pass adversarial tests and hold INV-G across the hardened input range.
