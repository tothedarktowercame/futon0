# Packet schema audit — existing task/run/evidence schemas (2026-10-16)

Audit for the `M-apm-capability-ratchet` hole
"Audit existing task/run/evidence schemas before defining packet and run
manifests", done before the `mathematical-capability-packet.v1` derivation in
this directory. Purpose: name what already exists, so the packet schema does
not duplicate, weaken, or silently diverge from established identity and
evidence conventions.

## What exists (inspected)

1. **APM problem/task manifest** —
   `/home/joe/code/futon3c/src/futon3c/apm/countdown_manifest.clj`:
   the APM problem inventory with pinned revisions and qualification
   reporting. Identity is per-problem id (`a94J05` class), provenance is
   git-revision pinned, and qualification is a separately recorded artifact
   (the Lean-backed manifest-qualification path). Lesson taken: identities
   are stable ids plus content digests, never moving references.
2. **Evidence-store entries (futon2)** —
   `/home/joe/code/futon2/data/` (`evidence/…​.source` files, `wm-*`
   directories): every durable record carries a `:schema` discriminator
   string (e.g. `wm/handoff-contract-v1`, `wm/open-cascade-authoring-requisition-v1`)
   and full-value sha256 pins. Lesson: a schema discriminator plus
   version-suffixed names is the established convention; sha256 hex (64
   lowercase) is the digest vocabulary.
3. **Analysis run manifests (futon0)** —
   `/home/joe/code/futon0/analysis/audits/` (`pattern-stage-manifest-*.json`,
   `operator-reply-pilot-2026-10-01/manifest.json`): run manifests here are
   flat JSON with recorded inputs and a fixed point requirement for
   generated outputs. Lesson: run manifests record their inputs by digest
   and must be replay-stable.
4. **Mission/hole records (futon0)** —
   `/home/joe/code/futon0/holes/missions/`: checkbox task lines with stable
   per-line ids. Lesson: acceptance is line-addressable; schema artifacts
   should cite the mission line they discharge.

## Consequences for `mathematical-capability-packet.v1`

- The packet needs its own `:schema` discriminator string and integer
  `:version`, following the `wm/*-v1` convention.
- Provenance must be sha256/git-revision pinned content digests (64-hex
  lowercase), not branch names or `HEAD`.
- Status is a closed keyword vocabulary with an explicit transition table;
  terminal states must have no outgoing edges, so promotion cannot be
  reached except through a sealed evaluation.
- The packet must keep **applicability boundaries** as first-class data
  (vector of `{boundary, rationale}`), not free text, so a consumer can
  refuse out-of-boundary use mechanically.
- No run-manifest fields belong in the packet: run manifests are a separate
  artifact family (existing flat-manifest convention above), and conflating
  them would make packet identity unstable across runs.
- Closed-world fields: unknown top-level keys are refused, mirroring the
  evidence store's typed-refusal discipline (a silent extra field would let
  an unaudited claim ride inside a sealed packet).

These decisions are carried into `mathematical-capability-packet.v1.edn`
(normative schema) and enforced mechanically by `validate_packet.py` with the
fixtures in `fixtures/`.
