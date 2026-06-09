# Excursion: E-affect-live

**Date:** 2026-06-09
**Owner:** codex-4
**Scope:** clear stale affect-feed remarks and wire live affect material into `M-pudding-peradams` without
certifying a peradam prematurely.

## Claim

The previous `M-capability-star-map` note said G1 was gated because `affect.jsonl` was absent and the affect feed
was not running live. That was stale. The producer already existed; the missing work was to materialise the file,
record the state, and give `M-pudding-peradams` a read-only resolver over the live material.

## Evidence

Live materialisation command:

```sh
clojure -Sdeps '{:paths ["scripts"] :deps {org.clojure/data.json {:mvn/version "2.5.0"}}}' \
  -M -m futon0.rhythm.affect \
  --evidence-url http://localhost:7070/api/alpha/evidence \
  --lookback-hours 24 --limit 1000 --max-transitions 200 --write
```

Observed output:

```text
wrote: /home/joe/code/storage/futon0/vitality/affect.jsonl
entries: 1000 turns: 138 transitions: 85
```

The generated JSONL rows include `transition_id`, `session_id`, `timestamp`, `marker`, `value`, `source`,
`author`, `trigger_text`, and `event-type`.

## Wiring

`futon7/holes/pudding-prover.bb` now exposes `arrow-witness-for-event`, a read-only G1 resolver for
`M-pudding-peradams`. It accepts either:

- `:transition-id` for an exact affect transition, or
- `:session-id` with optional `:since` / `:until` ISO timestamps for a turn-window query.

It reads `storage/futon0/vitality/affect.jsonl`, fails closed for missing feed / missing selector / no candidate
event, and returns only an `:arrow-witness?` candidate. It does not call `certify-peradam` and cannot award a
peradam by itself.

## Boundary

This excursion clears the stale "feed absent" blocker and advances G1 from discovery to live read-only binding.
It does **not** close `M-pudding-peradams`, certify the ready multiball, or relax the anti-laundering gates.
Affect remains necessary-not-sufficient: the labor witness, out-of-blanket fruit witness, and G2 performed-affect
guard still decide whether a live work-event may become a certified peradam.
