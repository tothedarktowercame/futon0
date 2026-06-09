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

## Per-turn live target

Joe's correction after the first WM pane: this does not need a second `E-affect-autoclock` excursion. The per-turn
runner is part of **E-affect-live** itself.

The target shape is analogous to `M-autoclock-in`: affect should be observed when a new Evidence Landscape turn
arrives, not only by a manual batch scan. The live path should:

- detect a newly logged turn or turn-window at the same boundary `M-autoclock-in` uses for reclocking;
- run the existing futon0/futon3c affect detector over only the new material;
- append or upsert one normalized JSONL transition per detected affect event;
- preserve stable `transition_id` / `session_id` / `timestamp` keys so repeated observer runs are idempotent;
- regenerate the WM `affect-events.json` summary, or replace it with a small live endpoint, so the Affect Events
  pane tracks the latest turn stream;
- leave `arrow-witness-for-event` read-only and leave peradam certification gated by labor + fruit + G2.

**Acceptance bar for the live runner:** create two adjacent test turns, run the observer twice, and show that the
first run writes exactly the new affect rows while the second run writes zero duplicates. Then show WM's Affect
Events pane changes after the new turn without hand-editing the pane data.

**Built 2026-06-09 (codex-4):** `futon0.rhythm.affect --live` is the per-turn runner entrypoint. It expands to
`--write --append --incremental --refresh-wm-summary`, fetches with overlap from the latest materialised transition,
filters already-written `transition_id`s, appends only unseen rows, and regenerates the WM `affect-events.json`
summary from the JSONL stream. The existing batch mode remains available; `--live` is the hook-friendly path for
`M-autoclock-in`-style invocation after new turns.

**Hook installed:** `futon3c/emacs/agent-chat.el` now calls the runner non-blockingly after a chat-turn evidence
POST succeeds. The hook is single-flight (`agent-chat--affect-live-process`) so repeated turns do not pile up
parallel refreshes; it appends the current `--evidence-url` and runs from `/home/joe/code/futon0`.

**Validation:** the regression fixture creates adjacent turns and runs `--live` twice: first run `new: 2`, second
run `new: 0`. The same check on the real Evidence Landscape appended 19 newly observed rows, refreshed WM to
104 events / 74 candidates, and an immediate second run returned `new: 0`.

## Boundary

This excursion clears the stale "feed absent" blocker and advances G1 from discovery to live read-only binding.
It does **not** close `M-pudding-peradams`, certify the ready multiball, or relax the anti-laundering gates.
Affect remains necessary-not-sufficient: the labor witness, out-of-blanket fruit witness, and G2 performed-affect
guard still decide whether a live work-event may become a certified peradam.
