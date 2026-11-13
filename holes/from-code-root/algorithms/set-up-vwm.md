# Algorithm: Set Up Virtual War Machine (vWM)

Date: 2026-09-17
Status: draft, untested. Drafted by claude-8 from the 2026-09-16 setup as
recorded; nobody has run it yet.

## Purpose

Stand up (or re-brief) a set of Zai seats that act as the War Machine's nodes
and item owners, so that work on the War Machine can be run *through* the
machine's own architecture (the vWM) alongside the coded machine. The vWM and
the coded WM are meant to simulate each other until they converge.

The algorithm owns the **setup spine**: seats, aliases, registry metadata,
briefing delivery, acknowledgement and the roster record.

It does **not** own what the seats are told. Briefings are **data**: a
versioned briefing set that changes as the War Machine is better understood.
Re-briefing means publishing a new briefing-set version and rerunning steps
5–7. Nothing in this file changes.

## Scope and non-ownership

- Not owned here: the content of any briefing; the checklist; the closure DAG;
  what a tick does (that is `p4ng/wm-walkthroughs/build-loop/vm/VM-PROTOCOL.md`);
  commissioning build work to a seat (that goes through the coordinator's own
  commission procedure).
- A setup turn is priming only. No seat starts work during setup.
- Seat identities are never recreated to re-brief (futon3c I-1). Re-briefing
  reuses existing sessions.

## Inputs

1. **Seat list.** One entry per seat: the checklist code or node id it holds
   (e.g. `WM-10`, `R2`, `TRACE`, `E05`) and the Zai agent id (`zai-N`).
2. **Briefing set** (data; see "Briefing set shape" below), with a version and
   an as-of date.
3. **Coordinator identity.** The agent that engages seats and receives their
   replies (2026-09-16: codex-28; from 2026-09-16 evening: claude-4).
4. **Campaign id** (e.g. `wm-item-ownership-2026-09-16`).

## Preconditions

1. The checklist the codes refer to exists and its hash is recorded
   (`p4ng/CHECKLIST-fundamentals.md`; the DAG stores `checklist-sha256`).
2. Each `zai-N` is registered in Agency (`GET :7070/api/alpha/agents`) and is
   invoke-ready. New seats come from `futon3c/scripts/zai-picker`, run by
   Joe or from a terminal. There is no Claude-callable spawn.
3. Zai quota is available for one short turn per seat. 46 seats = 46 turns.
4. No seat has in-flight work that the priming turn would interrupt.

## Briefing set shape

The briefing set is the data this algorithm delivers. One file per version,
e.g. `p4ng/wm-walkthroughs/item-owners/briefings/vN.edn`:

```clojure
{:briefing-set/version 2
 :briefing-set/as-of "2026-09-17"
 :briefing-set/supersedes 1
 :briefing-set/why "what changed in understanding of the WM since the last version"
 :coordinator "claude-4"
 :roles
 {:item-owner {:may [...] :may-not [...]}        ; e.g. no checkbox edits
  :vwm-node   {:may [...] :may-not [...]         ; e.g. one step per dispatch
               :protocol "p4ng/wm-walkthroughs/build-loop/vm/VM-PROTOCOL.md"}}
 :shared-sources {:checklist "p4ng/CHECKLIST-fundamentals.md"
                  :dag "p4ng/wm-walkthroughs/build-loop/closure/closure-dag.json"
                  :roster "p4ng/wm-walkthroughs/item-owners/roster.json"}
 :seats
 [{:code "WM-10" :alias "wm-10" :agent "zai-18"
   :roles #{:item-owner}
   :requirement "<exact checklist clause, verbatim, never weakened>"
   :sources {:cascade "p4ng/wm-walkthroughs/cascades/wm-10/README.md"}
   :notes "<item-specific current state, dated>"}
  {:code "R2" :alias "r2" :agent "zai-24"
   :roles #{:item-owner :vwm-node} :node :R2
   :real-code {:fn "observation/observe" :route-tag :R2}
   ...}]}
```

The rendered primer text is a projection of this data (template + seat entry).
It is regenerated, never hand-edited.

## Procedure

1. **Check existing seats and aliases.**
   - Read `GET /api/alpha/agents` and `futon3c/resources/agent-personas.edn`.
   - For each seat in the input list: confirm `zai-N` exists, and that its
     code/alias is either unmapped or already maps to that same `zai-N`.
   - An alias that maps to a different agent stops the run.
2. **Write aliases.** Add the code and its lowercase spelling for each seat to
   `agent-personas.edn`, then call `reload-agent-personas!` over Drawbridge
   (`futon3c.agency.registry`). Aliases change lookup only; they never add
   registry keys.
3. **Record registry metadata** on each seat: `wm-item` (code), `wm-role`,
   `wm-primer` (path of the rendered primer), `campaign-id`, and the
   `briefing-set/version`.
4. **Validate the briefing set.** Every seat in the input list has exactly one
   entry, and every entry's code is in the checklist. Each `:requirement`
   matches the checklist clause byte for byte. Each `:roles` value is defined
   under `:roles`. Every path in `:sources` and `:shared-sources` exists.
   Any failure stops the run before any seat is contacted.
5. **Render primers** from the briefing set into
   `p4ng/wm-walkthroughs/item-owners/primers/<code>.md`. Each primer states:
   the seat's identity and code; that this turn is priming only; the exact
   requirement; the authoritative paths; the role's may/may-not; the
   coordinator; the briefing-set version and as-of date; and that the briefing
   is dated orientation, so the first real turn re-verifies current evidence.
   The primer ends with the exact acknowledgement line (step 6).
6. **Deliver, one seat at a time.** Bell each seat with its primer
   (`agency_send.py --from <coordinator> --to <alias> --kind bell`). The only
   valid reply is `ACK <zai-N> owns <code>; briefing v<N>; no work started.`
   Wait for each acknowledgement before the next bell. No parallel wave.
7. **Record the roster.** In `roster.json`, per seat: `item`, `agent`,
   `nick`, `address`, `primer`, `briefing-version`, `state`
   (`primed` / `ack-missing` / `ack-malformed`), and the priming `job` id.
   Commit the briefing set, primers, roster and personas together.
8. **Report** to the coordinator and to Joe's surface: seats primed, seats
   not primed and why, and the briefing-set version now in force.

## Re-briefing (a new briefing-set version)

Run steps 4–8 with the new version. Seats whose entry did not change may be
skipped, but the roster still records which version each seat last
acknowledged. Once a new version is in force, a packet may not override a
role rule inline. Change the briefing set instead. (Observed 2026-09-16: packets
told owners the primer's "propose, don't implement" rule "doesn't apply";
that is the drift this rule prevents.)

## Stop conditions

- An alias collision (step 1), or a briefing-set validation failure (step 4).
- A seat replies with anything other than the acknowledgement line, or starts
  work during priming. Record `ack-malformed` and stop to report. Do not re-bell.
- Zai quota runs out mid-run. Record the seats done; resume from the roster.

## Outputs

- Updated `agent-personas.edn` and registry metadata.
- `briefings/vN.edn`, rendered `primers/*.md`, updated `roster.json`, in one commit.
- A short report naming the briefing-set version in force.

## Evidence this draft was built from (2026-09-17)

- Registry: 46 seats `zai-9`..`zai-54`, all `wm-role single-item-owner`,
  `campaign-id wm-item-ownership-2026-09-16`, `wm-item` and `wm-primer` set.
  Seats `zai-2`..`zai-8` carry no WM metadata.
- `futon3c/resources/agent-personas.edn`: code and lowercase alias → `zai-N`.
- p4ng `9ef80b5` "Establish 46 exclusive Zai checklist owners and bounded
  management protocol"; `c473cf5` records the 46 acknowledgements.
- `item-owners/primers/WM-10.md`: priming-only turn, `ACK` reply, verbatim
  requirement, authoritative paths, scope limits, "dated briefing" caveat.
- `vm/VM-PROTOCOL.md`: the @R agents as tick nodes (a role the 09-16 primers
  do not describe).
- Buffers `*zai-repl:zai-18*` (WM-10) and `*zai-repl:zai-13*` (WM-05), both
  glm-5.2. zai-18, primed as an owner, edited the checklist, STATUS.json and
  DAG hashes for codex-28. zai-13 did read-only planning for codex-7. The
  primers still name codex-28 as coordinator. Both are reasons the briefing
  has to be versioned data carrying roles explicitly.
- The zai `run_readonly` peripheral rejected several harmless compound
  commands as "destructive" (zai-13 buffer). Worth checking before relying on
  read-only seats.
