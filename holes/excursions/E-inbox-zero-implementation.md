# Excursion: E-inbox-zero-implementation

**Status:** IDENTIFY — data specification before implementation  
**Date opened:** 2026-08-21  
**Entry point:** [`README-inbox-zero.md`](../../README-inbox-zero.md) identifies
turn-end promotion as the missing mechanism for real uncommitted work. The
existing `futon3/scripts/multi_watcher.clj` already observes file creation and
change. The proposed extension is to retain enough provenance to remind the
right live session about its uncommitted files without issuing a bell for every
save.

## Goal

Maintain a durable, inspectable association among working-tree files, agent
sessions, notifications, and eventual commits. Use it to send a brief followup
when a session has accumulated a meaningful dirty-file batch, while preserving
the stronger inbox-zero invariant that a small batch cannot remain invisible
forever.

This excursion records the implementation and its later extensions. The first
slice is the data contract only. It does not yet add watcher behavior, dispatch
messages, create commits, or modify autoclock state.

## Boundary and invariants

1. **Observe; do not infer authorship from dirt alone.** A dirty file proves
   that a worktree differs from `HEAD`; it does not prove which session caused
   the difference. Attribution requires a witnessed session/file event.
2. **Address an exact seat.** A followup target is an `(agent-id, session-id)`
   pair validated against `GET /api/alpha/agents/<agent-id>`. Agent id alone is
   not durable identity, and `/agency/connected` is not a current endpoint.
3. **Fail closed on ambiguity.** If two live sessions have equally valid claims
   to a path, or the recorded session is stale, retain the observation but do
   not choose a recipient.
4. **Recheck before delivery.** A queued notification is cancelled if its files
   have become clean, committed, ignored, or attributed elsewhere before send.
5. **Thresholds suppress noise, not obligations.** The normal trigger is a
   batch of distinct dirty files. An age trigger independently catches one to
   four files that would otherwise remain dirty indefinitely.
6. **Notifications are followups, not parks.** A park is a continuation created
   by an agent waiting on dependencies. Watcher output is an external,
   event-triggered message delivered through the same durable ready-inbox and
   busy/lease/acknowledgement machinery, but it must have its own type.
7. **Git identity is explicit.** Every record names repository identity,
   worktree root, and path relative to that root. Absolute paths alone cannot
   distinguish worktrees or survive relocation.
8. **History rewrites are expected.** Commit linkage records both git SHA and,
   when available, a stable change identity. A SHA is an observation, not a
   permanent semantic identifier.

## Data specification v0

The examples use EDN to make cardinality and optionality explicit. They specify
logical records, not yet a choice between an EDN log, XTDB entities, or another
durable store. Times are UTC instants. IDs are opaque and globally unique.

### 1. Session seat

```clojure
{:record/type :inbox-zero/session-seat
 :seat/id "seat:<agent-id>:<session-id>"
 :agent/id "codex-11"
 :session/id "..."
 :surface :emacs-repl
 :repl/buffer "*codex-11*"       ; optional display locator
 :host/id "dionysus"
 :workspace/root "/home/joe/code"
 :observed-at #inst "..."
 :registry-witness
 {:endpoint "/api/alpha/agents/codex-11"
  :session/id "..."
  :observed-at #inst "..."}}
```

The registry witness validates delivery identity. It does not prove file
authorship.

### 2. File observation

```clojure
{:record/type :inbox-zero/file-observation
 :observation/id "file-observation:<uuid>"
 :repo/id "futon0"
 :repo/root "/home/joe/code/futon0"
 :worktree/id "worktree:<stable-local-id>"
 :path "holes/excursions/E-inbox-zero-implementation.md"
 :git/status :untracked          ; :modified :deleted :renamed :clean :ignored
 :content/hash "sha256:..."      ; nil for deletion
 :head/sha "..."
 :index/hash "..."               ; optional
 :observed-at #inst "..."
 :source :multi-watcher}
```

The watcher emits a new observation only when the meaningful state tuple
`[git/status content/hash head/sha index/hash]` changes. Mtime alone may trigger
a scan but is not durable evidence of a change.

### 3. Session–file claim

```clojure
{:record/type :inbox-zero/session-file-claim
 :claim/id "claim:<uuid>"
 :seat/id "seat:<agent-id>:<session-id>"
 :repo/id "futon0"
 :worktree/id "worktree:<stable-local-id>"
 :path "holes/excursions/E-inbox-zero-implementation.md"
 :relation :edited-by            ; initially only witnessed edits
 :witness/type :tool-edit        ; later: :buffer-save, :explicit-subscription
 :witness/id "<tool-call-or-save-event-id>"
 :first-observed-at #inst "..."
 :last-observed-at #inst "..."
 :state :active}                 ; :active :superseded :released
```

The unique live claim key is `[worktree/id path seat/id]`. Claims are not
silently reassigned by a later filesystem event. If multiple active claims
exist, routing is ambiguous until one is released or an explicit ownership
decision is recorded.

### 4. Dirty-set snapshot

```clojure
{:record/type :inbox-zero/dirty-set
 :dirty-set/id "dirty-set:<uuid>"
 :seat/id "seat:<agent-id>:<session-id>"
 :repo/id "futon0"
 :worktree/id "worktree:<stable-local-id>"
 :members [{:path "a.clj"
            :observation/id "file-observation:<uuid>"
            :dirty-since #inst "..."}]
 :count 5
 :oldest-dirty-at #inst "..."
 :computed-at #inst "..."}
```

Membership contains distinct currently dirty paths with one unambiguous active
claim for the seat. Ignored files, clean paths, and ambiguous claims are
excluded and reported separately.

### 5. Followup notification

```clojure
{:record/type :inbox-zero/followup
 :followup/id "inbox-zero-followup:<uuid>"
 :seat/id "seat:<agent-id>:<session-id>"
 :dirty-set/id "dirty-set:<uuid>"
 :trigger {:type :count :threshold 5} ; or {:type :age :threshold-ms 86400000}
 :dedupe/key ["seat:<agent-id>:<session-id>"
              "futon0"
              "<sorted-member-state-hash>"]
 :message "Inbox zero: futon0 has 5 uncommitted files associated with this session. Review, commit, or explicitly release them."
 :state :queued                       ; :delivered :acked :cancelled :expired
 :created-at #inst "..."
 :delivered-at nil
 :acknowledged-at nil
 :cancel-reason nil}
```

A followup is eligible when either:

- the seat has at least `dirty-count-threshold` distinct dirty paths (default
  **5**); or
- its oldest dirty path reaches `dirty-age-threshold` (default **24 hours**).

Only one unresolved followup exists per dedupe key. Further saves to the same
member states do not generate another message. After acknowledgement, another
followup requires either a changed member-state hash or an explicit reminder
interval. Delivery obeys the receiver's busy gate; eligibility never bypasses
normal turn serialization.

Thresholds are policy values stored with the emitted trigger, rather than
hard-coded facts. This lets the default of five be calibrated from observed
nag rate without making old decisions uninterpretable.

### 6. Commit observation and session linkage

```clojure
{:record/type :inbox-zero/commit-observation
 :commit-observation/id "commit-observation:<uuid>"
 :repo/id "futon0"
 :worktree/id "worktree:<stable-local-id>"
 :commit/sha "..."
 :change/id nil                         ; populate when a stable id exists
 :parents ["..."]
 :paths ["a.clj" "b.md"]
 :authored-at #inst "..."
 :observed-at #inst "..."
 :source :git}
```

```clojure
{:record/type :inbox-zero/session-commit-link
 :link/id "session-commit-link:<uuid>"
 :seat/id "seat:<agent-id>:<session-id>"
 :commit-observation/id "commit-observation:<uuid>"
 :paths [{:path "a.clj" :claim/id "claim:<uuid>"}]
 :coverage :partial                    ; :complete only if every changed path
 :basis :path-claim-intersection       ; never commit author-name matching
 :linked-at #inst "..."}
```

A commit can link to several sessions when its paths have witnessed claims from
several seats. Conversely, one session can link to many commits. `:complete`
means every path in the commit has an unambiguous witnessed claim; it does not
mean that one session authored the entire commit.

## State transitions

```text
tool edit/save ──> session-file claim ──> dirty observation
                                             │
                                  recompute per-seat dirty set
                                             │
                         count >= 5 OR oldest age >= 24h
                                             │
                                      queued followup
                                             │
                       revalidate identity + git state + claims
                                  │                       │
                               deliver                 cancel

git commit ──> commit observation ──> intersect witnessed path claims
                                      ──> session-commit link
                                      ──> clean observations / smaller sets
```

## Ambiguity and lifecycle rules

- A session ending makes its claims historical; it does not erase them.
- A new session for the same agent does not inherit claims automatically.
- Renames preserve association only when the watcher has a witnessed rename
  (content identity plus git rename evidence); otherwise old and new paths are
  separate observations.
- A commit does not close unrelated dirty claims in the same repository.
- A clean observation releases notification pressure but retains the claim and
  history needed for commit linkage.
- A notification for a disconnected seat stays queued only while the session
  remains registry-valid. It must not be redirected to a newer session merely
  because the agent id matches.
- Shared or manually edited files remain visible as `:ambiguous`; the system
  must never manufacture a session edge to make the dashboard look complete.

## Relationship to M-autoclock-in

`M-autoclock-in` establishes the discipline needed here: attribution must come
from an explicit, resolved witness, and missing attribution is safer than false
attribution. Its edit-activity reclock work may later consume the same
session-file claims, and commit observations may later carry the active
`campaign-id`, `mission-id`, and `excursion-id` witnessed for the seat.

Those clock fields are deliberately absent from v0. Adding them before the
session/file/commit chain is trustworthy would make the clock appear more
precise while inheriting uncertain authorship. A later version should link to a
clock-state observation by id rather than copying mutable labels into every
record.

## Implementation slices

1. **Specify and fixture.** Ratify these records and add fixtures for one-seat,
   multi-seat ambiguous, threshold, age, session-rotation, and commit cases.
2. **Observe.** Extend the watcher to emit content-aware git observations and
   consume witnessed tool-edit/save events. No notifications yet.
3. **Associate.** Materialize claims and dirty sets; expose ambiguous and
   unattributed files explicitly.
4. **Notify.** Add the typed followup queue using Agency's ready-inbox,
   serialization, lease/acknowledgement, retry, and exact-seat validation.
5. **Link commits.** Observe new commits, intersect their paths with claims,
   and close or shrink dirty sets from fresh git state.
6. **Evaluate.** Measure notification frequency, false attribution, stale
   delivery, time-to-clean, and whether five files plus 24 hours are useful
   defaults.
7. **Extend.** Only after those gates pass, consider autoclock witnesses,
   mission summaries, automatic promotion, and fleet-wide aggregation.

## Acceptance gates for the first implementation

- Restarting the watcher reproduces the same active state without duplicate
  claims or followups.
- Five distinct dirty files yield one followup; repeated saves yield none.
- One dirty file yields no immediate followup and one age-triggered followup at
  the configured limit.
- A file cleaned or committed before delivery is absent from the delivered
  message; an empty set cancels it.
- Session rotation, missing registry identity, and equal competing claims do
  not deliver to a guessed recipient.
- A commit touching claims from two seats produces two partial links, not one
  falsely complete author attribution.
- Every notification and link can be traced back to immutable observation and
  witness ids.

## Implementation log

### Slice 1 — durable observation and claim foundation (2026-08-21)

The storage boundary belongs to `futon3`, alongside `multi_watcher`, while
Agency remains the later delivery owner. Implemented
`futon3.inbox-zero.state` as a policy-free v0 store for session seats, file
observations, and session-file claims:

- records validate their required identity and witness fields;
- claims fail closed unless their exact seat has already been witnessed;
- record ids are immutable and identical replay is idempotent;
- snapshots replace atomically under an inter-process file lock;
- missing state starts empty, while corrupt, unsupported, or noncanonical state
  raises rather than being mistaken for an empty inbox;
- an EDN fixture exercises the v0 record shapes independently of live watcher
  and Agency services.

Evidence: `futon3/test/futon3/inbox_zero/state_test.clj` — 7 tests, 13
assertions, 0 failures/errors; clj-kondo clean; `check-parens.el` clean. The
watcher is not wired to this store yet. That remains deliberately outside the
foundation slice.

### Slice 2 — association, dirty sets, and eligibility (2026-08-21)

Implemented `futon3.inbox-zero.projection` as a pure view over the durable v0
records:

- the latest observation per worktree/path determines current Git state;
- the latest immutable claim per worktree/path/seat determines whether that
  seat still has an active claim;
- exactly one active seat produces membership, zero produces `:unattributed`,
  and competing seats produce `:ambiguous` with no routed dirty set;
- dirty age begins at the first observation in the current uninterrupted dirty
  run, so a clean transition resets the clock;
- sets contain distinct paths and remain scoped to seat/repo/worktree;
- notification eligibility defaults to five paths or an independently elapsed
  24 hours;
- dedupe keys hash evidence-bearing member state and remain stable across scan
  time and member ordering;
- a replacement session for the same agent receives no inherited claim.

Evidence: the state and projection namespaces together run 16 tests / 33
assertions with 0 failures/errors; clj-kondo reports 0 errors and 0 warnings;
`check-parens.el` and `git diff --check` pass. Watcher ingestion remains the
next slice; no notification has yet been emitted.

### Slice 3 — watcher observations and explicit witness intake (2026-08-21)

`multi_watcher.clj` now has an opt-in inbox-zero lifecycle:

```text
--inbox-zero-state <snapshot.edn>
--inbox-zero-witnesses <immutable-record-directory>
```

On every cycle it reads typed seat/claim witnesses, scans each configured Git
worktree with porcelain status, emits only meaningful state transitions, writes
the snapshot atomically, and computes the pure dirty-set projection. With no
state option, existing watcher behavior is unchanged.

The edit-source audit found no honest canonical stream that already covered all
surfaces. JVM autoclock observes agent edits only for exact mission documents
and durably records only clock transitions; Emacs after-save activity is also
mission-document-specific and is not an exact-seat authorship stream; session
artifact lists are summaries rather than immutable edit witnesses. Therefore
the slice adds `futon3.inbox-zero.watcher/write-witness!` as the canonical
producer boundary instead of inferring claims from dirt:

- every seat or claim is one immutable EDN file, atomically published;
- several surface producers may safely publish distinct witness files;
- the watcher is the single writer of the derived state snapshot;
- a witness id reused with different content fails closed;
- malformed witness input cannot replace state;
- a missing claim leaves a dirty path explicitly `:unattributed`.

Git observation covers modified, untracked, deleted, and renamed paths, hashes
current file content, records HEAD and stable local worktree identity, emits a
clean transition when prior dirt disappears, and emits nothing on unchanged
rescans. The transition id includes the prior observation id, so a path that
becomes dirty, clean, and dirty again remains a real history while restarts are
idempotent.

Evidence: all three inbox-zero namespaces run 20 tests / 46 assertions with 0
failures/errors; clj-kondo reports 0 errors and 0 warnings; `check-parens.el`
and `git diff --check` pass. A real Babashka `multi_watcher` one-cycle smoke on
a temporary Git repository wrote one durable observation for one untracked
file and projected it as one unattributed path. Producers on Emacs, Codex, and
Claude still need to call the typed witness boundary; that explicit producer
gap is the next integration after delivery semantics, not a reason to guess.

### Slice 4 — typed durable followup delivery (2026-08-21)

Agency now owns a distinct `futon3c.agency.followup-queue`; inbox-zero messages
do not enter `parked_on` and never acquire a `park-id` or continuation meaning.
The HTTP contract is:

```text
POST /api/alpha/followups
GET  /api/alpha/followups/ready?agent=&session=
POST /api/alpha/followups/ready/ack
POST /api/alpha/followups/cancel
```

Queue records carry `followup-id`, `:type :inbox-zero`, exact agent/session,
dedupe key, prompt, metadata, and lifecycle timestamps. Enqueue rejects an
agent/session pair that does not exactly match the live registry. Ready polling
uses the same authoritative server-side busy signal as parks but leases from a
separate durable queue. Immediately before lease, a validation callback checks
the queued exact session again; rotation cancels the item and never redirects
it. Unacknowledged leases expire and requeue. Explicit cancellation is
available for a sender that rechecks its source condition.

The Emacs poller now polls this queue separately and injects its messages as
`followup:` rather than `continuation:`. It deduplicates by `followup-id` and
ACKs only through the followup endpoint. Park finalization and `more-pending`
remain untouched.

On the producer side, `multi_watcher` accepts an optional
`--inbox-zero-followup-url`. It posts only sets passing the pure five-file or
24-hour eligibility rule, carrying the exact witnessed seat and stable dedupe
key. Agency remains authoritative for identity and delivery.

Evidence: futon3 inbox-zero tests run 21 tests / 51 assertions; the new
futon3c queue tests run 2 tests / 9 assertions; all pass with 0 failures/errors.
clj-kondo reports 0 errors and 0 warnings for the new/changed namespaces;
`check-parens.el` and `git diff --check` pass.

### Slice 5 — exact-seat producer and end-to-end contract (2026-08-21)

The first honest surface producer is the server-side Claude tool stream. It
retains edit-shaped `tool_use` details by tool id, then emits an immutable seat
and session-file claim only when the correlated `tool_result` is not an error.
Intent alone is not authorship evidence. The supported tools are `Edit`,
`Write`, and `MultiEdit`; the named file must resolve into a Git worktree.

`FUTON3_INBOX_ZERO_WITNESS_DIR` is the canonical producer/consumer boundary.
Claims carry the exact agent/session passed through the invoke path. Repeated
delivery of one result is idempotent and session rotation creates a different
seat. The producer never inspects Git dirt; the watcher remains responsible for
that independent observation.

The end-to-end focused tests now cover two joined halves of the real boundary:

- immutable witness intake -> five Git observations -> pure count threshold ->
  exact-session HTTP enqueue body;
- HTTP enqueue -> authoritative busy withholding -> Emacs-shaped ready lease ->
  ACK and terminal state.

The followup tests also bind a private queue path, ensuring a test run cannot
overwrite the serving JVM's durable queue.

Surface gaps remain explicit. The server does not receive a successful Codex
edit-tool result stream at this boundary, and ordinary Emacs save hooks do not
prove an authoring seat. Neither emits claims. The operational README documents
the standalone watcher command and shared intake path. The bootstrap-owned
in-JVM watcher still lacks this optional lifecycle, so production activation
requires one final slice to integrate it while retaining exactly one state
writer; running two watchers would violate the storage contract.

### Slice 6 — bootstrap-owned lifecycle and closure (2026-08-21)

`futon3c.watcher.multi` now adapts directly to the futon3-owned v0 state,
observation, projection, and sender namespaces. A minimal
`futon3/inbox-zero-lib/deps.edn` exposes those exact sources without importing
futon3's unrelated XTDB1/NLP dependency graph into the deliberately
single-major futon3c JVM. There is no second implementation of the rules.

The feature is explicit and defaults off. `FUTON3C_INBOX_ZERO_ENABLED=true`
activates it; state, witness intake, and followup URL have separate environment
settings and documented defaults. Bootstrap passes its canonical root/label
set into the v0 observer each cycle. The watcher acquires an OS-level exclusive
`<state>.writer.lock`, holds it for its service lifetime, and releases it on
stop. Competing lock-aware JVM ownership fails closed. The standalone Babashka watcher
remains a diagnostic/manual alternative and must not target the production
state concurrently.

Watcher status is the readiness projection: before the first successful cycle
it is enabled but not ready; afterward it reports paths plus observation,
dirty-set, ambiguity, unattributed, and delivery counts and the last successful
inbox-zero cycle time. A feature exception enters the watcher's existing loud
cycle-error state rather than being mistaken for successful readiness.

Focused tests cover disabled compatibility, adapter inputs, readiness counts,
exact HTTP forwarding, competing writer rejection, stop/restart lease release,
and durable replay/idempotency. Live activation is now configuration plus a
service restart performed from a separate operator session; this excursion does
not authorize this routed agent to restart its own Agency JVM.

Evidence: the combined futon3c watcher/witness/followup focus runs 28 tests / 108
assertions with zero failures or errors; the futon3 suite runs 171 tests / 1254
assertions green after extracting the library boundary. Clj-kondo reports 0
errors and 0 warnings on all changed Clojure, `check-parens.el` is clean, and
dependency-tree inspection shows `futon3/inbox-zero` without futon3's XTDB1 or
Stanford NLP graph.

### Slice 7 — commit observation and session linkage (2026-08-24)

The original plan's "Link commits" slice, built by codex-9 (futon3
`2b82dc21`, base `8d8e705`), reviewed by claude-3 (all gates re-run
independently; all nine acceptance behaviours verified as named tests).
Records `:inbox-zero/commit-observation` and `:inbox-zero/session-commit-link`
land per data spec §6, with `:basis :path-claim-intersection` enforced by
validation (author-name matching structurally absent). Coverage is
`:complete` only when every changed path has exactly one unambiguous active
claim from one seat; multi-seat commits yield partial links.

**Spec extension (settled by two stop-and-bell rounds):** the commit scanner's
durable cursor is a third immutable record type, not mutable snapshot state —

```clojure
{:record/type :inbox-zero/commit-scan-cursor
 :cursor/id "commit-scan-cursor:<hash>"
 :worktree/id "worktree:<id>"
 :cursor/sha "<last-observed commit>"
 :cursor/reason :baseline            ; | :advance | :rebaseline-rewrite
 :prior/cursor-id nil                ; nil only on :baseline; chains otherwise
 :observed-at #inst "..."}
```

Written only on movement (volume bounded by commit activity, not scan
frequency). The current cursor is the head of the `:prior/cursor-id` chain —
never timestamp-ordered; a forked or cyclic chain fails closed as corrupt
state. First run baselines at HEAD and emits no observations (claims cannot
predate producer activation, so deeper history cannot link);
`:commit-lookback` (default 0) is the explicit backfill knob, and retroactive
mission claiming remains a separate pass. A history rewrite emits a loud
`:rebaseline-rewrite` cursor whose prior-id names the cursor whose sha became
unreachable. Evidence: 30 tests / 87 assertions / 0 failures; clj-kondo 0/0;
check-parens and `git diff --check` clean — reported by the author and re-run
by the reviewer.

### Slices 8 and 9 — attribution join; pure promotion planner (2026-08-24)

Both authored by codex-9, reviewed by claude-3 (gates re-run independently).

**Slice 8** (futon3c `520cefb1`): `futon3c.inbox-zero.attribution` — the
commit→mission join. Pure exact-seat attribution of session-commit-links
against clock-lineage edges at a temporal basis; IO wrapper issues one
bounded `type+end+as-of` query per (agent, linked-at) group against the
futon1b compatibility server. Contract points settled during the exchange:
temporal validity is a QUERY BASIS, never a document field (no retraction
key exists or may be invented — `as-of` sets the XTDB2 valid-time base
server-side); fetch failure maps to `:attribution/status :unknown`, never
`:unattributed` (a store error must not read as "no mission"); a defensive
`clocked-at-ms <= linked-at` guard protects against wrong-basis callers.
Statuses: `:attributed`/`:unattributed`/`:ambiguous`/`:unknown`. Verified
live against the production substrate in review: a real 35-day-old edge
attributes (`M-capability-zones`), and a seat with zero edges reads
`:unattributed` against confirmed ground truth. Incidental find, fixed
directly: `clock_lineage/query-edges-of-type` sent `limit=10000` into a
server that 400s above 1000, so `reconstitute` silently read `[]` through
its error-swallowing fallback.

**Slice 9** (futon3 `d522f7c2`): `futon3.inbox-zero.promotion/plan-promotion`
— the pure first slice of turn-end promotion. Per repo/worktree with current
dirt: include only paths whose sole active claim names the seat; exclude
with visible reasons (`:ambiguous`/`:unattributed`/`:other-seat`); deletions
includable; a worktree whose dirt is all excluded yields a LOUD held plan
(`:verdict :held`, `:held/reason :nothing-promotable`) rather than
vanishing; globally clean state yields `[]` (no tuple exists — an unscoped
held record would be a manufactured edge). No IO, git, gates, messages, or
execution — those are later slices, and the execution slice's push policy is
an open operator decision. Evidence for both: focused suite 39 tests / 105
assertions / 0 failures; clj-kondo 0/0; check-parens and `git diff --check`
clean — author-reported and reviewer-re-run.

### Slices 10 and 11 — promotion execution and push decision (2026-08-24)

Both authored by codex-9, reviewed by claude-3 (gates re-run independently;
full focused suite now 51 tests / 155 assertions / 0 failures).

**Slice 10** (futon3 `3c3d75f8`): `futon3.inbox-zero.promote-exec/execute-plan!`
— executes one promotion plan: empty-index preflight (an occupied index holds
as `:index-not-empty` with the occupying paths, a NEW held reason — the plan
is not stale, the index is occupied, and the escalation routing needs the
distinction), per-path staleness revalidation, ordered gates with 4KB-bounded
output, staging of exactly the planned paths (NUL-delimited set equality
against `git diff --cached`), commit with caller-supplied message. Every
failure path resets only the index this call owns; `git reset --` is safe
precisely because the preflight guarantees ownership of the entire staged
set (codex-9's catch — the original spec's rollback would have flattened
pre-existing staged work).

**Slice 11** (futon3 `0ab1e579`): `futon3.inbox-zero.promote-push/push-promoted!`
— the auto-push-ordinary / escalate-outliers policy (Joe, 2026-08-24) as
data: ahead-count measured before pushing; counts above the threshold
(default 10) are never pushed and escalate as `:ahead-outlier` — unusual
accumulation is evidence a human should see, not tidy away; plain
fast-forward push otherwise; `:push-failed` (bounded stderr) and
`:no-upstream` are distinct reasons. Deliberately never fetches — fetch
cadence belongs to futon-sync's timer, and the docstring states honestly
that a stale upstream ref undercounts divergence, which the plain push then
surfaces as non-fast-forward.

**Build phase complete.** The unbuilt remainder is wiring: the turn-end hook
in the pouch path (plan → gates/execute → push-or-escalate per seat), and the
**escalation router** (policy refined by Joe, 2026-08-24 — see the tier table
in README-inbox-zero): tier 1 routes outliers and held plans to the
responsible seats resolved from session-commit-links (deliver via the slice-4
followup queue, exact-seat); tier 2 falls through to the street-sweeper
peripheral when exact-seat validation fails; tier 3 — the operator — receives
only holds that need human judgement, canonically `:held :sensitive-content`
from a pre-push sensitivity screen (describe-the-kind rules, not filename
enumerations). Volume alone never reaches the operator.

### 2026-08-24 (afternoon) — held-plan messages, amnesty planner, inferrer

Four reviewed handoffs plus one direct fix, all landed the same day the
watcher went live:

- **Held-plan messages** (futon3c `227eb140`): propose-mode tier-1 messages
  for empty-include plans now summarize exclusions ("N unattributed, M
  other-seat; unattributed: p1..p5 (+n more)") instead of "would promote 0
  path(s)". The loudest output was carrying the least information.
- **Amnesty planner** (futon3 `b0d4928b`): pure `futon3.inbox-zero.amnesty`
  partitions pre-witnessing unattributed dirt into `:exempt` / `:sensitive`
  / `:baseline` per repo/worktree. Exemption outranks sensitivity so a
  deliberate tracer survives every sweep. Execution slice deferred.
- **Attribution-inference discovery** (read-only): for the tracer
  (`futon3c-d:scripts/session-cost.py`, deliberately left unattributed),
  substrate session evidence at 08:21:54 + file mtime 08:21:27 + the seat's
  later same-worktree claims recover the ground truth
  (`seat:claude-3:7cdc25b0-…`). Git authorship is a shared-human-name
  prior, not seat evidence; post-restart roster proves nothing historical.
- **Inferrer core** (futon3 `a0f41644`): pure `futon3.inbox-zero.infer`,
  deterministic evidence-class ranking (`:direct` / `:corroborated` /
  `:weak`), fail-closed verdicts, `:weak` proposals gated behind explicit
  `:allow-weak? true`. Never mints; confirmation does. Adapters slice
  (state + substrate + stat) in flight.
- **Repo-id normalization** (futon3c `09fa4176`, direct fix): witness claims
  minted repo-id from directory basenames while observations carried watcher
  labels ("futon3c" vs "futon3c-d") — every repo-id join would silently miss
  all claims. Root table extracted to `futon3c.watcher.roots`, shared by
  bootstrap and the witness producer. Historical claims keep old labels
  (immutable), so joins stay worktree-id + path keyed forever.

Promotion stays in propose mode until the inferrer arc completes (Joe,
2026-08-24).

## 2026-08-25 — loop closed, restart, epoch amnesty

Slices E–I landed 2026-08-24 evening (all Codex handoffs, each reviewed as a
gate): evidence adapters (`futon3c.inbox-zero.infer-adapters`, 2e5647e2),
amnesty executor (`futon3.inbox-zero.amnesty-exec`, 94f3192f), sweeper lane
(`futon3c.inbox-zero.sweeper`, 5ac4531b), confirmation minting
(`futon3.inbox-zero.confirm`, 57866b31), confirmation intake + route
`POST /api/alpha/inbox-zero/confirm-attribution` (f260f639).

**Closing act (2026-08-24):** the tracer `futon3c/scripts/session-cost.py`
(edited before witnessing existed, deliberately left unattributed by hand) was
attributed end to end by the system: sweep proposed it to `claude-3`, the
seat confirmed via the endpoint, intake re-inferred server-side, minted
`claim:707c154e…` (repeat confirm → `:already? true`), and the next watcher
cycle projected it as a dirty-set member. No state was edited by hand.

**Restart (2026-08-25 18:44):** dev-zone-env block landed (`PROMOTION=propose`,
`SWEEPER=true`, 30 min). Watcher up with all 14 labelled roots; tracer
attribution survived the process boundary. Separately, c4922353 (Opus) made
`promote-at-turn-end!` refuse sessionless seats — 3,263 held-on-nothing plans
over 30 h were that flood seen from the other side.

**Epoch amnesty (2026-08-25 18:5x, Joe: "the usual one, in batches"):**
`plan-amnesty` on live state → 107 unattributed across 9 repos, 0 sensitive.
Exempt: `futon3c src/futon3c/substrate/client.clj` (someone's live, reasoned
change — deserves its own author) and `futon3 li` (1-byte stray). Executed
15 batches with `execute-amnesty-plan!` (kondo error-level gate on .clj):
futon0 1, futon3 3 (cycle-machine / math-formalization / war-room), futon3b 1,
futon3c 3 (labs / technotes / rest), futon5 2 (on branch
`M-propagators-2026-07-15`), futon5a 2, futon6 1, futon7 1 (re-run with
`namespace-name-mismatch` off — a standalone repro script), futon7a 1. All
`:committed`, index empty after each; nothing pushed. Next cycle:
unattributed 107 → 3 (the two exemptions plus
`holes/labs/M-diagramprover/apm-driver/axiom-audit.jsonl`, an append-only log
some process keeps writing — it will keep reappearing until gitignored).

## Open decisions

- Which future exact-success boundaries can cover Codex and direct Emacs edits;
  neither may use a save-time last-editor guess.
- Whether explicit file subscription should override competing edit claims or
  coexist as a differently typed claim.
- The reminder interval after acknowledgement when the exact same dirty set
  remains.
- Whether count thresholds are per seat across a worktree or per seat/repo.
  v0 specifies per seat/repo because it makes the requested action coherent.

## References

- [`README-inbox-zero.md`](../../README-inbox-zero.md) — operational evidence,
  inbox-zero invariant, and turn-end promotion proposal.
- `futon3/scripts/multi_watcher.clj` — existing multi-repository file and
  commit observer to extend.
- `futon3c/README-park.md` — exact-seat identity and durable continuation
  delivery behavior; followups reuse the delivery substrate but are not parks.
- `futon3c/holes/missions/M-autoclock-in.md` — explicit-witness attribution,
  session clock state, and edit-activity reclock concepts.
