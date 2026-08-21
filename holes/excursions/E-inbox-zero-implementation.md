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

## Open decisions

- Where tool-edit/save witnesses should be emitted for Emacs, Claude Code, and
  Codex, and how all three surfaces share one event shape.
- Whether explicit file subscription should override competing edit claims or
  coexist as a differently typed claim.
- The reminder interval after acknowledgement when the exact same dirty set
  remains.
- Whether count thresholds are per seat across a worktree or per seat/repo.
  v0 specifies per seat/repo because it makes the requested action coherent.
- Which durable store owns the records. This must be chosen from replay,
  query, and delivery requirements rather than by convenience.

## References

- [`README-inbox-zero.md`](../../README-inbox-zero.md) — operational evidence,
  inbox-zero invariant, and turn-end promotion proposal.
- `futon3/scripts/multi_watcher.clj` — existing multi-repository file and
  commit observer to extend.
- `futon3c/README-park.md` — exact-seat identity and durable continuation
  delivery behavior; followups reuse the delivery substrate but are not parks.
- `futon3c/holes/missions/M-autoclock-in.md` — explicit-witness attribution,
  session clock state, and edit-activity reclock concepts.
