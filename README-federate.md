# Federating FUTON memories

This document complements [`README-deploy.md`](README-deploy.md). Deployment
moves and activates code. Federation moves, exposes, or reconciles knowledge
created by that code. A working memory federation does not make code deployment
unnecessary, and identical code on two boxes does not make their memories
coherent.

The first concrete requirement is:

> Memories created by Amsterdam agents on Zone must survive Zone and become
> queryable from the operator’s next machine.

The design uses site names rather than hostnames. Today `ams` is Zone, `lon` is
London, and `chi` is Chicago; the operator’s laptop is a replaceable client and
may later become another named site.

This is a design and implementation order. It does not claim that memory
federation exists today.

---

## 1. What is and is not federated now

Agency federation is a **control plane**. It mirrors site-qualified agent
identities and routes invocations between Agency nodes. It does not replicate
the futon1b graph, evidence, memory projections, or search indexes.

The current memory path has several layers:

| layer | current role | federation consequence |
|---|---|---|
| evidence entries | append-only episodes in futon1b | suitable source history, but needs an incremental feed |
| `:memory/assert` hyperedges | bitemporal current projection, updated by review/challenge/supersession/retraction | mutations must be ordered and conflict-governed |
| futon1b memory projection | process-local materialized index, rebuilt at boot and refreshed on writes | must be rebuilt or refreshed at every serving replica |
| futon1b FTS5 sidecar | derived evidence text index | copying primary data alone does not make search ready |
| futon3a notions index | lexical source used by `psr_search` | separate deploy/index lifecycle; not contained in XTDB |

The existing `futon1b/migration/` pipeline performs full export, transform,
ingest, and verification for a migration. It is valuable backfill machinery,
but it is not continuous replication: it has no per-origin change cursor,
durable acknowledgement, conflict policy, or tombstone retention contract.

---

## 2. Federation invariants

These invariants apply whether transport is HTTP, Kafka, files, or a shared
database service.

### F-0 — Stable global identity

Every durable memory and every change event has one globally stable identity.
Transport must never mint a new ID for an existing object. Current UUID-based
evidence IDs can remain intact, but new records must also carry explicit origin
metadata:

```clojure
{:origin/site "ams"
 :origin/epoch 1
 :origin/sequence 4821
 :origin/agent "ams-zai-1"}
```

Site-qualified agent names are canonical in durable records. A bare `zai-1`
is an observer-relative alias and becomes ambiguous after leaving Amsterdam.
Legacy records with bare authors retain their bytes and gain migration
provenance; silently rewriting historical authorship is not acceptable.

### F-1 — History first, projection second

Evidence, challenge, correction, and retraction episodes are immutable facts.
The current `:memory/assert` view is a materialized projection of those facts.
A replica must be able to rebuild the same projection from the replicated
history plus a declared projection version.

This exposes a current seam: `memory_record` writes its evidence entry and its
derived hyperedge in two operations, and may return success with `:hx-error`
when only the evidence lands. Federation must not hide that partial state.
Either the write path becomes transactional, or reconciliation must
deterministically regenerate the hyperedge (`hx-mem-<evidence UUID>`) and keep
the release incomplete until the two representations agree.

### F-2 — One home writer per logical memory

The site that creates a memory is its home and serializes changes to its
projection. Other sites may mirror it and may submit challenges or proposed
corrections, but they do not independently overwrite the same projection.

A remote correction is therefore either:

- routed to the home site and recorded there; or
- recorded as a new, globally identified correcting episode whose relationship
  to the original is replicated back to the home site.

If a home site is permanently lost, authority moves only through an explicit
re-home event with a higher origin epoch and an audit record. “Whichever copy
is reachable becomes primary” creates split brain and is forbidden.

### F-3 — Ordered, resumable replication

Each origin exposes a monotonically ordered feed. A cursor is at least
`[origin-site origin-epoch origin-sequence]`; wall-clock time is descriptive,
not the cursor. Clock skew and equal timestamps make “everything since 14:00”
insufficient.

A receiver durably records the highest contiguous applied sequence per origin.
It acknowledges sequence `n` only after the primary document, projection
update, and replication receipt are durable. A gap blocks advancement and is
reported; later events cannot make the gap disappear.

### F-4 — Replay is idempotent and mismatch is loud

Applying an event twice has the same result as applying it once. For each event
ID, replicas retain a canonical payload hash:

- same ID and same hash: already applied;
- same ID and different hash: integrity/conflict failure; stop that origin;
- new ID with next sequence: apply;
- new ID after a sequence gap: retain or reject, but do not acknowledge past
  the gap.

Deletes and bitemporal retractions are explicit replicated events, not the
absence of a document in a snapshot.

### F-5 — Index readiness is part of federation

Every query response states the primary-data cursor and derived-index revision
from which it was produced. A replica is not “caught up” merely because it has
accepted the latest event. Its memory projection, FTS sidecar, and any notions
index required for that query must also be ready at compatible watermarks.

No search path silently falls back from a missing index to an incomplete empty
answer. It returns an explicit `not-ready`, `partial`, or `stale-through`
status.

### F-6 — Provenance survives query merging

Every returned memory includes its canonical memory ID, home site, author,
recorded time, valid time, state, and the site/index revision that answered the
query. The receiving site must not make a remote memory look locally authored.

### F-7 — Replication is not backup

Replication copies good writes, accidental writes, and some forms of
corruption. Each authoritative site still needs independent, point-in-time
recovery. Zone retains its source history until at least one durable replica
has acknowledged it and a separate recovery point exists.

---

## 3. Candidate topologies

### A. Periodic export and verified pull

Zone produces a closed snapshot; the operator pulls it, verifies counts and
sampled hashes, and ingests it into a separate recovery or import database.

**Use:** immediate insurance and initial backfill.

**Limits:** expensive, not current, awkward to merge repeatedly, and unable to
represent deletions safely without a snapshot-generation contract. Copying a
live XTDB local store directory is not a merge protocol and must not be used as
one.

The current migration pipeline is closest to this option. It should be adapted
to futon1b’s bounded cursor APIs and made to fail closed on any unknown or
failed population before it is used for Zone recovery.

### B. Application-level event replication — recommended first system

Each site owns an append-only outbox of memory/evidence changes. Peers pull
ordered batches, verify them, and apply them idempotently to local futon1b
tables. Derived indexes refresh from the applied writes.

Advantages:

- works with today’s local XTDB stores;
- preserves offline operation and lets a laptop disappear temporarily;
- makes authority and conflict semantics explicit;
- can replicate only memory-relevant populations before attempting the entire
  graph;
- leaves an inspectable protocol independent of the database implementation.

The outbox event should contain:

```clojure
{:federation/event-id "ams:1:4821"
 :federation/origin "ams"
 :federation/epoch 1
 :federation/sequence 4821
 :federation/schema 1
 :federation/op :put
 :federation/table :evidence
 :federation/document-id "e-..."
 :federation/valid-time "..."
 :federation/system-time "..."
 :federation/payload {...}
 :federation/payload-sha256 "..."
 :federation/caused-by nil}
```

The outbox write must be atomic with the source mutation it describes, or be
derived from an authoritative transaction log with a reconciler that proves no
source mutation lacks an event. A best-effort “announce after write” callback
has the same silent-loss shape as the deploy failures and is not sufficient.

### C. Federated query fan-out — useful complement, poor sole recovery path

A coordinator sends a read-only query to selected sites and merges their
responses. This exposes a new Zone memory immediately without copying its full
record locally, and it permits site-local policy over sensitive data.

It does not get the memory **back** in the durability sense: if Zone is offline,
the answer disappears. It also creates latency, partial-result, and ranking
problems. Use it alongside replication, not instead of replication.

A federated query response must include:

```clojure
{:query/id "..."
 :sites/requested ["ams" "lon" "chi"]
 :sites/answered ["ams" "lon"]
 :sites/missing [{:site "chi" :reason :timeout}]
 :partial? true
 :results [...]
 :watermarks {"ams" {...} "lon" {...}}}
```

Raw relevance scores from different index versions are not necessarily
comparable. Merge by deterministic rank fusion, such as reciprocal-rank
fusion, and preserve each site’s original rank and score. Deduplicate by global
memory ID, never by similar text.

### D. One multi-node XTDB database — evaluate later

XTDB supports multi-node deployments when nodes share a remote transaction log
and object storage. The official documentation describes Kafka as the
multi-node log and local-disk logs as single-node; nodes sharing one database
form a cluster over the common log and storage:

- [XTDB transaction logs](https://docs.xtdb.com/ops/config/log.html)
- [XTDB Kafka log](https://docs.xtdb.com/ops/config/log/kafka)
- [Databases in XTDB](https://docs.xtdb.com/about/dbs-in-xtdb.html)

That can provide several serving nodes for **one logical database**, but it is
not independent multi-master site federation. It introduces shared Kafka,
remote object storage, credentials, WAN availability, upgrade coordination,
and a common failure domain. It also does not deploy futon3c code or its
process-local and futon3a indexes.

Futon1b currently pins XTDB **2.1.0**. The current upstream documentation also
describes 2.2’s source/replica-log and single-writer changes. Any cluster spike
must therefore pin one XTDB version, read that version’s configuration and
upgrade notes, and run compatibility/recovery tests; the linked examples are
architecture evidence, not commands to paste into today’s service.

Treat this as a later infrastructure choice after the identity, writer,
projection, and query contracts above are proven. Those contracts remain
necessary even if XTDB supplies the underlying replication.

---

## 4. Recommended topology

Start with **site-owned writes plus pull replication**:

```text
                    read/query federation
             ┌────────────────────────────────┐
             │                                │
         ┌───▼────┐    ordered pull      ┌────▼─────┐
         │  ams   │ ───────────────────► │ operator │
         │ Zone   │   memory events      │ replica  │
         └───┬────┘                      └────┬─────┘
             │                               │
             └──────── recovery copy ────────┘

       home writer                    local search/indexes
```

As more durable sites participate, each receiver maintains one cursor per
origin. A hub may relay events for reachability, but the event retains its
original `origin/site`, epoch, and sequence. Relaying must not create a second
identity or a new ordering domain.

For the first slice, replicate only the closed memory subgraph:

- memory evidence entries;
- `:memory/assert` hyperedges;
- challenge, review, supersession, and retraction evidence;
- entities/endpoints required to interpret those memories;
- explicit references to patterns, missions, sessions, and agents.

Do not fabricate missing referenced nodes. The batch manifest lists unresolved
references, and the receiver reports them as incomplete until a later event or
backfill supplies them.

---

## 5. Protocol sketch

### 5.1 Origin endpoints

The exact paths are not implemented. A narrow first API could be:

```text
GET  /api/alpha/federation/memory/manifest
GET  /api/alpha/federation/memory/events?epoch=1&after=4820&limit=500
POST /api/alpha/federation/memory/ack
GET  /api/alpha/federation/memory/object/<global-id>
POST /api/alpha/federation/memory/query
```

`manifest` reports site, epoch, earliest retained sequence, latest durable
sequence, schema versions, population counts, projection revision, and index
watermarks. If a receiver asks for a sequence older than retention, the origin
returns `snapshot-required`; it never pretends an incomplete tail is complete.

### 5.2 Receiver state

Persist outside the code release tree:

```clojure
{:origin "ams"
 :epoch 1
 :last-contiguous-applied 4821
 :last-event-hash "..."
 :projection-through 4821
 :fts-through 4821
 :last-verified-at #inst "..."}
```

Inbox batches and receipts are durable before acknowledgment. A process crash
between applying an event and advancing the cursor is harmless because replay
is idempotent.

### 5.3 Backfill then tail

1. Ask Zone for a closed manifest at origin sequence `N`.
2. Export the memory subgraph at that boundary.
3. Transfer the snapshot and verify complete counts plus deterministic hashes.
4. Ingest into an empty replica namespace/database; do not merge directly into
   an uncharacterized existing store.
5. Rebuild memory projection and text indexes and run query oracles.
6. Begin the incremental feed at `N + 1`.
7. Declare the replica ready only after primary, projection, and index cursors
   all reach the advertised origin sequence.

### 5.4 Lifecycle writes

Writes received at a non-home site follow the memory’s `origin/site`:

- if home is reachable, forward with a globally unique command ID and return
  the home receipt;
- if home is unavailable, durably queue the proposal without changing the
  mirrored projection;
- if the action is a locally authored challenge, record it as a new local
  episode and replicate that episode to the memory’s home for projection;
- never resolve concurrent states with last-wall-clock-write-wins.

The home site deduplicates command IDs. Bitemporal valid time remains semantic;
origin sequence supplies replication order.

---

## 6. Security and policy

Memory federation crosses a stronger boundary than agent-roster federation.
Peers need authenticated identities and least-privilege capabilities:

- read manifest/feed;
- acknowledge a named origin;
- submit a lifecycle proposal;
- run federated search over permitted domains;
- administer epochs or retention.

Do not expose XTDB, Drawbridge, or an unrestricted graph API publicly. Use a
private network, mutually authenticated TLS, or SSH transport initially, with
separate credentials per site. A compromised read peer must not gain write or
re-home authority.

The feed enforces domain and sensitivity policy before export. Redaction is a
new derived record with provenance, not mutation of the original payload. Batch
manifests record included and excluded policy classes so a receiver knows the
difference between “no result” and “not replicated here”.

---

## 7. Verification

Federation is accepted per origin only when these tests pass:

- [ ] global IDs and canonical site-qualified authors survive byte-for-byte
- [ ] duplicate delivery produces no additional documents or projection changes
- [ ] same event ID with altered payload stops replication loudly
- [ ] a missing sequence prevents cursor advancement
- [ ] crash after apply/before acknowledgment replays safely
- [ ] challenge, supersession, and retraction converge to the same bitemporal view
- [ ] unresolved references are reported, never fabricated or silently dropped
- [ ] `memory_search` returns a known Amsterdam memory from the replica
- [ ] the same memory is absent when queried before its valid time or after retraction
- [ ] FTS and memory-projection readiness match the replicated cursor
- [ ] a three-site query labels a timed-out site and returns `partial? true`
- [ ] duplicate results from a relay are merged by global ID with origin preserved
- [ ] Zone can be offline after acknowledgment and the replicated memory remains retrievable
- [ ] restoration from backup plus event replay reaches the same manifest hash

The most important oracle is semantic, not numerical: write a distinctive
memory on `ams`, replicate it, disconnect `ams`, and retrieve that exact memory
through the operator replica’s normal `memory_search` path.

---

## 8. Relationship to deployment

Federation protocol and schema versions belong in the deployment manifest from
`README-deploy.md`. A deployment that changes memory shape, lifecycle rules,
index semantics, or the feed schema must prove compatibility with lagging peers
before activation.

Deployment must not advance a writer beyond the readers’ supported schema
without one of:

- a backward-compatible event form;
- a coordinated peer deployment;
- a durable down-converter whose output is verified against the same semantic
  oracle.

Likewise, successful peer replication does not prove the peer runs current
code. Each site still needs its own serving-process deployment attestation.

---

## 9. Implementation order

1. Give the operator an immediate, verified Zone memory snapshot and recovery
   copy using a fail-closed adaptation of the migration exporter.
2. Add canonical origin metadata to new memory writes without rewriting legacy
   identities.
3. Define the versioned event envelope and receiver cursor/receipt schema.
4. Make outbox coverage atomic or add a reconciler that proves every relevant
   source transaction has exactly one event.
5. Implement read-only `manifest` and `events` endpoints on Zone.
6. Implement an idempotent receiver into an empty test replica; test gaps,
   duplicates, mutation mismatch, and crash recovery.
7. Backfill at a closed sequence, tail forward, rebuild indexes, and pass the
   disconnected-Zone retrieval oracle.
8. Add lifecycle proposal routing and explicit home-site authority.
9. Add federated query fan-out with provenance, partial-result signaling, and
   deterministic rank fusion.
10. Only then compare application replication with a shared-log/shared-storage
    XTDB cluster using measured operational cost and failure behavior.

The first milestone is deliberately modest but real: an Amsterdam memory is
created on Zone, acknowledged by a second durable site, Zone is disconnected,
and the memory remains retrievable through the normal local tool surface.
