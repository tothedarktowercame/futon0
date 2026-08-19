# M-what-is-it-who-is-it-for

**Status:** HEAD captured 2026-08-17 · **MAP complete** 2026-08-17 (§2) ·
IDENTIFY not started · DERIVE not started.
**Gate:** operator-acceptance — HEAD must be recognised as faithful before
IDENTIFY hardens it into a gap statement. MAP was run ahead of IDENTIFY because
the landscape was being surveyed anyway (Joe: *"we've been mapping the
landscape"*); its facts are therefore available to IDENTIFY rather than
constraining it.

Per `futon4/holes/mission-lifecycle.md`: HEAD preserves the operator's voice and
carries tensions forward. **It is not design.** Nothing below prescribes a schema,
an attribute set, or an audit procedure. The finding and the tensions are the
payload.

Occasion: the Dionysus handback. A day spent moving ~570 GB into deep storage and
19 branches onto remotes made the bar visible.

---

## Operator-voice anchor

Joe, 2026-08-17:

> "just making sure we have things backed up is the lowest possible bar at this
> point. To know that the systems run is another good (but still low) bar. To know
> what they do — that would be much better. For example, maybe some of these bumps
> are not just 'features' but ... possible products that are hidden inside of repos
> that no one is ever going to look through! My recent WYSIWYG LaTeXML editor is an
> example of something really cool that would be easy to 'forget'. Other things
> like Agency, I won't forget, because I use it every day... but maybe others would
> be interested in using it too! So, in short, if we think of the manifest as the
> 'raw data' maybe what we need is not just to ensure that we transfer it, but to
> think about what it actually *is* as a data specification, and to think further
> about what associated processes relate to it."

And, on what has already been tried:

> "The 'What is it, and who is it for' isn't *entirely* a greenfield topic, but it
> is something that's been looked at in a sort of scattered way and doesn't have
> the same clear entry point as, say `~/code` or `M-x arxana-browser` — those
> things show me a clear list of their contents. The 'what is it, and who is it
> for' question has been tackled, often when I feel overwhelmed, through 'War
> Bulletins' in futon3 — in a kind of zen approach, i.e., 'what is it now?'. Those
> aggregate into the `war-room.md` which tries to provide an overall view of
> things. But I've been noticing that it kind of fails to do that very well. I've
> been accordingly hoping that I would find better ways to make sense of things...
> e.g. one idea was that the site-wide documentation VSATARCS could be developed
> into a site-wide 'forward model'... so, answering 'what is it... and who is it
> for' in a diachronic way. But so far this hasn't yet materialised clearly for me
> either. Maybe we're on our way to more clarity."

> "the move away from Brookes gives motivation to 'turn the compost heap' not just
> bury it in a USB drive"

On the unit of interest, from the attention infographic
(`python-practice/attention.html`, commissioned 2026-08-15):

> "it looks to me like there was a big wave of attention starting around June that
> touched on a bunch of repos (including futon5a) but that many have since gone
> quiet. The sparklines are kind of interesting but what would be even more useful
> would be to know what the contents of the bumps of work actually are. For
> example, futon1b in July is easy — that's when we created futon1b and further
> bumps have been refinements. But what was going on with futon3a in January?
> Well, I remember, that was the start of a futon1 rewrite... but our futon1a work
> remained pretty much abandoned until some bumps started kicking off around June...
> and I don't know what those are really. Counting the bumps, I see maybe 2 or 3
> per repo, and I'd tend to assume that those correspond to 'features' of some
> kind."

---

## Facts on the ground (verified 2026-08-17)

Four existing artifacts bear on the question. **Each is keyed on something
different, and that is the whole story:**

| artifact | keyed by | answers |
|---|---|---|
| `futon5a/scripts/piano_roll.py` (360 ln) | (repo, time) — commits as dits, mission events as dahs | **when** |
| `python-practice/attention.html` (14 KB) | repo × week → commit count (`weeks`/`ticks`/`series`/`values`) | **when**, rendered |
| `futon3/holes/war-room.md` (595 ln) | decision — `WR-1` … `WR-28`, 2026-02-09 → 2026-08-08 | **why** |
| `futon5a/holes/stack-annotations.edn`, schema `futon0/docs/stack-annotations-schema.md` (399 ln) | object + typed hyperedge | **what** |

`piano_roll.py` already performs the commits↔mission-dates join. It is not
greenfield and should not be reinvented.

The narrative raw material exists and is dated — ~700 `holes/` docs
(`M-*`/`E-*`/`TN-*`) across the stack: futon3c 301, futon2 102, futon6 58,
futon5a 51, futon3 49, futon5 42, futon7 32, futon0 28, futon4 27, futon3a 7,
futon1b 3.

---

## The finding

**1. `war-room.md` cannot answer the question, for structural reasons rather than
reasons of effort.** It is a *decision ledger*: roughly 430 of its 595 lines are
`WR-1`…`WR-28`, keyed by decision in time order. A ledger answers "why did we
decide this, and when" well. It cannot answer "what exists, and who is it for",
because that question is keyed on objects. The catalogue attempt is present —
`## Mission Portfolio`, with umbrella missions, completed-but-load-bearing
infrastructure, and Joe's three workstreams — and is ~40 of 595 lines. The
catalogue is a minority tenant in a building the ledger owns.

**2. The zen framing and the aggregation mechanism are in tension.** "What is it
*now*?" requires an answer that is **replaced**. Bulletins accumulate (15 of them)
and `war-room.md` aggregates by **appending**. The property that makes the record
durable is the same property that defeats the question asked of it.

**3. `stack-annotations.edn` is already the right *kind* of object.** Its schema is
object-keyed with typed hyperedges — `:aif/role`, `:aif/timescale`
(fast/medium/slow/glacial), `:annotation/grounds`, `:stack/cross-leaf`,
`:stack/surface-projects` (a surface such as War Machine or VSATARCS → the sections
it renders). This is the shape a catalogue needs and `war-room.md` cannot have.

**4. Two specific things stand between it and the question, both already named in
this stack's own documents.**

- **There is no audience attribute.** `:aif/role` records what a thing *does*.
  Nothing records *who it is for*. "Who is it for" is absent from the
  specification, not merely from the data.
- **`Q-SA2` excluded the relevant layer**: *"Mission docs and code/evidence layers:
  OUT OF SCOPE for v1"* (resolved 2026-05-17). That is exactly the layer where the
  bumps live. It was a sound v1 boundary that has aged into the blocker.

---

## Carried-forward tensions

**T1 — Daily use is an anti-signal for documentation risk.** Agency is safe
*because* it is used every day; the WYSIWYG LaTeXML editor is at risk *because* it
is not. The intuition "I know what this is, so it needs no write-up" selects
exactly wrongly. Any process built here must not rank by familiarity.

**T2 — "Bump" is a derived object that is currently only rendered, never
materialised.** It has a plausible definition (a contiguous run of weeks above
baseline in one repo) and a tractable cardinality (2–3 per repo, on Joe's count),
but no identity, so nothing can be said *about* one. **A bump with no narrative doc
is itself a finding**, and the join can only surface that once bumps exist as
objects.

**T3 — Entry point, not content, is what `~/code` and `M-x arxana-browser` supply.**
Both "show a clear list of their contents." Whatever answers this question has to
be enterable in that sense. Note that `war-room.md` fails this too: 40 KB with 35
lines of epigraph before the first section.

**T4 — Aggregation has already been tried and produced `war-room.md`.** A fifth
aggregate document is the move most likely to reproduce the failure. The
alternative shape — an attribute on objects that already exist, letting non-empty
rows surface themselves — is untried here.

**T5 — Product-ness is a filter, not a property to be authored.** A candidate
filter: does it have a boundary (runs without the rest of the stack), a user other
than Joe, and something demonstrable? Agency and the LaTeXML editor both pass;
most bumps would not. Whether "product" is even the right frame for a research
stack is open.

**T6 — Diachrony is what VSATARCS was hoped to add and what none of the four
artifacts has.** VSATARCS is constellational (stars + arcs + linked-to-the-stack)
and is described as "THE open-research artefact of the stack", with *coverage =
grounding*. The hoped-for development was a site-wide **forward model** answering
"what is it / who is it for" over time. It "hasn't yet materialised clearly."

**T7 — Turning compost is aeration, not relocation.** The handback pressure pushes
toward burial: a verified copy on a USB drive discharges the obligation to *keep*
without touching the obligation to *understand*. Today discharged the first
completely and the second not at all. Those are different debts and the deadline
only forces one of them.

---

## Explicitly NOT decided here

No schema change is proposed. No attribute is named. No audit is scoped or
scheduled. Joe, 2026-08-17: *"I'm not proposing that we start a deep audit now."*
Whether the answer belongs in `stack-annotations.edn`, in VSATARCS, in a new
surface, or in none of these, is an IDENTIFY question.

## Provenance

Operator text captured verbatim from the 2026-08-17 session (Dionysus handback
prep). Artifact facts verified the same day by reading
`futon5a/scripts/piano_roll.py`, `futon3/holes/war-room.md`,
`futon0/docs/stack-annotations-schema.md`, and `python-practice/attention.html`,
and by counting `holes/` docs across futon0–futon7. Cross-ref:
`futon0/holes/M-capability-levels.md` (same HEAD register, 2026-08-15),
`futon0/holes/missions/M-the-futon-stack.md`,
`futon0/holes/E-starmap-vsatarcs-regen.md`.

---

# 2. MAP

**Status:** MAP complete for the questions below, 2026-08-17. Research only —
facts, not decisions, per `futon4/holes/mission-lifecycle.md` ("Survey what
exists. Don't design yet — just look.").

Occasion: the Dionysus evidence store was moved to Zone and fully drained the
same day, giving — for the first time — a **frozen, drained store alongside a
live one on one machine**. Most of what follows is only observable in that
configuration.

## 2.1 Inventory — infrastructure

Two futon1b instances on Zone:

| | port | store | state |
|---|---|---|---|
| Zone site | 7073 / 7072 | `/home/joe/code/futon1b/migration-store-21` (27 G) | live, receiving; never drains |
| Dionysus store | 7083 / 7082 | `/home/dionysus/futon1b/migration-store-21` (23 G) | frozen at 2026-08-17T08:51:32, **drained** |

`futon1b-dionysus.service` runs as user `dionysus`, `MALLOC_ARENA_MAX=2`,
MemoryHigh/Max 48G/50G.

**FTS surface** — `GET /api/alpha/evidence/text-search`:
`?q=` (fts5 over `:evidence/body`), `?tags=` (repeatable, comma-splittable),
`?df=t1,…` (≤ `max-df-terms` = 32, index-only), `?stats=true`, `?hydrate=false`,
`?limit`, `?offset`. `POST {:op :catch-up}` is **penholder-gated and runs in
place — no restart needed** to process another store.

**Search architecture:** the index proposes, the store certifies. *"A candidate
survives only if the doc exists in the store AND still passes every requested
predicate read from the STORE's copy, not the index's."* Every response carries
`index-as-of` and `index-basis` (checkpoint + `basis-tx` + `basis-captured-at`).

**Object-layer routes, none FTS-indexed:** `/hyperedge`, `/hyperedges`,
`/entity`, `/entities`, `/entities/latest`, `/entities/batch`, `/relation`,
`/relations`, `/relations/batch`, `/graph/inhabited`, `/census`, `/types`,
`/memory/search`, `/memory/projection`, `/documents/retract`,
`/restart-readiness`.

**No XTQL or SQL surface is exposed.** pgwire binds loopback-only on an
ephemeral port (`127.0.0.1:35127`, the internal self-connection listener). The
API is a fixed set of typed routes.

## 2.2 Inventory — data

**Dionysus store, drained:** 140,296 rows; `ready true`; `tx-lag 0`;
**`recheck-rejections 0`** (index and store agree exactly);
`index-as-of 2026-08-17T08:51:32`.

**Declared residual — the index states its own blind spots:**
`channels [:calls :email :speech]` · `history [:before-basis-capture]` ·
`undeclared-fields [:evidence/in-reply-to :evidence/fork-of :evidence/id]` ·
**`undeclared-tables [:hyperedges :entities]`**.

**Projection:** `:evidence/body` → `ev_fts/body` as `:fts5-unicode61`;
`author`/`at`/`session` → btree (dual-homed `ev_fts` + `ev_attr`);
`type`/`claim-type` → btree `:with-at`;
**`:evidence/tags` → `ev_tags/tag-id` as `:junction-primary-key`.**

**Type catalogue:**

| | Zone site (7073) | Dionysus (7083) |
|---|---|---|
| types | 250 | 222 |
| shared | 222 | 222 |
| site-only | **28** | **0** |

138 entity + 85 relation types. The 28 Zone-only are **all test debris** —
`:h1`, `:h1/probe`, `:h1/probe2`, `:h1/repro`, `:idempotency`,
`:idempotency/probe`, `:probe`, `:probe/batchver`, `:probe/c`, `:probe/clean`,
`:probe/clean-newtype-1`, `:probe/n`, … — from the H1/H3 probes recorded in
`futon3c/holes/excursions/E-apm-A3-ingest-efficiency.md`.

**Evidence shape:** the 1,682 rows dated ≥ 2026-08-14 in the Dionysus store are
*all* `:evidence/type :coordination`, `:claim-type :step`, `ephemeral 0`.
Two tag families observed: `[:invoke :dev :<agent> :invoke-complete]` (turn
telemetry, body is an invoke envelope whose only prose is `result-preview`) and
`[:claude :chat :turn :user|:assistant]` (actual prose, including Joe's).

## 2.3 Survey questions, answered

**Q1 — Are the two stores the same store?** No. They are two Agency **sites**.
Evidence dated 2026-03 → 2026-08-10 sampled 12/12 present in both; 2026-08-14,
-16, -17 sampled 0/12 present. Divergence begins at Zone's own start (Aug 14
10:17). Federation carries shared evidence, not site-local runtime events.

**Q2 — What is site-local to Dionysus?** 1,682 rows ≥ 2026-08-14:
claude-6 427, claude-3 335, **joe 263**, claude-8 262, process-watchdog 219,
codex-9 33, claude-9 29, codex-1 29, mission-control/sync 23, codex-8 12,
codex-2 9. ~1,463 substantive, ~219 telemetry. All `ephemeral 0`.

**Q3 — Does the store contain descriptions of artifacts?** Partly, and only in
one tag family. `NEAR(latexml wysiwyg)` returns **0** across 140,296 rows — the
phrase "WYSIWYG LaTeXML editor" appears nowhere. Coordination evidence records
*steps*. The three documents containing both terms are `:chat :turn` entries
(2026-08-08 claude-1, 2026-08-10 **joe**, 2026-08-15 claude-6). Prose exists; it
lives in chat turns.

**Q4 — How do tags relate to the text?** They do not. Tags are a separate
exact-match facet (`ev_tags`, junction primary key) queried *beside* the fts5
body index. "Unified content/attribute search" means two indexes, one call.
Practically: `?tags=chat,turn` is the lever that separates prose from telemetry.

**Q5 — Does conjunctive FTS work?** Yes, fts5 syntax passes straight through:
`latexml AND wysiwyg` → 3 · `latexml wysiwyg` (implicit AND) → 3 ·
`latexml OR wysiwyg` → ~99 · `NEAR(latexml wysiwyg)` → 0.

**Q6 — Is `df` a usable salience signal?** Only with telemetry excluded.
Measured: `oomd` 2 · `orpm` 2 · `transportability` 14 · `voxterm` 25 ·
`latexml` 36 · `arena` 46 · `wysiwyg` 66 · `peeragogy` 280 · `anatomy` 306 ·
`zone` 642 · `vsat` 680 · `arxiv` 693 · `agency` 2,695 · `prelim` 8,100 ·
`editor` 8,964 · `futon3a` **19,296**. Two failure modes: `futon3a` at 14% of the
corpus is path noise, not salience; and of 66 `wysiwyg` hits **32 are
`process-watchdog`**, so at low df a monitor's passing mentions can be most of a
thing's footprint.

**Q7 — Can an episode be recovered from a rare term?** Yes, but only
chronologically. `wysiwyg` → all 66 hits in 2026-08-08 → 08-16: an eight-day
episode. `latexml` → bimodal, June 17 / July 1 / August 18 — two distinct
episodes sharing a tool name (June = anatomy golden-roles; August = the editor).
**BM25 ordering surfaced June and hid August; ordering by time exposed it.**

**Q8 — Where did the 223 types come from?** Minted on demand.
`register-types!` (`futon1b_graph.clj:60`) is called from the entity write path
(`:280`, `:326`) and relation write path (`:433`, `:504`), and validates exactly
`(keyword? type-id)`. The parent hierarchy is **inferred from the keyword
namespace** by `infer-parent` (`:apm/phase` → `:apm`), not authored. Vocabularies
are traceable to producers: `:would-refute`/`:attacks-claim` from futon5a's AIF
work (`holistic-argument*.edn`, `extract_holistic_argument.clj`), 12
`:pattern/*` from the pattern library, 8 `:interest/*` from the Interest Network,
`:arxana/*`, `:apm/*`.

**Q9 — Is there an audience relation?** **No.** Across 85 relation types there is
nothing for who-a-thing-is-for. The vocabulary is rich in provenance
(`:lives-in-repo`, `:covers-repo`, `:minted-from`, `:implemented-by`,
`:produces`, `:constructs`, `:defines`, `:evolved-into`), argumentation
(`:attacks-claim`, `:would-refute`, `:supported-by`, `:answered-by`,
`:responds-to`, `:generates-question`, `:discharged-by`), pattern composition (12
`:pattern/*`) and interest (8 `:interest/*` — but that models *Joe's* interests,
the mirror image of audience). Because the catalogue has no gatekeeper, this is
**not** a schema decision to exclude audience: it records that **no process in
the stack has ever produced audience information.**

## 2.4 Ready vs missing

| Ready — no new code needed | Missing — the actual work |
|---|---|
| Topic entry point over 140k rows, full fts5 boolean syntax | **Audience**: absent from 85 relation types *and* from every producing process |
| `?tags=` faceting to separate prose (`:chat :turn`) from telemetry (`:invoke`) | Object layer unsearchable — `:entities`/`:hyperedges` in the declared residual |
| `?df=` rarity signal (≤32 terms) once telemetry authors are excluded | No XTQL/SQL surface; fixed typed routes only, pgwire loopback-only |
| Episode dating from a rare term, ordered by time | **Bump** not materialised as an object — no identity, so nothing can be said *about* one |
| Type-vocabulary enumeration, and cross-site diffing of it | Vocabulary hygiene: 28 probe types, 6 entity/relation name collisions, glob types `:pattern/*` `:devmap/*` `:me/*` `:prototype/*` (all on **both** sites, so pre-Aug-14) |
| 2 of 3 product-filter criteria queryable: boundary (`:lives-in-repo`, `:produces`), demonstrable (`:demo`, `:surface`) | The third criterion — a user other than Joe — is the one that isn't |
| In-place `POST {:op :catch-up}` to process the other store, no restart | `/census` requires `?type=` or `?entity-type=`; there is no "list everything" entry point |
| Store-certified results (`recheck-rejections 0`), self-describing coverage | ~700 dated `holes/` docs exist but are **not** joined to evidence or to bumps |

## 2.5 Surprises — recorded before DERIVE

1. **Two separate gains, from two separate acts — do not conflate them.**
   An earlier draft of this section claimed "the frozen/live pair is the
   capability." That did not survive checking and is retracted. Tracing what
   each finding actually required:
   - **Copying** the store to Zone bought the *differential* queries — subset
     relationships, divergence, debris (Q1, Q2, the 28 types). These need two
     **reachable** stores and nothing more; `/api/alpha/types` reads the
     type-catalog directly from XTDB and never touches the FTS index. Neither
     frozenness nor drainedness is required.
   - **Processing** the FTS index bought *term → episode* (Q3, Q5, Q6, Q7): `df`
     statistics, conjunctive search, episode dating from a rare term.

   The practical consequence: the differential capability was available as soon
   as the bytes landed, hours before the index finished, and would be available
   for any two co-located stores regardless of index state.
2. **The type catalogue was reachable all along** — `:7073` has served `/types`
   since Aug 14. Nothing new was needed to enumerate it; only looking. The
   catalogue is thus itself an instance of this mission's subject: real,
   reachable, never enumerated, forgotten by default.
3. **Diagnostic writes are indistinguishable from real vocabulary.** 28 probe
   types persist because `register-types!` has no notion of provisional. Same
   family as the `#uuid`-string identity defect
   (`E-apm-A3-ingest-efficiency.md`): a write path validating shape but not
   identity, failing silently.
4. **The instrument is biased against its target.** Joe authored 8 of 66
   `wysiwyg` hits and 3 of 36 `latexml` hits. Dispatched work emits coordination
   evidence; work done directly at the keyboard barely registers — and that is
   exactly the category HEAD names as easiest to forget.
5. **Relevance ranking destroys the temporal structure** that bump-identification
   needs (Q7).
6. **`NEAR` = 0 is a load-bearing negative** (Q3): it is positive evidence that
   no phrase-level description of an artifact exists anywhere in the corpus.

**Exit criterion:** met. Q1–Q9 have concrete answers; the ready/missing table is
complete. **No design follows here** — DERIVE is not started, and per HEAD it
remains open whether the answer belongs in `stack-annotations.edn`, in VSATARCS,
in a new surface, or in none of them.
