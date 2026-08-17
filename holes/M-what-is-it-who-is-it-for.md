# M-what-is-it-who-is-it-for

**Status:** HEAD — operator shape captured 2026-08-17, not yet IDENTIFY.
**Gate:** operator-acceptance — HEAD must be recognised as faithful before
IDENTIFY hardens it into a gap statement.

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
