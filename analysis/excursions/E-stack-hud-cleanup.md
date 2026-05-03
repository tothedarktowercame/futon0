# Excursion: Stack HUD audit & purpose-clarification

**Date opened:** 2026-04-25
**Entry point:** M-repl-wins-over-cli Checkpoint 3 (Layer 1 evidence probe shipped, commit `8ea799a`).
**Frame correction (2026-04-25):** Joe pushed back on "wiring diagram = data-flow / dependency map." The thing that's worth drawing is **what decisions the HUD helps me make**. A block can be perfectly wired and still be useless. Boundary Gaps is the canary — present, populated, and irritating.

This excursion audits every block in the Stack HUD against the question *what decision is it supporting?* and either keeps, fixes, replaces, or drops each.

## 0a. Scope: ant's-eye view, not overview

**The Stack HUD is Joe's own ant's-eye view of what he's doing.** It is *not* an
overview of the whole futon stack — that's the War Machine
(`futon0/scripts/futon0/report/war_machine.clj` + `…war_machine_visual.clj`,
aliases `:war-machine` and `:war-machine-text` in `futon0/deps.edn`).

The two surfaces relate but should not duplicate:

| Surface       | View              | Audience      | Tempo                |
|---------------|-------------------|---------------|----------------------|
| War Machine   | overview of all   | Joe + agents  | scan / report / talk |
| Stack HUD     | ant's-eye-of-Joe  | Joe only      | continuous, ambient  |
| Per-agent HUD | ant's-eye-of-agent| each agent    | per-turn             |

The third row — per-agent HUDs — has existed in fragments (AIF signals, mana,
own EFE candidates etc.). It is **deferred to a follow-up excursion**; this
one rationalises Joe's HUD first, then the same framing can be projected onto
agent surfaces later.

**Phase ordering for this excursion:**
1. Phase 1 — Audit + remediate Joe's HUD against the decision-categories in §1.
2. Phase 2 — Project the surviving framing onto the agent-side ant's-eye view
   (this is where M-aif-head territory re-enters; see §0b).

## 0b. Three-level check: HUD ↔ War Machine ↔ M-aif-head

Each block, after Phase-1 audit, gets a three-level cross-check:

1. **Joe-HUD level** (this excursion): does the block answer a §1 decision for Joe?
2. **War Machine level** (cross-reference): is the same information already in
   the overview? If yes, the HUD doesn't need to repeat it — but maybe needs
   the *recent-delta* of it.
3. **M-aif-head level** (third-level check): would the same shape, projected
   onto a single agent's session, be useful for that agent's own observation
   channels (per the M-aif-head observation-channel concept,
   `futon3c/src/futon3c/aif/observe.clj`)? If yes, the block is a candidate for
   reuse in the per-agent HUD that M-aif-head's deferred Phase 2 was supposed
   to deliver. (Recall: M-aif-head's C8 — evidence landscape queryable — is
   still partially unwired; per_aif/observe channels read an `:evidence-counts`
   map nobody fills. A per-block reusable shape solves part of that problem.)

This three-level check is *bookkeeping*, not a gate — but it surfaces which
blocks are general infrastructure vs. which are Joe-specific. It also keeps
M-aif-head's open territory in view as we work, so we don't accidentally build
a Joe-only HUD that has to be re-architected when agent HUDs come back online.

## 0c. Why this isn't a dependency diagram

Knowing that the `vitality` block reads from
`/home/joe/code/futon3/resources/vitality/latest_scan.json`, which is populated by
`futon0/scripts/futon0/vitality/scanner.bb`, which probes
`http://localhost:7071/api/alpha/evidence` on futon1a, is true and useful but
it's not what makes the block worth having. What makes it worth having is whether
*seeing it changes what I do next*. If a block doesn't change my behaviour, the
fact that its plumbing works is no defence.

So the audit columns are:

| Block | Decision it serves | Currently serves it | Verdict |

…not:

| Block | Reads from | Writes to | Refresh interval |

The dependency map can live as an appendix — useful when *fixing* a block — but
it's not the steering document.

## 1. What decisions should the HUD support?

Draft list, to be edited with Joe before the per-block audit. Each is a question
*Joe asks himself in the morning / between sessions / during a stuck cycle* — if
the HUD answers it at a glance, the block earns its place.

1. **"Did the basics happen since I last looked?"**
   *Sanity check that the stack hasn't quietly stopped working — evidence
   accumulating, sessions running, services up, git activity nonzero.*

2. **"Where is recent work landing?"**
   *Which futons saw activity, which sessions are alive, what got committed.
   Helps decide where to dive in.*

3. **"What patterns are firing?"**
   *Top-N most-activated patterns from futon3a semantic search. Tells me which
   parts of the structural-law inventory the actual conversation is touching —
   surfaces emergent themes.*

4. **"Which surface is silent that shouldn't be?"**
   *The alarm dimension. e.g. evidence delta=0 while turns happened; futon
   active 7+ days ago; tatami log gap warning fires. Reazon-checked.*

5. **"What's queued for me?"**
   *Reminders, blockers, mission gates that need attention. The "what to do
   next" prompt.*

6. **"Is something rotting?"**
   *Stale data, dead links, sections that haven't moved in N weeks. The HUD
   should spot itself becoming unreliable.*

Open question for Joe:
- Is this list the right shape? Missing categories?
- Are categories 1+4+6 really the same question (does the system look healthy?)?
- Should "what was I working on" (cross-session continuity) be a category?

## 2. Per-block audit (first pass, 2026-04-25)

Recon was quick — read the render functions and the War Machine's strategic
synthesis report (`futon0/scripts/futon0/report/war_machine.clj`). Findings
below; each row is a draft verdict to push back on.

### War Machine cross-reference summary

War Machine produces 5 strategic synthesis sections: Loop Health (6-arrow
evidence completeness), Holistic Argument (claims + support), Mission Triage
(active/blocked counts by repo), Portfolio (workstream breakdown), Strategic
Judgement (mode inference + free energy). It works at the *strategic* tempo —
scan, report, talk. **It deliberately does not show: individual evidence
entries, session details, agent metadata, services, file activity, voice
state, hot-reload state.** That part of the surface is the HUD's responsibility.
There is almost no real overlap. Where there is potential overlap (mission
counts in War Machine vs. futon-liveness in HUD), the HUD's job is the
*recent-delta* and the *operational status*, not the strategic count.

### Block-by-block

| Block | Currently shows (file:line) | §1 dec | WM overlap | aif-head reuse | Quality | Verdict |
|-------|------------------------------|--------|------------|-----------------|---------|---------|
| liveness | List of f0..f7 with buckets ("alive", "working", etc), reads `:futon-activity` from vitality JSON (`:1835`) | 1, 2 | minimal — WM has mission-triage by repo, not file activity | partial — agent-side could show "futons I touched" | works, but coarse | keep, possibly trim |
| services | Local services up/down + ports + red flags for DOWN; remote stack URL when set (`:1665`) | 1 | none | yes — agents care about whether their backend is up | works, useful | keep |
| hot-reload | Hot reload watcher state + watched-file count + hours-since-reload + toggle button (`:1526`) | 1 (weak) | none | no | works, but low signal | downgrade — fold into a "dev runtime" mini-line, or drop |
| voice | Voice typing on/off + PID + toggle (`:1548`) | none I can name | none | no | binary, low signal | drop — modeline candidate, not HUD-block-worthy |
| musn | (disabled, code dead) — MUSN/futon3a affect + activity (`:1566`) | — | — | — | retired 2026-03-08 (per `:211` comment) | DROP entirely |
| affect | (disabled) — top 5 affect transitions, MUSN-gated (`:1623`) | 3 (if alive) | partial — WM mode-inference touches mode, not transitions | strong — affect is per-agent, retain shape | dead here | drop block; retain shape spec for per-agent HUD (this is the "third-level check" hit) |
| pattern-sync | (disabled) — futon1 reachability + entity/relation diff + invariant verify (`:1761`) | 1, 6 | none | no | references futon1 (the OLD service, retired) | DROP — futon1 is gone, replaced by futon1a; verify pattern semantics elsewhere |
| focus | Anchors / neighbors / topics / 3 most recent history entries from `:focus`/`:profile`/`:history` keys (`:1323`) | 2, 5 | none — WM doesn't do focus | maybe — agent focus exists conceptually | depends on whether `:focus` is being populated; check if vitality JSON still carries it | verify before keeping; if data is dead, drop |
| vitality | File counts per filesystem (zoomr4, tatami, ...), import totals, top-children, tatami last-event with gap warning (`:1346`) | 1, 2, 6 | minimal | no — file activity is Joe-specific | works | keep, possibly tighten layout |
| evidence | delta + latest_at + probe_ms + url (just landed, `:1893`) | 1; partial 4 (no alarm yet) | partial — WM has loop-health evidence-per-arrow, very different shape | strong — same probe shape projects directly onto per-agent | newborn | KEEP and EXTEND with sub-blocks (see §3 candidates) |
| git | Dominant commit sphere, streaks, quiet days, last commit, total count (`:1400`) | 1, 2 | none | yes — agents commit too | works | keep |
| boundary | Per-futon block of "missing evidence" titles + button to open devmap, lab counts (`:1447`); 2-hour stale threshold, `boundary.edn` + `futon0.boundary/scan-all` | tries 6, but stale reports become noise | partial — WM mission triage covers some "what's missing" semantics | no — Joe-only audit | **rotting (Joe's named annoyance)**: missing-evidence list grows when offline writes happen; 2-hour stale threshold means stale holes show for hours | **DROP** — replace its function with a War Machine cross-link or with "open mission gates" from the briefing source |
| reminders | Top 5 reminders + label + time-distance category + hours (`:1504`) | 5 | none | yes — agent-side reminders | depends on reminder service freshness | keep if backend alive |
| briefing | Active mission briefing markdown (~/.claude/stack-hud-briefing.sh output, 8h cache) (`:2376`) | 5 | partial — WM mission-triage overlaps but isn't a curated narrative | yes (per-agent briefing) | depends on script + cache freshness | keep, but health-check the script + show last-generated time inline |

### Verdict tally

- **Drop**: musn, pattern-sync, voice, boundary (4 of 14)
- **Drop here, keep shape for per-agent**: affect (1)
- **Verify before keeping**: focus (1)
- **Downgrade / collapse**: hot-reload (1)
- **Keep as-is, possibly trim**: liveness, vitality, git, services, reminders, briefing (6)
- **Keep + extend**: evidence (1)

If all drops land, the HUD shrinks from 14 to 9 active blocks. A real audit win
on signal-to-noise.

For each, read the render function (file:line in stack-hud.el) and document:

- What it actually shows (not what its name suggests).
- Which decision in §1 it tries to serve.
- Is it serving that decision well? (data fresh / signal clear / actionable)
- Verdict + rationale.

## 3. Candidate new blocks

Three candidates surfaced from Joe's remarks plus this audit. Each is sized
roughly so we can sequence the remediation.

### 3.1 evidence-per-session

Decompose the existing evidence-block delta by `session-id`. Replaces:
```
+5 since 11:17:14 | latest 11:24:54 (probe 22ms)
```
with something like:
```
Evidence per session (since 11:17:14):
  claude-5  +3   ← active here
  codex-3   +2   ← codex-cli scan
  claude-12 +0   ← idle 5d
```

**Decision served:** §1 question 1 (basics) AND 2 (where is work landing).
**Data shape:** scanner.bb adds a `:by_session` key built from
`(group-by :evidence/session-id)` over the since-window query results. Cheap.
**Per-agent reuse:** trivial — each agent's HUD shows its own row only.
**Cost:** ~30 lines in scanner.bb, ~30 lines in stack-hud.el render.

### 3.2 evidence-per-pattern (Joe-named "most useful")

Top-N most-activated patterns since last scan. Mining lives in the body of
`:coordination`/context-retrieval evidence entries (the `"results"` array
written by `dev/futon3c/dev.clj:emit-context-evidence!`).

```
Top patterns (since 11:17:14):
  fulab/changelog-trail       3  ★★
  f1/p4 XTDB Operations       2  ★
  f4/p7 XTDB Integration      1
```

**Decision served:** §1 question 3 (patterns firing).
**Data shape:** scanner.bb counts pattern IDs across the
`results` arrays in the since-window's context-retrieval entries. Sort
descending, top-N (e.g. top 5).
**Per-agent reuse:** strong — agent-side shows top patterns *for that agent's
session-id*. This is exactly the per-agent observation channel that
M-aif-head's H-3 spec (`futon3c/src/futon3c/aif/observe.clj`) gestures at.
**Cost:** ~50 lines in scanner.bb (pattern counter + sort), ~30 lines in
stack-hud.el render. **High ROI.**

**Open question:** the patterns are stored as a body field, not as
first-class annotations linked by `:evidence/in-reply-to`. If we go through
with E-evidence-viewer-deep-dive's α (per-pattern emission), this block becomes
even cheaper because the data shape becomes natural to query.

### 3.3 evidence-per-token

Joe-suggested: entries per N output tokens as a rough density proxy.

**Open question:** do we have token counts in evidence at all? Quick check:
the `:emit-invoke-evidence!` body shape doesn't include token counts. Need
to confirm whether the underlying invoke layer captures them and could
attach them.

**Verdict:** **defer** until the data exists. Not blocking 3.1 or 3.2.

### 3.4 alarm aggregator

Single ribbon at the top of the HUD that consolidates §1 question 4
(silent-when-it-shouldn't-be alarms). Reads from a list of registered
reazon-checks (the framework already exists in
`futon3c/emacs/agent-chat-invariants.el:171-199`). Only renders if there are
alarms; otherwise invisible.

**Decision served:** §1 question 4.
**Cost:** Layer 2 reazon work proper — slated for after the audit lands so
we know which surviving blocks should register checks.
**Per-agent reuse:** strong — agents care a lot about what they're missing.

### 3.5 (recommendation) what-am-I-in-the-middle-of

A combined "Joe's local context" block that absorbs focus, recent-git, and
recent-evidence into one compact "what was I just doing" snapshot. The
hypothesis: in practice these three blocks are all answering the same
question — §1 question 2 (where is work landing) — and a unified block
reads better than three siblings. Hold this candidate until Phase-1 drops
land; revisit if the trimmed HUD feels too fragmented.

## 4. Cross-futon dependency map (appendix, deprioritised)

Per Joe's clarification, this is not the wiring diagram he meant — but it is
useful when *fixing* blocks. To be drawn after §1–§3.

Skeleton:
- futon0 → futon1a (HTTP / XTDB queries)
- futon0 → futon3 (vitality JSON consumed by hud)
- futon0 → futon3a (pattern-search results, likely indirect via evidence)
- futon0 → futon3c (REPL session state, blackboard)
- futon0 → tatami / git / filesystem (direct fs reads)

## 6. Port manifest — stack-hud-1 → stack-hud-2 (2026-04-25)

The new HUD (`futon0/contrib/stack-hud-2.el`, frame-based, posframe
docs, ⁂-toggled) currently renders four widgets out of
`futon0/contrib/stack-hud-widgets.el`.  This section is the work-list
for porting remaining material from stack-hud-1, ranked by priority.

### A. Already in stack-hud-2

| Widget                | Replaces (in -1)         | Status            |
|-----------------------|--------------------------|-------------------|
| evidence-per-pattern  | (new — never in -1)      | live              |
| evidence-per-session  | (new — never in -1)      | live              |
| sessions-in-flight    | (new — pulls Arxana cache) | live (live probe per session) |
| briefing-summary      | partial replacement for `briefing` | live (no LLM) |

### B. Port candidates, ranked by priority

| Pri | Widget       | Decision (§1) | Cost  | Notes |
|-----|--------------|---------------|-------|-------|
| 1   | services     | 1             | S     | "Is the system up" — IRC, Drawbridge, Agency port checks. Pure status, no LLM. Easy port. |
| 2   | git          | 1, 2          | S     | Sphere / streak / quiet days / last commit. Reads `futon3/data/vitality/git_summary.edn`. Pure render port. |
| 3   | voice        | (ergonomics)  | M     | ON/OFF + toggle button.  Joe-named.  **Requires button support in stack-hud-2 buffer** — see §7 architectural items. |
| 4   | hot-reload   | (ergonomics)  | M     | Same shape as voice (status + toggle).  Same button-support dependency. |
| 5   | vitality     | 1, 2, 6       | M     | File-count / imports / tatami last-event with gap-warning.  Reads `latest_scan.json` (already loaded for evidence probe).  Layout work to keep terse. |
| 6   | liveness     | 1, 2          | S     | f0..f7 with buckets.  Reads same `latest_scan.json`'s `:futon-activity`.  Trivial port. |
| 7   | reminders    | 5             | M     | Top-5 with time-distance category.  Depends on the futon5a reminder-tracker being alive in your runtime. |
| 8   | briefing (full) | 5          | L     | The 8h LLM-generated mission briefing.  Complementary to briefing-summary (which is synthesised, no-LLM).  Could be a separate command `M-x stack-hud-2-show-full-briefing` rather than a permanent block, since it's narrative-not-glance. |

Cost legend: S = ~30 min, M = ~1–2h, L = half-day.

### C. Verify-before-decide

| Widget | Decision  | Open question |
|--------|-----------|----------------|
| focus  | 2, 5      | Is `:focus`/`:profile`/`:history` actually being populated in vitality JSON? If empty in your live data, drop. If populated, port. |

### D. Drop (do not port)

| Widget        | Reason for drop |
|---------------|-----------------|
| boundary      | Joe-named annoyance: 2-hour stale data drowns live alarms; serves no §1 decision.  Replace with War Machine cross-link. |
| musn          | MUSN was retired 2026-03-08 (per `stack-hud.el:211` comment).  Service no longer exists. |
| pattern-sync  | Targeted futon1 (the original, retired in favour of futon1a).  Semantics live in futon1a now and don't need a HUD probe. |

### E. Defer (per-agent HUD, separate excursion)

| Widget        | Why deferred |
|---------------|--------------|
| affect        | Affect transitions are inherently per-agent.  Retain shape spec for the deferred per-agent ant's-eye view (M-aif-head territory).  Not Joe's-HUD material. |

### F. New widgets (not in either old or new HUD yet)

| Widget                  | Decision | Notes |
|-------------------------|----------|-------|
| alarm-aggregator        | 4        | Layer 2 reazon checks, rendered as a single ribbon at the top.  Slots in once block shapes are stable.  Plumbing exists in `futon3c/emacs/agent-chat-invariants.el:171-199`. |
| evidence-per-token      | 3        | Joe-named; deferred until token counts are wired into evidence body. |
| war-machine-link        | 2, 5     | One-line block: "War Machine last run … `make war-machine`."  Replaces boundary. |
| (open) what-am-I-in-the-middle-of | 2 | Combined block (focus + recent-git + recent-evidence).  Deferred per Joe's response to the original A/B test (rejected as speculative). |

### Architectural items needed for some ports (§7)

- **Button support in stack-hud-2 buffer.** Voice and hot-reload have toggle buttons.  Easy with `insert-text-button`; a `stack-hud-2-mode-map` binding for `RET`/mouse-1 to invoke the toggle.  Required for B-3 and B-4.
- **Per-block error rendering.** Currently a render error in one widget shows as `⚠ <id> render error: <err>` — adequate.  Once we have more blocks, we may want a one-shot summary at the top.
- **Clickable widget headlines that pop the doc.** `?` already does this; future polish: mouse-1 on a headline pops the doc.

### Sequence proposal for porting

Sprint 1 (do now if Joe's keen): B-1 (services), B-2 (git), B-6 (liveness), C (verify focus), D (drop dead).  All small/medium, no architectural prerequisite.

Sprint 2: B-3 (voice), B-4 (hot-reload).  Requires button support (§7 first item).  Cluster these together.

Sprint 3: B-5 (vitality), B-7 (reminders), F-3 (war-machine-link replacing boundary).

Sprint 4: B-8 (full briefing as a separate command), F-1 (alarm aggregator with reazon).

## 5. Remediation plan (proposed sequence)

Five phases, sized for individual commits. Each ties back to §1.

**Phase A — drop the dead** (cheap, high-signal-recovery):
- Drop `musn`, `pattern-sync`, `voice` from defcustom + delete render fns + delete dispatch entries.
- Drop `boundary` (Joe-named annoyance), unless someone defends it. Replace
  with a War Machine cross-link on the HUD (one-line: "War Machine summary
  available — `make war-machine`").
- Affects §1 questions 1 and 6 positively (less noise, less chance of stale
  bad data masking a real problem).
- ~50–80 lines deleted across 4 render fns and the defcustom + dispatch tables.

**Phase B — verify and trim** (low-risk audit):
- Verify `focus` data is being populated; if not, drop. If yes, document what
  feeds it.
- Downgrade `hot-reload` and `voice` (pre-drop) to a single-line "dev runtime"
  mini-block, or drop entirely.
- No new functionality.

**Phase C — extend evidence** (highest signal gain):
- Add `evidence-per-session` to the existing evidence block (3.1).
- Add `evidence-per-pattern` as a new block under evidence (3.2). Joe's
  named "most useful" candidate.
- This earns the keep-and-extend verdict on the evidence block and starts
  paying off the M-aif-head third-level reuse: identical shape projects onto
  per-agent HUDs.

**Phase D — alarms and rotting-watch** (Layer 2):
- Add the alarm aggregator (3.4) using the existing reazon plumbing in
  `futon3c/emacs/agent-chat-invariants.el`.
- Register one reazon check: `evidence-monotonic-since-scan` (delta=0 in
  active turn window → alarm).
- This delivers the original M-repl-wins-over-cli motivation: "I'd have
  caught that evidence wasn't accumulating."

**Phase E — per-agent project (deferred to a separate excursion)**
Once Phases A–D are done, the surviving framing — categories §1.1–6, the
evidence-block shapes, the alarm registry — gets projected onto per-agent
ant's-eye views. This re-enters M-aif-head territory (its observation
channels, default-mode tier, AIF signals). New excursion will spec.

### Sequence rationale

A → B → C → D → E.

A first, because the cheapest signal-to-noise win is dropping rotten
sections; nothing downstream is blocked on it. C before D, because alarm
relations need stable data shapes to relate over. D before E, because the
per-agent surface inherits whatever the cleaned-up HUD-layer has stabilised.

### Open verdicts that need Joe's input

1. **boundary** — drop confirmed? Or is there a residual use you want kept,
   in which case what's the actionable signal it should produce?
2. **focus / history** — do you actually use these, or are they cruft? Quick
   yes/no determines whether to verify-and-keep or drop.
3. **briefing** — keep at top? It's the one block that's curated narrative
   rather than scan output; placement matters.
4. **hot-reload** — does the watcher state inform any decision, or is it
   ergonomics only?
5. **the "what am I in the middle of" combiner (3.5)** — interesting or
   over-engineering at this stage?

Phase A can start without those answers; Phase B blocks on (2) and (4); Phase
C is independent; D blocks on (1) (drop-vs-replace boundary affects what
alarms get registered).

---

## Process notes

- **Frame first, then audit.** §1 must be agreed before §2 starts, otherwise
  we audit against an implicit framing and reproduce the present mess.
- **Drop is a valid verdict.** Boundary Gaps is the canary — it ought to go
  unless we can articulate what decision it serves. Same scrutiny for
  voice / hot-reload / focus.
- **Layer 1.5 alarm glyph (deferred from earlier this session).** Not landed.
  Comes back in the per-block remediation if we keep evidence as a top-level
  block.
- **Layer 2 reazon alarms.** Still to come; they slot under §1 question 4 and
  ride on whatever data the surviving blocks expose.
