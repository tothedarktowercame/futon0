# M-landscape-positioning

**Status:** OPEN · HEAD drafted 2026-10-10 · MAP not started (a first-pass
inventory from 2026-10-08 is carried below as *input*, not as MAP).

Per `futon4/holes/mission-lifecycle.md`: HEAD preserves the operator's voice and
carries tensions forward. **It is not design.** Nothing below commits FUTON to
adopting, cloning or partnering with anything. The question, the triage rule and
the tensions are the payload.

Occasion: the end of SF Tech Week (2026-10-04 → 10-09). Joe logged companies and
products as he met them, into the Matrix room **FUTON 象 Demo**
(`!WXULllaFhTOchbIrmo:matrix.paragogy.net`).

---

## Operator-voice anchor

Joe, 2026-10-10:

> it would be good to understand the offerings that other folks have out there
> in their startups, and see how they relate to FUTON. Either we can ignore them,
> or if they are open source, reuse them, or if they are interesting but
> non-open, replicate them.

Joe, 2026-10-10 (fifth turn: intents vs actions):

> people use very anthropogenic terms like "memory" (I do this too) when in fact
> the reality is a bit different (graph database plus injected context into an
> agent session or something). Possibly the list I was thinking of was a list of
> "intents" that the agents could carry out on behalf of their operator, but,
> really, it is the operator's intent that is being carried out (for better or
> worse and with more or less fidelity) through the tool. This seems to be a
> common theme that the various different service providers are circling here,
> so it may be that there's no one "best" list. But I think we should be careful
> to distinguish between intents (per speech act) and, let's say, actions (like
> "query database").

(Fourth turn, in brief: apply Cognisee's idea and make a small ontology of "what
people do when they do tech".)

Joe, 2026-10-10 (third turn: the review format and the thesis):

> we could do a kind of Consumer Reports approach to these things: can we get
> them, can we build them, can we use them? The cotal example is maybe: we could
> use that, but we could also diagram-chase a different solution, e.g., agents
> coordinating via Agency or Matrix, and that's fine. The ipfs_accelerate might
> be: we do something a little similar in the work on the War Machine but this is
> cool and we should do it more often. But that review would be pending a serious
> investigation. Many of the others, if they are not open source, would be
> answered: "No we can't install them because they ask for money and I'm not
> prepared to spend anything on a service whose spec I don't trust". But again,
> that's TBD.

> In terms of a "thesis" broadly I'm not asserting that we're different from
> anything, just that we should be enabled and empowered by the best-of among
> open source ideas and solutions.

Joe, 2026-10-10 (second turn: this **revises** the rule below):

> we certainly don't need to "replicate" what others are doing, if they are
> solving a similar problem to ones we are solving in different ways, that's
> fine, it's OK for there to be different solutions. But it would be good for me
> to at least be acquainted with the problems that different folks are solving,
> particularly because almost all the companies have names that are nonsense
> words. The companies and products I have bumped into [...] are the ones that
> happened to be here in SF at a given point in time, it doesn't make them good
> or interesting, maybe it's mostly just a way to start to go a keyword search.

Joe, 2026-10-08 (metameso session, during the zone rebuild):

> It could be fun to try and map them to things we have inadvertent clones of.

> the stuff that is open source I could potentially use; the non-open stuff I
> could learn from. but broadly a lot of the AI stuff here in SF is samey samey

Joe, 2026-10-07 (hackathon, about the sponsor stack):

> unless you can convince me otherwise, I think I already have everything that
> these tools offer

## What the mission is for (as revised 2026-10-10)

**Acquaintance, not adoption.** For each entry, the output is a plain statement
of *the problem it solves*, in words that do not depend on its name. Nonsense
product names hide the problem, and this statement makes it visible. An entry that
solves a problem FUTON also solves, in a different way, needs no action.

**Entries are seeds, not a sample.** Each one is a starting point for a keyword
search, a Brownian particle released into the landscape. The problem statement
gives the search terms, and the search finds the other people solving that problem,
whether or not they were in SF this week. Being in the log says nothing about
whether an entry is good or interesting.

**The review format is Consumer Reports** (Joe, third turn). Each entry gets
three questions, then a verdict in a sentence or two:

| question | answers |
|---|---|
| **Can we get it?** | open licence (which) · free tier · paid only · waitlist/closed |
| **Can we build it?** | is there enough public spec/code to build our own, and roughly how big |
| **Can we use it?** | does it run here (self-hosted, our stack), and what would it plug into |

Example verdicts, in Joe's terms:
- *cotal*: we could use it. We could also diagram-chase a different solution
  (agents coordinating through Agency or Matrix), and that is fine.
- *ipfs_accelerate agent supervisor*: we do something a little like this in the
  War Machine work. This is cool and we should do it more often. **Pending a
  serious investigation**, so the verdict is provisional.
- *closed and paid*: "no, we can't install it: it asks for money, and I'm not
  prepared to spend anything on a service whose spec I don't trust". This is
  the provisional default for closed entries, still TBD.

**Thesis.** FUTON is *not* claiming to be different from these. The aim is
for FUTON to be enabled and empowered by the best open-source ideas and
solutions. So a review asks what we can learn or take from an entry, not how
FUTON differs from it.

**A vocabulary of verbs.** Joe (third turn) proposed extending the 象 intent
marks (`xiaoxiang-insert-mark`) with verbs that say what these tools *do*, so the
tools can be linked into a network of intents (one of the tools had an interesting
verb list). The verbs are collected per entry during MAP. Whether and how they go
into the mark list is a DERIVE question.

The bin table below is the earlier (10-07/10-08) rule, kept for the record.
Consumer Reports replaces it.

## The earlier triage rule (10-08; now secondary)

| bin | condition | what it produces |
|---|---|---|
| **ignore** | already cloned in FUTON, or not relevant | one line naming the FUTON counterpart |
| **reuse** | open licence, verified, *and* fills a real gap | a hole/mission naming where it plugs in |
| **replicate** | closed, but the idea fills a gap | a hole naming the idea, not the product |
| *(talk to)* | kindred project; the value is the people | a contact note. This bin is not in Joe's rule; it was proposed on 10-08 and needs his acceptance |

## Sources (where earlier work lives, so it is not redone)

1. **The room log**: 132 messages. The Tech Week link log runs 10-05 → 10-09.
   Read it with the fumarimo token (`/etc/zone-notify/matrix.token` on zone), or
   in Emacs as `*Ement Room: FUTON 象 Demo*`.
2. **metameso session `e13cc12c-3931-4a4e-934b-9c4bb403dded`** (Claude,
   `metameso:~/.claude/projects/-home-joe/`), turns 2026-10-08 23:18–23:28 UTC.
   It mapped the hackathon sponsor tools and the room links up to 10-08 23:23.
   The 10 unknowns got one WebFetch each. **This analysis was never written to
   a file. The table below is its only copy.**
3. **`metameso:~/hackathon/TOOLS.md`**: due diligence on the 2026-10-07
   hackathon sponsor stack (Kylon, BAND, RocketRide, AdaL, Querit, Apify, …).
4. **`~/code/TN-deep-research-landscape-position-2026-07.md` + `…-FINDINGS-2026-07-27.md`**:
   the July AI-for-mathematics landscape probe. Its held-out control (depintel)
   gives the key lesson for this mission. See tension T1.
5. **`M-joe-told-me-about-futon.md` §2b.5–§2b.8**: the 象-2000 demo and the
   three-offer ladder. This is what "relate to FUTON" is measured against.

## First-pass inventory (input from 2026-10-08, unverified)

Licences marked † were stated on 10-08 and have **not** been checked against the
repos. Nothing here has been run.

| entry | what it is (10-08 reading) | FUTON counterpart | provisional bin |
|---|---|---|---|
| Kylon | hosted workspace + local gateway running your Claude Code/Codex | Agency registry/dispatch, bells, codex-autowake, War Machine | ignore |
| BAND | agent message bus | bells & whistles, IRC/Matrix bridge, federation | ignore |
| RocketRide | declarative pipeline engine (`.pipe` JSON), MIT† | APM cascades, peripherals, systemd timers (code, not a format) | reuse? (format idea) |
| AdaL | terminal coding agent | Claude Code / Codex / Zai | ignore |
| Prelint | PR review against spec docs | mission-wholeness, invariant checkers, review worktrees | ignore |
| p2r | participation reports from agent work | evidence store, scribe, 象-2000 | ignore |
| Voiskey | phone dictation | voxterm | ignore |
| Paritok | prompt-compressing proxy | partly `compact_session.py` | ignore |
| Tenki | disposable sandboxes | worktrees, bare-metal disposable boxes | ignore |
| Querit, Apify | web search / scraping | **none** | reuse? (real gap) |
| merlin.build | "OS for AI-assisted dev" over Claude Code/Codex/Gemini: specialist agents, hooks, knowledge graph, checkpointed loops | Agency + futon3 patterns. Nearest clone in the list | ignore (or compare) |
| The Open Engine (Zeroshot) | microtasks + validation loops against code-quality decay; licence unclear | missions + gates | ignore / learn |
| TypeSafe AI — System One / Jev | small models returning typed decisions with calibrated probabilities | **none**. Routing/scoring/verification in Agency/APM calls full LLMs | replicate |
| Solid (trysolid) | prompt-to-internal-app; agents with own accounts/budgets | loosely War Machine | ignore |
| Agentic Fabriq | per-agent identity, permissions, audit trail | registry + evidence store, but **no per-agent permission scoping** | replicate (gap) |
| Smithers | durable agentic workflows, issue → reviewed change, MIT† | War Machine | reuse? (format idea) |
| Reticle | coding agent verifies its own work by driving the running app; Apache-2.0 SDK† | fucodex Playwright checks, done by hand | reuse |
| delegance.ai / Alinery | self-hosted multi-agent steering, your models, your machines | Agency's philosophy almost exactly | talk to |
| Cognisee ** | PBC, "Artificial Collective Intelligence", tacit expertise → "Wisdom Vaults" (Olaf Witkowski) | pattern language, evidence store, VSM/AIF at org level | talk to |
| Agent-deck (github.com/not-so-fat) | terminal manager for many agent sessions | claude-picker, codex-picker, agency HUD, teletype | ignore |
| Mastra ** | TS agent framework: workflows, memory, evals | Agency (a running system, not a framework) | ? (why starred?) |
| AG-UI / CopilotKit, aimock | agent↔frontend event protocol; mock LLM server | the Matrix + Element fork + fumarimo sidebar | reuse? (AG-UI as the event standard) |
| Judgment Labs, Braintrust | agent evals / tracing | evidence store + 象-2000. **Nearest commercial neighbours of the audit offer** | learn |
| Scribe "optimize" | reads how people work, recommends automations | the same move as 象-2000 | learn |
| HydraDB, ApertureData | agent memory stores | futon1b, federated memory | ? |
| atproto, Germ | federated identity/data; E2E messaging on atproto | README-federation | reuse? (spec) |
| TEE | trusted execution | "logs never leave" promise | learn |
| AIUC-1 mapping tool | AI-agent assurance standard | the "risky actions" part of the audit | reuse (map findings onto it) |
| Meticulous | frontend tests generated from real sessions | fucodex Playwright, by hand | learn |
| Composio | hosted tool/OAuth connectors | none | ignore? |
| tracn | "harness specialized to neo4j" | Zai harness + XTDB | ? |
| Tatras (conversation) | AI consultancy, India + US sales | — | market signal: client eng teams now fix things themselves |
| Poppy.01 | probably a Signal username, not a product | — | — |

### Added 2026-10-10 (from Joe directly, not in the room log)

- **cotal.ai** (Joe has a screenshot of the app; "a bit like Agency").
  cotal.ai says it is an agent coordination protocol: agents from different vendors
  find each other by name, channel or role and hand off work over peer-to-peer
  pub/sub (NATS + JetStream). It has no central orchestrator, keeps a durable
  record, is self-hosted and Apache-2.0, and has connectors for Claude Code,
  OpenCode and Hermes. *Problem:* making agents from different vendors work as one
  team with a shared record. FUTON counterpart: the Agency roster plus bells and
  whistles, plus the evidence store. Agency coordinates through a central hub on
  :7070, while Cotal does it peer-to-peer. That is a different answer to the same
  problem.
- **"Like Agency but with Lean-backed dispatches"**: this is
  **github.com/endomorphosis** (Benjamin Barber, Linux Foundation, who spoke on
  10-09; Joe identified him 2026-10-10). 157 public repos, mostly AGPL-3.0, all
  pushed within days. Read from READMEs only, nothing run:
  - **`ipfs_accelerate_py`**, its "agent supervisor" control plane. This is the
    part that resembles Agency. The pipeline runs: objective heap → AST, dependency,
    GraphRAG and *proof-gap* analysis → todos → leases and isolated worktrees →
    LLM proposals → deterministic validation → merge/proof receipts. *Problem:* let
    many coding agents work on one codebase while no LLM output is admitted until
    a deterministic checker or prover signs a receipt. FUTON counterpart: Agency
    dispatch + worktrees + gates (clj-kondo, invariant checkers) + the
    warrant/test registry. FUTON's gates are mostly linters and tests, while his
    route admission through prover receipts.
  - **`JevOps`** (AGPL-3.0): a Lean refactoring and agent-loop kernel. Autoencoder
    and refactoring modules propose Lean candidates, and only Lean/Lake admits
    them ("a gate, not a proof authority"). It also has a Lean IR autoencoder, MAB
    tactic selection and a proof-carrying cellular automaton. Its current
    application is US federal law, with source-locked legal theorems (the "Lean
    and law" from the talk). **Correction (batch C, 2026-10-10):** "TypeSafe / Jev
    kernel" in its README names the *API it calls*
    (`jevops/typesafe_inference.py` → `api.typesafe.ai/v1/systemone`, used as an
    advisor whose answers are "never proof evidence"), not its authorship.
    Barber is a TypeSafe *customer*. The two log entries are linked, but they are
    not one lead, contrary to what this file said earlier.
  - **`Mcp-Plus-Plus`**: a spec for MCP execution profiles: content-addressed
    contracts, an immutable event DAG for audit and replay, capability delegation
    chains, and temporal deontic policy. *Problem:* making a multi-agent tool call
    auditable and policy-bound. This overlaps the evidence store and the audit offer.
  - Around these: deontic-logic provers (DCEC/ShadowProver/Talos forks),
    `ipfs_datasets_py` (legal text → Z3/CVC5/Lean/Coq; GraphRAG), legal-aid tools.
  - *Reading:* the closest match so far to FUTON's thesis (T1: instrumenting what
    others assert) rather than to its harness. He combines agent orchestration,
    Lean as the admission gate, and provenance receipts in one stack. The
    differences: law where FUTON has mathematics, and IPFS/P2P where FUTON has
    a hub plus XTDB. Probably a "talk to".

### Not yet looked at (logged after 10-08 23:23, or skipped)

- immersivecommons.com (memory panel), advancedaisociety.org
- hyperbound.ai
- **ainative.studio ZeroDB** (starred **)
- **o-machine.com**: claims 86% blind win-rate over Opus/Gemini on causal
  reasoning; "architecture over scale". There is also a Google Scholar profile
  (`w68zyWwAAAAJ`) posted beside it, owner not yet identified.
- ~~Benjamin Barber / endomorphosis~~: now covered under "Added 2026-10-10" above.
- Merlin — the Tinder harness (lifeattinder.com). A different Merlin from merlin.build.
- joinplank.com
- The 10-09 panel roster: Artificial Analysis, Nous Research (Hermes Agent),
  RadixArk, Vercel, Command Code, Kylon, MiniMax, TinyFish, Zed, Zoowork
- 8 photos (10-04, 10-05), possibly slides
- tailwindcss: almost certainly ignore

# 2. MAP — first pass (2026-10-10)

`landscape/GLOSS.md` translates every name into a few plain verbs, grouped by
leading verb (coordinate, orchestrate, check, evaluate, govern, decide, remember,
connect, host, …). Start there.

Consumer Reports entries for 54 log items, written by three research subagents
and filed by batch:
- `landscape/batch-A.md`: harnesses and coordination (18)
- `landscape/batch-B.md`: evals, verification and protocols (18)
- `landscape/batch-C.md`: memory, models and kin (18)

Each entry gives the problem in plain words, get / build / use with licences checked
against the repos, the tool's own verbs, and search seeds. These entries are web
reading only. Nothing has been installed or run, and the FUTON counterparts are not
yet filled in. `landscape/hackathon-TOOLS-2026-10-07.md` is a copy of the metameso
sponsor-tool notes.

## 2.1 Surprises — recorded before DERIVE

- **Kin by vocabulary.** p2r, the hackathon's report format (BSD-3, Charlie
  Danoff), uses Peeragogy vocabulary (`decide/create/review/coordinate/reflect`).
- **Reticle's verification spec** (Apache-2.0) has four verdicts: yes / no /
  unknown / no-fault. It keeps "could not see" apart from "did not happen" and
  refuses actions that were not declared in advance. This is close to FUTON's
  evidence/claim discipline. Read it in full.
- **Cotal** is an Apache-2.0 NATS spec with work leases and fencing tokens. Its
  verbs are call / cast / watch / claim / scatter. **Kylon's** follow-up rule (a
  promise counts only once it exists with an id) is FUTON's park rule.
- **Advanced AI Society** has a draft standard for evidence of what agents did:
  8 checkpoints in an agent's run × 4 verdicts (allow / deny / modify / escalate).
  It bears on the evidence store and the audit offer.
- **TypeSafe's typed-decision API already has an open substitute.** SGLang copied
  `/v1/systemone` (2026-09-25) and serves Cloudflare's Apache-2.0 Clef decision
  models (2026-10-09). It needs a GPU.
- **o-machine's "86%"** is its best round. The aggregate is 71% vs Opus and 62% vs
  Gemini, judged by an LLM panel on rounds of questions o-machine chose itself,
  and nothing is published that would let anyone rerun it. The Scholar profile is
  Martin Trajkow, co-founder.
- **Same company, different names:** CopilotKit = AG-UI + aimock; Delegance =
  Alinery; Germ is an atproto app; trysolid = Codapt; BAND = formerly Thenvoi;
  TinyFish = AgentQL; Zeroshot = The Open Engine; Finch = FinChip + AgentOn (crypto
  skill tokens; identification not confirmed with the organisers). merlin.build and
  Tinder's Merlin are unrelated.
- **Agent memory is one idea sold several times.** HydraDB, ZeroDB, ApertureData and
  Cognisee all sell graph plus vectors. ZeroDB's GitHub looks mass-generated, and
  Cognisee is mostly a white paper.
- **Unidentified:** tracn (invite-only waitlist).

## 2.2 Verb lists worth harvesting (input to T7)

Cotal; Smithers (plan, approve, run; it separates cancel, signal and steer; fork and
rewind); Hermes Agent (~110 slash commands + kanban); Reticle's spec; Prelint's 21
MCP tools for a record of team decisions; AG-UI's 31 event types; atproto (every
verb typed as query, procedure, subscription or record); Alinery (16 workflows, 103
steps with human checkpoints); Immersive Commons (273 tools); Advanced AI Society's
checkpoint × verdict grid.

## 2.3 Still open in MAP

- FUTON counterpart and a get/build/use verdict for each entry (Joe's call where it
  is a judgement)
- the serious investigation of `ipfs_accelerate_py`'s agent supervisor (code, not
  READMEs)
- the 8 photos; the Cotal screenshot
- one wider search per problem cluster, using the search seeds (T6)

## Carried-forward tensions

**T1 — field vs thesis (as reframed by Joe, third turn).** The July probe missed
its held-out control because it chose competitors by *field* (AI-for-maths). The
fix proposed then was to choose them by *thesis*. Joe's thesis is not about being
different: it is to be empowered by the best of open source. That changes what
selection should test for. The question is not "who else instruments reasoning?"
but "who has an open idea or solution FUTON would be better for having?"
Tech Week's log was gathered by happening to be in the room, so it answers
neither question by itself.

**T2 — "inadvertent clone" cuts both ways.** Finding that Kylon, BAND, merlin and
Smithers each sell part of Agency confirms that FUTON works. It also means the
harness is not where FUTON's value is (10-08 reading, and §2b.6 of
M-joe-told-me-about-futon). Does a crowded harness market make Agency **less**
worth showing, or more? The answer differs by offer.

**T3 — reuse has a cost the triage rule hides.** "Open source → reuse" assumes
integration is cheap. Most of the open entries are TypeScript, and FUTON is
Clojure/Elisp/Python. Reusing Smithers or RocketRide may really mean
*replicate the format*, which the rule files under the closed-source bin.

**T4 — samey vs starred.** Joe found the scene "samey samey", yet starred Mastra,
Cognisee and ZeroDB. The stars are the operator's own signal, and only Cognisee's
has an explanation so far.

**T5 — market signal vs product signal.** The most decision-relevant entry
(Tatras: consulting is harder to sell because client teams now self-serve) is not
a product at all. It bears on the Ltd/consulting question rather than on Agency.

**T6 — a problem list drawn from one week in SF.** The problem statements are
meant to free the survey from names and from geography. But the starting points
still come from one city in one week, so the *problems* found will be the ones
SF startups were pitching that week. Whether the keyword searches move far enough
from where they started is something MAP has to check, not assume. T1 asks the
same question.

**T7 — speech acts vs operations.** The 象 marks
(`futon3c/emacs/xiaoxiang-preview.el`, `xiaoxiang-mark-keys`: 23 intents, each
assigned a PBASE stage from perceive to act) are *speech acts*: what a paragraph of a
turn does in the conversation (report, propose, delegate, verify). The tools' verbs
are mostly *operations on work*: spawn, hand off, lease, admit, merge, replay. A few
words appear on both lists (delegate, verify), and those words connect speech
to work. Adding tool verbs to the same list would put two kinds of thing on one
menu. A separate operation vocabulary, linked to the marks only where they share a
verb, keeps the marks as they are. MAP collects the verbs. This choice is DERIVE's.

**T8 — three layers that the vendors' vocabulary merges (Joe, fifth turn).**
1. **Intent**: the operator's, as a speech act (the 象 marks: delegate,
   verify, constrain, …). The intent belongs to the operator, not to the agent.
2. **Action**: what the tool mechanically does ("query database", "inject
   retrieved text into the session", "open a NATS subscription").
3. **Vendor word**: the human-sounding name for (2) that the product sells
   ("memory", "learns", "decides", "steers"). Joe uses these words too.

A tool *carries* an intent through actions, with more or less fidelity. So the
question to ask of a tool is not "which intents does it have?" but "which
operator intents can it carry, through which actions, and how would anyone tell
whether they were carried faithfully?" No list is likely to be "best": every
vendor is circling the intent layer with a list of its own.
`landscape/GLOSS.md` mixes layers 2 and 3 (e.g. "remember", "learn"). It is a
translation aid, not the ontology. T7 is a special case of T8.

## Explicitly NOT decided here

- Whether any entry gets adopted, cloned or contacted
- Whether the "talk to" bin exists
- Which FUTON offer the positioning is *for* (newcomer, audit, War Machine)
- Whether a public comparison page is wanted at all

## Provenance

- Room log pulled 2026-10-10 via the fumarimo token on zone (132 m.room.message events).
- 10-08 analysis extracted from metameso session `e13cc12c…` by claude-13, 2026-10-10.

---

# 3. DERIVE (draft, 2026-10-10): operator intents as SVO

Joe, 2026-10-10 (sixth turn):

> the "tool" layer is really just part of an SVO triple where the verbs are
> already part of our intent layer. So, e.g., "ask, constrain responses" might
> be an example. The point is to try to make the human side reasonably familiar
> without anthropomorphising the agent or tool. Even in a case where we might
> "delegate" the act of paying to an agent or subagent, I think it would be a bit
> silly to talk about them "forming an opinion" or whatever. I might use that
> kind of language informally to get my point across *to* an agent, but for the
> ontology, it's much more cut-and-dried.

## Rules

1. **Subject**: always the operator (or another named person). An agent or tool
   is never the subject of an intent.
2. **Verb**: drawn only from the 象 intents (`xiaoxiang-mark-keys`: approve,
   disagree, clarify, report, report-problem, verify, retract, withdraw, propose,
   qualify, explain, constrain, ask-action, delegate, prioritize, collect, extend,
   continue, defer, redirect, explore). A needed verb that is missing goes to
   *Verb gaps* below. It is not invented in the row.
3. **Object**: what the intent acts on (a task, responses, a change, access, …).
4. **Via**: the tool's *actions*, described mechanically (T8 layer 2). This is
   where the tool appears, as an instrument.
5. **Vendor words** (T8 layer 3) are recorded only so the names can be looked up.
   They never appear in the S, V or O columns.
6. **Fidelity**: how anyone could tell that the action carried the intent.

## Triples (first draft: 20 rows, drawn from GLOSS)

The subject is *the operator* throughout and is omitted from the table.

| # | verb · object | via (actions) | tools | vendor words | FUTON | fidelity check |
|---|---|---|---|---|---|---|
| 1 | delegate · a coding task | post the task to a queue; an agent process takes it in a worktree; a completion event is emitted | Kylon, Smithers, merlin, Hermes, MiniMax | "AI teammate", "works while you sleep" | Agency bell; War Machine | the returned diff does what the task said (review) |
| 2 | delegate · tasks across agents from several vendors | pub/sub on a message bus; leases with fencing tokens | Cotal, BAND | "agent team", "collaboration" | roster + bells/whistles | one record shows who held each lease; no task is done twice |
| 3 | ask-action + constrain · responses to a fixed set of typed answers | call a classifier that returns label + probability | TypeSafe; Clef via SGLang | "System One", "fast thinking" | none (full LLM calls) | stated probabilities match observed frequencies |
| 4 | collect · facts and decisions; constrain · later responses | write to a graph/vector store; retrieve by similarity; inject the matches into the prompt | HydraDB, ZeroDB, ApertureData, Mastra, Hermes | "memory", "learns", "second brain" | futon1b; memory files | retrieval returns what it should, and the injected text changes later output as intended |
| 5 | verify · a change in the running app | drive a browser; capture network/console/state; return yes/no/unknown/no-fault | Reticle, Meticulous, Tinder's Merlin | "self-verifying" | fucodex Playwright, by hand | an unseen outcome comes back as *unknown*, not as a pass |
| 6 | verify · a change, by a second process; report-problem · back to its author | run a separate reviewer; on failure, return the work with a limit on retries | Zeroshot | "won't let broken work ship" | author ≠ reviewer; warrants | the reviewer did not share the author's context |
| 7 | verify + constrain · admission of a change | run Lean/Lake or a type checker; merge only on a receipt | ipfs_accelerate, JevOps | "proof-carrying" | clj-kondo/invariant gates; futon6 Lean | the receipt names the exact sha that was checked |
| 8 | constrain · what an agent may access, and on whose behalf | broker OAuth tokens; per-user scopes; log each call | Agentic Fabriq, Composio | "agent identity", "trust" | none (no per-agent scoping) | the log shows each call under that person's scope |
| 9 | verify · a change against earlier decisions | compare the diff with decision records; flag mismatches | Prelint | "remembers what your team decided" | mission-wholeness; invariant checkers | flagged mismatches are real ones |
| 10 | explore · outputs across many runs; report-problem · regressions | collect traces; cluster failures; score against a rubric | Judgment, Braintrust | "observability", "evals" | evidence store + 象-2000 | a regression is caught before release |
| 11 | explore · the web | call a search API; crawl; drive a headless browser | Querit, Apify, TinyFish | "agentic browsing" | none | each result can be cited; page content is what was served |
| 12 | delegate · running untrusted code | start a disposable VM/container; snapshot, fork | Tenki, InstaCloud | "sandbox" | worktrees; disposable Linodes | the host is unchanged after the run |
| 13 | constrain · token spend | compress the prompt with a small model; cache | Paritok | "context compression" | `compact_session.py` | same task outcome at a lower token count |
| 14 | collect · run events; explore · run state as it changes | stream typed events from the agent process to a UI | AG-UI, CopilotKit | "copilot", "generative UI" | Matrix + Element fork + fumarimo | every state change in the run reaches the UI |
| 15 | redirect / approve · a running workflow | pause at a checkpoint; resume with the operator's input; rewind to a step | Smithers, Alinery | "steer", "align" | ground control; park/wake; 象 rewind | the resumed run uses the input given and does not guess a fresh one |
| 16 | defer · a follow-up | create a scheduled job with an id | Kylon | "follows up" | park with deadline | the promise exists as an id, or it is not a promise |
| 17 | constrain + verify · agent conduct, for outside parties | sign attestations at checkpoints (allow/deny/modify/escalate) | Proof-of-Control, AIUC-1, Mcp++ | "trustworthy AI", "governance" | evidence store; commit trailers | an outsider can check the record without trusting the operator |
| 18 | delegate · payment | spend from a funded wallet up to a limit | Solid, Finch | "agent with its own budget" | none | spending stays within the limit, and each purchase traces to a task |
| 19 | report · one's own contribution | fill in a fixed schema; validate it | p2r | "participation review" | scribe; evidence store | a reader can check the report against the work |
| 20 | collect · tacit know-how, with attribution | record it; store it with consent and source metadata | Cognisee | "wisdom vault", "tacit reasoner" | pattern library | attribution survives reuse |

## Verb gaps (needed, not in the 象 intents)

- **compare** (Braintrust: variant A vs B). Row 10 uses *explore*, which loses the
  pairing.
- *pay* is not missing: row 18 treats payment as the object of *delegate*. That
  is in line with Joe's example.

## What the draft shows (provisional)

- Ten tools from GLOSS fit under **delegate** or **verify**. Those are the two
  intents the SF market is selling against.
- Rows **3, 8 and 18** are the ones where FUTON has no counterpart. Each is a
  *constrain*: answers restricted to typed choices, access restricted by scope,
  spending restricted by a limit.
- Vendor words cluster on the *subject* slot ("the agent decides / learns /
  steers"). Rule 1 removes all of them without loss.

## 3.1 Toward client problems (Joe, 2026-10-10, seventh turn)

> What starts to become interesting is what actual customer or client problems
> any of this might solve for anyone. Asking that could explain why some of the
> verbs are more interesting than others. [...] a given company out there in
> middle America or wherever is going to be turning to AI/tech/agents/etc. to
> solve some actual problem for themselves or their clients. And in principle
> having a model of how the space works should allow us to help them solve it
> faster or better. Now, some problems are totally "human" problems, e.g.
> managers that don't listen to their employees won't be "solved" with
> Elephant-2000 methods for example.

> [the War Machine] as a model of the way people and organisations work, it
> seems like a great schematic. (Presumably whatever list of verbs we find or
> come up with can be fitted to the PBASE and R-number outlines that describe
> War Machine.)

**A filter that follows from the SVO form (proposed, not accepted):** a tool can
only change the *via* column, which is how faithfully an intent is carried. If a
client's problem lies in the **S or V**, meaning nobody holds the intent (the
manager has no intent to *collect* what employees report), then no improvement in
fidelity will fix it. It is a human problem. If the intent is present but is
carried badly or slowly, it is open to tooling. Many real problems are mixed.

**Fitting the triples to the War Machine (first reading, unchecked against the
Lean spec):**

| triple rows | WM node |
|---|---|
| 1, 2 delegate · task | R10 (commissioned and dispatched) |
| 6 verify by a second process | R9 (no self-certification) |
| 3 constrain responses to calibrated typed answers | R12 (two-layer calibration) |
| 13, 18 constrain token spend / payment | R11 (hierarchical shared budget) |
| 16 defer a follow-up | R16 (grounded actuation; parked and surfaced) |
| 10 report-problem · regressions | R20 (interoceptive tripwires) |
| prioritize / constrain · what matters | R19 (preference C-vector), which is set by people. This is where the "human problem" boundary sits in WM terms |

The three *constrain* rows with no FUTON counterpart (3, 8, 18) fall on R11 and
R12. Those are WM nodes, so the War Machine has a specification for them even
where the stack has no tool.

**Prior work to reuse:** `M-joe-told-me-about-futon.md` §2c already collected
public pain signals (clusters C1–C6). They are almost all *developer* pain. The
client in middle America is a different population, and that pass has not been
done.

## 3.2 The kernel sorted by who the work is for (2026-10-10, eighth turn)

Joe:

> It seemed to me that in many cases they are solving tech problems for tech
> people (e.g. Mastra seems like a great example of this)... being good at
> managing context for developer sessions or software factories does not
> immediately in my mind translate into solving non-technical problems. Whereas
> a possible distinction with the War Machine is that, yes, technically it is a
> kind of "software factory" but it is a software factory that can be grounded in
> the way an actual organisation works.

Sorted from the batch files by whose problem the product addresses:

**A. Tech for tech people (≈36 of 54).** Kylon, BAND, RocketRide, AdaL,
merlin.build, Tinder's Merlin, Smithers, Zeroshot, Agent Deck, Mastra, Cotal,
Composio, Command Code, TinyFish, Judgment, Braintrust, Prelint, Reticle,
Meticulous, AG-UI, CopilotKit, aimock, Paritok, Tenki, Artificial Analysis,
Querit, Apify, HydraDB, ApertureData, ZeroDB, TypeSafe, InstaCloud, RadixArk,
Glasser, Finch, Hermes. The buyer is a developer, and the problem is the
developer's own (context, drift, review, cost, plumbing).

**B. Doing a non-technical worker's task (≈5).** Solid (a business task run end to
end), Hyperbound (sales practice and call scoring), Voiskey (dictation into
finished text), MiniMax Agent (job → slides/spreadsheet/video), o-machine
(analyst "why did this happen"). The client's problem appears directly, but the
organisation does not. Each serves one worker and one task.

**C. Capturing an expert's know-how (2).** ZooWork (expert → deployable agent),
Cognisee (tacit decision-making, with attribution and consent).

**D. Making an organisation's working visible to itself or to outsiders (≈5).**
Scribe Optimize (what employees actually do, mined from their clicks), Agentic
Fabriq (whose authority an agent acts under), AIUC-1 (what an insurer or buyer may
rely on), Proof-of-Control (what outsiders can check), p2r (each participant's
own contribution in a mixed human/AI cycle).

**E. Not products:** Plank (rents engineers), Tatras (consultancy),
Immersive Commons (a place), atproto/Germ (social infrastructure).

**Reading (provisional).** Joe's distinction puts the War Machine nearest to **D**.
A software factory "grounded in the way an actual organisation works" needs what
D sells: a record of what people actually do (Scribe), under whose authority
(Fabriq), and checkable from outside (Proof-of-Control, AIUC-1). It also needs
what A sells, which is the factory itself. None of the D vendors builds the
factory, and none of the A vendors models the organisation. B products fix one
person's task without modelling the organisation. C products capture what one
expert knows, which in WM terms would be one source of preferences (R19).

In SVO terms, A's subject is a developer. B's and C's subject is a worker or an
expert. In D, the subject is an organisation with several people who hold
intents, and they may disagree. That is also where the human problems are (the
manager who does not listen). This section is input for the client-problem pass
(`landscape/client-problems-kernel.md`, in progress).
