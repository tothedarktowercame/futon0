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
| 20 | collect · tacit know-how, with attribution | record it; store it with consent and source metadata | Cognisee | "wisdom vault", "tacit reasoner" | pattern library; **futon6 arXiv mining** (same intent, with papers as input instead of interviews; Joe, 2026-10-10) | attribution survives reuse |

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

## 3.3 Scattering (Joe, 2026-10-10, ninth turn)

> I'm thinking this is a kind of scattering theory problem. We put a real problem
> in on one side. In the middle, it might bounce around inside the tech world for
> a while, with handoffs, factories, verification, etc., and then out the other
> side comes a concrete solution of some kind to a real problem. For example,
> consider Doordash or delivery. "We bring food to your house" solves a real
> human problem. Inside, no doubt, there's lots of tech involved, and as Doordash
> improves its operations, even more tech. But is it on the way to solving real
> human needs better or faster?

What the framing gives the survey (proposed):

- **Judge by in-state → out-state, not by the bounces.** In scattering, internal
  detail matters only through what comes out. A tool earns its place if there is
  a traceable path from the bounce it changes to a change in an out-state.
  Most of group A (§3.2) changes only internal bounces: a developer's context,
  review and handoffs.
- **There are several out-channels, not one.** DoorDash's out-state for the
  customer is food arrived, how fast, at what cost and correct. The same scattering
  also has out-channels for couriers (pay, hours) and restaurants (margin). "Better
  or faster" can improve one channel at another's expense, so the question
  "better for whom?" belongs in the out-state.
- **Bounces can cancel.** Judgment Labs' clients found that reviewing agent output
  used up the time the agent saved (client-problems-kernel.md). Each bounce was
  carried out faithfully (T8), and the out-state did not change. Fidelity per
  bounce does not add up to an end-to-end gain.
- **War Machine reading.** The R-nodes are internal bounces. "Grounded in the
  way an organisation works" would mean the WM's observations (R2, `o`) are taken
  from the organisation's out-channels, not from its own internal steps.

Per client problem, record: the in-state (the problem as the client states it),
the out-channels and who is on each, a measurable out-state per channel, and which
bounces each tool touches.

## 3.4 Client problems as scatterings (first pass, 2026-10-10)

Source: `landscape/client-problems-kernel.md`, published customer stories for
the companies in groups B–D (four sub-agents). **All numbers there are
vendor-reported.** Most stories are anonymised. Tatras Data (the consultancy Joe
met, §1) gave the most, about 50 stories, and AIUC-1 gave the most named ones.
Tool vendors that sell to builders (Composio, Braintrust, Judgment) show end-client
problems only second-hand.

The verbs below are the client's intents (the subject is the client organisation,
omitted). Kind: **T** tooling (the intent is present but carried slowly or
badly), **H** human (the intent or the trust is missing), **M** mixed.

| # | in-state (client's problem) | verb · object | out-channels (who) | kind | source |
|---|---|---|---|---|---|
| 1 | thousands of complaint tickets a day, read, routed and answered by hand under regulatory rules | delegate · routing; verify · rule compliance | complainant (time, outcome); regulator; staff load | T | Tatras (insurer) |
| 2 | paper invoices checked by hand against contract clauses held elsewhere | verify · invoice against contract | supplier (paid correctly); finance | T | Tatras |
| 3 | hail-damage claims assessed from photos: slow and inconsistent | delegate · assessment; constrain · consistency | claimant; insurer loss ratio | T | Tatras |
| 4 | field staff search 200-page manuals; veterans skim five per question | explore · manuals | plant uptime; staff | T | Tatras |
| 5 | corrosion samples wait 7+ days for lab results before treatment changes | delegate · measurement | plant; water users | T | Tatras |
| 6 | ad spend not linked to footfall, so staffing is guesswork | collect · linked data; prioritize · staffing | shoppers; staff hours | T | Tatras |
| 7 | staff log into 32k partner studios' booking systems by hand to check schedules | collect · schedules | customers (accurate classes); studios | T | TinyFish (ClassPass) |
| 8 | 90 reps need roleplay practice; managers' time is the bottleneck | delegate · practice; verify · call quality | prospects; reps' pay; managers | M | Hyperbound |
| 9 | an AI mandate with no map of how work is done; weeks of sticky-note workshops | collect · how work is done | employees; leadership | M (mandate before map) | Scribe (TXNM Energy) |
| 10 | 4x ROI demanded before automation; baselines from memory proved wrong | verify · baseline | the business case; staff | M | Scribe (Compare Club) |
| 11 | lawyers re-check agent output by hand, which uses up the time saved | verify · agent output | clients; lawyers' hours | T (bounces cancel, §3.3) | Judgment (anon.) |
| 12 | an agent approves privileged access on SOX systems; the auditor asks "can you prove it does what the policy says?" | verify + constrain · agent conduct, for an auditor | auditor; the firm's licence | H/M (trust between organisations) | AIUC-1 (MSCI) |
| 13 | enterprise deals stall at the buyer's security sign-off; nobody is clearly liable | verify · conduct, for a buyer; delegate · liability | buyer; insurer | H | AIUC-1 (Lovable, ElevenLabs) |
| 14 | a brand about to pivot "based on a hunch"; surveys can't show who is changing behaviour | explore · who changes behaviour | customers; the brand's margins | M (acting on a hunch) | o-machine (anon.) |
| 15 | incident investigations on paper forms with incomplete witness statements; incidents repeat | collect · accounts; report-problem · root cause | workers' safety | M (*inference:* witnesses may hold back) | Tatras |

**What the pass shows (provisional):**

- **The verbs clients need are *verify* and *collect*.** Twelve of the 15 rows
  use one or both. *Delegate* appears mostly as "delegate the reading". The
  verbs SF sells most (orchestrate, coordinate) do not appear as client intents
  at all. They are internal bounces.
- **The common in-state is documents in a different format from every source,
  checked by hand** (rows 1–5, 15). The tooling exists. What varies is fidelity:
  consistency (3) and whether the check can be trusted (11).
- **The human problems sit at boundaries between organisations or levels**:
  auditor and firm, buyer and vendor, leadership and staff (9, 12, 13). In scattering
  terms, the output has to pass through someone else's verification. Groups A and
  B do not address that channel. Group D (Proof-of-Control, AIUC-1) does. So do
  the FUTON evidence store and the audit offer.
- **Row 11 is a warning for the War Machine.** If a factory's output still needs
  full human review, nothing reaches the out-channel. R9 (no self-certification)
  helps only if the second check costs less than the first.

## 3.5 R-nodes as the internal factors; scale (Joe, 2026-10-10, tenth turn)

> to use the R-nodes as internal factors, since we've already done the analysis
> of its adjacency matrix, indeed, our work has somewhat turned Friston's physics
> into a physics-inspired ontology, though it's an "abstract ontology" rather than
> a lexical one. And because AIF is what it is, the system is somewhat scale
> free. So, "we bring food to your house" scales up to "the trade deficit with
> Mexico" or whatever.

**Two ontologies, joined by the fit in §3.1.** The SVO triples (§3) are the
*lexical* side: what people say they intend. The R-nodes are the *abstract* side:
the factors of the scattering. The fit in §3.1 (delegate → R10, verify by a second
process → R9, constrain spend → R11, …) is what joins them.

**Where the abstract side lives:**
- `futon3c/holes/labs/M-wm-wiring/wm-adjacency.edn`: the wiring matrix
  (104 wires at last count, PROOF-2a log 2026-09-26; 0 verified live, 2 hermetic)
- `futon2/holes/labs/wm-contract/aif-equations.edn`: the equation DAG
  (37 dependency edges after WM-EQUATIONS-APPLY-I)
- `futon2/holes/aif-r1-r16-pattern-map.md`: each R-node as a pattern, with
  its real status

Honest status: the schematic is specified, but its wires are almost all
unverified live. Using it as an ontology does not depend on the wires carrying
values. Using it as a *model* of a client does.

**Scale: one scattering at three sizes (illustration, not analysis).**

| factor | household: "food to your house" | city: restaurant delivery market | nation: food trade deficit with Mexico |
|---|---|---|---|
| o (R2) observations | the order arrived, when, and whether it was right | delivery times, courier churn, restaurant margins | import/export volumes, prices, border wait times |
| C (R19) preferences, set by people | hungry, cheap, now | city: congestion and labour rules; platform: growth | food security, farm incomes, consumer prices: contested |
| π, G (R6, R5) policies and their scoring | which app, which restaurant | fees, courier dispatch, restaurant onboarding | tariffs, subsidies, inspection regimes |
| R11 shared budget | the household's money and time | courier hours; platform burn | fiscal, and the inspectors' capacity at the border |
| R9 no self-certification | the customer rates the order | health inspectors; reviews | USDA/FDA inspection; trade-dispute panels |
| out-channels | the eater | eaters, couriers, restaurants | consumers, farmers on both sides, two governments |

The factor names stay the same at every scale. What grows is the number of parties
holding preferences (C), and how far apart those preferences are. That matches
§3.4: the human problems sit where several parties' C meet. In that sense the
system is scale-free in its *form*, and the parties holding preferences are what
make the difference between scales.

## 3.6 Problems before tech; assemblies with a history (Joe, 2026-10-10, eleventh turn)

> this points to a whole class of problems that are not really "tech" problems,
> but, say, "economics" or "data analysis" problems. Those could become part of a
> tech solution, i.e., if we understand a range of topics well, look at how they
> hook together, build a model of that, and then look for ways in which that
> model could run differently, we may put ourselves into a position to build a
> technical solution. But that comes rather late in the game from a logical
> standpoint.

> my tech stack. I'm using a Kinesis Advantage2 keyboard, but in the old days I
> was using a Kinesis Classic that I got around 2001. The next thing in the chain
> is a Dell deck that allows the keyboard to talk to my phone. The phone is a
> Samsung. The next things after that are the OS and software like Termux [...]
> from which it reaches Zone. Now, that's a "technical" diagram, but you could
> also look at an historical diagram in which Zone's GNU/Linux Ubuntu system and
> the Android OS share some common heredity. [...] inspired by W Brian Arthur
> [...] these are not "random" assemblages of things at all. Even just the
> keyboard I have been "carrying around" for 26 years, just without the
> high-powered server to connect it to.

**The order Joe gives:** understand the topics → see how they hook together →
model that → look for ways the model could run differently → *then* a technical
solution. Tooling comes last. The SF kernel (§3.2 group A) starts at the last step.

**Arthur, briefly (*The Nature of Technology*, 2009):** a technology is a
combination of earlier technologies. Each component is itself a technology, so
the structure is recursive. New technologies come from combining existing ones,
and the economy is "an expression of its technologies". Two diagrams follow from
this, and Joe's stack shows both:

| diagram | Joe's stack |
|---|---|
| **assembly** (what plugs into what, now) | Kinesis → Dell dock → Samsung/DeX → Android → Termux → 4G → zone (Ubuntu) |
| **heredity** (what descends from what) | the Linux kernel in both Android and Ubuntu; Kinesis Classic (≈2001) → Advantage2; ssh/mosh/tmux, older than the phone |

**What this suggests for the client pass (proposed):** for each client problem,
draw both diagrams and ask *which component has persisted longest*. That
component is the client's "keyboard". Its constraints have outlasted every
assembly built around it. In §3.4 these would be the paper invoice, the 200-page
manual, the regulator's rule and the auditor's question. Tools that ask the
client to drop that component tend to fail. Tools that build a new assembly
around it, as Joe's stack did with a 26-year-old keyboard, have a better chance.
In scattering terms (§3.3), the long-lived components shape the in-state before
any new tool is involved.

## 3.7 A co-op that helps create co-ops (Joe, 2026-10-10, twelfth turn)

> one approach I heard about at SF Tech Week (which I liked) was to focus on
> building a co-op that would help people create other co-ops. So, the "inner
> loop" here might be about solving some real-world problems, the "outer loop"
> would be about enabling that on a technical basis. The only challenge with this
> is that, if we come at it from a purely technical perspective, we'd be coming
> with a solution (outer loop) in search of a problem. That said, there may well
> be lots of real-world problems that are amenable to good solutions at the co-op
> level that are not being developed *because* technology is a barrier [...]
> vibe-coding plus an SF stack or a FUTON stack might help lower the
> energy-threshold that's needed. [...] it seems plausible that the person I was
> talking to about this is [in touch with inner-loop problems], and I could help
> with the technical delivery side. But it'd be good to know more about the
> concrete examples (even if hypothetical to start with)

**Why co-ops fit what this mission has found so far:** §3.4–§3.5 placed the
human problems where several parties' preferences (C, R19) meet. A co-op is an
institution built for exactly that meeting: the members are the people on the
out-channels (§3.3). Tooling cannot create the trust. It can lower the cost of
carrying out what the members decide (*collect* and *verify*, §3.4) so that the
co-op can afford to exist.

**Hypothetical inner-loop cases (from memory; being verified in
`landscape/coop-cases.md`):**

| inner loop (the real problem) | the long-lived component (§3.6) | where tech is the barrier | intents |
|---|---|---|---|
| couriers own the delivery platform (the DoorDash case, §3.3) | restaurants' order flow; city labour rules | dispatch, payments, onboarding; CoopCycle exists for this | delegate · dispatch; verify · pay |
| home-care workers' co-op | Medicaid billing and care-plan paperwork | scheduling, billing, compliance records | collect · visit records; verify · claims |
| small farms pooling food-safety traceability (FSMA 204) | the federal record-keeping rule | each farm can't afford its own records system | collect · lot records; verify · compliance |
| a retiring owner sells the business to its staff | the books, the valuation, the bank | bookkeeping and governance handover | collect · how the business runs (cf. Scribe, §3.4 row 9); verify · valuation |
| gig workers pool their own data to see their true pay | each platform's opaque pay rules | collecting and analysing the data | collect · earnings; explore · pay rules |
| freelancers share invoicing and social insurance | national tax and insurance rules | invoicing, contracts, compliance | verify · invoices against contracts (§3.4 row 2) |

**Outer loop (the co-op that makes co-ops), in FUTON terms:** a reusable
assembly (Arthur, §3.6) of what every new co-op needs to start: members'
decisions, records, compliance checks, money. Most of it is *collect* and
*verify* carried out for several parties at once, so that a new co-op's activation
energy is mostly its inner-loop work. Existing examples of an outer loop to check:
Co-op Cloud (shared hosting for co-ops), Start.coop, Platform Cooperativism
Consortium, Project Equity.

**Open:** who the person Joe met is, and which inner-loop problems they have in
hand. Their list beats this table.

## 3.8 Worked case: "we bring food to your neighbourhood" (Joe, 2026-10-10, thirteenth turn)

> Instead of "we bring food to your house", how about simply "we bring food to
> your neighbourhood". That's what corner shops do. And yet in plenty of urban
> settings there are "food deserts" where people don't have access to healthy
> food and where people don't know where a tomato comes from or what one looks
> like. So, possibly a "corner store co-op" would be in-demand in these
> neighbourhoods (and possibly not).

**Evidence for "and possibly not" (checked 2026-10-10):**
- **Allcott, Diamond & Dubé** (NBER w24094; *QJE* 2019): supermarket entry and
  households moving to healthier neighbourhoods had no meaningful effect on
  healthy eating. Giving low-income households the availability and prices that
  high-income households get closes **9%** of the nutrition gap. The other 91% is
  demand. In R-node terms, the gap is mostly in C (preferences, habits,
  knowledge: "what a tomato looks like"), not in the supply path.
- **Renaissance Community Co-op, Greensboro NC**: a community-owned grocery in a
  food desert. It opened in October 2016 after 18 years without a store and
  raised $1.2M from members plus city and county grants. It closed in January
  2019 because sales were too low. "People had developed other habits." Nonprofit
  Quarterly ran a post-mortem webinar on it.
- **Against that, at store level:** healthy-corner-store programmes report that
  when stores carry more fresh produce, customers buy more fruit and vegetables and
  fewer sugary drinks. The NYU Stern FoodMapNY report surveys these programmes. The
  two findings can both hold: a store-level effect can be real and still close
  little of the population gap.

**Two different co-ops hide in "corner store co-op":**

| | consumer co-op store (Renaissance model) | purchasing co-op of existing corner stores (Saba model) |
|---|---|---|
| long-lived component (§3.6) | replaced: a new store | **kept**: the bodega and its owner (NYC has 14,000+, about 12 per grocery store) |
| the hard part it pools | everything: capital, lease, staff, demand | distributors' minimum orders, refrigeration, small deliveries |
| example | Renaissance (closed 2019) | Saba Grocers Initiative, Oakland (nonprofit, founded 2020): refrigeration plus collective purchasing so stores can order below wholesale minimums; 3 pilot stores → 14 by July 2025 |

The purchasing co-op follows the advice of §3.6: build around the component that
has lasted.

**As a scattering:**
- *In-state:* a neighbourhood with corner stores and no fresh food on the shelves.
- *Out-channels:* residents (diet, price, distance); store owners (margin,
  spoilage); distributors (order size); the city (health costs).
- *R-nodes:*
  - o: sales by item; spoilage
  - C: residents' habits. **Most of the gap is here, per Allcott.**
  - R11: owners' cash and shelf space, and the cold chain
  - R9: SNAP/WIC retailer rules, health inspection
- *Kind:* **M**. Supply (pooled ordering, refrigeration) is a tooling problem.
  Demand (C) is a human one, so stocking alone will not move it. Moving it needs
  people: cooking, tasting, trust in the shop owner.

**Where an outer loop (§3.7) would help:**
- *collect*: pooled orders across stores; item-level sales and spoilage per store
- *verify*: SNAP/WIC stocking rules; delivered against ordered
- *prioritize*: what to stock next, from what actually sold

This is small, cheap software: purchasing, inventory and spoilage records shared
across stores. FUTON's evidence-store habit of keeping the record would also show
which demand-side efforts move sales and which do not. That is the 91% question,
asked store by store.

Sources: https://www.nber.org/digest/feb18/eliminating-food-deserts-wont-cure-nutritional-inequality ;
https://www.gsb.stanford.edu/faculty-research/working-papers/geography-poverty-nutrition-food-deserts-food-choices-across-united ;
https://nonprofitquarterly.org/the-ballad-of-the-rcc-or-nice-try-now-try-again/ ;
https://wfmynews2.com/article/news/greensboro-community-grocery-store-that-opened-nearly-3-years-ago-to-close/83-d6296585-6d10-4242-9c28-0b8c2d938a48 ;
https://ucanr.edu/sites/default/files/2025-09/Saba%20Case%20Study%20Brief.pdf ;
https://stern.nyu.edu/sites/default/files/2024-11/2b_FoodMapNY_ProjectReport_HealthyFoodinRetail_112224.pdf

**Joe, fourteenth turn:**

> Saba is probably one of the inspiring examples for the person I talked with
> b/c he is Oakland based. Broadly what's key here is the "demand side". So, of
> course, if what people want is liquor, beer, wine, video games and casinos,
> tobacco, etc., then, within reasonable limits of regulation, that's what they
> are going to get.

*Reading:* this restates the §3.4 filter at the scale of a neighbourhood. Tooling
can change how an intent is carried out, but it cannot supply the intent. A store
stocks what sells, so the owner's C (margin) follows the residents' C (wants).
Regulation is R9's limit on that. A co-op does not change this, but it changes
*whose* C governs. In a consumer co-op, the residents' preferences are the ones
that count, and outsiders' views of what they ought to want do not. So "is it in
demand?" is an inner-loop question. It is answered by asking residents and by
sales records, before anything is built. This is the corner-store version of
"solution in search of a problem" (§3.7).

## 3.9 Co-op cases, checked (2026-10-10)

Full write-up: `landscape/coop-cases.md` (12 cases; it corrects §3.7's
from-memory table: Resonate closed in 2024, Driver's Seat is effectively
inactive, and CoopCycle's licence limits commercial use to co-ops, so it is not
standard open source).

- **A shared stack that many small co-ops reuse is what has worked:** CoopCycle
  (≈85 co-ops in 2022; €49/month in year one, then 2% of revenue), Up & Go (built
  by the CoLab tech co-op and copied to Philadelphia and Detroit), Smart's central
  admin, Co-op Cloud. Drivers Co-op Colorado rented an existing app (TADA, ≈$0.60
  a ride).
- **Failures come from *keeping* the software running, not from launching it.**
  Resonate closed. Driver's Seat's app went stale. Drivers Co-op Colorado stopped
  in April 2025 over app problems. NYC's Drivers Cooperative started only because
  a founder personally guaranteed a $400k credit line.
- **Compliance software cannot be opted out of:** ride-hail fare apps; NY
  home-care electronic visit verification; FDA food traceability (enforcement
  pushed to 20 July 2028; FDA cost estimate $570M a year).
- **Capital comes first, then admin:** Euricse (December 2025) ranks finance as the
  top challenge, finds 41% of platform co-ops hit legal barriers, and counts 64
  failed or inactive, mostly delivery and taxi. The US worker co-op census (2025)
  finds 36% struggling with admin and 39% needing marketing help.
- **No one is building shared back-office software for co-ops, with AI or
  without.** The co-op developers (Start.coop, Project Equity, DAWI, PCC) offer
  coaching, legal templates, finance and research. No named co-op is building
  its software with agents.

*Reading:* the outer loop's job is **maintenance and compliance across many
co-ops**, not building apps. That is a small fixed set of *collect/verify*
duties, kept running. It matches the War Machine's shape: a factory whose output
has to stay correct over time.

## 3.10 What "futonic" adds (Joe, 2026-10-10, fifteenth turn)

> coming from the UK where "the Co-op" is a known national brand (but from a food
> standpoint certainly no better than any other grocery chain) it seems clear
> that the co-op aspect is possible. A Saba in every city would be the same kind
> of thing. And if it makes money, great. But I think the "futonic" ideas would
> indeed be a bit about helping people realise some of their own potential,
> whether that's for business or education or just eating healthier food or
> having more fun or whatever. And it remains a good question about which aspects
> of that technology can really help with. [...] technology could help me learn
> mathematics "faster" or at least "better"... but learning mathematics seems
> like something of a niche interest. [...] UK-based research that said that most
> people don't like learning at all (though I'm not sure if they counted young
> people learning football statistics).

**The UK figure (Learning and Work Institute, Adult Participation in Learning
Survey 2025):** 42% of adults report learning in the last three years, down
from 52% in 2024, and 21% are learning now. Half of those who left school at 16
or younger have not taken part in learning since. Leaving school at 18 rather than
16 makes adult learning 20% more likely. These are *self-reported* figures, so
they count what people call "learning". Football statistics, betting odds and
game strategy would mostly not be called that.

*Reading:* the same demand-side point as §3.8 applies. Learning that people
want often goes by another name, inside things they already do. The futonic
question is therefore less "how can tech make people learn" than "how can tech
carry what people already want to get better at": the inner loop again, with
people's own C setting the direction.

## 3.11 The hyperreal bet, and who it is proven for (Joe, 2026-10-10, sixteenth turn)

> the "hyperreal" bet (after Baudrillard but not obsequiously) is that suitable
> technologies can help people *understand* stuff better, including themselves
> and how to realise some of their dreams. [...] my idea to data mine the Arxiv
> is now proven out "in principle" and what remains is hooking all of that up.
> [...] someone else may have a different dream. And they might find that Claude
> or Claude Code or Mastra or whatever helps them realize it. If so, great,
> again. But my guess is that the FUTON stack would help a lot more than a "raw"
> Claude, and I've tried to make myself a proof point of that, much as Rob has
> made Mfuton a proof point for his work. We're both pretty convinced but we're
> coming at this from an already technical, already mathematical perspective. If
> someone's dream is to make more money as a financial trader or to open a
> grocery store, I don't know for sure that FUTON can help them.

**Candidate question for IDENTIFY:** does FUTON help someone whose dream is not
technical more than raw Claude does? The two existing proof points (Joe, Rob)
both have the same starting point, so they cannot answer it. This is the July
probe's held-out-control lesson applied to users, not competitors.

**Which parts of FUTON could carry over (to test, not assume):**
- *Probably domain-general:* the mission lifecycle (HEAD keeps the person's own
  words; MAP before design); keeping the record; *verify* as a habit; patterns as
  reusable answers to tensions. These serve the "understand" half of the bet. Note
  that this session is itself a small instance: a non-technical question (food
  deserts, co-ops) worked through with the mission discipline. But the user was
  still Joe.
- *Probably tech-for-tech:* Agency's dispatch and bells, the War Machine's
  plumbing, Lean. These are bounces (§3.3) and only matter through what they
  deliver.

**A test that could answer it (sketch):** one person with a non-technical dream
(e.g. Joe's Oakland contact and a Saba-style project) works on the same
inner-loop question in two arms, raw Claude and the FUTON discipline. Each arm
is judged on the out-channel (§3.3): did the person understand their problem
better, decide faster, act? The judging is not done on the artefacts produced.
The judge should be the person, plus someone who did not see which arm was which.

**Joe, seventeenth turn:** "Arxiv mining is kind of Cognisee just with papers
rather than interviews as inputs." This adds futon6 to triple row 20 (collect ·
know-how, with attribution). The difference is the input. Interviews capture what
was never written down. Papers are written down, and citations already attribute
them, but the moves of an argument that a paper does not state are tacit in the
same sense. That is the layer futon6's argument mining goes after. So arXiv mining
already sits in this survey as the Cognisee row, with an input that is easier to
get at scale.

## 3.12 What would a better memory store solve for Joe? (Joe, eighteenth turn)

> War Machine would be our "software factory" with Futon1b as the "memory store";
> [...] there would be a lot of things to learn from these SF folks *on a
> technical level* about how to run a good memory store, but that could be copied
> on a "best of" basis [...] it remains an interesting question what problems a
> better memory store would solve *for me*.

Candidate answers, in SVO form (operator intent · object), each tied to
something that actually happened:

| intent · object | what happened | fidelity check |
|---|---|---|
| collect · prior analysis, across hosts | **This mission's first step (today):** the 10-08 Tech Week analysis existed only in a Claude transcript on metameso and was found by grepping JSONL over ssh. The July landscape TNs were found by `ls`. Neither came from futon1b. | asking "have we looked at X before?" returns the earlier work, wherever it was done |
| constrain · later responses (don't redo work) | the same: without that grep, the 10 lookups would have been redone | repeated research falls |
| verify · a claim against the record | the JevOps/TypeSafe misreading (§2.1) was caught by reading code, not by any recorded prior | a claim can be traced to its source in one step |
| explore · the store, cheaply | futon1b outages (09-29 lock, 09-30 504 storm; memory notes) came from reads the store could not bound | ordinary queries never take the service down |
| collect · non-text evidence | the room's 8 photos and the Cotal screenshot are still unread | images are indexed with what they show |

*Reading:* for Joe, the first two rows are the problem. A better memory store
would mostly be about **recall across where work happened**: other hosts, other
agents' transcripts, Matrix rooms. Storing more, or storing it more cleverly,
matters less. That is a *collect* problem before it is a database problem. Of the
SF entries, HydraDB (time-ordered facts) and ApertureData (images and their
metadata in one place) bear on rows 1 and 5. futon1b's XTDB is already
bitemporal.

**Joe, nineteenth turn:** fixing metameso recall "isn't a priority right now, I'd
fit that under M-hardening.md". *Deferred.* No `M-hardening.md` or `README-hardening.md` exists, and neither ever did (checked futon0 history 2026-10-10). The hardening list Joe remembered is the "Hardening, once it is back" section of `metameso:~/notes/zone-outage-2026-10-06.md` (from line 75; Track A smartd/disk-watch done 10-08), now collected with the other hardening ideas in futon0 `README-hardening.md` (2026-10-10). Related: futon0 `README-firewall.md` (design only), `README-secrets.md`, `README-bare-metal.md`, and zone `~/README-ZONE-1.1.md`. The
nearest existing mission is `futon3c/holes/missions/M-federated-agency-hardening.md`
(cross-box federation, OPEN). Row 1 of the table above, whether metameso
transcripts reach futon1b, is the item to carry over.

> if I was running a virtual startup on Metameso and one on Lucy then eventually
> they could check in here for example, that could be an interesting way to go
> about playing with some of these ideas.

*Recorded as an experiment shape:* two simulated ventures, each on its own box
with its own inner loop (e.g. a Saba-style purchasing co-op on one and a
different dream on the other). Each checks in with zone, which acts as the
co-op of co-ops (§3.7). The experiment would exercise cross-host recall (§3.12
row 1), the outer loop's maintenance and compliance duties (§3.9), and the
§3.11 question in simulation before a real person tries it.

---

# 4. Joe's own problems (Joe, 2026-10-10, twentieth turn)

> if I set out to maintain a free/open clone of whatever is coming out of SF these
> days, that *could* be useful for people IF they could run it on their hardware
> [...] some of the items I found, like Mastra, are already open source, and also,
> I'm not actually *that* keen on creating open source material that I don't
> personally find useful. So, maybe rather than trying to discover the problems
> that Oakland residents might be trying to solve, I can leave that to my friend,
> and think about the problems that I personally am trying to solve, many of which
> are written down in capability stars, and see whether any of the technology I
> learned about could help me with any of those problems. It may also be a good
> time to do some introspection and think about whether I have a full list of
> problems-to-solve and capabilities-to-gain for myself or if my list is somewhat
> prejudiced.

The client-problem line (§3.4–§3.9) is handed to Joe's Oakland contact. This
section turns the survey on Joe.

## 4.1 The held stars, and what from the survey could help

Source: `M-capability-star-map.graph.edn` has 36 capabilities, 22 satisfied, 13
held and 1 active. The rows below are the held and active ones. "Could help"
names survey entries (batch files) that bear on the star, and says whether that
help is something to **take** (open, self-hostable) or to **learn from** (closed).

| star (region) | what it needs | could help | kind |
|---|---|---|---|
| `wm-overnight-unsupervised` | to trust the stack to code while Joe sleeps | Reticle's verdict spec (yes/no/unknown/no-fault; refuses undeclared actions); Zeroshot's separate review-and-repair loop; Smithers' durable runs with rewind; Cotal's leases with fencing tokens | take (all open) |
| `efe-trustworthy-over-starmap` | a ranking whose confidence means something | typed decisions with calibrated probabilities (TypeSafe API; open route via SGLang + Clef, GPU); Braintrust-style A/B comparison | take (Clef) / learn |
| `full-arxiv-mining` | harvest and represent at scale | Crawlee (harvest); HydraDB (time-ordered facts); SGLang (serve open models locally); Paritok (fewer tokens) | take |
| `ai-passes-prelims` | compute, and admission of answers by a checker | SGLang/RadixArk (open-model serving); JevOps' Lean-gate pattern; Artificial Analysis (choosing a model) | take / learn |
| `hypergraph-operator` (t5) | a person-facing capability model | Cognisee (tacit know-how, attributed); Scribe Optimize (mining what people actually do); p2r (self-reported contribution) | learn; talk to Cognisee |
| `symbol-grounding` (t5) | categorical wiring vocabulary, vision | nothing in the survey | — |
| `kit-outbox`, `kit-intake`, `kit-cadence` | the outreach pipeline wired end to end | Composio (mail access); Querit/Apify (lead foraging); Glasser (company and people lookups) | learn; mostly paid |
| `cold-eoi-authored-outbox`, `cold-eoi-sent`, `cold-send-response`, `cold-response-conversion` (t2) | send a cold expression of interest; get a reply; convert it | Hyperbound (practice conversations), and little else | see below |
| `distributed-proofreaders` (active, t3) | structure-first recognition and QA over the maths corpus | Reticle-style verdicts for QA | take (spec) |

**What the table shows:**
- The survey helps most with `wm-overnight-unsupervised`. That fits §3.2: the SF
  crowd is densest on *check* and *orchestrate*, which are exactly that star's
  needs, and the strongest entries there are open.
- **The t2 chain is stuck at its first link** (`cold-eoi-sent`: "the crux,
  n=0"). By the §3.4 filter, a chain at n=0 is probably not a tooling gap. No tool
  in the survey would send the first email. It is a question of intent, or of
  something blocking it, and only Joe can say which.

## 4.2 Is the list prejudiced? (introspection, offered as evidence, not a verdict)

**How stars get onto the map.** Capabilities are *minted* by missions, and the
pudding-prover is the registry (star-map §"Capabilities are MINTED"). So only
something that already has a mission can become a star. That is the same
selection effect as T1: the July probe drew competitors by field, and the star
map draws Joe's goals from the apparatus that already exists. All 36 stars sit in
five regions: the War Machine, mathematics (t3), the hypergraph and interest
network (t5), revenue (t2), and the pudding-prover kit.

**Things Joe has put real effort into recently that have no star** (from session
memory and notes, not from the map):

| candidate | evidence of effort |
|---|---|
| **Joe learns mathematics faster or better** | said today (§3.10). The map has "AI passes prelims" but nothing about Joe learning |
| a working physical setup: phone as workstation, the Kinesis, DeX, voxterm | many sessions (memory: phone-as-primary-workstation, Kinesis, voxterm, phone SSD) |
| infrastructure that survives failure | the 10-06 outage; `README-hardening.md` |
| money: taxes, filings, the Ltd vs sole-trader question, an income after December | four money repos; 10-08: "full time until the end of the year and then need to make a call" |
| teaching and workshops; Peeragogy | 10-08: "lots of workshop experience"; p2r uses Peeragogy vocabulary |
| writing and publishing: the futon7a site, the MMCA paper, papers with Rob | futon7a publish workflow; mmca-clj |
| ChipWits | port (playable). *Joe:* not just fun; it was a source of icons used in Xiang-2000, and a way to think about controlling an agent system |
| fun and play | "or having more fun" (§3.10) |
| friends' projects (the Oakland co-op) | this mission, §3.7–§3.9 |

The first row is the plainest case. Joe wants tech to help *him* learn
mathematics, and what the map records is the stack learning it.

**Question for Joe:** which of these are real goals, and which are maintenance or
pastimes that need no star? Only rows Joe confirms should be minted. The rest
stay here as a record that the question was asked.

**Joe, twenty-first turn:** no new stars yet. "Most of the other items *should*
have a mission and star associated, but maybe right now they are more like
fodder for a futon0/README-nebula.md b/c they are a bit nebulous and stars might
form there." Done: futon0 `README-nebula.md` holds them. An entry leaves the
nebula when a mission opens for it.

---

# 5. The alternative backlog (Joe, 2026-10-10, twenty-second turn)

> what if I use this as a kind of "alternative backlog". So if I run into
> technical problems myself I would be able to query the "alternative backlog"
> and find out what methods would help me do things better or faster. That may
> just be for me as a technologist, rather than me as a business-innovator or
> whatever. So, for example, if I keep having to remind claude agents to use
> bg.py surely there must be a "tool" or "skill" or MCP or somesuch that I could
> get them to use, all of that seems quite standard among the various
> technologies that I looked at and probably the fact that I've rolled my own
> REPL is the only thing to "blame" here.

**How it is queried.** By *problem*, written as an operator intent (verb ·
object, §3). The survey is already indexed this way: the §3 triples, the GLOSS
groups, and the "could help" column of §4.1. A query is "I keep having to
⟨verb⟩ ⟨object⟩ by hand". The answer is the methods in the survey, or in standard
practice, that carry that intent.

**Entry format:**

    ### <recurring problem, in Joe's words>
    - intent: <verb · object>
    - methods: <what the survey or standard practice offers; open? local?>
    - fits FUTON how: <where it would plug in>
    - status: idea | tried | adopted | rejected (why)

## Entries

### "I keep having to remind Claude agents to use bg.py"
- **intent:** constrain · how agents start long-running work (durable work goes
  through `futon3c/scripts/bg.py`, not `run_in_background`, `&`, `nohup` or
  `setsid`; rule in `futon3c/CLAUDE.md` §"Durable background work").
- **Why a reminder fails:** the rule lives in prose in CLAUDE.md, and the model
  reads it or misses it. The same thing happens with every prose rule; it is not
  peculiar to the hand-made REPL.
- **Methods, from strongest to weakest:**
  1. **A hook**, which runs deterministically. A Claude Code `PreToolUse`
     hook on `Bash` sees each command before it runs. If the command sets
     `run_in_background` or uses `nohup`, `setsid` or a trailing `&`, the hook
     denies it with a message telling the agent to use `scripts/bg.py launch "<cmd>"
     --agent <id>`. Agency's pouches run `claude --print` with no
     `--setting-sources` restriction (`agent_pouch.clj`), so a hook in user
     settings would apply to them. Hooks can deny even under
     `bypassPermissions`. *(Verify on one pouch before relying on this.)*
  2. **A skill** (`SKILL.md`): the procedure, loaded when its description
     matches the task. The model still has to choose it, so it is weaker than a
     hook.
  3. **An MCP tool or a native tool** (`bg_launch`): the right action becomes the
     easy one, but nothing stops `&`.
  4. Prose in CLAUDE.md: the current state.
- **Survey parallels:** this is *constrain* enforced at the action layer, the
  same move as Reticle's refusal of actions that were not declared in advance and
  Agentic Fabriq's scoping. Hermes Agent and merlin.build ship hooks for this
  kind of thing.
- **Evidence (claude-11, 2026-10-10):** its Lean session servers, started with
  `run_in_background`, died at pouch teardown. claude-11 first blamed "a 30-minute
  timeout". Joe corrected it, and from then on it used bg.py. Its own memory note
  says Monitor watches die with the pouch as well.
- **status:** **adopted 2026-10-10 as a warning.** `futon3c/scripts/claude-hooks/bg_warn.py`,
  wired in `~/.claude/settings.json` (PreToolUse, `Bash|Monitor`). It warns pouch
  agents only (`FUTON_AGENT_ID` set) through `additionalContext` and never denies.
  The pipe test covered 8 cases (`2>&1`, `&&` and bg.py commands stay silent).
  Proven live: it fired on claude-13's own `sleep 0.1 &`. Next step, if warnings
  turn out not to be enough: deny `run_in_background` only.

### "I don't want to think about bugs, just bell them out" (Joe, 2026-10-10)
- **Joe:** "maybe I should create a variant of report-emacs-bug, like
  report-futon-bug that would log context etc so I don't have to think about the
  bugs, I could just bell them out. that could even go to an agent on lucy or
  metameso, so the bug report would have to be relatively self contained"
- **intent:** delegate · triage of a bug; collect · the context automatically.
- **Methods:** `report-emacs-bug` (Emacs collects its own state into a mail
  buffer); Sentry/Judgment-style capture of traces and breadcrumbs; Meticulous
  replays the session. Nearby in the stack:
  `futon3c/emacs/session-turn-analysis.el` already dispatches through
  `agency_send.py` from Emacs.
- **Design sketch (not built):** `M-x report-futon-bug` asks for one line (typed
  or dictated) and then gathers:
  - the region or the tail of the current REPL buffer
  - `*Messages*`, and `*Backtrace*` if present
  - recent `journalctl --user` lines for futon3c, futon1b and emacs-graph
  - Agency and futon1b health
  - a zone-health summary
  - HEAD sha and dirty status of the futon repos
  - host, time, and Emacs version

  It writes all of that to one self-contained markdown file, **with secrets
  scrubbed** (tokens appear in environments and logs). The file is committed
  somewhere every host can pull, and it is belled with `--mode work`
  (memory: a bell without it runs nothing but still reports done) to a triage
  agent whose brief is: reproduce, locate, propose a fix, bell back.
- **Routing (corrected 2026-10-10):** the roster here *does* list remote
  proxies (`chi-claude-1` and `chi-codex-1` for metameso, `ams-claude-1`). An
  earlier note in this entry said it listed none, which was wrong. zone-health
  still reports the federation peers' `/health` unreachable from zone, so whether
  a bell to a `chi-*` proxy reaches metameso is untested. Joe: "we can sort out
  the routing later".
- **status:** **built 2026-10-10** (Joe: "build it directly"). It consists of:
  - `futon3c/emacs/report-futon-bug.el` (`M-x report-futon-bug`, autoloaded from
    `futon0/contrib/futon-config.el`)
  - `futon3c/scripts/futon_bug_report.py`

  Reports go to `~/notes/futon-bugs/` (outside any repo, so they are never
  published by accident). The prompt offers roster agents, and leaving it empty
  sends nothing. A bell carries the whole report with `--mode work`, from
  `futon-bug`.

  Tested: a batch end-to-end run (≈24 s, mostly zone-health); planted secrets
  scrubbed (GitHub, bearer, `*_TOKEN=`, JSON `access_token`, `sk-`); bell
  arguments checked with `--dry-run`. A real bell has not been sent yet.
  A bug found in testing and fixed: the context was read from inside the temp
  buffer.
