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
  - **`JevOps`**: "TypeSafe / Jev **kernel**, split from Lean Refactor Arena".
    This links him to the **TypeSafe AI "System One / Jev"** entry in the room log
    (10-06), so those two entries are one lead. The kernel is "a gate, not a proof
    authority". Autoencoder and refactoring modules propose Lean candidates, and only
    Lean/Lake admits them. There is also an explicit Lean IR autoencoder, MAB tactic
    selection and a proof-carrying cellular automaton. Its current application is
    US federal law, with source-locked legal theorems (the "Lean and law" from the
    talk).
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

## Explicitly NOT decided here

- Whether any entry gets adopted, cloned or contacted
- Whether the "talk to" bin exists
- Which FUTON offer the positioning is *for* (newcomer, audit, War Machine)
- Whether a public comparison page is wanted at all

## Provenance

- Room log pulled 2026-10-10 via the fumarimo token on zone (132 m.room.message events).
- 10-08 analysis extracted from metameso session `e13cc12c…` by claude-13, 2026-10-10.
