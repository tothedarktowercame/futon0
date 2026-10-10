# Gloss: the made-up names, in a few plain verbs

Each entry gets a few verbs and their object, which is what the thing does. The groups
are named by their leading verb. Read this table to look up a name; the batch
files hold the evidence.

**Caveat (T8 in the mission):** many of these verbs are the vendors'
human-sounding words ("remember", "learn", "decide"). They are not the
mechanism, and they are not the operator's intent. Read "remember" as "store
facts in a graph or vector DB and inject the matches into the session".

Open: **✓** open and self-hostable · **½** open SDK/library, closed service ·
**✗** closed or paid · **?** unclear.

## coordinate: get several agents working together

| name | verbs | open |
|---|---|---|
| Cotal | *discover, hand off, wake, record* (agents across vendors, peer-to-peer over NATS) | ✓ Apache-2.0 |
| BAND | *find, join rooms, message* (agents from different frameworks) | ½ MIT SDK |
| Kylon | *assign, run, bill* (team tasks to coding agents on your machines) | ✗ |
| Delegance / Alinery | *steer, checkpoint, approve* (several agents through one long task) | ✓ Apache-2.0 |
| ipfs_accelerate (endomorphosis) | *lease, validate, admit, receipt* (many agents, one codebase, prover-gated) | ✓ AGPL-3.0 |

## orchestrate: run an agent through a workflow

| name | verbs | open |
|---|---|---|
| Smithers | *plan, approve, run, rewind* (issue → reviewed change; durable) | ✓ MIT |
| RocketRide | *parse, embed, chain* (document/LLM dataflows) | ✓ MIT |
| Mastra | *build, remember, trace* (TypeScript agent library) | ✓ Apache-2.0 (minus `ee/`) |
| merlin.build | *plan, phase, execute, verify* (packaged workflow for Claude Code/Codex) | ? MIT claimed, repo private |
| Hermes Agent | *chat, remember, learn skills* (always-on personal agent on your server) | ✓ MIT |
| MiniMax Agent | *plan, split, produce* (long job → finished artifact) | ½ MIT CLI |
| Solid | *provision, spend, operate* (agent with its own machines and money) | ✗ |
| ZooWork | *capture know-how, deploy* (expert → hosted agent) | ✗ |

## write code: coding agents themselves

| name | verbs | open |
|---|---|---|
| AdaL | *code, launch, promote* | ✗ |
| Command Code | *code, learn your style* | ✗ |
| Agent Deck | *store, serve* (one set of procedures/keys to every coding agent) | ✓ MIT |

## check: did the agent's work actually work?

| name | verbs | open |
|---|---|---|
| Reticle | *drive, observe, verdict* (yes/no/unknown/no-fault, on a running app) | ✓ Apache-2.0 spec/SDK |
| Zeroshot (Open Engine) | *review, reject, repair* (separate agents check before delivery) | ✓ MIT |
| Tinder's Merlin | *gate, prove against ticket* | ✗ blog only |
| Prelint | *compare to decisions, flag drift* (PR vs what the team agreed) | ✗ |
| Meticulous | *record, replay, diff* (frontend sessions) | ½ ISC client |
| JevOps (endomorphosis) | *propose, admit via Lean* | ✓ AGPL-3.0 |

## evaluate: measure and trace agent behaviour

| name | verbs | open |
|---|---|---|
| Judgment Labs | *trace, cluster failures, regress* | ½ Apache-2.0 SDK |
| Braintrust | *score, compare, monitor* | ½ Apache-2.0 SDKs |
| Artificial Analysis | *benchmark, rank* (models and providers) | ✗ data |
| aimock | *fake, replay* (LLM/MCP/vector APIs for tests) | ✓ MIT |

## govern: who may an agent act as, and who can prove it

| name | verbs | open |
|---|---|---|
| Agentic Fabriq | *scope, broker credentials, audit* | ½ Apache-2.0 SDK |
| AIUC-1 | *certify, map to regulations* | ? |
| Advanced AI Society (Proof-of-Control) | *attest, checkpoint, allow/deny/modify/escalate* | ✓ Apache-2.0 |
| Mcp-Plus-Plus (endomorphosis) | *address by content, delegate capability, replay* | ? no licence |
| p2r | *self-report, validate* (each participant's own contribution) | ✓ BSD-3 |

## decide: small, typed judgements in place of free-text answers

| name | verbs | open |
|---|---|---|
| TypeSafe (System One / Jev) | *classify, score, calibrate* | ½ MIT SDK; open substitute via SGLang + Clef |
| o-machine | *explain causes* (from observed actions) | ✗ |

## remember: memory stores for agents

| name | verbs | open |
|---|---|---|
| HydraDB | *link facts, timestamp, recall* | ✓ AGPL-3.0 core |
| ApertureData | *store media + vectors + graph, query once* | ✗ eval-only image |
| ZeroDB | *bundle stores behind one API* | ½ MIT local |
| Cognisee | *capture tacit know-how, attribute* | ? mostly white paper |

## connect: give an agent the outside world

| name | verbs | open |
|---|---|---|
| Composio | *authenticate, call SaaS APIs* | ½ MIT SDK |
| TinyFish | *browse, fill, extract, watch* (real websites) | ½ MIT AgentQL |
| Apify | *crawl, scrape, schedule* | ½ Crawlee Apache-2.0 |
| Querit | *search the web* | ✗ (anonymous tier works) |
| Glasser | *resell data APIs* | ✗ |

## host: somewhere for the agent's code to run

| name | verbs | open |
|---|---|---|
| Tenki | *sandbox, fork, pause* | ✗ |
| InstaCloud | *provision db/storage/compute per branch* | ✓ Apache-2.0 OSS server |
| RadixArk / SGLang | *serve, train open models* | ✓ Apache-2.0 |

## economise: send fewer tokens

| name | verbs | open |
|---|---|---|
| Paritok | *compress context* | ✓ Apache-2.0 code + weights |

## show: put the agent in front of a person

| name | verbs | open |
|---|---|---|
| AG-UI | *stream events* (agent → any front end; 31 event types) | ✓ MIT |
| CopilotKit | *embed, share state* (agent in a web app) | ✓ MIT |
| Voiskey | *dictate, rewrite* | ✗ |

## federate: data and identity that move between hosts

| name | verbs | open |
|---|---|---|
| atproto | *sign, host, move, subscribe* | ✓ MIT/Apache-2.0 |
| Germ | *encrypt, message* (on an atproto identity) | ½ MIT libs |

## not tools

| name | what it is |
|---|---|
| Scribe Optimize | *mine* workflows to find what to automate (enterprise) |
| Hyperbound | *roleplay, score* sales calls |
| Immersive Commons | a coworking floor run through 273 agent tools |
| Plank | rents AI engineers |
| Finch | crypto marketplace for agent skills |
| tracn | waitlist; unknown |
| Tatras | AI consultancy (market signal only) |
