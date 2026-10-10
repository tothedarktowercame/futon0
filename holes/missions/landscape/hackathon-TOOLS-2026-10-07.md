# Sponsor tools: status on metameso (probed 2026-10-07)

CLIs live in `~/hackathon/node/bin`. Use `export PATH=~/hackathon/node/bin:$PATH`.
The JS SDKs are in `~/hackathon/node_modules`. This machine has 4 cores and 7 GB RAM,
no GPU, no Docker and no pip, and it also runs the futon services.

| tool | installed | works now? | needs from Joe |
|---|---|---|---|
| **Querit** | MCP `https://mcp.querit.ai/mcp`, no install needed | **yes, anonymous** (count≤5, no page content); each call returns a `search_id` | optional free key, 1k req/mo, for page content |
| **Apify** | `apify` 1.10.0 | installed | token from console.apify.com/settings/integrations, then `apify login -t …` ($5/mo free) |
| **InstaCloud** | `insta` 0.1.20 | installed | signup (email or GitHub), then `insta login`; 1 free org |
| **Glasser.ai** | `glasser` 0.1.65 | installed; even catalog search needs a key | key from app.glasser.ai/keys ($1 free credit), then `GLASSER_API_KEY` |
| **Kylon** | `kylon` 0.7.4 | installed, `Not logged in` | signup; approve a device code from your phone, or a setup token (Settings → Developer tools) |
| **Tenki** | SDK `@tenkicloud/sandbox` | installed | `TENKI_API_KEY` (the CLI login opens a browser) |
| **RocketRide** | SDK `rocketride` (npm) | client only | engine needs Docker (not here) or a cloud token (`ROCKETRIDE_AUTH`) |
| **BAND** | not yet (Python SDK; no pip here) | no | create an External Agent in app.band.ai → API key; needs Python tooling |
| **AdaL** | not installed (curl\|bash installer) | no | one browser login; then `adal -q … -o json` runs headless |
| **Prelint** | nothing to install | no | GitHub App on the repo + spec markdown; $1/review, "try free" |
| **Paritok** | not installed | no | the local model needs more than this box has; hosted costs $0.30/M tokens |
| **Voiskey** | n/a | no | phone/desktop app only; demo it from the phone |
| **Finch** | n/a | unknown | couldn't identify it; ask the organisers |

Proof of use: keep the IDs each tool returns (Querit `search_id`, Apify run IDs, Insta
deploy URL, Glasser billing entries), and cite them in each agent's p2r report
(`evidence:` / `artifact_produced:`).

## What each tool actually does, and whether to use it (due diligence 2026-10-07)

| tool | mechanically | card? | verdict |
|---|---|---|---|
| Kylon | hosted workspace; an always-on local `kylon gateway` daemon runs your Claude Code/Codex on tasks posted from their cloud; model calls billed via their proxy | **yes**: 7-day trial auto-bills Core $100+/mo, non-refundable. Closed, obfuscated CLI, server-pushed auto-update on by default, trial content not excluded from training | **skip** (or only on a throwaway box) |
| BAND | cloud message bus for agents; `band-peer` plugin plus `jamd` daemon | free tier: 20 agents, 2-week retention | ok if multi-agent messaging is needed |
| AdaL | terminal coding agent, a Claude Code clone | paid plans $20+/mo | skip, duplicates Claude Code |
| RocketRide | MIT-licensed pipeline engine (.pipe JSON), self-hostable | no | ok, but needs Docker (GPU box) |
| Paritok | prompt-compressing proxy in front of your LLM API | hosted: $5 free, no card | self-host only; hosted sees your prompts **and API key** |
| Prelint | GitHub App that reviews PRs against spec docs | no; $10 free, $1/review | ok on a throwaway repo |
| Tenki | disposable VMs, CI runners, PR reviewer (Luxor) | $50 free without a card | ok if a sandbox is needed; don't add a card |
| Voiskey | phone dictation app; audio goes to Google etc. | freemium/ads | skip |
| Finch | crypto-linked agent-skill marketplace (Avalanche), vague | ? | avoid |
| Apify | mature scraping platform | no; $5/mo free | **use** |
| Querit | web search API | anonymous works; 1k/mo free key | **use (working)** |
| Glasser | prepaid pay-per-call data marketplace | $1 free | optional |
| InstaCloud | Postgres, storage and compute via API; Apache-licensed CLI | free tier, card unclear | likely use for deploy |

Note: no public page links these sponsors to "Crewbase Collective". A similar "Zero Human
Company Hackathon" run by Terac had BAND as a sponsor. Check with the organisers.

## Update 2026-10-07: organiser announcement
**Mandatory core stack: AdaL, Kylon, BAND, RocketRide.** Read the docs for workflow details.
- Kylon coupon code (from the organisers): `HACKATHONSF07`
- Still check at checkout what the coupon covers, and whether the plan auto-renews after it.
