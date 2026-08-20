# Batch F — AI-adjacent companies (prospective preregistration)

Encoding date: 2026-08-20. This batch uses schema v1 plus `entity-type` and
`prediction`. Unlike batches A–D, `knowable-at-T` means what a public landscape
scan can establish on the encoding date. Predictions are frozen claims to be
resolved later by appending evidence, never by editing these records.

## LangChain / LangSmith

**case-id**  
`langchain-langsmith`

**org**  
LangChain, Inc.; this record separates its open-source LangChain/LangGraph
frameworks from LangSmith, its commercial agent-engineering platform. [LC1]

**entity-type**  
`company`

**period**  
Open-source project launched October 2022; company formed February 2023 ->
open, 2026. [LC1][LC2]

**node-types**  
Primary: **Dependency** — the open-source frameworks are imported into other
teams' agent applications, making adoption visible in package/dependency
graphs. [LC1][LC3]  
Secondary: **Environment** — LangSmith lets teams bring their own agents,
traces, datasets, evaluation criteria, and deployment goals. [LC1][LC4]

**chain**  
MIT-licensed framework dependency -> LangChain sponsorship and developer
mindshare -> separate LangSmith observability/evaluation/deployment service ->
team or enterprise software/infrastructure budget -> `terminal{interest}`.
[LC3][LC4] LangSmith offers annual invoicing, infosec review, SLAs, access
controls and self-hosted/hybrid options at Enterprise tier, but this scan found
no external regulation, audit clause, or contract requiring a buyer to procure
LangSmith specifically. [LC4] The framework and paid product are therefore two
different checkpoints; adoption of one is not payment for the other.
[inference from LC1][LC3][LC4]

**oracle**  
The adoption oracle is mechanical: the framework appears in package and build
graphs, while LangChain reports more than one billion open-source downloads.
[LC1] The capture oracle is different: billable LangSmith seats, trace/storage
usage, deployment compute, or an Enterprise order. [LC4] Whether LangSmith
improves an agent enough to justify renewal is customer-defined and
interpretive. [inference from LC4]

**oracle-hardness**  
**Mechanical** for Dependency adoption and metered platform usage;
**interpretive** for quality improvement and renewal. [inference from LC1][LC4]

**distance**  
**2 hops** on the principal route: OSS dependency -> a team chooses LangSmith
for reliability operations -> paid platform line item. [inference from
LC1][LC4] Direct LangSmith users have a one-hop usage-to-billing route, but the
large OSS population can realise framework value without entering it. [LC3][LC4]

**fixture-access**  
**y** — a newcomer can install the MIT-licensed frameworks and can start
LangSmith on a free Developer plan without procurement permission. [LC3][LC4]

**knowable-at-T**  
As of 2026-08-20, the company describes an explicit open-source-to-commercial
learning loop: develop open projects, incorporate what is learned into
LangSmith, and sell observability, evaluation, deployment and related agent
engineering services. [LC1] It reports 35% Fortune 500 usage, more than one
billion OSS downloads, and more than one billion LangSmith events per day;
these are company-reported figures, not independently audited here. [LC1]
Public pricing is $0 for Developer, $39/seat/month plus usage for Plus, and
custom Enterprise pricing. [LC4] LangGraph's published repository is MIT
licensed. [LC3] Existing MIT releases cannot be converted retroactively into
a paid-use obligation; any manufactured-obligation strategy would require a
new controlled component, future licence change, or commercial platform
checkpoint. [inference from LC3]

**outcome**  
**open** — the company and both the OSS and paid product lines are active.
[LC1][LC4] Revenue and profitability are `[recollection - unverified]`.

**prediction**  
Given a strong Dependency oracle but a two-hop gap to a separately purchased
Environment, the schema predicts that LangChain will capture value primarily
through LangSmith enterprise/usage contracts, not by turning existing
MIT-licensed LangChain or LangGraph adoption itself into a toll. Capture will
remain materially narrower than OSS adoption through **end 2028**. This claim
is falsified if, by then, LangChain publicly reports that a majority of active
framework-using organisations are paying LangChain, or introduces and
successfully enforces a licence/payment checkpoint required for ordinary use
of the dominant framework line. [inference from LC1][LC3][LC4]

**evidence-refs**

- [LC1 — LangChain history, product split, and company-reported usage](https://www.langchain.com/about)
- [LC2 — formation date and early project history](https://www.langchain.com/blog/three-years-langchain)
- [LC3 — LangGraph MIT licence](https://github.com/langchain-ai/langgraph/blob/main/LICENSE)
- [LC4 — LangSmith plans, metering, procurement and deployment options](https://www.langchain.com/pricing)

## Braintrust

**case-id**  
`braintrust-ai-evals`

**org**  
Braintrust, the AI observability and evaluation platform introduced publicly
by founder Ankur Goyal in September 2023. [BT1]

**entity-type**  
`company`

**period**  
2023 -> open, 2026. [BT1][BT4]

**node-types**  
Primary: **Environment** — teams bring their own applications, traces,
datasets, scorers, experiments and production-quality goals into Braintrust's
apparatus. [BT1][BT2]  
Secondary: **Requirement** — an individual customer's release criteria,
retention policy, security controls or acceptance tests can become a concrete
checklist, but the fixture is supplied by that customer rather than by an
outside public authority. [BT2][BT3]

**chain**  
Customer AI interaction/trace -> Braintrust evaluation and observability
workspace -> AI engineering/product team -> platform, data, or developer-tools
budget -> `terminal{interest}`. [BT1][BT2] Enterprise features support SOC 2,
HIPAA agreements, GDPR-oriented deployment, SSO and SLAs, but those obligations
govern the customer's handling of data; they do not require Braintrust as the
vendor. [BT2][BT3]

**oracle**  
Within a configured project, score results, trace volume and regressions are
mechanically recorded. [BT2] The decisive oracle — whether chosen scorers
represent user value well enough to gate a release — remains interpretive and
customer-authored. [inference from BT1][BT2]

**oracle-hardness**  
**Interpretive** overall: execution and scoring are mechanical after setup,
but the customer selects the dataset, judge and quality threshold.
[inference from BT1][BT2]

**distance**  
**1 hop** for an AI-native team: its production/evaluation workload consumes
processed-data and score allowances that map directly to a Braintrust plan.
[BT2] **2 hops** where an enterprise platform owner must first persuade
security or procurement that a dedicated eval system is preferable to internal
tools. [inference from BT2][BT3]

**fixture-access**  
**y** for the public attempt: Starter is free without a credit card and permits
traces and evals. [BT2] **n** for a decisive enterprise test involving private
production traffic, customer data and internal release criteria.
[inference from BT2][BT3]

**knowable-at-T**  
As of 2026-08-20, public plans include a free Starter tier, a $249/month Pro
tier, and custom Enterprise terms; usage is metered by processed data, scores
and retention. [BT2] Braintrust says Notion, Stripe, Vercel, Airtable,
Instacart, Zapier, Coda and The Browser Company use its product, and announced
an $80m Series B in February 2026. [BT4][BT5] Those are vendor-published
customer/funding claims, not audited revenue. Enterprise deployment and
compliance features create procurement fit, but no buyer obligation specific
to Braintrust was found. [BT2][BT3]

**outcome**  
**open** — product, pricing and company announcements remain active. [BT2][BT4]
Revenue and profitability are `[recollection - unverified]`.

**prediction**  
Given an Environment with a short usage-to-billing path but an interpretive,
customer-manufactured oracle, the schema predicts that Braintrust will capture
durable value among AI-native teams with continuous production traffic, while
broader enterprises will remain vulnerable to internal tooling and bundled
observability substitutes. By **end 2028**, it should retain a paid standalone
platform and publish continuing named production customers, but not become a
generally mandatory AI-release checkpoint. Falsification is either shutdown or
sale followed by product sunset, or evidence by end 2028 that an external
standard/regulator broadly requires Braintrust specifically (or its proprietary
oracle) for AI release approval. [inference from BT1][BT2][BT3]

**evidence-refs**

- [BT1 — 2023 public introduction and product thesis](https://www.braintrust.dev/blog/reliable-ai)
- [BT2 — current plans, metering and enterprise features](https://www.braintrust.dev/pricing)
- [BT3 — deployment, security and compliance architecture](https://www.braintrust.dev/docs/security)
- [BT4 — 2026 Series B announcement](https://www.braintrust.dev/blog/announcing-series-b)
- [BT5 — 2024 named-customer and Series A announcement](https://www.braintrust.dev/blog/announcing-series-a)

## Humanloop

**case-id**  
`humanloop-evals-platform`

**org**  
Humanloop, formerly a commercial LLM evaluation and prompt-management
platform; its own documentation says it joined Anthropic and the platform was
sunset in 2025. [HL1][HL2]

**entity-type**  
`company`

**period**  
Founding date `[recollection - unverified]` -> acquisition and platform sunset,
2025. [HL1]

**node-types**  
Primary: **Environment** — product teams brought prompts, agents, logs,
evaluations and expert feedback into the platform. [HL2][HL3]  
Secondary: **Requirement** — enterprise customers could encode domain-expert
evaluation and compliance requirements, including sensitive-data deployment
needs. [HL3]

**chain**  
Customer prompt/agent and evaluation data -> Humanloop collaboration/evaluation
platform -> enterprise AI product team -> software budget ->
`terminal{interest}`; acquisition then redirected value to the acquirer while
the standalone payer route ended. [HL1][HL3] Humanloop advertised HIPAA-related
deployment support, but this scan found no external rule requiring Humanloop
specifically. [HL3]

**oracle**  
Evaluation runs and expert annotations supplied repeatable evidence inside the
platform. [HL2][HL3] Whether those evaluations represented product quality was
set by the customer and therefore interpretive. [inference from HL2][HL3]

**oracle-hardness**  
**Interpretive** — mechanics could run a scorer, but customers supplied the
domain judgment and release meaning. [inference from HL2][HL3]

**distance**  
Before acquisition, **1–2 hops**: evaluation workflow -> product team's need
for collaboration/reliability -> paid enterprise platform. [inference from
HL2][HL3] After acquisition, there is no standalone Humanloop billing hop:
billing stopped July 30, 2025 and the platform closed September 8, 2025. [HL1]

**fixture-access**  
Historical public trial/access conditions are `[recollection - unverified]`.
As of T, **n**: the platform and its stored data are permanently inaccessible
after the sunset date. [HL1]

**knowable-at-T**  
As of 2026-08-20, Humanloop's documentation explicitly records acquisition,
billing cessation, mandatory data export and platform sunset. [HL1] Archived
pricing copy describes an enterprise LLM-evals product and dedicated HIPAA
deployment options, but no current purchasable Humanloop service. [HL3]
The official Humanloop changelog identifies the event as “Humanloop joins
Anthropic.” [HL1]

**outcome**  
**acquired-sideways** — the organisation's team or assets found an acquisition
route, while the standalone product and customer data were shut down. [HL1]
This enum does not distinguish a successful talent/IP acquisition from a weak
standalone capture outcome. [inference from HL1]

**prediction**  
Given an Environment whose standalone payer route has already terminated, the
schema predicts no renewed independent Humanloop subscription revenue through
**end 2027**; any remaining value will be realised inside the acquirer through
team, know-how or product integration rather than through the frozen platform.
This is falsified if a Humanloop-branded paid platform becomes generally
available again, with new independent customer billing, by end 2027.
[inference from HL1][HL2]

**evidence-refs**

- [HL1 — acquisition notice, billing stop, export requirement and sunset](https://humanloop.com/docs/changelog/2025/08)
- [HL2 — final-period prompt/agent and evaluation functionality](https://humanloop.com/docs/changelog/2025/05)
- [HL3 — archived product positioning and enterprise/HIPAA offering](https://humanloop.com/pricing)

## Tessl

**case-id**  
`tessl-spec-driven-ai-coding`

**org**  
Tessl, associated publicly with Guy Podjarny and focused on spec-driven,
AI-native software development. [TS1][TS3]

**entity-type**  
`company`

**period**  
Founding date `[recollection - unverified]` -> open, 2026. [TS2][TS3]

**node-types**  
Primary: **Environment** — developers bring their own software goals and use
Tessl agents, specifications, plugins, skills, reviews and evals. [TS1][TS2]  
Secondary: **Dependency** — shared plugins and skills can become installed
inputs to team coding workflows, and Enterprise can mandate organisation
standard skills. [TS2] This is an emerging dependency position, not evidence
that Tessl is already unavoidable. [inference from TS2]

**chain**  
Developer intent/specification and reusable skill -> Tessl workspace/agent ->
team standardisation and governance -> usage credits or enterprise platform
fee -> `terminal{interest}`. [TS1][TS2] Enterprise controls can mandate skills
inside a customer that has voluntarily adopted Tessl, but no outside body
requires that adoption. [TS2]

**oracle**  
At the local level, an approved specification and its generated implementation
can be checked against tests, reviews and eval scenarios. [TS1][TS2] Whether
the specification captures the intended product remains interpretive and
customer-authored. [inference from TS1]

**oracle-hardness**  
**Interpretive**, with mechanical subchecks: test/eval execution is mechanical,
while intent-to-spec correctness and organisational standardisation are human
judgments. [inference from TS1][TS2]

**distance**  
**1 hop** for self-serve use: agent/review/eval actions consume shared credits
under a $100/month Team plan. [TS2] **2 hops** for enterprise capture: useful
skills/spec workflow -> organisation-wide governance decision -> platform fee
and annual credit commitment. [TS2]

**fixture-access**  
**y** — the Free plan requires no card, includes 1,000 monthly credits, and
permits plugin/skill publishing, evals and agent use. [TS2]

**knowable-at-T**  
As of 2026-08-20, Tessl documents spec-driven development as requirements and
specification approval before code generation. [TS1] Pricing is Free,
$100/month Team, and custom Enterprise, with credits consumed by reviews,
evals and agent runs; publishing/installing plugins is free. [TS2] Enterprise
adds mandatory skills, audit logs, inventory, analytics, SSO and deployment
options. [TS2] Tessl publicly described a $125m funding milestone in November
2024; this is a company-published claim. [TS3] The earlier candidate file's
description of “specifications drive code” is consistent with current Tessl
documentation; its London/remote footprint was not freshly established here
and remains `[recollection - unverified]`.

**outcome**  
**open** — current documentation and purchasable plans are active. [TS1][TS2]
Revenue, customer count and profitability are `[recollection - unverified]`.

**prediction**  
Given an accessible Environment and only an emerging Dependency position, the
schema predicts that Tessl will capture value first from teams purchasing
agent/eval usage and governance, not from a universal specification standard.
By **end 2028**, it should either demonstrate repeat organisation-wide use of
mandated Tessl skills/specifications or remain a voluntary coding-tool
subscription with `terminal{interest}`. The prediction is falsified if, by
then, a Tessl-controlled spec or skill format becomes an externally required
industry/procurement standard, or if the paid product closes without evidence
of repeat team/enterprise adoption. [inference from TS1][TS2]

**evidence-refs**

- [TS1 — Tessl's spec-driven development workflow](https://docs.tessl.io/common-workflows/spec-driven-development-with-tessl)
- [TS2 — current plans, credits and enterprise governance](https://tessl.io/pricing)
- [TS3 — Tessl's company-published $125m funding discussion](https://tessl.io/podcast/tessl-raises-125m-to-build-ai-native-development/)

## Cognition / Devin

**case-id**  
`cognition-devin`

**org**  
Cognition, operator of Devin, which it describes as an autonomous software
engineer working in customer codebases and engineering tools. [CG1]

**entity-type**  
`company`

**period**  
Devin public launch, March 2024 -> open, 2026. [CG1][CG2]

**node-types**  
Primary: **Requirement** — a customer supplies a ticket, backlog item, refactor
or migration requirement; the returned artifact is code/PR plus test results.
[CG1][CG3]  
Secondary: **Environment** — teams bring their own repositories and goals into
the continuing Devin workspace, integrations and enterprise account. [CG1][CG4]
SWE-bench was a launch-time **Benchmark** signal, but it is not the primary
commercial node because customers pay for work in their own repositories.
[CG2][inference from CG1]

**chain**  
Customer engineering requirement -> Devin session producing/reviewing code ->
customer engineering organisation -> agent/developer-productivity budget ->
`terminal{interest}`. [CG1][CG3][CG4] The work may satisfy contractual product
requirements downstream, but this scan found no external party obliging an
engineering team to purchase Devin specifically. [inference from CG1][CG3]

**oracle**  
Repository tests, builds, review acceptance and merged PRs provide hard local
checks that Cognition does not solely control. [CG1][CG3] Productivity and
human-equivalent value across varied work remain interpretive even when usage
is metered. [inference from CG3][CG4]

**oracle-hardness**  
**Mechanical** for task-level build/test/merge acceptance; **interpretive** for
portfolio-level productivity and renewal. [inference from CG1][CG3]

**distance**  
**1 hop**: customer ticket/repository task -> metered Devin plan or enterprise
account. [CG3][CG4] This is the shortest payer path in the batch, because the
sold artifact is engineering execution rather than a tool for evaluating some
other AI product. [inference from CG1][CG3]

**fixture-access**  
**y** for a newcomer using the current Free/Pro self-serve entry; **n** for the
decisive enterprise test on private repositories, internal tooling and real
backlogs. [CG3][inference from CG1]

**knowable-at-T**  
As of 2026-08-20, Cognition presents Devin as planning, writing, testing and
shipping code inside existing customer tools, and says it is deployed at large
institutions. [CG1] Its April 2026 self-serve announcement lists Free and a
$20/month Pro entry while retiring the earlier $500/month Team threshold.
[CG3] Enterprise accounts support central membership and billing, with
usage-based overages. [CG4] Cognition's blog lists partnerships with Cognizant
and Infosys and a London office; these are company-published claims. [CG5]
The prior candidate file called Cognition the canonical autonomous-engineer
case and an agentic-coding/closing-the-loop fit; current product material agrees
with that characterization, so no disagreement is recorded. [inference from
CG1 and the prior-art file]

**outcome**  
**open** — product, self-serve pricing and enterprise activity remain active.
[CG1][CG3][CG5] Revenue and profitability are `[recollection - unverified]`.

**prediction**  
Given a Requirement node, hard repository-level oracles and a one-hop billing
path, the schema predicts stronger direct value capture than for the eval-only
Environments in this batch: Cognition should sustain paid self-serve and
enterprise sales through **end 2028**, with pricing increasingly tied to
accepted engineering work or measured usage. It does not predict monopoly or
profitability. Falsification is product shutdown/acquisition followed by Devin
sunset, or public evidence by end 2028 that paid adoption cannot persist unless
Devin is bundled free with another product despite continued task-level test
success. [inference from CG1][CG3][CG4]

**evidence-refs**

- [CG1 — Cognition product and company description](https://cognition.com/)
- [CG2 — March 2024 Devin launch and SWE-bench claim](https://cognition.com/blog/introducing-devin)
- [CG3 — April 2026 self-serve pricing change](https://cognition.com/blog/new-self-serve-plans-for-devin)
- [CG4 — enterprise accounts and usage billing](https://cognition.com/blog/jan-25-product-update)
- [CG5 — Cognition's dated company/partnership announcements](https://cognition.com/blog)

## Cross-case preregistration note

All five entities fit at least one of the six v1 node types; no seventh row was
required. The closest pressure on the schema is an **evaluation/observability
apparatus**: Braintrust and Humanloop are encoded as Environment because users
bring their own goals, but their distinctive output is evidence about another
artifact rather than completion of the user's goal itself. If later grading
shows that this distinction predicts capture independently, the missing row
would be **Assurance** — fixture: a system plus quality claim; oracle: repeatable
evaluation/audit evidence; business model: observability, certification or
risk assurance. That row was not added here because v1 is frozen.

The terminal result is uniform: every commercial chain terminates in
`interest`, not an externally imposed obligation. Compliance features shorten
procurement and make products acceptable, but none of the cited material makes
a named buyer legally or contractually obliged to buy one of these specific
vendors. [inference from LC4][BT3][HL3][TS2][CG1]
