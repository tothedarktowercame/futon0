# Batch B — Dependency / open-core

Encoding date: 2026-08-20. `knowable-at-T` is restricted to evidence available
at, or describing facts visible by, the stated T. Later evidence is used only in
`outcome`, unless a field explicitly distinguishes an early and later state.

## Docker, Inc.

**case-id**  
`docker-inc`

**org**  
Docker, Inc. (the company renamed from dotCloud around the Docker project's
2013 launch). [D1]

**period**  
Docker project/company pivot, 2013 -> enterprise-business sale and
recapitalisation, 2019. [D1][D4]

**node-types**  
Primary: **Dependency** — Docker artifacts became mechanically visible in
build, image, registry, and deployment graphs. [D1][D2]  
Secondary: **Environment** — Docker Hub and the surrounding workflow also let
developers bring their own applications and publishing goals. [D2][D5]

**chain**  
Docker Engine/image format and tooling -> Docker, Inc. as sponsor -> initially
an uncertain payer adjacent to the adopted artifact, later organisations buying
Desktop/Hub/Business governance, security, collaboration, and support ->
`terminal{interest}` at the 2013-14 snapshot (developer portability and
enterprise experimentation did not itself require payment to Docker), later
`terminal{obligation}` only where Docker's own licence requires a paid Desktop
subscription for commercial use above its company-size/revenue thresholds.
[D1][D2][D5] The obligation is therefore a vendor-created licence obligation,
not an external regulatory obligation. [D5]

**oracle**  
There are two non-equivalent oracles. The **adoption oracle** is the dependency
graph itself: downloads, contributors, Dockerfiles/images, and platforms able
to run them; by January 2014 the public scan showed more than 400,000 downloads
and 300 contributors. [D1] The **capture oracle** is whether that adoption
crosses into a Docker-controlled billable entitlement (paid Desktop seats,
private/managed Hub capacity, support, or enterprise controls). [D2][D5]
Conflating these oracles would score Docker's ecosystem success as Docker,
Inc.'s revenue success; the 2019 enterprise-business sale is direct evidence
that the distinction matters. [D4]

**oracle-hardness**  
**Mechanical** for adoption (dependency/download/contribution records) and for
post-2021 licence/seat eligibility; **interpretive** for the original sale,
because widespread use did not identify which adjacent enterprise product a
buyer would procure. [D1][D2][D5]

**distance**  
At T, **3 hops**: open artifact -> incorporated into somebody else's build or
platform -> Docker-adjacent management/support product -> payer line item.
[inference from D1][D2] This was unusually dangerous distance: cloud vendors
and application teams could realise portability and standardisation value
without the value passing through a Docker-controlled commercial checkpoint.
[inference from D1][D4] The later Desktop licence shortened one branch to
**1 hop** (eligible installation -> paid seat), but that was not the founding
capture mechanism. [D5]

**fixture-access**  
**y** — a newcomer could download/use Docker and publish or consume images
without permission; the early download and contributor counts demonstrate an
open attempt surface. [D1]

**knowable-at-T**  
T = approximately March 2014, one year after launch. A blind landscape scan
could see a portable application-container artifact addressing incompatibility
between hosts; more than 400,000 downloads; about 300 contributors; a $15m
financing round intended to expand engineering, sales, and support; and the
possibility of enterprise pilots. [D1] It could also see that the artifact's
adoption oracle was much clearer than its payer oracle: the visible commercial
hypothesis was sales/support around a potentially fundamental architecture,
not a mandatory toll embedded in every Docker use. [inference from D1] Nothing
in this snapshot presupposes the later enterprise sale, Kubernetes competition,
or Desktop licensing.

**outcome**  
**acquired-sideways** — scoped to the 2013-19 commercial thesis: Mirantis
acquired Docker's enterprise business while Docker recapitalised with $35m and
continued under a new direction. [D4] Residual the single outcome enum cannot
express: the company survived and later built a scaled subscription business,
while the open artifact/standard continued to create value outside the firm's
capture boundary; Docker reports more than 20 million developers today. [D2][D3]
Thus “acquired-sideways” describes the first enterprise capture vehicle, not
the death of Docker, Inc. [D4]

**evidence-refs**

- [D1 — January 2014 launch/adoption and financing report](https://www.theregister.com/2014/01/22/docker_series_b_funding/)
- [D2 — Docker pricing and billable product boundaries](https://www.docker.com/pricing/)
- [D3 — Docker company page and current reported developer reach](https://www.docker.com/company/)
- [D4 — Docker's 2019 recapitalisation and Mirantis enterprise-business acquisition](https://www.docker.com/press-release/docker-new-direction/)
- [D5 — Docker's commercial-use subscription threshold](https://www.docker.com/why-docker-/)

## JUXT (makers of XTDB)

**case-id**  
`juxt-xtdb`

**org**  
JUXT, sponsor and developer of XTDB. [J1]

**period**  
XTDB/Crux initial public period, approximately 2019 -> open, 2026. The exact
public-release date is `[recollection - unverified]`; the XTDB team page says a
team member joined in 2019 before initial public release. [J1]

**node-types**  
Primary: **Dependency** — XTDB is an embeddable/deployable database selected as
part of another organisation's system architecture. [J2]  
Secondary: **Requirement** — JUXT consulting can begin from a client's concrete
data-system requirement, but no public tender or mandatory XTDB checklist was
found for this record. [J3][J4]

**chain**  
XTDB database artifact -> JUXT sponsorship/R&D -> design-partner, support, or
consulting payer `[recollection - unverified]` -> `terminal{interest}`: buyers
want immutable history, safe systems of record, or bitemporal queries. [J2]
XTDB documentation describes regulated-data use and reconstructing data “as
understood” at a past system time, but this research found **no citable named
JUXT customer and no specific regulation, audit clause, or contract requiring
that customer to buy XTDB**. [J2][J4] “SQL:2011 defines bitemporal capabilities”
is a technical standard reference, not proof of a buyer obligation. [J2]

**oracle**  
For the artifact, a newcomer can mechanically test whether XTDB preserves and
queries valid-time and system-time history. [J2][J4] For a sale, the check is
whether that behavior satisfies a client's system-of-record acceptance tests
or reconstruction needs; absent a published customer requirement, that sales
oracle remains partly interpretive. [inference from J2][J4]

**oracle-hardness**  
**Mechanical** for temporal database behavior; **interpretive** for the
consulting/design-partner purchase and its alleged regulatory necessity.
[inference from J2][J4]

**distance**  
**2 hops**: database dependency -> client's application/system-of-record
requirement -> JUXT service/support line item. `[recollection - unverified]`
The open-source database is free under MPL, so mere dependency adoption is not
itself a payment event. [J2]

**fixture-access**  
**y** — XTDB is free-to-use/open source and has an interactive quickstart, so a
newcomer can attempt the temporal test without permission. [J2]

**knowable-at-T**  
T = approximately 2020. A blind scan could see an open-source database being
developed by JUXT, aimed at point-in-time/bitemporal queries, with a consulting
company close to the implementation. [J1][J5] It could test the artifact and
hypothesise demand in domains where corrections and historical reconstruction
matter. [J5] It could **not** infer from those capabilities alone that a named
regulator would compel procurement, nor that open-source users would convert
to JUXT clients. [inference from J2][J5]

**outcome**  
**open** — JUXT still leads XTDB R&D, XTDB remains free/open source under MPL,
and the project is seeking design partners. [J1][J2] Revenue, profitability,
and named XTDB-linked consulting customers are `[recollection - unverified]`.

**evidence-refs**

- [J1 — XTDB team: JUXT has led R&D from the beginning](https://xtdb.com/team)
- [J2 — XTDB overview, bitemporal behavior, MPL status, and design-partner invitation](https://docs.xtdb.com/index.html)
- [J3 — XTDB development diary: origin in a Tier-1-bank JUXT project](https://xtdb.com/blog/dev-diary-may-22)
- [J4 — XTDB concepts: database for regulated data](https://docs.xtdb.com/concepts/what-is-xtdb.html)
- [J5 — XTDB 1.x bitemporal vision document](https://xtdb.com/pdfs/vision-doc.pdf)

## Lightbend (formerly Typesafe; now doing business as Akka)

**case-id**  
`lightbend-typesafe-akka`

**org**  
Typesafe, later Lightbend and now doing business as Akka; founded in 2011 by
Martin Odersky, Jonas Bonér, and Paul Phillips. [L1]

**period**  
2011 -> open, 2026. The company says it later consolidated from several
projects/products onto the Akka product and brand. [L2]

**node-types**  
Primary: **Dependency** — Scala/Akka/Play libraries and runtimes sit inside
customer software systems. [L2][L3]  
Secondary: **Requirement** — commercial support, legal protection, and current
production-use licensing can become explicit enterprise procurement items.
[L3]

**chain**  
Akka/Scala ecosystem artifacts -> Typesafe/Lightbend sponsorship -> enterprises
running Akka and buying subscriptions/support -> initially
`terminal{interest}` in supported reactive/distributed application
infrastructure `[recollection - unverified]`; for current Akka releases,
`terminal{obligation}` because production use under BSL requires a Lightbend
commercial licence except for stated grants. [L3] This is a vendor-created
copyright/licence obligation, not an external regulation. [L3]

**oracle**  
The dependency graph mechanically shows Akka inside a production application;
technical load/resilience acceptance is customer-specific
`[recollection - unverified]`. Since the BSL change, production deployment is
also a mechanically inspectable licensing checkpoint. [L3]

**oracle-hardness**  
**Mechanical** for dependency and current licence coverage; **interpretive**
for whether support/productivity justified the original voluntary subscription.
[inference from L3]

**distance**  
Originally **2 hops**: open library -> mission-critical customer application ->
support/subscription budget. `[recollection - unverified]` Current BSL
production licensing shortens the covered branch to **1 hop**: production use
-> commercial licence. [L3]

**fixture-access**  
**y** — development and non-production use are freely allowed under the current
BSL, so a newcomer can attempt the technical test without permission, although
production requires commercial permission outside grants. [L3]

**knowable-at-T**  
T = approximately 2012. A blind scan could see a company founded around the
creators of Scala and Akka, with language/runtime dependencies that developers
could adopt independently. [L1] Commercial support and training were plausible
adjacent products `[recollection - unverified]`, but the scan could not know
whether dependency adoption would convert, whether one project would dominate,
or whether licensing would later change.

**outcome**  
**open** — the company remains operating and has consolidated onto Akka; Akka
production subscriptions and commercial licensing are currently offered. [L2][L3]
Private-company revenue/profitability is `[recollection - unverified]`, so this
record does not upgrade the outcome to `scaled` solely from longevity.

**evidence-refs**

- [L1 — company founding and founders](https://en.wikipedia.org/wiki/Akka.io)
- [L2 — Akka FAQ explaining the Lightbend-to-Akka consolidation/rebrand](https://akka.io/blog/akka-3-frequently-asked-questions)
- [L3 — official Akka BSL and commercial-production licence FAQ](https://akka.io/bsl-license-faq)

## Sourcegraph

**case-id**  
`sourcegraph`

**org**  
Sourcegraph, started in 2013 as a code-search/code-intelligence company. [S1]

**period**  
2013 -> scaled funding milestone, 2021, and still open as an operating company
in 2026. [S2][S3]

**node-types**  
Primary: **Environment** — users bring their own repositories and questions,
then repeatedly search, navigate, understand, and change code inside the
apparatus. [S3][S4]  
Secondary: none. Sourcegraph does **not** genuinely fit Dependency merely
because it reads dependency graphs or repositories: customers do not normally
put Sourcegraph in an application lockfile/build/upstream. [inference from S3]
No new node type is required because Environment captures the user-goal-bearing
apparatus; if Environment were interpreted only as a creative sandbox, the
missing type would be **Tooling/Observability**: an apparatus applied across a
customer-owned corpus whose oracle is reduced search/comprehension/change cost.

**chain**  
Code-search/indexing environment -> Sourcegraph sponsor -> software
organisations buying enterprise deployment over their code estate ->
`terminal{interest}` in developer productivity, code comprehension, security,
and large-scale change; no external obligation to buy Sourcegraph was found.
[S3][S4]

**oracle**  
On a customer's own corpus, the non-manufacturable check is whether Sourcegraph
returns correct, sufficiently fast cross-repository search/navigation results
and helps users answer real code questions. [S3][S4] The corpus and questions
come from outside Sourcegraph; the company cannot manufacture them. [inference
from S3] Conversion to a sale is still an organisation's productivity/security
judgment rather than a public universal score. [inference from S3][S4]

**oracle-hardness**  
**Interpretive** — result correctness/latency can be measured, but whether the
tool saves enough engineering effort to buy is contextual. [inference from S3][S4]

**distance**  
**1 hop**: customer's code corpus and developer workflow -> enterprise tooling
line item. [inference from S3][S4] Unlike an open library sponsor, Sourcegraph
can meter/control the environment sold to the organisation.

**fixture-access**  
**y** for the early/public test: the 2014-era system indexed hundreds of
thousands of public repositories, letting newcomers attempt searches without
permission. [S1] **n** for the decisive private-enterprise corpus test, because
access to that code requires customer permission. [S3]

**knowable-at-T**  
T = approximately 2014. A blind scan could see a large-scale, multi-language
code search and cross-reference engine indexing hundreds of thousands of open
repositories, publicly demonstrated in a Google I/O 2014 talk. [S1] It could
see the external fixture (real repositories and developer questions) and test
search utility. [S1] It could not know that enterprises would buy private-code
deployment, that code search would become mainstream, or that the company would
later raise at a multi-billion-dollar valuation.

**outcome**  
**scaled** — Sourcegraph announced a $125m Series D at a $2.625bn valuation in
2021 and later reported customers including Uber, Lyft, Dropbox, Atlassian,
Yelp, four of five FAANG companies, and four of the top ten US banks. [S2][S4]
Those are company-reported claims, not independently audited revenue. [S2][S4]

**evidence-refs**

- [S1 — Sourcegraph account of its Google I/O 2014 system and public index](https://sourcegraph.com/blog/google-i-o-talk-building-sourcegraph)
- [S2 — Sourcegraph's 2021 Series D and valuation announcement](https://sourcegraph.com/blog/announcing-sourcegraphs-series-d-round)
- [S3 — Sourcegraph company/product description](https://sourcegraph.com/about)
- [S4 — Sourcegraph's reported enterprise adoption and product evolution](https://sourcegraph.com/blog/code-search-to-code-intelligence)

## Redis Labs / Redis Inc.

**case-id**  
`redis-labs-redis-inc`

**org**  
Redis Ltd., founded in 2011 (then Garantia Data), later Redis Labs and Redis
Inc.; the company began contributing to the Redis project shortly after its
founding and became project sponsor in 2015. [R1]

**period**  
2011 -> scaled funding milestone, 2021, with licensing changes through 2025.
[R1][R2][R4]

**node-types**  
Primary: **Dependency** — Redis is a database/cache dependency embedded in
application architectures. [R1][R3]  
Secondary: **Environment** — the managed Redis data platform hosts customers'
own data and application goals. [R2]

**chain**  
Redis database artifact -> initially community/creator, then Redis Labs/Inc.
sponsorship -> customers buying managed Redis, enterprise software/support, or
commercial rights -> `terminal{interest}` for performance, operations, and
support in the early model `[recollection - unverified]`; later
`terminal{obligation}` for uses outside the grants of Redis's source-available
licences, where commercial terms are required. [R3] Redis 8 added AGPLv3 as an
OSI-approved option in 2025, so the exact obligation depends on version and use.
[R4] This is a vendor-created licence obligation, not a cited regulation. [R3][R4]

**oracle**  
The adoption oracle is mechanical: Redis appears in an application's deployed
data path and can be load/latency tested. `[recollection - unverified]` The
capture oracle is whether the user chooses Redis-managed operations/support or
falls outside free licence grants; those are respectively interpretive and
licence-mechanical. [R2][R3]

**oracle-hardness**  
**Mechanical** for dependency, performance, and licence applicability;
**interpretive** for managed-service conversion. [inference from R2][R3]

**distance**  
Originally **2 hops**: open Redis dependency -> production workload/operational
need -> managed-service or support line item. `[recollection - unverified]`
Restricted commercial-use branches later shorten to **1 hop**: covered use ->
commercial licence. [R3]

**fixture-access**  
**y** — a newcomer could run the Redis artifact and test its behavior without
permission; current licence choice and production rights vary by version/use.
[R3][R4]

**knowable-at-T**  
T = approximately 2012. A blind scan could see a company founded in 2011 that
began contributing to the young Redis project shortly afterward. [R1] A hosted
Redis/service business and conversion from users needing operations or support
were plausible `[recollection - unverified]`. The scan could not know that the
company would become project sponsor in 2015, reach a multi-billion-dollar
valuation, or alter licensing years later. [R1][R2][R3]

**outcome**  
**scaled** — Redis Labs announced a $110m financing at a valuation above $2bn
in 2021. [R2] The company continued sponsoring/developing Redis and changed the
licence in 2024, then added AGPLv3 for Redis 8 in 2025. [R3][R4] Valuation is
not proof of profitability; profitability is `[recollection - unverified]`.

**evidence-refs**

- [R1 — Redis founders' account of company founding and project sponsorship](https://redis.io/blog/becoming-one-redis/)
- [R2 — Redis Labs 2021 Series G and valuation announcement](https://redis.io/press/redis-labs-110-million-series-g-led-by-tiger-global/)
- [R3 — Redis 7.4 source-available licensing and commercial terms](https://redis.io/blog/redis-adopts-dual-source-available-licensing/)
- [R4 — Redis 8 addition of AGPLv3](https://redis.io/blog/agplv3/)
