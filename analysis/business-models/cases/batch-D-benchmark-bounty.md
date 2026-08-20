# Batch D — Benchmark / Bounty / sideways exits

Encoding date: 2026-08-20. Each `knowable-at-T` snapshot is restricted to the
organisation's founding plus approximately one year; later evidence appears
only in `outcome` or in explicitly labelled later-state clauses.

## Kaggle

**case-id**  
`kaggle`

**org**  
Kaggle, launched in April 2010 as a platform for data-science competitions.
[K1]

**period**  
2010 -> Google acquisition, 2017. [K2]

**node-types**  
Primary: **Benchmark** — a host supplies a task, data, scoring metric, and
prize; competitors submit against a scorer they do not control. [K3][K4]  
Secondary: **Environment** — Kaggle operates the apparatus in which hosts bring
problems and participants repeatedly compete, learn, and collaborate. [K1][K5]
The Benchmark row describes each competition better than it describes Kaggle's
own commercial role: Kaggle is the **operator of Benchmark nodes**, not usually
a competitor selling the winning capability. [inference from K3][K5]

**chain**  
Competition dataset/metric/leaderboard -> Kaggle platform plus the named
competition host -> the host pays prizes and, for featured competitions,
variable Kaggle service fees -> `terminal{interest}` in obtaining strong
solutions, engaging a specialist community, research, recruitment, or
promotion; no regulation or contract requiring an organisation to host on
Kaggle was found. [K3][K5] Competitors are prize recipients rather than
Kaggle's payer, and community hosts can use a no-cost self-service tier. [K4][K5]
Google later paid the shareholders as acquirer, which is a distinct chain from
competition operations. [K2]

**oracle**  
Within a competition, the held-out score and ranking are mechanical and
non-manufacturable by competitors; the host defines the metric before launch.
[K3][K4] For Kaggle's sale to a host, the check is whether the platform attracts
enough qualified teams and produces useful solutions; this is observable but
not reducible to one public score. [inference from K5]

**oracle-hardness**  
**Mechanical** for competitor ranking; **interpretive** for whether operating
the benchmark created enough sponsor value to justify Kaggle's fee. [inference
from K3][K5]

**distance**  
For a competitor, **far / 3 hops**: scored submission -> leaderboard reputation
or winning solution -> demonstrated capability/employment/licensing opportunity
-> payer line item. `[recollection - unverified]` For Kaggle itself, **1 hop**:
host's need for a managed competition -> platform-services line item. [K5]
The record shape has only one `distance`, so it cannot represent both sides
without this split.

**fixture-access**  
**y** for public competitions, whose rules, data, metric, and submission path
are openly attemptable; Kaggle also supports private/restricted competitions,
so access is not universal. [K3][K4]

**knowable-at-T**  
T = approximately April 2011. A blind scan could see a young online platform
connecting organisations' real-world prediction problems with outside data
scientists through scored competitions. [K1] It could see the unusually hard
oracle: entrants did not hold the host's scorer, and prizes were specified by
competition rules. [K3] It could hypothesise that hosts would pay for problem
design, community reach, and administration `[recollection - unverified]`, but
could not know whether this would become a durable line item, a recruiting
market, a learning community, or an acquisition target.

**outcome**  
**acquired-sideways** — Google confirmed its acquisition of Kaggle in 2017.
[K2] “Sideways” is relative to the schema's proposed Benchmark path of distant
capability licensing: the company itself captured value by owning the benchmark
environment/community, not by licensing contestants' capabilities. [inference
from K2][K5] Kaggle continued inside Google and currently reports 32m+ members
and 33,000 competitions. [K5]

**evidence-refs**

- [K1 — Kaggle history: 2010 launch as a competition platform](https://www.kaggle.com/competitions/meta-kaggle-hackathon/writeups/kaggle-chronicles-15-years-of-competitions-communi)
- [K2 — contemporary confirmation of Google's 2017 acquisition](https://www.axios.com/2017/12/15/google-buys-machine-learning-startup-releases-video-search-tool-1513300825)
- [K3 — Kaggle competition terms defining data, metric, prize, and winner rights](https://www.kaggle.com/terms)
- [K4 — Kaggle competition documentation: host-defined scoring rubric](https://www.kaggle.com/docs/competitions)
- [K5 — Kaggle host offering, pricing boundary, and current platform counts](https://www.kaggle.com/host)

## HackerOne

**case-id**  
`hackerone`

**org**  
HackerOne, founded in 2012 as a hacker-powered security / bug-bounty platform.
[H1]

**period**  
2012 -> open, 2026. HackerOne reported reaching $100m in bounties in 2020 and
currently reports more than 1,300 customer companies. [H1][H2]

**node-types**  
Primary: **Bounty** — a program publishes scope and reward terms; a researcher
submits a vulnerability; the customer accepts/triages it and awards the bounty.
[H3]  
Secondary: **Environment** — organisations and researchers bring recurring
security goals into HackerOne's coordination, reputation, triage, and payment
apparatus. [H1][H2]  
**Schema gap:** HackerOne is not primarily a bounty hunter. It monetises the
operation of many Bounty nodes. The missing node type is **Market/Exchange**:
outside principals post priced acceptance conditions, outside suppliers submit,
and the operator earns for matching, workflow, trust, triage, and settlement.
[inference from H1][H3]

**chain**  
Vulnerability report -> affected organisation sponsors a program through
HackerOne -> that organisation pays HackerOne for platform/managed access and
pays the researcher an accepted bounty -> `terminal{interest}` in reducing
security risk and coordinating disclosure. [H1][H3][H4] A valid report creates
a program-defined payment expectation, but no general external law requiring
the customer to run or pay through HackerOne was found; therefore this record
does not elevate the terminal to obligation. The US Senate record states that
the customer decides the bounties. [H4]

**oracle**  
At the bounty level, the customer's reproduction, scope, severity, and
acceptance decision gate payment; this combines mechanical reproduction with
customer interpretation. [H3][H4] At the platform-sale level, the external
check is whether HackerOne supplies credible researchers and actionable reports
while managing disclosure and global payouts; early customers reportedly first
bought payout management. [H1]

**oracle-hardness**  
**Interpretive** — exploit reproduction may be mechanical, but novelty,
scope, duplication, severity, and acceptance remain customer judgments. [H3][H4]

**distance**  
For the researcher, **zero**: accepted report -> posted bounty payment. [H3]
For HackerOne, **1 hop**: customer's desire to operate a bounty/disclosure
program -> platform or managed-service subscription. [H1] One scalar distance
cannot encode both supplier and exchange-operator economics.

**fixture-access**  
**y** for public programs: a newcomer can inspect published program scope and
attempt a report without HackerOne selecting them; private programs require an
invitation. The existence of both public and private access is
`[recollection - unverified]`.

**knowable-at-T**  
T = approximately 2013. A blind scan could see security researchers and
organisations attempting to replace ad hoc vulnerability email with structured
program scope, disclosure, acceptance, and rewards. `[recollection - unverified]`
The founders' stated early belief was that outside hackers could make
organisations safer more efficiently than traditional approaches, and an early
operational pain was paying researchers globally. [H1] A scan could test whether
real programs attracted valid reports, but could not assume marketplace
liquidity, enterprise willingness to expose attack surfaces, or later scale.

**outcome**  
**scaled** — HackerOne reported nearly 300 employees, more than 700,000 hackers,
over 1,900 customers, and $100m in cumulative bounties in 2020. [H1] Its current
company page reports 600,000+ bugs found and 1,300+ companies. [H2] These are
company-reported operating metrics; revenue and profitability are
`[recollection - unverified]`.

**evidence-refs**

- [H1 — HackerOne retrospective on its 2012 founding, payout product, customers, and $100m milestone](https://www.hackerone.com/blog/thanks-being-part-journey-100-million-bounties)
- [H2 — HackerOne company page and current reported scale](https://www.hackerone.com/company)
- [H3 — public HackerOne bug-bounty program listings and reward terms](https://www.hackerone.com/bug-bounty-programs)
- [H4 — US Senate hearing: customers decide bounties and platforms operate programs](https://www.govinfo.gov/content/pkg/CHRG-115shrg37302/pdf/CHRG-115shrg37302.pdf)

## Gitcoin

**case-id**  
`gitcoin`

**org**  
Gitcoin, founded in 2017 to support and monetise open-source development through
bounties. [G1]

**period**  
2017 -> open, 2026; the organisation pivoted from bounties toward grants and
quadratic-funding infrastructure beginning in 2018-19. [G1][G2]

**node-types**  
Primary: **Bounty** for the founding product — funded GitHub issues paid on
completion. [G2][G3]  
Secondary: **Call** for the later grants system — projects apply within a round,
community contributions signal support, and matching pools allocate funding.
[G1][G4]  
**Schema gap:** like HackerOne, founding Gitcoin operated Bounty nodes rather
than merely completing them. It needs **Market/Exchange** for matching issue
sponsors with developers and settling rewards. The later grants protocol is
partly expressible as Call, but Gitcoin also operates the allocation mechanism,
a role the Call row does not distinguish from applicant or funder. [inference
from G2][G4]

**chain**  
Initially, funded open-source GitHub issue -> project/backer sponsors bounty
through Gitcoin -> backer funds reward and accepted contributor receives it ->
`terminal{interest}` in completing an issue or sustaining open source; the
posted/escrowed bounty creates a program-level obligation to pay an accepted
completion, but no external regulation compels the bounty to exist.
[G2][G3] Later: grant application/public-good project -> Gitcoin grants round ->
community donors and matching partners fund grantees -> `terminal{interest}` in
supporting shared digital goods. [G1][G4]

**oracle**  
For the founding bounty, the issue sponsor's acceptance of completed work gates
payment. [G3] For grants, eligibility plus community contributions and the
quadratic-funding calculation gate allocation; the community signal is external
to the applicant, while round eligibility remains interpretive. [G4]

**oracle-hardness**  
**Interpretive** for bounty acceptance and grant eligibility, with a
**mechanical** settlement/allocation component once those inputs are fixed.
[inference from G3][G4]

**distance**  
For a bounty worker, **zero**: accepted issue -> reward. [G3] For Gitcoin as
operator, the payer/revenue distance is not established by the Bounty row:
ConsenSys funded the early project, while later hackathons reportedly supplied
revenue. [G2] That is at least **1 hop** from operated funding event to sponsor
or ecosystem-program budget. [inference from G2] The schema needs a separate
operator take-rate/subsidy field rather than borrowing participant distance.

**fixture-access**  
**y** — the founding product exposed funded open-source issues to developers;
the founder describes it as a marketplace, and Gitcoin described getting paid
for completing GitHub issues. [G2][G3]

**knowable-at-T**  
T = approximately late 2018. A blind scan could see a bounty MVP launched in
autumn 2017, self-funded initially and then funded by ConsenSys, intended to
remove recruiting intermediaries and fund open-source software. [G2] It could
see growing access to Ethereum projects through ConsenSys and test whether
developers completed posted issues. [G2] It could also see the founder's early
finding that bounties were transactional, non-recurring, and poorly matched to
some open-source work. [G2] This snapshot does not assume the later success of
quadratic grants, token governance, or cumulative distributions.

**outcome**  
**open** — Gitcoin evolved into grant-program and modular funding-protocol work;
its official history reports $60m+ distributed by 2024. [G1] The founder's
history says the original bounty model did not fully serve the mission and that
Gitcoin pivoted through experiments toward grants and hackathons. [G2] This is
not clean `scaled` evidence for the founding bounty business; current revenue
and profitability are `[recollection - unverified]`.

**evidence-refs**

- [G1 — Gitcoin official history and cumulative funding](https://gitcoin.co/about)
- [G2 — founder's history of the bounty MVP, ConsenSys funding, marketplace limits, and pivot](https://gov.gitcoin.co/t/a-brief-history-of-gitcoin-from-2017-2022/9431)
- [G3 — Gitcoin description of its founding GitHub-issue bounty platform](https://gitcoin.co/blog/gitcoin-grants)
- [G4 — Gitcoin Grants mechanics and distribution history](https://support.gitcoin.co/gitcoin-knowledge-base/gitcoin-grants)

## AppJet (and successor artifact Etherpad)

**case-id**  
`appjet-etherpad`

**org**  
AppJet, Inc., a 2007 Y Combinator-backed company founded by Aaron Iba, David
Greenspan, and J.D. Zamfirescu. [A1]

**period**  
2007 -> Google acquisition, 2009; successor artifact Etherpad continued as open
source after the acquisition. [A2][A3]

**node-types**  
Primary: **Environment** — the original AppJet product was a browser-based
JavaScript development and hosting environment in which users brought their own
web-application goals. [A1]  
Secondary: **Dependency** — AppJet also named the server-side JavaScript
framework beneath hosted applications, and Etherpad was built on it. [A1][A4]

**chain**  
Original chain: AppJet development/hosting platform -> AppJet sponsor -> hoped-for
developer or application-hosting payer `[recollection - unverified]` ->
`terminal{interest}` in easier web application creation/hosting; no mandatory
procurement obligation was found. Sideways chain, excluded from the T snapshot:
Etherpad real-time editor built with AppJet -> AppJet -> Google as acquirer
seeking the Etherpad team/technology for Google Wave -> `terminal{interest}`.
[A2][A4]

**oracle**  
For the original business, the external check was whether independent developers
could create useful web applications in AppJet and returned or paid for hosting;
users supplied their own goals, so AppJet could not manufacture this check.
[inference from A1] For the later Etherpad artifact, the check became whether
multiple users could edit one document in real time; Etherpad demonstrated that
behavior publicly. [A3]

**oracle-hardness**  
**Interpretive** for original developer adoption/payment; **mechanical** for
Etherpad's real-time collaborative editing behavior. [inference from A1][A3]

**distance**  
Original AppJet: **direct / 1 hop** from developer environment use to a hosting
or platform charge `[recollection - unverified]`, but it required a population.
Sideways Etherpad: **2 hops** from internal demonstration -> standalone
collaboration product -> acquisition consideration. [inference from A2][A4]
The distance scalar cannot express that the monetised artifact changed.

**fixture-access**  
**y** — AppJet's December 2007 public beta allowed anyone to create a web app in
the browser. [A1]

**knowable-at-T**  
T = approximately mid/late 2008. A blind scan could see a public browser-based
environment for creating and hosting web applications with server-side
JavaScript, backed by Y Combinator. [A1] The proposed payer was a developer or
team valuing simplified application development/hosting
`[recollection - unverified]`. A scan could test whether newcomers built apps
and returned, but could not presume that the platform would fail, that the team
would build a collaborative editor as its own application, or that Google would
acquire it. Etherpad launched in November 2008, near the edge of this T window;
to avoid hindsight leakage, it is not used to redefine the founding model. [A3]

**outcome**  
**acquired-sideways** — AppJet's development platform was closed in 2009 to
focus elsewhere; Google acquired AppJet in December 2009 around Etherpad, and
the team joined Google Wave. [A1][A2] Etherpad was released as open source and
continued under community/Foundation coordination. [A3] The schema captures
the exit label but needs an `artifact-at-outcome` or pivot-lineage field to say
that the acquired value was not the original payer chain.

**evidence-refs**

- [A1 — AppJet founding, public beta, platform description, and closure](https://en.wikipedia.org/wiki/AppJet)
- [A2 — contemporary report of Google's AppJet/Etherpad acquisition and Wave-team destination](https://techcrunch.com/2009/12/04/google-acquires-etherpad/)
- [A3 — Etherpad launch, open-source release, and continuation](https://en.wikipedia.org/wiki/Etherpad)
- [A4 — MIT thesis describing Etherpad as implemented on AppJet's server-side JavaScript framework](https://up.csail.mit.edu/other-pubs/maxg-thesis.pdf)

## Wolfram Research

**case-id**  
`wolfram-research`

**org**  
Wolfram Research, founded by Stephen Wolfram in 1987; Mathematica first shipped
in 1988. [W1]

**period**  
1987 -> open, 2026; the company has operated for more than three decades and
reports millions of Mathematica users. [W1]

**node-types**  
Primary: **Environment** — users bring mathematical, scientific, engineering,
programming, and knowledge-computation goals into Mathematica/Wolfram Language
and return to the notebook/tool environment. [W1][W2]  
Secondary: **Dependency** — Wolfram Engine can be embedded in applications,
in-house tools, report generation, or technology stacks under production
licences. [W3] The mixture is expressible: Environment describes interactive
goal-bearing use, while Dependency describes deployed embedding.

**chain**  
Mathematica/Wolfram computational environment and engine -> Wolfram Research
sponsorship -> individuals, universities, companies, and software distributors
buy licences/subscriptions `[recollection - unverified]` ->
`terminal{interest}` in productive technical computation, teaching, research,
or embedded computational functionality. [W1][W3][W4] Once a buyer chooses the
proprietary product, copyright and the Mathematica licence create a
`terminal{obligation}` to obtain/use the applicable licence class; this is a
vendor-created licence obligation, not an external scientific or regulatory
mandate. [W2]

**oracle**  
The non-manufacturable check is whether Mathematica correctly and usefully
performs a user's real symbolic, numerical, graphical, or programming task in
their workflow. [inference from W1][W4] For embedded production use, the check
also includes whether Wolfram Engine supplies the required computation inside
the customer's application and whether the deployment matches licensed
machine/site/cluster/distribution terms. [W2][W3]

**oracle-hardness**  
**Interpretive** overall: individual computations may have mechanical answers,
but the diversity of user-supplied goals and workflow value has no single
scorer. Licence compliance is mechanical once deployment facts are known.
[inference from W2][W3]

**distance**  
Interactive Mathematica: **direct / 1 hop** from valuable environment access
to licence line item. [W2][W4] Embedded Wolfram Engine: **2 hops** from engine
dependency -> customer's application/deployment -> production or distribution
licence. [W3] The two distances correspond to its Environment and Dependency
roles and should not be collapsed analytically.

**fixture-access**  
At T, **n** for the complete commercial product beyond demonstrations or
evaluation access `[recollection - unverified]`; the decisive test required
access to Mathematica. Today Wolfram Cloud offers free access to get started,
but production/product entitlements remain licensed. [W5][W2]

**knowable-at-T**  
T = approximately 1988. A blind scan could see a newly founded company and a
newly released integrated technical-computing product, Mathematica. [W1] A user
could test it on their own mathematical/scientific work and compare whether one
coherent environment displaced separate tools `[recollection - unverified]`.
The visible commercial hypothesis was direct software licensing to technical
users and institutions `[recollection - unverified]`; the scan could not know
multi-decade survival, millions of users, Wolfram|Alpha, cloud delivery, or
later engine embedding.

**outcome**  
**scaled** — Wolfram Research remains independent/operating after more than
three decades, calls Mathematica its longstanding flagship, and reports millions
of dedicated users. [W1] It currently sells individual, network, site, cluster,
cloud, and embedded/distribution licensing forms. [W3][W4][W6] Revenue,
profitability, and exact paid-seat counts are `[recollection - unverified]`.

**evidence-refs**

- [W1 — official company background: 1987 founding, 1988 Mathematica release, longevity, and reported users](https://www.wolfram.com/company/)
- [W2 — Mathematica licence agreement and licence-class restrictions](https://www.wolfram.com/legal/agreements/wolfram-mathematica/index.html.en)
- [W3 — Wolfram Engine production and distribution licensing](https://www.wolfram.com/engine/commercial-options/)
- [W4 — commercial Mathematica desktop/cloud licence offering](https://www.wolfram.com/mathematica/pricing/commercial/index.php.en?desktop=)
- [W5 — Wolfram Cloud environment and free-start access](https://www.wolfram.com/cloud/index.php.en)
- [W6 — Wolfram network licensing](https://www.wolfram.com/network-licensing/)
