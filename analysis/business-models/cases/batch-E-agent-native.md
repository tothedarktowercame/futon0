# Batch E — Agent-native systems (prospective preregistration)

**Frozen observation date:** 2026-08-20. These records use schema v1 plus
`entity-type` and `prediction`. Later resolution must append; it must not edit
these preregistered fields.

## OpenHands / All Hands AI

- **case-id:** `openhands-all-hands-ai`
- **org:** OpenHands (formerly OpenDevin) and All Hands AI
- **entity-type:** company. This value is incomplete: the record deliberately
  spans a company and its OSS project, which v1 cannot represent as two linked
  entities. [OH1][OH2]
- **period:** 2024 -> open as of 2026-08-20. All Hands AI announced a $5 million
  seed round in September 2024; OpenHands describes eighteen months of repository
  growth by September 2025. [OH1][OH3]
- **node-types:** primary **Dependency**; secondary **Environment**. Teams can
  adopt the MIT-licensed core and build workflows around it, while the company
  sells a control plane in which organisations run and govern agents. [OH2][OH4]
  `[inference from OH2, OH4]`
- **chain:** OSS agent/runtime -> adoption in developer workflows -> All Hands
  AI cloud/enterprise control plane -> enterprise software-engineering or AI
  tooling line item -> `terminal{obligation}` manufactured by the vendor:
  OpenHands Enterprise requires a paid licence after one month of use. [OH2]
  V1 cannot distinguish this manufactured licence obligation from an external
  regulatory or contractual obligation. `[inference from OH2]`
- **oracle:** the non-manufacturable check is whether independent teams put
  OpenHands into real workflows and return; the sale is then gated by whether
  the enterprise control plane can securely operate and govern those agents at
  organisational scale. [OH4] `[inference from OH4]`
- **oracle-hardness:** interpretive — repository adoption is observable, but
  production utility, governance adequacy, and renewal are customer judgments.
  `[inference from OH2, OH4]`
- **distance:** 2 hops: OSS artifact -> organisational adoption -> paid cloud or
  enterprise line item. `[inference from OH2, OH4]`
- **fixture-access:** y for the public core and local trial; n for the decisive
  organisation-wide control-plane test, which requires a team's repositories,
  policies, and permission. [OH2][OH4]
- **knowable-at-T:** On 2026-08-20, the company reports 81,000+ GitHub stars,
  500+ OSS contributors, and 9 million+ downloads; these are adoption signals,
  not revenue or retention evidence. [OH5] It announced $18.8 million Series A
  funding in November 2025 and says commercial development targets user
  management, cloud deployment, and team coordination. [OH6] The public core
  is MIT-licensed while the enterprise directory has separate commercial
  terms. [OH2] Whether adoption converts into durable paid control-plane use is
  not publicly established by the reviewed sources.
- **outcome:** **open** as of 2026-08-20. [OH4]
- **prediction:** Given a strong public Dependency position, an Environment
  product adjacent to adoption, medium distance, and a manufactured licence
  obligation, the schema predicts that All Hands AI will capture value through
  organisation-level control, deployment, and support rather than charging for
  the core agent. `[inference from OH2, OH4, OH5]` **Falsifier:** by 2028-12-31,
  no publicly evidenced recurring enterprise/control-plane customers or
  enterprise revenue, accompanied by discontinuation or relicensing-away of the
  paid enterprise offering while the OSS project remains materially active.
- **evidence-refs:**
  - [OH1] [All Hands AI — $5M seed announcement](https://www.openhands.dev/blog/press-release-all-hands-announces-5m-to-scale-ai-agent-for-software-development)
  - [OH2] [OpenHands repository — core and Enterprise licensing](https://github.com/OpenHands/OpenHands)
  - [OH3] [OpenHands — Path to v1](https://www.openhands.dev/blog/the-path-to-openhands-v1)
  - [OH4] [OpenHands Enterprise control plane](https://www.openhands.dev/blog/openhands-enterprise-agent-control-plane)
  - [OH5] [OpenHands — About](https://www.openhands.dev/about)
  - [OH6] [OpenHands — $18.8M Series A](https://www.openhands.dev/blog/weve-just-raised-18-8m-to-build-the-open-standard-for-autonomous-software-development)

## SWE-agent

- **case-id:** `swe-agent`
- **org:** SWE-agent (Princeton NLP / Stanford researchers)
- **entity-type:** research-system
- **period:** 2024 public release -> open as of 2026-08-20. The project reports
  a 2024 NeurIPS paper and identifies Princeton and Stanford researchers as its
  maintainers. [SA1][SA2]
- **node-types:** primary **Call**; secondary **Benchmark**. The durable payer
  route, if any, is a university/funder research budget; the system's public
  performance claim is judged on SWE-bench. [SA1] The specific grant supporting
  SWE-agent was not located, so the Call assignment is a model hypothesis rather
  than an evidenced award. `[inference from SA1]`
- **chain:** research code and Agent-Computer Interface -> Princeton/Stanford
  research programme -> university lab or grant budget
  `[recollection - unverified]` -> `terminal{interest}`: produce publishable
  automated-software-engineering research; no commercial sale or named funding
  obligation was found in the reviewed sources.
- **oracle:** percentage of real GitHub issues resolved under SWE-bench's test
  harness; the project reported 12.29% on the full benchmark for its original
  system. [SA2]
- **oracle-hardness:** mechanical — repository tests determine whether each
  generated patch resolves its issue, subject to benchmark-validity limits.
  [SA2][SB1]
- **distance:** 2 hops from research artifact through lab/funder to a research
  budget; benchmark success does not itself pay. `[inference from SA1, SA2]`
- **fixture-access:** y — the project is documented for research use and the
  SWE-bench fixture and harness are public. [SA1][SB1]
- **knowable-at-T:** On 2026-08-20, SWE-agent is an open, configurable research
  system for fixing GitHub issues and other tasks, maintained by Princeton and
  Stanford researchers. [SA1] Its documentation says current effort has shifted
  toward the simpler mini-SWE-agent, which supersedes SWE-agent while matching
  its performance. [SA1] No reviewed source identifies a company, paid product,
  or project-specific grant.
- **outcome:** **open** as a research system as of 2026-08-20. [SA1]
- **prediction:** Given a Call terminal that is only hypothesised, a hard public
  Benchmark oracle, and no direct payer line, the schema predicts sustained
  noncommercial influence through papers, forks, and successor systems rather
  than value capture by SWE-agent itself. `[inference from SA1, SA2]`
  **Falsifier:** by 2028-12-31, SWE-agent becomes the named core of a paid product
  or licence with publicly evidenced recurring revenue attributable to the
  project, rather than merely being reused by another product.
- **evidence-refs:**
  - [SA1] [SWE-agent documentation](https://github.com/princeton-nlp/SWE-agent/blob/main/docs/index.md)
  - [SA2] [SWE-agent research overview](https://github.com/SWE-agent/SWE-agent/blob/main/docs/background/index.md)
  - [SB1] [SWE-bench repository and evaluation harness](https://github.com/princeton-nlp/SWE-bench)

## SWE-bench

- **case-id:** `swe-bench`
- **org:** SWE-bench
- **entity-type:** benchmark
- **period:** 2023 preprint -> open as of 2026-08-20. The preprint was submitted
  in October 2023 and the work was published at ICLR 2024. [SB2][SB3]
- **node-types:** primary **Benchmark**; secondary **Call**. It is the scored
  public task set itself; maintaining it is research work whose plausible payer
  is a lab or funder rather than a benchmark entrant. [SB1]
- **chain:** dataset + containerised harness + leaderboard -> SWE-bench research
  maintainers -> Princeton/lab/collaboration budget `[recollection - unverified]`
  -> `terminal{interest}`: maintain a credible shared evaluation fixture. OpenAI
  Preparedness supported the containerised harness and Verified subset, but the
  reviewed source does not state payment terms or a continuing obligation. [SB1]
- **oracle:** tests in reproducible containers decide whether an agent-generated
  patch resolves a real GitHub issue. [SB1]
- **oracle-hardness:** mechanical at scoring time, although human review was
  required to establish the 500-problem Verified subset. [SB1]
- **distance:** far: the score informs model/agent buyers and research funders,
  but SWE-bench itself has no evidenced toll on those downstream line items.
  `[inference from SB1]`
- **fixture-access:** y — the datasets and Docker evaluation commands are
  publicly documented. [SB1]
- **knowable-at-T:** On 2026-08-20, SWE-bench provides public datasets,
  containerised evaluation, a leaderboard ecosystem, Lite/Verified/Multimodal
  variants, and an OpenAI-supported Verified collaboration. [SB1] The reviewed
  sources establish substantial field use but no durable capture mechanism for
  the benchmark maintainers. `[inference from SB1]`
- **outcome:** **open** as of 2026-08-20. [SB1]
- **prediction:** A pure Benchmark with a hard oracle but far distance and an
  interest terminal should create substantial downstream value while capturing
  little direct revenue; maintenance will depend on university, sponsor, or
  in-kind infrastructure support. `[inference from SB1]` **Falsifier:** by
  2028-12-31, the SWE-bench maintainers operate a paid scoring, certification,
  or data-access service with publicly evidenced recurring revenue sufficient
  to fund benchmark maintenance directly.
- **evidence-refs:**
  - [SB1] [SWE-bench repository, variants, OpenAI collaboration and harness](https://github.com/princeton-nlp/SWE-bench/blob/main/README.md?plain=1)
  - [SB2] [SWE-bench preprint](https://arxiv.org/abs/2310.06770)
  - [SB3] [SWE-bench ICLR 2024 paper](https://openreview.net/pdf?id=VTF8yNQM66)

## SICA

- **case-id:** `sica-self-improving-coding-agent`
- **org:** SICA — A Self-Improving Coding Agent
- **entity-type:** research-system
- **period:** 2025 publication -> open as of 2026-08-20. The paper was submitted
  in April 2025 and appeared at the ICLR 2025 Workshop on Scaling Self-Improving
  Foundation Models. [SI1][SI2]
- **node-types:** primary **Call**; secondary **Benchmark**. SICA is a research
  artifact rather than a sold agent; its demonstrated feedback comes from
  benchmark utility. [SI1][SI2] No specific supporting grant or commercial
  payer was found, so Call is a plausible research-budget route, not an
  evidenced award. `[inference from SI1, SI2]`
- **chain:** self-editing agent framework + experimental result -> authors'
  research programme -> university/lab budget `[recollection - unverified]` ->
  `terminal{interest}`: publish and extend self-improving-agent research; no
  buyer obligation or commercial line item was found.
- **oracle:** held benchmark performance after the agent edits its own code;
  the paper reports movement from 17% to 53% on a random SWE-bench Verified
  subset, plus tests on LiveCodeBench and synthetic benchmarks. [SI1]
- **oracle-hardness:** mechanical for benchmark scoring, with interpretive risk
  in experiment design and generalisation from the selected tasks.
  `[inference from SI1]`
- **distance:** 2 hops or more from code/paper through a lab/funder to a research
  line; downstream commercial reuse would add another hop. `[inference from SI2]`
- **fixture-access:** y — the reference framework is published under the MIT
  licence and links its workshop paper. [SI2]
- **knowable-at-T:** On 2026-08-20, SICA consists of an MIT-licensed reference
  framework and a workshop paper demonstrating autonomous self-editing against
  benchmark utility. [SI1][SI2] The repository showed 336 stars and 56 forks
  when indexed, but these volatile counts are adoption hints rather than payer
  evidence. [SI2] No reviewed source establishes an ongoing team, service,
  product, or dedicated funding stream.
- **outcome:** **open** as a research system as of 2026-08-20. [SI2]
- **prediction:** With a hard Benchmark oracle, distant/unknown payer, and an
  interest terminal, the schema predicts that SICA's code or method may be
  absorbed into other agents without the named system capturing value.
  `[inference from SI1, SI2]` **Falsifier:** by 2028-12-31, SICA itself becomes
  a maintained paid service, licensed product, or funded standalone programme
  with a publicly named recurring payer.
- **evidence-refs:**
  - [SI1] [Robeyns, Szummer and Aitchison — A Self-Improving Coding Agent](https://arxiv.org/abs/2504.15228)
  - [SI2] [SICA reference implementation](https://github.com/MaximeRobeyns/self_improving_coding_agent)

## VERSES AI — natural-language active-inference-agent patent

- **case-id:** `verses-ai-active-inference-patent`
- **org:** VERSES AI, Inc., scoped to US Patent 12,393,581
- **entity-type:** company
- **period:** 2023-07-12 priority -> open as of 2026-08-20. The patent record
  gives a July 2023 priority date, July 2024 filing, August 2025 grant, active
  status, and VERSES AI as assignee. [V1]
- **node-types:** primary **Requirement**; secondary **Dependency**. The claimed
  method starts with a user's natural-language specification and produces an
  active-inference agent representation; a future implementation dependency on
  patented claims could create the secondary position. [V1][V2]
- **chain:** patented method/implementation -> VERSES product or licence ->
  adopter's agent-platform or software line item -> `terminal{obligation}` only
  if VERSES requires a licence for an implementation that practises a valid
  patent claim. [V1] That would be a vendor-manufactured IP obligation, which v1
  cannot distinguish from an external compliance obligation. No reviewed source
  establishes material third-party adoption or patent-licensing revenue as of T.
- **oracle:** for a Requirement sale, whether the compiled agent satisfies the
  adopter's specification; for patent capture, claim construction plus evidence
  that a product practises every limitation of an enforceable claim. [V2]
  `[inference from V2]`
- **oracle-hardness:** interpretive — agent behaviour can be tested, but patent
  scope, infringement, validity, and commercial acceptance are not determined
  by VERSES alone. `[inference from V1, V2]`
- **distance:** currently 2 or more hops from patented method through a VERSES
  product/licence to an adopter line item; the distance is hypothetical until
  adoption or licensing is evidenced. `[inference from V1]`
- **fixture-access:** n for the decisive sale or infringement test: the patent
  text is public, but a newcomer needs an adopter's specification/product or an
  accused implementation. [V1][V2]
- **knowable-at-T:** On 2026-08-20, US 12,393,581 is a granted active patent
  assigned to VERSES AI and names Karl Friston among thirteen listed inventors.
  [V1] Its claims describe receiving natural-language input, producing queries,
  and representing observation/state entities of an active-inference model in
  an HSML knowledge graph; this record does not broaden that language into a
  monopoly on all natural-language agent generation. [V2] VERSES identifies the
  patent in investor materials. [V3] Public evidence reviewed here does not show
  that third parties have adopted the patented route or paid to license it.
- **outcome:** **open** as of 2026-08-20. [V1]
- **prediction:** A granted patent can manufacture an obligation only after a
  non-manufacturable adoption position exists; with no evidenced adoption at T,
  the schema predicts that this patent alone will not produce material capture
  by 2028. `[inference from V1, V3]` **Falsifier:** by 2028-12-31, VERSES reports
  a named arms-length licensee, material patent-linked licensing revenue, or a
  final enforceable infringement judgment/settlement specifically tied to US
  12,393,581.
- **evidence-refs:**
  - [V1] [Google Patents — US20250021548A1 / US12393581B2 record](https://patents.google.com/patent/US20250021548A1/en)
  - [V2] [Google Patents — granted US12393581B2 claims and description](https://patents.google.com/patent/US12393581B2/en)
  - [V3] [VERSES AI investor presentation identifying US 12,393,581](https://www.verses.ai/hubfs/Investor%20Material/Presentations/2025-08-28%20VERSES%20Investor%20Presentation.pdf)

## Node-type fit result

No entity fits none of the six rows, so no seventh row is required by this
batch. Two representation defects remain. OpenHands/All Hands AI needs linked
project and company records because one v1 `entity-type` and `outcome` cannot
separate OSS adoption from company capture. SWE-bench fits Benchmark exactly,
but the row describes entities *travelling* a benchmark better than the entity
that *produces and maintains* one; its payer chain remains expressible only by
adding Call as a secondary node and recording the unevidenced maintenance payer.
