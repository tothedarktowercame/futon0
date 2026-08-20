# Batch A — Requirement / formal-methods organisations

Encoding date: 2026-08-20. `knowable-at-T` is restricted to the landscape at
founding plus approximately one year; later evidence is used only in `outcome`.

## Andela

- **case-id:** `andela`
- **org:** Andela
- **period:** 2014 -> 2024 (ten-year operating-status checkpoint). Andela says
  it began in May 2014 and was still operating with a newly appointed CEO in
  September 2024. [A1]
- **node-types:** primary **Requirement**; secondary **Environment**. At T, the
  sellable unit was trained engineering capacity matched to a client need,
  making the client's staffing requirement the fixture and client acceptance
  the oracle. [A2] The Environment fit is secondary and reflects the later
  talent marketplace, not information used in the T snapshot. [A2]
- **chain:** trained engineer/team -> Andela matching and employment apparatus
  -> client company's engineering/staffing line item ->
  `terminal{interest}`: the employer wants additional software-delivery
  capacity; no regulation, audit, certification, accreditation, or other
  recurring external obligation was found in the reviewed sources. [A2]
- **oracle:** Andela's skills screening plus the client company's decision to
  engage and retain the engineer; the early model used online tests,
  interviews, intensive training, and placement. [A2]
- **oracle-hardness:** interpretive — tests constrain entry, but a client's
  judgment of a person/team is not a mechanical line-item checker. [A2]
- **distance:** 1 hop from accepted engineering capacity to the client's
  staffing/engineering line item. [A2]
- **fixture-access:** n — a newcomer could apply to Andela, but could not
  independently run the client-placement test without Andela and a client
  granting access. [A2]
- **knowable-at-T:** By roughly mid-2015, an observer could see a company that
  had hired an initial Lagos cohort, screened and trained developers, sold
  placements to client companies, raised a $10 million Series A, and begun
  expanding to Nairobi. [A1] The visible hypothesis was that scarce global
  demand for developers could pay for intensive African talent formation and
  matching. [A2] At T, client retention, training cost, geographic replication,
  and dependence on external capital remained observable risks; later pivots
  and valuation are deliberately excluded from this field. [A1][A2]
- **outcome:** **scaled**. Andela reports a $200 million Series E at a $1.5
  billion valuation in 2021, a permanent remote model from 2020, and a Talent
  Cloud launch after three acquisitions in 2023. [A1] IFC describes the shift
  from a campus/apprenticeship model to a global match-as-a-service model. [A2]
- **evidence-refs:**
  - [A1] [Andela — About and company history](https://www.andela.com/about)
  - [A2] [International Finance Corporation — Andela case study](https://www.ifc.org/en/insights-reports/2022/andela)

## Galois, Inc.

- **case-id:** `galois-inc`
- **org:** Galois, Inc.
- **period:** 1999 -> 2023 (employee-ownership checkpoint). The 1999 founding
  date is `[recollection - unverified]`; Galois documents that it became 100%
  employee-owned in June 2023. [G2]
- **node-types:** primary **Call**; secondary **Requirement**. The primary fit
  is competitively funded research against agency programme criteria; the
  secondary fit is delivery of high-assurance research and engineering against
  a customer work statement. Galois identifies DARPA, NASA, and DoD among its
  clients and describes both R&D and delivered solutions. [G1]
- **chain:** formal model/tool/high-assurance implementation -> government
  programme or customer project -> agency/customer R&D line item ->
  `terminal{obligation}`: satisfy the funded contract or award's work statement
  and acceptance terms. The reviewed sources do **not** establish that Galois's
  general revenue terminates in a named product-certification mandate; the
  named obligation here is contractual delivery, not DO-178C/Common Criteria.
  [G1]
- **oracle:** sponsor review of programme milestones and contracted technical
  deliverables; mathematical proof may be mechanical inside the artifact, but
  award acceptance remains sponsor interpretation. [G1]
- **oracle-hardness:** interpretive. [G1]
- **distance:** 1 hop where Galois is a prime contractor, or 2 where a programme
  prime sits between its artifact and the agency line item
  `[recollection - unverified]`.
- **fixture-access:** n — agency calls may be public, but a newcomer cannot
  attempt the funded programme's acceptance test without proposal selection
  and access to the relevant system. `[recollection - unverified]`
- **knowable-at-T:** Around 2000, an observer could see a newly formed company
  proposing to apply functional programming and formal methods to information
  assurance `[recollection - unverified]`. Government high-assurance R&D was a
  plausible buyer class, but repeat awards, procurement access, commercial
  demand, and the ability to turn research into accepted systems were not yet
  demonstrated `[recollection - unverified]`. No later client list, HACMS
  result, or ownership outcome is used in this snapshot.
- **outcome:** **scaled** (durable specialist rather than venture-scale exit).
  Galois currently reports clients including NASA, DARPA, Amazon, and DoD,
  work across multiple critical sectors, and both research and solutions
  lines. [G1] It introduced an ESOP in 2013 and became 100% employee-owned in
  2023. [G2]
- **evidence-refs:**
  - [G1] [Galois — About](https://www.galois.com/about)
  - [G2] [Galois — 100% employee ownership](https://www.galois.com/articles/galois-is-now-100-employee-owned-heres-what-that-means)

## Trail of Bits

- **case-id:** `trail-of-bits`
- **org:** Trail of Bits
- **period:** 2012 -> 2026 (current operating-status checkpoint). Trail of Bits
  states that it has operated since 2012. [T1]
- **node-types:** primary **Requirement**; secondary **Call**. A customer brings
  a product, network, or security gap and buys an audit, deployment advice, or
  engineering work; sponsored security research such as DARPA work supplies
  the secondary Call shape. [T1]
- **chain:** findings/tool/repair -> security-review engagement -> customer's
  security or product-engineering line item -> `terminal{interest}` in the
  general case: reduce exploitable risk before deployment. [T1] Some individual
  customers may have audit or assurance obligations, but no reviewed source
  supports assigning one named certification/regulation across Trail of Bits's
  business; therefore this record does not invent one.
- **oracle:** the contracted scope and customer acceptance of a reproducible
  findings report, mitigations, or delivered security feature. Trail of Bits
  describes audits, consultation, and feature development as engagement
  outputs. [T1]
- **oracle-hardness:** interpretive — findings can have mechanical exploits or
  tests, while engagement completeness and risk acceptance remain judgments.
  [T1]
- **distance:** 1 hop from accepted audit/engineering deliverable to the
  customer's security line item. [T1]
- **fixture-access:** n — public tools and publications are accessible, but a
  newcomer cannot inspect a client's private target or obtain customer
  acceptance without permission. [T1]
- **knowable-at-T:** Around 2013, an observer could see a young security firm
  founded in 2012 offering attacker-minded security research and engineering
  `[recollection - unverified]`. The visible market hypothesis was that
  organisations facing targeted attacks would pay scarce specialists to find
  and close security gaps `[recollection - unverified]`. Repeat demand,
  reputation, access to sensitive targets, and whether reusable tools would
  compound rather than merely subsidise consulting were open questions
  `[recollection - unverified]`.
- **outcome:** **scaled**. The company currently reports 620 audits, more than
  200 open-source repositories, and services spanning application security,
  cryptography, blockchain, AI/ML security, and R&D. [T2]
- **evidence-refs:**
  - [T1] [Trail of Bits — About](https://trailofbits.com/about/)
  - [T2] [Trail of Bits — services and publication counts](https://trailofbits.com/)

## Runtime Verification, Inc.

- **case-id:** `runtime-verification-inc`
- **org:** Runtime Verification, Inc.
- **period:** 2010 -> 2026 (current operating-status checkpoint). Runtime
  Verification says it was founded in 2010 as a University of Illinois
  spinout and remains an operating formal-methods company. [R1]
- **node-types:** primary **Call**; secondary **Requirement**. Early university
  technology commercialisation and sponsored R&D fit Call
  `[recollection - unverified]`; customer formalisation, analysis, and proof
  engagements fit Requirement. [R2]
- **chain:** executable semantics/proof or analysis report -> sponsored project
  or verification engagement -> agency/customer R&D or assurance line item ->
  `terminal{obligation}` for the evidenced NASA SBIR case: deliver the Phase I
  work under contract 80NSSC20C0497; `terminal{interest}` for voluntary
  blockchain assurance engagements where no regulatory mandate is evidenced.
  [R3][R2]
- **oracle:** for formal verification, specified behaviours become theorems
  checked against a mathematical EVM model; for NASA SBIR, agency evaluation of
  the contracted toolset and analysis of NASA flight software gates the award
  deliverable. [R2][R3]
- **oracle-hardness:** mechanical for proof checking; interpretive for sponsor
  acceptance of requirements, scope, and milestones. [R2][R3]
- **distance:** 1 hop from a verification deliverable to a direct customer or
  agency line item. [R2][R3]
- **fixture-access:** n — K and some artifacts can be public, but the sale's
  requirements and acceptance depend on a sponsor/customer engagement. [R2]
- **knowable-at-T:** Around 2011, an observer could see a University of Illinois
  spinout centred on Grigore Roșu's runtime-verification and language-semantics
  research. [R1] A plausible buyer landscape included aerospace and other
  critical-software organisations, but product-market fit, procurement access,
  repeatability beyond founder-led research, and willingness to pay for proofs
  rather than conventional testing were unresolved `[recollection - unverified]`.
  Blockchain work, later clients, and later SBIR awards are excluded from this
  snapshot.
- **outcome:** **scaled** (small specialist). Runtime Verification reports more
  than 25 senior engineers and work with NASA and Toyota. [R1] Its work page
  reports more than 100 protected clients and work for DARPA, NASA, Boeing,
  Ethereum Foundation, Solana, and Stellar. [R4]
- **evidence-refs:**
  - [R1] [Runtime Verification — About and history](https://runtimeverification.com/about)
  - [R2] [Runtime Verification — smart-contract analysis and verification](https://runtimeverification.com/smartcontract)
  - [R3] [SBIR.gov — NASA contract 80NSSC20C0497](https://www.sbir.gov/awards/182008)
  - [R4] [Runtime Verification — Work](https://runtimeverification.com/work)

## Kestrel Institute

- **case-id:** `kestrel-institute`
- **org:** Kestrel Institute
- **period:** 1981 -> 2026 (current operating-status checkpoint). Kestrel's
  formal-methods overview says it was founded out of Stanford in 1981, and its
  current site identifies it as an operating nonprofit research centre. [K2][K1]
- **node-types:** primary **Call**; secondary **Requirement**. Sponsor-funded
  research programmes fit Call, while formal-methods assistance and contracted
  synthesis/verification against a customer's specification fit Requirement.
  [K1]
- **chain:** formal specification/proof/synthesised program -> research sponsor
  or customer project -> government research or customer assurance line item ->
  `terminal{obligation}` where a grant/contract requires programme deliverables;
  Kestrel also explicitly offers certification evidence for Common Criteria,
  DO-178B/C, and FIPS 140-3, in which case the downstream terminal is the
  product sponsor's named certification objective. [K2]
- **oracle:** sponsor review of research milestones and, for certification
  support, the relevant external evaluator's checklist/evidence process;
  internally, ACL2 checks generated correctness proofs. [K1][K2]
- **oracle-hardness:** interpretive for award and certification acceptance,
  with mechanical proof checking inside the artifact. [K1][K2]
- **distance:** 1 hop for direct sponsored work; 2 where Kestrel supplies proof
  evidence to a product owner that then submits to a certification evaluator.
  [K2]
- **fixture-access:** n — publications and tools may be public, but programme
  acceptance and a product's certification submission require sponsor access.
  [K1][K2]
- **knowable-at-T:** Around 1982, an observer could see a Stanford-adjacent
  nonprofit research institute pursuing knowledge-based software development
  and formal specification `[recollection - unverified]`. Government research
  sponsorship was a plausible terminal `[recollection - unverified]`, but the
  practical scalability of program synthesis, availability of automated proof
  infrastructure, repeat sponsor demand, and transfer into deployed systems
  were unresolved `[recollection - unverified]`. Later ACL2 work, named modern
  sponsors, and certification offerings are excluded from this snapshot.
- **outcome:** **sustained-noncommercially**. Kestrel remains a nonprofit
  research centre, lists research across synthesis, verification, theorem
  proving and planning, and names sponsors/customers including DARPA, DoD,
  IARPA, NASA, NSF, GE, and several foundations. [K1]
- **evidence-refs:**
  - [K1] [Kestrel Institute — organisation, research and sponsors](https://www.kestrel.edu/)
  - [K2] [Kestrel Institute — Formal Methods Approaches](https://www.kestrel.edu/kestrel-formal-methods.pdf)

## Node-type fit result

All five organisations can be encoded with the existing six node types. No new
node type is required. Andela is the weakest fit: **Requirement** captures the
client staffing specification and vendor relationship, while **Environment**
captures its later talent marketplace. A future schema might distinguish a
`Capability intermediary` whose artifact is screened human capacity rather
than a technical deliverable, but this case does not require that seventh type
to preserve the payer chain.
