# Business-model cases: batch C — Environment

These records use `T = founding + approximately one year`.  The
`knowable-at-T` fields were written from evidence available at that cutoff;
later evidence is confined to `outcome`.  “Terminal” names whether the final
demand is an obligation, an interest, or absent.  Interpretive classifications
are judgments from the cited facts, not additional factual claims.

## C01 — Mojang (Minecraft)

- **case-id:** `C01-mojang-minecraft`
- **org:** Mojang Specifications / Mojang AB
- **period:** 2009–2014
- **node-types:** primary `Environment`; no secondary type
- **chain:** Minecraft, a user-directed construction and exploration space →
  Mojang as sponsor and operator → players buying game licences → terminal
  `interest` (play, construction, exploration, and social use), with no
  identified obligation compelling purchase.
- **oracle:** Mechanical: independent users enter the environment, construct
  goals and artifacts of their own, and voluntarily return or purchase access.
- **oracle-hardness:** `mechanical` — the product can be inspected without permission, but the
  Environment test cannot be completed without a real population choosing to
  use it.  Retention is stronger evidence than a founder-run demonstration.
- **distance:** `1` — the users who exercise the oracle are also the licence
  buyers.
- **fixture-access:** `n` — Mojang could publish a playable build, but could not
  supply the independent population whose voluntary use constitutes the test.
- **knowable-at-T:** At `T ≈ 2010`, an observer could see a sandbox game first
  made public in 2009 and a population already exceeding 100,000 players by
  January 2010 [C01-1][C01-2].  Contemporary-scale evidence therefore made the
  population precondition testable within months, while the terminal remained
  voluntary player interest rather than an obligation.  Nothing in this field
  assumes the later acquisition.
- **outcome:** `scaled`.  Microsoft announced in September 2014 that it would
  acquire Mojang and the Minecraft franchise for USD 2.5 billion; its release
  reports more than 100 million PC downloads since 2009 [C01-3].  This is coded
  `scaled`, rather than `acquired-sideways`, because the acquired asset was the
  core environment itself.
- **evidence-refs:**
  - **[C01-1]** Mojang, “15th Anniversary”: Minecraft appeared in 2009 and
    Mojang Specifications was founded —
    <https://www.minecraft.net/en-us/15th-anniversary>
  - **[C01-2]** Xbox Wire, “15 Years of Minecraft”: the Classic build appeared
    in 2009 and the player count exceeded 100,000 by January 2010 —
    <https://news.xbox.com/en-us/2024/05/17/minecraft-15-years-most-important-moments/>
  - **[C01-3]** Microsoft, “Minecraft to join Microsoft”: acquisition terms and
    download count —
    <https://news.microsoft.com/source/2014/09/15/minecraft-to-join-microsoft/>

## C02 — Roblox Corporation

- **case-id:** `C02-roblox`
- **org:** Roblox Corporation
- **period:** 2004–2021
- **node-types:** primary `Environment`; secondary `Dependency`, because
  creators' experiences execute inside and depend on Roblox's hosted platform
  and economy [C02-2].
- **chain:** Roblox's user-generated experience platform → Roblox Corporation
  as sponsor and operator → users purchasing Robux, with some proceeds flowing
  to creators → terminal `interest` (play, creation, and social participation),
  with no identified obligation compelling use [C02-2].
- **oracle:** Mechanical: users create experiences and other users voluntarily
  enter, return, and transact in them.
- **oracle-hardness:** `mechanical` — a build proves only that creation is possible.  The
  Environment oracle requires permission in practice: a real population must
  choose to create and play, and the platform operator controls access.
- **distance:** `1` for platform demand — participating users buy the platform's
  currency — although creator compensation introduces another economic leg.
- **fixture-access:** `n` — Roblox could admit invited users, but could not
  manufacture independent preference or sustained participation.
- **knowable-at-T:** At `T ≈ 2005`, an observer could establish that the company
  had begun in 2004 [C02-1] and was building a platform whose eventual defining
  object was user-generated virtual experience [C02-2].  The public sources
  reviewed do not establish a reliable first-cohort date, first-year population
  count, or first-year payment signal; Roblox itself says the first player is
  difficult to identify because early test accounts remained active [C02-1].
  The honest contemporaneous verdict is therefore `population test begun but
  not quantitatively evidenced`, not retrospective success.
- **outcome:** `scaled`.  By its 2020 registration statement Roblox described
  nearly seven million active developers, more than 18 million experiences,
  31.1 million daily active users in the first nine months of 2020, and Robux
  sales as substantially all revenue [C02-2].  It completed a direct listing in
  March 2021 [C02-3].
- **evidence-refs:**
  - **[C02-1]** Roblox Support, “Roblox Company Information”: founded in 2004
    and caveat about identifying the first player —
    <https://en.help.roblox.com/hc/en-us/articles/203313370-Roblox-Company-Information>
  - **[C02-2]** Roblox Corporation, SEC Form S-1/A (2020): user-generated
    platform, developer and experience counts, DAU, and Robux economics —
    <https://www.sec.gov/Archives/edgar/data/1315098/000119312520307391/d87104ds1a.htm>
  - **[C02-3]** Roblox Corporation, 2021 Form 10-K: March 2021 direct listing —
    <https://www.sec.gov/Archives/edgar/data/1315098/000131509822000058/rblx-20211231.htm>

## C03 — Linden Lab (Second Life)

- **case-id:** `C03-linden-lab-second-life`
- **org:** Linden Research, Inc. (Linden Lab)
- **period:** 1999–2023 observation
- **node-types:** primary `Environment`; no secondary type
- **chain:** Second Life's persistent user-created virtual world → Linden Lab
  as sponsor and operator → residents paying subscriptions, land-related
  charges, or currency-exchange fees `[recollection - unverified]` → terminal
  `interest` (creation, social life, trade, and community), with no identified
  obligation compelling participation.
- **oracle:** Mechanical: residents create places, objects, communities, and
  economic activity, and voluntarily return.
- **oracle-hardness:** `mechanical` — a publisher-controlled beta could expose the product,
  but only admitted residents could supply the population-dependent oracle.
  User-created value and operator value capture are separate measurements.
- **distance:** `1` — residents both exercise the environment and are the
  putative payers, though creator-to-resident transactions complicate the
  internal economy.
- **fixture-access:** `n` — access required Linden Lab's permission during the
  closed-beta period, and no fixture can substitute for voluntary community.
- **knowable-at-T:** At `T ≈ 2000`, an observer could establish that Linden Lab
  had been founded in 1999 and was working toward what became Second Life
  [C03-1].  The sources reviewed establish no admitted population, user-created
  economy, or payment test by that cutoff.  The contemporaneous evidence thus
  exposed the population precondition as unmet; it did not license a forecast
  from later community size.
- **outcome:** `zombie`, used here in the schema's narrow business-model sense:
  enduring and substantial environment value without public evidence in this
  record of commensurate operator capture.  The first documented population
  arrived only with closed beta in November 2002, followed by public beta in
  April 2003 — roughly three to four years after founding [C03-2].  In 2023
  Linden Lab reported 73 million created accounts, roughly 750,000 monthly
  active users, and a USD 650 million annual user economy [C03-3].  Those are
  strong population and environment-value measures, but they are not Linden
  Lab revenue or profit; operator capture is not established by the reviewed
  public sources.
- **evidence-refs:**
  - **[C03-1]** Linden Lab, “About”: founded in 1999 and creator of Second Life —
    <https://lindenlab.com/about>
  - **[C03-2]** Second Life Wiki, “History”: closed- and public-beta dates —
    <https://wiki.secondlife.com/wiki/History>
  - **[C03-3]** Linden Lab, “The Original Metaverse, Second Life, Celebrates
    20th Birthday”: accounts, active users, and user-economy measures —
    <https://lindenlab.com/press-release/original-metaverse-second-life-celebrates-20th-birthday>

## C04 — PlanetMath

- **case-id:** `C04-planetmath`
- **org:** PlanetMath.org, Ltd.
- **period:** 2001–2018 observation
- **node-types:** primary `Environment`; no secondary type
- **chain:** an open collaborative mathematics encyclopedia and its authoring
  system → PlanetMath's volunteer nonprofit community → donors, sponsors, or
  grant funders as contemplated by its administration document, with the actual
  historical payer mix not established [C04-2] → terminal `interest` in freely
  available mathematical knowledge, not an identified obligation.
- **oracle:** Mechanical but slow: independent contributors add and correct
  entries, peer review operates, readers use the resulting corpus, and some
  participants return.
- **oracle-hardness:** `mechanical` — the software can be demonstrated, but the Environment
  oracle cannot be attempted without permission and participation from a real
  author/reviewer population.  Sparse public telemetry makes the strength of
  that signal difficult to reconstruct.
- **distance:** `2` — beneficiaries and volunteer authors exercise the oracle,
  while any donors or grant sponsors pay for a public-good interest rather than
  purchasing their own use.
- **fixture-access:** `n` — founders could invite an initial group, but could
  not supply independent peer review or enduring contribution themselves.
- **knowable-at-T:** At `T ≈ 2002`, an observer could see a project started in
  2001 in response to MathWorld becoming unavailable, expressly organised as a
  free, collaborative, peer-reviewed mathematics encyclopedia [C04-1].  The
  initial contributor population is reported as having emerged from an IRC
  mathematics community `[recollection - unverified]`; exact first-population
  timing, size, retention, and first-year funding were not established from the
  reviewed sources.  The observable proposition was therefore a live small
  community experiment, not demonstrated scale.
- **outcome:** `zombie`, meaning archival or low-activity persistence rather
  than a proved hard shutdown.  The administration document describes a
  nonprofit with no permanent paid staff [C04-2]; public reports document
  substantial site unavailability during 2016–2018 [C04-3]; and a 2018 mirror
  describes the corpus as editable through GitHub [C04-4].  These support
  persistence of the artifact with weak evidence of a continuing on-site
  population.  A precise cessation date, current activity rate, and cumulative
  funding could not be established, so the outcome classification is uncertain.
- **evidence-refs:**
  - **[C04-1]** PlanetMath, “The PlanetMath FAQ”: 2001 origin, free
    collaborative encyclopedia, peer review, and licensing —
    <https://planetmath.org/theplanetmathfaq1>
  - **[C04-2]** PlanetMath, “PlanetMath Administration Main Document”:
    nonprofit governance, no permanent paid staff, and contemplated funding
    sources — <https://planetmath.org/planetmathadministrationmaindocument>
  - **[C04-3]** Mathematics Meta, “Links down to PlanetMath.org”: contemporary
    record of extended unavailability —
    <https://math.meta.stackexchange.com/questions/27794/links-down-to-planetmath-org>
  - **[C04-4]** University of Waterloo PlanetMath mirror: community description
    and 2018 GitHub-editing note — <https://planet2.math.uwaterloo.ca/>

## C05 — GNU Emacs / Free Software Foundation

- **case-id:** `C05-gnu-emacs-fsf`
- **org:** GNU Project / Free Software Foundation
- **period:** 1985–2025 observation
- **node-types:** primary `Environment`; secondary `Dependency`, because user
  configurations and extension packages execute inside and depend on Emacs's
  programmable environment [C05-3].
- **chain:** GNU Emacs and Emacs Lisp as an extensible working environment →
  GNU maintainers and the FSF as sponsors → initially tape and manual buyers,
  later members and donors → terminal `interest` in editing, extensibility,
  and software freedom, with no identified commercial or regulatory
  obligation [C05-1][C05-2][C05-4].
- **oracle:** Mechanical: users adopt the editor, construct workflows and
  extensions inside it, contribute changes, and voluntarily return or support
  its steward.
- **oracle-hardness:** `mechanical` — source availability permits inspection, but a real user
  and contributor population is still required.  Decentralised redistribution
  makes population size intrinsically less observable than hosted-platform
  activity.
- **distance:** `2` — many users exercise the oracle without paying, while
  purchasers, members, and donors support the wider freedom mission.
- **fixture-access:** `n` — the program can be copied freely, but neither GNU nor
  the FSF can supply the independent adoption and contribution that tests an
  Environment.
- **knowable-at-T:** At `T ≈ October 1986`, an observer could see GNU Emacs
  publicly released in March 1985 and repeatedly revised through 1985–1986
  [C05-3].  Stallman's later historical account records USD 150 tape
  distribution and the FSF's October 1985 formation to raise funds [C05-1];
  the GNU history independently dates the FSF to October 1985 [C05-2].  Thus a
  population, iterative contribution, and a modest payment channel were all
  observable within roughly a year; exact user count and retention were not.
  This field does not rely on later longevity.
- **outcome:** `sustained-noncommercially`.  GNU's history records continued
  releases through Emacs 30.2 in August 2025 [C05-3], while FSF historical
  materials describe tape and manual sales and later member-supported funding
  [C05-4].  The durable terminal is mission and user interest, not commercial
  capture; absence of central usage telemetry prevents a defensible cumulative
  population estimate.
- **evidence-refs:**
  - **[C05-1]** Richard Stallman, NYU transcript (2001): early-1985 working
    Emacs, tape distribution, and FSF formation —
    <https://www.gnu.org/philosophy/rms-nyu-2001-transcript.en.html>
  - **[C05-2]** GNU Project, “Overview of the GNU System”: GNU began in 1984
    and the FSF was founded in October 1985 —
    <https://www.gnu.org/gnu/gnu-history.html>
  - **[C05-3]** GNU Emacs, “History”: initial public release and release
    chronology — <https://www.gnu.org/software/emacs/history.html>
  - **[C05-4]** Free Software Foundation, *Free Software, Free Society*,
    historical discussion of Emacs tape/manual distribution and member support
    — <https://www.gnu.org/doc/fsfs3-hardcover.pdf>

**Node-type fit result:** all five organisations fit `Environment` without a
new node type.  Roblox and GNU Emacs additionally expose `Dependency`, but that
does not replace the primary Environment mechanism.  The batch does reveal a
measurement distinction inside the type: population value can be strong while
sponsor capture is weak, private, or noncommercial.
