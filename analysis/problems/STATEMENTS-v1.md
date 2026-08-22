# Statements for adjudication — v1

**Date:** 2026-08-21 · **Author:** claude-13 · **For:** Joe to weigh.
Each statement is evidenced from something verified in session
`66f62b84`, carries a falsifier, and has a verdict slot. **Joe is the oracle
here** — which is not `no-self-certification`-clean, and is the right standard
for a capability-mapping exercise about his own work. Noted, not hidden.

Verdicts: `AGREE` / `DISAGREE` / `NEEDS-WORK` / (blank = unadjudicated)

---

## A. Revenue and external contact

**S1. FUTON has had exactly one paying external customer, and the payment was
for delivered work, not for the mission apparatus.**
*Evidence:* `:warm-customer-pays` is `:status :satisfied`, grounded in "VSAT
proof-of-concept invoice + log.edn + ledger/statements", `:minted-by
["external/vsat-poc-customer"]`, witness "Oxford Brookes / Eric White". Joe,
2026-08-21: "while Eric technically paid for the missions, he didn't pay for
them 'as such'."
*Falsifier:* a second invoiced customer, or evidence the format itself was
priced.
*Verdict:*

**S2. The warm channel converts; the cold channel has not.**
*Evidence:* `:warm-customer-pays` `:satisfied`. `:cold-response-conversion`
`:held`, `:position "n=1 verbal, £5000 of March work, unsigned;
operator-attested 2026-08-15"`, `:next-rung "a countersigned MoU"`.
`:cold-eoi-sent` n=2 (hyatt 2026-07-05, message-id witnessed).
*Falsifier:* a countersigned engagement originating from cold outreach.
*Verdict:*

**S3. The system's own records systematically under-report its external
traction.**
*Evidence:* four independent instances in one session — (a) all four cold-*
star *titles* say n=0 while their `:position` fields say n=1–2; (b)
`starmap_to_capability_graph.bb` projects `:title :status :claimed :minted_by
:scope :frontier` and **not** `:position`, so the machine steering by the
capability graph cannot see the counts; (c) `:warm-customer-pays` is
`:satisfied` and was not surfaced in any business-model discussion until it was
searched for directly; (d) I was about to freeze a falsifier ("no mission
completed and paid for") already contradicted by an invoice.
*Falsifier:* show the counts are visible to a consumer of the projected graph.
*Verdict:*

## B. The capability space

**S4. The capability space is narrow and nameable, not general.**
*Evidence:* 23 `:satisfied` stars cluster into seven groups — multi-agent
orchestration; hypergraph/evidence substrate; mathematical corpus mining;
lead/interest scanning; pricing & certification (`kit-*`); self-audit; external
witnesses. Stated as one shape: *multi-agent orchestration over a hypergraph
evidence substrate, specialised to mathematical literature, with a
pricing/certification layer and a self-auditing trace chain.*
*Falsifier:* a satisfied star that fits none of the seven.
*Verdict:*

**S5. We cannot currently order the capabilities in time.**
*Evidence:* `M-capability-star-map.graph.edn` contains 37 capabilities and
**13 date strings in the whole file**; the only date-shaped field is `:since`.
*Falsifier:* a dated mint record per star — recoverable from `git log -p` on
the graph file, which is not yet done.
*Verdict:*

**S6. The dependency structure is too sparse to classify workbench-vs-work
mechanically.**
*Evidence:* the `:edges` block holds **18 edges over 37 capabilities** (17
`:requires`, 1 `:couples`); maximum in-degree is **2**
(`wm-steps-forward-guardrailed`, `math-ct-prior-substrate`). Most capabilities
record no dependency at all. So "high in-degree = workbench" cannot be computed
today.
*Falsifier:* a denser requires-graph, or a different mechanical signature.
*Note:* Joe's workbench reading (Agency as the first woodworking project being
the bench) is almost certainly right; the point is that the graph cannot
currently *demonstrate* it.
*Verdict:*

## C. Attribution and the record

**S7. Mission attribution via autoclock-in is populated but unreliable.**
*Evidence:* 47 of the last 60 joe turns are tagged `M-apm-demonstration`,
spanning at least three distinct workstreams — a business-model analysis, an
FTS/pagination bug hunt, and genuine APM work — while a fourth concurrent
stream (E-inbox-zero) carries `None`. The clock is session-sticky, not
content-sensitive.
*Falsifier:* show the tag tracks content.
*Corollary:* "65% mission-linkage coverage" (claude-13, earlier this session)
measured population, not correctness, and should not be relied on. The
**unclocked** entries are the honest ones, so coverage scores the truthful case
worse.
*Verdict:*

**S8. Term profiles cannot attribute value, but can falsify a declared
attribution.**
*Evidence:* per-mission term profiles separate cleanly and semantically on
small corpora — `M-futonzero-prelim-practice` (23 turns) yields
`prefillop, stepplan, llm, tokens, syntax`; `M-evidence-landscape-index` (10
turns) yields `candidate, attribute, p-query, clj-kondo, check-parens`. Two
registers, no labelling.
*Falsifier:* a mis-clocked day the detector fails to flag; today is a known
positive and is the natural test case.
*Verdict:*

## D. The problem ledgers

**S9. The three problem ledgers are biased toward problems that were noticed;
only reverse morphogenesis over the documented surface can recover the silent
ones.**
*Evidence:* WR entries arise from friction (28 of them, IF/HOWEVER/THEN/BECAUSE
form); `:held` stars are things that stalled (13); missions are things someone
chose to undertake (232 files, 146 with an explicit `**Status:**`). A problem
solved smoothly produces a working artifact, a doc entry, and no record.
*Method:* `futon-theory/reverse-morphogenesis` — 象 ← 香 requires **both** a
form and a salience; without salience it degenerates into just-so history.
*Salience source:* `context-retrieval` evidence events carry scored, ranked
results (`{:id … :score 0.4116 :rank 1 :retrieval-source "futon3a"}`) — which
documents actually get pulled, unmanufactured.
*Falsifier:* show the noticed-problem ledgers already contain the smooth cases.
*Verdict:*

**S10. High activity does not imply problem-holding.**
*Evidence:* WR rulings by month — Feb 3, Apr 4, Jul 4, Aug 2, and **May 0,
June 0** — against joe-turn volume of 2,307 (May) and 2,560 (June), the two
highest months at the time. Turn counts from the evidence store,
`author=joe`: Feb 55, Mar 41, Apr 516, May 2307, Jun 2560, Jul 2710, Aug(1–21)
2791; 10,980 total since 2026-02-18.
*Falsifier:* WR-numbered rulings dated May–June living outside
`futon3/holes/war-room.md` (15 of WR-1..28 do live elsewhere and are unchecked
for dates).
*Verdict:*

## E. The business position

**S11. No business-model case in the 30-case corpus terminates in an external
demand-side obligation.**
*Evidence:* `bb check.bb` over `records/*.edn` — `all-terminals-interest`
**holds, n=30**; `no-external-obligation` **holds, n=30**; 0 expectation
mismatches, 0 load errors. Obligations that do exist are either
contractual-delivery (downstream of the sale) or vendor-manufactured (Docker,
Lightbend, Redis, Wolfram, OpenHands).
*Falsifier:* one case with a citable external mandate and a named holder. The
strongest untested candidate is university impact (REF/funder), which is why
its record is encoded `:interest` with a discharge condition rather than
asserted.
*Verdict:*

**S12. The single discriminator that sorted outcomes is a per-unit acceptance
event.**
*Evidence:* within the Environment row, `:acceptance-event :per-unit` →
Minecraft, Roblox scaled and Second Life open; `:none` → PlanetMath zombie,
GNU Emacs sustained-noncommercially. Andela (per-unit, per placement) scaled;
AppJet (aggregate) exited sideways.
*Caveats:* N is small, outcome labels are encoder-assigned, causation
unestablished, and `no-acceptance-implies-zombie` is **refuted** by Redis —
diagnosed as `:acceptance-event` being read off the artifact rather than the
capture vehicle.
*Verdict:*
