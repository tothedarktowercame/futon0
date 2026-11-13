# Atthangika Interest Surface v1.1

*REDUCE-side projection over finalised EOI essays. Bipartite-graph structure of (essay, interest-territory) pairs. Companion EDN: `atthangika-interest-surface-v1.edn`. Algorithm: `~/code/algorithms/eoi-finalisation.md` §REDUCE step.*

**Generated:** 2026-05-26 by claude-3 (`emacs-claude-repl` session). **v1.1 supersedes v1.0** (which was 2-essay scope).
**Scope:** v1.1 over **5 essays** finalised (anthropic, glasgow, hyperreal-side-a, hyperreal-side-b, vsat-poc-scenarios). Operator's Foreword pending v0.2 (needs EOI-shape sidecar — its existing annotations.edn is foreword-flavoured).
**Operator framing (Joe, 2026-05-26):** *"this suggests a bipartite graph structure of EoIs and Interests which could be used later for graph-theoretic analysis."*

---

## Summary statistics

| Metric | Value | Δ from v1.0 (2 essays) |
|---|---|---|
| Essays finalised | 5 | +3 |
| Interest-territories | 21 | +13 |
| Bipartite edges (deduplicated) | 35 | +23 |
| Max interest-degree | 5 (peer-learning, every essay) | +3 |
| Max essay-degree | 9 (vsat-poc; 10 annotations, 9 distinct targets) | +2 |
| Singleton interests (degree 1) | 14 | +10 |
| Shared interests (degree ≥ 2) | 7 | +3 |

## Degree distribution — the recurrence signal

```
DEGREE  INTEREST-TERRITORY                                  ESSAYS
  5     peer-learning-and-distributed-collaboration         ⬛⬛⬛⬛⬛  (ALL FIVE essays; vsat-poc has 2 supporting annotations)
  4     cross-substrate-invariance                          ⬛⬛⬛⬛   (anthropic, glasgow, side-a, side-b)
  4     agentic-coding-as-research-record                   ⬛⬛⬛⬛   (anthropic, glasgow, side-a, vsat-poc)
  2     held-space-analytic-practice                        ⬛⬛     (anthropic, glasgow)
  2     mathematics-and-formal-verification                 ⬛⬛     (side-a, side-b)
  2     non-financial-progress-indicators                   ⬛⬛     (side-a, side-b)
  2     work-as-me-vs-work-as-hyperreal                     ⬛⬛     (side-a, side-b)
  1     14 singleton territories                            ⬛
```

## Joe's three load-bearing interests (the answer to "what do I wake up curious about")

The three interests that recur across more than two essays. **These are the core interests the corpus surfaces**:

### 1. peer-learning-and-distributed-collaboration (degree 6, every essay)

The longest-standing pre-FUTON interest — PlanetMath PhD, 16y Peeragogy co-convening, A First Proof Sprint Arxiv monograph, PLoP 2025, Multaka Oxford community-partner work, Rheingold-vision-of-peeragogy textbook-as-curriculum, UKRN-S open-research analogue. Every essay anchors it.

**Why it's load-bearing:** This is the through-line that connects the most disparate corpus elements (anthropic technical-policy work + glasgow academic postdoc + hyperreal self-authored institution + vsat-poc Brookes consulting). When Joe says "I'd thrive in a team", it's this interest doing the work.

### 2. cross-substrate-invariance (degree 4)

The Right View anchor — properties holding across radically different substrates (geometric, formal, institutional, pedagogical). Cited from Anthropic's "atoms of the future" framing through Glasgow's "intelligence augmentation as distributed mind vs cyborg interface" to Side A's Baudrillard-hyperreality-as-symbol-saturation to Side B's 25-year-arc-build-to-test.

**Why it's load-bearing:** It's the *meta-interest* — the interest in what makes interests durable. Surfaces wherever Joe explains the theoretical commitments of the futon stack.

### 3. agentic-coding-as-research-record (degree 4)

The Right Effort anchor — writing-into-AI-systems as control mechanism; agential coding turns as primary research record. Anthropic-explicit ("use Claude aggressively, creatively, daily"), Glasgow-via-AIF-control-loops-on-coding-agents, Side A-via-auto-communication-in-the-futon-stack, vsat-poc-via-AI-coding-as-cost-reducer in Scenario A's budget.

**Why it's load-bearing:** It's the *contrarian-to-the-discourse* interest. While the broader discourse worries vibe coding degrades writing skill, Joe holds the opposite thesis: writing into AI systems is how you actually control them. Recurs because it's central to Joe's working practice with the stack.

## Hyperreal sub-cluster (Side A + Side B linkage)

Four interests of degree 2 link the two Hyperreal essays specifically:

- `work-as-me-vs-work-as-hyperreal` — the operational separator
- `mathematics-and-formal-verification` — Austin/Berkeley Problems + Lean + Arxiv hypergraphs + Hyperreal-Dictionary-of-Mathematics origin
- `non-financial-progress-indicators` — Side A's substantive Right-Effort ask; Side B's downstream-financial-translation refinement
- `cross-substrate-invariance` (high-degree, but Side A's etik-fiction-to-incorporation framing and Side B's 25-year-arc framing connect deeply)

**Reading:** Side A and Side B genuinely compose — Side A's distinctions become Side B's operational moves. The bipartite graph confirms this isn't just narrative claim; it's structural co-occurrence.

## Anthropic+Glasgow held-space polarity differential (preserved from v1.0)

`held-space-analytic-practice` remains degree 2 between anthropic and glasgow, with the same polarity flip:

- **Anthropic**: `:fit` (connective-tissue IS the discipline at organisational scale)
- **Glasgow**: `:friction` ("Lead" recurrence collapses the analytic asymmetry)

Side A, Side B, vsat-poc don't surface this territory at v1.1 — it may be specifically a frontier-AI/postdoc-academic basin signal rather than a Hyperreal/consulting one. Worth tracking if/when more essays surface it.

## Distinctive interests (degree 1)

The 14 singleton territories cluster into thematic families:

**Hyperreal-Side-A-distinct:**
- `etik-as-anti-kitemark`

**Hyperreal-Side-B-distinct:**
- `business-validation-via-preregistered-study`
- `bootstrap-economic-self-financing`

**vsat-poc-distinct (Joe's freshest essay; 7 of 10 of its interests are essay-specific):**
- `bayesian-tradeoff-strategic-shape` (the intro's services-first/hybrid/software-first framing)
- `knowledge-commons-co-creation` (Scenario A)
- `vsat-as-thinking-tool` (Scenario B; the surprise discovery)
- `sharing-vs-commerce-tension` (Scenario B; explicit)
- `technology-first-deliverable-shape` (Scenario C tag)
- `writing-stories-in-3D-bob-brown-homage` (Scenario C natural-next)
- `speculative-vs-grounded-design` (Scenario A's "I'm not sure" honesty)

**Glasgow-distinct:**
- `open-research-as-institution` (the C5-clause strongest-fit point)
- `epistemology-of-self-representation`
- `sorry-factory-structured-absences`

**Anthropic-distinct:**
- `closing-the-loop-on-AI-development` (Challenge Area 3 by name)

## Path-arrow coverage (across the corpus)

| Arrow | Witness count | Coverage |
|---|---|---|
| Right View | 5 (every essay) | ✓ complete |
| Right Intention | 4 (all except glasgow) | gap at glasgow |
| Right Speech | 5 (every essay; vsat-poc collapsed via witness-fixing) | ✓ complete |
| Right Mindfulness | 5 (every essay) | ✓ complete |
| Right Livelihood | 5 (every essay) | ✓ complete |
| Right Effort | 1 (side-a clarity-probe surface) | thin |
| Right Concentration | 1 (glasgow via sorry-factory territory) | thin |

**Reading:** the 5 outward/inward EOIs hit View / Speech / Mindfulness / Livelihood reliably. Right Intention is missing at Glasgow (the pre-application register is too compressed for predecessor-anchoring). Right Effort surfaces only via Side A's clarity-probe (its absence in other essays is itself signal). Right Concentration is thin (sorry-factory is barely tagged).

## What the 5-essay graph reveals that the 2-essay graph couldn't

1. **The recurrence hierarchy emerges.** With N=2 every shared interest had identical degree; with N=5 there's a clear ranking. Peer-learning is markedly central; below it, cross-substrate-invariance and agentic-coding are close seconds.
2. **Hyperreal sub-cluster is structural.** The Side-A↔Side-B link via 4 territories isn't an artefact — it's the composition Joe's design anticipated.
3. **vsat-poc is a fresh-content essay.** 7 of 10 territories are essay-specific. This is the essay carrying the most novel content; if Joe's interests are evolving anywhere, it's here.
4. **Anthropic is the most-overlapping-yet-fewest-territories essay.** Degree 5 (lowest essay-degree), but 4 of 5 are shared with at least one other essay. This is what Joe's diagnostic note called "cleanest fit yet seen with the live prior" — the corpus's existing structure already prefigures the Anthropic application.

## Joe's reweighting question — empirical update

> *"do we reweight any of the markers, or just add a bigger bucket of diverse interests?"*

With N=5 the **unweighted-edges + polarity-attribute** approach is already informative — peer-learning emerges as #1 by pure recurrence; cross-substrate-invariance and agentic-coding tie for #2. No reweighting needed to surface the top tier.

Where reweighting would help next:
- **Severity-weighted edges** would let `:high`-severity edges dominate. Currently all 6 peer-learning edges count equally; in practice, anthropic's peer-learning anchor (16y Peeragogy co-convening as proof-of-collaboration) carries more weight than vsat-poc Scenario A's "Multaka volunteers" mention. Severity tagging is already in the annotations; just not used in edge weight.
- **Polarity-signed weights** would separate fit-bearing edges from friction-bearing ones. Useful if Joe wants to ask "which interests am I trying to *get away from*" rather than "which am I drawn toward".

**Recommendation refined:** the bipartite-graph shape is already producing signal at N=5. Reweighting is a v1.2 question — defer until specific decisions need it.

## Rolls forward when…

- **Operator's Foreword gets an EOI-shape sidecar.** It will likely add: `futonic-discipline` (probable new territory, possibly a parent of cross-substrate-invariance), `intelligence-augmentation-not-autonomy`, `morphogenesis-natural-shape` (currently bundled into closing-the-loop). May reveal that `closing-the-loop-on-AI-development` should de-cluster from `morphogenesis`.
- **A v0.2 operator-doubt bipartite layer** lands. The cross-essay operator-doubt clustering (free-solo rule, frame-conventions-rhythm, foreclosure-cost) is currently flat; a `(essay, doubt-cluster)` graph would surface them as a second projection.
- **A derived `:eoi/foreclosure-inventory` territory** lands. Both Anthropic and Glasgow surface the same foreclosure-cost cluster (Hyperreal, UKRN-S, geographic continuity, five-Erics). Side A and Side B add joyfulness-vs-slog as a related foreclosure-cost concern.

---

## Findings summary (in one breath, for Joe)

**Joe's three load-bearing interests, in degree order:**
1. *peer-learning-and-distributed-collaboration* (every essay; the longest-standing thread)
2. *cross-substrate-invariance* (4 essays; the theoretical-commitment thread)
3. *agentic-coding-as-research-record* (4 essays; the working-practice thread)

These three answer the philosophical question — *"what do I wake up curious about?"* — and they're stable enough across the corpus that the bipartite graph isn't just confirming what Joe already knew; it's quantifying which interests are corpus-load-bearing vs essay-incidental.

*Algorithm: `~/code/algorithms/eoi-finalisation.md`. Companion EDN: `atthangika-interest-surface-v1.edn` (bipartite-graph data; recomputable from the per-essay annotations.edn files).*
