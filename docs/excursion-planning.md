# Stack Excursions: A Human-Factors-Aware Approach

This document captures the planning reframe developed 2026-01-22, recognizing
that sustainable progress requires effort budgeting, not just architectural elegance.

## The Constraint

The FUTON stack spans 8+ repos with 80+ prototypes across devmaps. Previous
planning focused on *what* to build without accounting for *who* builds it
and at what cost.

**Current state**: Late-night sessions (1AM-4AM), day job, unsustainable pace.
This is 否 (Stagnation/Obstruction) territory — high effort, diminishing returns.

**The 180 recordings**: A parallel creative commitment — 1 recording/day for
180 days on a 4-track. This is the *literary* dimension of Mount Analogue,
where characters function like sigils. Non-negotiable practice.

## Daoist Reframe

The 泰 (Tai/Peace) zone identified in MMCA work applies to human effort:

| Zone | Characteristic | Manifestation |
|------|----------------|---------------|
| 乾 (Qian) | Pure yang, overexertion | 4AM sessions, burnout, forcing |
| 坤 (Kun) | Pure yin, withdrawal | Dropped projects, guilt, stagnation |
| 泰 (Tai) | Dynamic balance | Sustainable rhythm, compound gains |

**Wu wei** (無為) doesn't mean inaction — it means not forcing against the grain.
The grain includes: day job, sleep needs, creative practice, variable capacity.

## Stack Excursion Model

### Principle: Vertical Slices with Effort Budgets

Instead of per-futon prototype lists, organize around **cross-stack excursions**
that deliver complete value. Each excursion stage gets:

- **Joe-hours estimate**: Human effort required (target: ≤4 hrs/week sustained)
- **Codex-hours estimate**: Autonomous agent work
- **Coherence gate**: Pattern checks before/after

### Draft Excursion Map

```
Excursion A: Foundation Infrastructure
├── futon1/P0-1: Deterministic substrate [active]
├── futon3/P0: Transport baseline [active]
└── Gate: stack-coherence/futon1-determinism

Excursion B: Pattern Canon
├── futon3/P1: Pattern store [greenfield]
├── futon3/P3: Check DSL [greenfield]
├── futon4/P3: Flexiarg↔Arxana [stub]
└── Gate: library-coherence/library-template-discipline

Excursion C: Mathematical Habitat (futon6 bootstrap)
├── futon3/P1: Pattern Canon (prereq from B)
├── futon6/P1: Pattern/Proof Bridge [stub]
├── futon6/P3: HDM Seedbed [stub]
├── futon6/P4-5: L3/L4 reasoning [stub]
└── Gate: devmap-coherence/ifr-f6-upekkha

Excursion D: AIF Agent Pipeline
├── futon2/P0-4: AIF baseline + goldens [active]
├── futon2/P13: Graph Memory Adapter [greenfield]
├── futon5/E1: Xenotype motifs [greenfield]
├── futon5/P3: Transfer to ants [stub]
└── Gate: devmap-coherence/ifr-f2-viriya

Excursion E: Arxana Memory Atelier
├── futon4/P1: Arxana reboot [active]
├── futon4/P2: Graph memory Datascript [stub]
├── futon4/P6: Scholium mode [greenfield]
└── Gate: devmap-coherence/ifr-f4-passaddhi

Excursion F: Vitality & Rhythm (futon0)
├── futon0/P0-2: HUD + vitality scan [active]
└── Gate: workflow-coherence/sphere-equilibrium

Excursion G: Stack Self-Description
├── futon3/P10: Machine-readable manifest [stub]
├── All futons: IFR convergence check
└── Gate: devmap-coherence/ifr-state-convergence

Excursion H: Civic Scenarios (futon7)
├── futon7/P0-2: Naming + sphere architecture [stub]
├── futon7/P3: Accountability infrastructure [stub]
└── Gate: devmap-coherence/ifr-f7-upa-upekkha
```

### The Missing Pieces: futon3a and futon5a

### futon3a: The 象 (Xiang) Memory Beast

futon3a is the pattern guidance + audit logging layer:

- **Portal**: Drawbridge-based pattern retrieval from Futon1
- **Sidecar**: Append-only audit trail (selections, actions, evidence, promotions)
- **Compass demonstrator**: GFE-inspired scoring connecting patterns to action

Target architecture: flexiarg as serialization format, futon3a as the living
memory layer. Patterns become priors with confidence that updates based on
observed runs.

Key insight: The compass demonstrator already implements GFE scoring:
```
G = epistemic_value + pragmatic_value
  = (how much does this reduce uncertainty?) + (how well does outcome align with desired future?)
```

This applies to personal energy allocation, not just coding policies.

### futon5a: Nonstarter Personal Energy Allocation

futon5a applies nonstarter mechanics to time allocation:

```
Weekly pool: 168 hours (fixed, zero-sum)

Quadrants:
  Q1 Keep-alive    — paid stability, low bandwidth
  Q2 Throughput    — scoped delivery, clear buyers
  Q3 Relational    — sense-making, meetings, coordination
  Q4 Speculative   — self-funded exploration (FUTON lives here)

Mechanics:
  bid(pattern, hours)   → PSR: declare intent
  clear(pattern, hours) → PUR: report actuals
  Δ = clear - bid       → THE SIGNAL (where is time leaking?)
```

**The discrepancy IS the data:**
- Q3 consistently +Δ → relational work draining beyond intent
- Q4 consistently -Δ → speculative work being squeezed
- Sleep consistently -Δ → system is unsustainable

**GFE for personal allocation:**
```
For each probe (project competing for Q4 time):
  Pragmatic: Does this move toward where I want to be?
  Epistemic: Does this reduce uncertainty about my direction?
  G = -(0.6 * pragmatic + 0.4 * epistemic)
  Lower G = more worth starting
```

### The 180 Recordings

- **Quadrant**: Q4 speculative (no external buyer)
- **Bid**: ~7-14 hours/week (1-2 hours/day including setup)
- **Marker**: `marker("recording", :daily)` → expect 7/week
- **Integration**: futon0 telemetry can provide automatic PUR evidence

This is the literary dimension of Mount Analogue — characters in novels function
like sigils. Not code, but required for the climb.

## Pattern Maturity States

From p4ng (Patterns for a New Generation), machine-checkable from text:

| State | next-steps | evidence | Precision Prior |
|-------|------------|----------|-----------------|
| `:stub` | No | No | 0.2 |
| `:greenfield` | Yes | No | 0.4 |
| `:active` | Yes | Yes | 0.8 |
| `:settled` | No | Yes | 0.9 |

## Coherence Patterns

Five namespaces for keeping the mountain from falling apart while climbing:

- `stack-coherence/` — cross-repo alignment, blocker detection
- `devmap-coherence/` — roadmap integrity, prototype alignment
- `library-coherence/` — pattern catalog hygiene
- `workflow-coherence/` — personal practice rhythm
- `code-coherence/` — codebase hygiene

### Coherence as Gates

Before starting a stage:
- `stack-coherence/stack-blocker-detection`
- `devmap-coherence/prototype-alignment-tension`

After completing a stage:
- `stack-coherence/commit-intent-alignment`
- `devmap-coherence/next-steps-to-done`
- `library-coherence/library-evidence-ledger`

## Division of Labor

**Codex**: Heavy stack-wide tasks (maturity audits, blocker detection,
evidence gathering, staleness scans). Can run overnight.

**Claude**: Supervision, interpretation, planning refinement, human-facing
synthesis.

**Joe**: Decision points, creative direction, the 180 recordings, protected
deep work windows for code that requires taste.

## Realistic Effort Budget (Draft)

Using futon5a's nonstarter framework:

### Weekly Pool: 168 Hours

| Category | Bid | Notes |
|----------|-----|-------|
| Sleep | 56h | Non-negotiable (8h/day) |
| Q1 Keep-alive | 12h | Day job, ends August |
| Q2 Throughput | 10h | Scoped delivery |
| Q3 Relational | 10h | Meetings, coordination |
| Q4 Speculative | 35h | FUTON + 180 recordings |
| Maintenance | 25h | Life admin, meals, exercise |
| Slack | 20h | Buffer, recovery |

### Q4 Breakdown (The 35 Hours)

| Activity | Hours/Week | Marker |
|----------|------------|--------|
| 180 recordings | 10-14h | `marker("recording", :daily)` |
| FUTON deep work | 8-12h | `marker("q4-commit", :session)` |
| Review/planning | 4-6h | `marker("q4-review", :weekly)` |
| Codex supervision | 2-4h | `marker("codex-review", :daily)` |

### Good Week vs Survival Week

**Good week** (35h Q4):
- All 7 recordings made
- 2-3 deep coding sessions (8-12h)
- Codex running overnight, reviewed daily
- One excursion stage advances

**Survival week** (15h Q4):
- 4-5 recordings made
- 1 deep coding session (4h)
- Codex maintenance only
- No excursion progress (and that's okay)

### Stop-Rules (The Q4 Kaizen Question)

> "What light stop-rule protects health and choice?"

| Probe | Stop if... |
|-------|------------|
| 180 recordings | Skip 3 days in a row → reassess schedule |
| FUTON stack | Sleep -Δ > 7h for 2 consecutive weeks |
| Any excursion | No progress after 3 weeks → surface as blocker |
| Codex missions | Errors unreviewed for 48h → pause queue |

## Codex Coherence Missions

Heavy stack-wide tasks for autonomous execution:

### Mission C1: Stack Maturity Audit
- Scan all devmaps (futon0-7)
- Rate each prototype: `:stub`, `:greenfield`, `:active`, `:settled`
- Output: `futon3/holes/stack-maturity-audit.edn`
- Gate: `stack-coherence/staleness-scan`

### Mission C2: Blocker Detection
- Parse `blocked-by[...]` declarations across devmaps
- Check if referenced artifacts exist
- Output: `futon3/holes/stack-blockers.edn`
- Gate: `stack-coherence/stack-blocker-detection`

### Mission C3: Evidence Gathering
- For each prototype with `evidence[...]`
- Verify file paths resolve
- Note missing evidence
- Output: `futon3/holes/evidence-gaps.edn`
- Gate: `library-coherence/library-evidence-ledger`

### Mission C4: Cross-Stack Dependencies
- Map which prototypes depend on which
- Build dependency graph
- Identify critical path
- Output: `futon3/holes/dependency-graph.edn`
- Gate: `devmap-coherence/prototype-alignment-bridge`

## Next Steps

1. ~~Audit futon3a and futon5a to understand current state~~ ✓
2. [ ] Set up futon5a weekly bid/clear cycle (start tracking)
3. [ ] Draft Codex Mission C1 (stack maturity audit)
4. [ ] Integrate 180 recordings into futon0 telemetry
5. [ ] First weekly reconciliation (Sunday)

---

*Filed 2026-01-22. Updated with futon3a/futon5a integration.*
*This document is itself subject to coherence checks.*
