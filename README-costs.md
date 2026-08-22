# README-costs — what the APM loop actually spends, and on what

> **Measurement correction (2026-08-22):** The dollar figures below predate
> per-message usage deduplication in `claude-spend.py` and overcount spend by
> roughly 2.2×. They are retained as originally reported rather than rewritten.

**Goal (revised 2026-08-19).** **Burn down all three subscriptions — Claude,
Codex, Zai — to full utilisation each week.** The objective is *rate matching*,
not thrift. An unspent Codex or Zai week is wasted money exactly as much as an
overspent Claude week is, and the failure mode to fix is Claude running ahead
while the other two idle.

Everything below the "Levers" section was originally written against a
cost-*minimisation* objective. That was the wrong objective, and **several
recommendations flip sign under the right one** — see *What the burn-down
target changes* immediately below. The measurements are unaffected; only their
interpretation is.

Claude carries a structural disadvantage here that is not a defect: **it is
Joe's primary coding surface.** It will always run ahead. The question is not
how to make Claude cheap, it is which of Claude's work belongs on Codex or Zai.

**Status.** Measured at two levels — whole-box (2026-08-19) and per-seat
across frames 12/13/15 (same day) — root cause identified, **nothing changed
yet.** The config change below is a one-seat experiment to run and measure, not
a global flip to make on the strength of this document.

**The headline, if you read nothing else:** the proving loop is not what is
expensive. Every frame guide and analyst ever run comes to $251. Five
general-purpose standing Claude agents come to $5,279. Re-seating the loop onto
Codex addresses the $251.

## Raw token counts are the wrong unit

Every cost claim made in the `claude-2` buffer on 2026-08-19 was in raw tokens.
Raw tokens do not price:

| usage field | billed at |
|---|---|
| `input_tokens` (uncached) | 1× base input |
| `cache_read_input_tokens` | **0.1×** base input |
| `cache_creation_input_tokens`, 5m TTL | 1.25× |
| `cache_creation_input_tokens`, 1h TTL | **2×** |
| `output_tokens` | output rate (5× input, on Opus 5) |

A cache read and an output token differ by **50×** ($0.50 vs $25 per MTok). Any ratio computed on
`total_tokens` is therefore a ratio of something nobody is charged for. This is
the same shape of error as `total_tokens` tracking time-in-context rather than
work — noted in the buffer, then not applied to the buffer's own numbers.

Opus 5 is $5/$25 per MTok with a 1M context window **at standard pricing —
there is no long-context premium.** Verified against the API reference, not
recalled.

## The measurement (2026-08-19)

All sessions under `~/.claude/projects`, priced at Opus 5 list rates:

```
project dir                          $est    msgs    meanctx   $/msg
-home-joe-code                    5025.36  15,502    471,301   0.324
-home-joe-code-futon3c             615.06   3,160    153,364   0.195
-home-joe                          472.89   1,748    357,488   0.271
-home-joe-code-futon0               17.22     205     91,661   0.084
─────────────────────────────────────────────────────────────────────
GRAND TOTAL                      $6,131.97  20,643    408,622   0.297

  uncached-in $   0.20 | cache-write $1,421 | cache-read $4,147 | output $564
```

Reproduce with `futon3c/scripts/claude-spend.py` (`-s` for per-session);
verified to reproduce this table, modulo a few dollars of drift as live
sessions keep writing. This is the **before-picture** for any change made from
here.

### Three findings

**1. Cache reads are 68% of spend; output is 9%; uncached input is twenty
cents.** The twenty cents matters as a negative result: across 20,643 messages,
cache hygiene is already effectively perfect. There is nothing to win by tuning
breakpoints or hunting silent invalidators. Do not spend a morning there.

**2. A message costs ~$0.30 flat, whatever it does.** A one-line `ls` and a
deep analysis cost the same, because both re-read ~410k of context. This is
what makes the batching habit load-bearing: three-to-six sequential shell calls
per turn cost ~$1.50 where one combined call costs $0.30.

**3. Four long-lived orchestrator sessions are 82% of all spend.** In
`-home-joe-code`: `87332988` ($3,158, 2026-08-10→19, ground control), `82c37430`
($887, 08-15→18), `72fd77ea` ($751, 08-10→13), `626dc23f` ($218, claude-10).
The middle two are evidently prior ground-control incarnations and were absent
from the buffer's account entirely.

### Corrections to the figures recorded in the buffer

- *"I am 91% of all Claude consumption today"* — it is **51%** of all-time
  spend when weighted. The 91% came from comparing a **9-day** session against
  one day of the others. Direction right, magnitude wrong.
- *"the guide is 94% of frame cost, 30× the solver"* — derived from **f13
  alone**, a frame whose solver ran only 36 turns. In f15 the solver ran 212
  turns and the guide is **1.8×** it on cached input, not 30×. The ratio is a
  property of how much the solver happened to do, not of the seats.
- *"frames are 2% of Claude spend"* — this one **understates its own case**.
  Weighted, all frame guides plus both analysts are **$251 against $6,136, or
  4%**, while five non-seat standing agents are 86%.
- *the TTL analysis in this document's own first draft* — see lever 3. A
  per-token rate comparison that ignores cold-miss volume gets the sign wrong.
- *"lowering the ceiling gets ~2.5×"* — the token ratio says ~5×, the weighted
  decomposition says **~2.4×**, because cache writes and output do not scale
  with the ceiling. See below.

## What the burn-down target changes

**Levers that now rank LOW.** Lowering the context ceiling (lever 1) and
batching tool calls (lever 2) both *reduce Claude burn*. Under a burn-down
target that is only useful as a **throttle** — correct if Claude will exhaust
its quota before the week ends, and otherwise actively counterproductive.
Lever 1 in particular degrades Joe's primary coding surface (more compaction,
more file re-reads) in order to leave unspent a quota he wants spent. **Apply
it only when Claude is measurably ahead of pace, and reverse it when it is
not.** It is a valve, not an improvement.

**Levers that now rank HIGH.** Anything that moves work off Claude *onto* Codex
or Zai wins twice — it relieves the arm that is ahead and burns the arm that is
behind. Dispatching the proctor and putting seats on GLM-5.3 are no longer
consistency fixes with a cost side-benefit; they are the main event.

**The instrument already exists — I built a duplicate of it.** See *Use the
instrument that is already there* below. The burn-down is runnable today; it
was never blocked.

## Measured pace (`claude-spend.py --burn`)

```
date           claude $  cl turns       codex tok  cx sess
2026-08-12       473.89      1250               0        0   CODEX IDLE
2026-08-13       505.55      1561               0        0   CODEX IDLE
2026-08-14       895.19      2827               0        0   CODEX IDLE
2026-08-15       687.27      2495               0        0   CODEX IDLE
2026-08-16       585.20      1784     289,961,118        5
2026-08-17       634.23      2088      49,517,223        3
2026-08-18       738.74      2858      96,122,175        7
2026-08-19       830.78      2809     763,935,648       18
```

**Claude has not had an idle day in ten. Codex had four consecutive zero-days
(08-12 → 08-15). Zai cannot be seen at all.** That is the imbalance in one
picture, and it is a *duty-cycle* problem, not a per-turn-efficiency problem.

**Caveat on the Codex column:** these are local rollouts only. The roster shows
`ams-codex-1` at 982 dispatches and `ams-codex-2` at 583 — off-site at Zone,
with their rollouts on their own boxes. Use the vendor percentages below for
pace; use this table only for the *duty-cycle* question of which days were
idle.

## Use the instrument that is already there

**`futon0/contrib/current-usage-report.bb` + `usage-report.el` already report
all three vendors**, surfaced by `M-x stack-hud-1`. This was overlooked while
building `claude-spend.py --burn`, which duplicates part of it. Division of
labour from here:

- **`current-usage-report.bb` / `stack-hud-1` — the canonical pace instrument.**
  It reads vendor *quota percentages*, which is what a burn-down target is
  denominated in.
- **`claude-spend.py` — attribution and counterfactuals.** Per-seat cost, the
  read/write decomposition, the TTL simulation, and the per-day idle series,
  none of which the bb script produces.

### The vendor percentages, 2026-08-19 17:34Z

```
Claude   82% used                                       (stack-hud)
Codex    36% used   weekly, resets 2026-08-20 08:07Z    (vendor rate_limits)
Zai       2% used   resets 2026-08-25 02:04Z            (api.z.ai quota endpoint)
Zai       0% used   web-tools allowance, 4,000 units, resets 2026-09-04
```

**Zai is 98% unspent and its plan level is `max`.** Codex resets *tomorrow
morning* with 64% unspent. That is the burn-down failure in one block, and
neither number needed any new instrumentation to see.

### Correction: "Zai is unmeasurable" was wrong

An earlier version of this document said Zai spend could not be seen until
`repair/zai-token-usage` merged, and made that the blocking item for the whole
burn-down. **That conflated two different measurements:**

- **vendor quota %** — what a burn-down target needs. Live at
  `https://api.z.ai/api/monitor/usage/quota/limit`, already wired into
  `usage-report.el`, working today.
- **per-turn attribution** — which *seat* spent it. This is what the unmerged
  branch provides, and it is needed for per-seat analysis, not for weekly pace.

The burn-down was never blocked. The merge is still worth doing, but it is a
step-3 item, not a prerequisite.

This is the same error as the earlier student-seat claim, made twice in one
session: **reading one instrument's silence as absence in the world.** The rule
that keeps failing to get applied is to ask *which instrument would have
recorded this, and did I check that one?*

## How much Claude work is actually delegable

The same classification applied to the guide, run against the standing agents:

```
                turns   with tools   tool calls   mechanical shell
claude-2 (GC)   9,804    4,422 (45%)     4,422      4,040 (91%)
claude-7        2,625    1,033 (39%)     1,033        907 (88%)
claude-1        2,199      867 (39%)       867        650 (75%)
claude-10         707      280 (40%)       280        280 (100%)
```

claude-2's non-shell work is **279 calls out of 4,422** — 97 Read, 92 Write, 90
Edit. Everything else is shell: git, grep, cat, curl, python3, lake.

So the delegable *shape* is large and consistent — 75–100% of tool calls across
every standing agent, the same profile as the guide's 72–79%. **But the same
batching constraint applies:** the delegable unit is a batch of mechanical work
with a defined question attached, not an individual `grep`. Delegating calls
one at a time costs two turns where one would do, and would raise Claude burn
while lowering Claude's usefulness.

**This is the number that governs the whole rebalancing.** It says the work
Claude is doing is mostly the kind Codex and Zai could do — and that capturing
it depends entirely on being able to specify it in batches.

## Root cause: one line of config

```json
/home/joe/.claude/settings.json:  "model": "opus[1m]"
```

Set globally. `futon3c/scripts/claude-picker` execs a bare `claude` at four
sites (lines 325, 349, 393, 409) with no `--model`, so **every seat inherits
the 1M-context variant.** Context sawtooths 0 → ~1,000,000 → compact, so the
mean sits at ~500k and every message pays ~500k of cache reads whether the turn
needs the context or not.

**Compaction is not broken.** It fires reliably — 18 large drops in the ground
control session, flat context across all ten deciles, no growth trend. The
worry that the kangaroo/compaction setup was failing to hold a long session in
bounds is unfounded. It is holding the session at a ceiling five times higher
than the work requires.

## Levers, ranked by measured impact

**1. Drop `[1m]` on the orchestrator seats.** `--model opus` at the picker's
exec sites, or flip the global setting. Mean context ~471k → ~150k.

**Expect ~1.5–1.9×, not 2.4× and not 5×.** An earlier draft of this document
said 2.4×, projecting cache reads ÷4.5 with writes held constant. Measuring
sessions that *already* run at ~150k shows writes do not hold constant — see
*Compaction converts cheap reads into expensive writes* below. The honest
projection on ground control is **$3,158 → ~$1,700–2,100**.

**The second unmeasured cost is real:** more compactions means more file
re-reads, means more turns, which fights lever 2. **Flip one seat and
measure.** Do not flip globally on a projection.

**2. Fewer, denser messages.** $0.30 flat per message; multiplicative with
lever 1; free. Batch independent tool calls into one turn.

**3. Cache-write TTL — CLOSED, and the first draft had it backwards.** An
earlier version of this document flagged that 100% of writes use the 1h TTL
(2×) rather than 5m (1.25×) — "$1,421 vs $888 if 5m sufficed", implying up to
$533 on the table. **That figure was wrong in sign.** Simulated against the
real inter-turn gaps, switching everything to 5m would cost about **$363
more**, not save $533.

Why: the naive comparison prices the same tokens at two write rates. But a
5-minute TTL produces **~10× more cold misses**, and *a cold miss rewrites the
entire context, not the delta.* On ground control that is 147 cold turns
against 13 — each one re-writing ~482k tokens.

```
session      turns  <5m   5-60m  >60m   always-5m  cold  always-1h  cold  winner
87332988     9,804  98.5%  1.4%  0.1%   $3,131.67   147  $2,890.33    13  1h by $241
82c37430     2,625  98.3%  1.5%  0.2%   $  892.28    45  $  813.04     6  1h by $ 79
626dc23f       707  97.2%  2.7%  0.1%   $  242.05    21  $  192.73     2  1h by $ 49
f13-guide      256 100.0%  0.0%  0.0%   $   38.64     1  $   50.22     1  5m by $ 12
analyst-2      335  99.1%  0.0%  0.9%   $   42.07     4  $   50.47     4  5m by $  8
```

Run it with `claude-spend.py --ttl`. **The simulator reproduces measured spend
to within 0.1%** — claude-2 simulated $2,890.33 input-side + $269.50 output =
$3,159.83 against $3,157.90 actually billed — so its counterfactual arm is
worth believing.

**The result splits cleanly by session shape.** Long, big-context standing
sessions want 1h, because avoiding a rare catastrophic full-context rewrite
dominates. Short write-heavy guides want 5m, because they pay the 2× premium on
a lot of writes and almost never go cold. **The current all-1h policy is
already correct for the sessions that cost the most.** The available gain is a
split policy — 5m for bounded seats — worth about **$58**, which is not worth a
controller.

The mechanism exists if it is ever wanted: `ENABLE_PROMPT_CACHING_1H=1` and
`FORCE_PROMPT_CACHING_5M=1` are both real env vars in the CLI (verified present
in the 2.1.235 binary, alongside `DISABLE_PROMPT_CACHING`). They are
process-level, so a launcher can pick a policy per seat at spawn; nothing
supports switching mid-session.

**A cadence-aware controller is also ruled out empirically:** gaps over 5
minutes are 1.4–2.7% of turns and gaps over an hour are 0.1–0.9%. There is
almost no idle time in these sessions for such a controller to act on.

## Per-seat costs (frames 12, 13, 15 — measured 2026-08-19)

Seats mapped from `GET /api/alpha/agents` (`session-id` per seat) to session
files. Codex is shown in tokens, not dollars: it draws a separate quota pool,
so a cross-vendor dollar figure would answer a question nobody is asking.

```
seat          agent   turns  uncached-in   cached-in    output  reason  meanctx  $claude
f12-guide     claude    284          568  42,421,844   218,044      —   158,493   52.56
f13-guide     claude    256          512  38,695,307   240,285      —   163,155   56.08
f15-guide     claude    241          464  30,790,222   200,897      —   146,462   44.81
f13-solver    codex      36      101,830   2,485,248    18,970   7,504  258,400w    n/a
f15-solver    codex     212      345,815  17,257,984    56,047  23,713  258,400w    n/a
f13-scribe    codex      17        6,067     139,520     1,641     244  258,400w    n/a
f15-scribe    codex      25      105,253   1,588,992     6,510   1,004  258,400w    n/a
analyst-1     claude    233            —           —          —      —   159,072   40.05
analyst-2     claude    335            —           —          —      —   174,271   57.98
analyst-3     claude      1            —           —          —      —    34,512    0.19
```

### Which seats have ever actually been dispatched

Dispatch counts from `:seqs` in `~/.local/state/futon3c/ams-turn-queue.edn` —
this is the instrument to use, because it is per-seat, historical, and covers
Claude, Codex and zai seats alike:

```
       guide  solver  scribe  proctor  student
 f7       7      2       –        –       3
 f8      15      1       3        –       1
 f9       9      1       3        –       1
 f10     10      1       2        –       1
 f11     10      1       2        –       1
 f12     11      3       2        –       1
 f13     10      1       3        –       –     ← student suppressed, on the record
 f14      –      –       –        –       –     ← frame allocated, no seat ever dispatched
 f15      6      3       1        –       –
```

**The student seat ran in six frames (f7–f12)** and has six distinct zai
session ids on disk. It went dark at f13 by an explicit, recorded decision, not
by drift — the frame's own instruction reads: *"Do **not** dispatch
f13-student. The student arm measures 'can zai solve this aided by memories'.
On a problem whose model is provably empty, that measures nothing about
transfer and costs a dispatch."* That is a seat correctly declined, and the
reasoning survives in the queue.

**The proctor has never been dispatched in any of the nine frames.** No
`f*-proctor` key exists in `:seqs` at all. This is the one seat that is
genuinely inert.

**f14 has no dispatch of any seat.** The frame is on the roster with five seats
restored; nothing ever ran in it.

**And the analyst named on the card is not the analyst that worked.** The f13
and f15 frame cards both bind `analyst-3` — which has run exactly **one turn,
$0.19**. The $98 of analyst spend sits in `analyst-1` and `analyst-2`, neither
of which is the seat those frames declare.

### A methodological error worth recording

The first pass of this section claimed the student had *never* run in any
frame. It was wrong, and Joe caught it against his own f10 notes (*"f10
produced the series' first uncontaminated instance — the student derived its
own conjunct from a route memory"*).

The error: I read `"session-id": null` in `GET /api/alpha/agents` as evidence of
non-execution, on a roster that **only retains the last three frames** (f13–f15)
and which, for zai seats, may not carry a session-id at all. An absent field was
read as data, and a three-frame sample was generalised to "any frame".

This is the same failure this project has now hit four times — the `?df=`
filter, the evidence `tag` filter, the `:ids` map of nils, and this. **Absence
in a record is not absence in the world unless you have shown the record would
have carried it.** The fix here was to find an instrument that records
executions rather than registrations.

### The proving loop is not where the Claude money goes

**Three frames of guide + all three analyst sessions = $252.** Against that:

```
claude-2   $3,157.90   9,804 turns   2026-08-10→08-19   ground control
claude-7   $  886.84   2,625 turns   2026-08-15→08-18
claude-1   $  750.99   2,199 turns   2026-08-10→08-13
claude-3   $  265.53     917 turns   2026-08-10→08-12
claude-10  $  217.93     707 turns   2026-08-19
─────────────────────────────────────────────────────
           $5,279.19  = 86% of all Claude spend
```

**None of those five is a seat in the proving loop.** They are general-purpose
standing agents. Moving the Guide seat to Codex would save at most ~$150 and
would not touch the $5,279 — the same shape of error as optimising the frame's
seat roster for cost.

### The cost of a seat "per se"

A Claude turn costs **$0.17–$0.34 whatever seat it sits in**:

```
seat/agent      turns   meanctx   $/turn  = read      + write      + output
claude-7         2,625   490,670   0.338    0.242(72%)  0.067(20%)   0.028
claude-2 (GC)    9,804   482,599   0.322    0.238(74%)  0.056(17%)   0.027
claude-10          707   428,796   0.308    0.211(69%)  0.061(20%)   0.036
f13-guide          256   163,155   0.219    0.076(35%)  0.120(55%)   0.023
f15-guide          241   146,462   0.189    0.068(36%)  0.099(53%)   0.022
analyst-2          335   174,271   0.173    0.084(49%)  0.065(37%)   0.024
claude-6            205    91,661   0.084    0.045(54%)  0.017(20%)   0.022
```

Role is almost not a variable. **Total seat cost = turns × ~$0.25, and the
whole spread is in turn count.** Guides are cheap because frames end at ~250
turns; ground control is expensive because it has never ended. A guide is not a
frugal role — it is a bounded one.

This is the answer to "what does a seat cost per se": **nothing intrinsic to
the role. Seats cost what their session lifetime costs.**

### Compaction converts cheap reads into expensive writes

The reason lowering the ceiling does not pay 5×:

```
                write-tokens/turn   read share   write share
claude-2 (GC)         5,611            74%          17%
f13-guide            11,999            35%          55%
f15-guide             9,928            36%          53%
```

Guides run at a third of ground control's context but write **twice as many
tokens per turn**, and a written token costs **20× a read token** at the 1h TTL
($10 vs $0.50 per MTok). More than half a guide's cost is cache writes.

So shortening a session moves spend from the 0.1× column into the 2× column.
It still wins — $0.19 against $0.32 — but by ~1.7×, not 5×. Any plan that
projects savings from context size alone will overstate them.

Counter-example worth noting: `claude-6` at 91k context and 1,676 write-tokens
per turn costs $0.084. Write rate is a **workload** property (how much fresh
material enters context per turn), not purely a session-length one — which
means the write column is separately attackable, and nobody has tried.

### What Codex in the Guide seat would and would not buy

`f15-solver` ran 212 turns at ~81k mean context against `f15-guide`'s 241 turns
at ~146k — a 1.8× difference, not the order of magnitude the raw-token
comparison suggested. Codex's 258k context window structurally forbids the
expensive regime; that is most of the difference.

**On cost, the case is weak** — the whole Guide line is ~$50/frame. The case
for it, if there is one, is the consistency argument below: a proving loop
whose conductor is Claude is a Claude-dependent loop, whatever its seat roster
says.

## What the Guide seat actually buys (2026-08-19)

The question "is the Guide worth its cost" is answerable from the session
files, because every guide turn is on record. f15-guide: **281 turns, 243,226
output tokens, $44.81.** Anatomy:

```
turns with a tool call          160 (57%)   — all Bash, no Read/Grep/Edit
turns of text only               26 ( 9%)   — acks, findings, reports to Joe
remaining                        95 (34%)   — thinking-only, no visible output
```

Classifying the 162 Bash calls by what they invoke:

```
                          f15-guide      f13-guide
mechanical verification   116 (72%)      111 (79%)    lake/lean, clj, git, grep,
                                                      cat, find, python3, jq
conducting only             0 ( 0%)        0 ( 0%)
mixed                      23 (14%)       21 (15%)
```

**Roughly three quarters of what the Guide does is mechanical verification —
running Lean builds, re-running gates, reading blobs out of git.**

### Which is the proctor's job description, verbatim

The proctor seat was described in the 2026-08-19 buffer as existing for exactly
"the mechanical verification — `lake env lean`, `#print axioms`, gate re-runs".
It has never been dispatched in nine frames.

**The two findings are one finding.** The proctor is not unused because it is
unnecessary; it is unused *because the Guide is doing its work*, at Claude
prices. The inert seat and the expensive seat are the same defect seen from
opposite ends — which is the strongest available form of Joe's consistency
argument: the frame does not merely declare a seat it never uses, it declares a
seat whose job is being done, more expensively, by a seat that has a different
job.

### The delegation is not free — batch or it loses

Turns are the cost unit, so **naive delegation costs more than doing it.** A
guide that runs `lake build` itself spends one turn ($0.19). A guide that bells
a proctor and reads the answer spends **two** ($0.38), plus the proctor's own
turns.

Delegation only pays if the proctor takes **batches**: one dispatch that runs
the whole gate suite and reports, replacing ten or twenty guide turns with two.
A per-command proctor would make the frame more expensive and more consistent
at the same time. Any implementation must be specified batch-first or it will
regress the number it was meant to improve.

## Could Zai hold these seats? (GLM-5.3, released 2026-08-14)

**The model is already configurable** — `zai_api.clj:27` sets
`default-model "glm-5.2"` with a `ZAI_MODEL` env override at `:1316`. Pointing
a seat at GLM-5.3 is a config change, not development.

Published GLM-5.3 rates are $1.4 in / $4.4 out per MTok, cache reads at $0.26
(19% of cold), cache writes free during the launch promotion, 1M context, 128K
output. Applied to f15-guide's actual token profile:

```
                   tokens      Opus 5     GLM-5.3
cached input      30.79M       $16.40      $8.01
cache writes       2.39M       $23.90      $0.00–3.35   (promo / at input rate)
output           200,897        $5.02      $0.88
────────────────────────────────────────────────
                               $44.81      $8.89–12.24     ≈ 3.7–5×
```

The saving concentrates in **cache writes**, which are 53% of a guide's cost at
Opus's 1h-TTL rate and free-to-cheap on GLM. That is the single largest term
and it is the one most exposed to a vendor switch.

### Two things that must be true first, and currently are not

**1. Zai spend is unmeasurable today.** The `:cost/*` usage capture is on
`repair/zai-token-usage` (`48575a1b`) and is **not merged and not live**. Move a
seat to Zai now and there is no way to tell afterwards whether it helped. Merge
that first; it is the instrument for the whole experiment.

**2. Zai has never done this kind of work here.** Every zai dispatch on record
is a *student* turn: 1–3 dispatches per frame, single-shot, "solve this aided
by memories". A guide is a **281-turn agentic conductor** running Lean builds
and holding a frame's state. The system has zero evidence either way about GLM
at that job, and the price table says nothing about it.

### Recommended order

1. **Put load on Zai.** At 2% used on a `max` plan it is the most wasted of the
   three subscriptions by a wide margin, and GLM-5.3 is a one-env-var change
   (`ZAI_MODEL`, `zai_api.clj:1316`). Pace is visible today via the quota
   endpoint, so the experiment is measurable *before* any merge.
2. **Dispatch the proctor** — a Codex seat that already exists, already
   allocated, batch-specified. Removes the ~72% mechanical share from the
   Guide's turns and fixes the consistency defect in the same move.
3. **Merge the zai usage capture** — needed to attribute Zai burn *per seat*,
   which is what turns step 1 from "the number moved" into "this seat did it".

Steps 1 and 2 both carry capability risk on the seat they touch; neither is
blocked on anything.

## Rotating agents through the expensive roles (2026-08-19)

**Architecturally this works.** The Zai seat is not a chat wrapper — `zai_api.clj`
defines **28 OpenAI-style tools**, including the whole agentic surface:

```
run_shell   run_readonly   read_file   write_file   edit_file   list_files   search
+ memory_record/search/read, evidence_graph, pattern_memory, tool_history,
  mission_context, boot_context, repo_contract, reflect_*, irc_send/recent, psr_*
```

`run_shell` is described as "run a shell command in cwd — use for tests,
builds, git diff". That is exactly the Guide's measured profile (72–79%
mechanical shell). So Guide, Scribe and Ground Control are all *reachable* from
a Zai seat; nothing needs building.

### The operational record is thinner than the capability

Longest Zai run on record is **`ams-zai-1` at 27 dispatches**; every frame
student is 1–3. Ground control is **9,804 turns**. That is two orders of
magnitude of extrapolation, and there is a specific reason for caution:

`zai_api.clj:369` implements `detect-stuck!` — a guard that watches for
consecutive identical `(tool, args, result)` triples, warns at 3 repeats and at
5 tells the agent to **"STOP repeating NOW … then bell your reviewer for
help"**. The comment at `:354` records why it exists: *"zai-10's write_file …
sent it into an identical-retry loop"*.

**That escape hatch assumes a reviewer above the agent.** Ground Control is the
top of the stack — a stuck Ground Control has nobody to bell but Joe. Not
disqualifying, but it means Ground Control is the seat where the known Zai
failure mode has the least automatic recovery, and it should not be the first
one rotated.

### Order by blast radius, not by prize

1. **Scribe** — currently Codex, 17–25 dispatches per frame, ~1.7M tokens. A
   scribe's failure mode is a bad record, which the guide catches. Codex → Zai
   is also a pure burn-down win on its own (Codex 36%, Zai 2%).
2. **Guide** — 281 turns, agentic, but it sits *inside a bounded frame and has
   a reviewer*. If GLM-5.3 stalls, one frame stalls visibly and is re-run.
   Blast radius is one frame.
3. **Ground Control** — the biggest prize ($3,158, 51% of Claude spend) and the
   only seat with no reviewer above it. Rotate it last, and rotate it **with a
   session bound**, not into the same 9-day immortal seat.

### The first rotation is a calibration run, not just a safe start

A percentage alone does not give the Zai plan's absolute headroom: "2% used"
cannot be converted into "how much of ground control would fit" without the
token denominator, which the quota endpoint does not expose.

**Moving the scribe measures it.** A known quantity of work, moved onto Zai,
converts 2% into *points-of-plan per unit of work* — which is the number
required to plan steps 2 and 3 at all. Start small because it is the
measurement, not merely because it is cautious.

### Rotation also fixes the immortality problem for free

Cost is `turns × mean context`, and the turn term comes from sessions that
never end. **Rotating a role hands it to a fresh session by construction** — it
bounds session lifetime structurally, rather than by tuning a context ceiling
(lever 1) and degrading the surface to get there. If rotation happens on a
weekly cadence to match the burn-down target, the session-lifetime problem is
solved as a side effect of solving the balance problem.

## The f16 full-recast experiment — what stands in the way

Joe, 2026-08-19: run **one frame** with Scribe, Guide and Ground Control all
recast onto Zai GLM-5.3, starting at Ground Control because it sets the others
up. Scoped for f16; f15 is still live.

### Three green lights

1. **The tool surface is there.** 28 tools including `run_shell`, `read_file`,
   `write_file`, `edit_file`, `search` — see the rotation section above.
2. **The role cards are NOT vendor-coupled.** Checked rather than assumed: the
   only occurrence of "claude" in `claude-guide-v2.1.md` is an attribution line
   naming who drafted it. No Claude-specific tooling, no harness assumptions;
   the scribe card is the same. **The frozen blob transfers to a Zai seat
   unchanged**, so `:reg/role-cards` keeps pinning the same shas and the series
   stays comparable across the recast. The `claude-` filename prefix is
   historical labelling, not content.
3. **The model swap is one env var** — `ZAI_MODEL`, read at `zai_api.clj:1316`.

### The blocker: Ground Control is the only seat with no role card

```
role-cards/  analyst-v1.md  claude-guide-v2.1.md  codex-solver-v3.md
             proctor.md     scribe-v2.md          zai-student.md
             (no ground-control card, at any version)
```

Six seats carry a card pinned by blob in `:reg/role-cards`. Ground Control
carries none. **The seat to be recast first is the only one whose job is
undocumented** — it exists in claude-2's 9-day session and nowhere else.

There is a deadline quality to this: the rotation plan retires exactly the
session that holds the knowledge. **Write the card before rotating, not after.**

### What Ground Control's setup job actually consists of

From `frame-15-registration.edn`, the artefact GC produces:

- **`:reg/known-departures`** — 8 long prose entries, each recording a change
  since the last frame with commit shas, `file:line` references, and an
  argument for why it is declared rather than silently fixed.
- **`:reg/predictions`** — 11 adjudicable predictions, each carrying its track
  record and its traps.
- the structured fields: seats, pins, role-card blobs, caps, invariants,
  stop-rules, required measurements.

This is **cross-frame authoring**, not mechanical assembly — which is precisely
why the seat holding it grew a 9-day session, and why it is the interesting
test of GLM-5.3 rather than a formality.

### There is already a mechanical acceptance test

`gen-registration.bb` transforms a predecessor registration by field diff and
then **refuses to emit (exit 3) if any forbidden token survives the transform**.
It exists because every registration defect in this series had the same shape —
correct fields, previous frame's prose — and because four re-readings by the
author missed what a fresh reader caught in one pass.

So "can GLM-5.3 do Ground Control's setup job" has a pass/fail gate that needs
no judgement:

```
1. emit frame-16-registration.edn via gen-registration.bb   → exit 0, guard passes
2. every :reg/known-departures entry is true of f15→f16      → human review
3. every :reg/predictions entry is adjudicable BY f16        → human review
   (the trap that shipped twice: :analyst-survives-two-frames)
```

Step 1 is machine-checked. Steps 2 and 3 are the review Joe or the outgoing
Ground Control performs — and they are the same review a Claude GC gets today,
so the bar is unchanged by the recast.

### Seat vendors are hardcoded — the recast needs a code change

`agency/frame_seats.clj:6-11` is a `^:private` literal:

```clojure
(def ^:private seat-specs
  [[:reg/solver-seat  "solver"  :codex  nil]
   [:reg/student-seat "student" :zai    :mathematics]
   [:reg/guide-seat   "guide"   :claude nil]
   [:reg/proctor-seat "proctor" :codex  nil]
   [:reg/scribe-seat  "scribe"  :codex  nil]])
```

**Ground Control cannot assign roles to vendors**, and the ground-control card
is right to say nothing about it — the capability does not exist. The vendor is
fixed at compile time, not carried in the registration, so moving Guide or
Scribe to Zai is a source change, not a GC decision or a registration field.

If seat vendor is to become a load-balancing lever, it belongs in the
registration (a `:reg/seat-types` map, with the vector above as defaults) so
that each frame *records* which vendor held each seat. That has a second
benefit worth more than the flexibility: a frame's registration would then
carry its own vendor assignment, making cross-frame comparison after a recast
auditable instead of a matter of remembering what was configured that week.

`mint-seats!` already threads a single `model` for all minted seats
(`frame_seats.clj:68`), so per-seat *model* selection needs the same treatment
as per-seat vendor.

### Proposed sequence

1. **claude-2 authors the ground-control role card** — it holds the context; no
   other agent does. Freeze it by blob and add `:ground-control` to
   `:reg/role-cards`, which also closes a real gap in the series independent of
   any recast.
2. Register `f16-ground-control` as a **zai** seat with `ZAI_MODEL=glm-5.3`.
3. Hand it the card, `frame-15-registration.edn`, `gen-registration.bb`, and
   the f15→f16 change list. Acceptance is the three-step gate above.
4. Only if step 3 passes does the recast extend to Guide and Scribe — those
   seats take their existing frozen cards unchanged.

## The Ground Control card, and the division of content (2026-08-19)

`futon3c/holes/labs/M-apm-demonstration/role-cards/ground-control-v1.md` was
authored by claude-2 — the seat's first occupant, and the only agent holding the
context — and reviewed here. It closes the gap identified above: six seats had a
pinned card and the most senior one had none.

**The division, and why it is a rule rather than a tidy-up.** A role card is
**frozen by blob and pinned into every registration**. A measurement written
into one is still being quoted five frames after it stopped being true — which
is the same defect shape as the stale registration prose `gen-registration.bb`
exists to refuse. So:

- **the card carries behaviour** — batch your tool calls, use the proctor,
  never `git commit -a`, generate registrations rather than copy them;
- **this document carries numbers** — and the card points here for them.

### Review findings, fixed in place

**1. One instruction was wrong, not merely stale.** The card told its next
occupant that *"a sawtooth to ceiling C has mean C/2, so halving C halves your
cost linearly."* It does not: more frequent compaction converts cheap cache
reads (0.1x) into expensive writes (2x at the 1h TTL), and the measured figure
is **1.7x, not 3x**. Left standing, that sentence produces a wrong decision by
the next occupant. Replaced with the measured number and an explicit warning
against linear projection.

**2. The raw-token figures were removed**, not corrected in place — `67x`,
`91%`, `5,203,826,766`. All were computed on unweighted totals, and the 91%
additionally compared a nine-day session against one day of the others. Weighted
it is ~51%. The card now states the *shape* (this seat is the largest single
consumer, and cost is `turns x mean context`) and points here for magnitudes.

**3. "Conducting costs more than solving by an order of magnitude"** was drawn
from f13, whose solver ran 36 turns. In f15 the solver ran 212 and the guide was
1.8x it. Retained as an explicit retraction inside the card rather than deleted,
since the card's own doctrine is that retractions are more useful than the
claims they replace.

**4. Two facts strengthened** with the wider measurement: the proctor has never
been dispatched in **nine** frames, not five; and 72-91% of both the guide's and
ground control's tool calls are mechanical shell — the proctor's job description
verbatim.

**5. Added the batching constraint on delegation**, which the card lacked: a
per-command proctor costs two turns where doing it yourself costs one, so the
dispatch must be batch-shaped or it regresses the number it was meant to fix.

### What was deliberately left alone

The **HOUSE ERROR** section — *"a check that succeeded against a population
nobody had stated"* — is the strongest thing in the card and is untouched. It
also describes, exactly, the two errors made while producing this document (the
student seat, and Zai measurability), both of which were absence-in-one-record
read as absence-in-the-world. The card had the discipline written down before
the analysis using it made the mistake twice.

## What this analysis does *not* settle

**Unused seats.** On cost grounds, cutting a seat that never runs saves
nothing — a proctor that has never been dispatched in five frames costs exactly
zero. That is a true statement about the ledger and it is **not an argument for
keeping the seat.**

Joe's point, 2026-08-19: *if a seat is allocated and never runs, the model
being used is inconsistent.* The dispatch matrix above says where this actually
bites, and it is narrower and stranger than a first look suggested:

- the **proctor** is inert across all nine frames — never dispatched once;
- **f14** is a fully allocated frame in which nothing ran at all;
- the **analyst** named on the f13 and f15 cards (`analyst-3`) has run one
  turn, while the analyst work was done by seats those cards do not name.

The student is *not* an instance of this — it ran in six frames and was
declined in the seventh for a stated reason. A seat refused on the record is a
frame behaving correctly. A frame that declares a five-seat roster and
executes three is not the thing its own description says it is, and every claim
made about "a frame" is then a claim about something that did not happen as
written. That is a soundness question about the demonstration, and it is
adjudicated on its own terms — **the cost ledger has no vote in it.**

The failure mode to avoid is letting "it's free" stand in for "it's fine."

## Caveats

- These are **list API dollars**. Joe is on a subscription with its own
  weighting. Treat the proportions as solid and the absolute dollars as
  indicative.
- Priced at Opus 5 rates throughout; sessions on other models are mispriced.
- Scope is `~/.claude/projects` on this box only — Claude seats elsewhere, and
  all Codex/Zai spend, are outside it. Cross-agent comparison needs the
  `:cost/*` projection (`repair/zai-token-usage`, unmerged as of 2026-08-19).

## Re-measuring

```bash
futon3c/scripts/claude-spend.py        # per project dir
futon3c/scripts/claude-spend.py -s     # per session, with cost decomposition
futon3c/scripts/claude-spend.py --ttl  # inter-turn gaps, always-5m vs always-1h
futon3c/scripts/claude-spend.py --burn # per-day burn per vendor (weekly target)
```

Compare `$/msg` and `meanctx` against the table above. Those two columns are
the whole story: **cost ≈ messages × mean context**, and both terms are
adjustable.
