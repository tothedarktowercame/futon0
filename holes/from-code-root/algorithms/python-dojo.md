# Algorithm: python-dojo — unassisted-attempt practice engine for the CodeSignal assessment

Date: 2026-08-09
Status: spec (run by hand in Claude Code from day 1; encode only what survives three sessions)
Owner: Joe — 7-day window, single terminal assessment
Cross-ref:
  - `M-futonzero-prelim-practice.md` §2.1 (session protocol), §4 (unassisted-attempt scope rule), §6 (teaching tactics), §8.2 (programming-domain column)
  - `M-futonzero-capability.md` §2.1 (functioning vs capability; reproducibility ≥2 sessions)
  - `eoi-engine.md` (the engine-not-co-author discipline this inherits)

## Purpose

Take the assessment cold in ~6 days. The measured skill is narrow and known:
write correct Python 3 fast, from a spec, holding accumulating state in a class,
with no assistance. This engine drills that and only that.

## Two load-bearing invariants

1. **No-AI-writing of code during the attempt.** Claude states the spec and the
   tests, then goes silent. Claude does not hint, does not autocomplete, does not
   answer "is this right?" until the timer ends. This is `M-futonzero-prelim-practice`
   §4's out-of-scope rule ("the attempt must be unassisted to measure genuine
   capability") and it is *also* the assessment's own rule. The practice conditions
   and the exam conditions are identical by construction. Nothing else in the
   design matters as much as this.

2. **Timed and closure-detected.** A session ends on the clock, not on completion.
   Running over is data, not diligence — the real assessment stops at 90 minutes
   regardless.

## Session protocol

Adapted from prelim-practice §2.1, retimed for code. One session = 50 minutes.

| Phase | Time | What happens |
|---|---|---|
| **Cold recall** | 5 min | Claude names 8 idioms; Joe writes each from memory, no reference. Scored before anything else runs. |
| **Attempt** | 25 min | Claude states a spec + visible tests. Silence. Joe writes, runs tests, iterates. |
| **Self-diagnosis** | 5 min | *Before* seeing any solution: Joe writes where he got stuck and what he reached for that didn't exist. This is the highest-value five minutes in the session. |
| **Compare** | 10 min | Claude shows a reference implementation. Not "here's the answer" — a diff-read: where did the approaches diverge, and what did the divergence cost in seconds? |
| **Record** | 5 min | Evidence line appended (below). |

## Teaching tactics (prelim-practice §6, ported)

Available **only** in the compare phase, never during the attempt.

| Math tactic | Python tactic | What it does |
|---|---|---|
| `what-theorem?` | `what-builtin?` | "Three stdlib things close this. Which fits?" — never names one. |
| `why-this-lemma?` | `why-this-structure?` | "A dict-of-dicts works here because… can you see which lookup gets cheap?" |
| `chain-hint` | `pipeline-hint` | "You need sort → group → take. What's the sort key?" |
| `what-cases?` | `what-cases?` | unchanged |
| `where-stuck?` | `where-stuck?` | "What's the goal? What do you have? What's the gap?" |

## Functioning vs capability

The whole reason this isn't just cramming. Per `M-futonzero-capability` §2.1:

- **Functioning** — you wrote `defaultdict(list)` correctly once, with the earlier
  session's shape still in working memory.
- **Capability** — you wrote it cold, in a later session, on a different problem,
  without a lookup.

**Threshold: two clean cold-recall hits in non-adjacent sessions.** Anything below
that is not learned, however good it felt at the time. In a 6-day window the
temptation to score functionings and call them capabilities is enormous, and the
assessment will not be fooled. Track them separately or the trajectory lies.

## Evidence format

One line per session, append-only. Deliberately thin — a fat schema here is
procrastination wearing a lab coat.

```
2026-08-09 | S01 | recall 3/8 | levels 2/5 in 25m | stuck: dict-of-dict init, f-string nesting | new: defaultdict
```

Four numbers matter across the week: cold-recall fraction, levels reached,
minutes-to-first-passing-test, and the count of distinct stuck-points that recur.
Only the last one tells you what to drill.

## The ladder

Derived from the assessment's known shape (five accumulating levels, class holding
state, tests are the spec), not from a general Python curriculum.

**Day 1 — Python 3 basics, cold.** ~~Reimplement three functions from
`scan-and-update.py`~~ — **CORRECTION 2026-08-09, itself corrected the same day:
the file was absent when Day 1 was first set up, so this rung was unrunnable;
Joe then downloaded `python-practice/BibProject/`, and it is genuine Python 2
(print statements, `urllib2`, `except X, e:`). The rung is real, just deferred.**
Substituted for Day 1: eight
scored cold-recall idioms plus a five-level expense-log drill written from
scratch against a visible `unittest` suite —
`~/code/python-practice/day1/`. Covers f-strings, `.split`, comprehensions,
`enumerate`, unpacking, `dict.get`, `sorted(key=…)`, slicing. Writing from
scratch is closer to the target condition than translating anyway.

**Day 2 — state in a class.** `__init__`, instance dicts, methods that mutate and
return. Build a tiny in-memory key-value store with `set`/`get`/`delete`/`count`.
This is the single highest-yield day; the assessment is nearly always this shape.

**Day 3 — dicts as the workhorse.** Dict-of-dicts, `defaultdict`, `Counter`,
`.get(k, default)`, `.setdefault`, iterating while mutating (and why not to).

**Day 4 — full mock, 90 minutes, timed, cold.** Five levels. No preparation the
hour before. Treat the result as a baseline measurement, not a verdict.

**Day 5 — drill the recurring stuck-points from Day 4 only.** Not a general
review. The evidence log picks the content; you don't.

**Day 6 — second full mock, 90 minutes.** Compare trajectories. Expect the delta
to be in minutes-to-level-3, not in levels reached.

**Day 7 — 20 minutes of cold recall, then take the real one.** The 7-day deadline
is for *starting*; the 90-minute clock begins when you open it. Use the whole window.

## Scheduling policy

From prelim-practice §2.2, collapsed to four rules:

- **Due**: any idiom that failed cold recall reappears in the next session's recall set.
- **New**: introduced only on its ladder day.
- **Edge**: a level-5-style twist (retroactive requirement, TTL, versioning) enters
  from Day 3 — this is where mocks are usually lost.
- **Probe**: one idiom from two days back, unannounced, every session. Tests
  persistence, which is the capability signal.

## Failure modes this prevents

- **Framework drift** — the spec is capped at this file. If it grows before Day 3,
  the growth is displacement activity and should be reverted.
- **Assisted-attempt contamination** — the attempt phase has no Claude in it, so
  measured capability is the same quantity the assessment measures. If this
  invariant slips even once, every subsequent evidence line is uninterpretable.
- **Cramming mistaken for learning** — the ≥2-non-adjacent-session threshold
  refuses to count a one-shot as a capability.
- **Reading solutions instead of writing code** — prelim-practice §1.1's failure
  mode. The compare phase is capped at 10 minutes for exactly this reason.

## Out of scope

- Any Python beyond what the ladder names. No decorators, generators, async,
  `itertools` exotica, type-hint systems, or packaging. The email says no libraries
  and no ML; believe it.
- Code quality, style, readability. The email says explicitly you aren't graded on
  these. Optimising them costs minutes you don't have.
- Building the drill runner as software. Hand-run in Claude Code. If it survives
  six days of use, encode it after the assessment — not before.

## Open questions

- Whether cold-recall should be typed or written by hand. Typed is closer to the
  target condition; handwriting is a stronger retrieval cue. Try both by Day 3.
- Whether to sit the mock at the actual hour of day the real assessment will run.
  Cheap to do, plausibly worth it.
- Whether the recurring-stuck-point count is the right drill selector, or whether
  minutes-to-first-passing-test is the better signal. Day 5 will tell you.
