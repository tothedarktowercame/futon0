# Intake: Alinery and Mastra (live mode, first run)

Algorithm: `~/code/algorithms/conduct-problem-intake-with-route-questions.md`
(spec of 2026-10-11, run by hand). Governing flexiargs read before running:
futon3 `library/parsing/adopt-the-goal-not-the-method`,
`run-the-reference-before-modelling-it`, and the other three in `parsing/`.

Run by claude-13 on 2026-10-11. Requester: Joe. Material:
`deep-alinery.md`, `deep-mastra.md` (futon0 19d64a3).

**Adaptation (recorded as evidence about the algorithm):** the algorithm says
one record per problem. This is one problem with two neighbours, so there is one
record: Q1 and Q9 are shared, and Q2–Q8 are answered per neighbour.

## Request (verbatim, from M-landscape-positioning §6)

> Alinery and Mastra are probably the two projects that are most worth doing a
> deep dive on. [...] my overall sense is that I have been coming at similar
> problems to SF folks, but with a much more theory-heavy approach (Category
> Theory & Active Inference) and that I want to be able to demonstrate what's
> good about my approach while learning from theirs.

> I'm not convinced yet that I should be *using* their code, and similarly for
> any of the other stuff, but I'm also not convinced that I should *not* be.

> We can test out the [intake algorithm] on this material at a suitable juncture.

Handed over with the request: the 10-08 triage rule ("ignore / reuse if open /
replicate if interesting but closed"), and the deep-dive method (read their code,
map concepts, design demonstrations).

## Q1 Goal (INFERRED, not yet confirmed by Joe)

Draft acceptance check: *for each neighbour, at least one demonstration has
actually been run, and its result can be shown to a third party. The
demonstration compares FUTON with the neighbour's real, running code on the same
input. Its outcome shows one of two things: a thing the theory-heavy approach
does that the neighbour's does not, or that it buys nothing there. Alongside it,
a short list records which of the neighbour's mechanisms Joe has decided to use,
copy, or leave, each with a reason.*

Two halves, both from the request: "demonstrate what's good" (a run result, not
an argument) and "learning from theirs" (decisions on borrowables). "Use their
code?" is deliberately left open by Joe; Q1 does not presume an answer.

**Confirm with Joe:** is "shown to a third party" right, and who is the third
party (Q7)?

## Q2 Method that arrived with the request

- *10-08 triage rule* (ignore / reuse / replicate). A constraint Joe actually
  holds: no paid services whose spec he doesn't trust. Habit only: "replicate" as
  the default for interesting things. Joe has since said he doesn't want to
  write open source he doesn't find useful.
- *Deep-dive method* (read, map, design demos). Habit, not constraint. Its output
  is a **map of the reference**, which is the first stage of the failure shape
  ("a map of the reference, then a copy checked against the reference, then the
  reference itself").

## Q3 Authority (what defines correct)

| Question | Authority | Version |
|---|---|---|
| What does Mastra actually do? | Mastra running | `@mastra/core` on npm, matching clone ebe143c4 |
| What does Alinery actually do with a playbook? | Alinery's own parser and scheduler | release v0.22.0 (2026-10-07); clone cfc0d1e |
| Is FUTON's approach "good"? | Joe's assessment of a run result, then the third party in Q7 | n/a |
| Should a mechanism be used? | Joe (assessment, not ruling) | n/a |

## Q4 Executable: can the authority simply be run here?

- **Mastra:** plausibly yes. Node v24.9.0 and pnpm are installed. Workflows,
  suspend/resume and snapshots need no model. Memory, scorers and routing need a
  model, and **there is no model API key in this environment**. Possible bridge:
  `agent-sdks/` wraps the Claude Agent SDK, which could run on the existing
  subscription. Untested.
- **Alinery:** plausibly yes for the core. Its release ships
  `Alinery-x86_64-unknown-linux-gnu.tar.gz` (226 MB). There is no Rust toolchain
  here, so building from source is out. Unknown whether the release includes the
  `alinery-mcp` / `alineryd` binaries in a form that runs headless.

**Finding from Q4 (before any test):** both deep dives' demonstrations are, in
places, *models of the reference* and not runs of it.
- Alinery D1 and D2 parsed playbooks with our own script, not Alinery's parser.
- Mastra D3 compares minus40 against "an observational-memory-*style*
  summary", not against Mastra's Observational Memory.
- Mastra D1 *redraws* a config as a wiring diagram and never runs it.

This is the failure shape's first stage. Q4 says to try running the reference
first.

## Q5 Comparator

The planned demos compare FUTON outputs with the neighbour's behaviour. The
comparator should *be* the neighbour's code: D3's comparator should be real
`@mastra/memory` OM, and Alinery D1's should be Alinery's parser and scheduler,
with our script kept only as a cross-check.

## Q6 Two readings of the planned measurements

| Measurement | What our method needs | What the target already provides |
|---|---|---|
| Alinery playbook graph (D1/D2) | a structure to run cascade checks on | its own scheduler's dependency resolution, and its refusal when grouping is ambiguous. That is a check we could *use* rather than re-derive |
| Mastra cost-cap fail-open (deep-mastra §4) | a gap R11 would close | a processor hook (`processInputStep`) where *our* budget check could run inside Mastra |
| Mastra OM (D3) | a foil for minus40 | an ingest pipeline (observer/reflector) that could *feed* minus40 |
| Mastra workflow snapshots | evidence that park is weaker | a typed suspend/resume store that Agency park could copy, or call |

The second column is where "use their code" answers start to appear.

## Candidate routes (guesses written BEFORE tests)

| # | Route | Cheapest decisive test | Guessed cost | Guessed outcome |
|---|---|---|---|---|
| R-a | Argue from the write-ups (the method as handed over) | none; already done | 0 | persuasive to us, not to a third party |
| R-b | Model the neighbour and run demos against the model | write the OM-style summariser / our parser | 1–2 days | runs, but proves only "beats our model of them" |
| R-c | **Run the neighbour**, and demo against the real thing | T-M1, T-A1 below | 1–2 h for the tests | Mastra workflows run; Alinery uncertain |
| R-d | Run the neighbour *with FUTON parts inside it* (our check as a Mastra processor; minus40 as a Mastra memory store; cascade check on Alinery's resolved graph) | after R-c passes: one processor | half a day | the strongest "use" evidence, and the strongest demo |

Tests:
- **T-M1:** in a scratch dir outside the futon repos, install `@mastra/core`,
  then run a two-step workflow with a suspend/resume and an in-memory store. No
  model. *Guess: 20 min, pass.*
- **T-A1:** download the v0.22.0 Linux release into a fresh dir and verify its
  sha256. Then list the binaries, and see whether `alinery-mcp` runs headless and
  lists the bundled playbooks via its own parser. *Guess: 30 min, 50/50, because
  the app may need a display.*
- **T-M2 (model access):** can a Mastra agent call a model here without a new
  paid key? *Guess: blocked. Route the access question to Joe.*

## Test results (2026-10-11)

Scratch dirs, outside the futon repos: `~/code/external/try-mastra`,
`~/code/external/try-alinery`. Installs and runs ran under
`systemd-run --user --scope -p MemoryMax=…`.

**T-M1: PASS** (guess 20 min, actual about 10 min).
- `@mastra/core@1.75.0` and zod installed in 8 s. A two-step workflow, `draft → approve`,
  suspended at `approve` with a typed question payload.
- **T-M1b (real restart):** process A started the run against a LibSQL file
  store (`@mastra/libsql`) and exited. Process B, a fresh Mastra instance, resumed
  it by run id and got `success {"approvedBy":"joe"}`.
- A resume payload of the wrong shape was refused: `Invalid resume data: approvedBy: expected
  string`. A second resume of a finished run was refused: `This workflow run was not
  suspended`.
- Scripts: `t-m1.mjs`, `t-m1b-start.mjs`, `t-m1b-resume.mjs`.

**T-A1: PASS** (guess 30 min and 50/50, actual about 25 min; the guess that it
needed a display was wrong).
- Release v0.22.0 (Linux), sha256 OK. It ships `alinery-mcp`, `alineryd`,
  `alinery-runner` and `omp`. `alinery-mcp` runs headless over stdio, given a
  scratch `HOME` and an `app.toml` listing a scratch repo in `known_repos`.
- It lists 14 bundled playbooks (and 26 tools), which confirms the deep dive's
  corrected counts. Its own validator reports **0 diagnostics on all 14**.
- **Then a decisive check against their real validator** (`alinery_save_playbook`). It
  accepted all three of:
  - a control copy of `bug-hunting`;
  - `bh-dangling`: the `design` step needs `never-produced.md`, which no step outputs and
    which is not the ticket;
  - `bh-cycle`: the first step, `rca`, needs `fix-design.md`, which only `design`
    produces, and `design` needs `rca`'s output. So no step can ever start.
- All three were saved with `diagnostics: []`. The validator's error codes
  (`alinery-core/src/playbook.rs`) are all syntactic: `invalid_toml`,
  `missing_field`, `duplicate_key`, `invalid_selector`, and so on. None checks
  reachability.
- What their runtime does with `bh-cycle` was **not tested**: that needs a
  harness and a model.
- Files: `repo/.alinery/playbooks/bh-*/playbook.md`, `save-reqs.jsonl`.

**T-M2: BLOCKED on access (as guessed); unblocked later the same day, see below.**
- No model API key in this environment.
- A bridge exists: `@mastra/claude` (`mastra/agent-sdks/claude`) wraps the
  Claude Agent SDK, which signs in the way Claude Code does, so it might run on the
  existing subscription. Untested.
- Whether to spend subscription quota on Mastra experiments is Joe's call.
  Routed to Joe (stop condition 2).

## T-M2 unblocked (Joe, 2026-10-11: "you can use the claude credentials to test mastra")

Same scratch dir `~/code/external/try-mastra`. No API key: everything goes
through the Claude Agent SDK on the subscription (claude-haiku-5-5).

**T-M2a: `@mastra/claude` (Mastra's own wrapper): PASS but narrow.** One
`generate()` returned `"pong"` in 2.6 s. However, `ClaudeSDKAgent` bypasses
Mastra's agent loop: `supportsMemory()` returns false, and no processors,
tripwires or scorers run inside it. It is a Claude Code run with Mastra
tracing around it. Each call carried about 14k tokens of the Claude Code
system prompt.

**T-M2b: Mastra's *native* Agent on the subscription: PASS.**
- `sdk-model.mjs` is a 60-line AI-SDK `LanguageModelV2` whose calls go to
  the Agent SDK. It passes text in and text out, with no tool calls.
- With it, Mastra's own loop runs with LibSQL message-history memory, across two processes: B recalled
  A's "23rd Street, beer and chips". That took about 600 tokens per call.
- **Adapter trap (mine, fixed):** without `strictMcpConfig: true` and
  `ENABLE_CLAUDEAI_MCP_SERVERS=false`, the SDK loaded Joe's claude.ai connectors
  (Drive, Gmail…). That made each call about 42k tokens, and the agent offered to "search your
  Drive". An instruction I had appended also overrode Mastra's observer prompt, so
  no observations were written. The first OM run (268k tokens) is void for that reason.

**T-M3: Observational Memory, facts that change over time: Mastra wins.**
- Five dated notes about a corner store (stock changes, a move from 23rd St to
  40th and Telegraph). With `lastMessages: 2`, the raw notes drop out of context.
  Then four as-of questions were asked.
- All four were answered correctly from OM's observations.
- The observer wrote both times, as the deep dive described:
  `(10:19) User noted 2 June …: … greens dropped. (meaning June 2, 2026)`.
- 10 calls, 20.6k tokens. Script `t-m3-om.mjs`; calls logged in `m3-calls.json`.

**T-M4: the same, with reflection forced (`observationTokens: 200`): Mastra
still wins.**
- Reflection ran twice (generations 1 and 2 in `mastra_observational_memory`). The
  reflector *consolidated* the notes into a dated timeline rather than dropping the
  superseded ones. All four as-of answers were still correct.
- Each reflection took several attempts (calls 8–11, 13–17), as it compressed by
  levels. 19 calls, 120k tokens.

**What this does to deep-mastra D3.** The demonstration as designed
("minus40 as-of queries beat an OM summary") **failed at this scale**. That is
the deep dive's predicted "Mastra might win" case, and it came out that way.
- OM keeps dated history well when the operator *dates every statement*.
- Where valid-time storage should matter is narrower, and still untested:
  - *undated* supersession ("we sell greens now"), where the date is only the
    turn time;
  - facts that reflection merges silently;
  - "what did the memory *say* on 1 August?", which is the system-time side. OM has
    generations but no as-of read of them, though
    `getObservationalMemoryHistory` lists them.
- That is the refined D3, not run. The honest summary so far is that Mastra's
  prompt-level approach to time is good enough for dated notes.

## What the tests change

- **Alinery D1 now stands against the real reference, and is stronger.** The deep
  dive found the `{{TICKET_FILE}}` gap with our own parser, and added fairly that
  "a one-line check would find it too". Their real validator has no such check,
  and it accepts a playbook that can never start. So the question the cascade view
  asks (does every step have a common origin in the initial inputs?) is missing
  from the shipped product, not merely easy to add.
  - It remains a reachability check, not a deep CT result. The honest claim is that
    the theory *asked* the question. It does not need the theory to *answer* it.
  - This is also an easy patch to offer upstream (open question for Joe: an
    outward-facing act).
- **Mastra's suspend/resume is real and cheap to use.** The typed resume payload and
  the refusal of a double resume are both things Agency park lacks (see the
  missing-session-id bug, memory `park-needs-session-id`).
  - So route R-d ("FUTON parts inside the neighbour") is reachable for workflows
    today. For memory, scorers and routing it waits on T-M2.

## Chosen route (provisional, because Q1 is unconfirmed)

**R-c, then R-d.** Demonstrations run against the neighbours' real code; where
possible, FUTON's piece runs *inside* the neighbour.
- R-a (argue from the write-ups) stays as context only.
- R-b (model the neighbour) is dropped for anything the tests show can be run.

First two, once Joe confirms Q1:
1. **Alinery:** the "common origin" check run against Alinery's own output for
   the 14 bundled playbooks plus the two mutants, reading the
   `definition` JSON that `alinery_read_playbook` returns (their parser, not ours).
2. **Mastra:** one FUTON check as a Mastra processor. The candidate is a budget
   check on `processInputStep` that fails closed where `TokenCostControl` fails
   open. It is model-free if driven by a mock model, as Mastra's own tests do
   (`MockLanguageModelV2`).

## Q7 Consumer (who reads the output)

Unknown, so ask Joe. Candidates:
- Joe himself, for the use / copy / leave decisions;
- Rob, or the futon-2026 paper's readers, for "what's good about the approach";
- the neighbours themselves (an upstream issue or PR to Alinery).

Each reads a different artefact. Per Q7, the check should be "show it to that
reader", not our guess at what they'd want.

## Q8 Route falsifier and review date

- **R-d is wrong if** putting a FUTON check inside the neighbour takes more than
  one day for the first one. Or if the result is something the neighbour's own
  validators or processors already catch. Then fall back to stand-alone
  demonstrations (R-c only).
- **"Build our own" (the default for Agency park and similar) is wrong if** T-M1's
  mechanism (claim-and-fence restart, typed resume) can be adopted by Agency
  for less than the cost of designing our own fix. Judge this at the next
  park-related bug.
- **Review:** 2026-10-18, or at the first stall.

## Q9 Problem

Joe's: he has come at problems SF startups are also working on, with heavier
theory. Today he cannot point to a run that shows the theory doing something
their code does not, and he has no settled view on using their code. When Q1
passes, he has one shown result per neighbour, and a reasoned use / copy /
leave list.

## On the algorithm (first run outside porting)

- **Carried over well:** Q4 (run the reference) found at once that three of the
  deep dives' demonstrations were models of the reference. Running the
  reference turned a fair-but-weak finding (Alinery D1) into a stronger one,
  within an hour. Q6's second reading ("what does the target already provide")
  is where the *use their code* options showed up (table above).
- **Did not carry over cleanly:**
  - "One record per problem": this case has one goal and two neighbours.
  - Q3 assumes a single authority. Here there are two kinds: the neighbour, for
    *what it does*, and Joe, for *what counts as good*.
  - Q1 could not be confirmed live because the requester was not in the loop
    during the run. The algorithm allows this (it is recorded as inferred), but
    it means the route choice is provisional.
- One observation, not yet a pattern (the rule is three sessions).
