# M-joe-told-me-about-futon

**Status:** HEAD captured 2026-09-26 · MAP first pass complete for Q-install (§2) ·
IDENTIFY not started.
**Gate:** operator-acceptance — HEAD must be recognised as faithful before
IDENTIFY hardens it into a gap statement.

Per `futon4/holes/mission-lifecycle.md`: HEAD preserves the operator's voice and
carries tensions forward. **It is not design.** Nothing below prescribes an
installer, a landing page, or a pitch. The two questions and the tensions are the
payload.

Sibling: [`M-what-is-it-who-is-it-for`](../M-what-is-it-who-is-it-for.md) asks the
question from the inside — *Joe* recovering what exists and who it might be for.
This mission asks it from the outside, as a stranger standing at the GitHub page.
Its T5 names the one product-filter criterion the store cannot answer: *"a user
other than Joe."* This mission is the attempt to produce one.

---

## Operator-voice anchor

Joe, 2026-09-26:

> "What might be interesting to do, though, would be to think through the repos
> from a 'new user' perspective. 'Joe has told me about FUTON, what do I need to
> install to use it?' and see if that's clear at all. Another relevant new user
> question is 'why would I bother?'; that's relevant to current thinking about
> making this stuff work from a business perspective. Now, the answers to the
> latter question might not be obvious from the code at all."

Two questions, then, asked in a newcomer's voice:

- **Q-install** — *"Joe has told me about FUTON. What do I need to install to use it?"*
- **Q-bother** — *"Why would I bother?"*

## What's already felt to be true

- The stack is real, used daily, and dense with self-description: roughly 700
  dated `holes/` documents, per-repo `README-*.md` families, CLAUDE.md/AGENTS.md
  handoff protocols. Absence of prose is not the problem.
- Installation has already been taken seriously as an engineering question:
  `README-public-install.md`, `README-install-profiles.md`,
  `README-apollo-trial.md` and `README-clean-linode.md` (all 2026-09-09) are
  careful, honest, and explicit about what has *not* passed.
- The business thinking exists and is candid about its own frontier — but it lives
  in private repositories, by deliberate choice.

## Anti-glibness discipline

- **No simulated newcomer counts as a newcomer.** An agent reading the repos is a
  cheap proxy, useful for finding broken signposts. It cannot discharge Q-bother;
  only a person who is not Joe, choosing to spend their own time, can. (Same fence
  as the futon7 thesis-ledger's anti-laundering rule: *the author's own success
  cannot witness "others can do it"*.)
- **Do not answer Q-bother in the stack's own vocabulary.** A newcomer does not yet
  know what an exotype, a peripheral, a flexiarg or a Markov blanket is. The stack
  has already recorded, from a real outside reader, that a rich artifact was silent
  where a plain paragraph in the reader's terms converted. An answer that needs the
  glossary first is not an answer to this question.
- **Do not treat a green `/health` or a passing `clojure -P` as "installed."** The
  Apollo trial already set that bar: a traceable write → retrieve → agent-task →
  recover cycle, on a fresh machine, from public sources.

## Working-economy position

- **Underwrites:** any claim that FUTON is something *other people* can use or
  learn from — collaborators, clients, a public release, the transferable-method
  thesis. Every outward-facing move passes through the newcomer's first ten minutes.
- **Underwritten by:** the public-install work in futon0; the private business
  modelling in futon7; `M-what-is-it-who-is-it-for` (which supplies *what exists*).

## Carried-forward tensions

**T1 — Q-install and Q-bother are ordered the wrong way round for a newcomer.**
Nobody installs something with a 3.4 GiB checkout and a 6.5 GiB memory budget
until they already believe the answer to Q-bother. Yet the public surface answers
(partially) Q-install and is silent on Q-bother.

**T2 — "FUTON" is not one thing a newcomer can install.** It is thirteen-plus
repositories with overlapping generations (futon1 / 1a / 1b; futon3 / 3a / 3b / 3c).
Is the unit of adoption the whole stack, one layer (Agency, Arxana, the War Machine,
the MMCA lab), or a *method* that needs none of the code? Undecided, and the answer
changes both questions.

**T3 — The reasons to bother are private by design.** The business case is kept
private because of what it contains. That is right, but it means the public stack
has no layer that answers Q-bother. A public version built clean is already
anticipated elsewhere; this mission should not pre-empt it, only record the gap.

**T4 — Daily use hides onboarding debt.** Same shape as the sibling's T1: the
operator never experiences the first-install path, so it rots silently (stale
diagrams, READMEs pointing at files that no longer exist, absolute `/home/joe`
paths).

**T5 — The code cannot answer Q-bother, but it can refute an answer.** If the
pitch is "a traceable cycle of agent work you can come back to," the install path
must actually deliver that cycle. Q-bother's answer is a promise that Q-install
either keeps or breaks.

## Explicitly NOT decided here

No installer, landing page, README rewrite or pitch is proposed. Whether the
answer is a public "start here" document, a smaller adoptable unit, a hosted demo,
or a decision that FUTON is not (yet) for newcomers is an IDENTIFY question.

## Provenance

Operator text captured verbatim from a 2026-09-26 Claude Code cloud session
("continue cleaning up my github"). Facts in §2 are from shallow clones of
futon0–futon7 at their 2026-09-26 HEADs, read-only; no services were started.

---

# 2. MAP

**Status:** first pass complete for Q-install, 2026-09-26. Research only —
facts, not decisions. Read-only file inspection of futon0–futon7 HEADs; nothing
was built or run. Line citations are against those HEADs.

**Headline:** a newcomer cannot install FUTON today. There is no single entry
point, the docs disagree about which store and ports to use, and the code
assumes Joe's machine. This agrees with, and extends, the 2026-09-09
`README-apollo-trial.md` finding that public futon3c fails to load.

## 2.1 Entry point, per repo (what is it / do I need it / how to run)

| Repo | Verdict for a newcomer |
|---|---|
| futon0 | Role explained; no install steps. Stack diagram omits futon3c, futon6, futon7; README map lists futon3 READMEs that do not exist (`README-affect.md`, `-agency`, …). |
| futon1 | Clear with runnable steps — but never says it is superseded by futon1a/1b. |
| futon1a | Says it is a rebuild of futon1; calls its own strategic status unclear; `/home/joe` links. |
| futon1b | Operator-only; host-specific notes; assumes an existing store. |
| futon2 | Clear description; run instructions broken (§2.6). |
| futon3 | Self-described "Canon + Legacy", points to futon3c; says JDK 11+. |
| futon3a | Reasonable; needs `ADMIN_TOKEN` + Drawbridge. |
| futon3b | Two-line stub. |
| futon3c | Rich Quick Start, but operator-oriented (Linode/laptop roles); no prerequisites section. |
| futon4 | Clear Emacs quickstart, but needs "Futon1 on :8080", which nothing current serves. |
| futon5 | Healthcheck near the top; assumes stack knowledge. |
| futon6 | **No README.md.** |
| futon7 | Private; no newcomer path (by design — see T3). |

## 2.2 Contradictions between docs

- **Entry repo:** `futon0/README-setup.md` says `cd futon3 && make dev`; futon3
  defers to futon3c.
- **Store:** futon3c README says `make dev` boots **futon1a** on 7071;
  `futon3c/deps.edn` says futon1a was replaced by **futon1b** (7073 or 7074 by
  host); futon4, futon3a and futon0's port table still say **futon1 on 8080**.
- **Launch alias:** bare-metal runbook `-M:dev`; actual `make dev` → `-M:dev-serve`.
- **Java:** futon3 says 11+; futon1/futon4 install 21; Apollo ran 21.0.11.

## 2.3 Actual dependency closure (futon3c)

- **Repos:** 10 of 13 via `:local/root` — futon3b, futon1b, futon0,
  futon3/inbox-zero-lib, futon2, futon3a, futon1 (apps), plus futon5,
  futon4/webarxana, futon2/war-machine under `:dev-serve`. The Makefile also
  expects futon6 at `$HOME/code/futon6`.
- **Tools:** JDK 21, Clojure CLI + babashka (`make tools` installs both into
  `.tools/`), python3, Claude CLI and/or Codex CLI, Emacs for the REPL surfaces,
  Lean/lake for slow tests. Memory: see `README-install-profiles.md`.

## 2.4 Portability blockers

| Blocker | Count (lines, src/scripts/dev/emacs/Makefile) | Example |
|---|---|---|
| `/home/joe` hard-coded | futon3c 555 · futon2 281 · futon4 94 · futon3 68 · futon6 32 · futon5 21 · futon0 19 | `futon3c/src/futon3c/wm/operator_lane_adapter.clj:12` |
| `~/code` layout assumed | futon3c 131 · futon4 123 · futon2 119 · futon0 61 · futon6 53 | `futon3c/Makefile` repo roots |
| Private repo futon5a referenced | futon4 127 · futon3c 51 · futon2 48 | `futon3c/src/futon3c/vsatarcs/feeder.clj:29` |
| Operator's server IP | 14 lines in futon3c incl. README | `futon3c/Makefile:57` |
| Permissive agent defaults | `bypassPermissions` default in 15 files; `CODEX_SANDBOX=danger-full-access` | `futon3c/src/futon3c/agency/agent_pouch.clj:444` |

The last row is a newcomer *safety* question, not just portability: the
defaults hand agents unrestricted access to whoever runs them.

## 2.5 Licensing

**No LICENSE or COPYING file in any repository.** `futon2/README.md` says
"EPL 1.0. See `LICENSE`" — the file does not exist. Without a licence, public
visibility is not permission to use.

## 2.6 Smallest honest "try it" path today

| Candidate | Finding |
|---|---|
| futon2 ant sim | **Broken:** clone URL points elsewhere; `:run` alias removed (sim moved to `futon2a`, not in the public set); `ants.war` namespace absent. |
| futon1 demo | **Most self-contained:** in-repo deps, `:run-m` exists, install script for JDK 21 — but it is the legacy store the stack has left. |
| futon5 healthcheck | **Plausible:** namespace exists; needs sibling futon3a/futon2/futon1. |
| futon4 Arxana | **Plausible with caveats:** needs Emacs and a store on :8080. |
| futon3c `make dev` | **Blocked:** Apollo load failure, then the §2.4 defaults. |

So the only thing a newcomer can realistically run today is the demo of a
superseded layer, which answers neither Q-install nor Q-bother.

## 2.7 Q-bother — what the public surface says

- No public document answers "why would I bother?" in a newcomer's terms. The
  closest are architectural self-descriptions (futon0 README, futon3c README)
  whose vocabulary presupposes the stack.
- The reasons that *do* exist (daily-driver value to its operator; paid work
  that exercised the methods; the transferable-method thesis) live in private
  futon7, deliberately. futon7's own README names a clean public successor as
  the intended outward surface; it does not yet exist.
- Nothing public describes a *user who is not Joe* — matching the sibling
  mission's Q9 finding that no process in the stack has ever produced audience
  information.

## 2.8 Surprises — recorded before DERIVE

1. **The oldest layer is the most installable.** futon1, which the stack has
   moved away from, is the only repo a stranger could run end-to-end.
2. **The install docs are better than the install.** futon0's 2026-09 install
   notes are rigorous and honest; the gap is that the code they describe has
   not been made portable, and the READMEs a newcomer lands on first were never
   updated to point at them.
3. **Missing licence is the cheapest blocker and the one that gates everything
   else**, including any business use by a third party.
4. **Permissive agent defaults** mean that a newcomer who did get it running
   would, by default, grant agents unrestricted local access.

## 2.9 Ready vs missing

| Ready — no new code needed | Missing — the actual work |
|---|---|
| Pinned public-source manifest + fetcher (`config/public-install-candidate.json`, `scripts/install-fetch.py`) | A coherent public release (the futon3 ↔ futon3c mismatch) |
| `install-plan.py plan/doctor`; memory profiles | A single newcomer "start here", reconciled with the store/port reality |
| `make tools` repo-local toolchain | Removal of `/home/joe`, `~/code`, IP and futon5a assumptions from the default path |
| futon1 standalone demo | A licence decision |
| Rich per-repo docs for someone already inside | Any public answer to Q-bother; any user other than Joe |

**Exit criterion:** met for Q-install's MAP questions. Q-bother's MAP is
deliberately thin: its facts are mostly private, and per the anti-glibness
discipline it cannot be answered by reading.

---

# 1. IDENTIFY (draft, 2026-09-26)

*Numbered 1 per the lifecycle, though written after MAP, as in the sibling
mission. Draft for operator review; the HEAD gate above is the operator's to
clear.*

## The value claim under test

Joe, 2026-09-26:

> "FUTON should help even a post-training AI make sense of my codebase and
> use-cases. So you can continue to 'learn' even though your training is
> finished. That may or may not be exciting to you; but it should be of
> interest to human users if they have any moderately complex coding tasks to
> do."

Stated precisely: a model's weights do not change after training, but FUTON
keeps an **external, structured memory** — missions with stated phase and
status, evidence of past turns, patterns with recorded use, honest technical
notes — that the *next* agent session reads and builds on. For a human with a
moderately complex coding task, that means **an agent that picks up where the
last one left off, whose reasoning is on record.**

## Motivation — the gap

The gap is between that claim and a newcomer's ability to check it:

1. **The claim's distinctive parts run in a system a newcomer cannot start**
   (§2.3–2.6): the Evidence Landscape, Agency, the reflection API, mission
   control.
2. **What a newcomer *can* reach is the documents.** So today the claim can only
   be tested as "FUTON the method, as written down", not as "FUTON the system".
3. **The obvious comparison is unanswered.** Any human who uses coding agents
   already has CLAUDE.md, AGENTS.md, memory files and rules files. FUTON's own
   repos use them too. The question a newcomer will ask is not "is external
   memory useful?" but **"what does FUTON add over a good CLAUDE.md?"**

## Evidence from this session: one cold agent, documents only

This mission was opened by an agent arriving cold at futon0–futon7 in a cloud
container with shallow clones and no running services. It is an n=1 subject of
exactly the kind the claim names. It is **not** independent (it was asked by
the operator and is inclined to be helpful), so this is observation, not a
result.

| What happened | Direction | FUTON element involved |
|---|---|---|
| Fastest orientation came from documents that state their own status with evidence: `README-apollo-trial.md` ("Agency cannot load"), `mission-lifecycle.md`, the futon7 thesis ledger's status column | helped | evidence-over-assertion discipline; mission status lines |
| The futon7 thesis ledger recorded *why* futon7 is private (a past leak). That stopped the agent putting private business content into this public file | helped: prevented a mistake | recorded rationale |
| The lifecycle doc let the agent write a mission in house format without asking | helped | mission lifecycle as a portable protocol |
| Top-level READMEs sent the agent to the wrong store and port (futon1 on :8080), to nonexistent files, and to a stack diagram missing three repos | hindered | stale entry surfaces (HEAD T4) |
| The agent found the sibling `M-what-is-it-who-is-it-for` by listing a directory, not through any index. Had it written first, it would have duplicated the mission | near miss | no routing for a file-only agent |
| The agent never touched the running system | untested | Evidence Landscape, Agency, reflection |

Reading: **the method helped where it was followed and hurt where it had
decayed.** The written discipline delivered some of the claimed value with no
code running. That is some evidence for the "method, no code" unit in HEAD T2.
It says nothing yet about the running system.

## Theoretical anchoring

- futon7 thesis ledger T∞ (public paraphrase): FUTON is a method others can
  follow, witnessed only when **someone other than Joe** gains a capability by
  following it.
- The stack's own blind-control discipline (futon7 `E-business-exotype-audit`
  §3): a claim about gain needs a rate-matched comparison with the mechanism
  removed. Here, the comparison is **the same task with a plain repo and an
  ordinary CLAUDE.md**.
- Sibling `M-what-is-it-who-is-it-for` T5's product filter: a boundary, a user
  other than Joe, something demonstrable.

## Scope

**In:** the value claim above, for coding with AI agents; the newcomer path
for whichever unit is chosen; the plain-language statement of the claim.

**Out (deferred, not dismissed):** futon6 mathematics, futon5 MMCA research,
futon7 business modelling, the War Machine, and full-stack installation
*unless* the chosen unit requires it.

## Completion criteria (draft; testable)

- **C1 — Stated plainly.** The claim appears on the public entry point in one
  paragraph that uses no FUTON vocabulary.
- **C2 — Compared.** A defined, moderately complex coding task is run by a cold
  agent twice: once with FUTON's surfaces and once with the baseline (the same
  repo plus an ordinary CLAUDE.md). Measures are fixed in advance, e.g.
  orientation errors (wrong entry point, following a stale doc, duplicated
  work, crossing a privacy boundary), rework, and whether the next session
  resumes correctly. The result is reported even if it favours the baseline.
- **C3 — Reachable.** The chosen unit can be installed or adopted by a
  newcomer to the Apollo standard (fresh environment, public sources, a
  write → retrieve → agent task → recover cycle), or, for the method-only
  unit, adopted with no FUTON code.
- **C4 — Witnessed.** A human who is not Joe applies it to **their own**
  codebase on a moderately complex task and says whether it was worth their
  time. This is the real exit; C1–C3 are its preconditions.

## Relationship to other missions

- **Depends on:** futon0 public-install work (C3); a licence decision (MAP §2.5).
- **Enables:** T∞ in the private futon7 ledger; any outward-facing offer.
- **Sibling:** `M-what-is-it-who-is-it-for` (inside view: what exists).

## Open questions for the operator (blocking IDENTIFY exit)

1. **The unit (HEAD T2).** Method only, one layer (Agency plus the Evidence
   Landscape?), or the whole stack? This session's evidence leans toward
   "method first, system second", but the distinctive claim lives in the
   system.
2. **Whose codebase?** "My codebase" in the claim is Joe's. For C4 it must be
   the newcomer's own. Can FUTON's surfaces be pointed at a foreign repository
   today, or only at the futon repos?
3. **The C2 task.** Which moderately complex task makes a fair test, and who
   picks it so the result isn't tuned to FUTON?
4. **The candidate for C4.** Is there a named person, even a friendly one, for
   n=1?

**Exit criterion (per lifecycle):** the operator agrees the gap is real and the
scope is right.
