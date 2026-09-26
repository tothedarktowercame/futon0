# M-joe-told-me-about-futon

**Status:** HEAD and IDENTIFY accepted by the operator 2026-09-26 · **MAP in
progress** — first pass (§2, Q-install) complete; second pass (§2b, Q-bother)
recorded · Checkpoints 1–3 (install walkthrough, INSTALL.md) at end of file.

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
- **Correction (same day):** public business-model analysis *does* exist, in
  `futon0/analysis/business-models/` (e.g. `SPINE.md`: missions as a
  capability ledger, valuation left to the world). It is written for insiders
  and is not linked from any README, but it is public. The first pass missed it.
- **Correction (same day):** the common ground between futon and Rob's
  derivative, mfuton, is being worked out in public in
  `futon3c/holes/E-futon-mfuton-successor-requirements.md`: 31 shared
  requirements drawn from a futon/mfuton design dialogue, and a *configurator*
  sketch whose acceptance test is that it can describe both systems as
  configurations. The first pass missed this too.
- The reasons that *do* exist (daily-driver value to its operator; paid work
  that exercised the methods; the transferable-method thesis) live in private
  futon7, deliberately. futon7's own README names a clean public successor as
  the intended outward surface; it does not yet exist.
- ~~Nothing public describes a *user who is not Joe*~~ — **wrong**: mfuton and
  Rob are named across public futon0 and futon3c documents. What is true is
  narrower: no newcomer-facing page mentions that another person has adopted
  and adapted FUTON.

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
5. **The first MAP pass missed three public, directly relevant documents**
   (`analysis/business-models/`, `analysis/audits/PRODUCT-CENSUS-*`,
   `E-futon-mfuton-successor-requirements.md`). The cold agent found them only
   when a name from the operator (Rob, mfuton) gave it something to grep for.
   Same finding as the near-miss in §1: for a file-only agent, relevant work is
   reachable by search but not by navigation.

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
gives it **registers it can write, recall and act on**: missions with stated
phase and status, evidence of past turns, candid technical notes, and — the
one that changes behaviour, not just recall — the **pattern library**
(`futon3/library/`). An agent that writes a pattern, selects it later (PSR),
and records how it went (PUR) has changed its own functional behaviour on
evidence. Joe, 2026-09-26: *"We could call that memory or we could call it
learning, maybe it's both; what it is is a kind of slow-motion learning."*

For a human with a moderately complex coding task, that means **an agent that
picks up where the last one left off, whose reasoning is on record, and whose
working rules improve from recorded outcomes.**

This sharpens the CLAUDE.md comparison below. A CLAUDE.md is also a register
an agent can write to. FUTON's distinctive claim is not memory as such but
**evaluated memory**: each rule carries its selection and outcome records, so
revisions follow evidence rather than whoever last edited the file.

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

## Operator answers, 2026-09-26

**FUTON is both a codebase and a methodology; both are worth exploring and
testing on a newcomer basis** (Joe). The questions below were put to the
operator by the drafting agent; answers are paraphrased with key phrases quoted.

**Q1 — The unit: adoption comes in levels, and futon is a demonstration
instance.** Joe: *"we could say that futon is a demonstration instance."* The
plan is to find the common ground (for example, as a set of design patterns)
and then build *"a 'pushout' that's custom for any client."* In category terms:
given the shared core **C** with maps into the futon core and into a client's
own material, the client's system is the pushout that glues them along **C**.
mfuton is a second, independent point in that picture.

Proposed levels (for DERIVE to confirm or replace):

| Level | What is adopted | Needs FUTON code? |
|---|---|---|
| L0 — read | The method as documents: mission lifecycle, PSR/PUR, evidence-over-assertion | No |
| L1 — patterns | A pattern library of one's own, with selection and outcome records | No, or minimal |
| L2 — core | Shared-core services (evidence store, agent coordination) | Yes, the common core |
| L3 — pushout | Core + the client's custom material, glued along the shared core | Yes, per client |
| (futon itself) | The full demonstration instance | Yes, all of it |

The shared core **C** is not greenfield: the configurator sketch in
`futon3c/holes/E-futon-mfuton-successor-requirements.md` (seven parts:
carrier and ingress, typed schema, retrieval lanes, history, witness binding,
use and measurement, revision and governance) and its acceptance test
(describe *both* futon and mfuton as configurations) are a first candidate
for C.

**Q2 — Someone else's codebase: mfuton is the existence proof.** Joe:
*"my friend Rob got his agents to read futon and adapt it as 'mfuton' and made
what I think are many improvements so much so that mfuton is really Rob's thing
now."* Rob runs the core of futon alongside his own material, and *"the stuff
he does use he finds very much worth his time."*

Two readings, recorded separately so neither launders into the other:

- **For the value claim itself** ("helps a post-training AI make sense of a
  codebase"): Rob's *agents* read futon and produced a working adaptation.
  That is agents making sense of the codebase well enough to rebuild from it,
  by someone other than Joe. It is the strongest evidence for the claim so
  far, and it predates this mission.
- **For C4's grade:** Rob is a user other than Joe, but a *warm* one: a
  long-standing collaborator who already shares the vocabulary (missions,
  flexiargs). That witnesses "a close, ontology-compatible collaborator finds
  it worth their time". It does not yet witness "a stranger does". Both
  grades are worth having; they are different claims.

**Q3 — The fair C2 task: open, to be refined together.** Joe is *"a bit too
close to the material"* to pick it alone. Working proposal for DERIVE: **Rob
picks the task, from his own backlog, in a codebase that is his**, and judges
the outcome. That removes the operator's selection bias and matches C4's
"their own codebase". The baseline arm is the same task with the repo and an
ordinary CLAUDE.md.

**Q4 — The C4 witness: Rob, in person.** Joe will be working alongside Rob in
person for about a month. That is the occasion for C4 at warm grade and a
natural setting for C2, with the witness present to judge.

## Revised completion criteria

C1 and C3 stand as drafted. C2 and C4 are refined:

- **C2 — Compared, per level.** Test the *method* (L0–L1) and the *codebase*
  (L2–L3) separately, since either could carry the value without the other.
  Task chosen and outcome judged by Rob (Q3).
- **C4 — Witnessed, graded.** (a) **Warm:** Rob, in his own words, on what in
  futon or mfuton is worth his time and what is not, recorded during the
  in-person period. (b) **Cold:** a person with no prior contact with FUTON.
  (a) is in reach now; (b) stays open and may belong to a follow-on mission.

## Operator decisions, 2026-09-26 (second round)

- **Core value accepted:** *"memory that has been evaluated"* is the summary
  of what FUTON offers.
- **C2 at L0–L1 is judged validated by mfuton.** Joe: mfuton is *"frankly way
  ahead of futon in many regards"* in this connection, and he considers C2
  *"already validated at that level."* Recorded as an operator judgment grounded
  in an existing derivative, not as a measurement run by this mission.
- **The live question is L2 and L3.** Can someone adopt the shared core as
  code, and can a per-client pushout be built on it?
- **Candidate for the cold path: a single "CMYK-style projection".** Joe: *"If
  we wanted to combine all the futons into one best of CYMK-style projection,
  maybe that would be useful for 'cold' witnesses, i.e., rather than having to
  read a bunch of repos they could just read one that includes the core
  components of all."* Read as: each futon is a separation (one ink); the
  projection overprints the core of each into one printable image, one repo.
  Recorded here as a DERIVE candidate; note that it would be a concrete
  instance of the shared core **C** from Q1, so the configurator's acceptance
  test (describe both futon and mfuton) is a natural check on it.
- **C4(b), the cold witness, is a follow-on mission.** It cannot be settled in
  one session. This mission keeps C4(a), the warm witness.
- **This mission's next concrete work is the newcomer walkthrough**, done by
  the agent: follow the public docs literally on a clean machine and record
  what happens.

## Remaining before IDENTIFY exit

1. Operator acceptance of HEAD: remove the gate line above when satisfied
   (agents may not clear a gate).
2. The C2 task at L2–L3: settled with Rob rather than here.

**Exit criterion (per lifecycle):** the operator agrees the gap is real and the
scope is right.

---

### Checkpoint 1 — 2026-09-26: newcomer walkthrough, first attempt

**Setting:** fresh cloud container, Ubuntu 24.04, 16 GiB RAM, OpenJDK
21.0.10, python3, node, make; no Clojure tooling, no Emacs, no prior futon
state. **Network policy:** Maven Central and GitHub reachable; `repo.clojars.org`
and `download.clojure.org` **blocked by the container's egress policy** (a
property of this environment, not of FUTON). Followed
`README-apollo-trial.md` → `config/public-install-candidate.json`.

**What was done and what happened**

| Step | Result | Whose problem |
|---|---|---|
| `install-fetch.py` with the candidate manifest | Failed at once: `HTTP Error 403` from the unauthenticated GitHub API visibility check | Environment (API blocked here); but note the fetcher has no fallback |
| Same fetch, replicated by hand (init / fetch pinned commit / checkout) | 9 of 10 OK, futon4's `reazon` submodule OK, 3.4 GiB | — |
| futon1b pinned `d5a071e…` | **`not our ref`** — the commit is no longer on any public branch (current `master` is `ac3f83b`) | **FUTON: pinned manifest unreproducible** |
| Pinned futon0 `7a568e8` | Predates `scripts/install-plan.py`, which the install docs tell you to run | **FUTON: manifest pins a futon0 without its own installer** |
| futon3c `make tools` | Failed: `bootstrap-tools.sh` asks the GitHub API for "latest" and JSON-parses a 403 page (`JSONDecodeError`) | Environment trigger; FUTON has no pinned-version default |
| `bootstrap-tools.sh` with pinned versions | babashka 1.13.219 OK; Clojure CLI blocked (download.clojure.org) | Environment |
| Clojure CLI 1.12.5.1664 from the GitHub mirror, installed by hand | OK | — |
| `clojure -P` for futon1b `:server`, futon3c `:dev-serve`, futon1 `:run-m` | **All three stop at the first Clojars artifact** (e.g. `cheshire 5.11.0`) | Environment |
| `install-plan.py plan` / `doctor` (current futon0) | **Works as documented:** plan exit 0; doctor exit 2, `launch_ready false`, memory/ports/paths OK, flags missing `clojure`/`bb`/`clj-kondo` on PATH and lists the four remaining release gates | **FUTON: this part is good** |
| Apollo release blocker (`promote-exec/execute-plan-with-refresh!`) | **Fixed in public HEADs:** present in `futon3/inbox-zero-lib/.../promote_exec.clj:131`. But the candidate manifest still pins the old futon3 `bfa8a9c`, so a newcomer following the docs would reproduce the failure | **FUTON: manifest stale** |

**Findings for the mission**

1. **The candidate manifest has aged out in 17 days.** One pinned commit is gone
   from the public remote, one pins a futon0 without the installer, and one
   still pins the pre-fix futon3. A pinned manifest is only reproducible if the
   pinned commits are kept reachable (tags, not branch tips) and the manifest
   is re-cut after fixes.
2. **The planning tools are the best-behaved part of the newcomer path.**
   `doctor` gave accurate, honest output on a machine it had never seen.
3. **Toolchain bootstrap depends on network calls that can fail opaquely**
   (GitHub API "latest", `download.clojure.org`). Pinned versions, and a clear
   error instead of a Python traceback, would make failure legible.
4. **Not reached, so untested:** dependency resolution, cold load of futon3c
   at current HEADs, boot, write/retrieve/search, agent task, restart. These
   need Clojars access.

**Test state:** no FUTON tests run (dependencies unresolvable here).

**Next:** with `repo.clojars.org` allowed, re-run from `clojure -P` at current
public HEADs: resolve futon1b `:server` and futon3c `:dev-serve`, cold-load
`futon3c.dev`, then attempt the Apollo acceptance cycle.

### Checkpoint 2 — 2026-09-26: walkthrough with full network access

**Setting:** as Checkpoint 1, with network access widened. All ten public
repos moved to their **current HEADs** (the pinned manifest is stale, per
Checkpoint 1). Clojure CLI 1.12.5.1664, local Maven repo, `MALLOC_ARENA_MAX=2`.
Ports from the Apollo plan: store 7273, Agency 7270, Drawbridge 6968.
Repos linked into `~/code/`, as the docs assume.

**Result: the storage and coordination half of the Apollo acceptance cycle
passes from public sources on a fresh machine.** This is the first time that
has been recorded; on 2026-09-09 Agency could not load.

| Apollo acceptance step | Result |
|---|---|
| Resolve deps, futon1b `:server` and futon3c `:dev-serve` | **Pass** (163 MiB cache, both exit 0) |
| Cold load of `futon3c.dev` from public sources | **Pass** — the 09-09 missing-function blocker is fixed at HEAD |
| Boot futon1b on a fresh store | **Pass** — `/health` ok, text index built on the empty store |
| Boot Agency against futon1b | **Pass** — `I-evidence-per-turn boot check: OK (futon1b)`, `/health` status ok, `claude-1` and `codex-1` registered |
| Write → read → reply-chain → text search | **Pass** — direct to futon1b, and written via Agency's `/api/alpha/evidence`, read back through both |
| Stop both JVMs, restart, recover | **Pass** — all three entries and the search index survived; Agency re-read them |
| One agent task via `/api/alpha/invoke` | **Not tested — environment.** The only agent CLI here is the host session's own Claude Code, running as root; it refuses to start in `bypassPermissions` as root, and driving it would borrow the host session's identity rather than a newcomer's own login |

**Findings (FUTON-side), in the order a newcomer meets them**

1. **Without the right environment variables the stack still boots, and says
   loudly that it's wrong.** First launch without `FUTON3C_EVIDENCE_BACKEND`:
   *"I-evidence-per-turn BOOT CHECK FAILED … writes will not persist. Fix and
   restart."* That is exactly the right behaviour for a newcomer, and the
   variable to set is named in the message.
2. **No newcomer doc lists the environment that works.** The working launch
   needed `FUTON3C_EVIDENCE_BACKEND=futon1b`, `FUTON1B_URL`,
   `FUTON1B_PENHOLDER=api`, `FUTON3C_PORT`, `FUTON3C_DRAWBRIDGE_PORT` and
   `FUTON3C_ROLE=laptop`. The futon3c README's env table documents none of the
   `FUTON1B_*` variables and still describes futon1a on 7071.
3. **The penholder allowlist defaults to `joe` and `api`.** A write as any other
   penholder is refused with `layer 3 forbidden, allowed [joe api]`: clear, but
   the name of the operator is a default.
4. **The futon3c README's evidence-write example is rejected.** It omits
   `subject`, which `EvidenceEntry` now requires (`social/shapes.clj:324`). The
   error is well-formed and shows the rejected entry, but a newcomer's first
   copy-paste fails.
5. **The README's `CLAUDE_PERMISSION_MODE` is dead.** The code reads
   `CLAUDE_PERMISSION` (`dev/futon3c/dev/agents.clj:131`). The documented
   override for the permissive default does nothing.
6. **`FUTON_CODE_ROOT` is honoured in 3 places; the watcher is not one of
   them.** It watches `/home/joe/code/*` (14 roots, including private
   futon5a/futon7/futon7a) regardless, and logs a stream of
   `ConnectException` and `cannot change to '/home/joe/code/…'`. Boot
   continues.
7. **Boot emits one hard-coded-path failure** (`structural-law-inventory.sexp`
   under `/home/joe/code/futon3c/docs`), recorded as evidence and survived.
8. **The first boot creates `~/code/storage`** before the user has put
   anything in `~/code`.

**Environment-side, not FUTON (recorded so they are not mistaken for FUTON
defects):** Maven resolution through this container's proxy needed a
`~/.m2/settings.xml` proxy entry; the agent CLI is the host session's own.

**Test state:** no test suites run; acceptance checks above are end-to-end.

**Reading for IDENTIFY.** L2 (the shared core as code) is closer than MAP
suggested: a stranger can boot the store and Agency and get durable, searchable,
restart-safe evidence today, *if* told six environment variables. What stands
between that and a newcomer doc is small and specific (findings 2–6), not
architectural. The untested step, an agent task, is where FUTON's claimed value
("the next session picks up where the last one left off") actually lives, so it
should be the first thing run on a real newcomer machine, as a normal user with
their own agent CLI.

### Checkpoint 3 — 2026-09-26: single entry point shipped

**What was done:**
- `futon0/INSTALL.md` written from the Checkpoint 2 path, then re-verified
  with exactly its documented commands: futon1b on `:7074` (Agency's default),
  `make dev` with three settings (`FUTON3C_EVIDENCE_BACKEND=futon1b`,
  `FUTON3C_ROLE=laptop`, `CLAUDE_PERMISSION=default`), first write/read via
  Agency. The store also survived an unplanned container restart.
- Found while verifying: futon3c defaults to futon1b on **7074** while
  futon1b's README starts it on **7073**; `FUTON1B_PENHOLDER` already defaults
  to `api`; `make dev` sets `CLAUDE_BIN=~/.local/bin/claude` and
  Codex `danger-full-access`/`approval=never`. All stated in INSTALL.md.
- Every other futon README (futon1, 1a, 1b, 2, 3, 3a, 3b, 3c, 4, 5) now opens
  with a pointer to INSTALL.md and one line on that repo's role in the core
  install. futon6 had no README; a short one was added.

**Addresses:** MAP §2.1–2.2 (no single start, contradictory docs) and HEAD T4
for the install question. C1 (a plain statement of the value on the public
entry point) is partly met by INSTALL.md's "What you get".

**Still open:** the agent-task step (INSTALL.md §6, marked unverified);
the README-level fixes in futon3c (env table, evidence example, permission
variable name) — INSTALL.md works around them rather than fixing them.

---

# 2b. MAP — second pass: Q-bother (2026-09-26)

**Operator, on entering MAP:** *"now we have a reasonable sense that our
imagined interlocutors could understand how to install futon, but I still am
not sure they see the value in doing so."*

Research only: facts, not design. Question under survey: **after following
INSTALL.md, what would a newcomer see that shows the value claimed in
IDENTIFY** — memory that has been evaluated, and an agent that picks up where
the last session left off?

## 2b.1 Survey questions, answered

**QB1 — What does a newcomer have after INSTALL.md?** An empty evidence store,
Agency's HTTP API, and one entry they wrote by hand with `curl`. Nothing in the
install shows memory being *used*. (Checkpoints 2–3.)

**QB2 — Is the claimed mechanism wired for the agent a newcomer would use?**
For Claude Code, **no**. Verified on the invoke path:

- `make-claude-invoke-fn` (`futon3c/dev/futon3c/dev.clj:3608-3613`) runs
  `claude -p … [--resume sid] -- <prompt>`: no system-prompt addition, no
  retrieved evidence, missions, patterns or PSR/PUR records. The warm-pouch
  path (`src/futon3c/agency/agent_pouch.clj:439-446`) is the same.
- Evidence flows **outward only** on this path: `emit-invoke-evidence!`
  records the call; nothing reads back.
- Continuity for a Claude seat is therefore the Claude CLI's own `--resume`
  plus **instructions** in CLAUDE.md (select patterns, write PSR/PUR, keep
  mission docs). No CLAUDE.md tells an agent to query the evidence store at
  session start.

Where memory **is** pushed at session start, it is on other paths:

- The **zai/kimi API harness**: `boot-packet-string` and `rehydration-string`
  (`src/futon3c/peripheral/memory_backend.clj:673-721`, last 10 turns),
  injected into the system message by `src/futon3c/agents/zai_api.clj:2105-2117`,
  gated by `:memory-mode`; plus agent-callable tools `memory_search`,
  `pattern_memory`, `psr_search`, `evidence_graph`, `mission_context`
  (`zai_api.clj:170, 314-337`).
- **Recall-before-dispatch** for math-problem work:
  `src/futon3c/dispatch_with_recall.clj` assembles a pattern-conditioned
  memory packet and posts it with the task (`:push`, line 22; packet at
  1308; bell at 1465), run from scripts such as `scripts/codex_sorry_cron.py`.

**QB3 — How "evaluated" is the memory in practice?**

- The library is large: **1,411 patterns in 117 families** (`futon3/library`).
- Retrieval is heavily exercised: `analysis/audits/PATTERN-STAGES.md` (window
  2026-08-22 → 09-21) counts **20,306 retrieval records**, with a rank-one
  pattern joined to **84.7% of 3,398 recorded operator turns**.
- Explicit evaluation is thin: of **323 mission files**, **18 have a PSR
  section and 7 a PUR section**; there are 48 legacy PSR/PUR files. Recorded
  selections name **24 distinct patterns**, 17 of which resolve to library
  files.
- Reading: the stack *retrieves* patterns constantly; it *records how a
  pattern turned out* rarely. "Evaluated memory" is the right description of
  the design, but the evaluation half is sparse in the record.

**QB4 — Is there any demonstration of the cycle (session 1 records, session 2
benefits)?** **None found** as a tutorial, fixture or demo. The nearest are
experiments: `futon3c/holes/labs/M-memory-retrieval/` (e.g.
`E9-pull-probe-prereg.md`, whether an agent pulls known memories unprompted),
`test/futon3c/agents/zai_memory_tool_contract_test.clj`,
`test/futon3c/dispatch_with_recall_test.clj`, and futon3b's library-level
`full-loop-round-trip` (`futon3b/AGENTS.md:300-307`: gap → new pattern →
accepted), which is library evolution, not an agent recalling earlier work.

**QB5 — What public surfaces explain the value?**

- `hyperreal.enterprises` (company site): **does not mention FUTON.**
- `zone.hyperreal.enterprises`: a decision log (*"decisions, with the evidence
  attached"*) — rigorous, but written for someone already inside the work.
- INSTALL.md's "What you get" paragraph (Checkpoint 3) is now the only plain
  statement, and it asserts the value rather than showing it.

**QB6 — What independent evidence of value exists?** mfuton: another person's
agents read futon and built a working adaptation, which that person now uses
and judges worth the time (IDENTIFY, operator answers Q2). It is the strongest
evidence available, and it is **not visible to a newcomer**: no public page
says it happened or what was carried over.

## 2b.2 Ready vs missing (for Q-bother)

| Ready — exists today | Missing — the actual work |
|---|---|
| A working install path (INSTALL.md, verified) | Anything the install *shows*: the store starts empty and nothing reads it |
| Pushed memory at session start — for the zai harness, and recall-before-dispatch for math work | The same for the agents a newcomer is likely to bring (Claude Code, Codex) |
| A heavily used pattern-retrieval layer (20k records/month) | Outcome records at a rate that makes "evaluated" visible (7 of 323 missions have a PUR) |
| Experiments and tests of recall (M-memory-retrieval, recall tests) | A demonstration: session 1 records → session 2 is measurably better, runnable by a stranger |
| An independent adopter (mfuton) | A public account of that adoption, in the adopter's words |
| A decision log with evidence attached | A public page that says what FUTON is for, outside the log |

## 2b.3 Surprises — recorded before DERIVE

1. **The value claim is most true on the path a newcomer is least likely to
   use.** Memory is pushed at session start for the API-harness seats and for
   dispatched math work; the Claude Code path that INSTALL.md points to gets
   none of it.
2. **"Picking up where you left off" is, for Claude seats, mostly Claude's own
   `--resume` plus written instructions.** That is close to the CLAUDE.md
   baseline IDENTIFY asked FUTON to beat — so on that path, a C2 comparison
   would currently find little difference, by construction.
3. **Retrieval is abundant; evaluation is scarce.** The distinctive half of
   "evaluated memory" is the half least present in the record.
4. **The best evidence is private by default.** mfuton's existence is in
   public futon3c notes, but nowhere a newcomer would read it.

**Exit criterion for this pass:** QB1–QB6 answered with citations; ready vs
missing complete. **No design follows here.** Candidate directions for DERIVE
(not decisions): a runnable two-session demonstration on a public fixture;
extending session-start memory to the Claude path; a public account of mfuton
in Rob's words, if he agrees.

## 2b.4 Operator observations (2026-09-26, after §2b)

- **Rob had a clear use case**, which is part of why mfuton happened: the value
  was visible because he brought his own problem to it.
- **Other potential users recognise the pain points** FUTON (or a successor
  such as zabuton) addresses, in conversation: *"customer interviews" minus,
  so far, the willingness to pay.* The value currently travels **in the form
  of a call, not a quickstart.** (Details of those conversations belong in
  futon7, not here.)
- **Comparison, Claude Code:** on startup the user sees *"a picture of a
  hermit crab without a shell and a box they can type into"*: the benefit is
  not obvious either, *"though at least the intended user interaction is
  clear."*

**Reading (agent, for DERIVE to test):** Claude Code shows its value in the
first minutes because the unit of value is **one turn** on the user's own
problem. FUTON's claimed value arrives **across sessions**, so a quickstart
cannot show it in one sitting unless it ships a *yesterday*: prior sessions,
records and patterns already in the store, so the newcomer's first session is
the system's second. And the call works where the page does not because the
call starts from **the listener's pain point**, then shows the mechanism; the
public surfaces start from the mechanism.
