# Liability register — 2026-07-21, post M-capability-zones arc

Commissioned by Joe after the arc closed: "we can be sure that it will break down
again" — per the ledger's own arithmetic and the namesake's design (the war machine
works by breakdown; the register's job is to know where the load sits, not to promise
it won't shift). Surveyed live from the repair queue, the cohort ledgers, git state,
and this session's findings. Ordered by how soon each bites.

## 1. Bites the very next click (stop-line preemption)

- **`repair-attempt-044-artifact-binding-mismatch` — :open, machine-failure.**
  The next click CANNOT do ordinary work until this discharges. Note the irony
  guard: S2b landed the shape machinery, but the v1 kind→shape mapping sends
  everything to `:code-commit`, so 044's discharge is still commit-shaped. If its
  repair turns out data/spec-shaped, we re-enter the 039 dance (manual
  implementation records). *Catch:* first click of any future run. *Lane:* the
  follow-up mission's kind→shape mapping, or a one-off owner discharge.
- **`repair-attempt-047-review-execution-evidence-missing` — :open, machine-failure.**
  Same preemption. The R9 corroboration machinery exists (033/034 landed it), so
  this should discharge through a normal repair click — but it must actually run.

## 2. Chronic failure families, still live by the ledger

- **`build-failed` (9 in cohort-40 + 043/044/047-adjacent today).** The single
  biggest kind and the one with no systemic fix — it is "the work is hard" plus
  possibly something specific: **M-learning-loop ate 3+ consecutive build failures
  from two different authors today.** That is starting to look like a property of
  that mission's build surface, not author luck. *Catch:* if the next 2 clicks that
  select M-learning-loop also build-fail, treat the mission's gates as the suspect
  and survey it directly. *Lane:* worth its own excursion note.
- **`agent-unavailable` (5 environmental holds parked: 001/025/027/028/045).**
  S3 fixed the `restored`-not-woken slice; still unhandled: **busy-wedged agents**
  (zai-4 stuck `invoking` after heavy authoring — S3 correctly won't wake busy
  agents, so a wedged-busy agent stalls clicks until manually cleared) and **pool
  evaporation** (the zai pool went from 6 to 1 during one day of reaping; author
  diversity died with it). *Catch:* S3's `:failure-detail` now distinguishes these.
  *Lane:* un-wedge protocol (nightwatch memory) needs a mechanical form; pool
  management is operator terrain.
- **`review-execution-evidence-missing` (2 + today's 047).** Recurred AFTER the
  corroboration fix landed — meaning either a new sub-mode or the fix's coverage
  is partial. Nobody has diagnosed 047 yet. *Catch:* 047's discharge will force it.

## 3. Infrastructure fragilities observed this session

- **:7073 store instability post-OOM.** One transient misread (158 vs 242, exact
  flag set both times!) under load; the OOM-zombie history (2026-07-13) plus
  yesterday's CPU-saturation timeout. The store answers healthily *most* of the
  time, which is the dangerous kind of reliable. A count-read that lies once can
  fail a deposit guard or a review check silently. *Catch:* any load-bearing count
  should be read twice (the deposit tool should adopt this). *Lane:* evidence-infra.
- **Agency 30-min job cap.** Killed S1 mid-embed; will kill any future long job
  whose author doesn't checkpoint. S1's fix (resumable chunks) is local to that
  script, not a platform behavior. *Lane:* known, standing.
- **OOM itself.** Two JVMs >130% CPU + big heaps + BGE embedding + Emacs = the
  host has no headroom policy. Today's crash cost a session and orphaned attempts.
  *Lane:* operator/host.
- **Roster churn vs running work.** Reaping agents mid-click orphaned attempt-038
  and spawned the unreviewed-hack incident. There is no "drain before reap"
  protocol. *Catch:* none currently — this one is purely procedural. *Lane:*
  Agency ops; cheap to write down as a rule.

## 4. Interim design debt (each has a named heir)

- **Interim metric** — plain BGE cosine / PCA-3 Euclidean, `:held` on
  M-substrate-metric (Wasserstein/Fisher–Rao heir).
- **pca3-v1 partition at 16% explained variance; 79% high-D disagreement; no-op
  zone holding 51/269 missions; p10 resistance = always-10%-mixed by construction.**
  All recorded as accepted distortion at the walk — acceptable *because* versioned;
  liability if v2 never comes. Heir: follow-up mission (UMAP-3 candidate,
  meta-pattern layer probe verdict already in hand).
- **Zone load enters C from posterior mass only** (S2a) — the charter's fuller
  load (Figure-5 enacted magnitude + attestation) and demand (live traffic) are
  not wired. The current load signal double-counts the same evidence that feeds
  variance. Heir: follow-up mission load/demand channels.
- **Retro-harvest classification is text-similarity** — the hypergraph/term-index
  route (witness-bearing, incremental) is designed but unbuilt. Heir: XTDB#5637 demo.
- **Trickle updating not wired** — checkpoints are position-keyed (appends
  invalidate), daily cron not extended, seeds drift with no re-acceptance cadence
  scheduled. The partition ages from the moment it was accepted. Heir:
  steady-state note, follow-up mission.
- **Survey-heavy value ordering** — earned, but 58% of commit evidence sits in two
  zones; early learn-selections will lean survey-ish until live outcomes reshape
  the posteriors. Not a defect; a bias to watch in the first weeks of running.

## 5. Record and publication debt

- **futon2 branch `M-propagators-ant-gate` is 41 commits ahead of origin, and the
  entire arc lives on it, not on main.** One `git push` from safety; zero merges
  from main-visibility. The single largest do-it-today item. *Lane:* Joe's call
  (branch strategy is his).
- **4 unclosed attempt stubs** (038, 040, cohort-41's 001 and 042) — honest
  interruption artifacts, but they sit in ledgers as `(unclosed)` forever unless
  the cohort views learn to label operator-interrupted attempts. Minor.
- **Cross-cohort attempt-id ambiguity residue:** cohort-41's `attempt-001` dir
  duplicates cohort-40's id in any attempt-id-keyed lookup (first-match wins).
  The global-numbering fix prevents NEW collisions; the one existing pair remains
  a lookup trap. Minor but real.
- **Empirics snapshot stale** — predates 040–047; the §Empirics counted claims and
  the retired-kinds list are now wrong (policy-nondiscrimination should move to
  "retired by repair" once refreshed). Refresh per README-empirics + Joe re-checks
  hand prose. Also the capability-zones pattern footnote in `main-2026.tex` still
  says "chartered, not yet built" — now false; it should cite the discharge
  (attempts 046/036/041, resolutions on disk).
- **Cohort-41 budget: 3 attempts remain**, then the next preregistration (the
  validator now accepts any pos-int target — write the doc deliberately, not at
  2am).

## The standing wager

The tripwire architecture converted every breakdown above from a silent drift
into a typed record — that is why this register could be written from queries
rather than memory. The wager to keep making: every liability here should either
carry a tripwire that will announce it, or a named heir that will absorb it.
The ones with neither (roster churn, host headroom) are the ones that will
surprise us. They are marked. — claude-3
