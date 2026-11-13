# mfuton Algorithms

This directory holds constrained-English operational programs for repeatable
procedures that have been factored out of live evolver/operator interaction.

Subdirectory note:
- `operator/` holds algorithms whose primary execution surface is the human
  operator acting manually rather than an agent flattening the whole run into a
  single automated step
- `operator/` is the default discoverability lane for operator algorithms, but
  a stronger local owner or explicit placement decision may keep a specific
  operator-run procedure in a mission neighborhood instead
- `frontier-math/` holds the current local FrontierMath human-supervision
  prompt procedures that sit on top of the room/runtime loop rather than
  replacing it
- `mcp/` holds the current local MCP installation/integration procedures for
  bringing mission-owned MCP servers into this installation and keeping their
  payload/mount/inventory discipline explicit

For the EAL lane, this directory also now functions as a working example
corpus:
- these algorithms are not yet the canonical EAL spec
- but they are live constrained-English programs that exercise the same core
  idea in production-facing form
- they should therefore be read as implementation-adjacent exemplars for
  `holes/missions/M-english-algorithm-transcoding-pilot.md`
- they are also candidate examples of how an English artifact can describe a
  realizable arrow directly, and eventually how a larger arrow may be
  decomposed into smaller provable/implementable steps

Use an algorithm here when:
- the procedure is stable enough to replay
- a full script would be premature or too rigid
- the system benefits from isolating classical automatable steps from novel
  reasoning


Algorithm invocation rule:
- when the operator says `run <algorithm-name>` or otherwise explicitly names a specific algorithm to run, treat that as a method invocation of the named algorithm
- read and run that named algorithm exactly; do not replace it with a looser remembered approximation, a nearby habit, or a generic shortcut
- if the named algorithm needs packet values such as repo targets and the operator supplied them, bind them directly; if the packet is omitted and still needed, the algorithm itself owns the clarification or binding step rather than the evolver bypassing the algorithm

Strict execution rule:
- algorithms here are not suggestions; they are executable installation logic written in constrained English
- treat algorithm contents as code-like authority that must be adhered to during execution
- do not silently substitute a remembered habit, local convenience, or partial approximation for the written algorithm
- if an evolver is unable to perform an algorithm as written in the current environment, that is a failure state
- in that failure state, the evolver must not work around the algorithm
- instead, the evolver must stop, report the issue to the operator, and treat the inability to execute the algorithm as a real system problem to be repaired

Cross-reference policy:
- promotion rule:
  `library/social/extract-classical-automation-from-novel-evolver-work.flexiarg`
- post-mortem capture gate:
  `library/social/post-mortem-for-novel-learning-capture.flexiarg`
- EAL lane umbrella:
  `holes/missions/M-english-algorithm-transcoding-pilot.md`
- EAL pilot corpus:
  `docs/eal-pilot-corpus.md`
- two-step topology rule for artifact creation/merge waves:
  - first add the immediate connected-neighborhood links and discoverability
    edges needed so the new or changed artifact is honestly visible in context
  - then run
    `algorithms/conduct-knowledge-orthogonalization-pass.md`
  - the orthogonality pass is not optional after the immediate link-enrichment
    step; if it concludes that no broader cleanup is needed, record that
    trivial result explicitly rather than silently skipping it
- standalone orthogonality remains valid too:
  - the orthogonality pass may also be run by itself against a drifted
    neighborhood even when no immediate creation/merge wave triggered it
  - use that standalone mode when overlap, shadow ownership, or stale topology
    has accumulated over time and a bounded cleanup pass is now worthwhile

Companion-flexiarg note:
- each algorithm should carry its own `Companion Flexiarg Resolution`
  section
- when several sibling algorithms share one family-level companion posture, the
  strongest family owner should explain that shared note once and the
  siblings/dispatchers should keep only their explicit local delta

## Algorithms

- `create-document.md`
  - decide whether requested durable document writeback should create a new
    docs owner, update an existing docs owner instead, or route out of docs
- `update-document.md`
  - decide whether existing-document pressure should honestly update the
    current docs owner, route back into new-document creation, or stop at a
    stronger non-doc owner
- `author-document-core-and-route-writeback.md`
  - own the shared inner document-authoring/writeback core after the
    create-versus-update gate, including bounded docs deltas and
    discoverability wiring
- `create-mfuton-issue-before-triage.md`
  - capture an `mfuton` local execution issue from conversation or another
    source surface, write it up under `world/issues/`, and stop cleanly at the
    pre-triage boundary with external issue postings treated as projections or
    linked sources
- `create-futon-issue-before-triage.md`
  - capture a futon-origin issue into a local proxy under
    `world/futon-issues/<futon>/...`, keep the upstream futon issue canonical,
    and stop cleanly at the futon-origin triage boundary
- `triage-futon-issue-and-decide-local-work-handoff.md`
  - triage a futon-origin proxy and choose whether it stays proxy-only,
    becomes an `mfuton` local issue, or goes directly to a futon-scoped
    mission
- `process-futon-issue.md`
  - process a futon-origin issue through the local execution lane by creating
    a repair mission in `holes/missions/issues/`, creating the linked native
    local GitLab issue, triaging before implementation, publishing to PR-open,
    and then closing the local mission and GitLab issue
- `create-algorithm.md`
  - decide whether repeated live work should honestly create a new algorithm
    owner, route to an existing-owner update instead, or stop at
    flexiarg/doc/mission level
- `update-algorithm.md`
  - decide whether existing algorithm pressure should honestly update the
    current owner, route back into new-algorithm creation, or stop at
    flexiarg/doc/mission level
- `author-algorithm-core-and-map-companion-flexiargs.md`
  - own the shared inner algorithm-authoring/refactoring core after the
    create-versus-update gate, including companion-flexiarg mapping and
    discoverability wiring
- `de-ai-ify.md`
  - run a bounded rewrite pass over an existing prose artifact to reduce
    high-latent low-grounding LLM/managerial smoothness by preferring named
    instances, explicit mechanism, and cleaner omission over prestige wording
- `formalize-type-from-validator-source-through-example-roundtrip-and-derived-surface-parity.md`
  - search for shape authorities, bind the strongest validator/source surface,
    widen the runtime model through growing real-example roundtrip coverage,
    align JSON/Lean/CT plus class-shape regeneration against the chosen
    bounded source of truth, and close the current logical type set before
    starting the next one
- `create-flexiarg-from-recurring-judgment-surface.md`
  - decide whether a recurring judgment surface should create a new flexiarg
    owner, update an existing flexiarg owner instead, or route back to a
    procedural owner
- `update-flexiarg-from-recurring-judgment-surface.md`
  - decide whether existing-flexiarg pressure should honestly update the
    current owner, route back into new-flexiarg creation, or route back to a
    procedural owner
- `author-flexiarg-core-and-wire-discoverability.md`
  - own the shared inner flexiarg-authoring/refactoring core after the
    create-versus-update gate, including body authoring, carrier-boundary
    preservation, and discoverability wiring
- `operator/add-wsl-node-to-mfuton-manually.md`
  - manually add a governed WSL node to `mfuton`, from Windows discovery and
    `sshd` verification through dedicated node-control account/bootstrap and
    return to the `agent_skills/wsl-node/` surface
- `operator/create-operator-algorithm.md`
  - create a new operator algorithm under `algorithms/operator/` only when
    the lane is truly operator-run, replayable, and self-contained rather
    than still depending on hidden chat context or unresolved packet values
- `operator/install-first-matrix-stack-behind-unified-wsl-caddy-front-door-manually.md`
  - manual operator algorithm for the privileged WSL packet that installs the
    first Matrix stack behind one WSL-hosted `Caddy` front door while leaving
    non-privileged preparation and verification to the agent lane
- `create-new-agent-skill-with-typed-surface-grouping-python-wrapper-and-tests.md`
  - create skills under `agent_skills/` with typed-surface grouping discipline,
    Python-first implementation plus `.bat` wrapper invocation, and test-guided
    verification during build
- `render-operator-algorithm-in-chat.md`
  - reread a source operator algorithm and render it back into chat as literal
    numbered steps with prose preserved as prose, commands preserved as fenced
    code blocks, and continue/stop conditions kept explicit
- Artifact writeback and topology family:
  - `rename-artifact.md`
    - narrow front door for same-identity durable artifact renames, treated
      with the same gravity as moves and delegated to `merge-artifact.md` in
      `large` mode
  - `merge-artifact.md`
    - reconcile bounded durable session/subflow results back into installation
      artifacts, with `small` versus `large` writeback/restructuring control
  - `move-artifact.md`
    - narrow front door for relocating or reparenting an existing artifact to
      a stronger owner, executed through `merge-artifact` in `large` mode
  - `conduct-knowledge-orthogonalization-pass.md`
    - required bounded topology-cleanup/factoring pass paired with merge waves
      and especially with relocation waves
  - `decide-follow-on-orthogonality-and-devmap-wiring-after-mission-reconstruction.md`
    - narrower follow-on topology decision for mission reconstruction and
      devmap-wiring aftermath
- `install-python-capability-package-in-codex-env-and-record-requirement.md`
  - add a needed Python package capability through the governed `codex` env
    using conda-first, pip fallback inside `codex`, immediate smoke
    verification, and same-wave requirements-manifest recording
- `query-irc.md`
  - run the bounded IRC query procedure through the live IRC typed surface by
    taking one stateless room snapshot and optionally deriving a presence
    answer such as `is joe online?`
- `frontier-math/README.md`
  - family owner for the current FrontierMath human-supervision prompt
    choreography above the local room loop
- `mcp/README.md`
  - family owner for the current local MCP installation/integration algorithm
    neighborhood
- `mcp/install-mcp.md`
  - install a mission-owned MCP lane into the local system by choosing the
    honest payload/mount/companion shape, proving the mounted surface, and
    updating tracked inventory/snapshot surfaces
- `frontier-math/ask-frontiermath-room-for-briefing-and-current-state.md`
  - re-enter the FrontierMath room honestly by checking the live roster and
    asking for one bounded current-state briefing before pushing new work
- `frontier-math/seed-or-redirect-frontiermath-room-work.md`
  - introduce or redirect one bounded FrontierMath room task without drifting
    into a whole plan dump
- `frontier-math/supervise-long-running-frontiermath-room-task.md`
  - classify a long-running FrontierMath room task as healthy, complete,
    narrowable, or intervention-worthy without mistaking duration for failure
- `frontier-math/run-frontiermath-bell-and-next-work-advance.md`
  - drive and verify the Bell → repost → next-work transition on the
    FrontierMath room lane
- `frontier-math/close-frontiermath-room-cycle-with-summary-and-handoff.md`
  - close one bounded room cycle into a short summary, git/artifact read, and
    explicit next handoff
- `research-issue-online.md`
  - research a bounded technical issue online by searching the relevant public
    web surfaces, then directly reading GitHub issues, Stack Exchange posts,
    docs, and related hits to determine which ones actually match the current
    evidence before choosing the next local check
- `create-mission.md`
  - create a new mission by discussing the lane with the operator first,
    classifying forward creation versus honest retroactive `RMR`, and then
    wiring the mission cleanly into the local topology
- `complete-publication-continuation-after-pr-merge.md`
  - when a previously published PR is later merged, complete the stored
    continuation by re-anchoring the local repo to `main` or `master`; for
    futon repos this normally composes the futon pull/reconcile algorithm from
    that posture
- `check-approved-pr-and-dispatch-merge-continuation.md`
  - when a published PR is observed as approved, classify whether it is still
    just approved/open or has actually merged, and only then dispatch the
    merged-PR continuation
- `close-verified-mission-and-delegate-deferred-work.md`
  - close a mission only after its own acceptance criteria are evidenced and
    operator-verified, while delegating non-blocking remainder explicitly to
    parent/child/follow-on owners
- `how-to-evaluate-constraints.md`
  - evaluate whether a constraint is actually satisfied, distinguish
    implementation gaps from constraint problems, recognize when an already
    discovered layer is still open across required surfaces, and only offer a
    constraint change after proving it is not aligned with the mission’s
    goals/scope
- `conduct-brainstorming-discussion-on-idea-or-artifact.md`
  - run an explicit exploratory operator/evolver discussion on an idea,
    artifact, naming question, or boundary question so the concept can be
    sharpened before deciding whether to promote it
- `conduct-discussion-on-idea-before-writeup-or-promotion.md`
  - discuss an idea explicitly before jumping straight to writeup or durable
    promotion, with brainstorming as the usual default mode for that
    discussion
- `conduct-commit-phase-with-diff-and-total-dirty-closure.md`
  - enter a committing phase by first diffing the real dirty set, excluding
    obvious side-effect noise, grouping the remaining files only when the split
    is semantically strong, and looping until all intended dirty files are
    committed before the phase ends unless the operator explicitly approves a
    named remainder
- `conduct-post-mortem-learning-capture-discussion.md`
  - when the novelty gate says a run deserves post-mortem treatment, conduct
    the retrospective as an explicit operator/evolver discussion and only then
    finalize promotions or leave an honest draft/continuity state
- `process-codex-learning-experience.md`
  - after a concrete shared learning experience, discuss the warranted
    self/edge/collaboration branches explicitly, create any missing bounded
    world-model carriers needed to encode those branches honestly, keep the
    `codex` self-update fail-closed, and then run a bounded
    knowledge-integration pass so the revised notes point back to the
    artifacts that caused the learning
- `merge-session-results-into-mission-and-idea-artifacts.md`
  - reconcile the durable results of a live session or bounded subflow back
    into mission, idea, and docs-owned operator-facing artifacts, keeping
    trace retention distinct from installation merge and asking before
    adjacent improvements expand the wave; defer broader semantic-neighbor
    boundary discovery to the orthogonality algorithm when needed
- `retrieve-from-session.md`
  - recover prior operator prompts from governed Codex session/rollout
    artifacts when chat memory is in doubt, using the session-tools lane and
    explicit session-binding/fallback rules rather than remembered guesses
- `download-papers.md`
  - clarify the requested paper set and placement, author a supported paper
  reference list, download into a local corpus with manifests, and then
  extract the corpus into sibling plain-text artifacts
  - BibTeX is the default durable reference-list format; TSV remains a
    supported compatibility and migration input
- `untrack-local-only-artifacts-under-new-ignore-policy.md`
  - remove previously tracked artifacts from the current tracked tree while
    preserving local copies on disk when newer ignore policy says they should
    be local-only going forward
- `decide-follow-on-orthogonality-and-devmap-wiring-after-mission-reconstruction.md`
  - after a mission is newly created or reconstructed post hoc, decide whether
    to wire it into a devmap now, leave it floating intentionally, or defer the
    wiring into a bounded cleanup continuation
- `process-world-backed-issue-through-triage-and-scheduled-repair.md`
  - start from a written-up world-backed issue, perform triage, and create a
    repair mission under `holes/missions/issues/` only when substantive repair
    is actually scheduled, using a call with continuation boundary when it is not
- `publish-verified-repair-and-close-world-backed-issue.md`
  - after operator verification, reuse the PR publication algorithm when code
    publication is required, and only then claim honest issue/repair closure
- `publish-branch-pr-with-human-approved-evidence-comment.md`
  - publish branch, open PR, and post a human-approved reviewer evidence
  comment, using a call with continuation when publication is blocked and
  body-file posting for long stored comments
- `complete-futon-issue-mission-through-pr-open-and-local-proxy-finalization.md`
  - for futon-origin issue missions, compose canonical mission closure,
    publication, and local proxy PR-open finalization
    (`issue.md`/`links.edn`/`events.ndjson`), then checkpoint mission/proxy
    updates in `gh`
- `pull-futons-and-reconcile-active-branches.md`
  - run `pull-futons.bat`, then safely reconcile active local
    branches with updated upstream heads; if an already-published branch gains
    a reconcile merge, push it so the review surface stays current, and stop on
    ambiguity/conflict
- `conduct-problem-intake-with-route-questions.md`
  - before work starts, separate the requester's goal from any method handed
    over, ask whether the authority that defines correct can simply be run, and
    record routes with tests and falsifiers; drill mode trains the same
    questions on closed cases from day-one packets (spec, 2026-10-11)
