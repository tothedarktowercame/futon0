# Operator Algorithms

This subdirectory holds algorithms whose primary execution surface is the human
operator acting manually, rather than an evolver flattening the whole procedure
into one automated shell/program step.

Intent:
- in `mfuton`, the operator should be treated as a real system actor/entity,
  not as something outside the system
- that means a procedure executed manually by the operator can still be an
  algorithm in the same ontological sense as an evolver-executed procedure
- the difference is the executor surface, not the artifact class

Runbook note:
- some of these artifacts will feel runbook-like in ergonomics because they may
  contain concrete commands, expected outputs, and failure classifications
- keep the word `runbook` discoverable because operators may search for it
- but do not demote these artifacts to a weaker class just because the steps
  are executed manually
- preferred local read:
  - `operator runbook` describes the usage feel
  - `operator algorithm` describes the artifact ontology

Boundary:
- keep replayable manual procedures here when the operator is the intended
  executor
- treat this directory as the default discoverability lane for operator
  algorithms, not as a mandatory override of:
  - a stronger local owner
  - an explicit operator-bound placement decision
- use the parent `algorithms/` directory for general evolver/operator procedures
  that are not specifically operator-run runbooks
- do not duplicate mission-local contract prose here when a narrower mission
  doc or contract note already owns the invariant surface
- for service bring-up lanes, prefer this split:
  - mission owns service posture, acceptance, and boundary
  - operator algorithm owns the replayable manual procedure

Relevant foundational answering discipline:
- `library/foundational/respond-to-questions-with-sources.flexiarg`
  - when an operator or evolver challenges what an algorithm step means or asks what a
    current operator artifact actually says, reread the governing source before
    answering rather than defending a remembered explanation
- `library/foundational/respect-others-while-collaborating.flexiarg`
  - when an operator or evolver challenges a step or asks a clarifying question, answer
    promptly, directly, and honestly so the collaboration can converge on a
    consistent joint world model instead of optimizing for conflict-avoidance

## How To Write An Operator Algorithm

Operator algorithms should be written as real operator actions, not as a
mixture of vague stage titles and hidden implied work.

Preferred shape:

Gate before calling something an operator algorithm:
- all execution details needed by the operator must be present in the
  algorithm itself
- do not leave the operator dependent on remembered chat context, hidden
  packet values, or unresolved placeholders
- if the current run has already bound concrete values, the algorithm should
  show those concrete values rather than preserving open variables such as:
  - `<wsl_private_ip>`
  - `if the evolver gave you a URL`
  - `replace with the packet version`
- if the details are not yet fully written into the operator-facing artifact,
  the artifact is not yet ready to be used as an operator algorithm

1. Start with one global failure rule.
   - example:
     if any command fails unexpectedly, stop and hand the exact output back to
     the evolver
   - this is better than repeating a generic "contact me on failure" note
     under every step

2. Write each numbered step as an operator action or decision point.
   - good:
     `Open a privileged WSL shell and verify that sudo works there.`
   - bad:
     `Prepare privileged execution context.`

3. If a step contains commands, say explicitly what the operator should do
   with the result.
   - good:
     `Continue only if sudo -v succeeds.`
   - good:
     `If the result is caddy-installed, continue. If it is caddy-missing,
     run the install commands.`
   - bad:
     listing commands without saying what counts as success, failure, or the
     next branch

4. Keep branch points explicit and bounded.
   - acceptable:
     `If X, continue. If Y, run these exact commands.`
   - acceptable:
     `If the service status is failed, stop and send the output back.`
   - avoid open-ended repair language such as:
     `fix as needed`, `troubleshoot`, or `investigate further`

5. Keep one step to one operator obligation.
   - acceptable step types:
     - wait until the agent has supplied the exact packet
     - run a gate command and continue only if it succeeds
     - record current facts before mutation
     - install or configure one concrete service/component
     - run one bounded verification command group and hand off
   - avoid steps that only summarize what several inner commands are "about"
     without telling the operator what to do

6. Separate privileged packet work from post-packet validation.
   - operator algorithm should usually stop after the bounded privileged packet
   - the evolver then resumes for non-privileged validation, receipts, and
     follow-on interpretation unless the mission explicitly says otherwise

   Text-editor assumption:
   - assume the operator already has a text editor and does not need generic
     instructions for opening or using one
   - the algorithm should specify:
     - which file to edit
     - what exact content or lines must be present
   - the algorithm should not try to teach editor basics unless a narrower
     mission explicitly makes editor usage part of the task

7. Keep contract ownership out of the operator algorithm when possible.
   - mission owns service posture, acceptance, and boundary
   - contract note owns stable interface or deployment semantics when one
     exists
   - operator algorithm owns the replayable manual sequence

8. Use short interpretation lines only when they directly govern the operator's
   next move.
   - good:
     `Continue only if both systemctl commands succeed.`
   - good:
     `If those commands report active/running services, stop here and hand the
     output back to the agent.`
   - bad:
     free-floating descriptive prose that only explains what a cluster of
     commands is "for" without changing the operator's behavior

9. If an operator or evolver questions whether a step is correct, reread the written
   algorithm before answering from memory.
   - good:
     re-open the current algorithm, inspect the exact step, and answer from
     the artifact
   - bad:
     defend what the step was "supposed to mean" from remembered context while
     the actual written algorithm may still be wrong

11. When an operator or evolver asks a question about the algorithm, answer promptly,
    directly, and honestly.
   - good:
     say plainly what the algorithm currently says, what is wrong with it if
     needed, and what should be fixed
   - bad:
     optimize for avoiding friction by answering evasively, indirectly, or as
     though agreement already exists when the world models are still diverging

10. Do not claim closure without recording the closed state in the algorithm.
   - good:
     once the exact package URL, config values, file contents, and command
     targets are known, write them into the operator algorithm
   - bad:
     treating the packet as "settled" while the operator-facing instructions
     still contain placeholders or refer back to prior chat for missing values

In practice, the strongest operator algorithms usually look like:
- one global failure rule
- one packet-ready/wait step
- one or more exact command steps with explicit continue/stop conditions
- one bounded handoff step back to the agent

Current local exemplar:
- `install-first-matrix-stack-behind-unified-wsl-caddy-front-door-manually.md`
  - useful because it keeps the operator packet narrow, makes each step an
    explicit action, and tells the operator exactly when to continue, stop, or
    hand control back

Current contents:
- `create-operator-algorithm.md`
  - create a new operator algorithm under `algorithms/operator/` only when the
    lane is honestly operator-run, self-contained, and no longer depends on
    hidden packet values or remembered chat context
- `add-wsl-node-to-mfuton-manually.md`
  - manual operator runbook for adding a governed WSL node to `mfuton` and
    returning to the `agent_skills/wsl-node/` control surface
- `install-package-managed-localhost-gitlab-on-wsl-node-manually.md`
  - manual operator algorithm for first package-managed, localhost-only GitLab
    bring-up on the governed WSL node; the GitLab child mission remains the
    owner of the service contract and acceptance target
- `install-first-matrix-stack-behind-unified-wsl-caddy-front-door-manually.md`
  - manual operator algorithm for the privileged WSL packet that installs the
    first Matrix stack behind one WSL-hosted `Caddy` front door and then hands
    validation back to the agent lane
