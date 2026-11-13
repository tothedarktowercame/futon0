# Algorithm: Derive Completion Criteria for a META Item

Date: 2026-11-05
Status: active (trial)

## Purpose

Use this on one task file (a mission, excursion or ticket under some
`futon*/holes/`) that the War Machine excludes from its field with
`:completion-definition-not-found`, i.e. the file states nothing the machine
can check to tell whether the work is done.

The algorithm reads the item and ends in exactly one of three outcomes:

1. **finished**: the work is done. Propose closure, citing the evidence.
2. **retired**: the work was abandoned or superseded. Propose closure, naming
   the reason or the successor.
3. **open**: the work is not done. Write a `## Completion criteria` section
   in the form the machine reads, so the item can be admitted.

The algorithm proposes and never edits the item. Outcomes 1 and 2 go to the
author or Joe, who confirms closure. For outcome 3, the reviewer inserts the
section and commits it. Item files sit in trees that the War Machine's click
loop requires to be clean, so an uncommitted edit left by a seat stops the
machine.

Why it exists (Joe, 2026-11-04): 250 of 253 items excluded from the META
field were excluded for this reason, and some of them (for example
`E-R8-red-ring-fill`) are probably finished. Reading an item and deciding its
completion criteria is a straightforward step that the machine should not
have to skip.

## Prerequisites

- The exclusion list: `GET localhost:7070/api/alpha/wm/meta-latest`, entries
  whose `ineligible-reason` is `:completion-definition-not-found`.
- The parser the output must satisfy: `futon2/src/futon2/aif/mission_criteria.clj`
  (criteria heading and verdict forms) and
  `futon2/src/futon2/aif/task_completion_surface.clj` (how the class is
  decided). If the written section does not parse as `:criteria-locators`,
  outcome 3 has failed.
- Some items carry a provisional `**VERDICT (2026-10-09, provisional):**`
  line written by a Zai status classifier and never confirmed. Treat it as a
  claim to check, never as evidence.

## Inputs

- one item id and its file path
- the repositories it names (for commits, tests and files)

## Procedure

1. **Read the whole item.** Note what it set out to do (its goal, question
   or premise), what it says was delivered, and anything it names as a
   successor, parent or replacement.
2. **Gather evidence outside the file.** For each deliverable the item
   names, look for it: the file or module exists, the commit is on the
   default branch (`git log --oneline -S` or `--grep`), the test exists and
   names the behaviour, the successor item exists. Record each as a citation
   of absolute path plus line, or repo plus sha. A deliverable you could not
   find is recorded as not found. Do not drop it.
3. **Decide the outcome.**
   - *finished* only when every stated goal has a citation from step 2.
     "The excursion reached its findings" counts when the findings are
     written down in a file you can cite.
   - *retired* only when the item itself, or its named successor, says it
     was abandoned or replaced.
   - otherwise *open*.
   If you cannot decide, the outcome is *open*: an open item with honest
   criteria costs little, and a wrong closure hides work.
4. **Outcome 3, draft the criteria** in the proposal file, as the section
   to add to the item:

   ```markdown
   ## Completion criteria

   - <one checkable statement> — check: <path:line | repo sha | test ns/name | command and expected output> **Not met.**
   - <one checkable statement> — check: <...> **Met.**
   ```

   Rules:
   - The heading text is exactly `Completion criteria`. The parser also
     accepts `Acceptance`, `Success criteria`, `Exit criteria` and
     `Done when`, but use one name.
   - One bullet per criterion. Each ends with exactly one verdict:
     `**Met.**`, `**Not met.**` or `**Not started.**`. A bullet without a
     verdict has no locator and keeps the item excluded.
   - Every criterion names its check: something another reader can repeat.
     "The design is clear" is not a criterion.
   - Derive the criteria from the item's own stated goal. Do not invent new
     scope. If the goal is too vague to check, write the narrowest criterion
     the text supports and say so in the proposal.
   - Mark a criterion `**Met.**` only with a step-2 citation.
5. **Outcomes 1 and 2, write the proposal only.** Do not edit the item.
6. **Write the proposal file** (one per item; see Outputs).
7. **Self-check.** For outcome 3, re-read the section against the rules in
   step 4: there is one verdict per bullet, every bullet names a check, and
   the heading is exact. The reviewer confirms that it parses:
   `futon2.aif.task-completion-surface/describe` on the item with the section
   inserted must give `:criteria-locators`.

## Outputs

- `~/notes/completion-criteria/<item-id>.edn` with these keys:
  `{:item <id> :path <abs path> :source-sha256 <sha of the bytes read>`
  `:outcome :finished|:retired|:open|:needs-human`
  `:evidence [{:claim … :cite …} …] :not-found [ … ]`
  `:provisional-verdict <the file's VERDICT line, if any, verbatim>`
  `:agrees-with-provisional? true|false|nil`
  `:criteria-section <the markdown, outcome 3 only>`
  `:anchor <the heading or line after which the section goes>}`

## Review (by the requesting Claude owner)

- Sample at least one item of each outcome per batch, and every
  `:finished` or `:retired` whose `:agrees-with-provisional?` is false.
- Recheck two citations per sampled item. Any fabricated citation fails the
  whole batch from that seat.
- For accepted outcome-3 proposals, insert the section, run the parse check,
  and commit while no click is in its prepare step. Take accepted closures to the item's
  author, or to Joe, as one list.

## Stop conditions

- The item's bytes changed since the exclusion list was read: re-read it.
- The item is a requisition (`**Requisition:**` marker): it should not be in
  the list at all. Report it and skip it.
- More than a third of a batch is `:needs-human`: stop the batch and report.
  The algorithm, not the items, is probably wrong.
