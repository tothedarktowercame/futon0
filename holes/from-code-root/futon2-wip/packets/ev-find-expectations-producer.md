# EV-find-expectations, link 3: write the F2 expectations, blind

Repo: /home/joe/code/futon2. **You write ONE artifact file. You change no code
and run no tests.**

## Your independence is the whole point of this dispatch

DAG node `EV-find-expectations` requires "an independent producer" and says
plainly: "Finder/interpreter output cannot provide its own expectations."
48 open items wait on the chain this node heads. If you read what the
interpreter concluded, the evidence is void — not weakened, void — and nobody
downstream can tell from the artifact alone. So:

**You MAY read:**
  - `holes/labs/wm-contract/runs/wm-08-external-f2-2026-09-16/FROZEN-CONTEXT.edn`
    — the frozen occurrence: target, digests, pinned-at, the task-context query,
    the library root, and the 10 candidate patterns that resolve to canonical
    repository ids (of 29 considered);
  - the pattern sources themselves under `/home/joe/code/futon3/library/`, at
    the digests the frozen context pins;
  - `holes/missions/M-zaif-harness-v1.md`, the target mission.

**You MUST NOT open** — and must say in your report that you did not:
  - `holes/labs/wm-contract/runs/F13-model-manifest-2026-09-15/redo/interpreted-pattern-set.edn`
    or its `.json` sibling (the interpretation record: it contains the
    interpreter's relevance judgments, clauses and conclusions);
  - `src/futon2/aif/find_expectations.clj`, `find_designation.clj` or their
    tests (another agent's validator — you should not tune your output to it);
  - any file under `F13-model-manifest-2026-09-15/` other than the frozen
    `retrieval-index.tsv` if you need the library membership.

This is a declared-role control, like the one in the artifact schema: it is not
enforced, it is stated and relied upon. If you open one of those by accident,
say so — a recorded slip is recoverable, an unrecorded one poisons the node.

## What to produce

For each of the 10 candidate patterns, decide **from the pattern's own source
and the task context** whether it applies to this tension, and for those that
do, write the expectation row the receipted find should emit:

    {:schema :wm/find-expectations-v1
     :author {:id "<your agent id>" :role :external-expectation-producer}
     :occurrence {...copied verbatim from FROZEN-CONTEXT.edn's :occurrence:
                  :target :target-source :repository-sha256 :pinned-at
                  :source-digests...}
     :expected {<canonical family/name keyword>
                {:clause-kind :if-clause
                 :acknowledged-clause {:text "<the pattern's IF clause,
                                              whitespace-normalised to one
                                              line>"
                                       :lines [<start> <end>]}
                 :route :structured-antecedent
                 :as-of {:target-sha256 ... :repository-sha256 ... :pinned-at ...}
                 :produced-from "FROZEN-CONTEXT.edn"
                 :reasoning "<why this pattern applies to THIS tension, in one
                             or two sentences, in your own words>"}
                ...}}

Write it to
`holes/labs/wm-contract/runs/wm-08-external-f2-2026-09-16/EXPECTATIONS.edn`.

Notes on the fields:
- `:acknowledged-clause` is the pattern's `+ IF:` block: its text
  whitespace-normalised to a single line, and `:lines` the 1-based line span of
  the block's CONTENT within the flexiarg file. Get the span by counting lines
  in the file; be exact, it is checked byte-for-byte.
- A pattern you judge does NOT apply simply gets no row. Do not write a row you
  do not believe.
- If you judge that a pattern applies but should only constrain content when it
  fires rather than being required to fire, add `:must-fire? false` to that row
  and say why in `:reasoning`. The default is that every row you write MUST
  fire; that is deliberate, so a finder retrieving less than you expected is
  caught.

## Report back

Bell claude-4 with: the commit sha (stage that ONE path); how many of the 10
you wrote rows for and how many you rejected, with a one-line reason each;
which files you read; and an explicit statement that you did not open the
interpretation record or the validator. If the frozen context was missing
something you needed, say what — that is a finding about the capture, not a
failure of yours.
