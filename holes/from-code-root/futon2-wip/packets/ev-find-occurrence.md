# EV-find-expectations, part 2 (DISCOVERY): which real retrieval occurrence can be frozen

Repo: /home/joe/code/futon2 (shared checkout, master). This is a **discovery
and capture** dispatch. You write ONE new artifact file and a short report. You
change no code. If you find yourself editing a `.clj`, stop and report instead.

## What the DAG asks for

Node `EV-find-expectations` (p4ng
`wm-walkthroughs/build-loop/closure/closure-dag.json`, rev 75):

> Independent F2 expectations for one real retrieval occurrence, with frozen
> source/query/task context and an independent producer. Ordinary-run evidence
> checks interpreted receipts against them. Finder/interpreter output cannot
> provide its own expectations.

48 open items wait on the chain this node heads, so what matters is getting the
occurrence RIGHT, not quickly.

## Your job

**Establish which real retrieval occurrence we can freeze, and freeze its
context.** Specifically:

1. **Find the candidates.** Read `src/futon2/aif/find_receipt.clj` (especially
   `read-repository`, `context`, `validate-result!`) and
   `test/futon2/aif/find_receipt_test.clj`. Then find every real retrieval
   occurrence that exists in this repo — a record with schema
   `:wm/interpreted-pattern-set-v1`, or a recorded run of the receipted find.
   One I already know of:
   `holes/labs/wm-contract/runs/F13-model-manifest-2026-09-15/redo/interpreted-pattern-set.edn`
   (target `M-zaif-harness-v1`, 7 interpretations). Its JSON sibling shows 0
   sources and 0 retrieval runs — **check whether the EDN carries them**; an
   occurrence with no sources cannot be bound to source digests.
2. **Report what each candidate has and lacks**, against what the frozen
   context must carry: target id, target source path + sha256, the source
   digests, the repository digest, `pinned-at`, and the query/task context (what
   was being retrieved FOR).
3. **Capture the frozen context of the best candidate** as a new file

       holes/labs/wm-contract/runs/wm-08-external-f2-2026-09-16/FROZEN-CONTEXT.edn

   with schema `:wm/find-occurrence-context-v1`, carrying exactly:

       {:schema :wm/find-occurrence-context-v1
        :captured-by "<your agent id>" :captured-at "<ISO instant>"
        :occurrence {:record {:path "..." :sha256 "..."}
                     :target {:id ... :source {:path ... :sha256 ...}}
                     :repository-sha256 "..."
                     :pinned-at "..."
                     :source-digests {<source id> "<sha256>" ...}}
        :task-context "<what this retrieval was for, in the record's own words
                       or its target's — quote, do not summarise>"
        :library-root "..."
        :candidate-patterns [<the pattern ids the retrieval CONSIDERED, if the
                             record carries candidate judgments>]}

   Compute the digests yourself from the bytes on disk (sha256 of file
   contents) and say in the report how you computed each one.

## The one hard rule

**Do not put the finder's or interpreter's conclusions into this file.** No
`:interpretations`, no selected patterns, no relevance judgments, no clause
text, no expectations. A third agent will write the expectations from this file
alone, and if your file leaks what the interpreter decided, that agent is no
longer independent and the node cannot be accepted. Candidate pattern ids are
allowed (what was considered); what the interpreter concluded about them is not.

If you think a field you are asked for cannot be filled without leaking
conclusions, leave it out and say so.

## Report back

Bell claude-4 with: the candidate occurrences you found and what each has or
lacks; which you chose and why; the commit sha for the artifact (stage that ONE
path); and an explicit statement of what the artifact does NOT contain. Also
say whether an ORDINARY run — the receipted find on the serving path, not a
test fixture — is possible for this occurrence today, and what it would need.
No review cycle.
