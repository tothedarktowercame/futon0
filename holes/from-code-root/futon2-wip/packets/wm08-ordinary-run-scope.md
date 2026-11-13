# WM-08 link 4 (DISCOVERY ONLY): what an ordinary receipted-find run would cost

Repo: /home/joe/code/futon2. **Discovery only. Write no code, change no
namespace.** Output is one markdown file plus your report.

## The gap

`EV-find-expectations` ends: "Ordinary-run evidence checks interpreted receipts
against them." zai-6's capture established that this cannot happen today for
the frozen occurrence: the F13 retrieval was performed by the futon3a embedding
constructor and futon6 tier-0 scripts, **not** by `futon2.aif.find-receipt`,
and no `find/find` run exists anywhere in the repo. So we will shortly have
independent expectations with nothing ordinary to check them against.

zai-6 named three prerequisites. Your job is to turn them into a costed,
decidable plan:

  (a) the record re-expressed in the receipted-find record shape — sources as
      companion byte files, interpretations with guards;
  (b) a library snapshot pinned at the frozen index digest
      (`0cb12b24…`, frozen copy at
      `F13-model-manifest-2026-09-15/redo/retrieval-index.tsv`);
  (c) the expectations, which zai-45 is writing now.

## What to establish

1. **Shape gap.** Read `find-receipt/find`, `context`, `read-repository` and
   `find-receipt-test`'s fixture. State exactly what the receipted find
   requires of a record that the F13 record does not have — field by field,
   not in prose.
2. **Who may supply each missing piece without voiding independence.** The
   interpretations-with-guards are interpreter output. Say plainly which of the
   missing pieces are mechanical (a shape translation anyone may do) and which
   are judgements (and therefore constitute BEING the interpreter for this
   occurrence). For each judgement piece, name who may do it: not zai-45 (who
   is writing the expectations), and say whether you may, given you built both
   validators.
3. **Library snapshot.** Can a repository pinned at the frozen index digest be
   reconstructed from what is on disk today? zai-6 found the live futon3 index
   has drifted (`36a5ee5d…`), while the pattern flexiarg bytes still match the
   pinned digests. Say whether that is sufficient and what would make it not.
4. **Is "ordinary" satisfiable at all here?** The checklist wants an ordinary
   receipt-mode run, not a hermetic one. State what would make this run
   ordinary rather than a fixture, and whether re-expressing a 2026-09-15
   occurrence can ever be ordinary — or whether a NEW retrieval occurrence,
   run through the receipted path from the start, is the honest route. If it
   is, say what that costs instead.
5. **Cost each route** in the units we use: one small handoff ≈ 1 hour of
   agent time plus review.

Write it to
`holes/labs/wm-contract/runs/wm-08-external-f2-2026-09-16/ORDINARY-RUN-SCOPE.md`,
commit that one path, and bell claude-4 with the sha and your recommendation
between the two routes. A recommendation that says "neither is honest, here is
why" is an acceptable answer and a useful one.
