# WM-08 Route A: re-express the F13 occurrence in the receipted-find record shape

Repo: /home/joe/code/futon2 (shared checkout, master). You captured the frozen
context for this occurrence, so you know it. Now translate the record so the
receipted find can actually run on it.

**This is a REHEARSAL. It is not ordinary-run evidence and must never be
described as such** — a replay is hermetic by construction
(ORDINARY-RUN-SCOPE.md §4). Its value is that it exercises the whole apparatus
end to end and gives zai-45's expectations a consumer today.

## Read first, and follow it rather than this packet where they differ

`holes/labs/wm-contract/runs/wm-08-external-f2-2026-09-16/ORDINARY-RUN-SCOPE.md`
(zai-16, sections 1-7). §1 is a field-by-field table of the shape gap; §6 and
§7 are the adjudication rule, which binds you.

`test/futon2/aif/interpretation_evidence_test.clj`'s `zaif-fixture` is already
a working translation of this exact record — read it before writing anything.
Your job is largely to lift that into a first-class, digest-verified artifact
rather than to invent a translation.

## What to produce

1. **The translated record** at
   `holes/labs/wm-contract/runs/wm-08-external-f2-2026-09-16/REEXPRESSED-RECORD.edn`,
   in the shape `find-receipt/find` and `context` consume, plus the companion
   captured bytes it needs.

   **The translator's rule is verbatim-copy-or-refuse.** The
   interpretations-with-guards are codex-27's recorded output: copy them; do
   not improve them. Any place where the record's text disagrees with the
   captured bytes is a REFUSAL you report, never an edit. In particular q0
   stays `mission-document-assertion` even though the record's own finding
   flags it — upgrading it would be interpreting, and you are not the
   interpreter.

2. **The rehearsal run**: `find-receipt/find` over the translated record and
   captured bytes, then
   - `find-receipt/validate-result!` with the independently compiled context;
   - `find-expectations/validate-external!` against zai-45's
     `EXPECTATIONS.edn` and the occurrence from `FROZEN-CONTEXT.edn`;
   - `find-designation/resolve-designation` with **no** artifact, which must
     report `:vacuous` with `:vacuous-because :no-designation-supplied`.

3. **The report** at `…/ROUTE-A-REHEARSAL.md`: what the run selected, what the
   validators said, and — the point of the exercise — **what the two known
   disagreements did**. zai-45 expects `coordination/session-durability-check`
   to fire (the interpreter judged it not relevant) and wrote no row for
   `social/tension-before-code` (the interpreter judged it relevant). Per §6.2
   the decider is guard truth on the recorded frozen facts, so report, for each
   of those two, whether its recorded guard fires on the recorded facts, and
   which refusal shape (if any) followed.

**You may not edit `EXPECTATIONS.edn`.** It is immutable evidence for this run
(§7). If a row looks wrong to you, that is a finding for the report, and zai-45
answers it in a v2 artifact for a future occurrence — not now, not by you.

## Gates

Whatever namespaces you touch must pass, plus `find-receipt-test`,
`find-expectations-test` and `find-designation-test`. clj-kondo and
check-parens on any code. Prefer putting the rehearsal in a test namespace or a
script under `scripts/wm08/` over editing production namespaces; if you think a
production namespace must change, report it instead.

Bell claude-4 with the sha, the gate output, the rehearsal result, and the
guard-truth verdict on the two disputed patterns. zai-16 reviews after me.
