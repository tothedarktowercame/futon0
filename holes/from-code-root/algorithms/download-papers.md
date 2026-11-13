# Algorithm: Download Papers

Date: 2026-03-11
Status: active

## Authority routing

- grouped research skill neighborhood:
  `agent_skills/research/README.md`
- acquisition functionality owner:
  `agent_skills/research/download-papers/README.md`
- text-extraction functionality owner:
  `agent_skills/text-extract-corpus/README.md`
- governing research-mode mission:
  `holes/missions/M-mfuton-familiar-typed-surface-world-and-memory/M-my-familiar-research-modality/M-research-mode-papers-downloader.md`
- adjacent archive-scale research sibling:
  `holes/missions/M-mfuton-familiar-typed-surface-world-and-memory/M-my-familiar-research-modality/M-arxiv-math-archive-acquisition.md`
- retrieval / extracted-text supporting lane:
  `holes/missions/M-futon-full-system-operation-and-onboarding/M-governed-rag-mcp-retrieval-surface.md`
- algorithm-family lifecycle gate:
  `library/social/create-versus-update-algorithm.flexiarg`

## Purpose

Use this when the operator wants a paper set pulled into the local installation
as a searchable corpus.

This algorithm owns the normal three-stage flow:

1. author the paper reference list
2. download the papers
3. extract the downloaded papers into plain text

It is intentionally broader than the `download-papers` agent_skill alone, because
the honest operator goal is usually the local corpus, not merely the raw
downloads.

## Companion Flexiarg Resolution (2026-03-14)

Governing companion flexiargs for this algorithm:
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
  - governs reuse of existing paper lanes, governed classical code functionality,
    and corpus
    placement before inventing a parallel acquisition path
- `library/social/json-artifacts-default-ignore-with-operator-exceptions.flexiarg`
  - governs the local-only default posture for large machine-readable corpus
    byproducts and extracted support artifacts

Inspected but non-owning candidates:
- `library/social/create-versus-update-algorithm.flexiarg`
  - governs algorithm-family lifecycle questions, not the direct corpus
    acquisition/extraction procedure

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

## Inputs

- what paper set is wanted:
  - topic
  - author
  - paper list
  - reading lane
- desired owning repo / corpus placement, if already known
- any known source list or publication page
- whether the operator wants:
  - only raw downloads
  - or the normal full corpus flow with extracted text

## Preconditions

1. The requested paper set is bounded enough to author a real URL list.
2. The placement is known, or the operator is available to clarify it.
3. The network/download lane is available before claiming the papers were
   acquired.

## Procedure

1. Clarify the target paper set and destination.
   - Ask what should be downloaded if it is not already clear.
   - Ask where it should live if the destination is not already clear.
   - If the corpus is specific to a sibling project repo that owns the work,
     prefer that repo as the durable local home:
     - `redistrict/papers/<topic-or-lane>/`
     - or the analogous `papers/` lane in another sibling repo
   - Use `mfuton/papers/<topic-or-lane>/` when the corpus is
     installation-general rather than owned by a narrower sibling project.
   - Do not guess a durable installation location when the operator intent is
     still ambiguous.
2. Choose the local corpus layout.
   - For installation-general corpora under `mfuton/papers`, prefer this
     shape:
     - paper reference list:
       `mfuton/papers/<topic-or-lane>/<name>-links.bib`
     - raw downloads:
       `mfuton/papers/<topic-or-lane>/downloads/`
     - extracted text:
       `mfuton/papers/<topic-or-lane>-text/`
   - For project-local corpora in an owning sibling repo, prefer keeping the
     files with the project materials:
     - paper/reference folder under:
       `<sibling-repo>/papers/<topic-or-lane>/`
     - raw downloads either in that same folder or a local `downloads/`
       child when separation helps
     - extracted text next to the papers as sibling `.pdf.txt` files when
       local adjacency is the more useful working posture
   - Reuse an existing lane if it already exists and the new work truly belongs
     there.
3. Confirm the generated paper corpus is not going to be staged by accident.
   - Check the current ignore policy for the repo that owns the corpus:
     - `mfuton/papers/` for installation-general lanes
     - or the sibling repo's `papers/` lane for project-local corpora
   - If the corpus is meant to stay local-only, make sure the ignore posture is
     correct before a large acquisition run.
   - Do not silently untrack already-committed paper artifacts unless the
     operator explicitly asks for that separate step.
4. Author the paper reference list first.
   - This is the first required durable artifact.
   - Preferred format is BibTeX `@misc` entries with at least:
     - `title`
     - `url`
     - optional `urlformat`
   - Treat BibTeX as the default durable paper-list format for this lane.
   - Keep legacy TSV as a supported compatibility and migration input when a
     BibTeX list does not yet exist or an older lane has not been migrated yet.
   - Prefer direct/open paper URLs when possible:
     - PDF first
     - stable HTML copy next
     - DOI/landing page only when no better open source is yet available
   - If some links are weaker than others, record that honestly in comments or
     nearby notes rather than pretending every entry is equally good.
   - If the lane already has a TSV list and you want to migrate it instead of
     recreating it, use:
     `mfuton/agent_skills/research/download-papers/tsv_to_bibtex.py`
5. Improve obviously weak links before download when practical.
   - If a URL is clearly a DOI/redirect/publisher landing page, try to replace
     it with a stronger open or direct source first.
   - Stop this refinement step when it becomes a time sink; do not block the
     whole corpus forever on two stubborn papers.
6. Download from the reference list into the raw corpus directory.
   - Use:
     `mfuton/agent_skills/research/download-papers/download_papers.py`
   - Prefer the BibTeX reference list when available.
   - The downloader also accepts legacy TSV and line-based inputs during the
     transition period.
   - If you want the migration to be implicit for a legacy TSV lane, use the
     downloader's normalized-BibTeX write option rather than treating TSV as
     the new durable output.
   - Keep the manifest.
   - Treat the governed classical code functionality as the authoritative
     downloader rather than ad hoc
     browser saves.
7. Read the download manifest honestly.
   - The downloader should classify the fetched payload itself, not only the
     transport result.
   - Classify:
     - successful direct paper downloads
     - failures
     - landing-page / redirect downloads that are not yet the paper itself
     - access walls or other non-paper payloads
   - If a weak DOI/landing-page entry only produced a redirect/HTML shell,
     record that explicitly and decide whether to chase a better source now or
     later.
8. Extract the downloaded files into plain text.
   - Use:
     `mfuton/agent_skills/text-extract-corpus/extract_docs_to_text.py`
     or the PowerShell entrypoint
   - For installation-general corpora, write into the sibling `-text` corpus
     directory.
   - For project-local corpora, it is also valid to keep the extracted text
     beside the papers as sibling `.pdf.txt` files when that makes the project
     easier to inspect and avoids creating a second large local corpus lane.
   - Keep the extraction manifest.
9. Read the extraction manifest honestly.
   - Confirm which files produced useful text.
   - Call out tiny redirect/placeholder HTML extracts so they are not mistaken
     for real paper text.
10. Ask whether the operator wants to recover any missed papers manually.
   - Use this step when some papers are still missing or only weakly acquired:
     - failed downloads
     - landing-page downloads
     - unresolved source copies
   - Ask explicitly whether the operator wants to add any of those papers
     manually now.
   - If the answer is `no`, continue to the final report.
   - If the answer is `yes`:
     - use the same raw corpus download directory as the drop location:
       the owning repo's raw corpus directory for that lane
     - normalize the manually added filename so it fits the corpus naming lane
       as closely as possible
     - rectify the reference list if a better source URL was discovered during
       manual recovery
     - if the paper was acquired manually without a better durable source URL,
       leave the BibTeX or legacy compatibility list honest and add a nearby note
       rather than fabricating a false provenance link
     - rerun the text extraction pass so the added paper lands in the sibling
       `-text` corpus
     - reread the manifests
     - ask again whether the operator wants to recover any additional missed
       papers manually
   - Continue this loop until the operator says there are no more manual
     recoveries to add.
11. Report the corpus state.
   - Report:
     - URL list path
     - download directory
     - text directory
     - download manifest
     - extraction manifest
     - any weak or unresolved entries
   - If the paper set is meant to support a current idea or mission, say that
     explicitly so the corpus has a clear local reason for existence.

## Missing Reference Recovery And Source Improvement

When a lane has blank URLs, weak landing pages, or mixed outcomes in the
manifest, treat repair as a bounded evidence-driven cleanup pass rather than a
fresh bibliography hunt.

Recommended process:

1. Start from the lane-local durable reference list and the current
   `downloads/download-manifest.tsv`.
2. Reuse manifest knowledge first:
   - prior `ok` rows with a live file do not need re-fetch
   - prior `error` and `non-paper` rows are already evidence about bad targets
3. Repair the durable lane-local reference list under `mfuton/papers/<lane>/`
   or under the owning sibling repo's `papers/<lane>/`, rather than mutating a
   transfer bundle or ad hoc scratch file.
4. Prefer targets that the downloader can classify honestly and fetch without a
   browser-only flow.
5. Rerun only the affected lane and let manifest reuse avoid unnecessary
   refetching.

Patterns that worked well:

- direct open PDFs, including author-hosted or institution-hosted copies
- stable abstract or metadata pages with substantial paper content when a PDF
  is not available
- controller-side upgrades from paper landing pages to direct PDFs when the
  transport rule is stable, such as arXiv `abs/...` to `pdf/...pdf`
- domain-specific stable pages such as PubMed, ACL Anthology, IDEAS/RePEc,
  CiNii, and Open Library when they provide durable bibliographic or paper
  context
- official documentation or project pages for software/tool references when
  the lane intentionally includes tools, not just papers
- improving the classifier when a good page family is being misread by an
  over-broad heuristic

Patterns that worked poorly:

- JavaScript-heavy publisher pages, redirect shells, and browser-mediated flows
- access-walled or anti-bot pages that return HTML but not the paper itself
- generic Google Books or similar preview/search pages as durable fetch targets
- assuming `urlformat = {pdf}` means the existing URL is already a PDF
- blind bulk URL substitution based only on title pattern matches without
  inspecting the exact BibTeX entry first

Practical source-selection rule:

- prefer, in order:
  - direct accessible PDF
  - stable contentful abstract/metadata HTML
  - weaker publisher or DOI landing page only when nothing better is known yet

Durable writeback rule:

- keep what worked and what failed in the lane artifacts:
  - repaired URLs belong in the lane-local durable reference list
  - transport/payload outcomes belong in the manifest
  - recovery lessons that generalize belong in this algorithm or the governing
    mission, not only in chat residue

## Outputs

- a durable paper reference list, preferably BibTeX
- a raw paper download directory with manifest
- a sibling extracted-text directory with manifest
- explicit accounting of any unresolved or weak entries
- explicit accounting of any operator-manual recovery additions

## Stop Conditions

Stop and ask the operator if any of these happen:

- the paper set or destination is still ambiguous
- the download policy would stage a large local corpus unintentionally
- the requested source is behind access barriers and needs credentials/cookies
- the work is turning into large-scale bibliography curation rather than a
  bounded acquisition run
- the manual-recovery loop is drifting into a long operator-side document hunt
  that should be split into a follow-on continuation

## Notes

- The truthful first artifact is the paper reference list, not the downloads.
- The truthful final artifact is usually the searchable text corpus, not merely
  the raw files.
- Corpus placement is project-relative when the papers primarily serve one
  sibling repo such as `redistrict`; use `mfuton/papers` only when the corpus
  is truly installation-general.
- DOI or publisher landing pages may still be worth recording in the reference
  list when no better source is yet known, but they should remain visibly
  classified as weaker entries until a real paper copy is obtained.
- Manual operator recovery is a normal completion step for a bounded paper set,
  but provenance should remain honest: rename and re-place the file as needed,
  but do not invent a source URL that was never actually identified.
