# Changelog

This changelog tracks changes to Futon0's reporting and reflection surface.
It is part of the protocol: improvements in what the system can attest, how it
derives those claims, and which evidence sources are online should be recorded
here over time.

## 2026-03-10

### Added

- End-to-end quarterly demo using:
  - Stack HUD daily summaries as envelope input
  - `futon0.rhythm.affect` as the affect/event derivation layer
  - git logs as retrospective fallback evidence when the Evidence Landscape is not yet fully available
- Readable quarterly Markdown output alongside `quarterly.json`
- Quarter-local affect summaries with marker counts, source breakdowns, top terms, and recent trigger examples
- Protocol note on derivative attestation targets:
  - Activity
  - Attention
  - Time-shape
  - Speech mode
  - Social role
  - Energy effect
  - Compatibility
  - Distortion

### Changed

- `futon0.rhythm.affect` no longer depends on the old FUTON1 affect-transition endpoint
- Affect ingest is now Evidence-Landscape-first, with `--entries-file`, `--evidence-url`, and `--git-repo` fallback modes
- Quarterly reporting now emphasizes inspectable summaries instead of placeholder global averages

### Notes

- Current retrospective completeness still depends on available Stack HUD summary history
- Long-term source of truth for richer evidence remains the Evidence Landscape rooted in `futon1a`
- New report categories should be added here as they become operational rather than only conceptual
