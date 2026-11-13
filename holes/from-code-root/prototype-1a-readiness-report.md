# Prototype 1(a) Stack Readiness Report (Futon0-Futon4, optional Futon5)

Scope: Prototype 0-1 clauses for Futon0-Futon4, plus a light Futon5 tail.
Evidence sources: `futon3/holes/futon*.devmap`, repo READMEs, and recent git logs.
Patterns applied: stack-blocker-detection, ready-blocked-triage, evidence-ledger, readme-devmap-sync, commit-intent-alignment, staleness-scan, futon1-determinism.

## Executive Summary
- status[nearly-ready]
- ready[Futon1 determinism gate, Futon2, Futon3]
- ready-with-notes[Futon0 Prototype 0; Futon4 Prototype 0]
- ready-pending-final-checks[Futon4 Prototype 1 release tag]
- blocked[Futon0 Prototype 1 evidence; README <-> devmap drift across Futon0/2/3/4]

## Final Checklist (Prototype 1 Exit)
1. Record Futon1 determinism gate evidence (README + CI workflow).
2. Add concrete evidence pointers for Futon0 Prototype 1 (artifact, test, or doc entry).
3. Resolve README <-> devmap mismatches for Futon0, Futon2, Futon3, Futon4.
4. Tag Futon4 Prototype 1 final testing artifacts (test run command noted; release prep tag pending).

## Futon0
Prototype 0 - System Vitality Indicators
- status[ready]
- evidence[`futon0/README.md` vitality scanner + git activity manifest]
- blocked-by[none]
- mismatch[devmap claims vs README focuses on utilities; README does not name Prototype 0]

Prototype 1 - Epistemic Rhythm & Salients
- status[blocked]
- blocked-by[evidence missing (no concrete artifacts linked)]
- unsupported[devmap claims only; no file or fixture link]
- mismatch[README lacks Prototype 1 narrative]

## Futon1
Prototype 0 - Baseline Deterministic Substrate
- status[ready]
- evidence[`futon1/docs/baseline.md`, `futon1/resources/baseline/demo_session.edn`, `futon1/README.md` baseline section, `futon4/docs/docbook/futon1/futon1-98dba414ca3d.org`, `futon4/docs/docbook/futon1/futon1-5319d9d9dd2c.org`, `futon4/docs/docbook/futon1/futon1-03d5fb6be080.org`]
- blocked-by[none]
- mismatch[none spotted; README asserts prototypes 0-1 are done but devmap has no explicit done status]

Prototype 1 - Demo/Client Determinism & Focus-Header Instrumentation
- status[ready]
- evidence[`futon1/README.md` determinism suite + `futon1/apps/client/test/client/session_test.clj`, `futon1/.github/workflows/futon1-stack-tests.yml`, `futon4/docs/docbook/futon1/futon1-67770f77efab.org`]
- drift[commit `b098687` "Prototype1a RC" but devmap has no status update]

Determinism gate (stack-coherence/futon1-determinism)
- status[ready]
- evidence[`futon1/README.md` determinism section, `futon1/.github/workflows/futon1-stack-tests.yml`, `futon1/apps/client/test/client/session_test.clj`]

## Futon2
Prototype 0 - AIF Stack Baseline
- status[ready]
- evidence[`futon2/doc/stack-baseline.md`, `futon2/doc/trace/single_tick.edn`, `futon2/doc/trace/single_tick.svg`, `futon2/test/resources/goldens/microtraces.edn`]
- blocked-by[none]

Prototype 1 - Observation Layer Hardening
- status[ready]
- evidence[`futon2/test/ants/aif/observe_test.clj`, `futon2/src/ants/aif/observe.clj`]
- drift[commit `84d7fad` "Prototype 1 readiness" but devmap has no status update]

README <-> devmap sync
- mismatch[README describes runtime + tests but does not name Prototype 0-1 clauses]

## Futon3
Prototype 0 - Flexiformal Transport Baseline
- status[ready]
- evidence[`futon3/docs/protocol/transport-contract-v1.md`, `futon3/docs/protocol/golden-transcripts.md`, `futon3/docs/sandbox/README.md`, `futon3/test/transport_test.clj`]
- blocked-by[none]

Prototype 1 - Pattern Canon & Standard Library
- status[ready]
- evidence[`futon3/src/futon3/pattern_store.clj`, `futon3/test/pattern_store_test.clj`, `futon3/resources/pattern_store.edn`]
- drift[next-evidence goldens not yet landed]

README <-> devmap sync
- mismatch[README covers prototypes 0-5 only; devmap includes prototypes 6-9 with no README mention]

## Futon4
Prototype 0 - Foundations
- status[ready]
- evidence[`spine.org`, `dev/bootstrap.el`, `AGENTS.md`, `docs/reanimation-plan.org`, `test/arxana-bootstrap-test.el`, `arxana-tangled.el`]
- blocked-by[none]

Prototype 1 - Arxana Reboot
- status[ready-pending-final-checks]
- evidence[`spine.org`, `dev/arxana-relations.el`, `dev/arxana-browse.el`, `dev/arxana-links.el`, `0-Reintroduction.org`, `test/` suite (full `dev/run-tests.sh`), `test/arxana-links-test.el` integration coverage, `docs/arxana-links-guide.md`, `dev/arxana-store.el`, `docs/storage-bridge.org`, `dev/arxana-article.el`, `dev/arxana-scholium.el`, `dev/arxana-derivation.el`, `dev/arxana-inclusion.el`, `docs/contributor-guide.md`, `docs/docbook/futon4/futon4.org`]
- blocked-by[release prep tag]

## Futon5 (optional tail)
Prototype 0 - Nonstarter Frame
- status[blocked]
- blocked-by[design-only plan; no artifacts]
- unsupported[`futon5/README.md` notes design intent only]

Prototype 1 - Patterns of Improvisation
- status[blocked]
- blocked-by[no operator specs or protobook artifacts yet]
- unsupported[none]

## Staleness Scan (timestamps)
- freshness-window[30 days]
- stale[`futon3/holes/futon0.devmap` last touched Sat Dec 6 2025]
- stale[`futon3/holes/futon1.devmap` last touched Fri Dec 12 2025]
- stale[`futon3/holes/futon2.devmap` last touched Fri Dec 12 2025]
- stale[`futon3/holes/futon3.devmap` last touched Sun Dec 28 2025]
- stale[`futon3/holes/futon4.devmap` last touched Tue Dec 2 2025]
- stale[`futon3/holes/futon5.devmap` last touched Sat Dec 6 2025]
- stale[README touchpoints: Futon0 (Dec 26), Futon1 (Dec 13), Futon2 (Dec 28), Futon3 (Dec 28), Futon4 (Dec 27)]

## Prototype 1(a) Exit Criteria Check
- status[partial]
- blocked-by[Futon1 determinism gate needs run log; Futon0 Prototype 1 evidence missing; Futon4 final test run + release tag pending]
- mismatch[README <-> devmap alignment gaps in Futon0, Futon2, Futon3, Futon4]
