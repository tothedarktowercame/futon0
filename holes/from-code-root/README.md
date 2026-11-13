# futon5

Concentration layer for meta-pattern operators. Patterns are lifted from
Futon3 flexiarg entries, mapped onto sigils, and executed via operators that
manipulate CA/CT/music improvisation substrates. Refer to `AGENTS.md` for the
current roadmap. The `resources/futon5/sigil_patterns.edn` registry links sigils
(e.g. 手 for BlendHand) to pattern/role metadata.

### Future Work: String Diagram Documentation

As we lift more patterns out of Futon3, Futon5 will treat each facet as a node
in a string diagram: Futon3 narrative → Futon5 uplift/facet → Futon2 binding.
The next milestone is to log each match with its provenance (pattern IDs, facet
invariants, config deltas) and render the pipeline visually so other
domains—music improvisers, art-market scouts, hyper-ants walking graphs—can see
how their behaviours compose. Expect future releases to ship small diagram
generators alongside the HUD/analyzer traces so every “cyber-ant” or successor
agent has a narratable construction story.

### Devmaps + Model Spec Work (Status)

Solved:
- Open-world invariants pass end-to-end; verify run is clean.
- Model registry endpoint exists (`/meta/model/registry`) for patterns/docbook/open-world.
- Open-world descriptor + bootstrap/verify scripts in place.
- Hydration avoids creating graph-memory stubs for open-world entities.

Still to do:
- Pattern invariant: enforce core components (context/if/however/then/because/next-steps) plus conclusion/instantiated-by rule.
- Pattern sync UX: suppress "Missing in Futon1" line when diff summary is empty.
- Pattern sync runtime: improve progress + timeout handling during long `ensure_entity` runs.
- Open-world schema expansion: define specs for all ingest entity kinds (including `:proper`) once meaning/usage is clarified.
