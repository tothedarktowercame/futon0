`a02J04` worked solution reference bundle, full formal version.

This is the side-by-side companion to the earlier scaffold bundle. The
informal mathematics is the same, but the Lean layer now closes the actual
measure-theoretic bridge:

- it computes the marginal `τ` as a `withDensity`,
- it identifies the singular part of Lebesgue measure relative to `τ`,
- and it identifies the Radon–Nikodym derivative `m.rnDeriv τ`.

Bundle contents:

- `observe.md`
- `propose.md`
- `execute.md`
- `validate.md`
- `classify.md`
- `integrate.md`
- `proof-plan.edn`
- `changelog.edn`

The accompanying full Lean formalization is:

- [/home/joe/code/apm-lean/lean-proofs/worked/a02J04/Full.lean](/home/joe/code/apm-lean/lean-proofs/worked/a02J04/Full.lean)

The package-local build target used during development is:

- [/home/joe/code/apm-lean/ApmCanaries/A02J04Full.lean](/home/joe/code/apm-lean/ApmCanaries/A02J04Full.lean)
