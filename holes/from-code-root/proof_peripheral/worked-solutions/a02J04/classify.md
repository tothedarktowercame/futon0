### CLASSIFY

**Classification**

`partial`

The mathematical solution is complete, and the Lean file is nontrivial and
`sorry`-free, but the formal theorem still stops one layer short of the original
statement. It proves the right candidate density, support split, and reciprocal
identity, but it does not yet connect them to the actual pushforward measure via
`Measure.map`/`withDensity`/`rnDeriv`.

That makes this a real partial, not a fake one:

- the target objects are meaningful;
- several nontrivial lemmas are closed;
- the remaining gap is specific and local;
- no predicate has been collapsed to `True`.

**CONFIDENCE INVERSION**

The surprise is that the hard part is not the density `2x`; everybody sees that
once the slice picture is drawn. The real trap is the direction of the
decomposition. This is not "find `dτ/dm`"; it is "find the decomposition of `m`
with respect to `τ`." Once you feel that asymmetry, the singular part outside
`[0,1]` becomes unavoidable.
