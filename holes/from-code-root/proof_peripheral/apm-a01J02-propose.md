**THE KEY INSIGHT**
- Lebesgue measurable sets and functions are the completion of the Borel σ-algebra: there is always a Borel set/function that agrees almost everywhere with the original. For sets, pick the `μ`-to-measurable representative provided by completion; for functions, approximate by simple functions supported on measurable sets and replace each support by its Borel representative (or, equivalently, use the measurable “version” given by AEMeasurable.mk).

**THE NAIVE APPROACH THAT FAILS**
- Trying to make `A` Borel by taking closures/interiors, or to make `f` Borel by forcing continuity/smoothness, generally changes the measure on more than a null set. Without explicitly invoking completion (i.e., modifying only on sets of measure zero), there is no guarantee the modified object stays almost everywhere equal to the original.
