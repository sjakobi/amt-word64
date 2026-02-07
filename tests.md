# Test Inventory

## containers (containers-tests directory)
- Location: `containers-tests/` in the `haskell/containers` repository (separate package alongside the library).
- Frameworks: `tasty` with `tasty-quickcheck` and `tasty-hunit`; strictness checks use `ChasingBottoms` helpers.
- Property tests: QuickCheck properties for `Map`, `Set`, `IntMap`, `IntSet`, `Seq`, `Tree`, `Graph`, `BitQueue`, and `ListUtils` (modules like `map-properties.hs`, `set-properties.hs`, `intmap-properties.hs`, etc.).
- Strictness tests: dedicated strictness property suites for `Map`, `IntMap`, `Set`, and `IntSet` (`*-strictness.hs`).
- Invariant/validity helpers: `IntMapValidity` and `IntSetValidity` modules used by property suites.

## unordered-containers
- Location: `tests/` in the `haskell-unordered-containers/unordered-containers` repository.
- Frameworks: `tasty` with `tasty-quickcheck` and `tasty-hunit`; strictness checks use `ChasingBottoms`.
- Property tests: QuickCheck properties for `HashMap` and `HashSet` (in `tests/Properties/*`).
- Regression tests: targeted HUnit/QuickCheck cases for prior bugs (`Regressions.hs`).
- Strictness tests: bottom/strictness properties for strict `HashMap` operations (`Strictness.hs`).

## vector
- Location: `vector/tests/` and `vector/tests-inspect/` in the `haskell/vector` repository.
- Frameworks: `tasty` with `tasty-quickcheck` and `tasty-hunit` for the main suites.
- Property and unit tests: `vector-tests-O0` and `vector-tests-O2` run the same Tasty program at two optimization levels to catch rule-related issues; includes unit tests and QuickCheck properties across boxed, primitive, storable, strict, and unboxed vectors plus bundle/move/specialization suites.
- Doctests: `vector-doctest` runs documentation examples via `doctest` (`tests/doctests.hs`).
- Inspection tests: `vector-inspection` uses `Test.Tasty.Inspection` (plugin-based Core/alloc/fusion checks) in `tests-inspect`.

## amt-word64 suggestions
- Model-based properties: compare against `Data.Map.Strict` for core ops (insert/delete/lookup/union/intersection/difference/map/mapMaybe/filter/partition/folds/toList/fromList).
- Invariant checks: run `valid` after each operation sequence step, not just at the end.
- Round-trips: `fromList . toList` idempotence; `fromListWith` matches reference semantics.
- Algebraic laws: union/intersection/difference identities; map/mapWithKey identity and composition.
- Key distribution edge cases: shared prefixes at deep levels, plus `0`, `maxBound`, and alternating bit patterns.
- Shift progression coverage: keys designed to hit many levels (bits set at multiples of 6).
- Strictness tests: key strictness, value strictness where applicable, and no accidental strictness for lazy paths.
- Structural invariants after updates: no redundant branches, empty only at root, bitmap size matches array size, prefix consistency.
- Regressions: explicit tests for `differenceWith` leaf handling and singleton interactions across combinators.
- Performance/inspection (optional): allocation sanity for hot ops and bitmap scan behavior regressions.
