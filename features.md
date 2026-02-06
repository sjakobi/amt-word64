# Feature Comparison: GHC Word64Map vs. amt-word64

This document tracks the features typically found in GHC's `Word64Map` API and checks their current implementation status in this project.

## Query
- [x] **`null`**: Implemented.
- [x] **`size`**: Implemented.
- [x] **`member`**: Implemented.
- [x] **`notMember`**: Implemented.
- [x] **`lookup`**: Implemented.
- [x] **`findWithDefault`**: Implemented.
- [ ] **`lookupLT`, `lookupGT`, `lookupLE`, `lookupGE`**: Not planned.

## Construction
- [x] **`empty`**: Implemented.
- [x] **`singleton`**: Implemented.
- [x] **`insert`**: Implemented.
- [x] **`insertWith` / `insertWithKey`**: Implemented.
- [x] **`delete`**: Implemented.
- [x] **`adjust` / `adjustWithKey`**: Implemented.
- [x] **`update` / `updateWithKey`**: Implemented.
- [x] **`alter`**: Implemented.

## Combine
- [x] **`union`**: Implemented.
- [x] **`unionWith` / `unionWithKey`**: Implemented.
- [x] **`difference` / `differenceWith`**: Implemented.
- [x] **`intersection` / `intersectionWith`**: Implemented.
- [x] **`mergeWithKey`**: Implemented.

## Traversal / Conversion
- [x] **`map` / `mapWithKey`**: Implemented.
- [ ] **`traverseWithKey`**: Skip for now
- [x] **`foldr` / `foldl'` / `foldrWithKey` / `foldlWithKey'`**: Implemented.
- [x] **`elems`**: Implemented.
- [x] **`keys`**: Implemented.
- [x] **`assocs`**: Implemented.
- [x] **`toList`**: Implemented.
- [x] **`fromList`**: Implemented.

## Filter / Partition
- [x] **`filter` / `filterWithKey`**: Implemented.
- [x] **`partition` / `partitionWithKey`**: Implemented.
- [x] **`mapMaybe` / `mapEither`**: Implemented.

## Min / Max
- [ ] **`findMin` / `findMax`**: Not planned.
- [ ] **`deleteMin` / `deleteMax`**: Not planned.
- [ ] **`minView` / `maxView`**: Not planned.

## Submap
- [ ] **`split`**: Not planned.
- [ ] **`splitLookup`**: Not planned.
- [x] **`isSubmapOf` / `isSubmapOfBy`**: Implemented.

## Instances
- [x] **`Eq`**: Implemented.
- [x] **`Ord`**: Implemented.
- [x] **`Read`**: Implemented.
- [x] **`Show`**: Implemented.
- [x] **`Functor`**: Implemented.
- [x] **`Foldable`**: Implemented.
- [x] **`Traversable`**: Implemented (existing).
- [x] **`Semigroup`**: Implemented.
- [x] **`Monoid`**: Implemented.
- [x] **`NFData`**: Implemented.
- [x] **`Data`**: Implemented.
- [x] **`IsList`**: Implemented.
- [x] **`Eq1`**: Implemented.
- [x] **`Ord1`**: Implemented.
- [x] **`Read1`**: Implemented.
- [x] **`Show1`**: Implemented.
- [ ] **Untested instances**: `Traversable`, `NFData`, `Data`, `Eq1`, `Ord1`, `Read1`, `Show1`.
