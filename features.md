# Feature Comparison: GHC Word64Map vs. amt-word64

This document tracks the features typically found in GHC's `Word64Map` API and checks their current implementation status in this project.

## Query
- [x] **`null`**: Implemented.
- [x] **`size`**: Implemented.
- [ ] **`member`**: Not implemented (can be derived from `lookup`).
- [ ] **`notMember`**: Not implemented.
- [x] **`lookup`**: Implemented.
- [ ] **`findWithDefault`**: Not implemented.
- [ ] **`lookupLT`, `lookupGT`, `lookupLE`, `lookupGE`**: Not planned.

## Construction
- [x] **`empty`**: Implemented.
- [x] **`singleton`**: Implemented.
- [x] **`insert`**: Implemented (basic replacement).
- [ ] **`insertWith` / `insertWithKey`**: Not implemented.
- [x] **`delete`**: Implemented.
- [ ] **`adjust` / `adjustWithKey`**: Not implemented.
- [ ] **`update` / `updateWithKey`**: Not implemented.
- [ ] **`alter`**: Not implemented.

## Combine
- [x] **`union`**: Implemented (left-biased).
- [ ] **`unionWith` / `unionWithKey`**: Not implemented.
- [ ] **`difference` / `differenceWith`**: Not implemented.
- [ ] **`intersection` / `intersectionWith`**: Not implemented.
- [ ] **`mergeWithKey`**: Not implemented.

## Traversal / Conversion
- [ ] **`map` / `mapWithKey`**: Not implemented.
- [ ] **`traverseWithKey`**: Not implemented.
- [ ] **`foldr` / `foldl'` / `foldrWithKey` / `foldlWithKey'`**: Not implemented.
- [ ] **`elems`**: Not implemented.
- [ ] **`keys`**: Not implemented.
- [ ] **`assocs`**: Not implemented.
- [x] **`toList`**: Implemented (unsorted but deterministic).
- [x] **`fromList`**: Implemented.

## Filter / Partition
- [ ] **`filter` / `filterWithKey`**: Not implemented.
- [ ] **`partition` / `partitionWithKey`**: Not implemented.
- [ ] **`mapMaybe` / `mapEither`**: Not implemented.

## Min / Max
- [ ] **`findMin` / `findMax`**: Not planned.
- [ ] **`deleteMin` / `deleteMax`**: Not planned.
- [ ] **`minView` / `maxView`**: Not planned.

## Submap
- [ ] **`split`**: Not planned.
- [ ] **`splitLookup`**: Not planned.
- [ ] **`isSubmapOf` / `isSubmapOfBy`**: Not implemented.
