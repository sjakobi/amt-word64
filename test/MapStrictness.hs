module MapStrictness
  ( tests
  ) where

import Amt.Word64.Map.Strict qualified as Strict
import Data.Bifunctor (second)
import Data.Word (Word64)
import StrictnessTooling (isWhnfInt, mkThunk)
import Test.QuickCheck (Property)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (ioProperty, testProperty)

tests :: TestTree
tests =
  testGroup
    "Map.Strict strictness"
    [ testProperty "Strict.empty has WHNF values" $
        prop_strict_empty_whnf
    , testProperty "Strict.singleton forces WHNF values" $
        prop_strict_singleton_whnf
    , testProperty "Strict.fromList forces WHNF values" $
        prop_strict_fromList_whnf
    , testProperty "Strict.adjust forces WHNF values" $
        prop_strict_adjust_whnf
    , testProperty "Strict.adjustWithKey forces WHNF values" $
        prop_strict_adjust_with_key_whnf
    , testProperty "Strict.delete preserves WHNF values" $
        prop_strict_delete_whnf
    , testProperty "Strict.insert forces WHNF values" $
        prop_strict_insert_whnf_values
    , testProperty "Strict.insertWith forces WHNF values" $
        prop_strict_insert_with_whnf
    , testProperty "Strict.insertWithKey forces WHNF values" $
        prop_strict_insert_with_key_whnf
    , testProperty "Strict.update forces WHNF values" $
        prop_strict_update_whnf
    , testProperty "Strict.updateWithKey forces WHNF values" $
        prop_strict_update_with_key_whnf
    , testProperty "Strict.alter forces WHNF values" $
        prop_strict_alter_whnf
    , testProperty "Strict.map forces WHNF values" $
        prop_strict_map_whnf
    , testProperty "Strict.mapWithKey forces WHNF values" $
        prop_strict_map_with_key_whnf
    , testProperty "Strict.union preserves WHNF values" $
        prop_strict_union_whnf
    , testProperty "Strict.unionWith forces WHNF values" $
        prop_strict_union_with_whnf
    , testProperty "Strict.unionWithKey forces WHNF values" $
        prop_strict_union_with_key_whnf
    , testProperty "Strict.mergeWithKey forces WHNF values" $
        prop_strict_merge_with_key_whnf
    , testProperty "Strict.difference preserves WHNF values" $
        prop_strict_difference_whnf
    ]

prop_strict_empty_whnf :: Property
prop_strict_empty_whnf = ioProperty $ allWhnfMap Strict.empty

prop_strict_insert_whnf_values :: [(Word64, Int)] -> Word64 -> Int -> Property
prop_strict_insert_whnf_values entries k v = ioProperty $ do
  let m0 = Strict.fromList entries
  let vThunk = mkThunk $ v + 1
  let m1 = Strict.insert k vThunk m0
  allWhnfMap m1

prop_strict_insert_with_whnf :: [(Word64, Int)] -> Word64 -> Int -> Property
prop_strict_insert_with_whnf entries k v = ioProperty $ do
  let m0 = Strict.insert k v (Strict.fromList entries)
  let vThunk = mkThunk $ v + 1
  let m1 = Strict.insertWith (\_ _ -> vThunk) k v m0
  allWhnfMap m1

prop_strict_insert_with_key_whnf ::
  [(Word64, Int)] -> Word64 -> Int -> Property
prop_strict_insert_with_key_whnf entries k v = ioProperty $ do
  let m0 = Strict.insert k v (Strict.fromList entries)
  let vThunk = mkThunk $ v + 1
  let m1 = Strict.insertWithKey (\_ _ _ -> vThunk) k v m0
  allWhnfMap m1

prop_strict_update_whnf :: [(Word64, Int)] -> Word64 -> Int -> Property
prop_strict_update_whnf entries k v = ioProperty $ do
  let m0 = Strict.fromList ((k, v) : entries)
  let vThunk = mkThunk $ v + 1
  let m1 = Strict.update (\_ -> Just vThunk) k m0
  allWhnfMap m1

prop_strict_update_with_key_whnf ::
  [(Word64, Int)] -> Word64 -> Int -> Property
prop_strict_update_with_key_whnf entries k v = ioProperty $ do
  let m0 = Strict.fromList ((k, v) : entries)
  let vThunk = mkThunk $ v + 1
  let m1 = Strict.updateWithKey (\_ _ -> Just vThunk) k m0
  allWhnfMap m1

prop_strict_alter_whnf :: [(Word64, Int)] -> Word64 -> Int -> Property
prop_strict_alter_whnf entries k v = ioProperty $ do
  let m0 = Strict.fromList entries
  let vThunk = mkThunk $ v + 1
  let m1 = Strict.alter (const (Just vThunk)) k m0
  allWhnfMap m1

prop_strict_map_whnf :: [(Word64, Int)] -> Property
prop_strict_map_whnf entries = ioProperty $ do
  let m0 = Strict.fromList entries
  let m = Strict.map (\x -> mkThunk (x + 1)) m0
  allWhnfMap m

prop_strict_map_with_key_whnf :: [(Word64, Int)] -> Property
prop_strict_map_with_key_whnf entries = ioProperty $ do
  let m0 = Strict.fromList entries
  let m = Strict.mapWithKey (\_ x -> mkThunk (x + 1)) m0
  allWhnfMap m

prop_strict_singleton_whnf :: Word64 -> Int -> Property
prop_strict_singleton_whnf k v = ioProperty $ do
  let vThunk = mkThunk v
  let m = Strict.singleton k vThunk
  allWhnfMap m

prop_strict_fromList_whnf :: [(Word64, Int)] -> Property
prop_strict_fromList_whnf entries = ioProperty $ do
  let entriesThunk = fmap (second mkThunk) entries
  let m = Strict.fromList entriesThunk
  allWhnfMap m

prop_strict_adjust_whnf :: [(Word64, Int)] -> Word64 -> Int -> Property
prop_strict_adjust_whnf entries k v = ioProperty $ do
  let m0 = Strict.fromList ((k, v) : entries)
  let vThunk = mkThunk v
  let m1 = Strict.adjust (\_ -> vThunk) k m0
  allWhnfMap m1

prop_strict_adjust_with_key_whnf ::
  [(Word64, Int)] -> Word64 -> Int -> Property
prop_strict_adjust_with_key_whnf entries k v = ioProperty $ do
  let m0 = Strict.fromList ((k, v) : entries)
  let vThunk = mkThunk v
  let m1 = Strict.adjustWithKey (\_ _ -> vThunk) k m0
  allWhnfMap m1

prop_strict_delete_whnf :: [(Word64, Int)] -> Word64 -> Property
prop_strict_delete_whnf entries k = ioProperty $ do
  let m = Strict.delete k (Strict.fromList entries)
  allWhnfMap m

prop_strict_union_whnf :: [(Word64, Int)] -> [(Word64, Int)] -> Property
prop_strict_union_whnf entries1 entries2 = ioProperty $ do
  let m1 = Strict.fromList entries1
  let m2 = Strict.fromList entries2
  let m = Strict.union m1 m2
  allWhnfMap m

prop_strict_union_with_whnf ::
  [(Word64, Int)] ->
  [(Word64, Int)] ->
  Word64 ->
  Int ->
  Property
prop_strict_union_with_whnf entries1 entries2 k v = ioProperty $ do
  let m1 = Strict.fromList ((k, v) : entries1)
  let m2 = Strict.fromList ((k, v + 1) : entries2)
  let vThunk = mkThunk $ v + 2
  let m = Strict.unionWith (\_ _ -> vThunk) m1 m2
  allWhnfMap m

prop_strict_union_with_key_whnf ::
  [(Word64, Int)] ->
  [(Word64, Int)] ->
  Word64 ->
  Int ->
  Property
prop_strict_union_with_key_whnf entries1 entries2 k v = ioProperty $ do
  let m1 = Strict.fromList ((k, v) : entries1)
  let m2 = Strict.fromList ((k, v + 1) : entries2)
  let vThunk = mkThunk $ v + 2
  let m = Strict.unionWithKey (\_ _ _ -> vThunk) m1 m2
  allWhnfMap m

prop_strict_merge_with_key_whnf ::
  [(Word64, Int)] ->
  [(Word64, Int)] ->
  Word64 ->
  Int ->
  Property
prop_strict_merge_with_key_whnf entries1 entries2 k v = ioProperty $ do
  let m1 = Strict.fromList ((k, v) : entries1)
  let m2 = Strict.fromList ((k, v + 1) : entries2)
  let vThunk = mkThunk $ v + 2
  let m =
        Strict.mergeWithKey
          (\_ _ _ -> Just vThunk)
          id
          id
          m1
          m2
  allWhnfMap m

prop_strict_difference_whnf ::
  [(Word64, Int)] ->
  [(Word64, Int)] ->
  Word64 ->
  Int ->
  Property
prop_strict_difference_whnf entries1 entries2 k v = ioProperty $ do
  let m1 = Strict.fromList ((k, v) : entries1)
  let m2 = Strict.fromList ((k, v + 1) : entries2)
  let m = Strict.difference m1 m2
  allWhnfMap m

-- TODO: This forces the spine of the map.
-- It would be interesting to have a way to check the values without
-- forcing the spine.
--
-- Something like this maybe?!
-- https://stackoverflow.com/a/28687719/1013393
allWhnfMap :: Strict.Word64Map Int -> IO Bool
allWhnfMap m = and <$> traverse isWhnfInt m
