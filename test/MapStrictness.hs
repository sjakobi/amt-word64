module MapStrictness
  ( tests
  ) where

import Amt.Word64.Map.Strict qualified as Strict
import Control.DeepSeq (deepseq)
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
    [ testProperty "Strict.singleton forces WHNF values" $
        prop_strict_singleton_whnf
    , testProperty "Strict.fromList forces WHNF values" $
        prop_strict_fromList_whnf
    , testProperty "Strict.adjust forces WHNF values" $
        prop_strict_adjust_whnf
    , testProperty "Strict.insert forces WHNF values" $
        prop_strict_insert_whnf_values
    , testProperty "Strict.union preserves WHNF values" $
        prop_strict_union_whnf
    ]

prop_strict_insert_whnf_values :: [(Word64, Int)] -> Word64 -> Int -> Property
prop_strict_insert_whnf_values entries k v = ioProperty $ do
  let m0 = Strict.fromList entries
  m0 `deepseq` pure ()
  let vThunk = mkThunk $ v + 1
  let m1 = Strict.insert k vThunk m0
  allWhnfMap m1

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
  m0 `deepseq` pure ()
  let vThunk = mkThunk v
  let m1 = Strict.adjust (\_ -> vThunk) k m0
  allWhnfMap m1

prop_strict_union_whnf :: [(Word64, Int)] -> [(Word64, Int)] -> Property
prop_strict_union_whnf entries1 entries2 = ioProperty $ do
  let m1 = Strict.fromList entries1
  let m2 = Strict.fromList entries2
  m1 `deepseq` m2 `deepseq` pure ()
  let m = Strict.union m1 m2
  allWhnfMap m

-- TODO: This forces the spine of the map.
-- It would be interesting to have a way to check the values without
-- forcing the spine.
--
-- Something like this maybe?!
-- https://stackoverflow.com/a/28687719/1013393
allWhnfMap :: Strict.Word64Map Int -> IO Bool
allWhnfMap m = and <$> traverse isWhnfInt m
