module Main (main) where

import Control.Monad (unless)
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import MyLib (delete, fromList, lookup, null, singleton, size, toList, union, valid)
import System.Exit (exitFailure)
import Test.QuickCheck
import Prelude hiding (lookup, null)

prop_lookup :: [(Word64, Int)] -> [Word64] -> Bool
prop_lookup entries keys =
  let myMap = fromList entries
      refMap = Map.fromList entries
   in all (\k -> lookup k myMap == Map.lookup k refMap) (keys ++ map fst entries)

prop_valid :: [(Word64, Int)] -> Bool
prop_valid entries =
  let myMap = fromList entries
   in valid myMap

prop_singleton :: Word64 -> Int -> Bool
prop_singleton k v =
  let myMap = singleton k v
      refMap = Map.singleton k v
   in lookup k myMap == Map.lookup k refMap && valid myMap

prop_null :: [(Word64, Int)] -> Bool
prop_null entries =
  let myMap = fromList entries
      refMap = Map.fromList entries
   in null myMap == Map.null refMap

prop_size :: [(Word64, Int)] -> Bool
prop_size entries =
  let myMap = fromList entries
      refMap = Map.fromList entries
   in size myMap == Map.size refMap

prop_fromList :: [(Word64, Int)] -> Bool
prop_fromList entries =
  let myMap = fromList entries
      refMap = Map.fromList entries
   in all (\k -> lookup k myMap == Map.lookup k refMap) (map fst entries) && valid myMap

prop_toList :: [(Word64, Int)] -> Bool
prop_toList entries =
  let myMap = fromList entries
      refMap = Map.fromList entries
      myList = toList myMap
   in Map.fromList myList == refMap

prop_delete :: [(Word64, Int)] -> [Word64] -> Bool
prop_delete entries keys =
  let myMap = foldl (\m k -> delete k m) (fromList entries) keys
      refMap = foldl (\m k -> Map.delete k m) (Map.fromList entries) keys
   in all (\k -> lookup k myMap == Map.lookup k refMap) (keys ++ map fst entries) && valid myMap

prop_union :: [(Word64, Int)] -> [(Word64, Int)] -> Bool
prop_union e1 e2 =
  let m1 = fromList e1
      m2 = fromList e2
      ref1 = Map.fromList e1
      ref2 = Map.fromList e2
      myUnion = union m1 m2
      refUnion = Map.union ref1 ref2
   in all (\k -> lookup k myUnion == Map.lookup k refUnion) (map fst e1 ++ map fst e2) && valid myUnion

main :: IO ()
main = do
  results <-
    sequence
      [ quickCheckResult prop_lookup
      , quickCheckResult prop_valid
      , quickCheckResult prop_singleton
      , quickCheckResult prop_null
      , quickCheckResult prop_size
      , quickCheckResult prop_fromList
      , quickCheckResult prop_toList
      , quickCheckResult prop_delete
      , quickCheckResult prop_union
      ]
  unless (all isSuccess results) exitFailure
