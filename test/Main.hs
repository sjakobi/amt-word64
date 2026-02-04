module Main (main) where

import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import MyLib (delete, empty, fromList, insert, lookup, null, singleton, size, toList, union, valid)
import Test.Tasty
import Test.Tasty.QuickCheck
import Prelude hiding (lookup, null)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Word64Map tests"
    [ testGroup "empty"
        [ testProperty "valid invariant" $ valid (empty @Int)
        ]
    , testGroup "singleton"
        [ testProperty "matches Data.Map" prop_singleton_model
        , testProperty "valid invariant" prop_singleton_valid
        ]
    , testGroup "insert"
        [ testProperty "matches Data.Map" prop_insert_model
        , testProperty "valid invariant" prop_insert_valid
        ]
    , testGroup "delete"
        [ testProperty "matches Data.Map" prop_delete_model
        , testProperty "valid invariant" prop_delete_valid
        ]
    , testGroup "union"
        [ testProperty "matches Data.Map" prop_union_model
        , testProperty "valid invariant" prop_union_valid
        ]
    , testGroup "fromList"
        [ testProperty "matches Data.Map" prop_fromList_model
        , testProperty "valid invariant" prop_fromList_valid
        ]
    , testGroup "toList"
        [ testProperty "matches Data.Map" prop_toList_model
        ]
    , testGroup "null"
        [ testProperty "matches Data.Map" prop_null_model
        ]
    , testGroup "size"
        [ testProperty "matches Data.Map" prop_size_model
        ]
    , testGroup "lookup"
        [ testProperty "matches Data.Map" prop_lookup_model
        ]
    ]

prop_singleton_model :: Word64 -> Int -> Bool
prop_singleton_model k v =
  let myMap = singleton k v
      refMap = Map.singleton k v
   in lookup k myMap == Map.lookup k refMap

prop_singleton_valid :: Word64 -> Int -> Bool
prop_singleton_valid k v = valid (singleton k v)

prop_insert_model :: [(Word64, Int)] -> Word64 -> Int -> Bool
prop_insert_model entries k v =
  let myMap = insert k v (fromList entries)
      refMap = Map.insert k v (Map.fromList entries)
   in all (\k' -> lookup k' myMap == Map.lookup k' refMap) (k : map fst entries)

prop_insert_valid :: [(Word64, Int)] -> Word64 -> Int -> Bool
prop_insert_valid entries k v = valid (insert k v (fromList entries))

prop_delete_model :: [(Word64, Int)] -> [Word64] -> Bool
prop_delete_model entries keys =
  let myMap = foldl (\m k -> delete k m) (fromList entries) keys
      refMap = foldl (\m k -> Map.delete k m) (Map.fromList entries) keys
   in all (\k -> lookup k myMap == Map.lookup k refMap) (keys ++ map fst entries)

prop_delete_valid :: [(Word64, Int)] -> [Word64] -> Bool
prop_delete_valid entries keys =
  valid (foldl (\m k -> delete k m) (fromList entries) keys)

prop_union_model :: [(Word64, Int)] -> [(Word64, Int)] -> Bool
prop_union_model e1 e2 =
  let m1 = fromList e1
      m2 = fromList e2
      ref1 = Map.fromList e1
      ref2 = Map.fromList e2
      myUnion = union m1 m2
      refUnion = Map.union ref1 ref2
   in all (\k -> lookup k myUnion == Map.lookup k refUnion) (map fst e1 ++ map fst e2)

prop_union_valid :: [(Word64, Int)] -> [(Word64, Int)] -> Bool
prop_union_valid e1 e2 = valid (union (fromList e1) (fromList e2))

prop_fromList_model :: [(Word64, Int)] -> Bool
prop_fromList_model entries =
  let myMap = fromList entries
      refMap = Map.fromList entries
   in all (\k -> lookup k myMap == Map.lookup k refMap) (map fst entries)

prop_fromList_valid :: [(Word64, Int)] -> Bool
prop_fromList_valid entries = valid (fromList entries)

prop_toList_model :: [(Word64, Int)] -> Bool
prop_toList_model entries =
  let myMap = fromList entries
      refMap = Map.fromList entries
      myList = toList myMap
   in Map.fromList myList == refMap

prop_null_model :: [(Word64, Int)] -> Bool
prop_null_model entries =
  let myMap = fromList entries
      refMap = Map.fromList entries
   in null myMap == Map.null refMap

prop_size_model :: [(Word64, Int)] -> Bool
prop_size_model entries =
  let myMap = fromList entries
      refMap = Map.fromList entries
   in size myMap == Map.size refMap

prop_lookup_model :: [(Word64, Int)] -> [Word64] -> Bool
prop_lookup_model entries keys =
  let myMap = fromList entries
      refMap = Map.fromList entries
   in all (\k -> lookup k myMap == Map.lookup k refMap) (keys ++ map fst entries)
