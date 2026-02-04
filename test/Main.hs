module Main (main) where

import Amt.Word64.Map
  ( Word64Map
  , adjust
  , adjustWithKey
  , alter
  , assocs
  , delete
  , difference
  , differenceWith
  , elems
  , empty
  , filter
  , filterWithKey
  , findWithDefault
  , foldlWithKey'
  , foldrWithKey
  , fromList
  , insert
  , insertWith
  , insertWithKey
  , intersection
  , intersectionWith
  , intersectionWithKey
  , isSubmapOf
  , isSubmapOfBy
  , keys
  , lookup
  , map
  , mapEither
  , mapEitherWithKey
  , mapMaybe
  , mapMaybeWithKey
  , mapWithKey
  , member
  , mergeWithKey
  , notMember
  , null
  , partition
  , partitionWithKey
  , singleton
  , size
  , toList
  , union
  , unionWith
  , unionWithKey
  , update
  , updateWithKey
  , valid
  )
import Data.List qualified as L
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import Test.Tasty
import Test.Tasty.QuickCheck
import Prelude hiding (filter, lookup, map, null)

main :: IO ()
main =
  defaultMain $
    localOption (QuickCheckTests 500) $
      localOption (QuickCheckMaxSize 500) tests

tests :: TestTree
tests =
  testGroup
    "Word64Map tests"
    [ testGroup
        "empty"
        [ testProperty "valid invariant" $ checkValid (empty @Int)
        ]
    , testGroup
        "singleton"
        [ testProperty "matches Data.Map" prop_singleton_model
        , testProperty "valid invariant" prop_singleton_valid
        ]
    , testGroup
        "insert"
        [ testProperty "matches Data.Map" prop_insert_model
        , testProperty "valid invariant" prop_insert_valid
        ]
    , testGroup
        "delete"
        [ testProperty "matches Data.Map" prop_delete_model
        , testProperty "valid invariant" prop_delete_valid
        ]
    , testGroup
        "union"
        [ testProperty "matches Data.Map" prop_union_model
        , testProperty "valid invariant" prop_union_valid
        ]
    , testGroup
        "fromList"
        [ testProperty "matches Data.Map" prop_fromList_model
        , testProperty "valid invariant" prop_fromList_valid
        ]
    , testGroup
        "toList"
        [ testProperty "matches Data.Map" prop_toList_model
        ]
    , testGroup
        "null"
        [ testProperty "matches Data.Map" prop_null_model
        ]
    , testGroup
        "size"
        [ testProperty "matches Data.Map" prop_size_model
        ]
    , testGroup
        "lookup"
        [ testProperty "matches Data.Map" prop_lookup_model
        ]
    , testGroup
        "member"
        [ testProperty "matches Data.Map" prop_member_model
        ]
    , testGroup
        "notMember"
        [ testProperty "matches Data.Map" prop_notMember_model
        ]
    , testGroup
        "findWithDefault"
        [ testProperty "matches Data.Map" prop_findWithDefault_model
        ]
    , testGroup
        "keys"
        [ testProperty "matches toList" prop_keys_model
        ]
    , testGroup
        "elems"
        [ testProperty "matches toList" prop_elems_model
        ]
    , testGroup
        "assocs"
        [ testProperty "matches toList" prop_assocs_model
        ]
    , testGroup
        "Functor"
        [ testProperty "fmap matches Data.Map" prop_fmap_model
        ]
    , testGroup
        "Foldable"
        [ testProperty "foldr matches toList" prop_foldr_model
        , testProperty "length matches size" prop_length_model
        ]
    , {-
          , testGroup "Traversable"
              [ testProperty "traverse matches Data.Map" prop_traverse_model
              ]
      -}
      testGroup
        "foldrWithKey"
        [ testProperty "visits all elements" prop_foldrWithKey_model
        ]
    , testGroup
        "foldlWithKey'"
        [ testProperty "visits all elements" prop_foldlWithKey_model
        ]
    , testGroup
        "insertWith"
        [ testProperty "matches Data.Map" prop_insertWith_model
        , testProperty "valid invariant" prop_insertWith_valid
        ]
    , testGroup
        "insertWithKey"
        [ testProperty "matches Data.Map" prop_insertWithKey_model
        , testProperty "valid invariant" prop_insertWithKey_valid
        ]
    , testGroup
        "adjust"
        [ testProperty "matches Data.Map" prop_adjust_model
        , testProperty "valid invariant" prop_adjust_valid
        ]
    , testGroup
        "adjustWithKey"
        [ testProperty "matches Data.Map" prop_adjustWithKey_model
        , testProperty "valid invariant" prop_adjustWithKey_valid
        ]
    , testGroup
        "update"
        [ testProperty "matches Data.Map" prop_update_model
        , testProperty "valid invariant" prop_update_valid
        ]
    , testGroup
        "updateWithKey"
        [ testProperty "matches Data.Map" prop_updateWithKey_model
        , testProperty "valid invariant" prop_updateWithKey_valid
        ]
    , testGroup
        "alter"
        [ testProperty "matches Data.Map" prop_alter_model
        , testProperty "valid invariant" prop_alter_valid
        ]
    , testGroup
        "map"
        [ testProperty "matches Data.Map" prop_map_model
        , testProperty "valid invariant" prop_map_valid
        ]
    , testGroup
        "mapWithKey"
        [ testProperty "matches Data.Map" prop_mapWithKey_model
        , testProperty "valid invariant" prop_mapWithKey_valid
        ]
    , testGroup
        "unionWith"
        [ testProperty "matches Data.Map" prop_unionWith_model
        , testProperty "valid invariant" prop_unionWith_valid
        ]
    , testGroup
        "unionWithKey"
        [ testProperty "matches Data.Map" prop_unionWithKey_model
        , testProperty "valid invariant" prop_unionWithKey_valid
        ]
    , {-
          , testGroup
              "mergeWithKey"
              [ testProperty "matches Data.Map" prop_mergeWithKey_model
              , testProperty "valid invariant" prop_mergeWithKey_valid
              ]
      -}
      testGroup
        "difference"
        [ testProperty "matches Data.Map" prop_difference_model
        , testProperty "valid invariant" prop_difference_valid
        ]
    , testGroup
        "differenceWith"
        [ testProperty "matches Data.Map" prop_differenceWith_model
        , testProperty "valid invariant" prop_differenceWith_valid
        ]
    , testGroup
        "intersection"
        [ testProperty "matches Data.Map" prop_intersection_model
        , testProperty "valid invariant" prop_intersection_valid
        ]
    , testGroup
        "intersectionWith"
        [ testProperty "matches Data.Map" prop_intersectionWith_model
        , testProperty "valid invariant" prop_intersectionWith_valid
        ]
    , testGroup
        "intersectionWithKey"
        [ testProperty "matches Data.Map" prop_intersectionWithKey_model
        , testProperty "valid invariant" prop_intersectionWithKey_valid
        ]
    , testGroup
        "filter"
        [ testProperty "matches Data.Map" prop_filter_model
        , testProperty "valid invariant" prop_filter_valid
        ]
    , testGroup
        "filterWithKey"
        [ testProperty "matches Data.Map" prop_filterWithKey_model
        , testProperty "valid invariant" prop_filterWithKey_valid
        ]
    , testGroup
        "partition"
        [ testProperty "matches Data.Map" prop_partition_model
        , testProperty "valid invariant" prop_partition_valid
        ]
    , testGroup
        "partitionWithKey"
        [ testProperty "matches Data.Map" prop_partitionWithKey_model
        , testProperty "valid invariant" prop_partitionWithKey_valid
        ]
    , testGroup
        "mapMaybe"
        [ testProperty "matches Data.Map" prop_mapMaybe_model
        , testProperty "valid invariant" prop_mapMaybe_valid
        ]
    , testGroup
        "mapMaybeWithKey"
        [ testProperty "matches Data.Map" prop_mapMaybeWithKey_model
        , testProperty "valid invariant" prop_mapMaybeWithKey_valid
        ]
    , testGroup
        "mapEither"
        [ testProperty "matches Data.Map" prop_mapEither_model
        , testProperty "valid invariant" prop_mapEither_valid
        ]
    , testGroup
        "mapEitherWithKey"
        [ testProperty "matches Data.Map" prop_mapEitherWithKey_model
        , testProperty "valid invariant" prop_mapEitherWithKey_valid
        ]
    , testGroup
        "isSubmapOf"
        [ testProperty "matches Data.Map" prop_isSubmapOf_model
        ]
    , testGroup
        "isSubmapOfBy"
        [ testProperty "matches Data.Map" prop_isSubmapOfBy_model
        ]
    ]

sortToList :: Word64Map a -> [(Word64, a)]
sortToList = L.sortOn fst . toList

checkValid :: Word64Map a -> Property
checkValid m = case valid m of
  Nothing -> property True
  Just err -> counterexample (show err) False

prop_singleton_model :: Word64 -> Int -> Property
prop_singleton_model k v =
  sortToList (singleton k v) === Map.toList (Map.singleton k v)

prop_singleton_valid :: Word64 -> Int -> Property
prop_singleton_valid k v = checkValid (singleton k v)

prop_insert_model :: [(Word64, Int)] -> Word64 -> Int -> Property
prop_insert_model entries k v =
  sortToList (insert k v (fromList entries))
    === Map.toList (Map.insert k v (Map.fromList entries))

prop_insert_valid :: [(Word64, Int)] -> Word64 -> Int -> Property
prop_insert_valid entries k v = checkValid (insert k v (fromList entries))

prop_delete_model :: [(Word64, Int)] -> [Word64] -> Property
prop_delete_model entries ks =
  let myMap = foldl (\m k -> delete k m) (fromList entries) ks
      refMap = foldl (\m k -> Map.delete k m) (Map.fromList entries) ks
   in sortToList myMap === Map.toList refMap

prop_delete_valid :: [(Word64, Int)] -> [Word64] -> Property
prop_delete_valid entries ks =
  checkValid (foldl (\m k -> delete k m) (fromList entries) ks)

prop_union_model :: [(Word64, Int)] -> [(Word64, Int)] -> Property
prop_union_model e1 e2 =
  let m1 = fromList e1
      m2 = fromList e2
      ref1 = Map.fromList e1
      ref2 = Map.fromList e2
      myUnion = union m1 m2
      refUnion = Map.union ref1 ref2
   in sortToList myUnion === Map.toList refUnion

prop_union_valid :: [(Word64, Int)] -> [(Word64, Int)] -> Property
prop_union_valid e1 e2 = checkValid (union (fromList e1) (fromList e2))

prop_fromList_model :: [(Word64, Int)] -> Property
prop_fromList_model entries =
  sortToList (fromList entries) === Map.toList (Map.fromList entries)

prop_fromList_valid :: [(Word64, Int)] -> Property
prop_fromList_valid entries = checkValid (fromList entries)

prop_toList_model :: [(Word64, Int)] -> Property
prop_toList_model entries =
  sortToList (fromList entries) === Map.toList (Map.fromList entries)

prop_null_model :: [(Word64, Int)] -> Property
prop_null_model entries =
  null (fromList entries) === Map.null (Map.fromList entries)

prop_size_model :: [(Word64, Int)] -> Property
prop_size_model entries =
  size (fromList entries) === Map.size (Map.fromList entries)

prop_lookup_model :: [(Word64, Int)] -> Word64 -> Property
prop_lookup_model entries k =
  lookup k (fromList entries) === Map.lookup k (Map.fromList entries)

prop_member_model :: [(Word64, Int)] -> Word64 -> Property
prop_member_model entries k =
  member k (fromList entries) === Map.member k (Map.fromList entries)

prop_notMember_model :: [(Word64, Int)] -> Word64 -> Property
prop_notMember_model entries k =
  notMember k (fromList entries) === Map.notMember k (Map.fromList entries)

prop_findWithDefault_model :: [(Word64, Int)] -> Int -> Word64 -> Property
prop_findWithDefault_model entries def k =
  findWithDefault def k (fromList entries)
    === Map.findWithDefault def k (Map.fromList entries)

prop_keys_model :: [(Word64, Int)] -> Property
prop_keys_model entries =
  let m = fromList entries
   in L.sort (keys m) === L.sort (L.map fst (toList m))

prop_elems_model :: [(Word64, Int)] -> Property
prop_elems_model entries =
  let m = fromList entries
   in L.sort (elems m) === L.sort (L.map snd (toList m))

prop_assocs_model :: [(Word64, Int)] -> Property
prop_assocs_model entries =
  let m = fromList entries
   in L.sortOn fst (assocs m) === sortToList m

prop_fmap_model :: [(Word64, Int)] -> Property
prop_fmap_model entries =
  let f = (+ 1)
      m = fromList entries
      ref = Map.fromList entries
   in sortToList (fmap f m) === Map.toList (Map.map f ref)

prop_foldr_model :: [(Word64, Int)] -> Property
prop_foldr_model entries =
  let m = fromList entries
      f v acc = v : acc
   in L.sort (foldr f [] m) === L.sort (elems m)

prop_length_model :: [(Word64, Int)] -> Property
prop_length_model entries =
  let m = fromList entries
   in length m === size m

{-
prop_traverse_model :: [(Word64, Int)] -> Property
prop_traverse_model entries =
  let f v = [v, v + 1]
      m = fromList entries
      ref = Map.fromList entries
      -- traverse returns [Word64Map a]. We sort each map's list.
      sortMap m = sortToList m
   in L.sort (L.map sortMap (traverse f m)) === L.sort (L.map Map.toList (Map.traverseWithKey (\_ v -> f v) ref))
-}

prop_foldrWithKey_model :: [(Word64, Int)] -> Property
prop_foldrWithKey_model entries =
  let m = fromList entries
      f k v acc = (k, v) : acc
   in L.sortOn fst (foldrWithKey f [] m) === sortToList m

prop_foldlWithKey_model :: [(Word64, Int)] -> Property
prop_foldlWithKey_model entries =
  let m = fromList entries
      f acc k v = (k, v) : acc
   in L.sortOn fst (foldlWithKey' f [] m) === sortToList m

prop_insertWith_model :: [(Word64, Int)] -> Word64 -> Int -> Property
prop_insertWith_model entries k v =
  let f x y = x + y
      m = fromList entries
      ref = Map.fromList entries
   in sortToList (insertWith f k v m) === Map.toList (Map.insertWith f k v ref)

prop_insertWith_valid :: [(Word64, Int)] -> Word64 -> Int -> Property
prop_insertWith_valid entries k v =
  let f x y = x + y
   in checkValid (insertWith f k v (fromList entries))

prop_adjust_model :: [(Word64, Int)] -> Word64 -> Property
prop_adjust_model entries k =
  let f x = x + 1
      m = fromList entries
      ref = Map.fromList entries
   in sortToList (adjust f k m) === Map.toList (Map.adjust f k ref)

prop_adjust_valid :: [(Word64, Int)] -> Word64 -> Property
prop_adjust_valid entries k =
  let f x = x + 1
   in checkValid (adjust f k (fromList entries))

prop_update_model :: [(Word64, Int)] -> Word64 -> Property
prop_update_model entries k =
  let f x = if even x then Just (x + 1) else Nothing
      m = fromList entries
      ref = Map.fromList entries
   in sortToList (update f k m) === Map.toList (Map.update f k ref)

prop_update_valid :: [(Word64, Int)] -> Word64 -> Property
prop_update_valid entries k =
  let f x = if even x then Just (x + 1) else Nothing
   in checkValid (update f k (fromList entries))

prop_alter_model :: [(Word64, Int)] -> Word64 -> Property
prop_alter_model entries k =
  let f Nothing = Just 1
      f (Just x) = if even x then Just (x + 1) else Nothing
      m = fromList entries
      ref = Map.fromList entries
   in sortToList (alter f k m) === Map.toList (Map.alter f k ref)

prop_alter_valid :: [(Word64, Int)] -> Word64 -> Property
prop_alter_valid entries k =
  let f Nothing = Just 1
      f (Just x) = if even x then Just (x + 1) else Nothing
   in checkValid (alter f k (fromList entries))

prop_map_model :: [(Word64, Int)] -> Property
prop_map_model entries =
  let f x = x + 1
      m = fromList entries
      ref = Map.fromList entries
   in sortToList (map f m) === Map.toList (Map.map f ref)

prop_mapWithKey_model :: [(Word64, Int)] -> Property
prop_mapWithKey_model entries =
  let f k v = fromIntegral k + v
      m = fromList entries
      ref = Map.fromList entries
   in sortToList (mapWithKey f m) === Map.toList (Map.mapWithKey f ref)

prop_unionWith_model :: [(Word64, Int)] -> [(Word64, Int)] -> Property
prop_unionWith_model e1 e2 =
  let f x y = x + y
      m1 = fromList e1
      m2 = fromList e2
      ref1 = Map.fromList e1
      ref2 = Map.fromList e2
   in sortToList (unionWith f m1 m2) === Map.toList (Map.unionWith f ref1 ref2)

prop_unionWith_valid :: [(Word64, Int)] -> [(Word64, Int)] -> Property
prop_unionWith_valid e1 e2 =
  let f x y = x + y
   in checkValid (unionWith f (fromList e1) (fromList e2))

prop_difference_model :: [(Word64, Int)] -> [(Word64, Int)] -> Property
prop_difference_model e1 e2 =
  let m1 = fromList e1
      m2 = fromList e2
      ref1 = Map.fromList e1
      ref2 = Map.fromList e2
   in sortToList (difference m1 m2) === Map.toList (Map.difference ref1 ref2)

prop_difference_valid :: [(Word64, Int)] -> [(Word64, Int)] -> Property
prop_difference_valid e1 e2 = checkValid (difference (fromList e1) (fromList e2))

prop_intersection_model :: [(Word64, Int)] -> [(Word64, Int)] -> Property
prop_intersection_model e1 e2 =
  let m1 = fromList e1
      m2 = fromList e2
      ref1 = Map.fromList e1
      ref2 = Map.fromList e2
   in sortToList (intersection m1 m2) === Map.toList (Map.intersection ref1 ref2)

prop_intersection_valid :: [(Word64, Int)] -> [(Word64, Int)] -> Property
prop_intersection_valid e1 e2 = checkValid (intersection (fromList e1) (fromList e2))

prop_filter_model :: [(Word64, Int)] -> Property
prop_filter_model entries =
  let f x = even x
      m = fromList entries
      ref = Map.fromList entries
   in sortToList (filter f m) === Map.toList (Map.filter f ref)

prop_filter_valid :: [(Word64, Int)] -> Property
prop_filter_valid entries = checkValid (filter even (fromList entries))

prop_partition_model :: [(Word64, Int)] -> Property
prop_partition_model entries =
  let f x = even x
      m = fromList entries
      ref = Map.fromList entries
      (l, r) = partition f m
      (lRef, rRef) = Map.partition f ref
   in (sortToList l, sortToList r) === (Map.toList lRef, Map.toList rRef)

prop_partition_valid :: [(Word64, Int)] -> Property
prop_partition_valid entries =
  let (l, r) = partition even (fromList entries)
   in checkValid l .&&. checkValid r

prop_mapMaybe_model :: [(Word64, Int)] -> Property
prop_mapMaybe_model entries =
  let f x = if even x then Just (x `div` 2) else Nothing
      m = fromList entries
      ref = Map.fromList entries
   in sortToList (mapMaybe f m) === Map.toList (Map.mapMaybe f ref)

prop_mapMaybe_valid :: [(Word64, Int)] -> Property
prop_mapMaybe_valid entries =
  let f x = if even x then Just (x `div` 2) else Nothing
   in checkValid (mapMaybe f (fromList entries))

prop_mapEither_model :: [(Word64, Int)] -> Property
prop_mapEither_model entries =
  let f x = if even x then Left (x `div` 2) else Right (x * 2)
      m = fromList entries
      ref = Map.fromList entries
      (l, r) = mapEither f m
      (lRef, rRef) = Map.mapEither f ref
   in (sortToList l, sortToList r) === (Map.toList lRef, Map.toList rRef)

prop_mapEither_valid :: [(Word64, Int)] -> Property
prop_mapEither_valid entries =
  let f x = if even x then Left (x `div` 2) else Right (x * 2)
      (l, r) = mapEither f (fromList entries)
   in checkValid l .&&. checkValid r

prop_isSubmapOf_model :: [(Word64, Int)] -> [(Word64, Int)] -> Property
prop_isSubmapOf_model e1 e2 =
  let m1 = fromList e1
      m2 = fromList e2
      ref1 = Map.fromList e1
      ref2 = Map.fromList e2
   in isSubmapOf m1 m2 === Map.isSubmapOf ref1 ref2

prop_mergeWithKey_model :: [(Word64, Int)] -> [(Word64, Int)] -> Property
prop_mergeWithKey_model e1 e2 =
  let f k x y = if even (k + fromIntegral x + fromIntegral y) then Just (x + y) else Nothing
      g1 = filter (const True) -- id-like but structural
      g2 = const empty
      m1 = fromList e1
      m2 = fromList e2
      ref1 = Map.fromList e1
      ref2 = Map.fromList e2
   in sortToList (mergeWithKey f g1 g2 m1 m2)
        === Map.toList (Map.mergeWithKey f (Map.map id) (const Map.empty) ref1 ref2)

prop_insertWithKey_model :: [(Word64, Int)] -> Word64 -> Int -> Property
prop_insertWithKey_model entries k v =
  let f k' new old = fromIntegral k' + new + old
      m = fromList entries
      ref = Map.fromList entries
   in sortToList (insertWithKey f k v m) === Map.toList (Map.insertWithKey f k v ref)

prop_insertWithKey_valid :: [(Word64, Int)] -> Word64 -> Int -> Property
prop_insertWithKey_valid entries k v =
  let f k' new old = fromIntegral k' + new + old
   in checkValid (insertWithKey f k v (fromList entries))

prop_adjustWithKey_model :: [(Word64, Int)] -> Word64 -> Property
prop_adjustWithKey_model entries k =
  let f k' x = fromIntegral k' + x
      m = fromList entries
      ref = Map.fromList entries
   in sortToList (adjustWithKey f k m) === Map.toList (Map.adjustWithKey f k ref)

prop_adjustWithKey_valid :: [(Word64, Int)] -> Word64 -> Property
prop_adjustWithKey_valid entries k =
  let f k' x = fromIntegral k' + x
   in checkValid (adjustWithKey f k (fromList entries))

prop_updateWithKey_model :: [(Word64, Int)] -> Word64 -> Property
prop_updateWithKey_model entries k =
  let f k' x = if even (k' + fromIntegral x) then Just (x + 1) else Nothing
      m = fromList entries
      ref = Map.fromList entries
   in sortToList (updateWithKey f k m) === Map.toList (Map.updateWithKey f k ref)

prop_updateWithKey_valid :: [(Word64, Int)] -> Word64 -> Property
prop_updateWithKey_valid entries k =
  let f k' x = if even (k' + fromIntegral x) then Just (x + 1) else Nothing
   in checkValid (updateWithKey f k (fromList entries))

prop_map_valid :: [(Word64, Int)] -> Property
prop_map_valid entries = checkValid (map (+ 1) (fromList entries))

prop_mapWithKey_valid :: [(Word64, Int)] -> Property
prop_mapWithKey_valid entries = checkValid (mapWithKey (\k v -> fromIntegral k + v) (fromList entries))

prop_unionWithKey_model :: [(Word64, Int)] -> [(Word64, Int)] -> Property
prop_unionWithKey_model e1 e2 =
  let f k x y = fromIntegral k + x + y
      m1 = fromList e1
      m2 = fromList e2
      ref1 = Map.fromList e1
      ref2 = Map.fromList e2
   in sortToList (unionWithKey f m1 m2) === Map.toList (Map.unionWithKey f ref1 ref2)

prop_unionWithKey_valid :: [(Word64, Int)] -> [(Word64, Int)] -> Property
prop_unionWithKey_valid e1 e2 =
  let f k x y = fromIntegral k + x + y
   in checkValid (unionWithKey f (fromList e1) (fromList e2))

prop_mergeWithKey_valid :: [(Word64, Int)] -> [(Word64, Int)] -> Property
prop_mergeWithKey_valid e1 e2 =
  let f k x y = if even (k + fromIntegral x + fromIntegral y) then Just (x + y) else Nothing
      g1 = filter (const True)
      g2 = const empty
   in checkValid (mergeWithKey f g1 g2 (fromList e1) (fromList e2))

prop_differenceWith_model :: [(Word64, Int)] -> [(Word64, Int)] -> Property
prop_differenceWith_model e1 e2 =
  let f x y = if even (x + y) then Just (x + y) else Nothing
      m1 = fromList e1
      m2 = fromList e2
      ref1 = Map.fromList e1
      ref2 = Map.fromList e2
   in sortToList (differenceWith f m1 m2)
        === Map.toList (Map.differenceWith f ref1 ref2)

prop_differenceWith_valid :: [(Word64, Int)] -> [(Word64, Int)] -> Property
prop_differenceWith_valid e1 e2 =
  let f x y = if even (x + y) then Just (x + y) else Nothing
   in checkValid (differenceWith f (fromList e1) (fromList e2))

prop_intersectionWith_model :: [(Word64, Int)] -> [(Word64, Int)] -> Property
prop_intersectionWith_model e1 e2 =
  let f x y = x + y
      m1 = fromList e1
      m2 = fromList e2
      ref1 = Map.fromList e1
      ref2 = Map.fromList e2
   in sortToList (intersectionWith f m1 m2)
        === Map.toList (Map.intersectionWith f ref1 ref2)

prop_intersectionWith_valid :: [(Word64, Int)] -> [(Word64, Int)] -> Property
prop_intersectionWith_valid e1 e2 =
  let f x y = x + y
   in checkValid (intersectionWith f (fromList e1) (fromList e2))

prop_intersectionWithKey_model :: [(Word64, Int)] -> [(Word64, Int)] -> Property
prop_intersectionWithKey_model e1 e2 =
  let f k x y = fromIntegral k + x + y
      m1 = fromList e1
      m2 = fromList e2
      ref1 = Map.fromList e1
      ref2 = Map.fromList e2
   in sortToList (intersectionWithKey f m1 m2)
        === Map.toList (Map.intersectionWithKey f ref1 ref2)

prop_intersectionWithKey_valid :: [(Word64, Int)] -> [(Word64, Int)] -> Property
prop_intersectionWithKey_valid e1 e2 =
  let f k x y = fromIntegral k + x + y
   in checkValid (intersectionWithKey f (fromList e1) (fromList e2))

prop_filterWithKey_model :: [(Word64, Int)] -> Property
prop_filterWithKey_model entries =
  let f k v = even (k + fromIntegral v)
      m = fromList entries
      ref = Map.fromList entries
   in sortToList (filterWithKey f m) === Map.toList (Map.filterWithKey f ref)

prop_filterWithKey_valid :: [(Word64, Int)] -> Property
prop_filterWithKey_valid entries =
  let f k v = even (k + fromIntegral v)
   in checkValid (filterWithKey f (fromList entries))

prop_partitionWithKey_model :: [(Word64, Int)] -> Property
prop_partitionWithKey_model entries =
  let f k v = even (k + fromIntegral v)
      m = fromList entries
      ref = Map.fromList entries
      (l, r) = partitionWithKey f m
      (lRef, rRef) = Map.partitionWithKey f ref
   in (sortToList l, sortToList r) === (Map.toList lRef, Map.toList rRef)

prop_partitionWithKey_valid :: [(Word64, Int)] -> Property
prop_partitionWithKey_valid entries =
  let f k v = even (k + fromIntegral v)
      (l, r) = partitionWithKey f (fromList entries)
   in checkValid l .&&. checkValid r

prop_mapMaybeWithKey_model :: [(Word64, Int)] -> Property
prop_mapMaybeWithKey_model entries =
  let f k v = if even (k + fromIntegral v) then Just (v + 1) else Nothing
      m = fromList entries
      ref = Map.fromList entries
   in sortToList (mapMaybeWithKey f m) === Map.toList (Map.mapMaybeWithKey f ref)

prop_mapMaybeWithKey_valid :: [(Word64, Int)] -> Property
prop_mapMaybeWithKey_valid entries =
  let f k v = if even (k + fromIntegral v) then Just (v + 1) else Nothing
   in checkValid (mapMaybeWithKey f (fromList entries))

prop_mapEitherWithKey_model :: [(Word64, Int)] -> Property
prop_mapEitherWithKey_model entries =
  let f k v = if even (k + fromIntegral v) then Left (v + 1) else Right (v + 2)
      m = fromList entries
      ref = Map.fromList entries
      (l, r) = mapEitherWithKey f m
      (lRef, rRef) = Map.mapEitherWithKey f ref
   in (sortToList l, sortToList r) === (Map.toList lRef, Map.toList rRef)

prop_mapEitherWithKey_valid :: [(Word64, Int)] -> Property
prop_mapEitherWithKey_valid entries =
  let f k v = if even (k + fromIntegral v) then Left (v + 1) else Right (v + 2)
      (l, r) = mapEitherWithKey f (fromList entries)
   in checkValid l .&&. checkValid r

prop_isSubmapOfBy_model :: [(Word64, Int)] -> [(Word64, Int)] -> Property
prop_isSubmapOfBy_model e1 e2 =
  let f x y = x <= y
      m1 = fromList e1
      m2 = fromList e2
      ref1 = Map.fromList e1
      ref2 = Map.fromList e2
   in isSubmapOfBy f m1 m2 === Map.isSubmapOfBy f ref1 ref2
