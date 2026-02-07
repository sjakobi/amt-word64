{-# LANGUAGE ScopedTypeVariables #-}

module Word64SetTests
  ( tests
  , instanceTests
  ) where

import Amt.Word64.Set.Lazy
  ( Word64Set
  , alterF
  , delete
  , difference
  , disjoint
  , elems
  , empty
  , filter
  , foldl'
  , foldr
  , fromList
  , insert
  , intersection
  , isProperSubsetOf
  , isSubsetOf
  , map
  , mapMonotonic
  , member
  , notMember
  , null
  , partition
  , singleton
  , size
  , toList
  , union
  , unions
  , valid
  )
import Data.Data (cast, dataTypeConstrs, dataTypeOf, gmapQi, toConstr)
import Data.Functor.Identity (Identity (Identity))
import Data.List qualified as L
import Data.Maybe (listToMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Set qualified as Set
import Data.Word (Word64)
import Test.QuickCheck.Classes.Base
  ( Laws (Laws)
  , eqLaws
  , isListLaws
  , monoidLaws
  , ordLaws
  , semigroupLaws
  , showLaws
  , showReadLaws
  )
import Test.Tasty
import Test.Tasty.QuickCheck
import Prelude hiding (filter, foldl', foldr, map, null)

newtype K = K Word64
  deriving (Eq, Ord, Show, Num, Integral, Real, Enum)

instance Arbitrary K where
  arbitrary = K . getLarge <$> arbitrary
  shrink (K w) = K <$> shrink w

instance Arbitrary Word64Set where
  arbitrary = fromKList <$> arbitrary
  shrink s = fromKList <$> shrink (toSortedKList s)

toWord64 :: K -> Word64
toWord64 (K w) = w

fromWord64 :: Word64 -> K
fromWord64 = K

fromKList :: [K] -> Word64Set
fromKList = fromList . L.map toWord64

toSortedList :: Word64Set -> [Word64]
toSortedList = L.sort . toList

toSortedKList :: Word64Set -> [K]
toSortedKList = L.sort . L.map fromWord64 . toList

tests :: TestTree
tests =
  testGroup
    "Word64Set tests"
    [ testGroup
        "empty"
        [ testProperty "valid invariant" $ checkValid empty
        ]
    , testGroup
        "singleton"
        [ testProperty "matches Data.Set" prop_singleton_model
        , testProperty "valid invariant" prop_singleton_valid
        ]
    , testGroup
        "insert"
        [ testProperty "matches Data.Set" prop_insert_model
        , testProperty "valid invariant" prop_insert_valid
        ]
    , testGroup
        "delete"
        [ testProperty "matches Data.Set" prop_delete_model
        , testProperty "valid invariant" prop_delete_valid
        ]
    , testGroup
        "alterF"
        [ testProperty "matches insert/delete" prop_alterF_model
        ]
    , testGroup
        "union"
        [ testProperty "matches Data.Set" prop_union_model
        , testProperty "valid invariant" prop_union_valid
        ]
    , testGroup
        "unions"
        [ testProperty "matches Data.Set" prop_unions_model
        , testProperty "valid invariant" prop_unions_valid
        ]
    , testGroup
        "difference"
        [ testProperty "matches Data.Set" prop_difference_model
        , testProperty "valid invariant" prop_difference_valid
        ]
    , testGroup
        "intersection"
        [ testProperty "matches Data.Set" prop_intersection_model
        , testProperty "valid invariant" prop_intersection_valid
        ]
    , testGroup
        "filter"
        [ testProperty "matches Data.Set" prop_filter_model
        , testProperty "valid invariant" prop_filter_valid
        ]
    , testGroup
        "partition"
        [ testProperty "matches Data.Set" prop_partition_model
        , testProperty "valid invariant" prop_partition_valid
        ]
    , testGroup
        "map"
        [ testProperty "matches Data.Set" prop_map_model
        , testProperty "valid invariant" prop_map_valid
        ]
    , testGroup
        "mapMonotonic"
        [ testProperty "matches Data.Set" prop_mapMonotonic_model
        , testProperty "valid invariant" prop_mapMonotonic_valid
        ]
    , testGroup
        "fromList"
        [ testProperty "matches Data.Set" prop_fromList_model
        , testProperty "valid invariant" prop_fromList_valid
        ]
    , testGroup
        "toList"
        [ testProperty "matches Data.Set" prop_toList_model
        ]
    , testGroup
        "elems"
        [ testProperty "matches toList" prop_elems_model
        ]
    , testGroup
        "null"
        [ testProperty "matches Data.Set" prop_null_model
        ]
    , testGroup
        "size"
        [ testProperty "matches Data.Set" prop_size_model
        ]
    , testGroup
        "member"
        [ testProperty "matches Data.Set" prop_member_model
        ]
    , testGroup
        "notMember"
        [ testProperty "matches Data.Set" prop_notMember_model
        ]
    , testGroup
        "isSubsetOf"
        [ testProperty "matches Data.Set" prop_isSubsetOf_model
        ]
    , testGroup
        "isProperSubsetOf"
        [ testProperty "matches Data.Set" prop_isProperSubsetOf_model
        ]
    , testGroup
        "disjoint"
        [ testProperty "matches Data.Set" prop_disjoint_model
        ]
    , testGroup
        "foldr"
        [ testProperty "visits all elements" prop_foldr_model
        ]
    , testGroup
        "foldl'"
        [ testProperty "visits all elements" prop_foldl_model
        ]
    ]

instanceTests :: TestTree
instanceTests =
  testGroup
    "instance laws"
    [ lawsToTestTree (eqLaws (Proxy :: Proxy Word64Set))
    , lawsToTestTree (ordLaws (Proxy :: Proxy Word64Set))
    , lawsToTestTree (showLaws (Proxy :: Proxy Word64Set))
    , lawsToTestTree (showReadLaws (Proxy :: Proxy Word64Set))
    , lawsToTestTree (semigroupLaws (Proxy :: Proxy Word64Set))
    , lawsToTestTree (monoidLaws (Proxy :: Proxy Word64Set))
    , lawsToTestTree (isListLaws (Proxy :: Proxy Word64Set))
    , testGroup
        "Data"
        [ testProperty "gmapQi roundtrip" prop_data_gmapQi
        , testProperty "toConstr is fromList" prop_data_toConstr
        ]
    ]

lawsToTestTree :: Laws -> TestTree
lawsToTestTree (Laws name props) =
  testGroup name $
    [ testProperty propName prop
    | (propName, prop) <- props
    ]

checkValid :: Word64Set -> Property
checkValid s = case valid s of
  Nothing -> property True
  Just err -> counterexample (show err) False

prop_data_gmapQi :: [K] -> Property
prop_data_gmapQi xs =
  let s = fromKList xs
   in property $ gmapQi 0 cast s == Just (toList s)

prop_data_toConstr :: [K] -> Property
prop_data_toConstr xs =
  let s = fromKList xs
      dt = dataTypeOf s
   in property $ Just (toConstr s) == listToMaybe (dataTypeConstrs dt)

prop_singleton_model :: K -> Property
prop_singleton_model k =
  toSortedList (singleton (toWord64 k))
    === Set.toAscList (Set.singleton (toWord64 k))

prop_singleton_valid :: K -> Property
prop_singleton_valid k = checkValid (singleton (toWord64 k))

prop_insert_model :: [K] -> K -> Property
prop_insert_model entries k =
  let s = fromKList entries
      ref = Set.fromList (L.map toWord64 entries)
   in toSortedList (insert (toWord64 k) s)
        === Set.toAscList (Set.insert (toWord64 k) ref)

prop_insert_valid :: [K] -> K -> Property
prop_insert_valid entries k =
  checkValid (insert (toWord64 k) (fromKList entries))

prop_delete_model :: [K] -> [K] -> Property
prop_delete_model entries ks =
  let s = L.foldl' (\acc k -> delete (toWord64 k) acc) (fromKList entries) ks
      ref =
        L.foldl'
          (\acc k -> Set.delete (toWord64 k) acc)
          (Set.fromList (L.map toWord64 entries))
          ks
   in toSortedList s === Set.toAscList ref

prop_delete_valid :: [K] -> [K] -> Property
prop_delete_valid entries ks =
  checkValid
    (L.foldl' (\acc k -> delete (toWord64 k) acc) (fromKList entries) ks)

prop_alterF_model :: [K] -> K -> Bool -> Property
prop_alterF_model entries k b =
  let s = fromKList entries
      s' = case alterF (\_ -> Identity b) (toWord64 k) s of
        Identity result -> result
      ref =
        if b
          then insert (toWord64 k) s
          else delete (toWord64 k) s
   in s' === ref

prop_union_model :: [K] -> [K] -> Property
prop_union_model e1 e2 =
  let s1 = fromKList e1
      s2 = fromKList e2
      ref1 = Set.fromList (L.map toWord64 e1)
      ref2 = Set.fromList (L.map toWord64 e2)
   in toSortedList (union s1 s2) === Set.toAscList (Set.union ref1 ref2)

prop_union_valid :: [K] -> [K] -> Property
prop_union_valid e1 e2 =
  checkValid (union (fromKList e1) (fromKList e2))

prop_unions_model :: [[K]] -> Property
prop_unions_model groups =
  let sets = L.map fromKList groups
      refs = L.map (Set.fromList . L.map toWord64) groups
   in toSortedList (unions sets) === Set.toAscList (Set.unions refs)

prop_unions_valid :: [[K]] -> Property
prop_unions_valid groups = checkValid (unions (L.map fromKList groups))

prop_difference_model :: [K] -> [K] -> Property
prop_difference_model e1 e2 =
  let s1 = fromKList e1
      s2 = fromKList e2
      ref1 = Set.fromList (L.map toWord64 e1)
      ref2 = Set.fromList (L.map toWord64 e2)
   in toSortedList (difference s1 s2)
        === Set.toAscList (Set.difference ref1 ref2)

prop_difference_valid :: [K] -> [K] -> Property
prop_difference_valid e1 e2 =
  checkValid (difference (fromKList e1) (fromKList e2))

prop_intersection_model :: [K] -> [K] -> Property
prop_intersection_model e1 e2 =
  let s1 = fromKList e1
      s2 = fromKList e2
      ref1 = Set.fromList (L.map toWord64 e1)
      ref2 = Set.fromList (L.map toWord64 e2)
   in toSortedList (intersection s1 s2)
        === Set.toAscList (Set.intersection ref1 ref2)

prop_intersection_valid :: [K] -> [K] -> Property
prop_intersection_valid e1 e2 =
  checkValid (intersection (fromKList e1) (fromKList e2))

prop_filter_model :: [K] -> Property
prop_filter_model entries =
  let s = fromKList entries
      ref = Set.fromList (L.map toWord64 entries)
   in toSortedList (filter even s) === Set.toAscList (Set.filter even ref)

prop_filter_valid :: [K] -> Property
prop_filter_valid entries = checkValid (filter even (fromKList entries))

prop_partition_model :: [K] -> Property
prop_partition_model entries =
  let s = fromKList entries
      ref = Set.fromList (L.map toWord64 entries)
      (l, r) = partition even s
      (lRef, rRef) = Set.partition even ref
   in (toSortedList l, toSortedList r)
        === (Set.toAscList lRef, Set.toAscList rRef)

prop_partition_valid :: [K] -> Property
prop_partition_valid entries =
  let (l, r) = partition even (fromKList entries)
   in checkValid l .&&. checkValid r

prop_map_model :: [K] -> Property
prop_map_model entries =
  let f w = w * 2 + 1
      s = fromKList entries
      ref = Set.fromList (L.map toWord64 entries)
   in toSortedList (map f s) === Set.toAscList (Set.map f ref)

prop_map_valid :: [K] -> Property
prop_map_valid entries =
  let f w = w * 2 + 1
   in checkValid (map f (fromKList entries))

prop_mapMonotonic_model :: [K] -> Property
prop_mapMonotonic_model entries =
  let f w = w + 1
      s = fromKList entries
      ref = Set.fromList (L.map toWord64 entries)
   in toSortedList (mapMonotonic f s) === Set.toAscList (Set.map f ref)

prop_mapMonotonic_valid :: [K] -> Property
prop_mapMonotonic_valid entries =
  let f w = w + 1
   in checkValid (mapMonotonic f (fromKList entries))

prop_fromList_model :: [K] -> Property
prop_fromList_model entries =
  toSortedList (fromKList entries)
    === Set.toAscList (Set.fromList (L.map toWord64 entries))

prop_fromList_valid :: [K] -> Property
prop_fromList_valid entries = checkValid (fromKList entries)

prop_toList_model :: [K] -> Property
prop_toList_model entries =
  let s = fromKList entries
      ref = Set.fromList (L.map toWord64 entries)
   in toSortedList s === Set.toAscList ref

prop_elems_model :: [K] -> Property
prop_elems_model entries =
  let s = fromKList entries
   in L.sort (elems s) === L.sort (toList s)

prop_null_model :: [K] -> Property
prop_null_model entries =
  let s = fromKList entries
      ref = Set.fromList (L.map toWord64 entries)
   in null s === Set.null ref

prop_size_model :: [K] -> Property
prop_size_model entries =
  let s = fromKList entries
      ref = Set.fromList (L.map toWord64 entries)
   in size s === Set.size ref

prop_member_model :: [K] -> K -> Property
prop_member_model entries k =
  let s = fromKList entries
      ref = Set.fromList (L.map toWord64 entries)
   in member (toWord64 k) s === Set.member (toWord64 k) ref

prop_notMember_model :: [K] -> K -> Property
prop_notMember_model entries k =
  let s = fromKList entries
      ref = Set.fromList (L.map toWord64 entries)
   in notMember (toWord64 k) s === Set.notMember (toWord64 k) ref

prop_isSubsetOf_model :: [K] -> [K] -> Property
prop_isSubsetOf_model e1 e2 =
  let s1 = fromKList e1
      s2 = fromKList e2
      ref1 = Set.fromList (L.map toWord64 e1)
      ref2 = Set.fromList (L.map toWord64 e2)
   in isSubsetOf s1 s2 === Set.isSubsetOf ref1 ref2

prop_isProperSubsetOf_model :: [K] -> [K] -> Property
prop_isProperSubsetOf_model e1 e2 =
  let s1 = fromKList e1
      s2 = fromKList e2
      ref1 = Set.fromList (L.map toWord64 e1)
      ref2 = Set.fromList (L.map toWord64 e2)
   in isProperSubsetOf s1 s2 === Set.isProperSubsetOf ref1 ref2

prop_disjoint_model :: [K] -> [K] -> Property
prop_disjoint_model e1 e2 =
  let s1 = fromKList e1
      s2 = fromKList e2
      ref1 = Set.fromList (L.map toWord64 e1)
      ref2 = Set.fromList (L.map toWord64 e2)
      disjointRef = Set.null (Set.intersection ref1 ref2)
   in disjoint s1 s2 === disjointRef

prop_foldr_model :: [K] -> Property
prop_foldr_model entries =
  let s = fromKList entries
   in foldr (:) [] s === toList s

prop_foldl_model :: [K] -> Property
prop_foldl_model entries =
  let s = fromKList entries
   in foldl' (flip (:)) [] s === L.reverse (toList s)
