{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Amt.Word64.Map.Internal (Word64Map)
import Amt.Word64.Map.Internal qualified as MapInternal
import Amt.Word64.Map.Lazy qualified as Lazy
import Amt.Word64.Map.Strict qualified as Strict
import Control.Exception (SomeException, evaluate, try)
import Data.Bits qualified as Bits
import Data.Data (cast, dataTypeConstrs, dataTypeOf, gmapQi, toConstr)
import Data.Functor.Classes
  ( Eq1 (liftEq)
  , Ord1 (liftCompare)
  , Read1 (liftReadPrec)
  , Show1 (liftShowsPrec)
  )
import Data.List qualified as L
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Word (Word64)
import Test.QuickCheck.Classes.Base
  ( Laws (Laws)
  , eqLaws
  , foldableLaws
  , functorLaws
  , isListLaws
  , monoidLaws
  , ordLaws
  , semigroupLaws
  , showLaws
  , showReadLaws
  , traversableLaws
  )
import Test.Tasty
import Test.Tasty.QuickCheck
import Text.Read (readListPrec, readPrec, readPrec_to_S)
import Word64SetTests qualified as SetTests
import Prelude hiding (filter, lookup, map, null)

newtype K = K Word64
  deriving (Eq, Ord, Show, Num, Integral, Real, Enum)

data Box = Box Int

instance Arbitrary K where
  arbitrary = K . getLarge <$> arbitrary
  shrink (K w) = K <$> shrink w

instance Arbitrary a => Arbitrary (Word64Map a) where
  arbitrary = fromKListInternal <$> arbitrary
  shrink m = fromKListInternal <$> shrink (toSortedListInternal m)

toWord64 :: K -> Word64
toWord64 (K w) = w

fromWord64 :: Word64 -> K
fromWord64 = K

fromKListInternal :: [(K, a)] -> Word64Map a
fromKListInternal = MapInternal.fromList . L.map (\(k, v) -> (toWord64 k, v))

data MapOps = MapOps
  { empty :: forall a. Word64Map a
  , singleton :: forall a. Word64 -> a -> Word64Map a
  , null :: forall a. Word64Map a -> Bool
  , size :: forall a. Word64Map a -> Int
  , insert :: forall a. Word64 -> a -> Word64Map a -> Word64Map a
  , insertWith ::
      forall a.
      (a -> a -> a) ->
      Word64 ->
      a ->
      Word64Map a ->
      Word64Map a
  , insertWithKey ::
      forall a.
      (Word64 -> a -> a -> a) ->
      Word64 ->
      a ->
      Word64Map a ->
      Word64Map a
  , delete :: forall a. Word64 -> Word64Map a -> Word64Map a
  , adjust :: forall a. (a -> a) -> Word64 -> Word64Map a -> Word64Map a
  , adjustWithKey ::
      forall a.
      (Word64 -> a -> a) ->
      Word64 ->
      Word64Map a ->
      Word64Map a
  , update ::
      forall a.
      (a -> Maybe a) ->
      Word64 ->
      Word64Map a ->
      Word64Map a
  , updateWithKey ::
      forall a.
      (Word64 -> a -> Maybe a) ->
      Word64 ->
      Word64Map a ->
      Word64Map a
  , alter ::
      forall a.
      (Maybe a -> Maybe a) ->
      Word64 ->
      Word64Map a ->
      Word64Map a
  , lookup :: forall a. Word64 -> Word64Map a -> Maybe a
  , member :: forall a. Word64 -> Word64Map a -> Bool
  , notMember :: forall a. Word64 -> Word64Map a -> Bool
  , findWithDefault :: forall a. a -> Word64 -> Word64Map a -> a
  , map :: forall a b. (a -> b) -> Word64Map a -> Word64Map b
  , mapWithKey ::
      forall a b.
      (Word64 -> a -> b) ->
      Word64Map a ->
      Word64Map b
  , union :: forall a. Word64Map a -> Word64Map a -> Word64Map a
  , unionWith ::
      forall a.
      (a -> a -> a) ->
      Word64Map a ->
      Word64Map a ->
      Word64Map a
  , unionWithKey ::
      forall a.
      (Word64 -> a -> a -> a) ->
      Word64Map a ->
      Word64Map a ->
      Word64Map a
  , mergeWithKey ::
      forall a b c.
      (Word64 -> a -> b -> Maybe c) ->
      (Word64Map a -> Word64Map c) ->
      (Word64Map b -> Word64Map c) ->
      Word64Map a ->
      Word64Map b ->
      Word64Map c
  , difference :: forall a b. Word64Map a -> Word64Map b -> Word64Map a
  , differenceWith ::
      forall a b.
      (a -> b -> Maybe a) ->
      Word64Map a ->
      Word64Map b ->
      Word64Map a
  , intersection :: forall a b. Word64Map a -> Word64Map b -> Word64Map a
  , intersectionWith ::
      forall a b c.
      (a -> b -> c) ->
      Word64Map a ->
      Word64Map b ->
      Word64Map c
  , intersectionWithKey ::
      forall a b c.
      (Word64 -> a -> b -> c) ->
      Word64Map a ->
      Word64Map b ->
      Word64Map c
  , filter :: forall a. (a -> Bool) -> Word64Map a -> Word64Map a
  , filterWithKey ::
      forall a.
      (Word64 -> a -> Bool) ->
      Word64Map a ->
      Word64Map a
  , partition ::
      forall a.
      (a -> Bool) ->
      Word64Map a ->
      (Word64Map a, Word64Map a)
  , partitionWithKey ::
      forall a.
      (Word64 -> a -> Bool) ->
      Word64Map a ->
      (Word64Map a, Word64Map a)
  , mapMaybe ::
      forall a b.
      (a -> Maybe b) ->
      Word64Map a ->
      Word64Map b
  , mapMaybeWithKey ::
      forall a b.
      (Word64 -> a -> Maybe b) ->
      Word64Map a ->
      Word64Map b
  , mapEither ::
      forall a b c.
      (a -> Either b c) ->
      Word64Map a ->
      (Word64Map b, Word64Map c)
  , mapEitherWithKey ::
      forall a b c.
      (Word64 -> a -> Either b c) ->
      Word64Map a ->
      (Word64Map b, Word64Map c)
  , isSubmapOf :: forall a. Ord a => Word64Map a -> Word64Map a -> Bool
  , isSubmapOfBy ::
      forall a b.
      (a -> b -> Bool) ->
      Word64Map a ->
      Word64Map b ->
      Bool
  , fromList :: forall a. [(Word64, a)] -> Word64Map a
  , toList :: forall a. Word64Map a -> [(Word64, a)]
  , elems :: forall a. Word64Map a -> [a]
  , keys :: forall a. Word64Map a -> [Word64]
  , assocs :: forall a. Word64Map a -> [(Word64, a)]
  , foldrWithKey ::
      forall a b.
      (Word64 -> a -> b -> b) ->
      b ->
      Word64Map a ->
      b
  , foldlWithKey' ::
      forall a b.
      (b -> Word64 -> a -> b) ->
      b ->
      Word64Map a ->
      b
  }

lazyOps :: MapOps
lazyOps =
  MapOps
    { empty = Lazy.empty
    , singleton = Lazy.singleton
    , null = Lazy.null
    , size = Lazy.size
    , insert = Lazy.insert
    , insertWith = Lazy.insertWith
    , insertWithKey = Lazy.insertWithKey
    , delete = Lazy.delete
    , adjust = Lazy.adjust
    , adjustWithKey = Lazy.adjustWithKey
    , update = Lazy.update
    , updateWithKey = Lazy.updateWithKey
    , alter = Lazy.alter
    , lookup = Lazy.lookup
    , member = Lazy.member
    , notMember = Lazy.notMember
    , findWithDefault = Lazy.findWithDefault
    , map = Lazy.map
    , mapWithKey = Lazy.mapWithKey
    , union = Lazy.union
    , unionWith = Lazy.unionWith
    , unionWithKey = Lazy.unionWithKey
    , mergeWithKey = Lazy.mergeWithKey
    , difference = Lazy.difference
    , differenceWith = Lazy.differenceWith
    , intersection = Lazy.intersection
    , intersectionWith = Lazy.intersectionWith
    , intersectionWithKey = Lazy.intersectionWithKey
    , filter = Lazy.filter
    , filterWithKey = Lazy.filterWithKey
    , partition = Lazy.partition
    , partitionWithKey = Lazy.partitionWithKey
    , mapMaybe = Lazy.mapMaybe
    , mapMaybeWithKey = Lazy.mapMaybeWithKey
    , mapEither = Lazy.mapEither
    , mapEitherWithKey = Lazy.mapEitherWithKey
    , isSubmapOf = Lazy.isSubmapOf
    , isSubmapOfBy = Lazy.isSubmapOfBy
    , fromList = Lazy.fromList
    , toList = Lazy.toList
    , elems = Lazy.elems
    , keys = Lazy.keys
    , assocs = Lazy.assocs
    , foldrWithKey = Lazy.foldrWithKey
    , foldlWithKey' = Lazy.foldlWithKey'
    }

strictOps :: MapOps
strictOps =
  MapOps
    { empty = Strict.empty
    , singleton = Strict.singleton
    , null = Strict.null
    , size = Strict.size
    , insert = Strict.insert
    , insertWith = Strict.insertWith
    , insertWithKey = Strict.insertWithKey
    , delete = Strict.delete
    , adjust = Strict.adjust
    , adjustWithKey = Strict.adjustWithKey
    , update = Strict.update
    , updateWithKey = Strict.updateWithKey
    , alter = Strict.alter
    , lookup = Strict.lookup
    , member = Strict.member
    , notMember = Strict.notMember
    , findWithDefault = Strict.findWithDefault
    , map = Strict.map
    , mapWithKey = Strict.mapWithKey
    , union = Strict.union
    , unionWith = Strict.unionWith
    , unionWithKey = Strict.unionWithKey
    , mergeWithKey = Strict.mergeWithKey
    , difference = Strict.difference
    , differenceWith = Strict.differenceWith
    , intersection = Strict.intersection
    , intersectionWith = Strict.intersectionWith
    , intersectionWithKey = Strict.intersectionWithKey
    , filter = Strict.filter
    , filterWithKey = Strict.filterWithKey
    , partition = Strict.partition
    , partitionWithKey = Strict.partitionWithKey
    , mapMaybe = Strict.mapMaybe
    , mapMaybeWithKey = Strict.mapMaybeWithKey
    , mapEither = Strict.mapEither
    , mapEitherWithKey = Strict.mapEitherWithKey
    , isSubmapOf = Strict.isSubmapOf
    , isSubmapOfBy = Strict.isSubmapOfBy
    , fromList = Strict.fromList
    , toList = Strict.toList
    , elems = Strict.elems
    , keys = Strict.keys
    , assocs = Strict.assocs
    , foldrWithKey = Strict.foldrWithKey
    , foldlWithKey' = Strict.foldlWithKey'
    }

main :: IO ()
main =
  defaultMain $
    testGroup
      "amt-word64 tests"
      [ testGroup
          "Map"
          [ localOption (QuickCheckTests 1000) $
              localOption (QuickCheckMaxSize 500) word64MapTests
          , instanceTests
          ]
      , testGroup
          "Set"
          [ localOption (QuickCheckTests 1000) $
              localOption (QuickCheckMaxSize 500) SetTests.tests
          , SetTests.instanceTests
          ]
      ]

word64MapTests :: TestTree
word64MapTests =
  testGroup
    "Word64Map tests"
    [ testGroup "Lazy" (mapTests lazyOps)
    , testGroup "Strict" (mapTests strictOps <> strictnessTests)
    ]

mapTests :: MapOps -> [TestTree]
mapTests MapOps{..} =
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
  , testGroup
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
      "mergeWithKey"
      [ testProperty "matches Data.Map" prop_mergeWithKey_model
      , testProperty "matches unionWithKey" prop_mergeWithKey_unionWithKey_model
      , testProperty
          "matches differenceWithKey"
          prop_mergeWithKey_differenceWithKey_model
      , testProperty "valid invariant" prop_mergeWithKey_valid
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
  , testGroup
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
      , testProperty "self predicate" prop_isSubmapOfBy_self
      ]
  ]
 where
  fromKList :: [(K, a)] -> Word64Map a
  fromKList = fromList . L.map (\(k, v) -> (toWord64 k, v))

  toSortedList :: Word64Map a -> [(K, a)]
  toSortedList =
    L.sortOn fst . L.map (\(k, v) -> (fromWord64 k, v)) . toList

  prop_singleton_model :: K -> Int -> Property
  prop_singleton_model k v =
    toSortedList (singleton (toWord64 k) v) === Map.toList (Map.singleton k v)

  prop_singleton_valid :: K -> Int -> Property
  prop_singleton_valid k v = checkValid (singleton (toWord64 k) v)

  prop_insert_model :: [(K, Int)] -> K -> Int -> Property
  prop_insert_model entries k v =
    toSortedList
      ( insert
          (toWord64 k)
          v
          (fromKList entries)
      )
      === Map.toList (Map.insert k v (Map.fromList entries))

  prop_insert_valid :: [(K, Int)] -> K -> Int -> Property
  prop_insert_valid entries k v =
    checkValid
      ( insert
          (toWord64 k)
          v
          (fromKList entries)
      )

  prop_delete_model :: [(K, Int)] -> [K] -> Property
  prop_delete_model entries ks =
    let myMap =
          foldl
            (\m k -> delete (toWord64 k) m)
            (fromKList entries)
            ks
        refMap = foldl (\m k -> Map.delete k m) (Map.fromList entries) ks
     in toSortedList myMap === Map.toList refMap

  prop_delete_valid :: [(K, Int)] -> [K] -> Property
  prop_delete_valid entries ks =
    checkValid
      ( foldl
          (\m k -> delete (toWord64 k) m)
          (fromKList entries)
          ks
      )

  prop_union_model :: [(K, Int)] -> [(K, Int)] -> Property
  prop_union_model e1 e2 =
    let m1 = fromKList e1
        m2 = fromKList e2
        ref1 = Map.fromList e1
        ref2 = Map.fromList e2
        myUnion = union m1 m2
        refUnion = Map.union ref1 ref2
     in toSortedList myUnion === Map.toList refUnion

  prop_union_valid :: [(K, Int)] -> [(K, Int)] -> Property
  prop_union_valid e1 e2 =
    checkValid
      ( union
          (fromKList e1)
          (fromKList e2)
      )

  prop_fromList_model :: [(K, Int)] -> Property
  prop_fromList_model entries =
    toSortedList (fromKList entries)
      === Map.toList (Map.fromList entries)

  prop_fromList_valid :: [(K, Int)] -> Property
  prop_fromList_valid entries = checkValid (fromKList entries)

  prop_toList_model :: [(K, Int)] -> Property
  prop_toList_model entries =
    toSortedList (fromKList entries)
      === Map.toList (Map.fromList entries)

  prop_null_model :: [(K, Int)] -> Property
  prop_null_model entries =
    null (fromKList entries)
      === Map.null (Map.fromList entries)

  prop_size_model :: [(K, Int)] -> Property
  prop_size_model entries =
    size (fromKList entries)
      === Map.size (Map.fromList entries)

  prop_lookup_model :: [(K, Int)] -> K -> Property
  prop_lookup_model entries k =
    lookup (toWord64 k) (fromKList entries)
      === Map.lookup k (Map.fromList entries)

  prop_member_model :: [(K, Int)] -> K -> Property
  prop_member_model entries k =
    member (toWord64 k) (fromKList entries)
      === Map.member k (Map.fromList entries)

  prop_notMember_model :: [(K, Int)] -> K -> Property
  prop_notMember_model entries k =
    notMember
      (toWord64 k)
      (fromKList entries)
      === Map.notMember k (Map.fromList entries)

  prop_findWithDefault_model :: [(K, Int)] -> Int -> K -> Property
  prop_findWithDefault_model entries def k =
    findWithDefault
      def
      (toWord64 k)
      (fromKList entries)
      === Map.findWithDefault def k (Map.fromList entries)

  prop_keys_model :: [(K, Int)] -> Property
  prop_keys_model entries =
    let m = fromKList entries
     in L.sort (L.map fromWord64 (keys m))
          === L.sort (L.map fst (Map.toList (Map.fromList entries)))

  prop_elems_model :: [(K, Int)] -> Property
  prop_elems_model entries =
    let m = fromKList entries
     in L.sort (elems m) === L.sort (L.map snd (Map.toList (Map.fromList entries)))

  prop_assocs_model :: [(K, Int)] -> Property
  prop_assocs_model entries =
    let m = fromKList entries
     in L.sortOn fst (L.map (\(k, v) -> (fromWord64 k, v)) (assocs m))
          === Map.toList (Map.fromList entries)

  prop_fmap_model :: [(K, Int)] -> Property
  prop_fmap_model entries =
    let f = (+ 1)
        m = fromKList entries
        ref = Map.fromList entries
     in toSortedList (fmap f m) === Map.toList (Map.map f ref)

  prop_foldr_model :: [(K, Int)] -> Property
  prop_foldr_model entries =
    let m = fromKList entries
        f v acc = v : acc
     in L.sort (foldr f [] m) === L.sort (elems m)

  prop_length_model :: [(K, Int)] -> Property
  prop_length_model entries =
    let m = fromKList entries
     in length m === size m

  prop_foldrWithKey_model :: [(K, Int)] -> Property
  prop_foldrWithKey_model entries =
    let m = fromKList entries
        f k v acc = (fromWord64 k, v) : acc
     in L.sortOn fst (foldrWithKey f [] m) === Map.toList (Map.fromList entries)

  prop_foldlWithKey_model :: [(K, Int)] -> Property
  prop_foldlWithKey_model entries =
    let m = fromKList entries
        f acc k v = (fromWord64 k, v) : acc
     in L.sortOn fst (foldlWithKey' f [] m) === Map.toList (Map.fromList entries)

  prop_insertWith_model :: [(K, Int)] -> K -> Int -> Property
  prop_insertWith_model entries k v =
    let f x y = x + y
        m = fromKList entries
        ref = Map.fromList entries
     in toSortedList (insertWith f (toWord64 k) v m)
          === Map.toList (Map.insertWith f k v ref)

  prop_insertWith_valid :: [(K, Int)] -> K -> Int -> Property
  prop_insertWith_valid entries k v =
    let f x y = x + y
     in checkValid
          ( insertWith
              f
              (toWord64 k)
              v
              (fromKList entries)
          )

  prop_insertWithKey_model :: [(K, Int)] -> K -> Int -> Property
  prop_insertWithKey_model entries k v =
    let f k' new old = fromIntegral (toWord64 k') + new + old
        m = fromKList entries
        ref = Map.fromList entries
     in toSortedList
          (insertWithKey (\k' new old -> f (fromWord64 k') new old) (toWord64 k) v m)
          === Map.toList (Map.insertWithKey f k v ref)

  prop_insertWithKey_valid :: [(K, Int)] -> K -> Int -> Property
  prop_insertWithKey_valid entries k v =
    let f k' new old = fromIntegral (toWord64 k') + new + old
     in checkValid
          ( insertWithKey
              (\k' new old -> f (fromWord64 k') new old)
              (toWord64 k)
              v
              (fromKList entries)
          )

  prop_adjust_model :: [(K, Int)] -> K -> Property
  prop_adjust_model entries k =
    let f x = x + 1
        m = fromKList entries
        ref = Map.fromList entries
     in toSortedList (adjust f (toWord64 k) m) === Map.toList (Map.adjust f k ref)

  prop_adjust_valid :: [(K, Int)] -> K -> Property
  prop_adjust_valid entries k =
    let f x = x + 1
     in checkValid
          ( adjust
              f
              (toWord64 k)
              (fromKList entries)
          )

  prop_adjustWithKey_model :: [(K, Int)] -> K -> Property
  prop_adjustWithKey_model entries k =
    let f k' x = fromIntegral (toWord64 k') + x
        m = fromKList entries
        ref = Map.fromList entries
     in toSortedList (adjustWithKey (\k' x' -> f (fromWord64 k') x') (toWord64 k) m)
          === Map.toList (Map.adjustWithKey f k ref)

  prop_adjustWithKey_valid :: [(K, Int)] -> K -> Property
  prop_adjustWithKey_valid entries k =
    let f k' x = fromIntegral (toWord64 k') + x
     in checkValid
          ( adjustWithKey
              (\k' x' -> f (fromWord64 k') x')
              (toWord64 k)
              (fromKList entries)
          )

  prop_update_model :: [(K, Int)] -> K -> Property
  prop_update_model entries k =
    let f x = if even x then Just (x + 1) else Nothing
        m = fromKList entries
        ref = Map.fromList entries
     in toSortedList (update f (toWord64 k) m) === Map.toList (Map.update f k ref)

  prop_update_valid :: [(K, Int)] -> K -> Property
  prop_update_valid entries k =
    let f x = if even x then Just (x + 1) else Nothing
     in checkValid
          ( update
              f
              (toWord64 k)
              (fromKList entries)
          )

  prop_updateWithKey_model :: [(K, Int)] -> K -> Property
  prop_updateWithKey_model entries k =
    let f k' x = if even (toWord64 k' + fromIntegral x) then Just (x + 1) else Nothing
        m = fromKList entries
        ref = Map.fromList entries
     in toSortedList (updateWithKey (\k' x' -> f (fromWord64 k') x') (toWord64 k) m)
          === Map.toList (Map.updateWithKey f k ref)

  prop_updateWithKey_valid :: [(K, Int)] -> K -> Property
  prop_updateWithKey_valid entries k =
    let f k' x = if even (toWord64 k' + fromIntegral x) then Just (x + 1) else Nothing
     in checkValid
          ( updateWithKey
              (\k' x' -> f (fromWord64 k') x')
              (toWord64 k)
              (fromKList entries)
          )

  prop_alter_model :: [(K, Int)] -> K -> Property
  prop_alter_model entries k =
    let f Nothing = Just 1
        f (Just x) = if even x then Just (x + 1) else Nothing
        m = fromKList entries
        ref = Map.fromList entries
     in toSortedList (alter f (toWord64 k) m) === Map.toList (Map.alter f k ref)

  prop_alter_valid :: [(K, Int)] -> K -> Property
  prop_alter_valid entries k =
    let f Nothing = Just 1
        f (Just x) = if even x then Just (x + 1) else Nothing
     in checkValid
          (alter f (toWord64 k) (fromKList entries))

  prop_map_model :: [(K, Int)] -> Property
  prop_map_model entries =
    let f x = x + 1
        m = fromKList entries
        ref = Map.fromList entries
     in toSortedList (map f m) === Map.toList (Map.map f ref)

  prop_map_valid :: [(K, Int)] -> Property
  prop_map_valid entries =
    checkValid
      (map (+ 1) (fromKList entries))

  prop_mapWithKey_model :: [(K, Int)] -> Property
  prop_mapWithKey_model entries =
    let f k v = fromIntegral (toWord64 k) + v
        m = fromKList entries
        ref = Map.fromList entries
     in toSortedList (mapWithKey (\k' v' -> f (fromWord64 k') v') m)
          === Map.toList (Map.mapWithKey f ref)

  prop_mapWithKey_valid :: [(K, Int)] -> Property
  prop_mapWithKey_valid entries =
    checkValid
      ( mapWithKey
          (\k v -> fromIntegral k + v)
          (fromKList entries)
      )

  prop_unionWith_model :: [(K, Int)] -> [(K, Int)] -> Property
  prop_unionWith_model e1 e2 =
    let f x y = x + y
        m1 = fromKList e1
        m2 = fromKList e2
        ref1 = Map.fromList e1
        ref2 = Map.fromList e2
     in toSortedList (unionWith f m1 m2) === Map.toList (Map.unionWith f ref1 ref2)

  prop_unionWith_valid :: [(K, Int)] -> [(K, Int)] -> Property
  prop_unionWith_valid e1 e2 =
    let f x y = x + y
     in checkValid
          ( unionWith
              f
              (fromKList e1)
              (fromKList e2)
          )

  prop_unionWithKey_model :: [(K, Int)] -> [(K, Int)] -> Property
  prop_unionWithKey_model e1 e2 =
    let f k x y = fromIntegral (toWord64 k) + x + y
        m1 = fromKList e1
        m2 = fromKList e2
        ref1 = Map.fromList e1
        ref2 = Map.fromList e2
     in toSortedList (unionWithKey (\k' x' y' -> f (fromWord64 k') x' y') m1 m2)
          === Map.toList (Map.unionWithKey f ref1 ref2)

  prop_unionWithKey_valid :: [(K, Int)] -> [(K, Int)] -> Property
  prop_unionWithKey_valid e1 e2 =
    let f k x y = fromIntegral (toWord64 k) + x + y
     in checkValid
          ( unionWithKey
              (\k' x' y' -> f (fromWord64 k') x' y')
              (fromKList e1)
              (fromKList e2)
          )

  prop_difference_model :: [(K, Int)] -> [(K, Int)] -> Property
  prop_difference_model e1 e2 =
    let m1 = fromKList e1
        m2 = fromKList e2
        ref1 = Map.fromList e1
        ref2 = Map.fromList e2
     in toSortedList (difference m1 m2) === Map.toList (Map.difference ref1 ref2)

  prop_difference_valid :: [(K, Int)] -> [(K, Int)] -> Property
  prop_difference_valid e1 e2 =
    checkValid
      ( difference
          (fromKList e1)
          (fromKList e2)
      )

  prop_differenceWith_model :: [(K, Int)] -> [(K, Int)] -> Property
  prop_differenceWith_model e1 e2 =
    let f x y = if even (x + y) then Just (x + y) else Nothing
        m1 = fromKList e1
        m2 = fromKList e2
        ref1 = Map.fromList e1
        ref2 = Map.fromList e2
     in toSortedList (differenceWith f m1 m2)
          === Map.toList (Map.differenceWith f ref1 ref2)

  prop_differenceWith_valid :: [(K, Int)] -> [(K, Int)] -> Property
  prop_differenceWith_valid e1 e2 =
    let f x y = if even (x + y) then Just (x + y) else Nothing
     in checkValid
          ( differenceWith
              f
              (fromKList e1)
              (fromKList e2)
          )

  prop_intersection_model :: [(K, Int)] -> [(K, Int)] -> Property
  prop_intersection_model e1 e2 =
    let m1 = fromKList e1
        m2 = fromKList e2
        ref1 = Map.fromList e1
        ref2 = Map.fromList e2
     in toSortedList (intersection m1 m2) === Map.toList (Map.intersection ref1 ref2)

  prop_intersection_valid :: [(K, Int)] -> [(K, Int)] -> Property
  prop_intersection_valid e1 e2 =
    checkValid
      ( intersection
          (fromKList e1)
          (fromKList e2)
      )

  prop_intersectionWith_model :: [(K, Int)] -> [(K, Int)] -> Property
  prop_intersectionWith_model e1 e2 =
    let f x y = x + y
        m1 = fromKList e1
        m2 = fromKList e2
        ref1 = Map.fromList e1
        ref2 = Map.fromList e2
     in toSortedList (intersectionWith f m1 m2)
          === Map.toList (Map.intersectionWith f ref1 ref2)

  prop_intersectionWith_valid :: [(K, Int)] -> [(K, Int)] -> Property
  prop_intersectionWith_valid e1 e2 =
    let f x y = x + y
     in checkValid
          ( intersectionWith
              f
              (fromKList e1)
              (fromKList e2)
          )

  prop_intersectionWithKey_model :: [(K, Int)] -> [(K, Int)] -> Property
  prop_intersectionWithKey_model e1 e2 =
    let f k x y = fromIntegral (toWord64 k) + x + y
        m1 = fromKList e1
        m2 = fromKList e2
        ref1 = Map.fromList e1
        ref2 = Map.fromList e2
     in toSortedList (intersectionWithKey (\k' x' y' -> f (fromWord64 k') x' y') m1 m2)
          === Map.toList (Map.intersectionWithKey f ref1 ref2)

  prop_intersectionWithKey_valid :: [(K, Int)] -> [(K, Int)] -> Property
  prop_intersectionWithKey_valid e1 e2 =
    let f k x y = fromIntegral (toWord64 k) + x + y
     in checkValid
          ( intersectionWithKey
              (\k' x' y' -> f (fromWord64 k') x' y')
              (fromKList e1)
              (fromKList e2)
          )

  prop_filter_model :: [(K, Int)] -> Property
  prop_filter_model entries =
    let f x = even x
        m = fromKList entries
        ref = Map.fromList entries
     in toSortedList (filter f m) === Map.toList (Map.filter f ref)

  prop_filter_valid :: [(K, Int)] -> Property
  prop_filter_valid entries =
    checkValid
      (filter even (fromKList entries))

  prop_filterWithKey_model :: [(K, Int)] -> Property
  prop_filterWithKey_model entries =
    let f k v = even (toWord64 k + fromIntegral v)
        m = fromKList entries
        ref = Map.fromList entries
     in toSortedList (filterWithKey (\k' v' -> f (fromWord64 k') v') m)
          === Map.toList (Map.filterWithKey f ref)

  prop_filterWithKey_valid :: [(K, Int)] -> Property
  prop_filterWithKey_valid entries =
    let f k v = even (toWord64 k + fromIntegral v)
     in checkValid
          ( filterWithKey
              (\k' v' -> f (fromWord64 k') v')
              (fromKList entries)
          )

  prop_partition_model :: [(K, Int)] -> Property
  prop_partition_model entries =
    let f x = even x
        m = fromKList entries
        ref = Map.fromList entries
        (l, r) = partition f m
        (lRef, rRef) = Map.partition f ref
     in (toSortedList l, toSortedList r) === (Map.toList lRef, Map.toList rRef)

  prop_partition_valid :: [(K, Int)] -> Property
  prop_partition_valid entries =
    let (l, r) = partition even (fromKList entries)
     in checkValid l .&&. checkValid r

  prop_partitionWithKey_model :: [(K, Int)] -> Property
  prop_partitionWithKey_model entries =
    let f k v = even (toWord64 k + fromIntegral v)
        m = fromKList entries
        ref = Map.fromList entries
        (l, r) = partitionWithKey (\k' v' -> f (fromWord64 k') v') m
        (lRef, rRef) = Map.partitionWithKey f ref
     in (toSortedList l, toSortedList r) === (Map.toList lRef, Map.toList rRef)

  prop_partitionWithKey_valid :: [(K, Int)] -> Property
  prop_partitionWithKey_valid entries =
    let f k v = even (toWord64 k + fromIntegral v)
        (l, r) =
          partitionWithKey
            (\k' v' -> f (fromWord64 k') v')
            (fromKList entries)
     in checkValid l .&&. checkValid r

  prop_mapMaybe_model :: [(K, Int)] -> Property
  prop_mapMaybe_model entries =
    let f x = if even x then Just (x `div` 2) else Nothing
        m = fromKList entries
        ref = Map.fromList entries
     in toSortedList (mapMaybe f m) === Map.toList (Map.mapMaybe f ref)

  prop_mapMaybe_valid :: [(K, Int)] -> Property
  prop_mapMaybe_valid entries =
    let f x = if even x then Just (x `div` 2) else Nothing
     in checkValid
          (mapMaybe f (fromKList entries))

  prop_mapMaybeWithKey_model :: [(K, Int)] -> Property
  prop_mapMaybeWithKey_model entries =
    let f k v = if even (toWord64 k + fromIntegral v) then Just (v + 1) else Nothing
        m = fromKList entries
        ref = Map.fromList entries
     in toSortedList (mapMaybeWithKey (\k' v' -> f (fromWord64 k') v') m)
          === Map.toList (Map.mapMaybeWithKey f ref)

  prop_mapMaybeWithKey_valid :: [(K, Int)] -> Property
  prop_mapMaybeWithKey_valid entries =
    let f k v = if even (toWord64 k + fromIntegral v) then Just (v + 1) else Nothing
     in checkValid
          ( mapMaybeWithKey
              (\k' v' -> f (fromWord64 k') v')
              (fromKList entries)
          )

  prop_mapEither_model :: [(K, Int)] -> Property
  prop_mapEither_model entries =
    let f x = if even x then Left (x `div` 2) else Right (x * 2)
        m = fromKList entries
        ref = Map.fromList entries
        (l, r) = mapEither f m
        (lRef, rRef) = Map.mapEither f ref
     in (toSortedList l, toSortedList r) === (Map.toList lRef, Map.toList rRef)

  prop_mapEither_valid :: [(K, Int)] -> Property
  prop_mapEither_valid entries =
    let f x = if even x then Left (x `div` 2) else Right (x * 2)
        (l, r) = mapEither f (fromKList entries)
     in checkValid l .&&. checkValid r

  prop_mapEitherWithKey_model :: [(K, Int)] -> Property
  prop_mapEitherWithKey_model entries =
    let f k v = if even (toWord64 k + fromIntegral v) then Left (v + 1) else Right (v + 2)
        m = fromKList entries
        ref = Map.fromList entries
        (l, r) = mapEitherWithKey (\k' v' -> f (fromWord64 k') v') m
        (lRef, rRef) = Map.mapEitherWithKey f ref
     in (toSortedList l, toSortedList r) === (Map.toList lRef, Map.toList rRef)

  prop_mapEitherWithKey_valid :: [(K, Int)] -> Property
  prop_mapEitherWithKey_valid entries =
    let f k v = if even (toWord64 k + fromIntegral v) then Left (v + 1) else Right (v + 2)
        (l, r) =
          mapEitherWithKey
            (\k' v' -> f (fromWord64 k') v')
            (fromKList entries)
     in checkValid l .&&. checkValid r

  prop_isSubmapOf_model :: [(K, Int)] -> [(K, Int)] -> Property
  prop_isSubmapOf_model e1 e2 =
    let m1 = fromKList e1
        m2 = fromKList e2
        ref1 = Map.fromList e1
        ref2 = Map.fromList e2
     in isSubmapOf m1 m2 === Map.isSubmapOf ref1 ref2

  prop_isSubmapOfBy_model :: [(K, Int)] -> [(K, Int)] -> Property
  prop_isSubmapOfBy_model e1 e2 =
    let f x y = x <= y
        m1 = fromKList e1
        m2 = fromKList e2
        ref1 = Map.fromList e1
        ref2 = Map.fromList e2
     in isSubmapOfBy f m1 m2 === Map.isSubmapOfBy f ref1 ref2

  -- This fails when we accidentally introduce pointer-equality checking
  -- in isSubmapOfBy!
  prop_isSubmapOfBy_self :: [(K, Int)] -> Property
  prop_isSubmapOfBy_self entries =
    let f x _ = even x
        m = fromKList entries
        ref = Map.fromList entries
     in isSubmapOfBy f m m === Map.isSubmapOfBy f ref ref

  prop_mergeWithKey_model :: [(K, Int)] -> [(K, Int)] -> Property
  prop_mergeWithKey_model e1 e2 =
    let f k x y = if even (toWord64 k + fromIntegral (x + y)) then Just (x + y) else Nothing
        f_g1 k x = if even k then Just (x + 1) else Nothing
        f_g2 k x = if odd k then Just (x * 2) else Nothing
        g1 = mapMaybeWithKey f_g1
        g2 = mapMaybeWithKey f_g2
        refG1 = Map.mapMaybeWithKey (\k x -> f_g1 (toWord64 k) x)
        refG2 = Map.mapMaybeWithKey (\k x -> f_g2 (toWord64 k) x)
        m1 = fromKList e1
        m2 = fromKList e2
        ref1 = Map.fromList e1
        ref2 = Map.fromList e2
     in toSortedList (mergeWithKey (\k x y -> f (fromWord64 k) x y) g1 g2 m1 m2)
          === Map.toList (Map.mergeWithKey f refG1 refG2 ref1 ref2)

  prop_mergeWithKey_valid :: [(K, Int)] -> [(K, Int)] -> Property
  prop_mergeWithKey_valid e1 e2 =
    let f k x y = if even (toWord64 k + fromIntegral (x + y)) then Just (x + y) else Nothing
        f_g1 k x = if even k then Just (x + 1) else Nothing
        f_g2 k x = if odd k then Just (x * 2) else Nothing
        g1 = mapMaybeWithKey f_g1
        g2 = mapMaybeWithKey f_g2
        m1 = fromKList e1
        m2 = fromKList e2
     in checkValid (mergeWithKey (\k x y -> f (fromWord64 k) x y) g1 g2 m1 m2)

  prop_mergeWithKey_unionWithKey_model :: [(K, Int)] -> [(K, Int)] -> Property
  prop_mergeWithKey_unionWithKey_model e1 e2 =
    let fW k x y = x + y + fromIntegral (k Bits..&. 7)
        m1 = fromKList e1
        m2 = fromKList e2
     in mergeWithKey (\k x y -> Just (fW k x y)) id id m1 m2
          === unionWithKey (\k x y -> fW k x y) m1 m2

  prop_mergeWithKey_differenceWithKey_model ::
    [(K, Int)] -> [(K, Int)] -> Property
  prop_mergeWithKey_differenceWithKey_model e1 e2 =
    let f x y =
          if even (x + y)
            then Just (x - y)
            else Nothing
        m1 = fromKList e1
        m2 = fromKList e2
     in mergeWithKey (\_ x y -> f x y) id (const empty) m1 m2
          === differenceWith f m1 m2

strictnessTests :: [TestTree]
strictnessTests =
  [ testGroup
      "Strictness"
      [ testProperty "insert forces WHNF" prop_strict_insert_whnf
      ]
  ]

prop_strict_insert_whnf :: K -> Property
prop_strict_insert_whnf k = ioProperty $ do
  let !v1 = Box (error "strict insert should not force inner fields")
      !v2 = Box (error "strict insert should not force inner fields")
      m0 = Strict.singleton (toWord64 k) v1
      m1 = Strict.insert (toWord64 k) v2 m0
  result <-
    try
      ( evaluate
          ( Strict.foldlWithKey'
              (\acc _ v -> case v of Box _ -> acc)
              ()
              m1
          )
      )
  pure $ case result of
    Left (_ :: SomeException) -> property False
    Right _ -> property True

instanceTests :: TestTree
instanceTests =
  testGroup
    "instance laws"
    [ lawsToTestTree (eqLaws (Proxy :: Proxy (Word64Map Int)))
    , lawsToTestTree (ordLaws (Proxy :: Proxy (Word64Map Int)))
    , lawsToTestTree (showLaws (Proxy :: Proxy (Word64Map Int)))
    , lawsToTestTree (showReadLaws (Proxy :: Proxy (Word64Map Int)))
    , lawsToTestTree (semigroupLaws (Proxy :: Proxy (Word64Map Int)))
    , lawsToTestTree (monoidLaws (Proxy :: Proxy (Word64Map Int)))
    , lawsToTestTree (functorLaws (Proxy :: Proxy Word64Map))
    , lawsToTestTree (foldableLaws (Proxy :: Proxy Word64Map))
    , lawsToTestTree (traversableLaws (Proxy :: Proxy Word64Map))
    , lawsToTestTree (isListLaws (Proxy :: Proxy (Word64Map Int)))
    , testGroup
        "Data"
        [ testProperty "gmapQi roundtrip" prop_data_gmapQi
        , testProperty "toConstr is fromList" prop_data_toConstr
        ]
    , testGroup
        "Eq1/Ord1/Show1/Read1"
        [ testProperty "Eq1 liftEq matches Eq" prop_eq1_liftEq
        , testProperty "Ord1 liftCompare matches Ord" prop_ord1_liftCompare
        , testProperty "Show1 liftShowsPrec matches Show" prop_show1_matches_show
        , testProperty "Read1 liftReadPrec roundtrip" prop_read1_roundtrip
        ]
    ]

toSortedListInternal :: Word64Map a -> [(K, a)]
toSortedListInternal =
  L.sortOn fst
    . L.map (\(k, v) -> (fromWord64 k, v))
    . MapInternal.toList

lawsToTestTree :: Laws -> TestTree
lawsToTestTree (Laws name props) =
  testGroup name $
    [ testProperty propName prop
    | (propName, prop) <- props
    ]

checkValid :: Word64Map a -> Property
checkValid m = case MapInternal.valid m of
  Nothing -> property True
  Just err -> counterexample (show err) False

prop_data_gmapQi :: [(K, Int)] -> Property
prop_data_gmapQi xs =
  let m = fromKListInternal xs
   in property $ gmapQi 0 cast m == Just (MapInternal.toList m)

prop_data_toConstr :: [(K, Int)] -> Property
prop_data_toConstr xs =
  let m = fromKListInternal xs
      dt = dataTypeOf m
   in property $ Just (toConstr m) == listToMaybe (dataTypeConstrs dt)

prop_eq1_liftEq :: [(K, Int)] -> [(K, Int)] -> Property
prop_eq1_liftEq xs ys =
  let m1 = fromKListInternal xs
      m2 = fromKListInternal ys
   in property $ liftEq (==) m1 m2 == (m1 == m2)

prop_ord1_liftCompare :: [(K, Int)] -> [(K, Int)] -> Property
prop_ord1_liftCompare xs ys =
  let m1 = fromKListInternal xs
      m2 = fromKListInternal ys
   in property $ liftCompare compare m1 m2 == compare m1 m2

prop_show1_matches_show :: [(K, Int)] -> Property
prop_show1_matches_show xs =
  let m = fromKListInternal xs
      s1 = liftShowsPrec showsPrec showList 0 m ""
      s2 = showsPrec 0 m ""
   in property $ s1 == s2

prop_read1_roundtrip :: [(K, Int)] -> Property
prop_read1_roundtrip xs =
  let m = fromKListInternal xs
      s = showsPrec 0 m ""
      parses = readPrec_to_S (liftReadPrec readPrec readListPrec) 0 s
   in property $ any ((== m) . fst) parses
