{-# LANGUAGE BangPatterns #-}

{- | Strict API for 'Word64Map'.

Property: If all values stored in all maps in the arguments are in
WHNF, then all values stored in all maps in the results will be in
WHNF once those maps are evaluated.
-}
module Amt.Word64.Map.Strict
  ( Word64Map
  , empty
  , singleton
  , null
  , size
  , insert
  , insertWith
  , insertWithKey
  , delete
  , adjust
  , adjustWithKey
  , update
  , updateWithKey
  , alter
  , lookup
  , member
  , notMember
  , findWithDefault
  , map
  , mapWithKey
  , union
  , unionWith
  , unionWithKey
  , mergeWithKey
  , difference
  , differenceWith
  , intersection
  , intersectionWith
  , intersectionWithKey
  , filter
  , filterWithKey
  , partition
  , partitionWithKey
  , mapMaybe
  , mapMaybeWithKey
  , mapEither
  , mapEitherWithKey
  , isSubmapOf
  , isSubmapOfBy
  , fromList
  , toList
  , elems
  , keys
  , assocs
  , foldrWithKey
  , foldlWithKey'
  ) where

import Amt.Word64.Map.Internal
  ( Word64Map
  , assocs
  , delete
  , difference
  , elems
  , empty
  , filter
  , filterWithKey
  , findWithDefault
  , foldlWithKey'
  , foldrWithKey
  , intersection
  , isSubmapOf
  , isSubmapOfBy
  , keys
  , lookup
  , member
  , notMember
  , null
  , partition
  , partitionWithKey
  , size
  , toList
  , union
  )
import Amt.Word64.Map.Internal qualified as I
import Data.Word (Word64)
import Prelude hiding (filter, lookup, map, null)

force :: a -> a
force !x = x
{-# INLINE force #-}

forceMaybe :: Maybe a -> Maybe a
forceMaybe m = case m of
  Nothing -> Nothing
  Just x ->
    let !x' = x
     in Just x'
{-# INLINE forceMaybe #-}

forceEither :: Either a b -> Either a b
forceEither e = case e of
  Left x ->
    let !x' = x
     in Left x'
  Right y ->
    let !y' = y
     in Right y'
{-# INLINE forceEither #-}

forceMap :: Word64Map a -> Word64Map a
forceMap = I.map force
{-# INLINE forceMap #-}

singleton :: Word64 -> a -> Word64Map a
singleton !k !v = I.singleton k v

insert :: Word64 -> a -> Word64Map a -> Word64Map a
insert !k !v m = I.insert k v m

insertWith :: (a -> a -> a) -> Word64 -> a -> Word64Map a -> Word64Map a
insertWith f !k v m =
  I.insertWith (\new old -> force (f new old)) k (force v) m

insertWithKey ::
  (Word64 -> a -> a -> a) -> Word64 -> a -> Word64Map a -> Word64Map a
insertWithKey f !k v m =
  I.insertWithKey (\key new old -> force (f key new old)) k (force v) m

adjust :: (a -> a) -> Word64 -> Word64Map a -> Word64Map a
adjust f !k m = I.adjust (\x -> force (f x)) k m

adjustWithKey :: (Word64 -> a -> a) -> Word64 -> Word64Map a -> Word64Map a
adjustWithKey f !k m = I.adjustWithKey (\key x -> force (f key x)) k m

update :: (a -> Maybe a) -> Word64 -> Word64Map a -> Word64Map a
update f !k m = I.update (forceMaybe . f) k m

updateWithKey ::
  (Word64 -> a -> Maybe a) -> Word64 -> Word64Map a -> Word64Map a
updateWithKey f !k m = I.updateWithKey (\key x -> forceMaybe (f key x)) k m

alter :: (Maybe a -> Maybe a) -> Word64 -> Word64Map a -> Word64Map a
alter f !k m = I.alter (forceMaybe . f) k m

map :: (a -> b) -> Word64Map a -> Word64Map b
map f m = I.map (\x -> force (f x)) m

mapWithKey :: (Word64 -> a -> b) -> Word64Map a -> Word64Map b
mapWithKey f m = I.mapWithKey (\key x -> force (f key x)) m

unionWith :: (a -> a -> a) -> Word64Map a -> Word64Map a -> Word64Map a
unionWith f m1 m2 = I.unionWith (\x y -> force (f x y)) m1 m2

unionWithKey ::
  (Word64 -> a -> a -> a) -> Word64Map a -> Word64Map a -> Word64Map a
unionWithKey f m1 m2 =
  I.unionWithKey (\key x y -> force (f key x y)) m1 m2

mergeWithKey ::
  (Word64 -> a -> b -> Maybe c) ->
  (Word64Map a -> Word64Map c) ->
  (Word64Map b -> Word64Map c) ->
  Word64Map a ->
  Word64Map b ->
  Word64Map c
mergeWithKey f g1 g2 =
  I.mergeWithKey
    (\key x y -> forceMaybe (f key x y))
    (forceMap . g1)
    (forceMap . g2)

differenceWith ::
  (a -> b -> Maybe a) ->
  Word64Map a ->
  Word64Map b ->
  Word64Map a
differenceWith f m1 m2 = I.differenceWith (\x y -> forceMaybe (f x y)) m1 m2

intersectionWith :: (a -> b -> c) -> Word64Map a -> Word64Map b -> Word64Map c
intersectionWith f m1 m2 = I.intersectionWith (\x y -> force (f x y)) m1 m2

intersectionWithKey ::
  (Word64 -> a -> b -> c) ->
  Word64Map a ->
  Word64Map b ->
  Word64Map c
intersectionWithKey f m1 m2 =
  I.intersectionWithKey (\key x y -> force (f key x y)) m1 m2

mapMaybe :: (a -> Maybe b) -> Word64Map a -> Word64Map b
mapMaybe f m = I.mapMaybe (forceMaybe . f) m

mapMaybeWithKey ::
  (Word64 -> a -> Maybe b) -> Word64Map a -> Word64Map b
mapMaybeWithKey f m = I.mapMaybeWithKey (\key x -> forceMaybe (f key x)) m

mapEither ::
  (a -> Either b c) ->
  Word64Map a ->
  (Word64Map b, Word64Map c)
mapEither f m = I.mapEither (forceEither . f) m

mapEitherWithKey ::
  (Word64 -> a -> Either b c) ->
  Word64Map a ->
  (Word64Map b, Word64Map c)
mapEitherWithKey f m = I.mapEitherWithKey (\key x -> forceEither (f key x)) m

fromList :: [(Word64, a)] -> Word64Map a
fromList = forceMap . I.fromList
