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

singleton :: Word64 -> a -> Word64Map a
singleton !k !v = I.singleton k v

insert :: Word64 -> a -> Word64Map a -> Word64Map a
insert !k !v m = I.insert k v m

insertWith :: (a -> a -> a) -> Word64 -> a -> Word64Map a -> Word64Map a
insertWith f k v m = I.insertWith f k v m

insertWithKey ::
  (Word64 -> a -> a -> a) -> Word64 -> a -> Word64Map a -> Word64Map a
insertWithKey f k v m = I.insertWithKey f k v m

adjust :: (a -> a) -> Word64 -> Word64Map a -> Word64Map a
adjust f k m =
  I.alter
    ( \mv -> case mv of
        Nothing -> Nothing
        Just v ->
          let y = f v
           in y `seq` Just y
    )
    k
    m

adjustWithKey :: (Word64 -> a -> a) -> Word64 -> Word64Map a -> Word64Map a
adjustWithKey f k m =
  I.alter
    ( \mv -> case mv of
        Nothing -> Nothing
        Just v ->
          let y = f k v
           in y `seq` Just y
    )
    k
    m

update :: (a -> Maybe a) -> Word64 -> Word64Map a -> Word64Map a
update f k m = I.update f k m

updateWithKey ::
  (Word64 -> a -> Maybe a) -> Word64 -> Word64Map a -> Word64Map a
updateWithKey f k m = I.updateWithKey f k m

alter :: (Maybe a -> Maybe a) -> Word64 -> Word64Map a -> Word64Map a
alter f k m = I.alter f k m

map :: (a -> b) -> Word64Map a -> Word64Map b
map = I.map

mapWithKey :: (Word64 -> a -> b) -> Word64Map a -> Word64Map b
mapWithKey = I.mapWithKey

unionWith :: (a -> a -> a) -> Word64Map a -> Word64Map a -> Word64Map a
unionWith = I.unionWith

unionWithKey ::
  (Word64 -> a -> a -> a) -> Word64Map a -> Word64Map a -> Word64Map a
unionWithKey = I.unionWithKey

mergeWithKey ::
  (Word64 -> a -> b -> Maybe c) ->
  (Word64Map a -> Word64Map c) ->
  (Word64Map b -> Word64Map c) ->
  Word64Map a ->
  Word64Map b ->
  Word64Map c
mergeWithKey = I.mergeWithKey

differenceWith ::
  (a -> b -> Maybe a) ->
  Word64Map a ->
  Word64Map b ->
  Word64Map a
differenceWith = I.differenceWith

intersectionWith :: (a -> b -> c) -> Word64Map a -> Word64Map b -> Word64Map c
intersectionWith = I.intersectionWith

intersectionWithKey ::
  (Word64 -> a -> b -> c) ->
  Word64Map a ->
  Word64Map b ->
  Word64Map c
intersectionWithKey = I.intersectionWithKey

mapMaybe :: (a -> Maybe b) -> Word64Map a -> Word64Map b
mapMaybe = I.mapMaybe

mapMaybeWithKey ::
  (Word64 -> a -> Maybe b) -> Word64Map a -> Word64Map b
mapMaybeWithKey = I.mapMaybeWithKey

mapEither ::
  (a -> Either b c) ->
  Word64Map a ->
  (Word64Map b, Word64Map c)
mapEither = I.mapEither

mapEitherWithKey ::
  (Word64 -> a -> Either b c) ->
  Word64Map a ->
  (Word64Map b, Word64Map c)
mapEitherWithKey = I.mapEitherWithKey

fromList :: [(Word64, a)] -> Word64Map a
fromList xs =
  let forceValue (k, v) = v `seq` (k, v)
   in I.fromList (fmap forceValue xs)
