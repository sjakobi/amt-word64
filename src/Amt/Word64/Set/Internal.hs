{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Amt.Word64.Set.Internal
  ( Word64Set (..)
  , empty
  , singleton
  , null
  , size
  , member
  , notMember
  , insert
  , delete
  , alterF
  , union
  , unions
  , difference
  , (\\)
  , intersection
  , filter
  , partition
  , map
  , mapMonotonic
  , foldr
  , foldl'
  , isSubsetOf
  , isProperSubsetOf
  , disjoint
  , fromList
  , toList
  , elems
  , valid
  , InvariantViolation (..)
  ) where

import Amt.Word64.Map.Internal
  ( InvariantViolation (..)
  , Word64Map
  )
import Amt.Word64.Map.Internal qualified as Map
import Control.DeepSeq (NFData (rnf))
import Data.Data
  ( Constr
  , Data (..)
  , DataType
  , Fixity (Prefix)
  , mkConstr
  , mkDataType
  )
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.Word (Word64)
import GHC.Exts qualified as Exts
import Text.Read (Lexeme (Ident), lexP, parens, readPrec)
import Prelude hiding (filter, foldl', foldr, map, null)

newtype Word64Set = Word64Set {unWord64Set :: Word64Map ()}

instance Show Word64Set where
  show s = "fromList " ++ show (toList s)

instance Eq Word64Set where
  Word64Set a == Word64Set b = a == b

instance Ord Word64Set where
  compare (Word64Set a) (Word64Set b) = compare a b

instance Read Word64Set where
  readPrec = parens $ do
    Ident "fromList" <- lexP
    xs <- readPrec
    pure (fromList xs)

instance Exts.IsList Word64Set where
  type Item Word64Set = Word64
  fromList = Amt.Word64.Set.Internal.fromList
  toList = Amt.Word64.Set.Internal.toList

instance Semigroup Word64Set where
  (<>) = union

instance Monoid Word64Set where
  mempty = empty
  mappend = (<>)

instance NFData Word64Set where
  rnf (Word64Set m) = rnf m

instance Data Word64Set where
  gfoldl k z s = z fromList `k` toList s
  gunfold k z c
    | c == fromListConstr = k (z fromList)
    | otherwise = error "gunfold: expected fromList"
  toConstr _ = fromListConstr
  dataTypeOf _ = word64SetDataType

word64SetDataType :: DataType
word64SetDataType =
  mkDataType "Amt.Word64.Set.Word64Set" [fromListConstr]

fromListConstr :: Constr
fromListConstr = mkConstr word64SetDataType "fromList" [] Prefix

empty :: Word64Set
empty = Word64Set Map.empty

singleton :: Word64 -> Word64Set
singleton !k = Word64Set (Map.singleton k ())

null :: Word64Set -> Bool
null (Word64Set m) = Map.null m

size :: Word64Set -> Int
size (Word64Set m) = Map.size m

member :: Word64 -> Word64Set -> Bool
member !k (Word64Set m) = Map.member k m

notMember :: Word64 -> Word64Set -> Bool
notMember !k (Word64Set m) = Map.notMember k m

insert :: Word64 -> Word64Set -> Word64Set
insert !k (Word64Set m) = Word64Set (Map.insert k () m)

delete :: Word64 -> Word64Set -> Word64Set
delete !k (Word64Set m) = Word64Set (Map.delete k m)

alterF :: Functor f => (Bool -> f Bool) -> Word64 -> Word64Set -> f Word64Set
alterF f !k s =
  fmap
    (\b -> if b then insert k s else delete k s)
    (f (member k s))

union :: Word64Set -> Word64Set -> Word64Set
union (Word64Set m1) (Word64Set m2) = Word64Set (Map.union m1 m2)

unions :: [Word64Set] -> Word64Set
unions = Foldable.foldl' union empty

infixl 9 \\

(\\) :: Word64Set -> Word64Set -> Word64Set
(\\) = difference

difference :: Word64Set -> Word64Set -> Word64Set
difference (Word64Set m1) (Word64Set m2) = Word64Set (Map.difference m1 m2)

intersection :: Word64Set -> Word64Set -> Word64Set
intersection (Word64Set m1) (Word64Set m2) = Word64Set (Map.intersection m1 m2)

filter :: (Word64 -> Bool) -> Word64Set -> Word64Set
filter p (Word64Set m) = Word64Set (Map.filterWithKey (\k _ -> p k) m)

partition :: (Word64 -> Bool) -> Word64Set -> (Word64Set, Word64Set)
partition p (Word64Set m) =
  let (m1, m2) = Map.partitionWithKey (\k _ -> p k) m
   in (Word64Set m1, Word64Set m2)

map :: (Word64 -> Word64) -> Word64Set -> Word64Set
map f = fromList . List.map f . toList

mapMonotonic :: (Word64 -> Word64) -> Word64Set -> Word64Set
mapMonotonic = map

foldr :: (Word64 -> b -> b) -> b -> Word64Set -> b
foldr f z (Word64Set m) = Map.foldrWithKey (\k _ acc -> f k acc) z m

foldl' :: (b -> Word64 -> b) -> b -> Word64Set -> b
foldl' f z (Word64Set m) = Map.foldlWithKey' (\acc k _ -> f acc k) z m

isSubsetOf :: Word64Set -> Word64Set -> Bool
isSubsetOf (Word64Set m1) (Word64Set m2) = Map.isSubmapOf m1 m2

isProperSubsetOf :: Word64Set -> Word64Set -> Bool
isProperSubsetOf s1 s2 = isSubsetOf s1 s2 && size s1 < size s2

disjoint :: Word64Set -> Word64Set -> Bool
disjoint s1 s2 = null (intersection s1 s2)

fromList :: [Word64] -> Word64Set
fromList = Word64Set . Map.fromList . List.map (\k -> (k, ()))

toList :: Word64Set -> [Word64]
toList (Word64Set m) = Map.keys m

elems :: Word64Set -> [Word64]
elems = toList

valid :: Word64Set -> Maybe InvariantViolation
valid (Word64Set m) = Map.valid m
