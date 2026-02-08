{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module Amt.Word64.Map.Internal
  ( Word64Map (..)
  , Bitmap (..)
  , Index (..)
  , BitMatch (..)
  , Shift
  , ShiftBox (..)
  , empty
  , singleton
  , null
  , size
  , insert
  , insertWith
  , insertWithKey
  , insertIfNotExists
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
  , valid
  , InvariantViolation (..)
  ) where

import Control.DeepSeq (NFData (rnf), NFData1 (liftRnf), rnf1)
import Control.Monad.ST (ST, runST)
import Data.Bits hiding (bit, shift)
import Data.Bits qualified as Bits
import Data.Data
  ( Constr
  , Data (..)
  , DataType
  , Fixity (Prefix)
  , mkConstr
  , mkDataType
  )
import Data.Foldable qualified as Foldable
import Data.Functor.Classes
  ( Eq1 (liftEq)
  , Ord1 (liftCompare)
  , Read1 (liftReadPrec)
  , Show1 (liftShowsPrec)
  , showsUnaryWith
  )
import Data.Functor.Classes qualified as FunctorClasses
import Data.Primitive.SmallArray
import Data.Word (Word64)
import GHC.Exts
  ( Int (I#)
  , Int#
  , Word64#
  , eqWord64#
  , isTrue#
  , reallyUnsafePtrEquality
  , sameSmallArray#
  , (+#)
  , (>=#)
  )
import GHC.Exts qualified as Exts
import GHC.Word (Word64 (W64#))
import Text.Read (Lexeme (Ident), lexP, parens, readPrec)
import Prelude hiding (filter, lookup, map, null)

data InvariantViolation
  = PrefixMismatch
      { ivKey :: !Word64
      , ivShift :: !ShiftBox
      , ivPrefix :: !Word64
      }
  | BitmapCountMismatch
      { ivBitmap :: !Word64
      , ivArraySize :: !Int
      }
  | RedundantBranch
      { ivPrefix :: !Word64
      }
  | UnexpectedEmptyBranch
  deriving (Show, Eq)

{- | An array-mapped trie with 64-bit word keys.

=== Invariants

1. __Canonical empty__: The empty map is represented by a 'Branch' with an
   empty bitmap.

2. __No redundant branches__: If a 'Branch' has exactly one sub-node, this
   node must be a 'Branch' node too.

3. __Bitmap consistency__: The number of set bits in the 'Bitmap' must
   exactly match the size of the 'SmallArray'.

4. __Prefix consistency__: For any node at 'Shift' @s@, all keys in its
   subtree must share the same prefix for the bits more significant than @s@.

5. __Empty only at root__: The canonical empty node may only appear at the
   root. Internal nodes are never empty.
-}
data Word64Map a
  = Branch !Bitmap !(SmallArray (Word64Map a))
  | Leaf !Word64 a

instance Functor Word64Map where
  fmap f (Leaf k v) = Leaf k (f v)
  fmap f (Branch bm ary) = Branch bm (fmap (fmap f) ary)

instance Show a => Show (Word64Map a) where
  show m = "fromList " ++ show (toList m)

instance Eq a => Eq (Word64Map a) where
  m1 == m2 = eqMap m1 m2

eqMap :: Eq a => Word64Map a -> Word64Map a -> Bool
eqMap m1 m2 = eqMap_ m1 m2
 where
  eqMap_ (Leaf k1 v1) (Leaf k2 v2) = k1 == k2 && v1 == v2
  eqMap_ (Branch bm1 ary1) (Branch bm2 ary2) =
    bm1 == bm2 && eqSmallArray ary1 ary2
  eqMap_ _ _ = False

  eqSmallArray ary1 ary2
    | sameSmallArray ary1 ary2 = True
    | otherwise = loop (n - 1)
   where
    n = sizeofSmallArray ary1
    loop i
      | i < 0 = True
      | otherwise =
          let a1 = indexSmallArray ary1 i
              a2 = indexSmallArray ary2 i
           in eqMap a1 a2 && loop (i - 1)

sameSmallArray :: SmallArray a -> SmallArray a -> Bool
sameSmallArray (SmallArray a1#) (SmallArray a2#) =
  isTrue# (sameSmallArray# a1# a2#)
{-# INLINE sameSmallArray #-}

{- | Pointer equality for 'Word64Map' values.

Best when both maps are already in WHNF; otherwise this can return False
for equal but unevaluated thunks. Use only for sharing/fast-paths.
-}
sameMap :: Word64Map a -> Word64Map a -> Bool
sameMap m1 m2 = isTrue# (reallyUnsafePtrEquality m1 m2)
{-# INLINE sameMap #-}

sameValue :: a -> a -> Bool
sameValue x y = isTrue# (reallyUnsafePtrEquality x y)
{-# INLINE sameValue #-}

instance Ord a => Ord (Word64Map a) where
  compare m1 m2 = compare (toList m1) (toList m2)

instance Read a => Read (Word64Map a) where
  readPrec = parens $ do
    Ident "fromList" <- lexP
    xs <- readPrec
    pure (fromList xs)

instance Exts.IsList (Word64Map a) where
  type Item (Word64Map a) = (Word64, a)
  fromList = Amt.Word64.Map.Internal.fromList
  toList = Amt.Word64.Map.Internal.toList

instance Eq1 Word64Map where
  liftEq f m1 m2 =
    FunctorClasses.liftEq (FunctorClasses.liftEq f) (toList m1) (toList m2)

instance Ord1 Word64Map where
  liftCompare f m1 m2 =
    FunctorClasses.liftCompare
      (FunctorClasses.liftCompare f)
      (toList m1)
      (toList m2)

instance Show1 Word64Map where
  liftShowsPrec sp sl d =
    let showPair = FunctorClasses.liftShowsPrec sp sl
        showPairs = FunctorClasses.liftShowsPrec showPair (FunctorClasses.liftShowList sp sl)
     in showsUnaryWith showPairs "fromList" d . toList

instance Read1 Word64Map where
  liftReadPrec rp rlp = parens $ do
    Ident "fromList" <- lexP
    let readPair = FunctorClasses.liftReadPrec rp rlp
        readPairs =
          FunctorClasses.liftReadPrec readPair (FunctorClasses.liftReadListPrec rp rlp)
    xs <- readPairs
    pure (fromList xs)

instance Foldable Word64Map where
  foldMap f (Leaf _ v) = f v
  foldMap f (Branch _ ary) = Foldable.foldMap (foldMap f) ary

  foldr f z (Leaf _ v) = f v z
  foldr f z (Branch _ ary) = Foldable.foldr (\m acc -> foldr f acc m) z ary

  length = size

  null = null

instance Traversable Word64Map where
  traverse f (Leaf k v) = Leaf k <$> f v
  traverse f (Branch bm ary) = Branch bm <$> traverse (traverse f) ary

instance Semigroup (Word64Map a) where
  (<>) = union

instance Monoid (Word64Map a) where
  mempty = empty
  mappend = (<>)

instance NFData1 Word64Map where
  liftRnf f (Leaf _ v) = f v
  liftRnf f (Branch _ ary) =
    Foldable.foldr (\m acc -> liftRnf f m `seq` acc) () ary

instance NFData a => NFData (Word64Map a) where
  rnf = rnf1

instance Data a => Data (Word64Map a) where
  gfoldl k z m = z fromList `k` toList m
  gunfold k z c
    | c == fromListConstr = k (z fromList)
    | otherwise = error "gunfold: expected fromList"
  toConstr _ = fromListConstr
  dataTypeOf _ = word64MapDataType

word64MapDataType :: DataType
word64MapDataType =
  mkDataType "Amt.Word64.Map.Word64Map" [fromListConstr]

fromListConstr :: Constr
fromListConstr = mkConstr word64MapDataType "fromList" [] Prefix

newtype Bitmap = BM Word64
  deriving (Eq)

{- | Bitmap query result: bit mask for the current slot, compact array index,
and whether the bit is present.

The array index is the position in the compact 'SmallArray' for this slot.
Construct with 'index' when the array index is needed regardless of presence.
-}
data Index = Index !Bitmap !Int !BitMatch

-- | Does the Bitmap contain the Word64 at the given Shift?
data BitMatch = NoMatch | Match

-- | Unlifted shift counter in multiples of 6 bits.
type Shift = Int#

-- | Boxed shift value for diagnostics and 'InvariantViolation' payloads.
newtype ShiftBox = ShiftBox Int
  deriving (Eq, Show)

shiftToInt :: Shift -> Int
shiftToInt s = I# s
{-# INLINE shiftToInt #-}

nextShift :: Shift -> Shift
nextShift s = s +# 6#
{-# INLINE nextShift #-}

shiftGE64 :: Shift -> Bool
shiftGE64 s = case s >=# 64# of
  1# -> True
  _ -> False
{-# INLINE shiftGE64 #-}

shiftToBox :: Shift -> ShiftBox
shiftToBox s = ShiftBox (I# s)
{-# INLINE shiftToBox #-}

{- | Mask containing the lowest set bit.

When the input is @0@, the result is @0@.
-}
lowBit :: Word64 -> Word64
lowBit w = w .&. negate w
{-# INLINE lowBit #-}

{- | Clear the lowest set bit.

When the input is @0@, the result is @0@.
-}
clearLowBit :: Word64 -> Word64
clearLowBit w = w .&. (w - 1)
{-# INLINE clearLowBit #-}

valid :: Word64Map a -> Maybe InvariantViolation
valid (Branch (BM 0) ary)
  | n == 0 = Nothing
  | otherwise = Just $ BitmapCountMismatch 0 n
 where
  n = sizeofSmallArray ary
valid t = validInternal 0# 0 t

validInternal :: Shift -> Word64 -> Word64Map a -> Maybe InvariantViolation
validInternal shift !prefix (Leaf k _) =
  let mask =
        if shiftGE64 shift
          then complement 0
          else (Bits.bit (shiftToInt shift) :: Word64) - 1
   in if (k .&. mask) == prefix
        then Nothing
        else Just $ PrefixMismatch k (shiftToBox shift) prefix
validInternal _ _ (Branch (BM 0) _) = Just UnexpectedEmptyBranch
validInternal shift !prefix (Branch (BM bm) ary) =
  let n = sizeofSmallArray ary
   in if popCount bm /= n
        then Just $ BitmapCountMismatch bm n
        else validSubtrees shift prefix bm ary

validSubtrees ::
  Shift ->
  Word64 ->
  Word64 ->
  SmallArray (Word64Map a) ->
  Maybe InvariantViolation
validSubtrees shift prefix bm ary
  | sizeofSmallArray ary == 1
  , Leaf{} <- indexSmallArray ary 0 =
      Just $ RedundantBranch prefix
  | otherwise = valid_
 where
  valid_ =
    let children = Foldable.toList ary
        bits = [i | i <- [0 .. 63], testBit bm i]
     in Foldable.asum $
          zipWith
            ( \i child ->
                validInternal
                  (nextShift shift)
                  (prefix .|. (fromIntegral i `Bits.shiftL` shiftToInt shift :: Word64))
                  child
            )
            bits
            children

{- | Compute the bitmap bit for @k@ at @shift@ and return the 'Index'.

Use this when the array index is needed regardless of presence.
-}
index :: Shift -> Word64 -> Bitmap -> Index
index shift !k (BM bm) =
  let ix = fromIntegral ((k `unsafeShiftR` shiftToInt shift) .&. 0x3f)
      bit = 1 `unsafeShiftL` ix
      i = popCount (bm .&. (bit - 1))
      match = if bm .&. bit == 0 then NoMatch else Match
   in Index (BM bit) i match
{-# INLINE index #-}

{- | Like 'index', but only returns the array index when the bit is present.

This avoids a 'popCount' when the lookup misses.
-}
indexMatch :: Shift -> Word64 -> Bitmap -> Maybe Int
indexMatch shift !k (BM bm) =
  let ix = fromIntegral ((k `unsafeShiftR` shiftToInt shift) .&. 0x3f)
      bit = 1 `unsafeShiftL` ix
   in if bm .&. bit == 0
        then Nothing
        else Just (popCount (bm .&. (bit - 1)))
{-# INLINE indexMatch #-}

empty :: Word64Map a
empty = Branch (BM 0) mempty
{-# NOINLINE empty #-}

singleton :: Word64 -> a -> Word64Map a
singleton !k v = Leaf k v

null :: Word64Map a -> Bool
null (Branch (BM 0) _) = True
null _ = False

size :: Word64Map a -> Int
size (Leaf _ _) = 1
size (Branch _ ary) = Foldable.sum (fmap size ary)

lookup :: Word64 -> Word64Map a -> Maybe a
lookup !k m = case k of
  W64# ww -> lookupAtShift# 0# ww m

lookupAtShift :: Shift -> Word64 -> Word64Map a -> Maybe a
lookupAtShift shift !k = case k of
  W64# ww -> lookupAtShift# shift ww

lookupAtShift# :: Shift -> Word64# -> Word64Map a -> Maybe a
lookupAtShift# shift k = lookup_ shift
 where
  lookup_ _ (Leaf k' v) =
    case k' of
      W64# k'# ->
        case eqWord64# k k'# of
          1# -> Just v
          _ -> Nothing
  lookup_ s (Branch (BM bm) ary) =
    case indexMatch s (W64# k) (BM bm) of
      Nothing -> Nothing
      Just i -> lookup_ (nextShift s) (indexSmallArray ary i)

member :: Word64 -> Word64Map a -> Bool
member !k m = case lookup k m of
  Just _ -> True
  Nothing -> False

notMember :: Word64 -> Word64Map a -> Bool
notMember !k m = not (member k m)

findWithDefault :: a -> Word64 -> Word64Map a -> a
findWithDefault def !k m = case lookup k m of
  Just v -> v
  Nothing -> def

elems :: Word64Map a -> [a]
elems = fmap snd . toList

keys :: Word64Map a -> [Word64]
keys = fmap fst . toList

assocs :: Word64Map a -> [(Word64, a)]
assocs = toList

foldrWithKey :: (Word64 -> a -> b -> b) -> b -> Word64Map a -> b
foldrWithKey f z (Leaf k v) = f k v z
foldrWithKey f z (Branch _ ary) = Foldable.foldr (\m acc -> foldrWithKey f acc m) z ary

foldlWithKey' :: (b -> Word64 -> a -> b) -> b -> Word64Map a -> b
foldlWithKey' f z (Leaf k v) = f z k v
foldlWithKey' f z (Branch _ ary) = Foldable.foldl' (\acc m -> foldlWithKey' f acc m) z ary

insert :: Word64 -> a -> Word64Map a -> Word64Map a
insert !k v m = case m of
  Branch (BM 0) _ -> singleton k v
  Leaf k' v'
    | k == k' -> Leaf k v
    | otherwise -> two 0# k v k' v'
  Branch (BM bm) ary ->
    case index 0# k (BM bm) of
      Index _ i Match ->
        let child = indexSmallArray ary i
            newChild = insertAtShift (nextShift 0#) k v child
         in Branch (BM bm) (updateAt i newChild ary)
      Index (BM bit) i NoMatch ->
        Branch (BM (bm .|. bit)) (insertAt i (Leaf k v) ary)

-- | Unsafe insert that mutates arrays in-place under the hood.
insertUnsafe :: Word64 -> a -> Word64Map a -> Word64Map a
insertUnsafe !k v m = case m of
  Branch (BM 0) _ -> singleton k v
  _ -> runST (insertAtShiftUnsafe 0# k v m)

insertWith :: (a -> a -> a) -> Word64 -> a -> Word64Map a -> Word64Map a
insertWith f !k v m = insertWithKey (\_ new old -> f new old) k v m

insertWithKey ::
  (Word64 -> a -> a -> a) -> Word64 -> a -> Word64Map a -> Word64Map a
insertWithKey f !k v m = case m of
  Branch (BM 0) _ -> singleton k v
  Leaf k' v'
    | k == k' -> Leaf k (f k v v')
    | otherwise -> two 0# k v k' v'
  Branch (BM bm) ary ->
    case index 0# k (BM bm) of
      Index _ i Match ->
        let child = indexSmallArray ary i
            newChild = insertWithKeyAtShift (nextShift 0#) f k v child
         in Branch (BM bm) (updateAt i newChild ary)
      Index (BM bit) i NoMatch ->
        Branch (BM (bm .|. bit)) (insertAt i (Leaf k v) ary)

-- | Only valid for internal nodes.
insertWithKeyAtShift ::
  Shift -> (Word64 -> a -> a -> a) -> Word64 -> a -> Word64Map a -> Word64Map a
insertWithKeyAtShift s f !k v m = case m of
  Leaf k' v'
    | k == k' -> Leaf k (f k v v')
    | otherwise -> two s k v k' v'
  Branch (BM bm) ary ->
    case index s k (BM bm) of
      Index _ i Match ->
        let child = indexSmallArray ary i
            newChild = insertWithKeyAtShift (nextShift s) f k v child
         in Branch (BM bm) (updateAt i newChild ary)
      Index (BM bit) i NoMatch ->
        Branch (BM (bm .|. bit)) (insertAt i (Leaf k v) ary)

-- | Only valid for internal nodes.
insertAtShift :: Shift -> Word64 -> a -> Word64Map a -> Word64Map a
insertAtShift s !k v m = case m of
  Leaf k' v'
    | k == k' -> Leaf k v
    | otherwise -> two s k v k' v'
  Branch (BM bm) ary ->
    case index s k (BM bm) of
      Index _ i Match ->
        let child = indexSmallArray ary i
            newChild = insertAtShift (nextShift s) k v child
         in Branch (BM bm) (updateAt i newChild ary)
      Index (BM bit) i NoMatch ->
        Branch (BM (bm .|. bit)) (insertAt i (Leaf k v) ary)

-- | Unsafe insert using in-place updates. Expects a non-empty root.
insertAtShiftUnsafe :: Shift -> Word64 -> a -> Word64Map a -> ST s (Word64Map a)
insertAtShiftUnsafe s !k v m = case m of
  Leaf k' v'
    | k == k' -> pure (Leaf k v)
    | otherwise -> pure (two s k v k' v')
  branch@(Branch (BM bm) ary) ->
    case index s k (BM bm) of
      Index _ i Match -> do
        let child = indexSmallArray ary i
        newChild <- insertAtShiftUnsafe (nextShift s) k v child
        _ <- updateAtUnsafe i newChild ary
        pure branch
      Index (BM bit) i NoMatch ->
        pure (Branch (BM (bm .|. bit)) (insertAt i (Leaf k v) ary))

two :: Shift -> Word64 -> a -> Word64 -> a -> Word64Map a
two shift !k1 v1 !k2 v2 =
  let idx1 = fromIntegral ((k1 `Bits.shiftR` shiftToInt shift) .&. 0x3f)
      idx2 = fromIntegral ((k2 `Bits.shiftR` shiftToInt shift) .&. 0x3f)
   in if idx1 /= idx2
        then
          let bm = Bits.bit idx1 .|. Bits.bit idx2
              ary =
                if idx1 < idx2
                  then smallArrayFromList [Leaf k1 v1, Leaf k2 v2]
                  else smallArrayFromList [Leaf k2 v2, Leaf k1 v1]
           in Branch (BM bm) ary
        else
          let child = two (nextShift shift) k1 v1 k2 v2
              bm = Bits.bit idx1
           in Branch (BM bm) (smallArrayFromList [child])

delete :: Word64 -> Word64Map a -> Word64Map a
delete !k = deleteAtShift 0# k

deleteAtShift :: Shift -> Word64 -> Word64Map a -> Word64Map a
deleteAtShift shift !k m = del shift m
 where
  del _ (Leaf k' _) | k == k' = empty
  del _ leaf@(Leaf _ _) = leaf
  del s branch@(Branch (BM bm) ary) =
    case index s k (BM bm) of
      Index _ _ NoMatch -> branch
      Index (BM bit) i Match ->
        case indexSmallArray## ary i of
          (# child0 #) ->
            -- TODO: Verify whether array elements are always in WHNF so we can
            -- drop the bang on child/newChild here.
            let !child = child0
                !newChild = del (nextShift s) child
             in if sameMap child newChild
                  then branch
                  else
                    if null newChild
                      then
                        let newBm = bm .&. complement bit
                            newAry = removeAt i ary
                         in collapse (BM newBm) newAry
                      else
                        let newAry = updateAt i newChild ary
                         in collapse (BM bm) newAry

adjust :: (a -> a) -> Word64 -> Word64Map a -> Word64Map a
adjust f !k m = adjustWithKey (\_ x -> f x) k m

adjustWithKey :: (Word64 -> a -> a) -> Word64 -> Word64Map a -> Word64Map a
adjustWithKey f !k m = adj 0# m
 where
  adj _ leaf@(Leaf k' v)
    | k == k' =
        let v' = f k v
         in if sameValue v v'
              then leaf
              else Leaf k v'
    | otherwise = leaf
  adj shift branch@(Branch (BM bm) ary) =
    case index shift k (BM bm) of
      Index _ _ NoMatch -> branch
      Index _ i Match ->
        case indexSmallArray## ary i of
          (# child0 #) ->
            -- TODO: Verify whether array elements are always in WHNF so we can
            -- drop the bang on child/newChild here.
            let !child = child0
                !newChild = adj (nextShift shift) child
             in if sameMap child newChild
                  then branch
                  else Branch (BM bm) (updateAt i newChild ary)

update :: (a -> Maybe a) -> Word64 -> Word64Map a -> Word64Map a
update f !k m = updateWithKey (\_ x -> f x) k m

updateWithKey ::
  (Word64 -> a -> Maybe a) -> Word64 -> Word64Map a -> Word64Map a
updateWithKey f !k = alter (\v -> v >>= f k) k

alter :: (Maybe a -> Maybe a) -> Word64 -> Word64Map a -> Word64Map a
alter f !k m = case lookup k m of
  Nothing -> case f Nothing of
    Nothing -> m
    Just v -> insert k v m
  Just v -> case f (Just v) of
    Nothing -> delete k m
    Just v' -> insert k v' m

map :: (a -> b) -> Word64Map a -> Word64Map b
map = fmap

mapWithKey :: (Word64 -> a -> b) -> Word64Map a -> Word64Map b
mapWithKey f (Leaf k v) = Leaf k (f k v)
mapWithKey f (Branch bm ary) = Branch bm (fmap (mapWithKey f) ary)

union :: Word64Map a -> Word64Map a -> Word64Map a
union m1 m2 = unionAtShiftHandleEmpty 0# m1 m2

{- | Merge two branch arrays by walking the union bitmap once.

Assumes both bitmaps are non-zero (i.e. neither branch is empty), so the
union bitmap is also non-zero.

The @both@ function is used when a bit is present in both branches.
-}
unionBranches ::
  Word64 ->
  SmallArray (Word64Map a) ->
  Word64 ->
  SmallArray (Word64Map a) ->
  (Word64Map a -> Word64Map a -> Word64Map a) ->
  (Word64, SmallArray (Word64Map a))
unionBranches bm1 ary1 bm2 ary2 both =
  let newBm = bm1 .|. bm2
      n = popCount newBm
   in ( newBm
      , runST $ do
          mary <- newSmallArray n empty
          let step !w !i1 !i2 !j
                | w == 0 = unsafeFreezeSmallArray mary
                | otherwise =
                    let bit = lowBit w
                        has1 = bm1 .&. bit /= 0
                        has2 = bm2 .&. bit /= 0
                        (child, i1', i2') = case (has1, has2) of
                          (True, True) ->
                            ( both
                                (indexSmallArray ary1 i1)
                                (indexSmallArray ary2 i2)
                            , i1 + 1
                            , i2 + 1
                            )
                          (True, False) ->
                            (indexSmallArray ary1 i1, i1 + 1, i2)
                          (False, True) ->
                            (indexSmallArray ary2 i2, i1, i2 + 1)
                          (False, False) -> error "unionBranches: impossible"
                        w' = clearLowBit w
                     in do
                          writeSmallArray mary j child
                          step w' i1' i2' (j + 1)
          step newBm 0 0 0
      )

unionAtShiftHandleEmpty :: Shift -> Word64Map a -> Word64Map a -> Word64Map a
unionAtShiftHandleEmpty shift m1 m2 = case (m1, m2) of
  (Branch (BM 0) _, _) -> m2
  (_, Branch (BM 0) _) -> m1
  _ -> unionAtShiftNoEmpty shift m1 m2

unionAtShiftNoEmpty :: Shift -> Word64Map a -> Word64Map a -> Word64Map a
unionAtShiftNoEmpty shift m1 m2 = case (m1, m2) of
  (Leaf k1 v1, _) -> insertAtShift shift k1 v1 m2
  (_, Leaf k2 v2) -> insertIfNotExistsAtShift shift k2 v2 m1
  (Branch (BM bm1) ary1, Branch (BM bm2) ary2)
    | sameSmallArray ary1 ary2 ->
        m1
    | otherwise ->
        let (newBm, newAry) =
              unionBranches
                bm1
                ary1
                bm2
                ary2
                (unionAtShiftNoEmpty (nextShift shift))
         in collapse (BM newBm) newAry

unionWith :: (a -> a -> a) -> Word64Map a -> Word64Map a -> Word64Map a
unionWith f = unionWithKey (\_ x y -> f x y)

unionWithKey ::
  (Word64 -> a -> a -> a) -> Word64Map a -> Word64Map a -> Word64Map a
unionWithKey f m1 m2 = unionWithKeyAtShiftRoot 0# f m1 m2

unionWithKeyAtShiftRoot ::
  Shift -> (Word64 -> a -> a -> a) -> Word64Map a -> Word64Map a -> Word64Map a
unionWithKeyAtShiftRoot shift f m1 m2 = case (m1, m2) of
  (Branch (BM 0) _, _) -> m2
  (_, Branch (BM 0) _) -> m1
  _ -> unionWithKeyAtShift shift f m1 m2

unionWithKeyAtShift ::
  Shift -> (Word64 -> a -> a -> a) -> Word64Map a -> Word64Map a -> Word64Map a
unionWithKeyAtShift shift f m1 m2 = case (m1, m2) of
  (Leaf k1 v1, _) -> insertWithKeyAtShift shift f k1 v1 m2
  (_, Leaf k2 v2) -> insertWithKeyAtShift shift (\k new old -> f k old new) k2 v2 m1
  (Branch (BM bm1) ary1, Branch (BM bm2) ary2) ->
    let (newBm, newAry) =
          unionBranches
            bm1
            ary1
            bm2
            ary2
            (unionWithKeyAtShift (nextShift shift) f)
     in collapse (BM newBm) newAry

insertIfNotExists :: Word64 -> a -> Word64Map a -> Word64Map a
insertIfNotExists !k v m = insertIfNotExistsAtShift 0# k v m

{- | Insert the key/value only when the key is absent.

If called on the canonical empty root (a 'Branch' with an empty bitmap),
this returns a 'Leaf' to avoid introducing a redundant single-child branch.
Internal nodes are never empty.
-}
insertIfNotExistsAtShift :: Shift -> Word64 -> a -> Word64Map a -> Word64Map a
insertIfNotExistsAtShift shift !k v m = case m of
  Leaf k' v'
    | k == k' -> m
    | otherwise -> two shift k v k' v'
  Branch (BM 0) _ -> Leaf k v
  branch@(Branch (BM bm) ary) ->
    case index shift k (BM bm) of
      Index _ i Match ->
        case indexSmallArray## ary i of
          (# child0 #) ->
            -- TODO: Verify whether array elements are always in WHNF so we can
            -- drop the bang on child/newChild here.
            let !child = child0
                !newChild = insertIfNotExistsAtShift (nextShift shift) k v child
             in if sameMap child newChild
                  then branch
                  else Branch (BM bm) (updateAt i newChild ary)
      Index (BM bit) i NoMatch ->
        Branch (BM (bm .|. bit)) (insertAt i (Leaf k v) ary)

fromList :: [(Word64, a)] -> Word64Map a
fromList = Foldable.foldl' (\m (!k, v) -> insertUnsafe k v m) empty

toList :: Word64Map a -> [(Word64, a)]
toList (Leaf k v) = [(k, v)]
toList (Branch _ ary) = concatMap toList (Foldable.toList ary)

insertAt :: Int -> a -> SmallArray a -> SmallArray a
insertAt i a ary = runSmallArray $ do
  let n = sizeofSmallArray ary
  mary <- newSmallArray (n + 1) a
  copySmallArray mary 0 ary 0 i
  copySmallArray mary (i + 1) ary i (n - i)
  return mary

updateAt :: Int -> a -> SmallArray a -> SmallArray a
updateAt i a ary = runSmallArray $ do
  let n = sizeofSmallArray ary
  mary <- newSmallArray n a
  copySmallArray mary 0 ary 0 n
  writeSmallArray mary i a
  return mary

updateAtUnsafe :: Int -> a -> SmallArray a -> ST s (SmallArray a)
updateAtUnsafe i a ary = do
  mary <- unsafeThawSmallArray ary
  writeSmallArray mary i a
  unsafeFreezeSmallArray mary

removeAt :: Int -> SmallArray a -> SmallArray a
removeAt i ary = runSmallArray $ do
  let n = sizeofSmallArray ary
  mary <- newSmallArray (n - 1) (error "removeAt")
  copySmallArray mary 0 ary 0 i
  copySmallArray mary i ary (i + 1) (n - i - 1)
  return mary

-- TODO: Avoid calling collapse in hot paths like delete by handling single-child
-- branches directly to preserve sharing and reduce work.
collapse :: Bitmap -> SmallArray (Word64Map a) -> Word64Map a
collapse bm ary = case sizeofSmallArray ary of
  0 -> empty
  1 -> case indexSmallArray ary 0 of
    l@Leaf{} -> l
    _ -> Branch bm ary
  _ -> Branch bm ary

mergeWithKey ::
  forall a b c.
  (Word64 -> a -> b -> Maybe c) ->
  (Word64Map a -> Word64Map c) ->
  (Word64Map b -> Word64Map c) ->
  Word64Map a ->
  Word64Map b ->
  Word64Map c
mergeWithKey f g1 g2 m1_ m2_ = merge 0# m1_ m2_
 where
  merge _ (Branch (BM 0) _) m2 = g2 m2
  merge _ m1 (Branch (BM 0) _) = g1 m1
  merge shift (Leaf k1 v1) (Leaf k2 v2)
    | k1 == k2 =
        case f k1 v1 v2 of
          Nothing -> empty
          Just v' -> Leaf k1 v'
    | otherwise =
        unionAtShiftHandleEmpty shift (g1 (Leaf k1 v1)) (g2 (Leaf k2 v2))
  merge shift (Leaf k1 v1) m2@(Branch (BM bm2) ary2) =
    case index shift k1 (BM bm2) of
      Index _ _ NoMatch ->
        unionAtShiftHandleEmpty shift (g1 (Leaf k1 v1)) (g2 m2)
      Index _ i Match ->
        let (newBm, newAry) = runST (mergeLeafVsBranch shift k1 v1 bm2 ary2 i)
         in collapse (BM newBm) newAry
  merge shift m1@(Branch (BM bm1) ary1) (Leaf k2 v2) =
    case index shift k2 (BM bm1) of
      Index _ _ NoMatch ->
        unionAtShiftHandleEmpty shift (g1 m1) (g2 (Leaf k2 v2))
      Index _ i Match ->
        let (newBm, newAry) = runST (mergeBranchVsLeaf shift k2 v2 bm1 ary1 i)
         in collapse (BM newBm) newAry
  merge shift (Branch (BM bm1) ary1) (Branch (BM bm2) ary2) =
    let (newBm, newAry) = runST (mergeWithKeyBranches shift bm1 ary1 bm2 ary2)
     in collapse (BM newBm) newAry

  mergeWithKeyBranches ::
    forall s.
    Shift ->
    Word64 ->
    SmallArray (Word64Map a) ->
    Word64 ->
    SmallArray (Word64Map b) ->
    ST s (Word64, SmallArray (Word64Map c))
  mergeWithKeyBranches shift bm1 ary1 bm2 ary2 = do
    let unionBm = bm1 .|. bm2
        n = popCount unionBm
    if n == 0
      then pure (0, mempty)
      else do
        mary <- newSmallArray n empty
        let finish j bmAcc =
              if j == 0
                then pure (0, mempty)
                else do
                  shrinkSmallMutableArray mary j
                  newAry <- unsafeFreezeSmallArray mary
                  pure (bmAcc, newAry)
            step !w !i1 !i2 !j !newBm
              | w == 0 = finish j newBm
              | otherwise =
                  let bit = lowBit w
                      has1 = bm1 .&. bit /= 0
                      has2 = bm2 .&. bit /= 0
                      (m1, i1') =
                        if has1
                          then (Just (indexSmallArray ary1 i1), i1 + 1)
                          else (Nothing, i1)
                      (m2, i2') =
                        if has2
                          then (Just (indexSmallArray ary2 i2), i2 + 1)
                          else (Nothing, i2)
                      w' = clearLowBit w
                   in case (m1, m2) of
                        (Just c1, Just c2) ->
                          let child = merge (nextShift shift) c1 c2
                           in if null child
                                then step w' i1' i2' j newBm
                                else do
                                  writeSmallArray mary j child
                                  step w' i1' i2' (j + 1) (newBm .|. bit)
                        (Just c1, Nothing) ->
                          let child = g1 c1
                           in if null child
                                then step w' i1' i2' j newBm
                                else do
                                  writeSmallArray mary j child
                                  step w' i1' i2' (j + 1) (newBm .|. bit)
                        (Nothing, Just c2) ->
                          let child = g2 c2
                           in if null child
                                then step w' i1' i2' j newBm
                                else do
                                  writeSmallArray mary j child
                                  step w' i1' i2' (j + 1) (newBm .|. bit)
                        (Nothing, Nothing) -> error "mergeWithKey: impossible"
        step unionBm 0 0 0 0

  mergeLeafVsBranch ::
    forall s.
    Shift ->
    Word64 ->
    a ->
    Word64 ->
    SmallArray (Word64Map b) ->
    Int ->
    ST s (Word64, SmallArray (Word64Map c))
  mergeLeafVsBranch shift k1 v1 bm2 ary2 iMatch = do
    let n = sizeofSmallArray ary2
    mary <- newSmallArray n empty
    let step !w !i !j !newBm
          | w == 0 =
              if j == 0
                then pure (0, mempty)
                else do
                  shrinkSmallMutableArray mary j
                  newAry <- unsafeFreezeSmallArray mary
                  pure (newBm, newAry)
          | otherwise =
              let bit = lowBit w
                  child = indexSmallArray ary2 i
                  child' =
                    if i == iMatch
                      then merge (nextShift shift) (Leaf k1 v1) child
                      else g2 child
                  w' = clearLowBit w
               in if null child'
                    then step w' (i + 1) j newBm
                    else do
                      writeSmallArray mary j child'
                      step w' (i + 1) (j + 1) (newBm .|. bit)
    step bm2 0 0 0

  mergeBranchVsLeaf ::
    forall s.
    Shift ->
    Word64 ->
    b ->
    Word64 ->
    SmallArray (Word64Map a) ->
    Int ->
    ST s (Word64, SmallArray (Word64Map c))
  mergeBranchVsLeaf shift k2 v2 bm1 ary1 iMatch = do
    let n = sizeofSmallArray ary1
    mary <- newSmallArray n empty
    let step !w !i !j !newBm
          | w == 0 =
              if j == 0
                then pure (0, mempty)
                else do
                  shrinkSmallMutableArray mary j
                  newAry <- unsafeFreezeSmallArray mary
                  pure (newBm, newAry)
          | otherwise =
              let bit = lowBit w
                  child = indexSmallArray ary1 i
                  child' =
                    if i == iMatch
                      then merge (nextShift shift) child (Leaf k2 v2)
                      else g1 child
                  w' = clearLowBit w
               in if null child'
                    then step w' (i + 1) j newBm
                    else do
                      writeSmallArray mary j child'
                      step w' (i + 1) (j + 1) (newBm .|. bit)
    step bm1 0 0 0

difference :: Word64Map a -> Word64Map b -> Word64Map a
difference m1 m2 = differenceWith (\_ _ -> Nothing) m1 m2

differenceWith ::
  forall a b.
  (a -> b -> Maybe a) ->
  Word64Map a ->
  Word64Map b ->
  Word64Map a
differenceWith f m1_ m2_ = diff 0# m1_ m2_
 where
  diff _ (Branch (BM 0) _) _ = empty
  diff _ m1 (Branch (BM 0) _) = m1
  diff shift (Leaf k1 v1) m2 = case lookupAtShift shift k1 m2 of
    Nothing -> Leaf k1 v1
    Just v2 -> case f v1 v2 of
      Nothing -> empty
      Just v1' -> Leaf k1 v1'
  diff shift m1 (Leaf k2 v2) = case lookupAtShift shift k2 m1 of
    Nothing -> m1
    Just v1 -> case f v1 v2 of
      Nothing -> deleteAtShift shift k2 m1
      Just v1' -> insertAtShift shift k2 v1' (deleteAtShift shift k2 m1)
  diff shift (Branch (BM bm1) ary1) (Branch (BM bm2) ary2) =
    let (newBm, newAry) = runST (differenceBranches shift bm1 ary1 bm2 ary2)
     in collapse (BM newBm) newAry

  differenceBranches ::
    forall s.
    Shift ->
    Word64 ->
    SmallArray (Word64Map a) ->
    Word64 ->
    SmallArray (Word64Map b) ->
    ST s (Word64, SmallArray (Word64Map a))
  differenceBranches shift bm1 ary1 bm2 ary2 = do
    let unionBm = bm1 .|. bm2
        n = popCount bm1
    if n == 0
      then pure (0, mempty)
      else do
        mary <- newSmallArray n empty
        let finish j bmAcc =
              if j == 0
                then pure (0, mempty)
                else do
                  shrinkSmallMutableArray mary j
                  newAry <- unsafeFreezeSmallArray mary
                  pure (bmAcc, newAry)
            step !w !i1 !i2 !j !newBm
              | w == 0 = finish j newBm
              | otherwise =
                  let bit = lowBit w
                      has1 = bm1 .&. bit /= 0
                      has2 = bm2 .&. bit /= 0
                      -- TODO: Avoid Maybe allocations in this inner loop.
                      (m1, i1') =
                        if has1
                          then (Just (indexSmallArray ary1 i1), i1 + 1)
                          else (Nothing, i1)
                      (m2, i2') =
                        if has2
                          then (Just (indexSmallArray ary2 i2), i2 + 1)
                          else (Nothing, i2)
                      w' = clearLowBit w
                   in case (m1, m2) of
                        (Just c1, Just c2) ->
                          let child = diff (nextShift shift) c1 c2
                           in if null child
                                then step w' i1' i2' j newBm
                                else do
                                  writeSmallArray mary j child
                                  step w' i1' i2' (j + 1) (newBm .|. bit)
                        (Just c1, Nothing) -> do
                          writeSmallArray mary j c1
                          step w' i1' i2' (j + 1) (newBm .|. bit)
                        _ -> step w' i1' i2' j newBm
        step unionBm 0 0 0 0

intersection :: Word64Map a -> Word64Map b -> Word64Map a
intersection m1 m2 = intersectionWith (\x _ -> x) m1 m2

intersectionWith :: (a -> b -> c) -> Word64Map a -> Word64Map b -> Word64Map c
intersectionWith f = intersectionWithKey (\_ x y -> f x y)

intersectionWithKey ::
  forall a b c.
  (Word64 -> a -> b -> c) ->
  Word64Map a ->
  Word64Map b ->
  Word64Map c
intersectionWithKey f m1_ m2_ = inter 0# m1_ m2_
 where
  inter _ (Branch (BM 0) _) _ = empty
  inter _ _ (Branch (BM 0) _) = empty
  inter shift (Leaf k1 v1) m2 = case lookupAtShift shift k1 m2 of
    Nothing -> empty
    Just v2 -> Leaf k1 (f k1 v1 v2)
  inter shift m1 (Leaf k2 v2) = case lookupAtShift shift k2 m1 of
    Nothing -> empty
    Just v1 -> Leaf k2 (f k2 v1 v2)
  inter shift (Branch (BM bm1) ary1) (Branch (BM bm2) ary2) =
    let (newBm, newAry) = runST (interArr shift bm1 ary1 bm2 ary2)
     in collapse (BM newBm) newAry

  interArr ::
    forall s.
    Shift ->
    Word64 ->
    SmallArray (Word64Map a) ->
    Word64 ->
    SmallArray (Word64Map b) ->
    ST s (Word64, SmallArray (Word64Map c))
  interArr shift bm1 ary1 bm2 ary2 = do
    let commonBm = bm1 .&. bm2
        unionBm = bm1 .|. bm2
        n = popCount commonBm
    if n == 0
      then pure (0, mempty)
      else do
        mary <- newSmallArray n empty
        let finish j bmAcc =
              if j == 0
                then pure (0, mempty)
                else do
                  shrinkSmallMutableArray mary j
                  newAry <- unsafeFreezeSmallArray mary
                  pure (bmAcc, newAry)
            step !w !i1 !i2 !j !newBm
              | w == 0 = finish j newBm
              | otherwise =
                  let bit = lowBit w
                      has1 = bm1 .&. bit /= 0
                      has2 = bm2 .&. bit /= 0
                      (m1, i1') =
                        if has1
                          then (Just (indexSmallArray ary1 i1), i1 + 1)
                          else (Nothing, i1)
                      (m2, i2') =
                        if has2
                          then (Just (indexSmallArray ary2 i2), i2 + 1)
                          else (Nothing, i2)
                      w' = clearLowBit w
                   in case (m1, m2) of
                        (Just c1, Just c2) ->
                          let child = inter (nextShift shift) c1 c2
                           in if null child
                                then step w' i1' i2' j newBm
                                else do
                                  writeSmallArray mary j child
                                  step w' i1' i2' (j + 1) (newBm .|. bit)
                        _ -> step w' i1' i2' j newBm
        step unionBm 0 0 0 0

filter :: (a -> Bool) -> Word64Map a -> Word64Map a
filter f = filterWithKey (\_ x -> f x)

filterWithKey :: forall a. (Word64 -> a -> Bool) -> Word64Map a -> Word64Map a
filterWithKey f = filt
 where
  filt (Leaf k v) = if f k v then Leaf k v else empty
  filt (Branch (BM bm) ary)
    | bm == 0 = empty
    | otherwise =
        let (newBm, newAry) = runST (filtArr bm ary)
         in collapse (BM newBm) newAry -- keep invariant by collapsing empty/single-leaf branches
  filtArr ::
    forall s.
    Word64 -> SmallArray (Word64Map a) -> ST s (Word64, SmallArray (Word64Map a))
  filtArr bm ary = do
    let n = sizeofSmallArray ary
    mary <- newSmallArray n (empty :: Word64Map a)
    let step !w !i !j !newBm
          | w == 0 =
              if j == 0
                then pure (0, mempty)
                else do
                  shrinkSmallMutableArray mary j
                  newAry <- unsafeFreezeSmallArray mary
                  pure (newBm, newAry)
          | otherwise =
              let bit = lowBit w
                  child = filt (indexSmallArray ary i)
                  w' = clearLowBit w
               in if null child
                    then step w' (i + 1) j newBm
                    else do
                      writeSmallArray mary j child
                      step w' (i + 1) (j + 1) (newBm .|. bit)
    step bm 0 0 0

partition :: (a -> Bool) -> Word64Map a -> (Word64Map a, Word64Map a)
partition f = partitionWithKey (\_ x -> f x)

partitionWithKey ::
  forall a. (Word64 -> a -> Bool) -> Word64Map a -> (Word64Map a, Word64Map a)
partitionWithKey f m = part m
 where
  part (Leaf k v)
    | f k v = (Leaf k v, empty)
    | otherwise = (empty, Leaf k v)
  part (Branch (BM 0) _) = (empty, empty)
  part (Branch (BM bm) ary) =
    let (lBm, lAry, rBm, rAry) = partitionBranch bm ary
        l = collapse (BM lBm) lAry
        r = collapse (BM rBm) rAry
     in (l, r)

  partitionBranch ::
    Word64 ->
    SmallArray (Word64Map a) ->
    (Word64, SmallArray (Word64Map a), Word64, SmallArray (Word64Map a))
  partitionBranch bm ary = runST (partArr bm ary)

  partArr ::
    forall s.
    Word64 ->
    SmallArray (Word64Map a) ->
    ST s (Word64, SmallArray (Word64Map a), Word64, SmallArray (Word64Map a))
  partArr bm ary = do
    let n = sizeofSmallArray ary
    maryL <- newSmallArray n (empty :: Word64Map b)
    maryR <- newSmallArray n (empty :: Word64Map c)
    let finish ::
          forall x.
          SmallMutableArray s (Word64Map x) ->
          Int ->
          Word64 ->
          ST s (Word64, SmallArray (Word64Map x))
        finish mary j bmAcc =
          if j == 0
            then pure (0, mempty)
            else do
              shrinkSmallMutableArray mary j
              newAry <- unsafeFreezeSmallArray mary
              pure (bmAcc, newAry)
        step !w !i !jl !jr !lBm !rBm
          | w == 0 = do
              (lBm', lAry) <- finish maryL jl lBm
              (rBm', rAry) <- finish maryR jr rBm
              pure (lBm', lAry, rBm', rAry)
          | otherwise =
              let bit = lowBit w
                  child = indexSmallArray ary i
                  (lChild, rChild) = part child
                  w' = clearLowBit w
                  (jl', lBm') =
                    if null lChild
                      then (jl, lBm)
                      else (jl + 1, lBm .|. bit)
                  (jr', rBm') =
                    if null rChild
                      then (jr, rBm)
                      else (jr + 1, rBm .|. bit)
               in do
                    if null lChild
                      then pure ()
                      else writeSmallArray maryL jl lChild
                    if null rChild
                      then pure ()
                      else writeSmallArray maryR jr rChild
                    step w' (i + 1) jl' jr' lBm' rBm'
    step bm 0 0 0 0 0

mapMaybe :: (a -> Maybe b) -> Word64Map a -> Word64Map b
mapMaybe f = mapMaybeWithKey (\_ x -> f x)

mapMaybeWithKey ::
  forall a b. (Word64 -> a -> Maybe b) -> Word64Map a -> Word64Map b
mapMaybeWithKey f m = mapMay m
 where
  mapMay (Leaf k v) = case f k v of
    Nothing -> empty
    Just v' -> Leaf k v'
  mapMay (Branch (BM 0) _) = empty
  mapMay (Branch (BM bm) ary) =
    let (newBm, newAry) = mapMaybeBranch bm ary
     in collapse (BM newBm) newAry

  mapMaybeBranch ::
    Word64 -> SmallArray (Word64Map a) -> (Word64, SmallArray (Word64Map b))
  mapMaybeBranch bm ary = runST (mapMbArr bm ary)

  mapMbArr ::
    forall s.
    Word64 -> SmallArray (Word64Map a) -> ST s (Word64, SmallArray (Word64Map b))
  mapMbArr bm ary = do
    let n = sizeofSmallArray ary
    mary <- newSmallArray n empty
    let step !w !i !j !newBm
          | w == 0 =
              if j == 0
                then pure (0, mempty)
                else do
                  shrinkSmallMutableArray mary j
                  newAry <- unsafeFreezeSmallArray mary
                  pure (newBm, newAry)
          | otherwise =
              let bit = lowBit w
                  child = indexSmallArray ary i
                  child' = mapMay child
                  w' = clearLowBit w
               in if null child'
                    then step w' (i + 1) j newBm
                    else do
                      writeSmallArray mary j child'
                      step w' (i + 1) (j + 1) (newBm .|. bit)
    step bm 0 0 0

mapEither :: (a -> Either b c) -> Word64Map a -> (Word64Map b, Word64Map c)
mapEither f = mapEitherWithKey (\_ x -> f x)

mapEitherWithKey ::
  forall a b c.
  (Word64 -> a -> Either b c) ->
  Word64Map a ->
  (Word64Map b, Word64Map c)
mapEitherWithKey f m = mapE m
 where
  mapE (Leaf k v) = case f k v of
    Left b -> (Leaf k b, empty)
    Right c -> (empty, Leaf k c)
  mapE (Branch (BM 0) _) = (empty, empty)
  mapE (Branch (BM bm) ary) =
    let (lBm, lAry, rBm, rAry) = runST (mapEitherBranches bm ary)
        l = collapse (BM lBm) lAry
        r = collapse (BM rBm) rAry
     in (l, r)

  mapEitherBranches ::
    forall s.
    Word64 ->
    SmallArray (Word64Map a) ->
    ST s (Word64, SmallArray (Word64Map b), Word64, SmallArray (Word64Map c))
  mapEitherBranches bm ary = do
    let n = sizeofSmallArray ary
    maryL <- newSmallArray n (empty :: Word64Map b)
    maryR <- newSmallArray n (empty :: Word64Map c)
    let finish ::
          forall x.
          SmallMutableArray s (Word64Map x) ->
          Int ->
          Word64 ->
          ST s (Word64, SmallArray (Word64Map x))
        finish mary j bmAcc =
          if j == 0
            then pure (0, mempty)
            else do
              shrinkSmallMutableArray mary j
              newAry <- unsafeFreezeSmallArray mary
              pure (bmAcc, newAry)
        step !w !i !jl !jr !lBm !rBm
          | w == 0 = do
              (lBm', lAry) <- finish maryL jl lBm
              (rBm', rAry) <- finish maryR jr rBm
              pure (lBm', lAry, rBm', rAry)
          | otherwise =
              let bit = lowBit w
                  child = indexSmallArray ary i
                  (lChild, rChild) = mapE child
                  w' = clearLowBit w
                  (jl', lBm') =
                    if null lChild
                      then (jl, lBm)
                      else (jl + 1, lBm .|. bit)
                  (jr', rBm') =
                    if null rChild
                      then (jr, rBm)
                      else (jr + 1, rBm .|. bit)
               in do
                    if null lChild
                      then pure ()
                      else writeSmallArray maryL jl lChild
                    if null rChild
                      then pure ()
                      else writeSmallArray maryR jr rChild
                    step w' (i + 1) jl' jr' lBm' rBm'
    step bm 0 0 0 0 0

isSubmapOf :: Eq a => Word64Map a -> Word64Map a -> Bool
isSubmapOf = isSubmapOfBy (==)

isSubmapOfBy ::
  forall a b.
  (a -> b -> Bool) ->
  Word64Map a ->
  Word64Map b ->
  Bool
isSubmapOfBy f m1_ m2_ = submap 0# m1_ m2_
 where
  submap _ (Branch (BM 0) _) _ = True
  submap _ _ (Branch (BM 0) _) = False
  submap shift (Leaf k1 v1) m2 = case lookupAtShift shift k1 m2 of
    Nothing -> False
    Just v2 -> f v1 v2
  submap _ (Branch _ _) (Leaf _ _) = False
  submap shift (Branch (BM bm1) ary1) (Branch (BM bm2) ary2) =
    submapBranch (nextShift shift) bm1 ary1 bm2 ary2

  submapBranch ::
    Shift ->
    Word64 ->
    SmallArray (Word64Map a) ->
    Word64 ->
    SmallArray (Word64Map b) ->
    Bool
  submapBranch shift bm1 ary1 bm2 ary2 = step (bm1 .|. bm2) 0 0
   where
    step !w !i1 !i2
      | w == 0 = True
      | otherwise =
          let bit = lowBit w
              has1 = bm1 .&. bit /= 0
              has2 = bm2 .&. bit /= 0
              w' = clearLowBit w
           in case (has1, has2) of
                (True, True) ->
                  let c1 = indexSmallArray ary1 i1
                      c2 = indexSmallArray ary2 i2
                   in if submap shift c1 c2
                        then step w' (i1 + 1) (i2 + 1)
                        else False
                (True, False) -> False
                (False, True) -> step w' i1 (i2 + 1)
                (False, False) -> step w' i1 i2
