module Amt.Word64.Map
  ( Word64Map
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

import Data.Bits hiding (bit, shift)
import Data.Bits qualified as Bits
import Data.Foldable qualified as Foldable
import Data.Primitive.SmallArray
import Data.Word (Word64)
import Prelude hiding (filter, lookup, map, null)

data InvariantViolation
  = PrefixMismatch
      { ivKey :: !Word64
      , ivShift :: !Shift
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

2. __No redundant branches__: A 'Branch' must have either exactly zero
   children (only if it is the root) or at least two children. If a branch
   would have only one child, that child must be collapsed upwards.

3. __Bitmap consistency__: The number of set bits in the 'Bitmap' must
   exactly match the size of the 'SmallArray'.

4. __Prefix consistency__: For any node at 'Shift' @s@, all keys in its
   subtree must share the same prefix for the bits more significant than @s@.
-}
data Word64Map a
  = Branch !Bitmap !(SmallArray (Word64Map a))
  | Leaf !Word64 a

instance Functor Word64Map where
  fmap f (Leaf k v) = Leaf k (f v)
  fmap f (Branch bm ary) = Branch bm (fmap (fmap f) ary)

instance Show a => Show (Word64Map a) where
  show m = "fromList " ++ show (toList m)

instance Foldable Word64Map where
  foldr f acc (Leaf _ v) = f v acc
  foldr f acc (Branch _ ary) = Foldable.foldr (flip (foldr f)) acc ary

  length = size

newtype Bitmap = BM Word64
  deriving (Show, Eq, Bits)

data Index = Index !Bitmap !Int !BitMatch

data BitMatch = NoMatch | Match

type Shift = Int

valid :: Word64Map a -> Maybe InvariantViolation
valid (Branch (BM 0) ary)
  | n == 0 = Nothing
  | otherwise = Just $ BitmapCountMismatch 0 n
 where
  n = sizeofSmallArray ary
valid t = validInternal 0 0 t

validInternal shift prefix (Leaf k _) =
  let mask = if shift >= 64 then complement 0 else (Bits.bit shift :: Word64) - 1
   in if (k .&. mask) == prefix
        then Nothing
        else Just $ PrefixMismatch k shift prefix
validInternal shift prefix (Branch (BM 0) ary) = Just UnexpectedEmptyBranch
validInternal shift prefix (Branch (BM bm) ary) =
  let children = Foldable.toList ary
      bits = [i | i <- [0 .. 63], testBit bm i]
      n = sizeofSmallArray ary
      s = size (Branch (BM bm) ary)
   in if popCount bm /= n
        then Just $ BitmapCountMismatch bm n
        else validSubtrees shift prefix bm ary

validSubtrees shift prefix bm ary
  | sizeofSmallArray ary == 1
  , Leaf{} <- indexSmallArray ary 0 =
      Just $ RedundantBranch prefix
  | otherwise = go
 where
  go =
    let children = Foldable.toList ary
        bits = [i | i <- [0 .. 63], testBit bm i]
     in Foldable.asum $
          zipWith
            ( \i child ->
                validInternal
                  (shift + 6)
                  (prefix .|. (fromIntegral i `Bits.shiftL` shift :: Word64))
                  child
            )
            bits
            children

index :: Shift -> Word64 -> Bitmap -> Index
index shift k (BM bm) =
  let bit = Bits.bit (fromIntegral ((k `Bits.shiftR` shift) .&. 0x3f))
      i = popCount (bm .&. (bit - 1))
      match = if bm .&. bit == 0 then NoMatch else Match
   in Index (BM bit) i match
{-# INLINE index #-}

empty :: Word64Map a
empty = Branch (BM 0) mempty
{-# NOINLINE empty #-}

singleton :: Word64 -> a -> Word64Map a
singleton = Leaf

null :: Word64Map a -> Bool
null (Branch (BM 0) _) = True
null _ = False

size :: Word64Map a -> Int
size (Leaf _ _) = 1
size (Branch _ ary) = Foldable.foldl' (\acc child -> acc + size child) 0 ary

insert :: Word64 -> a -> Word64Map a -> Word64Map a
insert k v (Branch (BM 0) _) = singleton k v
insert k v m = insertAtShift 0 k v m

insertWith :: (a -> a -> a) -> Word64 -> a -> Word64Map a -> Word64Map a
insertWith f k v m = insertWithKeyAtShift 0 (\_ new old -> f new old) k v m

insertWithKey ::
  (Word64 -> a -> a -> a) -> Word64 -> a -> Word64Map a -> Word64Map a
insertWithKey f k v m = insertWithKeyAtShift 0 f k v m

insertIfNotExists :: Word64 -> a -> Word64Map a -> Word64Map a
insertIfNotExists k v m = insertIfNotExistsAtShift 0 k v m

insertIfNotExistsAtShift :: Shift -> Word64 -> a -> Word64Map a -> Word64Map a
insertIfNotExistsAtShift shift k v m = case m of
  Leaf k' v'
    | k == k' -> Leaf k' v'
    | otherwise -> two shift k v k' v'
  Branch (BM bm) ary ->
    case index shift k (BM bm) of
      Index _ i Match ->
        let child = indexSmallArray ary i
            newChild = insertIfNotExistsAtShift (shift + 6) k v child
         in Branch (BM bm) (updateAt i newChild ary)
      Index (BM bit) i NoMatch ->
        Branch (BM (bm .|. bit)) (insertAt i (Leaf k v) ary)

-- | Only valid for internal nodes.
insertWithKeyAtShift ::
  Shift -> (Word64 -> a -> a -> a) -> Word64 -> a -> Word64Map a -> Word64Map a
insertWithKeyAtShift s f k v m = case m of
  Leaf k' v'
    | k == k' -> Leaf k (f k v v')
    | otherwise -> two s k v k' v'
  Branch (BM bm) ary ->
    case index s k (BM bm) of
      Index _ i Match ->
        let child = indexSmallArray ary i
            newChild = insertWithKeyAtShift (s + 6) f k v child
         in Branch (BM bm) (updateAt i newChild ary)
      Index (BM bit) i NoMatch ->
        Branch (BM (bm .|. bit)) (insertAt i (Leaf k v) ary)

-- | Only valid for internal nodes.
insertAtShift :: Shift -> Word64 -> a -> Word64Map a -> Word64Map a
insertAtShift s k v m = case m of
  Leaf k' v'
    | k == k' -> Leaf k v
    | otherwise -> two s k v k' v'
  Branch (BM bm) ary ->
    case index s k (BM bm) of
      Index _ i Match ->
        let child = indexSmallArray ary i
            newChild = insertAtShift (s + 6) k v child
         in Branch (BM bm) (updateAt i newChild ary)
      Index (BM bit) i NoMatch ->
        Branch (BM (bm .|. bit)) (insertAt i (Leaf k v) ary)

two :: Shift -> Word64 -> a -> Word64 -> a -> Word64Map a
two shift k1 v1 k2 v2 =
  let idx1 = fromIntegral ((k1 `Bits.shiftR` shift) .&. 0x3f)
      idx2 = fromIntegral ((k2 `Bits.shiftR` shift) .&. 0x3f)
   in if idx1 /= idx2
        then
          let bm = Bits.bit idx1 .|. Bits.bit idx2
              ary =
                if idx1 < idx2
                  then smallArrayFromList [Leaf k1 v1, Leaf k2 v2]
                  else smallArrayFromList [Leaf k2 v2, Leaf k1 v1]
           in Branch (BM bm) ary
        else
          let child = two (shift + 6) k1 v1 k2 v2
              bm = Bits.bit idx1
           in Branch (BM bm) (smallArrayFromList [child])

delete :: Word64 -> Word64Map a -> Word64Map a
delete = deleteAtShift 0

deleteAtShift :: Shift -> Word64 -> Word64Map a -> Word64Map a
deleteAtShift shift k m = go shift m
 where
  go _ (Leaf k' _) | k == k' = empty
  go _ (Leaf k' v') = Leaf k' v'
  go s (Branch (BM bm) ary) =
    case index s k (BM bm) of
      Index (BM bit) i Match ->
        let child = indexSmallArray ary i
            newChild = go (s + 6) child
            newBm = bm .&. complement bit
         in if null newChild
              then collapse (BM newBm) (removeAt i ary)
              else collapse (BM bm) (updateAt i newChild ary)
      Index _ _ NoMatch -> Branch (BM bm) ary

adjust :: (a -> a) -> Word64 -> Word64Map a -> Word64Map a
adjust f = adjustWithKey (\_ x -> f x)

adjustWithKey :: (Word64 -> a -> a) -> Word64 -> Word64Map a -> Word64Map a
adjustWithKey f k m = updateWithKey (\k' x -> Just (f k' x)) k m

update :: (a -> Maybe a) -> Word64 -> Word64Map a -> Word64Map a
update f = updateWithKey (\_ x -> f x)

updateWithKey ::
  (Word64 -> a -> Maybe a) -> Word64 -> Word64Map a -> Word64Map a
updateWithKey f k m = go 0 m
 where
  go _ (Leaf k' v)
    | k == k' = case f k v of
        Nothing -> empty
        Just v' -> Leaf k v'
    | otherwise = Leaf k' v
  go s (Branch (BM bm) ary) =
    case index s k (BM bm) of
      Index (BM bit) i Match ->
        let child = indexSmallArray ary i
            newChild = go (s + 6) child
            newBm = bm .&. complement bit
         in if null newChild
              then collapse (BM newBm) (removeAt i ary)
              else Branch (BM bm) (updateAt i newChild ary)
      Index _ _ NoMatch -> Branch (BM bm) ary

alter :: (Maybe a -> Maybe a) -> Word64 -> Word64Map a -> Word64Map a
alter f k m = go 0 m
 where
  go s (Leaf k' v')
    | k == k' = case f (Just v') of
        Nothing -> empty
        Just v'' -> Leaf k v''
    | otherwise = case f Nothing of
        Nothing -> Leaf k' v'
        Just v -> two s k v k' v'
  go s (Branch (BM bm) ary) =
    case index s k (BM bm) of
      Index (BM bit) i Match ->
        let child = indexSmallArray ary i
            newChild = go (s + 6) child
            newBm = bm .&. complement bit
         in if null newChild
              then collapse (BM newBm) (removeAt i ary)
              else Branch (BM bm) (updateAt i newChild ary)
      Index (BM bit) i NoMatch ->
        case f Nothing of
          Nothing -> Branch (BM bm) ary
          Just v -> Branch (BM (bm .|. bit)) (insertAt i (Leaf k v) ary)

lookup :: Word64 -> Word64Map a -> Maybe a
lookup k m = lookupAtShift 0 k m

lookupAtShift :: Shift -> Word64 -> Word64Map a -> Maybe a
lookupAtShift s k m = case m of
  Leaf k' v
    | k == k' -> Just v
    | otherwise -> Nothing
  Branch (BM bm) ary ->
    case index s k (BM bm) of
      Index _ i Match -> lookupAtShift (s + 6) k (indexSmallArray ary i)
      Index _ _ NoMatch -> Nothing

member :: Word64 -> Word64Map a -> Bool
member k m = case lookup k m of
  Nothing -> False
  Just _ -> True

notMember :: Word64 -> Word64Map a -> Bool
notMember k m = not (member k m)

findWithDefault :: a -> Word64 -> Word64Map a -> a
findWithDefault def k m = case lookup k m of
  Nothing -> def
  Just v -> v

map :: (a -> b) -> Word64Map a -> Word64Map b
map = fmap

mapWithKey :: (Word64 -> a -> b) -> Word64Map a -> Word64Map b
mapWithKey f (Leaf k v) = Leaf k (f k v)
mapWithKey f (Branch bm ary) = Branch bm (fmap (mapWithKey f) ary)

union :: Word64Map a -> Word64Map a -> Word64Map a
union m1 m2 = unionAtShift 0 m1 m2

unionAtShift :: Shift -> Word64Map a -> Word64Map a -> Word64Map a
unionAtShift shift m1 m2 = case (m1, m2) of
  (Branch (BM 0) _, _) -> m2
  (_, Branch (BM 0) _) -> m1
  (Leaf k1 v1, _) -> insertAtShift shift k1 v1 m2
  (_, Leaf k2 v2) -> insertIfNotExistsAtShift shift k2 v2 m1
  (Branch (BM bm1) ary1, Branch (BM bm2) ary2) ->
    let newBm = bm1 .|. bm2
        bits = [b | b <- [0 .. 63], testBit newBm b]
        newAryList = flip fmap bits $ \b ->
          let bit = Bits.bit b
              mIndex1 = if bm1 .&. bit /= 0 then Just (popCount (bm1 .&. (bit - 1))) else Nothing
              mIndex2 = if bm2 .&. bit /= 0 then Just (popCount (bm2 .&. (bit - 1))) else Nothing
           in case (mIndex1, mIndex2) of
                (Just i1, Just i2) -> unionAtShift (shift + 6) (indexSmallArray ary1 i1) (indexSmallArray ary2 i2)
                (Just i1, Nothing) -> indexSmallArray ary1 i1
                (Nothing, Just i2) -> indexSmallArray ary2 i2
                (Nothing, Nothing) -> error "union: impossible"
     in collapse (BM newBm) (smallArrayFromList newAryList)

unionWith :: (a -> a -> a) -> Word64Map a -> Word64Map a -> Word64Map a
unionWith f = unionWithKey (\_ x y -> f x y)

unionWithKey ::
  (Word64 -> a -> a -> a) -> Word64Map a -> Word64Map a -> Word64Map a
unionWithKey f m1 m2 = unionWithKeyAtShift 0 f m1 m2

unionWithKeyAtShift ::
  Shift -> (Word64 -> a -> a -> a) -> Word64Map a -> Word64Map a -> Word64Map a
unionWithKeyAtShift shift f m1 m2 = case (m1, m2) of
  (Branch (BM 0) _, _) -> m2
  (_, Branch (BM 0) _) -> m1
  (Leaf k1 v1, _) -> insertWithKeyAtShift shift f k1 v1 m2
  (_, Leaf k2 v2) -> insertWithKeyAtShift shift (\k new old -> f k old new) k2 v2 m1
  (Branch (BM bm1) ary1, Branch (BM bm2) ary2) ->
    let newBm = bm1 .|. bm2
        bits = [b | b <- [0 .. 63], testBit newBm b]
        newAryList = flip fmap bits $ \b ->
          let bit = Bits.bit b
              mIndex1 = if bm1 .&. bit /= 0 then Just (popCount (bm1 .&. (bit - 1))) else Nothing
              mIndex2 = if bm2 .&. bit /= 0 then Just (popCount (bm2 .&. (bit - 1))) else Nothing
           in case (mIndex1, mIndex2) of
                (Just i1, Just i2) ->
                  unionWithKeyAtShift
                    (shift + 6)
                    f
                    (indexSmallArray ary1 i1)
                    (indexSmallArray ary2 i2)
                (Just i1, Nothing) -> indexSmallArray ary1 i1
                (Nothing, Just i2) -> indexSmallArray ary2 i2
                (Nothing, Nothing) -> error "unionWithKey: impossible"
     in collapse (BM newBm) (smallArrayFromList newAryList)

fromList :: [(Word64, a)] -> Word64Map a
fromList = Foldable.foldl' (\acc (k, v) -> insert k v acc) empty

toList :: Word64Map a -> [(Word64, a)]
toList (Leaf k v) = [(k, v)]
toList (Branch _ ary) = Foldable.concatMap toList ary

elems :: Word64Map a -> [a]
elems = foldr (:) []

keys :: Word64Map a -> [Word64]
keys = foldrWithKey (\k _ ks -> k : ks) []

assocs :: Word64Map a -> [(Word64, a)]
assocs = toList

foldrWithKey :: (Word64 -> a -> b -> b) -> b -> Word64Map a -> b
foldrWithKey f acc (Leaf k v) = f k v acc
foldrWithKey f acc (Branch _ ary) = Foldable.foldr (flip (foldrWithKey f)) acc ary

foldlWithKey' :: (b -> Word64 -> a -> b) -> b -> Word64Map a -> b
foldlWithKey' f acc (Leaf k v) = f acc k v
foldlWithKey' f acc (Branch _ ary) = Foldable.foldl' (foldlWithKey' f) acc ary

removeAt :: Int -> SmallArray a -> SmallArray a
removeAt i ary =
  let n = sizeofSmallArray ary
   in runSmallArray $ do
        mary <- newSmallArray (n - 1) undefined
        copySmallArray mary 0 ary 0 i
        copySmallArray mary i ary (i + 1) (n - i - 1)
        return mary

insertAt :: Int -> a -> SmallArray a -> SmallArray a
insertAt i x ary =
  let n = sizeofSmallArray ary
   in runSmallArray $ do
        mary <- newSmallArray (n + 1) x
        copySmallArray mary 0 ary 0 i
        copySmallArray mary (i + 1) ary i (n - i)
        return mary

updateAt :: Int -> a -> SmallArray a -> SmallArray a
updateAt i x ary =
  let n = sizeofSmallArray ary
   in runSmallArray $ do
        mary <- newSmallArray n undefined
        copySmallArray mary 0 ary 0 n
        writeSmallArray mary i x
        return mary

collapse :: Bitmap -> SmallArray (Word64Map a) -> Word64Map a
collapse (BM 0) _ = empty
collapse (BM bm) ary
  | popCount bm == 1 =
      let child = indexSmallArray ary 0
       in case child of
            Leaf{} -> child
            _ -> Branch (BM bm) ary
  | otherwise = Branch (BM bm) ary

mergeWithKey ::
  (Word64 -> a -> b -> Maybe c) ->
  (Word64Map a -> Word64Map c) ->
  (Word64Map b -> Word64Map c) ->
  Word64Map a ->
  Word64Map b ->
  Word64Map c
mergeWithKey f g1 g2 m1_ m2_ = go (0 :: Shift) m1_ m2_
 where
  go _ (Branch (BM 0) _) m2 = g2 m2
  go _ m1 (Branch (BM 0) _) = g1 m1
  go shift (Leaf k1 v1) m2 = case lookupAtShift shift k1 m2 of
    Nothing -> g1 (Leaf k1 v1)
    Just v2 -> case f k1 v1 v2 of
      Nothing -> empty
      Just v3 -> Leaf k1 v3
  go shift m1 (Leaf k2 v2) = case lookupAtShift shift k2 m1 of
    Nothing -> g2 (Leaf k2 v2)
    Just v1 -> case f k2 v1 v2 of
      Nothing -> g1 rest
       where
        rest = deleteAtShift shift k2 m1
      Just v' -> insertAtShift shift k2 v' (g1 rest)
       where
        rest = deleteAtShift shift k2 m1
  go shift (Branch (BM bm1) ary1) (Branch (BM bm2) ary2) =
    let allBm = bm1 .|. bm2
        bits = [b | b <- [0 .. 63], testBit allBm b]
        pairs = flip fmap bits $ \b ->
          let bit = Bits.bit b
              mIndex1 = if bm1 .&. bit /= 0 then Just (popCount (bm1 .&. (bit - 1))) else Nothing
              mIndex2 = if bm2 .&. bit /= 0 then Just (popCount (bm2 .&. (bit - 1))) else Nothing
           in case (mIndex1, mIndex2) of
                (Just i1, Just i2) -> (b, go (shift + 6) (indexSmallArray ary1 i1) (indexSmallArray ary2 i2))
                (Just i1, Nothing) -> (b, g1 (indexSmallArray ary1 i1))
                (Nothing, Just i2) -> (b, g2 (indexSmallArray ary2 i2))
                (Nothing, Nothing) -> error "mergeWithKey: impossible"
        validPairs = [(b, r) | (b, r) <- pairs, not (null r)]
        newBm = Foldable.foldl' (\acc (b, _) -> acc .|. (Bits.bit b :: Word64)) 0 validPairs
        newAry = smallArrayFromList [r | (_, r) <- validPairs]
     in collapse (BM newBm) newAry

difference :: Word64Map a -> Word64Map b -> Word64Map a
difference m1 m2 = differenceWith (\_ _ -> Nothing) m1 m2

differenceWith ::
  (a -> b -> Maybe a) -> Word64Map a -> Word64Map b -> Word64Map a
differenceWith f m1_ m2_ = go (0 :: Shift) m1_ m2_
 where
  go _ (Branch (BM 0) _) _ = empty
  go _ m1 (Branch (BM 0) _) = m1
  go shift (Leaf k1 v1) m2 = case lookupAtShift shift k1 m2 of
    Nothing -> Leaf k1 v1
    Just v2 -> case f v1 v2 of
      Nothing -> empty
      Just v1' -> Leaf k1 v1'
  go shift m1 (Leaf k2 v2) = case lookupAtShift shift k2 m1 of
    Nothing -> m1
    Just v1 -> case f v1 v2 of
      Nothing -> deleteAtShift shift k2 m1
      Just v1' -> insertAtShift shift k2 v1' m1
  go shift (Branch (BM bm1) ary1) (Branch (BM bm2) ary2) =
    let bits1 = [b | b <- [0 .. 63], testBit bm1 b]
        pairs = flip fmap bits1 $ \b ->
          let bit = Bits.bit b
              i1 = popCount (bm1 .&. (bit - 1))
              mIndex2 = if bm2 .&. bit /= 0 then Just (popCount (bm2 .&. (bit - 1))) else Nothing
           in case mIndex2 of
                Nothing -> (b, indexSmallArray ary1 i1)
                Just i2 -> (b, go (shift + 6) (indexSmallArray ary1 i1) (indexSmallArray ary2 i2))
        validPairs = [(b, r) | (b, r) <- pairs, not (null r)]
        newBm = Foldable.foldl' (\acc (b, _) -> acc .|. (Bits.bit b :: Word64)) 0 validPairs
        newAry = smallArrayFromList [r | (_, r) <- validPairs]
     in collapse (BM newBm) newAry

intersection :: Word64Map a -> Word64Map b -> Word64Map a
intersection m1 m2 = intersectionWith (\x _ -> x) m1 m2

intersectionWith :: (a -> b -> c) -> Word64Map a -> Word64Map b -> Word64Map c
intersectionWith f = intersectionWithKey (\_ x y -> f x y)

intersectionWithKey ::
  (Word64 -> a -> b -> c) -> Word64Map a -> Word64Map b -> Word64Map c
intersectionWithKey f m1_ m2_ = go (0 :: Shift) m1_ m2_
 where
  go _ (Branch (BM 0) _) _ = empty
  go _ _ (Branch (BM 0) _) = empty
  go shift (Leaf k1 v1) m2 = case lookupAtShift shift k1 m2 of
    Nothing -> empty
    Just v2 -> Leaf k1 (f k1 v1 v2)
  go shift m1 (Leaf k2 v2) = case lookupAtShift shift k2 m1 of
    Nothing -> empty
    Just v1 -> Leaf k2 (f k2 v1 v2)
  go shift (Branch (BM bm1) ary1) (Branch (BM bm2) ary2) =
    let commonBm = bm1 .&. bm2
        bits = [b | b <- [0 .. 63], testBit commonBm b]
        pairs = flip fmap bits $ \b ->
          let bit = Bits.bit b
              i1 = popCount (bm1 .&. (bit - 1))
              i2 = popCount (bm2 .&. (bit - 1))
              child = go (shift + 6) (indexSmallArray ary1 i1) (indexSmallArray ary2 i2)
           in (b, child)
        validPairs = [(b, r) | (b, r) <- pairs, not (null r)]
        newBm = Foldable.foldl' (\acc (b, _) -> acc .|. (Bits.bit b :: Word64)) 0 validPairs
        newAry = smallArrayFromList [r | (_, r) <- validPairs]
     in collapse (BM newBm) newAry

filter :: (a -> Bool) -> Word64Map a -> Word64Map a
filter f = filterWithKey (\_ x -> f x)

filterWithKey :: (Word64 -> a -> Bool) -> Word64Map a -> Word64Map a
filterWithKey f m = go m
 where
  go (Leaf k v) = if f k v then Leaf k v else empty
  go (Branch (BM bm) ary) =
    let results = fmap go (Foldable.toList ary)
        bits = [b | b <- [0 .. 63], testBit bm b]
        pairs = [(b, r) | (b, r) <- zip bits results, not (null r)]
        newBm = Foldable.foldl' (\acc (b, _) -> acc .|. (Bits.bit b :: Word64)) 0 pairs
        newAry = smallArrayFromList [r | (_, r) <- pairs]
     in collapse (BM newBm) newAry

partition :: (a -> Bool) -> Word64Map a -> (Word64Map a, Word64Map a)
partition f = partitionWithKey (\_ x -> f x)

partitionWithKey ::
  (Word64 -> a -> Bool) -> Word64Map a -> (Word64Map a, Word64Map a)
partitionWithKey f m = go m
 where
  go (Leaf k v)
    | f k v = (Leaf k v, empty)
    | otherwise = (empty, Leaf k v)
  go (Branch (BM bm) ary) =
    let results = fmap go (Foldable.toList ary)
        bits = [b | b <- [0 .. 63], testBit bm b]
        (lRes, rRes) = unzip results
        lPairs = [(b, r) | (b, r) <- zip bits lRes, not (null r)]
        rPairs = [(b, r) | (b, r) <- zip bits rRes, not (null r)]
        lBm = Foldable.foldl' (\acc (b, _) -> acc .|. (Bits.bit b :: Word64)) 0 lPairs
        rBm = Foldable.foldl' (\acc (b, _) -> acc .|. (Bits.bit b :: Word64)) 0 rPairs
        lAry = smallArrayFromList [r | (_, r) <- lPairs]
        rAry = smallArrayFromList [r | (_, r) <- rPairs]
     in (collapse (BM lBm) lAry, collapse (BM rBm) rAry)

mapMaybe :: (a -> Maybe b) -> Word64Map a -> Word64Map b
mapMaybe f = mapMaybeWithKey (\_ x -> f x)

mapMaybeWithKey :: (Word64 -> a -> Maybe b) -> Word64Map a -> Word64Map b
mapMaybeWithKey f m = go m
 where
  go (Leaf k v) = case f k v of
    Nothing -> empty
    Just v' -> Leaf k v'
  go (Branch (BM bm) ary) =
    let results = fmap go (Foldable.toList ary)
        bits = [b | b <- [0 .. 63], testBit bm b]
        pairs = [(b, r) | (b, r) <- zip bits results, not (null r)]
        newBm = Foldable.foldl' (\acc (b, _) -> acc .|. (Bits.bit b :: Word64)) 0 pairs
        newAry = smallArrayFromList [r | (_, r) <- pairs]
     in collapse (BM newBm) newAry

mapEither :: (a -> Either b c) -> Word64Map a -> (Word64Map b, Word64Map c)
mapEither f = mapEitherWithKey (\_ x -> f x)

mapEitherWithKey ::
  (Word64 -> a -> Either b c) -> Word64Map a -> (Word64Map b, Word64Map c)
mapEitherWithKey f m = go m
 where
  go (Leaf k v) = case f k v of
    Left b -> (Leaf k b, empty)
    Right c -> (empty, Leaf k c)
  go (Branch (BM bm) ary) =
    let results = fmap go (Foldable.toList ary)
        bits = [b | b <- [0 .. 63], testBit bm b]
        (lRes, rRes) = unzip results
        lPairs = [(b, r) | (b, r) <- zip bits lRes, not (null r)]
        rPairs = [(b, r) | (b, r) <- zip bits rRes, not (null r)]
        lBm = Foldable.foldl' (\acc (b, _) -> acc .|. (Bits.bit b :: Word64)) 0 lPairs
        rBm = Foldable.foldl' (\acc (b, _) -> acc .|. (Bits.bit b :: Word64)) 0 rPairs
        lAry = smallArrayFromList [r | (_, r) <- lPairs]
        rAry = smallArrayFromList [r | (_, r) <- rPairs]
     in (collapse (BM lBm) lAry, collapse (BM rBm) rAry)

isSubmapOf :: Eq a => Word64Map a -> Word64Map a -> Bool
isSubmapOf = isSubmapOfBy (==)

isSubmapOfBy :: (a -> b -> Bool) -> Word64Map a -> Word64Map b -> Bool
isSubmapOfBy f m1_ m2_ = go (0 :: Shift) m1_ m2_
 where
  go _ (Branch (BM 0) _) _ = True
  go _ _ (Branch (BM 0) _) = False
  go shift (Leaf k1 v1) m2 = case lookupAtShift shift k1 m2 of
    Nothing -> False
    Just v2 -> f v1 v2
  go _ (Branch _ _) (Leaf _ _) = False
  go shift (Branch (BM bm1) ary1) (Branch (BM bm2) ary2) =
    let bits1 = [b | b <- [0 .. 63], testBit bm1 b]
     in all
          ( \b ->
              let bit = Bits.bit b
                  i1 = popCount (bm1 .&. (bit - 1))
                  mIndex2 = if bm2 .&. bit /= 0 then Just (popCount (bm2 .&. (bit - 1))) else Nothing
               in case mIndex2 of
                    Nothing -> False
                    Just i2 -> go (shift + 6) (indexSmallArray ary1 i1) (indexSmallArray ary2 i2)
          )
          bits1
