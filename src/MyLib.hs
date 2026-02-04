module MyLib
  ( Word64Map
  , empty
  , singleton
  , null
  , size
  , insert
  , delete
  , lookup
  , union
  , fromList
  , toList
  , valid
  ) where

import Data.Bits hiding (bit, shift)
import Data.Bits qualified as Bits
import Data.Foldable qualified as Foldable
import Data.Primitive.SmallArray
import Data.Word (Word64)
import Prelude hiding (lookup, null)

data Word64Map a
  = Branch !Bitmap !(SmallArray (Word64Map a))
  | Leaf !Word64 a

newtype Bitmap = BM Word64

data Index = NoIndex | Index !Bitmap !Int

type Shift = Int

valid :: Word64Map a -> Bool
valid = go True 0 0
 where
  go _ shift prefix (Leaf k _) =
    let mask = if shift >= 64 then complement 0 else (1 `Bits.shiftL` shift) - 1
     in (k .&. mask) == prefix
  go isRoot shift prefix (Branch (BM bm) ary) =
    let children = Foldable.toList ary
        bits = [i | i <- [0 .. 63], testBit bm i]
        n = sizeofSmallArray ary
        s = size (Branch (BM bm) ary)
     in popCount bm == n
          && (s == 0 || s >= 2)
          && (isRoot || bm /= 0)
          && all
            (\(i, child) -> go False (shift + 6) (prefix .|. (fromIntegral i `Bits.shiftL` shift)) child)
            (zip bits children)

index :: Shift -> Word64 -> Bitmap -> Index
index shift k (BM bm) =
  let bit = 1 `Bits.shiftL` fromIntegral ((k `Bits.shiftR` shift) .&. 0x3f)
      i = popCount (bm .&. (bit - 1))
   in if bm .&. bit == 0
        then NoIndex
        else Index (BM bit) i
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
size (Branch _ ary) = Foldable.sum (fmap size ary)

lookup :: Word64 -> Word64Map a -> Maybe a
lookup k = go 0
 where
  go _ (Leaf k' v)
    | k == k' = Just v
    | otherwise = Nothing
  go shift (Branch (BM bm) ary) =
    case index shift k (BM bm) of
      NoIndex -> Nothing
      Index _ i -> go (shift + 6) (indexSmallArray ary i)

insert :: Word64 -> a -> Word64Map a -> Word64Map a
insert k v m = case m of
  Branch (BM 0) _ -> singleton k v
  Leaf k' v'
    | k == k' -> Leaf k v
    | otherwise -> two 0 k v k' v'
  Branch (BM bm) ary ->
    case index 0 k (BM bm) of
      Index _ i ->
        let child = indexSmallArray ary i
            newChild = insertAtShift 6 k v child
         in Branch (BM bm) (updateAt i newChild ary)
      NoIndex ->
        let bit = 1 `Bits.shiftL` fromIntegral (k .&. 0x3f)
            i = popCount (bm .&. (bit - 1))
         in Branch (BM (bm .|. bit)) (insertAt i (Leaf k v) ary)

insertAtShift :: Shift -> Word64 -> a -> Word64Map a -> Word64Map a
insertAtShift s k v m = case m of
  Leaf k' v'
    | k == k' -> Leaf k v
    | otherwise -> two s k v k' v'
  Branch (BM bm) ary ->
    case index s k (BM bm) of
      Index _ i ->
        let child = indexSmallArray ary i
            newChild = insertAtShift (s + 6) k v child
         in Branch (BM bm) (updateAt i newChild ary)
      NoIndex ->
        let bit = 1 `Bits.shiftL` fromIntegral ((k `Bits.shiftR` s) .&. 0x3f)
            i = popCount (bm .&. (bit - 1))
         in Branch (BM (bm .|. bit)) (insertAt i (Leaf k v) ary)

two :: Shift -> Word64 -> a -> Word64 -> a -> Word64Map a
two shift k1 v1 k2 v2 =
  let idx1 = fromIntegral ((k1 `Bits.shiftR` shift) .&. 0x3f)
      idx2 = fromIntegral ((k2 `Bits.shiftR` shift) .&. 0x3f)
   in if idx1 /= idx2
        then
          let bm = (1 `Bits.shiftL` idx1) .|. (1 `Bits.shiftL` idx2)
              ary =
                if idx1 < idx2
                  then smallArrayFromList [Leaf k1 v1, Leaf k2 v2]
                  else smallArrayFromList [Leaf k2 v2, Leaf k1 v1]
             in Branch (BM bm) ary
        else
          let child = two (shift + 6) k1 v1 k2 v2
              bm = 1 `Bits.shiftL` idx1
           in Branch (BM bm) (smallArrayFromList [child])

delete :: Word64 -> Word64Map a -> Word64Map a
delete k m = go 0 m
 where
  go _ (Leaf k' _) | k == k' = empty
  go _ leaf@(Leaf _ _) = leaf
  go shift (Branch (BM bm) ary) =
    case index shift k (BM bm) of
      NoIndex -> Branch (BM bm) ary
      Index (BM bit) i ->
        let child = indexSmallArray ary i
            newChild = go (shift + 6) child
         in if null newChild
              then
                let newBm = bm .&. complement bit
                 in case popCount newBm of
                      0 -> empty
                      1 ->
                        let remainingChild = indexSmallArray ary (if i == 0 then 1 else 0)
                         in if size remainingChild == 1 then remainingChild else Branch (BM newBm) (removeAt i ary)
                      _ -> Branch (BM newBm) (removeAt i ary)
              else
                if size newChild == 1 && popCount bm == 1
                  then newChild
                  else Branch (BM bm) (updateAt i newChild ary)

union :: Word64Map a -> Word64Map a -> Word64Map a
union m1 m2 = case (m1, m2) of
  (Branch (BM 0) _, _) -> m2
  (_, Branch (BM 0) _) -> m1
  (Leaf k1 v1, _) -> insert k1 v1 m2
  (_, Leaf k2 v2) -> insertIfNotExists k2 v2 m1
  (Branch (BM bm1) ary1, Branch (BM bm2) ary2) ->
    let newBm = bm1 .|. bm2
        bits = [b | b <- [0 .. 63], testBit newBm b]
        newAryList = flip map bits $ \b ->
          let bit = 1 `Bits.shiftL` b
              mIndex1 = if bm1 .&. bit /= 0 then Just (popCount (bm1 .&. (bit - 1))) else Nothing
              mIndex2 = if bm2 .&. bit /= 0 then Just (popCount (bm2 .&. (bit - 1))) else Nothing
           in case (mIndex1, mIndex2) of
                (Just i1, Just i2) -> unionAtShift 6 (indexSmallArray ary1 i1) (indexSmallArray ary2 i2)
                (Just i1, Nothing) -> indexSmallArray ary1 i1
                (Nothing, Just i2) -> indexSmallArray ary2 i2
                (Nothing, Nothing) -> error "union: impossible"
     in Branch (BM newBm) (smallArrayFromList newAryList)

unionAtShift :: Shift -> Word64Map a -> Word64Map a -> Word64Map a
unionAtShift shift m1 m2 = case (m1, m2) of
  (Leaf k1 v1, _) -> insertAtShift shift k1 v1 m2
  (_, Leaf k2 v2) -> insertIfNotExistsAtShift shift k2 v2 m1
  (Branch (BM 0) _, _) -> m2
  (_, Branch (BM 0) _) -> m1
  (Branch (BM bm1) ary1, Branch (BM bm2) ary2) ->
    let newBm = bm1 .|. bm2
        bits = [b | b <- [0 .. 63], testBit newBm b]
        newAryList = flip map bits $ \b ->
          let bit = 1 `Bits.shiftL` b
              mIndex1 = if bm1 .&. bit /= 0 then Just (popCount (bm1 .&. (bit - 1))) else Nothing
              mIndex2 = if bm2 .&. bit /= 0 then Just (popCount (bm2 .&. (bit - 1))) else Nothing
           in case (mIndex1, mIndex2) of
                (Just i1, Just i2) -> unionAtShift (shift + 6) (indexSmallArray ary1 i1) (indexSmallArray ary2 i2)
                (Just i1, Nothing) -> indexSmallArray ary1 i1
                (Nothing, Just i2) -> indexSmallArray ary2 i2
                (Nothing, Nothing) -> error "union: impossible"
     in Branch (BM newBm) (smallArrayFromList newAryList)

insertIfNotExists :: Word64 -> a -> Word64Map a -> Word64Map a
insertIfNotExists k v m = insertIfNotExistsAtShift 0 k v m

insertIfNotExistsAtShift :: Shift -> Word64 -> a -> Word64Map a -> Word64Map a
insertIfNotExistsAtShift shift k v m = case m of
  Leaf k' v'
    | k == k' -> m
    | otherwise -> two shift k v k' v'
  Branch (BM bm) ary ->
    case index shift k (BM bm) of
      Index _ i ->
        let child = indexSmallArray ary i
            newChild = insertIfNotExistsAtShift (shift + 6) k v child
         in Branch (BM bm) (updateAt i newChild ary)
      NoIndex ->
        let bit = 1 `Bits.shiftL` fromIntegral ((k `Bits.shiftR` shift) .&. 0x3f)
            i = popCount (bm .&. (bit - 1))
         in Branch (BM (bm .|. bit)) (insertAt i (Leaf k v) ary)

fromList :: [(Word64, a)] -> Word64Map a
fromList = Foldable.foldl' (\m (k, v) -> insert k v m) empty

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

removeAt :: Int -> SmallArray a -> SmallArray a
removeAt i ary = runSmallArray $ do
  let n = sizeofSmallArray ary
  mary <- newSmallArray (n - 1) (error "removeAt")
  copySmallArray mary 0 ary 0 i
  copySmallArray mary i ary (i + 1) (n - i - 1)
  return mary
