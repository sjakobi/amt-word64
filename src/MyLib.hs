module MyLib
  ( Word64Map
  , empty
  , insert
  , lookup
  , valid
  ) where

import Data.Bits hiding (bit, shift)
import Data.Bits qualified as Bits
import Data.Foldable (toList)
import Data.Primitive.SmallArray
import Data.Word (Word64)
import Prelude hiding (lookup)

data Word64Map a
  = Branch !Bitmap !(SmallArray (Word64Map a))
  | Leaf !Word64 a

newtype Bitmap = BM Word64

data Index = NoIndex | Index !Bitmap !Int

type Shift = Int

valid :: Word64Map a -> Bool
valid = go 0 0
 where
  go shift prefix (Leaf k _) =
    let mask = if shift >= 64 then complement 0 else (1 `Bits.shiftL` shift) - 1
     in (k .&. mask) == prefix
  go shift prefix (Branch (BM bm) ary) =
    let children = toList ary
        bits = [i | i <- [0 .. 63], testBit bm i]
     in popCount bm == sizeofSmallArray ary
          && all
            (\(i, child) -> go (shift + 6) (prefix .|. (fromIntegral i `Bits.shiftL` shift)) child)
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
insert k v m = go 0 m
 where
  go shift (Leaf k' v')
    | k == k' = Leaf k v
    | otherwise = split shift k v k' v'
  go shift (Branch (BM bm) ary) =
    case index shift k (BM bm) of
      Index _ i ->
        let child = indexSmallArray ary i
            newChild = go (shift + 6) child
         in Branch (BM bm) (updateAt i newChild ary)
      NoIndex ->
        let bit = 1 `Bits.shiftL` fromIntegral ((k `Bits.shiftR` shift) .&. 0x3f)
            i = popCount (bm .&. (bit - 1))
         in Branch (BM (bm .|. bit)) (insertAt i (Leaf k v) ary)

  split shift k1 v1 k2 v2 =
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
            let child = split (shift + 6) k1 v1 k2 v2
                bm = 1 `Bits.shiftL` idx1
             in Branch (BM bm) (smallArrayFromList [child])

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
