module MyLib
  ( Word64Map
  , empty
  , insert
  ) where

import Data.Word (Word64)
import Data.Primitive.SmallArray (SmallArray)

data Word64Map a =
    Branch Bitmap (SmallArray (Word64Map a))
  | Leaf !Word64 a

newtype Bitmap = BM Word64

empty :: Word64Map a
empty = Branch (BM 0) mempty
{-# noinline empty #-}

insert :: Word64 -> a -> Word64Map a -> Word64Map a
insert = undefined
