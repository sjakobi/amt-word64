{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module Amt.Word64.Internal.Bits
  ( Bitmap (..)
  , Index (..)
  , BitMatch (..)
  , Shift
  , ShiftBox (..)
  , shiftToInt
  , nextShift
  , shiftGE64
  , shiftToBox
  , lowBit
  , clearLowBit
  , index
  , indexMatch
  ) where

import Data.Bits hiding (bit, shift)
import Data.Word (Word64)
import GHC.Exts
  ( Int (I#)
  , Int#
  , (+#)
  , (>=#)
  )

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

{- | Return the lowest set bit.

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
