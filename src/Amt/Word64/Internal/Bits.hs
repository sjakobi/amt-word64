{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module Amt.Word64.Internal.Bits
  ( Bitmap (..)
  , Index (..)
  , SlotState (..)
  , Shift
  , ShiftBox (..)
  , bitsPerLevel
  , subkeyMask
  , shiftToInt
  , nextShift
  , shiftGE64
  , shiftToBox
  , lowBit
  , clearLowBit
  , index
  , indexIfSlotPresent
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
data Index = Index !Bitmap !Int !SlotState

-- | Does the Bitmap contain the slot for the Word64 at the given Shift?
data SlotState = SlotEmpty | SlotPresent

-- | Unlifted shift counter in multiples of 'bitsPerLevel'.
type Shift = Int#

-- | Number of bits consumed per level.
bitsPerLevel :: Int
bitsPerLevel = 6
{-# INLINE bitsPerLevel #-}

-- | Mask for extracting the subkey/slot from a 'Word64' key.
subkeyMask :: Word64
subkeyMask = (1 `unsafeShiftL` bitsPerLevel) - 1
{-# INLINE subkeyMask #-}

-- | Boxed shift value for diagnostics and 'InvariantViolation' payloads.
newtype ShiftBox = ShiftBox Int
  deriving (Eq, Show)

shiftToInt :: Shift -> Int
shiftToInt s = I# s
{-# INLINE shiftToInt #-}

nextShift :: Shift -> Shift
nextShift s = case bitsPerLevel of
  I# b -> s +# b
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

{- | Given a 'Shift' representing the level of the tree, a 'Word64' key and the
'Bitmap' of a 'Branch' node, compute 'Index' into that node.

When the array index is only needed if the slot is present, use
'indexIfSlotPresent' instead.
-}
index :: Shift -> Word64 -> Bitmap -> Index
index shift !k (BM bm) =
  let slot = fromIntegral ((k `unsafeShiftR` shiftToInt shift) .&. subkeyMask)
      bit = 1 `unsafeShiftL` slot
      i = popCount (bm .&. (bit - 1))
      match = if bm .&. bit == 0 then SlotEmpty else SlotPresent
   in Index (BM bit) i match
{-# INLINE index #-}

{- | Like 'index', but only returns the array index when the slot bit is present.

This avoids a 'popCount' when the lookup misses.
-}
indexIfSlotPresent :: Shift -> Word64 -> Bitmap -> Maybe Int
indexIfSlotPresent shift !k (BM bm) =
  let slot = fromIntegral ((k `unsafeShiftR` shiftToInt shift) .&. subkeyMask)
      bit = 1 `unsafeShiftL` slot
   in if bm .&. bit == 0
        then Nothing
        else Just (popCount (bm .&. (bit - 1)))
{-# INLINE indexIfSlotPresent #-}
