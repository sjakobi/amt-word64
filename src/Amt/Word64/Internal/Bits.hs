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
  , indexIfSlotOccupied
  ) where

import Data.Bits hiding (bit, shift)
import Data.Word (Word64)
import GHC.Exts
  ( Int (I#)
  , Int#
  , (+#)
  , (>=#)
  )

-- | A 'Bitmap' records which slots of a 'Branch' node are empty or occupied.
newtype Bitmap = BM Word64
  deriving (Eq)

{- | Bitmap query result.

Construct with 'index' when the array index is needed regardless of presence.
Otherwise it is more efficient to use 'indexIfSlotOccupied'.
-}
data Index = Index
  { indexBit :: !Bitmap
  -- ^ Singleton bitmap bit for the queried slot.
  , indexArrayIndex :: !Int
  -- ^ Position in the compact child array for this slot.
  , indexSlotState :: !SlotState
  -- ^ Whether the queried slot is occupied.
  }

-- | Is the slot for the given subkey empty or occupied?
data SlotState = SlotEmpty | SlotOccupied

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

When the array index is only needed if the slot is occupied, use
'indexIfSlotOccupied' instead.
-}
index :: Shift -> Word64 -> Bitmap -> Index
index shift !k (BM bm) =
  let slot = fromIntegral ((k `unsafeShiftR` shiftToInt shift) .&. subkeyMask)
      bit = 1 `unsafeShiftL` slot
      i = popCount (bm .&. (bit - 1))
      match = if bm .&. bit == 0 then SlotEmpty else SlotOccupied
   in Index (BM bit) i match
{-# INLINE index #-}

{- | Like 'index', but only returns the array index when the slot is occupied.

This avoids a 'popCount' when the lookup misses.
-}
indexIfSlotOccupied :: Shift -> Word64 -> Bitmap -> Maybe Int
indexIfSlotOccupied shift !k (BM bm) =
  let slot = fromIntegral ((k `unsafeShiftR` shiftToInt shift) .&. subkeyMask)
      bit = 1 `unsafeShiftL` slot
   in if bm .&. bit == 0
        then Nothing
        else Just (popCount (bm .&. (bit - 1)))
{-# INLINE indexIfSlotOccupied #-}
