module TestUtils
  ( K (..)
  , fromKListInternal
  , fromWord64
  , toWord64
  , toSortedListInternal
  ) where

import Amt.Word64.Map.Internal (Word64Map)
import Amt.Word64.Map.Internal qualified as MapInternal
import Data.List qualified as L
import Data.Word (Word64)
import Test.Tasty.QuickCheck (Arbitrary (arbitrary, shrink), getLarge)

newtype K = K Word64
  deriving (Eq, Ord, Show, Num, Integral, Real, Enum)

instance Arbitrary K where
  arbitrary = K . getLarge <$> arbitrary
  shrink (K w) = K <$> shrink w

instance Arbitrary a => Arbitrary (Word64Map a) where
  arbitrary = fromKListInternal <$> arbitrary
  shrink m = fromKListInternal <$> shrink (toSortedListInternal m)

toWord64 :: K -> Word64
toWord64 (K w) = w

fromWord64 :: Word64 -> K
fromWord64 = K

fromKListInternal :: [(K, a)] -> Word64Map a
fromKListInternal = MapInternal.fromList . L.map (\(k, v) -> (toWord64 k, v))

toSortedListInternal :: Word64Map a -> [(K, a)]
toSortedListInternal =
  L.sortOn fst
    . L.map (\(k, v) -> (fromWord64 k, v))
    . MapInternal.toList
