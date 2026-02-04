module Main (main) where

import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import MyLib (empty, insert, lookup, valid)
import Test.QuickCheck
import Prelude hiding (lookup)

prop_lookup :: [(Word64, Int)] -> [Word64] -> Bool
prop_lookup entries keys =
  let myMap = foldl' (\m (k, v) -> insert k v m) empty entries
      refMap = Map.fromList entries
   in all (\k -> lookup k myMap == Map.lookup k refMap) (keys ++ map fst entries)

prop_valid :: [(Word64, Int)] -> Bool
prop_valid entries =
  let myMap = foldl' (\m (k, v) -> insert k v m) empty entries
   in valid myMap

main :: IO ()
main = do
  quickCheck prop_lookup
  quickCheck prop_valid
