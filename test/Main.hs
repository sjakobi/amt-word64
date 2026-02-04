module Main (main) where

import Test.QuickCheck
import qualified Data.Map.Strict as Map
import MyLib (empty, insert, lookup)
import Prelude hiding (lookup)
import Data.Word (Word64)

prop_lookup :: [(Word64, Int)] -> [Word64] -> Bool
prop_lookup entries keys =
  let myMap = foldl' (\m (k, v) -> insert k v m) empty entries
      refMap = Map.fromList entries
  in all (\k -> lookup k myMap == Map.lookup k refMap) (keys ++ map fst entries)

main :: IO ()
main = quickCheck prop_lookup
