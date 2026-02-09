module StrictnessTooling
  ( isWhnfInt
  , tests
  ) where

import Amt.Word64.Map.Strict qualified as Strict
import Control.DeepSeq (deepseq)
import Control.Exception (SomeException, try)
import Data.Maybe (isNothing)
import Data.Word (Word64)
import GHC.Exts (lazy)
import NoThunks.Class (ThunkInfo, noThunks)
import Test.QuickCheck (Property)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (expectFail)
import Test.Tasty.QuickCheck (ioProperty, testProperty)

isWhnfInt :: Int -> IO Bool
isWhnfInt x = do
  result <- try (noThunks [] x) :: IO (Either SomeException (Maybe ThunkInfo))
  pure $ case result of
    Left _ -> False
    Right info -> isNothing info

tests :: TestTree
tests =
  testGroup
    "Tooling"
    [ testProperty "undefined Int is not WHNF" $
        ioProperty $ do
          ok <- isWhnfInt (error "isWhnfInt: undefined")
          pure (not ok)
    , testProperty "forced Int is WHNF" $
        ioProperty $
          isWhnfInt (42 :: Int)
    , expectFail $
        testProperty "Strict.insert forces WHNF values" $
          prop_strict_insert_whnf_values
    ]

prop_strict_insert_whnf_values :: [(Word64, Int)] -> Word64 -> Int -> Property
prop_strict_insert_whnf_values entries k v = ioProperty $ do
  let m0 = Strict.fromList entries
  m0 `deepseq` pure ()
  let vThunk = mkThunk $ v + 1
  preOk <- isWhnfInt vThunk
  if preOk
    then error "Expected a thunk for Strict.insert input, but got WHNF"
    else do
      let m1 = Strict.insert k vThunk m0
      oks <- traverse (isWhnfInt . snd) (Strict.toList m1)
      pure (and oks)

{-# NOINLINE mkThunk #-}
mkThunk :: Int -> Int
mkThunk ~x = lazy x
