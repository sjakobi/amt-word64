module StrictnessTooling
  ( isWhnfInt
  , tests
  ) where

import Control.Exception (SomeException, try)
import Data.Maybe (isNothing)
import NoThunks.Class (ThunkInfo, noThunks)
import Test.QuickCheck (Property)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (ioProperty, testProperty)

isWhnfInt :: Int -> IO Bool
isWhnfInt x = do
  result <- try (noThunks [] x) :: IO (Either SomeException (Maybe ThunkInfo))
  pure $ case result of
    Left _ -> False
    Right info -> isNothing info
{-# NOINLINE isWhnfInt #-}

tests :: TestTree
tests =
  testGroup
    "Tooling"
    [ testProperty "undefined Int is not WHNF" $
        ioProperty $ do
          ok <- isWhnfInt (error "isWhnfInt: undefined")
          pure (not ok)
    , testProperty "forced Int is WHNF" $
        prop_forced_int_whnf
    ]

prop_forced_int_whnf :: Int -> Property
prop_forced_int_whnf n = ioProperty $ do
  let x = n
  x `seq` isWhnfInt x
