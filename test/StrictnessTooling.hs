module StrictnessTooling
  ( isWhnfInt
  , tests
  ) where

import Control.Exception (SomeException, try)
import Data.Maybe (isNothing)
import NoThunks.Class (ThunkInfo, noThunks)
import Test.Tasty (TestTree, testGroup)
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
    ]
