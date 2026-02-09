module StrictnessTooling
  ( isWhnfInt
  , mkThunk
  , tests
  ) where

import Data.Maybe (isNothing)
import GHC.Exts (lazy)
import NoThunks.Class (noThunks)
import Test.QuickCheck (Property)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (ioProperty, testProperty)

isWhnfInt :: Int -> IO Bool
isWhnfInt x = do
  info <- noThunks [] x
  pure (isNothing info)
{-# NOINLINE isWhnfInt #-}

tests :: TestTree
tests =
  testGroup
    "Tooling"
    [ testGroup
        "isWhnfInt"
        [ testProperty "undefined Int is not WHNF" $
            ioProperty $ do
              ok <- isWhnfInt (error "isWhnfInt: undefined")
              pure (not ok)
        , testProperty "mkThunk creates a thunk" $
            prop_mkThunk_lazy
        , testProperty "forced Int is WHNF" $
            prop_forced_int_whnf
        ]
    ]

prop_forced_int_whnf :: Int -> Property
prop_forced_int_whnf n = ioProperty $ do
  let x = n
  x `seq` isWhnfInt x

prop_mkThunk_lazy :: Int -> Property
prop_mkThunk_lazy n = ioProperty $ do
  ok <- isWhnfInt (mkThunk n)
  pure (not ok)

{-# NOINLINE mkThunk #-}
mkThunk :: Int -> Int
mkThunk ~x = lazy x
