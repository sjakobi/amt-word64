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
    , testProperty "mkThunk creates a thunk" $
        prop_mkThunk_lazy
    , testProperty "Strict.singleton forces WHNF values" $
        prop_strict_singleton_whnf
    , testProperty "Strict.fromList forces WHNF values" $
        prop_strict_fromList_whnf
    , testProperty "Strict.adjust forces WHNF values" $
        prop_strict_adjust_whnf
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
      oks <- traverse isWhnfInt m1
      pure (and oks)

prop_strict_singleton_whnf :: Word64 -> Int -> Property
prop_strict_singleton_whnf k v = ioProperty $ do
  let vThunk = mkThunk v
  preOk <- isWhnfInt vThunk
  if preOk
    then error "Expected a thunk for Strict.singleton input, but got WHNF"
    else do
      let m = Strict.singleton k vThunk
      m `seq` pure ()
      allWhnfMap m

prop_strict_fromList_whnf :: [(Word64, Int)] -> Property
prop_strict_fromList_whnf entries = ioProperty $ do
  let entriesThunk = fmap (\(k, v) -> (k, mkThunk v)) entries
  case entriesThunk of
    [] -> do
      let m = Strict.fromList entriesThunk
      m `seq` allWhnfMap m
    (_, v0) : _ -> do
      preOk <- isWhnfInt v0
      if preOk
        then error "Expected a thunk for Strict.fromList input, but got WHNF"
        else do
          let m = Strict.fromList entriesThunk
          m `seq` allWhnfMap m

prop_strict_adjust_whnf :: [(Word64, Int)] -> Word64 -> Int -> Property
prop_strict_adjust_whnf entries k v = ioProperty $ do
  let forceValue (k', v') = v' `seq` (k', v')
  let entriesWhnf = fmap forceValue entries
  let m0 = Strict.fromList ((k, v) : entriesWhnf)
  m0 `deepseq` pure ()
  let vThunk = mkThunk v
  preOk <- isWhnfInt vThunk
  if preOk
    then error "Expected a thunk for Strict.adjust result, but got WHNF"
    else do
      let m1 = Strict.adjust (\_ -> vThunk) k m0
      m1 `seq` allWhnfMap m1

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

allWhnfMap :: Strict.Word64Map Int -> IO Bool
allWhnfMap m = and <$> traverse isWhnfInt m
