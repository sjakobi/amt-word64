module Instances
  ( instanceTests
  ) where

import Amt.Word64.Map.Internal (Word64Map)
import Amt.Word64.Map.Internal qualified as MapInternal
import Data.Data (cast, dataTypeConstrs, dataTypeOf, gmapQi, toConstr)
import Data.Functor.Classes
  ( Eq1 (liftEq)
  , Ord1 (liftCompare)
  , Read1 (liftReadPrec)
  , Show1 (liftShowsPrec)
  )
import Data.Maybe (listToMaybe)
import Data.Proxy (Proxy (Proxy))
import MapProperties (K, fromKListInternal)
import Test.QuickCheck.Classes.Base
  ( Laws (Laws)
  , eqLaws
  , foldableLaws
  , functorLaws
  , isListLaws
  , monoidLaws
  , ordLaws
  , semigroupLaws
  , showLaws
  , showReadLaws
  , traversableLaws
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, property, testProperty)
import Text.Read (readListPrec, readPrec, readPrec_to_S)

instanceTests :: TestTree
instanceTests =
  testGroup
    "instance laws"
    [ lawsToTestTree (eqLaws (Proxy :: Proxy (Word64Map Int)))
    , lawsToTestTree (ordLaws (Proxy :: Proxy (Word64Map Int)))
    , lawsToTestTree (showLaws (Proxy :: Proxy (Word64Map Int)))
    , lawsToTestTree (showReadLaws (Proxy :: Proxy (Word64Map Int)))
    , lawsToTestTree (semigroupLaws (Proxy :: Proxy (Word64Map Int)))
    , lawsToTestTree (monoidLaws (Proxy :: Proxy (Word64Map Int)))
    , lawsToTestTree (functorLaws (Proxy :: Proxy Word64Map))
    , lawsToTestTree (foldableLaws (Proxy :: Proxy Word64Map))
    , lawsToTestTree (traversableLaws (Proxy :: Proxy Word64Map))
    , lawsToTestTree (isListLaws (Proxy :: Proxy (Word64Map Int)))
    , testGroup
        "Data"
        [ testProperty "gmapQi roundtrip" prop_data_gmapQi
        , testProperty "toConstr is fromList" prop_data_toConstr
        ]
    , testGroup
        "Eq1/Ord1/Show1/Read1"
        [ testProperty "Eq1 liftEq matches Eq" prop_eq1_liftEq
        , testProperty "Ord1 liftCompare matches Ord" prop_ord1_liftCompare
        , testProperty "Show1 liftShowsPrec matches Show" prop_show1_matches_show
        , testProperty "Read1 liftReadPrec roundtrip" prop_read1_roundtrip
        ]
    ]

lawsToTestTree :: Laws -> TestTree
lawsToTestTree (Laws name props) =
  testGroup name $
    [ testProperty propName prop
    | (propName, prop) <- props
    ]

prop_data_gmapQi :: [(K, Int)] -> Property
prop_data_gmapQi xs =
  let m = fromKListInternal xs
   in property $ gmapQi 0 cast m == Just (MapInternal.toList m)

prop_data_toConstr :: [(K, Int)] -> Property
prop_data_toConstr xs =
  let m = fromKListInternal xs
      dt = dataTypeOf m
   in property $ Just (toConstr m) == listToMaybe (dataTypeConstrs dt)

prop_eq1_liftEq :: [(K, Int)] -> [(K, Int)] -> Property
prop_eq1_liftEq xs ys =
  let m1 = fromKListInternal xs
      m2 = fromKListInternal ys
   in property $ liftEq (==) m1 m2 == (m1 == m2)

prop_ord1_liftCompare :: [(K, Int)] -> [(K, Int)] -> Property
prop_ord1_liftCompare xs ys =
  let m1 = fromKListInternal xs
      m2 = fromKListInternal ys
   in property $ liftCompare compare m1 m2 == compare m1 m2

prop_show1_matches_show :: [(K, Int)] -> Property
prop_show1_matches_show xs =
  let m = fromKListInternal xs
      s1 = liftShowsPrec showsPrec showList 0 m ""
      s2 = showsPrec 0 m ""
   in property $ s1 == s2

prop_read1_roundtrip :: [(K, Int)] -> Property
prop_read1_roundtrip xs =
  let m = fromKListInternal xs
      s = showsPrec 0 m ""
      parses = readPrec_to_S (liftReadPrec readPrec readListPrec) 0 s
   in property $ any ((== m) . fst) parses
