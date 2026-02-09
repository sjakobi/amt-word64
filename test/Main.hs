module Main (main) where

import Instances qualified
import MapProperties qualified
import MapStrictness qualified
import StrictnessTooling qualified
import Test.Tasty (defaultMain, localOption, testGroup)
import Test.Tasty.QuickCheck (QuickCheckMaxSize (QuickCheckMaxSize))
import Word64SetTests qualified as SetTests

main :: IO ()
main =
  defaultMain $
    testGroup
      "amt-word64 tests"
      [ testGroup
          "Map"
          [ localOption (QuickCheckMaxSize 500) MapProperties.word64MapTests
          , Instances.instanceTests
          ]
      , MapStrictness.tests
      , StrictnessTooling.tests
      , testGroup
          "Set"
          [ localOption (QuickCheckMaxSize 500) SetTests.tests
          , SetTests.instanceTests
          ]
      ]
