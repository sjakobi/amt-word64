{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main (main) where

import Amt.Word64.Set qualified as AmtSet
import Criterion.Main
import Data.Bits (shiftR, xor)
import Data.HashSet qualified as HashSet
import Data.Word (Word64)
import GHC.Data.Word64Set qualified as GHCSet

data Impl s = Impl
  { implName :: String
  , implEmpty :: s
  , implInsert :: Word64 -> s -> s
  , implMember :: Word64 -> s -> Bool
  }

data SomeImpl = forall s. SomeImpl (Impl s)

impls :: [SomeImpl]
impls =
  [ SomeImpl
      Impl
        { implName = "amt-word64"
        , implEmpty = AmtSet.empty
        , implInsert = AmtSet.insert
        , implMember = AmtSet.member
        }
  , SomeImpl
      Impl
        { implName = "ghc-word64set"
        , implEmpty = GHCSet.empty
        , implInsert = GHCSet.insert
        , implMember = GHCSet.member
        }
  , SomeImpl
      Impl
        { implName = "hashset"
        , implEmpty = HashSet.empty
        , implInsert = HashSet.insert
        , implMember = HashSet.member
        }
  ]

sizes :: [Int]
sizes = [0, 1, 5, 10, 100, 1000, 10000, 100000]

insertAll :: Impl s -> [Word64] -> s
insertAll impl = foldl' (flip (implInsert impl)) (implEmpty impl)

memberCount :: Impl s -> s -> [Word64] -> Int
memberCount impl set =
  foldl' (\acc k -> acc + fromEnum (implMember impl k set)) 0

contiguousKeys :: Int -> [Word64]
contiguousKeys n
  | n <= 0 = []
  | otherwise = [0 .. fromIntegral (n - 1)]

randomKeys :: Int -> [Word64]
randomKeys n = go n seed
 where
  seed = 0x243f6a8885a308d3
  go 0 _ = []
  go k s =
    let (!w, !s') = splitmix64 s
     in w : go (k - 1) s'

splitmix64 :: Word64 -> (Word64, Word64)
splitmix64 s =
  let s' = s + 0x9e3779b97f4a7c15
      z1 = (s' `xor` (s' `shiftR` 30)) * 0xbf58476d1ce4e5b9
      z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
      z3 = z2 `xor` (z2 `shiftR` 31)
   in (z3, s')

benchInsert :: [Word64] -> SomeImpl -> Benchmark
benchInsert keys (SomeImpl impl) =
  bench (implName impl) $ whnf (insertAll impl) keys

benchMember :: [Word64] -> SomeImpl -> Benchmark
benchMember keys (SomeImpl impl) =
  let !set = insertAll impl keys
   in bench (implName impl) $ nf (memberCount impl set) keys

benchKind :: String -> (Int -> [Word64]) -> Benchmark
benchKind label mkKeys =
  bgroup
    label
    [ bgroup (show n) (map (benchInsert keys) impls)
    | n <- sizes
    , let keys = mkKeys n
    ]

benchMemberKind :: String -> (Int -> [Word64]) -> Benchmark
benchMemberKind label mkKeys =
  bgroup
    label
    [ bgroup (show n) (map (benchMember keys) impls)
    | n <- sizes
    , let keys = mkKeys n
    ]

main :: IO ()
main =
  defaultMain
    [ bgroup
        "insert"
        [ benchKind "contiguous" contiguousKeys
        , benchKind "random" randomKeys
        ]
    , bgroup
        "member"
        [ benchMemberKind "contiguous" contiguousKeys
        , benchMemberKind "random" randomKeys
        ]
    ]
