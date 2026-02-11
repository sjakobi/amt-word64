{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Amt.Word64.Set qualified as AmtSet
import Criterion.Main
import Data.HashSet qualified as HashSet
import Data.Word (Word64)
import GHC.Data.Word64Set qualified as GHCSet
import System.Random.SplitMix (mkSMGen, nextWord64)

sizes :: [Int]
sizes = [0, 1, 5, 10, 100, 1000, 10000, 100000]

insertAllAmt :: [Word64] -> AmtSet.Word64Set
insertAllAmt = foldl' (flip AmtSet.insert) AmtSet.empty

insertAllGhc :: [Word64] -> GHCSet.Word64Set
insertAllGhc = foldl' (flip GHCSet.insert) GHCSet.empty

insertAllHash :: [Word64] -> HashSet.HashSet Word64
insertAllHash = foldl' (flip HashSet.insert) HashSet.empty

memberCountAmt :: AmtSet.Word64Set -> [Word64] -> Int
memberCountAmt set =
  foldl' (\acc k -> acc + fromEnum (AmtSet.member k set)) 0

memberCountGhc :: GHCSet.Word64Set -> [Word64] -> Int
memberCountGhc set =
  foldl' (\acc k -> acc + fromEnum (GHCSet.member k set)) 0

memberCountHash :: HashSet.HashSet Word64 -> [Word64] -> Int
memberCountHash set =
  foldl' (\acc k -> acc + fromEnum (HashSet.member k set)) 0

contiguousKeys :: Int -> [Word64]
contiguousKeys n
  | n <= 0 = []
  | otherwise = [0 .. fromIntegral (n - 1)]

randomKeys :: Int -> [Word64]
randomKeys n = go n (mkSMGen seed)
 where
  seed = 0x243f6a8885a308d3
  go 0 _ = []
  go k s =
    let (!w, !s') = nextWord64 s
     in w : go (k - 1) s'

benchInsertKind :: String -> (Int -> [Word64]) -> Benchmark
benchInsertKind label mkKeys =
  bgroup
    label
    [ bgroup
        (show n)
        [ bench "amt-word64" $ whnf insertAllAmt keys
        , bench "ghc-word64set" $ whnf insertAllGhc keys
        , bench "hashset" $ whnf insertAllHash keys
        ]
    | n <- sizes
    , let keys = mkKeys n
    ]

benchMemberKind :: String -> (Int -> [Word64]) -> Benchmark
benchMemberKind label mkKeys =
  bgroup
    label
    [ bgroup
        (show n)
        [ let !set = insertAllAmt keys
           in bench "amt-word64" $ nf (memberCountAmt set) keys
        , let !set = insertAllGhc keys
           in bench "ghc-word64set" $ nf (memberCountGhc set) keys
        , let !set = insertAllHash keys
           in bench "hashset" $ nf (memberCountHash set) keys
        ]
    | n <- sizes
    , let keys = mkKeys n
    ]

main :: IO ()
main =
  defaultMain
    [ bgroup
        "insert"
        [ benchInsertKind "contiguous" contiguousKeys
        , benchInsertKind "random" randomKeys
        ]
    , bgroup
        "member"
        [ benchMemberKind "contiguous" contiguousKeys
        , benchMemberKind "random" randomKeys
        ]
    ]
