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

queryCount :: Int
queryCount = 10

presentSeedBase :: Word64
presentSeedBase = 0x243f6a8885a308d3

absentSeedBase :: Word64
absentSeedBase = 0x13198a2e03707344

seedMixConst :: Word64
seedMixConst = 0x9e3779b97f4a7c15

tagContiguous :: Word64
tagContiguous = 0xbf58476d1ce4e5b9

tagRandom :: Word64
tagRandom = 0x94d049bb133111eb

insertAllAmt :: [Word64] -> AmtSet.Word64Set
insertAllAmt = foldl' (flip AmtSet.insert) AmtSet.empty

insertAllGhc :: [Word64] -> GHCSet.Word64Set
insertAllGhc = foldl' (flip GHCSet.insert) GHCSet.empty

insertAllHash :: [Word64] -> HashSet.HashSet Word64
insertAllHash = foldl' (flip HashSet.insert) HashSet.empty

insertManyAmt :: AmtSet.Word64Set -> [Word64] -> AmtSet.Word64Set
insertManyAmt = foldl' (flip AmtSet.insert)

insertManyGhc :: GHCSet.Word64Set -> [Word64] -> GHCSet.Word64Set
insertManyGhc = foldl' (flip GHCSet.insert)

insertManyHash :: HashSet.HashSet Word64 -> [Word64] -> HashSet.HashSet Word64
insertManyHash = foldl' (flip HashSet.insert)

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

randomWords :: Word64 -> [Word64]
randomWords seed = go (mkSMGen seed)
 where
  go s =
    let (!w, !s') = nextWord64 s
     in w : go s'

seedFor :: Word64 -> Word64 -> Int -> Word64
seedFor base tag n =
  base + tag + seedMixConst * fromIntegral n

presentQueries :: Word64 -> [Word64]
presentQueries seed = take queryCount (randomWords seed)

absentQueries :: (Word64 -> Bool) -> Word64 -> [Word64]
absentQueries isMember seed =
  take queryCount $ filter (not . isMember) (randomWords seed)

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

benchMemberPresentKind :: String -> Word64 -> (Int -> [Word64]) -> Benchmark
benchMemberPresentKind label tag mkKeys =
  bgroup
    label
    [ let keys = mkKeys n
          queries = presentQueries (seedFor presentSeedBase tag n)
       in bgroup
            (show n)
            [ let !set = insertManyAmt (insertAllAmt keys) queries
               in bench "amt-word64" $ nf (memberCountAmt set) queries
            , let !set = insertManyGhc (insertAllGhc keys) queries
               in bench "ghc-word64set" $ nf (memberCountGhc set) queries
            , let !set = insertManyHash (insertAllHash keys) queries
               in bench "hashset" $ nf (memberCountHash set) queries
            ]
    | n <- sizes
    ]

benchMemberAbsentKind :: String -> Word64 -> (Int -> [Word64]) -> Benchmark
benchMemberAbsentKind label tag mkKeys =
  bgroup
    label
    [ let keys = mkKeys n
          baseAmt = insertAllAmt keys
          queries = absentQueries (`AmtSet.member` baseAmt) (seedFor absentSeedBase tag n)
       in bgroup
            (show n)
            [ let !set = baseAmt
               in bench "amt-word64" $ nf (memberCountAmt set) queries
            , let !set = insertAllGhc keys
               in bench "ghc-word64set" $ nf (memberCountGhc set) queries
            , let !set = insertAllHash keys
               in bench "hashset" $ nf (memberCountHash set) queries
            ]
    | n <- sizes
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
        [ bgroup
            "present"
            [ benchMemberPresentKind "contiguous" tagContiguous contiguousKeys
            , benchMemberPresentKind "random" tagRandom randomKeys
            ]
        , bgroup
            "absent"
            [ benchMemberAbsentKind "contiguous" tagContiguous contiguousKeys
            , benchMemberAbsentKind "random" tagRandom randomKeys
            ]
        ]
    ]
