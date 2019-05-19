module Main where

-- Problem: https://adventofcode.com/2018/day/2

import           Data.Map                      as M

type Accumulator = Int
type BoxID = String
type Count = Int

charCounts :: BoxID -> Map Char Count
charCounts boxID = M.fromListWith (+) [ (char, 1) | char <- boxID ]

twoCharCounts :: Accumulator -> BoxID -> Accumulator
twoCharCounts acc boxID | twosCount > 0 = acc + 1
                        | otherwise     = acc
 where
  counts    = charCounts boxID
  twosCount = length $ M.filter (== 2) counts

threesCharCounts :: Accumulator -> BoxID -> Accumulator
threesCharCounts acc boxID | threesCount > 0 = acc + 1
                           | otherwise       = acc
 where
  counts      = charCounts boxID
  threesCount = length $ M.filter (== 3) counts

main :: IO ()
main = do
  file <- readFile "data/day2.txt"
  let boxIDs      = lines file
  let twosCount   = Prelude.foldl twoCharCounts 0 boxIDs
  let threesCount = Prelude.foldl threesCharCounts 0 boxIDs
  print $ twosCount * threesCount
