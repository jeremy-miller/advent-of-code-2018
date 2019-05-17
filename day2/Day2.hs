module Main where

-- Problem: https://adventofcode.com/2018/day/2

import           Data.Map                      as M

type Acc = Int
type BoxID = String
type Count = Int

charCounts :: BoxID -> Map Char Count
charCounts boxID = M.fromListWith (+) [ (char, 1) | char <- boxID ]

twoCharCounts :: Acc -> BoxID -> Acc
twoCharCounts acc boxID | twosCount > 0 = acc + 1
                        | otherwise     = acc
  where twosCount = length $ M.filter (== 2) $ charCounts boxID

threesCharCounts :: Acc -> BoxID -> Acc
threesCharCounts acc boxID | twosCount > 0 = acc + 1
                           | otherwise     = acc
  where twosCount = length $ M.filter (== 3) $ charCounts boxID

main :: IO ()
main = do
  file <- readFile "data/box_ids.txt"
  let boxIDs      = lines file
  let twosCount   = Prelude.foldl twoCharCounts 0 boxIDs
  let threesCount = Prelude.foldl threesCharCounts 0 boxIDs
  print $ twosCount * threesCount
