module Main where

-- Problem: https://adventofcode.com/2018/day/2#part2

import           Data.Maybe

type BoxID = String
type BoxIDLength = Int
type CommonBoxID = String

commonChars :: (BoxID, BoxID) -> CommonBoxID
commonChars (boxID1, boxID2) =
  catMaybes $ zipWith (\x y -> if x == y then Just x else Nothing) boxID1 boxID2

filterBoxIDs :: BoxIDLength -> [CommonBoxID] -> [CommonBoxID]
filterBoxIDs boxIDLength = filter (\x -> length x == (boxIDLength - 1))

commonBoxID :: [(BoxID, BoxID)] -> CommonBoxID
commonBoxID boxIDTuples = head $ filterBoxIDs boxIDLength $ map commonChars
                                                                boxIDTuples
  where boxIDLength = length $ fst $ head boxIDTuples

main :: IO ()
main = do
  boxIDs <- lines <$> readFile "data/day2.txt"
  let boxIDTuples = [ (id1, id2) | id1 <- boxIDs, id2 <- boxIDs ]
  print $ commonBoxID boxIDTuples
