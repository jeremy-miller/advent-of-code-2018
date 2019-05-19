module Main where

-- Problem: https://adventofcode.com/2018/day/1#part2

type Frequency = Int
type FrequenciesSeen = [Frequency]
type UnparsedFrequencyLine = String
type UnparsedFrequencyLines = [UnparsedFrequencyLine]
data FrequencyLine = Add Frequency | Subtract Frequency deriving (Show)

parseLine :: UnparsedFrequencyLine -> FrequencyLine
parseLine ('+' : frequency) = Add $ read frequency
parseLine ('-' : frequency) = Subtract $ read frequency
parseLine _                 = error "Invalid line"

combine :: Frequency -> FrequencyLine -> Frequency
combine latestFreq (Add      change) = latestFreq + change
combine latestFreq (Subtract change) = latestFreq - change

checkForRepeatedFreq :: FrequenciesSeen -> UnparsedFrequencyLines -> Frequency
checkForRepeatedFreq []      _  = 0
checkForRepeatedFreq (_ : _) [] = 0
checkForRepeatedFreq freqSeen@(latestFreq : _) (line : rest)
  | newFreq `elem` freqSeen = newFreq
  | otherwise               = checkForRepeatedFreq (newFreq : freqSeen) rest
 where
  lineFreq = parseLine line
  newFreq  = combine latestFreq lineFreq

main :: IO ()
main = do
  file <- readFile "data/day1.txt"
  let frequencies       = lines file
  let startingFrequency = 0
  print $ checkForRepeatedFreq [startingFrequency] $ cycle frequencies  -- need to loop the list of frequencies
