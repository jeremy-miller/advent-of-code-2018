module Main where

-- Problem: https://adventofcode.com/2018/day/1

import           Control.Monad.State
import           Data.Foldable

type Frequency = Int
data FrequencyLine = Add Frequency | Subtract Frequency
type UnparsedFrequencyLine = String

parseLine :: UnparsedFrequencyLine -> FrequencyLine
parseLine ('+' : frequency) = Add $ read frequency
parseLine ('-' : frequency) = Subtract $ read frequency
parseLine _                 = error "Invalid line"

combine :: FrequencyLine -> Frequency -> Frequency
combine (Add      change) accumulated = accumulated + change
combine (Subtract change) accumulated = accumulated - change

adjustSingleFrequency :: UnparsedFrequencyLine -> State Frequency ()
adjustSingleFrequency line = do
  currentFrequency <- get
  let lineFrequency = parseLine line
  put (combine lineFrequency currentFrequency)

adjustFrequency :: [UnparsedFrequencyLine] -> State Frequency ()
adjustFrequency = traverse_ adjustSingleFrequency

main :: IO ()
main = do
  frequencies <- lines <$> readFile "data/day1.txt"
  let startingFrequency = 0
  print $ execState (adjustFrequency frequencies) startingFrequency
