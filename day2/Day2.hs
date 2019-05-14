module Main where

import           Control.Monad.State
import           Data.Foldable

type Frequency = Int
data FrequencyLine = Add Frequency | Subtract Frequency

parseLine :: String -> FrequencyLine
parseLine ('+' : frequency) = Add $ read frequency
parseLine ('-' : frequency) = Subtract $ read frequency
parseLine _                 = error "Invalid line"

combine :: FrequencyLine -> Frequency -> Frequency
combine (Add      x) n = n + x
combine (Subtract x) n = n - x

adjustSingleFrequency :: String -> State Frequency ()
adjustSingleFrequency line = do
  currentFrequency <- get
  let lineFrequency = parseLine line
  put (combine lineFrequency currentFrequency)

adjustFrequency :: [String] -> State Frequency ()
adjustFrequency = traverse_ adjustSingleFrequency

main :: IO ()
main = do
  file <- readFile "data/frequencies.txt"
  let frequencies       = lines file
  let startingFrequency = 0
  print $ execState (adjustFrequency frequencies) startingFrequency
