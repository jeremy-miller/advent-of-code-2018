module Main where

import           Control.Monad.State
import           Data.Foldable

data Frequency = Add Int | Subtract Int

parseLine :: String -> Frequency
parseLine ('+' : frequency) = Add $ read frequency
parseLine ('-' : frequency) = Subtract $ read frequency
parseLine _                 = error "Invalid line"

combine :: Frequency -> Int -> Int
combine (Add      x) n = n + x
combine (Subtract x) n = n - x

adjustSingleFrequency :: String -> State Int ()
adjustSingleFrequency line = do
  currentFrequency <- get
  let lineFrequency = parseLine line
  put (combine lineFrequency currentFrequency)

adjustFrequency :: [String] -> State Int ()
adjustFrequency = traverse_ adjustSingleFrequency

main :: IO ()
main = do
  f <- readFile "data/frequencies.txt"
  let frequencies = lines f
  print $ execState (adjustFrequency frequencies) 0
