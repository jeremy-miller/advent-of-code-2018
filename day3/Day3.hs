module Main where

-- Problem: https://adventofcode.com/2018/day/3

main :: IO ()
main = do
  claimLines <- lines <$> readFile "data/day3.txt"
  putStr "hello world"
