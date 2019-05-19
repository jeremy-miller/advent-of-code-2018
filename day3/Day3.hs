module Main where

main :: IO ()
main = do
  claimLines <- lines <$> readFile "data/day3.txt"
  putStr "hello world"
