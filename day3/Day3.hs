{-# LANGUAGE RecordWildCards #-}

module Main where

-- Problem: https://adventofcode.com/2018/day/3

import           Data.Matrix                    ( Matrix
                                                , toList
                                                , setElem
                                                , zero
                                                )
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( Parsec
                                                , between
                                                )
import           Text.Megaparsec.Char           ( char
                                                , space
                                                )
import           Text.Megaparsec.Char.Lexer     ( decimal )
import           Prelude                 hiding ( id )

type Coordinate = (Int, Int)
type OverlappingClaims = Int
type Parser = Parsec Void String
type UnparsedLine = String

data Claim = Claim
  { id :: Int
  , coordinates :: [Coordinate]
  } deriving (Show)

parseLine :: Parser Claim
parseLine = do
  id   <- char '#' >> decimal
  minX <- charBetweenSpaces '@' >> (+ 1) <$> decimal
  minY <- charBetweenSpaces ',' >> (+ 1) <$> decimal
  maxX <- charBetweenSpaces ':' >> (+ (minX - 1)) <$> decimal
  maxY <- charBetweenSpaces 'x' >> (+ (minY - 1)) <$> decimal
  let coordinates = [ (x, y) | x <- [minX .. maxX], y <- [minY .. maxY] ]
  return Claim { .. }
  where charBetweenSpaces = between space space . char

coordinateUpdate :: Matrix Int -> Coordinate -> Matrix Int
coordinateUpdate fabric coordinate = setElem 1 coordinate fabric

claimUpdate :: Matrix Int -> Claim -> Matrix Int
claimUpdate fabric claim = map (coordinateUpdate fabric) coordinates claim

overlappingClaims :: [UnparsedLine] -> OverlappingClaims
overlappingClaims claimLines = foldl (+) 0 $ toList updatedFabric
 where
  parsedClaims  = map parseLine claimLines
  rows          = 1000
  columns       = 1000
  fabric        = zero rows columns
  updatedFabric = map (claimUpdate fabric) parsedClaims

main :: IO ()
main = do
  claimLines <- lines <$> readFile "data/day3.txt"
  print $ overlappingClaims claimLines
