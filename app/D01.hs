module Main where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString (
    Parser,
    endOfInput,
    sepBy,
 )
import Data.Attoparsec.ByteString.Char8 (
    char8,
    decimal,
    skipSpace,
    space,
 )

import Util (run)
import Prelude hiding (takeWhile)

main :: IO ()
main = run 2025 1 parser part1 part2

parser :: Parser [Int]
parser = (line `sepBy` space) <* skipSpace <* endOfInput
  where
    line :: Parser Int
    line = (*) <$> (left <|> right) <*> decimal
    left :: Parser Int
    left = char8 'L' *> return (-1)
    right :: Parser Int
    right = char8 'R' *> return 1

part1 :: [Int] -> Int
part1 = part1' 0 50
  where
    part1' zeroes _ [] = zeroes
    part1' zeroes dial (turn : turns) = part1' zeroes' dial' turns
      where
        dial' = (dial + turn) `mod` 100
        zeroes' = zeroes + if dial' == 0 then 1 else 0

part2 :: [Int] -> Int
part2 = const 0