module Main where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString (
    Parser,
    sepBy,
 )
import Data.Attoparsec.ByteString.Char8 (
    char8,
    decimal,
    space,
 )
import Data.Functor (($>))
import Util (run)
import Prelude hiding (takeWhile)

main :: IO ()
main = run 2025 1 parser parser part1 part2

type Input = [Int]

parser :: Parser Input
parser = line `sepBy` space
  where
    line :: Parser Int
    line = (*) <$> (left <|> right) <*> decimal
    left :: Parser Int
    left = char8 'L' $> -1
    right :: Parser Int
    right = char8 'R' $> 1

part1 :: Input -> Int
part1 = fst . foldl part1' (0, 50)
  where
    part1' (zeroes, dial) turn = (zeroes', dial')
      where
        zeroes' = zeroes + if dial' == 0 then 1 else 0
        dial' = (dial + turn) `mod` 100

part2 :: Input -> Int
part2 = fst . foldl part2' (0, 50)
  where
    part2' (zeroes, dial) turn =
        (zeroes', dial')
      where
        zeroes'
            | turn > 0 = zeroes + (dial + turn) `div` 100
            | dial > 0 = zeroes + (dial + turn) `div` (-100) + 1
            | otherwise = zeroes + (dial + turn) `div` (-100)
        dial' = (dial + turn) `mod` 100
