{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.Attoparsec.ByteString (Parser, count, sepBy)
import Data.Attoparsec.ByteString.Char8 (char8, decimal, digit, skipSpace, space)

import Control.Applicative (Alternative (many), (<|>))
import Data.Attoparsec.Combinator (lookAhead)
import Data.Char (digitToInt)
import Data.Functor (($>))
import qualified Data.List as List
import Data.Maybe (catMaybes)
import Util (run)

main :: IO ()
main = run 2025 6 (parser1 @Int) (parser2 @Int) part1 part2

type Op a = (a, a -> a -> a)
type Input a = [([a], Op a)]

parser1 :: (Integral a) => Parser (Input a)
parser1 = do
    numbers' <- numbers `sepBy` space
    ops' <- ops
    return ((zip . List.transpose) numbers' ops')

parser2 :: (Integral a) => Parser (Input a)
parser2 = do
    widths <- lookAhead (((numbers @Int) `sepBy` space) *> width)
    rows <- (many (weirdNumbers widths))
    let cols = List.transpose rows
    let operands = map columnOperands cols
    operators <- ops
    return (operands `zip` operators)

numbers :: (Integral a) => Parser [a]
numbers = decimal `sepBy` many (char8 ' ')
ops :: (Integral a) => Parser [Op a]
ops = (add <|> mul) `sepBy` skipSpace
add :: (Integral a) => Parser (Op a)
add = char8 '+' $> (0, (+))
mul :: (Integral a) => Parser (Op a)
mul = char8 '*' $> (1, (*))

width :: Parser [Int]
width = many ((add @Int <|> mul) *> (length <$> many space))
weirdNumbers :: (Integral a) => [Int] -> Parser [[Maybe a]]
weirdNumbers = sequence . map digitsThenSpace
digitsThenSpace :: (Integral a) => Int -> Parser [Maybe a]
digitsThenSpace n = digits n <* space -- the space also consumes the line break
digits :: (Integral a) => Int -> Parser [Maybe a]
digits n = count n (Just <$> digit' <|> Nothing <$ space)
digit' :: (Integral a) => Parser a
digit' = fromIntegral . digitToInt <$> digit

columnOperands :: (Integral a) => [[Maybe a]] -> [a]
columnOperands = (map (concatDigits . catMaybes)) . List.transpose
concatDigits :: (Integral a) => [a] -> a
concatDigits = foldl concatTwo 0
concatTwo :: (Integral a) => a -> a -> a
concatTwo l r = l * 10 + r

part1 :: (Integral a) => Input a -> a
part1 = sum . map problem
  where
    problem :: (Integral a) => ([a], (a, a -> a -> a)) -> a
    problem (xs, (identity, op)) = foldl op identity xs

part2 :: (Integral a) => Input a -> a
part2 = part1

--- 123 328  51 64
---  45 64  387 23
---   6 98  215 314
--- *   +   *   +
---
--- [1 2 3] [3 2 8] [_ 5 1] [6 4 _]
--- [_ 4 5] [6 4 _] [3 8 7] [2 3 _]
--- [_ _ 6] [9 8 _] [2 1 5] [3 1 4]
--- *       +       *       +
---
--- transpose:
---
--- [1 2 3] [_ 4 5] [_ _ 6] *
--- [3 2 8] [6 4 _] [9 8 _] +
--- [_ 5 1] [3 8 7] [2 1 5] *
--- [6 4 _] [2 3 _] [3 1 4] +
---
--- inner transpose:
---
--- [1 _ _] [2 4 _] [3 5 6] *
--- [3 6 9] [2 4 8] [8 _ _] +
--- [_ 3 2] [5 8 1] [1 7 5] *
--- [6 2 3] [4 3 1] [_ _ 4] +