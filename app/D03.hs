{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.Attoparsec.ByteString (Parser, endOfInput, sepBy)
import Data.Attoparsec.ByteString.Char8 (digit, skipSpace, space)

import Control.Applicative (many)
import Data.Char (digitToInt)
import Util (run)

main :: IO ()
main = run 2025 3 (parser @Int) part1 part2

type Input a = [[a]]

parser :: (Integral a) => Parser (Input a)
parser = (many decimalDigit) `sepBy` space <* skipSpace <* endOfInput

decimalDigit :: (Integral a) => Parser a
decimalDigit = fromIntegral . digitToInt <$> digit

part1 :: (Integral a) => Input a -> a
part1 = sum . fmap part1'
  where
    part1' :: (Integral a) => [a] -> a
    part1' cells = concatCells (largest [0, 0] cells)

part2 :: (Integral a) => Input a -> a
part2 = sum . fmap part2'
  where
    part2' :: (Integral a) => [a] -> a
    part2' cells = concatCells (largest (take 12 (repeat 0)) cells)

concatCells :: (Integral a) => [a] -> a
concatCells = foldl concatTwo 0
  where
    concatTwo cells cell = cells * 10 + cell

largest :: (Integral a) => [a] -> [a] -> [a]
largest (l : ls) (x : xs)
    | (length ls) > (length xs) = l : largest ls (x : xs)
    | otherwise = largest (replace (l : ls) x) xs
largest ls _ = ls

replace :: (Integral a) => [a] -> a -> [a]
replace [] _ = []
replace (x : xs) y
    | y > x = y : (map (const 0) xs)
    | otherwise = x : (replace xs y)
