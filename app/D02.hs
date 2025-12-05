{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Attoparsec.ByteString (Parser, endOfInput, sepBy)
import Data.Attoparsec.ByteString.Char8 (char8, decimal, skipSpace)
import Data.Int (Int64)
import Util (run)

main :: IO ()
main = run 2025 2 (parser @Int64) part1 part2

parser :: (Integral a) => Parser [(a, a)]
parser = range `sepBy` char8 ',' <* skipSpace <* endOfInput
  where
    range :: (Integral a) => Parser (a, a)
    range = (,) <$> decimal <* (char8 '-') <*> decimal

part1 :: (Integral a) => [(a, a)] -> a
part1 = sum . concatMap part1'
  where
    part1' :: (Integral a) => (a, a) -> [a]
    part1' (lo, hi)
        | nextID lo <= hi = (nextID lo) : part1' (nextID lo + 1, hi)
        | otherwise = []

part2 :: (Integral a) => [(a, a)] -> a
part2 = const 0

digits :: (Integral a) => a -> a
digits = succ . floor . logBase (10.0 :: Float) . fromIntegral

nextID :: (Integral a) => a -> a
nextID n
    | (odd . digits) n = 10 ^ (digits n) + 10 ^ (`div` 2) (digits n)
    | n <= (decimalConcat . upperHalf) n = (decimalConcat . upperHalf) n
    | otherwise = (decimalConcat . (+ 1) . upperHalf) n

decimalConcat :: (Integral a) => a -> a
decimalConcat n = (decimalShiftLeft . digits) n n + n

lowerHalf :: (Integral a) => a -> a
lowerHalf n = n `mod` (10 ^ (`divCeil` 2) (digits n))

upperHalf :: (Integral a) => a -> a
upperHalf n = n `div` (10 ^ (`divCeil` 2) (digits n))

decimalShiftLeft :: (Integral a) => a -> a -> a
decimalShiftLeft = (*) . (10 ^)

decimalShiftRight :: (Integral a) => a -> a -> a
decimalShiftRight = (flip div) . (10 ^)

divCeil :: (Integral a) => a -> a -> a
divCeil = div . succ