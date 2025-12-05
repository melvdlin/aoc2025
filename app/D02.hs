{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.Attoparsec.ByteString (Parser, endOfInput, sepBy)
import Data.Attoparsec.ByteString.Char8 (char8, decimal, skipSpace)
import Data.Int (Int64)
import Data.Set (Set)
import qualified Data.Set as Set

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
    part1' (lo, hi) = repetitions lo hi 2

part2 :: (Integral a, Show a) => [(a, a)] -> a
part2 = sum . fmap (sum . part2')
  where
    part2' :: (Integral a) => (a, a) -> Set a
    part2' (lo, hi) = (Set.fromList . concatMap (repetitions lo hi)) [2 .. digits hi]

repetitions :: (Integral a) => a -> a -> a -> [a]
repetitions lo hi n = (takeWhile (<= hi) . map (decimalRepeat n)) [next ..]
  where
    first = upperNth n lo
    next
        | lo <= decimalRepeat n first = first
        | otherwise = first + 1

decimalRepeat :: (Integral a) => a -> a -> a
decimalRepeat n d = decimalRepeat' n 0
  where
    decimalRepeat' n' acc
        | n' > 0 = decimalRepeat' (n' - 1) (acc + d * 10 ^ ((n' - 1) * digits d))
        | otherwise = acc

upperNth :: (Integral a) => a -> a -> a
upperNth n d
    | digits d `mod` n == 0 = d `div` (10 ^ ((digits d) - (`div` n) (digits d)))
    | otherwise = 10 ^ (digits d `div` n)

digits :: (Integral a) => a -> a
digits = succ . floor . logBase (10.0 :: Float) . fromIntegral
