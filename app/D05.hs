{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.Attoparsec.ByteString (Parser, endOfInput, sepBy)
import Data.Attoparsec.ByteString.Char8 (char8, decimal, skipSpace, space)

import Util (run)

main :: IO ()
main = run 2025 5 (parser @Int) part1 part2

data Input a = Input [(a, a)] [a]

parser :: (Integral a) => Parser (Input a)
parser =
    Input
        <$> fresh `sepBy` space
        <* skipSpace
        <*> available `sepBy` space
        <* skipSpace
        <* endOfInput
  where
    fresh :: (Integral a) => Parser (a, a)
    fresh = (,) <$> decimal <* char8 '-' <*> decimal
    available = decimal

part1 :: (Integral a, Show a) => Input a -> a
part1 (Input fresh available) =
    (fromIntegral . length . filter ((`any` fresh) . inRange)) available

part2 :: (Integral a) => Input a -> a
part2 = const 0

inRange :: (Ord a) => a -> (a, a) -> Bool
inRange e (lo, hi) = lo <= e && e <= hi