{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.Attoparsec.ByteString (Parser, sepBy)
import Data.Attoparsec.ByteString.Char8 (char8, decimal, space)

import Util (run)

main :: IO ()
main = run 2025 9 parser parser part1 part2
type Input = [(Int, Int)]
type Point = (Int, Int)

parser :: Parser Input
parser = point `sepBy` space
  where
    point :: Parser Point
    point = (,) <$> decimal <* char8 ',' <*> decimal

part1 :: Input -> Int
part1 = maximum . map third . connect

part2 :: Input -> Int
part2 = const 0

area :: Point -> Point -> Int
area (x, y) (x', y') = (1 + abs (x - x')) * (1 + abs (y - y'))

connect :: [Point] -> [(Point, Point, Int)]
connect = concat . mapWithTail connections

connections :: Point -> [Point] -> [(Point, Point, Int)]
connections p = map (\q -> (p, q, area p q))

mapWithTail :: (Show a) => (a -> [a] -> b) -> [a] -> [b]
mapWithTail _ [] = []
mapWithTail f (x : xs) = f x xs : (mapWithTail f xs)

third :: (a, b, c) -> c
third (_, _, z) = z
