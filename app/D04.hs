{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.Attoparsec.ByteString (Parser, endOfInput, sepBy)
import Data.Attoparsec.ByteString.Char8 (char8, skipSpace, space)

import Control.Applicative (many, (<|>))
import Data.Set (Set)
import qualified Data.Set as Set
import Util (run)

main :: IO ()
main = run 2025 4 (parser @Int) part1 part2

type Input a = Set (a, a)

parser :: (Integral a) => Parser (Input a)
parser = toSet <$> parseRow `sepBy` space <* skipSpace <* endOfInput
  where
    parseRow :: Parser [Bool]
    parseRow = many (empty <|> occupied)
      where
        empty = char8 '.' *> return False
        occupied = char8 '@' *> return True

    toSet :: (Integral a) => [[Bool]] -> Input a
    occupiedCells :: (Integral a) => [[Bool]] -> [(a, a)]
    cellsWithCoords :: (Integral a) => [[Bool]] -> [(Bool, (a, a))]
    gridWithCoords :: (Integral a) => [[Bool]] -> [[(Bool, (a, a))]]
    rowWithCoords :: (Integral a) => ([Bool], a) -> [(Bool, (a, a))]

    toSet = Set.fromDistinctAscList . occupiedCells
    occupiedCells = map snd . filter fst . cellsWithCoords
    cellsWithCoords = concat . gridWithCoords
    gridWithCoords l = rowWithCoords `map` (l `zip` [0 ..])
    rowWithCoords (l, i) = l `zip` (repeat i `zip` [0 ..])

adjacent :: (Integral a) => (a, a) -> [(a, a)]
adjacent (row, col) =
    [ (row', col')
    | row' <- [row - 1 .. row + 1]
    , col' <- [col - 1 .. col + 1]
    ]

part1 :: (Integral a, Show a) => Input a -> a
part1 = fromIntegral . length . reachableCells

part2 :: (Integral a) => Input a -> a
part2 grid = fromIntegral (length grid - length (unreachable grid))

unreachable :: (Integral a) => Input a -> Input a
unreachable = fixpoint removeReachable

removeReachable :: (Integral a) => Input a -> Input a
removeReachable grid = grid `Set.difference` reachableCells grid

reachableCells :: (Integral a) => Input a -> Input a
reachableCells grid = Set.filter (reachableCell grid) grid

reachableCell :: (Integral a) => Input a -> (a, a) -> Bool
reachableCell grid cell = length (filter (`Set.member` grid) (adjacent cell)) < 5

fixpoint :: (Eq b) => (b -> b) -> b -> b
fixpoint f x
    | f x /= x = fixpoint f (f x)
    | otherwise = x
