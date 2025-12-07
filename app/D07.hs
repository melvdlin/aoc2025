{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.Attoparsec.ByteString (Parser, sepBy)
import Data.Attoparsec.ByteString.Char8 (char8, space)

import Control.Applicative (Alternative (many), (<|>))
import Data.Functor (($>))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Word (Word8)
import Util (run)

main :: IO ()
main = run 2025 7 parser parser part1 part2
type Input = (Int, [IntSet])

parser :: Parser Input
parser = (,) <$> start <*> splitters `sepBy` space
  where
    start :: Parser Int
    start = length <$> many air <* char8 'S' <* many air
    splitters :: Parser IntSet
    splitters = IntSet.fromList . positions <$> many cell
    cell :: Parser Bool
    cell = splitter $> True <|> False <$ air
    air :: Parser Word8
    air = char8 '.'
    splitter :: Parser Word8
    splitter = char8 '^'
    positions :: [Bool] -> [Int]
    positions = map (fst) . (filter snd) . (zip [0 ..])

part1 :: Input -> Int
part1 = sum . map (IntMap.size) . snd . runBeam

part2 :: Input -> Int
part2 = sum . fst . runBeam

runBeam :: Input -> (IntMap Int, [IntMap Int])
runBeam (start', splitters') = foldl line (IntMap.singleton start' 1, []) splitters'

line :: (IntMap Int, [IntMap Int]) -> IntSet -> (IntMap Int, [IntMap Int])
line (beams', splits') splitters' = (beams splitters' beams', splits splitters' beams' : splits')
beams :: IntSet -> IntMap Int -> IntMap Int
beams splitters' = foldl coalesce IntMap.empty . map (beam splitters') . IntMap.toList
splits :: IntSet -> IntMap Int -> IntMap Int
splits splitters' = IntMap.map (+ 1) . filterKeys (`IntSet.member` splitters')
beam :: IntSet -> (Int, Int) -> IntMap Int
beam splitters' (b, n)
    | b `IntSet.member` splitters' = IntMap.fromList [(b - 1, n), (b + 1, n)]
    | otherwise = IntMap.singleton b n

coalesce :: IntMap Int -> IntMap Int -> IntMap Int
coalesce = IntMap.unionWith (+)

filterKeys :: (Int -> Bool) -> IntMap a -> IntMap a
filterKeys = IntMap.filterWithKey . flip . const
