{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.Attoparsec.ByteString (Parser, endOfInput, sepBy)
import Data.Attoparsec.ByteString.Char8 (char8, decimal, skipSpace, space)

import Util (run)

main :: IO ()
main = run 2025 5 (parser @Int) (parser @Int) part1 part2

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

part1 :: (Integral a) => Input a -> a
part1 (Input fresh available) =
    (fromIntegral . length . filter ((`any` fresh) . inRange)) available

inRange :: (Ord a) => a -> (a, a) -> Bool
inRange e (lo, hi) = lo <= e && e <= hi

part2 :: (Integral a) => Input a -> a
part2 (Input fresh _) = (sum . (map rangeLen) . (foldr insert [])) fresh

rangeLen :: (Integral a) => (a, a) -> a
rangeLen (lo, hi) = 1 + hi - lo

type RangeSet a = [(a, a)]

insert :: (Ord a) => (a, a) -> RangeSet a -> RangeSet a
insert x@(lo, hi) []
    | hi < lo = []
    | otherwise = [x]
insert x@(loX, hiX) (y@(loY, hiY) : ys)
    -- ()  [] -> ():[]
    | lt = x : y : ys
    -- []  () -> []:()
    | gt = y : (insert x ys)
    -- ([)]   -> (]
    | lOverlap = insert (loX, hiY) ys
    -- [(])   -> [)
    | rOverlap = insert (loY, hiX) ys
    -- [()]   -> []
    | subsumed = y : ys
    -- ([])   -> ()
    | subsumes = x : ys
    -- () empty
    | otherwise = y : ys
  where
    lt = hiX < loY
    gt = hiY < loX
    lOverlap = sorted [loX, loY, hiX, hiY]
    rOverlap = sorted [loY, loX, hiY, hiX]
    subsumed = sorted [loY, loX, hiX, hiY]
    subsumes = sorted [loX, loY, hiY, hiX]
    sorted [] = True
    sorted [_] = True
    sorted (x' : y' : zs') = (x' <= y') && sorted (y' : zs')
