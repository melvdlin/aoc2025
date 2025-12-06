{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.Attoparsec.ByteString (Parser, endOfInput, sepBy)
import Data.Attoparsec.ByteString.Char8 (char8, decimal, skipSpace, space)

import Control.Applicative ((<|>))
import qualified Data.List as List
import Util (run)

main :: IO ()
main = run 2025 6 (parser @Int) part1 part2

type Input a = [([a], a -> a -> a)]

parser :: (Integral a) => Parser (Input a)
parser = zip . List.transpose <$> numbers `sepBy` space <*> ops <* skipSpace <* endOfInput
  where
    numbers :: (Integral a) => Parser [a]
    numbers = decimal `sepBy` space
    ops :: (Integral a) => Parser [a -> a -> a]
    ops = (add <|> mul) `sepBy` space
    add :: (Integral a) => Parser (a -> a -> a)
    add = return (+) <* char8 '+'
    mul :: (Integral a) => Parser (a -> a -> a)
    mul = return (*) <* char8 '*'

part1 :: (Integral a, Show a) => Input a -> a
part1 = const 0

part2 :: (Integral a, Show a) => Input a -> a
part2 = const 0
