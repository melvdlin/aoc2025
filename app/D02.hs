module Main where

import qualified Util (someFunc)

main :: IO ()
main = do
  putStrLn "AOC 2025 Day 02!"
  Util.someFunc
