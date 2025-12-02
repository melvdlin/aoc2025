module Main where

import qualified Util (fetchInput, someFunc)

main :: IO ()
main = do
  putStrLn "AOC 2025 Day 01!"
  Util.someFunc
  input <- Util.fetchInput 2025 1 ""
  putStrLn (show input)
