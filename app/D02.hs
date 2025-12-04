module Main where

import qualified Input (getInput)

main :: IO ()
main = do
  input <- Input.getInput 2025 1
  putStrLn (show input)
