module Util (run) where

import Data.Attoparsec.ByteString (
    Parser,
    parseOnly,
 )
import GHC.Natural (Natural)
import qualified Input (getInput)
import System.Exit (die)
import Prelude hiding (takeWhile)

run ::
    (Show b, Show c) => Natural -> Natural -> Parser a -> (a -> b) -> (a -> c) -> IO ()
run year day parser part1 part2 = do
    raw <- Input.getInput year day
    input <- case (parseOnly parser raw) of
        Left e -> die e
        Right i -> return i
    let solution1 = part1 input
    let solution2 = part2 input
    putStrLn $ "part 1:"
    putStrLn $ show solution1
    putStrLn $ "part 2:"
    putStrLn $ show solution2
    return ()