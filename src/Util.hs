module Util (run) where

import Data.Attoparsec.ByteString (
    Parser,
    endOfInput,
    parseOnly,
 )
import Data.Attoparsec.ByteString.Char8 (skipSpace)
import GHC.Natural (Natural)
import qualified Input (getInput)
import System.Exit (die)
import Prelude hiding (takeWhile)

run ::
    (Show c, Show d) =>
    Natural ->
    Natural ->
    Parser a ->
    Parser b ->
    (a -> c) ->
    (b -> d) ->
    IO ()
run year day parser1 parser2 part1 part2 = do
    raw <- Input.getInput year day
    input1 <- case (parseOnly (parser1 <* skipSpace <* endOfInput) raw) of
        Left e -> die e
        Right i -> return i
    input2 <- case (parseOnly parser2 raw) of
        Left e -> die e
        Right i -> return i
    let solution1 = part1 input1
    let solution2 = part2 input2
    putStrLn $ "part 1:"
    putStrLn $ show solution1
    putStrLn $ "part 2:"
    putStrLn $ show solution2
    return ()