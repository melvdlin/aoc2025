{-# LANGUAGE OverloadedStrings #-}

module Util (someFunc, fetchInput) where

import Data.ByteString.Char8 (ByteString, pack)
import Data.Functor ((<&>))
import Network.HTTP.Simple (addRequestHeader, getResponseBody, httpBS, parseRequest, setRequestSecure)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

inputRoute :: Int -> Int -> String
inputRoute year day =
    "https://adventofcode.com/" <> show year <> "/day/" <> show day <> "/input"

fetchInput :: Int -> Int -> String -> IO ByteString
fetchInput year day token = do
    let route = inputRoute year day
    request <-
        parseRequest route
            <&> setRequestSecure True
            <&> addRequestHeader "Cookie" (pack token)
    response <- httpBS request <&> getResponseBody
    pure response

-- let request =