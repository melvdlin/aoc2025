{-# LANGUAGE OverloadedStrings #-}

module Input (getInput) where

import Args (
    ArgInput (FileInput, ImmediateInput),
    Options (Options),
    optionInfo,
    options,
    orEnv,
 )

import Control.Exception (catch, throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import GHC.Natural (Natural)
import Network.HTTP.Simple (
    addRequestHeader,
    getResponseBody,
    httpBS,
    parseRequest,
    setRequestSecure,
 )
import Options.Applicative
import System.Exit (die)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)

getInput :: Natural -> Natural -> IO BS.ByteString
getInput year day = do
    cli <- execParser (info options (optionInfo year day))
    args <- orEnv cli
    getInput' year day args
getInput' :: Natural -> Natural -> Options -> IO ByteString
getInput' _year day (Options (Just inputArg) _ cacheDir) = do
    input <- getArg inputArg
    _ <- maybeCacheInput day cacheDir input
    return input
getInput' year day (Options _ (Just tokenArg) Nothing) = do
    token <- getArg tokenArg
    input <- fetchInput year day token
    return input
getInput' year day (Options _ tokenArg (Just cacheDir)) = do
    cached <- loadCachedInput (inputFilePath day cacheDir)
    case cached of
        Just input -> return input
        Nothing -> case tokenArg of
            Nothing -> die "input not found in cache and no token was provided"
            Just tokenArg' -> do
                token <- getArg tokenArg'
                input <- fetchInput year day token
                cacheInput day cacheDir input
                return input
getInput' _ _ _ = die "no input was provided"

getArg :: ArgInput -> IO ByteString
getArg (ImmediateInput immediate) = return immediate
getArg (FileInput path) = BS.readFile path

fetchInput :: Natural -> Natural -> ByteString -> IO ByteString
fetchInput year day token = do
    let route = inputRoute year day
    baseRequest <- parseRequest route
    let request = (setRequestSecure True . addRequestHeader "Cookie" token) baseRequest
    getResponseBody <$> httpBS request

inputRoute :: Natural -> Natural -> String
inputRoute year day =
    "https://adventofcode.com/" <> show year <> "/day/" <> show day <> "/input"

maybeCacheInput :: Natural -> Maybe FilePath -> ByteString -> IO ()
maybeCacheInput _ Nothing _ = return ()
maybeCacheInput day (Just cacheDir) input = cacheInput day cacheDir input

cacheInput :: Natural -> FilePath -> ByteString -> IO ()
cacheInput day cacheDir input = BS.writeFile (inputFilePath day cacheDir) input

loadCachedInput :: FilePath -> IO (Maybe ByteString)
loadCachedInput path =
    catch
        (Just <$> BS.readFile path)
        except
  where
    except e = do
        () <- assertDoesNotExist e
        return Nothing
    assertDoesNotExist :: IOError -> IO ()
    assertDoesNotExist e
        | isDoesNotExistError e = return ()
        | otherwise = throwIO e

inputFilePath :: Natural -> FilePath -> FilePath
inputFilePath day inputDir =
    inputDir </> pad '0' 2 (show day)
  where
    pad :: Char -> Natural -> String -> String
    pad char n s
        | fromIntegral n > length s = pad char (n - 1) (char : s)
        | otherwise = s
