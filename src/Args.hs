module Args (
    Options (..),
    ArgInput (..),
    orEnv,
    options,
    optionInfo,
    envInput,
    envToken,
    envCacheDir,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT), hoistMaybe)
import GHC.Natural (Natural)
import Options.Applicative
import System.Environment (lookupEnv)

data Options
    = Options
        -- input
        (Maybe ArgInput)
        -- token
        (Maybe ArgInput)
        -- cache dir
        (Maybe FilePath)

data ArgInput = ImmediateInput BS.ByteString | FileInput FilePath

orEnv :: Options -> IO Options
orEnv (Options inputArg tokenArg cacheDirArg) = do
    input <- runMaybeT $ hoistMaybe inputArg <|> envInput
    token <- runMaybeT $ hoistMaybe tokenArg <|> envToken
    cacheDir <- runMaybeT $ hoistMaybe cacheDirArg <|> envCacheDir

    return (Options input token cacheDir)

optionInfo :: Natural -> Natural -> InfoMod a
optionInfo year day = fullDesc <> progDesc ("Solution for AoC " <> show year <> " Day " <> show day)

options :: Parser Options
options =
    ( Options
        <$> optional parseInput
        <*> optional parseToken
        <*> optional parseCacheDir
    )
        <**> helper

parseInput :: Parser ArgInput
parseInput =
    FileInput
        <$> strOption
            (short 'f' <> long "input-file" <> help "the puzzle input file" <> metavar "PATH")

parseToken :: Parser ArgInput
parseToken =
    ImmediateInput
        <$> strOption
            (short 't' <> long "token" <> help "the AOC input token" <> metavar "TOKEN")
            <|> FileInput
        <$> strOption (long "token-file" <> help "the AOC input token file" <> metavar "PATH")

parseCacheDir :: Parser FilePath
parseCacheDir =
    strOption
        ( short 'c'
            <> long "cache-dir"
            <> help "the puzzle input cache directory"
            <> metavar "PATH"
        )

envInput :: MaybeT IO ArgInput
envInput = input <|> inputFile
  where
    input = ImmediateInput <$> BS8.pack <$> MaybeT (lookupEnv "AOC_INPUT")
    inputFile = FileInput <$> MaybeT (lookupEnv "AOC_INPUT_FILE")

envToken :: MaybeT IO ArgInput
envToken = token <|> tokenFile
  where
    token = ImmediateInput <$> BS8.pack <$> MaybeT (lookupEnv "AOC_TOKEN")
    tokenFile = FileInput <$> MaybeT (lookupEnv "AOC_TOKEN_FILE")

envCacheDir :: MaybeT IO FilePath
envCacheDir = MaybeT (lookupEnv "AOC_CACHE_DIR")
