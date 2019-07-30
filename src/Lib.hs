module Lib
    ( freer
    , allInIO
    ) where

import Polysemy (runM, Sem, Members, interpret, Embed, embed)
import Polysemy.Input (Input(..), input)
import Polysemy.Trace (Trace, trace, traceToIO)
import Polysemy.Error (Error, throw, runError)
import Prelude hiding (trace)
import Control.Lens (view)
import qualified Network.Wreq as Wreq
import Data.Aeson (eitherDecode)
import Control.Exception (try)
import Data.Bifunctor (second)
import System.Environment (getEnv)

import CatResponse (getUrl)
import Effect.HTTP (HTTP)
import qualified Effect.HTTP as HTTP
import FetchError (FetchError(NoEnv), showErr)

allInIO :: IO ()
allInIO = do
    putStrLn "All in IO"
    url <- getEnv "API_URL"
    r <- Wreq.get url
    result <- pure
        . second toText
        . fmap getUrl
        . eitherDecode
        . view Wreq.responseBody
        $ r
    case result of
        Left e -> putStrLn $ "Error: " <> e
        Right url' -> putStrLn $ toString url'

freer :: IO ()
freer = do
    res <- runM
        . traceToIO
        . runError
        . HTTP.httpToIO
        . inputUrl
        $ fetch
    case res of
        Left e -> putStrLn . showErr $ e
        Right _ -> pure ()

fetch :: Members '[Input Text, HTTP, Trace] r => Sem r ()
fetch = do
    trace "Freer"
    url <- input
    r <- HTTP.get url
    eitherGif <- pure
        . fmap getUrl
        . eitherDecode
        $ r
    case eitherGif of
        Left _ -> trace "InvalidCat"
        Right gif -> trace . toString $ gif

inputUrl ::
    Members '[ Embed IO, Error FetchError] r
    => Sem (Input Text ': r) a
    -> Sem r a
inputUrl = interpret $ \case
    Input -> do
        eitherUrl <- embed . try @SomeException $ getEnv "API_URL"
        case eitherUrl of
            Left _ -> throw NoEnv
            Right url -> pure . toText $ url

