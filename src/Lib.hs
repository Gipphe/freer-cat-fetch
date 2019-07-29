module Lib
    ( main
    ) where

import Control.Lens ((^.))
import Data.Text (Text)
import qualified Network.Wreq as Wreq
import CatResponse (getUrl, CatResponse)
import Data.Aeson (eitherDecode)
import Data.Bifunctor (second)
import System.Environment (getEnv)

allInIO :: IO ()
allInIO = do
    url <- getEnv "API_URL"
    r <- Wreq.get url
    let body = r ^. Wreq.responseBody
        (decoded :: Either String CatResponse) = eitherDecode body
        (withUrl :: Either String Text) = fmap getUrl decoded
        result = second toText $ withUrl
    case result of
        Left e -> putStrLn $ "Error: " <> e
        Right url' -> putStrLn $ toString url'

main :: IO ()
main = allInIO
