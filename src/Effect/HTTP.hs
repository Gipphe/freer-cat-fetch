{-# LANGUAGE TemplateHaskell #-}

module Effect.HTTP
    ( HTTP
    , get
    , httpToIO
    ) where

import Polysemy (Sem, makeSem, Embed, interpret, embed, Members)
import Polysemy.Error (Error, throw)
import Prelude hiding (get, ByteString)
import qualified Network.Wreq as Wreq
import Control.Lens ((^.))
import Control.Exception (try)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client (HttpException)

import FetchError (FetchError(NoCat))

data HTTP m a where
    Get :: Text -> HTTP m ByteString

makeSem ''HTTP

httpToIO ::
    Members '[Embed IO, Error FetchError] r => Sem (HTTP ': r) a -> Sem r a
httpToIO = interpret $ \case
    Get url -> do
        er <- embed . try @HttpException . Wreq.get . toString $ url
        case er of
            Left _ -> throw NoCat
            Right r -> pure $ r ^. Wreq.responseBody