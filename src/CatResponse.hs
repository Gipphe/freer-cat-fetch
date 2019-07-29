{-# LANGUAGE TemplateHaskell #-}
module CatResponse
    ( CatResponse
    , getUrl
    ) where

import Data.Aeson (FromJSON)
import Data.Aeson.TH (defaultOptions, deriveFromJSON, fieldLabelModifier)
import GHC.Generics (Generic)
import Data.Text (Text)

data DataResp =
    DataResp
        { image_url :: Text
        }
    deriving (Generic)

instance FromJSON DataResp

data CatResponse =
    CatResponse
        { _data :: DataResp
        }
    deriving (Generic)

deriveFromJSON defaultOptions {fieldLabelModifier = drop 1} ''CatResponse

getUrl :: CatResponse -> Text
getUrl = image_url . _data