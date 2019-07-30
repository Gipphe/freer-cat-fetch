module FetchError
    ( FetchError(..)
    , showErr
    ) where

data FetchError
    = NoCat
    | NoEnv
    | InvalidCat

showErr :: FetchError -> String
showErr NoCat = "NoCat"
showErr NoEnv = "NoEnv"
showErr InvalidCat = "InvalidCat"