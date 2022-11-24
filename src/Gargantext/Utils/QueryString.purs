module Gargantext.Utils.QueryString where

import Data.Array
import Data.Maybe
import Data.String.Common (joinWith)

import Gargantext.Prelude

queryParam :: forall a. Show a => String -> a -> String
queryParam key value = key <> "=" <> show value

queryParamS :: String -> String -> String
queryParamS key value = key <> "=" <> value

mQueryParam :: forall a. Show a => String -> Maybe a -> String
mQueryParam _ Nothing = ""
mQueryParam key (Just v) = queryParam key v

mQueryParamS :: forall a. String -> (a -> String) ->  Maybe a -> String
mQueryParamS _ _ Nothing = ""
mQueryParamS key mFunc (Just v) = queryParamS key $ mFunc v

mQueryParamS' :: String -> Maybe String -> String
mQueryParamS' _ Nothing = ""
mQueryParamS' key (Just v) = queryParamS key v

joinQueryStrings :: Array String -> String
joinQueryStrings qs =
  case uncons qs of
    Nothing -> ""
    Just { head, tail } -> "?" <> head <> (joinQS tail)
      where
        joinQS ys =
          case uncons ys of
            Nothing -> ""
            Just { tail: ys } -> "&" <> (joinWith "&" ys)
