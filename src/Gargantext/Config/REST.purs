module Gargantext.Config.REST where

import Prelude

import Affjax (defaultRequest, printResponseFormatError, request)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut (class DecodeJson, decodeJson)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.MediaType.Common (applicationJSON)
import Effect.Aff (Aff)

get :: forall t31. DecodeJson t31 => String ->
                      Aff (Either String t31)
get url = do
  affResp <- request defaultRequest
    { method  = Left GET
    , url     = url
    , responseFormat = ResponseFormat.json
    , headers =  [ ContentType applicationJSON
                 , Accept applicationJSON
                   --   , RequestHeader "Authorization" $  "Bearer " <> token
                 ]
    }
  case affResp.body of
    Left err -> do
      pure $ Left $ printResponseFormatError err
    Right a -> do
      let res = decodeJson a
      pure res
