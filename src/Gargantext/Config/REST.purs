module Gargantext.Config.REST where

import Prelude

import Affjax (defaultRequest, request)
import Affjax.RequestHeader (RequestHeader(..))
import Data.Argonaut (class DecodeJson, decodeJson)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.MediaType.Common (applicationJSON)
import Effect.Aff (Aff, attempt)
import Effect.Aff.Class (liftAff)

get :: forall t31. DecodeJson t31 => String ->
                      Aff (Either String t31)
get url = do
  affResp <- liftAff $ attempt $ request defaultRequest
    { method  = Left GET
    , url     = url
    , headers =  [ ContentType applicationJSON
                 , Accept applicationJSON
              --   , RequestHeader "Authorization" $  "Bearer " <> token
                 ]
    }
  case affResp of
    Left err -> do
      pure $ Left $ show err
    Right a -> do
      let res = decodeJson a.body
      pure res
