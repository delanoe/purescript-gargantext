module Gargantext.Config.REST where

import Prelude

import Data.Argonaut (class DecodeJson, decodeJson)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.MediaType.Common (applicationJSON)

get :: forall t2 t31. DecodeJson t31 => String ->
                      Aff (Either String t31)
get url = do
  affResp <- liftAff $ attempt $ affjax defaultRequest
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
      let res = decodeJson a.response
      pure res
