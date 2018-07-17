module Gargantext.Config.REST where

import Prelude

import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Argonaut (class DecodeJson, decodeJson)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.MediaType.Common (applicationJSON)
import Network.HTTP.Affjax (AJAX, affjax, defaultRequest)
import Network.HTTP.RequestHeader (RequestHeader(..))

get :: forall eff t2 t31. DecodeJson t31 => String ->
                      Aff (console :: CONSOLE, ajax :: AJAX| eff)
                          (Either String t31)
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
