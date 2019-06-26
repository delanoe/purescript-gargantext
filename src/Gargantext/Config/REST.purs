module Gargantext.Config.REST where

import Gargantext.Prelude

import Affjax (defaultRequest, printResponseFormatError, request)
import Affjax.RequestBody (RequestBody(..), string)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut (class DecodeJson, decodeJson, class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.MediaType.Common (applicationFormURLEncoded, applicationJSON)
import Effect.Aff (Aff, throwError)
import Effect.Exception (error)

send :: forall a b. EncodeJson a => DecodeJson b =>
        Method -> String -> Maybe a -> Aff b
send m url reqbody = do
  affResp <- request $ defaultRequest
         { url = url
         , responseFormat = ResponseFormat.json
         , method = Left m
         , headers =  [ ContentType applicationJSON
                      , Accept applicationJSON
                        --   , RequestHeader "Authorization" $  "Bearer " <> token
                      ]
         , content  = (Json <<< encodeJson) <$> reqbody
         }
  case affResp.body of
    Left err -> do
      _ <-  logs $ printResponseFormatError err
      throwError $ error $ printResponseFormatError err
    Right json -> do
      --_ <-  logs $ show json.status
      --_ <-  logs $ show json.headers
      --_ <-  logs $ show json.body
      case decodeJson json of
        Left err -> throwError $ error $ "decodeJson affResp.body: " <> err
        Right b -> pure b

noReqBody :: Maybe Unit
noReqBody = Nothing

get :: forall a. DecodeJson a => String -> Aff a
get url = send GET url noReqBody

put :: forall a b. EncodeJson a => DecodeJson b => String -> a -> Aff b
put url = send PUT url <<< Just

delete :: forall a. DecodeJson a => String -> Aff a
delete url = send DELETE url noReqBody

-- This might not be a good idea:
-- https://stackoverflow.com/questions/14323716/restful-alternatives-to-delete-request-body
deleteWithBody :: forall a b. EncodeJson a => DecodeJson b => String -> a -> Aff b
deleteWithBody url = send DELETE url <<< Just

post :: forall a b. EncodeJson a => DecodeJson b => String -> a -> Aff b
post url = send POST url <<< Just

postWwwUrlencoded :: forall b. DecodeJson b => String -> String -> Aff b
postWwwUrlencoded url body = do
  affResp <- request $ defaultRequest
             { url = url
             , responseFormat = ResponseFormat.json
             , method = Left POST
             , headers =  [ ContentType applicationFormURLEncoded
                          , Accept applicationJSON
                          ]
             , content  = Just $ string body
             }
  case affResp.body of
    Left err -> do
      _ <-  logs $ printResponseFormatError err
      throwError $ error $ printResponseFormatError err
    Right json -> do
      --_ <-  logs $ show json.status
      --_ <-  logs $ show json.headers
      --_ <-  logs $ show json.body
      case decodeJson json of
        Left err -> throwError $ error $ "decodeJson affResp.body: " <> err
        Right b -> pure b
