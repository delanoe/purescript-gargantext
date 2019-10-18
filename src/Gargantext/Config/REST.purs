module Gargantext.Config.REST where

import Prelude (Unit, bind, pure, ($), (<$>), (<<<), (<>))

import Affjax (defaultRequest, printResponseFormatError, request)
import Affjax.RequestBody (RequestBody(..), string)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Effect.Class (liftEffect)
import Data.Argonaut (class DecodeJson, decodeJson, class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.MediaType.Common (applicationFormURLEncoded, applicationJSON)
import Data.Foldable (foldMap)
import DOM.Simple.Console (log)
import Effect.Aff (Aff, throwError)
import Effect.Exception (error)

type Token = String

-- TODO too much duplicate code in `postWwwUrlencoded`
send :: forall a b. EncodeJson a => DecodeJson b =>
        Method -> Maybe Token -> String -> Maybe a -> Aff b
send m mtoken url reqbody = do
  affResp <- request $ defaultRequest
         { url = url
         , responseFormat = ResponseFormat.json
         , method = Left m
         , headers =  [ ContentType applicationJSON
                      , Accept applicationJSON
                      ] <>
                      foldMap (\token ->
                        [RequestHeader "Authorization" $  "Bearer " <> token]
                      ) mtoken
         , content  = (Json <<< encodeJson) <$> reqbody
         }
  case affResp.body of
    Left err -> do
      _ <-  liftEffect $ log $ printResponseFormatError err
      throwError $ error $ printResponseFormatError err
    Right json -> do
      --_ <-  liftEffect $ log json.status
      --_ <-  liftEffect $ log json.headers
      --_ <-  liftEffect $ log json.body
      case decodeJson json of
        Left err -> throwError $ error $ "decodeJson affResp.body: " <> err
        Right b -> pure b

noReqBody :: Maybe Unit
noReqBody = Nothing

get :: forall a. DecodeJson a => Maybe Token -> String -> Aff a
get mtoken url = send GET mtoken url noReqBody

put :: forall a b. EncodeJson a => DecodeJson b => Maybe Token -> String -> a -> Aff b
put mtoken url = send PUT mtoken url <<< Just

delete :: forall a. DecodeJson a => Maybe Token -> String -> Aff a
delete mtoken url = send DELETE mtoken url noReqBody

-- This might not be a good idea:
-- https://stackoverflow.com/questions/14323716/restful-alternatives-to-delete-request-body
deleteWithBody :: forall a b. EncodeJson a => DecodeJson b => Maybe Token -> String -> a -> Aff b
deleteWithBody mtoken url = send DELETE mtoken url <<< Just

post :: forall a b. EncodeJson a => DecodeJson b => Maybe Token -> String -> a -> Aff b
post mtoken url = send POST mtoken url <<< Just

-- TODO too much duplicate code with `send`
postWwwUrlencoded :: forall b. DecodeJson b => Maybe Token -> String -> String -> Aff b
postWwwUrlencoded mtoken url body = do
  affResp <- request $ defaultRequest
             { url = url
             , responseFormat = ResponseFormat.json
             , method = Left POST
             , headers =  [ ContentType applicationFormURLEncoded
                          , Accept applicationJSON
                          ] <>
                          foldMap (\token ->
                            [RequestHeader "Authorization" $  "Bearer " <> token]
                          ) mtoken
             , content  = Just $ string body
             }
  case affResp.body of
    Left err -> do
      _ <-  liftEffect $ log $ printResponseFormatError err
      throwError $ error $ printResponseFormatError err
    Right json -> do
      --_ <- liftEffect $ log json.status
      --_ <- liftEffect $ log json.headers
      --_ <- liftEffect $ log json.body
      case decodeJson json of
        Left err -> throwError $ error $ "decodeJson affResp.body: " <> err
        Right b -> pure b
