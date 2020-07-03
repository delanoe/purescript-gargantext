module Gargantext.Config.REST where

import Affjax (defaultRequest, printResponseFormatError, request)
import Affjax.RequestBody (RequestBody(..), formData, formURLEncoded)
import Affjax.RequestHeader as ARH
import Affjax.ResponseHeader as ARsH
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut (class DecodeJson, decodeJson, class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.FormURLEncoded as FormURLEncoded
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.MediaType.Common (applicationFormURLEncoded, applicationJSON, multipartFormData)
import Data.Tuple (Tuple(..))
import DOM.Simple.Console (log)
import Effect.Aff (Aff, throwError)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Web.XHR.FormData as XHRFormData

import Gargantext.Prelude
import Gargantext.Utils.Reactix as R2

type Token = String

-- TODO too much duplicate code in `postWwwUrlencoded`
send :: forall a b. EncodeJson a => DecodeJson b =>
        Method -> Maybe Token -> String -> Maybe a -> Aff b
send m mtoken url reqbody = do
  affResp <- request $ defaultRequest
         { url = url
         , responseFormat = ResponseFormat.json
         , method = Left m
         , headers =  [ ARH.ContentType applicationJSON
                      , ARH.Accept applicationJSON
                      ] <>
                      foldMap (\token ->
                        [ARH.RequestHeader "Authorization" $  "Bearer " <> token]
                      ) mtoken
         , content  = (Json <<< encodeJson) <$> reqbody
         }
  case mtoken of
    Nothing -> pure unit
    Just token -> liftEffect $ do
      let cookie = "JWT-Cookie=" <> token <> "; Path=/;" --" HttpOnly; Secure; SameSite=Lax"
      R2.setCookie cookie
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


sendWithRequest :: forall a b. EncodeJson a => DecodeJson b =>
        Method -> ARH.Request -> Maybe Token -> String -> Maybe a -> Aff (Tuple ARsH.Response b)
sendWithRequest m req mtoken url reqbody = do
  affResp <- request $ req
         { url = url
         , responseFormat = ResponseFormat.json
         , method = Left m
         , headers =  [ ARH.ContentType applicationJSON
                      , ARH.Accept applicationJSON
                      ]
                      <> req.headers
                      <> foldMap (\token ->
                        [ARH.RequestHeader "Authorization" $  "Bearer " <> token]
                      ) mtoken
         , content  = (Json <<< encodeJson) <$> reqbody
         }
  case mtoken of
    Nothing -> pure unit
    Just token -> liftEffect $ do
      let cookie = "JWT-Cookie=" <> token <> "; Path=/;" --" HttpOnly; Secure; SameSite=Lax"
      R2.setCookie cookie
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
        Right b -> pure $ Tuple affResp b

noReqBody :: Maybe Unit
noReqBody = Nothing

get :: forall a. DecodeJson a => Maybe Token -> String -> Aff a
get mtoken url = send GET mtoken url noReqBody

getH :: forall a. DecodeJson a => Maybe Token -> ARH.RequestHeader -> String -> Aff (Tuple ARsH.Response a)
getH mtoken headers url = sendWithRequest GET mtoken (ARH.defaultRequest { headers: headers }) url noReqBody

put :: forall a b. EncodeJson a => DecodeJson b => Maybe Token -> String -> a -> Aff b
put mtoken url = send PUT mtoken url <<< Just

put_ :: forall a. DecodeJson a => Maybe Token -> String -> Aff a
put_ mtoken url = send PUT mtoken url noReqBody

delete :: forall a. DecodeJson a => Maybe Token -> String -> Aff a
delete mtoken url = send DELETE mtoken url noReqBody

-- This might not be a good idea:
-- https://stackoverflow.com/questions/14323716/restful-alternatives-to-delete-request-body
deleteWithBody :: forall a b. EncodeJson a => DecodeJson b => Maybe Token -> String -> a -> Aff b
deleteWithBody mtoken url = send DELETE mtoken url <<< Just

post :: forall a b. EncodeJson a => DecodeJson b => Maybe Token -> String -> a -> Aff b
post mtoken url = send POST mtoken url <<< Just

type FormDataParams = Array (Tuple String (Maybe String))

-- TODO too much duplicate code with `send`
postWwwUrlencoded :: forall b. DecodeJson b => Maybe Token -> String -> FormDataParams -> Aff b
postWwwUrlencoded mtoken url bodyParams = do
  affResp <- request $ defaultRequest
             { url = url
             , responseFormat = ResponseFormat.json
             , method = Left POST
             , headers =  [ ARH.ContentType applicationFormURLEncoded
                          , ARH.Accept applicationJSON
                          ] <>
                          foldMap (\token ->
                            [ARH.RequestHeader "Authorization" $  "Bearer " <> token]
                          ) mtoken
             , content  = Just $ formURLEncoded urlEncodedBody
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
  where
    urlEncodedBody = FormURLEncoded.fromArray bodyParams

postMultipartFormData :: forall b. DecodeJson b => Maybe Token -> String -> String -> Aff b
postMultipartFormData mtoken url body = do
  fd <- liftEffect $ XHRFormData.new
  _ <- liftEffect $ XHRFormData.append (XHRFormData.EntryName "body") body fd
  affResp <- request $ defaultRequest
             { url = url
             , responseFormat = ResponseFormat.json
             , method = Left POST
             , headers = [ ARH.ContentType multipartFormData
                         , ARH.Accept applicationJSON
                         ] <>
                         foldMap (\token ->
                           [ ARH.RequestHeader "Authorization" $ "Bearer " <> token ]
                         ) mtoken
             , content = Just $ formData fd
             }
  case affResp.body of
    Left err -> do
      _ <-  liftEffect $ log $ printResponseFormatError err
      throwError $ error $ printResponseFormatError err
    Right json -> do
      case decodeJson json of
        Left err -> throwError $ error $ "decodeJson affResp.body: " <> err
        Right b -> pure b
