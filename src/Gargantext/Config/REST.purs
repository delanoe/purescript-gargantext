module Gargantext.Config.REST where

import Affjax (defaultRequest, printError, request)
import Affjax.RequestBody (formData, formURLEncoded, string)
import Affjax.RequestHeader as ARH
import Affjax.ResponseFormat as ResponseFormat
import DOM.Simple.Console (log)
import Data.Argonaut.Core as AC
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.FormURLEncoded as FormURLEncoded
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.MediaType.Common (applicationFormURLEncoded, applicationJSON, multipartFormData)
import Data.Tuple (Tuple)
import Effect.Aff (Aff, throwError)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Simple.JSON as JSON
import Web.XHR.FormData as XHRFormData

import Gargantext.Prelude
import Gargantext.Utils.Reactix as R2

type Token = String

readJSONOrFail affResp =
  case affResp of
    Left err -> do
      _ <-  liftEffect $ log $ printError err
      throwError $ error $ printError err
    Right resp -> do
      --_ <-  liftEffect $ log json.status
      --_ <-  liftEffect $ log json.headers
      --_ <-  liftEffect $ log json.body
      case JSON.readJSON (AC.stringify resp.body) of
        Left err -> throwError $ error $ "decodeJson affResp.body: " <> show err
        Right b -> pure b

-- TODO too much duplicate code in `postWwwUrlencoded`
send :: forall body res. JSON.WriteForeign body => JSON.ReadForeign res =>
        Method -> Maybe Token -> String -> Maybe body -> Aff res
send m mtoken url reqbody = do
  let req = defaultRequest
         { url = url
         , responseFormat = ResponseFormat.json
         , method = Left m
         , headers =  [ ARH.ContentType applicationJSON
                      , ARH.Accept applicationJSON
                      ] <>
                      foldMap (\token ->
                        [ARH.RequestHeader "Authorization" $  "Bearer " <> token]
                      ) mtoken
         , content  = Just $ string $ JSON.writeJSON reqbody
         }
  case mtoken of
    Nothing -> pure unit
    Just token -> liftEffect $ do
      let cookie = "JWT-Cookie=" <> token <> "; Path=/;" --" HttpOnly; Secure; SameSite=Lax"
      R2.setCookie cookie
  affResp <- request req
  readJSONOrFail affResp

noReqBody :: Maybe String
noReqBody = Just ""
--noReqBody = Nothing

get :: forall a. JSON.ReadForeign a => Maybe Token -> String -> Aff a
get mtoken url = send GET mtoken url noReqBody

put :: forall a b. JSON.WriteForeign a => JSON.ReadForeign b => Maybe Token -> String -> a -> Aff b
put mtoken url = send PUT mtoken url <<< Just

put_ :: forall a. JSON.ReadForeign a => Maybe Token -> String -> Aff a
put_ mtoken url = send PUT mtoken url noReqBody

delete :: forall a. JSON.ReadForeign a => Maybe Token -> String -> Aff a
delete mtoken url = send DELETE mtoken url noReqBody

-- This might not be a good idea:
-- https://stackoverflow.com/questions/14323716/restful-alternatives-to-delete-request-body
deleteWithBody :: forall a b. JSON.WriteForeign a => JSON.ReadForeign b => Maybe Token -> String -> a -> Aff b
deleteWithBody mtoken url = send DELETE mtoken url <<< Just

post :: forall a b. JSON.WriteForeign a => JSON.ReadForeign b => Maybe Token -> String -> a -> Aff b
post mtoken url = send POST mtoken url <<< Just

type FormDataParams = Array (Tuple String (Maybe String))

-- TODO too much duplicate code with `send`
postWwwUrlencoded :: forall b. JSON.ReadForeign b => Maybe Token -> String -> FormDataParams -> Aff b
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
  readJSONOrFail affResp
  where
    urlEncodedBody = FormURLEncoded.fromArray bodyParams

postMultipartFormData :: forall b. JSON.ReadForeign b => Maybe Token -> String -> String -> Aff b
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
  readJSONOrFail affResp
