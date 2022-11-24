module Gargantext.Config.REST where

import Affjax.Web (Error(..), defaultRequest, request)
import Affjax as Affjax
import Affjax.RequestBody (formData, formURLEncoded, string)
import Affjax.RequestHeader as ARH
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Core as AC
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.FormURLEncoded as FormURLEncoded
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.MediaType.Common (applicationFormURLEncoded, applicationJSON, multipartFormData)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign as Foreign
import Gargantext.Prelude
import Gargantext.Utils.Reactix as R2
import Simple.JSON as JSON
import Web.XHR.FormData as XHRFormData

type Token = String

data RESTError =
    SendResponseError Affjax.Error
  | ReadJSONError     Foreign.MultipleErrors
  | CustomError       String
derive instance Generic RESTError _
instance Show RESTError where
  show (SendResponseError e) = "SendResponseError " <> showError e
    where
      showError (RequestContentError e')  = "(RequestContentError " <> show e' <> ")"
      showError (ResponseBodyError fe _) = "(ResponseBodyError " <> show fe <> " (rf)"  -- <> show rf <> ")"
      showError (TimeoutError)            = "(TimeoutError)"
      showError (RequestFailedError)      = "(RequestFailedError)"
      showError (XHROtherError e')        = "(XHROtherError " <> show e' <> ")"
  show (ReadJSONError     e)              = "ReadJSONError " <> show e
  show (CustomError       s)              = "CustomError " <> s
instance Eq RESTError where
  -- this is crude but we need it only because of useLoader
  eq _ _ = false

logRESTError :: R2.Here -> String -> RESTError -> Effect Unit
logRESTError here prefix (SendResponseError e) = here.warn2 (prefix <> " SendResponseError ") e  -- TODO: No show
logRESTError here prefix (ReadJSONError e) = here.warn2 (prefix <> " ReadJSONError ") $ show e
logRESTError here prefix (CustomError e) = here.warn2 (prefix <> " CustomError ") $ e

type AffRESTError a = Aff (Either RESTError a)


readJSON :: forall a b. JSON.ReadForeign a =>
            Either Affjax.Error
            { body :: AC.Json
            | b
            } -> Either RESTError a
readJSON affResp =
  case affResp of
    Left err -> do
      -- _ <- liftEffect $ log $ printError err
      --throwError $ error $ printError err
      Left $ SendResponseError err
    Right resp -> do
      --_ <-  liftEffect $ log json.status
      --_ <-  liftEffect $ log json.headers
      --_ <-  liftEffect $ log json.body
      case (JSON.readJSON $ AC.stringify resp.body) of
        Left err -> Left $ ReadJSONError err
        Right r -> Right r

-- TODO too much duplicate code in `postWwwUrlencoded`
send :: forall body res. JSON.WriteForeign body => JSON.ReadForeign res =>
        Method -> Maybe Token -> String -> Maybe body -> AffRESTError res
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
  pure $ readJSON affResp

noReqBody :: Maybe String
noReqBody = Just ""
--noReqBody = Nothing

get :: forall a. JSON.ReadForeign a => Maybe Token -> String -> AffRESTError a
get mtoken url = send GET mtoken url noReqBody

put :: forall a b. JSON.WriteForeign a => JSON.ReadForeign b => Maybe Token -> String -> a -> AffRESTError b
put mtoken url = send PUT mtoken url <<< Just

put_ :: forall a. JSON.ReadForeign a => Maybe Token -> String -> AffRESTError a
put_ mtoken url = send PUT mtoken url noReqBody

delete :: forall a. JSON.ReadForeign a => Maybe Token -> String -> AffRESTError a
delete mtoken url = send DELETE mtoken url noReqBody

-- This might not be a good idea:
-- https://stackoverflow.com/questions/14323716/restful-alternatives-to-delete-request-body
deleteWithBody :: forall a b. JSON.WriteForeign a => JSON.ReadForeign b => Maybe Token -> String -> a -> AffRESTError b
deleteWithBody mtoken url = send DELETE mtoken url <<< Just

post :: forall a b. JSON.WriteForeign a => JSON.ReadForeign b => Maybe Token -> String -> a -> AffRESTError b
post mtoken url = send POST mtoken url <<< Just

type FormDataParams = Array (Tuple String (Maybe String))

-- TODO too much duplicate code with `send`
postWwwUrlencoded :: forall b. JSON.ReadForeign b => Maybe Token -> String -> FormDataParams -> AffRESTError b
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
  pure $ readJSON affResp
  where
    urlEncodedBody = FormURLEncoded.fromArray bodyParams

postMultipartFormData :: forall b. JSON.ReadForeign b => Maybe Token -> String -> String -> AffRESTError b
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
                           [ ARH.RequestHeader "Authorization" $ " " <> token ]
                         ) mtoken
             , content = Just $ formData fd
             }
  pure $ readJSON affResp
