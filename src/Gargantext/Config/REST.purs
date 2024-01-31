module Gargantext.Config.REST where

import Affjax.Web (Error(..), defaultRequest, request)
import Affjax as Affjax
import Affjax.RequestBody (formData, formURLEncoded, string)
import Affjax.RequestHeader as ARH
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut.Core as AC
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.FormURLEncoded as FormURLEncoded
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.MediaType.Common (applicationFormURLEncoded, applicationJSON, multipartFormData)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign as Foreign
import Gargantext.Prelude
import Gargantext.Utils.Reactix as R2
import Simple.JSON as JSON
import Web.XHR.FormData as XHRFormData

here :: R2.Here
here = R2.here "Gargantext.Config.REST"

type Token = String

data RESTError =
    CustomError        String
  | FE                 FrontendError
  | ReadJSONError      Foreign.MultipleErrors
  | SendResponseError  Affjax.Error
  | ServerError        String
  | UnknownServerError String
derive instance Generic RESTError _
instance Show RESTError where
  show (CustomError       s) = "CustomError " <> s
  show (FE                e) = show e
  show (ReadJSONError     e) = "ReadJSONError " <> show e
  show (SendResponseError e) = "SendResponseError " <> showError e
    where
      showError (ResponseBodyError fe _) = "(ResponseBodyError " <> show fe <> " (rf)"  -- <> show rf <> ")"
      showError (RequestContentError e') = "(RequestContentError " <> show e' <> ")"
      showError (RequestFailedError)     = "(RequestFailedError)"
      showError (TimeoutError)           = "(TimeoutError)"
      showError (XHROtherError e')       = "(XHROtherError " <> show e' <> ")"
  show (ServerError        e) = "ServerError: " <> e
  show (UnknownServerError e) = "UnknownServerError: " <> e
instance Eq RESTError where
  -- this is crude but we need it only because of useLoader
  eq _ _ = false


type AffRESTError a = Aff (Either RESTError a)


data FrontendError =
    EC_400__node_creation_failed_insert_node { user_id :: Int
                                             , parent_id :: Int }
  | EC_400__node_creation_failed_no_parent { user_id :: Int }
  | EC_400__node_creation_failed_parent_exists { parent_id :: Int
                                               , user_id :: Int }
  | EC_400__node_creation_failed_user_negative_id { user_id :: Int }
  | EC_400__node_lookup_failed_user_too_many_roots { user_id :: Int
                                                   , roots :: Array Int }
  | EC_400__node_needs_configuration
  | EC_403__login_failed_error { node_id :: Int
                               , user_id :: Int }
  | EC_403__login_failed_invalid_username_or_password
  | EC_403__user_not_authorized { user_id :: Int
                                , msg     :: String }
  | EC_404__node_context_not_found { context_id :: Int }
  | EC_404__node_lookup_failed_not_found { node_id :: Int }
  | EC_404__node_lookup_failed_parent_not_found { node_id :: Int }
  | EC_404__node_lookup_failed_username_not_found { username :: String }
  | EC_404__node_list_not_found { list_id :: Int }
  | EC_404__node_root_not_found
  | EC_500__node_generic_exception { error :: String }
  | EC_500__node_not_implemented_yet
derive instance Generic FrontendError _
instance Show FrontendError where
  show (EC_400__node_creation_failed_insert_node { user_id, parent_id }) =
    "Failed to insert node for user " <> show user_id <> ", parent " <> show parent_id
  show (EC_400__node_creation_failed_no_parent { user_id }) =
    "Failed to insert node for user " <> show user_id <> ": no parent"
  show (EC_400__node_creation_failed_parent_exists { user_id, parent_id }) =
    "Failed to insert node for user " <> show user_id <> ", parent " <> show parent_id <> " exists"
  show (EC_400__node_creation_failed_user_negative_id { user_id }) =
    "Failed to insert node for use " <> show user_id <> " (negative user_id)"
  show (EC_400__node_lookup_failed_user_too_many_roots { user_id, roots }) =
    "Failed to lookup node for user " <> show user_id <> ": too many roots (" <> show roots <> ")"
  show EC_400__node_needs_configuration = "Node needs configuration"
  show (EC_403__login_failed_error { node_id, user_id }) =
    "Login failed for node_id " <> show node_id <> ", user id " <> show user_id
  show EC_403__login_failed_invalid_username_or_password =
    "Invalid username or password"
  show (EC_403__user_not_authorized { user_id, msg }) =
    "User not authorized to perform action: " <> show user_id <> ". " <> msg
  show (EC_404__node_context_not_found { context_id }) =
    "Context not found with id " <> show context_id
  show (EC_404__node_lookup_failed_not_found { node_id }) =
    "Node not found with id " <> show node_id
  show (EC_404__node_lookup_failed_parent_not_found { node_id }) =
    "Node parent not found for id " <> show node_id
  show (EC_404__node_lookup_failed_username_not_found { username }) =
    "User '" <> username <> "' not found"
  show (EC_404__node_list_not_found { list_id }) =
    "Node list not found for id " <> show list_id
  show EC_404__node_root_not_found = "Node root not found"
  show (EC_500__node_generic_exception { error }) =
    "Node exception: " <> error
  show EC_500__node_not_implemented_yet = "Node not implemented yet"
instance JSON.ReadForeign FrontendError where
  readImpl f = do
    { type: type_ } <- JSON.readImpl f :: Foreign.F { type :: String }
    case type_ of
      "EC_400__node_creation_failed_insert_node" -> do
        { data: { parent_id, user_id } } <- JSON.readImpl f :: Foreign.F { data :: { parent_id :: Int
                                                                                   , user_id :: Int } }
        pure $ EC_400__node_creation_failed_insert_node { parent_id, user_id }
      "EC_400__node_creation_failed_no_parent" -> do
        { data: { user_id } } <- JSON.readImpl f :: Foreign.F { data :: { user_id :: Int } }
        pure $ EC_400__node_creation_failed_no_parent { user_id }
      "EC_400__node_creation_failed_parent_exists" -> do
        { data: { parent_id, user_id } } <- JSON.readImpl f :: Foreign.F { data :: { parent_id :: Int
                                                                                   , user_id :: Int } }
        pure $ EC_400__node_creation_failed_parent_exists { parent_id, user_id }
      "EC_400__node_creation_failed_user_negative_id" -> do
        { data: { user_id } } <- JSON.readImpl f :: Foreign.F { data :: { user_id :: Int } }
        pure $ EC_400__node_creation_failed_user_negative_id { user_id }
      "EC_400__node_lookup_failed_user_too_many_roots" -> do
        { data: { user_id, roots } } <- JSON.readImpl f :: Foreign.F { data :: { user_id :: Int
                                                                               , roots :: Array Int } }
        pure $ EC_400__node_lookup_failed_user_too_many_roots { user_id, roots }
      "EC_400__node_needs_configuration" -> do
        pure $ EC_400__node_needs_configuration
      "EC_403__login_failed_error" -> do
        { data: { node_id, user_id } } <- JSON.readImpl f :: Foreign.F { data :: { node_id :: Int
                                                                                 , user_id :: Int } }
        pure $ EC_403__login_failed_error { node_id, user_id }
      "EC_403__login_failed_invalid_username_or_password" -> do
        pure $ EC_403__login_failed_invalid_username_or_password
      "EC_403__user_not_authorized" -> do
        { data: { user_id, msg } } <- JSON.readImpl f :: Foreign.F { data :: { user_id :: Int
                                                                             , msg :: String } }
        pure $ EC_403__user_not_authorized { user_id, msg }
      "EC_404__node_context_not_found" -> do
        { data: { context_id } } <- JSON.readImpl f :: Foreign.F { data :: { context_id :: Int } }
        pure $ EC_404__node_context_not_found { context_id }
      "EC_404__node_lookup_failed_not_found" -> do
        { data: { node_id } } <- JSON.readImpl f :: Foreign.F { data :: { node_id :: Int } }
        pure $ EC_404__node_lookup_failed_not_found { node_id }
      "EC_404__node_lookup_failed_parent_not_found" -> do
        { data: { node_id } } <- JSON.readImpl f :: Foreign.F { data :: { node_id :: Int } }
        pure $ EC_404__node_lookup_failed_parent_not_found { node_id }
      "EC_404__node_lookup_failed_username_not_found" -> do
        { data: { username } } <- JSON.readImpl f :: Foreign.F { data :: { username :: String } }
        pure $ EC_404__node_lookup_failed_username_not_found { username }
      "EC_404__node_list_not_found" -> do
        { data: { list_id } } <- JSON.readImpl f :: Foreign.F { data :: { list_id :: Int } }
        pure $ EC_404__node_list_not_found { list_id }
      "EC_404__node_root_not_found" -> do
        pure $ EC_404__node_root_not_found
      "EC_500__node_generic_exception" -> do
        { data: { error } } <- JSON.readImpl f :: Foreign.F { data :: { error :: String } }
        pure $ EC_500__node_generic_exception { error }
      "EC_500__node_not_implemented_yet" -> do
        pure $ EC_500__node_not_implemented_yet
      _ -> do
        Foreign.fail $ Foreign.ForeignError $ "deserialization for '" <> type_ <> "' not implemented"

logRESTError :: R2.Here -> String -> RESTError -> Effect Unit
logRESTError here' prefix e = here'.warn2 (prefix <> " " <> show e) e
-- logRESTError here prefix (SendResponseError e) = here.warn2 (prefix <> " SendResponseError ") e  -- TODO: No show
-- logRESTError here prefix (ReadJSONError e) = here.warn2 (prefix <> " ReadJSONError ") $ show e
-- logRESTError here prefix (CustomError e) = here.warn2 (prefix <> " CustomError ") $ e


readJSON :: forall a. JSON.ReadForeign a
         => Either Affjax.Error (Affjax.Response AC.Json)
         -> Either RESTError a
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
      
      case resp.status of
        StatusCode 200 -> 
          case (JSON.readJSON $ AC.stringify resp.body) of
            Left err -> Left $ ReadJSONError err
            Right r -> Right r
        _ -> case (JSON.readJSON $ AC.stringify resp.body :: JSON.E FrontendError) of
          Right err -> Left $ FE err
          Left _    -> Left $ UnknownServerError $ AC.stringify resp.body


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
                      , ARH.RequestHeader "X-Garg-Error-Scheme" $ "new" 
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
  -- liftEffect $ here.log2 "[send] affResp" affResp
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
