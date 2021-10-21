module Gargantext.Components.GraphQL.AffjaxSimpleJSONClient
  (AffjaxClient(..))
  where

import Prelude

import Affjax (Error(..), Response, URL, defaultRequest, printError, request)
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.HTTP.Method as Method
import Data.List.NonEmpty as DLN
import Data.Maybe (Maybe(..))
import Data.MediaType.Common (applicationJSON)
import Effect.Aff (Aff, error, throwError)
import Foreign (unsafeToForeign)
import GraphQL.Client.Types (class QueryClient)
import Simple.JSON as JSON

data AffjaxClient
  = AffjaxClient URL (Array RequestHeader)
-- 
-- instance queryClient :: QueryClient AffjaxClient Unit Unit where
--   clientQuery _ (AffjaxClient url headers) name q vars = throwLeft =<< convertJsonResponse =<< queryPostForeign "query" url headers name q vars
--   clientMutation _ (AffjaxClient url headers) name q vars = throwLeft =<< convertJsonResponse =<< queryPostForeign "mutation" url headers name q vars
--   defQueryOpts = const unit
--   defMutationOpts = const unit
-- 
-- throwLeft :: forall r body. Either Error { body :: body | r } -> Aff body
-- throwLeft = case _ of
--   Left err -> throwError $ error $ printError err
--   Right { body } -> pure body
-- 
-- queryPostForeign ::
--   forall d.
--   JSON.WriteForeign d =>
--   String -> URL -> Array RequestHeader -> String -> String -> d -> Aff (Either Error (Response String))
-- queryPostForeign opStr url headers queryName q vars = do
--   request
--     defaultRequest
--       { withCredentials = true
--       , url = url
--       , method = Left Method.POST
--       --, responseFormat = ResponseFormat.json
--       , responseFormat = ResponseFormat.string
--       , content =
--         Just
--           -- $ RequestBody.Json
--           -- $ encodeJson
--           $ RequestBody.String
--           $ JSON.writeJSON
--               { query: opStr <> " " <> queryName <> " " <> q
--               , variables: vars
--               , operationName: queryName
--               }
--       , headers = headers <> [ ContentType applicationJSON ]
--       }
-- 
-- convertJsonResponse :: Either Error (Response String) -> Aff (Either Error (Response Json))
-- convertJsonResponse (Left err) = pure $ Left err
-- convertJsonResponse (Right res@{ body }) = pure $ case JSON.readJSON body of
--   Left err -> Left $ ResponseBodyError (DLN.head err) (res { body = unsafeToForeign body })
--   Right body' -> Right $ res { body = toJSON body' }
-- 
-- foreign import toJSON :: forall d. JSON.ReadForeign d => d -> Json
-- 
-- 
