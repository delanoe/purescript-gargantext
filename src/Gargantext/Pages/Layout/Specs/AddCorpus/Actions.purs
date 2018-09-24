module Gargantext.Pages.Layout.Specs.AddCorpus.Actions where

import Prelude hiding (div)

import Affjax (defaultRequest, printResponseFormatError, request)
import Affjax.RequestBody (RequestBody(..))
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Cont.Trans (lift)
import Data.Argonaut (class EncodeJson, decodeJson, encodeJson, jsonEmptyObject, stringify, (:=), (~>))
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(Just))
import Data.MediaType.Common (applicationJSON)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Gargantext.Components.Modals.Modal (modalHide)
import Gargantext.Pages.Layout.Specs.AddCorpus.States (Response, State)
import Routing.Hash (setHash)
import Thermite (PerformAction, modifyState)

data Action
  = SelectDatabase Boolean
  | UnselectDatabase Boolean
  | LoadDatabaseDetails
  | GO

performAction :: PerformAction State {} Action
performAction (SelectDatabase selected) _ _ = void do
  modifyState $ _ { select_database = selected }

performAction (UnselectDatabase unselected) _ _ = void do
  modifyState $ _ { unselect_database = unselected }

performAction (LoadDatabaseDetails) _ _ = do
  res <- lift $ getDatabaseDetails $ QueryString { query_query: "string",query_name: ["Pubmed"]}
  case res of
     Left err -> pure unit
     Right resData -> do
       void $ modifyState $ _ {response  = resData}

performAction GO _ _ = do
  liftEffect $ setHash "/corpus"
  liftEffect $ modalHide "addCorpus"
  pure unit


newtype QueryString = QueryString
  {
    query_query :: String
  , query_name :: Array String
  }

queryString :: QueryString
queryString = QueryString
  {
  query_query: "string",
  query_name: [
    "Pubmed"
  ]
  }


instance encodeJsonQueryString :: EncodeJson QueryString where
  encodeJson (QueryString obj) =
    "query_query"       := obj.query_query
    ~> "query_name"        := obj.query_name
    ~> jsonEmptyObject

getDatabaseDetails :: QueryString -> Aff (Either String (Array Response))
getDatabaseDetails reqBody = do
  let token = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJleHAiOjE1MTk5OTg1ODMsInVzZXJfaWQiOjUsImVtYWlsIjoiYWxleGFuZHJlLmRlbGFub2VAaXNjcGlmLmZyIiwidXNlcm5hbWUiOiJkZXZlbG9wZXIifQ.Os-3wuFNSmRIxCZi98oFNBu2zqGc0McO-dgDayozHJg"
  affResp <- request $ defaultRequest
    { method = Left POST
    , responseFormat = ResponseFormat.json
    , url = "http://localhost:8009/count"
    , headers = [ ContentType applicationJSON
                , Accept applicationJSON
                  --   , RequestHeader "Authorization" $  "Bearer " <> token
                ]
    , content = Just $ Json $ encodeJson reqBody
    }
  case affResp.body of
    Left err -> do
      liftEffect $ log $ "error" <> printResponseFormatError err
      pure $ Left $ printResponseFormatError err
    Right json -> do
      liftEffect $ log $ "POST method Completed"
      liftEffect $ log $ "GET /api response: " <> stringify json
      let obj = decodeJson json
      pure obj
