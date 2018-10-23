module Gargantext.Pages.Layout.Specs.AddCorpus.Actions where

import Control.Monad.Cont.Trans (lift)
import Data.Argonaut (class EncodeJson, jsonEmptyObject, (:=), (~>))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Routing.Hash (setHash)
import Thermite (PerformAction, modifyState)

import Gargantext.Prelude
import Gargantext.Config.REST (post)
import Gargantext.Components.Modals.Modal (modalHide)
import Gargantext.Pages.Layout.Specs.AddCorpus.States (Response, State)

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
  void $ modifyState $ _ {response = res}

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

getDatabaseDetails :: QueryString -> Aff (Array Response)
getDatabaseDetails reqBody = do
  -- TODO let token = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJleHAiOjE1MTk5OTg1ODMsInVzZXJfaWQiOjUsImVtYWlsIjoiYWxleGFuZHJlLmRlbGFub2VAaXNjcGlmLmZyIiwidXNlcm5hbWUiOiJkZXZlbG9wZXIifQ.Os-3wuFNSmRIxCZi98oFNBu2zqGc0McO-dgDayozHJg"
  post "http://localhost:8009/count" reqBody
