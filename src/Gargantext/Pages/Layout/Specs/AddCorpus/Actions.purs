module Gargantext.Pages.Layout.Specs.AddCorpus.Actions where

import Gargantext.Pages.Layout.Specs.AddCorpus.States
import Prelude hiding (div)

import Affjax (defaultRequest, request)
import Affjax.RequestBody (RequestBody(..))
import Affjax.RequestHeader (RequestHeader(..))
import Control.Monad.Cont.Trans (lift)
import Data.Argonaut (class EncodeJson, decodeJson, encodeJson, jsonEmptyObject, (:=), (~>))
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(Just))
import Data.MediaType.Common (applicationJSON)
import Effect.Aff (Aff, attempt)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Gargantext.Components.Modals.Modal (modalHide)
import Routing.Hash (setHash)
import Thermite (PerformAction, modifyState)

data Action
  = NoOp
  | SelectDatabase Boolean
  | UnselectDatabase Boolean
  | LoadDatabaseDetails
  | GO

performAction :: forall props. PerformAction State props Action
performAction NoOp _ _ = void do
  modifyState identity

performAction (SelectDatabase selected) _ _ = void do
  modifyState \( state) -> state { select_database = selected }

performAction (UnselectDatabase unselected) _ _ = void do
  modifyState \( state) ->  state { unselect_database = unselected }

performAction (LoadDatabaseDetails) _ _ = void do
  res <- lift $ getDatabaseDetails $ QueryString { query_query: "string",query_name: ["Pubmed"]}
  case res of
     Left err -> modifyState $ \(state) ->  state
     Right resData -> do
       modifyState $ \(state) -> state {response  = resData}

performAction GO _ _ = void do
  lift $ setHash "/corpus"
  _ <- liftEffect $ modalHide "addCorpus"
  modifyState identity


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
  affResp <- liftAff $ attempt $ request $ defaultRequest
    { method = Left POST
    , url ="http://localhost:8009/count"
    , headers =  [ ContentType applicationJSON
                , Accept applicationJSON
              --   , RequestHeader "Authorization" $  "Bearer " <> token
            ]
    , content = Just $ Json $ encodeJson reqBody
    }
  case affResp of
    Left err -> do
      liftEffect $ log $ "error" <> show err
      pure $ Left $ show err

    Right a -> do
      liftEffect $ log $ "POST method Completed"
      liftEffect $ log $ "GET /api response: " <> show a.body
      res <- case a.body of
        Left err -> []
        Right d -> decodeJson d
      pure res
