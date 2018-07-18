module Gargantext.Pages.Layout.Specs.AddCorpus.Actions where

import Prelude hiding (div)

import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Cont.Trans (lift)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Lens (over)
import Data.Maybe (Maybe(Just))
import Data.MediaType.Common (applicationJSON)

import Gargantext.Components.Modals.Modal (modalHide)
import Gargantext.Pages.Layout.Specs.AddCorpus.States

import Network.HTTP.Affjax (AJAX, affjax, defaultRequest)
import Network.HTTP.RequestHeader (RequestHeader(..))
import React (ReactElement)
import React.DOM (button, div, h3, h5, li, span, text, ul)
import React.DOM.Props (_data, _id, _type, aria, className, onClick, role)
import Routing.Hash.Aff (setHash)
import Thermite (PerformAction, Render, Spec, _render, cotransform, modifyState, simpleSpec)

data Action
  = NoOp
  | SelectDatabase Boolean
  | UnselectDatabase Boolean
  | LoadDatabaseDetails
  | GO

performAction :: forall eff props. PerformAction ( console :: CONSOLE
                                                 , ajax    :: AJAX
                                                 , dom     :: DOM
                                                 | eff ) State props Action
performAction NoOp _ _ = void do
  modifyState id

performAction (SelectDatabase selected) _ _ = void do
  modifyState \( state) -> state { select_database = selected }

performAction (UnselectDatabase unselected) _ _ = void do
  modifyState \( state) ->  state { unselect_database = unselected }

performAction (LoadDatabaseDetails) _ _ = void do
  res <- lift $ getDatabaseDetails $ QueryString { query_query: "string",query_name: ["Pubmed"]}
  case res of
     Left err -> cotransform $ \(state) ->  state
     Right resData -> do
       cotransform $ \(state) -> state {response  = resData}

performAction GO _ _ = void do
  lift $ setHash "/corpus"
  _ <- liftEff $ modalHide "addCorpus"
  modifyState id


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



getDatabaseDetails :: forall eff.  QueryString -> Aff (console::CONSOLE,ajax :: AJAX | eff) (Either String (Array Response))
getDatabaseDetails reqBody = do
  let token = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJleHAiOjE1MTk5OTg1ODMsInVzZXJfaWQiOjUsImVtYWlsIjoiYWxleGFuZHJlLmRlbGFub2VAaXNjcGlmLmZyIiwidXNlcm5hbWUiOiJkZXZlbG9wZXIifQ.Os-3wuFNSmRIxCZi98oFNBu2zqGc0McO-dgDayozHJg"
  affResp <- liftAff $ attempt $ affjax defaultRequest
    { method = Left POST
    , url ="http://localhost:8009/count"
    , headers =  [ ContentType applicationJSON
                , Accept applicationJSON
              --   , RequestHeader "Authorization" $  "Bearer " <> token
            ]
    , content = Just $ encodeJson reqBody
    }
  case affResp of
    Left err -> do
      liftAff $ log $ "error" <> show err
      pure $ Left $ show err

    Right a -> do
      liftAff $ log $ "POST method Completed"
      liftAff $ log $ "GET /api response: " <> show a.response
      let res = decodeJson a.response
      pure res



