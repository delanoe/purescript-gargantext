module AddCorpusview where

import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Cont.Trans (lift)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Location (host)
import DOM.HTML.Window (localStorage)
import DOM.WebStorage.Storage (getItem, setItem)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Lens (Lens', Prism', lens, over)
import Data.List (List, fold, fromFoldable, toUnfoldable)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType.Common (applicationJSON)
import Data.Tuple (Tuple(..))
import Landing as L
import Network.HTTP.Affjax (AJAX, affjax, defaultRequest)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Prelude hiding (div)
import React (ReactElement)
import React.DOM (a, button, div, form, h2, h3, h4, h5, i, input, label, li, p, span, text, ul)
import React.DOM.Props (_data, _id, _type, aria, className, href, maxLength, name, onClick, onInput, placeholder, role, target, value)
import Routing.Hash.Aff (setHash)
import Thermite (PerformAction, Render, Spec, _render, cotransform, focus, foreach, modifyState, simpleSpec, withState)
import Unsafe.Coerce (unsafeCoerce)


type State =
  { select_database :: Boolean
  , unselect_database :: Boolean  --  dummy state
  , response :: Array Response
  }

newtype Response = Response
  {
    count :: Int
  , name :: String
  }

initialState :: State
initialState =
  {
    select_database   : true
  , unselect_database : true
  , response : []
  }


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
  res <- lift $ getDatabaseDetails $ QueryString{query_query: "string",query_name: ["Pubmed"]}
  case res of
     Left err -> cotransform $ \(state) ->  state
     Right resData -> do
       cotransform $ \(state) -> state {response  = resData}

performAction GO _ _ = void do
  lift $ setHash "/docView"
  modifyState id



layoutAddcorpus :: forall props eff . Spec (console::CONSOLE, ajax::AJAX, dom::DOM | eff) State props Action
layoutAddcorpus = simpleSpec performAction render
  where
    render :: Render State props Action
    render dispatch _ state _ =
      [
        div [className "container"] [L.jumboTitle false]
      ,  div [className "container"]
        [
          div [className "jumbotron"]
          [ div [className "row"]
           [
             div [className "col-md-6"]
             [
               button [_type "button", _data {"toggle" : "modal", "target" : ".myModal"}][text "Launch modal"]
             , div [className "modal fade myModal",role "dialog", _data {show : true}  ]
               [ div [className "modal-dialog",role "document"]
                 [ div [className "modal-content"]
                   [ div [className "modal-header"]
                     [  h5 [className "modal-title"] [ text "CorpusView"]
                     , button [ _type "button",className "close", _data { dismiss : "modal"}]
                       [ span [aria {hidden : true}] [ text "X"]
                       ]
                     ]
                   ,  div [className "modal-body"]
                      [ ul [className "list-group"] $ map fn1 state.response
                      ]
                   , div [className "modal-footer"]
                     [ button [ _type "button", className "btn btn-secondary", _data {dismiss : "modal"}]
                       [ text "GO"]
                     ]
                   ]
                 ]
               ]
             ]
           , div [className "col-md-6"]
             [
               h3 [] [text "Corpusview"]
             , ul [className "list-group"] $ map fn1 state.response
             , button [onClick \_ -> dispatch GO] [text "GO"]
             ]

           ]
          ]
        ]
      ]
      where
        fn1 (Response o) =
          li [className "list-group-item justify-content-between"]
          [
          span [] [text  o.name]
          ,  span [className "badge badge-default badge-pill"] [ text $ show o.count]
          ]


newtype QueryString = QueryString
  {
    query_query :: String
  ,  query_name :: Array String
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


instance decodeJsonresponse :: DecodeJson Response where
  decodeJson json = do
    obj   <- decodeJson json
    count <- obj .? "count"
    name  <- obj .? "name"
    pure $ Response {count,name }
