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
import Data.Maybe (Maybe(..))
import Data.MediaType.Common (applicationJSON)
import Network.HTTP.Affjax (AJAX, affjax, defaultRequest)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Prelude hiding (div)
import React.DOM (a, button, div, form, h2, h3, h4, i, input, label, li, p, span, text, ul)
import React.DOM.Props (_id, _type, className, href, maxLength, name, onClick, onInput, placeholder, target, value)
import Routing.Hash.Aff (setHash)
import Thermite (PerformAction, Render, Spec, modifyState, simpleSpec)
import Unsafe.Coerce (unsafeCoerce)


newtype State = State
  { select_database :: Boolean
  , unselect_database :: Boolean  --  dummy state
  }

initialState :: State
initialState = State
  {
    select_database : true
  , unselect_database : true
  }


data Action
  = NoOp
  | SelectDatabase Boolean
  | UnselectDatabase Boolean


performAction :: forall eff props. PerformAction (console :: CONSOLE, ajax :: AJAX,dom::DOM | eff) State props Action
performAction NoOp _ _ = void do
  modifyState id

performAction (SelectDatabase selected) _ _ = void do
  modifyState \(State state) -> State $ state { select_database = selected }


performAction (UnselectDatabase unselected) _ _ = void do
  modifyState \(State state) -> State $ state { unselect_database = unselected }



addcorpusviewSpec :: forall props eff . Spec (console::CONSOLE, ajax::AJAX, dom::DOM | eff) State props Action
addcorpusviewSpec = simpleSpec performAction render
  where
    render :: Render State props Action
    render dispatch _ state _ =
      [
        div [className "container"]
        [
          div [className "jumbotron"]
          [div [className "row"]
          [
            div [className "col-md-3"]
            [
              h3 [] [text "Treeview"]
            ]
          , div [className "col-md-9"]
            [
              h3 [] [text "Corpusview"]
            , ul [className "list-group"]
              [
                li [className "list-group-item justify-content-between"]
                [
                  span [className "badge badge-default badge-pill"] []
                ]
              ]
            ]

          ]
          ]
        ]
      ]



getDatabaseDetais = do
  let token = ""
  -- liftEff $ log $ "calling update Age "
  affResp <- liftAff $ attempt $ affjax defaultRequest
    { method = Left GET
    , url ="http://unstable.gargantext.org/api/auth/token"
    , headers =  [ ContentType applicationJSON
                , Accept applicationJSON
                , RequestHeader "Authorization" $  "Bearer " <> token
            ]
    }
  case affResp of
    Left err -> do
      pure $ Left $ show err
    Right a -> do
      liftAff $ log $ "POST method Completed"
      liftAff $ log $ "GET /api response: " <> show a.response
      let res = decodeJson a.response
      pure res



-- updateProfileAge :: forall eff. State -> UpdateAgeReq -> Aff (console::CONSOLE,ajax :: AJAX | eff) (Either String UpdateProfileUserProfile)
-- updateProfileAge state reqBody = do
--   let token = fromMaybe "" $ (\(State s) -> s.token) state
--   liftEff $ log $ "calling update Age " <> show reqBody
--   affResp <- liftAff $ attempt $ affjax defaultRequest
--     { method = Left PUT
--     , url = host <> "api/users/update_profile_fields"
--     , headers =  [ ContentType applicationJSON
--                 , Accept applicationJSON
--                 , RequestHeader "Authorization" $  "Bearer " <> token
--             ]
--     , content = Just $ encodeJson reqBody
--     }
--   case affResp of
--     Left err -> do
--       pure $ Left $ show err
--     Right a -> do
--       liftEff $ log $ "POST method Completed"
--       liftEff $ log $ "GET /api response: " <> show a.response
--       let res = decodeJson a.response
--       pure res
