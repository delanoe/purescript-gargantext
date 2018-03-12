module Login where

import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Cont.Trans (lift)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import DOM.HTML (window)
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
import React.DOM (a, button, div, h2, h4, i, input, label, p, span, text)
import React.DOM.Props (_id, _type, className, href, maxLength, name, onClick, onInput, placeholder, target, value)
import Routing.Hash.Aff (setHash)
import Thermite (PerformAction, Render, Spec, modifyState, simpleSpec)
import Unsafe.Coerce (unsafeCoerce)


newtype State = State
  { username :: String
  , password :: String
  , response :: LoginRes
  , errorMessage :: String
  }


initialState :: State
initialState = State
  {username : ""
 , password : ""
 , response : LoginRes {token : ""}
 , errorMessage : ""
  }

data Action
  = NoOp
  | Login
  | SetUserName String
  | SetPassword String


performAction :: forall eff props. PerformAction (console :: CONSOLE, ajax :: AJAX,dom::DOM | eff) State props Action
performAction NoOp _ _ = void do
  modifyState id

performAction (SetUserName usr) _ _ = void do
  modifyState \(State state) -> State $ state { username = usr }


performAction (SetPassword pwd) _ _ = void do
  modifyState \(State state) -> State $ state { password = pwd }



performAction Login _ (State state) = void do
  lift $ setHash "/addCorpus"
  modifyState id
  -- res <- lift $ loginReq $ LoginReq { username : state.username, password : state.password }
  -- case res of
  --   Left e -> do
  --     lift $ log $ show e
  --     modifyState \(State s) ->  State $ s { errorMessage = e}
  --   Right r@(LoginRes response) -> do
  --     lift $ setHash "/addCorpus"
  --     modifyState \(State s) ->  State $ s {response = r, errorMessage = ""}



renderSpec :: forall props eff . Spec (console::CONSOLE, ajax::AJAX, dom::DOM | eff) State props Action
renderSpec = simpleSpec performAction render
  where
    render :: Render State props Action
    render dispatch _ (State state) _ =
      [
        div [className "row"]
        [
          div [className "col-sm-10 col-sm-push-1 col-md-6 col-md-push-3 col-lg-6 col-lg-push-3"]
          [
            h2 [className "text-primary center m-a-2"]
            [ i [className "material-icons md-36"] [text "control_point"]
            , span [className "icon-text"] [text "Gargantext"]
            ]
          , div [className "card-group"]
            [
              div [className "card"]
              [
                div [className "card-block"]
                [
                  div [className "center"]
                  [ h4 [className "m-b-0"]
                    [ span [className "icon-text"] [ text "Connexion"]
                    ]
                  , p [className "text-muted"]
                    [ text $ "Login to your account or",
                      a [ target "blank",href "https://iscpif.fr/services/applyforourservices/"] [text " ask to get an access"]
                    ]
                  ]
                , div []
                  [ input [_type "hidden",
                           name "csrfmiddlewaretoken",
                           value "Wy52D2nor8kC1r1Y4GrsrSIxQ2eqW8UwkdiQQshMoRwobzU4uldknRUhP0j4WcEM" ]
                    []
                  , div [className "form-group"]
                    [ p [] [text state.errorMessage]
                     , input [className "form-control", _id "id_username",maxLength "254", name "username", placeholder "username", _type "text",value state.username,  onInput \e -> dispatch (SetUserName (unsafeEventValue e))] []
                    ]
                  , div [className "form-group"]
                    [ input [className "form-control", _id "id_password", name "password", placeholder "password", _type "password",value state.password,onInput \e -> dispatch (SetPassword (unsafeEventValue e))] []
                    , div [className "clearfix"] []
                    ]
                  , div [className "center"]
                    [
                      label [] [
                      div [className "checkbox"]
                      [ input [_id "terms-accept", _type "checkbox", value "", className "checkbox"]
                        [
                        ]
                      , text "I accept the terms of uses ",
                        a [href "http://gitlab.iscpif.fr/humanities/tofu/tree/master"] [text "[Read the terms of use]"]
                      ]
                    , button [_id "login-button",className "btn btn-primary btn-rounded", _type "submit", onClick \_ -> dispatch $ Login] [text "Login"]
                    ]
                    ]
                  ]
                ]
              ]
            ]

          ]
        ]
      ]


unsafeEventValue :: forall event. event -> String
unsafeEventValue e = (unsafeCoerce e).target.value



getDeviseID ::  forall eff. Eff (dom :: DOM | eff) (Maybe String)
getDeviseID = do
  w <- window
  ls <- localStorage w
  i <- getItem "token" ls
  pure $  i


setToken :: forall e . String -> Eff (dom :: DOM | e) Unit
setToken s = do
  w <- window
  ls <- localStorage w
  liftEff $ setItem "token" s ls
  pure unit



newtype LoginRes = LoginRes
  {token :: String
  }


newtype LoginReq = LoginReq
  {  username :: String
  , password :: String
  }

loginReq :: forall eff. LoginReq -> Aff (console :: CONSOLE, ajax :: AJAX, dom :: DOM | eff) (Either String LoginRes)
loginReq encodeData =
  let
    setting =
      defaultRequest
        { url = "https://dev.gargantext.org/api/auth/token"
        , method = Left POST
        , headers =
            [ ContentType applicationJSON
            , Accept applicationJSON
            ]
        , content = Just $ encodeJson encodeData
        }
  in
    do
      affResp <- liftAff $ attempt $ affjax setting
      case affResp of
        Left err -> do
          liftAff $ log $ show err
          pure $ Left $ show err
        Right a -> do
          liftAff $ log $ "POST method Completed"
          liftAff $ log $ "GET /api response: " <> show a.response
          let res = decodeJson a.response
          liftAff $ log $ "res: " <> show a.response
          case res of
            Left e ->
              liftAff $ log $ "Error Decoding : " <> show e
            Right (LoginRes res1) ->
              liftEff $ setToken res1.token
          pure res

instance decodeLoginRes :: DecodeJson LoginRes where
  decodeJson json = do
    obj <- decodeJson json
    token <- obj .? "token"
    pure $ LoginRes { token}

instance encodeLoginReq :: EncodeJson LoginReq where
  encodeJson (LoginReq obj) =
       "username"          := obj.username
    ~> "password"          := obj.password
    ~> jsonEmptyObject
