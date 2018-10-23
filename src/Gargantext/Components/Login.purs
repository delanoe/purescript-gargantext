module Gargantext.Components.Login where

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Lens (over)
import Data.Maybe (Maybe)
import Effect.Class (liftEffect)
import Effect (Effect)
import Effect.Aff (Aff)
import React.DOM (a, button, div, h2, h4, h5, i, input, label, p, span, text)
import React.DOM.Props (_data, _id, _type, aria, className, href, maxLength, name, onClick, onInput, placeholder, role, target, value)
import Thermite (PerformAction, Render, Spec, _render, modifyState, simpleSpec)
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

------------------------------------------------------------------------
import Gargantext.Prelude
import Gargantext.Config.REST (post)
import Gargantext.Components.Modals.Modal (modalHide)

-- TODO: ask for login (modal) or account creation after 15 mn when user
-- is not logged and has made one search at least

newtype State = State
  { username :: String
  , password :: String
  , response :: LoginRes
  , errorMessage :: String
  , loginC :: Boolean
  }


initialState :: State
initialState = State
  {username : ""
 , password : ""
 , response : LoginRes {token : ""}
 , errorMessage : ""
 , loginC : false
  }

data Action
  = Login
  | SetUserName String
  | SetPassword String


modalSpec :: forall props. Boolean -> String -> Spec State props Action -> Spec State props Action
modalSpec sm t = over _render \render d p s c ->
  [ div [ _id "loginModal", className $ "modal myModal" <> if sm then "" else " fade"
            , role "dialog"
            , _data {show : true}
            ][ div [ className "modal-dialog"
                   , role "document"
                   ] [ div [ className "modal-content"]
                       [ div [ className "modal-header"]
                         [ h5 [ className "modal-title"
                              ]
                           [ text $ t
                           ]
                         , button [ _type "button"
                                  , className "close"
                                  , _data { dismiss : "modal"}
                                  ] [ span [ aria {hidden : true}]
                                      [ text "X"]
                                    ]
                         ]

                       , div [ className "modal-body"]
                         (render d p s c)
                       ]
                     ]
             ]
  ]

spec' :: Spec State {} Action
spec' = modalSpec true "Login" renderSpec

renderSpec :: Spec State {} Action
renderSpec = simpleSpec performAction render
  where
    performAction :: PerformAction State {} Action

    performAction (SetUserName usr) _ _ = void do
      modifyState \(State state) -> State $ state { username = usr }

    performAction (SetPassword pwd) _ _ = void do
      modifyState \(State state) -> State $ state { password = pwd }

    performAction Login _ _ = void do
      --lift $ setHash "/search"
      liftEffect $ modalHide "loginModal"
      modifyState \(State state) -> State $ state {loginC = true}
      -- res <- lift $ loginReq $ LoginReq { username : state.username, password : state.password }
      -- case res of
      --   Left e -> do
      --     logs e
      --     modifyState \(State s) ->  State $ s { errorMessage = e}
      --   Right r@(LoginRes response) -> do
      --     lift $ setHash "/addCorpus"
      --     modifyState \(State s) ->  State $ s {response = r, errorMessage = ""}

    render :: Render State {} Action
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

                  , div [className "form-group"]
                    [ p [] [text state.errorMessage]
                    , input [className "form-control", _id "id_username",maxLength "254", name "username", placeholder "username", _type "text",value state.username,  onInput \e -> dispatch (SetUserName (unsafeEventValue e))]
                    ]
                  , div [className "form-group"]
                    [ input [className "form-control", _id "id_password", name "password", placeholder "password", _type "password",value state.password,onInput \e -> dispatch (SetPassword (unsafeEventValue e))]
                    , div [className "clearfix"] []
                    ]
                  , div [className "center"]
                    [
                      label [] [
                         div [className "checkbox"]
                         [ input [_id "terms-accept", _type "checkbox", value "", className "checkbox"]
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

      -- div [ className "modal fade myModal"
      --                    , role "dialog"
--                    , _data {show : true}
--                    ][ div [ className "modal-dialog"
--                           , role "document"
--                           ] [ div [ className "modal-content"]
--                                   [ div [ className "modal-header"]
--                                         [ h5 [ className "modal-title"
--                                              ]
--                                              [ text "CorpusView"
--                                              ]
--                                         , button [ _type "button"
--                                                  , className "close"
--                                                  , _data { dismiss : "modal"}
--                                                  ] [ span [ aria {hidden : true}]
--                                                           [ text "X"]
--                                                    ]
--                                         ]

--                                   , div [ className "modal-body"]
--                                         [ ul [ className "list-group"] ( map fn1 state.response ) ]

--                                   , div [className "modal-footer"]
--                                         [ button [ _type "button"
--                                                  , className "btn btn-secondary"
--                                                  , _data {dismiss : "modal"}
--                                                  ] [ text "GO"]
--                                         ]
--                                    ]
--                             ]
--                      ]
--         ]



unsafeEventValue :: forall event. event -> String
unsafeEventValue e = (unsafeCoerce e).target.value



getDeviseID ::  Effect (Maybe String)
getDeviseID = do
  w  <- window
  ls <- localStorage w
  getItem "token" ls


setToken :: String -> Effect Unit
setToken s = do
  w  <- window
  ls <- localStorage w
  setItem "token" s ls



newtype LoginRes = LoginRes
  {token :: String
  }


newtype LoginReq = LoginReq
  { username :: String
  , password :: String
  }

loginReq :: LoginReq -> Aff LoginRes
loginReq = post "https://dev.gargantext.org/api/auth/token"

instance decodeLoginRes :: DecodeJson LoginRes where
  decodeJson json = do
    obj   <- decodeJson json
    token <- obj .? "token"
    pure $ LoginRes { token}

instance encodeLoginReq :: EncodeJson LoginReq where
  encodeJson (LoginReq obj) =
       "username"          := obj.username
    ~> "password"          := obj.password
    ~> jsonEmptyObject
