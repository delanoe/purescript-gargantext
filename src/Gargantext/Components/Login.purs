module Gargantext.Components.Login where

import Control.Monad.Cont.Trans (lift)
import Data.Int as Int
import Data.Lens (over)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested((/\))
import Effect.Class (liftEffect)
import Effect (Effect)
import Effect.Aff (Aff)
import Reactix as R
import Reactix.DOM.HTML as H
import React.DOM (button, div, h5, span, text)
import React.DOM.Props (_data, _id, _type, aria, className, role)
import Thermite (PerformAction, Render, Spec, _render, modifyState_, simpleSpec)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem, removeItem)

------------------------------------------------------------------------
import Gargantext.Prelude
import Gargantext.Config (toUrl, Path(..), End(..))
import Gargantext.Config.REST (post)
import Gargantext.Components.Modals.Modal (modalHide)
import Gargantext.Components.Login.Types
import Gargantext.Utils.Reactix as R2

-- TODO: ask for login (modal) or account creation after 15 mn when user
-- is not logged and has made one search at least

type State =
  { username :: String
  , password :: String
  , authData :: Maybe AuthData
  , errorMessage :: String
  }


initialState :: Effect State
initialState = do
  authData <- getAuthData
  pure
    { authData
    , username : ""
    , password : ""
    , errorMessage : ""
    }

data Action
  = PostAuth
  | SetCredentials String String


modalSpec :: forall props. Boolean -> String -> Spec State props Action -> Spec State props Action
modalSpec sm t = over _render \render d p s c ->
  [ div [ _id "loginModal", className $ "modal myModal" <> if sm then "" else " fade"
        , role "dialog"
        , _data {show : true}
        ]
    [ div [ className "modal-dialog"
          , role "document"
          ]
      [ div [ className "modal-content"]
        [ div [ className "modal-header"]
          [ h5 [ className "modal-title" ]
            [ -- text t
            ]
          , button [ _type "button"
                   , className "close"
                   , _data { dismiss : "modal"}
                   ]
            [ span [ aria {hidden : true}] [ text "X"]
            ]
          ]
        , div [ className "modal-body"] (render d p s c)
        ]
      ]
    ]
  ]

spec' :: Spec State {} Action
spec' = modalSpec true "Login" renderSpec

performAction :: PerformAction State {} Action
performAction (SetCredentials usr pwd) _ _ = do
  modifyState_ $ _ { username = usr, password = pwd }
performAction PostAuth _ {username, password} = do
  res <- lift $ postAuthRequest $ AuthRequest {username, password}
  case res of
    AuthResponse {inval: Just (AuthInvalid {message})} ->
      modifyState_ $ _ { errorMessage = message }
    AuthResponse {valid} -> do
      liftEffect $ setAuthData valid
      modifyState_ $ _ {authData = valid, errorMessage = ""}
      liftEffect $ modalHide "loginModal"

renderSpec :: Spec State {} Action
renderSpec = simpleSpec performAction render
  where
    render :: Render State {} Action
    render dispatch _ state _ =
      [R2.scuff $ renderCpt dispatch state]


renderCpt :: (Action -> Effect Unit) -> State -> R.Element
renderCpt d s = R.createElement el {} []
  where
    el = R.hooksComponent "RenderComponent" cpt
    cpt {} _children = do
      (state /\ setState) <- R.useState' s

      R.useEffect $
        if (state /= s) then do
          _ <- d $ SetCredentials state.username state.password
          pure $ d $ PostAuth
        else
          pure $ pure $ unit

      pure $ renderLogin (state /\ setState)

renderLogin :: R.State State -> R.Element
renderLogin (state /\ setState) = R.createElement el {} []
  where
    el = R.hooksComponent "RenderLogin" cpt
    cpt {} _children = do
      username <- R.useState' state.username
      password <- R.useState' state.password

      pure $ H.div {className: "row"}
        [ gargLogo
        , H.div {className: "card-group"}
          [ H.div {className: "card"}
            [ H.div {className: "card-block"}
              [ H.div {className: "center"}
                [ H.h4 {className: "m-b-0"}
                  [ H.span {className: "icon-text"}
                    [ H.text "Welcome :)"]
                  ]
                , H.p {className: "text-muted"}
                  [ H.text $ "Login to your account or",
                    H.a { target: "blank"
                        , href: "https://iscpif.fr/services/applyforourservices/"
                        }
                    [H.text " ask to get an access"]
                  ]
                ]
              , H.div {}
                [ H.input { type: "hidden"
                          , name: "csrfmiddlewaretoken"
                            -- TODO hard-coded CSRF token
                          , value: "Wy52D2nor8kC1r1Y4GrsrSIxQ2eqW8UwkdiQQshMoRwobzU4uldknRUhP0j4WcEM"
                          }

                , H.div {className: "form-group"}
                  [ H.p {} [H.text state.errorMessage]
                  , usernameInput username
                  ]
                , H.div {className: "form-group"}
                  [ passwordInput password
                  , H.div {className: "clearfix"} []
                  ]
                , H.div {className: "center"}
                  [ H.label {}
                    [ H.div {className: "checkbox"}
                      [ H.input { id: "terms-accept"
                                , type: "checkbox"
                                , value: ""
                                , className: "checkbox"
                                }
                      , H.text "I accept the terms of uses "
                      , H.a {href: "http://gitlab.iscpif.fr/humanities/tofu/tree/master"}
                        [ H.text " [ Read the terms of use ] "]
                      ]
                    ]
                  , H.button { id: "login-button"
                             , className: "btn btn-primary btn-rounded"
                             , type: "submit"
                                   -- TODO
                                   --, on: {click: \_ -> dispatch $ PostAuth}
                             , on: {click: onClick username password}
                             }
                    [H.text "Login"]
                  ]
                ]
              ]
            ]
          ]
        ]

    gargLogo =
      H.div {className: "col-md-10 col-md-push-1"}
      [ H.h2 {className: "text-primary center m-a-2"}
        [ H.i {className: "material-icons md-36"}
          [H.text "control_point"]
        , H.span {className: "icon-text"}
          [H.text "Gargantext"]
        ]
      ]
    usernameInput (username /\ setUsername) =
      H.input { className: "form-control"
              , id: "id_username"
              , maxLength: "254"
              , name: "username"
              , placeholder: "username"
              , type: "text"
              , defaultValue: username
                --, on: {input: \e -> dispatch (SetUserName $ R2.unsafeEventValue e)}
              , on: {change: \e -> setUsername $ const $ R2.unsafeEventValue e}
              }
    passwordInput (password /\ setPassword) =
      H.input { className: "form-control"
              , id: "id_password"
              , name: "password"
              , placeholder: "password"
              , type: "password"
              , defaultValue: password
              --, on: {input: \e -> dispatch (SetPassword $ R2.unsafeEventValue e)}
              , on: {change: \e -> setPassword $ const $ R2.unsafeEventValue e}
              }
    onClick (username /\ _) (password /\ _) = \e -> do
      setState $ \st -> st {username = username, password = password}


getAuthData :: Effect (Maybe AuthData)
getAuthData = do
  w  <- window
  ls <- localStorage w
  mto <- getItem "token" ls
  mti <- getItem "tree_id" ls
  pure do
    token <- mto
    tree_id <- Int.fromString =<< mti
    pure $ AuthData {token, tree_id}

setAuthData :: Maybe AuthData -> Effect Unit
setAuthData Nothing = do
  w  <- window
  ls <- localStorage w
  removeItem "token"   ls
  removeItem "tree_id" ls
setAuthData (Just (AuthData {tree_id, token})) = do
  w  <- window
  ls <- localStorage w
  setItem "token"   token          ls
  setItem "tree_id" (show tree_id) ls

postAuthRequest :: AuthRequest -> Aff AuthResponse
postAuthRequest = post $ toUrl Back Auth Nothing
