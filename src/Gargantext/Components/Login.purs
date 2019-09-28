module Gargantext.Components.Login where

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple.Nested((/\))
import DOM.Simple.Console (log2)
import Effect.Class (liftEffect)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Foreign (MultipleErrors)
import Foreign.Generic (encodeJSON, decodeJSON)
import Reactix as R
import Reactix.DOM.HTML as H
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem, removeItem)

------------------------------------------------------------------------
import Gargantext.Prelude
import Gargantext.Config (Ends, BackendRoute(..), backendKey, url)
import Gargantext.Config.REST (post)
import Gargantext.Components.Modals.Modal (modalHide)
import Gargantext.Components.Login.Types
import Gargantext.Utils.Reactix as R2

-- TODO: ask for login (modal) or account creation after 15 mn when user
-- is not logged and has made one search at least

type Auths = Map String AuthData

type Props = ( ends :: Ends, setVisible :: R2.Setter Boolean)

type ModalProps = ( visible :: Boolean )

modal :: Record ModalProps -> Array R.Element -> R.Element
modal = R.createElement modalCpt

modalCpt :: R.Component ModalProps
modalCpt = R.staticComponent "Modal" cpt
  where
    cpt {visible} children =
      H.div { id: "loginModal", className: modalClass visible, role: "dialog", "data": {show: true}}
      [ H.div { className: "modal-dialog", role: "document"}
        [ H.div { className: "modal-content" }
          [ H.div { className: "modal-header" }
            [ H.h5 { className: "modal-title" } []
            , H.button { "type": "button", className: "close", "data": { dismiss: "modal" } }
              [ H.span { aria: { hidden: true } } [ H.text "X" ] ]
            , H.div { className: "modal-body" } children ] ] ] ]
    modalClass s = "modal myModal" <> if s then "" else " fade"

login :: Record Props -> R.Element
login props = R.createElement loginCpt props []

loginCpt :: R.Component Props
loginCpt = R.hooksComponent "Login" cpt
  where
    cpt {ends, setVisible} _children = do
      (username /\ setUsername) <- R.useState' ""
      (password /\ setPassword) <- R.useState' ""
      (error /\ setError) <- R.useState' ""
      (authData /\ setAuthData) <- R.useState' Nothing
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
                    [H.text " request access"]
                  ]
                ]
              , H.div {}
                [ H.input { type: "hidden"
                          , name: "csrfmiddlewaretoken"
                            -- TODO hard-coded CSRF token
                          , value: "Wy52D2nor8kC1r1Y4GrsrSIxQ2eqW8UwkdiQQshMoRwobzU4uldknRUhP0j4WcEM"
                          }

                , H.div {className: "form-group"}
                  [ H.p {} [H.text error]
                  , usernameInput username setUsername
                  ]
                , H.div {className: "form-group"}
                  [ passwordInput password setPassword
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
                      , H.text "I accept the terms of use "
                      , H.a {href: "http://gitlab.iscpif.fr/humanities/tofu/tree/master"}
                        [ H.text " [ Read the terms of use ] "]
                      ]
                    ]
                  , H.button { id: "login-button"
                             , className: "btn btn-primary btn-rounded"
                             , type: "submit"
                                   -- TODO
                                   --, on: {click: \_ -> dispatch $ PostAuth}
                             , on: {click: onClick ends setError setAuthData setVisible username password}
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
        [ H.i {className: "material-icons md-36"} [ H.text "control_point" ]
        , H.span {className: "icon-text"} [ H.text "Gargantext" ] ] ]
    usernameInput username setUsername =
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
 
    passwordInput password setPassword =
      H.input { className: "form-control"
              , id: "id_password"
              , name: "password"
              , placeholder: "password"
              , type: "password"
              , defaultValue: password
              --, on: {input: \e -> dispatch (SetPassword $ R2.unsafeEventValue e)}
              , on: {change: \e -> setPassword $ const $ R2.unsafeEventValue e}
              }
    onClick ends setError setAuthData setVisible username password = \e ->
      launchAff_ $ do
        res <- postAuthRequest ends $ AuthRequest {username, password}
        case res of
          AuthResponse {inval: Just (AuthInvalid {message})} -> liftEffect $ do
            setError (const message)
            setAuthData (const Nothing)
          AuthResponse {valid} -> liftEffect $ do
            setAuthData (const valid)
            setError (const "")
            setVisible (const false)

-- getAuth :: Effect Auth
-- getAuth = do
--   window >>= localStorage >>= getItem

-- setAuth :: Auth -> Effect Unit

getAuths :: Effect (Maybe Auths)
getAuths = pure Nothing

-- getAuths = window >>= localStorage >>= getItem "auths" >>= traverse decode
--   where
--     decode :: String -> Effect (Maybe Auths)
--     decode = ret <<< runExcept <<< decodeJSON
--     ret (Right v) = pure $ Just v
--     ret (Left e) = log2 "Error reading serialised auths:" e *> pure Nothing

setAuths :: Maybe Auths -> Effect Unit
-- setAuths Map.empty = -- window >>= localStorage >>= removeItem "auths"
setAuths _ = pure unit -- auths = window >>= localStorage >>= setItem "auths" (encodeJSON auths)


-- TODO
-- useLocalStorageAuths :: String -> R.Hooks (R.State Auths)
-- useLocalStorageAuths key = do

postAuthRequest :: Ends -> AuthRequest -> Aff AuthResponse
postAuthRequest ends = post $ url ends Auth

getCurrentAuth :: Ends -> Auths -> Maybe AuthData
getCurrentAuth ends = Map.lookup (backendKey ends.backend)
