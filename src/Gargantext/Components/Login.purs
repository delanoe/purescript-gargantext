module Gargantext.Components.Login where

import Control.Monad.Cont.Trans (lift)
import Data.Int as Int
import Data.Lens (over, view)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Effect.Class (liftEffect)
import Effect (Effect)
import Effect.Aff (Aff)
import React.DOM (a, button, div, h2, h4, h5, i, input, label, p, span, text)
import React.DOM.Props (_data, _id, _type, aria, className, href, maxLength, name, onClick, onInput, placeholder, role, target, value)
import Thermite (PerformAction, Render, Spec, _render, modifyState_, simpleSpec)
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem, removeItem)

------------------------------------------------------------------------
import Gargantext.Prelude
import Gargantext.Config (toUrl, Path(..), End(..))
import Gargantext.Config.REST (post)
import Gargantext.Components.Modals.Modal (modalHide)
import Gargantext.Components.Login.Types

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
  | SetUserName String
  | SetPassword String


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

renderSpec :: Spec State {} Action
renderSpec = simpleSpec performAction render
  where
    performAction :: PerformAction State {} Action

    performAction (SetUserName usr) _ _ =
      modifyState_ $ _ { username = usr }

    performAction (SetPassword pwd) _ _ =
      modifyState_ $ _ { password = pwd }

    performAction PostAuth _ {username, password} = do
      res <- lift $ postAuthRequest $ AuthRequest {username, password}
      case res of
        AuthResponse {inval: Just (AuthInvalid {message})} ->
          modifyState_ $ _ { errorMessage = message }
        AuthResponse {valid} -> do
          liftEffect $ setAuthData valid
          modifyState_ $ _ {authData = valid, errorMessage = ""}
          liftEffect $ modalHide "loginModal"

    render :: Render State {} Action
    render dispatch _ state _ =
      [ div [className "row"]
        [ div [className "col-md-10 col-md-push-1"]
          [ h2 [className "text-primary center m-a-2"]
            [ i [className "material-icons md-36"] [text "control_point"]
            , span [className "icon-text"] [text "Gargantext"]
            ]
          , div [className "card-group"]
            [ div [className "card"]
              [ div [className "card-block"]
                [ div [className "center"]
                  [ h4 [className "m-b-0"]
                    [ span [className "icon-text"] [ text "Welcome :)"] ]
                  , p [className "text-muted"]
                    [ text $ "Login to your account or",
                      a [ target "blank",href "https://iscpif.fr/services/applyforourservices/"] [text " ask to get an access"]
                    ]
                  ]
                , div []
                  [ input [_type "hidden",
                           name "csrfmiddlewaretoken",
                           -- TODO hard-coded CSRF token
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
                    [ label []
                      [ div [className "checkbox"]
                        [ input [_id "terms-accept", _type "checkbox", value "", className "checkbox"]
                        , text "I accept the terms of uses ",
                          a [href "http://gitlab.iscpif.fr/humanities/tofu/tree/master"] [text " [ Read the terms of use ] "]
                        ]
                      , button [_id "login-button",className "btn btn-primary btn-rounded", _type "submit", onClick \_ -> dispatch $ PostAuth] [text "Login"]
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
--                                         [ ul [ className "list-group"] ( map fn1 state.authData ) ]

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
