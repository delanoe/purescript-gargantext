module Login where

import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)
import Prelude hiding (div)
import React.DOM (a, button, div, footer, form, h1, h2, h3, h4, hr, i, img, input, li, p, span, text, ul)
import React.DOM.Props (_data, _id, _type, aria, className, href, maxLength, name, placeholder, role, src, style, tabIndex, target, title, value)
import Thermite (PerformAction, Render, Spec, simpleSpec)
import Thermite as T

newtype State = State
  { userName :: String
  , password :: String
  }


initialState :: State
initialState = State
  {userName : ""
 , password : ""
  }

data Action
  = NoOp
  | Login


performAction :: forall eff props.PerformAction (console :: CONSOLE, ajax :: AJAX,dom::DOM | eff) State props Action
performAction NoOp _ _ = void do
  T.modifyState \state -> state

performAction Login _ _ = void do
  T.modifyState \state -> state



renderSpec :: forall props eff . Spec (console::CONSOLE, ajax::AJAX, dom::DOM | eff) State props Action
renderSpec = simpleSpec performAction render
  where
    render :: Render State props Action
    render dispatch _ state _ =
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
                      a [ target "blank",href "https://iscpif.fr/services/applyforourservices/"] [text "ask to get an access"]
                    ]
                  ]
                , form []
                  [ input [_type "hidden",
                           name "csrfmiddlewaretoken",
                           value "Wy52D2nor8kC1r1Y4GrsrSIxQ2eqW8UwkdiQQshMoRwobzU4uldknRUhP0j4WcEM" ]
                    []
                  , div [className "form-group"]
                    [
                      input [className "form-control", _id "id_username",maxLength "254", name "username", placeholder "username", _type "text"] []
                    ]
                  , div [className "form-group"]
                    [ input [className "form-control", _id "id_password", name "password", placeholder "password", _type "password"] []
                    , div [className "clearfix"] []
                    ]
                  , div [className "center"]
                    [

                      div [className "checkbox"]
                      [ input [_id "terms-accept", _type "checkbox", value "", className "checkbox"]
                        [
                        ]
                      , text "I accept the terms of uses",
                      a [href "http://gitlab.iscpif.fr/humanities/tofu/tree/master"] [text "[Read the terms of use]"]
                      ]
                    , button [_id "login-button",className "btn btn-primary btn-rounded", _type "submit"] [text "Login"]
                    ]
                  ]
                ]
              ]
            ]

          ]


        ]
      ]
