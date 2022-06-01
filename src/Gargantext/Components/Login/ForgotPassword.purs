module Gargantext.Components.Login.ForgotPassword where

import DOM.Simple.Event as DE
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Gargantext.Components.Forms (formGroup)
import Gargantext.Ends (Backend)
import Gargantext.Prelude
import Gargantext.Sessions (Sessions, postForgotPasswordRequest)
import Gargantext.Utils.Reactix as R2
import Formula as F
import Reactix as R
import Reactix.DOM.HTML as H
import Reactix.SyntheticEvent as E
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Login.ForgotPassword"

type Email = String

type Props =
  ( backend :: Backend
  , sessions :: T.Box Sessions )

forgotPassword :: R2.Leaf Props
forgotPassword = R2.leaf forgotPasswordCpt
forgotPasswordCpt :: R.Component Props
forgotPasswordCpt = here.component "forgotPassword" cpt where
  cpt { backend, sessions } _ = do
    email <- T.useBox ""
    
    pure $ H.div { className: "row" }
      [ H.form { className: "text-center col-md-12" }
        [ H.h4 {} [ H.text "Forgot password" ]
        , formGroup
          [ emailInput email ]
        , submitButton { backend, email, sessions }
        ]
      ]

emailInput :: forall cell. T.ReadWrite cell Email => cell -> R.Element
emailInput value = F.bindInput { value
                               , type: "email"
                               , className: "form-control"
                               , id: "id_email"
                               , placeholder: "email"
                               , name: "email"
                               , maxLength: "254" }

type SubmitButtonProps =
  ( email :: T.Box Email
  | Props )

submitButton :: R2.Leaf SubmitButtonProps
submitButton = R2.leafComponent submitButtonCpt
submitButtonCpt :: R.Component SubmitButtonProps
submitButtonCpt = here.component "submitButton" cpt where
  cpt { backend, email, sessions } _ = do
    email' <- T.useLive T.unequal email
    
    pure $ H.div {className: "form-group text-center"} 
      [ H.button { className: "btn btn-primary"
                 , on: { click: click email' }}
        [ H.text "Submit" ]
      ]
    where
      click :: Email -> R.SyntheticEvent DE.MouseEvent -> Effect Unit
      click email' e = do
        E.preventDefault e
        here.log2 "email" email'
        here.log2 "backend" backend
        here.log2 "sessions" sessions
        launchAff_ $ do
          res <- postForgotPasswordRequest backend email'
          liftEffect $ here.log2 "res" res
