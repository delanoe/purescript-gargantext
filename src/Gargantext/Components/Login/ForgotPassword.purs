module Gargantext.Components.Login.ForgotPassword where

import Gargantext.Prelude

import DOM.Simple.Event as DE
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Formula as F
import Gargantext.Components.Forms (formGroup)
import Gargantext.Ends (Backend)
import Gargantext.Sessions (Sessions, postForgotPasswordRequest)
import Gargantext.Utils.Reactix as R2
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
    message <- T.useBox ""
    disabled <- T.useBox false
    
    pure $ H.div { className: "row" }
      [ H.form { className: "text-center col-md-12" }
        [ H.h4 {} [ H.text "Forgot password" ]
        , messageDisplay { message }
        , formGroup
          [ emailInput { email, disabled} ]
        , submitButton { backend, email, sessions, message, disabled }
        ]
      ]

emailInput :: R2.Leaf (email :: T.Box Email, disabled :: T.Box Boolean)
emailInput = R2.leaf emailInputCpt

emailInputCpt :: R.Component (email :: T.Box Email, disabled :: T.Box Boolean)
emailInputCpt = here.component "emailInput" cpt where
  cpt { email, disabled } _ = do
    disabled' <- T.useLive T.unequal disabled  
    pure $ F.bindInput { value: email
                  , type: "email"
                  , className: "form-control"
                  , id: "id_email"
                  , placeholder: "email"
                  , name: "email"
                  , maxLength: "254"
                  , disabled: disabled' }

type SubmitButtonProps =
  ( email    :: T.Box Email
  , message  :: T.Box String
  , disabled :: T.Box Boolean
  | Props )

submitButton :: R2.Leaf SubmitButtonProps
submitButton = R2.leafComponent submitButtonCpt
submitButtonCpt :: R.Component SubmitButtonProps
submitButtonCpt = here.component "submitButton" cpt where
  cpt { backend, email, sessions, message, disabled} _ = do
    email' <- T.useLive T.unequal email
    disabled' <- T.useLive T.unequal disabled
    
    pure $ H.div {className: "form-group text-center"} 
      [ H.button { className: "btn btn-primary"
                 , disabled: disabled'
                 , on: { click: click email' }}
        [ H.text "Submit" ]
      ]
    where
      click :: Email -> R.SyntheticEvent DE.MouseEvent -> Effect Unit
      click email' e = do
        liftEffect $ T.write_ true disabled
        E.preventDefault e
        here.log2 "email" email'
        here.log2 "backend" backend
        here.log2 "sessions" sessions
        launchAff_ $ do
          res <- postForgotPasswordRequest backend email'
          liftEffect $ here.log2 "res" res
          liftEffect $ case res of
            Left s -> do
              T.write_ false disabled
              T.write_ s message
            Right _ -> T.write_ "Request sent!" message

messageDisplay :: R2.Leaf (message :: T.Box String)
messageDisplay = R2.leafComponent messageDisplayCpt

messageDisplayCpt :: R.Component (message :: T.Box String)
messageDisplayCpt = here.component "messageDisplay" cpt where
  cpt {message} _ = do
    message' <- T.useLive T.unequal message

    pure $ H.p {} [H.text message']