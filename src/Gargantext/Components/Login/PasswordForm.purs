module Gargantext.Components.Login.PasswordForm
  ( component
  ) where

import Gargantext.Prelude

import Data.Either (Either(..))
import Data.Foldable (foldl, intercalate)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ButtonVariant(..), ComponentStatus(..), Elevation(..), Sizing(..), Variant(..))
import Gargantext.Components.Login.Types (FormType(..))
import Gargantext.Ends (Backend)
import Gargantext.Hooks.FormValidation (VForm, useFormValidation)
import Gargantext.Hooks.FormValidation.Unboxed as FV
import Gargantext.Hooks.StateRecord (useStateRecord)
import Gargantext.Sessions (postForgotPasswordRequest)
import Gargantext.Utils ((?))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Login.ForgotPassword"

type Props =
  ( backend   :: Backend
  , formType  :: T.Box FormType
  )

component :: R2.Leaf Props
component = R2.leaf componentCpt
componentCpt :: R.Component Props
componentCpt = here.component "main" cpt where
  cpt { backend
      , formType
      } _ = do
    -- | States
    -- |
    error' /\ error
      <- R2.useBox' (Nothing :: Maybe String)

    success' /\ success
      <- R2.useBox' (Nothing :: Maybe String)

    onPending' /\ onPending
      <- R2.useBox' false

    -- | Hooks
    -- |
    { state
    , bindStateKey
    } <- useStateRecord (defaultData :: FormData)

    fv <- useFormValidation

    -- | Behaviors
    -- |
    let
      onReturnClick :: Unit -> Effect Unit
      onReturnClick _ = T.write_ (Login) formType

      onSubmit :: Unit -> Effect Unit
      onSubmit _ = do

        result <- fv.try (\_ -> formValidation state)

        case result of

          Left err -> here.log3 "validation error" state err

          Right _ -> do

            T.write_ true onPending
            launchAff_
              $   sendEmail backend state
              >>= case _ of

                Left err -> liftEffect
                  $   here.warn3 "request error" state err
                  *>  T.write_ (Just err) error
                  *>  T.write_ false onPending

                Right _ -> liftEffect
                  $   T.write_ (Just "Request sent!") success

    -- | Render
    -- |
    pure $

      H.div
      { className: "forgot-password-form" }
      [
        H.div
        { className: "forgot-password-form__title" }
        [
          B.iconButton
          { name: "arrow-left"
          , className: "forgot-password-form__title__return"
          , elevation: Level2
          , callback: onReturnClick
          }
        ,
          H.span
          { className: "forgot-password-form__title__text" }
          [
            H.text $ "garg://" <> show backend
          ]
        ]
      ,
        B.div'
        { className: "forgot-password-form__subtitle" }
        "Password forgotten"
      ,
        H.form
        { className: "forgot-password-form__form" }
        [
          -- Username
          H.div
          { className: intercalate " "
              [ "form-group"
              , (fv.hasError' "email") ?
                  "form-group--error" $
                  mempty
              ]
          }
          [
            H.div { className: "form-group__label" }
            [
              H.label {} [ H.text "Email" ]
            ]
          ,
            H.div { className: "form-group__field" }
            [
              B.formInput $
              { size: LargeSize
              } `Record.merge` bindStateKey "email"
            ,
              R2.when (fv.hasError' "email") $

                B.div'
                { className: "form-group__error" }
                "Invalid email format"
            ]
          ]
        ,
          -- Error
          R2.fromMaybe error' $

            B.div'
            { className: "forgot-password-form__error" }
        ,
          -- Suvvess
          R2.fromMaybe success' $

            B.div'
            { className: "forgot-password-form__success" }
        ,
          -- Submit
          B.button
          { callback: onSubmit
          , status: onPending' ? Disabled $ Enabled
          , variant: ButtonVariant Primary
          , type: "submit"
          , className: "forgot-password-form__submit"
          }
          [ H.text "Submit" ]
        ]
      ]

type FormData =
  { email :: String
  }

defaultData :: FormData
defaultData =
  { email: ""
  }

formValidation :: FormData -> Effect VForm
formValidation r = foldl append mempty rules
  where
    rules =
      [ FV.email "email" r.email
      ]

-------------------------------------------------------

sendEmail ::
      Backend
  ->  FormData
  ->  Aff (Either String { status :: String })
sendEmail backend { email } = postForgotPasswordRequest backend email
