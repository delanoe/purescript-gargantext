module Gargantext.Components.Login.LoginForm
  ( component
  ) where

import Gargantext.Prelude

import Data.Either (Either(..))
import Data.Foldable (foldl, intercalate)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ButtonVariant(..), ComponentStatus(..), Elevation(..), Sizing(..), Variant(..))
import Gargantext.Components.Login.Types (AuthRequest(..), FormType(..))
import Gargantext.Ends (Backend)
import Gargantext.Hooks.FormValidation (VForm, useFormValidation)
import Gargantext.Hooks.FormValidation.Unboxed as FV
import Gargantext.Hooks.StateRecord (useStateRecord)
import Gargantext.Sessions (Session, Sessions, postAuthRequest)
import Gargantext.Sessions as Sessions
import Gargantext.Utils ((?))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Record.Unsafe (unsafeSet)
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Login.Form"

type Props =
  ( backend  :: Backend
  , formType :: T.Box FormType
  , sessions :: T.Box Sessions
  , visible  :: T.Box Boolean
  )

component :: R2.Leaf Props
component = R2.leaf componentCpt
componentCpt :: R.Component Props
componentCpt = here.component "main" cpt where
  cpt { backend
      , formType
      , sessions
      , visible
      } _ = do
    -- | States
    -- |
    error' /\ error
      <- R2.useBox' (Nothing :: Maybe String)

    onPending' /\ onPending
      <- R2.useBox' false

    -- | Hooks
    -- |
    { state
    , bindStateKey
    , stateBox
    } <- useStateRecord (defaultData :: FormData)

    fv <- useFormValidation

    -- | Behaviors
    -- |
    let
      onReturnClick :: Unit -> Effect Unit
      onReturnClick _ = T.write_ (Manager) formType

      onPasswordForgottenClick :: Unit -> Effect Unit
      onPasswordForgottenClick _ = T.write_ (ForgotPassword) formType

      onSubmit :: Unit -> Effect Unit
      onSubmit _ = do

        result <- fv.try (\_ -> formValidation state)

        case result of

          Left err -> here.log3 "validation error" state err

          Right _  -> do
            T.write_ true onPending
            launchAff_
              $   signin backend state
              >>= case _ of

                Left err -> liftEffect
                  $   here.warn3 "request error" state err
                  *>  T.write_ (Just err) error

                Right session_ -> liftEffect
                  $   Sessions.change (Sessions.Login session_) sessions
                  *>  T.write_ false visible

            T.write_ false onPending

      -- @XXX StateRecord with distinct value types
      onAgreedCheckboxChange :: Boolean -> Effect Unit
      onAgreedCheckboxChange value = T.modify_
        (\prev -> unsafeSet "agreed" value prev)
        stateBox

      -- @XXX StateRecord with distinct value types
      onAgreedLabelClick :: Unit -> Effect Unit
      onAgreedLabelClick _ = T.modify_
        (\prev -> unsafeSet "agreed" (not state.agreed) prev)
        stateBox

    -- | Render
    -- |
    pure $

      H.div
      { className: "login-modal-form" }
      [
        H.div
        { className: "login-modal-form__title" }
        [
          B.iconButton
          { name: "arrow-left"
          , className: "login-modal-form__title__return"
          , elevation: Level2
          , callback: onReturnClick
          }
        ,
          H.span
          { className: "login-modal-form__title__text" }
          [
            H.text $ "garg://" <> show backend
          ]
        ]
      ,
        H.a
        { href: "https://iscpif.fr/apply-for-a-services-account/"
        , target: "_blank"
        , className: intercalate " "
            [ "login-modal-form__request-access"
            , "btn btn-primary"
            ]
        }
        [
          B.icon
          { name: "hand-o-right" }
        ,
          B.wad_
          [ "d-inline-block", "virtual-space", "w-2" ]
        ,
          H.text "Request access"
        ]
      ,
        H.div
        { className: "login-modal-form__separator" }
        [
          H.hr
          {}
        ,
          H.span
          { className: "login-modal-form__separator__text" }
          [ H.text "or" ]
        ]
      ,
        H.form
        { className: "login-modal-form__form" }
        [
          -- (?) never used?
          -- H.input
          -- { type: "hidden"
          -- , name: "csrfmiddlewaretoken"
          -- , value: csrfMiddlewareToken
          -- }

          -- Username
          H.div
          { className: intercalate " "
              [ "form-group"
              , (fv.hasError' "username") ?
                  "form-group--error" $
                  mempty
              ]
          }
          [
            H.div { className: "form-group__label" }
            [
              H.label {} [ H.text "Username" ]
            ]
          ,
            H.div { className: "form-group__field" }
            [
              B.formInput $
              { size: LargeSize
              } `Record.merge` bindStateKey "username"
            ]
          ]
        ,
          -- Password
          H.div
          { className: intercalate " "
              [ "form-group"
              , (fv.hasError' "password") ?
                  "form-group--error" $
                  mempty
              ]
          }
          [
            H.div { className: "form-group__label" }
            [
              H.label {} [ H.text "Password" ]
            ]
          ,
            H.div { className: "form-group__field" }
            [
              B.formInput $
              { size: LargeSize
              , type: "password"
              } `Record.merge` bindStateKey "password"
            ,
              H.a
              { on: { click: onPasswordForgottenClick }
              , className: "font-size-95 text-decoration-underline float-right"
              }
              [ H.text "Password forgotten" ]
            ]
          ]
        ,
          -- Agreed
          H.div
          { className: intercalate " "
              [ "form-group"
              , (fv.hasError' "agreed") ?
                  "form-group--error" $
                  mempty
              , "d-flex"
              ]
          }
          [
            H.div
            { className: "form-group__field" }
            [
              B.formCheckbox
              { value: state.agreed
              , callback: onAgreedCheckboxChange
              }
            ]
          ,
            H.div
            { className: intercalate " "
                [ "form-group__label"
                , "px-1"
                ]
            }
            [
              H.label
              { on: { click: onAgreedLabelClick }
              , className: "cursor-pointer"
              }
              [
                H.text "I hereby accept the "
              ,
                H.a
                { target: "_blank"
                , className: "text-decoration-underline"
                , href: "http://gitlab.iscpif.fr/humanities/tofu/tree/master"
                }
                [ H.text "terms of use" ]
              ]
            ]
          ]
        ,
          -- Error
          R2.fromMaybe error' $

            B.div'
            { className: "login-modal-form__error" }
        ,
          -- Submit
          B.button
          { callback: onSubmit
          , status: onPending' ? Deferred $ Enabled
          , variant: ButtonVariant Primary
          , type: "submit"
          , className: "login-modal-form__log-in"
          }
          [
            B.icon
            { name: "sign-in" }
          ,
            B.wad_
            [ "d-inline-block", "virtual-space", "w-1" ]
          ,
            H.text "Log in"
          ]
        ]
      ]

type FormData =
  { username  :: String
  , password  :: String
  , agreed    :: Boolean
  }

defaultData :: FormData
defaultData =
  { username  : ""
  , password  : ""
  , agreed    : false
  }

formValidation :: FormData -> Effect VForm
formValidation r = foldl append mempty rules
  where
    rules =
      [ FV.nonEmpty "username" r.username
      , FV.nonEmpty "password" r.password
      , FV.equals "agreed" true r.agreed
      ]

-----------------------------------------------

signin ::
      Backend
  ->  FormData
  ->  Aff (Either String Session)
signin backend { username, password } = postAuthRequest backend request
  where
    -- (?) is `cleanString` necessary?
    request
      = AuthRequest { username: cleanString username
                    , password: cleanString password
                    }

cleanString :: String -> String
cleanString str =
  String.replace (String.Pattern " ")
                 (String.Replacement "") str
