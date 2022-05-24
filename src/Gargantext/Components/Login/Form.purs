module Gargantext.Components.Login.Form where

import Prelude (Unit, bind, discard, not, notEq, pure, show, ($), (&&), (*>), (<>))
import Data.Either (Either(..))
import DOM.Simple.Event as DE
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Formula as F
import Reactix as R
import Reactix.SyntheticEvent as E
import Reactix.DOM.HTML as H
import Toestand as T
import Toestand (useFocusedFields)

import Gargantext.Components.Forms (clearfix, formGroup)
import Gargantext.Components.Login.Types (AuthRequest(..), FormType(..))
import Gargantext.Ends (Backend)
import Gargantext.Sessions as Sessions
import Gargantext.Sessions (Sessions, postAuthRequest)
import Gargantext.Utils (csrfMiddlewareToken)
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.Login.Form"

type Form =
  { error    :: String
  , username :: String
  , password :: String
  , agreed   :: Boolean
  }

emptyForm :: Form
emptyForm = { error: "", username: "", password: "", agreed: false }

type Boxes =
  { error    :: T.Box String
  , username :: T.Box String
  , password :: T.Box String
  , agreed   :: T.Box Boolean }

formBoxes :: T.Box Form -> R.Hooks Boxes
formBoxes box = useFocusedFields box {}

type Props s v =
  ( backend  :: Backend
  , formType :: T.Box FormType
  , sessions :: s
  , visible  :: v
  )

form :: forall s v. T.ReadWrite s Sessions => T.ReadWrite v Boolean
     => Record (Props s v) -> R.Element
form props = R.createElement formCpt props []
formCpt :: forall s v. T.ReadWrite s Sessions => T.ReadWrite v Boolean
        => R.Component (Props s v)
formCpt = here.component "form" cpt where
  cpt { backend, formType, sessions, visible } _ = do
    cell    <- T.useBox emptyForm
    cursors <- useFocusedFields cell {}
    pure $ R2.row
      [ H.form { className: "col-md-12" }
        [ formLoginLink backend
        , requestAccessLink
        , csrfTokenInput
        , formGroup
          [ H.p {} [ F.viewText { text: cursors.error } ]
          , usernameInput cursors.username ]
        , formGroup
          [ passwordInput cursors.password
          , clearfix ]
        , termsCheckbox cursors.agreed
        , forgotPassword { formType }
        , submitButton { backend, formType, sessions, visible, cell }
        ]]

-- might be wrong, all we care about is preventDefault
type ChangeEvent = R.SyntheticEvent DE.MouseEvent

formLoginLink :: Backend -> R.Element
formLoginLink backend =
  H.h4 { className: "text-center" } {-className: "text-muted"-}
  [ H.text $ "Login to garg://" <> show backend ]

type SubmitButtonProps s v = ( cell :: T.Box Form | Props s v )

submitButton
  :: forall s v. T.ReadWrite s Sessions => T.Write v Boolean
  => R2.Leaf (SubmitButtonProps s v)
submitButton = R2.leafComponent submitButtonCpt
submitButtonCpt
  :: forall s v. T.ReadWrite s Sessions => T.Write v Boolean
  => R.Component (SubmitButtonProps s v)
submitButtonCpt = here.component "submitButton" cpt where
  cpt { backend, formType, sessions, visible, cell } _ = do
    { agreed, username, password } <- T.useLive T.unequal cell
    let isValid = agreed && (username `notEq` "") && (password `notEq` "")
    pure $ H.div { className: "text-center" }
           [ loginSubmit isValid $ submitForm { backend, formType, sessions, visible } cell ]
    
-- Attempts to submit the form
submitForm :: forall s v. T.ReadWrite s Sessions => T.Write v Boolean
           => Record (Props s v) -> T.Box Form -> ChangeEvent -> Effect Unit
submitForm { backend, sessions, visible } cell e = do
  E.preventDefault e
  state <- T.read cell
  launchAff_ $ do
    res <- postAuthRequest backend (req state)
    case res of
      Left message -> liftEffect $ T.write (state { error = message }) cell
      Right sess ->
        liftEffect $
          Sessions.change (Sessions.Login sess) sessions
          *> T.write false visible
          *> T.write (state { error = "" }) cell
    where
      req { username, password } = AuthRequest { username, password }

csrfTokenInput :: R.Element -- TODO hard-coded CSRF token
csrfTokenInput = H.input { type: "hidden", name, value } where
  name = "csrfmiddlewaretoken"
  value = csrfMiddlewareToken

termsCheckbox :: forall cell. T.ReadWrite cell Boolean => cell -> R.Element
termsCheckbox checked =
  H.div { className: "form-group form-check text-center" }
  [ F.bindCheckbox { checked, className: "form-check-input" }
  , H.label { className: "form-check-label" }
    [ H.text "I hereby accept the "
    , H.a { target: "_blank", href: termsUrl }
      [ H.text "terms of use" ] ]]
  where termsUrl = "http://gitlab.iscpif.fr/humanities/tofu/tree/master"

requestAccessLink :: R.Element
requestAccessLink =
  H.div { className: "text-center" }
  [ H.a { href, target: "_blank" } [ H.text "request access" ] ]
  where href = "https://iscpif.fr/apply-for-a-services-account/"

usernameInput :: forall cell. T.ReadWrite cell String => cell -> R.Element
usernameInput value =
  F.bindInput
  { value
  , type: "text",         className:   "form-control"
  , id:   "id_username",  placeholder: "username"
  , name: "username",     maxLength:   "254"
  }

passwordInput :: forall cell. T.ReadWrite cell String => cell -> R.Element
passwordInput value =
  F.bindInput
  { value
  , type: "password",     className:   "form-control"
  , name: "password",     placeholder: "password"
  , id:   "id_password"
  }

loginSubmit :: Boolean -> (ChangeEvent -> Effect Unit) -> R.Element
loginSubmit isEnabled click =
  H.button { id
           , className
           , disabled: not isEnabled
           , type: "submit"
           , on: { click } }
  [ H.text "Login" ] where
    id = "login-button"
    className = "btn btn-primary btn-rounded"

type ForgotPasswordProps =
  ( formType :: T.Box FormType )

forgotPassword :: R2.Leaf ForgotPasswordProps
forgotPassword = R2.leaf forgotPasswordCpt
forgotPasswordCpt :: R.Component ForgotPasswordProps
forgotPasswordCpt = here.component "forgotPassword" cpt where
  cpt { formType } _ = do
    pure $ H.div { className: "form-group text-center" }
      [ H.button { className: "btn btn-danger"
                 , on: { click } } [ H.text "Forgot password" ]
      ]
    where
      click _ = T.write_ ForgotPassword formType
