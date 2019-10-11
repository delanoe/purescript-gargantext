-- The Login component is a modal which allows the user to:
  -- See the current login session
  -- Select a backend and log into it
module Gargantext.Components.Login where

import Prelude (Unit, bind, const, discard, pure, flip, show, ($), (<>), (*>), (<$>))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd)
import DOM.Simple.Console (log)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (launchAff_)
import Reactix as R
import Reactix.DOM.HTML as H

------------------------------------------------------------------------
import Gargantext.Components.Forms (clearfix, card, cardBlock, cardGroup, center, formGroup)
import Gargantext.Components.Login.Types (AuthRequest(..))
import Gargantext.Ends (Backend(..))
import Gargantext.Sessions (Session, Sessions, postAuthRequest, unSessions)
import Gargantext.Sessions as Sessions
import Gargantext.Utils (csrfMiddlewareToken)
import Gargantext.Utils.Reactix as R2

-- TODO: ask for login (modal) or account creation after 15 mn when user
-- is not logged and has made one search at least

type Props =
  ( backends :: Array Backend
  , sessions :: R2.Reductor Sessions Sessions.Action
  , visible :: R.State Boolean )

type ModalProps = ( visible :: R.State Boolean )

modal :: Record ModalProps -> R.Element -> R.Element
modal props child = R.createElement modalCpt props [ child ]

modalCpt :: R.Component ModalProps
modalCpt = R.hooksComponent "G.C.Login.modal" cpt where
  cpt {visible} children = do
    R.createPortal elems <$> R2.getPortalHost
    where
      click _ = log "click!" *> (snd visible) (const false)
      elems = 
        [ H.div { id: "loginModal", className: modalClass (fst visible), key: 0
                , role: "dialog", "data": {show: true}, style: {display: "block"}}
          [ H.div { className: "modal-dialog", role: "document"}
            [ H.div { className: "modal-content" }
              [ H.div { className: "modal-header" }
                [ H.h5 { className: "modal-title" } []
                , H.button { "type": "button", className: "close"
                           , "data": { dismiss: "modal" } }
                  [ H.span { on: {click} } [ H.text "X" ] ] ]
              , H.div { className: "modal-body" } children ] ] ] ]
      modalClass s = "modal myModal" <> if s then "" else " fade"
      

login :: Record Props -> R.Element
login props = R.createElement loginCpt props []

loginCpt :: R.Component Props
loginCpt = R.hooksComponent "G.C.Login.login" cpt
  where
    cpt props@{backends, sessions, visible} _ = do
      backend <- R.useState' Nothing
      pure $
        modal {visible} $
          case fst backend of
            Nothing -> chooser { backends, backend, sessions, visible }
            Just b -> form { sessions, visible, backend: b }

type ChooserProps = ( backend :: R.State (Maybe Backend) | Props )

chooser :: Record ChooserProps -> R.Element
chooser props = R.createElement chooserCpt props []

chooserCpt :: R.Component ChooserProps
chooserCpt = R.staticComponent "G.C.Login.chooser" cpt where
  cpt :: Record ChooserProps -> Array R.Element -> R.Element
  cpt {backend, backends, sessions} _ =
    H.ul {}
    [ renderSessions sessions, renderBackends backends backend ]
  
renderSessions :: R2.Reductor Sessions Sessions.Action -> R.Element
renderSessions sessions = R.fragment (renderSession <$> unSessions (fst sessions))

renderSession :: Session -> R.Element
renderSession session = H.li {} [ H.text $ "Active session: " <> show session ]

renderBackends :: Array Backend -> R.State (Maybe Backend) -> R.Element
renderBackends backends state = R.fragment $ (flip renderBackend $ state) <$> backends

renderBackend :: Backend -> R.State (Maybe Backend) -> R.Element
renderBackend backend@(Backend {name}) state =
  H.li {} [ H.a {on: {click}} [ H.text $ "Connect to " <> name ] ] where
    click _ = (snd state) (const $ Just backend)

type FormProps =
  ( backend :: Backend
  , sessions :: R2.Reductor Sessions Sessions.Action
  , visible :: R.State Boolean )

form :: Record FormProps -> R.Element
form props = R.createElement formCpt props []

formCpt :: R.Component FormProps
formCpt = R.hooksComponent "G.C.Login.form" cpt where
  cpt :: Record FormProps -> Array R.Element -> R.Hooks R.Element
  cpt props@{backend, sessions, visible} _ = do
    error <- R.useState' ""
    username <- R.useState' ""
    password <- R.useState' ""
    pure $ H.div {className: "row"}
      [ logo
      , cardGroup
        [ card
          [ cardBlock
            [ center
              [ H.h4 {className: "m-b-0"}
                [ H.span {className: "icon-text"} [ H.text "Welcome :)" ] ]
              , H.p {className: "text-muted"}
                [ H.text $ "Login to your account or", requestAccessLink {} ] ]
            , H.div {}
              [ csrfTokenInput {}
              , formGroup [ H.p {} [ H.text (fst error) ], usernameInput username ]
              , formGroup [ passwordInput password, clearfix {} ]
              , center
                [ H.label {}
                  [ H.div {className: "checkbox"}
                    [ termsCheckbox {}, H.text "I accept the terms of use ", termsLink {} ] ]
                , loginSubmit $
                    onClick props error username password ] ] ] ] ] ]
  onClick {backend, sessions, visible} error username password e =
    launchAff_ $ do
      let req = AuthRequest {username: fst username, password: fst password}
      res <- postAuthRequest backend req
      case res of
        Left message -> liftEffect $ (snd error) (const message)
        Right sess -> liftEffect $ do
          (snd sessions) (Sessions.Login sess)
          (snd error) (const "")
          (snd visible) (const false)
  logo =
    H.div {className: "col-md-10 col-md-push-1"}
        [ H.h2 {className: "text-primary center m-a-2"}
    [ H.i {className: "material-icons md-36"} [ H.text "control_point" ]
    , H.span {className: "icon-text"} [ H.text "Gargantext" ] ] ]

csrfTokenInput :: {} -> R.Element
csrfTokenInput _ =
  H.input { type: "hidden", name: "csrfmiddlewaretoken"
          , value: csrfMiddlewareToken }-- TODO hard-coded CSRF token

termsCheckbox :: {} -> R.Element
termsCheckbox _ =
  H.input { id: "terms-accept", type: "checkbox", value: "", className: "checkbox" }

termsLink :: {} -> R.Element
termsLink _ =
  H.a { target: "_blank", href: termsUrl } [ H.text " [ Read the terms of use ] " ]
  where termsUrl = "http://gitlab.iscpif.fr/humanities/tofu/tree/master"

requestAccessLink :: {} -> R.Element
requestAccessLink _ =
  H.a { target: "_blank", href: applyUrl } [ H.text " request access" ]
  where applyUrl = "https://iscpif.fr/services/applyforourservices/"

usernameInput :: R.State String -> R.Element
usernameInput username =
  H.input { className: "form-control"
          , id: "id_username"
          , maxLength: "254"
          , name: "username"
          , placeholder: "username"
          , type: "text"
          , defaultValue: (fst username)
          --, on: {input: \e -> dispatch (SetUserName $ R2.unsafeEventValue e)}
          , on: {change: \e -> (snd username) $ const $ R2.unsafeEventValue e} }
 
passwordInput :: R.State String -> R.Element
passwordInput password =
  H.input { className: "form-control"
          , id: "id_password"
          , name: "password"
          , placeholder: "password"
          , type: "password"
          , defaultValue: (fst password)
          --, on: {input: \e -> dispatch (SetPassword $ R2.unsafeEventValue e)}
          , on: {change: \e -> (snd password) $ const $ R2.unsafeEventValue e} }

loginSubmit :: forall e. (e -> Effect Unit) -> R.Element
loginSubmit click =
  H.button { id, className, type: "submit", on: {click} } [ H.text "Login" ]
  where
    id = "login-button"
    className = "btn btn-primary btn-rounded"

