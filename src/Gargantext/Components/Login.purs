-- The Login component is a modal which allows the user to:
  -- See the current login session
  -- Select a backend and log into it
module Gargantext.Components.Login where

import Prelude (Unit, bind, const, discard, pure, show, ($), (<>), (*>), (<$>), (>), map, (==), (/=), not, (&&))
import Data.Array (head)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Data.String as DST
import DOM.Simple.Console (log)
import Data.Sequence as DS
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (launchAff_)
import Reactix as R
import Reactix.DOM.HTML as H

------------------------------------------------------------------------
import Gargantext.Components.Forms (clearfix, card, cardBlock, cardGroup, center, formGroup)
import Gargantext.Components.Login.Types (AuthRequest(..))
import Gargantext.Ends (Backend(..))
import Gargantext.Sessions (Session, Sessions(..), postAuthRequest, unSessions)
import Gargantext.Sessions as Sessions
import Gargantext.Utils (csrfMiddlewareToken)
import Gargantext.Utils.Reactix as R2

-- TODO: ask for login (modal) or account creation after 15 mn when user
-- is not logged and has made one search at least

type Props =
  ( backends :: Array Backend
  , sessions :: R2.Reductor Sessions Sessions.Action
  , visible  :: R.State Boolean
  )

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
                [ closing
                , logo
                , H.h2 { className: "center modal-title" } [H.text "Instances manager"]
                ]
              , H.div { className: "modal-body" } children ] ] ] ]
      modalClass s = "modal myModal" <> if s then "" else " fade"
      logo =
        H.div {className: "col-md-10 col-md-push-1"}
            [ H.h2 {className: "text-primary center m-a-2"}
        [
          -- H.i {className: "material-icons md-36"} [ H.text "control_point" ]
          H.span {className: "icon-text"} [ H.text "Gargantext" ] ] ]

      closing = H.button { "type": "button", className: "close"
                           , "data": { dismiss: "modal" } }
                           [ H.a { on: {click}
                                 , className: "btn glyphicon glyphicon-remove-circle" 
                                 -- TODO , font-size : "50px"
                                 } [] 
                           ]



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
    R.fragment $ active <> new <> search
      where
        active = if DS.length ss > 0 then [ H.h3 {} [H.text "Active connection(s)"]
                 , H.ul {} [ renderSessions sessions]
                 ] else [] where
                   Sessions {sessions:ss} = fst sessions
        search = [ H.input {className: "form-control", type:"text", placeholder: "Search for your institute"}]
        new    = [ H.h3 {} [H.text "Last connection(s)"]
                 , H.table {className : "table"}
                 [ H.thead {className: "thead-dark"} [ H.tr {} [ H.th {} [ H.text ""]
                                                               , H.th {} [H.text "Label of instance"]
                                                               , H.th {} [H.text "Gargurl"]
                                                               ]
                                                     ]
                           , H.tbody {} (map (renderBackend backend) backends)
                           ]
                 ]

renderSessions :: R2.Reductor Sessions Sessions.Action -> R.Element
renderSessions sessions = R.fragment (renderSession sessions <$> unSessions (fst sessions))
  where
    renderSession :: R2.Reductor Sessions Sessions.Action -> Session -> R.Element
    renderSession sessions' session = H.li {} $ [ H.text $ show session ]
                                            <> [ H.a { on : {click}
                                                     , className: "glyphitem glyphicon glyphicon-log-out"
                                                     , id : "log-out"
                                                     , title: "Log out"
                                                     } [] ]
                                                where
                                                  click _ = (snd sessions') (Sessions.Logout session)

renderBackend :: R.State (Maybe Backend) -> Backend -> R.Element
renderBackend state backend@(Backend {name}) =
  H.tr {} [ iconLog
          , H.td {} [H.a { on : {click}} [H.text label]]
          , H.td {} [ H.text url ]
          ]
    where
      iconLog = H.td {} [ H.a { on : {click}
                        , className : "glyphitem glyphicon glyphicon-log-in"
                        , title: "Log In"} []
                        ]

      click _ = (snd state) (const $ Just backend)
      label   = DST.toUpper $ fromMaybe "" $ head $ DST.split (DST.Pattern ".") name
      url     = "garg://" <> name

type FormProps =
  ( backend :: Backend
  , sessions :: R2.Reductor Sessions Sessions.Action
  , visible :: R.State Boolean
  )

form :: Record FormProps -> R.Element
form props = R.createElement formCpt props []

formCpt :: R.Component FormProps
formCpt = R.hooksComponent "G.C.Login.form" cpt where
  cpt :: Record FormProps -> Array R.Element -> R.Hooks R.Element
  cpt props@{backend, sessions, visible} _ = do
    error    <- R.useState' ""
    username <- R.useState' ""
    password <- R.useState' ""
    setBox@(checkBox /\ setCheckBox) <- R.useState' false
    pure $ R2.row
      [ cardGroup
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
                    [ termsCheckbox setBox , H.text "I accept the terms of use ", termsLink {} ] ]
                ] 
              ]
            , if checkBox == true
                 && fst username /= ""
                 && fst password /= ""
                 then H.div {} [center [loginSubmit $ onClick props error username password]]
                 else H.div {} []
            ] 
          ] 
        ] 
      ]
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

csrfTokenInput :: {} -> R.Element
csrfTokenInput _ =
  H.input { type: "hidden", name: "csrfmiddlewaretoken"
          , value: csrfMiddlewareToken }-- TODO hard-coded CSRF token

termsCheckbox :: R.State Boolean -> R.Element
termsCheckbox setCheckBox =
  H.input { id: "terms-accept"
          , type: "checkbox"
          , value: fst setCheckBox
          , className: "checkbox"
          , on: { click: \_ -> (snd setCheckBox) $ const $ not (fst setCheckBox)}
          }

termsLink :: {} -> R.Element
termsLink _ =
  H.a { target: "_blank", href: termsUrl } [ H.text " [Read the terms of use]" ]
  where termsUrl = "http://gitlab.iscpif.fr/humanities/tofu/tree/master"

requestAccessLink :: {} -> R.Element
requestAccessLink _ =
  H.a { target: "_blank", href: applyUrl } [ H.text " request access" ]
  where applyUrl = "https://iscpif.fr/apply-for-a-services-account/"

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

