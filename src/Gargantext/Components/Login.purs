-- The Login component is a modal which allows the user to:
-- * See the currently logged in sessions
-- * Select a backend and log into it
module Gargantext.Components.Login where

import Data.Array (head)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as DST
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.Prelude
import Gargantext.Components.Login.Modal (modal)
import Gargantext.Components.Login.Form (form)
import Gargantext.Components.NgramsTable.Loader as NTL
import Gargantext.Ends (Backend(..))
import Gargantext.Hooks.Loader as GHL
import Gargantext.Sessions (Session, Sessions, Action(Logout), unSessions)
import Gargantext.Sessions as Sessions
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2

here :: R2.Here
here = R2.here "Gargantext.Components.Login"

-- TODO
-- enable anonymous login for first map
-- and ask for login (modal) or account creation after 15 mn when user
-- if not logged user can not save his work

type Props =
  ( backends :: Array Backend
  , backend  :: T.Cursor (Maybe Backend)
  , sessions :: T.Cursor Sessions
  , visible  :: T.Cursor Boolean
  )

login :: R2.Leaf Props
login props = R.createElement loginCpt props []

loginCpt :: R.Component Props
loginCpt = here.component "login" cpt where
  cpt props@{ visible, sessions } _ = do
    b <- T.useLive T.unequal props.backend
    pure $ modal { visible } (inner b) where
      inner Nothing = chooser props
      inner (Just b) = form { sessions, visible, backend: b }

chooser :: R2.Leaf Props
chooser props = R.createElement chooserCpt props []

chooserCpt :: R.Component Props
chooserCpt = here.component "chooser" cpt where
  cpt { backend, backends, sessions } _ = do
    sessions' <- T.useLive T.unequal sessions
    pure $
      R.fragment $
        [ H.h2 { className: "center modal-title" }
          [ H.text "Instances manager" ]]
        <> activeConnections sessions sessions' <>
        [ H.h3 {} [ H.text "Existing connections" ]
        , H.table { className : "table" }
          [ H.thead { className: "thead-light" }
            [ H.tr {} (map header headers) ]
          , H.tbody {} (map (renderBackend backend) backends) ]
        , H.input { className: "form-control", type:"text", placeholder } ]
  placeholder = "Search for your institute"
  headers = [ "", "Label of instance", "Gargurl" ]
  header label = H.th {} [ H.text label ]

-- Shown in the chooser
activeConnections :: forall s. T.ReadWrite s Sessions => s -> Sessions -> Array R.Element
activeConnections sessions sessions' | Sessions.null sessions' = []
activeConnections sessions sessions' =
  [ H.h3 {} [ H.text "Active connection(s)" ]
  , H.ul {} [ renderSessions sessions sessions' ] ]

renderSessions :: forall s. T.ReadWrite s Sessions => s -> Sessions -> R.Element
renderSessions sessions sessions' =
  R.fragment (map renderSession $ unSessions sessions') where
    renderSession session =
      H.li {}
      [ H.text $ show session
      , signOutButton sessions session
      , clearCacheButton ]

signOutButton :: forall c. T.ReadWrite c Sessions => c -> Session -> R.Element
signOutButton sessions session =
  H.a { className, on: { click }, id: "log-out", title: "Log out" } [] where
    className = "glyphitem fa fa-sign-out"
    click _ = Sessions.change (Logout session) sessions

clearCacheButton :: R.Element
clearCacheButton =
  H.a { className, on: { click }, id: "log-out", title: "Clear cache" } [] where
    className = "glyphitem fa fa-eraser" 
    click _ =
      launchAff_
        $  GHL.clearCache unit
        *> NTL.clearCache unit
        *> liftEffect (here.log "cache cleared")

renderBackend :: forall b. T.Write b (Maybe Backend) => b -> Backend -> R.Element
renderBackend cursor backend@(Backend {name}) =
  H.tr {}
  [ H.td {} [ H.a { on: { click }, title: "Log In", className } [] ]
  , H.td {} [ H.a { on: { click }} [ H.text (backendLabel name) ]]
  , H.td {} [ H.text $ "garg://" <> name ]] where
    className = "fa fa-hand-o-right" -- "glyphitem fa fa-log-in"
    click _ = T2.write_ (Just backend) cursor

backendLabel :: String -> String
backendLabel =
  DST.toUpper <<< fromMaybe "" <<< head <<< DST.split (DST.Pattern ".")
