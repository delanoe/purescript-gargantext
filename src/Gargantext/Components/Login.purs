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
import Gargantext.Sessions (Session(..), Sessions, Action(Logout), unSessions)
import Gargantext.Sessions as Sessions
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.Login"

-- TODO
-- enable anonymous login for first map
-- and ask for login (modal) or account creation after 15 mn when user
-- if not logged user can not save his work

type Props =
  ( backend  :: T.Box (Maybe Backend)
  , backends :: Array Backend
  , sessions :: T.Box Sessions
  , visible  :: T.Box Boolean
  )

login :: R2.Leaf Props
login props = R.createElement loginCpt props []

loginCpt :: R.Component Props
loginCpt = here.component "login" cpt where
  cpt props@{ backend, sessions, visible } _ = do
    b <- T.useLive T.unequal backend
    pure $ modal { visible } (inner b) where
      inner Nothing = chooser props
      inner (Just b) = form { backend: b, sessions, visible }

chooser :: R2.Leaf Props
chooser props = R.createElement chooserCpt props []

chooserCpt :: R.Component Props
chooserCpt = here.component "chooser" cpt where
  cpt { backend, backends, sessions } _ = do
    sessions' <- T.useLive T.unequal sessions
    pure $
      R.fragment $
        [ H.h2 { className: "mx-auto" } [ H.text "Workspace manager" ]]
        <> activeConnections sessions sessions' <>
        [ H.h3 {} [ H.text "Existing places (click to login)" ]
        , H.table { className : "table" }
                  [ H.thead { className: "thead-light" }
                            [ H.tr {} (map header headers) ]
                  , H.tbody {} (map (renderBackend backend) backends)
                  ]
        , H.input { className: "form-control", type:"text", placeholder } ]
  placeholder = "Search for your institute"
  headers = [ "", "GarganText places", "Fonction", "Garg protocol url" ]
  header label = H.th {} [ H.text label ]

-- Shown in the chooser
activeConnections :: forall s. T.ReadWrite s Sessions => s -> Sessions -> Array R.Element
activeConnections _        sessions' | Sessions.null sessions' = []
activeConnections sessions sessions' =
  [ H.h3 {} [ H.text "Active place(s)" ]
  , H.table { className : "table" }
                  [ H.thead { className: "thead-light" }
                            [ H.tr {} (map header headers) ]
                  , H.tbody {} [renderSessions sessions sessions']
                  ]
  ]
    where
      headers = [ "", "Active(s) connection(s)", "Fonction", "Clear data/Logout"]
      header label = H.th {} [ H.text label ]



renderSessions :: forall s. T.ReadWrite s Sessions => s -> Sessions -> R.Element
renderSessions sessions sessions' =
  R.fragment (map renderSession $ unSessions sessions')
    where
      renderSession session@(Session {backend}) =
        H.tr {}
        [ H.td {} [H.text ""] 
        , H.td {} [H.text $ show session]
        , H.td {} [H.text backendType]
        , H.td {} [signOutButton sessions session, clearCacheButton]
        ]
          where
            Backend {backendType} = backend

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
renderBackend cursor backend@(Backend {name, baseUrl, backendType}) =
  H.tr {}
  [ H.td {} [ H.a { on: { click }, title: "Log In", className } [] ]
  , H.td {} [ H.a { on: { click }} [ H.text (backendLabel name) ]]
  , H.td {} [ H.a { on: { click }} [ H.text backendType ]]
  , H.td {} [ H.text $ DST.replace (DST.Pattern "http") (DST.Replacement "garg") $ baseUrl ]
  ]
    where
      className = "fa fa-hand-o-right" -- "glyphitem fa fa-log-in"
      click _ = T.write_ (Just backend) cursor

backendLabel :: String -> String
backendLabel =
  DST.toUpper <<< fromMaybe "" <<< head <<< DST.split (DST.Pattern ".")
