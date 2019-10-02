module Gargantext.Components.Forest where

import Prelude (const)
import Data.Maybe (Maybe(..))
import Reactix as R
import Reactix.DOM.HTML as H
import Gargantext.Ends (Frontends)
import Gargantext.Routes (AppRoute)
import Gargantext.Sessions (Session(..), Sessions, unSessions)
import Gargantext.Components.Tree (treeView)
import Gargantext.Utils.Reactix as R2

type Props =
  ( sessions  :: Sessions
  , route     :: AppRoute
  , frontends :: Frontends
  , showLogin :: R2.Setter Boolean
  )

forest :: Record Props -> R.Element
forest props = R.createElement forestCpt props []

forestCpt :: R.Component Props
forestCpt = R.staticComponent "G.C.Forest.forest" cpt where
  cpt {sessions, route, frontends, showLogin} _ =
    R.fragment [ plus showLogin, trees ]
    where
      trees =
        case unSessions sessions of
          Nothing -> R.fragment []
          Just s@(Session {treeId}) ->
            R.fragment
            [ treeView { root: treeId
                       , frontends
                       , mCurrentRoute: Just route
                       , session: s }
            ]

plus :: R2.Setter Boolean -> R.Element
plus showLogin = H.button {on: {click}}
                          [ H.i { className: "glyphicon glyphicon-log-in"}
                                []
                          ]
  where
    click _ = do
      showLogin (const true)
