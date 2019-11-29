module Gargantext.Components.Forest where

import Prelude (const, pure, ($), (<$>))
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested((/\))
import Reactix as R
import Reactix.DOM.HTML as H
import Gargantext.Ends (Frontends)
import Gargantext.Routes (AppRoute)
import Gargantext.Sessions (Session(..), Sessions, unSessions)
import Gargantext.Components.Forest.Tree (treeView)
import Gargantext.Utils.Reactix as R2

type Props =
  ( frontends :: Frontends
  , route     :: AppRoute
  , sessions  :: Sessions
  , showLogin :: R2.Setter Boolean
  )

forest :: Record Props -> R.Element
forest props = R.createElement forestCpt props []

forestCpt :: R.Component Props
forestCpt = R.hooksComponent "G.C.Forest.forest" cpt where
  cpt {frontends, route, sessions, showLogin} _ = R2.useCache (frontends /\ route /\ sessions) (cpt' showLogin)
  cpt' showLogin (frontends /\ route /\ sessions) =
    pure $ R.fragment $ A.cons (plus showLogin) trees
    where
      trees = tree <$> unSessions sessions
      tree s@(Session {treeId}) =
        treeView { root: treeId, frontends, mCurrentRoute: Just route, session: s }

plus :: R2.Setter Boolean -> R.Element
plus showLogin =
  H.button {on: {click}, className: "btn btn-primary"}
  [ H.div { "type": ""
          , className: "fa fa-plus-circle fa-lg"
          } [H.text "Login"] ]
  -- TODO same as the one in the Login Modal (same CSS)
  -- [ H.i { className: "material-icons md-36"} [] ]
  where
    click _ = do
      showLogin (const true)
