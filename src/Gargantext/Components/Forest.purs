module Gargantext.Components.Forest where

import Prelude (const, otherwise, ($), (<>), (<$>))
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Sequence as Seq
import Reactix as R
import Reactix.DOM.HTML as H
import Gargantext.Ends (Frontends)
import Gargantext.Routes (AppRoute)
import Gargantext.Sessions (Session(..), Sessions, unSessions)
import Gargantext.Components.Forest.Tree (treeView)
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
    R.fragment $ A.cons (plus showLogin) trees
    where
      trees = tree <$> unSessions sessions
      tree s@(Session {treeId}) =
        treeView { root: treeId, frontends, mCurrentRoute: Just route, session: s }

plus :: R2.Setter Boolean -> R.Element
plus showLogin =
  H.button {on: {click}}
  [ H.i { className: "glyphicon glyphicon-plus"} [] ]
  -- TODO [ H.i { className: "material-icons md-36"} [] ]
  where
    click _ = do
      showLogin (const true)
