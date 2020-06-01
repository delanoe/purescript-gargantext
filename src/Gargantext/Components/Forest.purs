module Gargantext.Components.Forest where

import Gargantext.Prelude

import Data.Array as A
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.AsyncTasks as GAT
import Gargantext.Components.Forest.Tree (treeView)
import Gargantext.Components.Forest.Tree.Node.Action (Reload)
import Gargantext.Ends (Frontends)
import Gargantext.Routes (AppRoute)
import Gargantext.Sessions (Session(..), Sessions, OpenNodes, unSessions)
import Gargantext.Utils.Reactix as R2

type Props =
  ( frontends :: Frontends
  , reload :: R.State Int
  , route     :: AppRoute
  , sessions  :: Sessions
  , showLogin :: R2.Setter Boolean
  )

forest :: Record Props -> R.Element
forest props = R.createElement forestCpt props []

forestCpt :: R.Component Props
forestCpt = R.hooksComponent "G.C.Forest.forest" cpt where
  cpt { frontends, reload: extReload, route, sessions, showLogin } _ = do
    -- NOTE: this is a hack to reload the tree view on demand
    reload <- R.useState' (0 :: Reload)
    openNodes <- R2.useLocalStorageState R2.openNodesKey (Set.empty :: OpenNodes)
    asyncTasks <- R2.useLocalStorageState GAT.localStorageKey GAT.empty
    R2.useCache
      (frontends /\ route /\ sessions /\ fst openNodes /\ fst extReload /\ fst reload /\ fst asyncTasks)
      (cpt' openNodes asyncTasks reload showLogin)
  cpt' openNodes asyncTasks reload showLogin (frontends /\ route /\ sessions /\ _ /\ _ /\ _ /\ _) = do
    pure $ R.fragment $ A.cons (plus showLogin) trees
    where
      trees = tree <$> unSessions sessions
      tree s@(Session {treeId}) =
        treeView { root: treeId, asyncTasks, frontends, mCurrentRoute: Just route, session: s, openNodes, reload }

plus :: R2.Setter Boolean -> R.Element
plus showLogin =
  H.button {on: {click}, className: "btn btn-primary"}
  [ H.div { "type": "", className: "fa fa-universal-access fa-lg"} [H.text " Log "]
  , H.div {} [H.text "    "]
  --, H.div { "type": "", className: "fa fa-plus-circle fa-lg"} []
  --, H.div { "type": "", className: "fa fa-minus-circle fa-lg"} []
  ]
  -- TODO same as the one in the Login Modal (same CSS)
  -- [ H.i { className: "material-icons md-36"} [] ]
  where
    click _ = do
      showLogin $ const true
