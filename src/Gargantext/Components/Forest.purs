module Gargantext.Components.Forest where

import DOM.Simple.Console (log)

import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Gargantext.AsyncTasks as GAT
import Gargantext.Components.Forest.Tree (treeView)
import Gargantext.Ends (Frontends, Backend(..))
import Gargantext.Prelude
import Gargantext.Routes (AppRoute)
import Gargantext.Sessions (Session(..), Sessions, OpenNodes, unSessions)
import Gargantext.Types (Reload, Handed(..))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H

thisModule = "Gargantext.Components.Forest"

type Props =
  ( frontends :: Frontends
  , handed    :: Handed
  , reload    :: R.State Int
  , route     :: AppRoute
  , sessions  :: Sessions
  , showLogin :: R2.Setter Boolean
  , backend   :: R.State (Maybe Backend)
  )

forest :: Record Props -> R.Element
forest props = R.createElement forestCpt props []

forestCpt :: R.Component Props
forestCpt = R2.hooksComponent thisModule "forest" cpt where
  cpt { frontends, handed, reload: extReload, route, sessions, showLogin, backend} _ = do
    -- NOTE: this is a hack to reload the tree view on demand
    reload     <- R.useState' (0 :: Reload)
    openNodes  <- R2.useLocalStorageState R2.openNodesKey (Set.empty :: OpenNodes)
    asyncTasks <- R2.useLocalStorageState GAT.localStorageKey GAT.empty
    R2.useCache
      (  frontends
      /\ route
      /\ sessions
      /\ fst openNodes
      /\ fst extReload
      /\ fst reload
      /\ fst asyncTasks
      /\ handed
      )
      (cpt' openNodes asyncTasks reload showLogin backend)
  cpt' openNodes asyncTasks reload showLogin backend (frontends /\ route /\ sessions /\ _ /\ _ /\ _ /\ _ /\ handed) = do
    pure $ R2.row $ [plus handed showLogin backend] <> trees
    where
      trees = tree <$> unSessions sessions
      tree s@(Session {treeId}) =
        treeView { root: treeId
                 , asyncTasks
                 , frontends
                 , handed
                 , mCurrentRoute: Just route
                 , openNodes
                 , reload
                 , session: s
                 }

plus :: Handed -> R2.Setter Boolean -> R.State (Maybe Backend) -> R.Element
plus handed showLogin backend = H.div {className: if handed == RightHanded
                                             then "flex-start"  -- TODO we should use lefthanded SASS class here
                                             else "flex-end"
                              } [
  H.button { title: "Add or remove connections to the server(s)."
           , on: {click}
           , className: "btn btn-default"
           }
          [ H.div { "type": ""
                  , className: "fa fa-universal-access fa-lg"
                  } [H.text " Log "]
          , H.div {} [H.text "    "]
  --, H.div { "type": "", className: "fa fa-plus-circle fa-lg"} []
  --, H.div { "type": "", className: "fa fa-minus-circle fa-lg"} []
          ]
          ]
  -- TODO same as the one in the Login Modal (same CSS)
  -- [ H.i { className: "material-icons md-36"} [] ]
  where
    click _ = (snd backend) (const Nothing)
            *> showLogin (const true)
