module Gargantext.Components.Forest where

import Data.Array (reverse)
import Data.Tuple (fst)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.AsyncTasks as GAT
import Gargantext.Components.Forest.Tree (treeView)
import Gargantext.Components.TopBar (topBar)
import Gargantext.Ends (Frontends, Backend(..))
import Gargantext.Prelude
import Gargantext.Routes (AppRoute)
import Gargantext.Sessions (Session(..), Sessions, OpenNodes, unSessions)
import Gargantext.Types (Reload, Handed(..))
import Gargantext.Utils.Reactix as R2

thisModule :: String
thisModule = "Gargantext.Components.Forest"

type Props =
  ( asyncTasks :: GAT.Reductor
  , backend    :: R.State (Maybe Backend)
  , frontends  :: Frontends
  , handed     :: Handed
  , reload     :: R.State Int
  , route      :: AppRoute
  , sessions   :: Sessions
  , showLogin  :: R.Setter Boolean
  )

forest :: Record Props -> R.Element
forest props = R.createElement forestCpt props []

forestCpt :: R.Component Props
forestCpt = R.hooksComponentWithModule thisModule "forest" cpt where
  cpt { asyncTasks, frontends, handed, reload: extReload, route, sessions, showLogin, backend} _ = do
    -- NOTE: this is a hack to reload the tree view on demand
    reload     <- R.useState' (0 :: Reload)
    openNodes  <- R2.useLocalStorageState R2.openNodesKey (Set.empty :: OpenNodes)
    R2.useCache
      (  frontends
      /\ route
      /\ sessions
      /\ fst openNodes
      /\ fst extReload
      /\ fst reload
      /\ (fst asyncTasks).storage
      /\ handed
      )
      (cpt' openNodes asyncTasks reload showLogin backend)
  cpt' openNodes asyncTasks reload showLogin backend (frontends /\ route /\ sessions /\ _ /\ _ /\ _ /\ _ /\ handed) = do
    pure $ R2.row $ [plus handed showLogin backend] <> trees
    where
      trees = tree <$> unSessions sessions
      tree s@(Session {treeId}) =
        treeView { asyncTasks
                 , frontends
                 , handed
                 , mCurrentRoute: Just route
                 , openNodes
                 , reload
                 , root: treeId
                 , session: s
                 }

plus :: Handed -> R.Setter Boolean -> R.State (Maybe Backend) -> R.Element
plus handed showLogin backend = H.div { className: handedClass } [
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
    handedClass = if handed == RightHanded then
                        "flex-start"  -- TODO we should use lefthanded SASS class here
                  else
                        "flex-end"

    click _ = (snd backend) (const Nothing)
            *> showLogin (const true)


-------------------------
type ForestLayoutProps =
  ( asyncTasks :: GAT.Reductor
  , backend   :: R.State (Maybe Backend)
  , child     :: R.Element
  , frontends :: Frontends
  , handed    :: R.State Handed
  , reload    :: R.State Int
  , route     :: AppRoute
  , sessions  :: Sessions
  , showLogin :: R.Setter Boolean
  )

forestLayout :: Record ForestLayoutProps -> R.Element
forestLayout props = R.createElement forestLayoutCpt props []

forestLayoutCpt :: R.Component ForestLayoutProps
forestLayoutCpt = R.hooksComponentWithModule thisModule "forestLayout" cpt
  where
    cpt props@{ handed } _ = do
      pure $ R.fragment [ topBar { handed }, forestLayoutMain props ]

forestLayoutMain :: Record ForestLayoutProps -> R.Element
forestLayoutMain props = R.createElement forestLayoutMainCpt props []

forestLayoutMainCpt :: R.Component ForestLayoutProps
forestLayoutMainCpt = R.hooksComponentWithModule thisModule "forestLayoutMain" cpt
  where
    cpt { asyncTasks, child, frontends, handed, reload, route, sessions, showLogin, backend} _ = do
      let ordering =
            case fst handed of
              LeftHanded  -> reverse
              RightHanded -> identity

      pure $ R2.row $ ordering [
        H.div { className: "col-md-2", style: { paddingTop: "60px" } }
            [ forest { asyncTasks, backend, frontends, handed: fst handed, reload, route, sessions, showLogin } ]
      , mainPage child
      ]


mainPage :: R.Element -> R.Element
mainPage child =
  H.div {className: "col-md-10"}
  [ H.div {id: "page-wrapper"}
    [ H.div {className: "container-fluid"} [ child ] ] ]
