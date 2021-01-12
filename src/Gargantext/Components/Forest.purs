module Gargantext.Components.Forest where

import Data.Array as A
import Data.Tuple (fst)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log, log2)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.AsyncTasks as GAT
import Gargantext.Components.Forest.Tree (treeView)
import Gargantext.Components.TopBar (topBar)
import Gargantext.Ends (Frontends, Backend(..))
import Gargantext.Prelude
import Gargantext.Routes (AppRoute)
import Gargantext.Sessions (Session(..), Sessions, OpenNodes, unSessions)
import Gargantext.Types (Reload, ReloadS, Handed(..))
import Gargantext.Utils.Reactix as R2

thisModule :: String
thisModule = "Gargantext.Components.Forest"

type Props = (
    appReload     :: ReloadS
  , asyncTasksRef :: R.Ref (Maybe GAT.Reductor)
  , backend       :: R.State (Maybe Backend)
  , currentRoute  :: AppRoute
  , frontends     :: Frontends
  , handed        :: Handed
  , sessions      :: Sessions
  , showLogin     :: R.Setter Boolean
  , treeReloadRef :: R.Ref (Maybe ReloadS)
  )

forest :: R2.Component Props
forest = R.createElement forestCpt
  where
    forestCpt :: R.Component Props
    forestCpt = R.hooksComponentWithModule thisModule "forest" cpt

    cpt { appReload
        , asyncTasksRef
        , backend
        , currentRoute
        , frontends
        , handed
        , sessions
        , showLogin
        , treeReloadRef } _ = do
      -- NOTE: this is a hack to reload the tree view on demand
      reload     <- R.useState' (0 :: Reload)
      asyncTasks <- GAT.useTasks appReload reload
      openNodes  <- R2.useLocalStorageState R2.openNodesKey (Set.empty :: OpenNodes)

      -- TODO If `treeReloadRef` is set, `reload` state should be updated
      R.useEffect' $ do
        R.setRef asyncTasksRef $ Just asyncTasks
        case R.readRef treeReloadRef of
          Nothing -> R.setRef treeReloadRef $ Just reload
          Just _  -> pure unit

      R2.useCache (
          frontends
        /\ currentRoute
        /\ sessions
        /\ fst openNodes
        /\ fst appReload
        /\ fst reload
        /\ (fst asyncTasks).storage
        /\ handed
        )
        (cpt' openNodes asyncTasks appReload reload showLogin backend)
    cpt' openNodes asyncTasks appReload reload showLogin backend (frontends /\ currentRoute /\ sessions /\ _ /\ _ /\ _ /\ _ /\ handed) = do
      pure $ R2.row $ [plus handed showLogin backend] <> trees
      where
        trees = tree <$> unSessions sessions
        tree s@(Session {treeId}) =
          treeView { appReload
                  , asyncTasks
                  , currentRoute
                  , frontends
                  , handed
                  , openNodes
                  , reload
                  , root: treeId
                  , session: s
                  } []

plus :: Handed -> R.Setter Boolean -> R.State (Maybe Backend) -> R.Element
plus handed showLogin backend = H.div { className: handedClass } [
  H.button { title: "Add or remove connections to the server(s)."
           , on: {click}
           , className: "btn btn-default"
           }
          [ H.div { "type": ""
                  , className: "fa fa-universal-access fa-lg"
                  } [H.text " Log in/out "]
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
type ForestLayoutProps = (
    appReload     :: ReloadS
  , asyncTasksRef :: R.Ref (Maybe GAT.Reductor)
  , backend       :: R.State (Maybe Backend)
  , currentRoute  :: AppRoute
  , frontends     :: Frontends
  , handed        :: R.State Handed
  , sessions      :: Sessions
  , showLogin     :: R.Setter Boolean
  , treeReloadRef :: R.Ref (Maybe ReloadS)
  )

forestLayout :: R2.Component ForestLayoutProps
forestLayout props = R.createElement forestLayoutCpt props
  where
    forestLayoutCpt :: R.Component ForestLayoutProps
    forestLayoutCpt = R.hooksComponentWithModule thisModule "forestLayout" cpt

    cpt props@{ handed } children = do
      pure $ R.fragment [ topBar { handed } [], forestLayoutMain props children ]

-- a component, for which first child element is placed inside the top bar
-- while the remaining ones are put into the main view
forestLayoutWithTopBar :: R2.Component ForestLayoutProps
forestLayoutWithTopBar props = R.createElement forestLayoutWithTopBarCpt props
  where
    forestLayoutWithTopBarCpt :: R.Component ForestLayoutProps
    forestLayoutWithTopBarCpt = R.hooksComponentWithModule thisModule "forestLayoutWithTopBar" cpt

    cpt props@{ handed } children = do
      let { head: topBarChild, tail: mainChildren } =
            fromMaybe { head: H.div {} [], tail: [] } $ A.uncons children
      pure $ R.fragment [
          topBar { handed } [ topBarChild ]
        , forestLayoutMain props mainChildren
      ]

forestLayoutMain :: R2.Component ForestLayoutProps
forestLayoutMain props = R.createElement forestLayoutMainCpt props
  where
    forestLayoutMainCpt :: R.Component ForestLayoutProps
    forestLayoutMainCpt = R.hooksComponentWithModule thisModule "forestLayoutMain" cpt

    cpt props children = do
      pure $ forestLayoutRaw props [
          mainPage {} children
        ]

forestLayoutRaw :: R2.Component ForestLayoutProps
forestLayoutRaw props = R.createElement forestLayoutRawCpt props
  where
    forestLayoutRawCpt :: R.Component ForestLayoutProps
    forestLayoutRawCpt = R.hooksComponentWithModule thisModule "forestLayoutRaw" cpt

    cpt { appReload
        , asyncTasksRef
        , backend
        , currentRoute
        , frontends
        , handed
        , sessions
        , showLogin
        , treeReloadRef } children = do
      let ordering =
            case fst handed of
              LeftHanded  -> A.reverse
              RightHanded -> identity

      pure $ R2.row $ ordering ([
        H.div { className: "col-md-2", style: { paddingTop: "60px" } } [
          forest { appReload
                 , asyncTasksRef
                 , backend
                 , currentRoute
                 , frontends
                 , handed: fst handed
                 , sessions
                 , showLogin
                 , treeReloadRef } []
          ]
        ] <> children)

mainPage :: R2.Component ()
mainPage = R.createElement mainPageCpt
  where
    mainPageCpt :: R.Component ()
    mainPageCpt = R.hooksComponentWithModule thisModule "mainPage" cpt

    cpt {} children = do
      pure $ H.div {className: "col-md-10"} [
        H.div {id: "page-wrapper"} [
          H.div {className: "container-fluid"} children
          ]
        ]
