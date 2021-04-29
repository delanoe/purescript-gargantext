module Gargantext.Components.Forest
  ( forest
  -- , forestLayout
  -- , forestLayoutWithTopBar
  -- , forestLayoutMain
  -- , forestLayoutRaw
  , forestLayout
  , Common
  , Props
  ) where

import Gargantext.Prelude

import Data.Array as A
import Data.Maybe (Maybe, fromMaybe)
import Gargantext.AsyncTasks as GAT
import Gargantext.Components.Forest.Tree (doSearch, treeLoader)
import Gargantext.Components.TopBar (topBar)
import Gargantext.Ends (Frontends, Backend)
import Gargantext.Routes (AppRoute)
import Gargantext.Sessions (Session(..), Sessions, OpenNodes, unSessions)
import Gargantext.Types (Handed, reverseHanded, switchHanded)
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Reactix.DOM.HTML as H
import Record.Extra as RX
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Forest"

-- Shared by components here with Tree
type Common = 
  ( frontends      :: Frontends
  , handed         :: T.Box Handed
  , reloadMainPage :: T2.ReloadS
  , reloadRoot     :: T2.ReloadS
  , route          :: T.Box AppRoute
  )

type Props =
  ( backend            :: T.Box (Maybe Backend)
  , forestOpen         :: T.Box OpenNodes
  , reloadForest       :: T2.ReloadS
  , sessions           :: T.Box Sessions
  , showLogin          :: T.Box Boolean
  , showTree           :: T.Box Boolean
  , tasks              :: T.Box GAT.Storage
  | Common 
  )

type TreeExtra = (
    forestOpen :: T.Box OpenNodes
  )

forest :: R2.Component Props
forest = R.createElement forestCpt
forestCpt :: R.Component Props
forestCpt = here.component "forest" cpt where
  cpt props@{ backend
            , forestOpen
            , frontends
            , handed
            , reloadForest
            , reloadMainPage
            , reloadRoot
            , route
            , sessions
            , showLogin
            , showTree
            , tasks } _ = do
    -- TODO Fix this. I think tasks shouldn't be a Box but only a Reductor
    -- tasks'        <- GAT.useTasks reloadRoot reloadForest
    -- R.useEffect' $ do
    --   T.write_ (Just tasks') tasks
    handed'       <- T.useLive T.unequal handed
    sessions'     <- T.useLive T.unequal sessions
    -- forestOpen'   <- T.useLive T.unequal forestOpen
    -- reloadRoot'   <- T.useLive T.unequal reloadRoot
    -- route'        <- T.useLive T.unequal route

    showTree' <- T.useLive T.unequal showTree

    -- TODO If `reloadForest` is set, `reload` state should be updated
    -- TODO fix tasks ref
    pure $ H.div { className: "forest " <> if showTree' then "" else "d-none" }
      (A.cons (plus { handed, showLogin }) (trees handed' sessions'))
    where
      common = RX.pick props :: Record Common
      trees handed' sessions' = (tree handed') <$> unSessions sessions'
      tree handed' s@(Session {treeId}) =
        treeLoader { forestOpen
                   , frontends
                   , handed: handed'
                   , reload: reloadForest
                   , reloadMainPage
                   , reloadRoot
                   , root: treeId
                   , route
                   , session: s
                   , tasks } []

type Plus =
  ( handed    :: T.Box Handed
  , showLogin :: T.Box Boolean )

plus :: R2.Leaf Plus
plus p = R.createElement plusCpt p []
plusCpt :: R.Component Plus
plusCpt = here.component "plus" cpt where
  cpt { handed, showLogin } _ = do
    handed' <- T.useLive T.unequal handed

    pure $ H.div { className: "row" }
      [ H.button { className: buttonClass handed'
                , on: { click }
                , title }
          [ H.div { className: divClass } [ H.text " Log in/out " ] -- fa-lg
          , H.div {} [ H.text "    " ] ]
      ]
  --, H.div { "type": "", className: "fa fa-plus-circle fa-lg"} []
  --, H.div { "type": "", className: "fa fa-minus-circle fa-lg"} []
  -- TODO same as the one in the Login Modal (same CSS)
  -- [ H.i { className: "material-icons md-36"} [] ]
    where
      click _ = do
        -- _ <- T.write Nothing backend
        T.write_ true showLogin
      title = "Add or remove connections to the server(s)."
      divClass = "fa fa-universal-access"
      buttonClass handed' =
        "btn btn-primary col-5 " <> switchHanded "mr-1 ml-auto" "ml-1 mr-auto" handed'

forestLayout :: R2.Component Props
forestLayout = R.createElement forestLayoutCpt
forestLayoutCpt :: R.Component Props
forestLayoutCpt = here.component "forestLayout" cpt where
  cpt p _ = do
    pure $ H.div { className: "forest-layout" }
      [ forest p [] ]
