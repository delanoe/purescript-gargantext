module Gargantext.Components.Forest
  ( forest
  , forestLayout
  , Props
  ) where

import Gargantext.Prelude

import Data.Array as A
import Data.Maybe (Maybe(..))
import Gargantext.Components.App.Data (Boxes)
import Gargantext.Components.Forest.Tree (treeLoader)
import Gargantext.Ends (Frontends)
import Gargantext.Sessions (Session(..), unSessions)
import Gargantext.Types (switchHanded)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Forest"

-- Shared by components here with Tree
type Props =
  ( boxes     :: Boxes
  , frontends :: Frontends
  )

forest :: R2.Component Props
forest = R.createElement forestCpt
forestCpt :: R.Component Props
forestCpt = here.component "forest" cpt where
  cpt { boxes: boxes@{ handed
                     , reloadForest
                     , sessions }
      , frontends } _ = do
    -- TODO Fix this. I think tasks shouldn't be a Box but only a Reductor
    -- tasks'        <- GAT.useTasks reloadRoot reloadForest
    -- R.useEffect' $ do
    --   T.write_ (Just tasks') tasks
    handed'       <- T.useLive T.unequal handed
    sessions'     <- T.useLive T.unequal sessions
    -- forestOpen'   <- T.useLive T.unequal forestOpen
    -- reloadRoot'   <- T.useLive T.unequal reloadRoot
    -- route'        <- T.useLive T.unequal route

    -- TODO If `reloadForest` is set, `reload` state should be updated
    -- TODO fix tasks ref
    pure $ H.div { className: "forest-layout-content" }
      (A.cons (plus { boxes }) (trees handed' sessions'))
    where
      trees handed' sessions' = (tree handed') <$> unSessions sessions'
      tree handed' s@(Session { treeId }) = 
        treeLoader { boxes
                   , frontends
                   , handed: handed'
                   , reload: reloadForest
                   , root: treeId
                   , session: s } []

type Plus = ( boxes :: Boxes )

plus :: R2.Leaf Plus
plus p = R.createElement plusCpt p []
plusCpt :: R.Component Plus
plusCpt = here.component "plus" cpt where
  cpt { boxes: { backend, handed, showLogin } } _ = do
    handed' <- T.useLive T.unequal handed

    pure $ H.div {}
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
        -- NOTE Reset backend in case G.C.N.Home.homeLayout set that to (Just b)
        -- from current url
        _ <- T.write Nothing backend
        T.write_ true showLogin
      title = "Add or remove connections to the server(s)."
      divClass = "fa fa-universal-access"
      buttonClass handed' =
        "btn btn-primary d-block " <> switchHanded "mr-1 ml-auto" "ml-1 mr-auto" handed'

forestLayout :: R2.Component Props
forestLayout = R.createElement forestLayoutCpt
forestLayoutCpt :: R.Component Props
forestLayoutCpt = here.component "forestLayout" cpt where
  cpt p _ = pure $

    H.div { className: "forest-layout-wrapper col-md-2" }
    [
      H.div { className: "forest-layout" }
      [
        forest p []
      ,
        H.div { className: "forest-layout-teaser" } []
      ]
    ]
