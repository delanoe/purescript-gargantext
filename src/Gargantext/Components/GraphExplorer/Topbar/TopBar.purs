module Gargantext.Components.GraphExplorer.TopBar (topBar) where

import Gargantext.Prelude hiding (max, min)

import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ButtonVariant(..), Variant(..))
import Gargantext.Components.GraphExplorer.Store as GraphStore
import Gargantext.Components.GraphExplorer.Topbar.Search (nodeSearchControl)
import Gargantext.Types as GT
import Gargantext.Utils ((?))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.GraphExplorer.TopBar"

topBar :: R2.Leaf ()
topBar = R2.leaf component

component :: R.Component ()
component = here.component "topBar" cpt where
  cpt _ _ = do
    -- States
    { graph
    , multiSelectEnabled
    , selectedNodeIds
    , showControls
    , showSidebar
    } <- GraphStore.use

    graph'              <- R2.useLive' graph
    showControls'       <- R2.useLive' showControls
    showSidebar'        <- R2.useLive' showSidebar

    -- Render
    pure $

      H.div
      { className: "graph-topbar" }
      [
        -- Toolbar toggle
        B.button
        { className: "graph-topbar__toolbar"
        , callback: \_ -> T.modify_ (not) showControls
        , variant: showControls' ?
            ButtonVariant Light $
            OutlinedButtonVariant Light
        }
        [
          H.text $ showControls' ? "Hide toolbar" $ "Show toolbar"
        ]
      ,
        -- Sidebar toggle
        B.button
        { className: "graph-topbar__sidebar"
        , callback: \_ -> T.modify_ GT.toggleSidePanelState showSidebar

        , variant: showSidebar' == GT.Opened ?
            ButtonVariant Light $
            OutlinedButtonVariant Light
        }
        [
          H.text $ showSidebar' == GT.Opened ?
            "Hide sidebar" $
            "Show sidebar"
        ]
      ,
        -- Search
        nodeSearchControl
        { graph: graph'
        , multiSelectEnabled
        , selectedNodeIds
        , className: "graph-topbar__search"
        }
      ]
