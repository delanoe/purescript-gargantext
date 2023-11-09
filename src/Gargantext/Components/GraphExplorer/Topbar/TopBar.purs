module Gargantext.Components.GraphExplorer.TopBar (topBar) where

import Gargantext.Prelude hiding (max, min)

import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ButtonVariant(..), Variant(..))
import Gargantext.Components.GraphExplorer.Store as GraphStore
import Gargantext.Components.GraphExplorer.Topbar.Search (nodeSearchControl)
import Gargantext.Types as GT
import Gargantext.Utils (nbsp, (?))
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
      { className: "node-topbar graph-topbar bg-primary h-100" }
      [
        H.div
        { className: "node-topbar__title graph-topbar__title align-self-center px-1 text-light" }
        [
          B.icon { name: "hubzilla" }
        ,
          H.text $ nbsp 1
        ,
          H.text "Graph"
        ]
      ,
        -- Toolbar toggle
        B.button
        { className: "graph-topbar__toolbar rounded-circle-2 py-0 px-2 mr-1"
        , callback: \_ -> T.modify_ (not) showControls
        , variant: showControls' ?
            ButtonVariant Light $
            OutlinedButtonVariant Light
        , title: showControls' ?
            "Hide toolbar" $
            "Show toolbar"
        }
        [
          B.icon { name: "sliders" }
        ,
          H.text $ nbsp 1
        ,
          H.text "Toolbar"
        -- ,
        --   H.text $ showControls' ? "Hide toolbar" $ "Show toolbar"
        ]
      ,
        -- Sidebar toggle
        B.button
        { className: "graph-topbar__sidebar rounded-circle-2 py-0 px-2 mr-1"
        , callback: \_ -> T.modify_ GT.toggleSidePanelState showSidebar
        , variant: showSidebar' == GT.Opened ?
            ButtonVariant Light $
            OutlinedButtonVariant Light
        , title: showSidebar' == GT.Opened ?
            "Hide sidebar" $
            "Show sidebar"
        }
        [
          B.icon { name: "columns" }
        ,
          H.text $ nbsp 1
        ,
          H.text "Sidebar"
        -- ,
        --   H.text $ showSidebar' == GT.Opened ?
        --     "Hide sidebar" $
        --     "Show sidebar"
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
