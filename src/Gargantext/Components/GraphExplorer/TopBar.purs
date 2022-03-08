module Gargantext.Components.GraphExplorer.TopBar where

import Data.Maybe (Maybe(..))
import Reactix as R
import Reactix.DOM.HTML as RH
import Toestand as T

import Gargantext.Prelude hiding (max,min)

import Gargantext.Components.App.Data (Boxes)
import Gargantext.Components.GraphExplorer.Search (nodeSearchControl)
import Gargantext.Components.GraphExplorer.Sidebar.Types as GEST
import Gargantext.Components.GraphExplorer.ToggleButton as Toggle
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.GraphExplorer.TopBar"

type TopBar =
  (
    boxes    :: Boxes
  )

topBar :: R2.Leaf TopBar
topBar = R2.leafComponent topBarCpt
topBarCpt :: R.Component TopBar
topBarCpt = here.component "topBar" cpt where
  cpt { boxes: { sidePanelGraph
               , sidePanelState } } _ = do
    { mGraph, multiSelectEnabled, selectedNodeIds, showControls } <- GEST.focusedSidePanel sidePanelGraph

    mGraph' <- T.useLive T.unequal mGraph

    let search = case mGraph' of
          Just graph -> nodeSearchControl { graph
                                         , multiSelectEnabled
                                         , selectedNodeIds } []
          Nothing -> RH.div {} []

    pure $ RH.form { className: "graph-topbar d-flex" }
      [ Toggle.controlsToggleButton { state: showControls } []
      , Toggle.sidebarToggleButton { state: sidePanelState } []
      , search
      ]
