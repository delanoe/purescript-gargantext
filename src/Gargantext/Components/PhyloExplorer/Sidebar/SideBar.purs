module Gargantext.Components.PhyloExplorer.SideBar
  ( sideBar
  ) where

import Gargantext.Prelude

import Effect (Effect)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (Elevation(..))
import Gargantext.Components.PhyloExplorer.DetailsTab (detailsTab)
import Gargantext.Components.PhyloExplorer.SelectionTab (selectionTab)
import Gargantext.Components.PhyloExplorer.Store as PhyloStore
import Gargantext.Components.PhyloExplorer.Types (TabView(..))
import Gargantext.Types (SidePanelState(..))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

type Props =
  ( selectTermCallback    :: String -> Effect Unit
  )

sideBar :: R2.Leaf Props
sideBar = R2.leaf component

componentName :: String
componentName = "Gargantext.Components.PhyloExplorer.SideBar"

component :: R.Component Props
component = R.hooksComponent componentName cpt where
  cpt props _ = do
    -- | States
    -- |
    { sideBarTabView
    , phyloId
    , sideBarDisplayed
    } <- PhyloStore.use

    sideBarTabView' <- R2.useLive' sideBarTabView
    phyloId'        <- R2.useLive' phyloId

    -- | Computed
    -- |
    let
      tabList = [ DetailsTab, SelectionTab ]

    -- | Behaviors
    -- |
    let
      closeCallback :: Unit -> Effect Unit
      closeCallback _ = T.write_ Closed sideBarDisplayed

    -- | Render
    -- |
    pure $

      H.div
      { className: "phylo-sidebar" }
      [
        -- Close CTA
        B.iconButton
        { name: "times"
        , elevation: Level2
        , callback: closeCallback
        , className: "phylo-sidebar__close"
        }
      ,
        -- Menu
        B.tabs
        { value: sideBarTabView'
        , list: tabList
        , callback: flip T.write_ sideBarTabView
        }
      ,
        -- Content
        case sideBarTabView' of

          DetailsTab ->
            detailsTab
            { key: (show phyloId') <> "-details" }

          SelectionTab ->
            selectionTab
            { key: (show phyloId') <> "-selection"
            , selectTermCallback: props.selectTermCallback
            }
      ]
