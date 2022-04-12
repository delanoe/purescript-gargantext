module Gargantext.Components.PhyloExplorer.SideBar
  ( sideBar
  ) where

import Gargantext.Prelude

import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.PhyloExplorer.DetailsTab (detailsTab)
import Gargantext.Components.PhyloExplorer.SelectionTab (selectionTab)
import Gargantext.Components.PhyloExplorer.Types (ExtractedCount, ExtractedTerm, TabView(..))
import Gargantext.Types (NodeID)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

type Props =
  ( nodeId                :: NodeID

  , docCount              :: Int
  , foundationCount       :: Int
  , periodCount           :: Int
  , termCount             :: Int
  , groupCount            :: Int
  , branchCount           :: Int

  , selectedTerm          :: Maybe String
  , selectedBranch        :: Maybe String
  , selectedSource        :: Maybe String
  , extractedTerms        :: Array ExtractedTerm
  , extractedCount        :: Maybe ExtractedCount
  , selectTermCallback    :: String -> Effect Unit
  )

sideBar :: R2.Leaf Props
sideBar = R2.leaf component

componentName :: String
componentName = "Gargantext.Components.PhyloExplorer.SideBar"

component :: R.Component Props
component = R.hooksComponent componentName cpt where
  cpt props _ = do
    -- States
    tabView /\ tabViewBox <- R2.useBox' DetailsTab

    -- Computed
    let
      tabList = [ DetailsTab, SelectionTab ]

    -- Render
    pure $

      H.div
      { className: "phylo-sidebar" }
      [
        -- Menu
        B.tabs
        { value: tabView
        , list: tabList
        , callback: flip T.write_ tabViewBox
        }
      ,
        -- Content
        case tabView of

          DetailsTab ->
            detailsTab
            { key: (show props.nodeId) <> "-details"
            , docCount: props.docCount
            , foundationCount: props.foundationCount
            , periodCount: props.periodCount
            , termCount: props.termCount
            , groupCount: props.groupCount
            , branchCount: props.branchCount
            }

          SelectionTab ->
            selectionTab
            { key: (show props.nodeId) <> "-selection"
            , extractedTerms: props.extractedTerms
            , extractedCount: props.extractedCount
            , selectedTerm: props.selectedTerm
            , selectedBranch: props.selectedBranch
            , selectedSource: props.selectedSource
            , selectTermCallback: props.selectTermCallback
            }
      ]
