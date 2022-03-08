module Gargantext.Components.PhyloExplorer.SideBar
  ( sideBar
  ) where

import Gargantext.Prelude

import Data.Foldable (intercalate)
import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Gargantext.Components.PhyloExplorer.DetailsTab (detailsTab)
import Gargantext.Components.PhyloExplorer.SelectionTab (selectionTab)
import Gargantext.Components.PhyloExplorer.Types (ExtractedCount, ExtractedTerm, TabView(..))
import Gargantext.Types (NodeID)
import Gargantext.Utils ((?))
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

    -- Render
    pure $

      H.div
      { className: "phylo-sidebar" }
      [
        -- Teasers
        H.div
        { className: "phylo-sidebar__top-teaser" }
        []
      ,
        -- Menu
        H.ul
        { className: intercalate " "
            [ "nav nav-tabs"
            , "phylo-sidebar__menu"
            ]
        }
        [
          H.li
          { className: "nav-item"
          , on: { click: \_ -> T.write_ DetailsTab tabViewBox }
          }
          [
            H.a
            { className: intercalate " "
                [ "nav-link"
                , tabView == DetailsTab ? "active" $ ""
                ]
            }
            [
              H.text "Details"
            ]
          ]
        ,
          H.li
          { className: "nav-item"
          , on: { click: \_ -> T.write_ SelectionTab tabViewBox }
          }
          [
            H.a
            { className: intercalate " "
                [ "nav-link"
                , tabView == SelectionTab ? "active" $ ""
                ]
            }
            [
              H.text "Selection"
            ]
          ]
        ]
      ,
        -- Details tab
        R2.if' (tabView == DetailsTab) $
          detailsTab
          { key: (show props.nodeId) <> "-details"
          , docCount: props.docCount
          , foundationCount: props.foundationCount
          , periodCount: props.periodCount
          , termCount: props.termCount
          , groupCount: props.groupCount
          , branchCount: props.branchCount
          }
      ,
        -- Selection tab
        R2.if' (tabView == SelectionTab) $
          selectionTab
          { key: (show props.nodeId) <> "-selection"
          , extractedTerms: props.extractedTerms
          , extractedCount: props.extractedCount
          , selectedTerm: props.selectedTerm
          , selectedBranch: props.selectedBranch
          , selectedSource: props.selectedSource
          , selectTermCallback: props.selectTermCallback
          }
      ,
        -- Teaser
        H.div
        { className: "phylo-sidebar__bottom-teaser" }
        []
      ]
