module Gargantext.Components.PhyloExplorer.DetailsTab
  ( detailsTab
  ) where

import Gargantext.Prelude

import Gargantext.Components.PhyloExplorer.Store as PhyloStore
import Gargantext.Components.PhyloExplorer.Types (PhyloDataSet(..))
import Gargantext.Utils (nbsp)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H

here :: R2.Here
here = R2.here "Gargantext.Components.PhyloExplorer.SideBar.DetailsTab"

detailsTab :: R2.Leaf ( key :: String )
detailsTab = R2.leaf detailsTabCpt


detailsTabCpt :: R.Component ( key :: String )
detailsTabCpt = here.component "" cpt where
  cpt _ _ = do
    -- | States
    -- |
    store <- PhyloStore.use

    (PhyloDataSet o) <- R2.useLive' store.phyloDataSet

    -- | Render
    -- |
    pure $

      H.div
      { className: "phylo-details-tab" }
      [
        -- Counters
        H.ul
        { className: "phylo-details-tab__counter" }
        [
          detailsCount o.nbDocs "docs"
        ,
          detailsCount o.nbFoundations "foundations"
        ,
          detailsCount o.nbPeriods "periods"
        ]
      ,
        H.ul
        { className: "phylo-details-tab__counter" }
        [
          detailsCount o.nbTerms "terms"
        ,
          detailsCount o.nbGroups "groups"
        ,
          detailsCount o.nbBranches "branches"
        ]
      ,
        H.hr
        { className: "phylo-details-tab__delimiter" }
      ,
        -- Link description
        H.a
        { className: "phylo-details-tab__link"
        , href: "http://maps.gargantext.org/unpublished_maps_phylo/vaccines_countries/documentation.html"
        , target: "_blank" }
        [
          H.text "How the phylomemy was built?"
        ]
      ]

detailsCount :: Int -> String -> R.Element
detailsCount value label =
  H.li
  { className: "phylo-details-tab__counter__item" }
  [
    H.span
    { className: "phylo-details-tab__counter__value" }
    [
      H.text $ show value
    ]
  ,
    H.span
    { className: "phylo-details-tab__counter__label "}
    [
      H.text $ nbsp 1 <> label
    ]
  ]
