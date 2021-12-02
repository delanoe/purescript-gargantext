module Gargantext.Components.PhyloExplorer.DetailsTab
  ( detailsTab
  ) where

import Gargantext.Prelude

import Gargantext.Utils (nbsp)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H

type Props =
  ( key             :: String

  , docCount        :: Int
  , foundationCount :: Int
  , periodCount     :: Int
  , termCount       :: Int
  , groupCount      :: Int
  , branchCount     :: Int
  )

detailsTab :: R2.Leaf Props
detailsTab = R2.leaf detailsTabCpt

componentName :: String
componentName = "Gargantext.Components.PhyloExplorer.SideBar.DetailsTab"

detailsTabCpt :: R.Component Props
detailsTabCpt = R.hooksComponent componentName cpt where
  cpt props _ =

    -- Render
    pure $

      H.div
      { className: "phylo-details-tab" }
      [
        -- Counters
        H.ul
        { className: "phylo-details-tab__counter" }
        [
          detailsCount props.docCount "docs"
        ,
          detailsCount props.foundationCount "foundations"
        ,
          detailsCount props.periodCount "periods"
        ]
      ,
        H.ul
        { className: "phylo-details-tab__counter" }
        [
          detailsCount props.termCount "terms"
        ,
          detailsCount props.groupCount "groups"
        ,
          detailsCount props.branchCount "branches"
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
