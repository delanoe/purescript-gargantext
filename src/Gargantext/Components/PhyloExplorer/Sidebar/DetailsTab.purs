module Gargantext.Components.PhyloExplorer.DetailsTab
  ( detailsTab
  ) where

import Gargantext.Prelude

import Gargantext.Components.PhyloExplorer.JSON (Cluster(..), TimeUnit(..))
import Gargantext.Components.PhyloExplorer.Store as PhyloStore
import Gargantext.Components.PhyloExplorer.Types (PhyloConfig(..), PhyloData(..))
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

    (PhyloData o) <- R2.useLive' store.phyloData
    (PhyloConfig c) <- R2.useLive' store.phyloConfig

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
      ,
        H.hr 
        { className: "phylo-details-tab__delimiter" }
      , 
        -- Phylo params
        H.ul
        { className: "phylo-details-tab__counter" }
        [ detailsTimeUnit c.timeUnit
        , detailsParams c.levelOfObservation "Level of observation"
        , detailsClique c.clique 
        , detailsParams c.proximity "Proximity"
        , detailsParams c.synchrony "Synchrony"
        , detailsParams c.minBranch "Minimum Branch Size"
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

detailsParams :: forall a. (Show a) => a -> String -> R.Element
detailsParams value label = 
  H.li {} [ H.text $ label <> ": " <> show value ]

detailsClique :: Cluster -> R.Element
detailsClique c =
  H.li {} [ H.text $ "Cluster algo: " <> parseClique c]
  where
    parseClique (Fis { _fis_support, _fis_size }) = "FIS. Support: " <> show _fis_support <> ". Size: " <> show _fis_size
    parseClique (MaxClique { _mcl_size, _mcl_threshold, _mcl_filter}) =
      "MaxClique. Size: " <> show _mcl_size <> ". Threshhold: " <> show _mcl_threshold <> ". Filter: " <> _mcl_filter

detailsTimeUnit :: TimeUnit -> R.Element
detailsTimeUnit t =
  H.li {} [ H.text $ "Time unit: " <> parseTimeUnit t]
  where
    parseTimeUnit (Epoch {_epoch_period, _epoch_step, _epoch_matchingFrame}) = 
      "Epoch. Period: " <> show _epoch_period <> ". Step: " <> show _epoch_step <>". Matching frame: " <> show _epoch_matchingFrame
    parseTimeUnit (Year {_year_period, _year_step, _year_matchingFrame}) = 
      "Year. Period: " <> show _year_period <> ". Step: " <> show _year_step <>". Matching frame: " <> show _year_matchingFrame
    parseTimeUnit (Month {_month_period, _month_step, _month_matchingFrame}) = 
      "Month. Period: " <> show _month_period <> ". Step: " <> show _month_step <>". Matching frame: " <> show _month_matchingFrame
    parseTimeUnit (Week {_week_period, _week_step, _week_matchingFrame}) = 
      "Week. Period: " <> show _week_period <> ". Step: " <> show _week_step <>". Matching frame: " <> show _week_matchingFrame
    parseTimeUnit (Day {_day_period, _day_step, _day_matchingFrame}) = 
      "Day. Period: " <> show _day_period <> ". Step: " <> show _day_step <>". Matching frame: " <> show _day_matchingFrame
