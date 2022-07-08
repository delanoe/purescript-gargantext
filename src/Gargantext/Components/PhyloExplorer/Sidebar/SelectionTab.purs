module Gargantext.Components.PhyloExplorer.SelectionTab
  ( selectionTab
  ) where

import Gargantext.Prelude

import Data.Array (length, mapWithIndex, null)
import Data.Foldable (intercalate)
import Data.Int (ceil)
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ButtonVariant(..), Variant(..))
import Gargantext.Components.PhyloExplorer.Sidebar.DocList (docListWrapper)
import Gargantext.Components.PhyloExplorer.Sidebar.UpdateTerms (updateTerms)
import Gargantext.Components.PhyloExplorer.Store as PhyloStore
import Gargantext.Components.PhyloExplorer.Types (ExtractedCount(..), ExtractedTerm(..), defaultCacheParams)
import Gargantext.Hooks.FirstEffect (useFirstEffect')
import Gargantext.Types (CTabNgramType(..))
import Gargantext.Utils (nbsp, setter, (?))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

type Props =
  ( key                 :: String
  , selectTermCallback  :: String -> Effect Unit
  )

selectionTab :: R2.Leaf Props
selectionTab = R2.leaf component

here :: R2.Here
here = R2.here "Gargantext.Components.PhyloExplorer.SideBar.SelectionTab"

component :: R.Component Props
component = here.component "main" cpt where
  cpt { selectTermCallback
      } _ = do
    -- | State
    -- |
    store <- PhyloStore.use

    extractedTerms      <- R2.useLive' store.extractedTerms
    extractedCount      <- R2.useLive' store.extractedCount
    selectedTerm        <- R2.useLive' store.selectedTerm
    selectedBranch      <- R2.useLive' store.selectedBranch
    selectedSource      <- R2.useLive' store.selectedSource
    expandNeighborhood  <- R2.useLive' store.expandNeighborhood
    expandSelection     <- R2.useLive' store.expandSelection

    showMore' /\ showMore <- R2.useBox' false

    let
      haveSelection
         = isJust selectedTerm
        || isJust selectedBranch
        || isJust selectedSource

      termCount = length extractedTerms

      maxTruncateResult = 5

      truncateResults
         = termCount > maxTruncateResult
        && not showMore'

    -- | Effects
    -- |

    -- reset "show more" button to hidding mode on selected terms change
    R.useEffect1' extractedTerms $
      T.write_ false showMore

    -- transfer local Component change to Local Storage cache
    useFirstEffect' $
      flip T.listen store.expandNeighborhood onExpandNeighborhoodChange


    -- | Behaviors
    -- |
    let
      onExpandNeighborhoodClick _ = T.modify_ (not) store.expandNeighborhood
      onExpandSelectionClick _ = T.modify_ (not) store.expandSelection

    -- | Render
    -- |
    pure $

      H.div
      { className: "phylo-selection-tab" }
      [
        -- No result
        R2.when (not haveSelection) $

          B.caveat
          { className: "phylo-selection-tab__nil" }
          [
            H.text "Select term, branch or source to get their informations"
          ]
      ,
        -- Selected source
        case selectedSource of
          Nothing -> mempty
          Just s  -> R.fragment
            [
              H.div
              { className: "phylo-selection-tab__highlight" }
              [
                H.ul
                { className: "list-group" }
                [
                  H.li
                  { className: "list-group-item" }
                  [
                    H.span
                    { className: intercalate " "
                        [ "phylo-selection-tab__highlight__badge"
                        , "badge badge-info"
                        ]
                    }
                    [
                      H.text s
                    ]
                  ,
                    H.span
                    { className: "phylo-selection-tab__highlight__type" }
                    [
                      H.text "source"
                    ]
                  ]
                ]
              ]
            ]
      ,
        -- Selected branch
        case selectedBranch of
          Nothing -> mempty
          Just s  -> R.fragment
            [
              H.div
              { className: "phylo-selection-tab__highlight" }
              [
                H.ul
                { className: "list-group" }
                [
                  H.li
                  { className: "list-group-item" }
                  [
                    H.span
                    { className: intercalate " "
                        [ "phylo-selection-tab__highlight__badge"
                        , "badge badge-info"
                        ]
                    }
                    [
                      H.text s
                    ]
                  ,
                    H.span
                    { className: "phylo-selection-tab__highlight__type" }
                    [
                      H.text "branch"
                    ]
                  ]
                ]
              ]
            ]
      ,
        -- Selected term
        case selectedTerm of
          Nothing -> mempty
          Just s  -> R.fragment
            [
              H.div
              { className: "phylo-selection-tab__highlight" }
              [
                H.ul
                { className: "list-group" }
                [
                  H.li
                  { className: "list-group-item" }
                  [
                    H.span
                    { className: intercalate " "
                        [ "phylo-selection-tab__highlight__badge"
                        , "badge badge-info"
                        ]
                    }
                    [
                      H.text s
                    ]
                  ,
                    H.span
                    { className: "phylo-selection-tab__highlight__type" }
                    [
                      H.text "term"
                    ]
                  ,
                    -- Expand Selection actions
                    B.iconButton
                    { name: expandSelection ?
                        "caret-up" $
                        "caret-down"
                    , className: "phylo-selection-tab__highlight__expand"
                    , callback: onExpandSelectionClick
                    }
                  ]
                ,
                  -- Selection actions
                  R2.when expandSelection $

                    H.li
                    { className: "list-group-item" }
                    [
                      -- Wikipedia informations
                      H.a
                      { href: "https://en.wikipedia.org/w/index.php?search=\""
                          <> s
                          <> "\""
                      , target: "_blank"
                      }
                      [
                        H.text "Click here for more info"
                      ]
                    ,
                      -- NGrams edition
                      H.div
                      { className: "phylo-selection-tab__highlight__actions" }
                      [
                        updateTerms
                        { selectedTerm: s
                        , ngramType: CTabTerms
                        }
                      ]
                    ]
                ]
              ]
            ]
      ,
        -- (separator)
        R2.when (haveSelection) $

          H.div
          { className: "phylo-selection-tab__separator" }
          [
            B.icon
            { name: "angle-double-down" }
          ]
      ,
        -- No extracted result
        R2.when (haveSelection && null extractedTerms) $

          H.div
          { className: "phylo-selection-tab__selection" }
          [
            B.caveat
            {}
            [
              H.text "No result found for your selection"
            ]
          ]
      ,
        -- Extracted Results
        R2.when (not null extractedTerms) $

          H.div
          { className: "phylo-selection-tab__selection" }
          [
            H.ul
            { className: "list-group" }
            [
              -- Extracted count
              case extractedCount of
                Nothing                     -> mempty
                Just (ExtractedCount count) ->

                  H.li
                  { className: "list-group-item" }
                  [
                    H.ul
                    { className: "phylo-selection-tab__counter" }
                    [
                      detailsCount count.termCount "terms" true
                    ,
                      detailsCount count.groupCount "groups" false
                    ,
                      detailsCount count.branchCount "branches" false
                    ]
                  ,
                    -- Expand word cloud
                    B.iconButton
                    { name: expandNeighborhood ?
                        "caret-up" $
                        "caret-down"
                    , className: "phylo-selection-tab__counter__expand"
                    , callback: onExpandNeighborhoodClick
                    }
                  ]
            ,
              -- Term word cloud
              R2.when expandNeighborhood $

                H.li
                { className: "list-group-item" }
                [
                  H.ul
                  {} $
                  flip mapWithIndex extractedTerms
                    \index (ExtractedTerm { label, ratio }) ->

                      R2.when
                      (
                        truncateResults == false
                      || index < maxTruncateResult
                      ) $
                        H.li
                        { className: "phylo-selection-tab__selection__item"}
                        [
                          H.a
                          { className: "badge badge-light"
                          -- adjust font size according to term frequency
                          , style:
                              { fontSize: termFontSize ratio
                              , lineHeight: termFontSize ratio
                              }
                          , on:
                            { click: \_ -> selectTermCallback label
                            }
                          }
                          [
                            H.text label
                          ]
                        ]
                ,
                  R2.when (truncateResults) $

                    B.button
                    { variant: ButtonVariant Light
                    , callback: \_ -> T.modify_ not showMore
                    , block: true
                    , className: "phylo-selection-tab__selection__show-more"
                    }
                    [
                      H.text "Show more"
                    ]
                ]
            ]
          ]
      ,
        -- (separator)
        R2.when (not null extractedTerms) $

          H.div
          { className: "phylo-selection-tab__separator" }
          [
            B.icon
            { name: "angle-double-down" }
          ]
      ,
        -- Extracted Docs
        R2.when (not null extractedTerms) $

          H.div
          { className: "phylo-selection-tab__extracted-docs" }
          [
            docListWrapper
            {}
          ]
      ]

termFontSize :: Number -> String
termFontSize
    = (_ * 10.0)
  >>> (_ + 14.0)
  >>> ceil
  >>> show
  >>> (_ <> "px")

detailsCount :: Int -> String -> Boolean -> R.Element
detailsCount value label weighty =
  H.li
  { className: "phylo-selection-tab__counter__item" }
  [
    H.span
    { className: intercalate " "
        [ "phylo-selection-tab__counter__value"
        , weighty ? "text-bold" $ ""
        ]
    }
    [
      H.text $ show value
    ]
  ,
    H.span
    { className: intercalate " "
        [ "phylo-selection-tab__counter__label"
        , weighty ? "text-bold" $ ""
        ]
    }
    [
      H.text $ nbsp 1 <> label
    ]
  ]

onExpandNeighborhoodChange :: T.Change Boolean -> Effect Unit
onExpandNeighborhoodChange { new } = do
  cache <- R2.loadLocalStorageState' R2.phyloParamsKey defaultCacheParams
  let update = setter (_ { expandNeighborhood = new }) cache
  R2.setLocalStorageState R2.phyloParamsKey update
