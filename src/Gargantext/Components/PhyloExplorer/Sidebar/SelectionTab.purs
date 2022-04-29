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
import Gargantext.Components.PhyloExplorer.Store as PhyloStore
import Gargantext.Components.PhyloExplorer.Types (ExtractedCount(..), ExtractedTerm(..))
import Gargantext.Utils (nbsp, (?))
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

componentName :: String
componentName = "Gargantext.Components.PhyloExplorer.SideBar.SelectionTab"

component :: R.Component Props
component = R.hooksComponent componentName cpt where
  cpt { selectTermCallback
      } _ = do
    -- | State
    -- |
    store <- PhyloStore.use

    extractedTerms <- R2.useLive' store.extractedTerms
    extractedCount <- R2.useLive' store.extractedCount
    selectedTerm   <- R2.useLive' store.selectedTerm
    selectedBranch <- R2.useLive' store.selectedBranch
    selectedSource <- R2.useLive' store.selectedSource

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

    -- | Render
    -- |
    pure $

      H.div
      { className: "phylo-selection-tab" }
      [
        -- No result
        R2.if' (not haveSelection) $

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
                  ]
                ,
                  H.li
                  { className: "list-group-item" }
                  [
                    H.a
                    { href: "https://en.wikipedia.org/w/index.php?search=\""
                        <> s
                        <> "\""
                    , target: "_blank"
                    }
                    [
                      H.text "Click here for more info"
                    ]
                  ]
                ]
              ]
            ]
      ,
        -- (separator)
        R2.if' (haveSelection) $

          H.div
          { className: "phylo-selection-tab__separator" }
          [
            B.icon
            { name: "angle-down" }
          ]
      ,
        -- No extracted result
        R2.if' (haveSelection && null extractedTerms) $

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
        R2.if' (not null extractedTerms) $

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
                  ]
            ,
              -- Term word cloud
              H.li
              { className: "list-group-item" }
              [
                H.ul
                {} $
                flip mapWithIndex extractedTerms
                  \index (ExtractedTerm { label, ratio }) ->

                    R2.if'
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
                R2.if' (truncateResults) $

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
      -- ,
        -- (separator)
        -- R2.if' (not null extractedTerms) $

        --   H.div
        --   { className: "phylo-selection-tab__separator" }
        --   [
        --     B.icon
        --     { name: "angle-down" }
        --   ]
      -- ,
        -- Extracted Docs
        -- R2.if' (not null extractedTerms) $
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
