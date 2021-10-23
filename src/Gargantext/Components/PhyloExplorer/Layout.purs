module Gargantext.Components.PhyloExplorer.Layout
  ( layout
  ) where

import Gargantext.Prelude

import DOM.Simple.Console (log2)
import Data.Array as Array
import Data.Int (fromString)
import Data.Maybe (maybe)
import Data.String as String
import Gargantext.Components.PhyloExplorer.Types (PhyloDataset(..))
import Gargantext.Utils (nbsp)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.PhyloExplorer"

type Props =
  ( phyloDataset :: PhyloDataset
  )

layout :: R2.Component Props
layout = R.createElement layoutCpt
layoutCpt :: R.Component Props
layoutCpt = here.component "layout" cpt where
  cpt { phyloDataset: (PhyloDataset phyloDataset)
      } _ = do
    -- States
    let
      { phyloDocs
      , phyloBranches
      , phyloGroups
      , phyloTerms
      , phyloPeriods
      , phyloFoundations
      , phyloSources
      } = phyloDataset

      nbDocs        = parseInt phyloDocs
      nbBranches    = parseInt phyloBranches
      nbGroups      = parseInt phyloGroups
      nbTerms       = parseInt phyloTerms
      nbPeriods     = parseInt phyloPeriods
      nbFoundations = parseInt phyloFoundations

    sourcesBox <- T.useBox (mempty :: Array String)
    sources    <- T.useLive T.unequal sourcesBox

    -- Hooks
    R.useEffectOnce' $ do
      sources' <- pure $ stringArrToArr phyloSources
      T.write_ sources' sourcesBox

    -- @hightlightSource
    let
      highlightSource = \_ -> unit

    -- Render
    pure $
      H.div
      { className: "phylo" }
      [


      -- <!-- row 1 -->
        H.div
        { className: "phylo-title font-bold" }
        [ H.text "Mèmiescape" ]
      ,
        H.div
        { className: "phylo-folder" }
        [
        -- <!-- title bar (static mode) -->
          H.label
          { id: "phyloName"
          , className: "phylo-name"
          }
          []
        ,
        -- <!-- folder bar -->
        --   H.label
        --   { id: "file-label"
        --   , for: "file-path"
        --   , className: "input-file"
        --   }
        --   [ H.text "load a phylomemy →" ]
        -- ,
        --   H.input
        --   { id: "file-path"
        --   , type: "file"
        --   , maxLength: "10"
        --   }
        -- ,
        --   H.label
        --   { id: "file-name"
        --   , className: "input-name"
        --   }
        --   []
        -- ,
        --   H.button
        --   { id: "draw"
        --   , className: "button draw"
        --   }
        --   [ H.text "draw" ]
        -- ,
        -- <!-- source selector -->
          R2.select
          { id: "checkSource"
          , className: "select-source"
          , defaultValue: ""
          , on: { change: \_ -> unit }
          } $
          [
            H.option
            { disabled: true
            , value: ""
            }
            [ H.text "select a source ↴" ]
          ,
            H.option
            { value: "unselect" }
            [ H.text "unselect source ✕" ]
          ]
          <>
            flip Array.mapWithIndex sources
            ( \idx val ->
                H.option
                { value: idx }
                [ H.text val ]
            )

        ,
        -- <!-- search bar -->
          H.label
          { id: "search-label"
          , className: "search-label"
          }
          [ H.text "find a term →" ]
        ,
          H.input
          { id: "search-box"
          , type: "text"
          , className: "search"
          }
        ,
          H.input
          { id: "search-autocomplete"
          , text: "text"
          , className: "autocomplete"
          , disabled: true
          , value: ""
          }
        ]
      ,


      -- <!-- row 2 & 3 -->
        phyloCorpus {} []
      ,
        phyloCorpusInfo
        { nbDocs, nbFoundations, nbPeriods }
        []
      ,
        -- H.div
        -- { id: "phyloHow"
        -- , className: "phylo-how"
        -- }
        -- []
      -- ,
        phyloPhylo {} []
      ,


        phyloPhyloInfo
        { nbTerms, nbGroups, nbBranches }
        []
      ,


        H.div
        { id: "phyloIsoLine"
        , className: "phylo-isoline-info"
        }
        [
          H.div
          { className: "btn-group" }
          [
            H.button
            { id: "reset"
            , className: "button reset"
            }
            [
              H.i
              { className: "fas fa-expand-arrows-alt" }
              []
            ]
          ,
            H.button
            { id: "label"
            , className: "button label"
            }
            [
              H.i
              { className: "fas fa-dot-circle" }
              []
            ]
          ,
            H.button
            { id: "heading"
            , className: "button heading"
            }
            [
              H.i
              { className: "fas fa-sort-alpha-down" }
              []
            ]
          ,
            H.button
            { id: "export"
            , className: "button export"
            }
            [
              H.i
              { className: "fas fa-camera" }
              []
            ]
          ]
        ]
      ,

      -- <!-- row 4 -->
        H.div
        { id: "phyloScape"
        , className: "phylo-scape"
        }
        []
      ,
        H.div
        { id: "phyloTimeline"
        , className: "phylo-timeline"
        }
        []
      ,
        H.div
        { id: "phyloGraph"
        , className: "phylo-graph"
        }
        []
      ,

      -- <!-- row 5 -->
        H.div
        { className: "phylo-footer font-bold font-small"
        }
        [ H.text "iscpif // cnrs // 2021" ]


      ]

parseInt :: String -> Int
parseInt s = maybe 0 identity $ fromString s

stringArrToArr :: String -> Array String
stringArrToArr
  =   String.replace (String.Pattern "[") (String.Replacement "")
  >>> String.replace (String.Pattern "]") (String.Replacement "")
  >>> String.split (String.Pattern ",")
  >>> Array.filter (\s -> not eq 0 $ String.length s)

--------------------------------------------------------

type PhyloCorpusProps = ()

phyloCorpus :: R2.Component PhyloCorpusProps
phyloCorpus = R.createElement phyloCorpusCpt
phyloCorpusCpt :: R.Component PhyloCorpusProps
phyloCorpusCpt = here.component "phyloCorpus" cpt where
  cpt _ _ = do
    -- Render
    pure $

      H.div
      { id: "phyloCorpus"
      , className: "phylo-corpus"
      }
      [ H.text "corpus" ]


---------------------------------------------------------

type PhyloPhyloProps = ()

phyloPhylo :: R2.Component PhyloPhyloProps
phyloPhylo = R.createElement phyloPhyloCpt
phyloPhyloCpt :: R.Component PhyloPhyloProps
phyloPhyloCpt = here.component "phyloPhylo" cpt where
  cpt _ _ = do
    -- Render
    pure $

      H.div
      { id: "phyloPhylo"
      , className: "phylo-phylo"
      }
      [ H.text "phylomemy" ]


---------------------------------------------------------

type PhyloCorpusInfoProps =
  ( nbDocs :: Int
  , nbFoundations :: Int
  , nbPeriods :: Int
  )

phyloCorpusInfo :: R2.Component PhyloCorpusInfoProps
phyloCorpusInfo = R.createElement phyloCorpusInfoCpt
phyloCorpusInfoCpt :: R.Component PhyloCorpusInfoProps
phyloCorpusInfoCpt = here.component "phyloCorpusInfo" cpt where
  cpt props _ = do
    -- Render
    pure $

      H.div
      { id: "phyloCorpusInfo"
      , className: "phylo-corpus-info"
      }
      [
        H.span
        {}
        [
          H.b {} [ H.text $ show props.nbDocs ]
        , H.text $ nbsp 1 <> "docs"
        ]
      ,
        H.span
        {}
        [
          H.b {} [ H.text $ show props.nbFoundations ]
        , H.text $ nbsp 1 <> "foundations"
        ]
      ,
        H.span
        {}
        [
          H.b {} [ H.text $ show props.nbPeriods ]
        , H.text $ nbsp 1 <> "periods"
        ]
      ]


---------------------------------------------------------

type PhyloPhyloInfoProps =
  ( nbTerms :: Int
  , nbGroups :: Int
  , nbBranches :: Int
  )

phyloPhyloInfo :: R2.Component PhyloPhyloInfoProps
phyloPhyloInfo = R.createElement phyloPhyloInfoCpt
phyloPhyloInfoCpt :: R.Component PhyloPhyloInfoProps
phyloPhyloInfoCpt = here.component "phyloPhyloInfo" cpt where
  cpt props _ = do
    -- Render
    pure $

      H.div
      { id: "phyloPhyloInfo"
      , className: "phylo-phylo-info"
      }
      [
        H.span
        {}
        [
          H.b
          { id: "phyloTerms" }
          [ H.text $ show props.nbTerms ]
        , H.text $ nbsp 1 <> "terms"
        ]
      ,
        H.span
        {}
        [
          H.b
          { id: "phyloGroups" }
          [ H.text $ show props.nbGroups ]
        , H.text $ nbsp 1 <> "groups"
        ]
      ,
        H.span
        {}
        [
          H.b
          { id: "phyloBranches" }
          [ H.text $ show props.nbBranches ]
        , H.text $ nbsp 1 <> "branches"
        ]
      ]
