module Gargantext.Components.PhyloExplorer.Layout
  ( layout
  ) where

import Gargantext.Prelude

import DOM.Simple (Window, window)
import DOM.Simple.Console (log2)
import Data.Array as Array
import Data.Date as Date
import Data.FoldableWithIndex (forWithIndex_)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Number as Number
import Data.String as String
import Data.Symbol (SProxy(..))
import Data.Traversable (for, for_)
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import FFI.Simple (maybeGetProperty, (..), (...), (.=), (.?))
import Gargantext.Components.PhyloExplorer.Types (GlobalTerm(..), Group(..), PhyloDataSet(..))
import Gargantext.Utils (nbsp)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Record (get)
import Toestand as T
import Type.Proxy (Proxy(..))

here :: R2.Here
here = R2.here "Gargantext.Components.PhyloExplorer"

type Props =
  ( phyloDataSet :: PhyloDataSet
  )

layout :: R2.Component Props
layout = R.createElement layoutCpt
layoutCpt :: R.Component Props
layoutCpt = here.component "layout" cpt where
  cpt { phyloDataSet: (PhyloDataSet o)
      } _ = do
    -- States


    R.useEffectOnce' $ do
      setGlobalDependencies window (PhyloDataSet o)

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
            flip Array.mapWithIndex o.sources
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
        { nbDocs        : o.nbDocs
        , nbFoundations : o.nbFoundations
        , nbPeriods     : o.nbPeriods
        }
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
        { nbTerms     : o.nbTerms
        , nbGroups    : o.nbGroups
        , nbBranches  : o.nbBranches
        }
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



setGlobalDependencies :: Window -> PhyloDataSet -> Effect Unit
setGlobalDependencies w (PhyloDataSet o)
  = do
    _ <- pure $ (w .= "freq") {}
    _ <- pure $ (w .= "nbBranches") o.nbBranches
    _ <- pure $ (w .= "nbDocs") o.nbDocs
    _ <- pure $ (w .= "nbFoundations") o.nbFoundations
    _ <- pure $ (w .= "nbGroups") o.nbGroups
    _ <- pure $ (w .= "nbPeriods") o.nbPeriods
    _ <- pure $ (w .= "nbTerms") o.nbTerms
    _ <- pure $ (w .= "sources") o.sources
    _ <- pure $ (w .= "terms") []
    _ <- pure $ (w .= "timeScale") o.timeScale
    _ <- pure $ (w .= "weighted") o.weighted

    (freq :: Array Int)         <- pure $ w .. "freq"
    (terms :: Array GlobalTerm) <- pure $ w .. "terms"

    void $ for o.groups \(Group g) -> do

      let
        f = g.foundation
        l = g.label

      log2 "FOUNDATION" f
      log2 "LABEL" l

      -- For each entries in group.foundation array,
      -- increment consequently the global window.keys array
      -- forWithIndex_ f \i _ ->
      --   let i' = show i
      --   in case (freq .? i') of
      --     Nothing -> pure $ (freq .= i') 0
      --     Just v  -> pure $ (freq .= i') (v +1)
      for_ f \i ->
        let i' = show i
        in case (freq .? i') of
          Nothing -> pure $ (freq .= i') 0
          Just v  -> pure $ (freq .= i') (v +1)


      -- For each entries in group.foundation array,
      -- if the global window.terms does not have it in property,
      -- append an item to the global window.terms
      for_ f \i ->
        let i' = show i
        in case (terms .? i') of
          Nothing -> pure unit
          Just _  -> void <<< pure $ (terms .= i') $ GlobalTerm
            { label: l .. i'
            , fdt  : f .. i'
            }
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
