module Gargantext.Components.PhyloExplorer.TopBar
  ( topBar
  ) where

import Gargantext.Prelude

import Data.Array as A
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ButtonVariant(..), Variant(..))
import Gargantext.Components.InputWithAutocomplete (inputWithAutocomplete)
import Gargantext.Components.PhyloExplorer.Store as PhyloStore
import Gargantext.Components.PhyloExplorer.Types (Source(..))
import Gargantext.Types (SidePanelState(..), toggleSidePanelState)
import Gargantext.Utils ((?))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

type Props =
  ( autocompleteClickCallback :: String -> Effect Unit
  , sourceCallback            :: String -> Effect Unit
  , searchCallback            :: String -> Effect (Array String)
  )

here :: R2.Here
here = R2.here "Gargantext.Components.PhyloExplorer.TopBar"

topBar :: R2.Leaf Props
topBar = R2.leaf component

component :: R.Component Props
component = here.component "main" cpt where
  cpt { autocompleteClickCallback
      , sourceCallback
      , searchCallback
      } _ = do
    -- | States
    -- |
    store@
      { toolBarDisplayed
      , sideBarDisplayed
      } <- PhyloStore.use

    toolBar'  <- R2.useLive' toolBarDisplayed
    sideBar'  <- R2.useLive' sideBarDisplayed

    source    <- R2.useLive' store.source
    sources   <- R2.useLive' store.sources

    searchState <- T.useBox ""

    -- | Render
    --
    pure $

      H.div
      { className: "phylo-topbar" }
      [
        -- Toolbar toggle
        B.button
        { className: "phylo-topbar__toolbar rounded-circle-2"
        , callback: \_ -> T.modify_ (not) toolBarDisplayed
        , variant: toolBar' ?
            ButtonVariant Light $
            OutlinedButtonVariant Light
        }
        [ H.text $ toolBar' ? "Hide toolbar" $ "Show toolbar" ]
      ,
        -- Sidebar toggle
        B.button
        { className: "phylo-topbar__sidebar rounded-circle-2"
        , callback: \_ -> T.modify_ (toggleSidePanelState) sideBarDisplayed
        , variant: sideBar' == Opened ?
            ButtonVariant Light $
            OutlinedButtonVariant Light
        }
        [ H.text $ sideBar' == Opened ? "Hide sidebar" $ "Show sidebar" ]
      ,
        -- Source
        H.div
        { className: "phylo-topbar__source"}
        [
          B.formSelect
          { value: source
          , callback: sourceCallback
          } $
          [
            H.option
            { value: ""
            , disabled: true
            }
            [ H.text "Select a source" ]
          ]
          <>
            flip map sources

              \(Source { id, label }) ->
                H.option
                { value: id }
                [ H.text label ]

        ]
      ,
        -- Search (wrapped in its form for the "enter" keyboard event submit)
        H.form
        { className: "phylo-topbar__autocomplete"
        }
        [
          inputWithAutocomplete
          { autocompleteSearch: searchCallback
          , onAutocompleteClick: autocompleteClickCallback
          , onEnterPress: \s -> do
                  cs <- searchCallback s
                  case A.head cs of
                    Nothing -> pure unit
                    Just h -> autocompleteClickCallback h
          , classes: "filter-results-completions"
          , placeholder: "Find a term"
          , state: searchState
          }
        ]
      ]


