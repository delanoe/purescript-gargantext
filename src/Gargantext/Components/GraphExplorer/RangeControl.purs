module Gargantext.Components.GraphExplorer.RangeControl
  ( Props
  , rangeControl
  , edgeConfluenceControl
  , edgeWeightControl
  , nodeSizeControl
  ) where

import Prelude
import Data.Tuple.Nested ((/\))
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Components.RangeSlider as RS
import Gargantext.Hooks.Sigmax as Sigmax
import Gargantext.Hooks.Sigmax.Sigma as Sigma
import Gargantext.Utils.Range as Range

type Props = (
    caption :: String
  , sliderProps :: Record RS.Props
  )

rangeControl :: Record Props -> R.Element
rangeControl props = R.createElement rangeControlCpt props []

rangeControlCpt :: R.Component Props
rangeControlCpt = R.hooksComponent "RangeButton" cpt
  where
    cpt {caption, sliderProps} _ = do
      pure $
        H.span {className: "range text-center"}
          [ H.label {} [ H.text caption ]
          , RS.rangeSlider sliderProps
          ]

edgeConfluenceControl :: R.Ref Sigmax.Sigma -> R.State Range.NumberRange -> R.Element
edgeConfluenceControl sigmaRef (state /\ setState) =
  rangeControl {
      caption: "Edge Confluence Weight"
    , sliderProps: {
        bounds: Range.Closed { min: 0.0, max: 1.0 }
      , initialValue: state
      , epsilon: 0.01
      , step: 1.0
      , width: 10.0
      , height: 5.0
      , onChange: setState <<< const
      }
    }

edgeWeightControl :: R.Ref Sigmax.Sigma -> R.State Range.NumberRange -> R.Element
edgeWeightControl sigmaRef (state /\ setState) =
  rangeControl {
      caption: "Edge Weight"
    , sliderProps: {
        bounds: Range.Closed { min: 0.0, max: 3.0 }
      , initialValue: state
      , epsilon: 0.01
      , step: 1.0
      , width: 10.0
      , height: 5.0
      , onChange: setState <<< const
      -- , onChange: \range@(Range.Closed {min, max}) -> do
      --     let sigma = R.readRef sigmaRef
      --     Sigmax.dependOnSigma sigma "[edgeWeightControl] sigma: Nothing" $ \s -> do
      --       Sigma.setSettings s {
      --         minEdgeSize: min
      --       , maxEdgeSize: max
      --       }
      --     setState $ const range
      }
    }

nodeSizeControl :: Range.NumberRange -> R.State Range.NumberRange -> R.Element
nodeSizeControl (Range.Closed { min: rangeMin, max: rangeMax }) (state /\ setState) =
  rangeControl {
      caption: "Node Size"
    , sliderProps: {
        bounds: Range.Closed { min: rangeMin, max: rangeMax }
      , initialValue: state
      , epsilon: 0.1
      , step: 1.0
      , width: 10.0
      , height: 5.0
      , onChange: \range@(Range.Closed {min, max}) -> do
          -- let sigma = R.readRef sigmaRef
          -- Sigmax.dependOnSigma sigma "[nodeSizeControl] sigma: Nothing" $ \s -> do
          --   Sigma.setSettings s {
          --     minNodeSize: min
          --   , maxNodeSize: max
          --   }
          setState $ const range
      }
    }
