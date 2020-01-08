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

edgeConfluenceControl :: Range.NumberRange -> R.State Range.NumberRange -> R.Element
edgeConfluenceControl (Range.Closed { min, max }) (state /\ setState) =
  rangeControl {
      caption: "Edge Confluence Weight"
    , sliderProps: {
        bounds: Range.Closed { min, max }
      , initialValue: state
      , epsilon: 0.01
      , step: 1.0
      , width: 10.0
      , height: 5.0
      , onChange: setState <<< const
      }
    }

edgeWeightControl :: Range.NumberRange -> R.State Range.NumberRange -> R.Element
edgeWeightControl (Range.Closed { min, max }) (state /\ setState) =
  rangeControl {
      caption: "Edge Weight"
    , sliderProps: {
        bounds: Range.Closed { min, max }
      , initialValue: state
      , epsilon: 1.0
      , step: 1.0
      , width: 10.0
      , height: 5.0
      , onChange: setState <<< const
      }
    }

nodeSizeControl :: Range.NumberRange -> R.State Range.NumberRange -> R.Element
nodeSizeControl (Range.Closed { min, max }) (state /\ setState) =
  rangeControl {
      caption: "Node Size"
    , sliderProps: {
        bounds: Range.Closed { min, max }
      , initialValue: state
      , epsilon: 0.1
      , step: 1.0
      , width: 10.0
      , height: 5.0
      , onChange: setState <<< const
      }
    }
