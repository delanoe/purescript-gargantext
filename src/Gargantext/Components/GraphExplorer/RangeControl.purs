module Gargantext.Components.GraphExplorer.RangeControl
  ( Props
  , rangeControl
  , edgeSizeControl
  , nodeSizeControl
  ) where

import Global (readFloat)
import Prelude
import Data.Tuple.Nested ((/\))
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Components.RangeSlider as RS
import Gargantext.Utils.Range as Range
import Gargantext.Utils.Reactix as R2

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
        H.span {}
          [ H.label {} [ H.text caption ]
          , RS.rangeSlider sliderProps
          ]

edgeSizeControl :: R.State Range.NumberRange -> R.Element
edgeSizeControl (state /\ setState) =
  rangeControl {
      caption: "Edge Size"
    , sliderProps: {
        bounds: Range.Closed { min: 0.0, max: 3.0 }
      , initialValue: state
      , epsilon: 0.1
      , step: 1.0
      , width: 10.0
      , height: 5.0
      , onChange: setState <<< const
      }
    }

nodeSizeControl :: R.State Range.NumberRange -> R.Element
nodeSizeControl (state /\ setState) =
  rangeControl {
      caption: "Node Size"
    , sliderProps: {
        bounds: Range.Closed { min: 5.0, max: 15.0 }
      , initialValue: state
      , epsilon: 0.1
      , step: 1.0
      , width: 10.0
      , height: 5.0
      , onChange: setState <<< const
      }
    }
