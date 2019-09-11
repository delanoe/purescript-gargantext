module Gargantext.Components.GraphExplorer.RangeControl
  ( Props
  , rangeControl
  , edgeSizeControl
  , nodeSizeControl
  ) where

import Prelude
import Data.Maybe (Maybe(..))
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
        H.span {}
          [ H.label {} [ H.text caption ]
          , RS.rangeSlider sliderProps
          ]

edgeSizeControl :: R.Ref (Maybe Sigmax.Sigma) -> R.State Range.NumberRange -> R.Element
edgeSizeControl sigmaRef (state /\ setState) =
  rangeControl {
      caption: "Edge Size"
    , sliderProps: {
        bounds: Range.Closed { min: 0.0, max: 3.0 }
      , initialValue: state
      , epsilon: 0.1
      , step: 1.0
      , width: 10.0
      , height: 5.0
      , onChange: \range@(Range.Closed {min, max}) -> do
          let mSigma = Sigmax.readSigma <$> R.readRef sigmaRef
          case mSigma of
            Just (Just s) -> Sigma.setSettings s {
                minEdgeSize: min
              , maxEdgeSize: max
              }
            _             -> pure unit
          setState $ const range
      }
    }

nodeSizeControl :: R.Ref (Maybe Sigmax.Sigma) -> R.State Range.NumberRange -> R.Element
nodeSizeControl sigmaRef (state /\ setState) =
  rangeControl {
      caption: "Node Size"
    , sliderProps: {
        bounds: Range.Closed { min: 5.0, max: 15.0 }
      , initialValue: state
      , epsilon: 0.1
      , step: 1.0
      , width: 10.0
      , height: 5.0
      , onChange: \range@(Range.Closed {min, max}) -> do
          let mSigma = Sigmax.readSigma <$> R.readRef sigmaRef
          case mSigma of
            Just (Just s) -> Sigma.setSettings s {
                minNodeSize: min
              , maxNodeSize: max
              }
            _             -> pure unit
          setState $ const range
      }
    }
