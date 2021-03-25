module Gargantext.Components.GraphExplorer.RangeControl
  ( Props
  , rangeControl
  , edgeConfluenceControl
  , edgeWeightControl
  , nodeSizeControl
  ) where

import Prelude
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.Components.RangeSlider as RS
import Gargantext.Utils.Range as Range
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.GraphExplorer.RangeControl"

type Props = (
    caption :: String
  , sliderProps :: Record RS.Props
  )

rangeControl :: R2.Component Props
rangeControl = R.createElement rangeControlCpt

rangeControlCpt :: R.Component Props
rangeControlCpt = here.component "rangeButton" cpt
  where
    cpt {caption, sliderProps} _ = do
      pure $
        H.span {className: "range text-center"}
          [ H.label {} [ R2.small {} [ H.text caption ] ]
          , RS.rangeSlider sliderProps
          ]

type EdgeConfluenceControlProps = (
    range :: Range.NumberRange
  , state :: T.Box Range.NumberRange
  )

edgeConfluenceControl :: R2.Component EdgeConfluenceControlProps
edgeConfluenceControl = R.createElement edgeConfluenceControlCpt

edgeConfluenceControlCpt :: R.Component EdgeConfluenceControlProps
edgeConfluenceControlCpt = here.component "edgeConfluenceControl" cpt
  where
    cpt { range: Range.Closed { min, max }
        , state } _ = do
      state' <- T.useLive T.unequal state

      pure $ rangeControl {
        caption: "Edge Confluence Weight"
        , sliderProps: {
          bounds: Range.Closed { min, max }
          , initialValue: state'
          , epsilon: 0.01
          , step: 1.0
          , width: 10.0
          , height: 5.0
          , onChange: \rng -> T.write_ rng state
          }
        } []

type EdgeWeightControlProps = (
    range :: Range.NumberRange
  , state :: T.Box Range.NumberRange
  )

edgeWeightControl :: R2.Component EdgeWeightControlProps
edgeWeightControl = R.createElement edgeWeightControlCpt

edgeWeightControlCpt :: R.Component EdgeWeightControlProps
edgeWeightControlCpt = here.component "edgeWeightControl" cpt
  where
    cpt { range: Range.Closed { min, max }
        , state } _ = do
      state' <- T.useLive T.unequal state

      pure $ rangeControl {
        caption: "Edge Weight"
        , sliderProps: {
          bounds: Range.Closed { min, max }
          , initialValue: state'
          , epsilon: 1.0
          , step: 1.0
          , width: 10.0
          , height: 5.0
          , onChange: \rng -> T.write_ rng state
          }
        } []

type NodeSideControlProps = (
    range :: Range.NumberRange
  , state :: T.Box Range.NumberRange
  )

nodeSizeControl :: R2.Component NodeSideControlProps
nodeSizeControl = R.createElement nodeSizeControlCpt

nodeSizeControlCpt :: R.Component NodeSideControlProps
nodeSizeControlCpt = here.component "nodeSizeControl" cpt
  where
    cpt { range: Range.Closed { min, max }
        , state } _ = do
      state' <- T.useLive T.unequal state

      pure $ rangeControl {
        caption: "Node Size"
        , sliderProps: {
          bounds: Range.Closed { min, max }
          , initialValue: state'
          , epsilon: 0.1
          , step: 1.0
          , width: 10.0
          , height: 5.0
          , onChange: \rng -> T.write_ rng state
          }
        } []
