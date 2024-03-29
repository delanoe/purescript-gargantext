module Gargantext.Components.GraphExplorer.Toolbar.RangeControl
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
import Gargantext.Hooks.Sigmax.Types as SigmaxTypes
import Gargantext.Utils.Range as Range
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.GraphExplorer.Toolbar.RangeControl"

type Props =
  ( caption     :: String
  , sliderProps :: Record RS.Props
  )

rangeControl :: R2.Leaf Props
rangeControl = R2.leaf rangeControlCpt
rangeControlCpt :: R.Component Props
rangeControlCpt = here.component "rangeButton" cpt
  where
    cpt { caption, sliderProps } _ = do
      pure $ H.span
        { className: "range-control" }
        [
          H.label
          { className: "range-control__label" }
          [ H.text caption ]
        ,
          RS.rangeSlider sliderProps
        ]

----------------------------------------

type EdgeConfluenceControlProps =
  ( forceAtlasState :: T.Box SigmaxTypes.ForceAtlasState
  , range           :: Range.NumberRange
  , state           :: T.Box Range.NumberRange
  )

edgeConfluenceControl :: R2.Leaf EdgeConfluenceControlProps
edgeConfluenceControl = R2.leaf edgeConfluenceControlCpt
edgeConfluenceControlCpt :: R.Component EdgeConfluenceControlProps
edgeConfluenceControlCpt = here.component "edgeConfluenceControl" cpt
  where
    cpt { forceAtlasState
        , range: Range.Closed { min, max }
        , state
        } _ = do
      forceAtlasState' <- R2.useLive' forceAtlasState
      state' <- T.useLive T.unequal state

      pure $ rangeControl {
        caption: "Edge Confluence Weight"
        , sliderProps: {
            bounds: Range.Closed { min, max }
          , epsilon: 0.01
          , height: 5.0
          , initialValue: state'
          , onChange: \rng -> T.write_ rng state
          , status: SigmaxTypes.forceAtlasComponentStatus forceAtlasState'
          , step: 1.0
          , width: 10.0
          }
        }

--------------------------------------

type EdgeWeightControlProps =
  ( forceAtlasState :: T.Box SigmaxTypes.ForceAtlasState
  , range :: Range.NumberRange
  , state :: T.Box Range.NumberRange
  )

edgeWeightControl :: R2.Leaf EdgeWeightControlProps
edgeWeightControl = R2.leaf edgeWeightControlCpt
edgeWeightControlCpt :: R.Component EdgeWeightControlProps
edgeWeightControlCpt = here.component "edgeWeightControl" cpt
  where
    cpt { forceAtlasState
        , range: Range.Closed { min, max }
        , state
        } _ = do
      forceAtlasState' <- R2.useLive' forceAtlasState
      state' <- T.useLive T.unequal state

      pure $ rangeControl {
        caption: "Edge Weight"
        , sliderProps: {
          bounds: Range.Closed { min, max }
          , initialValue: state'
          , epsilon: 1.0
          , height: 5.0
          , onChange: \rng -> T.write_ rng state
          , status: SigmaxTypes.forceAtlasComponentStatus forceAtlasState'
          , step: 1.0
          , width: 10.0
          }
        }

--------------------------------------

type NodeSideControlProps =
  ( forceAtlasState :: T.Box SigmaxTypes.ForceAtlasState
  , range           :: Range.NumberRange
  , state           :: T.Box Range.NumberRange
  )

nodeSizeControl :: R2.Leaf NodeSideControlProps
nodeSizeControl = R2.leaf nodeSizeControlCpt
nodeSizeControlCpt :: R.Component NodeSideControlProps
nodeSizeControlCpt = here.component "nodeSizeControl" cpt
  where
    cpt { forceAtlasState
        , range: Range.Closed { min, max }
        , state
        } _ = do
      forceAtlasState' <- R2.useLive' forceAtlasState
      state' <- R2.useLive' state

      pure $ rangeControl {
        caption: "Node Size"
        , sliderProps: {
          bounds: Range.Closed { min, max }
          , initialValue: state'
          , epsilon: 0.1
          , height: 5.0
          , onChange: \rng -> T.write_ rng state
          , status: SigmaxTypes.forceAtlasComponentStatus forceAtlasState'
          , step: 1.0
          , width: 10.0
          }
        }
