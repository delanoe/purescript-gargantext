module Gargantext.Components.GraphExplorer.Toolbar.SlideButton
  ( Props
  , sizeButton
  , labelSizeButton
  , labelRenderedSizeThresholdButton
  , mouseSelectorSizeSlider
  ) where

import Data.Maybe (Maybe(..))
import Data.Number as DN
import Prelude
import Effect (Effect)
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.Components.Bootstrap.Types (ComponentStatus(Disabled))
import Gargantext.Hooks.Sigmax as Sigmax
import Gargantext.Hooks.Sigmax.Sigma as Sigma
import Gargantext.Hooks.Sigmax.Types as SigmaxTypes
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.GraphExplorer.Toolbar.SlideButton"

type Props =
  ( caption         :: String
  , forceAtlasState :: T.Box SigmaxTypes.ForceAtlasState
  , min             :: Number
  , max             :: Number
  , onChange        :: forall e. e -> Effect Unit
  , state           :: T.Box Number
  )

sizeButton :: Record Props -> R.Element
sizeButton props = R.createElement sizeButtonCpt props []
sizeButtonCpt :: R.Component Props
sizeButtonCpt = here.component "sizeButton" cpt where
  cpt { state, caption, forceAtlasState, min, max, onChange } _ = do
    defaultValue <- T.useLive T.unequal state
    forceAtlasState' <- R2.useLive' forceAtlasState

    let status = SigmaxTypes.forceAtlasComponentStatus forceAtlasState'

    pure $

      H.span
      { className: "range-simple" }
      [
        H.label
        { className: "range-simple__label" }
        [ H.text caption ]
      ,
        H.span
        { className: "range-simple__field" }
        [
          H.input
          { type: "range"
          , min: show min
          , max: show max
          , defaultValue
          , on: { input: onChange }
          , className: "range-simple__input"
          , disabled: status == Disabled
          }
        ]
      ]

type LabelSizeButtonProps =
  ( forceAtlasState :: T.Box SigmaxTypes.ForceAtlasState
  , sigmaRef        :: R.Ref Sigmax.Sigma
  , state           :: T.Box Number)

labelSizeButton :: R2.Leaf LabelSizeButtonProps
labelSizeButton = R2.leaf labelSizeButtonCpt
labelSizeButtonCpt :: R.Component LabelSizeButtonProps
labelSizeButtonCpt = here.component "labelSizeButton" cpt
  where
    cpt { forceAtlasState, sigmaRef, state} _ = do
      pure $ sizeButton {
          state
        , caption: "Label size"
        , forceAtlasState
        , min: 1.0
        , max: 30.0
        , onChange: \e -> do
          let sigma = R.readRef sigmaRef
          let newValue' = DN.fromString $ R.unsafeEventValue e
          case newValue' of
            Nothing -> pure unit
            Just newValue ->
              Sigmax.dependOnSigma sigma "[labelSizeButton] sigma: Nothing" $ \s -> do
                Sigma.setSettings s {
                    defaultLabelSize: newValue
                  , drawLabels: true
                  , labelSize: newValue
                  , maxNodeSize: newValue / 2.5
                    --, labelSizeRatio: newValue / 2.5
                  }
                T.write_ newValue state
        }

type LabelRenderedSizeThresholdButtonProps =
  ( forceAtlasState :: T.Box SigmaxTypes.ForceAtlasState
  , sigmaRef :: R.Ref Sigmax.Sigma
  , state    :: T.Box Number)

labelRenderedSizeThresholdButton :: R2.Leaf LabelRenderedSizeThresholdButtonProps
labelRenderedSizeThresholdButton = R2.leaf labelRenderedSizeThresholdButtonCpt
labelRenderedSizeThresholdButtonCpt :: R.Component LabelRenderedSizeThresholdButtonProps
labelRenderedSizeThresholdButtonCpt = here.component "labelRenderedSizeThresholdButton" cpt
  where
    cpt { forceAtlasState, sigmaRef, state} _ = do
      pure $ sizeButton {
        state
        , caption: "Label rendered size threshold"
        , forceAtlasState
        , min: 0.0
        , max: 10.0
        , onChange: \e -> do
          let sigma = R.readRef sigmaRef
          let newValue' = DN.fromString $ R.unsafeEventValue e
          case newValue' of
            Nothing -> pure unit
            Just newValue ->
              Sigmax.dependOnSigma sigma "[labelRenderdSizeThresholdButton] sigma: Nothing" $ \s -> do
                Sigma.setSettings s {
                  labelRenderedSizeThreshold: newValue
                  }
                T.write_ newValue state
        }

type MouseSelectorSizeSliderProps =
  ( forceAtlasState :: T.Box SigmaxTypes.ForceAtlasState
  , sigmaRef :: R.Ref Sigmax.Sigma
  , state    :: T.Box Number)

mouseSelectorSizeSlider :: R2.Leaf MouseSelectorSizeSliderProps
mouseSelectorSizeSlider = R2.leaf mouseSelectorSizeSliderCpt
mouseSelectorSizeSliderCpt :: R.Component MouseSelectorSizeSliderProps
mouseSelectorSizeSliderCpt = here.component "mouseSelectorSizeSlider" cpt
  where
    cpt { forceAtlasState, sigmaRef, state } _ = do
      pure $ sizeButton {
          caption: "Selector size (Shift + wheel)"
        , forceAtlasState
        , min: 1.0
        , max: 100.0
        , onChange: \e -> do
          let sigma = R.readRef sigmaRef
          let newValue' = DN.fromString $ R.unsafeEventValue e
          case newValue' of
            Nothing -> pure unit
            Just newValue ->
              Sigmax.dependOnSigma sigma "[mouseSelectorSizeButton] sigma: Nothing" $ \s -> do
                Sigma.setSettings s {
                  mouseSelectorSize: newValue
                  }
                T.write_ newValue state
        , state
        }
