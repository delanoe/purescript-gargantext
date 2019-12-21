module Gargantext.Components.GraphExplorer.SlideButton
  ( Props
  , sizeButton
  , labelSizeButton
  , mouseSelectorSizeButton
  ) where

import Global (readFloat)
import Prelude
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Hooks.Sigmax as Sigmax
import Gargantext.Hooks.Sigmax.Sigma as Sigma
import Gargantext.Utils.Reactix as R2

type Props = (
    state :: R.State Number
  , caption :: String
  , min :: Number
  , max :: Number
  , onChange :: forall e. e -> Effect Unit
  )

sizeButton :: Record Props -> R.Element
sizeButton props = R.createElement sizeButtonCpt props []

sizeButtonCpt :: R.Component Props
sizeButtonCpt = R.hooksComponent "SizeButton" cpt
  where
    cpt {state, caption, min, max, onChange} _ = do
      let (value /\ setValue) = state
      pure $
        H.span {}
          [ H.label {} [ H.text caption ]
          , H.input { type: "range"
                    , className: "form-control"
                    , min: show min
                    , max: show max
                    , defaultValue: value
                    , on: {input: onChange}
                    }
          ]

labelSizeButton :: R.Ref Sigmax.Sigma -> R.State Number -> R.Element
labelSizeButton sigmaRef state =
  sizeButton {
      state
    , caption: "Label Size"
    , min: 5.0
    , max: 30.0
    , onChange: \e -> do
      let sigma = R.readRef sigmaRef
      let newValue = readFloat $ R2.unsafeEventValue e
      let (_ /\ setValue) = state
      Sigmax.dependOnSigma sigma "[labelSizeButton] sigma: Nothing" $ \s -> do
        Sigma.setSettings s {
          defaultLabelSize: newValue
        }
      setValue $ const newValue
    }

mouseSelectorSizeButton :: R.Ref Sigmax.Sigma -> R.State Number -> R.Element
mouseSelectorSizeButton sigmaRef state =
  sizeButton {
      state
    , caption: "Selector Size"
    , min: 1.0
    , max: 50.0
    , onChange: \e -> do
      let sigma = R.readRef sigmaRef
      let (_ /\ setValue) = state
      let newValue = readFloat $ R2.unsafeEventValue e
      Sigmax.dependOnSigma sigma "[mouseSelectorSizeButton] sigma: Nothing" $ \s -> do
        Sigma.setSettings s {
          mouseSelectorSize: newValue
        }
      setValue $ const newValue
  }
