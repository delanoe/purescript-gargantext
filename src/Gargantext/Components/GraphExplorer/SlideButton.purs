module Gargantext.Components.GraphExplorer.SlideButton
  ( Props
  , sizeButton
  , cursorSizeButton
  , labelSizeButton
  ) where

import Global (readFloat)
import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (snd)
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

cursorSizeButton :: R.State Number -> R.Element
cursorSizeButton state =
  sizeButton {
      state: state
    , caption: "Cursor Size"
    , min: 1.0
    , max: 4.0
    , onChange: \e -> snd state $ const $ readFloat $ R2.unsafeEventValue e
    }

labelSizeButton :: R.Ref Sigmax.Sigma -> R.State Number -> R.Element
labelSizeButton sigmaRef state =
  sizeButton {
      state: state
    , caption: "Label Size"
    , min: 5.0
    , max: 30.0
    , onChange: \e -> do
      let mSigma = Sigmax.readSigma $ R.readRef sigmaRef
      let newValue = readFloat $ R2.unsafeEventValue e
      let (value /\ setValue) = state
      case mSigma of
        Just s -> Sigma.setSettings s {
          defaultLabelSize: newValue
          }
        _      -> pure unit
      setValue $ const newValue
    }
