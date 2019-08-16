module Gargantext.Components.GraphExplorer.SlideButton
  ( Props
  , sizeButton
  , cursorSizeButton
  , labelSizeButton
  ) where

import Global (readFloat)
import Prelude
import Data.Tuple.Nested ((/\))
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Utils.Reactix as R2

type Props = ( state :: R.State Number, caption :: String, min :: Number, max :: Number )

sizeButton :: Record Props -> R.Element
sizeButton props = R.createElement sizeButtonCpt props []

sizeButtonCpt :: R.Component Props
sizeButtonCpt = R.hooksComponent "SizeButton" cpt
  where
    cpt {state, caption, min, max} _ = do
      let (value /\ setValue) = state
      pure $
        H.span {}
          [ H.label {} [ H.text caption ]
          , H.input { type: "range"
                    , className: "form-control"
                    , min: show min
                    , max: show max
                    , defaultValue: value
                    , on: {input: \e -> setValue $ const $ readFloat $ R2.unsafeEventValue e }
                    }
          ]

cursorSizeButton :: R.State Number -> R.Element
cursorSizeButton state =
  sizeButton { state: state, caption: "Cursor Size", min: 1.0, max: 4.0 }

labelSizeButton :: R.State Number -> R.Element
labelSizeButton state =
  sizeButton { state: state, caption: "Label Size", min: 1.0, max: 4.0 }
