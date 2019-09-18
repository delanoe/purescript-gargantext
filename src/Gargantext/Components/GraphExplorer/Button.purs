module Gargantext.Components.GraphExplorer.Button
  (
    centerButton
  , Props
  , simpleButton
  ) where

import Global (readFloat)
import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log, log2)
import Effect (Effect)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Hooks.Sigmax as Sigmax
import Gargantext.Hooks.Sigmax.Sigma as Sigma
import Gargantext.Utils.Reactix as R2

type Props = (
    onClick :: forall e. e -> Effect Unit
  , text :: String
  )

simpleButton :: Record Props -> R.Element
simpleButton props = R.createElement simpleButtonCpt props []

simpleButtonCpt :: R.Component Props
simpleButtonCpt = R.hooksComponent "SimpleButton" cpt
  where
    cpt {onClick, text} _ = do
      pure $
        H.span {}
          [
            H.button
              { className: "btn btn-primary", on: {click: onClick} }
              [ H.text text ]
          ]

centerButton :: R.Ref (Maybe Sigmax.Sigma) -> R.Element
centerButton sigmaRef = simpleButton {
    onClick: \_ -> do
      let mSigma = Sigmax.readSigma <$> R.readRef sigmaRef
      log2 "[centerButton] mSigma" mSigma
      case mSigma of
        Just (Just s) -> Sigma.goToAllCameras s {x: 0.0, y: 0.0, ratio: 1.0, angle: 0.0}
        _             -> pure unit
  , text: "Center"
  }
