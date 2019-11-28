module Gargantext.Components.GraphExplorer.Button
  ( centerButton
  , Props
  , simpleButton
  ) where

import Prelude
import Effect (Effect)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Hooks.Sigmax as Sigmax
import Gargantext.Hooks.Sigmax.Sigma as Sigma

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

centerButton :: R.Ref Sigmax.Sigma -> R.Element
centerButton sigmaRef = simpleButton {
    onClick: \_ -> do
      let sigma = R.readRef sigmaRef
      Sigmax.dependOnSigma sigma "[centerButton] sigma: Nothing" $ \s ->
        Sigma.goToAllCameras s {x: 0.0, y: 0.0, ratio: 1.0, angle: 0.0}
  , text: "Center"
  }
