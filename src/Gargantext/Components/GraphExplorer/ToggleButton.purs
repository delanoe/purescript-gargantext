module Gargantext.Components.GraphExplorer.ToggleButton
  ( Props, toggleButton, toggleButtonCpt
  , controlsToggleButton
  , edgesToggleButton
  , sidebarToggleButton
  , pauseForceAtlasButton
  , treeToggleButton
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Hooks.Sigmax as Sigmax
import Gargantext.Hooks.Sigmax.Sigma as Sigma

type Props = (
    state :: R.State Boolean
  , onMessage :: String
  , offMessage :: String
  , onClick :: forall e. e -> Effect Unit
  )

toggleButton :: Record Props -> R.Element
toggleButton props = R.createElement toggleButtonCpt props []

toggleButtonCpt :: R.Component Props
toggleButtonCpt = R.hooksComponent "ToggleButton" cpt
  where
    cpt {state, onMessage, offMessage, onClick} _ = do
      let (toggled /\ _) = state
      pure $
        H.span {}
          [
            H.button
              { className: "btn btn-primary", on: {click: onClick} }
              [ H.text (text onMessage offMessage toggled) ]
          ]
    text on _off true = on
    text _on off false = off

controlsToggleButton :: R.State Boolean -> R.Element
controlsToggleButton state =
  toggleButton {
      state: state
    , onMessage: "Hide Controls"
    , offMessage: "Show Controls"
    , onClick: \_ -> snd state not
    }

edgesToggleButton :: R.Ref Sigmax.Sigma -> R.State Boolean -> R.Element
edgesToggleButton sigmaRef state =
  toggleButton {
      state: state
    , onMessage: "Hide Edges"
    , offMessage: "Show Edges"
    , onClick: \_ -> do
      let sigma = R.readRef sigmaRef
      let (toggled /\ setToggled) = state
      Sigmax.dependOnSigma sigma "[edgesToggleButton] sigma: Nothing" $ \s -> do
        let settings = {
              drawEdges: not toggled
            , drawEdgeLabels: not toggled
            , hideEdgesOnMove: toggled
          }
        Sigma.setSettings s settings
      setToggled not
    }

pauseForceAtlasButton :: R.Ref Sigmax.Sigma -> R.State Boolean -> R.Element
pauseForceAtlasButton sigmaRef state =
  toggleButton {
      state: state
    , onMessage: "Pause Force Atlas"
    , offMessage: "Start Force Atlas"
    , onClick: \_ -> do
      let (_ /\ setToggled) = state
      setToggled not
    }

treeToggleButton :: R.State Boolean -> R.Element
treeToggleButton state =
  toggleButton {
      state: state
    , onMessage: "Hide Tree"
    , offMessage: "Show Tree"
    , onClick: \_ -> snd state not
    }

sidebarToggleButton :: R.State Boolean -> R.Element
sidebarToggleButton state =
  toggleButton {
      state: state
    , onMessage: "Hide Sidebar"
    , offMessage: "Show Sidebar"
    , onClick: \_ -> snd state not
    }
