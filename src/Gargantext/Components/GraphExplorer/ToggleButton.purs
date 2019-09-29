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

edgesToggleButton :: R.Ref (Maybe Sigmax.Sigma) -> R.State Boolean -> R.Element
edgesToggleButton sigmaRef state =
  toggleButton {
      state: state
    , onMessage: "Hide Edges"
    , offMessage: "Show Edges"
    , onClick: \_ -> do
      let mSigma = Sigmax.readSigma <$> R.readRef sigmaRef
      let (toggled /\ setToggled) = state
      case mSigma of
        Just (Just s) -> do
          let settings = {
                drawEdges: not toggled
              , drawEdgeLabels: not toggled
              , hideEdgesOnMove: toggled
            }
          Sigma.setSettings s settings
        _             -> pure unit
      setToggled not
    }

pauseForceAtlasButton :: R.Ref (Maybe Sigmax.Sigma) -> R.State Boolean -> R.Element
pauseForceAtlasButton sigmaRef state =
  toggleButton {
      state: state
    , onMessage: "Pause Force Atlas"
    , offMessage: "Start Force Atlas"
    , onClick: \_ -> do
      let mSigma = Sigmax.readSigma <$> R.readRef sigmaRef
      let (toggled /\ setToggled) = state
      case mSigma of
        Just (Just s) -> if toggled then
            Sigma.stopForceAtlas2 s
          else
            Sigma.restartForceAtlas2 s
        _             -> pure unit
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
