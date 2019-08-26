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
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log2)
import Effect.Class (liftEffect)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Hooks.Sigmax as Sigmax
import Gargantext.Hooks.Sigmax.Sigma (startForceAtlas2, stopForceAtlas2)
import Gargantext.Utils.Reactix as R2

type Props = (
    state :: R.State Boolean
  , onMessage :: String
  , offMessage :: String
  )

toggleButton :: Record Props -> R.Element
toggleButton props = R.createElement toggleButtonCpt props []

toggleButtonCpt :: R.Component Props
toggleButtonCpt = R.hooksComponent "ToggleButton" cpt
  where
    cpt {state, onMessage, offMessage} _ = do
      let (toggled /\ setToggled) = state
      pure $
        H.span {}
          [
            H.button
              { className: "btn btn-primary", on: {click: \_ -> setToggled not} }
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
    }

edgesToggleButton :: R.State Boolean -> R.Element
edgesToggleButton state =
  toggleButton {
      state: state
    , onMessage: "Hide Edges"
    , offMessage: "Show Edges"
    }

pauseForceAtlasButton :: R.Ref (Maybe Sigmax.Sigma) -> R.State Boolean -> R.Element
pauseForceAtlasButton sigmaRef state = R.createElement el props []
  where
    props = {
        state
      , onMessage: "Pause Force Atlas"
      , offMessage: "Start Force Atlas"
      }
    el = R.hooksComponent "ForceAtlasButton" cpt
    cpt {state, onMessage, offMessage} _ = do
      let (toggled /\ setToggled) = state
      pure $
        H.span {}
          [
            H.button
              { className: "btn btn-primary"
              , on: {click: \_ -> do
                        let mSigma = R.readRef sigmaRef
                        log2 "[pauseForceAtlasButton] mSigma" mSigma
                        case mSigma of
                          Nothing -> pure unit
                          Just sigma -> do
                            log2 "[pauseForceAtlasButton] sigma" sigma
                            log2 "[pauseForceAtlasButton] toggled" toggled
                            if toggled then
                              case Sigmax.readSigma sigma of
                                Nothing -> pure unit
                                Just s -> stopForceAtlas2 s
                            else
                              --startForceAtlas2 $ graph.sigma
                              pure unit
                        --setToggled not
                    }
              }
              [ H.text (text onMessage offMessage toggled) ]
          ]
    text on _off true = on
    text _on off false = off

treeToggleButton :: R.State Boolean -> R.Element
treeToggleButton state =
  toggleButton {
      state: state
    , onMessage: "Hide Tree"
    , offMessage: "Show Tree"
    }

sidebarToggleButton :: R.State Boolean -> R.Element
sidebarToggleButton state =
  toggleButton {
      state: state
    , onMessage: "Hide Sidebar"
    , offMessage: "Show Sidebar"
    }
