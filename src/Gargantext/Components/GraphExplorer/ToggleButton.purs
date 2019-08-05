module Gargantext.Components.GraphExplorer.ToggleButton
  ( Props, toggleButton, toggleButtonCpt
  , treeToggleButton
  , controlsToggleButton
  , sidebarToggleButton
  , edgesToggleButton
  ) where

import Prelude (bind, pure, ($))
import Reactix as R
import Reactix.HTML as H

type Props = ( state :: R.State Boolean, onMessage :: String, offMessage :: String )

toggleButton :: Record Props -> R.Element
toggleButton props = R.createElement toggleButtonCpt props []
  
toggleButtonCpt :: R.Component Props
toggleButtonCpt = R.hooksComponent "ToggleButton" cpt
  where
    cpt {state, onMessage, offMessage} _ = do
      let (toggled /\ setToggled) = state
      pure $
        RH.button
          { className: "btn btn-primary", on: {click: \_ -> setToggled not } }
          [ RH.text (text onMessage offMessage toggled) ]
    text on _off true = on
    text _on off false = off

treeToggleButton :: R.State Boolean -> R.Element
treeToggleButton state =
  toggleButton { state: state, onMessage: "Hide Tree", offMessage: "Show Tree" }

controlsToggleButton :: R.State Boolean -> R.Element
controlsToggleButton state =
  toggleButton { state: state, onMessage: "Hide Controls", offMessage: "Show Controls" }

sidebarToggleButton :: R.State Boolean -> R.Element
sidebarToggleButton state =
  toggleButton { state: state, onMessage: "Hide Sidebar", offMessage: "Show Sidebar" }

edgesToggleButton :: R.State Boolean -> R.Element
edgesToggleButton state =
  toggleButton { state: state, onMessage: "Hide Edges", offMessage: "Show Edges" }

