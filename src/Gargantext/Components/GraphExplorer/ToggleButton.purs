module Gargantext.Components.GraphExplorer.ToggleButton
  ( Props
  , toggleButton
  , toggleButtonCpt
  , controlsToggleButton
  ) where

import Prelude

import Effect (Effect)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

-- @WIP: used?

here :: R2.Here
here = R2.here "Gargantext.Components.GraphExplorer.ToggleButton"

type Props = (
    state      :: T.Box Boolean
  , onMessage  :: String
  , offMessage :: String
  , style      :: String
  , onClick    :: forall e. e -> Effect Unit
  )

toggleButton :: R2.Component Props
toggleButton = R.createElement toggleButtonCpt
toggleButtonCpt :: R.Component Props
toggleButtonCpt = here.component "toggleButton" cpt
  where
    cpt { state
        , onMessage
        , offMessage
        , onClick
        , style } _ = do
      state' <- T.useLive T.unequal state

      pure $ H.div { className: "btn btn-outline-" <> style <> " " <> cls state' <> " mx-2"
                   , on: { click: onClick }
                   } [ R2.small {} [ H.text (text onMessage offMessage state') ] ]

    cls true = "active"
    cls false = ""
    text on _off true = on
    text _on off false = off

----------------------------------------------------------------

type ControlsToggleButtonProps = (
  state :: T.Box Boolean
  )

controlsToggleButton :: R2.Component ControlsToggleButtonProps
controlsToggleButton = R.createElement controlsToggleButtonCpt
controlsToggleButtonCpt :: R.Component ControlsToggleButtonProps
controlsToggleButtonCpt = here.component "controlsToggleButton" cpt
  where
    cpt { state } _ = do
      pure $ toggleButton {
          state: state
        , onMessage: "Hide Controls"
        , offMessage: "Show Controls"
        , onClick: \_ -> T.modify_ not state
        , style: "light"
        } []
