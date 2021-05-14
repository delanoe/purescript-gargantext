module Gargantext.Components.GraphExplorer.ToggleButton
  ( Props
  , toggleButton
  , toggleButtonCpt
  , controlsToggleButton
  , edgesToggleButton
  , louvainToggleButton
  , multiSelectEnabledButton
  , sidebarToggleButton
  , pauseForceAtlasButton
  , treeToggleButton
  ) where

import Prelude

import Effect (Effect)
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.Hooks.Sigmax.Types as SigmaxTypes
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2

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

      pure $ H.button { className: "btn btn-outline-" <> style <> " " <> cls state' <> " mx-2"
                      , on: { click: onClick }
                      } [ R2.small {} [ H.text (text onMessage offMessage state') ] ]

    cls true = "active"
    cls false = ""
    text on _off true = on
    text _on off false = off

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

type EdgesButtonProps = (
  state :: T.Box SigmaxTypes.ShowEdgesState
)

edgesToggleButton :: R2.Component EdgesButtonProps
edgesToggleButton = R.createElement edgesToggleButtonCpt
edgesToggleButtonCpt :: R.Component EdgesButtonProps
edgesToggleButtonCpt = here.component "edgesToggleButton" cpt
  where
    cpt { state } _ = do
      state' <- T.useLive T.unequal state

      pure $ H.button { className: "btn btn-outline-primary " <> cls state'
                      , on: { click: onClick state }
                      } [ R2.small {} [ H.text (text state') ] ]

    text s = if SigmaxTypes.edgeStateHidden s then "Show edges" else "Hide edges"

    cls SigmaxTypes.EShow = ""
    cls _ = "active"

    -- TODO: Move this to Graph.purs to the R.useEffect handler which renders nodes/edges
    onClick state _ = T.modify_ SigmaxTypes.toggleShowEdgesState state

type LouvainToggleButtonProps = (
  state :: T.Box Boolean
)

louvainToggleButton :: R2.Component LouvainToggleButtonProps
louvainToggleButton = R.createElement louvainToggleButtonCpt
louvainToggleButtonCpt :: R.Component LouvainToggleButtonProps
louvainToggleButtonCpt = here.component "louvainToggleButton" cpt
  where
    cpt { state } _ = do
      pure $ toggleButton {
          state: state
        , onMessage: "Louvain off"
        , offMessage: "Louvain on"
        , onClick: \_ -> T.modify_ not state
        , style: "primary"
        } []

type MultiSelectEnabledButtonProps = (
  state :: T.Box Boolean
)

multiSelectEnabledButton :: R2.Component MultiSelectEnabledButtonProps
multiSelectEnabledButton = R.createElement multiSelectEnabledButtonCpt
multiSelectEnabledButtonCpt :: R.Component MultiSelectEnabledButtonProps
multiSelectEnabledButtonCpt = here.component "lmultiSelectEnabledButton" cpt
  where
    cpt { state } _ = do
      pure $ toggleButton {
          state: state
        , onMessage: "Single-node"
        , offMessage: "Multi-node"
        , onClick: \_ -> T.modify_ not state
        , style : "primary"
        } []

type ForceAtlasProps = (
  state :: T.Box SigmaxTypes.ForceAtlasState
)

pauseForceAtlasButton :: R2.Component ForceAtlasProps
pauseForceAtlasButton = R.createElement pauseForceAtlasButtonCpt
pauseForceAtlasButtonCpt :: R.Component ForceAtlasProps
pauseForceAtlasButtonCpt = here.component "forceAtlasToggleButton" cpt
  where
    cpt { state } _ = do
      state' <- T.useLive T.unequal state

      pure $ H.button { className: "btn btn-outline-primary " <> cls state'
                      , on: { click: onClick state }
                      } [ R2.small {} [ H.text (text state') ] ]

    cls SigmaxTypes.InitialRunning = "active"
    cls SigmaxTypes.Running = "active"
    cls _ = ""

    text SigmaxTypes.InitialRunning = "Pause Force Atlas"
    text SigmaxTypes.InitialStopped = "Start Force Atlas"
    text SigmaxTypes.Running = "Pause Force Atlas"
    text SigmaxTypes.Paused = "Start Force Atlas"

    onClick state _ = T.modify_ SigmaxTypes.toggleForceAtlasState state

type TreeToggleButtonProps = (
  state :: T.Box Boolean
)

treeToggleButton :: R2.Component TreeToggleButtonProps
treeToggleButton = R.createElement treeToggleButtonCpt
treeToggleButtonCpt :: R.Component TreeToggleButtonProps
treeToggleButtonCpt = here.component "treeToggleButton" cpt
  where
    cpt { state } _ = do
      pure $ toggleButton {
          state: state
        , onMessage: "Hide Tree"
        , offMessage: "Show Tree"
        , onClick: \_ -> T.modify_ not state
        , style: "light"
        } []

type SidebarToggleButtonProps = (
  state :: T.Box GT.SidePanelState
)

sidebarToggleButton :: R2.Component SidebarToggleButtonProps
sidebarToggleButton = R.createElement sidebarToggleButtonCpt
sidebarToggleButtonCpt :: R.Component SidebarToggleButtonProps
sidebarToggleButtonCpt = here.component "sidebarToggleButton" cpt
  where
    cpt { state } _ = do
      state' <- T.useLive T.unequal state

      pure $ H.button { className: "btn btn-outline-light " <> cls state'
                      , on: { click: onClick state }
                      } [ R2.small {} [ H.text (text onMessage offMessage state') ] ]

    cls GT.Opened = "active"
    cls _         = ""

    onMessage = "Hide Sidebar"
    offMessage = "Show Sidebar"
    text on _off GT.Opened        = on
    text _on off GT.InitialClosed = off
    text _on off GT.Closed        = off

    onClick state = \_ ->
      T.modify_ GT.toggleSidePanelState state
                  -- case s of
        -- GET.InitialClosed -> GET.Opened GET.SideTabLegend
        -- GET.Closed        -> GET.Opened GET.SideTabLegend
        -- (GET.Opened _)    -> GET.Closed) state
