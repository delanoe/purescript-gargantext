module Gargantext.Components.GraphExplorer.Toolbar.Buttons
  ( centerButton
  , cameraButton
  , edgesToggleButton
  , louvainToggleButton
  , pauseForceAtlasButton
  , resetForceAtlasButton
  , multiSelectEnabledButton
  ) where

import Prelude

import Data.Array as A
import Data.Either (Either(..))
import Data.Formatter.DateTime as DFDT
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.Sequence as Seq
import DOM.Simple.Console (log2)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Now as EN
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ButtonVariant(..), ComponentStatus(..), Variant(..))
import Gargantext.Components.Forest.Tree.Node.Action.Upload (uploadArbitraryData)
import Gargantext.Components.Forest.Tree.Node.Action.Upload.Types (FileFormat(..))
import Gargantext.Components.GraphExplorer.API (cloneGraph)
import Gargantext.Components.GraphExplorer.Resources as Graph
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Components.GraphExplorer.Utils as GEU
import Gargantext.Hooks.Sigmax as Sigmax
import Gargantext.Hooks.Sigmax.Camera as Camera
import Gargantext.Hooks.Sigmax.Graphology as Graphology
import Gargantext.Hooks.Sigmax.Sigma as Sigma
import Gargantext.Hooks.Sigmax.Types as SigmaxTypes
import Gargantext.Sessions (Session)
import Gargantext.Utils ((?))
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.GraphExplorer.Toolbar.Button"

------------------------------------------------------

centerButton :: R.Ref Sigmax.Sigma -> R.Element
centerButton sigmaRef = B.button
  { variant: OutlinedButtonVariant Secondary
  , callback: \_ -> do
      let sigma = R.readRef sigmaRef
      Sigmax.dependOnSigma sigma "[centerButton] sigma: Nothing" $ \s ->
        Camera.updateCamera (Camera.camera s) Camera.defaultCamera
  }
  [ H.text "Center" ]

------------------------------------------------------

type CameraButtonProps =
  ( id             :: Int
  , hyperdataGraph :: GET.HyperdataGraph
  , reloadForest   :: T2.ReloadS
  , session        :: Session
  , sigmaRef       :: R.Ref Sigmax.Sigma
  )

screenshotFilename :: Effect String
screenshotFilename = do
  nowdt <- EN.nowDateTime
  pure $ case DFDT.formatDateTime "YYYY-MM-DDTHH:mm:ss" nowdt of
    Left err -> err
    Right s -> s <> "-screenshot.png"

cameraButton :: R2.Leaf CameraButtonProps
cameraButton = R2.leaf cameraButtonCpt
cameraButtonCpt :: R.Component CameraButtonProps
cameraButtonCpt = here.component "cameraButton" cpt
  where
    cpt { id
        , hyperdataGraph: GET.HyperdataGraph { graph: GET.GraphData hyperdataGraph }
        , reloadForest
        , session
        , sigmaRef } _ = do
      pure $ B.button
        { callback: \_ -> do
             filename <- screenshotFilename
             let sigma = R.readRef sigmaRef
             Sigmax.dependOnSigma sigma "[cameraButton] sigma: Nothing" $ \s -> do
               screen <- Sigma.takeScreenshot s
               let graph = Sigma.graph s
                   edges = Graphology.edges graph
                   nodes = Graphology.nodes graph
                   graphData = GET.GraphData $ hyperdataGraph { edges = A.fromFoldable $ Seq.map GEU.stEdgeToGET edges
                                                              , nodes = A.fromFoldable $ GEU.normalizeNodes $ Seq.map GEU.stNodeToGET nodes }
               let camera = Camera.toCamera $ Camera.camera s
               let hyperdataGraph' = GET.HyperdataGraph { graph: graphData, mCamera: Just camera }
               launchAff_ $ do
                 eClonedGraphId <- cloneGraph { id, hyperdataGraph: hyperdataGraph', session }
                 case eClonedGraphId of
                   Left err -> liftEffect $ log2 "[cameraButton] RESTError" err
                   Right clonedGraphId -> do
                     eRet <- uploadArbitraryData session clonedGraphId Plain (Just filename) screen
                     case eRet of
                       Left err -> liftEffect $ log2 "[cameraButton] RESTError" err
                       Right _ret -> do
                         liftEffect $ T2.reload reloadForest
        , variant: OutlinedButtonVariant Secondary
        } [ H.text "Screenshot" ]

------------------------------------------------------

type EdgesButtonProps =
  ( state       :: T.Box SigmaxTypes.ShowEdgesState
  , stateAtlas  :: T.Box SigmaxTypes.ForceAtlasState
  )

edgesToggleButton :: R2.Leaf EdgesButtonProps
edgesToggleButton = R2.leaf edgesToggleButtonCpt
edgesToggleButtonCpt :: R.Component EdgesButtonProps
edgesToggleButtonCpt = here.component "edgesToggleButton" cpt
  where
    cpt { state, stateAtlas } _ = do
      -- States
      state'      <- R2.useLive' state
      stateAtlas' <- R2.useLive' stateAtlas

      -- Computed
      let
        cst SigmaxTypes.InitialRunning  = Disabled
        cst SigmaxTypes.Running         = Disabled
        cst _                           = Enabled

      -- Render
      pure $
        B.button
        { variant: state' == SigmaxTypes.EShow ?
            ButtonVariant Secondary $
            OutlinedButtonVariant Secondary
        , status: cst stateAtlas'
          -- TODO: Move this to Graph.purs to the R.useEffect handler which renders nodes/edges
        , callback: \_ -> T.modify_ SigmaxTypes.toggleShowEdgesState state
        }
        [ H.text "Edges" ]

------------------------------------------------------

type LouvainToggleButtonProps =
  ( state :: T.Box Boolean
  )

louvainToggleButton :: R2.Leaf LouvainToggleButtonProps
louvainToggleButton = R2.leaf louvainToggleButtonCpt
louvainToggleButtonCpt :: R.Component LouvainToggleButtonProps
louvainToggleButtonCpt = here.component "louvainToggleButton" cpt
  where
    cpt { state } _ = do
      state' <- R2.useLive' state

      pure $

        B.button
        { variant: state' ?
            ButtonVariant Secondary $
            OutlinedButtonVariant Secondary
        , callback: \_ -> T.modify_ (not) state
        }
        [ H.text "Louvain" ]

--------------------------------------------------------------

type ForceAtlasProps =
  ( state :: T.Box SigmaxTypes.ForceAtlasState
  )

pauseForceAtlasButton :: R2.Leaf ForceAtlasProps
pauseForceAtlasButton = R2.leaf pauseForceAtlasButtonCpt
pauseForceAtlasButtonCpt :: R.Component ForceAtlasProps
pauseForceAtlasButtonCpt = here.component "pauseForceAtlasButtonButton" cpt
  where
    cpt { state } _ = do
      -- States
      state' <- R2.useLive' state

      -- Computed
      let
        cls SigmaxTypes.InitialRunning  = "on-running-animation active"
        cls SigmaxTypes.Running         = "on-running-animation active"
        cls _                           = ""

        vrt SigmaxTypes.InitialRunning  = ButtonVariant Secondary
        vrt SigmaxTypes.Running         = ButtonVariant Secondary
        vrt _                           = OutlinedButtonVariant Secondary

        icn SigmaxTypes.InitialRunning  = "pause"
        icn SigmaxTypes.InitialStopped  = "play"
        icn SigmaxTypes.Running         = "pause"
        icn SigmaxTypes.Paused          = "play"
        icn SigmaxTypes.Killed          = "play"

      -- Render
      pure $

        B.button
        { variant: vrt state'
        , className: intercalate " "
            [ cls state'
            , "toolbar-atlas-button"
            ]
        , callback: \_ -> T.modify_ SigmaxTypes.toggleForceAtlasState state
        }
        [
          B.icon
          { name: icn state'}
        ]

--------------------------------------------------------

type ResetForceAtlasProps =
  ( forceAtlasState :: T.Box SigmaxTypes.ForceAtlasState
  , sigmaRef        :: R.Ref Sigmax.Sigma
  )

resetForceAtlasButton :: R2.Leaf ResetForceAtlasProps
resetForceAtlasButton = R2.leaf resetForceAtlasButtonCpt
resetForceAtlasButtonCpt :: R.Component ResetForceAtlasProps
resetForceAtlasButtonCpt = here.component "resetForceAtlasToggleButton" cpt
  where
    cpt { forceAtlasState, sigmaRef } _ = do
      pure $ H.button { className: "btn btn-outline-secondary"
                      , on: { click: onClick forceAtlasState sigmaRef }
                      } [ R2.small {} [ H.text "Reset Force Atlas" ] ]

    onClick forceAtlasState sigmaRef _ = do
      -- TODO Sigma.killForceAtlas2 sigma
      -- startForceAtlas2 sigma
      Sigmax.dependOnSigma (R.readRef sigmaRef) "[resetForceAtlasButton] no sigma" $ \sigma -> do
        -- TODO Use fa2Ref instead of sigmaRef
        --Sigma.killForceAtlas2 sigma
        --Sigma.refreshForceAtlas sigma Graph.forceAtlas2Settings
        T.write_ SigmaxTypes.Killed forceAtlasState

------------------------------------------------------------------

type MultiSelectEnabledButtonProps =
  ( state       :: T.Box Boolean
  )

multiSelectEnabledButton :: R2.Leaf MultiSelectEnabledButtonProps
multiSelectEnabledButton = R2.leaf multiSelectEnabledButtonCpt
multiSelectEnabledButtonCpt :: R.Component MultiSelectEnabledButtonProps
multiSelectEnabledButtonCpt = here.component "multiSelectEnabledButton" cpt
  where
    cpt { state } _ = do
      state' <- R2.useLive' state

      pure $

        H.div
        { className: "btn-group"
        , role: "group"
        }
        [
          B.button
          { variant: state' ?
              OutlinedButtonVariant Secondary $
              ButtonVariant Secondary
          , callback: \_ -> T.write_ false state
          }
          [ H.text "Single" ]
        ,
          B.button
          { variant: state' ?
              ButtonVariant Secondary $
              OutlinedButtonVariant Secondary
          , callback: \_ -> T.write_ true state
          }
          [ H.text "Multiple" ]
        ]
