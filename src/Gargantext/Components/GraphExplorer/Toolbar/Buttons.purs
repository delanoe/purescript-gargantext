module Gargantext.Components.GraphExplorer.Toolbar.Buttons
  ( centerButton
  , cameraButton
  , edgesToggleButton
  , louvainButton
  , pauseForceAtlasButton
  , pauseNoverlapButton
  , resetForceAtlasButton
  , multiSelectEnabledButton
  ) where

import Prelude

import DOM.Simple.Console (log2)
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Formatter.DateTime as DFDT
import Data.Maybe (Maybe(..))
import Data.Sequence as Seq
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Now as EN
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ButtonVariant(..), Variant(..))
import Gargantext.Components.Forest.Tree.Node.Action.Upload (uploadArbitraryData)
import Gargantext.Components.Forest.Tree.Node.Action.Upload.Types (FileFormat(..))
import Gargantext.Components.GraphExplorer.API (cloneGraph)
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Components.GraphExplorer.Utils as GEU
import Gargantext.Data.Louvain as DLouvain
import Gargantext.Hooks.Sigmax as Sigmax
import Gargantext.Hooks.Sigmax.Camera as Camera
import Gargantext.Hooks.Sigmax.Graphology as Graphology
import Gargantext.Hooks.Sigmax.Louvain as Louvain
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

type CenterButtonProps =
  ( forceAtlasState :: T.Box SigmaxTypes.ForceAtlasState
  , sigmaRef        :: R.Ref Sigmax.Sigma )

centerButton :: R2.Leaf CenterButtonProps
centerButton = R2.leaf centerButtonCpt
centerButtonCpt :: R.Component CenterButtonProps
centerButtonCpt = here.component "centerButton" cpt
  where
    cpt { forceAtlasState
        , sigmaRef } _ = do
      forceAtlasState' <- R2.useLive' forceAtlasState

      pure $ B.button
        { callback: \_ -> do
          Sigmax.dependOnSigma (R.readRef sigmaRef) "[centerButton] sigma: Nothing" $ \s ->
            Camera.updateCamera (Camera.camera s) Camera.defaultCamera
        , status: SigmaxTypes.forceAtlasComponentStatus forceAtlasState'
        , variant: OutlinedButtonVariant Secondary
        }
        [ H.text "Center" ]

------------------------------------------------------

type CameraButtonProps =
  ( id              :: Int
  , hyperdataGraph  :: GET.HyperdataGraph
  , forceAtlasState :: T.Box SigmaxTypes.ForceAtlasState
  , reloadForest    :: T2.ReloadS
  , session         :: Session
  , sigmaRef        :: R.Ref Sigmax.Sigma
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
        , forceAtlasState
        , hyperdataGraph: GET.HyperdataGraph { graph: GET.GraphData graphData' }
        , reloadForest
        , session
        , sigmaRef } _ = do
      forceAtlasState' <- R2.useLive' forceAtlasState

      pure $ B.button
        { callback: \_ -> do
             filename <- screenshotFilename
             Sigmax.dependOnSigma (R.readRef sigmaRef) "[cameraButton] sigma: Nothing" $ \s -> do
               screen <- Sigma.takeScreenshot s
               let graph = Sigma.graph s
                   edges = Graphology.edges graph
                   nodes = Graphology.nodes graph
                   graphData = GET.GraphData $ graphData' { edges = A.fromFoldable $ Seq.map GEU.stEdgeToGET edges
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
        , status: SigmaxTypes.forceAtlasComponentStatus forceAtlasState'
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

      -- Render
      pure $
        B.button
        { -- TODO: Move this to Graph.purs to the R.useEffect handler which renders nodes/edges
          callback: \_ -> T.modify_ SigmaxTypes.toggleShowEdgesState state
        , status: SigmaxTypes.forceAtlasComponentStatus stateAtlas'
        , variant: state' == SigmaxTypes.EShow ?
            ButtonVariant Secondary $
            OutlinedButtonVariant Secondary
        }
        [ H.text "Edges" ]

------------------------------------------------------

type LouvainButtonProps =
  ( forceAtlasState   :: T.Box SigmaxTypes.ForceAtlasState
  , graph             :: T.Box SigmaxTypes.SGraph
  , sigmaRef          :: R.Ref Sigmax.Sigma
  , transformedGraph  :: T.Box SigmaxTypes.SGraph
  )

louvainButton :: R2.Leaf LouvainButtonProps
louvainButton = R2.leaf louvainButtonCpt
louvainButtonCpt :: R.Component LouvainButtonProps
louvainButtonCpt = here.component "louvainButton" cpt
  where
    cpt { forceAtlasState, graph, sigmaRef, transformedGraph } _ = do
      graph' <- R2.useLive' graph
      forceAtlasState' <- R2.useLive' forceAtlasState

      pure $
        B.button
        { callback: \_ -> do
             Sigmax.dependOnSigma (R.readRef sigmaRef) "[graphView (louvainGraph)] no sigma" $ \sigma -> do
               newGraph <- Louvain.assignVisible (Sigma.graph sigma) {}
               let cluster = Louvain.cluster newGraph :: DLouvain.LouvainCluster
               let lgraph = SigmaxTypes.louvainGraph graph' cluster :: SigmaxTypes.SGraph
               T.write_ lgraph transformedGraph
             pure unit

        , status: SigmaxTypes.forceAtlasComponentStatus forceAtlasState'
        , variant: OutlinedButtonVariant Secondary
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

        --icn SigmaxTypes.InitialLoading  = "pause"
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



type NoverlapButtonProps =
  ( state :: T.Box SigmaxTypes.NoverlapState
  )

pauseNoverlapButton :: R2.Leaf NoverlapButtonProps
pauseNoverlapButton = R2.leaf pauseNoverlapButtonCpt
pauseNoverlapButtonCpt :: R.Component NoverlapButtonProps
pauseNoverlapButtonCpt = here.component "pauseNoverlapButton" cpt
  where
    cpt { state } _ = do
      -- States
      state' <- R2.useLive' state

      -- Computed
      let
        vrt SigmaxTypes.NoverlapRunning = ButtonVariant Secondary
        vrt _                           = OutlinedButtonVariant Secondary

        icn SigmaxTypes.NoverlapRunning = "object-ungroup"
        icn SigmaxTypes.NoverlapPaused  = "object-ungroup"

      -- Render
      pure $

        B.button
        { variant: vrt state'
        , className: intercalate " "
            [ "toolbar-atlas-button"
            ]
        , callback: \_ -> T.modify_ SigmaxTypes.toggleNoverlapState state
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
      Sigmax.dependOnSigma (R.readRef sigmaRef) "[resetForceAtlasButton] no sigma" $ \_sigma -> do
        -- TODO Use fa2Ref instead of sigmaRef
        --Sigma.killForceAtlas2 sigma
        --Sigma.refreshForceAtlas sigma Graph.forceAtlas2Settings
        T.write_ SigmaxTypes.Killed forceAtlasState

------------------------------------------------------------------

type MultiSelectEnabledButtonProps =
  ( forceAtlasState :: T.Box SigmaxTypes.ForceAtlasState
  , state           :: T.Box Boolean
  )

multiSelectEnabledButton :: R2.Leaf MultiSelectEnabledButtonProps
multiSelectEnabledButton = R2.leaf multiSelectEnabledButtonCpt
multiSelectEnabledButtonCpt :: R.Component MultiSelectEnabledButtonProps
multiSelectEnabledButtonCpt = here.component "multiSelectEnabledButton" cpt
  where
    cpt { forceAtlasState, state } _ = do
      state' <- R2.useLive' state
      forceAtlasState' <- R2.useLive' forceAtlasState

      pure $
        H.div
        { className: intercalate " "
            [ "btn-group"
            , "align-items-center"
            ]
        , role: "group"
        }
        [
          B.button
          { callback: \_ -> T.write_ false state
          , status: SigmaxTypes.forceAtlasComponentStatus forceAtlasState'
          , variant: state' ?
              OutlinedButtonVariant Secondary $
              ButtonVariant Secondary
          }
          [ H.text "Single" ]
        ,
          B.button
          { callback: \_ -> T.write_ true state
          , status: SigmaxTypes.forceAtlasComponentStatus forceAtlasState'
          , variant: state' ?
              ButtonVariant Secondary $
              OutlinedButtonVariant Secondary
          }
          [ H.text "Multiple" ]
        ]
