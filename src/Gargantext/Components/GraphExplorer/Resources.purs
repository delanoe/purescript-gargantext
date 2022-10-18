module Gargantext.Components.GraphExplorer.Resources
  ( drawGraph
  , sigmaSettings, SigmaSettings--, SigmaOptionalSettings
  , forceAtlas2Settings, ForceAtlas2Settings--, ForceAtlas2OptionalSettings
  )
  where

import Gargantext.Prelude

import DOM.Simple (window)
import DOM.Simple.Types (Element)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Effect.Class.Console as ECC
import Gargantext.Components.App.Store as AppStore
import Gargantext.Components.GraphExplorer.Store as GraphStore
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Components.Themes (darksterTheme)
import Gargantext.Components.Themes as Themes
import Gargantext.Hooks.Sigmax as Sigmax
import Gargantext.Hooks.Sigmax.Camera as Camera
import Gargantext.Hooks.Sigmax.Graphology as Graphology
import Gargantext.Hooks.Sigmax.ForceAtlas2 as ForceAtlas2
import Gargantext.Hooks.Sigmax.Sigma as Sigma
import Gargantext.Hooks.Sigmax.Types as SigmaxTypes
import Gargantext.Utils (getter)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Record (merge)
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.GraphExplorer.Resources"

type Props sigma forceatlas2 =
  ( elRef                 :: R.Ref (Nullable Element)
  , fa2Ref                :: R.Ref (Maybe ForceAtlas2.FA2Layout)
  , forceAtlas2Settings   :: forceatlas2
  , sigmaRef              :: R.Ref Sigmax.Sigma
  , sigmaSettings         :: sigma
  , transformedGraph      :: SigmaxTypes.SGraph
  )

drawGraph :: forall s fa2. R2.Leaf (Props s fa2)
drawGraph = R2.leaf drawGraphCpt
drawGraphCpt :: forall s fa2. R.Memo (Props s fa2)
drawGraphCpt = R.memo' $ here.component "graph" cpt where
-- drawGraphCpt :: forall s fa2. R.Component (Props s fa2)
-- drawGraphCpt = here.component "graph" cpt where
  -- | Component
  -- |
  cpt { elRef
      , fa2Ref
      , sigmaRef
      , forceAtlas2Settings: fa2Settings
      , transformedGraph
      } _ = do

    boxes <- AppStore.use

    { showEdges
    , edgeConfluence
    , edgeWeight
    , graph
    , graphStage
    , hyperdataGraph
    , mouseSelectorSize
    , multiSelectEnabled
    , selectedNodeIds
    , startForceAtlas
    } <- GraphStore.use

    showEdges'        <- R2.useLive' showEdges
    edgeConfluence'   <- R2.useLive' edgeConfluence
    edgeWeight'       <- R2.useLive' edgeWeight
    graphStage'       <- R2.useLive' graphStage
    graph'            <- R2.useLive' graph
    startForceAtlas'  <- R2.useLive' startForceAtlas
    hyperdataGraph'   <- R2.useLive' hyperdataGraph

    -- | Hooks
    -- |

    -- Clean up
    R.useEffectOnce $ do
      pure $ do
        here.log "[graphCpt (Cleanup)]"
        case R.readRef fa2Ref of
          Nothing -> pure unit
          Just fa2 -> do
            ForceAtlas2.stop fa2
            ForceAtlas2.kill fa2
            here.log2 "[graphCpt (Cleanup)] forceAtlas stopped for" fa2
            R.setRef fa2Ref Nothing
        Sigmax.dependOnSigma (R.readRef sigmaRef) "[graphCpt (Cleanup)] no sigma" $ \sigma -> do
          Sigma.kill sigma
          here.log "[graphCpt (Cleanup)] sigma killed"

    -- Stage Init
    R.useEffect1' graphStage' $ case graphStage' of

      GET.Init -> do
        let mCamera = getter _.mCamera hyperdataGraph'
        let rSigma = R.readRef sigmaRef

        case Sigmax.readSigma rSigma of
          Nothing -> do
            theme <- T.read boxes.theme
            eSigma <- case R.readNullableRef elRef of
              Nothing -> do
                _ <- ECC.error "elRef is empty"
                pure $ Left "elRef is empty"
              Just el -> Sigma.sigma el { settings: sigmaSettings theme }
            case eSigma of
              Left err -> here.warn2 "[graphCpt] error creating sigma" err
              Right sig -> do
                Sigmax.writeSigma rSigma $ Just sig

                Sigmax.dependOnContainer elRef "[graphCpt (Ready)] container not found" $ \c -> do
                  _ <- Sigma.addRenderer sig {
                      "type": "canvas"
                    , container: c
                    , additionalContexts: ["mouseSelector"]
                    }
                  pure unit

                --newGraph <- Graphology.graphFromSigmaxGraph graph'
                --Sigmax.refreshData sig newGraph

                Sigmax.dependOnSigma (R.readRef sigmaRef) "[graphCpt (Ready)] no sigma" $ \sigma -> do
                  -- bind the click event only initially, when ref was empty
                  Sigmax.bindSelectedNodesClick sigma selectedNodeIds multiSelectEnabled
                  Sigmax.bindShiftWheel sigma mouseSelectorSize
                  _ <- Sigma.bindMouseSelectorPlugin sigma
                  -- NOTE For some reason, setting 'renderLabels:
                  -- false' in sigmaSettings and initializing sigma
                  -- with those settings doesn't work. We need to set
                  -- the renderLabels: false here again.
                  Sigma.setSettings sigma { renderLabels: false }
                  pure unit

                Sigmax.setSigmaEdgesVisibility sig { edgeConfluence: edgeConfluence'
                                                   , edgeWeight: edgeWeight'
                                                   , showEdges: showEdges' }

                -- here.log2 "[graph] startForceAtlas" startForceAtlas
                if startForceAtlas' then
                  case R.readRef fa2Ref of
                    Nothing -> do
                      fa2 <- ForceAtlas2.init (Sigma.graph sig) fa2Settings
                      ForceAtlas2.start fa2
                      R.setRef fa2Ref (Just fa2)
                    Just fa2 -> do
                      -- TODO Kill and restart? Maybe check fa2.graph first? Should be equal to sigma.graph
                      pure unit
                else
                  case R.readRef fa2Ref of
                    Nothing -> pure unit
                    Just fa2 -> ForceAtlas2.stop fa2

                case mCamera of
                  Just cam -> do
                    Camera.updateCamera (Camera.camera sig) cam
                  -- Default camera: slightly de-zoom the graph to avoid
                  -- nodes sticking to the container borders
                  Nothing ->
                    Camera.updateCamera (Camera.camera sig) Camera.defaultCamera

                -- Reload Sigma on Theme changes
                -- TODO
                -- _ <- flip T.listen boxes.theme \{ old, new } ->
                --   if (eq old new) then pure unit
                --   else Sigma.proxySetSettings window sig $ sigmaSettings new

                pure unit
          Just _sig -> do
            pure unit

        T.write_ GET.Ready graphStage

      _ -> pure unit

    -- Stage ready
    --
    -- @TODO Probably this can be optimized to re-mark selected nodes only when
    --       they changed → one solution could be to list every effects subject
    --       to a graph transformation (eg. "showLouvain", "edgeConfluence",
    --       etc) // drawback: don't forget to modify the effect white-list
    R.useEffect' case graphStage' of

      GET.Ready -> do
        let tEdgesMap = SigmaxTypes.edgesGraphMap transformedGraph
        let tNodesMap = SigmaxTypes.nodesGraphMap transformedGraph

        Sigmax.dependOnSigma (R.readRef sigmaRef) "[graphCpt (Ready)] no sigma" $ \sigma -> do
          Sigmax.performDiff sigma transformedGraph
          -- Sigmax.updateEdges sigma tEdgesMap
          -- Sigmax.updateNodes sigma tNodesMap
          let edgesState = not $ SigmaxTypes.edgeStateHidden showEdges'
          -- here.log2 "[graphCpt] edgesState" edgesState
          Sigmax.setSigmaEdgesVisibility sigma { edgeConfluence: edgeConfluence'
                                               , edgeWeight: edgeWeight'
                                               , showEdges: showEdges' }

      _ -> pure unit


    -- | Render
    -- |
    pure $

      R2.fromMaybe (R.readNullableRef elRef) (R.createPortal [])

    -- NOTE: This div is not empty after sigma initializes.
    -- When we change state, we make it empty though.
    --pure $ RH.div { ref: elRef, style: {height: "95%"} } []


type SigmaSettings =
  ( --animationsTime :: Number
  --, autoRescale :: Boolean
  --, autoResize :: Boolean
  --, batchEdgesDrawing :: Boolean
  --, borderSize :: Number
  -- , canvasEdgesBatchSize :: Number
  -- , clone :: Boolean
  -- , defaultEdgeColor :: String
  --, defaultEdgeHoverColor :: String
    defaultEdgeType :: String
  --, defaultHoverLabelBGColor :: String
  --, defaultHoverLabelColor :: String
  --, defaultLabelColor :: String
  -- , defaultLabelHoverColor :: String
  , defaultLabelSize :: Number
  --, defaultNodeBorderColor :: String
  , defaultNodeColor :: String
  -- , defaultNodeHoverColor :: String
  -- , defaultNodeType :: String
  --, doubleClickEnabled :: Boolean
  -- , doubleClickTimeout :: Number
  -- , doubleClickZoomDuration :: Number
  -- , doubleClickZoomingRatio :: Number
  -- , doubleTapTimeout :: Number
  -- , dragTimeout :: Number
  -- , drawEdgeLabels :: Boolean
  -- , drawEdges :: Boolean
  -- , drawLabels :: Boolean
  -- , drawNodes :: Boolean
  -- , edgeColor :: String
  -- , edgeHoverColor :: String
  -- , edgeHoverExtremities :: Boolean
  -- , edgeHoverPrecision :: Number
  -- , edgeHoverSizeRatio :: Number
  -- , edgesPowRatio :: Number
  -- , enableCamera :: Boolean
  , enableEdgeHoverEvents :: Boolean
  -- , enableHovering :: Boolean
  -- , eventsEnabled :: Boolean
  -- , font :: String
  -- , fontStyle :: String
  , hideEdgesOnMove :: Boolean
  -- , hoverFont :: String
  -- , hoverFontStyle :: String
  -- , immutable :: Boolean
  -- , labelColor :: String
  -- , labelHoverBGColor :: String
  -- , labelHoverColor :: String
  -- , labelHoverShadow :: String
  -- , labelHoverShadowColor :: String
  -- , labelSize :: String
  -- , labelSizeRatio :: Number
  , labelRenderedSizeThreshold :: Number
  --, labelThreshold :: Number
  -- , maxEdgeSize :: Number
  -- , maxNodeSize :: Number
  -- , minArrowSize :: Number
  -- , minEdgeSize :: Number
  -- , minNodeSize :: Number
  -- , mouseEnabled :: Boolean
  -- , mouseInertiaDuration :: Number
  -- , mouseInertiaRatio :: Number
  -- , mouseSelectorSize :: Number
  -- , mouseWheelEnabled :: Boolean
  -- , mouseZoomDuration :: Number
  -- , nodeBorderColor :: String
  -- , nodeHoverColor :: String
  --, nodesPowRatio :: Number
  , renderLabels :: Boolean
  -- , rescaleIgnoreSize :: Boolean
  -- , scalingMode :: String
  -- , sideMargin :: Number
  -- , singleHover :: Boolean
  -- , skipErrors :: Boolean
  -- , touchEnabled :: Boolean
  -- , touchInertiaDuration :: Number
  -- , touchInertiaRatio :: Number
  -- , twBorderGreyColor     :: String
  -- , twEdgeDefaultOpacity  :: Number
  -- , twEdgeGreyColor       :: String
  -- , twNodeRendBorderColor :: String
  -- , twNodeRendBorderSize :: Number
  -- , twNodesGreyOpacity    :: Number
  -- , twSelectedColor       :: String
  -- , verbose :: Boolean
  -- , webglEdgesBatchSize :: Number
  -- , webglOversamplingRatio :: Number
  -- , zoomMax :: Number
  -- , zoomMin :: Number
  -- , zoomingRatio :: Number
  )

  -- not selected <=> (1-greyness)
  -- selected nodes <=> special label
sigmaSettings :: Themes.Theme -> {|SigmaSettings}
sigmaSettings theme =
  { -- animationsTime : 30000.0
  -- , autoRescale : true
  --, autoResize : true
  --, batchEdgesDrawing : true
  --, borderSize : 1.0                   -- for ex, bigger border when hover
  --, defaultEdgeHoverColor : "#f00"
    defaultEdgeType : "line"          -- 'curve' or 'line' (curve iff ourRendering)
  -- , defaultHoverLabelBGColor : "#fff"
  -- , defaultHoverLabelColor : "#000"
  -- , defaultLabelColor : "#000"         -- labels text color
  , defaultLabelSize : 15.0                -- (old tina: showLabelsIfZoom)
  --, defaultNodeBorderColor  : "#000"   -- <- if nodeBorderColor = 'default'
  , defaultNodeColor : "#FFF"
  --, doubleClickEnabled : false -- indicates whether or not the graph can be zoomed on double-click
  -- , drawEdgeLabels : true
  -- , drawEdges : true
  --, drawLabels : true
  --, drawNodes : true
  -- , edgeHoverExtremities : true
  -- , edgeHoverColor : "edge"
  -- , edgeHoverPrecision : 2.0
  -- , edgeHoverSizeRatio : 2.0
  , enableEdgeHoverEvents : false
  -- , enableHovering : true
  -- , font : "arial"
  -- , fontStyle : ""
  , hideEdgesOnMove : true
  -- , labelSize  : "proportional" -- alt : proportional, fixed
  -- , labelSize : "fixed"
  -- , labelSizeRatio : 2.0               -- label size in ratio of node size
  , labelRenderedSizeThreshold: 2.0
  --, labelThreshold : 5.0 -- 5.0 for more labels              -- min node cam size to start showing label
  -- , maxEdgeSize : 1.0
  -- , maxNodeSize : 10.0
  -- , minEdgeSize : 0.5              -- in fact used in tina as edge size
  -- , minNodeSize : 1.0
  -- , mouseEnabled : true
  -- , mouseSelectorSize : 15.0
  -- , mouseZoomDuration : 150.0
  -- , nodeBorderColor : "default"           -- choices: "default" color vs. "node" color
  --, nodesPowRatio  : 10.8
  , renderLabels: false  -- initially false, because of forceatlas
  -- , rescaleIgnoreSize  : false
  -- , singleHover  : true
  -- , touchEnabled  : true
  -- , twBorderGreyColor  : "rgba(100, 100, 100, 0.9)"
  -- , twEdgeDefaultOpacity  : 0.4       -- initial opacity added to src/tgt colors
  -- , twEdgeGreyColor  : "rgba(100, 100, 100, 0.25)"
  -- , twNodeRendBorderColor  : "#FFF"
  -- , twNodeRendBorderSize  : 2.5          -- node borders (only iff ourRendering)
  -- , twNodesGreyOpacity  : 5.5           -- smaller value: more grey
  -- , twSelectedColor  : "node"     -- "node" for a label bg like the node color, "default" for white background
  -- , verbose  : true
  -- , zoomMax : 1.7
  -- , zoomMin : 0.0
  -- , zoomingRatio : 1.4
  }
  -- `merge` themeSettings theme
  -- where
  --   themeSettings t
  --     | eq t darksterTheme =
  --         { defaultHoverLabelBGColor: "#FFF"
  --         , defaultHoverLabelColor : "#000"
  --         , defaultLabelColor: "#FFF"
  --         }
  --     | otherwise =
  --         { defaultHoverLabelBGColor: "#FFF"
  --         , defaultHoverLabelColor : "#000"
  --         , defaultLabelColor: "#000"
  --         }

type ForceAtlas2Settings =
  ( adjustSizes                    :: Boolean
  , barnesHutOptimize              :: Boolean
  -- , barnesHutTheta              :: Number
  , batchEdgesDrawing              :: Boolean
  , edgeWeightInfluence            :: Number
  -- , fixedY                      :: Boolean
  , hideEdgesOnMove                :: Boolean
  , gravity                        :: Number
  , includeHiddenEdges             :: Boolean
  , includeHiddenNodes             :: Boolean
  , iterationsPerRender            :: Number
  , linLogMode                     :: Boolean
  , outboundAttractionDistribution :: Boolean
  , scalingRatio                   :: Number
  , skipHidden                     :: Boolean
  , slowDown                       :: Number
  , startingIterations             :: Number
  , strongGravityMode              :: Boolean
  -- , timeout                     :: Number
  -- , worker                      :: Boolean
  )

forceAtlas2Settings :: Record ForceAtlas2Settings
forceAtlas2Settings =
  { adjustSizes                    : true
  , barnesHutOptimize              : true
  , batchEdgesDrawing              : true
  , edgeWeightInfluence            : 1.0
    -- fixedY                      : false
  , gravity                        : 1.0
  , hideEdgesOnMove                : true
  , includeHiddenEdges             : false
  , includeHiddenNodes             : true
  , iterationsPerRender            : 100.0 -- 10.0
  , linLogMode                     : false  -- false
  , outboundAttractionDistribution : false
  , scalingRatio                   : 1000.0
  , skipHidden                     : false
  , slowDown                       : 0.2
  , startingIterations             : 10.0
  , strongGravityMode              : false
  }
