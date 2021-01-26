module Gargantext.Components.Graph
  -- ( graph, graphCpt
  -- , sigmaSettings, SigmaSettings, SigmaOptionalSettings
  -- , forceAtlas2Settings, ForceAtlas2Settings, ForceAtlas2OptionalSettings
  -- )
  where
import Prelude (bind, const, discard, not, pure, unit, ($))

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log, log2)
import DOM.Simple.Types (Element)
import FFI.Simple (delay)
import Reactix as R
import Reactix.DOM.HTML as RH

import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Hooks.Sigmax as Sigmax
import Gargantext.Hooks.Sigmax.Types as SigmaxTypes
import Gargantext.Hooks.Sigmax.Sigma as Sigma
import Gargantext.Utils.Reactix as R2

thisModule = "Gargantext.Components.Graph"

type OnProps  = ()

data Stage = Init | Ready | Cleanup

type Props sigma forceatlas2 = (
    elRef :: R.Ref (Nullable Element)
  , forceAtlas2Settings :: forceatlas2
  , graph :: SigmaxTypes.SGraph
  , mCamera :: Maybe GET.Camera
  , multiSelectEnabledRef :: R.Ref Boolean
  , selectedNodeIds :: R.State SigmaxTypes.NodeIds
  , showEdges :: R.State SigmaxTypes.ShowEdgesState
  , sigmaRef :: R.Ref Sigmax.Sigma
  , sigmaSettings :: sigma
  , stage :: R.State Stage
  , startForceAtlas :: Boolean
  , transformedGraph :: SigmaxTypes.SGraph
  )

graph :: forall s fa2. Record (Props s fa2) -> R.Element
graph props = R.createElement graphCpt props []

graphCpt :: forall s fa2. R.Component (Props s fa2)
graphCpt = R.hooksComponentWithModule thisModule "graph" cpt
  where
    cpt props _ = do
      stageHooks props

      R.useEffectOnce $ do
        pure $ do
          log "[graphCpt (Cleanup)]"
          Sigmax.dependOnSigma (R.readRef props.sigmaRef) "[graphCpt (Cleanup)] no sigma" $ \sigma -> do
            Sigma.stopForceAtlas2 sigma
            log2 "[graphCpt (Cleanup)] forceAtlas stopped for" sigma
            Sigma.kill sigma
            log "[graphCpt (Cleanup)] sigma killed"

      -- NOTE: This div is not empty after sigma initializes.
      -- When we change state, we make it empty though.
      --pure $ RH.div { ref: props.elRef, style: {height: "95%"} } []
      pure $ case R.readNullableRef props.elRef of
        Nothing -> RH.div {} []
        Just el -> R.createPortal [] el

    stageHooks props@{multiSelectEnabledRef, selectedNodeIds, sigmaRef, stage: (Init /\ setStage)} = do
      R.useEffectOnce' $ do
        let rSigma = R.readRef props.sigmaRef

        case Sigmax.readSigma rSigma of
          Nothing -> do
            eSigma <- Sigma.sigma {settings: props.sigmaSettings}
            case eSigma of
              Left err -> log2 "[graphCpt] error creating sigma" err
              Right sig -> do
                Sigmax.writeSigma rSigma $ Just sig

                Sigmax.dependOnContainer props.elRef "[graphCpt (Ready)] container not found" $ \c -> do
                  _ <- Sigma.addRenderer sig {
                      "type": "canvas"
                    , container: c
                    , additionalContexts: ["mouseSelector"]
                    }
                  pure unit

                Sigmax.refreshData sig $ Sigmax.sigmafy props.graph

                Sigmax.dependOnSigma (R.readRef sigmaRef) "[graphCpt (Ready)] no sigma" $ \sigma -> do
                  -- bind the click event only initially, when ref was empty
                  Sigmax.bindSelectedNodesClick sigma selectedNodeIds multiSelectEnabledRef
                  _ <- Sigma.bindMouseSelectorPlugin sigma
                  pure unit

                Sigmax.setEdges sig false

                -- log2 "[graph] startForceAtlas" props.startForceAtlas
                if props.startForceAtlas then
                  Sigma.startForceAtlas2 sig props.forceAtlas2Settings
                else
                  Sigma.stopForceAtlas2 sig

                case props.mCamera of
                  Nothing -> pure unit
                  Just (GET.Camera { ratio, x, y }) -> do
                    Sigma.updateCamera sig { ratio, x, y }

                pure unit
          Just sig -> do
            pure unit

        setStage $ const Ready

    stageHooks props@{ showEdges: (showEdges /\ _), sigmaRef, stage: (Ready /\ setStage), transformedGraph } = do
      let tEdgesMap = SigmaxTypes.edgesGraphMap transformedGraph
      let tNodesMap = SigmaxTypes.nodesGraphMap transformedGraph

      -- TODO Probably this can be optimized to re-mark selected nodes only when they changed
      R.useEffect' $ do
        Sigmax.dependOnSigma (R.readRef sigmaRef) "[graphCpt (Ready)] no sigma" $ \sigma -> do
          Sigmax.performDiff sigma transformedGraph
          Sigmax.updateEdges sigma tEdgesMap
          Sigmax.updateNodes sigma tNodesMap
          Sigmax.setEdges sigma (not $ SigmaxTypes.edgeStateHidden showEdges)

    stageHooks _ = pure unit


type SigmaSettings =
  ( animationsTime :: Number
  , autoRescale :: Boolean
  , autoResize :: Boolean
  , batchEdgesDrawing :: Boolean
  , borderSize :: Number
  -- , canvasEdgesBatchSize :: Number
  -- , clone :: Boolean
  -- , defaultEdgeColor :: String
  , defaultEdgeHoverColor :: String
  , defaultEdgeType :: String
  , defaultHoverLabelBGColor :: String
  , defaultHoverLabelColor :: String
  , defaultLabelColor :: String
  -- , defaultLabelHoverColor :: String
  , defaultLabelSize :: Number
  , defaultNodeBorderColor :: String
  , defaultNodeColor :: String
  -- , defaultNodeHoverColor :: String
  -- , defaultNodeType :: String
  , doubleClickEnabled :: Boolean
  -- , doubleClickTimeout :: Number
  -- , doubleClickZoomDuration :: Number
  -- , doubleClickZoomingRatio :: Number
  -- , doubleTapTimeout :: Number
  -- , dragTimeout :: Number
  , drawEdgeLabels :: Boolean
  , drawEdges :: Boolean
  , drawLabels :: Boolean
  , drawNodes :: Boolean
  -- , edgeColor :: String
  , edgeHoverColor :: String
  , edgeHoverExtremities :: Boolean
  , edgeHoverPrecision :: Number
  , edgeHoverSizeRatio :: Number
  -- , edgesPowRatio :: Number
  -- , enableCamera :: Boolean
  , enableEdgeHovering :: Boolean
  , enableHovering :: Boolean
  -- , eventsEnabled :: Boolean
  , font :: String
  , fontStyle :: String
  , hideEdgesOnMove :: Boolean
  -- , hoverFont :: String
  -- , hoverFontStyle :: String
  -- , immutable :: Boolean
  -- , labelColor :: String
  -- , labelHoverBGColor :: String
  -- , labelHoverColor :: String
  -- , labelHoverShadow :: String
  -- , labelHoverShadowColor :: String
  , labelSize :: String
  , labelSizeRatio :: Number
  , labelThreshold :: Number
  , maxEdgeSize :: Number
  , maxNodeSize :: Number
  -- , minArrowSize :: Number
  , minEdgeSize :: Number
  , minNodeSize :: Number
  , mouseEnabled :: Boolean
  -- , mouseInertiaDuration :: Number
  -- , mouseInertiaRatio :: Number
  , mouseSelectorSize :: Number
  -- , mouseWheelEnabled :: Boolean
  , mouseZoomDuration :: Number
  , nodeBorderColor :: String
  -- , nodeHoverColor :: String
  --, nodesPowRatio :: Number
  , rescaleIgnoreSize :: Boolean
  -- , scalingMode :: String
  -- , sideMargin :: Number
  , singleHover :: Boolean
  -- , skipErrors :: Boolean
  , touchEnabled :: Boolean
  -- , touchInertiaDuration :: Number
  -- , touchInertiaRatio :: Number
  , twBorderGreyColor     :: String
  , twEdgeDefaultOpacity  :: Number
  , twEdgeGreyColor       :: String
  , twNodeRendBorderColor :: String
  , twNodeRendBorderSize :: Number
  , twNodesGreyOpacity    :: Number
  , twSelectedColor       :: String
  , verbose :: Boolean
  -- , webglEdgesBatchSize :: Number
  -- , webglOversamplingRatio :: Number
  , zoomMax :: Number
  , zoomMin :: Number
  , zoomingRatio :: Number
  )

  -- not selected <=> (1-greyness)
  -- selected nodes <=> special label
sigmaSettings :: {|SigmaSettings}
sigmaSettings =
  { animationsTime: 30000.0
  , autoRescale: true
  , autoResize: true
  , batchEdgesDrawing: true
  , borderSize: 1.0                   -- for ex, bigger border when hover
  , defaultEdgeHoverColor: "#f00"
  , defaultEdgeType: "curve"          -- 'curve' or 'line' (curve iff ourRendering)
  , defaultHoverLabelBGColor: "#fff"
  , defaultHoverLabelColor: "#000"
  , defaultLabelColor: "#000"         -- labels text color
  , defaultLabelSize: 15.0                -- (old tina: showLabelsIfZoom)
  , defaultNodeBorderColor : "#000"   -- <- if nodeBorderColor = 'default'
  , defaultNodeColor: "#FFF"
  , doubleClickEnabled: false -- indicates whether or not the graph can be zoomed on double-click
  , drawEdgeLabels: true
  , drawEdges: true
  , drawLabels: true
  , drawNodes: true
  , enableEdgeHovering: false
  , edgeHoverExtremities: true
  , edgeHoverColor: "edge"
  , edgeHoverPrecision: 2.0
  , edgeHoverSizeRatio: 2.0
  , enableHovering: true
  , font: "arial"                -- font params
  , fontStyle: "bold"
  , hideEdgesOnMove: true
  --, labelSize : "proportional" -- alt : proportional, fixed
  , labelSize: "fixed"
  , labelSizeRatio: 2.0               -- label size in ratio of node size
  , labelThreshold: 5.0               -- min node cam size to start showing label
  , maxEdgeSize: 1.0
  , maxNodeSize: 8.0
  , minEdgeSize: 0.5              -- in fact used in tina as edge size
  , minNodeSize: 1.0
  , mouseEnabled: true
  , mouseSelectorSize: 15.0
  , mouseZoomDuration: 150.0
  , nodeBorderColor: "default"           -- choices: "default" color vs. "node" color
  --, nodesPowRatio : 10.8
  , rescaleIgnoreSize : false
  , singleHover : true
  , touchEnabled : true
  , twBorderGreyColor : "rgba(100, 100, 100, 0.9)"
  , twEdgeDefaultOpacity : 0.4       -- initial opacity added to src/tgt colors
  , twEdgeGreyColor : "rgba(100, 100, 100, 0.25)"
  , twNodeRendBorderColor : "#FFF"
  , twNodeRendBorderSize : 2.5          -- node borders (only iff ourRendering)
  , twNodesGreyOpacity : 5.5           -- smaller value: more grey
  , twSelectedColor : "node"     -- "node" for a label bg like the node color, "default" for white background
  , verbose : true
  , zoomMax: 1.7
  , zoomMin: 0.0
  , zoomingRatio: 1.7
  }
  
type ForceAtlas2Settings =
  ( adjustSizes :: Boolean
  , barnesHutOptimize :: Boolean
  -- , barnesHutTheta :: Number
  , edgeWeightInfluence :: Number
  -- , fixedY  :: Boolean
  , gravity :: Number
  , iterationsPerRender :: Number
  , linLogMode :: Boolean
  , outboundAttractionDistribution :: Boolean
  , scalingRatio :: Number
  , skipHidden :: Boolean
  , slowDown :: Number
  , startingIterations :: Number
  , strongGravityMode :: Boolean
  -- , timeout :: Number
  -- , worker :: Boolean
  )

forceAtlas2Settings :: {|ForceAtlas2Settings}
forceAtlas2Settings =
  { adjustSizes : true
  , barnesHutOptimize   : true
  , edgeWeightInfluence : 1.0
    -- fixedY : false
  , gravity : 1.0
  , iterationsPerRender : 10.0
  , linLogMode : false  -- false
  , outboundAttractionDistribution: false
  , scalingRatio : 10.0
  , skipHidden: false
  , slowDown : 1.0
  , startingIterations : 10.0
  , strongGravityMode : false
  }
