module Gargantext.Components.Graph
  -- ( graph, graphCpt
  -- , sigmaSettings, SigmaSettings, SigmaOptionalSettings
  -- , forceAtlas2Settings, ForceAtlas2Settings, ForceAtlas2OptionalSettings
  -- )
  where
import Prelude (bind, const, discard, pure, ($), unit)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log, log2)
import DOM.Simple.Types (Element)
import FFI.Simple (delay)
import Reactix as R
import Reactix.DOM.HTML as RH

import Gargantext.Hooks.Sigmax as Sigmax
import Gargantext.Hooks.Sigmax.Types as SigmaxTypes
import Gargantext.Hooks.Sigmax.Sigma as Sigma

type OnProps  = ()

type Graph = SigmaxTypes.Graph SigmaxTypes.Node SigmaxTypes.Edge

data Stage = Init | Ready | Cleanup

type Props sigma forceatlas2 =
  ( elRef :: R.Ref (Nullable Element)
  , forceAtlas2Settings :: forceatlas2
  , graph :: Graph
  , selectedEdgeIds :: R.State SigmaxTypes.SelectedEdgeIds
  , selectedNodeIds :: R.State SigmaxTypes.SelectedNodeIds
  , sigmaSettings :: sigma
  , sigmaRef :: R.Ref Sigmax.Sigma
  , stage :: R.State Stage
  )

graph :: forall s fa2. Record (Props s fa2) -> R.Element
graph props = R.createElement graphCpt props []

graphCpt :: forall s fa2. R.Component (Props s fa2)
graphCpt = R.hooksComponent "Graph" cpt
  where
    cpt props _ = do
      stageHooks props

      -- NOTE: This div is not empty after sigma initializes.
      -- When we change state, we make it empty though.
      --pure $ RH.div { ref: props.elRef, style: {height: "95%"} } []
      pure $ case R.readNullableRef props.elRef of
        Nothing -> RH.div {} []
        Just el -> R.createPortal [] el

    stageHooks props@{stage: (Init /\ setStage)} = do
      R.useEffectOnce $ do
        log "[graphCpt] effect once"
        let rSigma = R.readRef props.sigmaRef

        case Sigmax.readSigma rSigma of
          Nothing -> do
            eSigma <- Sigma.sigma {settings: props.sigmaSettings}
            case eSigma of
              Left err -> log2 "[graphCpt] error creating sigma" err
              Right sig -> do
                Sigmax.writeSigma rSigma $ Just sig

                Sigmax.dependOnContainer props.elRef "[graphCpt] container not found" $ \c -> do
                  _ <- Sigma.addRenderer sig {
                      "type": "canvas"
                    , container: c
                    }
                  pure unit

                Sigmax.refreshData sig $ Sigmax.sigmafy props.graph

                Sigmax.setEdges sig false
                Sigma.startForceAtlas2 sig props.forceAtlas2Settings

                -- bind the click event only initially, when ref was empty
                Sigmax.bindSelectedNodesClick props.sigmaRef props.selectedNodeIds
                Sigmax.bindSelectedEdgesClick props.sigmaRef props.selectedEdgeIds
          Just sig -> do
            pure unit

        setStage $ const $ Ready

        delay unit $ \_ -> do
          log "[graphCpt] cleanup"
          pure $ pure unit

    stageHooks props@{stage: (Ready /\ setStage)} = do
      let edgesMap = SigmaxTypes.edgesGraphMap props.graph
      let nodesMap = SigmaxTypes.nodesGraphMap props.graph

      -- TODO Probably this can be optimized to re-mark selected nodes only when they changed
      R.useEffect' $ do
        Sigmax.dependOnSigma (R.readRef props.sigmaRef) "[graphCpt] no sigma" $ \sigma -> do
          Sigmax.markSelectedEdges sigma (fst props.selectedEdgeIds) edgesMap
          Sigmax.markSelectedNodes sigma (fst props.selectedNodeIds) nodesMap

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
  -- , edgeHoverPrecision :: Number
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
  -- , mouseWheelEnabled :: Boolean
  , mouseZoomDuration :: Number
  , nodeBorderColor :: String
  -- , nodeHoverColor :: String
  -- , nodesPowRatio :: Number
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
  { animationsTime: 5500.0
  , autoRescale: true
  , autoResize: true
  , batchEdgesDrawing: true
  , borderSize: 3.0                   -- for ex, bigger border when hover
  , defaultEdgeHoverColor: "#f00"
  , defaultEdgeType: "curve"          -- 'curve' or 'line' (curve iff ourRendering)
  , defaultHoverLabelBGColor: "#fff"
  , defaultHoverLabelColor: "#000"
  , defaultLabelColor: "#000"         -- labels text color
  , defaultLabelSize: 8.0                -- (old tina: showLabelsIfZoom)
  , defaultNodeBorderColor: "black"   -- <- if nodeBorderColor = 'default'
  , defaultNodeColor: "#ddd"
  , doubleClickEnabled: false
  , drawEdgeLabels: true
  , drawEdges: true
  , drawLabels: true
  , drawNodes: true
  , enableEdgeHovering: true
  , edgeHoverExtremities: true
  , edgeHoverColor: "edge"
  , edgeHoverSizeRatio: 2.0
  , enableHovering: true
  , font: "Droid Sans"                -- font params
  , fontStyle: "bold"
  , hideEdgesOnMove: true
  , labelSize : "fixed"
  , labelSizeRatio: 1.0               -- label size in ratio of node size
  , labelThreshold: 1.0               -- min node cam size to start showing label
  , maxEdgeSize: 1.0
  , maxNodeSize: 10.0
  , minEdgeSize: 0.5              -- in fact used in tina as edge size
  , minNodeSize: 5.0
  , mouseEnabled: true
  , mouseZoomDuration: 150.0
  , nodeBorderColor: "node"           -- choices: 'default' color vs. node color
  -- , nodesPowRatio: 0.3
  , rescaleIgnoreSize: false
  , singleHover: true
  , touchEnabled: true
  , twBorderGreyColor: "rgba(100, 100, 100, 0.5)"
  , twEdgeDefaultOpacity: 0.4       -- initial opacity added to src/tgt colors
  , twEdgeGreyColor: "rgba(100, 100, 100, 0.25)"
  , twNodeRendBorderColor: "#222"
  , twNodeRendBorderSize: 0.5          -- node borders (only iff ourRendering)
  , twNodesGreyOpacity: 5.5           -- smaller value: more grey
  , twSelectedColor: "default"     -- "node" for a label bg like the node color, "default" for white background
  , verbose : true
  , zoomMax: 1.7
  , zoomMin: 0.0
  , zoomingRatio: 3.2
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
  { adjustSizes : false
  , barnesHutOptimize   : true
  , edgeWeightInfluence : 0.0
    -- fixedY : false
  , gravity : 1.0
  , iterationsPerRender : 4.0
  , linLogMode : true  -- false
  , outboundAttractionDistribution: false
  , scalingRatio : 4.0
  , skipHidden: false
  , slowDown : 0.7
  , startingIterations : 2.0
  , strongGravityMode : false
  }
