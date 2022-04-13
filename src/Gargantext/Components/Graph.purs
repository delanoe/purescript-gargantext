module Gargantext.Components.Graph
  -- ( graph, graphCpt
  -- , sigmaSettings, SigmaSettings, SigmaOptionalSettings
  -- , forceAtlas2Settings, ForceAtlas2Settings, ForceAtlas2OptionalSettings
  -- )
  where

import Gargantext.Prelude

import DOM.Simple (window)
import DOM.Simple.Types (Element)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Gargantext.Components.App.Data (Boxes)
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Components.Themes (darksterTheme)
import Gargantext.Components.Themes as Themes
import Gargantext.Hooks.Sigmax as Sigmax
import Gargantext.Hooks.Sigmax.Sigma as Sigma
import Gargantext.Hooks.Sigmax.Types as SigmaxTypes
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as RH
import Record (merge)
import Record as Record
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Graph"

type OnProps  = ()

data Stage = Init | Ready | Cleanup
derive instance Generic Stage _
derive instance Eq Stage


type Props sigma forceatlas2 =
  ( boxes                 :: Boxes
  , elRef                 :: R.Ref (Nullable Element)
  , forceAtlas2Settings   :: forceatlas2
  , graph                 :: SigmaxTypes.SGraph
  , mCamera               :: Maybe GET.Camera
  , multiSelectEnabledRef :: R.Ref Boolean
  , selectedNodeIds       :: T.Box SigmaxTypes.NodeIds
  , showEdges             :: T.Box SigmaxTypes.ShowEdgesState
  , sigmaRef              :: R.Ref Sigmax.Sigma
  , sigmaSettings         :: sigma
  , stage                 :: T.Box Stage
  , startForceAtlas       :: Boolean
  , transformedGraph      :: SigmaxTypes.SGraph
  )

graph :: forall s fa2. R2.Component (Props s fa2)
graph = R.createElement graphCpt

graphCpt :: forall s fa2. R.Component (Props s fa2)
graphCpt = here.component "graph" cpt where
    cpt props@{ elRef
              , showEdges
              , sigmaRef
              , stage } _ = do
      showEdges' <- T.useLive T.unequal showEdges
      stage' <- T.useLive T.unequal stage

      stageHooks (Record.merge { showEdges', stage' } props)

      R.useEffectOnce $ do
        pure $ do
          here.log "[graphCpt (Cleanup)]"
          Sigmax.dependOnSigma (R.readRef sigmaRef) "[graphCpt (Cleanup)] no sigma" $ \sigma -> do
            Sigma.stopForceAtlas2 sigma
            here.log2 "[graphCpt (Cleanup)] forceAtlas stopped for" sigma
            Sigma.kill sigma
            here.log "[graphCpt (Cleanup)] sigma killed"

      -- NOTE: This div is not empty after sigma initializes.
      -- When we change state, we make it empty though.
      --pure $ RH.div { ref: elRef, style: {height: "95%"} } []
      pure $ case R.readNullableRef elRef of
        Nothing -> RH.div {} []
        Just el -> R.createPortal [] el

    stageHooks { elRef
               , mCamera
               , multiSelectEnabledRef
               , selectedNodeIds
               , forceAtlas2Settings: fa2
               , graph: graph'
               , sigmaRef
               , stage
               , stage': Init
               , startForceAtlas
               , boxes
               } = do
      R.useEffectOnce' $ do
        let rSigma = R.readRef sigmaRef

        case Sigmax.readSigma rSigma of
          Nothing -> do
            theme <- T.read boxes.theme
            eSigma <- Sigma.sigma {settings: sigmaSettings theme}
            case eSigma of
              Left err -> here.log2 "[graphCpt] error creating sigma" err
              Right sig -> do
                Sigmax.writeSigma rSigma $ Just sig

                Sigmax.dependOnContainer elRef "[graphCpt (Ready)] container not found" $ \c -> do
                  _ <- Sigma.addRenderer sig {
                      "type": "canvas"
                    , container: c
                    , additionalContexts: ["mouseSelector"]
                    }
                  pure unit

                Sigmax.refreshData sig $ Sigmax.sigmafy graph'

                Sigmax.dependOnSigma (R.readRef sigmaRef) "[graphCpt (Ready)] no sigma" $ \sigma -> do
                  -- bind the click event only initially, when ref was empty
                  Sigmax.bindSelectedNodesClick sigma selectedNodeIds multiSelectEnabledRef
                  _ <- Sigma.bindMouseSelectorPlugin sigma
                  pure unit

                Sigmax.setEdges sig false

                -- here.log2 "[graph] startForceAtlas" startForceAtlas
                if startForceAtlas then
                  Sigma.startForceAtlas2 sig fa2
                else
                  Sigma.stopForceAtlas2 sig

                case mCamera of
                  Just (GET.Camera { ratio, x, y }) -> do
                    Sigma.updateCamera sig { ratio, x, y }
                  -- Default camera: slightly de-zoom the graph to avoid
                  -- nodes sticking to the container borders
                  Nothing                           ->
                    Sigma.updateCamera sig { ratio: 1.1, x: 0.0, y: 0.0 }

                -- Reload Sigma on Theme changes
                _ <- flip T.listen boxes.theme \{ old, new } ->
                  if (eq old new) then pure unit
                  else Sigma.proxySetSettings window sig $ sigmaSettings new

                pure unit
          Just _sig -> do
            pure unit

        T.write Ready stage


    stageHooks { showEdges'
               , sigmaRef
               , stage': Ready
               , transformedGraph
               } = do
      let tEdgesMap = SigmaxTypes.edgesGraphMap transformedGraph
      let tNodesMap = SigmaxTypes.nodesGraphMap transformedGraph

      -- TODO Probably this can be optimized to re-mark selected nodes only when they changed
      R.useEffect' $ do
        Sigmax.dependOnSigma (R.readRef sigmaRef) "[graphCpt (Ready)] no sigma" $ \sigma -> do
          Sigmax.performDiff sigma transformedGraph
          Sigmax.updateEdges sigma tEdgesMap
          Sigmax.updateNodes sigma tNodesMap
          let edgesState = not $ SigmaxTypes.edgeStateHidden showEdges'
          here.log2 "[graphCpt] edgesState" edgesState
          Sigmax.setEdges sigma edgesState


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
sigmaSettings :: Themes.Theme -> {|SigmaSettings}
sigmaSettings theme =
  { animationsTime : 30000.0
  , autoRescale : true
  , autoResize : true
  , batchEdgesDrawing : true
  , borderSize : 1.0                   -- for ex, bigger border when hover
  , defaultEdgeHoverColor : "#f00"
  , defaultEdgeType : "curve"          -- 'curve' or 'line' (curve iff ourRendering)
  -- , defaultHoverLabelBGColor : "#fff"
  -- , defaultHoverLabelColor : "#000"
  -- , defaultLabelColor : "#000"         -- labels text color
  , defaultLabelSize : 15.0                -- (old tina: showLabelsIfZoom)
  , defaultNodeBorderColor  : "#000"   -- <- if nodeBorderColor = 'default'
  , defaultNodeColor : "#FFF"
  , doubleClickEnabled : false -- indicates whether or not the graph can be zoomed on double-click
  , drawEdgeLabels : true
  , drawEdges : true
  , drawLabels : true
  , drawNodes : true
  , enableEdgeHovering : false
  , edgeHoverExtremities : true
  , edgeHoverColor : "edge"
  , edgeHoverPrecision : 2.0
  , edgeHoverSizeRatio : 2.0
  , enableHovering : true
  , font : "arial"
  , fontStyle : ""
  , hideEdgesOnMove : true
  , labelSize  : "proportional" -- alt : proportional, fixed
  -- , labelSize : "fixed"
  , labelSizeRatio : 2.0               -- label size in ratio of node size
  , labelThreshold : 9.0 -- 5.0 for more labels              -- min node cam size to start showing label
  , maxEdgeSize : 1.0
  , maxNodeSize : 10.0
  , minEdgeSize : 0.5              -- in fact used in tina as edge size
  , minNodeSize : 1.0
  , mouseEnabled : true
  , mouseSelectorSize : 15.0
  , mouseZoomDuration : 150.0
  , nodeBorderColor : "default"           -- choices: "default" color vs. "node" color
  --, nodesPowRatio  : 10.8
  , rescaleIgnoreSize  : false
  , singleHover  : true
  , touchEnabled  : true
  , twBorderGreyColor  : "rgba(100, 100, 100, 0.9)"
  , twEdgeDefaultOpacity  : 0.4       -- initial opacity added to src/tgt colors
  , twEdgeGreyColor  : "rgba(100, 100, 100, 0.25)"
  , twNodeRendBorderColor  : "#FFF"
  , twNodeRendBorderSize  : 2.5          -- node borders (only iff ourRendering)
  , twNodesGreyOpacity  : 5.5           -- smaller value: more grey
  , twSelectedColor  : "node"     -- "node" for a label bg like the node color, "default" for white background
  , verbose  : true
  , zoomMax : 1.7
  , zoomMin : 0.0
  , zoomingRatio : 1.4
  } `merge` themeSettings theme
  where
    themeSettings t
      | eq t darksterTheme =
          { defaultHoverLabelBGColor: "#FFF"
          , defaultHoverLabelColor : "#000"
          , defaultLabelColor: "#FFF"
          }
      | otherwise =
          { defaultHoverLabelBGColor: "#FFF"
          , defaultHoverLabelColor : "#000"
          , defaultLabelColor: "#000"
          }

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

forceAtlas2Settings :: {|ForceAtlas2Settings}
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
  , slowDown                       : 1.0
  , startingIterations             : 10.0
  , strongGravityMode              : false
  }
