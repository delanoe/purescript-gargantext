module Gargantext.Components.Graph
  -- ( graph, graphCpt
  -- , sigmaSettings, SigmaSettings, SigmaOptionalSettings
  -- , forceAtlas2Settings, ForceAtlas2Settings, ForceAtlas2OptionalSettings
  -- )
  where
import Prelude (bind, discard, pure, ($), (<$>), (<*>))
import Data.Either (Either(..), either, note)
import Data.Maybe (Maybe)
import Data.Nullable (null, Nullable)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import DOM.Simple as DOM
import Effect (Effect)
import Reactix as R
import Reactix.DOM.HTML as RH

import Gargantext.Hooks.Sigmax
import Gargantext.Hooks.Sigmax.Sigma as Sigma
import Gargantext.Hooks.Sigmax.Types as Sigmax
import Gargantext.Utils.Reactix as R2

type OnProps  = ()

type Node =
  ( id    :: String
  , label :: String
  , x     :: Number
  , y     :: Number
  , size  :: Number
  , color :: String )

type Edge = ( id :: String, source :: String, target :: String )

type Graph = Sigmax.Graph Node Edge

type Props sigma fa2 =
  ( graph :: Graph
  , forceAtlas2Settings :: fa2
  , sigmaSettings :: sigma
  )

type Last sigma fa2 =
  ( graph :: R.Ref (Maybe Graph)
  , fa2   :: R.Ref (Maybe fa2)
  , sigma :: R.Ref (Maybe sigma)
  )

graph :: forall s fa2. Record (Props s fa2) -> R.Element
graph props = R.createElement graphCpt props []

graphCpt :: forall s fa2. R.Component (Props s fa2)
graphCpt = R.hooksComponent "Graph" cpt
  where
    cpt props _ = do
      containerRef <- R2.nullRef
      sigmaRef <- R2.nothingRef
      last <- R2.nothingRef
      R.useEffectOnce do
        let r = (/\) <$> assertContainer containerRef <*> assertCreateSigma unit
        case r of
          Left e -> e *> pure R.nothing
          Right (container /\ sigma) -> do
            Sigma.setSettings sigma props.sigmaSettings
            good <- addRenderer container sigma
            if good then
              setLast last props
            else
              pure unit
            R.setRef sigmaRef sigma

      R.useEffect3 props.graph props.forceAtlas2Settings props.sigmaSettings $
        startSigma ref props.sigmaRef props.sigmaSettings props.forceAtlas2Settings props.graph

      pure $ RH.div { ref: containerRef, style: {height: "95%"} } []

graphEffect {graph, forceAtlas2Settings, sigmaSettings} = R.useEffect3 graph forceAtlas2Settings sigmaSettings $
  pure unit
    
setLast :: forall s fa2. R.Ref (Record (Last s fa2)) -> Record (Props s fa2) -> Effect Unit
setLast ref {graph, forceAtlas2Settings, sigmaSettings} = R.setRef ref new
  where
    new = {graph, fa2: forceAtlas2Settings, sigma: sigmaSettings}

assertContainer :: R.Ref (Nullable DOM.Element) -> Either (Effect Unit) DOM.Element
assertContainer ref = note err $ R.readNullableRef containerRef where
  err = log "[G.C.Graph.graph] container not found"

assertCreateSigma :: Unit -> Either (Effect Unit) Sigma
assertCreateSigma = either err Right $ Sigma.sigma where
  err = log2 "[G.C.Graph.graph] Error initialising sigma: "

addRenderer :: DOM.Element -> Sigma -> Effect Bool
addRenderer container sigma = do
  ret <- Sigma.addRenderer sigma {container, "type": "canvas"}
  case ret of
    Right r -> pure True
    Left e -> log2 "[G.C.Graph.graph] Error creating renderer: " e *> pure False

  --either err Right $ Sigma.sigma where
  --  err = log2 "[G.C.Graph.graph] Error initialising sigma: "
  
useSigma containerRef props = do
  sigmaRef <- R2.nothingRef
  R.useEffectOnce $ delay unit $ \_ ->
    case R.readNullableRef containerRef of
      Nothing -> log "[G.C.Graph.useSigma] container not found!" *> pure R.nothing
      Just container ->
        case Sigma.sigma unit of
          Left e -> log2 "[G.C.Graph.useSigma] Error initialising sigma: " e *> pure R.nothing
          Right sigma -> do
            log2 "[G.H.Sigmax.useSigma] Sigma initialised: " sigma
            R.setRef sigmaRef sigma
            pure $ Sigma.killSigma sigma
          --h sigma = do
          --      log2 "[G.C.Graph.graph] Found sigma!" sigma
  pure sigmaRef
  where
    named msg = "[G.C.Graph.useSigma] " <> msg
    signed = log <<< named
    signed2 msg = log2 <<< named msg



--assertAddRenderer sigma container =
  
type SigmaSettings =
  ( animationsTime :: Number
  , autoRescale :: Boolean
  , autoResize :: Boolean
  , batchEdgesDrawing :: Boolean
  , borderSize :: Number
  -- , canvasEdgesBatchSize :: Number
  -- , clone :: Boolean
  -- , defaultEdgeColor :: String
  -- , defaultEdgeHoverColor :: String
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
  -- , doubleClickEnabled :: Boolean
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
  -- , edgeHoverColor :: String
  -- , edgeHoverExtremities :: Boolean
  -- , edgeHoverPrecision :: Number
  -- , edgeHoverSizeRatio :: Number
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
  , defaultEdgeType: "curve"          -- 'curve' or 'line' (curve iff ourRendering)
  , defaultHoverLabelBGColor: "#fff"
  , defaultHoverLabelColor: "#000"
  , defaultLabelColor: "#000"         -- labels text color
  , defaultLabelSize: 14.0                -- (old tina: showLabelsIfZoom)
  , defaultNodeBorderColor: "black"   -- <- if nodeBorderColor = 'default'
  , defaultNodeColor: "#ddd"
  , drawEdgeLabels: true
  , drawEdges: true
  , drawLabels: true
  , drawNodes: true
  , enableEdgeHovering: false
  , enableHovering: true
  , font: "Droid Sans"                -- font params
  , fontStyle: "bold"
  , hideEdgesOnMove: true
  , labelSize : "fixed"
  , labelSizeRatio: 2.0               -- label size in ratio of node size
  , labelThreshold: 2.0               -- min node cam size to start showing label
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
