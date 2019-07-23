module Gargantext.Components.Graph
  -- ( graph, graphCpt
  -- , sigmaSettings, SigmaSettings, SigmaOptionalSettings
  -- , forceAtlas2Settings, ForceAtlas2Settings, ForceAtlas2OptionalSettings
  -- )
  where
import Prelude
import Control.Monad.Except.Trans
import Data.Array as A
import Data.Bitraversable (bitraverse)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (Nullable, null)
import Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Traversable (for, for_, traverse, traverse_)
import DOM.Simple.Types (Element)
import DOM.Simple.Console (log, log2)
import Effect (Effect)
import FFI.Simple (delay)
import Reactix as R
import Reactix.DOM.HTML as RH
import Gargantext.Utils.Reactix as R2
import Gargantext.Types (class Optional)
import Gargantext.Hooks.Sigmax
import Gargantext.Hooks.Sigmax.Types as Sigmax
import Gargantext.Hooks.Sigmax.Sigma (SigmaOpts)
import Gargantext.Hooks.Sigmax.Sigma as Sigma
import Gargantext.Hooks.Sigmax.Sigmajs ()

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

type Props sigma forceatlas2 =
  ( graph :: Graph
  , sigmaSettings :: sigma
  , forceAtlas2Settings :: forceatlas2 )

graph :: forall s fa2. Record (Props s fa2) -> R.Element
graph props = R.createElement graphCpt props []

graphCpt :: forall s fa2. R.Component (Props s fa2)
graphCpt = R.hooksComponent "Graph" cpt
  where
    cpt props _ = do
      ref <- R.useRef null
      sigma <- useSigma ref props.sigmaSettings
      useCanvasRenderer ref sigma
      useData sigma props.graph
      useForceAtlas2 sigma props.forceAtlas2Settings
      pure $ RH.div { ref, style: {height: "95%"} } []

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
  -- , defaultLabelSize :: String
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
  , labelMaxSize :: Number
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
  , batchEdgesDrawing: false
  , borderSize: 3.0                   -- for ex, bigger border when hover
  , defaultEdgeType: "curve"          -- 'curve' or 'line' (curve iff ourRendering)
  , defaultHoverLabelBGColor: "#fff"
  , defaultHoverLabelColor: "#000"
  , defaultLabelColor: "#000"         -- labels text color
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
  , labelMaxSize: 3.0                -- (old tina: showLabelsIfZoom)
  , labelSize : "proportional"
  , labelSizeRatio: 2.0               -- label size in ratio of node size
  , labelThreshold: 2.0               -- min node cam size to start showing label
  , maxEdgeSize: 0.0
  , maxNodeSize: 30.0
  , minEdgeSize: 0.0              -- in fact used in tina as edge size
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
