module Gargantext.Components.GraphExplorer.Layout where

import Gargantext.Prelude hiding (max, min)

import DOM.Simple.Types (Element)
import Data.Array as A
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Nullable (null, Nullable)
import Data.Sequence as Seq
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Gargantext.Components.App.Data (Boxes)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.GraphExplorer.Resources as Graph
import Gargantext.Components.GraphExplorer.Controls as Controls
import Gargantext.Components.GraphExplorer.Sidebar as GES
import Gargantext.Components.GraphExplorer.Sidebar.Types as GEST
import Gargantext.Components.GraphExplorer.TopBar as GETB
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Config (defaultFrontends)
import Gargantext.Data.Louvain as Louvain
import Gargantext.Hooks.Sigmax.Types as SigmaxT
import Gargantext.Sessions (Session)
import Gargantext.Types (SidePanelState(..))
import Gargantext.Types as GT
import Gargantext.Types as Types
import Gargantext.Utils ((?))
import Gargantext.Utils.Range as Range
import Gargantext.Utils.Reactix as R2
import Math as Math
import Partial.Unsafe (unsafePartial)
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T


type Props =
  ( mMetaData'      :: Maybe GET.MetaData
  , graph           :: SigmaxT.SGraph
  , hyperdataGraph  :: GET.HyperdataGraph
  , session         :: Session
  , boxes           :: Boxes
  , graphId         :: GET.GraphId
  )

here :: R2.Here
here = R2.here "Gargantext.Components.GraphExplorer.Layout"

layout :: R2.Leaf Props
layout = R2.leaf layoutCpt

layoutCpt :: R.Component Props
layoutCpt = here.component "explorerWriteGraph" cpt where
  cpt props@{ boxes
            , graph
            , mMetaData'
            , graphId
            , session
            , hyperdataGraph
            } _ = do
  -- Computed
  -----------------

    let
      topBarPortalKey = "portal-topbar::" <> show graphId

      startForceAtlas = maybe true
        (\(GET.MetaData { startForceAtlas: sfa }) -> sfa) mMetaData'

      forceAtlasS = if startForceAtlas
                    then SigmaxT.InitialRunning
                    else SigmaxT.InitialStopped

  -- States
  -----------------

    sideBarDisplayed /\ sideBarDisplayedBox <-
      R2.useBox' (InitialClosed :: SidePanelState)

    { mMetaData: mMetaDataBox
    } <- GEST.focusedSidePanel boxes.sidePanelGraph
    _graphVersion' <- T.useLive T.unequal boxes.graphVersion


    -- _dataRef <- R.useRef graph
    graphRef <- R.useRef null

  -- Hooks
  -----------------

    controls <- Controls.useGraphControls
      { forceAtlasS
      , graph
      , graphId
      , hyperdataGraph
      , reloadForest: boxes.reloadForest
      , session
      , sidePanel: boxes.sidePanelGraph
      , sidePanelState: sideBarDisplayedBox
      }

    mTopBarHost <- R.unsafeHooksEffect $ R2.getElementById "portal-topbar"

    showControls' <- R2.useLive' controls.showControls

    -- graphVersionRef <- R.useRef graphVersion'
    -- R.useEffect' $ do
    --   let readData = R.readRef dataRef
    --   let gv = R.readRef graphVersionRef
    --   if SigmaxT.eqGraph readData graph then
    --     pure unit
    --   else do
    --     -- Graph data changed, reinitialize sigma.
    --     let rSigma = R.readRef controls.sigmaRef
    --     Sigmax.cleanupSigma rSigma "explorerCpt"
    --     R.setRef dataRef graph
    --     R.setRef graphVersionRef graphVersion'
    --     -- Reinitialize bunch of state as well.
    --     T.write_ SigmaxT.emptyNodeIds controls.removedNodeIds
    --     T.write_ SigmaxT.emptyNodeIds controls.selectedNodeIds
    --     T.write_ SigmaxT.EShow controls.showEdges
    --     T.write_ forceAtlasS controls.forceAtlasState
    --     T.write_ Graph.Init controls.graphStage
    --     T.write_ Types.InitialClosed controls.sidePanelState

  -- Render
  -----------------

    pure $

      H.div
      { className: "graph-layout" }
      [
        -- Topbar
        R2.createPortal' mTopBarHost
        [
          R2.fragmentWithKey topBarPortalKey
          [
            GETB.topBar
            { sidePanelState: sideBarDisplayedBox
            , sidePanelGraph: props.boxes.sidePanelGraph
            }
          ]
        ]
      ,
        -- Sidebar
        H.div
        { className: "graph-layout__sidebar"
        -- @XXX: ReactJS lack of "keep-alive" feature workaround solution
        -- @link https://github.com/facebook/react/issues/12039
        , style: { display: sideBarDisplayed == GT.Opened ? "block" $ "none" }
        }
        [
          case mMetaData' of
            Nothing ->
              B.caveat
              {}
              [ H.text "No meta data has been found for this node." ]

            Just metaData ->
              GES.sidebar
              { boxes
              , frontends: defaultFrontends
              , graph
              , graphId
              , metaData
              , session
              }
        ]
      ,
        -- Toolbar
        H.div
        { className: "graph-layout__toolbar"
        -- @XXX: ReactJS lack of "keep-alive" feature workaround solution
        -- @link https://github.com/facebook/react/issues/12039
        , style: { display: showControls' ? "block" $ "none" }
        }
        [
          Controls.controls controls []
        ]
      ,
        -- Content
        H.div
        { ref: graphRef
        , className: "graph-layout__content"
        }
        [
          graphView
          { boxes: props.boxes
          , controls
          , elRef: graphRef
          , graph
          , hyperdataGraph
          , mMetaData: mMetaDataBox
          }
        ]
      ]

--------------------------------------------------------------

type GraphProps =
  ( boxes          :: Boxes
  , controls       :: Record Controls.Controls
  , elRef          :: R.Ref (Nullable Element)
  , graph          :: SigmaxT.SGraph
  , hyperdataGraph :: GET.HyperdataGraph
  , mMetaData      :: T.Box (Maybe GET.MetaData)
)

graphView :: R2.Leaf GraphProps
graphView = R2.leaf graphViewCpt
graphViewCpt :: R.Component GraphProps
graphViewCpt = here.component "graphView" cpt
  where
    cpt { boxes
        , controls
        , elRef
        , graph
        , hyperdataGraph: GET.HyperdataGraph { mCamera }
        , mMetaData } _ = do
      edgeConfluence' <- T.useLive T.unequal controls.edgeConfluence
      edgeWeight' <- T.useLive T.unequal controls.edgeWeight
      mMetaData' <- T.useLive T.unequal mMetaData
      multiSelectEnabled' <- T.useLive T.unequal controls.multiSelectEnabled
      nodeSize' <- T.useLive T.unequal controls.nodeSize
      removedNodeIds' <- T.useLive T.unequal controls.removedNodeIds
      selectedNodeIds' <- T.useLive T.unequal controls.selectedNodeIds
      showEdges' <- T.useLive T.unequal controls.showEdges
      showLouvain' <- T.useLive T.unequal controls.showLouvain

      multiSelectEnabledRef <- R.useRef multiSelectEnabled'

      -- TODO Cache this?
      let louvainGraph =
            if showLouvain' then
              let louvain = Louvain.louvain unit in
              let cluster = Louvain.init louvain (SigmaxT.louvainNodes graph) (SigmaxT.louvainEdges graph) in
              SigmaxT.louvainGraph graph cluster
            else
              graph
      let transformedGraph = transformGraph louvainGraph { edgeConfluence'
                                                         , edgeWeight'
                                                         , nodeSize'
                                                         , removedNodeIds'
                                                         , selectedNodeIds'
                                                         , showEdges' }
      let startForceAtlas = maybe true (\(GET.MetaData { startForceAtlas: sfa }) -> sfa) mMetaData'

      R.useEffect1' multiSelectEnabled' $ do
        R.setRef multiSelectEnabledRef multiSelectEnabled'

      pure $

        Graph.graph
        { boxes
        , elRef
        , forceAtlas2Settings: Graph.forceAtlas2Settings
        , graph
        , mCamera
        , multiSelectEnabledRef
        , selectedNodeIds: controls.selectedNodeIds
        , showEdges: controls.showEdges
        , sigmaRef: controls.sigmaRef
        , sigmaSettings: Graph.sigmaSettings
        , stage: controls.graphStage
        , startForceAtlas
        , transformedGraph
        } []

--------------------------------------------------------

convert :: GET.GraphData -> Tuple (Maybe GET.MetaData) SigmaxT.SGraph
convert (GET.GraphData r) = Tuple r.metaData $ SigmaxT.Graph {nodes, edges}
  where
    nodes = foldMapWithIndex nodeFn r.nodes
    nodeFn _i nn@(GET.Node n) =
      Seq.singleton {
          borderColor: color
        , children: n.children
        , color : color
        , equilateral: { numPoints: 3 }
        , gargType
        , hidden : false
        , id    : n.id_
        , label : n.label
        , size  : Math.log (toNumber n.size + 1.0)
        , type  : modeGraphType gargType
        , x     : n.x -- cos (toNumber i)
        , y     : n.y -- sin (toNumber i)
        , _original: nn
        }
      where
        cDef (GET.Cluster {clustDefault}) = clustDefault
        color = GET.intColor (cDef n.attributes)
        gargType =  unsafePartial $ fromJust $ Types.modeFromString n.type_
    nodesMap = SigmaxT.nodesMap nodes
    edges = foldMapWithIndex edgeFn $ A.sortWith (\(GET.Edge {weight}) -> weight) r.edges
    edgeFn i ee@(GET.Edge e) =
      Seq.singleton
        { id : e.id_
        , color
        , confluence : e.confluence
        , hidden : false
        , size: 1.0
        , source : e.source
        , sourceNode
        , target : e.target
        , targetNode
        , weight : e.weight
        , weightIdx: i
        , _original: ee
        }
      where
        sourceNode = unsafePartial $ fromJust $ Map.lookup e.source nodesMap
        targetNode = unsafePartial $ fromJust $ Map.lookup e.target nodesMap
        color = sourceNode.color

--------------------------------------------------------------

-- | See sigmajs/plugins/sigma.renderers.customShapes/shape-library.js
modeGraphType :: Types.Mode -> String
modeGraphType Types.Authors = "square"
modeGraphType Types.Institutes = "equilateral"
modeGraphType Types.Sources = "star"
modeGraphType Types.Terms = "def"

--------------------------------------------------------------


type LiveProps = (
    edgeConfluence'  :: Range.NumberRange
  , edgeWeight'      :: Range.NumberRange
  , nodeSize'        :: Range.NumberRange
  , removedNodeIds'  :: SigmaxT.NodeIds
  , selectedNodeIds' :: SigmaxT.NodeIds
  , showEdges'       :: SigmaxT.ShowEdgesState
  )

transformGraph :: SigmaxT.SGraph -> Record LiveProps -> SigmaxT.SGraph
transformGraph graph { edgeConfluence'
                     , edgeWeight'
                     , nodeSize'
                     , removedNodeIds'
                     , selectedNodeIds' } = SigmaxT.Graph {nodes: newNodes, edges: newEdges}
  where
    edges = SigmaxT.graphEdges graph
    nodes = SigmaxT.graphNodes graph
    selectedEdgeIds =
      Set.fromFoldable
        $ Seq.map _.id
        $ SigmaxT.neighbouringEdges graph selectedNodeIds'
    hasSelection = not $ Set.isEmpty selectedNodeIds'

    newEdges' = Seq.filter edgeFilter $ Seq.map (
      -- NOTE We don't use edgeShowFilter anymore because of
      -- https://gitlab.iscpif.fr/gargantext/purescript-gargantext/issues/304
      -- edgeHideWeight <<< edgeHideConfluence <<< edgeShowFilter <<< edgeMarked
      edgeHideWeight <<< edgeHideConfluence <<< edgeMarked
      ) edges
    newNodes  = Seq.filter nodeFilter $ Seq.map (nodeMarked <<< nodeHideSize) nodes
    newEdges  = Seq.filter (edgeInGraph $ Set.fromFoldable $ Seq.map _.id newNodes) newEdges'

    edgeFilter _e = true
    nodeFilter n = nodeRemovedFilter n

    nodeRemovedFilter { id } = not $ Set.member id removedNodeIds'

    edgeHideConfluence :: Record SigmaxT.Edge -> Record SigmaxT.Edge
    edgeHideConfluence edge@{ confluence } =
      if Range.within edgeConfluence' confluence then
        edge
      else
        edge { hidden = true }

    edgeHideWeight :: Record SigmaxT.Edge -> Record SigmaxT.Edge
    edgeHideWeight edge@{ weightIdx } =
      if Range.within edgeWeight' $ toNumber weightIdx then
        edge
      else
        edge { hidden = true }

    edgeInGraph :: SigmaxT.NodeIds -> Record SigmaxT.Edge -> Boolean
    edgeInGraph nodeIds e = (Set.member e.source nodeIds) && (Set.member e.target nodeIds)

    edgeMarked :: Record SigmaxT.Edge -> Record SigmaxT.Edge
    edgeMarked edge@{ id, sourceNode } = do
      let isSelected = Set.member id selectedEdgeIds
      case Tuple hasSelection isSelected of
        Tuple false true  -> edge { color = "#ff0000" }
        Tuple true  true  -> edge { color = sourceNode.color }
        Tuple true false  -> edge { color = "rgba(221, 221, 221, 0.5)" }
        _                 -> edge

    nodeMarked :: Record SigmaxT.Node -> Record SigmaxT.Node
    nodeMarked node@{ id } =
      if Set.member id selectedNodeIds' then
        node { borderColor = "#000", type = "selected" }
      else
        node

    nodeHideSize :: Record SigmaxT.Node -> Record SigmaxT.Node
    nodeHideSize node@{ size } =
      if Range.within nodeSize' size then
        node
      else
        node { hidden = true }
